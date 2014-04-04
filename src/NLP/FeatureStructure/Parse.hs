{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


--  | Unification grammar parsing.  A pleliminary effort.


module NLP.FeatureStructure.Parse
(
-- * Rule
  Rule (..)
, mkRule
, mkEntry
, isFull

-- * Parse
, Sent
, Token
, parse

-- * Misc
, consume
, printRule
, drawRule
) where


import           Control.Applicative ((<$>), (<*>), (*>))
import           Control.Monad (forM_, guard, mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Loops (unfoldrM)
import           Data.Maybe (catMaybes)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Vector as V
import qualified Data.Traversable as Tr
-- import qualified Data.MemoCombinators as Memo
import           Control.Monad.Memo (memo, for2)
import qualified Control.Monad.Memo as Memo
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes

-- Graphviz
import qualified Data.GraphViz as Z
-- import qualified Data.GraphViz.Attributes.Complete as ZA
-- import           Data.GraphViz.Types.Monadic ((-->))
import qualified Data.GraphViz.Types.Monadic as Z


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.DisjSet as D
import qualified NLP.FeatureStructure.Join as J
-- import           NLP.FeatureStructure.Reid (Reid)
import qualified NLP.FeatureStructure.Reid as Reid


--------------------------------------------------------------------
-- Lexicon
--
-- A lexicon assigns a list/set of feature graphs to each form
-- of the language.
--
-- We don't need to define a lexicon data type at this moment.
-- We will assume, that individual words have already been proccessed
-- and transformed to graph representations.
--------------------------------------------------------------------


--------------------------------------------------------------------
-- Ruleset
--------------------------------------------------------------------


-- A fully parsed rule is a tree, in fact.  The same applies to
-- body-elements of the already parsed part in the partially
-- parsed rule.


-- -- | Beware!  An orphan instance...
-- instance Ord a => Ord (T.Tree a)


-- | A grammar ,,rule''.  It is a generalization of a regular rule.
-- We should probably change its name to a more appropriate one.
-- Anyway, it represents a regular rule when its `left` part is empty
-- and, in general, it can be thought as a partially proccessed rule.
data Rule f a = Rule {
    -- | Head of the rule.
      root  :: ID
    -- | Right part of the rule, to be parsed.
    , right :: [ID]
    -- | Left part of the rule, already parsed.
    -- Given in the reverse order.
    , left  :: T.Forest ID
    -- | Graph corresponding to the rule.
    , graph  :: Graph f a
    } deriving (Show, Eq)


-- | Draw the rule using the graphviz library.
drawRule :: (Z.Labellable f, Z.Labellable a) => Rule f a -> IO ()
drawRule rule = do

    let g = Z.digraph (Z.Str "R") $ do
        Z.graphAttrs [Z.ordering Z.OutEdges]
        -- Z.nodeAttrs [Z.ordering Z.OutEdges]
        drawStruct rule
        drawGraphNodes $ graph rule
        drawGraphEdges $ graph rule

    Z.runGraphvizCanvas Z.Dot g Z.Xlib

  where

    -- draw the feature graph (only nodes and clusters)
    drawGraphNodes g = forM_ (I.toList $ nodeMap g) $ \(i, nd) -> case nd of
        -- we don't draw orphan frontier nodes!
        Frontier _ -> return ()
        -- Edge order is not preserved with clusters, unfortunately!
        -- Interior m -> Z.cluster (Z.Int i) $ do
        Interior m -> do
            Z.node (gn g i) []
            forM_ (M.elems m) $ \j ->
                case I.lookup (D.repr j $ disjSet g) (nodeMap g) of
                    Nothing -> return ()
                    Just n' -> case n' of
                        Interior _  -> return ()
                        Frontier x  -> Z.node (fn g i j)
                            [Z.toLabel x, Z.shape Z.PlainText]

    -- draw the feature graph (only edges)
    drawGraphEdges g = forM_ (I.toList $ nodeMap g) $ \(i, nd) -> case nd of
        -- we don't draw orphan frontier nodes!
        Frontier _ -> return () -- Z.node (gn g i) [Z.toLabel x]
        Interior m -> forM_ (M.toList m) $ \(f, j) -> do
            case I.lookup (D.repr j $ disjSet g) (nodeMap g) of
                Nothing -> return ()
                Just n' -> case n' of
                    Interior _  -> Z.edge (gn g i) (gn g j) [Z.toLabel f]
                    Frontier _  -> Z.edge (gn g i)
                        (fn g i j) [Z.toLabel f, Z.style Z.dotted]

    -- draw the rule structure
    drawStruct Rule{..} = do
        addSN graph root
        forM_ (reverse left) $ \t -> do
            let root' = T.rootLabel t
            Z.edge (sn graph root) (sn graph root') [Z.style Z.bold]
            drawTree graph t
        forM_ right $ \x -> do
            addSN' graph x [Z.color Z.LightGray]
            Z.edge (sn graph root) (sn graph x) [Z.style Z.bold]
    drawTree g t = do
        addSN g $ T.rootLabel t
        forM_ (reverse $ T.subForest t) $ \c -> do
            Z.edge (sn g (T.rootLabel t)) (sn g (T.rootLabel c))
                [Z.style Z.bold]
            drawTree g c

    -- Structure node "identifier"
    -- sn g x = "S" ++ show (D.repr x $ disjSet g)
    sn g x = show $ D.repr x $ disjSet g
    -- Define structure node
    addSN g x = addSN' g x []
    addSN' g x as = Z.node (sn g x) $
        [Z.style Z.filled, Z.color Z.LightBlue] ++ as

    -- Graph node "identifier"
    gn g x = show $ D.repr x $ disjSet g

    -- Graph frontier node "identifier"; takes
    -- into account the identifier of the parent
    fn g x y = gn g x ++ "-" ++ gn g y


-- | Print the rule to stdout.
printRule :: (Show f, Show a) => Rule f a -> IO ()
printRule Rule{..} = do
    putStrLn $ "# root: " ++ show root
    printTree graph root
    putStrLn ""
 
    putStrLn "# to be parsed:"
    forM_ right $ printTree graph
    putStrLn ""

    putStrLn "# derived forest (reversed):"
    putStr $ T.drawForest $ map (fmap show) left

    putStrLn "# FGs for the forest:"
    forM_ left $ Tr.mapM $ printTree graph
    putStrLn ""


-- | `Reid.split` and reidentify the rule.
ruleInst :: (Functor m, Monad m) => Rule f a -> Reid.ReidT m (Rule f a)
ruleInst Rule{..} = Reid.split *> ( Rule
    <$> Reid.reid root
    <*> mapM Reid.reid right
    <*> mapM (Tr.mapM Reid.reid) left
    <*> Reid.reidGraph graph )


-- | A smart rule constructur.
mkRule :: ID -> [ID] -> Graph f a -> Rule f a
mkRule x xs g = Rule
    { root  = x
    , right = xs
    , left  = []
    , graph = g }


-- | A smart entry constructur.
mkEntry :: ID -> Graph f a -> Rule f a
mkEntry x g = Rule
    { root  = x
    , right = []
    , left  = []
    , graph = g }


-- | Is it a fully processed rule?
isFull :: Rule f a -> Bool
isFull Rule{..} = null right


-- | A set of rules.
-- type RuleSet f a = S.Set (Rule f a)


-- | Move the ,,dot'' in the partially parsed rule by unifying
-- the next `right` node with the *fully* parsed rule.
--
-- It is not checked, if the second argument is indeed fully
-- parsed.  On the other hand, when the first one is fully
-- parsed, the function will crash.
--
-- We also assume, that the sets of identifiers are disjoint
-- in both the function arguments.
consume :: (Eq a, Ord f) => Rule f a -> Rule f a -> Maybe (Rule f a)
consume p f = do
    -- x  <- rightHead p
    (x, p') <- shift p $ left f
    g' <- J.execJoin
        (J.join x (root f))
        (fromTwo (graph p) (graph f))
    return $ p' { graph = g' }
        

-- consume
--     :: Rule i f a -> Rule i f a
--     -> Join i f a (Maybe (Rule i f a))
-- consume p f = do
--         (fromTwo (graph p) (graph f))
--         doit
--   where
--     doit = do
--         J.join (Left $ beforeHead p) (Right $ root f)


-- -- | Head of the rule-before.
-- rightHead :: Rule f a -> Maybe ID
-- rightHead Rule{..} = case right of
--     (x:_) -> Just x
--     []    -> Nothing


-- | Shift the next right node and link the given children to it.
shift :: Rule f a -> T.Forest ID -> Maybe (ID, Rule f a)
shift r@Rule{..} ts = case right of
    (x:xs) -> Just (x, r
        { left  = T.Node x ts : left   -- reverse!
        , right = xs } )
    [] -> Nothing


--------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------


-- | A sentence to be parsed
type Sent f a = V.Vector (Token f a)

-- A token is a set of lexicon entries (fully processed rules)
-- identified for a particular input word.
type Token f a = S.Set (Rule f a)


-- | A recursive definition with memoization.
--
-- = General definition
--
-- Let `T = parse sent` for a given sentence `sent`.
-- We define `T(i, j)` as a set of partially processed rules covering
-- positions `[i, j)` of the input sentence.  Therefore, the result
-- of parsing will consists of fully processed rules in
-- `T(0, length(sent))`.
-- 
-- = Recursive definition, first attempt
--
-- For a given `k` satisfying `i <= k < j` we consider every partially
-- processed rule `p` covering `[i, k)` and every fully processed rule
-- `q` covering `[k, j)`.  If `p` can `consume` `q`, then we add the
-- resulting rule into the set `T(i, j)`.
--
-- Let us note that, within the definition above, for `k = i`, `T(i, j)`
-- is defined on the basis of `T(i, j)` itself!  This is certainly not
-- what we want.
--
-- = Recursive definition, second attempt
--
-- We rule out the case of `k = i`.
--
-- For a given `k` satisfying `i < k < j` we consider every partially
-- processed rule `p` covering `[i, k)` and every fully processed rule
-- `q` covering `[k, j)`.  If `p` can `consume` `q`, then we add the
-- resulting rule into the set `T(i, j)`.
--
-- Now, to avoid infinite looping, we assume a linear ordering on the
-- rules `T(i, i)` of the grammar.  Without loss of generality we can
-- assume that the ordering corresponds to the placement of elements
-- in the list `T(i, i)`.
-- We consider extending the `T(i, j)` set with the rule `r` after it has
-- already been extended with all rules from `T(i, i)` lower then `r`.
--
-- TODO: in order to make the method described above work we need
-- to store lists and not sets in `T(i, i)`.  Otherwise, we cannot
-- enforce a custom ordering of the grammar rules.
--
-- = Input
--
-- First we need to populate the `T(i, i)` sets for `0 <= i < length sent`
-- with reidentified rules of the grammar.
-- We also need to take the input words into account, of course.
-- For each position `i` in the sentence we define `T(i, i+1)` as a set
-- of lexicon entries matching the word on position `i` converted into
-- the form of fully parsed rules.
--
-- Therefore, at the beginning, we have a `T` structure populated with
-- rules of the grammar (represented as partially parsed rules) and
-- lexicon entries (represented as fully parsed rules).
--
-- TODO: change grammar and sentence input types.
--
parse
    :: (Ord a, Ord f)
    => [Rule f a]       -- ^ Grammar rules (to be reid/instantiated)
    -> [Rule f a]       -- ^ Sentence (to be reid)
    -> Int -> Int       -- ^ Positions in the sentence
    -> [Rule f a]
parse rules sent0 mi mj =
        
    Reid.runReid $ do
        sent <- V.fromList . map (:[]) <$> mapM ruleInst sent0
        Memo.startEvalMemoT $ doit sent mi mj

  where
    doit sent = t
      where

        -- all rules on the span [i, j)
        t i j = do
            xs  <- u0 i j
            xss <- unfoldrM uk xs
            return $ xs ++ concat xss

        -- u(k) on the basis of u(k-1)
        uk xs = do
            rs <- lift $ mapM ruleInst rules
            let ys = catMaybes [consume r x | x <- xs, isFull x, r <- rs]
            return $ case ys of
                -- stop unfolding
                []  -> Nothing  
                -- argument for the next unfolding step is the same
                -- as the element of the unfolding result
                _   -> Just (ys, ys)
    
        -- Move the ,,dot'' in the beforehand introduced rules
        u0 i j
            | i+1 == j  = return $ sent V.! i
            | otherwise = Pipes.toListM $ Pipes.every $ do
                k <- list [i+1 .. j-1]
                -- Partially processed rule on [i .. k)
                lefts <- lift $ for2 memo t i k
                p <- list lefts
                guard $ not $ isFull p
                -- Fully processed rule on [k .. j)
                rights <- lift $ for2 memo t k j
                f <- list rights
                guard $ isFull f
                -- Consume and unify
                case consume p f of
                    Nothing -> mzero
                    Just q  -> return q
                -- q <- list $ maybeToList $ consume p f
                -- return q
                where list = Pipes.Select . Pipes.each

    
--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- -- | Remove duplicate elements.  Doesn't preserve the order of the list.
-- nub :: Ord a => [a] -> [a]
-- nub = S.toList . S.fromList
-- {-# INLINE nub #-}
