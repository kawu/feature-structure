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
) where


import           Control.Applicative (pure, (<$>), (<*>), (*>))
import           Control.Monad (forM, forM_, guard, mzero)
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe (maybeToList)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Traversable as Tr
-- import qualified Data.MemoCombinators as Memo
import           Control.Monad.Memo (memo, for2)
import qualified Control.Monad.Memo as Memo
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.Join as J
import           NLP.FeatureStructure.Reid (Reid)
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


-- | Beware!  An orphan instance...
instance Ord a => Ord (T.Tree a)


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
    -- Given in a reverse direction.
    , left  :: T.Forest ID
    -- | Graph corresponding to the rule.
    , graph  :: Graph f a
    } deriving (Show, Eq, Ord)


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
    => [Rule f a]       -- ^ Grammar rules (to be reid)
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

        -- The final result
        -- t = Memo.memo2 Memo.integral Memo.integral u -- t'
        -- t' i j = u (rules M.! (i, j)) i j
        t i j = do
            rs <- lift $ mapM ruleInst rules
            u rs i j
    
        -- Introduce new grammar rules
        -- TODO: u[k+2] should not depend on u[k] and lower?
        -- TODO: we should first compute u [] i j, then compute
        -- (using pure code) other u's.
        u (r:rs) i j = do
            u1 <- u rs i j
            u2 <- return
                [ q | f <- u1, isFull f
                , q <- maybeToList (consume r f) ]
            return $ u1 ++ u2
    
        -- Move ,,dot'' in beforehand introduced rules
        u [] i j
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
              where
                list = Pipes.Select . Pipes.each

    
--     -- Reidentify rules and sentence
--     (rules, sent) = Reid.runReid $ do
--         -- Rules
--         let ixs =
--                 [ (i, j)
--                 | i <- [0 .. length sent0]
--                 , j <- [i .. length sent0] ]
--         rs' <- forM ixs $ \ij -> do
--             rs <- forM rules0 $ \r -> do
--                 Reid.split >> reidRule r
--             return (ij, rs)
--         -- Words
--         ws' <- forM sent0 $ \x -> do
--             Reid.split
--             -- y <- Reid.reidRule $ compileEntry x
--             y <- reidRule x
--             return [y]
--         return (M.fromList rs', V.fromList ws')

--     -- Compile entry
--     compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
--         (i, g) <- R.compile fn
--         return $ mkEntry i g


-- -- | A recursive definition.
-- parse :: (Ord a, Ord f) => Sent f a -> Int -> Int -> RuleSet f a
-- parse sent = 
--     doit
--   where
--     doit = Memo.memo2 Memo.integral Memo.integral doit'
--     doit' i j
--         | i == j    = sent V.! i
--         | otherwise = S.fromList
--             [ r
--             | k <- [i .. j - 1]
--             -- Partially processed rules on [i .. k]
--             , p <- partial $ doit i k
--             -- Fully processed rules on [k+1 .. j]
--             , f <- full $ doit (k + 1) j
--             -- Consume and unify
--             , r <- maybeToList $ consume p f ]
--     full = filter isFull . S.toList
--     partial = filter (not . isFull) . S.toList


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- -- | Remove duplicate elements.  Doesn't preserve the order of the list.
-- nub :: Ord a => [a] -> [a]
-- nub = S.toList . S.fromList
-- {-# INLINE nub #-}
