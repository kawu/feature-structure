{-# LANGUAGE RecordWildCards #-}


--  | Unification grammar parsing.  A pleliminary effort.


module NLP.FeatureStructure.Parse
( Rule (..)
, Sent
, Token
, parse
) where


import           Data.Maybe (maybeToList)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.MemoCombinators as Memo


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.Join as J


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


-- | Be carefull!  An orphan instance...
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


-- | Is it a fully processed rule?
isFull :: Rule f a -> Bool
isFull Rule{..} = null right


-- | A set of rules.
type RuleSet f a = S.Set (Rule f a)


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
    (x, p') <- shift p
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


-- | Head of the rule-before.
-- rightHead :: Rule f a -> ID
-- rightHead Rule{..} = case right of
--     (x:_) = return x
--     []    = error "rightHead: empty body"


-- | Shift the ,,dot'' of the partially parsed rule.
-- Return the shifted node and the shifted rule.
shift :: Rule f a -> Maybe (ID, Rule f a)
shift r@Rule{..} = case right of
    (x:xs) -> Just (x, r
        { left  = mkNode x : left   -- reverse!
        , right = xs } )
    [] -> Nothing
  where
    mkNode x = T.Node x []


--------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------


-- | A sentence to be parsed
type Sent f a = V.Vector (Token f a)

-- A token is a set of lexicon entries (fully processed rules)
-- identified for a particular input word.
type Token f a = S.Set (Rule f a)


-- | A recursive definition.
parse :: (Ord a, Ord f) => Sent f a -> Int -> Int -> RuleSet f a
parse sent = 
    doit
  where
    doit = Memo.memo2 Memo.integral Memo.integral doit'
    doit' i j
        | i == j    = sent V.! i
        | otherwise = S.fromList
            [ r
            | k <- [i .. j - 1]
            -- Partially processed rules on [i .. k]
            , p <- partial $ doit i k
            -- Fully processed rules on [k+1 .. j]
            , f <- full $ doit (k + 1) j
            -- Consume and unify
            , r <- maybeToList $ consume p f ]
    full = filter isFull . S.toList
    partial = filter (not . isFull) . S.toList
