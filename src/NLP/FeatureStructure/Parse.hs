--  | Unification grammar parsing.  A pleliminary effort.


module NLP.FeatureStructure.Parse
(
) where


import qualified Data.Tree as T


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


-- | A grammar rule.
data Rule i f a = Rule {
    -- | Head of the rule.
      root   :: i
    -- | Part of the rule to be parsed.
    , before :: [i]
    -- | Part of the rule already parsed.
    , after  :: T.Forest i
    -- | Graph corresponding to the rule.
    , graph  :: FG i f a
    } deriving (Show, Eq, Ord)


-- | Set of rules.
data RuleSet i f a = S.Set (Rule i f a)


-- | Move the "dot" in the partially parsed rule by unifying
-- the next `before` node with the fully parsed rule.
--
-- It is not checked, if the second argument is indeed fully
-- parsed.  On the other hand, when the first one is fully
-- parsed, the function will crash.
consume :: Rule i f a -> Rule i f a -> Maybe (Rule i f a)
consume p f =
    fst <$> J.runJoin
        (fromTwo (graph p) (graph f))
        doit
  where
    doit = do
        J.join (Left $ beforeHead p) (Right $ root f)


-- | Head of the rule-before.
beforeHead :: Rule i f a -> i
beforeHead Rule{..} = case before of
    (x:_) = return x
    []    = error "bodyHead: empty body"


--------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------


-- | A recursive definition.
parse :: Int -> Int -> RuleSet i f a
parse i j = S.fromList
    [ r
    | k <- [i .. j - 1]
    -- Partially processed rules on [i .. k]
    , p <- partial $ parse i k
    -- Fully processed rules on [k+1 .. j]
    , f <- full $ parse (k + 1) j
    -- Consume and unify
    , r <- maybeToList $ consume p f ]
