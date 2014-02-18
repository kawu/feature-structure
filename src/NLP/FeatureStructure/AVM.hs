-- | An AVM language for defining feature structures.
-- TODO: Change the name?


-- An interface for defining feature structures in an AVM style.
--
-- The idea behind the algorithm is as follows.  A feature structure
-- is defined in a "tree-like style", but some of the subtrees are
-- supposed to unify (and, as a result, represent the same subgraph).
--
-- We therefore start by creating a tree (or a sequence of trees),
-- in which each node will have an identifier assigned.  In fact,
-- since a tree is a trivial graph (and since each node has an
-- identifier), we can use our feature-graph structure to represent
-- the tree.
--
-- The question is, how to trasform a tree into a graph.  But it should
-- not be a difficult task: we already know which nodes are supposed
-- to represent the same subgraphs.  Therefore, we have a list of
-- pairs, each pair consisting of two nodes which are to be joined.
-- And we can just supply the unification algorithm with this list
-- of node-pairs.
--
--
--
-- The main steps of the process:
-- * Describe a feature structure with the help of a free monad.
-- * Compile (or interpret) the free monad to a feature tree.
-- * Transform the feature tree to a feature graph.


module NLP.FeatureStructure.AVM
(
) where


import           Control.Monad.Free
import qualified NLP.FeatureStructure.Tree as T



-- To summurize: what we are really trying to do here is to use a
-- monadic interface to define a tree!
-- 
-- Lets look at the following, simplified example.
-- Basically, it consists of a sequence of three monadic instructions:
-- `verb`, `plular` and `subcat`.  While `verb` and `plural` are simple
-- monadic computations, `subcat` is a complex one and it requires an
-- argument which is a monadic computation itself!

love = do
    verb >> plural
    subcat $ do
        leaf "cat" "np"
        leaf "case" "acc"

-- In principle, we would like to abstract over the form of the individual
-- arguments of the subsequent instructions.  We don't care how the
-- subcategorization frame looks, because it doesn't really have any
-- influence on how the feature node of `love` will be constructed!
--
-- There's really nothing more to it!  Lets look at another example,
-- which should have more or less the same semantics:

love = do
    verb >> plural
    x <- do
        leaf "cat" "np"
        leaf "case" "acc"
    subcat x

-- Yet there's a significant difference here: the argument of subcat
-- is no longer a monadic computation.


--------------------------------------------------------------------
-- ???????????????
--------------------------------------------------------------------


single :: FeatMonad AVM -> 


-- | The only instruction of our language is an introduction
-- of an attribute with a corresponding value, really.
-- However, there's a trick: a value of an attribute will
-- usually be a monadic computation itself!
data AVM a next = Attr a next






-- | A feature monad can be used to define a `FS` in a monadic fashion.
-- TODO: Implement.
data FM a = undefined
instance Functor FM where
instance Monad FM where


-- | Parse the feature-structure expression.
runFM :: FM i f a b -> FS i f a
runFM = undefined


-- | An atomic value.
atom :: a -> FM i f a (FV i f a)
atom = return . Atom


-- | An atomic value.
atom :: a -> FM i f a (FV i f a)
atom = return . Atom


-- -- | An attribute.
-- attr :: f -> a -> FV i f a
-- attr = 
-- 
-- 
-- -- | An atomic value.
-- leaf :: a -> FV i f a
-- leaf = 








-- --------------------------------------------------------------------
-- -- AVM Monads
-- --------------------------------------------------------------------
-- 
-- 
-- -- | The state of the AVM monad.
-- data AVS a = AVS {
--     -- | 
-- 
-- 
-- --------------------------------------------------------------------
-- -- Core
-- --------------------------------------------------------------------
-- 
-- 
-- -- | An atomic value.
-- atom :: Eq a => a -> AVM (?)
-- atom = undefined
-- 
-- 
-- 
-- --------------------------------------------------------------------
-- -- Misc
-- --------------------------------------------------------------------
-- 
-- 
-- -- | An atomic value assigned to an feature.
-- leaf x y = attr x $ atom y
-- 
-- 
-- 
-- --------------------------------------------------------------------
-- -- Example grammar: lexicon
-- --------------------------------------------------------------------
-- 
-- 
-- sleep = do
--     verb >> plural
--     subcat nil
-- 
-- 
-- love = do
--     verb >> plural
--     subcat $ single $ do
--         leaf "cat" "np"
--         leaf "case" "acc"
-- 
-- 
-- tell = do
--     verb >> plural
--     subcat $ list
--         [ do
--             leaf "cat" "np"
--             leaf "case" "acc"
--         , leaf "cat" "s" ]
-- 
-- 
-- lamb = noun >> singular
-- lambs = noun >> plural
-- she = pron >> singular >> nominative
-- her = pron >> singular >> accusative
-- 
-- 
-- --------------------------------------------------------------------
-- -- Example grammar: rules
-- --------------------------------------------------------------------
-- 
-- 
-- -- | In case of rules the task is a bit trickier, since a rule
-- -- has to be represented by a multi-rooted structure.
-- 
-- 
-- sentR = rule $ do
--     head $ leaf "cat" "s"
--     item $ do
--         leaf "cat" "np"
--         attr "num" $ name 1 undef
--         nominative
--     item $ do
--         verb
--         attr "num" $ name 1 undef
--         subcat nul
-- 
-- 
-- consumeR = do
--     item $ do
--         verb
--         attr "num" $ name "number" undef
--         subcat $ name "rest" undef
--     item $ do
--         verb
--         attr "num" $ name "number" undef
--         subcat $ item $ do
--             attr "first" $ name "first" undef
--             attr "rest"  $ name "rest"  undef
--     item $ name "first" undef
--         
-- 
-- --------------------------------------------------------------------
-- -- Example grammar: helpers
-- --------------------------------------------------------------------
-- 
-- -- | Grammatical class.
-- verb = leaf "cat" "v"
-- 
-- 
-- -- | Number.
-- singular = leaf "num" "sg"
-- plural = leaf "num" "pl"
-- 
-- 
-- -- | Case.
-- nominative = leaf "cas" "nom"
-- accusative = leaf "cas" "acc"
-- 
-- 
-- -- | Subcategorization frame.
-- subcat = attr "subcat"
-- 
-- 
-- --------------------------------------------------------------------
-- -- Lists
-- --------------------------------------------------------------------
