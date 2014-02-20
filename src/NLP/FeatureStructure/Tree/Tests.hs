{-# LANGUAGE OverloadedStrings #-}


-- | A few tree construction tests.


module NLP.FeatureStructure.Tree.Tests
( love
, dummy
) where


import           Data.Text (Text)
import qualified Data.Text as T


import           NLP.FeatureStructure.Tree
    (avm, leaf, atom, feat, name, undef, list)
import qualified NLP.FeatureStructure.Tree as R


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- Types with instantiated parameters.
type FT = R.FT Text Text Text
type FF = R.FF Text Text Text
type FN = R.FN Text Text Text
type AV = R.AV Text Text Text
type Avm = R.Avm Text Text Text


--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------


-- | Grammatical class.
verb :: Avm
verb = leaf "cat" "v"


-- | Number.
singular, plural :: Avm
singular = leaf "num" "sg"
plural = leaf "num" "pl"


-- | Case.
nominative, accusative :: Avm
nominative = leaf "cas" "nom"
accusative = leaf "cas" "acc"


-- | Subcategorization frame.
subcat :: FF -> Avm
subcat = feat "subcat" . avm . list "first" "rest"


-- At this point it is worth to think about how naming within
-- the context of subcategorization frames might work.  And
-- in a broader context too.
--
-- First of all, let think about which structures can have
-- (or should) a named assigned.
-- * FV: yes, for sure.  The main argument: otherwise, there
--   would be no way to assign a name to an `Atom`, and therefore
--   no way to specify that two frontier nodes must be equal.
--
-- OK, maybe attack this problem from another end.  We need
-- to be able to assign an identifier to every node in the graph.
-- BTW, we should be able to specify a graph which consists of
-- a single frontier node: therefore, it is `FV` which should
-- constitute a root of our structure, and not `FT`!
--
-- We should be able to assign identifiers to individual elements
-- of a feature forest as well.  Which supports the thesis
-- presented above.
--
-- What about the entire feature forest?  Let us consider two cases:
-- * FF can be used to represent a rule.  There's no point of
--   assigning an identifier to a rule.
-- * FF is used to represent a subcategorization frame.  But is
--   it, really?  Nope, it's just a syntax sugar, the list is
--   subsequently trasformed to a regular feature tree.
--   And, anyway, I don't really see a point of assigning
--   an identifier to an entire subcategorization frame.


-- | A singleton forest.
single :: Avm -> FF
single x = [avm x]


--------------------------------------------------------------------
-- Lexicon
--------------------------------------------------------------------


love :: FN
love = avm $ do
    verb >> plural
    subcat $ single $ do
        leaf "cat" "np"
        leaf "case" "acc"


dummy :: FN
dummy = avm $ do
    singular
    feat "cat" $ name "x" $ atom "v"
    subcat $ single $ do
        feat "cat" $ name "x" undef
        leaf "case" "acc"


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


-- test1 :: IO ()
-- test1 = do
--     print =<< unifyIO (1 :: Int, f1) (1, f2)
--   where
--     f1 = M.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2)])
--         , (2, Frontier 'x') ]
--     f2 = M.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3)])
--         , (2, Frontier 'x')
--         , (3, Frontier 'y') ]
