{-# LANGUAGE OverloadedStrings #-}


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
-- Rules
--------------------------------------------------------------------


sentR =
    [ avm $ do
        leaf "cat" "s"
    , avm $ do
        leaf "cat" "np"
        feat "num" $ name "?num" undef
        nominative
    , avm $ do
        verb
        feat "num" $ name "?num" undef
        subcat [] ]


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


-- | A singleton forest.
single :: Avm -> FF
single x = [avm x]
