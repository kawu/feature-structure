{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


import           Data.Text (Text)
import qualified Data.Text as T


import           Diagrams.Prelude (Diagram)
import           Diagrams.Backend.SVG.CmdLine


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.AVM as A
import           NLP.FeatureStructure.AVM
    (avm, list, feat, empty, label, atom)
import qualified NLP.FeatureStructure.Tree as R
import qualified NLP.FeatureStructure.Join as J
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Graph.Draw as D


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- Types with instantiated parameters.
type FT = R.FT Text Text Text
type FF = R.FF Text Text Text
type FN = R.FN Text Text Text
type AVM = A.AVM Text Text Text

-- | Locally used graph type.
-- type Graph = G.Graph Int Char Char
type Graph = G.Graph Int Text Text


--------------------------------------------------------------------
-- Lexicon
--------------------------------------------------------------------


love :: AVM
love = do
    verb >> plural
    subcat $ single $ do
        feat "cat" "np"
        feat "case" "acc"


dummy :: AVM
dummy = do
    singular
    feat "cat" $ do
        label "x"
        atom "v"
    subcat $ single $ do
        feat "cat" "?x"
        feat "case" "acc"


-- --------------------------------------------------------------------
-- -- Rules
-- --------------------------------------------------------------------
-- 
-- 
-- sentR =
--     [ avm $ do
--         leaf "cat" "s"
--     , avm $ do
--         leaf "cat" "np"
--         feat "num" $ name "?num" undef
--         nominative
--     , avm $ do
--         verb
--         feat "num" $ name "?num" undef
--         subcat [] ]
-- 
-- 
-- --------------------------------------------------------------------
-- -- Helpers
-- --------------------------------------------------------------------


-- | Grammatical class.
verb :: AVM
verb = feat "cat" "v"


-- | Number.
singular, plural :: AVM
singular = feat "num" "sg"
plural = feat "num" "pl"


-- | Case.
nominative, accusative :: AVM
nominative = feat "cas" "nom"
accusative = feat "cas" "acc"


-- | Subcategorization frame.
subcat :: [AVM] -> AVM
subcat = feat "subcat" .  list "nil" "first" "rest"


-- | A singleton forest.
single :: AVM -> [AVM]
single x = [x]
-- single :: AVM -> FF
-- single x = [avm x]


--------------------------------------------------------------------
-- Main
--------------------------------------------------------------------


-- | Convert a given AVM to a graph.
fromAVM :: AVM -> Maybe (Graph, ID)
fromAVM m = do
    (i, J.Res{..}) <- R.runCon $ R.fromFN $ avm m
    return (resGraph, convID i)


main :: IO ()
main = case fromAVM love of
    Nothing -> print "?"
    Just (g, i) -> do
        let testAvm = D.toAVM g i
        mainWith (D.drawAvmId testAvm :: Diagram B)
