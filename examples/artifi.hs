{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative ((<$>))
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T


import           NLP.FeatureStructure.Tree
    (avm, leaf, atom, feat, name, undef)
import qualified NLP.FeatureStructure.Tree as R
import           NLP.FeatureStructure.Graph (printFG)
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Unify as U


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- Types with instantiated parameters.
type FG = G.FG R.ID Text Text
type FT = R.FT Text Text Text
type FF = R.FF Text Text Text
type FN = R.FN Text Text Text
type AV = R.AV Text Text Text
type Avm = R.Avm Text Text Text


--------------------------------------------------------------------
-- Trees
--------------------------------------------------------------------


f1 :: FN
f1 = avm $ do
    leaf "a" "x"
    feat "b" $ name "?b" undef
    feat "c" $ avm $ do
        feat "a" undef
        feat "b" $ name "?b" undef


f2 :: FN
f2 = avm $ do
    feat "a" $ name "?a" undef
    feat "b" $ list
        [ avm $ do
            feat "a" $ name "?a" undef ]
                

--------------------------------------------------------------------
-- Graphs
--------------------------------------------------------------------


compile :: FN -> IO (R.ID, FG)
compile = fmap fromJust . R.compile


unifyThem :: IO ()
unifyThem = do
    g1 <- compile f1
    g2 <- compile f2
    case U.unify g1 g2 of
        Nothing -> putStrLn "don't unify"
        Just (x, g) -> print x >> printFG g


--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------


-- | Subcategorization frame.
list :: FF -> FN
list = avm . R.list "first" "rest"
