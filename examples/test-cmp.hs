{-# LANGUAGE OverloadedStrings #-}


module Test1 where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM, replicateM)
import           Data.Text (Text)
import qualified Data.Vector as V


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.AVM
    (avm, empty, atom, label, feat, tryFeat, list, (##), (#?))
import qualified NLP.FeatureStructure.AVM as A
import qualified NLP.FeatureStructure.Tree as R
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Reid as Reid


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


type AVM = A.AVM Text Text Text


test1 :: AVM
test1 = do
    "A" ## do
        "B" ## "?b"
        "C" ## "cv"
    "D" ## "?b"


test2 :: AVM
test2 = do
    "A" ## do
        "B" ## "bv"
        "C" ## "?b"
    "D" ## "?b"


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Un`Just` with the given error message.
unjust :: String -> Maybe a -> a
unjust _ (Just x) = x
unjust e Nothing  = error e


-- | Uncons the list.
unCons :: [a] -> Maybe (a, [a])
unCons (x:xs) = Just (x, xs)
unCons []     = Nothing


--------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------


-- | Make a feature graph from an AVM
mkGraph :: AVM -> (G.Graph Text Text, ID)
mkGraph =
    swap . compileEntry . avm
  where
    compileEntry fn
        = unjust ("compileEntry: " ++ show fn)
        $ R.compile fn
    swap (x, y) = (y, x)


reidGraph
    :: (Functor m, Monad m)
    => (G.Graph Text Text, ID)
    -> Reid.ReidT m (G.Graph Text Text, ID)
reidGraph (g, i) = do
    g' <- Reid.reidGraph g
    i' <- Reid.reid i
    return (g', i')


main = do
    let g1 = mkGraph test1
        g2 = mkGraph test2
    let cmp (g, i) (h, j) = G.compare' g i h j
    (h1, h2) <- Reid.runReidT $ (,)
        <$> reidGraph g1
        <*> (Reid.split >> reidGraph g2)
    let printTree = uncurry G.printTree
    printTree h1
    printTree h2
    print $ cmp h1 h2
