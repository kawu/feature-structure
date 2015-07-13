{-# LANGUAGE RecordWildCards #-}


-- | Testing AVM->Tree and Tree->Graph compilation.


module NLP.FeatureStructure.AVM.Tests where


import           Control.Applicative ((<$>))
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe (isJust)

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.QuickCheck   (testProperty)
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty.HUnit        (testCase)

import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.AVM as A
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Tree as T
import qualified NLP.FeatureStructure.Unify as U
import qualified NLP.FeatureStructure.Join as J

-- import qualified NLP.FeatureStructure.Graph.Tests as GT


--------------------------------------------------------------------
-- Test tree
--------------------------------------------------------------------


-- | The actual test set.
tests :: TestTree
tests = testGroup "NLP.FeatureStructure.AVM"
    [ testCase "testEmpty" testEmpty
    , testCase "testCycle" testCycle
    , testCase "testOther" testOther
    ]


--------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------


-- | Test if empty AVM is equal to (almost) empty graph.
testEmpty :: Assertion
testEmpty = onJust $ do
    (g, i) <- maybeT $ fromAVM A.empty
    lift $ G.equal g i h 1 @?= True
  where
    h = mkG [(1, mkI [])]


-- | Test a simple cyclic graph.
testCycle :: Assertion
testCycle = onJust $ do
    (g, i) <- maybeT $ fromAVM a
    lift $ G.equal g i h 1 @?= True
  where
    a = do
        A.label 1
        A.feat 'a' $ A.label 1
    h = mkG [(1, mkI [('a', 1)])]


-- | Test a slightly more complex example.
testOther :: Assertion
testOther = onJust $ do
    (g, i) <- maybeT $ fromAVM a
    lift $ G.equal g i h 1 @?= True
  where
    a = do
        A.label 1
        A.feat 'a' $ do
            A.label 2
            A.feat 'a' $ A.label 1
            A.feat 'b' $ A.label 2
    h = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI
            [('a', 1), ('b', 2)]) ]


--------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------


-- | Locally used AVM type.
type AVM = A.AVM Int Char Char


-- | Locally used graph type.
type Graph = G.Graph Int Char Char


-- | Locally used node type.
type Node = G.Node Int Char Char


mkG :: [(Int, Node)] -> Graph
mkG = G.Graph . M.fromList


mkI :: [(Char, Int)] -> Node
mkI = G.Interior . M.fromList


mkF :: Char -> Node
mkF = G.Frontier


unify :: Graph -> Int -> Graph -> Int -> Maybe (Graph, Int)
unify g i h j = case U.unify g h [(i, j)] of
    Nothing -> Nothing
    Just J.Res{..} -> Just (resGraph, convID $ Left i)


maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return


-- | Assertion from a MaybeT computation which fails if the
-- MaybeT transformer returns Nothing.
onJust :: MaybeT IO a -> Assertion
onJust m = do
    x <- runMaybeT m
    case x of
        Nothing -> fail "MaybeT returned Nothing"
        Just _  -> return ()


-- | Convert a given AVM to a graph.
fromAVM :: AVM -> Maybe (Graph, ID)
fromAVM m = do
    (i, J.Res{..}) <- T.runCon $ T.fromFN $ A.avm m
    return (resGraph, convID i)
