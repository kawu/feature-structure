{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.Unify.Tests where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe (isJust)
import           Data.List (sort)

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.QuickCheck   (testProperty)
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty.HUnit        (testCase)

-- import           NLP.FeatureStructure.Graph
-- import           NLP.FeatureStructure.Reid
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Unify as U
import qualified NLP.FeatureStructure.Join as J

import qualified NLP.FeatureStructure.Graph.Tests as GT


--------------------------------------------------------------------
-- Test tree
--------------------------------------------------------------------


-- | The actual test set.
tests :: TestTree
tests = testGroup "NLP.FeatureStructure.Unify"
    [ testCase "test1" test1
    , testCase "test2" test2
    , testCase "test2v2" test2v2
    , testCase "test3" test3
    , testCase "test4" test4
    , testCase "test4v2" test4v2
    , testCase "test5" test5
    , testCase "test5v2" test5v2
    , testCase "test6" test6
    , testProperty "valid unification converter" checkConv
    , testProperty "no frontier duplicates after unification" checkNoFrontDups
    , testProperty "unification symmetric" checkSymmetric
    ]


--------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------


-- | Verify that the result of unification of two graphs contains
-- the converter which provides valid IDs for all IDs of the
-- input graphs.
checkConv :: GT.SGraphID -> GT.SGraphID -> Bool
checkConv s1 s2 = case U.unify g1 g2 [(i1, i2)] of
    Nothing -> True
    Just J.Res{..} ->
        check g1 (convID . Left)  resGraph &&
        check g2 (convID . Right) resGraph
  where
    (g1, i1) = GT.toGraphID s1
    (g2, i2) = GT.toGraphID s2
    check g f g' = and
        [ isJust $ G.getNode (f i) g'
        | i <- S.toList $ G.getIDs g ]


-- | Verify that unification result doesn't contain frontier
-- duplicates.
checkNoFrontDups :: GT.SGraphID -> GT.SGraphID -> Bool
checkNoFrontDups s1 s2 = case U.unify g1 g2 [(i1, i2)] of
    Nothing -> True
    Just J.Res{..} -> noDups $ atoms resGraph
  where
    (g1, i1) = GT.toGraphID s1
    (g2, i2) = GT.toGraphID s2
    -- check that there are no duplicate in the input list
    noDups xs = sort xs == S.toList (S.fromList xs)
    -- retrieve the list of frontier, atomic values
    atoms G.Graph{..} =
        [x | G.Frontier x <- M.elems nodeMap]


-- | Check that unification is symmetric.
checkSymmetric :: GT.SGraphID -> GT.SGraphID -> Bool
checkSymmetric s1 s2 =
    unify g1 g2 `egal` unify g2 g1
  where
    g1 = GT.toGraphID s1
    g2 = GT.toGraphID s2
    egal Nothing Nothing    = True
    egal (Just x) (Just y)  = equal x y
    egal _ _                = False
    unify (g, i) (h, j) = case U.unify g h [(i, j)] of
        Nothing -> Nothing
        Just J.Res{..} -> Just
            (resGraph, convID $ Left i)
    equal (g, i) (h, j) = G.equal g i h j


--------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------


test1 :: Assertion
test1 = do
    let b = isJust $ unify g1 1 g2 1
    b @?= False
  where
    g1 = mkG
        [ (1, mkI [('a', 2), ('b', 2)])
        , (2, mkF 'x') ]
    g2 = mkG
        [ (1, mkI [('a', 2), ('b', 3)])
        , (2, mkF 'x')
        , (3, mkF 'y') ]


test2 :: Assertion
test2 = do
    let b = isJust $ unify g1 1 g2 1
    b @?= False
  where
    g1 = mkG [(1, mkI [('a', 1)])]
    g2 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkF 'x') ]


test2v2 :: Assertion
test2v2 = onJust $ do
    (g3, r) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g1 1 g3 r @?= True
  where
    g1 = mkG [(1, mkI [('a', 1)])]
    g2 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI []) ]


test3 :: Assertion
test3 = onJust $ do
    (g3, i) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g1 1 g3 i @?= True
  where
    g1 = mkG [(1, mkI [('a', 1)])]
    g2 = mkG [(1, mkI [('a', 1)])]


test4 :: Assertion
test4 = do
    let b = isJust $ unify g1 1 g2 1
    b @?= False
  where
    g1 = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkF 'y') ]
    g2 = mkG
        [ (1, mkI
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkF 'y') ]


test4v2 :: Assertion
test4v2 = onJust $ do
    (g3, i) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g3 i r 1 @?= True
  where
    g1 = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkI []) ]
    g2 = mkG
        [ (1, mkI
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkI []) ]
    r = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 2)])
        , (2, mkF 'x') ]


test5 :: Assertion
test5 = do
    let b = isJust $ unify g1 1 g2 1
    b @?= False
  where
    g1 = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkI [('a', 2)]) ]
    g2 = mkG
        [ (1, mkI
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, mkI [('a', 3)])
        , (3, mkF 'x') ]


test5v2 :: Assertion
test5v2 = onJust $ do
    (g3, i) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g3 i r 1 @?= True
  where
    g1 = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, mkI [])
        , (3, mkI [('a', 4)])
        , (4, mkF 'x') ]
    g2 = mkG
        [ (1, mkI
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, mkI [('a', 4)])
        , (3, mkI [])
        , (4, mkF 'x') ]
    r = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 2)])
        , (2, mkI [('a', 3)])
        , (3, mkF 'x') ]


test6 :: Assertion
test6 = onJust $ do
    (g3, i) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g3 i r 1 @?= True
  where
    g1 = mkG
        [ (1, mkI
            [('a', 1), ('b', 2)])
        , (2, mkI [('a', 3)])
        , (3, mkI []) ]
    g2 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI
            [('a', 1), ('b', 2)]) ]
    r = mkG
        [ (1, mkI [('a', 1), ('b', 1)]) ]


--------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------


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
