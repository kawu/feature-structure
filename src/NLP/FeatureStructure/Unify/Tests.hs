{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.Unify.Tests where


import           Control.Applicative ((<$>))
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)

import           Test.Tasty              (TestTree, testGroup)
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty.HUnit        (testCase)

-- import           NLP.FeatureStructure.Graph
-- import           NLP.FeatureStructure.Reid
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Unify as U
import qualified NLP.FeatureStructure.Join as J


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
    ]


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
test2v2 = void $ runMaybeT $ do
    (g3, r) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g1 1 g3 r @?= True
  where
    g1 = mkG [(1, mkI [('a', 1)])]
    g2 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI []) ]


test3 :: Assertion
test3 = void $ runMaybeT $ do
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
test4v2 = void $ runMaybeT $ do
    (g3, i) <- maybeT $ unify g1 1 g2 1
    lift $ G.equal g3 i r 1 @?= True
  where
    g1 = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkF 'x') ]
    g2 = mkG
        [ (1, mkI
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, mkF 'x')
        , (3, mkF 'x') ]
    r = mkG
        [ (1, mkI
            [('a', 2), ('b', 2), ('c', 2)])
        , (2, mkF 'x') ]


-- test5 :: Maybe (Graph Char Char)
-- test5 = runReid $ do
--     g1 <- (,) <$> reid 1 <*> reidGraph f1
--     split
--     g2 <- (,) <$> reid 1 <*> reidGraph f2
--     return $ unify g1 g2
--   where
--     f1 = mkGraph
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2), ('c', 3)])
--         , (2, Frontier 'x')
--         , (3, Interior $ M.fromList [('a', 4)])
--         , (4, Frontier 'x') ]
--     f2 = mkGraph
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3), ('c', 3)])
--         , (2, Interior $ M.fromList [('a', 4)])
--         , (3, Frontier 'x')
--         , (4, Frontier 'x') ]
-- 
-- 
-- test5v2 :: Maybe (Graph Char Char)
-- test5v2 = runReid $ do
--     g1 <- (,) <$> reid 1 <*> reidGraph f1
--     split
--     g2 <- (,) <$> reid 1 <*> reidGraph f2
--     return $ unify g1 g2
--   where
--     f1 = mkGraph
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2), ('c', 3)])
--         , (2, Interior M.empty)
--         , (3, Interior $ M.fromList [('a', 4)])
--         , (4, Frontier 'x') ]
--     f2 = mkGraph
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3), ('c', 3)])
--         , (2, Interior $ M.fromList [('a', 4)])
--         , (3, Interior M.empty)
--         , (4, Frontier 'x') ]
-- 
-- 
-- test6 :: Maybe (Graph Char Char)
-- test6 = runReid $ do
--     g1 <- (,) <$> reid 1 <*> reidGraph f1
--     split
--     g2 <- (,) <$> reid 1 <*> reidGraph f2
--     return $ unify g1 g2
--   where
--     f1 = mkGraph
--         [ (1, Interior $ M.fromList
--             [('a', 1), ('b', 2)])
--         , (2, Interior $ M.fromList [('a', 3)])
--         , (3, Interior M.empty) ]
--     f2 = mkGraph
--         [ (1, Interior $ M.fromList [('a', 2)])
--         , (2, Interior $ M.fromList
--             [('a', 1), ('b', 2)]) ]


------------------------------------
-- Helpers
------------------------------------


-- uni :: Int -> Graph a b -> Int -> Graph a b -> Maybe (Graph a b)
-- uni i x j y = unify (i, x) (j, y)


--------------------------------------------------------------------
-- Utility
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
