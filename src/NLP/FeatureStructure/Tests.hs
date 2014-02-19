-- | A few unification tests.


module NLP.FeatureStructure.Tests
( test1
, test2
, test2v2
, test3
, test4
, test4v2
, test5
, test5v2
, test6
, test6v2
) where


import qualified Data.Map.Strict as M


import           NLP.FeatureStructure.Graph
import           NLP.FeatureStructure.Unify


test1 :: IO ()
test1 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2)])
        , (2, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test2 :: IO ()
test2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Frontier 'x') ]


test2v2 :: IO ()
test2v2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior M.empty) ]


test3 :: IO ()
test3 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]


test4 :: IO ()
test4 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test4v2 :: IO ()
test4v2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]


test5 :: IO ()
test5 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Frontier 'x')
        , (4, Frontier 'x') ]


test5v2 :: IO ()
test5v2 = do
    print =<< unifyIO (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Interior M.empty)
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Interior M.empty)
        , (4, Frontier 'x') ]


test6 :: IO ()
test6 = do
    print =<< unifyIO (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 1), ('b', 2)])
        , (2, Interior $ M.fromList [('a', 3)])
        , (3, Interior M.empty) ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior $ M.fromList
            [('a', 1), ('b', 2)]) ]


test6v2 :: IO ()
test6v2 = do
    print $ unify (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 1), ('b', 2)])
        , (2, Interior $ M.fromList [('a', 3)])
        , (3, Interior M.empty) ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior $ M.fromList
            [('a', 1), ('b', 2)]) ]
