-- | A few unification tests.


module NLP.FeatureStructure.Unify.Tests
( test1
, test2
, test2v2
, test3
, test4
, test4v2
, test5
, test5v2
, test6
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M


import           NLP.FeatureStructure.Graph
import           NLP.FeatureStructure.Reid
import           NLP.FeatureStructure.Unify


test1 :: Maybe (Graph Char Char)
test1 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2)])
        , (2, Frontier 'x') ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test2 :: Maybe (Graph Char Char)
test2 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = mkGraph
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Frontier 'x') ]


test2v2 :: Maybe (Graph Char ())
test2v2 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = mkGraph
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior M.empty) ]


test3 :: Maybe (Graph Char ())
test3 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = mkGraph
        [(1, Interior $ M.fromList [('a', 1)])]


test4 :: Maybe (Graph Char Char)
test4 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test4v2 :: IO ()
test4v2 = runReidT $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    liftIO $ do
        putStrLn "f1: "
        printGraph $ snd g1
        putStrLn "f2: "
        printGraph $ snd g2
        case unify g1 g2 of
            Nothing -> putStrLn "unification failed"
            Just g3 -> do
                putStrLn "unification result:"
                printGraph $ clean g3
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]


test5 :: Maybe (Graph Char Char)
test5 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Frontier 'x')
        , (4, Frontier 'x') ]


test5v2 :: Maybe (Graph Char Char)
test5v2 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Interior M.empty)
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Interior M.empty)
        , (4, Frontier 'x') ]


test6 :: Maybe (Graph Char Char)
test6 = runReid $ do
    g1 <- (,) <$> reid 1 <*> reidGraph f1
    split
    g2 <- (,) <$> reid 1 <*> reidGraph f2
    return $ unify g1 g2
  where
    f1 = mkGraph
        [ (1, Interior $ M.fromList
            [('a', 1), ('b', 2)])
        , (2, Interior $ M.fromList [('a', 3)])
        , (3, Interior M.empty) ]
    f2 = mkGraph
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior $ M.fromList
            [('a', 1), ('b', 2)]) ]


------------------------------------
-- Helpers
------------------------------------


-- uni :: Int -> Graph a b -> Int -> Graph a b -> Maybe (Graph a b)
-- uni i x j y = unify (i, x) (j, y)
