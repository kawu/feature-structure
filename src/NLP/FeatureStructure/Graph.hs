{-# LANGUAGE RecordWildCards #-}


-- | A graph-based representation of a feature structure.
--
-- At this point we assume that values are `atomic` and the only
-- operation defined over them is the equality check.


module NLP.FeatureStructure.Graph
(
-- * Basics
  FG
, Node (..)
, edgeMap

-- * Equivalence
, UM
, uniCls

-- * Tests
, test1
, test2
, test3
, test4
, test5
) where


import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad (unless, forM_)
import           Data.These

import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Partition as P
import qualified Control.Monad.State.Strict as S


-- | A node identifier.
type NodeID = Int


-- | A feature graph with edges labed by `a` values and frontier nodes
-- labeled by `b` values.  It is a map from node identifiers to actual
-- nodes.
--
-- Invariant: every node identifier points to a node which *exists*
-- in the feature graph.
type FG a b = I.IntMap (Node a b)


-- | A node in a feature graph.
data Node a b
    = Interior (M.Map a NodeID)
    | Frontier b
    deriving (Show, Eq, Ord)


-- -- | A feature graph with a distinguished root node.  Can be used to
-- -- represent a single-rooted feature graph, hence the name.
-- data SFG a = SFG
--     { fg :: FG a
--     , nd :: NodeID }


-- | List edges outgoing from a given node.  Return empty map for
-- non-existent identifiers and frontier nodes.
edgeMap :: NodeID -> FG a b -> M.Map a NodeID
edgeMap k fg = case I.lookup k fg of
    Nothing -> M.empty
    Just n  -> case n of
        Interior m  -> m
        Frontier _  -> M.empty


--------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------


-- | Equivalence computation monad.
type EqM a b = S.State (P.Partition a) b


-- | Join two elements.
join :: Ord a => a -> a -> EqM a ()
join x y = S.modify $ P.join x y


-- | Are the two elements equivalent?
equivalent :: Ord a => a -> a -> EqM a Bool
equivalent x y = do
    par <- S.get
    return $ (P.rep par x) == (P.rep par y)


-- | Element representative.
repr :: Ord a => a -> EqM a a
repr x = P.rep <$> S.get <*> pure x


-- | Unification monad.
type UM b = EqM (Either NodeID NodeID) b


--------------------------------------------------------------------
-- Equivalence relation
--------------------------------------------------------------------


-- | A graph with a selected node.
type NodeFG a b = (NodeID, FG a b)


-- | Given two feature graphs and corresponding nodes, compute
-- equivalence classes which would result from a unification
-- of the two graphs.
uniCls :: Ord a => NodeFG a b -> NodeFG a b -> UM ()
uniCls (k1, fg1) (k2, fg2) =
    -- TODO: still seems to be a bit controversial, though...
    unlessM (equivalent p q) $ do
        join p q
        forM_ common $ uncurry uniCls
  where
    p = Left k1
    q = Right k2
    common = M.elems $ M.intersectionWith mkPair
        (edgeMap k1 fg1)
        (edgeMap k2 fg2)
    mkPair x y =
        ( (x, fg1)      -- Add context to corresponding
        , (y, fg2) )    -- node identifiers.


--------------------------------------------------------------------
-- Output graph
--------------------------------------------------------------------


-- -- | Given the equivalence classes (stored in the state monad),
-- -- construct the resultant graph.
-- joinBoth :: Ord a => NodeFG a b -> NodeFG a b -> UM NodeID
-- joinBoth (k1, fg1) (k2, fg2) = do
--     k  <- addNode $ repr $ Left k1
--     ks <- forM_ children $ \ths -> case ths of
--         These x y   -> joinBoth (x, fg1) (y, fg2)
--         This x      -> joinLeft (x, fg1)
--         That y      -> joinRight         (y, fg2)
--     addEdges k ks
--     return k
--   where
--     children = interleave
--         (M.toList $ edgeMap k1 fg1)
--         (M.toList $ edgeMap k2 fg2)
-- 
-- 
-- joinLeft :: Ord a => NodeFG a b -> UM NodeID
-- joinLeft (k1, fg1) = do
--     k  <- addNode $ repr $ Left k1
--     ks <- forM_ children $ \ths -> case ths of
--         These x y   -> joinBoth (x, fg1) (y, fg2)
--         This x      -> joinLeft (x, fg1)
--         That y      -> joinRight         (y, fg2)
--     addEdges k ks
--     return k
--   where
--     children = interleave
--         (M.toList $ edgeMap k1 fg1)
--         (M.toList $ edgeMap k2 fg2)


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


test1 :: IO ()
test1 = do
    mapM_ print $ P.nontrivialSets par
  where
    par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
    f1 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2)])
        , (2, Frontier 'x') ]
    f2 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test2 :: IO ()
test2 = do
    mapM_ print $ P.nontrivialSets par
  where
    par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
    f1 = I.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = I.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Frontier 'x') ]


test3 :: IO ()
test3 = do
    mapM_ print $ P.nontrivialSets par
  where
    par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
    f1 = I.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = I.fromList
        [(1, Interior $ M.fromList [('a', 1)])]


test4 :: IO ()
test4 = do
    mapM_ print $ P.nontrivialSets par
  where
    par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
    f1 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]
    f2 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test5 :: IO ()
test5 = do
    mapM_ print $ P.nontrivialSets par
  where
    par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
    f1 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = I.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Frontier 'x')
        , (4, Frontier 'x') ]


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | A version of the `unless` function with a monadic argument.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM m n = do
    b <- m
    unless b n


-- | Interleave two lists given in an ascending order.
interleave :: Ord a => [a] -> [a] -> [These a a]
interleave xxs@(x:xs) yys@(y:ys)
    | x < y     = This x    : interleave xs yys
    | x > y     = That y    : interleave xxs ys
    | otherwise = These x y : interleave xs ys
interleave xs [] = map This xs
interleave [] ys = map That ys
