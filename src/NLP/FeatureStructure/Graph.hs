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
) where


import           Control.Monad (unless, forM_)

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
-- Equivalence relation
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


-- | Equivalence monad for unification.
type UM b = EqM (Either NodeID NodeID) b


-- | Given two feature graphs and corresponding nodes, compute
-- equivalence classes which would result from a unification
-- of the two graphs.
uniCls :: Ord a => (NodeID, FG a b) -> (NodeID, FG a b) -> UM ()
uniCls (k1, fg1) (k2, fg2) =
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
-- Misc
--------------------------------------------------------------------


-- | A version of the `unless` function with a monadic argument.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM m n = do
    b <- m
    unless b n


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
