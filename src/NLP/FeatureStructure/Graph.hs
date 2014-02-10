{-# LANGUAGE RecordWildCards #-}


-- | A graph-based representation of a feature structure.
--
-- At this point we assume that values are `atomic` and the only
-- operation defined over them is the equality check.


module NLP.FeatureStructure.Graph
( FG
, Node (..)
) where


-- import qualified Data.UnionFind.IO as U
import qualified Data.Map as M
import qualified Data.IntMap as I
-- import qualified Data.Partition as P
-- import qualified Control.Monad.State.Strict as S


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
-- 
-- 
-- -- | Enumerate children of a given `SFG`.
-- children :: SFG a -> [SFG a]
-- children SFG{..} =
--     [ SFG fg nd'
--     | node <- maybeToList $ I.lookup nd fg ]
--     , nd' <- S.toList $ edges node ]
-- 
-- 
-- -- | Enumerate edges present in both lists.
-- common :: [SFG a] -> [SFG a]
-- 
-- 
-- -- | Equivalence computation monad.
-- type EqM a b = S.State (Partition a) b
-- 
-- 
-- -- | Join two elements.
-- join :: Ord a => a -> a -> EqM a ()
-- join p q = S.modify $ P.join p q
-- 
-- 
-- -- | A version of the `unless` function with a monadic argument.
-- unlessM :: Monad m => m Bool -> m () -> m ()
-- unlessM m n = do
--     b <- m
--     unless b n
-- 
-- 
-- -- | Given two feature graphs, compute equivalence classes which would
-- -- result from a unification of the two graphs.
-- eqCls :: SFG a -> SFG a -> MyMonad ()
-- eqCls x y = unlessM (equivalent p q) $ do
--     join p q
--     forM_ (common (children x) (children y)) (uncurry eqCls)
--   where
--     p = Left $ nd x
--     q = Right $ nd y
