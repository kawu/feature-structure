{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A graph representation of a feature structure.


module NLP.FeatureStructure.Graph
(
-- * Basic types
  Graph (..)
, Node (..)
, empty
, mkGraph

-- * Monad
, GraphT
, GraphM
, runGraphT
, runGraphM
, getRepr
, mkReprOf
, getNode
, setNode
, remNode

-- * Utility
, fromTwo
, printGraph
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_)
import           Control.Monad.Identity (Identity)
import qualified Control.Monad.State.Strict as S

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.DisjSet as D


--------------------------------------------------------------------
-- Feature graph data structure
--------------------------------------------------------------------


-- TODO: We should try to hide some of the implemntation details
-- and not to export definitions of FG and Node (or perhaps to
-- export, but only in an `Internal` submodule).
-- All higher level operations should work on node identifiers
-- alone (i.e. they should not have access to node themselves).


-- | A feature graph with edges labeled by features of type `f`
-- and frontier nodes labeled with atomic values of type `a`.
data Graph f a = Graph {
    -- | A map from IDs to nodes.
      nodeMap   :: I.IntMap (Node f a)
    -- | A disjoint-set data structure, which keeps track of
    -- the node merging (in a way). 
    , disjSet   :: D.DisjSet ID
    } deriving (Show, Eq, Ord)


-- | A node in a feature graph.
data Node f a
    = Interior (M.Map f ID)
    | Frontier a
    deriving (Show, Eq, Ord)


-- | An empty graph.
empty :: Graph f a
empty = Graph I.empty D.empty


-- | Construct graph form an arbitrary list of nodes.
mkGraph :: [(ID, Node f a)] -> Graph f a
mkGraph xs = Graph (I.fromList xs) D.empty


-- TODO: Replaced by `Ident.ridGraph`.
-- -- | Clean the graph: replace every identifier with its representant.
-- -- As a result, the resultant graph will have an empty `disjSet`.
-- clean :: Graph f a -> Graph f a
-- clean = snd . runGraphM $ do
--     nodeMap' <- mapM cleaNodePair . I.toList
--         =<< S.gets nodeMap
--     return $ Graph nodeMap' D.empty


--------------------------------------------------------------------
-- Graph monad
--------------------------------------------------------------------


-- | A feature graph monad transformer. 
type GraphT f a = S.StateT (Graph f a)


-- | A feature graph monad (a transformer over Identify monad).
type GraphM f a = S.StateT (Graph f a) Identity


-- | Run the graph monad trasformer.
runGraphT :: GraphT f a m b -> Graph f a -> m (b, Graph f a)
runGraphT = S.runStateT


-- | Run the graph monad.
runGraphM :: GraphM f a b -> Graph f a -> (b, Graph f a)
runGraphM = S.runState


--------------------------------------------------------------------
-- Graph monad: interface
--------------------------------------------------------------------


-- | Identify the current representant of the node.
getRepr :: (Functor m, Monad m) => ID -> GraphT f a m ID
getRepr k = D.repr k <$> S.gets disjSet


-- | Set the representant of the node.
mkReprOf :: (Monad m) => ID -> ID -> GraphT f a m ()
mkReprOf x y = S.modify $ \g@Graph{..} ->
        g {disjSet = D.mkReprOf x y disjSet}


-- | Retrieve node hidden behind the given identifier.
getNode :: (Functor m, Monad m) => ID -> GraphT f a m (Maybe (Node f a))
getNode i = I.lookup <$> getRepr i <*> S.gets nodeMap


-- | Set node under the given identifier.
setNode :: Monad m => ID -> Node f a -> GraphT f a m ()
setNode i x = S.modify $ \g@Graph{..} ->
    g {nodeMap = I.insert i x nodeMap}


-- | Remove node under the given identifier.
remNode :: Monad m => ID -> GraphT f a m ()
remNode i = S.modify $ \g@Graph{..} ->
    g {nodeMap = I.delete i nodeMap}


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  We assume, that identifiers
-- in the graphs are disjoint.
fromTwo :: Graph f a -> Graph f a -> Graph f a
fromTwo f g = Graph
    { nodeMap   = I.union (nodeMap f) (nodeMap g)
    , disjSet   = D.union (disjSet f) (disjSet g) }


-- -- | Join two feature graphs.  Nodes from the first graph will be
-- -- marked as `Left`s, nodes from the second one -- as `Right`s. 
-- fromTwo :: Ord i => FG i f a -> FG i f a -> FG (Either i i) f a
-- fromTwo f g = M.fromAscList $
--     (M.toAscList $ mapIDsMono Left f) ++
--     (M.toAscList $ mapIDsMono Right g)
-- 
-- 
-- -- | Map keys of the feature graph using a strictly monotonic function.
-- mapIDsMono :: Ord j => (i -> j) -> FG i f a -> FG j f a
-- mapIDsMono f m = M.fromAscList
--     [ (f k, mapNodeIDs f v)
--     | (k, v) <- M.toAscList m ]
-- 
-- 
-- -- | Map identifiers of the node using a strictly monotonic function.
-- mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
-- mapNodeIDs f (Interior m) = Interior $ M.map f m
-- mapNodeIDs _ (Frontier x) = Frontier x
-- {-# INLINE mapNodeIDs #-}


--------------------------------------------------------------------
-- Graph printing
--------------------------------------------------------------------


-- | Print information about the graph into stdout.
printGraph :: (Show f, Show a) => Graph f a -> IO ()
printGraph Graph{..} = do
    putStrLn "# node map"
    forM_ (I.toList nodeMap) $ \(i, nd) -> case nd of
        Frontier x  -> do
            putStrLn $ "# frontier " ++ show i ++ " => " ++ show x
        Interior m  -> do
            putStrLn $ "# interior " ++ show i
            forM_ (M.toList m) print
    putStrLn "# disjoint-set"
    D.printDisjSet disjSet


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- swap :: (a, b) -> (b, a)
-- swap (x, y) = (y, x)
-- {-# INLINE swap #-}
