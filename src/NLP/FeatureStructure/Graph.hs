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

-- * Monad
, GraphM

-- * Utility
-- , fromTwo
-- , printFG
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM, forM_)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Strict as S
import qualified Control.Error as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Traversable as Tr
import qualified Pipes as P

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))
import qualified Control.Monad.Atom as Atom


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


--------------------------------------------------------------------
-- Graph monad
--------------------------------------------------------------------


-- | A feature graph monad transformer. 
type GraphM f a m b = S.StateT (Graph f a) m b


--------------------------------------------------------------------
-- Graph monad: low-level interface
--------------------------------------------------------------------


-- | Identify the current representant of the node.
repr :: (Functor m, Monad m) => ID -> GraphM f a m ID
repr k = D.repr k <$> S.gets disjSet


--------------------------------------------------------------------
-- Graph monad: high-level interface
--------------------------------------------------------------------


-- | Retrieve node hidden behind the given identifier.
node :: (Functor m, Monad m) => ID -> GraphM f a m (Maybe (Node f a))
node i = I.lookup <$> repr i <*> S.gets nodeMap


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- -- | Join two feature graphs.  We assume, that identifiers
-- -- in the graphs are disjoint.
-- fromTwo :: Ord i => FG i f a -> FG i f a -> FG (Either i i) f a
-- fromTwo f g = FG
--     { nodeMap   = I.union (nodeMap f) (nodeMap g)
--     , disjSet   = D.union (disjSet f) (disjSet g) }


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


-- --------------------------------------------------------------------
-- -- Graph printing
-- --------------------------------------------------------------------
-- 
-- 
-- printFG :: (Show i, Show f, Show a) => FG i f a -> IO ()
-- printFG g = forM_ (M.toList g) $ \(i, nd) -> case nd of
--     Frontier x  -> do
--         putStrLn $ "# frontier " ++ show i ++ " => " ++ show x
--     Interior m  -> do
--         putStrLn $ "# interior " ++ show i
--         forM_ (M.toList m) print
