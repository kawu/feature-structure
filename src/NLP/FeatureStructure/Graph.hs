{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A graph representation of a feature structure.


module NLP.FeatureStructure.Graph
(
-- * Basic types
  FG
, Node (..)
, NodeFG

-- * Utility
, fromTwo
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>))
import           Control.Monad (forM)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Strict as S
import qualified Control.Error as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Traversable as Tr
import qualified Pipes as P

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))
import qualified Control.Monad.Atom as Atom


import qualified NLP.FeatureStructure.DisjSet as D


-- --------------------------------------------------------------------
-- -- Logging
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Log a value.
-- log :: Monad m => String -> P.Producer' String m ()
-- log = P.yield
-- 
-- 
-- -- | Run logger in the IO-based monad stack.
-- logLoud :: MonadIO m => P.Producer String m r -> m r
-- logLoud p = P.runEffect $ P.for p (liftIO . putStrLn)
-- 
-- 
-- -- | Supress logging messages. 
-- logSilent :: Monad m => P.Producer a m r -> m r
-- logSilent p = P.runEffect $ P.for p P.discard
-- 
-- 
-- -- purely :: P.Producer String Identity () -> [String]
-- -- purely = Pipes.Prelude.toList


--------------------------------------------------------------------
-- Feature graph
--------------------------------------------------------------------


-- | A feature graph with node identifiers of type `i`, edges labeled
-- by features of type `f`, and frontier nodes labeled with atomic
-- values of type `a`.
--
-- Invariant: every node identifier points to a node which *exists*
-- in the feature graph.
type FG i f a = M.Map i (Node i f a)


-- | A node in a feature graph.
data Node i f a
    = Interior (M.Map f i)
    | Frontier a
    deriving (Show, Eq, Ord)


-- | A graph with a selected node.
type NodeFG i f a = (i, FG i f a)


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  Nodes from the first graph will be
-- marked as `Left`s, nodes from the second one -- as `Right`s. 
fromTwo :: Ord i => FG i f a -> FG i f a -> FG (Either i i) f a
fromTwo f g = M.fromAscList $
    (M.toAscList $ mapIDsMono Left f) ++
    (M.toAscList $ mapIDsMono Right g)


-- | Map keys of the feature graph using a strictly monotonic function.
mapIDsMono :: Ord j => (i -> j) -> FG i f a -> FG j f a
mapIDsMono f m = M.fromAscList
    [ (f k, mapNodeIDs f v)
    | (k, v) <- M.toAscList m ]


-- | Map identifiers of the node using a strictly monotonic function.
mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
mapNodeIDs f (Interior m) = Interior $ M.map f m
mapNodeIDs _ (Frontier x) = Frontier x
{-# INLINE mapNodeIDs #-}


-- --------------------------------------------------------------------
-- -- Unification monad
-- --------------------------------------------------------------------
-- 
-- 
-- -- | The state of the unification monad.
-- data UMS i f a = UMS {
--     -- | A sequence of node pairs.
--       umSq  :: Seq (i, i)
--     -- | A feature graph.
--     , umFg  :: FG i f a
--     -- | A disjoint-set covering information about representants.
--     , umDs  :: D.DisjSet i }
-- 
-- 
-- -- | The unification monad transformer.
-- type UMT i f a m b =
--     P.Producer String
--     (MaybeT
--     (S.StateT (UMS i f a) m)) b
-- 
-- 
-- -- -- | The unification monad.
-- -- type UM i f a b = UMT i f a Identity b
-- 
-- 
-- --------------------------------------------------------------------
-- -- Unification class
-- --------------------------------------------------------------------
-- 
-- 
-- class (Show i, Ord i, Show a, Eq a, Show f, Ord f)
--     => Uni i f a where
-- instance (Show i, Ord i, Show a, Eq a, Show f, Ord f)
--     => Uni i f a where
-- 
-- 
-- --------------------------------------------------------------------
-- -- Unification monad: core
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Pop the node pair from the queue.  TODO: By the way, does
-- -- it have to be a queue for the algorithm to be correct?
-- pop :: (Uni i f a, Monad m) => UMT i f a m (Maybe (i, i))
-- pop = do
--     mx <- S.state $ \ums@UMS{..} -> case Seq.viewl umSq of
--         EmptyL  -> (Nothing, ums)
--         x :< sq -> (Just x, ums {umSq = sq})
--     log $ "[pop] " ++ show mx
--     return mx
-- 
-- 
-- -- | Push the node pair into the queue.
-- push :: (Uni i f a, Monad m) => (i, i) -> UMT i f a m ()
-- push x = do
--     log $ "[push] " ++ show x
--     S.modify $ \ums@UMS{..} -> ums {umSq = umSq |> x}
-- 
-- 
-- -- | Return the representant of the given node.
-- -- TODO: It doesn't fail with the `Other`, perhaps we should change that?
-- repr :: (Uni i f a, Monad m) => i -> UMT i f a m i
-- repr x = do
--     y <- D.repr x <$> S.gets umDs
--     log $ "[repr] " ++ show x ++ " -> " ++ show y
--     return y
-- 
-- 
-- -- | Set the representant of the node.
-- mkReprOf :: (Uni i f a, Monad m) => i -> i -> UMT i f a m ()
-- mkReprOf x y = do
--     log $ "[mkReprOf] " ++ show y ++ " -> " ++ show x
--     S.modify $ \ums@UMS{..} ->
--         ums {umDs = D.mkReprOf x y umDs}
-- 
-- 
-- -- | Unification failure.
-- uniFail :: Monad m => UMT i f a m b
-- uniFail = P.lift E.nothing
-- 
-- 
-- -- | Node behind the identifier.
-- node :: (Uni i f a, Monad m) => i -> UMT i f a m (Node i f a)
-- node x = do
--     fg <- S.gets umFg
--     case M.lookup x fg of
--         Just y  -> return y
--         Nothing -> do
--             log $ "ERROR: node " ++ show x ++ " doesn't exist!"
--             uniFail
-- 
-- 
-- -- | Set node under the given identifier.
-- setNode :: (Uni i f a, Monad m) => i -> Node i f a -> UMT i f a m ()
-- setNode i x = do
--     log $ "[setNode] " ++ show i ++ " -> " ++ show x
--     S.modify $ \ums@UMS{..} ->
--         ums {umFg = M.insert i x umFg}
-- 
-- 
-- -- | Remove node under the given identifier.
-- remNode :: (Uni i f a, Monad m) => i -> UMT i f a m ()
-- remNode i = do
--     log $ "[remNode] " ++ show i
--     S.modify $ \ums@UMS{..} ->
--         ums {umFg = M.delete i umFg}
-- 
-- 
-- --------------------------------------------------------------------
-- -- Unification monad: advanced
-- --------------------------------------------------------------------
-- 
-- 
-- -- -- | Pop next pair of distinct nodes from the queue.
-- -- popNext :: (Ord i, Show i, Monad m) => UMT i f a m (Maybe (i, i))
-- -- popNext = whileM (fmap not . isEmptyQ) tryPop
-- 
-- 
-- -- | Top of the queue, failure.
-- data Fail
--     = Equal -- ^ Top values are not distinct
--     | Empty -- ^ Queue is empty
-- 
-- 
-- -- | Pop next pair of nodes from the queue.  If the nodes are
-- -- not distinct, return `Nothing`.
-- popNext :: (Uni i f a, Monad m) => UMT i f a m (Either Fail (i, i))
-- popNext = E.runEitherT $ do
--     (i0, j0) <- E.tryJust Empty =<< S.lift pop
--     i <- S.lift $ repr i0
--     j <- S.lift $ repr j0
--     E.tryAssert Equal $ i /= j
--     return (i, j)
-- 
-- 
-- -- | Run the second monad on the elements acquired from the first monad
-- -- as long as the first monad doesn't return the `Empty` element.
-- whileNotEmpty :: Monad m => (m (Either Fail a)) -> (a -> m ()) -> m ()
-- whileNotEmpty cond m = do
--     mx <- cond
--     case mx of
--         Right x     -> m x >> whileNotEmpty cond m
--         Left Equal  -> whileNotEmpty cond m
--         Left Empty  -> return ()


