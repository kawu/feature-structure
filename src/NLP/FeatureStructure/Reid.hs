{-# LANGUAGE RecordWildCards #-}


-- | The `Reid` monad allows to re-assign idenifiers in a given
-- structure.  It is similiar to `Control.Monad.Atom`, but it
-- works with identifiers only and it provides a way to specify
-- split points which divide the input structure into a set
-- of ID-disjoint substructures.


module NLP.FeatureStructure.Reid
(
-- * Reid monad
  ReidT
, Reid
, runReidT
, runReid

-- * Monadic functions
, reid
, reidGraph
, split

-- * Utilities
, clean
) where


import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad.Identity (Identity(..))
import           Control.Monad ((<=<))
import qualified Control.Monad.State.Strict as S
import qualified Data.Traversable as Tr
import qualified Data.IntMap.Strict as I


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.DisjSet as D
import qualified NLP.FeatureStructure.Graph as G


--------------------------------------------------------------------
-- Core
--------------------------------------------------------------------


-- | State of the reidentification monad.
data ReidS = ReidS {
    -- | Base identifier: the maximal value (plus 1) assigned
    -- to any of the keys in the mapping.
      base  :: !ID
    -- | Current ID mapping.
    , imap  :: !(I.IntMap ID)
    } deriving (Show, Eq, Ord)


-- | The reidentification monad transformer.
type ReidT m = S.StateT ReidS m


-- | The reidentification monad.
type Reid = ReidT Identity


-- | Run the reidentification monad transformer.
runReidT :: Monad m => ReidT m a -> m a
runReidT = flip S.evalStateT $ ReidS 0 I.empty


-- | Run the reidentification monad.
runReid :: Reid a -> a
runReid = runIdentity . runReidT


-- | Set split point.
split :: Monad m => ReidT m ()
split = S.modify $ \s -> s {imap = I.empty}


-- | Re-identify a single identifier.
reid :: Monad m => ID -> ReidT m ID
reid x = S.state $ \s@ReidS{..} -> case I.lookup x imap of
    Just y  -> (y, s)
    Nothing -> ( base, ReidS
        { base = base + 1
        , imap = I.insert x base imap } )


--------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------
        

-- | Reidentify the graph.  As a by-product, the disjoint-set of the
-- resultant graph will be empty (every identifier will be  replaced
-- with its representant).
--
-- TODO: Show, that there is no reason to have a plain reidentification
-- function which would only replace identifiers and, as such, would not
-- neccessearily lead to a graph with an empty disjoint-set.
--
-- Idea: we need to clean the graph only after it has been an object
-- of the node-merging process.  In such a case, some nodes may have
-- been removed, so if we wanted to preserve any external structure of
-- node identifiers we would need to consolidate them with the merging
-- result.  But we don't want the user to have the direct access to
-- the disjoint-set, so the only sensible solution is to reidentify
-- both the graph and the external structure.
--
-- An even better idea: when we reidentify the graph, all external
-- identifiers become invalid, unless reidentified as well.  As a
-- result, there is no need to support any external identifiers
-- pointing to the redundant nodes in the disjoint-set, becase
-- either they have became invalid, or they have been reidentified.
reidGraph :: (Functor m, Monad m) => G.Graph f a -> ReidT m (G.Graph f a)
reidGraph g = fmap fst . flip G.runGraphT g $ G.Graph
    <$> reidNodeMap (G.nodeMap g)
    <*> pure D.empty
  where
    reidNodeMap m = I.fromList <$> mapM reidNodePair (I.toList m)
    reidNodePair (i, x) = (,) <$> reidRepr i <*> reidNode x
    reidNode (G.Interior m) = fmap G.Interior $ Tr.mapM reidRepr m
    reidNode (G.Frontier x) = return $ G.Frontier x
    reidRepr = S.lift . reid <=< G.getRepr


-- | Clean the graph using the `reidGraph` function.
clean :: G.Graph f a -> G.Graph f a
clean = runReid . reidGraph


-- reidGraph :: (Functor m, Monad m) => G.Graph f a -> ReidT m (G.Graph f a)
-- reidGraph G.Graph{..} = G.Graph
--     <$> reidNodeMap nodeMap
--     <*> reidDisjSet disjSet
--   where
--     reidNodeMap m = I.fromList <$> mapM reidNodePair (I.toList m)
--     reidDisjSet d = D.fromList <$> mapM reidPair (D.toList d)
--     reidNodePair (i, x) = (,) <$> reid i <*> reidNode x
--     reidNode (G.Interior m) = fmap G.Interior $ Tr.mapM reid m
--     reidNode (G.Frontier x) = return $ G.Frontier x
--     reidPair (x, y) = (,) <$> reid x <*> reid y


-- --------------------------------------------------------------------
-- -- Helpers
-- --------------------------------------------------------------------
-- 
-- 
-- -- | `reid` the `fst` element of the pair.
-- reidFst :: (Functor m, Monad m) => (ID, a) -> ReidT m (ID, a)
-- reidFst (x, y) = (,) <$> reid x <*> pure y
