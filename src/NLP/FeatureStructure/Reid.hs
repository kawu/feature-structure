{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.Reid
(
-- * Pure operations
  reidGraph

-- * Reid monad
, ReidT
, Reid
, runReidT
, runReid

-- * Primitives
, reid
, reidGraphM
, split
) where


import           Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Identity (Identity(..))

import qualified Data.Traversable as Tr
import qualified Data.Map.Strict as M


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.Join as J
import qualified NLP.FeatureStructure.Graph as G


-- | State of the reidentification monad.
data ReidS i = ReidS {
    -- | Base identifier: the maximal value (plus 1) assigned
    -- to any of the keys in the mapping.
      base  :: !ID
    -- | Current ID mapping.
    , imap  :: !(M.Map i ID)
    } deriving (Show, Eq, Ord)


-- | The reidentification monad transformer.
type ReidT i m = S.StateT (ReidS i) m


-- | The reidentification monad.
type Reid i = ReidT i Identity


-- | Run the reidentification monad transformer.
runReidT :: Monad m => ReidT i m a -> m (a, ReidS i)
runReidT = flip S.runStateT $ ReidS 0 M.empty


-- | Run the reidentification monad.
runReid :: Reid i a -> (a, ReidS i)
runReid = runIdentity . runReidT


-- | Set split point.
split :: Monad m => ReidT i m ()
split = S.modify $ \s -> s {imap = M.empty}


-- | Re-identify a single identifier.
reid :: (Monad m, Ord i) => i -> ReidT i m ID
reid x = S.state $ \s@ReidS{..} -> case M.lookup x imap of
    Just y  -> (y, s)
    Nothing -> ( base, ReidS
        { base = base + 1
        , imap = M.insert x base imap } )


-- | Reidentify the graph within the monad.
reidGraphM
    :: (Functor m, Monad m, Ord i)
    => G.Graph i f a
    -> ReidT i m (G.Graph ID f a)
reidGraphM g =
    G.Graph <$> reidNodeMap (G.nodeMap g)
  where
    reidNodeMap m = M.fromList <$> mapM reidNodePair (M.toList m)
    reidNodePair (i, x) = (,) <$> reid i <*> reidNode x
    reidNode (G.Interior m) = fmap G.Interior $ Tr.mapM reid m
    reidNode (G.Frontier x) = return $ G.Frontier x


-- | Reidentify the graph.
reidGraph :: Ord i => G.Graph i f a -> J.Res i ID f a
reidGraph g = case runReid $ reidGraphM g of
    (g', ReidS{..}) -> J.Res
        { J.resGraph = g'
        , J.convID   = \i -> case M.lookup i imap of
            Just j  -> j
            Nothing -> error "reidGraph: no such identifier" }
