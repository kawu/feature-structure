{-# LANGUAGE RecordWildCards #-}


-- | The `Ident` monad allows to re-assign idenifiers in a given
-- structure.  It is similiar to `Control.Monad.Atom`, but it
-- works with identifiers only and it provides a way to specify
-- split points which divide the input structure into a set
-- of ID-disjoint substructures.


module NLP.FeatureStructure.Ident
( IdentT
, runIdentT
, rid
, ridGraph
, split
) where


import           Control.Applicative ((<$>), (<*>))
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
data IdentS = IdentS {
    -- | Base identifier: the maximal value (plus 1) assigned
    -- to any of the keys in the mapping.
      base  :: !ID
    -- | Current ID mapping.
    , imap  :: !(I.IntMap ID)
    } deriving (Show, Eq, Ord)


-- | The re-identification monad transformer.
type IdentT m = S.StateT IdentS m


-- | Run the reidentification monad.
runIdentT :: Monad m => IdentT m a -> m a
runIdentT = flip S.evalStateT $ IdentS 0 I.empty


-- | Set split point.
split :: Monad m => IdentT m ()
split = S.modify $ \s -> s {imap = I.empty}


-- | Re-identify a single identifier.
rid :: Monad m => ID -> IdentT m ID
rid x = S.state $ \s@IdentS{..} -> case I.lookup x imap of
    Just y  -> (y, s)
    Nothing -> ( base, IdentS
        { base = base + 1
        , imap = I.insert x base imap } )


--------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------
        

-- | Reidentify graph.
ridGraph :: (Functor m, Monad m) => G.Graph f a -> IdentT m (G.Graph f a)
ridGraph G.Graph{..} = G.Graph
    <$> ridNodeMap nodeMap
    <*> ridDisjSet disjSet
  where
    ridNodeMap m = I.fromList <$> mapM ridNodePair (I.toList m)
    ridDisjSet d = D.fromList <$> mapM ridPair (D.toList d)
    ridNodePair (i, x) = (,) <$> rid i <*> ridNode x
    ridNode (G.Interior m) = fmap G.Interior $ Tr.mapM rid m
    ridNode (G.Frontier x) = return $ G.Frontier x
    ridPair (x, y) = (,) <$> rid x <*> rid y


-- --------------------------------------------------------------------
-- -- Helpers
-- --------------------------------------------------------------------
-- 
-- 
-- -- | `rid` the `fst` element of the pair.
-- ridFst :: (Functor m, Monad m) => (ID, a) -> IdentT m (ID, a)
-- ridFst (x, y) = (,) <$> rid x <*> pure y
