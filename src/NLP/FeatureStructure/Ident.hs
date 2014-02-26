{-# LANGUAGE RecordWildCards #-}


-- | Using `Control.Monad.Atom` to re-assigne identifiers.
--
-- The monad extends the `Control.Monda.Atom` monad.  It allows
-- to specify control points which divide the input structure
-- into a set of disjoint substructures.


module NLP.FeatureStructure.Ident
( 
, rid
, ridGraph
) where


import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as S
import qualified Data.Traversable as Tr
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.Graph as G


-- | State of the reidentification monad.
data IdentS = IdentS {
    -- | Base identifier: new identifers will be added to it.
      base  :: !ID
    -- | Current ID mapping.
    , imap  :: !(I.IntMap ID)
    } deriving (Show, Eq, Ord)


-- | The re-identification monad transformer.
type IdentT m = S.StateT IdentS ID m


-- | Re-identify a single identifier.
rid :: Monda m => ID -> IdentT m ID
rid x = S.state $ \s@IdentS{..} -> case I.lookup x imap of
    Just y  -> (y, s)
    Nothing -> ????     -- END 
        
--     IdentS <- S.get
--     y <- lift $ Atom.toAtom x
--     return $ t + y


-- | Set control point.
control :: Ident ()
control 

    


-- | Reidentify graph.
ridGraph :: G.Graph f a -> Atom (G.Graph f a)
ridGraph Graph = Graph
    <$> ridNodeMap nodeMap
    <*> ridDisjSet disjSet
  where
    -- TODO: To speedup things, we could try to use
    -- M.toAscList/M.fromAscList pair here.
    -- BUT: we can't do that, not in a general case!
    -- We have no guarantee that after reidentification
    -- the list will be ascending.
    ridNodeMap m = M.fromList <$> mapM ridPair (M.toList m)
    ridDisjSet d = D.fromList <$> mapM ridPair (D.toList d)
    ridFst (i, x) = (,) <$> rid i <*> reIdentNode x
    ridNode (Interior m) = fmap Interior $ Tr.mapM rid m
    ridNode (Frontier x) = return $ Frontier x
