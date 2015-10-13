{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE FlexibleContexts #-}


-- | Feature graph unification.


module NLP.FeatureStructure.Unify
( unify
) where


import           Control.Arrow (first, second)
import           Control.Monad (forM_)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.Join as J
import           NLP.FeatureStructure.Join (Res (..))
import qualified NLP.FeatureStructure.Reid as R


--------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------


-- -- | Unify two feature graphs.  The function assumes, that the input
-- -- graphs have mutually-disjoint sets of node identifiers.  Either one
-- -- of the input identifiers can be used to designate the root of the
-- -- resultant graph.
-- unify
--     :: (Ord f, Eq a)
--     => (ID, Graph f a)
--     -> (ID, Graph f a)
--     -> Maybe (Graph f a)
-- unify (i, f) (j, g) = J.execJoin
--     (J.join i j)
--     (fromTwo f g)


-- | Unify two feature graphs.
_unify
    :: (Ord i, Ord j, Ord f, Ord a)
    => Graph i f a    -- ^ The first graph
    -> Graph j f a    -- ^ The second graph
    -> [(i, j)]       -- ^ Pairs of corresponding node IDs
    -> Maybe (Res (Either i j) (Either i j) f a)
_unify g h is = flip J.execJoin gh $ do
    -- note: almost the same code as in runConT
    -- from the Tree module
    forM_ (M.elems $ getFronts gh) $ \ks -> do
        forM_ (adja $ S.toList ks) $ \(i, j) -> do
            J.join i j
    mapM_ (uncurry J.join) $
        map (first Left . second Right) is
  where
    gh = fromTwo g h


-- | Unify two feature graphs.
unify
    :: (Ord i, Ord j, Ord f, Ord a)
    => Graph i f a    -- ^ The first graph
    -> Graph j f a    -- ^ The second graph
    -> [(i, j)]       -- ^ Pairs of corresponding node IDs
    -> Maybe (Res (Either i j) ID f a)
unify g h is = do
    Res v con  <- _unify g h is
    Res w con' <- return $ R.reidGraph v
    return $ Res w (con'.con)


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Obtain frontier nodes of the graph and return them in a form
-- of a map from atomic values to *sets* of identifiers.
--
-- Note that in a properly formed feature graph each atomic value
-- should be assigned a unique node in the graph and thus be
-- assiged a single identifier.  However, if two graphs 
getFronts :: (Ord i, Ord a) => Graph i f a -> M.Map a (S.Set i)
getFronts Graph{..} = M.fromListWith S.union
    [ (x, S.singleton i)
    | (i, Frontier x) <- M.toList nodeMap ]


-- | Join two feature graphs.  Nodes from the first graph will be
-- marked as `Left`s, nodes from the second one -- as `Right`s. 
--
-- WARNING: Note that the result may be not a valid graph in the
-- sense that it can contain duplicate frontier nodes.
--
-- TODO:  It should be not exported by the library.  It is only
-- used internall in Unify and it should be defined there.
fromTwo
    :: (Ord i, Ord j)
    => Graph i f a
    -> Graph j f a
    -> Graph (Either i j) f a
fromTwo f g = Graph $ M.fromAscList $
    (nodeList $ mapIDsMono Left f) ++
    (nodeList $ mapIDsMono Right g)
    where nodeList = M.toAscList . nodeMap


-- | Map keys of the feature graph using a strictly monotonic function.
mapIDsMono :: Ord j => (i -> j) -> Graph i f a -> Graph j f a
mapIDsMono f (Graph m) = Graph $ M.fromAscList
    [ (f k, mapNodeIDs f v)
    | (k, v) <- M.toAscList m ]


-- | Map identifiers of the node.
mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
mapNodeIDs f (Interior m) = Interior $ M.map f m
mapNodeIDs _ (Frontier x) = Frontier x
{-# INLINE mapNodeIDs #-}


-- | Pairs of adjacent elements in a list.
adja :: [a] -> [(a, a)]
adja xs = zip xs (drop 1 xs)
