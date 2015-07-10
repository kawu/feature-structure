{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts #-}


-- | Feature graph unification.


module NLP.FeatureStructure.Unify
( unify
) where


import           Control.Arrow (first, second)

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
    :: (Ord i, Ord j, Ord f, Eq a)
    => Graph i f a    -- ^ The first graph
    -> Graph j f a    -- ^ The second graph
    -> [(i, j)]       -- ^ Pairs of corresponding node IDs
    -> Maybe (Res (Either i j) (Either i j) f a)
_unify g h is = flip J.execJoin (fromTwo g h) $
    mapM_ (uncurry J.join) $
        map (first Left . second Right) is


-- | Unify two feature graphs.
unify
    :: (Ord i, Ord j, Ord f, Eq a)
    => Graph i f a    -- ^ The first graph
    -> Graph j f a    -- ^ The second graph
    -> [(i, j)]       -- ^ Pairs of corresponding node IDs
    -> Maybe (Res (Either i j) ID f a)
unify g h is = do
    Res v con  <- _unify g h is
    Res w con' <- return $ R.reidGraph v
    return $ Res w (con'.con)
