-- | Tree -> Graph conversion.  Provisional module.


module NLP.FeatureStructure.Conv where


import           NLP.FeatureStructure.Tree

import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Join as J


-- -- | Run the conversion monad.
-- runConT
--     :: Monad m
--     => ConvT m b
--     -> m (Maybe (b, G.Graph f a))
-- runConT convT = flip J.runJoinT G.empty $ do
--     -- First we run the conversion
--     (x, st) <- S.runStateT convT initConS
--     -- The second step is to join all nodes which have to be
--     -- merged based on the identifiers specified by the user.
--     forM_ (M.elems $ conI st) $ \ks -> do
--         forM_ (adja $ Set.toList ks) $ \(i, j) -> do
--             J.join i j
--     -- T.mapM (J.liftGraph . G.getRepr) t1
--     return x
-- 
-- 
-- -- | Convert a given feature tree to a feature graph.
-- --
-- -- TODO: the reason we cannot export this function as it is is
-- -- that the ID value returned could possibly change in the "future".
-- -- C.f. the `compile(s)` functions.
-- convTree
--     :: (Monad m, Ord i, Eq a, Ord f)
--     => FN i f a
--     -> ConT i f a m ID
-- convTree = fromFN


-- | A regular, 'forward-traveling' state of the conversion monad.
data ConF i f a = ConF {
    -- | A counter for producing new identifiers.
      conCounter :: Int
    -- | A mapping from old to new identifiers.
    , conMapping :: M.Map i (Set.Set ID) }


-- | Initial value of the state.
initConS :: ConF i f a
initConS = ConF
    { conC = 1
    , conI = M.empty }


-- | A 'backward-traveling' state of the conversion monad.
data ConB i f a = ConB {
    -- | A mapping from old to new identifiers.
    , conMapping :: M.Map i (Set.Set ID) }
