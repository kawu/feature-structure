{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}


-- | A monad for joining nodes in a feature graph. 


module NLP.FeatureStructure.Join2 where


import           Prelude hiding (log)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (void)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Error as E

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))


import           NLP.FeatureStructure.Graph2
import qualified NLP.FeatureStructure.DisjSet as D


--------------------------------------------------------------------
-- Join monad
--------------------------------------------------------------------


-- | State of the unification monad.
data JoinS i = JoinS {
    -- | The queue of identifiers to be joined.
      joinQ :: Seq (ID i, ID i)
    -- | The disjoint set keeps track of merged nodes.
    , disjS :: D.DisjSet (ID i)
    } deriving (Show)


-- | Join monad transformer.
type JoinT i f a m =
    MaybeT (S.StateT (JoinS i) m)


-- | Join monad.
type Join i f a = JoinT i f a Identity


-- | The result of the Join computation. 
data Res i f a b = Res {
    -- | The resulting graph,
      graph :: Graph i f a
    -- | A function which returns a representant for each ID of
    -- the input graph. 
    , 


-- | Execute the `JoinT` computation.
runJoinT
    :: Monad m
    => JoinT i f a m b
    -> Graph i f a
    -> m (Maybe (b, Graph i f a))
runJoinT m g = do
    (mx, g') <- flip runGraphT g 
        $ flip S.evalStateT Seq.empty
        $ runMaybeT m
    return $ case mx of
        Just x  -> Just (x, g')
        Nothing -> Nothing


-- -- | Run the `Join` monad on the given graph in a pure setting.
-- runJoin :: Join f a b -> Graph f a -> Maybe (b, Graph f a)
-- runJoin m = runIdentity . runJoinT m
-- 
-- 
-- -- | Execute the `JoinT` computation.
-- execJoinT :: Monad m => JoinT f a m b -> Graph f a -> m (Maybe (Graph f a))
-- execJoinT m g = do
--     x <- runJoinT m g
--     return $ snd <$> x
-- 
-- 
-- -- | Execute the `JoinT` computation.
-- execJoin :: Join f a b -> Graph f a -> Maybe (Graph f a)
-- execJoin m = runIdentity . execJoinT m
-- 
-- 
-- -- | Evaluate the `JoinT` computation.
-- evalJoinT :: Monad m => JoinT f a m b -> Graph f a -> m (Maybe b)
-- evalJoinT m g = do
--     x <- runJoinT m g
--     return $ fst <$> x
-- 
-- 
-- -- | Evaluate the `JoinT` computation.
-- evalJoin :: Join f a b -> Graph f a -> Maybe b
-- evalJoin m = runIdentity . evalJoinT m
-- 
-- 
-- -- | Lift the computation to the Graph monad.
-- liftGraph :: Monad m => GraphT f a m b -> JoinT f a m b
-- liftGraph = lift . lift
-- 
-- 
-- --------------------------------------------------------------------
-- -- Join monad: core
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Pop the node pair from the queue.  TODO: By the way, does
-- -- it have to be a queue for the algorithm to be correct?
-- pop :: Monad m => JoinT f a m (Maybe (ID, ID))
-- pop = S.state $ \q -> case Seq.viewl q of
--     EmptyL  -> (Nothing, q)
--     x :< q' -> (Just x, q')
-- 
-- 
-- -- | Push the node pair into the queue.
-- push :: Monad m => (ID, ID) -> JoinT f a m ()
-- push x = S.modify (|>x)
-- 
-- 
-- -- | Unification failure.
-- uniFail :: Monad m => JoinT f a m b
-- uniFail = E.nothing
-- 
-- 
-- --------------------------------------------------------------------
-- -- Join monad: advanced
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Top of the queue, failure.
-- data Fail
--     = Equal -- ^ Top values are not distinct
--     | Empty -- ^ Queue is empty
-- 
-- 
-- -- | Pop next pair of nodes from the queue.  If the nodes are
-- -- not distinct, return `Equal`.
-- popNext :: (Functor m, Monad m) => JoinT f a m (Either Fail (ID, ID))
-- popNext = E.runEitherT $ do
--     (i0, j0) <- E.tryJust Empty =<< lift pop
--     (i, j) <- lift . liftGraph $ (,)
--         <$> getRepr i0
--         <*> getRepr j0
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
-- 
-- 
-- --------------------------------------------------------------------
-- -- Node merging
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Merge all node-pairs remaining in the queue.
-- mergeRest :: (Functor m, Monad m, Ord f, Eq a) => JoinT f a m ()
-- mergeRest = whileNotEmpty popNext $ \(i, j) -> merge i j
-- 
-- 
-- -- | Merge two nodes under the given identifiers and populate
-- -- queue with new node-pairs which need to be joined.
-- merge
--     :: (Functor m, Monad m, Ord f, Eq a)
--     => ID -> ID -> JoinT f a m ()
-- merge i j = void $ runMaybeT $ do
--     n <- MaybeT $ liftGraph $ getNode i
--     m <- MaybeT $ liftGraph $ getNode j
--     lift $ doit n m
--   where
--     doit (Interior p) (Interior q) = do
--         -- TODO: We could probably speed it up by checking if some
--         -- of the pairs have not been already joined.
--         mapM_ push $ joint p q
--         liftGraph $ do
--             setNode i $ Interior $ M.union p q
--             remNode j >> i `mkReprOf` j
--     doit (Frontier _) (Interior q)
--         | M.null q  = liftGraph $ remNode j >> i `mkReprOf` j
--         | otherwise = uniFail
--     doit (Interior p) (Frontier _)
--         | M.null p  = liftGraph $ remNode i >> j `mkReprOf` i
--         | otherwise = uniFail
--     doit (Frontier x) (Frontier y)
--         | x == y    = liftGraph $ remNode j >> i `mkReprOf` j
--         | otherwise = uniFail
--     joint x y = M.elems $ M.intersectionWith (,) x y
-- 
-- 
-- --------------------------------------------------------------------
-- -- Join operations
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Join the two given nodes (and more, when necesseary)
-- -- in the feature graph.
-- join :: (Functor m, Monad m, Ord f, Eq a) => ID -> ID -> JoinT f a m ()
-- join i j = merge i j >> mergeRest
-- 
-- 
-- -- | Join the given pairs of nodes (and more, when necesseary)
-- -- in the feature graph.
-- joinMany
--     :: (Functor m, Monad m, Ord f, Eq a)
--     => [(ID, ID)] -> JoinT f a m ()
-- joinMany = mapM_ $ uncurry join
