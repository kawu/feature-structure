{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}


-- | A monad for joining nodes in a feature graph. 


module NLP.FeatureStructure.Join
(
-- * JoinT
  JoinT
, Join
, runJoinT
, runJoin
, execJoinT
, execJoin
, evalJoinT
, evalJoin
, liftGraph

-- * Joining nodes
, join
, joinMany
-- , repr
) where


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


-- import qualified NLP.FeatureStructure.DisjSet as D
import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph


--------------------------------------------------------------------
-- Join monad
--------------------------------------------------------------------


-- | The join monad transformer.
type JoinT f a m =
    MaybeT
    (S.StateT (Seq (ID, ID))
    (GraphT f a m))


-- | The join monad over the identity monad.
type Join f a = JoinT f a Identity


-- | Execute the `JoinT` computation.
runJoinT
    :: Monad m
    => JoinT f a m b
    -> Graph f a
    -> m (Maybe (b, Graph f a))
runJoinT m g = do
    (mx, g') <- flip runGraphT g 
        $ flip S.evalStateT Seq.empty
        $ runMaybeT m
    return $ case mx of
        Just x  -> Just (x, g')
        Nothing -> Nothing


-- | Run the `Join` monad on the given graph in a pure setting.
runJoin :: Join f a b -> Graph f a -> Maybe (b, Graph f a)
runJoin m = runIdentity . runJoinT m


-- | Execute the `JoinT` computation.
execJoinT :: Monad m => JoinT f a m b -> Graph f a -> m (Maybe (Graph f a))
execJoinT m g = do
    x <- runJoinT m g
    return $ snd <$> x


-- | Execute the `JoinT` computation.
execJoin :: Join f a b -> Graph f a -> Maybe (Graph f a)
execJoin m = runIdentity . execJoinT m


-- | Evaluate the `JoinT` computation.
evalJoinT :: Monad m => JoinT f a m b -> Graph f a -> m (Maybe b)
evalJoinT m g = do
    x <- runJoinT m g
    return $ fst <$> x


-- | Evaluate the `JoinT` computation.
evalJoin :: Join f a b -> Graph f a -> Maybe b
evalJoin m = runIdentity . evalJoinT m


-- | Lift the computation to the Graph monad.
liftGraph :: Monad m => GraphT f a m b -> JoinT f a m b
liftGraph = lift . lift


--------------------------------------------------------------------
-- Join monad: core
--------------------------------------------------------------------


-- | Pop the node pair from the queue.  TODO: By the way, does
-- it have to be a queue for the algorithm to be correct?
pop :: Monad m => JoinT f a m (Maybe (ID, ID))
pop = S.state $ \q -> case Seq.viewl q of
    EmptyL  -> (Nothing, q)
    x :< q' -> (Just x, q')


-- | Push the node pair into the queue.
push :: Monad m => (ID, ID) -> JoinT f a m ()
push x = S.modify (|>x)


-- | Unification failure.
uniFail :: Monad m => JoinT f a m b
uniFail = E.nothing


--------------------------------------------------------------------
-- Join monad: advanced
--------------------------------------------------------------------


-- | Top of the queue, failure.
data Fail
    = Equal -- ^ Top values are not distinct
    | Empty -- ^ Queue is empty


-- | Pop next pair of nodes from the queue.  If the nodes are
-- not distinct, return `Nothing`.
popNext :: (Functor m, Monad m) => JoinT f a m (Either Fail (ID, ID))
popNext = E.runEitherT $ do
    (i0, j0) <- E.tryJust Empty =<< lift pop
    (i, j) <- lift . liftGraph $ (,)
        <$> getRepr i0
        <*> getRepr j0
    E.tryAssert Equal $ i /= j
    return (i, j)


-- | Run the second monad on the elements acquired from the first monad
-- as long as the first monad doesn't return the `Empty` element.
whileNotEmpty :: Monad m => (m (Either Fail a)) -> (a -> m ()) -> m ()
whileNotEmpty cond m = do
    mx <- cond
    case mx of
        Right x     -> m x >> whileNotEmpty cond m
        Left Equal  -> whileNotEmpty cond m
        Left Empty  -> return ()


--------------------------------------------------------------------
-- Node merging
--------------------------------------------------------------------


-- | Merge all node-pairs remaining in the `jsQu` queue.
mergeRest :: (Functor m, Monad m, Ord f, Eq a) => JoinT f a m ()
mergeRest = whileNotEmpty popNext $ \(i, j) -> merge i j


-- | Merge two nodes under the given identifiers and populate
-- queue with new node-pairs which need to be joined.
merge
    :: (Functor m, Monad m, Ord f, Eq a)
    => ID -> ID -> JoinT f a m ()
merge i j = void $ runMaybeT $ do
    n <- MaybeT $ liftGraph $ getNode i
    m <- MaybeT $ liftGraph $ getNode j
    lift $ doit n m
  where
    doit (Interior p) (Interior q) = do
        -- TODO: We could probably speed it up by checking if some
        -- of the pairs have not been already joined.
        mapM_ push $ joint p q
        liftGraph $ do
            setNode i $ Interior $ M.union p q
            remNode j >> i `mkReprOf` j
    doit (Frontier _) (Interior q)
        | M.null q  = liftGraph $ remNode j >> i `mkReprOf` j
        | otherwise = uniFail
    doit (Interior p) (Frontier _)
        | M.null p  = liftGraph $ remNode i >> j `mkReprOf` i
        | otherwise = uniFail
    doit (Frontier x) (Frontier y)
        | x == y    = liftGraph $ remNode j >> i `mkReprOf` j
        | otherwise = uniFail
    joint x y = M.elems $ M.intersectionWith (,) x y


--------------------------------------------------------------------
-- Join operations
--------------------------------------------------------------------


-- | Join the two given nodes (and more, when necesseary)
-- in the feature graph.
join :: (Functor m, Monad m, Ord f, Eq a) => ID -> ID -> JoinT f a m ()
join i j = merge i j >> mergeRest


-- | Join the given pairs of nodes (and more, when necesseary)
-- in the feature graph.
joinMany
    :: (Functor m, Monad m, Ord f, Eq a)
    => [(ID, ID)] -> JoinT f a m ()
joinMany = mapM_ $ uncurry join


--------------------------------------------------------------------
-- Update identifiers
--------------------------------------------------------------------


-- -- | Traverse the graph and update node identifiers.  As a side effect,
-- -- the `jsDs` component of the state will be cleared.
-- updateIDs :: (Uni i f a, Monad m) => JoinT i f a m ()
-- updateIDs = S.modify $ \JoinS{..} ->
--     let upd (Interior m) = Interior $ M.map rep m
--         upd (Frontier x) = Frontier x
--         rep = flip D.repr jsDs
--         both f (x, y) = (f x, f y)
--     in JoinS
--         { jsQu  = fmap (both rep) jsQu
--         -- Keys do not need to be updated, becase an invariant is
--         -- preserved that only representants are stored as keys.
--         -- TODO: Give this info also in a more visible place!
--         , jsFg  = M.map upd jsFg
--         -- The implementator has to be aware, that after this step
--         -- using the `repr` function on a node which is not present
--         -- in the graph (e.g. it has been removed) will lead to an
--         -- incorrect result.
--         , jsDs  = D.empty }


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- -- | Sequnce to monadic actions and return result of the first one.
-- (<<) :: Monad m => m a -> m b -> m a
-- m << n = do
--     x <- m
--     _ <- n
--     return x
