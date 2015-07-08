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

-- import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))


import           NLP.FeatureStructure.Graph2 hiding (getNode)
import qualified NLP.FeatureStructure.DisjSet as D


--------------------------------------------------------------------
-- Join monad
--------------------------------------------------------------------


-- | State of the unification monad.
data JoinS i f a = JoinS {
    -- | The underlying graph.
      graph :: Graph i f a
    -- | The queue of identifiers to be joined.
    , joinQ :: Seq (i, i)
    -- | The disjoint set keeps track of merged nodes.
    , disjS :: D.DisjSet i
    } deriving (Show)


-- | Empty JoinS state.
emptyJoinS :: Graph i f a -> JoinS i f a
emptyJoinS g = JoinS g Seq.empty D.empty


-- | Join monad transformer.
type JoinT i f a m =
    MaybeT (S.StateT (JoinS i f a) m)


-- | Join monad.
type Join i f a = JoinT i f a Identity


-- | Execute the `JoinT` computation.
runJoinT
    :: (Monad m, Ord i)
    => JoinT i f a m b
    -> Graph i f a
    -> m (Maybe (b, Res i i f a))
runJoinT m g = do
    (mx, joinS) <- S.runStateT
        (runMaybeT m) (emptyJoinS g)
    return $ case mx of
        Just x -> Just (x, Res
            { resGraph = graph joinS
            , convID = flip D.repr $ disjS joinS })
        Nothing -> Nothing


-- | Run the `Join` monad on the given graph in a pure setting.
runJoin :: Ord i => Join i f a b -> Graph i f a -> Maybe (b, Res i i f a)
runJoin m = runIdentity . runJoinT m


-- | Execute the `JoinT` computation.
execJoinT :: (Monad m, Ord i) => JoinT i f a m b -> Graph i f a -> m (Maybe (Res i i f a))
execJoinT m g = do
    x <- runJoinT m g
    return $ snd <$> x


-- | Execute the `JoinT` computation.
execJoin :: Ord i => Join i f a b -> Graph i f a -> Maybe (Res i i f a)
execJoin m = runIdentity . execJoinT m


-- | Evaluate the `JoinT` computation.
evalJoinT :: (Monad m, Ord i) => JoinT i f a m b -> Graph i f a -> m (Maybe b)
evalJoinT m g = do
    x <- runJoinT m g
    return $ fst <$> x


-- | Evaluate the `JoinT` computation.
evalJoin :: Ord i => Join i f a b -> Graph i f a -> Maybe b
evalJoin m = runIdentity . evalJoinT m


--------------------------------------------------------------------
-- Join monad: core
--------------------------------------------------------------------


-- | Pop the node pair from the queue.  TODO: By the way, does
-- it have to be a queue for the algorithm to be correct?
pop :: Monad m => JoinT i f a m (Maybe (i, i))
pop = S.state $ \s@JoinS{..} -> case Seq.viewl joinQ of
    EmptyL -> (Nothing, s)
    x :< q -> (Just x,  s{joinQ=q})


-- | Push the node pair into the queue.
push :: Monad m => (i, i) -> JoinT i f a m ()
push x = S.modify $ \s -> s
    { joinQ = joinQ s |> x }


-- | Unification failure.
uniFail :: Monad m => JoinT i f a m b
uniFail = E.nothing


--------------------------------------------------------------------
-- Join monad: graph-related part
--------------------------------------------------------------------


-- | Identify the current representant of the node.
getRepr :: (Functor m, Monad m, Ord i) => i -> JoinT i f a m i
getRepr k = D.repr k <$> S.gets disjS


-- | Set the representant of the node.
mkReprOf :: (Monad m, Ord i) => i -> i -> JoinT i f a m ()
mkReprOf x y = S.modify $ \s ->
        s {disjS = D.mkReprOf x y $ disjS s}


-- | Retrieve node hidden behind the given identifier.
getNode :: (Functor m, Monad m, Ord i) => i -> JoinT i f a m (Node i f a)
getNode i = maybeT =<<
    M.lookup <$> getRepr i <*> S.gets (nodeMap.graph)


-- | Set node under the given identifier.
setNode :: (Monad m, Ord i) => i -> Node i f a -> JoinT i f a m ()
setNode i x = S.modify $ \s ->
    let g = graph s
    in  s { graph = g { nodeMap = M.insert i x (nodeMap g) } }


-- | Remove node under the given identifier.
remNode :: (Monad m, Ord i) => i -> JoinT i f a m ()
remNode i = S.modify $ \s ->
    let g = graph s
    in  s { graph = g { nodeMap = M.delete i (nodeMap g) } }


--------------------------------------------------------------------
-- Join monad: advanced
--------------------------------------------------------------------


-- | Top of the queue, failure.
data Fail
    = Equal -- ^ Top values are not distinct
    | Empty -- ^ Queue is empty


-- | Pop next pair of nodes from the queue.  If the nodes are
-- not distinct, return `Equal`.
popNext :: (Functor m, Monad m, Ord i) => JoinT i f a m (Either Fail (i, i))
popNext = E.runEitherT $ do
    (i0, j0) <- E.tryJust Empty =<< lift pop
    (i, j) <- lift $ (,)
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


-- | Merge all node-pairs remaining in the queue.
mergeRest :: (Functor m, Monad m, Ord i, Ord f, Eq a) => JoinT i f a m ()
mergeRest = whileNotEmpty popNext $ \(i, j) -> merge i j


-- | Merge two nodes under the given identifiers and populate
-- queue with new node-pairs which need to be joined.
merge
    :: (Functor m, Monad m, Ord i, Ord f, Eq a)
    => i -> i -> JoinT i f a m ()
merge i j = do
    n <- getNode i
    m <- getNode j
    doit n m
  where
    doit (Interior p) (Interior q) = do
        -- TODO: We could probably speed it up by checking if some
        -- of the pairs have not been already joined.
        mapM_ push $ joint p q
        setNode i $ Interior $ M.union p q
        remNode j >> i `mkReprOf` j
    doit (Frontier _) (Interior q)
        | M.null q  = remNode j >> i `mkReprOf` j
        | otherwise = uniFail
    doit (Interior p) (Frontier _)
        | M.null p  = remNode i >> j `mkReprOf` i
        | otherwise = uniFail
    doit (Frontier x) (Frontier y)
        | x == y    = remNode j >> i `mkReprOf` j
        | otherwise = uniFail
    joint x y = M.elems $ M.intersectionWith (,) x y


--------------------------------------------------------------------
-- Join operations
--------------------------------------------------------------------


-- | Join the two given nodes (and more, when necesseary)
-- in the feature graph.
join :: (Functor m, Monad m, Ord i, Ord f, Eq a) => i -> i -> JoinT i f a m ()
join i j = merge i j >> mergeRest


-- | Join the given pairs of nodes (and more, when necesseary)
-- in the feature graph.
joinMany
    :: (Functor m, Monad m, Ord i, Ord f, Eq a)
    => [(i, i)] -> JoinT i f a m ()
joinMany = mapM_ $ uncurry join
