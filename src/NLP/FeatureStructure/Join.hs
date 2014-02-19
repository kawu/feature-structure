{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}


-- | A monad for joining nodes in a feature graph. 


module NLP.FeatureStructure.Join
(
-- * JoinT
  JoinS (..)
, JoinT
, Join
, runJoin
, runJoinT
, runJoinIO

-- * Joining nodes
, join
, joinMany
, repr
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>))
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Error as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Pipes as P

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))
import qualified NLP.FeatureStructure.DisjSet as D


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
-- import qualified NLP.FeatureStructure.Graph as G
-- import           NLP.FeatureStructure.Graph (FG, Node(..))


--------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------


-- | Log a value.
log :: Monad m => String -> P.Producer' String m ()
log = P.yield


-- | Run logger in the IO-based monad stack.
logLoud :: MonadIO m => P.Producer String m r -> m r
logLoud p = P.runEffect $ P.for p (liftIO . putStrLn)


-- | Supress logging messages. 
logSilent :: Monad m => P.Producer a m r -> m r
logSilent p = P.runEffect $ P.for p P.discard


--------------------------------------------------------------------
-- Join monad
--------------------------------------------------------------------


-- | The state of the join monad.
data JoinS i f a = JoinS {
    -- | A queue of node pairs.
      jsQu  :: Seq (i, i)
    -- | A feature graph.
    , jsFg  :: FG i f a
    -- | A disjoint-set covering information about representants.
    , jsDs  :: D.DisjSet i }


-- | The join monad transformer.
type JoinT i f a m b =
    P.Producer String
    (MaybeT
    (S.StateT (JoinS i f a) m)) b


-- | The join monad overt identity monad.
type Join i f a b = JoinT i f a Identity b


-- | Run the `Join` monad on the given graph in a pure setting.
-- Log messages will be ignored.
runJoin :: Uni i f a => FG i f a -> Join i f a b -> Maybe (b, FG i f a)
runJoin g = runIdentity . runJoinT g


-- | Run the `JoinT` monad on the given graph.
-- Log messages will be ignored.
runJoinT
    :: (Uni i f a, Monad m)
    => FG i f a -> JoinT i f a m b
    -> m (Maybe (b, FG i f a))
runJoinT g m = do
    (mx, js) <- flip S.runStateT (initState g) $
        runMaybeT $ logSilent $ m << updateIDs
    return $ case mx of
        Just x  -> Just (x, jsFg js)
        Nothing -> Nothing


-- | Run the `JoinT` monad on the given graph.
-- Log messages will be printed to stdio.
runJoinIO
    :: (Uni i f a, MonadIO m)
    => FG i f a -> JoinT i f a m b
    -> m (Maybe (b, FG i f a))
runJoinIO g m = do
    (mx, js) <- flip S.runStateT (initState g) $
        runMaybeT $ logLoud $ m << updateIDs
    return $ case mx of
        Just x  -> Just (x, jsFg js)
        Nothing -> Nothing


-- | Initialize `JoinT` state with the given graph.
initState :: FG i f a -> JoinS i f a
initState g = JoinS
    { jsQu  = Seq.empty
    , jsFg  = g
    , jsDs  = D.empty }


--------------------------------------------------------------------
-- Join monad: core
--------------------------------------------------------------------


-- | Pop the node pair from the queue.  TODO: By the way, does
-- it have to be a queue for the algorithm to be correct?
pop :: (Uni i f a, Monad m) => JoinT i f a m (Maybe (i, i))
pop = do
    mx <- S.state $ \js@JoinS{..} -> case Seq.viewl jsQu of
        EmptyL  -> (Nothing, js)
        x :< qu -> (Just x, js {jsQu = qu})
    log $ "[pop] " ++ show mx
    return mx


-- | Push the node pair into the queue.
push :: (Uni i f a, Monad m) => (i, i) -> JoinT i f a m ()
push x = do
    log $ "[push] " ++ show x
    S.modify $ \js@JoinS{..} -> js {jsQu = jsQu |> x}


-- | Return the representant of the given node.
-- TODO: It doesn't fail, perhaps we should change that?
repr :: (Uni i f a, Monad m) => i -> JoinT i f a m i
repr x = do
    y <- D.repr x <$> S.gets jsDs
    log $ "[repr] " ++ show x ++ " -> " ++ show y
    return y


-- | Set the representant of the node.
mkReprOf :: (Uni i f a, Monad m) => i -> i -> JoinT i f a m ()
mkReprOf x y = do
    log $ "[mkReprOf] " ++ show y ++ " -> " ++ show x
    S.modify $ \js@JoinS{..} ->
        js {jsDs = D.mkReprOf x y jsDs}


-- | Unification failure.
uniFail :: Monad m => JoinT i f a m b
uniFail = P.lift E.nothing


-- | Node behind the identifier.
node :: (Uni i f a, Monad m) => i -> JoinT i f a m (Node i f a)
node x = do
    fg <- S.gets jsFg
    case M.lookup x fg of
        Just y  -> return y
        Nothing -> do
            log $ "ERROR: node " ++ show x ++ " doesn't exist!"
            uniFail


-- | Set node under the given identifier.
setNode :: (Uni i f a, Monad m) => i -> Node i f a -> JoinT i f a m ()
setNode i x = do
    log $ "[setNode] " ++ show i ++ " -> " ++ show x
    S.modify $ \js@JoinS{..} ->
        js {jsFg = M.insert i x jsFg}


-- | Remove node under the given identifier.
remNode :: (Uni i f a, Monad m) => i -> JoinT i f a m ()
remNode i = do
    log $ "[remNode] " ++ show i
    S.modify $ \js@JoinS{..} ->
        js {jsFg = M.delete i jsFg}


--------------------------------------------------------------------
-- Join monad: advanced
--------------------------------------------------------------------


-- | Top of the queue, failure.
data Fail
    = Equal -- ^ Top values are not distinct
    | Empty -- ^ Queue is empty


-- | Pop next pair of nodes from the queue.  If the nodes are
-- not distinct, return `Nothing`.
popNext :: (Uni i f a, Monad m) => JoinT i f a m (Either Fail (i, i))
popNext = E.runEitherT $ do
    (i0, j0) <- E.tryJust Empty =<< S.lift pop
    i <- S.lift $ repr i0
    j <- S.lift $ repr j0
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
mergeRest :: (Uni i f a, Monad m) => JoinT i f a m ()
mergeRest = whileNotEmpty popNext $ \(i, j) -> merge i j


-- | Merge two nodes under the given identifiers and populate
-- `jsQu` with new node-pairs needed to be joined.
merge :: (Uni i f a, Monad m) => i -> i -> JoinT i f a m ()
merge i j = do
    n <- node i
    m <- node j
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
join :: (Uni i f a, Monad m) => i -> i -> JoinT i f a m ()
join i j = merge i j >> mergeRest


-- | Join the given pairs of nodes (and more, when necesseary)
-- in the feature graph.
joinMany :: (Uni i f a, Monad m) => [(i, i)] -> JoinT i f a m ()
joinMany = mapM_ $ uncurry join


--------------------------------------------------------------------
-- Update identifiers
--------------------------------------------------------------------


-- | Traverse the graph and update node identifiers.  As a side effect,
-- the `jsDs` component of the state will be cleared.
updateIDs :: (Uni i f a, Monad m) => JoinT i f a m ()
updateIDs = S.modify $ \JoinS{..} ->
    let upd (Interior m) = Interior $ M.map rep m
        upd (Frontier x) = Frontier x
        rep = flip D.repr jsDs
        both f (x, y) = (f x, f y)
    in JoinS
        { jsQu  = fmap (both rep) jsQu
        -- Keys do not need to be updated, becase an invariant is
        -- preserved that only representants are stored as keys.
        -- TODO: Give this info also in a more visible place!
        , jsFg  = M.map upd jsFg
        -- The implementator has to be aware, that after this step
        -- using the `repr` function on a node which is not present
        -- in the graph (e.g. it has been removed) will lead to an
        -- incorrect result.
        , jsDs  = D.empty }


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Sequnce to monadic actions and return result of the first one.
(<<) :: Monad m => m a -> m b -> m a
m << n = do
    x <- m
    _ <- n
    return x
