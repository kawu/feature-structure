{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A graph-based representation of a feature structure.
--
-- At this point we assume that values are `atomic` and the only
-- operation defined over them is the equality check.


module NLP.FeatureStructure.Graph
(
-- * Basic types
  FG
, Node (..)
, NodeFG

-- * Unification
, unify
, unifyIO

-- * Tests
, test1
, test2
, test2v2
, test3
, test4
, test4v2
, test5
, test5v2
, test6
, test6v2
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>))
import           Control.Monad (forM)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Strict as S
import qualified Control.Error as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Traversable as Tr
import qualified Pipes as P

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))
import qualified Control.Monad.Atom as Atom

import qualified NLP.FeatureStructure.DisjSet as D


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


-- purely :: P.Producer String Identity () -> [String]
-- purely = Pipes.Prelude.toList


--------------------------------------------------------------------
-- Feature graph
--------------------------------------------------------------------


-- | A feature graph with node identifiers of type `i`, edges labeled
-- by features of type `f`, and frontier nodes labeled with atomic
-- values of type `a`.
--
-- Invariant: every node identifier points to a node which *exists*
-- in the feature graph.
type FG i f a = M.Map i (Node i f a)


-- | A node in a feature graph.
data Node i f a
    = Interior (M.Map f i)
    | Frontier a
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  Nodes from the first graph will be
-- marked as `Left`s, nodes from the second one -- as `Right`s. 
fromTwo :: Ord i => FG i f a -> FG i f a -> FG (Either i i) f a
fromTwo f g = M.fromAscList $
    (M.toAscList $ mapIDsMono Left f) ++
    (M.toAscList $ mapIDsMono Right g)


-- | Map keys of the feature graph using a strictly monotonic function.
mapIDsMono :: Ord j => (i -> j) -> FG i f a -> FG j f a
mapIDsMono f m = M.fromAscList
    [ (f k, mapNodeIDs f v)
    | (k, v) <- M.toAscList m ]


-- | Map identifiers of the node using a strictly monotonic function.
mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
mapNodeIDs f (Interior m) = Interior $ M.map f m
mapNodeIDs _ (Frontier x) = Frontier x
{-# INLINE mapNodeIDs #-}


--------------------------------------------------------------------
-- Unification monad
--------------------------------------------------------------------


-- | The state of the unification monad.
data UMS i f a = UMS {
    -- | A sequence of node pairs.
      umSq  :: Seq (i, i)
    -- | A feature graph.
    , umFg  :: FG i f a
    -- | A disjoint-set covering information about representants.
    , umDs  :: D.DisjSet i }


-- | The unification monad transformer.
type UMT i f a m b =
    P.Producer String
    (MaybeT
    (S.StateT (UMS i f a) m)) b


-- -- | The unification monad.
-- type UM i f a b = UMT i f a Identity b


--------------------------------------------------------------------
-- Unification monad: class
--------------------------------------------------------------------


class (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where
instance (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where


--------------------------------------------------------------------
-- Unification monad: core
--------------------------------------------------------------------


-- | Pop the node pair from the queue.  TODO: By the way, does
-- it have to be a queue for the algorithm to be correct?
pop :: (Uni i f a, Monad m) => UMT i f a m (Maybe (i, i))
pop = do
    mx <- S.state $ \ums@UMS{..} -> case Seq.viewl umSq of
        EmptyL  -> (Nothing, ums)
        x :< sq -> (Just x, ums {umSq = sq})
    log $ "[pop] " ++ show mx
    return mx


-- | Push the node pair into the queue.
push :: (Uni i f a, Monad m) => (i, i) -> UMT i f a m ()
push x = do
    log $ "[push] " ++ show x
    S.modify $ \ums@UMS{..} -> ums {umSq = umSq |> x}


-- | Return the representant of the given node.
-- TODO: It doesn't fail with the `Other`, perhaps we should change that?
repr :: (Uni i f a, Monad m) => i -> UMT i f a m i
repr x = do
    y <- D.repr x <$> S.gets umDs
    log $ "[repr] " ++ show x ++ " -> " ++ show y
    return y


-- | Set the representant of the node.
mkReprOf :: (Uni i f a, Monad m) => i -> i -> UMT i f a m ()
mkReprOf x y = do
    log $ "[mkReprOf] " ++ show y ++ " -> " ++ show x
    S.modify $ \ums@UMS{..} ->
        ums {umDs = D.mkReprOf x y umDs}


-- | Unification failure.
uniFail :: Monad m => UMT i f a m b
uniFail = P.lift E.nothing


-- | Node behind the identifier.
node :: (Uni i f a, Monad m) => i -> UMT i f a m (Node i f a)
node x = do
    fg <- S.gets umFg
    case M.lookup x fg of
        Just y  -> return y
        Nothing -> do
            log $ "ERROR: node " ++ show x ++ " doesn't exist!"
            uniFail


-- | Set node under the given identifier.
setNode :: (Uni i f a, Monad m) => i -> Node i f a -> UMT i f a m ()
setNode i x = do
    log $ "[setNode] " ++ show i ++ " -> " ++ show x
    S.modify $ \ums@UMS{..} ->
        ums {umFg = M.insert i x umFg}


-- | Remove node under the given identifier.
remNode :: (Uni i f a, Monad m) => i -> UMT i f a m ()
remNode i = do
    log $ "[remNode] " ++ show i
    S.modify $ \ums@UMS{..} ->
        ums {umFg = M.delete i umFg}


--------------------------------------------------------------------
-- Unification monad: advanced
--------------------------------------------------------------------


-- -- | Pop next pair of distinct nodes from the queue.
-- popNext :: (Ord i, Show i, Monad m) => UMT i f a m (Maybe (i, i))
-- popNext = whileM (fmap not . isEmptyQ) tryPop


-- | Top of the queue, failure.
data Fail
    = Equal -- ^ Top values are not distinct
    | Empty -- ^ Queue is empty


-- | Pop next pair of nodes from the queue.  If the nodes are
-- not distinct, return `Nothing`.
popNext :: (Uni i f a, Monad m) => UMT i f a m (Either Fail (i, i))
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
-- Unification
--------------------------------------------------------------------


-- | A graph with a selected node.
type NodeFG i f a = (i, FG i f a)


-- | Unify two feature graphs.  Logging messages will be printed to stdout.
unify
    :: Uni i f a
    => NodeFG i f a -> NodeFG i f a
    -> Maybe (NodeFG Int f a)
unify n m
    = runIdentity
    $ flip S.evalStateT (unifySt0 n m)
    $ runMaybeT $ logSilent
    $ unifyGen n m


-- | Unify two feature graphs.  Logging messages will be printed to stdout.
unifyIO
    :: Uni i f a
    => NodeFG i f a -> NodeFG i f a
    -> IO (Maybe (NodeFG Int f a))
unifyIO n m
    = flip S.evalStateT (unifySt0 n m)
    $ runMaybeT $ logLoud
    $ unifyGen n m


-- | Unify two feature graphs.  Generic function.
unifyGen
    :: (Uni i f a, Monad m)
    => NodeFG i f a -> NodeFG i f a
    -> UMT (Either i i) f a m
        (NodeFG Int f a)
unifyGen (i, _) (_, _) = do
    unifyLoop               -- Unification
    k <- repr $ Left i      -- The new root
    updateIDs               -- Update identifiers
    h <- S.gets umFg        -- The new graph
    return $ reIdent (k, h)


-- | Initial state of the unification computation.
unifySt0 :: Ord i => NodeFG i f a -> NodeFG i f a -> UMS (Either i i) f a
unifySt0 (i, f) (j, g) = UMS
    { umSq  = Seq.singleton (Left i, Right j)
    , umFg  = fromTwo f g
    , umDs  = D.empty }


-- | Unify within the `UM` monad.
unifyLoop :: (Uni i f a, Monad m) => UMT i f a m ()
unifyLoop = whileNotEmpty popNext $ \(i, j) -> do
    p <- node i
    q <- node j
    mergeNodes (i, p) (j, q)


-- | A node with corresponding identifier.
type NodeID i f a = (i, Node i f a)


-- | Merge the two given nodes.
mergeNodes
    :: (Uni i f a, Monad m)
    => NodeID i f a -> NodeID i f a
    -> UMT i f a m ()
mergeNodes (i, n) (j, m) =
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


-- | Traverse the graph and update node identifiers.  As a side effect,
-- the `umDs` component of the state will be cleared.
updateIDs :: (Uni i f a, Monad m) => UMT i f a m ()
updateIDs = S.modify $ \UMS{..} ->
    let upd (Interior m) = Interior $ M.map rep m
        upd (Frontier x) = Frontier x
        rep = flip D.repr umDs
        both f (x, y) = (f x, f y)
    in UMS
        { umSq  = fmap (both rep) umSq
        -- Keys do not need to be updated, becase an invariant is
        -- preserved that only representants are stored as keys.
        -- TODO: Give this info also in a more visible place!
        , umFg  = M.map upd umFg
        -- The implementator has to be aware, that after this step
        -- using the `repr` function on a node which is not present
        -- in the graph (e.g. it has been removed) will lead to
        -- incorrect results.
        , umDs  = D.empty }


--------------------------------------------------------------------
-- Assign new identifiers
--------------------------------------------------------------------


-- | Assign new node identifiers [0, 1, ..].
reIdent :: Ord i => NodeFG i f a -> NodeFG Int f a
reIdent (r, f) = Atom.evalAtom $ do
    s <- Atom.toAtom r
    -- TODO: To speedup things, we could try to use
    -- M.toAscList/M.fromAscList pair here.
    g <- forM (M.toList f) $ \(i, x) -> do 
        j <- Atom.toAtom i
        y <- reIdentNode x
        return (j, y)
    return (s, M.fromList g)
  where
    reIdentNode (Interior m) = fmap Interior $ Tr.mapM Atom.toAtom m
    reIdentNode (Frontier x) = return $ Frontier x


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


test1 :: IO ()
test1 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2)])
        , (2, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test2 :: IO ()
test2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Frontier 'x') ]


test2v2 :: IO ()
test2v2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior M.empty) ]


test3 :: IO ()
test3 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]


test4 :: IO ()
test4 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'y') ]


test4v2 :: IO ()
test4v2 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Frontier 'x') ]


test5 :: IO ()
test5 = do
    print =<< unifyIO (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Frontier 'x')
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Frontier 'x')
        , (4, Frontier 'x') ]


test5v2 :: IO ()
test5v2 = do
    print =<< unifyIO (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 2), ('c', 3)])
        , (2, Interior M.empty)
        , (3, Interior $ M.fromList [('a', 4)])
        , (4, Frontier 'x') ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 2), ('b', 3), ('c', 3)])
        , (2, Interior $ M.fromList [('a', 4)])
        , (3, Interior M.empty)
        , (4, Frontier 'x') ]


test6 :: IO ()
test6 = do
    print =<< unifyIO (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 1), ('b', 2)])
        , (2, Interior $ M.fromList [('a', 3)])
        , (3, Interior M.empty) ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior $ M.fromList
            [('a', 1), ('b', 2)]) ]


test6v2 :: IO ()
test6v2 = do
    print $ unify (1, f1) (1, f2)
  where
    f1 :: FG Int Char Char
    f1 = M.fromList
        [ (1, Interior $ M.fromList
            [('a', 1), ('b', 2)])
        , (2, Interior $ M.fromList [('a', 3)])
        , (3, Interior M.empty) ]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior $ M.fromList
            [('a', 1), ('b', 2)]) ]
