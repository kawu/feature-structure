{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}


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
, Fail (..)

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
) where


import           Control.Applicative ((<$>))
import           Control.Monad (guard)
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Strict as S
import qualified Control.Error as E

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL(..))

import qualified NLP.FeatureStructure.DisjSet as D

import Debug.Trace (trace)


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


-- | Error types for the unification monad.
data Fail
    = UniFail           -- ^ Non-recoverable failure of unification
    | OtherFail String  -- ^ An unexpected runtime error
    deriving (Show, Eq, Ord)


-- | The unification monad.
type UM i f a b = E.EitherT Fail (S.State (UMS i f a)) b


-- | Pop the node pair from the queue.  TODO: By the way, does
-- it have to be a queue for the algorithm to be correct?
pop :: UM i f a (Maybe (i, i))
pop = S.state $ \ums@UMS{..} -> case Seq.viewl umSq of
    EmptyL  -> (Nothing, ums)
    x :< sq -> (Just x, ums {umSq = sq})


-- | Push the node pair into the queue.
push :: (i, i) -> UM i f a ()
push x = S.modify $ \ums@UMS{..} -> ums {umSq = umSq |> x}


-- | Return the representant of the given node.
-- TODO: It doesn't fail with the `Other`, perhaps we should change that?
repr :: Ord i => i -> UM i f a i
repr x = D.repr x <$> S.gets umDs


-- | Unification failure.
uniFail :: UM i f a b
uniFail = E.left UniFail


-- | Node behind the identifier.
node :: (Ord i, Show i) => i -> UM i f a (Node i f a)
node x = do
    fg <- S.gets umFg
    E.tryJust
        (OtherFail $ "Node " ++ show x ++ " not found")
        (M.lookup x fg)


-- | Set the representant of the node.
mkReprOf :: Ord i => i -> i -> UM i f a ()
mkReprOf x y = S.modify $ \ums@UMS{..} ->
    ums {umDs = D.mkReprOf x y umDs}


-- | Set node under the given identifier.
setNode :: Ord i => i -> Node i f a -> UM i f a ()
setNode i x = S.modify $ \ums@UMS{..} ->
    ums {umFg = M.insert i x umFg}


-- | Remove node under the given identifier.
remNode :: Ord i => i -> UM i f a ()
remNode i = S.modify $ \ums@UMS{..} ->
    ums {umFg = M.delete i umFg}


-- | Pop next pair of distinct nodes from the queue.
popNext :: Ord i => UM i f a (Maybe (i, i))
popNext = runMaybeT $ do
    (i0, j0) <- MaybeT pop
    i <- S.lift $ repr i0
    j <- S.lift $ repr j0
    guard $ i /= j
    return (i, j)


--------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------


-- | A graph with a selected node.
type NodeFG i f a = (i, FG i f a)


-- | Unify two feature graphs.
unify
    :: (Ord i, Show i, Eq a, Ord f)
    => NodeFG i f a -> NodeFG i f a
    -> Either Fail (NodeFG (Either i i) f a)
unify (i, f) (j, g) = flip S.evalState st0 $ E.runEitherT $ do
    unifyLoop               -- Unification
    updateIDs               -- New identifiers
    k <- repr $ Left i      -- The new root
    h <- S.gets umFg        -- The new graph
    return (k, h)
  where
    -- Initial unification state
    st0 = UMS
        { umSq  = Seq.singleton (Left i, Right j)
        , umFg  = fromTwo f g
        , umDs  = D.empty }


-- | Unify within the `UM` monad.
unifyLoop :: (Ord i, Show i, Ord f, Eq a) => UM i f a ()
unifyLoop = whileM popNext $ \(i, j) -> trace (show (i, j)) $ do
    p <- node i
    q <- node j
    mergeNodes (i, p) (j, q)


-- | A node with corresponding identifier.
type NodeID i f a = (i, Node i f a)


-- | Merge the two given nodes.
mergeNodes
    :: (Eq a, Ord i, Ord f)
    => NodeID i f a -> NodeID i f a
    -> UM i f a ()
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
updateIDs :: Ord i => UM i f a ()
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
        , umDs  = D.empty }


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Run the second monad in a loop as long as the first one is able
-- to supply value to it.
whileM :: Monad m => (m (Maybe a)) -> (a -> m ()) -> m ()
whileM cond m = do
    mx <- cond
    case mx of
        Nothing -> return ()
        Just x  -> m x >> whileM cond m


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


test1 :: IO ()
test1 = do
    print $ unify (1 :: Int, f1) (1, f2)
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
    print $ unify (1 :: Int, f1) (1, f2)
  where
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Frontier 'x') ]


test2v2 :: IO ()
test2v2 = do
    print $ unify (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [ (1, Interior $ M.fromList [('a', 2)])
        , (2, Interior M.empty) ]


test3 :: IO ()
test3 = do
    print $ unify (1 :: Int, f1) (1, f2)
  where
    f1 :: FG Int Char ()
    f1 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]
    f2 = M.fromList
        [(1, Interior $ M.fromList [('a', 1)])]


test4 :: IO ()
test4 = do
    print $ unify (1 :: Int, f1) (1, f2)
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
    print $ unify (1 :: Int, f1) (1, f2)
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
    print $ unify (1 :: Int, f1) (1, f2)
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
    print $ unify (1, f1) (1, f2)
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
