{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A graph representation of a feature structure.


module NLP.FeatureStructure.Graph
(
-- * Basic types
  Graph (..)
, Node (..)
, empty
, mkGraph
, clean

-- * Monad
, GraphT
, GraphM
, runGraphT
, runGraphM
, getRepr
, mkReprOf
, getNode
, setNode
, remNode

-- * Utility
, equal
, equals
, compare'
, compares
, fromTwo
, printGraph
, printTree
, showFlat
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Applicative as App
import           Control.Monad (forM_, when, guard)
import           Control.Monad.Identity (Identity)
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Trans.Maybe (MaybeT(..))
-- import           Control.Monad.Trans.Either (EitherT(..))
import qualified Control.Monad.Trans.Either as E
import           Control.Monad.Trans.Class (lift)

import qualified Data.Traversable as Tr
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.List (intercalate)
import qualified Data.IntMap.Strict as I
import           Data.Maybe (isJust)

import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.DisjSet as D


--------------------------------------------------------------------
-- Feature graph data structure
--------------------------------------------------------------------


-- TODO: We should try to hide some of the implemntation details
-- and not to export definitions of FG and Node (or perhaps to
-- export, but only in an `Internal` submodule).
-- All higher level operations should work on node identifiers
-- alone (i.e. they should not have access to node themselves).


-- | A feature graph with edges labeled by features of type `f`
-- and frontier nodes labeled with atomic values of type `a`.
data Graph f a = Graph {
    -- | A map from IDs to nodes.
      nodeMap   :: I.IntMap (Node f a)
    -- | A disjoint-set data structure, which keeps track of
    -- the node merging (in a way). 
    , disjSet   :: D.DisjSet ID
    } deriving (Show)


-- | A node in a feature graph.
data Node f a
    = Interior (M.Map f ID)
    | Frontier a
    deriving (Show, Eq, Ord)


-- | An empty graph.
empty :: Graph f a
empty = Graph I.empty D.empty


-- | Construct graph form an arbitrary list of nodes.
mkGraph :: [(ID, Node f a)] -> Graph f a
mkGraph xs = Graph (I.fromList xs) D.empty


-- | Clean the graph: replace every identifier with its representant.
-- As a result, the resultant graph will have an empty `disjSet`.
clean :: Graph f a -> Graph f a
clean g = fst . flip runGraphM g $ do
    nodeMap' <- mapM cleanPair $ I.toList $ nodeMap g
    return $ Graph (I.fromList nodeMap') D.empty
  where
    cleanPair (i, x) = (,) <$> getRepr i <*> cleanNode x
    cleanNode (Interior m) = Interior <$> Tr.mapM getRepr m
    cleanNode (Frontier x) = return $ Frontier x
    -- cleanSnd (x, i) = (x,) <$> getRepr i



-- | Check whether the two graphs are equal given node
-- identifiers which must correspond to each other.
equals
    :: (Eq f, Eq a)
    => Graph f a    -- ^ The first feature graph
    -> Graph f a    -- ^ The second feature graph
    -> [(ID, ID)]   -- ^ Nodes from the first and the second graph
                    --   respectively which should correspond to
                    --   each other.
    -> Bool
equals g h

    = isJust
    . flip S.evalState Set.empty
    . runMaybeT
    . mapM_ (uncurry checkIDs)

  where

    -- check that the two nodes are equal
    check (Interior p) (Interior q) = do
        guard $ M.size p == M.size q
        let xs = zip (M.toAscList p) (M.toAscList q)
        forM_ xs $ \((x, i), (y, j)) -> do
            guard $ x == y
            checkIDs i j
    check (Frontier x) (Frontier y) = guard $ x == y
    check _ _ = App.empty

    -- check that the two identifiers represent identical nodes
    checkIDs i0 j0 = do
        -- find representants
        let i = D.repr i0 $ disjSet g
            j = D.repr j0 $ disjSet h
        -- mark two states as equal; the function returns true
        -- if nodes were not marked as equal earlier
        b <- lift $ markEqual i j
        when b $ do
            -- this should not actually fail 
            p <- maybeT $ I.lookup i $ nodeMap g
            q <- maybeT $ I.lookup j $ nodeMap h
            check p q

--     -- mark two nodes as equal and return info if they were not
--     -- already marked as such (i.e. if marking was effective)
--     markEqual i j = lift $ S.state $ \s ->
--         if Set.member (i, j) s
--             then (False, s)
--             else (True, Set.insert (i, j) s)
        

-- | Check whether the two graphs are equal given node
-- identifiers which must correspond to each other.
equal
    :: (Eq f, Eq a)
    => Graph f a    -- ^ The first feature graph
    -> ID           -- ^ Node from the first graph
    -> Graph f a    -- ^ The second feature graph
    -> ID           -- ^ Node from the second graph
    -> Bool
equal g i h j = equals g h [(i, j)]


-- | Compare two graphs given nodes which should
-- correspond to each other.
compares
    :: (Ord f, Ord a)
    => Graph f a    -- ^ The first feature graph
    -> Graph f a    -- ^ The second feature graph
    -> [(ID, ID)]   -- ^ Nodes from the first and the second graph
                    --   respectively which should correspond to
                    --   each other.
    -> Ordering
compares g h

    = mkOrd
    . flip S.evalState Set.empty
    . E.runEitherT
    . mapM_ (uncurry checkIDs)

  where

    -- determine the final ordering
    mkOrd (Left o)  = o
    mkOrd (Right _) = EQ

    -- compare two nodes; compare with the analog from `equals`:
    -- here `guard` is replace with `match` while `App.empty`
    -- had to be avoided
    check (Interior p) (Interior q) = do
        match (M.size p) (M.size q)
        let xs = zip (M.toAscList p) (M.toAscList q)
        forM_ xs $ \((x, i), (y, j)) -> do
            match x y
            checkIDs i j
    check (Frontier x) (Frontier y) = match x y
    check (Frontier _) (Interior _) = E.left LT
    check (Interior _) (Frontier _) = E.left GT

    -- an analog of `guard` for the EitherT monad
    match x y = case compare x y of
        EQ -> E.right ()
        z  -> E.left z

    -- compare two nodes represented by their identifiers;
    -- note that implementation of this function is very similar
    -- to the one within `equals`.
    checkIDs i0 j0 = do
        -- find representants
        let i = D.repr i0 $ disjSet g
            j = D.repr j0 $ disjSet h
        -- mark two states as equal; the function returns true
        -- if nodes were not marked as equal earlier
        b <- lift $ markEqual i j
        when b $ do
            -- TODO: well, these should actualy fail with error!
            p <- maybeE LT $ I.lookup i $ nodeMap g
            q <- maybeE GT $ I.lookup j $ nodeMap h
            check p q


-- | Compare two graphs given node identifiers which must
-- correspond to each other.
compare'
    :: (Ord f, Ord a)
    => Graph f a    -- ^ The first feature graph
    -> ID           -- ^ Node from the first graph
    -> Graph f a    -- ^ The second feature graph
    -> ID           -- ^ Node from the second graph
    -> Ordering
compare' g i h j = compares g h [(i, j)]


-- | Mark two nodes as equal and return info if they were not
-- already marked as such (i.e. if marking was effective).
-- A utility function for both `compares` and `equals`.
markEqual :: ID -> ID -> S.State (Set.Set (ID, ID)) Bool
markEqual i j = S.state $ \s ->
    if Set.member (i, j) s
        then (False, s)
        else (True, Set.insert (i, j) s)


--------------------------------------------------------------------
-- Graph monad
--------------------------------------------------------------------


-- | A feature graph monad transformer. 
type GraphT f a = S.StateT (Graph f a)


-- | A feature graph monad (a transformer over Identify monad).
type GraphM f a = S.StateT (Graph f a) Identity


-- | Run the graph monad trasformer.
runGraphT :: GraphT f a m b -> Graph f a -> m (b, Graph f a)
runGraphT = S.runStateT


-- | Run the graph monad.
runGraphM :: GraphM f a b -> Graph f a -> (b, Graph f a)
runGraphM = S.runState


--------------------------------------------------------------------
-- Graph monad: interface
--------------------------------------------------------------------


-- | Identify the current representant of the node.
getRepr :: (Functor m, Monad m) => ID -> GraphT f a m ID
getRepr k = D.repr k <$> S.gets disjSet


-- | Set the representant of the node.
mkReprOf :: (Monad m) => ID -> ID -> GraphT f a m ()
mkReprOf x y = S.modify $ \g@Graph{..} ->
        g {disjSet = D.mkReprOf x y disjSet}


-- | Retrieve node hidden behind the given identifier.
getNode :: (Functor m, Monad m) => ID -> GraphT f a m (Maybe (Node f a))
getNode i = I.lookup <$> getRepr i <*> S.gets nodeMap


-- | Set node under the given identifier.
setNode :: Monad m => ID -> Node f a -> GraphT f a m ()
setNode i x = S.modify $ \g@Graph{..} ->
    g {nodeMap = I.insert i x nodeMap}


-- | Remove node under the given identifier.
remNode :: Monad m => ID -> GraphT f a m ()
remNode i = S.modify $ \g@Graph{..} ->
    g {nodeMap = I.delete i nodeMap}


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  We assume, that identifiers
-- in the graphs are disjoint.
fromTwo :: Graph f a -> Graph f a -> Graph f a
fromTwo f g = Graph
    { nodeMap   = I.union (nodeMap f) (nodeMap g)
    , disjSet   = D.union (disjSet f) (disjSet g) }


-- -- | Join two feature graphs.  Nodes from the first graph will be
-- -- marked as `Left`s, nodes from the second one -- as `Right`s. 
-- fromTwo :: Ord i => FG i f a -> FG i f a -> FG (Either i i) f a
-- fromTwo f g = M.fromAscList $
--     (M.toAscList $ mapIDsMono Left f) ++
--     (M.toAscList $ mapIDsMono Right g)
-- 
-- 
-- -- | Map keys of the feature graph using a strictly monotonic function.
-- mapIDsMono :: Ord j => (i -> j) -> FG i f a -> FG j f a
-- mapIDsMono f m = M.fromAscList
--     [ (f k, mapNodeIDs f v)
--     | (k, v) <- M.toAscList m ]
-- 
-- 
-- -- | Map identifiers of the node using a strictly monotonic function.
-- mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
-- mapNodeIDs f (Interior m) = Interior $ M.map f m
-- mapNodeIDs _ (Frontier x) = Frontier x
-- {-# INLINE mapNodeIDs #-}


--------------------------------------------------------------------
-- Graph printing
--------------------------------------------------------------------


-- | Print information about the graph into stdout.
printGraph :: (Show f, Show a) => Graph f a -> IO ()
printGraph Graph{..} = do
    putStrLn "# node map"
    forM_ (I.toList nodeMap) $ \(i, nd) -> case nd of
        Frontier x  -> do
            putStrLn $ "# frontier " ++ show i ++ " => " ++ show x
        Interior m  -> do
            putStrLn $ "# interior " ++ show i
            forM_ (M.toList m) print
    putStrLn "# disjoint-set"
    D.printDisjSet disjSet


-- | Print information about the graph into stdout.  Alternative version.
printTree :: (Show f, Show a) => Graph f a -> ID -> IO ()
printTree Graph{..} =
    -- flip S.runStateT S.empty . doit ""
    doit ""
  where
    doit ind i = case I.lookup (D.repr i disjSet) nodeMap of
        Nothing -> return ()
        Just nd -> do
            putStrLn $ show i ++ " ("
                ++ show (D.repr i disjSet) ++ ")"
            case nd of
                Interior m  -> do
                    forM_ (M.toList m) $ putFeat ind
                Frontier y  -> do
                    putStrLn $ ind ++ " " ++ show y
    putFeat ind (x, j) = do
        putStr $ ind ++ "  " ++ show x ++ " -> "
        doit (ind ++ "    ") j 


-- | Show the graph in one line.
showFlat :: (Show f, Show a) => Graph f a -> ID -> String
showFlat Graph{..} =
    enclose "[" "]" . doit
  where
    enclose l r x = l ++ x ++ r
    doit i = case I.lookup (D.repr i disjSet) nodeMap of
        Nothing -> ""
        Just nd ->
            "" -- show i ++ "(" ++ show (D.repr i disjSet) ++ ")"
         ++ ( case nd of
                Interior m -> intercalate ", "
                    $ map putFeat $ M.toList m
                Frontier y -> show y )
    putFeat (x, j) = show x ++ "=" ++ doit j


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Lift a maybe value to a MaybeT transformer.
maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return


-- | Lift a maybe value to a EitherT transformer.
maybeE :: Monad m => e -> Maybe a -> E.EitherT e m a
maybeE _ (Just x) = E.right x
maybeE e Nothing  = E.left e



-- swap :: (a, b) -> (b, a)
-- swap (x, y) = (y, x)
-- {-# INLINE swap #-}
