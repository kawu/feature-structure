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
, getNode
, getIDs

-- * Equality 
, equal
, equals

-- * Comparison
, compare'
, compares

-- * Utility
, mapIDs
, fromTwo
, trim
-- , printGraph
, printTree
, showFlat
) where


import           Prelude hiding (log)

import           Control.Applicative ((<$>))
import qualified Control.Applicative as App
import           Control.Monad (forM_, when, guard)
-- import           Control.Monad.Identity (Identity)
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Trans.Maybe (MaybeT(..))
-- import           Control.Monad.Trans.Either (EitherT(..))
import qualified Control.Monad.Trans.Either as E
import           Control.Monad.Trans.Class (lift)

-- import qualified Data.Traversable as Tr
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.List (intercalate)
-- import qualified Data.IntMap.Strict as I
import           Data.Maybe (isJust, catMaybes)


--------------------------------------------------------------------
-- Feature graph data structure
--------------------------------------------------------------------
  
  
-- | A feature graph with edges labeled by features of type `f`
-- and frontier nodes labeled with atomic values of type `a`.
newtype Graph i f a = Graph {
      nodeMap   :: M.Map i (Node i f a)
    } deriving (Show, Eq, Ord)


-- | A node in a feature graph.
data Node i f a
    = Interior (M.Map f i)
    | Frontier a
    deriving (Show, Eq, Ord)


-- | An empty graph.
empty :: Graph i f a
empty = Graph M.empty


-- | Retrieve node under the given identifier.
getNode :: Ord i => i -> Graph i f a -> Maybe (Node i f a)
getNode i = M.lookup i . nodeMap


-- | Get the set of node identifiers in the graph.
getIDs :: Ord i => Graph i f a -> Set.Set i
getIDs = M.keysSet . nodeMap


-- | Check whether the two graphs are equal given node
-- identifiers which must correspond to each other.
equals
    :: (Ord i, Ord j, Eq f, Eq a)
    => Graph i f a      -- ^ The first feature graph
    -> Graph j f a      -- ^ The second feature graph
    -> [(i, j)]   -- ^ Nodes from the first and the second graph
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
    checkIDs i j = do
        -- mark two states as equal; the function returns true
        -- if nodes were not marked as equal earlier
        b <- lift $ markEqual i j
        when b $ do
            -- this should not actually fail 
            p <- maybeT $ getNode i g
            q <- maybeT $ getNode j h
            check p q

-- | Check whether the two graphs are equal given node
-- identifiers which must correspond to each other.
equal
    :: (Ord i, Ord j, Eq f, Eq a)
    => Graph i f a  -- ^ The first feature graph
    -> i         -- ^ Node from the first graph
    -> Graph j f a  -- ^ The second feature graph
    -> j         -- ^ Node from the second graph
    -> Bool
equal g i h j = equals g h [(i, j)]


-- | Compare two graphs given nodes which should
-- correspond to each other.
compares
    :: (Ord i, Ord j, Ord f, Ord a)
    => Graph i f a  -- ^ The first feature graph
    -> Graph j f a  -- ^ The second feature graph
    -> [(i, j)] -- ^ Nodes from the first and the second graph
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
    checkIDs i j = do
        -- mark two states as equal; the function returns true
        -- if nodes were not marked as equal earlier
        b <- lift $ markEqual i j
        when b $ do
            -- TODO: well, these should actualy fail with error!
            p <- maybeE LT $ getNode i g
            q <- maybeE GT $ getNode j h
            check p q


-- | Compare two graphs given node identifiers which must
-- correspond to each other.
compare'
    :: (Ord i, Ord j, Ord f, Ord a)
    => Graph i f a  -- ^ The first feature graph
    -> i         -- ^ Node from the first graph
    -> Graph j f a  -- ^ The second feature graph
    -> j         -- ^ Node from the second graph
    -> Ordering
compare' g i h j = compares g h [(i, j)]


-- | Mark two nodes as equal and return info if they were not
-- already marked as such (i.e. if marking was effective).
-- A utility function for both `compares` and `equals`.
markEqual :: (Ord i, Ord j) => i -> j -> S.State (Set.Set (i, j)) Bool
markEqual i j = S.state $ \s ->
    if Set.member (i, j) s
        then (False, s)
        else (True, Set.insert (i, j) s)


--------------------------------------------------------------------
-- Join two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  Nodes from the first graph will be
-- marked as `Left`s, nodes from the second one -- as `Right`s. 
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


-- | Map keys of the feature graph.
mapIDs :: Ord j => (i -> j) -> Graph i f a -> Graph j f a
mapIDs f (Graph m) = Graph $ M.fromList
    [ (f k, mapNodeIDs f v)
    | (k, v) <- M.toList m ]


-- | Map identifiers of the node.
mapNodeIDs :: (i -> j) -> Node i f a -> Node j f a
mapNodeIDs f (Interior m) = Interior $ M.map f m
mapNodeIDs _ (Frontier x) = Frontier x
{-# INLINE mapNodeIDs #-}


--------------------------------------------------------------------
-- Graph trimming
--------------------------------------------------------------------


-- | Certain graph-modifying operations, e.g. unification, do not
-- touch the set(s) of nodes of the underlying graph(s).  Some nodes
-- are merged with others, true, but in the end we can use each of
-- the identifiers of the input graph in order to find its
-- corresponding node in the output graph (see `Res` data type).
--
-- Sometimes it may be useful, though, to drop parts of information
-- of the underlying structure, e.g. nodes no longer reachable from
-- the given set of nodes wchich are of interest at the moment.
-- This function allows to remove such nodes given the set of IDs
-- of nodes of interest.
trim
    :: Ord i
    => Graph i f a  -- ^ The underlying graph 
    -> [i]          -- ^ The set of interesting nodes: in the
                    --  resultant graph only nodes reachable from
                    --  this set will be preserved.
    -> Graph i f a
trim g roots = Graph $ M.fromList $ catMaybes
    [ (i,) <$> getNode i g
    | i <- Set.toList reachable ]
  where
    reachable
        = flip S.execState Set.empty
        $ mapM_ visitID roots
    visitID i = do
        b <- isVisited i
        when (not b) $ do
            markVisited i
            case getNode i g of
                Just n  -> visitNode n
                Nothing -> return ()
    visitNode (Interior m) = mapM_ visitID $ M.elems m
    visitNode (Frontier _) = return ()
    isVisited i = S.gets $ Set.member i
    markVisited i = S.modify $ Set.insert i


--------------------------------------------------------------------
-- Graph printing
--------------------------------------------------------------------


-- | Show the graph in one line.
showFlat
    :: (Ord i, Show i, Show f, Show a)
    => Graph i f a -> i -> String
showFlat g =
    enclose "[" "]" . doit
  where
    enclose l r x = l ++ x ++ r
    doit i = case getNode i g of
        Nothing -> ""
        Just nd ->
            "" -- show i ++ "(" ++ show (D.repr i disjSet) ++ ")"
         ++ ( case nd of
                Interior m -> intercalate ", "
                    $ map putFeat $ M.toList m
                Frontier y -> show y )
    putFeat (x, j) = show x ++ "=" ++ doit j


-- | Print information about the graph into stdout.  Alternative version.
printTree
    :: (Ord i, Show i, Show f, Show a)
    => Graph i f a -> i -> IO ()
printTree Graph{..} =
    doit ""
  where
    doit ind i = case M.lookup i nodeMap of
        Nothing -> error "Graph.printTree: unknown ID"
        Just nd -> do
            putStrLn $ show i
            case nd of
                Interior m  -> do
                    forM_ (M.toList m) $ putFeat ind
                Frontier y  -> do
                    putStrLn $ ind ++ " " ++ show y
    putFeat ind (x, j) = do
        putStr $ ind ++ "  " ++ show x ++ " -> "
        doit (ind ++ "    ") j


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
