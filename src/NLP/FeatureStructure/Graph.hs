{-# LANGUAGE TupleSections #-}

-- | A graph-based representation of a feature structure.
--
-- At this point we assume that values are `atomic` and the only
-- operation defined over them is the equality check.


module NLP.FeatureStructure.Graph
(
-- * Basics
  FG
, Node (..)
, edgeMap

-- -- * Equivalence
-- , UM
-- , uniCls
-- 
-- -- * Tests
-- , test1
-- , test2
-- , test3
-- , test4
-- , test5
) where


import           Control.Applicative (Applicative(..), pure, (<$>), (<*>))
import           Control.Monad (when)
import           Data.These

import qualified Data.Map.Strict as M
-- import qualified Data.IntMap.Strict as I
-- import qualified Data.Partition as P
import qualified Control.Monad.State.Strict as S


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


-- | List edges outgoing from a given node.  Return empty map for
-- non-existent identifiers and frontier nodes.
edgeMap :: Ord i => i -> FG i f a -> M.Map f i
edgeMap k fg = case M.lookup k fg of
    Nothing -> M.empty
    Just n  -> case n of
        Interior m  -> m
        Frontier _  -> M.empty


--------------------------------------------------------------------
-- Joing two feature graphs
--------------------------------------------------------------------


-- | Join two feature graphs.  Nodes from the first graph will be
-- marked as `Left`s, nodes from the second one -- as `Right`s. 
fromTwo :: FG i f a -> FG i f a -> FG (Either i i) f a
fromTwo = undefined


--------------------------------------------------------------------
-- Unification monad
--------------------------------------------------------------------


-- | Unification monad (dummy).  We assume, among others, that
-- there is a `MaybeT` in the monad stack, so that we can
-- easily represent "what's the name" (computation cut-off?)
-- in the algorithm.  But perhaps `MaybeT` is not needed at
-- every level?
-- Error should be representable as well, since unification
-- may fail. 
data UM i f a b = UM


-- | TODO: proper instances
instance Functor (UM i f a)
instance Applicative (UM i f a)
instance Monad (UM i f a)


-- | Pop the node pair from the queue.  TODO: By the way, it seems
-- that it is crucial for the algorithm that this is a queue, and
-- not a stack, for example.  It should be explicit!
pop :: UM i f a (i, i)
pop = undefined


-- | Return the representant of the given node.
repr :: i -> UM i f a i
repr = undefined


-- | Cut-off (TODO: what's the proper name of that?) the computation.
pass :: UM i f a b
pass = undefined


-- | Unification fail.
uniFail :: UM i f a b
uniFail = undefined


-- | Node behind the identifier.
node :: i -> UM i f a (Node i f a)
node = undefined


-- | Set the representant of the node.
mkReprOf :: i -> i -> UM i f a ()
mkReprOf = undefined


-- | Set node under the given identifier.
setNode :: i -> Node i f a -> UM i f a ()
setNode = undefined


-- | Remove node under the given identifier.
remNode :: i -> UM i f a ()
remNode = undefined


--------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------


-- | A node with corresponding identifier.
type NodeID i f a = (i, Node i f a)


-- | A graph with a selected node.
type NodeFG i f a = (i, FG i f a)


-- | Unify two feature graphs.
unify :: NodeFG i f a -> NodeFG i f a -> NodeFG (Either i i) f a
unify (i0, f0) (j0, g0) =
    (i, g)  -- TODO: implement
  where
    -- Joined graphs and corresponding roots.
    g = fromTwo f0 g0
    i = Left i0
    j = Right j0


-- | One step of the iterative unification process.
-- As arguments, two nodes which are to be merged
-- are given.
-- TODO: Perhaps we should supply the function with the
-- node pair; it might be more elemegant in the end.
mergeTop :: (Eq i, Eq a) => UM i f a ()
mergeTop = do
    -- Pop the node pair from the queue.
    (i, j) <- popRepr
    -- Perhaps the nodes have been already merged?  TODO: check,
    -- if this step is neccesseary.
    when (i == j) pass
    -- | Merge the nodes behind the identifiers (if possible).
    p <- node i
    q <- node j
    mergeNodes (i, p) (j, q)


-- | Pop nodes from the queue and return their representants.
popRepr :: UM i f a (i, i)
popRepr = do
    (i, j) <- pop
    (,) <$> repr i <*> repr j


-- | Merge the two given nodes.
mergeNodes :: Eq a => NodeID i f a -> NodeID i f a -> UM i f a ()
mergeNodes (i, n) (j, m) =
    doit n m
  where
    doit (Interior p) (Interior q) = do
        setNode i $ Interior $ mergeEdgeMaps p q
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


-- | Compute the union of the two given edge maps.  In case
-- of overlapping, the node identifier from the first map
-- is chosen (but this behaviour is arbitrary and could be
-- easily changed without the impact on the correctness
-- of the algorithm).
mergeEdgeMaps :: M.Map f i -> M.Map f i -> M.Map f i
mergeEdgeMaps = undefined


-- --------------------------------------------------------------------
-- -- Monad
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Equivalence computation monad.
-- type EqM a b = S.State (P.Partition a) b
-- 
-- 
-- -- | Join two elements.
-- join :: Ord a => a -> a -> EqM a ()
-- join x y = S.modify $ P.join x y
-- 
-- 
-- -- | Are the two elements equivalent?
-- equivalent :: Ord a => a -> a -> EqM a Bool
-- equivalent x y = do
--     par <- S.get
--     return $ (P.rep par x) == (P.rep par y)
-- 
-- 
-- -- | Element representative.
-- repr :: Ord a => a -> EqM a a
-- repr x = P.rep <$> S.get <*> pure x
-- 
-- 
-- -- | Unification monad.
-- type UM b = EqM (Either NodeID NodeID) b
-- 
-- 
-- --------------------------------------------------------------------
-- -- Equivalence relation
-- --------------------------------------------------------------------
-- 
-- 
-- -- | A graph with a selected node.
-- type NodeFG a b = (NodeID, FG a b)
-- 
-- 
-- -- | Given two feature graphs and corresponding nodes, compute
-- -- equivalence classes which would result from a unification
-- -- of the two graphs.
-- uniCls :: Ord a => NodeFG a b -> NodeFG a b -> UM ()
-- uniCls (k1, fg1) (k2, fg2) =
--     -- TODO: still seems to be a bit controversial, though...
--     unlessM (equivalent p q) $ do
--         join p q
--         forM_ common $ uncurry uniCls
--   where
--     p = Left k1
--     q = Right k2
--     common = M.elems $ M.intersectionWith mkPair
--         (edgeMap k1 fg1)
--         (edgeMap k2 fg2)
--     mkPair x y =
--         ( (x, fg1)      -- Add context to corresponding
--         , (y, fg2) )    -- node identifiers.
-- 
-- 
-- --------------------------------------------------------------------
-- -- Output graph
-- --------------------------------------------------------------------
-- 
-- 
-- -- -- | Given the equivalence classes (stored in the state monad),
-- -- -- construct the resultant graph.
-- -- joinBoth :: Ord a => NodeFG a b -> NodeFG a b -> UM NodeID
-- -- joinBoth (k1, fg1) (k2, fg2) = do
-- --     k  <- addNode $ repr $ Left k1
-- --     ks <- forM_ children $ \ths -> case ths of
-- --         These x y   -> joinBoth (x, fg1) (y, fg2)
-- --         This x      -> joinLeft (x, fg1)
-- --         That y      -> joinRight         (y, fg2)
-- --     addEdges k ks
-- --     return k
-- --   where
-- --     children = interleave
-- --         (M.toList $ edgeMap k1 fg1)
-- --         (M.toList $ edgeMap k2 fg2)
-- -- 
-- -- 
-- -- joinLeft :: Ord a => NodeFG a b -> UM NodeID
-- -- joinLeft (k1, fg1) = do
-- --     k  <- addNode $ repr $ Left k1
-- --     ks <- forM_ children $ \ths -> case ths of
-- --         These x y   -> joinBoth (x, fg1) (y, fg2)
-- --         This x      -> joinLeft (x, fg1)
-- --         That y      -> joinRight         (y, fg2)
-- --     addEdges k ks
-- --     return k
-- --   where
-- --     children = interleave
-- --         (M.toList $ edgeMap k1 fg1)
-- --         (M.toList $ edgeMap k2 fg2)
-- 
-- 
-- --------------------------------------------------------------------
-- -- Tests
-- --------------------------------------------------------------------
-- 
-- 
-- test1 :: IO ()
-- test1 = do
--     mapM_ print $ P.nontrivialSets par
--   where
--     par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
--     f1 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2)])
--         , (2, Frontier 'x') ]
--     f2 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3)])
--         , (2, Frontier 'x')
--         , (3, Frontier 'y') ]
-- 
-- 
-- test2 :: IO ()
-- test2 = do
--     mapM_ print $ P.nontrivialSets par
--   where
--     par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
--     f1 = I.fromList
--         [(1, Interior $ M.fromList [('a', 1)])]
--     f2 = I.fromList
--         [ (1, Interior $ M.fromList [('a', 2)])
--         , (2, Frontier 'x') ]
-- 
-- 
-- test3 :: IO ()
-- test3 = do
--     mapM_ print $ P.nontrivialSets par
--   where
--     par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
--     f1 = I.fromList
--         [(1, Interior $ M.fromList [('a', 1)])]
--     f2 = I.fromList
--         [(1, Interior $ M.fromList [('a', 1)])]
-- 
-- 
-- test4 :: IO ()
-- test4 = do
--     mapM_ print $ P.nontrivialSets par
--   where
--     par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
--     f1 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2), ('c', 3)])
--         , (2, Frontier 'x')
--         , (3, Frontier 'y') ]
--     f2 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3), ('c', 3)])
--         , (2, Frontier 'x')
--         , (3, Frontier 'y') ]
-- 
-- 
-- test5 :: IO ()
-- test5 = do
--     mapM_ print $ P.nontrivialSets par
--   where
--     par = flip S.execState P.empty $ uniCls (1, f1) (1, f2)
--     f1 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 2), ('c', 3)])
--         , (2, Frontier 'x')
--         , (3, Interior $ M.fromList [('a', 4)])
--         , (4, Frontier 'x') ]
--     f2 = I.fromList
--         [ (1, Interior $ M.fromList
--             [('a', 2), ('b', 3), ('c', 3)])
--         , (2, Interior $ M.fromList [('a', 4)])
--         , (3, Frontier 'x')
--         , (4, Frontier 'x') ]
-- 
-- 
-- --------------------------------------------------------------------
-- -- Misc
-- --------------------------------------------------------------------
-- 
-- 
-- -- | A version of the `unless` function with a monadic argument.
-- unlessM :: Monad m => m Bool -> m () -> m ()
-- unlessM m n = do
--     b <- m
--     unless b n


-- | Interleave two lists given in an ascending order.
interleave :: Ord a => [a] -> [a] -> [These a a]
interleave xxs@(x:xs) yys@(y:ys)
    | x < y     = This x    : interleave xs yys
    | x > y     = That y    : interleave xxs ys
    | otherwise = These x y : interleave xs ys
interleave xs [] = map This xs
interleave [] ys = map That ys
