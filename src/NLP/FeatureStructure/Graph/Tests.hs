{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.Graph.Tests where


import           Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map.Strict as M 
import           Data.Maybe (isJust)

import qualified Test.SmallCheck         as SC
import qualified Test.SmallCheck.Series  as SC
import qualified Test.Tasty.SmallCheck   as SC
import qualified Test.QuickCheck         as QC
import qualified Test.Tasty.QuickCheck   as QC
import           Test.Tasty              (TestTree, testGroup, localOption)
import           Test.HUnit              (Assertion, (@?=))
import           Test.Tasty.HUnit        (testCase)
import           Test.Tasty.SmallCheck (SmallCheckDepth (..))

import qualified NLP.FeatureStructure.Graph as G


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


-- | The actual test set.
tests :: TestTree
tests = opts $ testGroup "NLP.FeatureStructure.Graph"
    [ testCase "testTrim" testTrim
    , testCase "testEq1" testEq1
    -- , testCase "testEq2" testEq2
    , testCase "testCmp1" testCmp1
    , testCase "testAntiSym1" testAntiSym1
    -- , testCase "testSub1" testSub1
    , QC.testProperty "arbitrary graph valid"
        (G.valid . toGraph :: SGraph -> Bool)
    , QC.testProperty "graph equals to itself" eqItself
    , QC.testProperty "size of mapIDs (+1) the same" checkMapIDs
    , QC.testProperty "comparison is antysymmetric" checkAntiSymm
    , SC.testProperty "comparison is antysymmetric (smallcheck)" checkAntiSymm'
    , QC.testProperty "transitivity of the comparison" checkTrans
    -- , QC.testProperty "correctness of `fromTwo`" checkTwo
    , QC.testProperty "dummy trimming doesn't remove nodes" checkTrimNo
    , QC.testProperty "full trimming remove all nodes" checkTrimAll ]
  where
    opts = localOption (SmallCheckDepth 2)


--------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------


-- | Check that trimming works properly.
testTrim :: Assertion
testTrim = do
    let b = G.equal g2 3 g3 3
    G.size g3 @?= 4
    b @?= True
  where
--     mkG = G.Graph . M.fromList
--     mkI = G.Interior . M.fromList
--     mkF = G.Frontier
    g1 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkF 'a')
        , (3, mkI [('a', 2), ('b', 4)])
        , (4, mkI [('a', 5)])
        , (5, mkI [('a', 3)]) ]
    g2 = mkG
        [ (2, mkF 'a')
        , (3, mkI [('a', 2), ('b', 4)])
        , (4, mkI [('a', 5)])
        , (5, mkI [('a', 3)]) ]
    g3 = G.trim g1 [4]


-- | A more tricky test where equality check should fail.
testEq1 :: Assertion
testEq1 = do
    G.equal g1 1 g2 1 @?= False
  where
    g1 :: G.Graph Int Char ()
    g1 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI [('a', 1)]) ]
    g2 = mkG
        [ (1, mkI [('a', 1)]) ]


-- -- | Another tricky test where equality check should,
-- -- suprisingly, not fail.  The two graphs are equal from the
-- -- unification point of view nevertheless.
-- testEq2 :: Assertion
-- testEq2 = do
--     G.equal g1 1 g2 1 @?= True
--   where
--     g1 = mkG
--         [ (1, mkI [('a', 2), ('b', 3)])
--         , (2, mkF 'a')
--         , (3, mkF 'a') ]
--     g2 = mkG
--         [ (1, mkI [('a', 2), ('b', 2)])
--         , (2, mkF 'a') ]


-- | Similar to 'testEq1' but with comparison.
testCmp1 :: Assertion
testCmp1 = do
    G.compare' g1 1 g2 1 @?= GT
    G.compare' g2 1 g1 1 @?= LT
  where
    g1 :: G.Graph Int Char ()
    g1 = mkG
        [ (1, mkI [('a', 2)])
        , (2, mkI [('a', 1)]) ]
    g2 = mkG
        [ (1, mkI [('a', 1)]) ]


-- | A concrete example to check that the relation is
-- antisymmetric.
testAntiSym1 :: Assertion
testAntiSym1 = do
    G.compare' g1 1 g2 1 @?= LT
    G.compare' g2 1 g1 1 @?= GT
  where
    g1 :: G.Graph Int Char ()
    g1 = mkG
        [ (1, mkI [('x', 2), ('y', 3), ('z', 2)])
        , (2, mkI [])
        , (3, mkI []) ]
    g2 = mkG
        [ (1, mkI [('x', 2), ('y', 3), ('z', 3)])
        , (2, mkI [])
        , (3, mkI []) ]


-- -- | Subsumption test.
-- testSub1 :: Assertion
-- testSub1 = do
--     G.subsumes g1 1 g2 1 @?= True
--     G.subsumes g2 1 g1 1 @?= False
--   where
--     g1 :: G.Graph Int Char ()
--     g1 = mkG
--         [ (1, mkI [('a', 2)])
--         , (2, mkI [('a', 1)]) ]
--     g2 = mkG
--         [ (1, mkI [('a', 1)]) ]


--------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------


-- | Check that trimming removes all nodes if we trim
-- w.r.t. an empty set of nodes.
checkTrimAll :: SGraphID -> Bool
checkTrimAll = G.null . flip G.trim [] . fst . toGraphID


-- | Check that trimming doesn't remove any nodes if we trim
-- w.r.t. all nodes in the input graph.
checkTrimNo :: SGraphID -> Bool
checkTrimNo s =
    G.equal g i g' i
  where
    (g, i) = toGraphID s
    g' = G.trim g $ S.toList $ G.getIDs g


-- -- | Check that `G.fromTwo` produces a graph containing all
-- -- nodes from the two input graphs and that it doesn't contain
-- -- any other nodes.
-- checkTwo :: SGraph -> SGraph -> Bool
-- checkTwo s1' s2' =
--     check s1 Left && check s2 Right &&
--     G.size s1 + G.size s2 == G.size r
--   where
--     s1 = toGraph s1'
--     s2 = toGraph s2'
--     r = G.fromTwo s1 s2
--     check s f = and
--         [ isJust $ G.getNode (f i) r
--         | i <- S.toList $ G.getIDs s ]


-- | Check that the comparison is antysymettric.
checkAntiSymm :: SGraphID -> SGraphID -> Bool
checkAntiSymm s1 s2 =
    if G.compare' g1 i1 g2 i2 /= GT &&
       G.compare' g2 i2 g1 i1 /= GT
        then G.equal g1 i1 g2 i2
        else True
  where
    (g1, i1) = toGraphID s1
    (g2, i2) = toGraphID s2


-- | Check that the comparison is antysymettric.
checkAntiSymm'
    :: GraphID Int Int Char Char
    -> GraphID Int Int Char Char
    -> Bool
checkAntiSymm' s1 s2 =
    if G.compare' g1 i1 g2 i2 /= GT &&
       G.compare' g2 i2 g1 i1 /= GT
        then G.equal g1 i1 g2 i2
        else True
  where
    (g1, i1) = toGraphID s1
    (g2, i2) = toGraphID s2


-- | Transitivity of the comparison.
checkTrans :: SGraphID -> SGraphID -> SGraphID -> Bool
checkTrans s1 s2 s3 =
    check c12 c13 c23
  where
    check LT x LT = x == LT
    check _ _ _ = True
    (g1, i1) = toGraphID s1
    (g2, i2) = toGraphID s2
    (g3, i3) = toGraphID s3
    c12 = G.compare' g1 i1 g2 i2
    c13 = G.compare' g1 i1 g3 i3
    c23 = G.compare' g2 i2 g3 i3


-- -- | Transitivity of the comparison.
-- checkTrans'
--     :: GraphID Int Int Char Char
--     -> GraphID Int Int Char Char
--     -> GraphID Int Int Char Char
--     -> Bool
-- checkTrans' s1 s2 s3 =
--     check c12 c13 c23
--   where
--     check LT x LT = x == LT
--     check _ _ _ = True
--     (g1, i1) = toGraphID s1
--     (g2, i2) = toGraphID s2
--     (g3, i3) = toGraphID s3
--     c12 = G.compare' g1 i1 g2 i2
--     c13 = G.compare' g1 i1 g3 i3
--     c23 = G.compare' g2 i2 g3 i3


-- | Size of the result of mapIDs (+1) should not change.
-- Moreover, the result should still be valid (not corrupted).
-- Finally, the two graph should be equal to each other.
checkMapIDs :: SGraphID -> Bool
checkMapIDs s0 =
    let plus1 (SInt x) = SInt $ x + 1
        modID (Left i) = Left $ plus1 i
        modID (Right j) = Right $ plus1 j
        (g1, i1) = toGraphID s0
        (g2, i2) = ( G.mapIDs modID g1
                   , modID i1 )
    in  G.size g1 == G.size g2 &&
        G.valid g2 && G.equal g1 i1 g2 i2


-- | Is the graph equal to itself?
eqItself :: SGraphID -> Bool
eqItself s =
    let (g, i) = toGraphID s
    in  G.equal g i g i


--------------------------------------------------------------------
-- Alternative graph representation
--------------------------------------------------------------------


-- | Graph parametrized with SInts.
type SGraph = Graph SInt SInt SInt SInt


-- | SGraph with info about the root.
type SGraphID = GraphID SInt SInt SInt SInt


-- | Alternative definition of a graph, should be easier to
-- generate.
data Graph i j a b = Graph {
      nodes     :: S.Set i
    , edges     :: M.Map (i, a) (Either i j)
    , leaves    :: M.Map j b }
    deriving (Show)


-- | Graph with info about the root.
data GraphID i j a b = GraphID
    { graph :: Graph i j a b 
    , root  :: i }
    deriving (Show)


-- | Number of nodes in the graph.
numNodes :: Graph i j a b -> Int
numNodes Graph{..} = S.size nodes


-- | An empty graph.
empty :: Graph i j a b
empty = Graph S.empty M.empty M.empty


--------------------------------------------------------------------
-- QuickCheck Instances
--------------------------------------------------------------------


-- | Construct an edge based on the given sets of
-- underlying nodes.
genEdge
    :: QC.Arbitrary a
    => S.Set i -> S.Set j
    -> QC.Gen ((i, a), Either i j)
genEdge nodeSet leafSet = do
    let is = S.toList nodeSet
        js = S.toList leafSet
    i <- QC.elements is
    x <- QC.arbitrary
    v <- chooseEither
        (QC.elements is)
        (QC.elements js)
    return ((i, x), v)


instance ( Ord i, Ord j, QC.Arbitrary i, QC.Arbitrary j
         , Ord a, QC.Arbitrary a, QC.Arbitrary b )
        => QC.Arbitrary (Graph i j a b) where
    arbitrary = QC.sized $ \n -> QC.sized $ \m -> do
        -- we want the set of nodes to be always non-empty
        nodeSet <- S.fromList <$> QC.vector (max n 1)
        leafSet <- S.fromList <$> QC.vector m
        edges'  <- M.fromList <$> QC.listOf (genEdge nodeSet leafSet)
        leaves' <- M.fromList <$> sequence
            [ (j,) <$> QC.arbitrary
            | j <- S.toList leafSet ]
        return $ Graph nodeSet edges' leaves'


instance ( Ord i, Ord j, QC.Arbitrary i, QC.Arbitrary j
         , Ord a, QC.Arbitrary a, QC.Arbitrary b )
        => QC.Arbitrary (GraphID i j a b) where
    arbitrary = do
        g <- QC.arbitrary
        i <- QC.elements $ S.toList $ nodes g
        return $ GraphID g i


--------------------------------------------------------------------
-- SmallCheck Instances
--------------------------------------------------------------------


instance Monad m => SC.Serial m (Graph Int Int Char Char) where
--     series = SC.generate $ \k -> take 10 ( genGraphs
--         (S.fromList ['a', 'b'])
--         (S.fromList ['x', 'y']) k )
    series = SC.generate $ genGraphs
        (S.fromList ['a', 'b'])
        (S.fromList ['x', 'y'])


instance Monad m => SC.Serial m (GraphID Int Int Char Char) where
    series = do
        g <- SC.series
        i <- choose $ S.toList $ nodes g
        return $ GraphID g i


-- | Choose an element from the input list.
choose :: Monad m => [a] -> SC.Series m a
choose = SC.generate . const


--------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------


-- | Turn the graph into the graph actually used in the library.
toGraph
    :: (Ord i, Ord j, Ord f)
    => Graph i j f a
    -> G.Graph (Either i j) f a
toGraph Graph{..} = G.Graph $
    part1 `M.union` part2
  where
    part1 = M.fromListWith union $
        [ (Left i, G.Interior (M.singleton x j))
        | ((i, x), j) <- M.toList edges ] ++
        [ (Left i, G.Interior M.empty)
        | i <- S.toList nodes ]
    part2 = M.fromList
        [ (Right j, G.Frontier y)
        | (j, y) <- M.toList leaves ]
    union (G.Interior m) (G.Interior m') = G.Interior $ M.union m m'
    union _ _ = error "Tests.toGraph: cannot union"


-- | Turn the graph into the graph actually used in the library.
toGraphID
    :: (Ord i, Ord j, Ord f)
    => GraphID i j f a
    -> (G.Graph (Either i j) f a, Either i j)
toGraphID GraphID{..} =
    let g = toGraph graph
        i = Left root
    in  (g, i)


--------------------------------------------------------------------
-- Manual Graph Generation
--------------------------------------------------------------------


-- | Generate recursively all graphs of a given rank.
genGraphs
    :: (Ord a, Ord b)
    => S.Set a  -- ^ The set of features
    -> S.Set b  -- ^ The set of values
    -> Int      -- ^ The rank
    -> [Graph Int Int a b]
genGraphs featSet atomSet 1 = g0 :
    [ gs x
    | x <- S.toList featSet ]
  where
    g0 = Graph
        { nodes  = S.singleton 1
        , edges  = M.empty
        , leaves = M.empty }
    gs x = Graph
        { nodes  = S.singleton 1
        , edges  = M.singleton (1, x) (Left 1)
        , leaves = M.empty }
genGraphs featSet atomSet n = concat
    [ addNode n g ++ addLeaf n g
    | g <- genGraphs
        featSet atomSet (n - 1) ]
  where

    -- Find all possible graphs given a new internal node.
    addLeaf i g = do
        edges' <- newLeafEdges i g
        y      <- S.toList atomSet
        return $ Graph
            { nodes = nodes g
            , edges = M.union (edges g) (M.fromList edges')
            , leaves = M.insert i y (leaves g) }

    -- Generate all sets of possible new ingoing edges leading to
    -- the leaf with the given ID.
    newLeafEdges i g = subsets
        [ [ ((j, x), Right i)
          | x <- S.toList featSet ]
        | j <- getInternalNodes g ]

    -- Find all possible graphs given a new internal node.
    addNode i g = do
        edges' <- newEdges i g
        return $ Graph
            { nodes = S.insert i (nodes g)
            , edges = M.union (edges g) (M.fromList edges')
            , leaves = leaves g }

    -- Generate all edge-sets which can be added to the graph
    -- with the new internal node.
    newEdges i g = do
        es1 <- newOutEdges1 i g
        es2 <- newOutEdges2 i g
        es3 <- newInEdges   i g
        return $ es1 ++ es2 ++ es3

    -- Generate all sets of possible new in-going edges.
    newInEdges i g = subsets
        [ [ ((j, x), Left i)
          | x <- S.toList featSet ]
        | j <- i : getInternalNodes g ]

    -- Generate all sets of possible new out-going edges
    -- leading to internal nodes.
    newOutEdges1 i g = subsets
        [ [ ((i, x), Left j)
          | x <- S.toList featSet ]
        | j <- getInternalNodes g ]

    -- Generate all sets of possible new out-going edges
    -- leading to leaf nodes.
    newOutEdges2 i g = subsets
        [ [ ((i, x), Right j)
          | x <- S.toList featSet ]
        | j <- getLeafNodes g ]

    -- Retrieve all internal node IDs.
    getInternalNodes Graph{..} = S.toList nodes
    -- Retrieve all leaf node IDs.
    getLeafNodes Graph{..} = M.keys leaves
        
    
    
-- | Compute possible subsets of the input set of sets taking
-- into account that elements from one input set cannot be added
-- to the output together. 
subsets :: [[a]] -> [[a]]
subsets (xs:xss) =
    -- none element from `xs` is added to the result 
    subsets xss ++ concat
    -- one of the `xs` elements is added to the result
    [ map (x:) (subsets xss)
    | x  <- xs ]
subsets [] = [[]]


--------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------


-- | Choose one of the arbitrary generators.
chooseEither :: QC.Gen a -> QC.Gen b -> QC.Gen (Either a b)
chooseEither g h = QC.arbitrary >>= \b -> case b of
    True -> Left <$> g
    _   -> Right <$> h


-- | Small integral.
newtype SInt = SInt Int
    deriving (Eq, Ord, Show)


instance QC.Arbitrary SInt where
    arbitrary = SInt <$> QC.choose (1, 10)


-- mkG :: [(Int, Node)] -> Graph
mkG = G.Graph . M.fromList
-- mkI :: [(Char, Int)] -> Node
mkI = G.Interior . M.fromList
-- mkF :: Char -> Node
mkF = G.Frontier
