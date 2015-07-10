{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.Graph.Tests where


import           Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map.Strict as M 

import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck   (testProperty)


import qualified NLP.FeatureStructure.Graph as G


--------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------


-- | Graph parametrized with SInts.
type SGraph = Graph SInt SInt SInt SInt


-- | SGraph with info about the root.
type SGraphID = GraphID SInt SInt SInt SInt


-- | The actual test set.
tests :: TestTree
tests = testGroup "NLP.FeatureStructure.Graph"
    [ testProperty "arbitrary graph valid"
        (valid . toGraph :: SGraph -> Bool)
    , testProperty "graph equals to itself" eqItself
    , testProperty "size of mapIDs (+1) the same" checkMapIDs
    , testProperty "transitivity of the comparison" checkTrans
    ]


--------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------


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


-- | Size of the result of mapIDs (+1) should not change.
-- Moreover, the result should still be valid (not corrupted).
checkMapIDs :: SGraph -> Bool
checkMapIDs g0 =
    let plus1 (SInt x) = SInt $ x + 1
        modID (Left i) = Left $ plus1 i
        modID (Right j) = Right $ plus1 j
        g1 = toGraph g0
        g2 = G.mapIDs modID g1
    in  G.size g1 == G.size g2 && valid g2


-- | Is the graph equal to itself?
eqItself :: SGraphID -> Bool
eqItself s =
    let (g, i) = toGraphID s
    in  G.equal g i g i


-- | Is it not `corrupted`?
valid :: (Ord i) => G.Graph i f a -> Bool
valid = not . corrupted


-- | The graph is corrupted if one of its nodes points to an
-- inexistent node.
corrupted :: (Ord i) => G.Graph i f a -> Bool
corrupted g =
    not $ and [check i | i <- S.toList $ G.getIDs g]
  where
    check i = case G.getNode i g of
        Nothing -> False
        Just n  -> case n of
            G.Interior m -> and $ map member $ M.elems m
            G.Frontier _ -> True
    member j = case G.getNode j g of
        Nothing -> False
        Just _  -> True


--------------------------------------------------------------------
-- Alternative graph representation
--------------------------------------------------------------------


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
    arbitrary = SInt <$> QC.choose (1, 25)
