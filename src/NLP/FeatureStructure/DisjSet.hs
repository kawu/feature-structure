{-# LANGUAGE RecordWildCards #-}


module NLP.FeatureStructure.DisjSet
( 
-- * Disjoint-set
  DisjSet
, empty
, fromList
, toList
, union
, mkReprOf
, repr

-- * Utility
, printDisjSet
) where


-- import           Data.Traversable (Traversable)
-- import           Data.Foldable (Foldable)
import qualified Data.Map.Strict as M


-- | A naive implementation of a disjoint set.
newtype DisjSet a = DisjSet { unDisjSet :: M.Map a a }
    deriving (Show, Eq, Ord)


-- | An empty disjoint set.
empty :: DisjSet a
empty = DisjSet M.empty


-- | Construct disjoint-set from the list of pairs.
-- The function doesn't check, if the list is well-formed,
-- i.e. represents a valid disjoint-set.
fromList :: Ord a => [(a, a)] -> DisjSet a
fromList = DisjSet . M.fromList


-- | Convert disjoint-set to a list of pairs.
toList :: Ord a => DisjSet a -> [(a, a)]
toList = M.toList . unDisjSet


-- | A union of two disjoint sets.  The precondition is not
-- checked.
union :: Ord a => DisjSet a -> DisjSet a -> DisjSet a
union x y = DisjSet $ M.union
    (unDisjSet x) (unDisjSet y)


-- | Representative of the given element.  Returns the same element,
-- if it is not present in the set.
-- TODO: Implement path compression.
repr :: Ord a => a -> DisjSet a -> a
repr x ds@(DisjSet m) = case M.lookup x m of
    Just y  -> repr y ds
    Nothing -> x


-- | Make the first node a representative of the second node.
mkReprOf :: Ord a => a -> a -> DisjSet a -> DisjSet a
mkReprOf x y (DisjSet m) = DisjSet $ M.insert y x m


--------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------


printDisjSet :: Show a => DisjSet a -> IO ()
printDisjSet DisjSet{..} =
    mapM_ print $ M.toList unDisjSet
