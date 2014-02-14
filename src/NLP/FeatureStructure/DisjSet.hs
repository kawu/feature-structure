module NLP.FeatureStructure.DisjSet
( DisjSet
, empty
, repr
, mkReprOf
) where


import qualified Data.Map.Strict as M


-- | A naive implementation of a disjoint set.
newtype DisjSet a = DisjSet (M.Map a a)


-- | An empty disjoint set.
empty :: DisjSet a
empty = DisjSet M.empty


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
