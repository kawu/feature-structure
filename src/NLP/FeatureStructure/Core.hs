{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Class representing assumptions on individual types.


module NLP.FeatureStructure.Core
( ID
, Uni
) where


--------------------------------------------------------------------
-- Node identifier
--------------------------------------------------------------------


-- | A node identifier.
type ID = Int


--------------------------------------------------------------------
-- Unification class
--
-- Basic assumptions about individual types.
--
-- TODO: Rename the class?
-- TODO: It isn't ideal.  For example, the `unify` function is
--   requires that both input graphs has identifiers of the
--   same type.  In the general case, it doesn't have to be. 
--------------------------------------------------------------------


class (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where
instance (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where
