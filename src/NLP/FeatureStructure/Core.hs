{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Class representing assumptions on individual types.


module NLP.FeatureStructure.Core
( Uni
) where


--------------------------------------------------------------------
-- Unification class
--
-- Basic assumptions about individual types.
--
-- TODO: Move to a separate, Core module?  And rename the class?
--------------------------------------------------------------------


class (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where
instance (Show i, Ord i, Show a, Eq a, Show f, Ord f)
    => Uni i f a where
