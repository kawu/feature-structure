{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts #-}


-- | Feature graph unification.


module NLP.FeatureStructure.Unify
( unify
, unifyIO
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forM)
import qualified Control.Monad.Atom as Atom
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as M
import           Data.Traversable (Traversable)
import qualified Data.Traversable as Tr
-- import qualified Data.List.NonEmpty as N


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.Join as J


--------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------


-- | Unify two feature graphs.  Log messages resulting from 
-- the joining process will be ignored.
unify
    :: Uni i f a
    => (i, FG i f a)
    -> (i, FG i f a)
    -> Maybe (Int, FG Int f a)
unify (_i, f) (_j, g) = reIdent <$> J.runJoin
    (fromTwo f g)
    (J.join i j >> J.repr i)
  where
    i = Left _i
    j = Right _j


-- | Unify two feature graphs.  Log messages will be printed to stdout.
unifyIO
    :: (Uni i f a, Functor m, MonadIO m)
    => (i, FG i f a) -> (i, FG i f a)
    -> m (Maybe (Int, FG Int f a))
unifyIO (_i, f) (_j, g) = fmap reIdent <$> J.runJoinIO
    (fromTwo f g)
    (J.join i j >> J.repr i)
  where
    i = Left _i
    j = Right _j


--------------------------------------------------------------------
-- Substituable data structures
--------------------------------------------------------------------


-- -- | A class of ,,substituable'' structures.
-- class Traversable t => Subst t p where
--     subst :: p -> t a -> t a -> t a
-- 
-- 
-- instance Subst [] Int where
--     subst i xs ys =
--         let (ls, rs) = splitAt i ys
--         in  ls ++ xs ++ drop 1 rs


--------------------------------------------------------------------
-- Unification in context
--------------------------------------------------------------------

-- Arguments:
-- * The first structure is a tree/a list,
-- * The second one is a rule (i.e. a non-empty sequence),
-- * A pointer to an element of the first structure which
--   is supposed to be substituted by a unified rule.
--
-- Process:
-- * Join the specified node with the head of the rule,
-- * Translate both input structures using the `repr` function,
-- * Put body of the rule under the pointer.
--
-- Assumptions:
-- * A rule is a list,
-- * An first, input structure is:
--   a) Traversable,
--   b) Substituable, i.e. we must be able to provide a pointer
--     for this structure and a function which will allow us to
--     substitute substructure under the pointer for the unified
--     body of the rule.  In other words, 



-- -- | Apply a rule (represented by a sequence) on the given
-- -- position of another sequence. 
-- applyRule
--     :: (Subst t p, Uni i f a)
--     => [i]          -- ^ A rule (non-empty list of node identifiers)
--     -> FG i f a     -- ^ Graph corresponding to the rule
--     -> p            -- ^ Pointer within the structure
--     -> t i          -- ^ The structure of node identifiers
--     -> FG i f a     -- ^ Graph corresponding to the structure
--     -> Maybe 


--------------------------------------------------------------------
-- Assign new identifiers
--------------------------------------------------------------------


-- | Assign new node identifiers [0, 1, ..].
reIdent :: Ord i => (i, FG i f a) -> (Int, FG Int f a)
reIdent (r, f) = Atom.evalAtom $ do
    s <- Atom.toAtom r
    -- TODO: To speedup things, we could try to use
    -- M.toAscList/M.fromAscList pair here.
    g <- forM (M.toList f) $ \(i, x) -> do 
        j <- Atom.toAtom i
        y <- reIdentNode x
        return (j, y)
    return (s, M.fromList g)
  where
    reIdentNode (Interior m) = fmap Interior $ Tr.mapM Atom.toAtom m
    reIdentNode (Frontier x) = return $ Frontier x
