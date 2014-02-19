{-# LANGUAGE FlexibleContexts #-}


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
import qualified Data.Traversable as Tr


import           NLP.FeatureStructure.Core
import           NLP.FeatureStructure.Graph
import qualified NLP.FeatureStructure.Join as J



-- | Unify two feature graphs.  Log messages resulting from 
-- the joining process will be ignored.
unify :: Uni i f a => NodeFG i f a -> NodeFG i f a -> Maybe (NodeFG Int f a)
unify (_i, f) (_j, g) = reIdent <$> J.runJoin
    (fromTwo f g)
    (J.join i j >> J.repr i)
  where
    i = Left _i
    j = Right _j


-- | Unify two feature graphs.  Log messages will be printed to stdout.
unifyIO
    :: (Uni i f a, Functor m, MonadIO m)
    => NodeFG i f a -> NodeFG i f a
    -> m (Maybe (NodeFG Int f a))
unifyIO (_i, f) (_j, g) = fmap reIdent <$> J.runJoinIO
    (fromTwo f g)
    (J.join i j >> J.repr i)
  where
    i = Left _i
    j = Right _j


--------------------------------------------------------------------
-- Assign new identifiers
--------------------------------------------------------------------


-- | Assign new node identifiers [0, 1, ..].
reIdent :: Ord i => NodeFG i f a -> NodeFG Int f a
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
