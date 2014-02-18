-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | A tree representation of a feature structure.


module NLP.FeatureStructure.Tree
( 
-- * Feature tree 
  FT
, FN (..)
, FV (..)

-- * Compile 
-- , compileIO

-- -- * Language
-- , TreeM (..)
-- , Tree
) where


import           Control.Monad (forM)
import qualified Control.Monad.State.Strict as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
-- import qualified Pipes.Prelude as P
-- import           Pipes
-- import qualified Data.Sequence as Seq
-- import           Data.Sequence (Seq, (|>), ViewL(..))

import qualified NLP.FeatureStructure.Graph as G
import           NLP.FeatureStructure.Graph (Uni)


--------------------------------------------------------------------
-- Feature tree
--------------------------------------------------------------------


-- | A feature tree consists of a list of edges.  Multiple subtrees
-- can be assigned to a single feature (but they will have to be
-- unified).
type FT i f a = M.Map f (FN i f a)
-- type FT i f a = [(f, FN i f a)]


-- | A feature value with optional identifier.
data FN i f a = FN {
    -- | Optional identifier.
      fnId  :: Maybe i
    -- | The actual value.
    , fnVl  :: FV i f a
    } deriving (Show, Eq, Ord)


-- | A feature value.
data FV i f a
    = Subs (FT i f a)   -- ^ A substructure
    | Atom a            -- ^ An atomic value
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------


-- | An identifier.
type ID = Int


-- | Compile `FT` to a graph representation.
compileIO :: Uni i f a => FT i f a -> IO (G.NodeFG ID f a)
compileIO x = do
    (i, st) <- S.runStateT (fromTree x) initConS
    return (i, conR st)
    

-- | A state of the conversion monad.
data ConS i f a = ConS {
    -- | A counter for producing new identifiers.
      conC  :: Int
    -- | A mapping from old to new identifiers.
    , conI  :: M.Map i (Set.Set ID)
    -- | The result set.
    , conR  :: M.Map ID (G.Node ID f a) }


-- | Initial value of the state.
initConS :: ConS i f a
initConS = ConS
    { conC  = 1
    , conI  = M.empty
    , conR  = M.empty }


-- | A conversion monad. 
type ConM i f a m b = S.StateT (ConS i f a) m b


-- | Convert the given tree to a feature graph.
fromTree :: (Monad m, Uni i f a) => FT i f a -> ConM i f a m ID
fromTree fs = do
    i  <- newID
    xs <- forM (M.toList fs) $ \(ft, FN{..}) -> do
        x <- fromFV fnVl
        justM (register x) fnId
        return (ft, x)
    addNode i $ G.Interior $ M.fromList xs
    return i


-- | Convert the given feature value to a feature graph.
fromFV :: (Monad m, Uni i f a) => FV i f a -> ConM i f a m ID
fromFV (Subs x) = fromTree x
fromFV (Atom x) = do
    i <- newID
    addNode i $ G.Frontier x
    return i


-- | Register the relation between the new and the old identifier.
register :: (Monad m, Uni i f a) => ID -> i -> ConM i f a m ()
register i j = S.modify $ \st@ConS{..} ->
    let conI' = M.alter (addKey i) j conI
    in  st { conI = conI' }
  where
    addKey x Nothing  = Just $ Set.singleton x
    addKey x (Just s) = Just $ Set.insert x s


-- | New identifier.
newID :: Monad m => ConM i f a m ID
newID = S.state $ \st@ConS{..} ->
    (conC, st {conC=conC+1})


-- | Add node.
addNode :: Monad m => ID -> G.Node ID f a -> ConM i f a m ()
addNode x y = S.modify $ \st@ConS{..} ->
    st {conR = M.insert x y conR}


--------------------------------------------------------------------
-- Language
--
-- * Define a monad, which will be used to defina a *single*
--   level of a feature tree.  See e.g. `Heist.SpliceAPI`.
-- * A monad for every node in a tree will be evaluated once.
--   We don't need a monad which would work over the entire
--   tree structure.
-- * Some of the functions used to define attributes will
--   take a monadic action as an argument and, simply,
--   evaluate it before taking the result into accout.
-- * Potential problem: what if there are functions, which
--   should accept both pure and monadic arguments?  Well,
--   we could use `return` of course, but that doesn't seem
--   a very elegant solution.
--------------------------------------------------------------------


-- -- | A monad providing convenient syntax for defining feature trees.
-- newtype TreeM i f a b = TreeM { unTree :: S.State (FT i f a) b }
--     deriving (Monad, S.MonadState (FT i f a))
-- 
-- 
-- -- | Convenient type alias that will probably be used most of the time.
-- type Tree i f a = TreeM i f a ()
-- 
-- 
-- -- -- | Monoid instance does a union of the two maps with the second map
-- -- -- overwriting any duplicates.
-- -- instance Monoid (Splices s) where
-- --     mempty  = empty
-- --     mappend = unionWithS (\_ b -> b)
-- 
-- 
-- -- | Forces a subtree to be added.  If the feature already exists,
-- -- its value is overwritten.
-- (##) :: f -> FN i f a  -> Tree i f a
-- (##) ft fn = S.modify $ M.insert ft fn
-- infixr 0 ##


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Run a monadic action on a `Just` value.
justM :: Monad m => (a -> m ()) -> Maybe a -> m ()
justM f (Just x) = f x
justM _ Nothing  = return ()
