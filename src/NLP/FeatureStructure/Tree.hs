{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | A tree representation of a feature structure.


module NLP.FeatureStructure.Tree
( 
-- * Types
  AV
, FT (..)
, FN (..)
, FF
, ID

-- * Compile
, compile
, compileT

-- * Convenient syntax
, AvmM (..)
, Avm
, avm
-- ** Core combinators
, feat
, atom
, name
, undef
-- ** Other combinators
, leaf
, list
-- ** Infix verions
, (##)
) where


-- import           Control.Applicative ((<$>))
import           Control.Monad (forM, forM_)
import qualified Control.Monad.State.Strict as S
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import           Data.Traversable (Traversable)
import qualified Data.Traversable as T
-- import qualified Pipes.Prelude as P
-- import           Pipes
-- import qualified Data.Sequence as Seq
-- import           Data.Sequence (Seq, (|>), ViewL(..))


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Join as J


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- | An attribute-value map.
type AV i f a = M.Map f (FN i f a)


-- | A feature tree.
data FT i f a
    = Subs (AV i f a)
    | Atom a
    deriving (Show, Eq, Ord)


-- | A named feature tree, i.e. with an optional identifier.
data FN i f a = FN {
    -- | Optional identifier.
      ide :: Maybe i
    -- | The actual value.
    , val :: FT i f a
    } deriving (Show, Eq, Ord)


-- | A feature forest.
-- NOTE: a term "parse forest" has a different meaning w.r.t. parsing.
type FF i f a = [FN i f a]


-- | An identifier.
type ID = Int


--------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------


-- | Compile a named feature tree to a graph representation.
compile
    :: (MonadIO m, Uni i f a) => FN i f a
    -> m (Maybe (ID, G.FG ID f a))
compile x = do
    -- First we need to convert a tree to a trivial feature graph
    -- (`conR`) and identify nodes which need to be joined.
    (r0, st) <- S.runStateT (fromFN x) initConS
    -- The second step is to join all nodes which have to be
    -- merged based on the identifiers specified by the user.
    J.runJoinIO (conR st) $ do
        forM_ (M.elems $ conI st) $ \ks -> do
            forM_ (adja $ Set.toList ks) $ \(i, j) -> do
                J.join i j
        J.repr r0


-- | Compile a traversable structure of named feature trees
-- to a graph representation.  The resulting traversable data
-- structure will be returned with identifiers in place of
-- named feature trees.
compileT
    :: (MonadIO m, Uni i f a, Traversable t)
    => t (FN i f a)
    -> m (Maybe (t ID, G.FG ID f a))
compileT t0 = do
    -- First we need to convert a traversable to a trivial feature
    -- graph (`conR`) and identify nodes which need to be joined.
    (t1, st) <- S.runStateT (fromTravFN t0) initConS
    -- The second step is to join all nodes which have to be
    -- merged based on the identifiers specified by the user.
    J.runJoinIO (conR st) $ do
        forM_ (M.elems $ conI st) $ \ks -> do
            forM_ (adja $ Set.toList ks) $ \(i, j) -> do
                J.join i j
        T.mapM J.repr t1
    

-- | A state of the conversion monad.
data ConS i f a = ConS {
    -- | A counter for producing new identifiers.
      conC  :: Int
    -- | A mapping from old to new identifiers.
    , conI  :: M.Map i (Set.Set ID)
    -- | The result.
    , conR  :: G.FG ID f a }


-- | Initial value of the state.
initConS :: ConS i f a
initConS = ConS
    { conC  = 1
    , conI  = M.empty
    , conR  = M.empty }


-- | A conversion monad. 
type ConM i f a m b = S.StateT (ConS i f a) m b


-- | Convert the given traversable structure of named feature trees
-- to a trivial feature graph.
fromTravFN
    :: (Monad m, Uni i f a, Traversable t)
    => t (FN i f a)
    -> ConM i f a m (t ID)
fromTravFN = T.mapM fromFN


-- | Convert the given named feature tree to a feature graph.
fromFN :: (Monad m, Uni i f a) => FN i f a -> ConM i f a m ID
fromFN FN{..} = do
    x <- fromFT val
    justM ide (register x)
    return x
 

-- | Convert the given feature tree to a trivial feature graph.
fromFT :: (Monad m, Uni i f a) => FT i f a -> ConM i f a m ID
fromFT (Subs x) = fromAV x
fromFT (Atom x) = do
    i <- newID
    addNode i $ G.Frontier x
    return i


-- | Convert the given tree to a trivial feature graph.
-- The result (`conI` and `conR`) will be represented
-- within the state of the monad.
fromAV :: (Monad m, Uni i f a) => AV i f a -> ConM i f a m ID
fromAV fs = do
    i  <- newID
    xs <- forM (M.toList fs) (secondM fromFN)
    addNode i $ G.Interior $ M.fromList xs
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


-- | A monad providing convenient syntax for defining feature trees.
newtype AvmM i f a b = AvmM { unAvm :: S.State (AV i f a) b }
    deriving (Monad, S.MonadState (AV i f a))


-- | Convenient type alias that will probably be used most of the time.
type Avm i f a = AvmM i f a ()


-- | Runs the AvmM monad, generating a feature tree.
avm :: AvmM i f a b -> FN i f a
avm m = FN Nothing $ Subs $ S.execState (unAvm m) M.empty


-- -- | Monoid instance does a union of the two maps with the second map
-- -- overwriting any duplicates.
-- instance Monoid (Splices s) where
--     mempty  = empty
--     mappend = unionWithS (\_ b -> b)


--------------------------------------------------------------------
-- Core combinators
--------------------------------------------------------------------


-- | Forces a subtree to be added.  If the feature already exists,
-- its value is overwritten.
feat :: Ord f => f -> FN i f a  -> Avm i f a
feat ft fn = S.modify $ M.insert ft fn


-- | An atomic value.
atom :: a -> FN i f a
atom = FN Nothing . Atom


-- | An undefined subtree.
undef :: FN i f a
undef = FN Nothing $ Subs M.empty


-- | Assign name to an `FN`.
name :: i -> FN i f a -> FN i f a
name i fn = fn { ide = Just i }


--------------------------------------------------------------------
-- Other combinators
--------------------------------------------------------------------


-- | An atomic value assigned to a feature.
leaf :: Ord f => f -> a -> Avm i f a
leaf x = feat x . atom


-- | A list encoded as a feature structure.
-- The first two arguments correspond to "head" and "tail"
-- features, TODO: odpowiednio.
list :: Ord f => f -> f -> FF i f a -> Avm i f a
list i j ff =
    doit ff
  where
    doit (x:xs) = do
        feat i x
        feat j $ avm $ doit xs
    doit [] = return ()


--------------------------------------------------------------------
-- Infix versions of common combinators
--------------------------------------------------------------------


-- | An infix version of `feat`.
(##) :: Ord f => f -> FN i f a  -> Avm i f a
(##) = feat
infixr 0 ##


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Run a monadic action on a `Just` value.
justM :: Monad m => Maybe a -> (a -> m ()) -> m ()
justM (Just x) f = f x
justM Nothing  _ = return ()


-- | Run a monadic action on a second element of a pair.
secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (x, y) = do
    z <- f y
    return (x, z)


-- | Pairs of adjacent elements in a list.
adja :: [a] -> [(a, a)]
adja xs = zip xs (drop 1 xs)
