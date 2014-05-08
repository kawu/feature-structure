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
-- ** Core combinators
, empty
, atom
, label
, name

-- * Compile
, compile
, compiles

-- * AVM monad
, AVMM (..)
, AVM
, avm
-- ** Core combinators
, feat
, nameAVM
-- ** Other combinators
, leaf
, list
-- ** Infix verions
, (##)
) where


-- import           Control.Applicative ((<$>))
import           Control.Monad (forM, forM_)
import qualified Control.Monad.State.Strict as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import qualified Data.Traversable as T
import           Data.String (IsString (..))


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
type FF i f a = [FN i f a]


-- | If the string is empty, it represents an `empty` tree.
-- If the string starts with '?', it represents a `label`.
-- Otherwise, it represents a `atom`.
instance (IsString i, IsString a) => IsString (FN i f a) where
    fromString xs = case xs of
        []      -> empty
        ('?':_) -> label $ fromString xs
        _       -> atom $ fromString xs


--------------------------------------------------------------------
-- Core combinators
--------------------------------------------------------------------


-- | An empty tree.  It can be unified with both
-- complex structures and atomic values.
empty :: FN i f a
empty = FN Nothing $ Subs M.empty


-- | An atomic `FN`.
atom :: a -> FN i f a
atom = FN Nothing . Atom


-- | A lone identifier.
label :: i -> FN i f a
label = flip name empty


-- | Assign name to an `FN`.
name :: i -> FN i f a -> FN i f a
name i fn = fn { ide = Just i }


--------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------


-- | A state of the conversion monad.
data ConS i f a = ConS {
    -- | A counter for producing new identifiers.
      conC  :: Int
    -- | A mapping from old to new identifiers.
    , conI  :: M.Map i (Set.Set ID) }
    -- -- | The result.
    -- , conR  :: [(ID, G.Node f a)] }


-- | Initial value of the state.
initConS :: ConS i f a
initConS = ConS
    { conC  = 1
    , conI  = M.empty }
    -- , conR  = [] }


-- | Compile a named feature tree to a graph representation.
compile :: (Ord i, Eq a, Ord f) => FN i f a -> Maybe (ID, G.Graph f a)
compile x = flip J.runJoin G.empty $ do
    -- First we need to convert a tree to a trivial feature graph
    -- (tree stored as `conR`) and identify nodes which need to be
    -- joined.
    (r0, st) <- S.runStateT (fromFN x) initConS
    -- The second step is to join all nodes which have to be
    -- merged based on the identifiers specified by the user.
    forM_ (M.elems $ conI st) $ \ks -> do
        forM_ (adja $ Set.toList ks) $ \(i, j) -> do
            J.join i j
    J.liftGraph $ G.getRepr r0


-- | Compile a traversable structure of named feature trees
-- to a graph representation.  The resulting traversable data
-- structure will be returned with identifiers in place of
-- named feature trees.
compiles
    :: (Ord i, Eq a, Ord f, T.Traversable t)
    => t (FN i f a) -> Maybe (t ID, G.Graph f a)
compiles t0 = flip J.runJoin G.empty $ do
    -- First we need to convert a traversable to a trivial feature
    -- graph (`conR`) and identify nodes which need to be joined.
    (t1, st) <- S.runStateT (fromTravFN t0) initConS
    -- The second step is to join all nodes which have to be
    -- merged based on the identifiers specified by the user.
    forM_ (M.elems $ conI st) $ \ks -> do
        forM_ (adja $ Set.toList ks) $ \(i, j) -> do
            J.join i j
    T.mapM (J.liftGraph . G.getRepr) t1


-- | A conversion monad. 
type Con i f a b = S.StateT (ConS i f a) (J.Join f a) b


-- | Convert the given traversable structure of named feature trees
-- to a trivial feature graph.
fromTravFN
    :: (T.Traversable t, Ord i, Ord f, Eq a)
    => t (FN i f a) -> Con i f a (t ID)
fromTravFN = T.mapM fromFN


-- | Convert the given named feature tree to a feature graph.
fromFN :: (Ord i, Ord f, Eq a) => FN i f a -> Con i f a ID
fromFN FN{..} = do
    x <- fromFT val
    justM ide (register x)
    return x
 

-- | Convert the given feature tree to a trivial feature graph.
fromFT :: (Ord i, Ord f, Eq a) => FT i f a -> Con i f a ID
fromFT (Subs x) = fromAV x
fromFT (Atom x) = do
    i <- newID
    addNode i $ G.Frontier x
    return i


-- | Convert the given tree to a trivial feature graph.
-- The result (`conI` and `conR`) will be represented
-- within the state of the monad.
fromAV :: (Ord i, Ord f, Eq a) => AV i f a -> Con i f a ID
fromAV fs = do
    i  <- newID
    xs <- forM (M.toList fs) (secondM fromFN)
    addNode i $ G.Interior $ M.fromList xs
    return i


-- | Register the relation between the new and the old identifier.
register :: (Ord i, Ord f, Eq a) => ID -> i -> Con i f a ()
register i j = S.modify $ \st@ConS{..} ->
    let conI' = M.alter (addKey i) j conI
    in  st { conI = conI' }
  where
    addKey x Nothing  = Just $ Set.singleton x
    addKey x (Just s) = Just $ Set.insert x s


-- | New identifier.
newID :: Con i f a ID
newID = S.state $ \st@ConS{..} ->
    (conC, st {conC=conC+1})


-- | Add node.
addNode :: ID -> G.Node f a -> Con i f a ()
addNode i = S.lift . J.liftGraph . G.setNode i
-- addNode x y = S.modify $ \st@ConS{..} ->
--     st {conR = (x, y) : conR}


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
newtype AVMM i f a b = AVMM { unAVM :: S.State (AVMS i f a) b }
    deriving (Monad, S.MonadState (AVMS i f a))


-- | An AVM state.
data AVMS i f a = AVMS {
    -- | An identifier of the AVM.
      avmID :: Maybe i
    -- | An attribute-value map.
    , avmAV :: AV i f a }


-- | Convenient type alias that will probably be used most of the time.
type AVM i f a = AVMM i f a ()


-- | Run the AVMM monad and return a feature tree.
avm :: AVMM i f a b -> FN i f a
avm m =
    let AVMS{..} = S.execState (unAVM m) (AVMS Nothing M.empty)
    in  FN avmID $ Subs avmAV


--------------------------------------------------------------------
-- Core AVM combinators
--------------------------------------------------------------------


-- | Forces a subtree to be added.  If the feature already exists,
-- its value is overwritten.
feat :: Ord f => f -> FN i f a  -> AVM i f a
feat ft fn = S.modify $ \s -> s { avmAV = M.insert ft fn (avmAV s) }


-- | Assign the name to the AVM.  If the lable already exists,
-- its value is overwritten.
nameAVM :: i -> AVM i f a
nameAVM x = S.modify $ \s -> s { avmID = Just x }


--------------------------------------------------------------------
-- Other AVM combinators
--------------------------------------------------------------------

-- | An atomic value assigned to a feature.
leaf :: Ord f => f -> a -> AVM i f a
leaf x = feat x . atom


-- | A list encoded as a named feature structure.
list
    :: Ord f
    => a            -- ^ An empty list
    -> f            -- ^ First
    -> f            -- ^ Rest
    -> FF i f a     -- ^ The list to encode
    -> FN i f a
list nil first rest ff =
    doit ff
  where
    doit (x:xs) = avm $ do
        feat first x
        feat rest $ doit xs
    doit [] = atom nil


--------------------------------------------------------------------
-- Infix versions of common combinators
--------------------------------------------------------------------


-- | An infix version of `feat`.
(##) :: Ord f => f -> FN i f a  -> AVM i f a
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
