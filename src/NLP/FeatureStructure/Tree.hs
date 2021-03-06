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

-- * Conversion
, runConT
, runCon
, fromFN
, fromFT
, fromAV

-- * Utility
, showFN
, showFT
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forM, forM_)
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Identity (Identity, runIdentity)

import           Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
-- import qualified Data.Traversable as T
import           Data.String (IsString (..))


import           NLP.FeatureStructure.Core
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Join as J


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- | An attribute-value map.
-- * 'i' -- type of identifier
-- * 'f' -- type of a feature (attribute)
-- * 'a' -- type of an atomic (leaf) feature value
type AV i f a = M.Map f (FN i f a)


-- | A feature tree is either an atomic 'Atom a' value or a
-- sub-attribute-value map.
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


-- | If the string starts with '?', it represents a `label`.
-- Otherwise, it represents an `atom`.
instance (IsString i, IsString a) => IsString (FN i f a) where
    fromString xs = case xs of
        ('?':_) -> label $ fromString xs
        _       -> atom $ fromString xs
--     fromString xs = case xs of
--         []      -> empty
--         ('?':_) -> label $ fromString xs
--         _       -> atom $ fromString xs
-- -- | If the string is empty, it represents an `empty` tree.
-- -- If the string starts with '?', it represents a `label`.
-- -- Otherwise, it represents a `atom`.


showFN :: (Show i, Show f, Show a) => FN i f a -> String
showFN FN{..} =
    let showIde Nothing = ""
        -- showIde (Just i) = "(" ++ show i ++ ")"
        showIde (Just i) = show i
    in  showIde ide ++ showFT val


showFT :: (Show i, Show f, Show a) => FT i f a -> String
showFT (Atom x) = show x
showFT (Subs m) =
    let showFeat (x, fn) = show x ++ "=" ++ showFN fn
        body = intercalate ", " $ map showFeat $ M.toList m
    in  "[" ++ body ++ "]"


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


-- | Assign a name to an `FN`.
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
    , conI  :: M.Map i (Set.Set ID)
    -- | A mapping from atomic values to IDs.  Ensures that there
    -- are no frontier duplicates in the conversion result.
    , conA  :: M.Map a ID }


-- | Initial value of the state.
initConS :: ConS i f a
initConS = ConS
    { conC  = 1
    , conI  = M.empty
    , conA  = M.empty }


-- | A conversion transformer. 
type ConT i f a m b = S.StateT (ConS i f a) (J.JoinT ID f a m) b


-- | A conversion monad. 
type Con i f a b = ConT i f a Identity b


-- | Run the conversion transformer.
runConT
    :: (Functor m, Monad m, Ord i, Ord f, Eq a)
    => ConT i f a m b -> m (Maybe (b, J.Res ID ID f a))
runConT con = flip J.runJoinT G.empty $ do
    -- First we need to convert a tree to a trivial feature graph
    -- (tree stored as `conC`) and identify nodes which need to be
    -- joined.
    (r0, st) <- S.runStateT con initConS
    -- The second step is to join all nodes which have to be
    -- merged based on the identifiers specified by the user.
    forM_ (M.elems $ conI st) $ \ks -> do
        forM_ (adja $ Set.toList ks) $ \(i, j) -> do
            J.join i j
    return r0


-- | Run the conversion monad.
runCon
    :: (Ord i, Ord f, Eq a)
    => Con i f a b
    -> Maybe (b, J.Res ID ID f a)
runCon = runIdentity . runConT


-- -- | Convert the given traversable structure of named feature trees
-- -- to a trivial feature graph.
-- fromTravFN
--     :: (Monad m, T.Traversable t, Ord i, Ord f, Eq a)
--     => t (FN i f a) -> ConT i f a m (t ID)
-- fromTravFN = T.mapM fromFN


-- | Convert the given named feature tree to a feature graph.
fromFN
    :: (Functor m, Monad m, Ord i, Ord f, Ord a)
    => FN i f a
    -> ConT i f a m ID
fromFN FN{..} = do
    x <- fromFT val
    justM ide $ register x
    return x
 

-- | Convert the given feature tree to a trivial feature graph.
fromFT
    :: (Functor m, Monad m, Ord i, Ord f, Ord a)
    => FT i f a
    -> ConT i f a m ID
fromFT (Subs x) = fromAV x
fromFT (Atom x) = atomID x >>= \mi -> case mi of
    Just i  -> return i
    Nothing -> do
        i <- newID
        addNode i $ G.Frontier x
        saveAtomID x i
        return i


-- | Convert the given tree to a trivial feature graph.
-- The result (`conI` and `conC`) will be represented
-- within the state of the monad.
fromAV
    :: (Functor m, Monad m, Ord i, Ord f, Ord a)
    => AV i f a -> ConT i f a m ID
fromAV fs = do
    i  <- newID
    xs <- forM (M.toList fs) (secondM fromFN)
    addNode i $ G.Interior $ M.fromList xs
    return i


-- | Register the relation between the new and the old identifier.
register
    :: (Monad m, Ord i, Ord f, Eq a)
    => ID -> i -> ConT i f a m ()
register i j = S.modify $ \st@ConS{..} ->
    let conI' = M.alter (addKey i) j conI
    in  st { conI = conI' }
  where
    addKey x Nothing  = Just $ Set.singleton x
    addKey x (Just s) = Just $ Set.insert x s


-- | Rertieve ID for the given atom.
atomID :: (Functor m, Monad m, Ord a) => a -> ConT i f a m (Maybe ID)
atomID x = M.lookup x <$> S.gets conA


-- | Save info about ID of the given atom.
saveAtomID :: (Monad m, Ord a) => a -> ID -> ConT i f a m ()
saveAtomID x i = S.modify $ \st ->
    st {conA = M.insert x i $ conA st}


-- | New identifier.
newID :: Monad m => ConT i f a m ID
newID = S.state $ \st@ConS{..} ->
    (conC, st {conC=conC+1})


-- | Add node.
addNode :: Monad m => ID -> G.Node ID f a -> ConT i f a m ()
addNode i = S.lift . J.setNode i


-- -- | Compile a named feature tree to a graph representation.
-- compile :: (Ord i, Eq a, Ord f) => FN i f a -> Maybe (ID, G.Graph f a)
-- compile x = flip J.runJoin G.empty $ do
--     -- First we need to convert a tree to a trivial feature graph
--     -- (tree stored as `conC`) and identify nodes which need to be
--     -- joined.
--     (r0, st) <- S.runStateT (fromFN x) initConS
--     -- The second step is to join all nodes which have to be
--     -- merged based on the identifiers specified by the user.
--     forM_ (M.elems $ conI st) $ \ks -> do
--         forM_ (adja $ Set.toList ks) $ \(i, j) -> do
--             J.join i j
--     J.liftGraph $ G.getRepr r0


-- -- | Compile a traversable structure of named feature trees
-- -- to a graph representation.  The resulting traversable data
-- -- structure will be returned with identifiers in place of
-- -- named feature trees.
-- compiles
--     :: (Ord i, Eq a, Ord f, T.Traversable t)
--     => t (FN i f a) -> Maybe (t ID, G.Graph f a)
-- compiles t0 = flip J.runJoin G.empty $ do
--     -- First we need to convert a traversable to a trivial feature
--     -- graph (`conC`) and identify nodes which need to be joined.
--     (t1, st) <- S.runStateT (fromTravFN t0) initConS
--     -- The second step is to join all nodes which have to be
--     -- merged based on the identifiers specified by the user.
--     forM_ (M.elems $ conI st) $ \ks -> do
--         forM_ (adja $ Set.toList ks) $ \(i, j) -> do
--             J.join i j
--     T.mapM (J.liftGraph . G.getRepr) t1


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
