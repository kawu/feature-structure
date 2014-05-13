{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- | An embedded domain specific language for defining feature structures.


module NLP.FeatureStructure.AVM
(
-- * AVM monad
  AVMM (..)
, AVM
, avm

-- * Core combinators
, empty
, atom
, label
, feat
, tryFeat

-- * List combinators
, list

-- * Infix verions
, (##)
, (#?)
) where


import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import           Data.String (IsString (..))


import qualified NLP.FeatureStructure.Tree as T


--------------------------------------------------------------------
-- AVM Monad
--------------------------------------------------------------------


-- | A monad providing convenient syntax for defining feature trees.
newtype AVMM i f a b = AVMM { unAVM :: S.State (T.FN i f a) b }
    deriving (Monad, S.MonadState (T.FN i f a))


-- -- | A monad providing convenient syntax for defining feature trees.
-- newtype AVMM i f a b = AVMM { unAVM :: S.State (AVMS i f a) b }
--     deriving (Monad, S.MonadState (AVMS i f a))


-- | Convenient type alias that will probably be used most of the time.
type AVM i f a = AVMM i f a ()


-- | If the string is empty, it represents an `empty` tree.
-- If the string starts with '?', it represents a `label`.
-- Otherwise, it represents a `atom`.
instance (IsString i, IsString a) => IsString (AVM i f a) where
    fromString = S.put . fromString


-- | Run the AVMM monad and return a feature tree.
avm :: AVMM i f a b -> T.FN i f a
avm m = S.execState (unAVM m) T.empty


--------------------------------------------------------------------
-- Core AVM combinators
--------------------------------------------------------------------


-- | An empty tree.  It is equivalent to `return ()`, so when used
-- in combination with other AVM builders (`atom` or `feat`) it will
-- be ignored.
empty :: AVM i f a
empty = return ()


-- | An atomic value.  Overwrites previous `atom` and `feat` definitions.
atom :: a -> AVM i f a
atom x = S.modify $ \s -> s { T.val = T.Atom x }


-- | Ad an identifier to a feature tree.
label :: i -> AVM i f a
label x = S.modify $ \s -> s { T.ide = Just x }


-- | Forces a subtree to be added.  If the feature already exists,
-- its value is overwritten.
feat :: Ord f => f -> AVM i f a  -> AVM i f a
feat x y = S.modify $ \s -> case T.val s of
    T.Atom _ -> s { T.val = T.Subs $ M.singleton x $ avm y }
    T.Subs m -> s { T.val = T.Subs $ M.insert x (avm y) m }


-- Inserts into the map only if the feature does not already exist.
tryFeat :: Ord f => f -> AVM i f a  -> AVM i f a
tryFeat x y = S.modify $ \s -> case T.val s of
    T.Atom _ -> s { T.val = T.Subs $ M.singleton x $ avm y }
    T.Subs m -> s { T.val = T.Subs $ M.insertWith const2 x (avm y) m }
    where const2 _ = id


-- -- | Forces a subtree to be added.  If the feature already exists,
-- -- its value is overwritten.
-- feat :: Ord f => f -> FN i f a  -> AVM i f a
-- feat ft fn = S.modify $ \s -> s { avmAV = M.insert ft fn (avmAV s) }
-- 
-- 
-- -- | Assign the name to the AVM.  If the lable already exists,
-- -- its value is overwritten.
-- nameAVM :: i -> AVM i f a
-- nameAVM x = S.modify $ \s -> s { avmID = Just x }


--------------------------------------------------------------------
-- Other AVM combinators
--------------------------------------------------------------------


-- -- | An atomic value assigned to a feature.
-- leaf :: Ord f => f -> a -> AVM i f a
-- leaf x = feat x . atom


-- -- | A list encoded as a named feature structure.
-- list
--     :: Ord f
--     => a            -- ^ An empty list
--     -> f            -- ^ First feature
--     -> f            -- ^ Rest feature
--     -> FF i f a     -- ^ The list to encode
--     -> FN i f a
-- list nil first rest ff =
--     doit ff
--   where
--     doit (x:xs) = avm $ do
--         feat first x
--         feat rest $ doit xs
--     doit [] = atom nil


-- | A list encoded as a named feature structure.
list
    :: Ord f
    => a            -- ^ An empty list
    -> f            -- ^ First feature
    -> f            -- ^ Rest feature
    -> [AVM i f a]  -- ^ The list to encode
    -> AVM i f a
list nil first rest =
    doit
  where
    doit (x:xs) = do
        first ## x
        rest ## doit xs
    doit [] = atom nil


--------------------------------------------------------------------
-- Infix versions of common combinators
--------------------------------------------------------------------


-- | An infix version of `feat`.
(##) :: Ord f => f -> AVM i f a -> AVM i f a
(##) = feat
infixr 0 ##


-- | An infix version of `tryFeat`.
(#?) :: Ord f => f -> AVM i f a -> AVM i f a
(#?) = tryFeat
infixr 0 #?
