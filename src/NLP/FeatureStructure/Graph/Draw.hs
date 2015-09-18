{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


-- | Draw a feature graph using the diagrams framework.


module NLP.FeatureStructure.Graph.Draw where


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Strict as ST
import           Data.List (intersperse)
-- import           Data.Typeable (Typeable)


import           Diagrams.Prelude
-- import           Diagrams.Backend.SVG.CmdLine

-- Text and fonts
import           Graphics.SVGFonts.Text
import           Graphics.SVGFonts.Fonts (lin2) -- , bit)


-- String printing
import           Data.String.ToString (toString, ToString)


import           NLP.FeatureStructure.Graph


----------------------------------------
-- Drawing parameters (configuration)
----------------------------------------


-- | Size of feature text.
featSize :: Double
featSize = 20.0


-- -- | Size of feature value text.
-- valSize :: Double
-- valSize = 18.0


-- | Size of identifier text.
idSize :: Double
idSize = 10.0


-- | Tiny space size.
tinyBreakSize :: Double
tinyBreakSize = 1 


-- | Small space size.
smallBreakSize :: Double
smallBreakSize = 3


----------------------------------------
-- Drawing graphs and nodes
----------------------------------------


-- | A "local" diagram monad for drawing graph nodes.
type Diag i = ST.State (S.Set i)


-- | Evaluation of the local diagram monad.
runDiag :: Diag i b -> b
runDiag = flip ST.evalState S.empty


-- | Draw the particular node of the graph.
drawNode
    :: ( Ord i, Ord f, Show i, Renderable (Path V2 Double) b
       , ToString a, ToString f )
    => Graph i f a -> i -> Diag i (QDiagram b V2 Double Any)
drawNode g i = do
    avmID <- toAVM g i
    return $ drawAvmId avmID


-- | Pure version of the drawNode function.
drawNode_
    :: ( Ord i, Ord f, Show i, Renderable (Path V2 Double) b
       , ToString a, ToString f )
    => Graph i f a -> i -> QDiagram b V2 Double Any
drawNode_ g = runDiag . drawNode g


----------------------------------------
-- AVM representation
----------------------------------------


-- | An AVM with an ID.
data AvmID i f a = AvmID {
    -- | Avm identifier.
      avmID :: i
    -- | Avm proper.
    , avmPR :: Avm i f a
    } deriving (Show, Eq, Ord)


-- | An AVM representation.
data Avm i f a
    = Leaf a
    | Node (M.Map f (AvmID i f a))
    deriving (Show, Eq, Ord)


-- | (Graph, Root ID) to AVM transformation.  Non-monadic
-- version.
toAVM_ :: (Ord i, Ord f) => Graph i f a -> i -> AvmID i f a
toAVM_ g = runDiag . toAVM g


-- | (Graph, Root ID) to AVM transformation.
toAVM :: (Ord i, Ord f) => Graph i f a -> i -> Diag i (AvmID i f a)
toAVM g =
    toAvmID
  where
    nodeToAvm (Frontier x) = return $ Leaf x
    nodeToAvm (Interior m) = Node . M.fromList
       <$> mapM pairM (M.toList m)
    pairM (x, i) = (x,) <$> toAvmID i
    toAvmID i = do
        b <- hasID i
        if (not b) then do
            saveID i
            AvmID i <$> nodeToAvm (nodeMap g M.! i)
        else do
            return $ AvmID i $ Node M.empty
    saveID i = ST.modify $ S.insert i
    hasID  i = ST.gets   $ S.member i


----------------------------------------
-- Showing AVM data structure
----------------------------------------


-- | Construct a diagram from an AVM with ID.
drawAvmId
    :: ( Show i, ToString a, ToString f
       , Renderable (Path V2 Double) b )
    => AvmID i f a
    -> QDiagram b V2 Double Any
drawAvmId AvmID{..}
    =   renderID (show avmID)
    ||| strutX 1
    ||| drawAvm avmPR
  where
    renderID i = drawText idSize grey i # pad 1.5 --  # fc grey


-- | Construct a diagram from an AVM.
drawAvm
    :: ( Show i, ToString a, ToString f
       , Renderable (Path V2 Double) b )
    => Avm i f a -> QDiagram b V2 Double Any
drawAvm (Leaf x) = drawText featSize black $ toString x
drawAvm (Node m)
  = frame tinyBreakSize
  $ addBounds
  $ frame tinyBreakSize
  $ centerXY
  $ vcat -- vsep (-5)
--   $ addPauses
    [ drawText featSize black (toString attr)  |||
      strutX tinyBreakSize  ||| drawText featSize black ":" |||
      strutX smallBreakSize ||| drawAvmId avmId
    | (attr, avmId) <- M.toList m ]
--   where
--     addPauses = intersperse $ strutY tinyBreakSize


-- addBounds
--     :: ( Monoid a, Semigroup a, TrailLike a
--        , Transformable a, Enveloped a
--        , V a ~ V2)
--     => a -> a
addBounds x = x <> (boundingRect x # lw ultraThin)


----------------------------------------
-- Usage
----------------------------------------


-- testGraph = Graph $ M.fromList
--     [ (0, Interior $ M.fromList
--         [("X", 1), ("Y", 2), ("Z", 0)])
--     , (1, Interior $ M.fromList
--         [("X", 2), ("Y", 3)])
--     , (2, Frontier "a")
--     , (3, Frontier "b") ]
-- 
-- 
-- testMain =
--     let testAvm = toAVM testGraph 0
--     in  mainWith (drawAvmId testAvm :: Diagram B)


----------------------------------------
-- Helpers
----------------------------------------


-- | Draw the given text with the given size.
drawText
    :: Renderable (Path V2 Double) b
    => Double -> Colour Double -> String
    -> QDiagram b V2 Double Any
drawText theSize col theText = id
    ( textSVG_ (TextOpts lin2 INSIDE_H KERN False undefined theSize)
    theText ) # fillRule EvenOdd # lw ultraThin # fc col # lc col -- # bg lightgrey


-- | Draw the given text with the given size.  Origin in the
-- center of the text.
drawText'
    :: Renderable (Path V2 Double) b
    => Double -> Colour Double -> String
    -> QDiagram b V2 Double Any
drawText' theSize col theText = stroke
    ( textSVG' (TextOpts lin2 INSIDE_H KERN False undefined theSize)
    theText ) # fillRule EvenOdd # lw ultraThin # fc col # lc col
