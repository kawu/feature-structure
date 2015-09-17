{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

{-# LANGUAGE RankNTypes #-}


-- | Draw a feature graph using the diagrams framework.


module NLP.FeatureStructure.Graph.Draw where


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Strict as ST
import           Data.List (intersperse)


import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine

-- Text and fonts
import           Graphics.SVGFonts.Text
import           Graphics.SVGFonts.Fonts (lin2, bit)


import           NLP.FeatureStructure.Graph


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


-- | (Graph, Root ID) to AVM transformation.
toAVM :: (Ord i, Ord f) => Graph i f a -> i -> AvmID i f a
toAVM g =
    flip ST.evalState S.empty . toAvmID
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


drawAvmId AvmID{..}
    =   addBounds (pad 1.1 $ textSmall $ show avmID)
    ||| strutX 1
    ||| drawAvm avmPR


drawAvm (Leaf x) = text' $ show x
drawAvm (Node m)
  = addBounds
  $ frame 1
  $ centerXY
  $ vcat
  $ addPauses
    [ text' (show attr) ||| text' ":" |||
      strutX 3   ||| drawAvmId avmId
    | (attr, avmId) <- M.toList m ]
  where
    addPauses = intersperse $ strutY 1
    drawVal = text'


addBounds x = x <> boundingRect x


----------------------------------------
-- Usage
----------------------------------------


testGraph = Graph $ M.fromList
    [ (0, Interior $ M.fromList
        [("X", 1), ("Y", 2), ("Z", 0)])
    , (1, Interior $ M.fromList
        [("X", 2), ("Y", 3)])
    , (2, Frontier "a")
    , (3, Frontier "b") ]


testMain =
    let testAvm = toAVM testGraph 0
    in  mainWith (drawAvmId testAvm :: Diagram B)


----------------------------------------
-- Helpers
----------------------------------------


-- | Draw the given text with the given size.
textSize k t = stroke
    ( textSVG' (TextOpts bit INSIDE_H KERN False undefined k)
    t ) # fillRule EvenOdd # fc black -- # lc blue # bg lightgrey


-- | Draw the given text.
text' = textSize 20


-- | Draw a small text.
textSmall = textSize 5
