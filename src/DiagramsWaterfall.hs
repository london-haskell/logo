{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO very WIP
-- heavily based on my Gloss backend, as well as the SVG backend
-- what's quite different is how we render paths, since Waterfall has no explicit notion of lines with width
-- not widely tested - only really on London Haskell logo
-- see various TODOs below

-- | A Diagrams backend for Waterfall CAD.
module DiagramsWaterfall (
    renderWaterfall,
    B,
    Waterfall (Waterfall),
    Options (WaterfallOptions, globalFont),
    extrude,
) where

import Control.Monad
import Control.Monad.Zip
import Data.Colour qualified
import Data.Foldable
import Data.Function
import Data.Kind
import Data.Maybe
import Data.Tree
import Data.Typeable
import Diagrams.Core.Compile
import Diagrams.Prelude
import Diagrams.Transform.Matrix
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Text
import Linear
import Waterfall qualified as W

renderWaterfall :: W.Font -> Diagram (Waterfall V3) -> W.Solid
renderWaterfall globalFont = renderDia Waterfall WaterfallOptions{globalFont}

-- TODO parameterise this by a monad so that we can have an optional pure renderer, and only need IO for text?
-- i.e. have pretty much everything work for any monad, but then only `instance Renderable (Text Double) (Waterfall IO)`
-- for now we just have a pure renderer which requires a single font to be passed in up front and used everywhere
data Waterfall (v :: Type -> Type) = Waterfall
type B = Waterfall V3

type instance V (Waterfall v) = v
type instance N (Waterfall v) = Double

type family WaterfallResult (v :: Type -> Type) where
    WaterfallResult V2 = [(W.Shape, AlphaColour Double)]
    WaterfallResult V3 = W.Solid

instance (Monoid (WaterfallResult v)) => Backend (Waterfall v) v Double where
    type Result (Waterfall v) v Double = WaterfallResult v
    newtype Render (Waterfall v) v Double = Render
        { unRender :: Options (Waterfall v) v Double -> Style v Double -> Result (Waterfall v) v Double
        }
    data Options (Waterfall v) v Double = WaterfallOptions
        { globalFont :: W.Font -- TODO temporary crutch - see above about fonts and pure rendering
        }
    renderRTree Waterfall opts t =
        fold $ fmap (uncurry (flip prim)) $ mzip t $ collectParents $ fmap (fromMaybe mempty . style) t
      where
        -- TODO don't ignore annotation nodes?
        style = \case
            RStyle s -> Just s
            _ -> Nothing
        prim sty = \case
            RPrim p -> (unRender $ render Waterfall p) opts sty
            _ -> mempty

-- TODO is there a way to retain information about richer shapes, e.g. turn circles directly in to circles?
-- probably not currently: https://github.com/diagrams/diagrams-svg/issues/121
instance Renderable (Path V2 Double) (Waterfall V2) where
    render Waterfall path = Render \_ sty -> do
        trail <- pathTrails path
        guard $ not $ isTrailEmpty $ unLoc trail -- prevents segfault
        concat
            [ maybeToList $ (renderLocTrail trail,) <$> getFillColour sty
            , do
                w <- getLineWidth <$> maybeToList (getAttr sty)
                guard $ w > 0 -- main base case
                c <- maybeToList $ getLineColour sty
                -- TODO this probably isn't handling hollow shapes correctly - we'll need `W.difference2D`
                shape <- renderLocTrail <$> pathTrails (expandTrail (w / 2) $ undegenerate trail)
                pure (shape, c)
            ]
      where
        renderLocTrail trail =
            W.makeShape
                . applyWhen (isLoop $ unLoc trail) W.closeLoop2D
                . foldMap
                    ( \case
                        FLinear (P a) (P b) -> W.line2D a b
                        FCubic (P a) (P b) (P c) (P d) -> W.bezier2D a b c d
                    )
                $ fixTrail trail

-- TODO stop ignoring text alignment, non-colour styles etc.
instance Renderable (Text Double) (Waterfall V2) where
    render Waterfall (Text tt _ s) = Render \opts sty -> mconcat do
        c <- maybeToList $ getFillColour sty
        pure
            . pure
            . (,c)
            . convertTransform2D tt
            $ W.text opts.globalFont s

-- TODO add:
-- Path V3 Double
-- CSG Double
-- all other 3D shapes from https://hackage-content.haskell.org/package/diagrams-lib/docs/Diagrams-ThreeD-Shapes.html
instance Renderable (Ellipsoid Double) (Waterfall V3) where
    render Waterfall (Ellipsoid t) = Render \_ sty ->
        maybe id convertColour (getSurfaceColour sty) $ convertTransform t W.unitSphere
instance Renderable (Box Double) (Waterfall V3) where
    render Waterfall (Box t) = Render \_ sty ->
        maybe id convertColour (getSurfaceColour sty) $ convertTransform t $ W.box 1
instance Renderable (Frustum Double) (Waterfall V3) where
    render Waterfall (Frustum _ _ t) = Render \_ sty ->
        -- TODO handle properly (solid of revolution?), rather than assuming everything is a unit cylinder
        maybe id convertColour (getSurfaceColour sty) $ convertTransform t $ W.unitCylinder

convertTransform :: Transformation V3 Double -> W.Solid -> W.Solid
convertTransform t = W.translate (transl t) . W.matTransform (mkMat t <&> \(V3 x y z) -> V4 x y z 0)
convertTransform2D :: Transformation V2 Double -> W.Shape -> W.Shape
convertTransform2D t = W.translate2D (transl t) . W.matTransform2D (mkMat t <&> \(V2 x y) -> V3 x y 0)

getSurfaceColour :: Style v Double -> Maybe (Colour Double)
getSurfaceColour = view _sc
getFillColour :: Style v Double -> Maybe (AlphaColour Double)
getFillColour = textureToColour . getFillTexture <=< getAttr
getLineColour :: Style v Double -> Maybe (AlphaColour Double)
getLineColour = textureToColour . getLineTexture <=< getAttr
textureToColour :: Texture Double -> Maybe (AlphaColour Double)
textureToColour = \case
    SC c -> Just $ someToAlpha c
    -- TODO handle gradients
    LG _ -> Nothing
    RG _ -> Nothing

convertColourA :: AlphaColour Double -> W.Solid -> W.Solid
convertColourA c =
    W.paintSolid
        ( colourToPaint (c `Data.Colour.over` black)
            <> W.paintWithOpacity (alphaChannel c)
        )
convertColour :: Colour Double -> W.Solid -> W.Solid
convertColour = W.paintSolid . colourToPaint
colourToPaint :: Colour Double -> W.Paint
colourToPaint c = W.paintWithColour (W.Colour r g b)
  where
    RGB r g b = toSRGB c

-- TODO is there a better API than parameterising the backend type and creating entire 2D Diagrams?
-- e.g. would it be better to just make all of the primitives renderable directly in 3D?
-- TODO also is there a better name for this?
-- it seems somewhat standard, but then there are identifers in Diagrams using that word quite differently
-- I'd be happy to just use Prism, but that clashes with Diagrams' lens re-exports
data Extrusion m = Extrusion (T3 Double) Double (QDiagram (Waterfall V2) V2 Double m)
type instance V (Extrusion m) = V3
type instance N (Extrusion m) = Double
instance Transformable (Extrusion m) where
    transform t (Extrusion t' z d) = Extrusion (t <> t') z d
instance (Monoid m) => Renderable (Extrusion m) (Waterfall V3) where
    -- TODO _sty being ignored is a bit sus
    render Waterfall (Extrusion t prismLength dia) = Render \(WaterfallOptions{globalFont}) _sty ->
        convertTransform t
            . foldMap (uncurry (\d c -> convertColourA c $ W.prism prismLength d))
            $ renderDia Waterfall (WaterfallOptions{globalFont}) dia
extrude :: (Typeable m, Monoid m) => Double -> QDiagram (Waterfall V2) V2 Double m -> QDiagram B V3 Double m
extrude d dia =
    mkQD
        (Prim $ Extrusion mempty d dia)
        -- TODO fill envelope etc. in properly, if we're sticking with this API
        mempty
        mempty
        mempty
        (Query \(P (V3 x y z)) -> if 0 <= z && z <= d then sample dia $ P (V2 x y) else mempty)

-- TODO is this equivalent to some standard recursion scheme?
-- we can't use anything like `foldTree` as we need to pass things _down_
-- and `traverse` etc. don't rewind when re-ascending
-- the simpler version would be inefficient (prefixes keep getting re-evaluated):
-- collectParents (Node n ts) = Node n $ map (((n <>) <$>) . collectParents) ts
collectParents :: (Monoid a) => Tree a -> Tree a
collectParents = go mempty
  where
    go r (Node n ts) = let r' = r <> n in Node r' $ map (go r') ts

-- TODO this is a vibed hack workaround for https://github.com/diagrams/diagrams-lib/issues/268#issuecomment-5209421453

{- | Nudge zero-length Bezier control handles off zero so that
`Diagrams.TwoD.Offset` can derive a tangent direction from them.

`offsetSegment` gets its offset direction from @unitPerp = signorm . perp@,
which for a zero handle is @V2 NaN NaN@; its termination test is a comparison,
and comparisons against NaN are always False, so the subdivision never
terminates. Subdividing cannot rescue it either: halving a cubic whose first
handle is zero leaves the first half's handle zero too.

We substitute a very short handle pointing along the limiting tangent, which
is given by the next derivative. For control points 0, a, b, c:
  B''(0) = 6b - 12a       -- with a == 0, the start direction is b
  B(t)   = t^3 c          -- with a == b == 0, the segment is straight, along c
  B''(1) = 6a - 12b + 6c  -- with b == c, the end direction is a - c

Note @unitPerp@ normalises, so @eps@ has *no* effect on the direction the
offset is computed in -- it is exactly the direction the upstream fix picks.
It only controls how much the curve itself is perturbed, so keep it small; it
need only be large enough that @quadrance@ does not underflow.
-}
undegenerate :: Located (Trail V2 Double) -> Located (Trail V2 Double)
undegenerate =
    mapLoc $
        withTrail
            (wrapLine . onLineSegments (map fixSeg))
            -- glueLine . cutLoop === id, and glueLine re-closes the loop, so
            -- this is safe even though fixSeg perturbs the geometry
            (wrapLoop . glueLine . onLineSegments (map fixSeg) . cutLoop)
  where
    fixSeg = \case
        s@(Cubic c1 c2 (OffsetClosed c3))
            | degenerate c1 && degenerate c2 -> Linear (OffsetClosed c3)
            | degenerate c1 -> Cubic (c2 ^* eps) c2 (OffsetClosed c3)
            | degenerate (c3 ^-^ c2) -> Cubic c1 (c2 ^+^ (c1 ^-^ c3) ^* eps) (OffsetClosed c3)
            | otherwise -> s
        s -> s
    eps = 1e-9
    -- Same reasoning as the upstream patch: ask whether `unitPerp` can actually
    -- produce a direction, rather than picking an arbitrary length threshold.
    -- (`unitPerp` itself isn't exported, hence `signorm . perp` inline.)
    degenerate v = not (all finite (signorm (perp v)))
      where
        finite x = not (isNaN x || isInfinite x)
