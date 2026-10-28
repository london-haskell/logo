{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Diagrams.Prelude hiding (font)
import Diagrams.Prelude
import DiagramsWaterfall
import Linear.V3
import Waterfall qualified as W

stationCircle = joinedStations . pure

joinedStations :: [Point V2 Double] -> Diagram B
joinedStations points = 
    let trail = fromVertices points
        points3D = map (\(P (V2 x y)) -> P (V3 x y 0)) points
    in  atPoints points3D (repeat (sphere # skin # scale 2 # sc white # translateZ 1))
            <> mconcat (zipWith
                (\a b -> let v = b .-. a in cylinder # skin # scaleZ (norm v)
                    # transform (rotationAbout 0 (direction $ cross unitZ v) (angleBetween unitZ v))
                    # moveTo a # sc white # translateZ 1.5
                ) points3D (drop 1 points3D))
            <> atPoints points (repeat (circle 3 # fc black)) # extrude 2
            <> trail # lc black # lwG 3 # extrude 2

circleLine = 
    lc (sRGB24 255 211 0)

victoriaLine = 
    lc (sRGB24 0 152 212)

bakerlooLine =
    lc (sRGB24 179 99 5)

centralLine =
    lc (sRGB24 227 32 23)

hammersmithAndCityLine = 
    lc (sRGB24 243 169 187)

districtLine = 
    lc (sRGB24 0 120 42)

leg = fromSegments 
    [straight (r2 (-10, 10))
    , bezier3 zero (r2 (-2, 2)) (r2 (-2, 4))
    , straight (r2 (0, 4))
    ]

horizontalA = straight (r2 (24, 0)) `at` (p2 (6, 6))

horizontalB = straight (r2 (12, 0)) `at` (p2 (18, -6))

upperLine = translate (r2 (0, 1))
lowerLine = translate (r2 (0, -1))

lambdaTube =
    ( mconcat
    [ joinedStations [zero, p2 (6, 6)]
    , stationCircle $ p2 (12, 0)
    , stationCircle $ p2 (18, (-6))
    ] <> mconcat
    [ leg # lwG 2 # victoriaLine
    , leg # reflectY # lwG 2 # victoriaLine
    , leg # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # reflectX # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # translate (12 *^ unit _x) # lwG 2 # centralLine
    , horizontalA # stroke # lwG 2 # hammersmithAndCityLine # upperLine 
    , horizontalA # stroke # lwG 2 # circleLine # lowerLine
    , horizontalB # stroke # lwG 2 # districtLine # lowerLine 
    , horizontalB # stroke # lwG 2 # circleLine # upperLine
    ]
        # extrude 2
    )
    # center

logoName = 
    text "London Haskell"
        # font "P22 Johnston Underground"
        # fontSizeG 8
        # fc (sRGB24 0 54 136)

diag =
    (atPoints [0, p3 (0, -14, 1)] [lambdaTube, logoName # extrude 0.5])
    # pad 1.5

main = do
    -- renderSVG "logo.svg" (mkSizeSpec $ V2 Nothing (Just 72)) diag
    globalFont <- W.fontFromPath "P22 Johnston Underground Regular.ttf" 8
    W.writeSolid 0.1 "logo.glb" $
        renderWaterfall
            globalFont
            -- $ mconcat
            --     [ cube & skin & scale 10 & sc blue & translateX 5
            --     , cube & skin & scale 10 & sc red & translateZ -20 & translateX 5
            --     , sphere & skin & translateY 1 & scale 5 & sc green & scaleZ 3 & translateX -5
            --     ]
            --     # translateX -40
            --     # translateZ -20
            -- <>
            diag
