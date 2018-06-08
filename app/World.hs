module World
(
    WindowDimensions,
    World(..),
    Particle(..),
    points,
    floatDimensions,
    render,
    worldStep,
    defaultWorld,
    newParticle,
    newRandomParticle
) where

import Data.List (tails)
import Control.Monad
import System.Random
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Vector as G
import Graphics.Gloss.Data.Vector (magV)

type WindowDimensions = (Int, Int)

data Particle = Particle
    {
        pos :: !G.Point,
        dir :: !G.Vector,
        colour :: !G.Color
    }

data World = World
    {
        particles :: ![Particle],
        getIntensity :: G.Point -> G.Point -> Float,
        circleRadius :: !Float,
        dimensions :: !WindowDimensions
    }

points :: World -> [G.Point]
points = map pos . particles

floatDimensions :: World -> (Float, Float)
floatDimensions World { dimensions = (w, h) } = (fromIntegral w, fromIntegral h)

render :: World -> G.Picture
render w = originTransform (floatDimensions w) $ G.pictures $ particlesP ++ connectionsP
    where
        originTransform (width, height) = G.translate (-width / 2) (-height / 2)
        -- Construct particle pictures
        translate (x, y) = G.translate x y
        makePoint p = G.color (colour p) $ translate (pos p) $ G.circleSolid (circleRadius w)
        particlesP = map makePoint $ particles w
        -- Construct line pictures
        makeLine p1 p2 i = G.color (G.withAlpha i G.white) $ G.line [p1, p2]
        connectionsP = map (\(p1, p2) -> makeLine p1 p2 (255 * getIntensity w p1 p2)) (getPairs $ points w)
    
-- Get all unique pairs: [1,2,3] -> [(1,2), (1,3), (2,3)]
-- Used to increase efficiency when constructing pictures, as we don't consider the same pair twice
getPairs :: [a] -> [(a, a)]
getPairs l = [ (x, y) | (x:t) <- tails l, y <- t ]


worldStep :: Float -> World -> World
worldStep dt w = w { particles = map (updateParticle dt w) (particles w) }


-- Bounce particles off the window edges, update positions according to velocities
updateParticle :: Float -> World -> Particle -> Particle
updateParticle dt w p 
    | x < 0      = updateParticle dt w $ p { pos = (0, y), dir = (-dx, dy) }
    | y < 0      = updateParticle dt w $ p { pos = (x, 0), dir = (dx, -dy) }
    | x > width  = updateParticle dt w $ p { pos = (width, y), dir = (-dx, dy) }
    | y > height = updateParticle dt w $ p { pos = (x, height), dir = (dx, -dy) }
    | otherwise  = p { pos = pos p + G.mulSV dt (dir p) }
    where
        (width, height) = floatDimensions w
        (x, y) = pos p
        (dx, dy) = dir p

defaultWorld :: WindowDimensions -> IO World
defaultWorld d = do
        let particleCount = 100
        let particleRadius = 2
        ps <- replicateM particleCount (newRandomParticle d)
        return World { particles = ps, getIntensity = intensity, circleRadius = particleRadius, dimensions = d }
    where
        intensity p1 p2 = 1 / (magV (p1 - p2))**1.6

newParticle :: G.Point -> IO Particle
newParticle position = Particle position <$> newDir <*> pure G.white
    where newDir = (,) <$> randomRIO (-20, 20) <*> randomRIO (-20, 20)

newRandomParticle :: WindowDimensions -> IO Particle
newRandomParticle (width, height) = newPoint >>= newParticle
    where newPoint = (,) <$> randomRIO (0, fromIntegral width) <*> randomRIO (0, fromIntegral height)