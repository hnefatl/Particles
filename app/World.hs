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

import System.Random
import qualified Data.Vector as V
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
        particles :: !(V.Vector Particle),
        getIntensity :: Particle -> Particle -> Float,
        circleRadius :: !Float,
        dimensions :: !WindowDimensions
    }

points :: World -> V.Vector G.Point
points = V.map pos . particles

floatDimensions :: World -> (Float, Float)
floatDimensions World { dimensions = (w, h) } = (fromIntegral w, fromIntegral h)

render :: World -> G.Picture
render w = originTransform (floatDimensions w) $ G.pictures $ particlesP ++ connectionsP
    where
        originTransform (width, height) = G.translate (-width / 2) (-height / 2)
        -- Construct particle pictures
        translate (x, y) = G.translate x y
        makePoint p = G.color (colour p) $ translate (pos p) $ G.circleSolid (circleRadius w)
        particlesP = V.toList $ V.map makePoint $ particles w
        -- Construct line pictures
        connectionsP = V.toList $ V.map (makeLine w) (getPairs $ particles w)
    
makeLine :: World -> (Particle, Particle) -> G.Picture
makeLine w (p1, p2) = G.color (G.withAlpha i avgColour) $ G.line [pos p1, pos p2]
    where
        avgColour = G.mixColors 0.5 0.5 (colour p1) (colour p2)
        i = 255 * getIntensity w p1 p2

-- Get all unique pairs: [1,2,3] -> [(1,2), (1,3), (2,3)]
-- Used to increase efficiency when constructing pictures, as we don't consider the same pair twice
getPairs :: V.Vector a -> V.Vector (a, a)
getPairs l = V.fromList $ [ (l V.! x, l V.! y) | x <- [0..V.length l-1], y <- [0..V.length l-1], x < y ]

worldStep :: Float -> World -> World
worldStep dt w = w { particles = V.map (updateParticle dt w) (particles w) }

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
        ps <- V.replicateM particleCount (newRandomParticle d)
        return World { particles = ps, getIntensity = intensity, circleRadius = particleRadius, dimensions = d }
    where
        intensity p1 p2 = 1 / (magV (pos p1 - pos p2))**1.6

newParticle :: G.Point -> IO Particle
newParticle position = Particle position <$> newDir <*> col
    where newDir = (,) <$> randomRIO (-20, 20) <*> randomRIO (-20, 20)
          col = G.makeColor <$> randomRIO (0,1) <*> randomRIO (0,1) <*> randomRIO (0,1) <*> pure 1

newRandomParticle :: WindowDimensions -> IO Particle
newRandomParticle (width, height) = newPoint >>= newParticle
    where newPoint = (,) <$> randomRIO (0, fromIntegral width) <*> randomRIO (0, fromIntegral height)