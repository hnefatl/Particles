module EventHandler
(
    eventHandler
) where

import World
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

eventHandler :: Event -> World -> IO World
eventHandler (EventResize newDims) w = return $ w { dimensions = newDims }
eventHandler (EventKey (MouseButton LeftButton) Down _ position) w = do
    let (width, height) = floatDimensions w
    p <- newParticle (position + (width/2, height/2))
    return $ w { particles = p:particles w}
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
eventHandler _ w = return $ w