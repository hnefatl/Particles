module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import World
import EventHandler

main :: IO ()
main = do
    let fps = 60
    let size = (1920, 1000)
    let window = FullScreen --InWindow "Particles" size (0, 0)
    world <- defaultWorld size
    playIO window black fps world (return . render) eventHandler (\dt w -> return $ worldStep dt w)