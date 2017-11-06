module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Typing Game" (width, height) (offset, offset))
              black            -- Background color
              fps              -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

-- | Width and Height of the screen
width, height :: Int
width = 600
height = 600

-- | The offset
offset :: Int
offset = 0

-- | Frames per second
fps :: Int
fps = 60