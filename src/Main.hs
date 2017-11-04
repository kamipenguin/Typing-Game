module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Typing Game" (width, height) (offset, offset)) -- Or FullScreen
              black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

width, height :: Int
width = 400
height = 400

offset :: Int
offset = 0

fps :: Int
fps = 60