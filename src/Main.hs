module Main where

import Controller
import Model
import View
import Game

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Typing Game" (width, height) (offset, offset))
              black            -- Background color
              fps              -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function