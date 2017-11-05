-- | This module defines all the
--   possible words in the game
--   and functions with words
module Words where

import Model
import Game

-- | List of all the easy words
easyWords :: [String]
easyWords = [
            "time", "mark", "ufo", "crow", "dish", 
            "map", "fate", "cook", "page", "fire", 
            "ice", "cat", "era", "nice", "red"
            ]

-- | List of all the normal words
normalWords :: [String]
normalWords = [
              "scarf", "sweet", "potato", "penguin", "pearl", 
              "obvious", "enough", "grammar", "majesty", "haskell", 
              "yoghurt", "exact", "squid", "painter", "south"
              ]

-- | List of all the hard words
hardWords :: [String]
hardWords = [
            "unconsidered", "advantageous", "balancing", "surfacing", "normalize", 
            "anaesthesia", "foldable", "giftwrap", "laziness", "cauliflower", 
            "photosynthesis", "population", "surround", "cropland", "ambiance"
            ]

-- | Given a difficulty, picks random word out of the corresponding list
pickRandomWord :: Difficulty -> IO String
pickRandomWord diff | diff == Easy = 
                            do randomIndex <- getRandomNumber 0 (length easyWords - 1)
                               return $ easyWords !! randomIndex 
                    | diff == Normal = 
                            do randomIndex <- getRandomNumber 0 (length normalWords - 1)
                               return $ normalWords !! randomIndex 
                    | otherwise = 
                            do randomIndex <- getRandomNumber 0 (length hardWords - 1)
                               return $ hardWords !! randomIndex 
