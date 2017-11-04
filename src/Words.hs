-- | This module defines all the
--   possible words in the game
--   and the methods using the words
module Words where

import Model

import Data.Maybe
import Data.Set

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

-- | Determines the difficulty of the word
difficulty :: String -> WordDifficulty
difficulty s | length s < 5 = Easy
             | length s >= 5 && length s < 8 = Normal
             | otherwise = Hard

-- | Given a difficulty, picks random word out of the corresponding list
pickRandomWord :: WordDifficulty -> IO String
pickRandomWord diff | diff == Easy = 
                            do randomIndex <- getRandomNumber 0 (length easyWords - 1)
                               return $ easyWords !! randomIndex 
                    | diff == Normal = 
                            do randomIndex <- getRandomNumber 0 (length normalWords - 1)
                               return $ normalWords !! randomIndex 
                    | otherwise = 
                            do randomIndex <- getRandomNumber 0 (length hardWords - 1)
                               return $ hardWords !! randomIndex 

-- | Checks if the word the player has typed is the same as an enemy's word, update the enemies in the game and update the score
checkWord :: String -> GameState -> GameState
checkWord s gstate = gstate { enemies = e, gameScore = score }
                        where
                            -- List of enemies who are still active in the game
                            e = toList (difference (fromList (enemies gstate)) enemySet)
                            -- Set of enemies which has the same word as the word the player has typed in
                            enemySet = fromList (mapMaybe (checkWordHelper s) (enemies gstate))
                            -- Updates the score
                            score = gameScore gstate + sum (Prelude.map getScore (toList enemySet))

-- | Helper function to check if the word the player has typed is the same as an enemy's word
checkWordHelper :: String -> Enemy -> Maybe Enemy
checkWordHelper s e | enemyWord e == s = Just e
                    | otherwise = Nothing

-- | Calculates the score of a defeated enemy
getScore :: Enemy -> Int
getScore e | difficulty (enemyWord e) == Easy = 100
           | difficulty (enemyWord e) == Normal = 500
           | otherwise = 1000