-- | Basic game functions and variables
module Game where

import System.Random
import System.IO
import Data.Maybe
import Data.Set

import Model

-- | How fast the player is
playerSpeed :: Float
playerSpeed = 1

-- | How fast the player rotates
playerRotationSpeed :: Float
playerRotationSpeed = 2

-- | Radius of the player
playerRadius :: Float
playerRadius = 10

-- | How fast the enemy is
enemySpeed :: Float
enemySpeed = 15

-- | Radus of the enemy
enemyRadius :: Float
enemyRadius = 10

-- | Gets a random number between a given range
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber min max = getStdRandom (randomR (min,max))

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

-- | Determines the difficulty of the word
getWordDifficulty :: String -> Difficulty
getWordDifficulty s | length s < 5 = Easy
                    | length s >= 5 && length s < 8 = Normal
                    | otherwise = Hard
    
-- | Calculates the score of a defeated enemy
getScore :: Enemy -> Int
getScore e | getWordDifficulty (enemyWord e) == Easy = 100
           | getWordDifficulty (enemyWord e) == Normal = 500
           | otherwise = 1000

-- | Updates the highscore list when game over
updateHighScores :: GameState -> IO GameState
updateHighScores gstate | state gstate == IsGameOver = 
                            do
                                appendFile "highscores.txt" (show (gameScore gstate) ++ "\n")
                                return initialState
                        | otherwise = return gstate