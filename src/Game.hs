-- | Basic typing game functions and variables
module Game where

import System.Random
import Data.Maybe
import Data.Set hiding (map)

import Model

-- | Speed of the player
playerSpeed :: Float
playerSpeed = 1

-- | How fast the player rotates
playerRotationSpeed :: Float
playerRotationSpeed = 2

-- | Radius of the player
playerRadius :: Float
playerRadius = 10

-- | Speed of the enemy
enemySpeed :: Float
enemySpeed = 15

-- | Radius of the enemy
enemyRadius :: Float
enemyRadius = 10

-- | Gets a random number between a given range
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber minInt maxInt = getStdRandom (randomR (minInt, maxInt))

-- | Checks if the word the player has typed is the same as an enemy's word, update the enemies in the game and update the score
checkWord :: String -> GameState -> GameState
checkWord s gstate = gstate { enemies = e, gameScore = score }
                    where
                        -- List of enemies who are still active in the game
                        e = toList (difference (fromList (enemies gstate)) enemySet)
                        -- Set of enemies which has the same word as the word the player has typed in
                        enemySet = fromList (mapMaybe (checkWordHelper s) (enemies gstate))
                        -- Updates the score
                        score = gameScore gstate + sum (map getScore (toList enemySet))
    
-- | Helper function to check if the word the player has typed is the same as an enemy's word
checkWordHelper :: String -> Enemy -> Maybe Enemy
checkWordHelper s e | enemyWord e == s = Just e
                    | otherwise = Nothing
    
-- | Calculates the score of a defeated enemy
getScore :: Enemy -> Int
getScore e | getWordDifficulty (enemyWord e) == Easy = 100
           | getWordDifficulty (enemyWord e) == Normal = 500
           | otherwise = 1000

-- | Determines the difficulty of the word
getWordDifficulty :: String -> Difficulty
getWordDifficulty s | length s < 5 = Easy
                    | length s >= 5 && length s < 8 = Normal
                    | otherwise = Hard
                    
-- | Handles enemy collision
handleEnemyCollision :: GameState -> GameState
handleEnemyCollision gstate | True `elem` map (checkEnemyCollision gstate) (enemies gstate) = gstate { state = IsGameOver }
                            | otherwise = gstate { state = IsPlaying }

-- | Checks if an enemy collides with the player, so when the distance between the enemy and the player is smaller than the sum of their radius
checkEnemyCollision :: GameState -> Enemy -> Bool
checkEnemyCollision gstate e = sqrt (x^2 + y^2) < playerRadius + enemyRadius
                            where
                                x = fst (playerPos (player gstate)) - fst (enemyPos e)
                                y = snd (playerPos (player gstate)) - snd (enemyPos e)

-- | Updates the highscore list when game over and now you can restart the game
updateHighScores :: GameState -> IO GameState
updateHighScores gstate = do appendFile "highscores.txt" (show (gameScore gstate) ++ "\n")
                             return gstate { state = CanRestart }