-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Stores the data in the current game state
data GameState = GameState {
                 state :: State,
                 player :: Player,
                 enemies :: [Enemy],
                 keyVar :: SpecialKey,
                 keyState :: KeyState,
                 typedWord :: String,
                 randomSpawnPosition :: (Float, Float),
                 randomWord :: String,
                 gameDifficulty :: WordDifficulty,
                 gameScore :: Int,
                 elapsedTime :: Float,
                 gameTime :: Float
                 } deriving Show

-- | The initial game state
initialState :: GameState
initialState = GameState { 
               state = IsPlaying,
               player = Player { playerPos = (0, 0), playerRotationVal = 0 },
               enemies = [],
               keyVar = KeyUnknown,
               keyState = Up,
               typedWord = "",
               randomSpawnPosition = (0, 250),
               randomWord = "test",
               gameDifficulty = Easy,
               gameScore = 0,
               elapsedTime = 0,
               gameTime = 0 
               }

-- | Defines the states of the game
data State = IsPlaying | IsGameOver | IsPaused
            deriving (Show, Eq)

-- | Stores the data of an enemy
data Enemy = Enemy {enemyPos :: (Float, Float), enemyWord :: String } 
            deriving (Show, Ord, Eq)
-- | Stores the data of the player
data Player = Player { playerPos :: (Float, Float), playerRotationVal :: Float } 
             deriving Show

-- | Defines the word difficulty
data WordDifficulty = Easy | Normal | Hard
                     deriving (Eq, Show)

-- | Used to spawn enemies at a specified interval
noSecsBetweenCylces :: Float
noSecsBetweenCylces = 2

-- | The max enemies that can be present at the same time in the game
maxEnemies :: Int
maxEnemies = 15

-- | How fast the enemy is
enemySpeed :: Float
enemySpeed = 15

-- | How fast the player is
playerSpeed :: Float
playerSpeed = 1

-- | How fast the player rotates
playerRotationSpeed :: Float
playerRotationSpeed = 2

-- | Gets a random number between a given range
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber min max = getStdRandom (randomR (min,max))

-- | Updates the highscore list
-- TODO PUT THIS SOMEWHERE TO EXECUTE WHEN THE GAME IS OVER
updateHighScores :: GameState -> IO ()
updateHighScores gstate = do
                            currentHighScores <- readFile "highscores.txt"
                            writeFile "highscores.txt" (currentHighScores ++ "\n" ++ show (gameScore gstate))