-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Stores the data in the current game state
data GameState = GameState {
                 state :: State,                        -- the state of the game
                 player :: Player,                      -- the player
                 enemies :: [Enemy],                    -- the enemies in the game
                 maxEnemies :: Int,                     -- max enemies that can be present at the same time
                 enemySpawnInterval :: Float,           -- used to spawn enemies at a certain interval
                 keyVar :: SpecialKey,                  -- the key
                 keyState :: KeyState,                  -- state of the key (Up or Down)
                 typedWord :: String,                   -- the word the player has typed
                 randomSpawnPosition :: (Float, Float), -- the random spawn position where an enemy will be spawned in this gamestate
                 randomWord :: String,                  -- the random word of the enemy that will be spawned in this gamestate
                 gameDifficulty :: WordDifficulty,      -- the difficulty of the game
                 gameScore :: Int,                      -- the score of the player
                 elapsedTime :: Float,                  -- the elapsed game time used to keep track of when to spawn a new enemy
                 gameTime :: Float                      -- the total elapsed game time
                 } deriving Show

-- | The initial game state
initialState :: GameState
initialState = GameState { 
               state = IsPlaying,
               player = Player { playerPos = (0, 0), playerRotationVal = 0 },
               enemies = [],
               maxEnemies = 20,
               enemySpawnInterval = 2,
               keyVar = KeyUnknown,
               keyState = Up,
               typedWord = "",
               randomSpawnPosition = (0, 350),
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

-- | Updates the highscore list when game over
updateHighScores :: GameState -> IO GameState
updateHighScores gstate | state gstate == IsGameOver = 
                            do
                                appendFile "highscores.txt" (show (gameScore gstate) ++ "\n")
                                return initialState
                        | otherwise = return gstate