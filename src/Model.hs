-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Stores the data in the current game state
data GameState = GameState {
                 state :: State,                        -- the state of the game
                 player :: Player,                      -- the player
                 enemies :: [Enemy],                    -- the enemies in the game
                 maxEnemies :: Int,                     -- max enemies that can be present at the same time
                 enemySpawnInterval :: Float,           -- used to spawn enemies at a certain interval
                 elapsedTime :: Float,                  -- the elapsed game time used to keep track of how many seconds have passed by after spawning a new enemy
                 keyVar :: SpecialKey,                  -- the key
                 keyState :: KeyState,                  -- state of the key (Up or Down)
                 typedWord :: String,                   -- the word the player has typed
                 randomSpawnPosition :: Point,          -- the random spawn position where an enemy will be spawned in this gamestate
                 randomWord :: String,                  -- the random word of the enemy that will be spawned in this gamestate
                 gameDifficulty :: Difficulty,          -- the difficulty of the game
                 gameScore :: Int,                      -- the score of the player
                 gameTime :: Float,                     -- the total elapsed game time
                 animationDuration :: Float             -- the duration of the "animation"
                 } deriving Show

-- | The initial game state
initialState :: GameState
initialState = GameState { 
               state = IsPlaying,
               player = Player { playerPos = (0, 0), playerRotationVal = 0 },
               enemies = [],
               maxEnemies = 20,
               enemySpawnInterval = 2,
               elapsedTime = 0,
               keyVar = KeyUnknown,
               keyState = Up,
               typedWord = "",
               randomSpawnPosition = (0, 350),
               randomWord = "test",
               gameDifficulty = Easy,
               gameScore = 0,
               gameTime = 0,
               animationDuration = 1.5
               }

-- | Defines the states of the game
data State = IsPlaying | IsGameOver | IsPaused | CanRestart
            deriving (Show, Eq)

-- | Stores the data of an enemy
data Enemy = Enemy {enemyPos :: Point, enemyWord :: String } 
            deriving (Show, Ord, Eq)
            
-- | Stores the data of the player
data Player = Player { playerPos :: Point, playerRotationVal :: Float } 
             deriving Show

-- | Defines the difficulty of the game
data Difficulty = Easy | Normal | Hard
                     deriving (Eq, Show)