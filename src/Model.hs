-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data GameState = GameState {
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

initialState :: GameState
initialState = GameState { 
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

data Enemy = Enemy {enemyPos :: (Float, Float), enemyWord :: String } deriving (Show, Ord, Eq)
data Player = Player { playerPos :: (Float, Float), playerRotationVal :: Float } deriving Show

data WordDifficulty = Easy | Normal | Hard
                      deriving (Eq, Show)

maxEnemies :: Int
maxEnemies = 15

enemySpeed :: Float
enemySpeed = 15

playerSpeed :: Float
playerSpeed = 1

playerRotationSpeed :: Float
playerRotationSpeed = 2

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber min max = getStdRandom (randomR (min,max))

difficulty :: String -> WordDifficulty
difficulty s | length s < 5 = Easy
             | length s >= 5 && length s < 8 = Normal
             | otherwise = Hard