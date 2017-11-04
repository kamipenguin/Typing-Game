-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
import Model
import View
import Words

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
  
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = spawnEnemies secs $ movePlayer $ updateEnemies secs $ updateDifficulty secs $ handleCollision $ checkGameState gstate

-- | Checks in which state the game is and act accordingly
checkGameState :: GameState -> GameState
                        -- TODO MAKE GAME OVER STATE (eg freeze the game and print game over and save high score in file)
checkGameState gstate | state gstate == IsGameOver = initialState
                        -- TODO MAKE PAUSE FUNCTIONALITY
                      | state gstate == IsPaused = gstate
                      | otherwise = gstate
  
-- | Spawns an enemy after some amount of time
spawnEnemies :: Float -> GameState -> IO GameState
spawnEnemies secs gstate | elapsedTime gstate + secs > noSecsBetweenCylces && length (enemies gstate) < maxEnemies
                                -- Spawn an enemy at a random position outside a certain radius and with a random word
                            = do randomAngle <- getRandomNumber 0 360
                                 word <- pickRandomWord (gameDifficulty gstate)
                                 return $ gstate { 
                                          enemies = e, 
                                          randomSpawnPosition = mulSV 240 (unitVectorAtAngle ((fromIntegral (randomAngle) - 90) * pi / 180)), 
                                          randomWord = word, elapsedTime = 0 
                                          }
                         | otherwise
                                -- Just update the elapsed time
                            = return $ gstate { elapsedTime = elapsedTime gstate + secs }
                            where
                                currentEnemies = enemies gstate
                                e = currentEnemies ++ [Enemy { enemyPos = randomSpawnPosition gstate, enemyWord = randomWord gstate }]

-- | Updates the game difficulty over time
updateDifficulty :: Float -> GameState -> GameState
updateDifficulty secs gstate | gameTime gstate > 30 = gstate { gameDifficulty = Normal, gameTime = updatedTime }
                             | gameTime gstate > 60 = gstate { gameDifficulty = Hard, gameTime = updatedTime }
                             | otherwise = gstate { gameDifficulty = Easy, gameTime = updatedTime }
                             where
                                updatedTime = gameTime gstate + secs

-- | Updates the enemy
updateEnemies :: Float -> GameState -> GameState
updateEnemies secs gstate = gstate { enemies = e }
                            where e = getUpdatedEnemies secs gstate

-- | Gets the enemies in the game and updates the position
getUpdatedEnemies :: Float -> GameState -> [Enemy]
getUpdatedEnemies secs gstate = map (updateEnemyPos secs gstate) (enemies gstate)

-- | Update the position of an enemy so that it will walk towards the player
updateEnemyPos :: Float -> GameState -> Enemy -> Enemy
updateEnemyPos secs gstate e = e {enemyPos = (x', y'), enemyWord = enemyWord e}
                                where
                                  -- the old position of the enemy
                                  (x, y) = enemyPos e
                                  -- calculate the direction vector between the enemy and player
                                  (dx, dy) = normalizeV (playerPos (player gstate) - (x, y))
                                  -- calculate the new position of the enemy with the direction vector, so it will walk towards the player
                                  x' = x + dx * secs * enemySpeed
                                  y' = y + dy * secs * enemySpeed

-- | Handles collision
handleCollision :: GameState -> GameState
handleCollision gstate | any (True==) (map (checkCollision gstate) (enemies gstate)) = gstate { state = IsGameOver }
                       | otherwise = gstate { state = IsPlaying }

-- | Checks if an enemy collides with the player, so if the distance between the enemy and the player is smaller than the sum of their radius
checkCollision :: GameState -> Enemy -> Bool
checkCollision gstate e = sqrt (x^2 + y^2) < playerRadius + enemyRadius
                        where
                            x = fst (playerPos (player gstate)) - fst (enemyPos e)
                            y = snd (playerPos (player gstate)) - snd (enemyPos e)

-- | Handles user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
  
-- | Stores in the gamestate the state of a key. Also handles when a character is pressed and when the pause (esc) key is pressed.
inputKey :: Event -> GameState -> GameState
-- store the the key and the state of the key
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate     = gstate { keyVar = KeyUp, keyState = Down}
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate       = gstate { keyVar = KeyUp, keyState = Up}
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate   = gstate { keyVar = KeyDown, keyState = Down}
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate     = gstate { keyVar = KeyDown, keyState = Up}
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate   = gstate { keyVar = KeyLeft, keyState = Down}
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate     = gstate { keyVar = KeyLeft, keyState = Up}
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate  = gstate { keyVar = KeyRight, keyState = Down}
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate    = gstate { keyVar = KeyRight, keyState = Up}
-- Update the word when a character is typed
inputKey (EventKey (Char c) Down _ _) gstate               = gstate { typedWord = (typedWord gstate) ++ [c] }
-- Check the word when the enter key is pressed
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate  = checkWord (typedWord gstate) $ gstate { typedWord = "" }
-- Delete the last character when the delete key is pressed 
inputKey (EventKey (SpecialKey KeyDelete) Down _ _) gstate | typedWord gstate /= "" = gstate { typedWord = init (typedWord gstate) }
                                                           | otherwise = gstate
-- TODO ADD PAUSE KEY FUNCTIONALITY
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate    = gstate { state = IsPaused }
-- Otherwise keep the same gamestate
inputKey _ gstate                                          = gstate
  
-- | Checks which key is pressed down and act accordingly (moving up, moving down, rotating left, rotating right)
movePlayer :: GameState -> GameState
movePlayer gstate | keyVar gstate == KeyUp && keyState gstate == Down 
                      = (gstate { player = Player { playerPos = (x', y'), playerRotationVal = r } })
                  | keyVar gstate == KeyDown && keyState gstate == Down 
                      = (gstate { player = Player { playerPos = (x'', y''), playerRotationVal = r} })
                  | keyVar gstate == KeyLeft && keyState gstate == Down 
                      = (gstate { player = Player { playerPos = (x, y), playerRotationVal = (r - playerRotationSpeed) * playerSpeed } })
                  | keyVar gstate == KeyRight && keyState gstate == Down 
                      = (gstate { player = Player { playerPos = (x, y), playerRotationVal = (r + playerRotationSpeed) * playerSpeed } })
                  | otherwise 
                      = gstate
                  where
                    -- old position of the player
                    (x, y) = playerPos (player gstate)
                    -- old rotation of the player
                    r = playerRotationVal (player gstate)
                    -- new position of the player when going up
                    (x', y') = mulSV playerSpeed ((x, y) + direction)
                    -- new position of the player when going down
                    (x'', y'') = mulSV playerSpeed ((x, y) - direction)
                    -- calculate the direction vector
                    direction = unitVectorAtAngle (-(r - 90) * pi / 180)