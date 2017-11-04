-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model
  import View
  import Words

  import Graphics.Gloss
  import Graphics.Gloss.Data.Vector
  import Graphics.Gloss.Interface.IO.Game
  
  noSecsBetweenCylces :: Float
  noSecsBetweenCylces = 2
  
  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step secs gstate = spawnEnemies secs $ movePlayer $ updateEnemies secs $ updateDifficulty secs gstate
  
  -- | Spawns a monster after some amount of time
  spawnEnemies :: Float -> GameState -> IO GameState
  spawnEnemies secs gstate | elapsedTime gstate + secs > noSecsBetweenCylces && length (enemies gstate) < maxEnemies
                                -- Spawn a monster
                                = do randomAngle <- getRandomNumber 0 360
                                     word <- pickRandomWord (gameDifficulty gstate)
                                     return $ gstate { 
                                              enemies = e, 
                                              randomSpawnPosition = mulSV 250 (unitVectorAtAngle ((fromIntegral (randomAngle) - 90) * pi / 180)), 
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

  updateEnemies :: Float -> GameState -> GameState
  updateEnemies secs gstate = gstate { enemies = e }
                              where e = getUpdatedEnemies secs gstate
  
  getUpdatedEnemies :: Float -> GameState -> [Enemy]
  getUpdatedEnemies secs gstate = map (updateEnemyPos secs gstate) (enemies gstate)

  updateEnemyPos :: Float -> GameState -> Enemy -> Enemy
  updateEnemyPos secs gstate e = e {enemyPos = (x', y'), enemyWord = enemyWord e}
                                where
                                  (x, y) = enemyPos e
                                  (dx, dy) = normalizeV (playerPos (player gstate) - (x, y))
                                  x' = x + dx * secs * enemySpeed
                                  y' = y + dy * secs * enemySpeed

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  -- | Stores in the gamestate the state of the keys 
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = gstate { keyVar = KeyUp, keyState = Down}
  inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = gstate { keyVar = KeyUp, keyState = Up}
  inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate = gstate { keyVar = KeyDown, keyState = Down}
  inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = gstate { keyVar = KeyDown, keyState = Up}
  inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = gstate { keyVar = KeyLeft, keyState = Down}
  inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = gstate { keyVar = KeyLeft, keyState = Up}
  inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = gstate { keyVar = KeyRight, keyState = Down}
  inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = gstate { keyVar = KeyRight, keyState = Up}
  inputKey (EventKey (Char c) Down _ _) gstate = gstate { typedWord = (typedWord gstate) ++ [c] }
  inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate = checkWord (typedWord gstate) $ gstate { typedWord = "" }
  inputKey (EventKey (SpecialKey KeyDelete) Down _ _) gstate | typedWord gstate /= "" = gstate { typedWord = init (typedWord gstate) }
                                                             | otherwise = gstate
  inputKey _ gstate = gstate -- Otherwise keep the same
  
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
                      (x', y') = (x, y) + direction
                      -- new position of the player when going down
                      (x'', y'') = (x, y) - direction
                      -- calculate the direction vector
                      direction = unitVectorAtAngle (-(r - 90) * pi / 180)