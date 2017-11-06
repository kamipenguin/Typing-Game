-- | This module defines how to turn
--   the game state into a picture
module View where

import Model
import Game

import Graphics.Gloss

view :: GameState -> IO Picture
view gstate = do showPlayer <- playerSprite gstate
                 return $ viewPure gstate showPlayer

-- | Combines all the defined pictures into one picture
viewPure :: GameState -> Picture -> Picture
viewPure gstate playerPic = Pictures ([playerPic] ++ enemySprites gstate ++ enemyTexts gstate ++ [currentText gstate] ++ [scoreText gstate] ++ [gameStateText gstate])

-- | Defines how the player should be displayed and where
playerSprite :: GameState -> IO Picture
playerSprite gstate | state gstate == CanRestart = return Blank
                    | otherwise                  = do playerPicture <- loadBMP "player.bmp"
                                                      return $ uncurry translate (playerPos p) $ rotate (playerRotationVal p) playerPicture
                    where p = player gstate

-- | Defines how the enemies should be displayed and where
enemySprites :: GameState -> [Picture]
enemySprites gstate = map enemySprite (enemies gstate)

enemySprite :: Enemy -> Picture
enemySprite e = uncurry translate (enemyPos e) $ color (dark red) $ circleSolid enemyRadius

-- | Defines how the text should be displayed and where
enemyTexts :: GameState -> [Picture]
enemyTexts gstate = map enemyText (enemies gstate)

enemyText :: Enemy -> Picture
enemyText e = uncurry translate (x, y) $ scale 0.15 0.15 $ color (light (light red)) $ text (enemyWord e)
            where
              x = fst (enemyPos e) - 15
              y = snd (enemyPos e) + 15

-- | Defines where the typed word should be displayed and where
currentText :: GameState -> Picture
currentText gstate = uncurry translate (-290, -290) $ scale 0.15 0.15 $ color white $ text (typedWord gstate)

-- | Defines where the score text should be displayed and where
scoreText :: GameState -> Picture
scoreText gstate = uncurry translate (-290, 280) $ scale 0.1 0.1 $ color white $ text ("Score: " ++ show (gameScore gstate))

-- | Should display "Game Over" when the game is over, "Paused" when the game is paused and how to restart when you can restart the game
gameStateText :: GameState -> Picture
gameStateText gstate | state gstate == IsGameOver = showText (-100, 0) "Game Over"
                     | state gstate == IsPaused = showText (-50, 0) "Paused"
                     | state gstate == CanRestart = showText (-285, 0) "Press SPACE to restart the game"
                     | otherwise = Blank
                     where
                      showText pos s = uncurry translate pos $ scale 0.25 0.25 $ color green $ text s