-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures ([playerSprite gstate] ++ enemySprites gstate ++ enemyTexts gstate ++ [currentText gstate] ++ [scoreText gstate])

playerSprite :: GameState -> Picture
playerSprite gstate = uncurry translate (playerPos (player gstate)) $ rotate (playerRotationVal (player gstate)) $ color white $ rectangleSolid 10 20

enemySprites :: GameState -> [Picture]
enemySprites gstate = map enemySprite (enemies gstate)

enemyTexts :: GameState -> [Picture]
enemyTexts gstate = map enemyText (enemies gstate)

enemySprite :: Enemy -> Picture
enemySprite e = uncurry translate (enemyPos e) $ color red $ circleSolid 10

enemyText :: Enemy -> Picture
enemyText e = uncurry translate (x, y) $ scale 0.15 0.15 $ color red $ text (enemyWord e)
            where
              x = fst (enemyPos e) - 15
              y = snd (enemyPos e) + 15

currentText :: GameState -> Picture
currentText gstate = uncurry translate (-190, -190) $ scale 0.15 0.15 $ color white $ text (typedWord gstate)

scoreText :: GameState -> Picture
scoreText gstate = uncurry translate (-190, 180) $ scale 0.1 0.1 $ color white $ text ("Score: " ++ show (gameScore gstate))