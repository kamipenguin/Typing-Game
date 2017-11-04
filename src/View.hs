-- | This module defines how to turn
--   the game state into a picture
module View where

import Model

import Graphics.Gloss

view :: GameState -> IO Picture
view gstate = do showPlayer <- playerSprite gstate
                 return $ viewPure gstate showPlayer

-- | Combines all the defined pictures into one picture
viewPure :: GameState -> Picture -> Picture
viewPure gstate playerPic = Pictures ([playerPic] ++ enemySprites gstate ++ enemyTexts gstate ++ [currentText gstate] ++ [scoreText gstate])

-- | Defines how the player should be displayed and where
playerSprite :: GameState -> IO Picture
playerSprite gstate = do playerPicture <- loadBMP "player.bmp"
                         return $ uncurry translate (playerPos p) $ rotate (playerRotationVal p) $ playerPicture
                         where p = player gstate

-- | Radius of the player
playerRadius :: Float
playerRadius = 10

-- | Defines how the enemies should be displayed and where
enemySprites :: GameState -> [Picture]
enemySprites gstate = map enemySprite (enemies gstate)

enemySprite :: Enemy -> Picture
enemySprite e = uncurry translate (enemyPos e) $ color red $ circleSolid enemyRadius

-- | Radus of the enemy
enemyRadius :: Float
enemyRadius = 10

-- | Defines how the text should be displayed and where
enemyTexts :: GameState -> [Picture]
enemyTexts gstate = map enemyText (enemies gstate)

enemyText :: Enemy -> Picture
enemyText e = uncurry translate (x, y) $ scale 0.15 0.15 $ color red $ text (enemyWord e)
            where
              x = fst (enemyPos e) - 15
              y = snd (enemyPos e) + 15

-- | Defines where the typed word should be displayed and where
currentText :: GameState -> Picture
currentText gstate = uncurry translate (-190, -190) $ scale 0.15 0.15 $ color white $ text (typedWord gstate)

-- | Defines where the score text should be displayed and where
scoreText :: GameState -> Picture
scoreText gstate = uncurry translate (-190, 180) $ scale 0.1 0.1 $ color white $ text ("Score: " ++ show (gameScore gstate))