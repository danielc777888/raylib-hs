{-# LANGUAGE OverloadedStrings #-}

import Raylib

data GameScreen
  = Logo
  | Title
  | GamePlay
  | Ending

data Game = Game
  { framesCounter :: Int,
    currentScreen :: GameScreen
  }

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

main :: IO ()
main = do
  initWindow screenWidth screenHeight "raylib [core] example - basic screen manager"
  setTargetFPS 60
  let g = Game {framesCounter = 0, currentScreen = Logo}
  gameLoop g
  closeWindow

gameLoop :: Game -> IO ()
gameLoop g = do
  done <- windowShouldClose
  if not done
    then do
      ke <- isKeyPressed Key_Enter
      gt <- isGestureDetected GestureTap
      let g' = update g ke gt
      beginDrawing
      clearBackground rayWhite
      draw g'
      endDrawing
      gameLoop g'
    else end

end :: IO ()
end = return ()

update :: Game -> Bool -> Bool -> Game
update g ke gt = case (currentScreen g) of
  Logo -> if (framesCounter g) > 120 then g {currentScreen = Title} else g {framesCounter = (framesCounter g) + 1}
  Title -> if ke || gt then g {currentScreen = GamePlay} else g
  GamePlay -> if ke || gt then g {currentScreen = Ending} else g
  Ending -> if ke || gt then g {currentScreen = Title} else g

draw :: Game -> IO ()
draw g = case (currentScreen g) of
  Logo -> do
    drawText "LOGO SCREEN" 20 20 40 lightGray
    drawText "WAIT for 2 SECONDS" 290 220 20 gray
  Title -> do
    drawRectangle 0 0 screenWidth screenHeight green
    drawText "TITLE SCREEN" 20 20 40 darkGreen
    drawText "PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN" 120 220 20 darkGreen
  GamePlay -> do
    drawRectangle 0 0 screenWidth screenHeight purple
    drawText "GAMEPLAY SCREEN" 20 20 40 maroon
    drawText "PRESS ENTER or TAP to JUMP to ENDING SCREEN" 130 220 20 maroon
  Ending -> do
    drawRectangle 0 0 screenWidth screenHeight blue
    drawText "ENDING SCREEN" 20 20 40 darkBlue
    drawText "PRESS ENTER or TAP to JUMP to TITLE SCREEN" 120 220 20 darkBlue
