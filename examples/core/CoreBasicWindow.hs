module Examples.CoreBasicWindow where

import Raylib

main :: IO ()
main = do
  initWindow 800 450 "raylib [core] example - basic window"
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop :: IO ()
gameLoop = do
  done <- windowShouldClose
  if not done
    then do
      beginDrawing
      endDrawing
      gameLoop
    else return ()
