{-# LANGUAGE OverloadedStrings #-}

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
      clearBackground rayWhite
      drawText "Congrats! You created your first window!" 190 200 20 lightGray
      endDrawing
      gameLoop
    else return ()
