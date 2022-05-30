{-# LANGUAGE OverloadedStrings #-}
module Examples.CoreBasicScreenManager where

import Raylib

main :: IO ()
main = do
  initWindow 800 450 "raylib [core] example - basic screen manager"
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
