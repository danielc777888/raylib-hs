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
  let game = Game {framesCounter = 0, currentScreen = Logo}
  gameLoop game
  closeWindow

gameLoop :: Game -> IO ()
gameLoop g = do
  done <- windowShouldClose
  if not done
    then do
      keyEnter <- isKeyPressed KeyEnter
      gestureTap <- isGestureDetected GestureTap
      let g' = update g keyEnter gestureTap
      beginDrawing
      clearBackground rayWhite
      draw (currentScreen g')
      endDrawing
      gameLoop g'
    else end

end :: IO ()
end = return ()

update :: Game -> Bool -> Bool -> Game
update g keyEnter gestureTap = case currentScreen g of
  Logo -> if framesCounter g > 120 then g {currentScreen = Title} else g {framesCounter = framesCounter g + 1}
  Title -> if keyEnter || gestureTap then g {currentScreen = GamePlay} else g
  GamePlay -> if keyEnter || gestureTap then g {currentScreen = Ending} else g
  Ending -> if keyEnter || gestureTap then g {currentScreen = Title} else g

draw :: GameScreen -> IO ()
draw screen = case screen of
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
