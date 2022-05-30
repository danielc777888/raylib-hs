module Raylib.Core (
 -- Window related functions
 initWindow,
 windowShouldClose,
 closeWindow,
 toggleFullScreen,
 getMonitorWidth,
 getMonitorHeight,
  --core drawing
 beginDrawing,
 endDrawing,
 --core timing
 setTargetFPS,
 getTime,
 getFrameTime,
 --core input
 isKeyPressed,
 isKeyDown,
 --core misc
 getRandomValue,
 traceLog,

) where

import qualified Data.Text as T
import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

-- Initialize window and OpenGL context
foreign import ccall unsafe "raylib.h InitWindow" cInitWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Int -> Int -> T.Text -> IO ()
initWindow width height title = do title' <- newCString (T.unpack title)
                                   cInitWindow (fromIntegral width) (fromIntegral height) title'

foreign import ccall unsafe "raylib.h WindowShouldClose" cWindowShouldClose :: IO CBool
windowShouldClose :: IO Bool
windowShouldClose = do
                    done <- cWindowShouldClose
                    return (toBool done)

foreign import ccall unsafe "raylib.h CloseWindow" cCloseWindow :: IO ()
closeWindow :: IO ()
closeWindow = do cCloseWindow

foreign import ccall unsafe "raylib.h ToggleFullscreen" cToggleFullScreen :: IO ()
toggleFullScreen :: IO ()
toggleFullScreen = do cToggleFullScreen

foreign import ccall unsafe "raylib.h GetMonitorWidth" cGetMonitorWidth :: CInt -> IO CInt
getMonitorWidth :: Int -> IO Int
getMonitorWidth x = do w <- cGetMonitorWidth (fromIntegral x)
                       return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetMonitorHeight" cGetMonitorHeight :: CInt -> IO CInt
getMonitorHeight :: Int -> IO Int
getMonitorHeight x = do h <- cGetMonitorHeight (fromIntegral x)
                        return (fromIntegral h)

--core drawing
foreign import ccall unsafe "raylib.h BeginDrawing" cBeginDrawing :: IO ()
beginDrawing :: IO ()
beginDrawing = do cBeginDrawing

foreign import ccall unsafe "raylib.h EndDrawing" cEndDrawing :: IO ()
endDrawing :: IO ()
endDrawing = do cEndDrawing

--core timing
foreign import ccall unsafe "raylib.h SetTargetFPS" cSetTargetFPS :: CInt -> IO ()
setTargetFPS :: Int -> IO ()
setTargetFPS fps = do cSetTargetFPS (fromIntegral fps)

foreign import ccall unsafe "raylib.h GetTime" cGetTime :: IO CDouble
getTime :: IO Double
getTime = do t <- cGetTime
             return (realToFrac t)

foreign import ccall unsafe "raylib.h GetFrameTime" cGetFrameTime :: IO CFloat
getFrameTime :: IO Float
getFrameTime = do t <- cGetFrameTime
                  return (realToFrac t)

--core input
foreign import ccall unsafe "raylib.h IsKeyPressed" cIsKeyPressed :: CInt -> IO CBool
isKeyPressed :: RayKeyboardKey -> IO Bool
isKeyPressed k = do p <- cIsKeyPressed (fromIntegral (fromEnum k))
                    return (toBool p)

foreign import ccall unsafe "raylib.h IsKeyDown" cIsKeyDown :: CInt -> IO CBool
isKeyDown :: RayKeyboardKey -> IO Bool
isKeyDown k = do p <- cIsKeyDown (fromIntegral (fromEnum k))
                 return (toBool p)
--core misc
foreign import ccall unsafe "raylib.h GetRandomValue" cGetRandomValue :: CInt -> CInt -> IO CInt
getRandomValue :: Int -> Int -> IO Int
getRandomValue min max = do v <- cGetRandomValue (fromIntegral min) (fromIntegral max)
                            return (fromIntegral v)

foreign import ccall unsafe "raylib.h TraceLog" cTraceLog :: CInt -> CString -> IO ()
traceLog :: TraceLogType -> String -> IO ()
traceLog t s = do s' <- newCString s
                  cTraceLog (fromIntegral (fromEnum t)) s'





