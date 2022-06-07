module Raylib.Core (
 initWindow,
 windowShouldClose,
 closeWindow,
 toggleFullScreen,
 getMonitorWidth,
 getMonitorHeight,
 clearBackground,
 beginDrawing,
 endDrawing,
 setTargetFPS,
 getTime,
 getFrameTime,
 isKeyPressed,
 isKeyDown,
 getRandomValue,
 traceLog,
 isGestureDetected
) where

import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

-- window related functions

foreign import ccall unsafe "raylib.h InitWindow" cInitWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = withCString title (\title' ->
                                   cInitWindow (fromIntegral width) (fromIntegral height) title')

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

foreign import ccall unsafe "raylib.h IsKeyPressed" cIsKeyPressed :: CInt -> IO CBool
isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed k = do p <- cIsKeyPressed (fromIntegral (fromEnum k))
                    return (toBool p)

foreign import ccall unsafe "raylib.h IsKeyDown" cIsKeyDown :: CInt -> IO CBool
isKeyDown :: KeyboardKey -> IO Bool
isKeyDown k = do p <- cIsKeyDown (fromIntegral (fromEnum k))
                 return (toBool p)

foreign import ccall unsafe "raylib.h GetRandomValue" cGetRandomValue :: CInt -> CInt -> IO CInt
getRandomValue :: Int -> Int -> IO Int
getRandomValue min max = do v <- cGetRandomValue (fromIntegral min) (fromIntegral max)
                            return (fromIntegral v)

foreign import ccall unsafe "raylib.h TraceLog" cTraceLog :: CInt -> CString -> IO ()
traceLog :: TraceLogLevel -> String -> IO ()
traceLog t s = withCString s (\s' ->
                  cTraceLog (fromIntegral (fromEnum t)) s')

 -- drawing related functions

foreign import ccall unsafe "raylib-hs.h C_ClearBackground" cClearBackground :: Ptr Color -> IO ()
clearBackground :: Color -> IO ()
clearBackground c = with c(\c_ptr -> cClearBackground c_ptr)

foreign import ccall unsafe "raylib.h BeginDrawing" cBeginDrawing :: IO ()
beginDrawing :: IO ()
beginDrawing = do cBeginDrawing

foreign import ccall unsafe "raylib.h EndDrawing" cEndDrawing :: IO ()
endDrawing :: IO ()
endDrawing = do cEndDrawing

-- gestures and touch handling functions

foreign import ccall unsafe "raylib.h IsGestureDetected" cIsGestureDetected :: CInt -> IO CBool
isGestureDetected :: Gesture -> IO Bool
isGestureDetected g = do
                    d <- cIsGestureDetected (fromIntegral (fromEnum g))
                    return (toBool d)


