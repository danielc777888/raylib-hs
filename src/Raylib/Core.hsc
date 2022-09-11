
module Raylib.Core (
    -- window-related
    initWindow,
    windowShouldClose,
    closeWindow,
    isWindowReady,
    isWindowFullscreen,
    isWindowHidden,
    isWindowMinimized,
    isWindowMaximized,
    isWindowFocused,
    isWindowResized,
    isWindowState,
    setWindowState,
    setWindowTitle,
    setWindowPosition,
    setWindowMonitor,
    setWindowMinSize,
    setWindowSize,
    setWindowOpacity,
    getWindowHandle,
    getScreenWidth,
    getScreenHeight,
    getRenderWidth,
    getRenderHeight,
    getMonitorCount,
    getCurrentMonitor,
    getMonitorPosition,
    getMonitorWidth,
    getMonitorHeight,
    getMonitorPhysicalWidth,
    getMonitorPhysicalHeight,
    getMonitorRefreshRate,
    getWindowPosition,
    getWindowScaleDPI,
    clearWindowState,
    toggleFullScreen,
    maximizeWindow,
    minimizeWindow,
    restoreWindow,
    setWindowIcon,
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
import qualified Data.Text as T
import Raylib.Structs
import Raylib.Enums

#include <raylib.h>

foreign import ccall unsafe "raylib.h InitWindow" cInitWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Int -> Int -> T.Text -> IO ()
initWindow width height title = withCString (T.unpack title) (\title' ->
                                   cInitWindow (fromIntegral width) (fromIntegral height) title')

foreign import ccall unsafe "raylib.h WindowShouldClose" cWindowShouldClose :: IO CBool
windowShouldClose :: IO Bool
windowShouldClose = do done <- cWindowShouldClose
                       return (toBool done)

foreign import ccall unsafe "raylib.h CloseWindow" cCloseWindow :: IO ()
closeWindow :: IO ()
closeWindow = do cCloseWindow

foreign import ccall unsafe "raylib.h IsWindowReady" cIsWindowReady :: IO CBool
isWindowReady :: IO Bool
isWindowReady = do done <- cIsWindowReady
                   return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowFullscreen" cIsWindowFullscreen :: IO CBool
isWindowFullscreen :: IO Bool
isWindowFullscreen = do done <- cIsWindowFullscreen
                        return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowHidden" cIsWindowHidden :: IO CBool
isWindowHidden :: IO Bool
isWindowHidden = do done <- cIsWindowHidden
                    return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowMinimized" cIsWindowMinimized :: IO CBool
isWindowMinimized :: IO Bool
isWindowMinimized = do done <- cIsWindowMinimized
                       return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowMaximized" cIsWindowMaximized :: IO CBool
isWindowMaximized :: IO Bool
isWindowMaximized = do done <- cIsWindowMaximized
                       return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowFocused" cIsWindowFocused :: IO CBool
isWindowFocused :: IO Bool
isWindowFocused = do done <- cIsWindowFocused
                     return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowResized" cIsWindowResized :: IO CBool
isWindowResized :: IO Bool
isWindowResized = do done <- cIsWindowResized
                     return (toBool done)

foreign import ccall unsafe "raylib.h IsWindowState" cIsWindowState :: CUInt -> IO CBool
isWindowState :: Int -> IO Bool
isWindowState x = do ws <- cIsWindowState (fromIntegral x)
                     return (toBool ws)

foreign import ccall unsafe "raylib.h SetWindowState" cSetWindowState :: CUInt -> IO ()
setWindowState :: Int -> IO ()
setWindowState x = do cSetWindowState (fromIntegral x)

foreign import ccall unsafe "raylib.h ClearWindowState" cClearWindowState :: CUInt -> IO ()
clearWindowState :: Int -> IO ()
clearWindowState x = do cClearWindowState (fromIntegral x)

foreign import ccall unsafe "raylib.h ToggleFullscreen" cToggleFullScreen :: IO ()
toggleFullScreen :: IO ()
toggleFullScreen = do cToggleFullScreen

foreign import ccall unsafe "raylib.h MaximizeWindow" cMaximizeWindow :: IO ()
maximizeWindow :: IO ()
maximizeWindow = do cMaximizeWindow

foreign import ccall unsafe "raylib.h MinimizeWindow" cMinimizeWindow :: IO ()
minimizeWindow :: IO ()
minimizeWindow = do cMinimizeWindow

foreign import ccall unsafe "raylib.h RestoreWindow" cRestoreWindow :: IO ()
restoreWindow :: IO ()
restoreWindow = do cRestoreWindow

foreign import ccall unsafe "raylib-hs.h C_SetWindowIcon" cSetWindowIcon :: Ptr Image -> IO ()
setWindowIcon :: Ptr Image -> IO ()
setWindowIcon i_ptr = do cSetWindowIcon i_ptr

foreign import ccall unsafe "raylib-hs.h C_SetWindowTitle" cSetWindowTitle :: CString -> IO ()
setWindowTitle :: T.Text -> IO ()
setWindowTitle t = withCString (T.unpack t) (\t' -> cSetWindowTitle t')

foreign import ccall unsafe "raylib.h SetWindowPosition" cSetWindowPosition :: CInt -> CInt -> IO ()
setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = do cSetWindowPosition (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h SetWindowMonitor" cSetWindowMonitor :: CInt -> IO ()
setWindowMonitor :: Int -> IO ()
setWindowMonitor m = do cSetWindowMonitor (fromIntegral m)

foreign import ccall unsafe "raylib.h SetWindowMinSize" cSetWindowMinSize :: CInt -> CInt -> IO ()
setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize w h = do cSetWindowMinSize (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "raylib.h SetWindowSize" cSetWindowSize :: CInt -> CInt -> IO ()
setWindowSize :: Int -> Int -> IO ()
setWindowSize w h = do cSetWindowSize (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "raylib.h SetWindowOpacity" cSetWindowOpacity :: CFloat -> IO ()
setWindowOpacity :: Float -> IO ()
setWindowOpacity o = do cSetWindowOpacity (realToFrac o) 

foreign import ccall unsafe "raylib.h GetWindowHandle" cGetWindowHandle :: IO (Ptr CInt)
getWindowHandle :: IO (Ptr CInt)
getWindowHandle = do cGetWindowHandle

foreign import ccall unsafe "raylib.h GetScreenWidth" cGetScreenWidth :: IO CInt
getScreenWidth :: IO Int
getScreenWidth = do w <- cGetScreenWidth
                    return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetScreenHeight" cGetScreenHeight :: IO CInt
getScreenHeight :: IO Int
getScreenHeight = do h <- cGetScreenHeight
                     return (fromIntegral h)

foreign import ccall unsafe "raylib.h GetRenderWidth" cGetRenderWidth :: IO CInt
getRenderWidth :: IO Int
getRenderWidth = do w <- cGetRenderWidth
                    return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetRenderHeight" cGetRenderHeight :: IO CInt
getRenderHeight :: IO Int
getRenderHeight = do h <- cGetRenderHeight
                     return (fromIntegral h)

foreign import ccall unsafe "raylib.h GetMonitorCount" cGetMonitorCount :: IO CInt
getMonitorCount :: IO Int
getMonitorCount = do c <- cGetMonitorCount
                     return (fromIntegral c)

foreign import ccall unsafe "raylib.h GetCurrentMonitor" cGetCurrentMonitor :: IO CInt
getCurrentMonitor :: IO Int
getCurrentMonitor = do c <- cGetCurrentMonitor
                       return (fromIntegral c)

foreign import ccall unsafe "raylib-hs.h C_GetMonitorPosition" cGetMonitorPosition :: CInt -> Ptr Vector2 -> IO ()
getMonitorPosition :: Int -> IO Vector2
getMonitorPosition x =
  alloca $ \vector2Ptr -> do
    cGetMonitorPosition (fromIntegral x) vector2Ptr
    peek vector2Ptr

foreign import ccall unsafe "raylib.h GetMonitorWidth" cGetMonitorWidth :: CInt -> IO CInt
getMonitorWidth :: Int -> IO Int
getMonitorWidth x = do w <- cGetMonitorWidth (fromIntegral x)
                       return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetMonitorHeight" cGetMonitorHeight :: CInt -> IO CInt
getMonitorHeight :: Int -> IO Int
getMonitorHeight x = do h <- cGetMonitorHeight (fromIntegral x)
                        return (fromIntegral h)


foreign import ccall unsafe "raylib.h GetMonitorPhysicalWidth" cGetMonitorPhysicalWidth :: CInt -> IO CInt
getMonitorPhysicalWidth :: Int -> IO Int
getMonitorPhysicalWidth x = do w <- cGetMonitorPhysicalWidth (fromIntegral x)
                               return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetMonitorPhysicalHeight" cGetMonitorPhysicalHeight :: CInt -> IO CInt
getMonitorPhysicalHeight :: Int -> IO Int
getMonitorPhysicalHeight x = do h <- cGetMonitorPhysicalHeight (fromIntegral x)
                                return (fromIntegral h)

foreign import ccall unsafe "raylib.h GetMonitorRefreshRate" cGetMonitorRefreshRate :: CInt -> IO CInt
getMonitorRefreshRate :: Int -> IO Int
getMonitorRefreshRate x = do h <- cGetMonitorRefreshRate (fromIntegral x)
                             return (fromIntegral h)

foreign import ccall unsafe "raylib-hs.h C_GetWindowPosition" cGetWindowPosition :: Ptr Vector2 -> IO ()
getWindowPosition :: IO Vector2
getWindowPosition  =
  alloca $ \vector2Ptr -> do
    cGetWindowPosition vector2Ptr
    peek vector2Ptr

foreign import ccall unsafe "raylib-hs.h C_GetWindowScaleDPI" cGetWindowScaleDPI :: Ptr Vector2 -> IO ()
getWindowScaleDPI :: IO Vector2
getWindowScaleDPI =
  alloca $ \vector2Ptr -> do
    cGetWindowScaleDPI vector2Ptr
    peek vector2Ptr


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
traceLog :: TraceLogLevel -> T.Text -> IO ()
traceLog t s = withCString (T.unpack s) (\s' ->
                  cTraceLog (fromIntegral (fromEnum t)) s')

foreign import ccall unsafe "raylib-hs.h C_ClearBackground" cClearBackground :: Ptr Color -> IO ()
clearBackground :: Color -> IO ()
clearBackground c = with c(\c_ptr -> cClearBackground c_ptr)

foreign import ccall unsafe "raylib.h BeginDrawing" cBeginDrawing :: IO ()
beginDrawing :: IO ()
beginDrawing = do cBeginDrawing

foreign import ccall unsafe "raylib.h EndDrawing" cEndDrawing :: IO ()
endDrawing :: IO ()
endDrawing = do cEndDrawing

foreign import ccall unsafe "raylib.h IsGestureDetected" cIsGestureDetected :: CInt -> IO CBool
isGestureDetected :: Gesture -> IO Bool
isGestureDetected g = do
                    d <- cIsGestureDetected (fromIntegral (fromEnum g))
                    return (toBool d)

