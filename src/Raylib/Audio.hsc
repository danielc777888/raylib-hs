module Raylib.Audio(
     --audio device management
 initAudioDevice,
 --audio loading
 loadSound,
 unloadSound,
 --audio management
 playSound
) where

import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

--audio device management
foreign import ccall unsafe "raylib-hs.h InitAudioDevice" cInitAudioDevice :: IO ()
initAudioDevice :: IO ()
initAudioDevice = do cInitAudioDevice

--audio loading
foreign import ccall unsafe "raylib-hs.h C_LoadSound" cLoadSound :: CString -> IO (Ptr Sound)
loadSound :: String -> IO (Ptr Sound)
loadSound f = withCString f (\f' ->
                 cLoadSound f')

foreign import ccall unsafe "raylib-hs.h C_UnloadSound" cUnloadSound :: Ptr Sound -> IO ()
unloadSound :: Ptr Sound -> IO ()
unloadSound ptr = do cUnloadSound ptr

--audio management
foreign import ccall unsafe "raylib-hs.h C_PlaySound" cPlaySound :: Ptr Sound -> IO ()
playSound :: Ptr Sound -> IO ()
playSound s_ptr = do cPlaySound s_ptr
