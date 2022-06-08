module Raylib.Text(
 -- font loading/unloading functions
 loadFont,
 unloadFont,
 -- text drawing functions
 drawText,
 drawTextEx
) where


import qualified Data.Text as T

import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

-- font loading/unloading functions
foreign import ccall unsafe "raylib-hs.h C_LoadFont" cLoadFont :: CString -> IO (Ptr Font)
loadFont :: T.Text -> IO (Ptr Font)
loadFont f = withCString (T.unpack f) (\f' ->
                cLoadFont f')

foreign import ccall unsafe "raylib-hs.h C_UnloadFont" cUnloadFont :: Ptr Font -> IO ()
unloadFont :: Ptr Font -> IO ()
unloadFont ptr = do cUnloadFont ptr

-- text drawing functions
-- draw text (using default font)
foreign import ccall unsafe "raylib-hs.h C_DrawText" cDrawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
drawText :: T.Text -> Int -> Int -> Int -> Color -> IO ()
drawText t x y fs c = with c (\c_ptr ->
                                   withCString (T.unpack t) (\t' -> cDrawText t' (fromIntegral x) (fromIntegral y) (fromIntegral fs) c_ptr))

-- draw text using font and additional parameters
foreign import ccall unsafe "raylib-hs.h C_DrawTextEx" cDrawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> Float -> Float -> Ptr Color -> IO ()
drawTextEx :: Ptr Font -> T.Text -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx f_ptr t v fs s c = with c (\c_ptr ->
                                   with v (\v_ptr ->
                                        withCString (T.unpack t) (\t' -> cDrawTextEx f_ptr t' v_ptr (realToFrac fs) (realToFrac s) c_ptr)))
