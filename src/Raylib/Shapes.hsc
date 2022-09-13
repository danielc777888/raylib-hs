module Raylib.Shapes(
    drawRectangle
) where

import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

-- basic shapes drawing functions

-- draw a color-filled rectangle
foreign import ccall unsafe "raylib-hs.h C_DrawRectangle" cDrawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle x y w h c = with c(\c_ptr -> cDrawRectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) c_ptr)
