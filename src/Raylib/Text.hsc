module Raylib.Text(
     --text loading
 loadFont,
 unloadFont,
 --text drawing
 drawTextEx
) where

import qualified Data.Text as T
import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

--text loading
foreign import ccall unsafe "raylib-hs.h C_LoadFont" cLoadFont :: CString -> IO (Ptr Font)
loadFont :: T.Text -> IO (Ptr Font)
loadFont f = do f' <- newCString (T.unpack f)
                cLoadFont f'

foreign import ccall unsafe "raylib-hs.h C_UnloadFont" cUnloadFont :: Ptr Font -> IO ()
unloadFont :: Ptr Font -> IO ()
unloadFont ptr = do cUnloadFont ptr

--text drawing
foreign import ccall unsafe "raylib-hs.h C_DrawTextEx" cDrawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> Float -> Float -> Ptr Color -> IO ()
drawTextEx :: Ptr Font -> T.Text -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx f_ptr t v fs s c = with c (\c_ptr ->
                                   with v (\v_ptr ->
                                        withCString (T.unpack t) (\t' -> cDrawTextEx f_ptr t' v_ptr (realToFrac fs) (realToFrac s) c_ptr)))
