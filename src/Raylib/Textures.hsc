module Raylib.Textures(
 --textures loading
 loadTexture,
 unloadTexture,
  --textures drawing
 drawTextureEx,
 drawTexturePro
) where

import qualified Data.Text as T
import Foreign.Storable
import Foreign
import Foreign.C
import Raylib.Structs

#include <raylib.h>

--texture loading
foreign import ccall unsafe "raylib-hs.h C_LoadTexture" cLoadTexture :: CString -> IO (Ptr Texture2D)
loadTexture :: T.Text -> IO (Ptr Texture2D)
loadTexture f = do f' <- newCString (T.unpack f)
                   cLoadTexture f'

foreign import ccall unsafe "raylib-hs.h C_UnloadTexture" cUnloadTexture :: Ptr Texture2D -> IO ()
unloadTexture :: Ptr Texture2D -> IO ()
unloadTexture ptr = do cUnloadTexture ptr

--texture drawing
foreign import ccall unsafe "raylib-hs.h C_DrawTextureEx" cDrawTextureEx :: Ptr Texture2D -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
drawTextureEx :: Ptr Texture2D -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureEx t_ptr v r s c = with c (\c_ptr ->
                                        with v (\v_ptr -> cDrawTextureEx t_ptr v_ptr (realToFrac r) (realToFrac s) c_ptr))

foreign import ccall unsafe "raylib-hs.h C_DrawTexturePro" cDrawTexturePro :: Ptr Texture2D -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawTexturePro :: Ptr Texture2D -> Rectangle -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTexturePro t_ptr rc rc' v r c = with c (\c_ptr ->
                                                with rc (\rc_ptr ->
                                                           with rc' (\rc_ptr' ->
                                                                       with v (\v_ptr -> cDrawTexturePro t_ptr rc_ptr rc_ptr' v_ptr (realToFrac r) c_ptr))))
