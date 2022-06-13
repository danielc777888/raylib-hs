module Raylib.Structs (
 Texture,
 Texture2D,
 RenderTexture,
 RenderTexture2D,
 Image,
 Font,
 Sound,
 Vector2 (Vector2),
 Vector3 (Vector3),
 Vector4 (Vector4),
 Quaternion,
 Color (Color),
 Rectangle (Rectangle),
 Matrix (Matrix),
 NPatchInfo (NPatchInfo)) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

type Texture2D = Texture

type Quaternion = Vector4

data Texture = Texture
  { textureId :: !CInt
  , textureWidth :: !CInt
  , textureHeight :: !CInt
  , textureMipmaps :: !CInt
  , textureFormat :: !CInt
  } deriving (Eq, Show)

instance Storable Texture where
  sizeOf _ = #{size Texture}
  alignment _ = #{alignment Texture}
  peek ptr = do
    id' <- (#peek Texture, id) ptr
    width' <- (#peek Texture, width) ptr
    height' <- (#peek Texture, height) ptr
    mipmaps' <- (#peek Texture, mipmaps) ptr
    format' <- (#peek Texture, format) ptr
    return $! Texture id' width' height' mipmaps' format'
  poke ptr (Texture id' width' height' mipmaps' format') = do
    (#poke Texture, id) ptr id'
    (#poke Texture, width) ptr width'
    (#poke Texture, height) ptr height'
    (#poke Texture, mipmaps) ptr mipmaps'
    (#poke Texture, format) ptr format'

type RenderTexture2D = RenderTexture

data RenderTexture = RenderTexture
  { renderTextureId :: !CInt
  , renderTextureTexture :: !Texture
  , renderTextureDepth :: !Texture
  }

instance Storable RenderTexture where
  sizeOf _ = #{size RenderTexture}
  alignment _ = #{alignment RenderTexture}
  peek ptr = do
    id' <- (#peek RenderTexture, id) ptr
    texture' <- (#peek RenderTexture, texture) ptr
    depth' <- (#peek RenderTexture, depth) ptr
    return $! RenderTexture id' texture' depth'
  poke ptr (RenderTexture id' texture' depth') = do
    (#poke RenderTexture, id) ptr id'
    (#poke RenderTexture, texture) ptr texture'
    (#poke RenderTexture, depth) ptr depth'

data Color = Color
  { colorR :: !CUChar
  , colorG :: !CUChar
  , colorB :: !CUChar
  , colorA :: !CUChar
  } deriving (Eq, Show)

instance Storable Color where
  sizeOf _ = #{size Color}
  alignment _ = #{alignment Color}
  peek ptr = do
    r' <- (#peek Color, r) ptr
    g' <- (#peek Color, g) ptr
    b' <- (#peek Color, b) ptr
    a' <- (#peek Color, a) ptr
    return $! Color r' g' b' a'
  poke ptr (Color r' g' b' a') = do
    (#poke Color, r) ptr r'
    (#poke Color, g) ptr g'
    (#poke Color, b) ptr b'
    (#poke Color, a) ptr a'


data AudioStream = AudioStream
  { audioStreamBuffer :: !(Ptr CInt)
  , audioStreamSampleRate :: !CUInt
  , audioStreamSampleSize :: !CUInt
  , audioStreamChannels :: !CUInt
  }

instance Storable AudioStream where
  sizeOf _ = #{size AudioStream}
  alignment _ = #{alignment AudioStream}
  peek ptr = do
    buffer' <- (#peek AudioStream, buffer) ptr
    sampleRate' <- (#peek AudioStream, sampleRate) ptr
    sampleSize' <- (#peek AudioStream, sampleSize) ptr
    channels' <- (#peek AudioStream, channels) ptr
    return $! AudioStream buffer' sampleRate' sampleSize' channels'
  poke ptr (AudioStream buffer' sampleRate' sampleSize' channels') = do
    (#poke AudioStream, buffer) ptr buffer'
    (#poke AudioStream, sampleRate) ptr sampleRate'
    (#poke AudioStream, sampleSize) ptr sampleSize'
    (#poke AudioStream, channels) ptr channels'

data Sound = Sound
  { soundStream :: AudioStream
  , soundFrameCount :: !CUInt
  }

instance Storable Sound where
  sizeOf _ = #{size Sound}
  alignment _ = #{alignment Sound}
  peek ptr = do
    stream' <- (#peek Sound, stream) ptr
    frameCount' <- (#peek Sound, frameCount) ptr
    return $! Sound stream' frameCount'
  poke ptr (Sound stream' frameCount') = do
    (#poke Sound, stream) ptr stream'
    (#poke Sound, frameCount) ptr frameCount'

data Vector2 = Vector2
  { vector2X :: !CFloat
  , vector2Y :: !CFloat
  } deriving (Show, Eq)

instance Storable Vector2 where
  sizeOf _ = #{size Vector2}
  alignment _ = #{alignment Vector2}
  peek ptr = do
    x' <- (#peek Vector2, x) ptr
    y' <- (#peek Vector2, y) ptr
    return $! Vector2 x' y'
  poke ptr (Vector2 x' y') = do
    (#poke Vector2, x) ptr x'
    (#poke Vector2, y) ptr y'

data Vector3 = Vector3
  { vector3X :: !CFloat
  , vector3Y :: !CFloat
  , vector3Z :: !CFloat
  } deriving (Show, Eq)

instance Storable Vector3 where
  sizeOf _ = #{size Vector3}
  alignment _ = #{alignment Vector3}
  peek ptr = do
    x' <- (#peek Vector3, x) ptr
    y' <- (#peek Vector3, y) ptr
    z' <- (#peek Vector3, z) ptr
    return $! Vector3 x' y' z'
  poke ptr (Vector3 x' y' z') = do
    (#poke Vector3, x) ptr x'
    (#poke Vector3, y) ptr y'
    (#poke Vector3, z) ptr z'

data Vector4 = Vector4
  { vector4X :: !CFloat
  , vector4Y :: !CFloat
  , vector4Z :: !CFloat
  , vector4W :: !CFloat
  } deriving (Show, Eq)

instance Storable Vector4 where
  sizeOf _ = #{size Vector4}
  alignment _ = #{alignment Vector4}
  peek ptr = do
    x' <- (#peek Vector4, x) ptr
    y' <- (#peek Vector4, y) ptr
    z' <- (#peek Vector4, z) ptr
    w' <- (#peek Vector4, w) ptr
    return $! Vector4 x' y' z' w'
  poke ptr (Vector4 x' y' z' w') = do
    (#poke Vector4, x) ptr x'
    (#poke Vector4, y) ptr y'
    (#poke Vector4, z) ptr z'
    (#poke Vector4, w) ptr w'


data Image = Image
  { imageData :: !(Ptr CInt)
  , imageWidth :: !CInt
  , imageHeight :: !CInt
  , imageMipmaps :: !CInt
  , imageFormat :: !CInt
  }

instance Storable Image where
  sizeOf _ = #{size Image}
  alignment _ = #{alignment Image}
  peek ptr = do
    data' <- (#peek Image, data) ptr
    width' <- (#peek Image, width) ptr
    height' <- (#peek Image, height) ptr
    mipmaps' <- (#peek Image, mipmaps) ptr
    format' <- (#peek Image, format) ptr
    return $! Image data' width' height' mipmaps' format'
  poke ptr (Image data' width' height' mipmaps' format') = do
    (#poke Image, data) ptr data'
    (#poke Image, width) ptr width'
    (#poke Image, height) ptr height'
    (#poke Image, mipmaps) ptr mipmaps'
    (#poke Image, format) ptr format'

data GlyphInfo = GlyphInfo
  { glyphInfoValue :: !CInt
  , glyphInfoOffsetX :: !CInt
  , glyphInfoOffsetY :: !CInt
  , glyphInfoAdvanceX :: !CInt
  , glyphInfoImage :: !Image
  }

instance Storable GlyphInfo where
  sizeOf _ = #{size GlyphInfo}
  alignment _ = #{alignment GlyphInfo}
  peek ptr = do
    value' <- (#peek GlyphInfo, value) ptr
    offsetX' <- (#peek GlyphInfo, offsetX) ptr
    offsetY' <- (#peek GlyphInfo, offsetY) ptr
    advanceX' <- (#peek GlyphInfo, advanceX) ptr
    image' <- (#peek GlyphInfo, image) ptr
    return $! GlyphInfo value' offsetX' offsetY' advanceX' image'
  poke ptr (GlyphInfo value' offsetX' offsetY' advanceX' image') = do
    (#poke GlyphInfo, value) ptr value'
    (#poke GlyphInfo, offsetX) ptr offsetX'
    (#poke GlyphInfo, offsetY) ptr offsetY'
    (#poke GlyphInfo, advanceX) ptr advanceX'
    (#poke GlyphInfo, image) ptr image'

data Rectangle = Rectangle
  { rectangleX :: !CFloat
  , rectangleY :: !CFloat
  , rectangleWidth :: !CFloat
  , rectangleHeight :: !CFloat
  } deriving (Show, Eq)

instance Storable Rectangle where
  sizeOf _ = #{size Rectangle}
  alignment _ = #{alignment Rectangle}
  peek ptr = do
    x' <- (#peek Rectangle, x) ptr
    y' <- (#peek Rectangle, y) ptr
    width' <- (#peek Rectangle, width) ptr
    height' <- (#peek Rectangle, height) ptr
    return $! Rectangle x' y' width' height'
  poke ptr (Rectangle x' y' width' height') = do
    (#poke Rectangle, x) ptr x'
    (#poke Rectangle, y) ptr y'
    (#poke Rectangle, width) ptr width'
    (#poke Rectangle, height) ptr height'

data Font = Font
  { fontBaseSize :: !CInt
  , fontGlyphCount :: !CInt
  , fontGlyphPadding :: !CInt
  , fontTexture :: !Texture2D
  , fontRecs :: !(Ptr Rectangle)
  , fontGlyphs :: !(Ptr GlyphInfo)
  }

instance Storable Font where
  sizeOf _ = #{size Font}
  alignment _ = #{alignment Font}
  peek ptr = do
    baseSize' <- (#peek Font, baseSize) ptr
    glyphCount' <- (#peek Font, glyphCount) ptr
    glyphPadding' <- (#peek Font, glyphPadding) ptr
    texture' <- (#peek Font, texture) ptr
    recs' <- (#peek Font, recs) ptr
    glyphs' <- (#peek Font, glyphs) ptr
    return $! Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs'
  poke ptr (Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs') = do
    (#poke Font, baseSize) ptr baseSize'
    (#poke Font, glyphCount) ptr glyphCount'
    (#poke Font, glyphPadding) ptr glyphPadding'
    (#poke Font, texture) ptr texture'
    (#poke Font, recs) ptr recs'
    (#poke Font, glyphs) ptr glyphs'


data Matrix = Matrix
  { matrixM0 :: !CFloat
  , matrixM1 :: !CFloat
  , matrixM2 :: !CFloat
  , matrixM3 :: !CFloat
  , matrixM4 :: !CFloat
  , matrixM5 :: !CFloat
  , matrixM6 :: !CFloat
  , matrixM7 :: !CFloat
  , matrixM8 :: !CFloat
  , matrixM9 :: !CFloat
  , matrixM10 :: !CFloat
  , matrixM11 :: !CFloat
  , matrixM12 :: !CFloat
  , matrixM13 :: !CFloat
  , matrixM14 :: !CFloat
  , matrixM15 :: !CFloat
  } deriving (Show, Eq)

instance Storable Matrix where
  sizeOf _ = #{size Matrix}
  alignment _ = #{alignment Matrix}
  peek ptr = do
    m0' <- (#peek Matrix, m0) ptr
    m1' <- (#peek Matrix, m1) ptr
    m2' <- (#peek Matrix, m2) ptr
    m3' <- (#peek Matrix, m3) ptr
    m4' <- (#peek Matrix, m4) ptr
    m5' <- (#peek Matrix, m5) ptr
    m6' <- (#peek Matrix, m6) ptr
    m7' <- (#peek Matrix, m7) ptr
    m8' <- (#peek Matrix, m8) ptr
    m9' <- (#peek Matrix, m9) ptr
    m10' <- (#peek Matrix, m10) ptr
    m11' <- (#peek Matrix, m11) ptr
    m12' <- (#peek Matrix, m12) ptr
    m13' <- (#peek Matrix, m13) ptr
    m14' <- (#peek Matrix, m14) ptr
    m15' <- (#peek Matrix, m15) ptr
    return $! Matrix m0' m1' m2' m3' m4' m5' m6' m7' m8' m9' m10' m11' m12' m13' m14' m15'
  poke ptr (Matrix m0' m1' m2' m3' m4' m5' m6' m7' m8' m9' m10' m11' m12' m13' m14' m15') = do
    (#poke Matrix, m0) ptr m0'
    (#poke Matrix, m1) ptr m1'
    (#poke Matrix, m2) ptr m2'
    (#poke Matrix, m3) ptr m3'
    (#poke Matrix, m4) ptr m4'
    (#poke Matrix, m5) ptr m5'
    (#poke Matrix, m6) ptr m6'
    (#poke Matrix, m7) ptr m7'
    (#poke Matrix, m8) ptr m8'
    (#poke Matrix, m9) ptr m9'
    (#poke Matrix, m10) ptr m10'
    (#poke Matrix, m11) ptr m11'
    (#poke Matrix, m12) ptr m12'
    (#poke Matrix, m13) ptr m13'
    (#poke Matrix, m14) ptr m14'
    (#poke Matrix, m15) ptr m15'

data NPatchInfo = NPatchInfo
  { nPatchInfoSource :: !Rectangle
  , nPatchInfoLeft :: !CInt
  , nPatchInfoTop :: !CInt
  , nPatchInfoRight :: !CInt
  , nPatchInfoBottom :: !CInt
  , nPatchInfoLayout :: !CInt
  }

instance Storable NPatchInfo where
  sizeOf _ = #{size NPatchInfo}
  alignment _ = #{alignment NPatchInfo}
  peek ptr = do
    source' <- (#peek NPatchInfo, source) ptr
    left' <- (#peek NPatchInfo, left) ptr
    top' <- (#peek NPatchInfo, top) ptr
    right' <- (#peek NPatchInfo, right) ptr
    bottom' <- (#peek NPatchInfo, bottom) ptr
    layout' <- (#peek NPatchInfo, layout) ptr
    return $! NPatchInfo source' left' top' right' bottom' layout'
  poke ptr (NPatchInfo source' left' top' right' bottom' layout') = do
    (#poke NPatchInfo, source) ptr source'
    (#poke NPatchInfo, left) ptr left'
    (#poke NPatchInfo, top) ptr top'
    (#poke NPatchInfo, right) ptr right'
    (#poke NPatchInfo, bottom) ptr bottom'
    (#poke NPatchInfo, layout) ptr layout'
