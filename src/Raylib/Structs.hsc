module Raylib.Structs (
    AudioStream (..),
    BoneInfo (..),
    BoundingBox (..),
    Camera2D (..),
    Camera3D,
    Color (..),
    Font (..),
    GlyphInfo (..),
    Image (..),
    Material (..),
    MaterialMap (..),
    Matrix (..),
    Mesh (..),
    Model (..),
    ModelAnimation (..),
    Music (..),
    NPatchInfo (..),
    Quaternion,
    Ray (..),
    RayCollision (..),
    Rectangle (..),
    RenderTexture (..),
    RenderTexture2D,
    Shader (..),
    Sound (..),
    Texture (..),
    Texture2D,
    TextureCubeMap,
    Transform (..),
    Vector2 (..),
    Vector3 (..),
    Vector4 (..),
    VrDeviceInfo (..),
    VrStereoConfig (..),
    Wave (..),
    ) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

type Texture2D = Texture
type TextureCubeMap = Texture

type Quaternion = Vector4

data Texture = Texture
  { txtId :: CInt
  , txtWidth :: CInt
  , txtHeight :: CInt
  , txtMipmaps :: CInt
  , txtFormat :: CInt
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
    return (Texture id' width' height' mipmaps' format')
  poke ptr (Texture id' width' height' mipmaps' format') = do
    (#poke Texture, id) ptr id'
    (#poke Texture, width) ptr width'
    (#poke Texture, height) ptr height'
    (#poke Texture, mipmaps) ptr mipmaps'
    (#poke Texture, format) ptr format'

type RenderTexture2D = RenderTexture

data RenderTexture = RenderTexture
  { rtxId :: CInt
  , rtxTexture :: Texture
  , rtxDepth :: Texture
  }

instance Storable RenderTexture where
  sizeOf _ = #{size RenderTexture}
  alignment _ = #{alignment RenderTexture}
  peek ptr = do
    id' <- (#peek RenderTexture, id) ptr
    texture' <- (#peek RenderTexture, texture) ptr
    depth' <- (#peek RenderTexture, depth) ptr
    return (RenderTexture id' texture' depth')
  poke ptr (RenderTexture id' texture' depth') = do
    (#poke RenderTexture, id) ptr id'
    (#poke RenderTexture, texture) ptr texture'
    (#poke RenderTexture, depth) ptr depth'

data Color = Color
  { colorR :: CUChar
  , colorG :: CUChar
  , colorB :: CUChar
  , colorA :: CUChar
  } deriving (Eq, Show)

instance Storable Color where
  sizeOf _ = #{size Color}
  alignment _ = #{alignment Color}
  peek ptr = do
    r' <- (#peek Color, r) ptr
    g' <- (#peek Color, g) ptr
    b' <- (#peek Color, b) ptr
    a' <- (#peek Color, a) ptr
    return (Color r' g' b' a')
  poke ptr (Color r' g' b' a') = do
    (#poke Color, r) ptr r'
    (#poke Color, g) ptr g'
    (#poke Color, b) ptr b'
    (#poke Color, a) ptr a'


data AudioStream = AudioStream
  { ausBuffer :: (Ptr CInt)
  , ausSampleRate :: CUInt
  , ausSampleSize :: CUInt
  , ausChannels :: CUInt
  }

instance Storable AudioStream where
  sizeOf _ = #{size AudioStream}
  alignment _ = #{alignment AudioStream}
  peek ptr = do
    buffer' <- (#peek AudioStream, buffer) ptr
    sampleRate' <- (#peek AudioStream, sampleRate) ptr
    sampleSize' <- (#peek AudioStream, sampleSize) ptr
    channels' <- (#peek AudioStream, channels) ptr
    return (AudioStream buffer' sampleRate' sampleSize' channels')
  poke ptr (AudioStream buffer' sampleRate' sampleSize' channels') = do
    (#poke AudioStream, buffer) ptr buffer'
    (#poke AudioStream, sampleRate) ptr sampleRate'
    (#poke AudioStream, sampleSize) ptr sampleSize'
    (#poke AudioStream, channels) ptr channels'

data Sound = Sound
  { soundStream :: AudioStream
  , soundFrameCount :: CUInt
  }

instance Storable Sound where
  sizeOf _ = #{size Sound}
  alignment _ = #{alignment Sound}
  peek ptr = do
    stream' <- (#peek Sound, stream) ptr
    frameCount' <- (#peek Sound, frameCount) ptr
    return (Sound stream' frameCount')
  poke ptr (Sound stream' frameCount') = do
    (#poke Sound, stream) ptr stream'
    (#poke Sound, frameCount) ptr frameCount'

data Vector2 = Vector2
  { vec2X :: CFloat
  , vec2Y :: CFloat
  } deriving (Show, Eq)

instance Storable Vector2 where
  sizeOf _ = #{size Vector2}
  alignment _ = #{alignment Vector2}
  peek ptr = do
    x' <- (#peek Vector2, x) ptr
    y' <- (#peek Vector2, y) ptr
    return (Vector2 x' y')
  poke ptr (Vector2 x' y') = do
    (#poke Vector2, x) ptr x'
    (#poke Vector2, y) ptr y'

data Vector3 = Vector3
  { vc3X :: CFloat
  , vc3Y :: CFloat
  , vc3Z :: CFloat
  } deriving (Show, Eq)

instance Storable Vector3 where
  sizeOf _ = #{size Vector3}
  alignment _ = #{alignment Vector3}
  peek ptr = do
    x' <- (#peek Vector3, x) ptr
    y' <- (#peek Vector3, y) ptr
    z' <- (#peek Vector3, z) ptr
    return (Vector3 x' y' z')
  poke ptr (Vector3 x' y' z') = do
    (#poke Vector3, x) ptr x'
    (#poke Vector3, y) ptr y'
    (#poke Vector3, z) ptr z'

data Vector4 = Vector4
  { vc4X :: CFloat
  , vc4Y :: CFloat
  , vc4Z :: CFloat
  , vc4W :: CFloat
  } deriving (Show, Eq)

instance Storable Vector4 where
  sizeOf _ = #{size Vector4}
  alignment _ = #{alignment Vector4}
  peek ptr = do
    x' <- (#peek Vector4, x) ptr
    y' <- (#peek Vector4, y) ptr
    z' <- (#peek Vector4, z) ptr
    w' <- (#peek Vector4, w) ptr
    return (Vector4 x' y' z' w')
  poke ptr (Vector4 x' y' z' w') = do
    (#poke Vector4, x) ptr x'
    (#poke Vector4, y) ptr y'
    (#poke Vector4, z) ptr z'
    (#poke Vector4, w) ptr w'


data Image = Image
  { imageData :: (Ptr CInt)
  , imageWidth :: CInt
  , imageHeight :: CInt
  , imageMipmaps :: CInt
  , imageFormat :: CInt
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
    return (Image data' width' height' mipmaps' format')
  poke ptr (Image data' width' height' mipmaps' format') = do
    (#poke Image, data) ptr data'
    (#poke Image, width) ptr width'
    (#poke Image, height) ptr height'
    (#poke Image, mipmaps) ptr mipmaps'
    (#poke Image, format) ptr format'

data GlyphInfo = GlyphInfo
  { giValue :: CInt
  , giOffsetX :: CInt
  , giOffsetY :: CInt
  , giAdvanceX :: CInt
  , giImage :: Image
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
    return (GlyphInfo value' offsetX' offsetY' advanceX' image')
  poke ptr (GlyphInfo value' offsetX' offsetY' advanceX' image') = do
    (#poke GlyphInfo, value) ptr value'
    (#poke GlyphInfo, offsetX) ptr offsetX'
    (#poke GlyphInfo, offsetY) ptr offsetY'
    (#poke GlyphInfo, advanceX) ptr advanceX'
    (#poke GlyphInfo, image) ptr image'

data Rectangle = Rectangle
  { recX :: CFloat
  , recY :: CFloat
  , recWidth :: CFloat
  , recHeight :: CFloat
  } deriving (Show, Eq)

instance Storable Rectangle where
  sizeOf _ = #{size Rectangle}
  alignment _ = #{alignment Rectangle}
  peek ptr = do
    x' <- (#peek Rectangle, x) ptr
    y' <- (#peek Rectangle, y) ptr
    width' <- (#peek Rectangle, width) ptr
    height' <- (#peek Rectangle, height) ptr
    return (Rectangle x' y' width' height')
  poke ptr (Rectangle x' y' width' height') = do
    (#poke Rectangle, x) ptr x'
    (#poke Rectangle, y) ptr y'
    (#poke Rectangle, width) ptr width'
    (#poke Rectangle, height) ptr height'

data Font = Font
  { fontBaseSize :: CInt
  , fontGlyphCount :: CInt
  , fontGlyphPadding :: CInt
  , fontTexture :: Texture2D
  , fontRecs :: (Ptr Rectangle)
  , fontGlyphs :: (Ptr GlyphInfo)
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
    return (Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs')
  poke ptr (Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs') = do
    (#poke Font, baseSize) ptr baseSize'
    (#poke Font, glyphCount) ptr glyphCount'
    (#poke Font, glyphPadding) ptr glyphPadding'
    (#poke Font, texture) ptr texture'
    (#poke Font, recs) ptr recs'
    (#poke Font, glyphs) ptr glyphs'


data Matrix = Matrix
  { mtxM0 :: CFloat
  , mtxM1 :: CFloat
  , mtxM2 :: CFloat
  , mtxM3 :: CFloat
  , mtxM4 :: CFloat
  , mtxM5 :: CFloat
  , mtxM6 :: CFloat
  , mtxM7 :: CFloat
  , mtxM8 :: CFloat
  , mtxM9 :: CFloat
  , mtxM10 :: CFloat
  , mtxM11 :: CFloat
  , mtxM12 :: CFloat
  , mtxM13 :: CFloat
  , mtxM14 :: CFloat
  , mtxM15 :: CFloat
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
    return (Matrix m0' m1' m2' m3' m4' m5' m6' m7' m8' m9' m10' m11' m12' m13' m14' m15')
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
  { npiSource :: Rectangle
  , npiLeft :: CInt
  , npiTop :: CInt
  , npiRight :: CInt
  , npiBottom :: CInt
  , npiLayout :: CInt
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
    return (NPatchInfo source' left' top' right' bottom' layout')
  poke ptr (NPatchInfo source' left' top' right' bottom' layout') = do
    (#poke NPatchInfo, source) ptr source'
    (#poke NPatchInfo, left) ptr left'
    (#poke NPatchInfo, top) ptr top'
    (#poke NPatchInfo, right) ptr right'
    (#poke NPatchInfo, bottom) ptr bottom'
    (#poke NPatchInfo, layout) ptr layout'

type Camera = Camera3D

data Camera3D = Camera3D
  { cam3DPosition :: Vector3
  , cam3DTarget :: Vector3
  , cam3DUp :: Vector3
  , cam3DFovy :: CFloat
  , cam3DProjection :: CInt
  }

instance Storable Camera3D where
  sizeOf _ = #{size Camera3D}
  alignment _ = #{alignment Camera3D}
  peek ptr = do
    position' <- (#peek Camera3D, position) ptr
    target' <- (#peek Camera3D, target) ptr
    up' <- (#peek Camera3D, up) ptr
    fovy' <- (#peek Camera3D, fovy) ptr
    projection' <- (#peek Camera3D, projection) ptr
    return (Camera3D position' target' up' fovy' projection')
  poke ptr (Camera3D position' target' up' fovy' projection') = do
    (#poke Camera3D, position) ptr position'
    (#poke Camera3D, target) ptr target'
    (#poke Camera3D, up) ptr up'
    (#poke Camera3D, fovy) ptr fovy'
    (#poke Camera3D, projection) ptr projection'

data Camera2D = Camera2D
  { cam2DOffset :: Vector2
  , cam2DTarget :: Vector2
  , cam2DRotation :: CFloat
  , cam2DZoom :: CFloat
  }

instance Storable Camera2D where
  sizeOf _ = #{size Camera2D}
  alignment _ = #{alignment Camera2D}
  peek ptr = do
    offset' <- (#peek Camera2D, offset) ptr
    target' <- (#peek Camera2D, target) ptr
    rotation' <- (#peek Camera2D, rotation) ptr
    zoom' <- (#peek Camera2D, zoom) ptr
    return $ Camera2D offset' target' rotation' zoom'
  poke ptr (Camera2D offset' target' rotation' zoom') = do
    (#poke Camera2D, offset) ptr offset'
    (#poke Camera2D, target) ptr target'
    (#poke Camera2D, rotation) ptr rotation'
    (#poke Camera2D, zoom) ptr zoom'

data Mesh = Mesh
  { meshVertexCount :: CInt
  , meshTriangleCount :: CInt
  , meshVertices :: (Ptr CFloat)
  , meshTexcoords :: (Ptr CFloat)
  , meshTexcoords2 :: (Ptr CFloat)
  , meshNormals :: (Ptr CFloat)
  , meshTangents :: (Ptr CFloat)
  , meshColors :: (Ptr CInt)
  , meshIndices :: (Ptr CInt)
  , meshAnimVertices :: (Ptr CFloat)
  , meshAnimNormals :: (Ptr CFloat)
  , meshBoneIds :: (Ptr CInt)
  , meshBoneWeights :: (Ptr CFloat)
  , meshVaoId :: CInt
  , meshVboId :: (Ptr CInt)
  }

instance Storable Mesh where
  sizeOf _ = #{size Mesh}
  alignment _ = #{alignment Mesh}
  peek ptr = do
    vertexCount' <- (#peek Mesh, vertexCount) ptr
    triangleCount' <- (#peek Mesh, triangleCount) ptr
    vertices' <- (#peek Mesh, vertices) ptr
    texcoords' <- (#peek Mesh, texcoords) ptr
    texcoords2' <- (#peek Mesh, texcoords2) ptr
    normals' <- (#peek Mesh, normals) ptr
    tangents' <- (#peek Mesh, tangents) ptr
    colors' <- (#peek Mesh, colors) ptr
    indices' <- (#peek Mesh, indices) ptr
    animVertices' <- (#peek Mesh, animVertices) ptr
    animNormals' <- (#peek Mesh, animNormals) ptr
    boneIds' <- (#peek Mesh, boneIds) ptr
    boneWeights' <- (#peek Mesh, boneWeights) ptr
    vaoId' <- (#peek Mesh, vaoId) ptr
    vboId' <- (#peek Mesh, vboId) ptr
    return (Mesh vertexCount' triangleCount' vertices' texcoords' texcoords2' normals' tangents' colors' indices' animVertices' animNormals' boneIds' boneWeights' vaoId' vboId')
  poke ptr (Mesh vertexCount' triangleCount' vertices' texcoords' texcoords2' normals' tangents' colors' indices' animVertices' animNormals' boneIds' boneWeights' vaoId' vboId') = do
    (#poke Mesh, vertexCount) ptr vertexCount'
    (#poke Mesh, triangleCount) ptr triangleCount'
    (#poke Mesh, vertices) ptr vertices'
    (#poke Mesh, texcoords) ptr texcoords'
    (#poke Mesh, texcoords2) ptr texcoords2'
    (#poke Mesh, normals) ptr normals'
    (#poke Mesh, tangents) ptr tangents'
    (#poke Mesh, colors) ptr colors'
    (#poke Mesh, indices) ptr indices'
    (#poke Mesh, animVertices) ptr animVertices'
    (#poke Mesh, animNormals) ptr animNormals'
    (#poke Mesh, boneIds) ptr boneIds'
    (#poke Mesh, boneWeights) ptr boneWeights'
    (#poke Mesh, vaoId) ptr vaoId'
    (#poke Mesh, vboId) ptr vboId'

data Shader = Shader
    { shdId :: CInt
    , shdLocs :: (Ptr CInt)
    }

instance Storable Shader where
    sizeOf _ = #{size Shader}
    alignment _ = #{alignment Shader}
    peek ptr = do
        id' <- (#peek Shader, id) ptr
        locs' <- (#peek Shader, locs) ptr
        return $ Shader id' locs'
    poke ptr (Shader id' locs') = do
        (#poke Shader, id) ptr id'
        (#poke Shader, locs) ptr locs'

data MaterialMap = MaterialMap
    { matMapTexture :: Texture2D
    , matMapColor :: Color
    , matMapValue :: CFloat
    }

instance Storable MaterialMap where
    sizeOf _ = #{size MaterialMap}
    alignment _ = #{alignment MaterialMap}
    peek ptr = do
        texture' <- (#peek MaterialMap, texture) ptr
        color' <- (#peek MaterialMap, color) ptr
        value' <- (#peek MaterialMap, value) ptr
        return (MaterialMap texture' color' value')
    poke ptr (MaterialMap texture' color' value') = do
        (#poke MaterialMap, texture) ptr texture'
        (#poke MaterialMap, color) ptr color'
        (#poke MaterialMap, value) ptr value'

data Material = Material
    { matShader :: Shader
    , matMaps :: (Ptr MaterialMap)
    , matParams :: [CFloat]
    }

instance Storable Material where
    sizeOf _ = #{size Material}
    alignment _ = #{alignment Material}
    peek ptr = do
        shader' <- (#peek Material, shader) ptr
        maps' <- (#peek Material, maps) ptr
        params' <- peekArray 4 ((#ptr Material, params) ptr)
        return (Material shader' maps' params')
    poke ptr (Material shader' maps' params') = do
        (#poke Material, shader) ptr shader'
        (#poke Material, maps) ptr maps'
        pokeArray ((#ptr Material, params) ptr) params'

data Transform = Transform
    { tsfTranslation :: Vector3
    , tsfRotation :: Quaternion
    , tsfScale :: Vector3
    }

instance Storable Transform where
    sizeOf _ = #{size Transform}
    alignment _ = #{alignment Transform}
    peek ptr = do
        translation' <- (#peek Transform, translation) ptr
        rotation' <- (#peek Transform, rotation) ptr
        scale' <- (#peek Transform, scale) ptr
        return (Transform translation' rotation' scale')
    poke ptr (Transform translation' rotation' scale') = do
        (#poke Transform, translation) ptr translation'
        (#poke Transform, rotation) ptr rotation'
        (#poke Transform, scale) ptr scale'

data BoneInfo = BoneInfo
    { biName :: [CChar]
    , biParent :: CInt
    }

instance Storable BoneInfo where
    sizeOf _ = #{size BoneInfo}
    alignment _ = #{alignment BoneInfo}
    peek ptr = do
        name' <- peekArray 32 ((#ptr BoneInfo, name) ptr)
        parent' <- (#peek BoneInfo, parent) ptr
        return (BoneInfo name' parent')
    poke ptr (BoneInfo name' parent') = do
        pokeArray ((#ptr BoneInfo, name) ptr) name'
        (#poke BoneInfo, parent) ptr parent'


data Model = Model
    { modelTransform :: Matrix
    , modelMeshCount :: CInt
    , modelMaterialCount :: CInt
    , modelMeshes :: (Ptr Mesh)
    , modelMaterials :: (Ptr Material)
    , modelMeshMaterial :: (Ptr CInt)
    , modelBoneCount :: CInt
    , modelBones :: (Ptr BoneInfo)
    , modelBindPose :: (Ptr Transform)
    }

instance Storable Model where
    sizeOf _ = #{size Model}
    alignment _ = #{alignment Model}
    peek ptr = do
        transform' <- (#peek Model, transform) ptr
        meshCount' <- (#peek Model, meshCount) ptr
        materialCount' <- (#peek Model, materialCount) ptr
        meshes' <- (#peek Model, meshes) ptr
        materials' <- (#peek Model, materials) ptr
        meshMaterial' <- (#peek Model, meshMaterial) ptr
        boneCount' <- (#peek Model, boneCount) ptr
        bones' <- (#peek Model, bones) ptr
        bindPose' <- (#peek Model, bindPose) ptr
        return (Model transform' meshCount' materialCount' meshes' materials' meshMaterial' boneCount' bones' bindPose')
    poke ptr (Model transform' meshCount' materialCount' meshes' materials' meshMaterial' boneCount' bones' bindPose') = do
        (#poke Model, transform) ptr transform'
        (#poke Model, meshCount) ptr meshCount'
        (#poke Model, materialCount) ptr materialCount'
        (#poke Model, meshes) ptr meshes'
        (#poke Model, materials) ptr materials'
        (#poke Model, meshMaterial) ptr meshMaterial'
        (#poke Model, boneCount) ptr boneCount'
        (#poke Model, bones) ptr bones'
        (#poke Model, bindPose) ptr bindPose'

data ModelAnimation = ModelAnimation
    { mdaBoneCount :: CInt
    , mdaFrameCount :: CInt
    , mdaBones :: (Ptr BoneInfo)
    , mdaFramePoses :: (Ptr (Ptr BoneInfo))
    }

instance Storable ModelAnimation where
    sizeOf _ = #{size ModelAnimation}
    alignment _ = #{alignment ModelAnimation}
    peek ptr = do
        boneCount' <- (#peek ModelAnimation, boneCount) ptr
        frameCount' <- (#peek ModelAnimation, frameCount) ptr
        bones' <- (#peek ModelAnimation, bones) ptr
        framePoses' <- (#peek ModelAnimation, framePoses) ptr
        return (ModelAnimation boneCount' frameCount' bones' framePoses')
    poke ptr (ModelAnimation boneCount' frameCount' bones' framePoses') = do
        (#poke ModelAnimation, boneCount) ptr boneCount'
        (#poke ModelAnimation, frameCount) ptr frameCount'
        (#poke ModelAnimation, bones) ptr bones'
        (#poke ModelAnimation, framePoses) ptr framePoses'

data Ray = Ray
    { rayPosition :: Vector3
    , rayDirection :: Vector3
    }

instance Storable Ray where
    sizeOf _ = #{size Ray}
    alignment _ = #{alignment Ray}
    peek ptr = do
        position' <- (#peek Ray, position) ptr
        direction' <- (#peek Ray, direction) ptr
        return $ Ray position' direction'
    poke ptr (Ray position' direction') = do
        (#poke Ray, position) ptr position'
        (#poke Ray, direction) ptr direction'


data RayCollision = RayCollision
    { rycHit :: CBool
    , rycDistance :: CFloat
    , rycPoint :: Vector3
    , rycNormal :: Vector3
    }

instance Storable RayCollision where
    sizeOf _ = #{size RayCollision}
    alignment _ = #{alignment RayCollision}
    peek ptr = do
        hit' <- (#peek RayCollision, hit) ptr
        distance' <- (#peek RayCollision, distance) ptr
        point' <- (#peek RayCollision, point) ptr
        normal' <- (#peek RayCollision, normal) ptr
        return (RayCollision hit' distance' point' normal')
    poke ptr (RayCollision hit' distance' point' normal') = do
        (#poke RayCollision, hit) ptr hit'
        (#poke RayCollision, distance) ptr distance'
        (#poke RayCollision, point) ptr point'
        (#poke RayCollision, normal) ptr normal'

data BoundingBox = BoundingBox
    { bbMin :: Vector3
    , bbMax :: Vector3
    }

instance Storable BoundingBox where
    sizeOf _ = #{size BoundingBox }
    alignment _ = #{alignment BoundingBox}
    peek ptr = do
        min' <- (#peek BoundingBox, min) ptr
        max' <- (#peek BoundingBox, max) ptr
        return (BoundingBox min' max')
    poke ptr (BoundingBox min' max') = do
        (#poke BoundingBox, min) ptr min'
        (#poke BoundingBox, max) ptr max'

data Wave = Wave
    { waveFrameCount :: CUInt
    , waveSampleRate :: CUInt
    , waveSampleSize :: CUInt
    , waveChannels :: CUInt
    , waveData :: (Ptr ())
    }

instance Storable Wave where
    sizeOf _ = #{size Wave}
    alignment _ = #{alignment Wave}
    peek ptr = do
        frameCount' <- (#peek Wave, frameCount) ptr
        sampleRate' <- (#peek Wave, sampleRate) ptr
        sampleSize' <- (#peek Wave, sampleSize) ptr
        channels' <- (#peek Wave, channels) ptr
        data' <- (#peek Wave, data) ptr
        return (Wave frameCount' sampleRate' sampleSize' channels' data')
    poke ptr (Wave frameCount' sampleRate' sampleSize' channels' data') = do
        (#poke Wave, frameCount) ptr frameCount'
        (#poke Wave, sampleRate) ptr sampleRate'
        (#poke Wave, sampleSize) ptr sampleSize'
        (#poke Wave, channels) ptr channels'
        (#poke Wave, data) ptr data'

data Music = Music
  { musicStream :: AudioStream
  , musicFrameCount :: CUInt
  , musicLooping :: CBool
  , musicCtxType :: CInt
  , musicCtxData :: (Ptr ())
  }

instance Storable Music where
  sizeOf _ = #{size Music}
  alignment _ = #{alignment Music}
  peek ptr = do
    stream' <- (#peek Music, stream) ptr
    frameCount' <- (#peek Music, frameCount) ptr
    looping' <- (#peek Music, looping) ptr
    ctxType' <- (#peek Music, ctxType) ptr
    ctxData' <- (#peek Music, ctxData) ptr
    return (Music stream' frameCount' looping' ctxType' ctxData')
  poke ptr (Music stream' frameCount' looping' ctxType' ctxData') = do
    (#poke Music, stream) ptr stream'
    (#poke Music, frameCount) ptr frameCount'
    (#poke Music, looping) ptr looping'
    (#poke Music, ctxType) ptr ctxType'
    (#poke Music, ctxData) ptr ctxData'

data VrDeviceInfo = VrDeviceInfo
  { vdiHResolution :: CInt
  , vdiVResolution :: CInt
  , vdiHScreenSize :: CFloat
  , vdiVScreenSize :: CFloat
  , vdiVScreenCenter :: CFloat
  , vdiEyeToScreenDistance :: CFloat
  , vdiLensSeparationDistance :: CFloat
  , vdiInterpupillaryDistance :: CFloat
  , vdiLensDistortionValues :: [CFloat]
  , vdiChromaAbCorrection :: [CFloat]
  }

instance Storable VrDeviceInfo where
  sizeOf _ = #{size VrDeviceInfo}
  alignment _ = #{alignment VrDeviceInfo}
  peek ptr = do
    hResolution' <- (#peek VrDeviceInfo, hResolution) ptr
    vResolution' <- (#peek VrDeviceInfo, vResolution) ptr
    hScreenSize' <- (#peek VrDeviceInfo, hScreenSize) ptr
    vScreenSize' <- (#peek VrDeviceInfo, vScreenSize) ptr
    vScreenCenter' <- (#peek VrDeviceInfo, vScreenCenter) ptr
    eyeToScreenDistance' <- (#peek VrDeviceInfo, eyeToScreenDistance) ptr
    lensSeparationDistance' <- (#peek VrDeviceInfo, lensSeparationDistance) ptr
    interpupillaryDistance' <- (#peek VrDeviceInfo, interpupillaryDistance) ptr
    lensDistortionValues' <-peekArray 4 ((#ptr VrDeviceInfo, lensDistortionValues) ptr)
    chromaAbCorrection' <-peekArray 4 ((#ptr VrDeviceInfo, chromaAbCorrection) ptr)
    return (VrDeviceInfo hResolution' vResolution' hScreenSize' vScreenSize' vScreenCenter' eyeToScreenDistance' lensSeparationDistance' interpupillaryDistance' lensDistortionValues' chromaAbCorrection')
  poke ptr (VrDeviceInfo hResolution' vResolution' hScreenSize' vScreenSize' vScreenCenter' eyeToScreenDistance' lensSeparationDistance' interpupillaryDistance' lensDistortionValues' chromaAbCorrection') = do
    (#poke VrDeviceInfo, hResolution) ptr hResolution'
    (#poke VrDeviceInfo, vResolution) ptr vResolution'
    (#poke VrDeviceInfo, hScreenSize) ptr hScreenSize'
    (#poke VrDeviceInfo, vScreenSize) ptr vScreenSize'
    (#poke VrDeviceInfo, vScreenCenter) ptr vScreenCenter'
    (#poke VrDeviceInfo, eyeToScreenDistance) ptr eyeToScreenDistance'
    (#poke VrDeviceInfo, lensSeparationDistance) ptr lensSeparationDistance'
    (#poke VrDeviceInfo, interpupillaryDistance) ptr interpupillaryDistance'
    pokeArray ((#ptr VrDeviceInfo, lensDistortionValues) ptr) lensDistortionValues'
    pokeArray ((#ptr VrDeviceInfo, chromaAbCorrection) ptr) chromaAbCorrection'

data VrStereoConfig = VrStereoConfig
  { vscProjection :: [Matrix]
  , vscViewOffset :: [Matrix]
  , vscLeftLensCenter :: [CFloat]
  , vscRightLensCenter :: [CFloat]
  , vscLeftScreenCenter :: [CFloat]
  , vscRightScreenCenter :: [CFloat]
  , vscScale :: [CFloat]
  , vscScaleIn :: [CFloat]
  }

instance Storable VrStereoConfig where
  sizeOf _ = #{size VrStereoConfig}
  alignment _ = #{alignment VrStereoConfig}
  peek ptr = do
    projection' <-peekArray 2 ((#ptr VrStereoConfig, projection) ptr)
    viewOffset' <-peekArray 2 ((#ptr VrStereoConfig, viewOffset) ptr)
    leftLensCenter' <-peekArray 2 ((#ptr VrStereoConfig, leftLensCenter) ptr)
    rightLensCenter' <-peekArray 2 ((#ptr VrStereoConfig, rightLensCenter) ptr)
    leftScreenCenter' <-peekArray 2 ((#ptr VrStereoConfig, leftScreenCenter) ptr)
    rightScreenCenter' <-peekArray 2 ((#ptr VrStereoConfig, rightScreenCenter) ptr)
    scale' <-peekArray 2 ((#ptr VrStereoConfig, scale) ptr)
    scaleIn' <-peekArray 2 ((#ptr VrStereoConfig, scaleIn) ptr)
    return (VrStereoConfig projection' viewOffset' leftLensCenter' rightLensCenter' leftScreenCenter' rightScreenCenter' scale' scaleIn')
  poke ptr (VrStereoConfig projection' viewOffset' leftLensCenter' rightLensCenter' leftScreenCenter' rightScreenCenter' scale' scaleIn') = do
    pokeArray ((#ptr VrStereoConfig, projection) ptr) projection'
    pokeArray ((#ptr VrStereoConfig, viewOffset) ptr) viewOffset'
    pokeArray ((#ptr VrStereoConfig, leftLensCenter) ptr) leftLensCenter'
    pokeArray ((#ptr VrStereoConfig, rightLensCenter) ptr) rightLensCenter'
    pokeArray ((#ptr VrStereoConfig, leftScreenCenter) ptr) leftScreenCenter'
    pokeArray ((#ptr VrStereoConfig, rightScreenCenter) ptr) rightScreenCenter'
    pokeArray ((#ptr VrStereoConfig, scale) ptr) scaleIn'


