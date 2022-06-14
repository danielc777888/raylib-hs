module Raylib.Structs (
 Texture (..),
 Texture2D (..),
 TextureCubeMap (..),
 RenderTexture (..),
 RenderTexture2D (..),
 Image (..),
 Font (..),
 Sound (..),
 AudioStream (..),
 Vector2 (..),
 Vector3 (..),
 Vector4 (..),
 Quaternion (..),
 Color (..),
 Rectangle (..),
 Matrix (..),
 NPatchInfo (..),
 Camera (..),
 Camera3D (..),
 Camera2D (..),
 Mesh (..),
 Shader (..),
 MaterialMap (..),
 Material (..),
 Transform (..),
 BoneInfo (..),
 Model (..)) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

type Texture2D = Texture
type TextureCubeMap = Texture

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

type Camera = Camera3D

data Camera3D = Camera3D
  { camera3DPosition :: !Vector3
  , camera3DTarget :: !Vector3
  , camera3DUp :: !Vector3
  , camera3DFovy :: !CFloat
  , camera3DProjection :: !CInt
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
    return $! Camera3D position' target' up' fovy' projection'
  poke ptr (Camera3D position' target' up' fovy' projection') = do
    (#poke Camera3D, position) ptr position'
    (#poke Camera3D, target) ptr target'
    (#poke Camera3D, up) ptr up'
    (#poke Camera3D, fovy) ptr fovy'
    (#poke Camera3D, projection) ptr projection'

data Camera2D = Camera2D
  { camera2DOffset :: !Vector2
  , camera2DTarget :: !Vector2
  , camera2DRotation :: !CFloat
  , camera2DZoom :: !CFloat
  }

instance Storable Camera2D where
  sizeOf _ = #{size Camera2D}
  alignment _ = #{alignment Camera2D}
  peek ptr = do
    offset' <- (#peek Camera2D, offset) ptr
    target' <- (#peek Camera2D, target) ptr
    rotation' <- (#peek Camera2D, rotation) ptr
    zoom' <- (#peek Camera2D, zoom) ptr
    return $! Camera2D offset' target' rotation' zoom'
  poke ptr (Camera2D offset' target' rotation' zoom') = do
    (#poke Camera2D, offset) ptr offset'
    (#poke Camera2D, target) ptr target'
    (#poke Camera2D, rotation) ptr rotation'
    (#poke Camera2D, zoom) ptr zoom'

data Mesh = Mesh
  { meshVertexCount :: !CInt
  , meshTriangleCount :: !CInt
  , meshVertices :: !(Ptr CFloat)
  , meshTexcoords :: !(Ptr CFloat)
  , meshTexcoords2 :: !(Ptr CFloat)
  , meshNormals :: !(Ptr CFloat)
  , meshTangents :: !(Ptr CFloat)
  , meshColors :: !(Ptr CInt)
  , meshIndices :: !(Ptr CInt)
  , meshAnimVertices :: !(Ptr CFloat)
  , meshAnimNormals :: !(Ptr CFloat)
  , meshBoneIds :: !(Ptr CInt)
  , meshBoneWeights :: !(Ptr CFloat)
  , meshVaoId :: !CInt
  , meshVboId :: !(Ptr CInt)
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
    return $! Mesh vertexCount' triangleCount' vertices' texcoords' texcoords2' normals' tangents' colors' indices' animVertices' animNormals' boneIds' boneWeights' vaoId' vboId'
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
    { shaderId :: !CInt
    , shaderLocs :: !(Ptr CInt)
    }

instance Storable Shader where
    sizeOf _ = #{size Shader}
    alignment _ = #{alignment Shader}
    peek ptr = do
        id' <- (#peek Shader, id) ptr
        locs' <- (#peek Shader, locs) ptr
        return $! Shader id' locs'
    poke ptr (Shader id' locs') = do
        (#poke Shader, id) ptr id'
        (#poke Shader, locs) ptr locs'

data MaterialMap = MaterialMap
    { materialMapTexture :: !Texture2D
    , materialMapColor :: !Color
    , materialMapValue :: !CFloat
    }

instance Storable MaterialMap where
    sizeOf _ = #{size MaterialMap}
    alignment _ = #{alignment MaterialMap}
    peek ptr = do
        texture' <- (#peek MaterialMap, texture) ptr
        color' <- (#peek MaterialMap, color) ptr
        value' <- (#peek MaterialMap, value) ptr
        return $! MaterialMap texture' color' value'
    poke ptr (MaterialMap texture' color' value') = do
        (#poke MaterialMap, texture) ptr texture'
        (#poke MaterialMap, color) ptr color'
        (#poke MaterialMap, value) ptr value'

data Material = Material
    { materialShader :: !Shader
    , materialMaps :: !(Ptr MaterialMap)
    , materialParams :: ![CFloat]
    }

instance Storable Material where
    sizeOf _ = #{size Material}
    alignment _ = #{alignment Material}
    peek ptr = do
        shader' <- (#peek Material, shader) ptr
        maps' <- (#peek Material, maps) ptr
        params' <- peekArray 4 ((#ptr Material, params) ptr)
        return $! Material shader' maps' params'
    poke ptr (Material shader' maps' params') = do
        (#poke Material, shader) ptr shader'
        (#poke Material, maps) ptr maps'
        pokeArray ((#ptr Material, params) ptr) params'

data Transform = Transform
    { transformTranslation :: !Vector3
    , transformRotation :: !Quaternion
    , transformScale :: !Vector3
    }

instance Storable Transform where
    sizeOf _ = #{size Transform}
    alignment _ = #{alignment Transform}
    peek ptr = do
        translation' <- (#peek Transform, translation) ptr
        rotation' <- (#peek Transform, rotation) ptr
        scale' <- (#peek Transform, scale) ptr
        return $! Transform translation' rotation' scale'
    poke ptr (Transform translation' rotation' scale') = do
        (#poke Transform, translation) ptr translation'
        (#poke Transform, rotation) ptr rotation'
        (#poke Transform, scale) ptr scale'

data BoneInfo = BoneInfo
    { boneInfoName :: ![CChar]
    , boneInfoParent :: !CInt
    }

instance Storable BoneInfo where
    sizeOf _ = #{size BoneInfo}
    alignment _ = #{alignment BoneInfo}
    peek ptr = do
        name' <- peekArray 32 ((#ptr BoneInfo, name) ptr)
        parent' <- (#peek BoneInfo, parent) ptr
        return $! BoneInfo name' parent'
    poke ptr (BoneInfo name' parent') = do
        pokeArray ((#ptr BoneInfo, name) ptr) name'
        (#poke BoneInfo, parent) ptr parent'

    {-
typedef struct Model {
    Matrix transform;       // Local transform matrix

    int meshCount;          // Number of meshes
    int materialCount;      // Number of materials
    Mesh *meshes;           // Meshes array
    Material *materials;    // Materials array
    int *meshMaterial;      // Mesh material number

    // Animation data
    int boneCount;          // Number of bones
    BoneInfo *bones;        // Bones information (skeleton)
    Transform *bindPose;    // Bones base transformation (pose)
} Model;
-}

data Model = Model
    { modelTransform :: !Matrix
    , modelMeshCount :: !CInt
    , modelMaterialCount :: !CInt
    , modelMeshes :: !(Ptr Mesh)
    , modelMaterials :: !(Ptr Material)
    , modelMeshMaterial :: !(Ptr CInt)
    , modelBoneCount :: !CInt
    , modelBones :: !(Ptr BoneInfo)
    , modelBindPose :: !(Ptr Transform)
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
        return $! Model transform' meshCount' materialCount' meshes' materials' meshMaterial' boneCount' bones' bindPose'
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


