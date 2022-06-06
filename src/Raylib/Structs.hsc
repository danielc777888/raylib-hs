module Raylib.Structs (
 Texture,
 Texture2D,
 Font (Font),
 Sound,
 Vector2 (Vector2),
 Color (Color),
 KeyboardKey (..),
 Gesture (..),
 TraceLogLevel (..),
 Rectangle (Rectangle)) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

type Texture2D = Texture

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
    return (Texture id' width' height' mipmaps' format')
  poke ptr (Texture id' width' height' mipmaps' format') = do
    (#poke Texture, id) ptr id'
    (#poke Texture, width) ptr width'
    (#poke Texture, height) ptr height'
    (#poke Texture, mipmaps) ptr mipmaps'
    (#poke Texture, format) ptr format'

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
    return (Color r' g' b' a')
  poke ptr (Color r' g' b' a') = do
    (#poke Color, r) ptr r'
    (#poke Color, g) ptr g'
    (#poke Color, b) ptr b'
    (#poke Color, a) ptr a'

data KeyboardKey = Key_Apostrophe
                | Key_Comma
                | Key_Minus
                | Key_Period
                | Key_Slash
                | Key_Zero
                | Key_One
                | Key_Two
                | Key_Three
                | Key_Four
                | Key_Five
                | Key_Six
                | Key_Seven
                | Key_Eight
                | Key_Nine
                | Key_Semicolon
                | Key_Equal
                | Key_A
                | Key_B
                | Key_C
                | Key_D
                | Key_E
                | Key_F
                | Key_G
                | Key_H
                | Key_I
                | Key_J
                | Key_K
                | Key_L
                | Key_M
                | Key_N
                | Key_O
                | Key_P
                | Key_Q
                | Key_R
                | Key_S
                | Key_T
                | Key_U
                | Key_V
                | Key_W
                | Key_X
                | Key_Y
                | Key_Z
                | Key_Space
                | Key_Escape
                | Key_Enter
                | Key_Tab
                | Key_Backspace
                | Key_Insert
                | Key_Delete
                | Key_Right
                | Key_Left
                | Key_Down
                | Key_Up
                | Key_Page_Up
                | Key_Page_Down
                | Key_Home
                | Key_End
                | Key_Caps_Lock
                | Key_Scroll_Lock
                | Key_Num_Lock
                | Key_Print_Screen
                | Key_Pause
                | Key_F1
                | Key_F2
                | Key_F3
                | Key_F4
                | Key_F5
                | Key_F6
                | Key_F7
                | Key_F8
                | Key_F9
                | Key_F10
                | Key_F11
                | Key_F12
                | Key_Left_Shift
                | Key_Left_Control
                | Key_Left_Alt
                | Key_Left_Super
                | Key_Right_Shift
                | Key_Right_Control
                | Key_Right_Alt
                | Key_Right_Super
                | Key_Kb_Menu
                | Key_Left_Bracket
                | Key_Backslash
                | Key_Right_Bracket
                | Key_Grave
                | Key_Kp_0
                | Key_Kp_1
                | Key_Kp_2
                | Key_Kp_3
                | Key_Kp_4
                | Key_Kp_5
                | Key_Kp_6
                | Key_Kp_7
                | Key_Kp_8
                | Key_Kp_9
                | Key_Kp_Decimal
                | Key_Kp_Divide
                | Key_Kp_Multiply
                | Key_Kp_Subtract
                | Key_Kp_Add
                | Key_Kp_Enter
                | Key_Kp_Equal deriving (Show, Eq)

instance Enum KeyboardKey where
  fromEnum Key_Apostrophe = #{const KEY_APOSTROPHE}
  fromEnum Key_Comma = #{const KEY_COMMA}
  fromEnum Key_Minus = #{const KEY_MINUS}
  fromEnum Key_Period = #{const KEY_PERIOD}
  fromEnum Key_Slash = #{const KEY_SLASH}
  fromEnum Key_Zero = #{const KEY_ZERO}
  fromEnum Key_One = #{const KEY_ONE}
  fromEnum Key_Two = #{const KEY_TWO}
  fromEnum Key_Three = #{const KEY_THREE}
  fromEnum Key_Four = #{const KEY_FOUR}
  fromEnum Key_Five = #{const KEY_FIVE}
  fromEnum Key_Six = #{const KEY_SIX}
  fromEnum Key_Seven = #{const KEY_SEVEN}
  fromEnum Key_Eight = #{const KEY_EIGHT}
  fromEnum Key_Nine = #{const KEY_NINE}
  fromEnum Key_Semicolon = #{const KEY_SEMICOLON}
  fromEnum Key_Equal = #{const KEY_EQUAL}
  fromEnum Key_A = #{const KEY_A}
  fromEnum Key_B = #{const KEY_B}
  fromEnum Key_C = #{const KEY_C}
  fromEnum Key_D = #{const KEY_D}
  fromEnum Key_E = #{const KEY_E}
  fromEnum Key_F = #{const KEY_F}
  fromEnum Key_G = #{const KEY_G}
  fromEnum Key_H = #{const KEY_H}
  fromEnum Key_I = #{const KEY_I}
  fromEnum Key_J = #{const KEY_J}
  fromEnum Key_K = #{const KEY_K}
  fromEnum Key_L = #{const KEY_L}
  fromEnum Key_M = #{const KEY_M}
  fromEnum Key_N = #{const KEY_N}
  fromEnum Key_O = #{const KEY_O}
  fromEnum Key_P = #{const KEY_P}
  fromEnum Key_Q = #{const KEY_Q}
  fromEnum Key_R = #{const KEY_R}
  fromEnum Key_S = #{const KEY_S}
  fromEnum Key_T = #{const KEY_T}
  fromEnum Key_U = #{const KEY_U}
  fromEnum Key_V = #{const KEY_V}
  fromEnum Key_W = #{const KEY_W}
  fromEnum Key_X = #{const KEY_X}
  fromEnum Key_Y = #{const KEY_Y}
  fromEnum Key_Z = #{const KEY_Z}
  fromEnum Key_Space = #{const KEY_SPACE}
  fromEnum Key_Escape = #{const KEY_ESCAPE}
  fromEnum Key_Enter = #{const KEY_ENTER}
  fromEnum Key_Tab = #{const KEY_TAB}
  fromEnum Key_Backspace = #{const KEY_BACKSPACE}
  fromEnum Key_Insert = #{const KEY_INSERT}
  fromEnum Key_Delete = #{const KEY_DELETE}
  fromEnum Key_Right = #{const KEY_RIGHT}
  fromEnum Key_Left = #{const KEY_LEFT}
  fromEnum Key_Down = #{const KEY_DOWN}
  fromEnum Key_Up = #{const KEY_UP}
  fromEnum Key_Page_Up = #{const KEY_PAGE_UP}
  fromEnum Key_Page_Down = #{const KEY_PAGE_DOWN}
  fromEnum Key_Home = #{const KEY_HOME}
  fromEnum Key_End = #{const KEY_END}
  fromEnum Key_Caps_Lock = #{const KEY_CAPS_LOCK}
  fromEnum Key_Scroll_Lock = #{const KEY_SCROLL_LOCK}
  fromEnum Key_Num_Lock = #{const KEY_NUM_LOCK}
  fromEnum Key_Print_Screen = #{const KEY_PRINT_SCREEN}
  fromEnum Key_Pause = #{const KEY_PAUSE}
  fromEnum Key_F1 = #{const KEY_F1}
  fromEnum Key_F2 = #{const KEY_F2}
  fromEnum Key_F3 = #{const KEY_F3}
  fromEnum Key_F4 = #{const KEY_F4}
  fromEnum Key_F5 = #{const KEY_F5}
  fromEnum Key_F6 = #{const KEY_F6}
  fromEnum Key_F7 = #{const KEY_F7}
  fromEnum Key_F8 = #{const KEY_F8}
  fromEnum Key_F9 = #{const KEY_F9}
  fromEnum Key_F10 = #{const KEY_F10}
  fromEnum Key_F11 = #{const KEY_F11}
  fromEnum Key_F12 = #{const KEY_F12}
  fromEnum Key_Left_Shift = #{const KEY_LEFT_SHIFT}
  fromEnum Key_Left_Control = #{const KEY_LEFT_CONTROL}
  fromEnum Key_Left_Alt = #{const KEY_LEFT_ALT}
  fromEnum Key_Left_Super = #{const KEY_LEFT_SUPER}
  fromEnum Key_Right_Shift = #{const KEY_RIGHT_SHIFT}
  fromEnum Key_Right_Control = #{const KEY_RIGHT_CONTROL}
  fromEnum Key_Right_Alt = #{const KEY_RIGHT_ALT}
  fromEnum Key_Right_Super = #{const KEY_RIGHT_SUPER}
  fromEnum Key_Kb_Menu = #{const KEY_KB_MENU}
  fromEnum Key_Left_Bracket = #{const KEY_LEFT_BRACKET}
  fromEnum Key_Backslash = #{const KEY_BACKSLASH}
  fromEnum Key_Right_Bracket = #{const KEY_RIGHT_BRACKET}
  fromEnum Key_Grave = #{const KEY_GRAVE}
  fromEnum Key_Kp_0 = #{const KEY_KP_0}
  fromEnum Key_Kp_1 = #{const KEY_KP_1}
  fromEnum Key_Kp_2 = #{const KEY_KP_2}
  fromEnum Key_Kp_3 = #{const KEY_KP_3}
  fromEnum Key_Kp_4 = #{const KEY_KP_4}
  fromEnum Key_Kp_5 = #{const KEY_KP_5}
  fromEnum Key_Kp_6 = #{const KEY_KP_6}
  fromEnum Key_Kp_7 = #{const KEY_KP_7}
  fromEnum Key_Kp_8 = #{const KEY_KP_8}
  fromEnum Key_Kp_9 = #{const KEY_KP_9}
  fromEnum Key_Kp_Decimal = #{const KEY_KP_DECIMAL}
  fromEnum Key_Kp_Divide = #{const KEY_KP_DIVIDE}
  fromEnum Key_Kp_Multiply = #{const KEY_KP_MULTIPLY}
  fromEnum Key_Kp_Subtract = #{const KEY_KP_SUBTRACT}
  fromEnum Key_Kp_Add = #{const KEY_KP_ADD}
  fromEnum Key_Kp_Enter = #{const KEY_KP_ENTER}
  fromEnum Key_Kp_Equal = #{const KEY_KP_EQUAL}
  toEnum #{const KEY_APOSTROPHE} = Key_Apostrophe
  toEnum #{const KEY_COMMA} = Key_Comma
  toEnum #{const KEY_MINUS} = Key_Minus
  toEnum #{const KEY_PERIOD} = Key_Period
  toEnum #{const KEY_SLASH} = Key_Slash
  toEnum #{const KEY_ZERO} = Key_Zero
  toEnum #{const KEY_ONE} = Key_One
  toEnum #{const KEY_TWO} = Key_Two
  toEnum #{const KEY_THREE} = Key_Three
  toEnum #{const KEY_FOUR} = Key_Four
  toEnum #{const KEY_FIVE} = Key_Five
  toEnum #{const KEY_SIX} = Key_Six
  toEnum #{const KEY_SEVEN} = Key_Seven
  toEnum #{const KEY_EIGHT} = Key_Eight
  toEnum #{const KEY_NINE} = Key_Nine
  toEnum #{const KEY_SEMICOLON} = Key_Semicolon
  toEnum #{const KEY_EQUAL} = Key_Equal
  toEnum #{const KEY_A} = Key_A
  toEnum #{const KEY_B} = Key_B
  toEnum #{const KEY_C} = Key_C
  toEnum #{const KEY_D} = Key_D
  toEnum #{const KEY_E} = Key_E
  toEnum #{const KEY_F} = Key_F
  toEnum #{const KEY_G} = Key_G
  toEnum #{const KEY_H} = Key_H
  toEnum #{const KEY_I} = Key_I
  toEnum #{const KEY_J} = Key_J
  toEnum #{const KEY_K} = Key_K
  toEnum #{const KEY_L} = Key_L
  toEnum #{const KEY_M} = Key_M
  toEnum #{const KEY_N} = Key_N
  toEnum #{const KEY_O} = Key_O
  toEnum #{const KEY_P} = Key_P
  toEnum #{const KEY_Q} = Key_Q
  toEnum #{const KEY_R} = Key_R
  toEnum #{const KEY_S} = Key_S
  toEnum #{const KEY_T} = Key_T
  toEnum #{const KEY_U} = Key_U
  toEnum #{const KEY_V} = Key_V
  toEnum #{const KEY_W} = Key_W
  toEnum #{const KEY_X} = Key_X
  toEnum #{const KEY_Y} = Key_Y
  toEnum #{const KEY_Z} = Key_Z
  toEnum #{const KEY_SPACE} = Key_Space
  toEnum #{const KEY_ESCAPE} = Key_Escape
  toEnum #{const KEY_ENTER} = Key_Enter
  toEnum #{const KEY_TAB} = Key_Tab
  toEnum #{const KEY_BACKSPACE} = Key_Backspace
  toEnum #{const KEY_INSERT} = Key_Insert
  toEnum #{const KEY_DELETE} = Key_Delete
  toEnum #{const KEY_RIGHT} = Key_Right
  toEnum #{const KEY_LEFT} = Key_Left
  toEnum #{const KEY_DOWN} = Key_Down
  toEnum #{const KEY_UP} = Key_Up
  toEnum #{const KEY_PAGE_UP} = Key_Page_Up
  toEnum #{const KEY_PAGE_DOWN} = Key_Page_Down
  toEnum #{const KEY_HOME} = Key_Home
  toEnum #{const KEY_END} = Key_End
  toEnum #{const KEY_CAPS_LOCK} = Key_Caps_Lock
  toEnum #{const KEY_SCROLL_LOCK} = Key_Scroll_Lock
  toEnum #{const KEY_NUM_LOCK} = Key_Num_Lock
  toEnum #{const KEY_PRINT_SCREEN} = Key_Print_Screen
  toEnum #{const KEY_PAUSE} = Key_Pause
  toEnum #{const KEY_F1} = Key_F1
  toEnum #{const KEY_F2} = Key_F2
  toEnum #{const KEY_F3} = Key_F3
  toEnum #{const KEY_F4} = Key_F4
  toEnum #{const KEY_F5} = Key_F5
  toEnum #{const KEY_F6} = Key_F6
  toEnum #{const KEY_F7} = Key_F7
  toEnum #{const KEY_F8} = Key_F8
  toEnum #{const KEY_F9} = Key_F9
  toEnum #{const KEY_F10} = Key_F10
  toEnum #{const KEY_F11} = Key_F11
  toEnum #{const KEY_F12} = Key_F12
  toEnum #{const KEY_LEFT_SHIFT} = Key_Left_Shift
  toEnum #{const KEY_LEFT_CONTROL} = Key_Left_Control
  toEnum #{const KEY_LEFT_ALT} = Key_Left_Alt
  toEnum #{const KEY_LEFT_SUPER} = Key_Left_Super
  toEnum #{const KEY_RIGHT_SHIFT} = Key_Right_Shift
  toEnum #{const KEY_RIGHT_CONTROL} = Key_Right_Control
  toEnum #{const KEY_RIGHT_ALT} = Key_Right_Alt
  toEnum #{const KEY_RIGHT_SUPER} = Key_Right_Super
  toEnum #{const KEY_KB_MENU} = Key_Kb_Menu
  toEnum #{const KEY_LEFT_BRACKET} = Key_Left_Bracket
  toEnum #{const KEY_BACKSLASH} = Key_Backslash
  toEnum #{const KEY_RIGHT_BRACKET} = Key_Right_Bracket
  toEnum #{const KEY_GRAVE} = Key_Grave
  toEnum #{const KEY_KP_0} = Key_Kp_0
  toEnum #{const KEY_KP_1} = Key_Kp_1
  toEnum #{const KEY_KP_2} = Key_Kp_2
  toEnum #{const KEY_KP_3} = Key_Kp_3
  toEnum #{const KEY_KP_4} = Key_Kp_4
  toEnum #{const KEY_KP_5} = Key_Kp_5
  toEnum #{const KEY_KP_6} = Key_Kp_6
  toEnum #{const KEY_KP_7} = Key_Kp_7
  toEnum #{const KEY_KP_8} = Key_Kp_8
  toEnum #{const KEY_KP_9} = Key_Kp_9
  toEnum #{const KEY_KP_DECIMAL} = Key_Kp_Decimal
  toEnum #{const KEY_KP_DIVIDE} = Key_Kp_Divide
  toEnum #{const KEY_KP_MULTIPLY} = Key_Kp_Multiply
  toEnum #{const KEY_KP_SUBTRACT} = Key_Kp_Subtract
  toEnum #{const KEY_KP_ADD} = Key_Kp_Add
  toEnum #{const KEY_KP_ENTER} = Key_Kp_Enter
  toEnum #{const KEY_KP_EQUAL} = Key_Kp_Equal
  toEnum _ = error "no such value for KeyboardKey"


data Gesture = GestureNone
              | GestureTap
              | GestureDoubleTap
              | GestureHold
              | GestureDrag
              | GestureSwipeRight
              | GestureSwipeLeft
              | GestureSwipeUp
              | GestureSwipeDown
              | GesturePinchIn
              | GesturePinchOut deriving (Show, Eq)

instance Enum Gesture where
  fromEnum GestureNone = #{const GESTURE_NONE}
  fromEnum GestureTap = #{const GESTURE_TAP}
  fromEnum GestureDoubleTap = #{const GESTURE_DOUBLETAP}
  fromEnum GestureHold = #{const GESTURE_HOLD}
  fromEnum GestureDrag = #{const GESTURE_DRAG}
  fromEnum GestureSwipeRight = #{const GESTURE_SWIPE_RIGHT}
  fromEnum GestureSwipeLeft = #{const GESTURE_SWIPE_LEFT}
  fromEnum GestureSwipeUp = #{const GESTURE_SWIPE_UP}
  fromEnum GestureSwipeDown = #{const GESTURE_SWIPE_DOWN}
  fromEnum GesturePinchIn = #{const GESTURE_PINCH_IN}
  fromEnum GesturePinchOut = #{const GESTURE_PINCH_OUT}
  toEnum #{const GESTURE_NONE} = GestureNone
  toEnum #{const GESTURE_TAP} = GestureTap
  toEnum #{const GESTURE_DOUBLETAP} = GestureDoubleTap
  toEnum #{const GESTURE_HOLD} = GestureHold
  toEnum #{const GESTURE_DRAG} = GestureDrag
  toEnum #{const GESTURE_SWIPE_RIGHT} = GestureSwipeRight
  toEnum #{const GESTURE_SWIPE_LEFT} = GestureSwipeLeft
  toEnum #{const GESTURE_SWIPE_UP} = GestureSwipeUp
  toEnum #{const GESTURE_SWIPE_DOWN} = GestureSwipeDown
  toEnum #{const GESTURE_PINCH_IN} = GesturePinchIn
  toEnum #{const GESTURE_PINCH_OUT} = GesturePinchOut

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
    return (AudioStream buffer' sampleRate' sampleSize' channels')
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
    return (Sound stream' frameCount')
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
    return (Vector2 x' y')
  poke ptr (Vector2 x' y') = do
    (#poke Vector2, x) ptr x'
    (#poke Vector2, y) ptr y'

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
    return (Image data' width' height' mipmaps' format')
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
    return (GlyphInfo value' offsetX' offsetY' advanceX' image')
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
    return (Rectangle x' y' width' height')
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
    return (Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs')
  poke ptr (Font baseSize' glyphCount' glyphPadding' texture' recs' glyphs') = do
    (#poke Font, baseSize) ptr baseSize'
    (#poke Font, glyphCount) ptr glyphCount'
    (#poke Font, glyphPadding) ptr glyphPadding'
    (#poke Font, texture) ptr texture'
    (#poke Font, recs) ptr recs'
    (#poke Font, glyphs) ptr glyphs'


data TraceLogLevel = LogAll
                  | LogTrace
                  | LogDebug
                  | LogInfo
                  | LogWarning
                  | LogError
                  | LogFatal
                  | LogNone deriving (Show, Eq)

instance Enum TraceLogLevel where
  fromEnum LogAll = #{const LOG_ALL}
  fromEnum LogTrace = #{const LOG_TRACE}
  fromEnum LogDebug = #{const LOG_DEBUG}
  fromEnum LogInfo = #{const LOG_INFO}
  fromEnum LogWarning = #{const LOG_WARNING}
  fromEnum LogError = #{const LOG_ERROR}
  fromEnum LogFatal = #{const LOG_FATAL}
  fromEnum LogNone = #{const LOG_NONE}
  toEnum #{const LOG_ALL} = LogAll
  toEnum #{const LOG_TRACE} = LogTrace
  toEnum #{const LOG_DEBUG} = LogDebug
  toEnum #{const LOG_INFO} = LogInfo
  toEnum #{const LOG_WARNING} = LogWarning
  toEnum #{const LOG_ERROR} = LogError
  toEnum #{const LOG_FATAL} = LogFatal
  toEnum #{const LOG_NONE} = LogNone


