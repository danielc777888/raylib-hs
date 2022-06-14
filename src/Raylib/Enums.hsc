module Raylib.Enums (
 KeyboardKey (..),
 Gesture (..),
 TraceLogLevel (..)) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

data KeyboardKey = KeyApostrophe
                | KeyComma
                | KeyMinus
                | KeyPeriod
                | KeySlash
                | KeyZero
                | KeyOne
                | KeyTwo
                | KeyThree
                | KeyFour
                | KeyFive
                | KeySix
                | KeySeven
                | KeyEight
                | KeyNine
                | KeySemicolon
                | KeyEqual
                | KeyA
                | KeyB
                | KeyC
                | KeyD
                | KeyE
                | KeyF
                | KeyG
                | KeyH
                | KeyI
                | KeyJ
                | KeyK
                | KeyL
                | KeyM
                | KeyN
                | KeyO
                | KeyP
                | KeyQ
                | KeyR
                | KeyS
                | KeyT
                | KeyU
                | KeyV
                | KeyW
                | KeyX
                | KeyY
                | KeyZ
                | KeySpace
                | KeyEscape
                | KeyEnter
                | KeyTab
                | KeyBackspace
                | KeyInsert
                | KeyDelete
                | KeyRight
                | KeyLeft
                | KeyDown
                | KeyUp
                | KeyPageUp
                | KeyPageDown
                | KeyHome
                | KeyEnd
                | KeyCapsLock
                | KeyScrollLock
                | KeyNumLock
                | KeyPrintScreen
                | KeyPause
                | KeyF1
                | KeyF2
                | KeyF3
                | KeyF4
                | KeyF5
                | KeyF6
                | KeyF7
                | KeyF8
                | KeyF9
                | KeyF10
                | KeyF11
                | KeyF12
                | KeyLeftShift
                | KeyLeftControl
                | KeyLeftAlt
                | KeyLeftSuper
                | KeyRightShift
                | KeyRightControl
                | KeyRightAlt
                | KeyRightSuper
                | KeyKbMenu
                | KeyLeftBracket
                | KeyBackslash
                | KeyRightBracket
                | KeyGrave
                | KeyKp0
                | KeyKp1
                | KeyKp2
                | KeyKp3
                | KeyKp4
                | KeyKp5
                | KeyKp6
                | KeyKp7
                | KeyKp8
                | KeyKp9
                | KeyKpDecimal
                | KeyKpDivide
                | KeyKpMultiply
                | KeyKpSubtract
                | KeyKpAdd
                | KeyKpEnter
                | KeyKpEqual deriving (Show, Eq)

instance Enum KeyboardKey where
  fromEnum KeyApostrophe = #{const KEY_APOSTROPHE}
  fromEnum KeyComma = #{const KEY_COMMA}
  fromEnum KeyMinus = #{const KEY_MINUS}
  fromEnum KeyPeriod = #{const KEY_PERIOD}
  fromEnum KeySlash = #{const KEY_SLASH}
  fromEnum KeyZero = #{const KEY_ZERO}
  fromEnum KeyOne = #{const KEY_ONE}
  fromEnum KeyTwo = #{const KEY_TWO}
  fromEnum KeyThree = #{const KEY_THREE}
  fromEnum KeyFour = #{const KEY_FOUR}
  fromEnum KeyFive = #{const KEY_FIVE}
  fromEnum KeySix = #{const KEY_SIX}
  fromEnum KeySeven = #{const KEY_SEVEN}
  fromEnum KeyEight = #{const KEY_EIGHT}
  fromEnum KeyNine = #{const KEY_NINE}
  fromEnum KeySemicolon = #{const KEY_SEMICOLON}
  fromEnum KeyEqual = #{const KEY_EQUAL}
  fromEnum KeyA = #{const KEY_A}
  fromEnum KeyB = #{const KEY_B}
  fromEnum KeyC = #{const KEY_C}
  fromEnum KeyD = #{const KEY_D}
  fromEnum KeyE = #{const KEY_E}
  fromEnum KeyF = #{const KEY_F}
  fromEnum KeyG = #{const KEY_G}
  fromEnum KeyH = #{const KEY_H}
  fromEnum KeyI = #{const KEY_I}
  fromEnum KeyJ = #{const KEY_J}
  fromEnum KeyK = #{const KEY_K}
  fromEnum KeyL = #{const KEY_L}
  fromEnum KeyM = #{const KEY_M}
  fromEnum KeyN = #{const KEY_N}
  fromEnum KeyO = #{const KEY_O}
  fromEnum KeyP = #{const KEY_P}
  fromEnum KeyQ = #{const KEY_Q}
  fromEnum KeyR = #{const KEY_R}
  fromEnum KeyS = #{const KEY_S}
  fromEnum KeyT = #{const KEY_T}
  fromEnum KeyU = #{const KEY_U}
  fromEnum KeyV = #{const KEY_V}
  fromEnum KeyW = #{const KEY_W}
  fromEnum KeyX = #{const KEY_X}
  fromEnum KeyY = #{const KEY_Y}
  fromEnum KeyZ = #{const KEY_Z}
  fromEnum KeySpace = #{const KEY_SPACE}
  fromEnum KeyEscape = #{const KEY_ESCAPE}
  fromEnum KeyEnter = #{const KEY_ENTER}
  fromEnum KeyTab = #{const KEY_TAB}
  fromEnum KeyBackspace = #{const KEY_BACKSPACE}
  fromEnum KeyInsert = #{const KEY_INSERT}
  fromEnum KeyDelete = #{const KEY_DELETE}
  fromEnum KeyRight = #{const KEY_RIGHT}
  fromEnum KeyLeft = #{const KEY_LEFT}
  fromEnum KeyDown = #{const KEY_DOWN}
  fromEnum KeyUp = #{const KEY_UP}
  fromEnum KeyPageUp = #{const KEY_PAGE_UP}
  fromEnum KeyPageDown = #{const KEY_PAGE_DOWN}
  fromEnum KeyHome = #{const KEY_HOME}
  fromEnum KeyEnd = #{const KEY_END}
  fromEnum KeyCapsLock = #{const KEY_CAPS_LOCK}
  fromEnum KeyScrollLock = #{const KEY_SCROLL_LOCK}
  fromEnum KeyNumLock = #{const KEY_NUM_LOCK}
  fromEnum KeyPrintScreen = #{const KEY_PRINT_SCREEN}
  fromEnum KeyPause = #{const KEY_PAUSE}
  fromEnum KeyF1 = #{const KEY_F1}
  fromEnum KeyF2 = #{const KEY_F2}
  fromEnum KeyF3 = #{const KEY_F3}
  fromEnum KeyF4 = #{const KEY_F4}
  fromEnum KeyF5 = #{const KEY_F5}
  fromEnum KeyF6 = #{const KEY_F6}
  fromEnum KeyF7 = #{const KEY_F7}
  fromEnum KeyF8 = #{const KEY_F8}
  fromEnum KeyF9 = #{const KEY_F9}
  fromEnum KeyF10 = #{const KEY_F10}
  fromEnum KeyF11 = #{const KEY_F11}
  fromEnum KeyF12 = #{const KEY_F12}
  fromEnum KeyLeftShift = #{const KEY_LEFT_SHIFT}
  fromEnum KeyLeftControl = #{const KEY_LEFT_CONTROL}
  fromEnum KeyLeftAlt = #{const KEY_LEFT_ALT}
  fromEnum KeyLeftSuper = #{const KEY_LEFT_SUPER}
  fromEnum KeyRightShift = #{const KEY_RIGHT_SHIFT}
  fromEnum KeyRightControl = #{const KEY_RIGHT_CONTROL}
  fromEnum KeyRightAlt = #{const KEY_RIGHT_ALT}
  fromEnum KeyRightSuper = #{const KEY_RIGHT_SUPER}
  fromEnum KeyKbMenu = #{const KEY_KB_MENU}
  fromEnum KeyLeftBracket = #{const KEY_LEFT_BRACKET}
  fromEnum KeyBackslash = #{const KEY_BACKSLASH}
  fromEnum KeyRightBracket = #{const KEY_RIGHT_BRACKET}
  fromEnum KeyGrave = #{const KEY_GRAVE}
  fromEnum KeyKp0 = #{const KEY_KP_0}
  fromEnum KeyKp1 = #{const KEY_KP_1}
  fromEnum KeyKp2 = #{const KEY_KP_2}
  fromEnum KeyKp3 = #{const KEY_KP_3}
  fromEnum KeyKp4 = #{const KEY_KP_4}
  fromEnum KeyKp5 = #{const KEY_KP_5}
  fromEnum KeyKp6 = #{const KEY_KP_6}
  fromEnum KeyKp7 = #{const KEY_KP_7}
  fromEnum KeyKp8 = #{const KEY_KP_8}
  fromEnum KeyKp9 = #{const KEY_KP_9}
  fromEnum KeyKpDecimal = #{const KEY_KP_DECIMAL}
  fromEnum KeyKpDivide = #{const KEY_KP_DIVIDE}
  fromEnum KeyKpMultiply = #{const KEY_KP_MULTIPLY}
  fromEnum KeyKpSubtract = #{const KEY_KP_SUBTRACT}
  fromEnum KeyKpAdd = #{const KEY_KP_ADD}
  fromEnum KeyKpEnter = #{const KEY_KP_ENTER}
  fromEnum KeyKpEqual = #{const KEY_KP_EQUAL}
  toEnum #{const KEY_APOSTROPHE} = KeyApostrophe
  toEnum #{const KEY_COMMA} = KeyComma
  toEnum #{const KEY_MINUS} = KeyMinus
  toEnum #{const KEY_PERIOD} = KeyPeriod
  toEnum #{const KEY_SLASH} = KeySlash
  toEnum #{const KEY_ZERO} = KeyZero
  toEnum #{const KEY_ONE} = KeyOne
  toEnum #{const KEY_TWO} = KeyTwo
  toEnum #{const KEY_THREE} = KeyThree
  toEnum #{const KEY_FOUR} = KeyFour
  toEnum #{const KEY_FIVE} = KeyFive
  toEnum #{const KEY_SIX} = KeySix
  toEnum #{const KEY_SEVEN} = KeySeven
  toEnum #{const KEY_EIGHT} = KeyEight
  toEnum #{const KEY_NINE} = KeyNine
  toEnum #{const KEY_SEMICOLON} = KeySemicolon
  toEnum #{const KEY_EQUAL} = KeyEqual
  toEnum #{const KEY_A} = KeyA
  toEnum #{const KEY_B} = KeyB
  toEnum #{const KEY_C} = KeyC
  toEnum #{const KEY_D} = KeyD
  toEnum #{const KEY_E} = KeyE
  toEnum #{const KEY_F} = KeyF
  toEnum #{const KEY_G} = KeyG
  toEnum #{const KEY_H} = KeyH
  toEnum #{const KEY_I} = KeyI
  toEnum #{const KEY_J} = KeyJ
  toEnum #{const KEY_K} = KeyK
  toEnum #{const KEY_L} = KeyL
  toEnum #{const KEY_M} = KeyM
  toEnum #{const KEY_N} = KeyN
  toEnum #{const KEY_O} = KeyO
  toEnum #{const KEY_P} = KeyP
  toEnum #{const KEY_Q} = KeyQ
  toEnum #{const KEY_R} = KeyR
  toEnum #{const KEY_S} = KeyS
  toEnum #{const KEY_T} = KeyT
  toEnum #{const KEY_U} = KeyU
  toEnum #{const KEY_V} = KeyV
  toEnum #{const KEY_W} = KeyW
  toEnum #{const KEY_X} = KeyX
  toEnum #{const KEY_Y} = KeyY
  toEnum #{const KEY_Z} = KeyZ
  toEnum #{const KEY_SPACE} = KeySpace
  toEnum #{const KEY_ESCAPE} = KeyEscape
  toEnum #{const KEY_ENTER} = KeyEnter
  toEnum #{const KEY_TAB} = KeyTab
  toEnum #{const KEY_BACKSPACE} = KeyBackspace
  toEnum #{const KEY_INSERT} = KeyInsert
  toEnum #{const KEY_DELETE} = KeyDelete
  toEnum #{const KEY_RIGHT} = KeyRight
  toEnum #{const KEY_LEFT} = KeyLeft
  toEnum #{const KEY_DOWN} = KeyDown
  toEnum #{const KEY_UP} = KeyUp
  toEnum #{const KEY_PAGE_UP} = KeyPageUp
  toEnum #{const KEY_PAGE_DOWN} = KeyPageDown
  toEnum #{const KEY_HOME} = KeyHome
  toEnum #{const KEY_END} = KeyEnd
  toEnum #{const KEY_CAPS_LOCK} = KeyCapsLock
  toEnum #{const KEY_SCROLL_LOCK} = KeyScrollLock
  toEnum #{const KEY_NUM_LOCK} = KeyNumLock
  toEnum #{const KEY_PRINT_SCREEN} = KeyPrintScreen
  toEnum #{const KEY_PAUSE} = KeyPause
  toEnum #{const KEY_F1} = KeyF1
  toEnum #{const KEY_F2} = KeyF2
  toEnum #{const KEY_F3} = KeyF3
  toEnum #{const KEY_F4} = KeyF4
  toEnum #{const KEY_F5} = KeyF5
  toEnum #{const KEY_F6} = KeyF6
  toEnum #{const KEY_F7} = KeyF7
  toEnum #{const KEY_F8} = KeyF8
  toEnum #{const KEY_F9} = KeyF9
  toEnum #{const KEY_F10} = KeyF10
  toEnum #{const KEY_F11} = KeyF11
  toEnum #{const KEY_F12} = KeyF12
  toEnum #{const KEY_LEFT_SHIFT} = KeyLeftShift
  toEnum #{const KEY_LEFT_CONTROL} = KeyLeftControl
  toEnum #{const KEY_LEFT_ALT} = KeyLeftAlt
  toEnum #{const KEY_LEFT_SUPER} = KeyLeftSuper
  toEnum #{const KEY_RIGHT_SHIFT} = KeyRightShift
  toEnum #{const KEY_RIGHT_CONTROL} = KeyRightControl
  toEnum #{const KEY_RIGHT_ALT} = KeyRightAlt
  toEnum #{const KEY_RIGHT_SUPER} = KeyRightSuper
  toEnum #{const KEY_KB_MENU} = KeyKbMenu
  toEnum #{const KEY_LEFT_BRACKET} = KeyLeftBracket
  toEnum #{const KEY_BACKSLASH} = KeyBackslash
  toEnum #{const KEY_RIGHT_BRACKET} = KeyRightBracket
  toEnum #{const KEY_GRAVE} = KeyGrave
  toEnum #{const KEY_KP_0} = KeyKp0
  toEnum #{const KEY_KP_1} = KeyKp1
  toEnum #{const KEY_KP_2} = KeyKp2
  toEnum #{const KEY_KP_3} = KeyKp3
  toEnum #{const KEY_KP_4} = KeyKp4
  toEnum #{const KEY_KP_5} = KeyKp5
  toEnum #{const KEY_KP_6} = KeyKp6
  toEnum #{const KEY_KP_7} = KeyKp7
  toEnum #{const KEY_KP_8} = KeyKp8
  toEnum #{const KEY_KP_9} = KeyKp9
  toEnum #{const KEY_KP_DECIMAL} = KeyKpDecimal
  toEnum #{const KEY_KP_DIVIDE} = KeyKpDivide
  toEnum #{const KEY_KP_MULTIPLY} = KeyKpMultiply
  toEnum #{const KEY_KP_SUBTRACT} = KeyKpSubtract
  toEnum #{const KEY_KP_ADD} = KeyKpAdd
  toEnum #{const KEY_KP_ENTER} = KeyKpEnter
  toEnum #{const KEY_KP_EQUAL} = KeyKpEqual
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
