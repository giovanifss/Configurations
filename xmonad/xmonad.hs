import XMonad
import XMonad.Config.Desktop

main = xmonad desktopConfig
  { terminal              = myTerminal
  , modMask               = myModMask
  , borderWidth           = myBorderWidth
  , focusFollowsMouse     = myFocusFollowsMouse
  , clickJustFocuses      = myClickJustFocuses
  , normalBorderColor     = myNormalBorderColor
  , focusedBorderColor    = myFocusedBorderColor
  }

myTerminal    = "urxvt"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1

myNormalBorderColor   = "black"
myFocusedBorderColor  = "#4693D8"
myFocusFollowsMouse   = False
myClickJustFocuses    = False
