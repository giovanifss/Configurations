import XMonad
import XMonad.Config.Desktop

main = xmonad desktopConfig
  { terminal    = myTerminal
  , modMask     = myModMask
  , borderWidth = myBorderWidth
  }

myTerminal    = "urxvt"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1
