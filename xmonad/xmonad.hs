import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Hooks.DynamicBars as Bars
import qualified XMonad.Actions.CopyWindow as CopyW
import qualified XMonad.Hooks.DynamicLog as DLog
import qualified XMonad.Hooks.WorkspaceHistory as WH

import System.IO

main = do
  xmonad $ docks $ desktopConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , startupHook = do
        Bars.dynStatusBarStartup xmobarCreator xmobarDestroyer
    , logHook = do
        copies <- CopyW.wsContainingCopies
        WH.workspaceHistoryHook
        Bars.multiPP (myLogPPActive copies) (myLogPP copies)
    , handleEventHook = Bars.dynStatusBarEventHook xmobarCreator xmobarDestroyer
    , terminal              = myTerminal
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    }

myTerminal    = "terminator"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1

myNormalBorderColor   = "black"
myFocusedBorderColor  = "#404040"
myFocusFollowsMouse   = False
myClickJustFocuses    = False

-- Xmobar multiple screens --
xmobarCreator :: Bars.DynamicStatusBar
xmobarCreator (S sid) = spawnPipe $ "<XMOBAR-BIN> <XMOBAR-RC> --screen " ++ show sid

xmobarDestroyer :: Bars.DynamicStatusBarCleanup
xmobarDestroyer = return ()

myLogPP :: [WorkspaceId] -> DLog.PP
myLogPP copies = DLog.defaultPP

myLogPPActive :: [WorkspaceId] -> DLog.PP
myLogPPActive copies = (myLogPP copies)
  { DLog.ppCurrent = DLog.xmobarColor myCurrentBG myNormalFG . DLog.pad
  }

myCurrentBG = "#888888"
myNormalFG = "#ffffff"
