import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Hooks.DynamicBars as Bars

import System.IO

main = do
  xmonad $ docks $ desktopConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , startupHook = do
        Bars.dynStatusBarStartup xmobarCreator xmobarDestroyer
    , handleEventHook = Bars.dynStatusBarEventHook xmobarCreator xmobarDestroyer
    , terminal              = myTerminal
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , keys                  = myKeys
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

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, xK_Return), spawn $ XMonad.terminal conf)                           -- Launch terminal
  , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")      -- Launch dmenu
  , ((modm, xK_c), kill)                                                        -- Close focused window
  , ((modm, xK_k), windows W.focusUp)                                           -- Move focus to the previous window
  , ((modm, xK_j), windows W.focusDown)                                         -- Move focus to the next window
  , ((modm, xK_t), withFocused $ windows . W.sink)                              -- Push window back into tiling
  ]
  ++
  [((m .|. modm, k), windows $ f i)                                             -- mod-shift-[1..9], Move client to workspace N
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]                     -- mod-[1..9], Switch to workspace N
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]                                 -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  [((modm, xK_g), goToSelected defaultGSConfig)]                                -- GridSelect
