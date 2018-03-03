import System.IO
import qualified Data.Map as M

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import qualified XMonad.StackSet as W
import qualified XMonad.Hooks.DynamicLog as DLog
import qualified XMonad.Hooks.DynamicBars as Bars
import qualified XMonad.Actions.CopyWindow as CopyW
import qualified XMonad.Hooks.WorkspaceHistory as WH
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.Layout.IndependentScreens as IS

main = do
  nScreens <- IS.countScreens
  xmonad $ docks $ desktopConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , startupHook = do
        Bars.dynStatusBarStartup xmobarCreator xmobarDestroyer
    , handleEventHook = Bars.dynStatusBarEventHook xmobarCreator xmobarDestroyer
    , logHook = do copies <- CopyW.wsContainingCopies
                   WH.workspaceHistoryHook
                   Bars.multiPP (myLogPPActive copies) (myLogPP copies)
    , terminal              = myTerminal
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , normalBorderColor     = black
    , focusedBorderColor    = lightgrey
    , workspaces            = IS.withScreens nScreens (map show [1..9])
    , keys                  = myKeys
    }

-- Usability --
myTerminal    = "terminator"
myModMask     = mod4Mask      -- Win key or Super_L
myBorderWidth = 1

myFocusFollowsMouse   = False
myClickJustFocuses    = False

-- Color configuration --
black       = "#000000"
grey        = "#808080"
lightgrey   = "#dddddd"
white       = "#ffffff"
darkred     = "#cc0000"
orange      = "#ee9a00"
lightgreen  = "#00c500"
darkgreen   = "#008000"
lightblue   = "#70c4df"

-- Xmobar multiple screens --
xmobarCreator :: Bars.DynamicStatusBar
xmobarCreator (S sid) = spawnPipe $ "<XMOBAR-BIN> <XMOBAR-RC> --screen " ++ show sid

xmobarDestroyer :: Bars.DynamicStatusBarCleanup
xmobarDestroyer = return ()

myLogPP :: [WorkspaceId] -> DLog.PP
myLogPP copies = DLog.defaultPP
  { DLog.ppCurrent = DLog.xmobarColor grey black . DLog.pad
  , DLog.ppVisible = DLog.xmobarColor grey black . DLog.pad
  , DLog.ppHidden  = DLog.xmobarColor grey black
  , DLog.ppUrgent  = DLog.xmobarColor orange black . DLog.wrap ">" "<" . DLog.xmobarStrip
  , DLog.ppTitle   = DLog.xmobarColor orange black . DLog.shorten 35
  , DLog.ppLayout  = DLog.xmobarColor lightgrey black
  , DLog.ppSep     = DLog.pad $ DLog.xmobarColor lightgrey black "|"
  -- , DLog.ppOutput  = Run.hPutStrLn h
  }

myLogPPActive :: [WorkspaceId] -> DLog.PP
myLogPPActive copies = (myLogPP copies)
  { DLog.ppCurrent = DLog.xmobarColor orange black . DLog.pad
  }

-- Keybindings --
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, xK_Return), spawn $ XMonad.terminal conf)                           -- Launch terminal
  , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")      -- Launch dmenu
  , ((modm, xK_c), kill)                                                        -- Close focused window
  , ((modm, xK_k), windows W.focusUp)                                           -- Move focus to the previous window
  , ((modm, xK_j), windows W.focusDown)                                         -- Move focus to the next window
  , ((modm, xK_t), withFocused $ windows . W.sink)                              -- Push window back into tiling
  ]
  ++
  [ ((0, XF86.xF86XK_AudioPlay), spawn "playerctl play-pause")                  -- Play/Pause media
  , ((0, XF86.xF86XK_AudioPause), spawn "playerctl play-pause")                 -- Play/Pause media
  , ((0, XF86.xF86XK_AudioNext), spawn "playerctl next")                        -- Jump to next media
  , ((0, XF86.xF86XK_AudioPrev), spawn "playerctl previous")                    -- Jump to previous media
  ]
  ++
  [((m .|. modm, k), windows $ IS.onCurrentScreen f i)
      | (i, k) <- zip (IS.workspaces' conf) [xK_1 .. xK_9]                      -- mod-shift-[1..9], Move client to workspace N in focused screen
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]                    -- mod-[1..9], Switch to workspace N in focused screen
  ++
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]                               -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  [((modm, xK_g), goToSelected defaultGSConfig)]                                -- GridSelect
