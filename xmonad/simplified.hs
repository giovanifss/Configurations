import System.IO
import qualified Data.Map as M

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.NamedWindows
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.Layout.IndependentScreens as IS

main = do
  nScreens <- IS.countScreens
  xmonad $ docks
         $ desktopConfig
    { manageHook = manageDocks <+> manageScratchpad <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , startupHook = spawn "xmodmap -e 'keysym Menu = Super_R'"  -- Make xk_menu key be xk_super
    , terminal              = myTerminal
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , normalBorderColor     = black
    , focusedBorderColor    = darkred
    , workspaces            = IS.withScreens nScreens (map show [1..9])
    , keys                  = myKeys
    }

-- Usability --
myTerminal    = "xterm"
myModMask     = mod4Mask      -- Win key or Super_L
myBorderWidth = 2
myLauncher    = "exe=`dmenu_path | dmenu` && eval \"exec $exe\"" 
myLockScreen  = "xlock"

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
darkblue    = "#002436"

-- Scratchpad stuff --
termScratch = "terminal-scratchpad"                               -- Scratchpad terminal identifier
termScratchRect = W.RationalRect leftEdge topEdge width height    -- Scratchpad terminal window size/location
  where height    = 0.7
        width     = 0.8
        topEdge   = 0.1
        leftEdge  = 0.1

manageScratchpad :: ManageHook
manageScratchpad = namedScratchpadManageHook scratchpads

scratchpads :: NamedScratchpads
scratchpads =
  [ NS termScratch (myTerminal ++ " -T " ++ termScratch) (title =? termScratch) (customFloating termScratchRect) ]

-- Keybindings --
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, xK_c), kill)                                                        -- Close focused window
  , ((modm, xK_p), spawn myLauncher)                                            -- Launch launcher
  , ((modm, xK_backslash), spawn myLockScreen)                                  -- Lock screen
  , ((modm, xK_Return), spawn $ XMonad.terminal conf)]                          -- Launch terminal
  ++
  [ ((modm, xK_h), sendMessage Shrink)                                          -- Shrink the master area
  , ((modm, xK_l), sendMessage Expand)                                          -- Expand the master area
  , ((modm, xK_k), windows W.focusUp)                                           -- Move focus to the previous window
  , ((modm, xK_j), windows W.focusDown)                                         -- Move focus to the next window
  , ((modm, xK_t), withFocused $ windows . W.sink)                              -- Push window back into tiling
  , ((modm, xK_comma), sendMessage (IncMasterN 1))                              -- Increment the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))                          -- Deincrement the number of windows in the master area
  , ((modm .|. controlMask, xK_k), windows W.swapUp)                            -- Swap the focused window with the previous window.
  , ((modm .|. controlMask, xK_j), windows W.swapDown)                          -- Swap the focused window with the next window.
  , ((modm .|. controlMask, xK_m), windows W.swapMaster)]                       -- Swap the focused window and the master window
  ++
  [ ((0, xK_F7), namedScratchpadAction scratchpads termScratch)]                -- Launch scratchpad terminal
  ++
  [((m .|. modm, k), windows $ IS.onCurrentScreen f i)
      | (i, k) <- zip (IS.workspaces' conf) [xK_1 .. xK_9]                      -- mod-shift-[1..9], Move client to workspace N in focused screen
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]                    -- mod-[1..9], Switch to workspace N in focused screen
  ++
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]                               -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
