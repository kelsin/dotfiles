--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

{-# OPTIONS_GHC -cpp #-}

{-
#include <X11/XF86keysym.h>
-}

import XMonad
import System.Exit
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.UpdatePointer
import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Magnifier
import XMonad.Layout.IM
import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Contrib
import XMonad.Actions.CycleWS

-- Gnome
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Width of the window border in pixels.
--
myBorderWidth   = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1-Emacs", "2-Screen", "3-Web", "4-Chat", "5-Oracle", "6-Icedove"] ++ map show [7..8] ++ ["9-Virt", "0-Nest"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "darkred"
myFocusedBorderColor = "orange"

-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
-- myDefaultGaps   = [(14,0,0,0),(0,0,0,0)]

-- Font
kelsinFont :: Integer -> String
kelsinFont size = "xft:DejaVu Sans Mono for Powerline:size=" ++ (show size)

-- Shell Prompt
kelsinXPConfig = defaultXPConfig { position = Top
                                 , height = 23
                                 , promptBorderWidth = 0
                                 , bgColor = "black"
                                 , fgColor = "white"
                                 , fgHLight = "orange"
                                 , bgHLight = "black"
                                 , font = kelsinFont 12
                                 }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch Shell Prompt
    , ((modMask, xK_p ), shellPrompt kelsinXPConfig)

    -- launch XMonad Prompt
    , ((modMask .|. shiftMask, xK_p     ), shellPrompt kelsinXPConfig)

    -- close focused window
    , ((modMask,               xK_w     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask .|. shiftMask, xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Goto next screen
    , ((modMask,               xK_h     ), nextScreen)

    -- Goto prev screen
    , ((modMask,               xK_l     ), prevScreen)

    -- Move window to next screen
    , ((modMask .|. shiftMask, xK_h     ), swapNextScreen)

    -- Move window to prev screen
    , ((modMask .|. shiftMask, xK_l     ), swapPrevScreen)

    -- Focus the last urgent window
    , ((modMask,               xK_u     ), focusUrgent)

    -- Move to next workspace that's empty
    , ((modMask,               xK_n     ), moveTo Next EmptyWS)

    -- Shrink the master area
    , ((modMask .|. controlMask, xK_h   ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask .|. controlMask, xK_l   ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle Struts
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    -- , ((modMask .|. shiftMask, xK_q     ), spawn "gnome-session-save --gui --kill")
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)

    -- Media keys
    , ((0, XF86XK_AudioPrev), spawn "mpc prev")
    , ((0, XF86XK_AudioNext), spawn "mpc next")
    , ((0, XF86XK_AudioPlay), spawn "mpc toggle")
    , ((0, XF86XK_AudioStop), spawn "mpc stop")
    , ((0, XF86XK_AudioLowerVolume), spawn "amixer sset Master 10%- unmute")
    , ((0, XF86XK_AudioRaiseVolume), spawn "amixer sset Master 10%+ unmute")
    , ((0, XF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((0, XF86XK_ScreenSaver), spawn "xscreensaver-command -lock")
    , ((0, XF86XK_Display), spawn "~/bin/monitors.sh")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    -- Replaced the following with h and l to move to next and prev screen
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = showWName' myShowWNameConfig $
           smartBorders $
           avoidStruts (
           onWorkspaces ["3-Web"] (Full ||| tiled) $
           onWorkspaces ["9-Virt", "0-Nest"] (noBorders Full) $
           onWorkspaces ["4-Chat"] chat $
           tiled ||| Full ||| magnifiercz 1.2 Grid ||| Circle )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = Tall nmaster delta ratio

     -- Chat layout
     chat = withIM (1%4) (Title "Buddy List") Grid

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio = 1/2

     -- Percent of screen to increment by when resizing panes
     delta = 5/100

     myShowWNameConfig = defaultSWNConfig { swn_bgcolor = "black"
                                          , swn_color = "orange"
                                          , swn_fade = 1
                                          , swn_font = kelsinFont 48
                                          }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "display" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Pidgin" --> doF (W.shift "4-Chat")
    , className =? "Liferea-bin" --> doF (W.shift "5-Rss")
    , title =? "xnest" --> doF (W.shift "0-Nest")
    , title =? "Irssi" --> doF (W.shift "4-Chat")
    , className =? "Firefox" --> doF (W.shift "3-Web")
    , className =? "Chromium" --> doF (W.shift "3-Web")
    , className =? "Iceweasel" --> doF (W.shift "3-Web")
    , className =? "Conkeror" --> doF (W.shift "3-Web")
    , className =? "epiphany-browser" --> doF (W.shift "3-Web")
    , className =? "Emacs" --> doF (W.shift "1-Emacs")
    , className =? "OracleCalendar" --> doF (W.shift "5-Oracle")
    , className =? "Icedove-bin" --> doF (W.shift "6-Icedove")
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop" --> doIgnore ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- My Printer for Dynamic Log
kelsinPP :: PP
kelsinPP = defaultPP { ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
                     , ppVisible = xmobarColor "darkred" "" . wrap "[" "]"
                     , ppHidden = id
                     , ppTitle = xmobarColor "white" "" . shorten 30
                     , ppHiddenNoWindows = const ""
                     , ppUrgent = xmobarColor "red" "" . wrap "*" "*"
                     , ppLayout = xmobarColor "green" ""
                     , ppSep = " / "
                     }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
     -- xmobar <- spawnPipe "~/.cabal/bin/xmobar"
     xmobar <- spawnPipe "/usr/bin/xmobar"
     xmonad $ defaults xmobar

myLogHook xmobar = (dynamicLogWithPP $ kelsinPP { ppOutput = hPutStrLn xmobar })
                   >> updatePointer (0.5, 0.5) (0, 0)

myStartupHook = ewmhDesktopsStartup >> setWMName "XMonad"

defaults xmobar = (withUrgencyHook NoUrgencyHook)
                  $ ewmh gnomeConfig { terminal      = myTerminal
                                , focusFollowsMouse  = myFocusFollowsMouse
                                , borderWidth        = myBorderWidth
                                , modMask            = myModMask
                                -- , numlockMask        = myNumlockMask
                                , workspaces         = myWorkspaces
                                , normalBorderColor  = myNormalBorderColor
                                , focusedBorderColor = myFocusedBorderColor
                                , manageHook         = myManageHook <+> manageDocks

                                -- key bindings
                                , keys               = myKeys
                                , mouseBindings      = myMouseBindings

                                -- hooks, layouts
                                , layoutHook         = myLayout
                                , startupHook        = myStartupHook
                                , logHook            = myLogHook xmobar
                                }
