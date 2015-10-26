import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.Run -- (spawnPipe, hPutStrLn)
import XMonad.Util.Loggers
import XMonad.Util.EZConfig(mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import System.IO
import Text.Printf

import qualified XMonad.StackSet as W
import qualified Data.Map as M


data MyTheme = MyTheme
	{ bgNormal :: String
	, bgCurrent :: String
	, fgInactive :: String
	, fgNormal :: String
	, fgCurrent :: String
	, fgAlternate :: String
	, fgHighlight :: String
	, font :: String
	}

myTheme = MyTheme
	{ bgNormal = "#303030"
	, bgCurrent = "#202020"
	, fgInactive = "#808080"
	, fgNormal = "#c0c0c0"
	, fgCurrent = "#30c030"
	, fgAlternate = "#c07730"
	, fgHighlight = "#c03030"
	, font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*-*"
	}


myTerminal = "urxvt"

myModMask = mod4Mask
myModMaskP = "M4-"

myWorkspaces = ["1", "2", "3", "q", "w", "e", "s"]


scratchpads =
	[ NS "htop" (myTerminal ++ " -e htop") (title =? "htop") big
	, NS "scratch" (myTerminal ++ " -name scratch") (appName =? "scratch") big
	, NS "cmus" (myTerminal ++ " -name cmus -e cmus") (appName =? "cmus") big
	{-, NS "mixer" (myTerminal ++ " -name mixer -e alsamixer") (appName =? "mixer") big-}
	, NS "mixer" (myTerminal ++ " -name mixer -e pacmixer") (appName =? "mixer") big
	-- , NS "agenda" (myTerminal ++ " -name agenda -e vim ~/agenda.txt") (appName =? "agenda") big
	-- , NS "psi" "psi-plus" (className =? "Psi-plus") huge
	, NS "transmission" "transmission-qt" (title =? "Transmission") huge
	, NS "odesk" "odeskteam-qt4" (title =? "oDesk Team") small
	, NS "upwork" "upwork" (title =? "Upwork") small
	] where
		big = customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)
		huge = customFloating $ W.RationalRect 0 (1/73) 1 (72/73)
		small = customFloating $ W.RationalRect (2/5) (1/5) (1/5) (1/5)


dmenuOpts :: String
dmenuOpts = printf "-fn %s -nb '%s' -nf '%s' -sb '%s' -sf '%s'" (font myTheme) (bgNormal myTheme) (fgNormal myTheme) (bgCurrent myTheme) (fgCurrent myTheme)

myKeys = \conf -> mkKeymap conf $ map (\(k, f) -> (myModMaskP ++ k, f)) $
	[ ("S-<Return>", spawn $ XMonad.terminal conf) -- launch a terminal
	, ("r", spawn $ "dmenu_run " ++ dmenuOpts) -- launch dmenu

	, ("S-c", kill) -- close focused window
	, ("<Space>", sendMessage NextLayout) -- cycle layout algorithms
	, ("S-<Space>", setLayout $ XMonad.layoutHook conf) -- reset layout
	, ("n", refresh) -- resize viewed windows to the correct size
	, ("<Tab>", windows W.focusDown) -- next window
	, ("j", windows W.focusDown) -- next window
	, ("k", windows W.focusUp) -- prev window
	, ("m", windows W.focusMaster) -- focus master window
	, ("<Return>", windows W.shiftMaster) -- swap focused w/ master
	, ("S-j", windows W.swapDown) -- swap focused w/ next
	, ("S-k", windows W.swapUp) -- swap focused w/ prev
	, ("h", sendMessage Shrink) -- shrink the master area
	, ("l", sendMessage Expand) -- expand the master area
	, ("t", withFocused $ windows . W.sink) -- push window back into tiling
	, (",", sendMessage $ IncMasterN 1) -- increment the number of windows in the master area
	, (".", sendMessage $ IncMasterN (-1)) -- decrement the number of windows in the master area
	, ("S-b", sendMessage ToggleStruts) -- toggle the status bar gap

	, ("C-q", io $ exitWith ExitSuccess) -- quit
	, ("S-r", spawn "xmonad --restart") -- restart
	, ("C-r", spawn "xmonad --recompile && xmonad --restart") -- recompile and restart

	-- unmount devices
	, ("] u", spawn $ "unmount " ++ dmenuOpts)

	-- cmus
	, ("z", spawn "cmus-remote -r")
	, ("x", spawn "cmus-remote -p")
	, ("c", spawn "cmus-remote -u")
	, ("v", spawn "cmus-remote -s")
	, ("b", spawn "cmus-remote -n")

	-- master volume
	, ("-", spawn "amixer set Master playback 1-")
	, ("=", spawn "amixer set Master playback 1+")

	-- scratchpads
	, ("`", namedScratchpadAction scratchpads "scratch")
	, ("C-<Delete>", namedScratchpadAction scratchpads "htop")
	, ("p", namedScratchpadAction scratchpads "cmus")
	, ("S-m", namedScratchpadAction scratchpads "mixer")
	, ("S-t", namedScratchpadAction scratchpads "transmission")
	, ("C-t", namedScratchpadAction scratchpads "odesk")
	, ("C-y", namedScratchpadAction scratchpads "upwork")

	, ("<Print>", spawn "scrot") -- scrot
	]
	++
	-- workspaces
	[(mask ++ key, windows $ action workspace)
		| (workspace, key) <- zip (XMonad.workspaces conf) myWorkspaces
		, (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
	]
	-- screens
	++
	[(mask ++ key, screenWorkspace screen >>= flip whenJust (windows . action))
		| (key, screen) <- zip ["<F1>", "<F2>"] [0..]
		, (action, mask) <- [(W.view, ""), (W.shift, "S-")]
	]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
	-- mod-left: set the window to floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
	-- mod-middle: raise the window to the top of the stack
	, ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
	-- mod-right: set the window to floating mode and resize by dragging
	, ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
	]


myLayout = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| noBorders Full
	where
		tiled = Tall nmaster delta ratio
		nmaster = 1 -- default number of windows in the master pane
		ratio = 6/10 -- default proportion of screen occupied by master pane
		delta = 2/100 -- percent of screen to increment by when resizing panes


myManageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> composeAll
	[ className =? "MPlayer" --> doFloat
	, className =? "mplayer2" --> doFloat
	, className =? "Vlc" --> doFloat
	, appName =? "XXkb" --> doIgnore
	, appName =? "desktop_window" --> doIgnore
	, appName =? "kdesktop" --> doIgnore
	-- , appName =? "gens" --> doFloat
	-- , className =? "Exe" --> doIgnore
	, className =? "Plugin-container" --> doIgnore
	, className =? "Xmessage" --> doFloat
	{-, className =? "Chromium" --> doShift "e"-}
	, className =? "Chromium" --> doFloat
	, title =? "Chromium Preferences" --> doFloat
	, className =? "Firefox" --> doShift "e"
	, title =? "Firefox Preferences" --> doFloat
	, className =? "feh" --> doFullFloat
	, (appName =? "event" <&&> className =? "psi") --> doFloat
	, className =? "psi+" --> doShift "s"
	, className =? "Psi-plus" --> doShift "s"
	-- , title =? "agenda" --> doFloat
	, className =? "Odeskteam-qt4" --> doFloat
	{-, title =? "upwork" --> doShift "NSP"-}
	, className =? "Upwork" --> doFloat
	, className =? "Qjackctl" --> doFloat
	] <+> manageHook defaultConfig


-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.
myStartupHook = do
	spawn "conky -t '$cpu' | ~/.dzen/graph.py -rOo ~/.dzen/cpugraph.out"
	dynStatusBarStartup mySB mySBCleanup
	return ()


mySB :: DynamicStatusBar
mySB screen = do
	h <- spawnPipe $ printf "%s -e - -w 1000 -ta l" (dzenCmd screen)
	-- TODO: store pids in mvars and kill procs on cleanup
	spawn $ printf "conky | %s -x 1000 -tw 900 -ta r -sa r -l 1 -e 'button1=togglecollapse;'" (dzenCmd screen)
	spawn $ printf "(xkb-switch && xkb-switch -W) | sed -u 's#.*#^i(/home/nl/icons/lang/&.xpm)#' | %s -x 1900 -w 20 -e -" (dzenCmd screen)
	return h

mySBCleanup :: DynamicStatusBarCleanup
mySBCleanup = do
	spawn "killall xkb-switch"
	return ()

-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = dynStatusBarEventHook mySB mySBCleanup


-- Status bars and logging
myLayoutIcon clr = dzenColor clr "" . getIcon
	where getIcon layout = printf "^i(/home/nl/icons/layouts/%s)" icon
		where icon = case layout of
			"Tall" -> "tall.xbm"
			"Mirror Tall" -> "tall_mirr.xbm"
			"Full" -> "full.xbm"

myPPTitle _ "" = ""
myPPTitle clr title = dzenColor clr "" $ " " ++ shorten 120 title

myFocusPP = defaultPP
	{ ppCurrent = dzenColor (fgCurrent myTheme) (bgCurrent myTheme) . pad
	, ppVisible = wrap "<" ">"
	, ppHidden = dzenColor (fgNormal myTheme) "" . pad
	, ppHiddenNoWindows = dzenColor (fgInactive myTheme) "" . pad
	, ppUrgent = dzenColor (fgHighlight myTheme) "" . pad
	, ppSep = " "
	, ppWsSep = ""
	, ppTitle = myPPTitle (fgCurrent myTheme)
	, ppLayout = myLayoutIcon (fgAlternate myTheme)
	, ppSort = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
	}

myUnfocusPP = myFocusPP
	{ ppCurrent = dzenColor (fgNormal myTheme) (bgCurrent myTheme) . pad
	, ppHidden = dzenColor (fgNormal myTheme) "" . pad
	, ppHiddenNoWindows = dzenColor (fgInactive myTheme) "" . pad
	, ppUrgent = dzenColor (fgHighlight myTheme) "" . pad
	, ppTitle = myPPTitle (fgNormal myTheme)
	, ppLayout = myLayoutIcon (fgNormal myTheme)
	}


dzenCmd :: ScreenId -> String
dzenCmd (S screenN) = printf "dzen2 -xs %d -bg '%s' -fg '%s' -fn '%s'" (screenN + 1) (bgNormal myTheme) (fgNormal myTheme) (font myTheme)


-- running it
main = do
	xmonad defaultConfig
		{ terminal = myTerminal
		, focusFollowsMouse = True
		, borderWidth = 1
		, modMask = myModMask
		, workspaces = myWorkspaces
		, normalBorderColor = bgCurrent myTheme
		, focusedBorderColor = fgCurrent myTheme

		, keys = myKeys
		, mouseBindings = myMouseBindings

		, layoutHook = myLayout
		, manageHook = myManageHook
		, handleEventHook = myEventHook
		, logHook = multiPP myFocusPP myUnfocusPP
		, startupHook = myStartupHook
		}
