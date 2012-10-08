import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
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


-- colors
bg_dark = "#202020"
bg_light = "#303030"
fg_dark = "#808080"
fg_light = "#c0c0c0"
green = "#30c030"
red = "#c03030"
orange = "#c07730"

myFont = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*-*"

myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 1

myModMask = mod4Mask
myModMaskP = "M4-"

myWorkspaces = ["1", "2", "3", "q", "w", "e", "s"]

myNormalBorderColor  = bg_dark
myFocusedBorderColor = green


scratchpads =
	[ NS "htop" (myTerminal ++ " -e htop") (title =? "htop") big
	, NS "scratch" (myTerminal ++ " -name scratch") (appName =? "scratch") big
	, NS "cmus" (myTerminal ++ " -name cmus -e cmus") (appName =? "cmus") big
	, NS "mixer" (myTerminal ++ " -name mixer -e alsamixer") (appName =? "mixer") big
	-- , NS "agenda" (myTerminal ++ " -name agenda -e vim ~/agenda.txt") (appName =? "agenda") big
	-- , NS "psi" "psi-plus" (className =? "Psi-plus") huge
	] where
		big = customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)
		huge = customFloating $ W.RationalRect 0 (1/73) 1 (72/73)


myKeys = \conf -> mkKeymap conf $ map (\(k, f) -> (myModMaskP ++ k, f)) $
	[ ("S-<Return>", spawn $ XMonad.terminal conf) -- launch a terminal
	, ("r", spawn $ "dmenu_run -fn " ++ myFont) -- launch dmenu

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
	, ("S-r", spawn "killall conky xxkb; xmonad --restart") -- restart
	, ("C-r", spawn "killall conky xxkb; xmonad --recompile && xmonad --restart") -- recompile and restart

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

	, ("<Print>", spawn "scrot") -- scrot
	]
	++
	-- workspaces
	[(mask ++ key, windows $ action workspace)
		| (workspace, key) <- zip (XMonad.workspaces conf) myWorkspaces
		, (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
	]
	-- screens
	{-
	++
	[(mask ++ key, screenWorkspace screen >>= flip whenJust (windows . action))
		| (key, screen) <- zip ["<F1>", "<F2>"] [0..]
		, (action, mask) <- [(W.view, ""), (W.shift, "S-")]
	]
	-}


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
	-- mod-left: set the window to floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
	-- mod-middle: raise the window to the top of the stack
	, ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
	-- mod-right: set the window to floating mode and resize by dragging
	, ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
	]


myLayout = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full
	where
		tiled = Tall nmaster delta ratio
		nmaster = 1 -- default number of windows in the master pane
		ratio = 6/10 -- default proportion of screen occupied by master pane
		delta = 2/100 -- percent of screen to increment by when resizing panes


myManageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> composeAll
	[ className =? "MPlayer" --> doFloat
	, className =? "Vlc" --> doFloat
	, appName =? "XXkb" --> doIgnore
	, appName =? "desktop_window" --> doIgnore
	, appName =? "kdesktop" --> doIgnore
	-- , appName =? "gens" --> doFloat
	-- , className =? "Exe" --> doIgnore
	, className =? "Plugin-container" --> doIgnore
	, className =? "Xmessage" --> doFloat
	, className =? "Chromium" --> doShift "e"
	, title =? "Chromium Preferences" --> doFloat
	, className =? "Firefox" --> doShift "e"
	, title =? "Firefox Preferences" --> doFloat
	, className =? "feh" --> doFullFloat
	, (appName =? "event" <&&> className =? "psi") --> doFloat
	, className =? "Psi-plus" --> doShift "s"
	, className =? "psi" --> doShift "s"
	-- , title =? "agenda" --> doFloat
	] <+> manageHook defaultConfig


-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = mempty


-- Status bars and logging
myLayoutIcon = dzenColor orange "" . getIcon
	where getIcon layout = "^i(/home/nl/icons/layouts/" ++ icon ++ ")"
		where icon = case layout of
			"Tall" -> "tall.xbm"
			"Mirror Tall" -> "tall_mirr.xbm"
			"Full" -> "full.xbm"

myPPTitle "" = ""
myPPTitle title = dzenColor green "" $ " " ++ shorten 120 title

myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppCurrent  = dzenColor green bg_dark . pad
	, ppHidden   = dzenColor fg_light "" . pad
	, ppHiddenNoWindows = dzenColor fg_dark  "" . pad
	, ppUrgent   = dzenColor red "" . pad
	, ppWsSep    = ""
	, ppSep      = " "
	, ppTitle    = myPPTitle
	, ppLayout   = myLayoutIcon
	, ppOutput   = hPutStrLn h
	, ppSort     = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
	}

-- statusbars
dzenCmd = printf "dzen2 -bg '%s' -fg '%s' -fn '%s'" bg_light fg_light myFont


-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.
myStartupHook = return ()


-- running it
main = do
	h <- spawnPipe (dzenCmd ++ " -e - -w 1000 -ta l")
	spawn ("conky | " ++ dzenCmd ++ " -x 1000 -tw 920 -ta r -sa r -l 1" ++
		" -e 'button1=togglecollapse;'")
	spawn "conky -t '$cpu' | ~/.dzen/graph.py -rOo ~/.dzen/cpugraph.out"

	spawn "sleep 1; xxkb"

	xmonad defaultConfig {
		-- simple stuff
		terminal = myTerminal,
		focusFollowsMouse = myFocusFollowsMouse,
		borderWidth = myBorderWidth,
		modMask = myModMask,
		workspaces = myWorkspaces,
		normalBorderColor = myNormalBorderColor,
		focusedBorderColor = myFocusedBorderColor,

		-- key bindings
		keys = myKeys,
		mouseBindings = myMouseBindings,

		-- hooks, layouts
		layoutHook = myLayout,
		manageHook = myManageHook,
		handleEventHook = myEventHook,
		logHook = myLogHook h,
		startupHook = myStartupHook
	}

