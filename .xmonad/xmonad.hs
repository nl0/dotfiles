
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

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
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import System.IO
import Text.Printf

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
{-myFocusFollowsMouse = True-}
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1", "2", "3", "q", "w", "e", "s"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#303030"
myFocusedBorderColor = "#30c030"

scratchpads =
	[ NS "htop" (myTerminal ++ " -e htop") (title =? "htop") big
	, NS "scratch" (myTerminal ++ " -name scratch") (appName =? "scratch") big
	, NS "cmus" (myTerminal ++ " -name cmus -e cmus") (appName =? "cmus") big
	, NS "mixer" (myTerminal ++ " -name mixer -e alsamixer") (appName =? "mixer") big
	, NS "agenda" (myTerminal ++ " -name agenda -e vim ~/agenda.txt") (appName =? "agenda") big
	] where
		big = customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_r     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next/prev window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.shiftMaster)
    -- Swap the focused window with the next/prev window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink/expand the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment/decrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm .|. shiftMask, xK_b     ), sendMessage ToggleStruts)
    -- quit xmonad
    , ((modm .|. controlMask, xK_q   ), io (exitWith ExitSuccess))
    -- restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "killall conky xxkb; xmonad --restart")
    -- recompile and restart xmonad
    , ((modm .|. controlMask, xK_r   ), spawn "killall conky xxkb; xmonad --recompile; xmonad --restart")
		-- cmus controls
		, ((modm              , xK_z     ), spawn "cmus-remote -r")
		, ((modm              , xK_x     ), spawn "cmus-remote -p")
		, ((modm              , xK_c     ), spawn "cmus-remote -u")
		, ((modm              , xK_v     ), spawn "cmus-remote -s")
		, ((modm              , xK_b     ), spawn "cmus-remote -n")
		-- alsa volume controls
		, ((modm              , xK_minus ), spawn "amixer set Master playback 1-")
		, ((modm              , xK_equal ), spawn "amixer set Master playback 1+")
		-- scratchpads
		, ((modm              , xK_grave ), namedScratchpadAction scratchpads "scratch")
		, ((mod1Mask .|. controlMask, xK_Delete ), namedScratchpadAction scratchpads "htop")
		, ((modm              , xK_p     ), namedScratchpadAction scratchpads "cmus")
		, ((modm .|. shiftMask, xK_m     ), namedScratchpadAction scratchpads "mixer")
		, ((modm              , xK_a     ), namedScratchpadAction scratchpads "agenda")
		-- scrot
		, ((modm              , xK_Print ), spawn "scrot")
    ]
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    ++
    [((m .|. modm, k), windows $ f i)
        -- | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_q, xK_w, xK_e, xK_s]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    {-++-}
    {-[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))-}
        {-| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]-}
        {-, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]-}

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
myLayout = smartBorders ( avoidStruts ( tiled ||| Mirror tiled ||| Full ) )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 2/100

------------------------------------------------------------------------
-- Window rules:
myManageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> composeAll
	[ className =? "MPlayer"        --> doFloat
	, className =? "Vlc"            --> doFloat
	, appName   =? "XXkb"           --> doIgnore
	, appName   =? "desktop_window" --> doIgnore
	, appName   =? "kdesktop"       --> doIgnore
	, appName   =? "gens"           --> doFloat
	, className =? "Exe"            --> doIgnore
	, className =? "Plugin-container" --> doIgnore
	, className =? "Xmessage"       --> doFloat
	, className =? "Chromium"       --> doShift "e"
	, title     =? "Chromium Preferences" --> doFloat
	, className =? "Firefox"        --> doShift "e"
	, className =? "psi"            --> doShift "s"
	, className =? "Psi-plus"       --> doShift "s"
	, className =? "feh"            --> doFullFloat
	, (appName =? "event" <&&> className =? "psi") --> doFloat
	, title     =? "agenda"         --> doFloat
	{-, appName =? "Gemini_Rue.exe"   --> doIgnore-}
	] <+> manageHook defaultConfig

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- colors
bg_dark = "#202020"
bg_light = "#303030"
fg_dark = "#808080"
fg_light = "#c0c0c0"
green = "#30c030"
red = "#c03030"
orange = "#c07730"

myLayoutIcon layout = dzenColor orange "" (icon layout)
	where
		icon layout =
			"^i(/home/nl/icons/layouts/" ++
			(\x -> case x of
				"Tall"            -> "tall.xbm"
				"Mirror Tall"     -> "tall_mirr.xbm"
				"Full"            -> "full.xbm"
			) layout ++ ")"

myPPTitle "" = ""
myPPTitle title = dzenColor green "" (" " ++ shorten 64 title)

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
myFont = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*-*"
dzenCmd = printf "dzen2 -bg '%s' -fg '%s' -fn '%s'" bg_light fg_light myFont

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- running it
main = do
	h <- spawnPipe (dzenCmd ++ " -e - -w 600 -ta l")
	spawn ("conky | " ++ dzenCmd ++ " -x 600 -tw 680 -ta r -sa r -l 1" ++
		" -e 'button1=togglecollapse;'")
	spawn "conky -t '$cpu' | ~/.dzen/graph.py -rOo ~/.dzen/cpugraph.out"

	spawn "sleep 0.2; xxkb"

	xmonad defaultConfig {
		-- simple stuff
		terminal           = myTerminal,
		focusFollowsMouse  = myFocusFollowsMouse,
		borderWidth        = myBorderWidth,
		modMask            = myModMask,
		workspaces         = myWorkspaces,
		normalBorderColor  = myNormalBorderColor,
		focusedBorderColor = myFocusedBorderColor,

		-- key bindings
		keys               = myKeys,
		mouseBindings      = myMouseBindings,

		-- hooks, layouts
		layoutHook         = myLayout,
		manageHook         = myManageHook,
		handleEventHook    = myEventHook,
		logHook            = myLogHook h,
		startupHook        = myStartupHook
	}

