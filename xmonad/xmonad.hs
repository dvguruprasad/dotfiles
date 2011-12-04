import System.IO
import System.Exit
import Control.Monad
import List
import Control.Concurrent
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.WindowArranger
import XMonad.Layout.Master
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers
import WorkspaceCompareUpgrade
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Window
import XMonad.Prompt.AppendFile
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog

bigXPConfig = defaultXPConfig
	{ font = "xft:terminal-50"
	, fgColor = "#a8f7a3"
	, bgColor = "#3f3c6d"
	, fgHLight = "#3fc1c32"
	, bgHLight = "#8BA314"
  , promptBorderWidth = 0
	, borderColor =  "#3f3c6d"
	, height = 100
	}

smallXPConfig = bigXPConfig
 {
  font = "xft:terminal-8"
  , height = 20
  }

scratchpads = [
     NS "notes" "gvim --role notes -c 'set autoread' -c'set wrap' -c 'au FocusLost * :wa' -c 'colorscheme slate' -c 'Note'" (role =? "notes")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
 ] where role = stringProperty "WM_WINDOW_ROLE"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. controlMask, xK_l     ), spawn "gnome-screensaver-command -l")
    , ((controlMask, xK_space     ), spawn "gnome-do")
    , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)

    , ((modMask .|. shiftMask, xK_c     ), kill1)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_f ), sendMessage $ Toggle "Full")
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask,               xK_Tab   ), windows W.swapDown)
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask,               xK_bracketleft     ), sendMessage Shrink)
    , ((modMask,               xK_bracketright     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask .|. shiftMask, xK_g     ), windowPromptGoto smallXPConfig { autoComplete = Just 500000 } )
    , ((modMask .|. shiftMask, xK_b     ), windowPromptBring  smallXPConfig)
    -- , ((modMask, xK_s), sendMessage ToggleStruts)
    , ((modMask              , xK_BackSpace), focusUrgent)
    , ((modMask .|. controlMask, xK_y), defaultCommands >>= runCommand)
    , ((modMask,                 xK_Right), sendMessage $ Go R)
    , ((modMask,                 xK_Left), sendMessage $ Go L)
    , ((modMask,                 xK_Up), sendMessage $ Go U)
    , ((modMask,                 xK_Down), sendMessage $ Go D)
    , ((modMask,                 xK_l), sendMessage $ Go R)
    , ((modMask,                 xK_h), sendMessage $ Go L)
    , ((modMask,                 xK_k), sendMessage $ Go U)
    , ((modMask,                 xK_j), sendMessage $ Go D)
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_Left), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_Up), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_Down), sendMessage $ Swap D)
    , ((modMask .|. shiftMask, xK_l), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_h), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_k), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_j), sendMessage $ Swap D)

    , ((modMask, xK_i), SM.submap . M.fromList $
            [((modMask, xK_n), namedScratchpadAction scratchpads "notes")])
    , ((modMask, xK_o), SM.submap . M.fromList $
            [ ((modMask, xK_e), spawn "gvim")
            , ((modMask, xK_b), spawn "google-chrome")
            , ((modMask, xK_v), spawn "vlc")
            , ((modMask, xK_t), spawn $ XMonad.terminal conf)
            , ((modMask, xK_f), spawn "nautilus")
            ])
    , ((modMask, xK_a), SM.submap . M.fromList $
            [ ((modMask, xK_n), appendFilePrompt smallXPConfig "~/Dropbox/notes/Everything")
            ])
    , ((modMask, xK_d), SM.submap . M.fromList $
            zip (zip (repeat (modMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial W.greedyView) ['a'..'z'])
            ++
            zip (zip (repeat (modMask .|. shiftMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial (liftM2 (.) W.view W.shift)) ['a'..'z'])
            ++
            zip (zip (repeat (modMask .|. controlMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial (liftM2 (.) W.view copy)) ['a'..'z'])
            ++
            [ 
            ((modMask, xK_space), renameWorkspace bigXPConfig)
            , ((modMask, xK_BackSpace), removeWorkspace)
            , ((modMask, xK_Return), toggleWS)
            , ((modMask .|. shiftMask, xK_Return), shiftToPreviousWorkspace)
            ]
      )
    , ((modMask, xK_s), SM.submap . M.fromList $
            [
            ((modMask, xK_Return), swapNextScreen),
            ((modMask, xK_Left), viewScreen 0),
            ((modMask, xK_Right), viewScreen 1),
            ((modMask, xK_Up), viewScreen 0),
            ((modMask, xK_Down), viewScreen 1),
            ((modMask .|. shiftMask, xK_Left), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Right), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_Up), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Down), do
             sendToScreen 1
             sendToScreen 1),
            ((modMask, xK_h), viewScreen 0),
            ((modMask, xK_l), viewScreen 1),
            ((modMask, xK_k), viewScreen 0),
            ((modMask, xK_j), viewScreen 1),
            ((modMask .|. shiftMask, xK_h), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_l), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_k), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_j), do
             sendToScreen 1
             sendToScreen 1)
            ]
            )
            ]

withWorkspaceByInitial job fstLet =
  do ws <- gets (map W.tag . W.workspaces . windowset)
     case find (\w -> fstLet == w !! 0) ws of
              Just w -> windows $ job w
              Nothing -> promptWorkspaceNameAndJob fstLet job

promptWorkspaceNameAndJob fstLet job = minimalPrompt bigXPConfig {defaultText = [fstLet]} ?+ selectOrAddWorkspaceAndJob job

selectOrAddWorkspaceAndJob job name = do s <- gets windowset
                                         if W.tagMember name s
                                           then windows $ job name
                                           else addWorkspaceAndJob job name
                                         return ()

addWorkspaceAndJob job name = do addHiddenWorkspace name
                                 windows $ job name
                                 return ()

selectOrAddWorkspaceAndMoveThere = selectOrAddWorkspaceAndJob W.greedyView
data MinimalPrompt = MinimalPrompt String

instance XPrompt MinimalPrompt where
  showXPrompt (MinimalPrompt s) = ""

minimalPrompt c = mkXPromptWithReturn (MinimalPrompt undefined) c (const (return [])) return

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

defaultLayout = layoutHintsToCenter (tiled)
            ||| layoutHintsToCenter (Mirror tiled)
  where
     tiled  = Tall 1 (3 / 100) (1 / 2)

myLayout = toggleLayouts Full $ workspaceDir "" $ windowNavigation $ avoidStruts
        $ onWorkspace "im" (withIM (1%7) (Role "buddy_list") defaultLayout)
        $ onWorkspace "terminal" (workspaceDir "~/openstack/melange" defaultLayout)
        $ onWorkspace "code" (workspaceDir "~/openstack/melange" defaultLayout)
        $ onWorkspace "wall" defaultLayout
        $ defaultLayout


myManageHook = composeAll .concat $ [[namedScratchpadManageHook scratchpads, manageDocks], [className =? "Do" --> doIgnore ]]

main = xmonad $ withUrgencyHook dzenUrgencyHook {duration = 2000, args = ["-bg", "#3f3c6d", "-fg", "#a8f7a3", "-xs", "1"] } $ defaultConfig {
        focusFollowsMouse  = True,
        terminal  = "konsole",
        modMask            = mod4Mask,
        workspaces         = ["mail", "im"],
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        focusedBorderColor = "#00FF00",
        normalBorderColor = "#000000"
        , borderWidth        = 3
        , manageHook         = manageHook defaultConfig <+> myManageHook
        , logHook            = do
                       dynamicLogWithPP dzenPP2
                       updatePointer (Relative 0.5 0.5)
        , startupHook        = do
                  startupHook defaultConfig
                  spawn "killall trayer; trayer --edge top --align right --widthtype request --SetDockType true"
                  spawn "killall nm-applet; nm-applet"
                  spawn "killall gnome-sound-applet; gnome-sound-applet"
                  spawn "killall gnome-power-manager; gnome-power-manager"
                  spawn "killall xflux; ~/xflux  -l 12.9833 -g 77.5833"
                  spawn "pidgin"
                  spawn "killall parcellite; parcellite"
        , layoutHook         = windowArrange $ smartBorders $ myLayout
  }


dzenPP2 = defaultPP { ppHidden  = dzenColor "#a8f7a3"  "#3f3c6d". pad
                     , ppCurrent  = dzenColor "#3f3c6d" "#a8f7a3" . pad
                     , ppVisible  = dzenColor "#3f3c6d" "#a8f7a3" . pad
                     , ppHiddenNoWindows = const ""
                     , ppUrgent   = dzenColor "red" "yellow" . dzenStrip
                     , ppWsSep    = "|"
                     , ppOrder = \(ws:l:t:e:[]) -> [e, l, ws]
                     , ppSort = fmap (namedScratchpadFilterOutWorkspace.) getSortByXineramaPhysicalRule
                     , ppSep      = "          "
                     , ppLayout   = const ""
                     , ppTitle   = const ""
                     , ppExtras = [ dzenColorL "green" "blue" $ date "%H:%M %a %b %d"] 
                     }


shiftToPreviousWorkspace = do
    hs <- gets (W.hidden . windowset)
    unless (null hs) (windows . (liftM2 (.) W.view W.shift) . W.tag $ head hs)
