import XMonad
import XMonad.Core
import XMonad.Config.Gnome
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.TabBarDecoration
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import System.IO 
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Actions.Warp

-- Ignore Gnome-Do
myManageHook :: [ManageHook]
myManageHook =
  [resource =? "Do" --> doIgnore]

defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
-- remove some of the default key bindings
toRemove XConfig{modMask = modm} =
    [ (modm              , xK_h     )
    , (modm              , xK_l     )
--    , (modm              , xK_w     )
--    , (modm              , xK_e     )
--    , (modm              , xK_r     )
--    , (modm .|. shiftMask, xK_w     )
--    , (modm .|. shiftMask, xK_e     )
--    , (modm .|. shiftMask, xK_r     )
    , (modm              , xK_c     )
    ] 
-- These are my personal key bindings
toAdd conf@(XConfig{modMask = modm}) =
    [ ((modm              , xK_minus   ), sendMessage Shrink )
    , ((modm              , xK_equal   ), sendMessage Expand )
    , ((modm              , xK_y       ), kill )
    ] ++

    -- mod-{u,i,o} for workspaces 1,2,3    
    [((m .|. modm, key), windows $ f i)
        | (i, key) <- zip (XMonad.workspaces conf) [xK_u, xK_i, xK_o]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{l,;} %! Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{l,;} %! Move client to screen 1 or 2
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_l, xK_semicolon] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main = do 
  xmonad $ gnomeConfig
    { terminal  = "gnome-terminal"
    , modMask   = mod4Mask
    , focusFollowsMouse = False
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    , keys = newKeys
    }
