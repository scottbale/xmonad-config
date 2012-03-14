import XMonad
import XMonad.Core
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

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
    [ ((modm              , xK_minus     ), sendMessage Shrink )
    , ((modm              , xK_equal     ), sendMessage Expand )
    , ((modm              , xK_y         ), kill )
    , ((modm              , xK_l         ), windows toggleScreenFocus )
    , ((modm              , xK_semicolon ), windows greedyMoveWindow )
    ] ++

    -- mod-{u,i,o,p,]} for workspaces 1,2,3,4,5    
    [((m .|. modm, key), windows $ f i)
        | (i, key) <- zip (XMonad.workspaces conf) [xK_u, xK_i, xK_o, xK_p, xK_bracketleft]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- Move window to next Xinerama screen, and follow focus
greedyMoveWindow :: WindowSet->WindowSet
greedyMoveWindow = doToNextScreen greedyMoveWinFn
   where greedyMoveWinFn i ws = W.view i (W.shift i ws)

-- Toggle focus to next Xinerama screen (if more than one)
toggleScreenFocus :: WindowSet->WindowSet
toggleScreenFocus = doToNextScreen W.view

-- takes a function that operates on a workspace ID, and invokes it using the 
-- workspace ID of the next Xinerama screen.
doToNextScreen :: (WorkspaceId->WindowSet->WindowSet) -> WindowSet -> WindowSet
doToNextScreen fn ws = case maybeWid of
                  Just wid -> fn wid ws
                  Nothing -> ws
   where maybeWid = (nextXineramaWorkspaceId . W.visible) ws

nextXineramaWorkspaceId :: [W.Screen WorkspaceId l a sid sd]->Maybe WorkspaceId
nextXineramaWorkspaceId [] = Nothing -- only one monitor
nextXineramaWorkspaceId xs = Just (W.tag (W.workspace (head xs)))

main = do 
  xmonad =<< xmobar gnomeConfig
    { terminal  = "gnome-terminal"
    , modMask   = mod4Mask
    , focusFollowsMouse = False
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    , keys = newKeys
    }
