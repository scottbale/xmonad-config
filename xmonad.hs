import XMonad
import XMonad.Core
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Graphics.X11.Types as X

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
    , (modm              , xK_p     )
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
--    , ((modm              , xK_h         ), focusUrgent )
    , ((modm              , xK_h         ), windows uberFocusDown )
    , ((modm              , xK_l         ), windows toggleScreenFocus )
    , ((modm              , xK_semicolon ), windows greedyMoveWindow )
    ] ++

    -- mod-{u,i,o,p,[,]} for workspaces 1,2,3,4,5,6
    [((m .|. modm, key), windows $ f i)
        | (i, key) <- zip (XMonad.workspaces conf) [xK_u, xK_i, xK_o, xK_p, xK_bracketleft, xK_bracketright]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- Toggle focus between all windows of current screen, then toggle screen focus; repeat

uberFocusUp :: WindowSet->WindowSet
uberFocusUp = uberFocus W.up W.focusUp'

uberFocusDown :: WindowSet->WindowSet
uberFocusDown = uberFocus W.down W.focusDown'

uberFocus :: (W.Stack Window -> [Window])->(W.Stack Window -> W.Stack Window)->WindowSet->WindowSet
uberFocus stackFn stackFocusFn ws = case (moFocus stackFn ws) of
          True -> W.modify' stackFocusFn ws
          False -> W.modify' stackFocusFn . toggleScreenFocus $ ws

moFocus :: (W.Stack Window -> [Window])->WindowSet->Bool
moFocus stackFn ws = case currentStack of
        Just s -> not $ null $ stackFn s
        Nothing -> False
   where currentStack = W.stack . W.workspace . W.current $ ws

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

main = xmonad myConfig 

  -- =<< xmobar (withUrgencyHook NoUrgencyHook $ myConfig)

myConfig = gnomeConfig
    { 
      modMask   = mod4Mask
    , layoutHook = smartBorders (layoutHook gnomeConfig)
    , focusFollowsMouse = False
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    , keys = newKeys
    , startupHook = setWMName "LG3D"
    }

-- testing ------------------------------------------------------------------------------

testWS :: WindowSet
testWS = foldr W.insertUp ws testWindows
  where ws = W.new (Layout Full) ["0", "1", "2", "3"] [SD $ Rectangle 0 1 2 3, SD $ Rectangle 2 3 4 5]

testWindows :: [X.Window]
testWindows = [22, 33, 44]

