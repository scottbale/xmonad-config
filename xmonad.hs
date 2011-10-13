import XMonad
import XMonad.Config.Gnome

-- Ignore Gnome-Do
myManageHook :: [ManageHook]
myManageHook =
  [resource =? "Do" --> doIgnore]

main = do 
  xmonad $ gnomeConfig
    { terminal  = "gnome-terminal"
    , modMask   = mod4Mask
    , focusFollowsMouse = False
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    }
