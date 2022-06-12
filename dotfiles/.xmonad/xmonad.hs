import XMonad

config :: XConfig
config = def
    { terminal = "alacritty"
    , modMask = mod4Mask
    }

main :: IO ()
main = xmonad config