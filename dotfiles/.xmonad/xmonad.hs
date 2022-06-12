import XMonad

main :: IO ()
main = xmonad $ def
    { terminal = "alacritty"
    , modMask = mod4Mask
    }