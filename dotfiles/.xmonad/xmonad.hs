import XMonad

import XMonad.Layout
import XMonad.Layout.SimpleFloat
 
layout' = tall ||| Full ||| simpleFloat
    where
        nmaster = 1
        ratio = 1/2
        delta = 3/100

        tall = Tall nmaster delta ratio

config' :: XConfig
config' = def
    {
        terminal = "alacritty",
        modMask = mod4Mask,
        layoutHook = layout',

        borderWidth = 2,
        normalBorderColor = "#404040",
        focusedBorderColor = "#ffffff"
    }

main :: IO ()
main = xmonad config'