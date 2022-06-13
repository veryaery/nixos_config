import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout
import XMonad.Layout.Spacing

layout' :: Layout
layout' = spacing' $ tall ||| Full
    where
        screenBorder = 16
        windowBorder = 8
        spacing' =
            spacingRaw
            True
            (Border screenBorder screenBorder screenBorder screenBorder)
            True
            (Border windowBorder windowBorder windowBorder windowBorder)
            True
        tall = Tall 1 (3 / 100) (1 / 2)

config' :: XConfig Layout
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
main = xmonad
    . emwhFullscreen
    . emwh
    . xmobarProp
    $ config'