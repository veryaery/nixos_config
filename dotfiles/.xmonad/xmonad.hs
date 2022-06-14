import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout
import XMonad.Layout.Spacing

import Theme

layout' = avoidStruts . spacing' $ tall ||| Full
    where
        tall = Tall 1 (3 / 100) (1 / 2)

        screenBorder = 16
        windowBorder = 8
        spacing' =
            spacingRaw
            False
            (Border screenBorder screenBorder screenBorder screenBorder) True
            (Border windowBorder windowBorder windowBorder windowBorder) True

config' = def
    {
        terminal = "alacritty",
        modMask = mod4Mask,
        layoutHook = layout',

        borderWidth = 2,
        normalBorderColor = themeBackground,
        focusedBorderColor = themeForeground
    }

prettyPrint :: PP
prettyPrint = def
    {
        ppCurrent = xmobarBorder "Bottom" themePrimary 2 . wrapPadding,
        ppVisible = wrapPadding,
        ppVisibleNoWindows = Just wrapPadding,
        ppHidden = wrapPadding,
        ppHiddenNoWindows = wrapPadding,
        ppWsSep = "",

        ppOrder = \(workspaces:_) -> [ workspaces ],
        ppSep = " "
    }
        where
            wrapPadding = wrap " " " "

main :: IO ()
main =
    xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withSB statusBarConfig
    $ config'
        where
            statusBarConfig = statusBarProp "xmobar" (pure prettyPrint)