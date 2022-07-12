import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed

import Theme

layout' = avoidStruts . spacing' $ tall ||| Grid ||| simpleTabbed
    where
        tall = Tall 1 (3 / 100) (1 / 2)

        screenBorder = 32
        windowBorder = 16
        spacing' =
            spacingRaw
            True
            (Border screenBorder screenBorder screenBorder screenBorder) True
            (Border windowBorder windowBorder windowBorder windowBorder) True

config' = def
    {
        terminal = "alacritty",
        modMask = mod4Mask,
        layoutHook = layout',

        borderWidth = 2,
        normalBorderColor = themeBackground,
        focusedBorderColor = themePrimary
    }

prettyPrint :: PP
prettyPrint = def
    {
        ppCurrent =
            xmobarBorder "Bottom" themePrimary 2
            . wrapPadding
            . colorPrimary,
        ppVisible = wrapPadding,
        ppVisibleNoWindows = Just wrapPadding,
        ppHidden = wrapPadding,
        ppHiddenNoWindows = wrapPadding,
        ppWsSep = "",

        ppLayout = colorPrimary,
        ppTitle = colorPrimary . shorten 64,

        ppOrder = \(_:layout:_:_) -> [ layout ],
        ppSep = " "
    }
        where
            wrapPadding = wrap " " " "
            colorPrimary = xmobarColor themePrimary ""

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
