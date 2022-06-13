import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout
import XMonad.Layout.Spacing

import XMonad.Util.Loggers

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
        normalBorderColor = "#404040",
        focusedBorderColor = "#ffffff"
    }

prettyPrint :: PP
prettyPrint = def
    {
        ppCurrent = xmobarBorder "Top" "#ffffff" 2 . wrapPadding,
        ppVisible = wrapPadding,
        ppVisibleNoWindows = Just wrapPadding,
        ppHidden = wrapPadding,
        ppHiddenNoWindows = wrapPadding,
        ppWsSep = "",

        ppExtras = [ logWindows ],
        ppOrder = \[ workspaces, _, _, windows ] -> [ workspaces, windows ],
        ppSep = " "
    }
        where
            ppFocused = xmobarBorder "Top" "#ffffff" 2 . ppWindow
            ppUnfocused = ppWindow
            ppWindow =
                wrapPadding
                . shorten 32
                . (\w -> if null w then "[untitled]" else w)
            logWindows = logTitles ppFocused ppUnfocused

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