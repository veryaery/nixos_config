import Data.Ratio

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig

import Theme

xpConfig :: XPConfig
xpConfig = def
    {
        font = "xft:" ++ themeFont ++ ":pixelsize=12",
        position = CenteredAt (1 % 2) (1 % 3),
        bgColor = themeBackground,
        fgColor = themeForeground,
        bgHLight = themePrimary,
        fgHLight = themeBackground,
        borderColor = themePrimary,
        promptBorderWidth = 1,
        defaultPrompter = const "run "
    }

layout' = avoidStruts $ grid ||| full
    where
        grid = spacing' $ TallGrid 2 2 (3 % 4) (16 % 9) (3 % 100)
        full = noBorders Full

        screenBorder = 32
        windowBorder = 16
        spacing' =
            spacingRaw
            False
            (Border screenBorder screenBorder screenBorder screenBorder) True
            (Border windowBorder windowBorder windowBorder windowBorder) True

config'' = def
    {
        terminal = "alacritty",
        modMask = mod4Mask,

        borderWidth = 2,
        normalBorderColor = themeBackground,
        focusedBorderColor = themePrimary,

        layoutHook = layout',
        startupHook = spawn "~/.fehbg"
            >> spawn "picom --experimental-backend"
    }

keys' XConfig { modMask = modMask } =
    [ ( (modMask, xK_r), shellPrompt xpConfig )
    , ( (0, xK_Print), spawn "flameshot gui" )
    , ( (shiftMask, xK_Print), spawn "flameshot full")
    ]

config' =  additionalKeys config'' $ keys' config''

mapLayout :: String -> String
mapLayout layout
    | layout == "Spacing TallGrid" = "grid"
    | layout == "Full"             = "full"
    | otherwise                    = layout

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

        ppLayout = colorPrimary . mapLayout,
        ppTitle = colorPrimary . shorten 64,

        ppOrder = \(workspaces:layout:_:_) -> [ layout, workspaces ],
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
