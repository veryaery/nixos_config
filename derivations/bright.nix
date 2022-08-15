pkgs:

pkgs.writeScriptBin "bright"
''
#!${pkgs.fish}/bin/fish

set -l brightpercent $argv[1]
set -l brightmax $(cat /sys/class/backlight/*/max_brightness)
set -l bright $(math "(min($brightpercent, 100) / 100) * $brightmax")

echo $bright | doas tee /sys/class/backlight/*/brightness
''
