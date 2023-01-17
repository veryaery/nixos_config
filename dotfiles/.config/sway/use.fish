#!/usr/bin/env fish

set -l name_args $argv
[ -z "$name_args" ]; and return 1

set -l w
if [ "$name_args[1]" = number ]
    set -l num $name_args[2]
    [ -z "$num" ]; and return 1
    
    set w (swaymsg -r -t get_workspaces | jq -r ".[] | select(.num == $num) | .name")
else
    set w (string match -rg "^\"(.*)\"\$|(.*)" "$name_args") # Remove sorrounding quotations.
    set w (string match -rg "[^:]*:(.*)" "$w") # Remove [<num>:] prefix.
end

set -l outs (swaymsg -r -t get_outputs)
set -l prev_out (printf %s\n $outs | jq -r ".[] | select(.current_workspace == \"$w\") | .name")
set -l focus (printf %s\n $outs | jq -r ".[] | select(.focused == true) | .name, .current_workspace")
set -l focus_out $focus[1]
set -l focus_w $focus[2]

[ -n "$prev_out" ]; and swaymsg -- "workspace \"$focus_w\"; move workspace output \"$prev_out\""
swaymsg -- workspace $name_args
swaymsg -- move workspace output \"$focus_out\"
swaymsg -- workspace $name_args
