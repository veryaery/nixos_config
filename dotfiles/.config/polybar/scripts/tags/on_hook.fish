#!{{ pkgs.fish }}/bin/fish

set -l theme_primary "{{ primary }}"
set -l theme_foreground "{{ foreground }}"
set -l theme_background "{{ background }}"
set -l theme_primary_mix "{{ primaryMix }}"
set -l theme_background_mix "{{ backgroundMix }}"

alias hc herbstclient

set -l tags (string split -n \t (hc tag_status $MONITOR_I))
set -l tags_count (count $tags)
for i_plus_1 in (seq $tags_count)
    set -l tag $tags[$i_plus_1]
    set -l i (math $i_plus_1 - 1)

    set -l prefix (string sub -l 1 -- $tag)
    set -l name (string sub -s 2 -- $tag)
    
    switch $prefix
        case "."
            echo -n "%{F$theme_foreground}"
        case ":"
            echo -n "%{F$theme_foreground}"
        case "+"
            echo -n "%{B$theme_primary_mix}%{F$theme_background}"
        case "#"
            echo -n "%{B$theme_primary}%{F$theme_background}"
        case "-"
            echo -n "%{B$theme_background_mix}%{F$theme_foreground}"
        case "%"
            echo -n "%{B$theme_background_mix}%{F$theme_foreground}"
        case "!"
            echo -n "%{B#ffff00}%{F#ff0000}"
    end

    echo -n "%{A1:herbstclient focus_monitor $MONITOR_I; herbstclient use_index $i:} $name %{A -o -u F- B-}"
end

echo -n "%{F- B-}"
echo
