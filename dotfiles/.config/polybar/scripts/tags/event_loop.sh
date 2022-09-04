#!{{ runtimeShell }}

hc() {
    herbstclient "$@"
}

on_hook() {
    while read; do
        ~/.config/polybar/scripts/tags/on_hook.fish
    done
}

echo | on_hook
hc --idle tag | on_hook
