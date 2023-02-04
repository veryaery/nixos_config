source {{ pkgs.fundle }}/functions/fundle.fish

fundle plugin "laughedelic/pisces"
fundle init

alias .. "cd .."
alias lst "exa -alT -I ".git" --icons --no-time --git --group-directories-first"
alias ls "lst -L2"
alias ls1 "lst -L1"

fish_vi_key_bindings

# vim cursor shapes.
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block

function bind_all_modes
    set -l modes default insert replace_one replace visual
    for mode in $modes
        bind -M $mode $argv
    end
end

function fish_user_key_bindings
    bind_all_modes \cl accept-autosuggestion
    bind_all_modes \cj history-search-forward
    bind_all_modes \ck history-search-backward
end

function fish_greeting
    set -l greetings ~/images/greetings/*/*
    [ -z "$greetings"  ]; and return
    timg -g24x24 (shuf -n1 -e $greetings)
end
