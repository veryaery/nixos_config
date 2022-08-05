# Remove greeting.
set -U fish_greeting

# Erase default mode prompt.
functions -e fish_mode_prompt

set -x EDITOR "nvim"

alias .. "cd .."
alias commit "git add --all; git commit"
alias lsl "ls -l"
alias treel "tree -L 2"

fish_vi_key_bindings

# Set vim mode cursor shapes.
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

function fish_prompt
    printf "\n"
    printf "%s\n" (prompt_pwd -d 0)
    printf "%sλ%s " \
        (set_color -o "<primary>") \
        (set_color normal)
end

function fish_right_prompt
    if [ $status = 0 ]
        printf "%s✓%s " \
            (set_color -o brgreen) \
            (set_color normal)
    else
        printf "%s$status%s " \
            (set_color -o brred) \
            (set_color normal)
    end

    set -l s (math "floor($CMD_DURATION / 1000)")
    set -l ms (math "$CMD_DURATION % 1000")

    [ $s -gt 0 ]
        or [ $ms -gt 0 ]
    set -l had_duration $status

    [ $had_duration = 0 ]; and printf "("
    if [ $s -gt 0 ]
        printf "$s s"
        [ $ms -gt 0 ]; and printf " "
    end
    [ $ms -gt 0 ]; and printf "$ms ms"
    [ $had_duration = 0 ]; and printf ") "
    
    switch $fish_bind_mode
        case default
            printf "%sNORMAL%s" \
                (set_color -b green black) \
                (set_color normal)
        case insert
            printf "%sINSERT%s" \
                (set_color -b cyan black) \
                (set_color normal)
        case replace_one
            printf "%sREPLACE%s" \
                (set_color -b red black) \
                (set_color normal)
        case replace
            printf "%sREPLACE%s" \
                (set_color -b red black) \
                (set_color normal)
        case visual
            printf "%sVISUAL%s" \
                (set_color -b magenta black) \
                (set_color normal)
    end
end
