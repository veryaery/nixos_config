set -U primary "{{ primary }}"

# Plugin management.
source {{ pkgs.fundle }}/functions/fundle.fish

fundle plugin "jethrokuan/z"
fundle plugin "laughedelic/pisces"
fundle plugin "acomagu/fish-async-prompt"
fundle init 1>/dev/null
fundle install 1>/dev/null

alias .. "cd .."
alias lst "exa -alT -I ".git" --icons --no-time --git --group-directories-first"
alias ls "lst -L2"
alias ls1 "lst -L1"

# Typos.
alias claer "clear"

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

function fmt_duration
    argparse short -- $argv
    
    set -l units

    set -l ms $argv[1]
    set -l ms_unit " msec"; [ -n "$_flag_short" ]; and set ms_unit "ms"

    set -l s 0
    set -l s_unit " sec"; [ -n "$_flag_short" ]; and set s_unit "s"
    if [ $ms -ge 1000 ]
        set s (math "floor($ms / 1000)")
        set ms (math "$ms % 1000")
    end
    [ $ms -gt 0 ]; and set -a units "$ms$ms_unit"

    set -l m 0 
    set -l m_unit " min"; [ -n "$_flag_short" ]; and set m_unit "m"
    if [ $s -ge 60 ]
        set m (math "floor ($s / 60)")
        set s (math "$s % 60")
    end
    [ $s -gt 0 ]; and set -a units "$s$s_unit"

    set -l h 0 
    set -l h_unit " hr"; [ -n "$_flag_short" ]; and set h_unit "h"
    if [ $m -ge 60 ]
        set h (math "floor ($m / 60)")
        set m (math "$m % 60")
    end
    [ $m -gt 0 ]; and set -a units "$m$m_unit"

    set -l d 0 
    set -l d_unit " day"; [ -n "$_flag_short" ]; and set d_unit "d"
    if [ $h -ge 24 ]
        set d (math "floor ($h / 24)")
        set h (math "$h % 24")
    end
    [ $h -gt 0 ]; and set -a units "$h$h_unit"

    set -l w 0 
    set -l w_unit " wk"; [ -n "$_flag_short" ]; and set w_unit "w"
    if [ $d -ge 7 ]
        set w (math "floor ($d / 7)")
        set d (math "$d % 7")
    end
    [ $d -gt 0 ]; and set -a units "$d$d_unit"

    [ $w -gt 0 ]; and set -a units "$w$w_unit"

    string join \n $units
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

# Prompt.
functions -e fish_mode_prompt

set -U async_prompt_inherit_variables primary COLUMNS pipestatus CMD_DURATION

# https://github.com/acomagu/fish-async-prompt/blob/b866f232a4a5ded7e16cda648648c43925c01ba0/README.md#loading-indicator
function strip_color
    echo "$argv" | sed -r "s/\x1B\[[0-9;]*[JKmsu]//g"
end

function padding_str
    set -l s
    for n in (seq $argv[1])
        set s "$s "
    end
    echo "$s"
end

function vim_mode_prompt
    set -l text
    set -l color
    switch $fish_bind_mode
        case default
            set text N
            set color green
        case insert
            set text I
            set color cyan
        case replace_one replace
            set text R
            set color cyan
        case visual
            set text V
            set color yellow
    end
    string join \n -- --text $text --color $color
end

function user_prompt
    set -l uid (id -u)
    if [ $uid = 0 ]
        printf "%sroot%s " \
            (set_color -o black -b red) \
            (set_color normal)
    else
        set -l user (id -nu)
        printf "%s@%s%s" \
            (set_color green) \
            $user \
            (set_color normal)
    end
end

function hostname_prompt
    printf "@%s " (hostname -s)
end

function pwd_prompt
    printf "%s " (prompt_pwd -d0)
end

function branch_prompt
    set -l branch (git symbolic-ref --short HEAD 2>/dev/null)
    if [ -n "$branch" ]
        printf " %s" "$branch"
    end
end

function duration_prompt
    [ $CMD_DURATION -le 100 ]; and return 0
    set -l units (fmt_duration --short $CMD_DURATION)
    [ -n "$units[-1]" ]; and printf "%s%s%s" \
        (set_color $primary) \
        "$units[-1]" \
        (set_color normal)
    [ -n "$units[-2]" ]; and printf " %s%s%s" $units[-2]
end

function fmt_pipestatus
    # https://github.com/fish-shell/fish-shell/blob/96deaae7d86edfbc16e411bdb73bf54ced7fb447/share/functions/__fish_print_pipestatus.fish
    if contains $argv[1] 0 141
        printf "%s✓%s" \
            (set_color -o green) \
            (set_color normal)
    else
        printf "%s%s%s" \
            (set_color red) \
            (string lower (fish_status_to_signal $argv[1])) \
            (set_color normal)
    end
end

function pipestatus_prompt
    if contains $_pipestatus[-1] 0 141
        printf " %s" (fmt_pipestatus $_pipestatus[-1])
    else
        set -l lst
        for __pipestatus in $_pipestatus
            set -a lst (fmt_pipestatus $__pipestatus)
        end
        printf " "
        string join "|" $lst
    end
end

function left_prompt
    printf "\n"
    printf "%s▎%s%s%s%s%s\n" \
        (set_color $primary) \
        (set_color normal) \
        (user_prompt) \
        (hostname_prompt) \
        (pwd_prompt) \
        (branch_prompt)
    printf "%s▎%s%sλ%s " \
        (set_color $primary) \
        (set_color normal) \
        (set_color -o $primary) \
        (set_color normal)
end

function right_prompt
    printf "\n"
    printf "%s%s\n" \
        (duration_prompt) \
        (pipestatus_prompt)
end

function fish_prompt
    set -l left (left_prompt)
    set -l right (right_prompt)

    set -l max_count (count $left);
    [ (count $right) -gt $max_count ]; and set max_count (count $right)

    for i in (seq $max_count)
        # Left prompt.
        printf "%s" "$left[$i]"

        # Right prompt.
        if [ -n "$right[$i]" ]
            set -l padding_len (math "\
                $COLUMNS \
                - $(string length -V "$left[$i]") \
                - $(string length -V "$right[$i]")
            ")
            set -l padding (padding_str $padding_len)
            printf "%s%s" \
                "$padding" \
                "$right[$i]"
        end

        printf "\n"
    end
end

function fish_right_prompt
    set -U _pipestatus $pipestatus

    argparse "text=" "color=" -- (vim_mode_prompt)

    printf "%s %s%s %s%s" \
        (set_color -b $_flag_color) \
        (set_color -o black -b $_flag_color) \
        "$_flag_text" \
        (set_color normal -b $_flag_color) \
        (set_color normal)
end

set -U async_prompt_functions fish_prompt user_prompt hostname_prompt pwd_prompt branch_prompt duration_prompt pipestatus_prompt
