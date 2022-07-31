# Remove greeting.
set -U fish_greeting

set -x EDITOR "nvim"

alias .. "cd .."
alias commit "git add --all; git commit"
alias lsl "ls -l"
alias treel "tree -L 2"

function fish_prompt
    printf "\n"
    printf "%s\n" (prompt_pwd -d 0)
    printf "%sλ%s " \
        (set_color -o "<primary>") \
        (set_color normal)
end

function fish_right_prompt
    if [ $status = 0 ]
        printf "%s✓%s" \
            (set_color -o brgreen) \
            (set_color normal)
    else
        printf "%s$status%s" \
            (set_color -o brred) \
            (set_color normal)
    end

    set -l s (math "floor($CMD_DURATION / 1000)")
    set -l ms (math "$CMD_DURATION % 1000")

    [ $s -gt 0 ]
        or [ $ms -gt 0 ]
    set -l had_duration $status

    [ $had_duration = 0 ]; and printf " ("
    if [ $s -gt 0 ]
        printf "$s s"
        [ $ms -gt 0 ]; and printf " "
    end
    [ $ms -gt 0 ]; and printf "$ms ms"
    [ $had_duration = 0 ]; and printf ")"
end
