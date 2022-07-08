# Remove greeting.
set -U fish_greeting

alias .. "cd .."

function fish_prompt
    printf "\n"
    printf "%s\n" (prompt_pwd -d 0)
    printf "%sλ%s " \
        (set_color -o "<primary>") \
        (set_color normal)
end

function fish_right_prompt
    printf $status

    set -l s (math "floor($CMD_DURATION / 1000)")
    set -l ms (math "$CMD_DURATION % 1000")

    [ $s -gt 0 ]
        or [ $ms -gt 0 ]
    set -l hasDuration $status

    [ $hasDuration = 0 ]; and printf " ("
    if [ $s -gt 0 ]
        printf "$s s"
        [ $ms -gt 0 ]; and printf " "
    end
    [ $ms -gt 0 ]; and printf "$ms ms"
    [ $hasDuration = 0 ]; and printf ")"
end
