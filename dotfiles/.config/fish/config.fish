function fish_greeting
    set -l greetings ~/images/greetings/*/*
    [ -z "$greetings"  ]; and return
    timg -g32x32 (shuf -n1 -e $greetings)
end

fish_vi_key_bindings
