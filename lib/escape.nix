std:

let
    inherit (builtins)
        replaceStrings;
in
{
    # https://unix.stackexchange.com/questions/32907/what-characters-do-i-need-to-escape-when-using-sed-in-a-sh-script
    breEscape = 
        replaceStrings
        [ "$"   "."   "*"   "^"   "["   "\\"   ]
        [ "\\$" "\\." "\\*" "\\^" "\\[" "\\\\" ];

    sedEscape = replaceStrings [ "/" ] [ "\\/" ];

    bashEscape = replaceStrings [ "'" ] [ "'\\''" ];

    bashString = s: "'${bashEscape s}'";

    fishEscape =
        replaceStrings
        # \"         "      \$      $     \
        # \\\"       \"     \\$     \$    \\
        [ "\\\""     "\""   "\\$"   "$"   "\\"   ]
        [ "\\\\\\\"" "\\\"" "\\\\$" "\\$" "\\\\" ];

    fishString = s: "\"${fishEscape s}\"";
}