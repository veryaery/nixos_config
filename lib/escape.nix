std:

let
    inherit (builtins)
        replaceStrings;

    bashEscape = replaceStrings [ "'" ] [ "'\\''" ];

    fishEscape =
        replaceStrings
        # \"         "      \$      $     \
        # \\\"       \"     \\$     \$    \\
        [ "\\\""     "\""   "\\$"   "$"   "\\"   ]
        [ "\\\\\\\"" "\\\"" "\\\\$" "\\$" "\\\\" ];
in
{
    inherit
        bashEscape
        fishEscape;

    # https://unix.stackexchange.com/questions/32907/what-characters-do-i-need-to-escape-when-using-sed-in-a-sh-script
    breEscape = 
        replaceStrings
        [ "$"   "."   "*"   "^"   "["   "\\"   ]
        [ "\\$" "\\." "\\*" "\\^" "\\[" "\\\\" ];

    sedEscape = replaceStrings [ "/" ] [ "\\/" ];

    bashString = s: "'${bashEscape s}'";
    fishString = s: "\"${fishEscape s}\"";
}