std:

let
    inherit (builtins)
        concatStringsSep
        map
        replaceStrings;

    bashEscape = replaceStrings [ "'" ] [ "'\\''" ];

    fishEscape =
        replaceStrings
        # \"         "      \$      $     \
        # \\\"       \"     \\$     \$    \\
        [ "\\\""     "\""   "\\$"   "$"   "\\"   ]
        [ "\\\\\\\"" "\\\"" "\\\\$" "\\$" "\\\\" ];

    bashString = s: "'${bashEscape s}'";
    fishString = s: "\"${fishEscape s}\"";
in
{
    inherit
        bashEscape
        bashString
        fishEscape
        fishString;

    # https://unix.stackexchange.com/questions/32907/what-characters-do-i-need-to-escape-when-using-sed-in-a-sh-script
    breEscape = 
        replaceStrings
        [ "$"   "."   "*"   "^"   "["   "\\"   ]
        [ "\\$" "\\." "\\*" "\\^" "\\[" "\\\\" ];

    escapeBREScript =
        let
            commands = (map (s: "s/\\${s}/\\\\${s}/g") [
                "$" "." "*" "^" "["
            ]) + [ "s/\\\\/\\\\\\\\/g" ];
        in bashString (concatStringsSep " ; " commands);

    sedEscape = replaceStrings [ "/" ] [ "\\/" ];

    escapeSEDScript = bashString "s/\\//\\\\\\//g";
}