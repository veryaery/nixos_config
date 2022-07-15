std:

# These functions are untested and are not guaranteed to cover all edge cases!

let
    inherit (builtins)
        concatStringsSep
        map
        replaceStrings;

    # bashEscape :: string -> string
    bashEscape = replaceStrings [ "'" ] [ "'\\''" ];

    # fishEscape :: string -> string
    fishEscape =
        replaceStrings
        # \"         "      \$      $     \
        # \\\"       \"     \\$     \$    \\
        [ "\\\""     "\""   "\\$"   "$"   "\\"   ]
        [ "\\\\\\\"" "\\\"" "\\\\\\$" "\\$" "\\\\" ];

    # bashString :: string -> string
    bashString = s: "'${bashEscape s}'";
    # fishString :: string -> string
    fishString = s: "\"${fishEscape s}\"";

    # escapeBREScript :: string
    escapeBREScript =
        let
            commands = (map (s: "s/\\${s}/\\\\${s}/g") [
                "$" "." "*" "^" "["
            ]) ++ [ "s/\\\\/\\\\\\\\/g" ];
        in concatStringsSep " ; " commands;

    # escapeSEDScript :: string
    escapeSEDScript = "s/\\//\\\\\\//g";
in
{
    inherit
        bashEscape
        bashString
        fishEscape
        fishString;

    # https://unix.stackexchange.com/questions/32907/what-characters-do-i-need-to-escape-when-using-sed-in-a-sh-script
    # breEscape :: string -> string
    breEscape = 
        replaceStrings
        [ "$"   "."   "*"   "^"   "["   "\\"   ]
        [ "\\$" "\\." "\\*" "\\^" "\\[" "\\\\" ];

    # sedEscape :: string -> string
    sedEscape = replaceStrings [ "/" ] [ "\\/" ];

    # escapeBREScriptBash :: string
    escapeBREScriptBash = bashString escapeBREScript;
    # escapeBREScriptFish :: string
    escapeBREScriptFish = fishString escapeBREScript;

    # escapeSEDScriptBash :: string
    escapeSEDScriptBash = bashString escapeSEDScript;
    # escapeSEDScriptFish :: string
    escapeSEDScriptFish = fishString escapeSEDScript;
}