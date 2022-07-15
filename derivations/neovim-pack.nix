pkgs:

{
    # start? :: [ derivation ]
    start ? [],

    # opt? :: [ derivation ]
    opt ? []
}:

let
    inherit (builtins)
        concatStringsSep
        map;

    startCommands =
        map
        (plugin: "ln -s ${plugin} $startout")
        start;
    
    optCommands =
        map
        (plugin: "ln -s ${plugin} $optout")
        opt;

    commands = startCommands ++ optCommands;
in
pkgs.runCommandLocal
"pack"
{}
''
startout=$out/pack/_/start
optout=$out/pack/_/opt

mkdir -p $startout
mkdir $optout

${concatStringsSep "\n" commands}
''