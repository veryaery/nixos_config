pkgs:

let
    bat = "BAT0";

    batcap =
        pkgs.writeScriptBin "batcap"
        ''
            #!${pkgs.fish}/bin/fish
            
            cat /sys/class/power_supply/${bat}/capacity
        '';

    batico =
        pkgs.writeScriptBin "batico"
        ''
            #!${pkgs.fish}/bin/fish
    
            set -l capacity $(${batcap}/bin/batcap)
            set -l status $(cat /sys/class/power_supply/${bat}/status)

            if [ $status = "Charging" ]
                echo   
            else if [ $capacity -gt 80 ]
                echo 
            else if [ $capacity -gt 60 ]
                echo 
            else if [ $capacity -gt 40 ]
                echo 
            else if [ $capacity -gt 20 ]
                echo 
            else
                echo 
            end
        '';

    path = pkgs.lib.makeBinPath [
        batcap
        batico
    ];
in
pkgs.runCommand "xmobar"
{ buildInputs = with pkgs; [ makeWrapper ]; }
''
makeWrapper ${pkgs.xmobar}/bin/xmobar $out/bin/xmobar \
    --prefix PATH ${path}
''
