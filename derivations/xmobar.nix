pkgs:

let
    supply = "BAT0";

    batcap =
        pkgs.writeScriptBin "batcap"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l capacity $(cat /sys/class/power_supply/${supply}/capacity)
                echo $capacity%
                sleep 1
            end
        '';

    batico =
        pkgs.writeScriptBin "batico"
        ''
            #!${pkgs.fish}/bin/fish

            while true 
                set -l capacity $(cat /sys/class/power_supply/${supply}/capacity)
                set -l supply_status $(cat /sys/class/power_supply/${supply}/status)

                if [ $supply_status != "Discharging" ] && [ $supply_status != "Unknown" ]
                    echo 
                else if [ $capacity -ge 80 ]
                    echo 
                else if [ $capacity -ge 60 ]
                    echo 
                else if [ $capacity -ge 40 ]
                    echo 
                else if [ $capacity -ge 20 ]
                    echo 
                else
                    echo 
                end

                sleep 1
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
    --prefix PATH : ${path}
''
