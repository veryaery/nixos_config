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

    bars =
        pkgs.writeScriptBin "bars"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l signal $( \
                    nmcli -f IN-USE,SIGNAL device wifi | \
                    awk "/^\*/ { print \$2 }" \
                )
                
                if [ -n $signal ]
                    if [ $signal -ge 90 ]
                        echo ▂▄▆█
                    else if [ $signal -ge 55 ]
                        echo ▂▄▆
                    else if [ $signal -ge 30 ]
                        echo ▂▄
                    else
                        echo ▂
                    end
                end

                sleep 1
            end
        '';

    temp' =
        pkgs.writeScript "tempprime"
        ''
            #!${pkgs.fish}/bin/fish

            set -l max_temp
            set -l min_max

            for i in $(seq 2 5)
                set -l mtemp $(cat /sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp$(echo $i)_input)
                set -l temp $(math $mtemp / 1000)
                set -l mmax $(cat /sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp$(echo $i)_max)
                set -l max $(math $mmax / 1000)

                if [ -z $max_temp ] || [ $temp -gt $max_temp ]
                    set max_temp $temp
                end

                if [ -z $min_max ] || [ $max -lt $min_max ]
                    set min_max $max
                end
            end

            echo $max_temp
            echo $min_max
        '';

    temp =
        pkgs.writeScriptBin "temp"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l res $(${temp'}) 
                set -l temp $res[1]
                
                echo $temp °C

                sleep 1
            end
        '';

    tempico =
        pkgs.writeScriptBin "tempico"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l res $(${temp'})
                set -l temp $res[1]
                set -l max $res[2]
                set -l percent $(math round $temp / $max \* 100)

                if [ $percent -ge 80 ]
                    echo 
                else if [ $percent -ge 60 ]
                    echo 
                else if [ $percent -ge 40 ]
                    echo 
                else if [ $percent -ge 20 ]
                    echo 
                else
                    echo 
                end

                sleep 1
            end
        '';

    vol =
        pkgs.writeScriptBin "vol"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l vol $(${pkgs.pamixer}/bin/pamixer --get-volume)
                echo $vol%
                sleep 1
            end
        '';

    volico =
        pkgs.writeScriptBin "volico"
        ''
            #!${pkgs.fish}/bin/fish

            while true
                set -l vol $(${pkgs.pamixer}/bin/pamixer --get-volume)
                set -l muted $(${pkgs.pamixer}/bin/pamixer --get-mute)

                if [ $muted = true ]
                    echo 
                else
                    if [ $vol -ge 75 ]
                        echo 
                    else if [ $vol -ge 25 ]
                        echo 
                    else
                        echo 
                    end
                end

                sleep 1
            end
        '';

    path = pkgs.lib.makeBinPath [ 
        batcap
        batico
        bars
        temp
        tempico
        vol
        volico
    ];
in
pkgs.runCommand "xmobar"
{ buildInputs = with pkgs; [ makeWrapper ]; }
''
makeWrapper ${pkgs.xmobar}/bin/xmobar $out/bin/xmobar \
    --prefix PATH : ${path}
''
