pkgs:

{
    # themes :: Map string Theme
    themes
}:

let
    # xmobar' :: string -> Theme -> derivation
    xmobar' = themeName: theme:
        let
            supply = "BAT0";

            battery =
                pkgs.writeScriptBin "battery"
                ''
                    #!${pkgs.fish}/bin/fish

                    while true 
                        set -l batcapacity $(cat /sys/class/power_supply/${supply}/capacity)
                        set -l batstatus $(cat /sys/class/power_supply/${supply}/status)
                        
                        set -l icon
                        if [ $batstatus != "Discharging" ] && [ $batstatus != "Unknown" ]
                            set icon 
                        else if [ $batcapacity -ge 80 ]
                            set icon 
                        else if [ $batcapacity -ge 60 ]
                            set icon 
                        else if [ $batcapacity -ge 40 ]
                            set icon 
                        else if [ $batcapacity -ge 20 ]
                            set icon 
                        else
                            set icon 
                        end

                        echo $(string join "" \
                            "<box type=Bottom width=2 color=${theme.primary}>" \
                                "<fc=${theme.primary}>" \
                                    "<fn=1>$icon</fn>" \
                                "</fc>" \
                                " $batcapacity%" \
                            "</box>" \
                        )

                        sleep 1
                    end
                '';

            signal =
                pkgs.writeScriptBin "signal"
                ''
                    #!${pkgs.fish}/bin/fish

                    while true
                        set -l signal $( \
                            nmcli -f IN-USE,SIGNAL device wifi | \
                            awk "/^\*/ { print \$2 }" \
                        )
                        
                        set -l icon
                        set -l bars
                        if [ -n "$signal" ]
                            set icon 

                            if [ $signal -ge 90 ]
                                set bars ▂▄▆█
                            else if [ $signal -ge 55 ]
                                set bars ▂▄▆
                            else if [ $signal -ge 30 ]
                                set bars ▂▄
                            else
                                set bars ▂
                            end
                        else
                            set icon 
                        end

                        echo $(string join "" \
                            "<box type=Bottom width=2 color=${theme.primary}>" \
                                "<fc=${theme.primary}>" \
                                    "<fn=1>$icon</fn>" \
                                "</fc>" \
                                "$(if [ -n "$bars" ]
                                    echo " $bars"
                                end)" \
                            "</box>" \
                        )

                        sleep 1
                    end
                '';

            temp =
                pkgs.writeScriptBin "temp"
                ''
                    #!${pkgs.fish}/bin/fish
                    
                    function read_temp
                        set -l mtemps $(cat /sys/bus/platform/devices/coretemp.*/hwmon/hwmon*/temp*_input)
                        set -l mmaxs $(cat /sys/bus/platform/devices/coretemp.*/hwmon/hwmon*/temp*_max)

                        set -l maxmtemp
                        set -l minmmax

                        set -l mtempsc $(count $mtemps)
                        set -l i 1
                        while [ $i -le $mtempsc ]
                            set -l mtemp $mtemps[$i]
                            set -l mmax $mmax[$i]

                            if [ -z "$maxmtemp" ] || [ $mtemp -gt $maxmtemp ]
                                set maxmtemp $mtemp
                            end

                            if [ -z "$minmmax" ] || [ $mmax -lt $minmmax ]
                                set minmmax $mmax
                            end

                            set i $(math $i + 1)
                        end

                        echo $(math $maxmtemp / 1000)
                        echo $(math $minmmax / 1000)
                    end

                    while true
                        set -l res $(read_temp)
                        set -l temp $res[1]
                        set -l max $res[2]

                        set -l percent $(math round $temp / $max \* 100) 

                        set -l icon
                        if [ $percent -ge 80 ]
                            set icon 
                        else if [ $percent -ge 40 ]
                            set icon 
                        else if [ $percent -ge 20 ]
                            set icon 
                        else
                            set icon 
                        end

                        echo $(string join "" \
                            "<box type=Bottom width=2 color=${theme.primary}>" \
                                "<fc=${theme.primary}>" \
                                    "<fn=1>$icon</fn>" \
                                "</fc>" \
                                "$temp °C" \
                            "</box>" \
                        )

                        sleep 1
                    end
                '';

            vol =
                pkgs.writeScriptBin "vol"
                ''
                    #!${pkgs.fish}/bin/fish

                    while true
                        set -l vol $(${pkgs.pamixer}/bin/pamixer --get-volume)
                        set -l muted $(${pkgs.pamixer}/bin/pamixer --get-mute)
                        
                        set -l icon
                        if [ $muted = true ]
                            echo 
                        else
                            if [ $vol -ge 75 ]
                                set icon 
                            else if [ $vol -ge 25 ]
                                set icon 
                            else
                                set icon 
                            end
                        end

                        echo $(string join "" \
                            "<box type=Bottom width=2 color=${theme.primary}>" \
                                "<fc=${theme.primary}>" \
                                    "<fn=1>$icon</fn>" \
                                "</fc>" \
                                " $vol%" \
                            "</box>" \
                        )

                        sleep 1
                    end
                '';

            path = pkgs.lib.makeBinPath [ 
                battery
                signal
                temp
                vol
            ];
        in
            pkgs.runCommandLocal "xmobarprime"
            { nativeBuildInputs = with pkgs; [ makeWrapper ]; }
            ''
            makeWrapper ${pkgs.xmobar}/bin/xmobar $out/bin/xmobarprime \
                --prefix PATH : ${path}
            '';

    xmobarThemes =
        pkgs.themes
        {
            inherit themes;
            drvFn = xmobar';
        };
in
pkgs.writeScriptBin "xmobar"
''
    #!${pkgs.fish}/bin/fish

    set -l themedrv $(cat ${xmobarThemes}/$NIXOSCFG_THEME)

    $themedrv/bin/xmobarprime $argv
''
