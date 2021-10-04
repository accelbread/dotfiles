# vi:syntax=sh

update_tile_battery() {
    tile_battery_acpi=$(acpi -b | head -n 1)
    if [ "x${tile_battery_acpi}" != "x${tile_battery_last}" ]; then
        tile_battery_last=$tile_battery_acpi
        tile_battery_charge=$(expr "$tile_battery_acpi" : '^.*: \([^,]*\),')
        tile_battery_level=$(expr "$tile_battery_acpi" : '^.*,\( [0-9]*\)%')
        tile_battery_remain=$(expr "$tile_battery_acpi" : '^.*\( [0-9][0-9]:[0-5][0-9]\):[0-5][0-9] ')
        case "${tile_battery_charge}" in
            'Discharging') tile_battery_stat='BAT';;
            'Charging') tile_battery_stat='CHR';;
            *) tile_battery_stat='???';;
        esac
        if [ "$tile_battery_level" -le "$BATTERY_LOW" ]; then
            tile_battery_left="%{F$COLOR_WARN}%{T2}"
            tile_battery_right="%{F-}%{T-}"
        elif [ "$tile_battery_level" -le "$BATTERY_MEDIUM" ]; then
            tile_battery_left="%{F$COLOR_HIGH}"
            tile_battery_right="%{F-}"
        else
            tile_battery_left=""
            tile_battery_right=""
        fi
        tile_battery=" ${tile_battery_left}${tile_battery_stat}${tile_battery_level}%"\
"${tile_battery_remain}${tile_battery_right} "
    fi
}

init_tile_battery() {
    update_tile_battery
    tile_battery_last=''
    tile_add_refresh
}

