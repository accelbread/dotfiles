# vi:syntax=sh

update_tile_network() {
    tile_network_ap=$(nmcli -f AP.IN-USE -c no device show $NETWORK_DEVICE | grep '\*')
    if [ $? -ne 0 ]; then
        tile_network="%{F$COLOR_HIGH} no connection %{F-}"
    else
        tile_network_ap_id=$(expr "$tile_network_ap" : '^AP\[\([0-9]*\)\]')
        tile_network=" $(nmcli -t -f AP.SSID,AP.RATE,AP.BARS -c no device show $NETWORK_DEVICE | grep "^AP\\[$tile_network_ap_id" | sed 's/^[^:]*://' | awk -v ORS=' ' '{print $0}') "
    fi
}

init_tile_network() {
    update_tile_network
    tile_add_refresh
}

