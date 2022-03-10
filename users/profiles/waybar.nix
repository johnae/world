{ config, ... }:

{
  programs.waybar.enable = true;
  programs.waybar.settings.mainBar = {
    position = "bottom";
    spacing = 8;
    modules-left = ["river/tags" "custom/media"];
    modules-right = ["idle_inhibitor" "pulseaudio" "network" "cpu" "temperature" "backlight" "battery" "battery#bat2" "clock" "tray"];
    network = {
        format-wifi = " {bandwidthDownOctets}  {bandwidthUpOctets} {essid} ({signalStrength}%) ";
        format-ethernet = " {bandwidthDownOctets}  {bandwidthUpOctets} {ipaddr}/{cidr} ";
        tooltip-format = "{ifname} via {gwaddr} ";
        format-linked = "{ifname} (No IP) ";
        format-disconnected = "Disconnected ⚠";
        format-alt = "{ifname}: {ipaddr}/{cidr}";
    };
  };
  programs.waybar.systemd.enable = true;
  programs.waybar.style = ''
    window#waybar {
        background: @theme_base_color;
        background: shade(alpha(@borders, 0.9), 1.25);
        border-bottom: 1px solid @unfocused_borders;
        color: @theme_text_color;
    }
  '';
}
