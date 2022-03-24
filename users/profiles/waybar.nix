{config, ...}: {
  programs.waybar.enable = true;
  programs.waybar.settings.mainBar = {
    position = "bottom";
    spacing = 8;
    modules-left = ["river/tags" "sway/workspaces" "sway/mode" "custom/media"];
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
    * {
        border: none;
        border-radius: 0;
        font-family: "Roboto, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
        font-size: 18px;
        min-height: 30px;
    }

    window#waybar {
        background: rgba(0, 0, 0, 0.5);
        color: white;
    }

    #window {
        font-weight: bold;
        font-family: "Roboto, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
    }

    #workspaces button {
        padding: 0 5px;
        background: transparent;
        color: #bababa;
        border-top: 2px solid transparent;
    }

    #workspaces button.focused {
        color: white;
        background: transparent;
        border-top: 2px solid #c9545d;
    }

    #tags button {
        background: transparent;
        color: transparent;
    }

    #tags button.occupied {
        padding: 0 5px;
        background: transparent;
        color: #bababa;
        border-top: 2px solid transparent;
    }

    #tags button.focused {
        color: white;
        background: transparent;
        border-top: 2px solid #c9545d;
    }

    #mode {
        background: #64727D;
        border-bottom: 3px solid white;
    }

    #clock, #battery, #cpu, #memory, #network, #pulseaudio, #custom-spotify, #tray, #mode {
        padding: 0 3px;
        margin: 0 2px;
    }

    #clock {
        font-weight: bold;
    }

    #battery {
    }

    #battery icon {
        color: red;
    }

    #battery.charging {
    }

    @keyframes blink {
        to {
            background-color: #ffffff;
            color: black;
        }
    }

    #battery.warning:not(.charging) {
        color: white;
        animation-name: blink;
        animation-duration: 0.5s;
        animation-timing-function: linear;
        animation-iteration-count: infinite;
        animation-direction: alternate;
    }

    #cpu {
    }

    #memory {
    }

    #network {
    }

    #network.disconnected {
        background: #f53c3c;
    }

    #pulseaudio {
    }

    #pulseaudio.muted {
    }

    #custom-spotify {
        color: rgb(102, 220, 105);
    }

    #tray {
    }

  '';
}
