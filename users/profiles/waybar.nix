{
  config,
  lib,
  pkgs,
  ...
}: let
  pomodoro = pkgs.writeShellApplication {
    name = "pomodoro";
    runtimeInputs = [pkgs.jq pkgs.coreutils pkgs.notify-desktop];
    text = ''
        store=/tmp/.pomodoro
        MIN_25_IN_SECS=$(( 60 * 25 ))
        CMD=''${1:-}
        if [ ! -e "$store" ]; then
          echo 0 > "$store"
        fi
        if [ "$CMD" = "start" ]; then
          if [ -e "$store" ]; then
            start="$(<"$store")"
            now="$(date +'%s')"
            elapsed=$(( now - start ))
            if [ "$elapsed" -lt "$MIN_25_IN_SECS" ]; then
              exit
            fi
          fi
          date +'%s' > "$store"
        elif [ "$CMD" = "reset" ]; then
          echo 0 > "$store"
        elif [ "$CMD" = "status" ]; then
          start="$(<"$store")"
          if [ "$start" = "0" ]; then
            echo break
            exit
          fi
          now="$(date +'%s')"
          elapsed=$(( now - start ))
          left=$(( MIN_25_IN_SECS - elapsed ))
          if [ "$elapsed" -gt "$MIN_25_IN_SECS" ]; then
            echo 0 > "$store"
            notify-desktop -t 5000 -a Pomodoro "Pomodoro block ended" "Time for a break"
            echo ended
          else
            set +o errexit
            ((sec=left%60, left/=60, min=left%60, hrs=left/60))
            set -o errexit
            if [ "$hrs" -eq 0 ]; then
              timestamp=$(printf "%02d:%02d" $min $sec)
            else
              timestamp=$(printf "%02d:%02d:%02d" $hrs $min $sec)
            fi
            echo "$timestamp"
          fi
        else
          cat<<EOF
          Please provide a command, supported commands:

          start  - starts a new pomodoro, exits silently if there is a running pomodoro
          reset  - resets any running pomodoro
          status - returns time left if a pomodoro is running, otherwise returns the word "break"
      EOF
          exit 1
        fi
    '';
  };
in {
  home.packages = [pomodoro];
  programs.waybar.enable = true;
  programs.waybar.settings.mainBar = {
    bar_id = "main";
    layer = "top";
    ipc = true;
    position = "top";
    spacing = 8;
    modules-left = ["river/tags" "sway/workspaces" "hyprland/workspaces" "sway/mode" "hyprland/submap" "custom/media"];
    modules-right = ["custom/pomodoro" "network" "network#wifi" "idle_inhibitor" "pulseaudio" "cpu" "temperature" "backlight" "battery" "clock" "tray"];
    modules-center = ["hyprland/window"];
    "custom/media" = {
      format = "{icon}{}";
      return-type = "json";
      format-icons = {
        Playing = " ";
        Paused = " ";
      };
      max-length = 70;
      exec = "playerctl -a metadata --format '{\"text\": \"{{playerName}}: {{artist}} - {{markup_escape(title)}}\", \"tooltip\": \"{{playerName}} : {{markup_escape(title)}}\", \"alt\": \"{{status}}\", \"class\": \"{{status}}\"}' -F";
      on-click = "playerctl play-pause";
    };
    "hyprland/workspaces" = {
      format = "{name}";
      format-icons = {
        active = "";
        default = "";
        persistent = "";
      };
      on-scroll-up = "hyprctl dispatch workspace r-1";
      on-scroll-down = "hyprctl dispatch workspace r+1";
      all-outputs = false;
      #persistent_workspaces = {
      #  "*" = 5;
      #};
    };
    battery = {
      format = "{capacity}% ({power}W)";
    };
    "hyprland/submap" = {
      format = "✌️ {}";
      tooltip = false;
    };
    "custom/pomodoro" = {
      format = " {icon}{}";
      interval = 1;
      format-icons = {
        Playing = " ";
        Paused = " ";
      };
      max-length = 70;
      on-click = "${pomodoro}/bin/pomodoro start";
      on-click-right = "${pomodoro}/bin/pomodoro reset";
      exec = "${pomodoro}/bin/pomodoro status";
    };
    "custom/clock" = {
      exec = "date +'%Y-%m-%d %H:%M:%S'";
      interval = 1;
      tooltip = false;
    };
    #clock = {
    #  locale = "en_GB.UTF-8";
    #  format = "{:%H:%M}  ";
    #  format-alt = "{:%A, %B %d, %Y (%R)}  ";
    #  tooltip-format = "<tt><small>{calendar}</small></tt>";
    #  calendar = {
    #    mode = "year";
    #    mode-mon-col = 3;
    #    weeks-pos = "right";
    #    on-scroll = 1;
    #    on-click-right = "mode";
    #    format = {
    #      months = "<span color='#ffead3'><b>{}</b></span>";
    #      days = "<span color='#ecc6d9'><b>{}</b></span>";
    #      weeks = "<span color='#99ffdd'><b>W{}</b></span>";
    #      weekdays = "<span color='#ffcc66'><b>{}</b></span>";
    #      today = "<span color='#ff6699'><b><u>{}</u></b></span>";
    #    };
    #  };
    #  actions = {
    #    on-click-right = "mode";
    #    on-click-forward = "tz_up";
    #    on-click-backward = "tz_down";
    #    on-scroll-up = "shift_up";
    #    on-scroll-down = "shift_down";
    #  };
    #};
    idle_inhibitor = {
      format = "{icon}";
      format-icons = {
        activated = "";
        deactivated = "";
      };
    };
    pulseaudio = {
      format = "{volume}% {icon}";
      format-bluetooth = "{volume}% {icon}";
      format-muted = "";
      format-icons = {
        headphone = "";
        hands-free = "";
        headset = "";
        phone = "";
        portable = "";
        car = "";
        default = ["" ""];
      };
      scroll-step = 1;
      on-click = "pavucontrol";
    };
    network = {
      interface = lib.mkDefault "enp*";
      format-wifi = lib.mkDefault " {bandwidthDownOctets:>}  {bandwidthUpOctets:>} {essid} ({signalStrength}%) ";
      format-ethernet = lib.mkDefault " {bandwidthDownOctets:>}  {bandwidthUpOctets:>} {ipaddr}/{cidr} ";
      tooltip-format = lib.mkDefault "{ifname} via {gwaddr} ";
      format-linked = lib.mkDefault "{ifname} (No IP) ";
      format-disconnected = lib.mkDefault "";
      format-alt = lib.mkDefault "{ifname}: {ipaddr}/{cidr}";
      interval = 1;
    };
    "network#wifi" = {
      interface = lib.mkDefault "wlan*";
      format-wifi = lib.mkDefault " {bandwidthDownOctets:>}  {bandwidthUpOctets:>} {essid} ({signalStrength}%) ";
      format-ethernet = lib.mkDefault " {bandwidthDownOctets:>}  {bandwidthUpOctets:>} {ipaddr}/{cidr} ";
      tooltip-format = lib.mkDefault "{ifname} via {gwaddr} ";
      format-linked = lib.mkDefault "{ifname} (No IP) ";
      format-disconnected = lib.mkDefault "";
      format-alt = lib.mkDefault "{ifname}: {ipaddr}/{cidr}";
      interval = 1;
    };
  };
  programs.waybar.systemd.enable = true;
  systemd.user.services.waybar = {
    serviceConfig = {
      RestartSec = 1;
      RestartSteps = 4;
      RestartMaxDelaySec = 10;
    };
  };
  programs.waybar.style = ''
    * {
        border: none;
        border-radius: 0;
        font-family: "Roboto Mono, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
        font-size: 18px;
        min-height: 30px;
    }

    window#waybar {
        background: rgba(0, 0, 0.4, 0.4);
        color: white;
    }

    #window {
        font-weight: bold;
        font-family: "Roboto Mono, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
    }

    #workspaces button {
        border-radius: 20px;
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

    #workspaces button.active {
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

    #mode, #submap {
        background: #64727D;
        border-bottom: 3px solid white;
    }

    #custom-clock, #clock, #battery, #cpu, #memory, #network, #pulseaudio, #custom-spotify, #tray, #mode {
        padding: 0 3px;
        margin: 0 2px;
    }

    #clock, #custom-clock {
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

    #custom-pomodoro {
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
