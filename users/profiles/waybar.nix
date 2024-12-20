{
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
    margin-top = 0;
    modules-left = ["custom/logo" "river/tags" "sway/workspaces" "hyprland/workspaces" "sway/mode" "river/mode" "hyprland/submap" "custom/media"];
    modules-right = ["custom/pomodoro" "network" "network#wifi" "idle_inhibitor" "pulseaudio" "cpu" "temperature" "backlight" "battery" "clock" "tray"];
    modules-center = ["hyprland/window" "river/window"];
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
    "river/tags" = {
      num-tags = 9;
      tag-labels = ["1: " "2:  work" "3: " "4:  private" "5" "6" "7" "8: local " "9: remote "];
      set-tags = [
        1
        2
        4
        8
        16
        32
        64
        128
        256
      ];
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
    "custom/logo" = {
      format = "    ";
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
  programs.waybar.style = ''
    * {
        font-family: "Roboto, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
        font-size: 15px;
        font-weight: bold;
        transition: background-color .3s ease-out;
        border: none;
        border-radius: 0px;
    }

    window#waybar {
        background: rgba(0,0,40,0.4);
        color: #c0caf5;
        transition: background-color .5s;
    }

    .modules-left,
    .modules-center,
    .modules-right
    {
        /* background: rgba(0, 0, 8, .7); */
        margin: 5px 10px;
        padding: 0 5px;
        border-radius: 15px;
    }
    .modules-left {
        padding: 0;
    }
    .modules-center {
        padding: 0 10px;
    }

    #clock,
    #battery,
    #cpu,
    #memory,
    #disk,
    #temperature,
    #backlight,
    #network,
    #pulseaudio,
    #wireplumber,
    #custom-media,
    #tray,
    #mode,
    #idle_inhibitor,
    #scratchpad,
    #power-profiles-daemon,
    #language,
    #mpd {
        padding: 0 10px;
        border-radius: 15px;
    }

    #clock:hover,
    #battery:hover,
    #cpu:hover,
    #memory:hover,
    #disk:hover,
    #temperature:hover,
    #backlight:hover,
    #network:hover,
    #pulseaudio:hover,
    #wireplumber:hover,
    #custom-media:hover,
    #tray:hover,
    #mode:hover,
    #idle_inhibitor:hover,
    #scratchpad:hover,
    #power-profiles-daemon:hover,
    #language:hover,
    #mpd:hover {
        background: rgba(26, 27, 38, 0.9);
    }

    #custom-logo {
        background-image: url('${../../files/nix-snowflake.svg}');
        background-color: @backgroundlight;
        background-position: center;
        padding: 3px;
        background-origin: content-box;
        background-repeat: no-repeat;
        background-size: contain;
        margin: 3px 3px 3px 16px;
        font-size:16px;
        color: @iconcolor;
        border-radius: 15px;
        opacity: 0.8;
        transition: all 0.3s ease-in-out;
    }

    #workspaces button {
      background: transparent;
      font-family:
        SpaceMono Nerd Font,
        feather;
      font-weight: 900;
      font-size: 13pt;
      color: #c0caf5;
      border:none;
      border-radius: 15px;
    }

    #workspaces button.active {
        background: rgba(70,80,200,0.3);
    }

    #workspaces button:hover {
      background: #11111b;
      color: #cdd6f4;
      box-shadow: none;
    }

    #custom-arch {
        margin-left: 5px;
        padding: 0 10px;
        font-size: 25px;
        transition: color .5s;
    }
    #custom-arch:hover {
        color: #1793d1;
    }

  '';
  #   programs.waybar.style = ''
  #     @define-color backgroundlight rgba(0,0,0,0);
  #     @define-color backgrounddark rgba(0,0,0,0);
  #     @define-color workspacesbackground1 rgba(0,0,0,0);
  #     @define-color workspacesbackground2 rgba(0,0,0,0);
  #     @define-color bordercolor #eceff4;
  #     @define-color textcolor1 #eceff4;
  #     @define-color textcolor2 #2e3440;
  #     @define-color textcolor3 #e5e9f0;
  #     @define-color iconcolor #2e3440;
  #     @define-color warn #bf616a;
  #     @define-color info #ebcb8b;
  #     @define-color focus #b48ead;

  #     * {
  #         font-family: "Roboto Mono, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
  #         border: none;
  #         border-radius: 0px;
  #     }

  #     window#waybar {
  #         background-color: rgba(5,5,5,0.0);
  #         border-bottom: 0px solid #ffffff;
  #         transition-property: background-color;
  #         transition-duration: .5s;
  #     }

  #     #workspaces, #tags {
  #         background: @workspacesbackground1;
  #         margin: 0px 1px 3px 1px;
  #         padding: 0px 1px;
  #         border-radius: 15px;
  #         border: 0px;
  #         font-weight: bold;
  #         font-style: normal;
  #         opacity: 0.8;
  #         font-size: 16px;
  #         color: @textcolor1;
  #     }

  #     #workspaces, #tags button {
  #         padding: 0px 5px;
  #         margin: 4px 3px;
  #         border-radius: 15px;
  #         border: 0px;
  #         color: @textcolor1;
  #         background-color: @workspacesbackground2;
  #         transition: all 0.3s ease-in-out;
  #         opacity: 0.4;
  #     }

  #     #workspaces, #tags button.active {
  #         color: @textcolor1;
  #         background: @workspacesbackground2;
  #         border-radius: 15px;
  #         min-width: 40px;
  #         transition: all 0.3s ease-in-out;
  #         opacity:1.0;
  #     }

  #     #tags button.focused {
  #         color: @textcolor1;
  #         background: @workspacesbackground2;
  #         border-radius: 15px;
  #         min-width: 40px;
  #         transition: all 0.3s ease-in-out;
  #         opacity:1.0;
  #     }

  #     #workspaces,#tags button:hover {
  #         color: @textcolor1;
  #         background: @workspacesbackground2;
  #         border-radius: 15px;
  #         opacity:0.7;
  #     }

  #     #workspaces, #tags button:not(.occupied):not(.focused) {
  #     	font-size: 0;
  #     	min-width: 0;
  #     	min-height: 0;
  #     	margin: -17px;
  #     	padding: 0;
  #     	border: 0;
  #     	opacity: 0;
  #     	box-shadow: none;
  #     	background-color: transparent;
  #     }

  #     tooltip {
  #         border-radius: 10px;
  #         background-color: @backgroundlight;
  #         opacity:0.8;
  #         padding:20px;
  #         margin:0px;
  #     }

  #     tooltip label {
  #         color: @textcolor2;
  #     }

  #     #window {
  #         background: @backgroundlight;
  #         margin: 5px 15px 5px 0px;
  #         padding: 2px 10px 0px 10px;
  #         border-radius: 12px;
  #         color:@textcolor2;
  #         font-size:16px;
  #         font-weight:normal;
  #         opacity:0.8;
  #     }

  #     window#waybar.empty #window:not(.focused) {
  #         background-color:transparent;
  #     }

  #     .modules-left > widget:first-child > #workspaces {
  #         margin-left: 0;
  #     }

  #     .modules-right > widget:last-child > #workspaces {
  #         margin-right: 0;
  #     }

  #     #disk,#memory,#cpu,#idle_inhibitor,#temperature {
  #         background-color: @backgroundlight;
  #         margin:0px;
  #         padding:0px;
  #         font-size:16px;
  #         color:@iconcolor;
  #         border-radius: 15px;
  #         padding: 1px 10px 0px 10px;
  #         margin: 3px 15px 3px 0px;
  #         opacity:0.8;
  #         transition: all 0.3s ease-in-out;
  #     }

  #     #idle_inhibitor.activated {
  #         background-color: @focus;
  #         color:@textcolor1;
  #         border:3px solid @bordercolor;
  #         transition: all 0.3s ease-in-out;
  #     }

  #     #custom-logo {
  #         background-image: url('${../../files/nix-snowflake.svg}');
  #         background-color: @backgroundlight;
  #         background-position: center;
  #         padding: 3px;
  #         background-origin: content-box;
  #         background-repeat: no-repeat;
  #         background-size: contain;
  #         margin: 3px 3px 3px 16px;
  #         font-size:16px;
  #         color: @iconcolor;
  #         border-radius: 15px;
  #         opacity: 0.8;
  #         transition: all 0.3s ease-in-out;
  #     }

  #     #clock, #custom-clock, #custom-pomodoro {
  #         background-color: @backgrounddark;
  #         font-size: 16px;
  #         color: @textcolor1;
  #         border-radius: 15px;
  #         padding: 1px 10px 0px 10px;
  #         margin: 3px 16px 3px 0px;
  #         opacity:0.8;
  #         border:3px solid @bordercolor;
  #     }

  #     #pulseaudio {
  #         background-color: @backgroundlight;
  #         font-size: 16px;
  #         color: @textcolor2;
  #         border-radius: 15px;
  #         padding: 2px 10px 0px 10px;
  #         margin: 5px 15px 5px 0px;
  #         opacity:0.8;
  #     }

  #     #pulseaudio.muted {
  #         background-color: @backgrounddark;
  #         color: @textcolor1;
  #     }

  #     #network {
  #         background-color: @backgroundlight;
  #         font-size: 16px;
  #         color: @textcolor2;
  #         border-radius: 15px;
  #         padding: 2px 10px 0px 10px;
  #         margin: 5px 15px 5px 0px;
  #         opacity:0.8;
  #     }

  #     #network.disconnected {
  #         background: #bf616a;
  #     }

  #     #network.ethernet {
  #         background-color: @backgroundlight;
  #         color: @textcolor2;
  #     }

  #     #network.wifi {
  #         background-color: @backgroundlight;
  #         color: @textcolor2;
  #     }

  #     #battery {
  #         background-color: @backgroundlight;
  #         font-size: 16px;
  #         color: @textcolor2;
  #         border-radius: 15px;
  #         padding: 2px 15px 0px 10px;
  #         margin: 5px 15px 5px 0px;
  #         opacity:0.8;
  #     }

  #     #battery.charging, #battery.plugged {
  #         color: @textcolor2;
  #         background-color: @backgroundlight;
  #     }

  #     @keyframes blink {
  #         to {
  #             background-color: @backgroundlight;
  #             color: @textcolor2;
  #         }
  #     }

  #     #mode, #submap {
  #         background-color: @warn;
  #         border-radius: 15px;
  #         color: @textcolor3;
  #         padding: 1px 10px 0px 10px;
  #         margin: 2px 1px 3px 1px;
  #         transition: all 0.3s ease-in-out;
  #     }

  #     #battery.critical:not(.charging) {
  #         background-color: @warn;
  #         color: @textcolor3;
  #         animation-name: blink;
  #         animation-duration: 0.5s;
  #         animation-timing-function: linear;
  #         animation-iteration-count: infinite;
  #         animation-direction: alternate;
  #     }

  #     #tray {
  #         background-color: #2980b9;
  #     }

  #     #tray > .passive {
  #         -gtk-icon-effect: dim;
  #     }

  #     #tray > .needs-attention {
  #         -gtk-icon-effect: highlight;
  #         background-color: #eb4d4b;
  #     }

  #     label:focus {
  #         background-color: #000000;
  #     }
  #   '';
}
