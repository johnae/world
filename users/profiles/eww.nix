{pkgs, ...}: let
  eww = pkgs.eww-wayland;
  ewwbin = "${eww}/bin/eww";
  volumeListener = pkgs.writeShellApplication {
    name = "volume-listener";
    text = ''
      PATH=${pkgs.pulseaudio}/bin:${pkgs.pamixer}/bin:$PATH
      pamixer --get-volume
      # shellcheck disable=SC2034
      pactl subscribe | grep --line-buffered "sink" | while read -r line; do
        pamixer --get-volume
      done
    '';
  };

  setVolume = pkgs.writeShellApplication {
    name = "set-volume";
    text = ''
      PATH=${pkgs.pamixer}/bin:$PATH
      pamixer --set-volume "$(printf '%.0f' "$1")"
    '';
  };

  rivertags = pkgs.writeShellApplication {
    name = "rivertags";
    text = ''
      toyuck() {
          active="$1"
          shift
          printf "(box :orientation \"h\" :class \"tags\" :space-evenly false :spacing 0 "
          for ws in "$@"; do
              tags=$((1 << (ws - 1)))
              if [ "$ws" = "$active" ]; then
                printf "(button :onclick \"${pkgs.river}/bin/riverctl set-focused-tags %s\" :class \"wsbutton wsactive\" \"$ws\")" "$tags"
              else
                printf "(button :onclick \"${pkgs.river}/bin/riverctl set-focused-tags %s\" :class \"wsbutton\" \"$ws\")" "$tags"
              fi
          done
          printf ")\n"
      }

      while read -r state;
      do
          tags="$(echo "$state" | ${pkgs.jq}/bin/jq -r '"\(.tags[0].Unknown[0]) \([.viewstag[0].Unknown[], .tags[0].Unknown[]] | unique | join(" "))"' 2>/dev/null)"
          # shellcheck disable=SC2086
          toyuck $tags
      done < <(${pkgs.ristate}/bin/ristate -vt -t -w)
    '';
  };

  ewwYuck = pkgs.writeText "eww.yuck" ''
    (defvar wifi_reveal false)

    (defwindow bar
               :monitor 0
               :geometry (geometry :x "0px"
                                   :y "0px"
                                   :width "100%"
                                   :height "30px"
                                   :anchor "top center")
               :stacking "fg"
               :exclusive true
      (bar))

    (defwidget bar []
      (centerbox :orientation "h"
        (tags)
        (music)
        (right)
      )
    )

    (defwidget wifi []
      (eventbox :onhover "${ewwbin} update wifi_reveal=true"
                  :onhoverlost "${ewwbin} update wifi_reveal=false"
        (box :vexpand "false" :hexpand "false" :space-evenly "false"
             (button :class "module-wifi" :onclick "ls" " (''${round(wifi_strength,0)}%)")
             (revealer :transition "slideright"
                       :reveal wifi_reveal
                       :duration "350ms"
                       (label  :class "module_essid"
                               :text ESSID_WLAN
                       )
             )
         )
      )
     )

    ;; divide by 2 since this runs every 2 seconds, see: https://github.com/elkowar/eww/blob/master/crates/eww/src/config/inbuilt.rs#L27
    (defwidget netspeed []
      (box :class "netspeed"
           :orientation "h"
           :space-evenly false
           :halign "center"
        " ''${round(EWW_NET[default_iface].NET_DOWN / 1024 / 1024 / 2, 2)}MB/s  ''${round(EWW_NET[default_iface].NET_UP / 1024 / 1024 / 2, 2)}MB/s"))


    (defwidget music []
      (box :class "music"
           :orientation "h"
           :space-evenly false
           :halign "center"
        {music != "" ? "🎵''${music}" : ""}))


    (defwidget metric [label value onchange]
      (box :orientation "h"
           :class "metric"
           :space-evenly false
        (box :class "label" label)
        (scale :min 0
               :max 100
               :active {onchange != ""}
               :value value
               :onchange onchange)))

    (defwidget txt [value]
      (box :orientation "h"
           :class "txt"
           :space-evenly false
        (box :class "content" value)
        ))

    (defwidget tags []
      (box :orientation "h"
           :class "tags"
           :space-evenly false
           :spacing 20
           :halign "start"
           :valign "center"
           (literal :content rivertags)
      )
    )

    (defwidget right []
      (box :class "right"
           :orientation "h"
           :space-evenly false
           :spacing 20
           :halign "end"
        (metric :label "Volume" :value volume :onchange "${setVolume}/bin/set-volume {}")
        (netspeed)
        (wifi)
        (label :text " ''${round(EWW_CPU.avg, 0)}% ")
        time)
    )

    (defpoll time :interval "10s"
      "date '+%Y-%m-%d %H:%M'")

    (defpoll wifi_strength :interval "15s" "tail -n1 /proc/net/wireless | ${pkgs.gawk}/bin/awk '{ if($4 < -90) print 0; else if($4 > -30) print 100; else print (-0.011*$4*$4)-(0.3794*$4)+98.182;}'")

    (defpoll default_iface :interval "15s" "${pkgs.gawk}/bin/awk '$2 == 00000000 { print $1 }' /proc/net/route")

    (defpoll ESSID_WLAN :interval "1m" "${pkgs.iwd}/bin/iwctl station wlan0 show | ${pkgs.gnugrep}/bin/grep 'Connected network' | ${pkgs.gawk}/bin/awk '{print $NF}'")

    (deflisten rivertags :initial ""
      "${rivertags}/bin/rivertags || true")

    (deflisten volume :initial "0"
      "${volumeListener}/bin/volume-listener || true")

    (deflisten music :initial ""
      "${pkgs.playerctl}/bin/playerctl --follow metadata --format '{{ artist }} - {{ title }}' || true")
  '';

  ewwScss = pkgs.writeText "eww.scss" ''
    * {
      all: unset; //Unsets everything so you can style everything from scratch
    }

    .bar {
        font-family: "Roboto Mono, Font Awesome 5 Free, Font Awesome 5 Brands, Arial, sans-serif";
        background: rgba(0, 0, 0, 0.6);
        font-size: 18px;
        min-height: 30px;
    }

    .tags {
        .wsbutton {
          padding: 0 8px;
          background: transparent;
          color: #bababa;
          border-top: 2px solid transparent;
        }
        .wsactive {
          color: white;
          background: transparent;
          border-top: 2px solid #c9545d;
        }
    }

    .bar slider {
      all: unset;
      color: #ffd5cd;
    }

    .metric scale trough highlight {
      all: unset;
      background-color: #D35D6E;
      color: #000000;
      border-radius: 10px;
    }

    .metric scale trough {
      all: unset;
      background-color: #4e4e4e;
      border-radius: 50px;
      min-height: 3px;
      min-width: 50px;
      margin-left: 10px;
      margin-right: 20px;
    }

    .metric scale trough highlight {
      all: unset;
      background-color: #D35D6E;
      color: #000000;
      border-radius: 10px;
    }

    .metric scale trough {
      all: unset;
      background-color: #4e4e4e;
      border-radius: 50px;
      min-height: 3px;
      min-width: 50px;
      margin-left: 10px;
      margin-right: 20px;
    }

  '';

  ewwConf = pkgs.linkFarm "ewwConf" [
    {
      name = "eww.yuck";
      path = ewwYuck;
    }
    {
      name = "eww.scss";
      path = ewwScss;
    }
  ];
in {
  programs.eww = {
    enable = true;
    package = eww;
    configDir = ewwConf;
  };

  #systemd.user.services.eww = {
  #  Unit = {
  #    Description = "Elkowars Wacky Widgets.";
  #    Documentation = "https://elkowar.github.io/eww/eww.html";
  #    PartOf = ["graphical-session.target"];
  #    After = ["graphical-session.target"];
  #  };

  #  Service = {
  #    Type = "oneshot";
  #    RemainAfterExit = false;
  #    ExecStart = "${eww}/bin/eww open bar";
  #    ExecStop = "${eww}/bin/eww kill";
  #  };

  #  Install = {WantedBy = ["graphical-session.target"];};
  #};
}
