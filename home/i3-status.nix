{ pkgs, config, lib, options, ... }:
let

  home = config.home;

  isDesktop = home.extraConfig.hostname == "eris";
  isLaptop = !isDesktop;

  ## add the days ago thingie later
  githubGQLCheck = owner: repo: branch: pkgs.writeStrictShellScriptBin "check" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.pass}/bin:${pkgs.dateutils}/bin:${pkgs.jq}/bin:${pkgs.curl}/bin:${pkgs.gawk}/bin:$PATH
    latest_release="$(mktemp /tmp/${owner}-${repo}-${branch}-latest-version.XXXXXXXXXXX)"
    trap 'rm -f "$latest_release"; exit' EXIT
    cat<<GQL | tr '\n' ' ' | curl -u johnae:"$(pass show web/github.com/token)" -X POST \
                                    -H "Content-Type: application/json" -d @- https://api.github.com/graphql | \
                                    jq -r '.data.repository.ref.target.history.nodes[0] | "\(.oid) \(.committedDate)"' > "$latest_release"
    { "query": "{
      repository(name: \"${repo}\", owner: \"${owner}\") {
        ref(qualifiedName: \"${branch}\") {
          target {
            ... on Commit {
              history(first: 1) {
                nodes {
                  oid
                  committedDate
                }
              }
            }
          }
        }
      }
    }"}
    GQL

    awk '{print $1}' < "$latest_release"
  '';

  upToDateCheck = pkgs.writeStrictShellScriptBin "up-to-date-check" ''
    latest_os_version="$(${githubGQLCheck "nixos" "nixpkgs" "nixos-unstable"}/bin/check)"
    latest_config_version="$(${githubGQLCheck "johnae" "world" "main"}/bin/check)"

    local_config_version="$(nixos-version --json | jq -r .configurationRevision)"
    local_os_version="$(nixos-version --json | jq -r .nixpkgsRevision)"

    short_config_version="$(echo "$latest_config_version" | cut -c-11)"
    if [ "$local_config_version" != "$latest_config_version" ]; then
      conf_status=" $short_config_version"
    else
      conf_status=" $short_config_version"
    fi

    short_os_version="$(echo "$latest_os_version" | cut -c-11)"
    if [ "$local_os_version" != "$latest_os_version" ]; then
      os_status=" $short_os_version"
    else
      os_status=" $short_os_version"
    fi

    echo "OS: $os_status / Config: $conf_status"
  '';

  vpnStatus = pkgs.writeStrictShellScriptBin "vpn-status" ''
    vpn_exit="$(${pkgs.curl}/bin/curl -m 5 --connect-timeout 5 -sS https://am.i.mullvad.net/json | \
                ${pkgs.jq}/bin/jq -r .mullvad_exit_ip_hostname)"
    if [ "$vpn_exit" = "null" ]; then
      echo " vpn: down"
    else
      echo " vpn: $vpn_exit"
    fi
  '';

  wifiStatus = pkgs.writeStrictShellScriptBin "wifi-status" ''
    ${pkgs.iwd}/bin/iwctl station wlan0 get-networks rssi-dbms | \
    ${pkgs.gnugrep}/bin/grep -E '>' | sed "s,\x1B\[[0-9;]*[a-zA-Z],,g" | \
    ${pkgs.gawk}/bin/awk '{ dbms=$4/100 ; signal_strength = (-0.0154 * dbms * dbms) - (0.3794 * dbms) + 98.182 ; printf "%s %1.0f%%", $2, signal_strength}'
  '';
in
{
  programs.my-i3status-rust = {
    enable = true;
    settings.block = [

      {
        block = "custom";
        interval = 600;
        command = "${upToDateCheck}/bin/up-to-date-check";
      }

      {
        block = "custom";
        interval = 60;
        command = "${vpnStatus}/bin/vpn-status";
      }

      ## this enables us to get the signal strength regardless
      ## of network namespace - via iwd/iwctl (and dbus)
      {
        block = "custom";
        interval = 60;
        command = "${wifiStatus}/bin/wifi-status";
      }
    ] ++

    (if isDesktop then
      [{
        block = "temperature";
        collapsed = false;
        interval = 10;
        format = "{average}° avg";
        #chip = "*-isa-*";
        inputs = [ "Tdie" ];
      }]
    else
      [ ]
    )
    ++
    [

      ## ssid/signal_strength won't work
      ## when in private mode (eg. only wireguard
      ## interfaces in namespace)
      {
        block = "net";
        device = "wlan0";
        ssid = false;
        signal_strength = false;
        speed_up = true;
        graph_up = false;
        interval = 30;
      }

      {
        block = "cpu";
        interval = 1;
      }

    ]
    ++
    (if isLaptop then
      [
        {
          block = "backlight";
        }

        {
          block = "battery";
          interval = 30;
          format = "{percentage}% {time}";
        }

      ]
    else [ ]
    )
    ++ [

      {
        block = "sound";
      }

      {
        block = "time";
        interval = 1;
        format = "%b-%d %H:%M:%S";
      }

    ];
  };
}
