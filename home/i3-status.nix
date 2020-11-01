{ pkgs, config, lib, options }:
let

  checkNixosVersion = pkgs.writeStrictShellScriptBin "check-nixos-version" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.pass}/bin:${pkgs.dateutils}/bin:${pkgs.jq}/bin:${pkgs.curl}/bin:$PATH

    latest_release="$(mktemp /tmp/nixos-latest-version.XXXXXXXXXXX)"
    trap 'rm -f "$latest_release"; exit' EXIT
    cat<<'GQL' | tr '\n' ' ' | curl -u johnae:"$(pass show web/github.com/token)" -X POST \
                                    -H "Content-Type: application/json" -d @- https://api.github.com/graphql | \
                                    jq -r '.data.repository.ref.target.history.nodes[0] | "\(.oid) \(.committedDate)"' > "$latest_release"
    { "query": "{
      repository(name: \"nixpkgs\", owner: \"nixos\") {
        ref(qualifiedName: \"nixos-unstable\") {
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

    latest_version="$(awk '{print $1}' < "$latest_release")"
    last_modified="$(awk '{print $2}' < "$latest_release")"

    age="$(ddiff -f '%d' "$last_modified" now)"

    if [ "$age" = "1" ]; then
        age="$age day ago"
    else
        age="$age days ago"
    fi

    # shellcheck disable=SC1091
    source /etc/os-release
    local_version=$(echo "$VERSION_ID" | awk -F'.' '{print $3}')

    short_latest="$(echo "$latest_version" | cut -c1-11)"

    if [ "$local_version" != "$short_latest" ]; then
      echo "NixOS:  $short_latest ($age)"
    else
      echo "NixOS:  $short_latest ($age)"
    fi
  '';

  checkConfigurationVersion = pkgs.writeStrictShellScriptBin "check-configuration-version" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.git}/bin:${pkgs.jq}/bin:$PATH
    LATEST="$(git ls-remote https://github.com/johnae/world master | awk '{print $1}' | cut -c-11)"
    LOCAL="$(nixos-version --json | jq -r .configurationRevision | cut -c-11)"
    if [ "$LOCAL" != "$LATEST" ]; then
      echo "Config:  $LATEST"
    else
      echo "Config:  $LATEST"
    fi
  '';

  vpnStatus = pkgs.writeStrictShellScriptBin "vpn-status" ''
    VPN_EXIT="$(${pkgs.curl}/bin/curl -m 5 --connect-timeout 5 -sS https://am.i.mullvad.net/json | \
                ${pkgs.jq}/bin/jq -r .mullvad_exit_ip_hostname)"
    if [ "$VPN_EXIT" = "null" ]; then
      echo " vpn: down"
    else
      echo " vpn: $VPN_EXIT"
    fi
  '';

  wifiStatus = pkgs.writeStrictShellScriptBin "wifi-status" ''
    ${pkgs.iwd}/bin/iwctl station wlan0 get-networks rssi-dbms | \
    ${pkgs.gnugrep}/bin/grep -E '>' | sed "s,\x1B\[[0-9;]*[a-zA-Z],,g" | \
    ${pkgs.gawk}/bin/awk '{ dbms=$4/100 ; signal_strength = (-0.0154 * dbms * dbms) - (0.3794 * dbms) + 98.182 ; printf "%s %1.0f%%", $2, signal_strength}'
  '';
in
{
  programs.i3status-rust = {
    enable = true;
    settings.block = [

      {
        block = "custom";
        interval = 600;
        command = "${checkNixosVersion}/bin/check-nixos-version";
      }

      {
        block = "custom";
        interval = 600;
        command = "${checkConfigurationVersion}/bin/check-configuration-version";
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
        interval = 5;
      }

      {
        block = "cpu";
        interval = 1;
      }

      {
        block = "backlight";
      }

      {
        block = "battery";
        interval = 10;
        format = "{percentage}% {time}";
      }

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
