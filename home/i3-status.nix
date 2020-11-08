{ pkgs, config, lib, options }:
let

  checkNixosVersion = pkgs.writeStrictShellScriptBin "check-nixos-version" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.git}/bin:${pkgs.jq}/bin:$PATH
    latest_version="$(git ls-remote https://github.com/nixos/nixpkgs nixos-unstable | awk '{print $1}' | cut -c-11)"
    # shellcheck disable=SC1091
    source /etc/os-release
    local_version=$(echo "$VERSION_ID" | awk -F'.' '{print $3}')
    if [ "$local_version" != "$latest_version" ]; then
      echo "NixOS:  $latest_version"
    else
      echo "NixOS:  $latest_version"
    fi
  '';

  checkConfigurationVersion = pkgs.writeStrictShellScriptBin "check-configuration-version" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.git}/bin:${pkgs.jq}/bin:$PATH
    latest_version="$(git ls-remote https://github.com/johnae/world master | awk '{print $1}' | cut -c-11)"
    local_version="$(nixos-version --json | jq -r .configurationRevision | cut -c-11)"
    if [ "$local_version" != "$latest_version" ]; then
      echo "Config:  $latest_version"
    else
      echo "Config:  $latest_version"
    fi
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
