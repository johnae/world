{ pkgs, config, lib, options }:
let
  checkNixosVersion = pkgs.writeStrictShellScriptBin "check-nixos-version" ''
    PATH=${pkgs.stdenv}/bin:${pkgs.dateutils}/bin:${pkgs.yq}/bin:${pkgs.curl}/bin:$PATH
    RELEASES="$(mktemp /tmp/nixos-version.XXXXXXXXXXX)"
    curl -sS 'https://nix-releases.s3.amazonaws.com/?delimiter=/&prefix=nixos/unstable/' | \
      xq -r '.ListBucketResult.Contents[] | "\(.Key) \(.LastModified)"' > "$RELEASES"
    trap 'rm -f "$RELEASES"; exit' EXIT

    CURRENT="$(tail -n1 "$RELEASES" | awk '{print $1}')"
    LAST_MODIFIED="$(tail -n1 "$RELEASES" | awk '{print $2}')"
    AGE_DAYS="$(ddiff -f '%d' "$LAST_MODIFIED" now)"

    if [ "$AGE_DAYS" = "1" ]; then
      AGE_DAYS="$AGE_DAYS day ago"
    else
      AGE_DAYS="$AGE_DAYS days ago"
    fi

    LATEST=$(echo "$CURRENT" | awk -F'.' '{print $NF}')
    # shellcheck disable=SC1091
    source /etc/os-release
    LOCAL=$(echo "$VERSION_ID" | awk -F'.' '{print $3}')

    if [ "$LOCAL" != "$LATEST" ]; then
      echo " $LATEST ($AGE_DAYS)"
    else
      echo " $LATEST ($AGE_DAYS)"
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
