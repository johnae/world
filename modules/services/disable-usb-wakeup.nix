{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.disable-usb-wakeup;
in
{
  options.services.disable-usb-wakeup = {
    enable = mkEnableOption "prevent usb devices from waking up computer";
  };

  config = mkIf cfg.enable {
    systemd.services.disable-usb-wakeup = rec {
      description = "Disable USB wakeup";
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
      };
      script = ''
        echo XHC > /proc/acpi/wakeup
      '';
      postStop = ''
        echo XHC > /proc/acpi/wakeup
      '';
      wantedBy = [ "multi-user.target" ];
    };
  };
}
