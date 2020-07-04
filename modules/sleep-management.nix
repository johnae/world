{ config, lib, ... }:

with lib;
let

  cfg = config.sleepManagement;

in
{

  options = {

    sleepManagement = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description =
          ''
            Whether to enable sleep management. This enables managing sleep specifically
            rather than the more coarse-grained powerManagement options.
          '';
      };

      wakeCommands = mkOption {
        type = types.lines;
        default = "";
        description = "Commands executed after the system resumes from any sleep state including hibernation.";
      };

      sleepCommands = mkOption {
        type = types.lines;
        default = "";
        description = "Commands executed before the system enters any sleep state including hibernation.";
      };

    };

  };


  config = mkIf cfg.enable {

    systemd.services.sleep-mgmt-before-sleep =
      {
        description = "Pre-Sleep Actions";
        wantedBy = [ "sleep.target" ];
        before = [ "sleep.target" ];
        script =
          ''
            ${cfg.sleepCommands}
          '';
        serviceConfig.Type = "oneshot";
      };

    systemd.services.sleep-mgmt-post-sleep =
      {
        description = "Post-Sleep Actions";
        after = [ "sleep.target" ];
        wantedBy = [ "sleep.target" ];
        script =
          ''
            ${cfg.wakeCommands}
          '';
        serviceConfig.Type = "oneshot";
      };

  };

}
