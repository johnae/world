{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.user-init;
in
{
  options.services.user-init = {

    enable = mkEnableOption
      "ensures basic user initialization takes place - should be made idempotent";

    script = mkOption {
      type = types.lines;
      example = ''
        mkdir -p $HOME/Directory
      '';
    };

    target = mkOption {
      type = types.listOf types.str;
      example = [ "sway-session.target" ];
    };

  };

  config = mkIf cfg.enable {
    systemd.user.services.user-init = {
      Unit = {
        Description = "User initialization service";
        After = cfg.target;
        BindsTo = cfg.target;
      };
      Service = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart =
          let
            script = pkgs.writeStrictShellScriptBin "user-init" cfg.script;
          in
          "${script}/bin/user-init";
      };
      Install = {
        WantedBy = cfg.target;
      };
    };
  };
}
