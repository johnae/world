{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.pasuspender;
in
{
  options.services.pasuspender = {
    enable = mkEnableOption "enable pulseaudio suspender";
  };

  config = mkIf cfg.enable {
    systemd.user.services.pasuspender = rec {
      description = "Fix PulseAudio resume after suspend";
      after = [ "suspend.target" ];
      enable = true;
      serviceConfig = { Type = "oneshot"; };
      environment = { XDG_RUNTIME_DIR = "/run/user/%U"; };
      script = ''
        ${pkgs.pulseaudioFull}/bin/pasuspender ${pkgs.coreutils}/bin/true
      '';
      wantedBy = [ "suspend.target" ];
    };
  };
}
