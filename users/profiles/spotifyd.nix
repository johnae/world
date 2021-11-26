{ hostName, config, lib, pkgs, ... }:
let
  home = config.home;
in
{
  services.spotifyd = {
    enable = true;
    settings.global = {
      username = "binx";
      password_cmd = "cat /run/agenix/spotifyd";
      backend = "pulseaudio";
      mixer = "PCM";
      volume-control = "alsa";
      device_name = hostName;
      bitrate = 320;
      cache_path = "/home/${home.username}/.cache/spotifyd";
      volume-normalisation = "true";
      normalisation-pregain = -10;
    };
  };
  systemd.user.services.spotifyd = {
    Install.WantedBy = lib.mkForce [ "sway-session.target" ];
  };
  ## because it seems the spotifyd speaker is gone after a suspend/resume cycle
  systemd.user.services.spotifyd-resume-action = {
    Unit = {
      Description = "Restarts spotifyd after sleep";
      StartLimitInterval = 300;
      StartLimitBurst = 5;
    };
    Install.WantedBy = [ "suspend.target"
                         "hibernate.target"
                         "hybrid-sleep.target" ];
    Service = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = 30;
      ExecStart = "${pkgs.systemd}/bin/systemctl --user restart spotifyd.service";
    };
  };
}
