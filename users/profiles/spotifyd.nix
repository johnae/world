{ pkgs, config, lib, options, ... }:
let
  home = config.home;
in
{
  services.spotifyd = {
    enable = true;
    settings = rec {
      global = rec {
        username = "binx";
        password_cmd = "${pkgs.rbw}/bin/rbw get spotify.com ${username}";
        backend = "pulseaudio";
        mixer = "PCM";
        volume-control = "alsa";
        device_name = home.extraConfig.hostname;
        bitrate = 320;
        cache_path = "/home/${home.username}/.cache/spotifyd";
        volume-normalisation = "true";
        normalisation-pregain = -10;
      };
    };
  };
  systemd.user.services.spotifyd = {
    Install.WantedBy = lib.mkForce [ "sway-session.target" ];
    Unit.PartOf = [ "spotnix.service" ];
  };
}
