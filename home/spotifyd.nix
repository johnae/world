{ pkgs, config, lib, options }:

{
  services.spotifyd = {
    enable = true;
    settings = rec {
      global = rec {
        username = "binx";
        password_cmd = "${pkgs.pass}/bin/pass show web/spotify.com/${username}";
        backend = "pulseaudio";
        mixer = "PCM";
        volume-control = "alsa";
        device_name = lib.removeSuffix "\n" (builtins.readFile /etc/hostname);
        bitrate = "320";
        cache_path = "${builtins.getEnv "HOME"}/.cache/spotifyd";
        volume-normalisation = "true";
        normalisation-pregain = "-10";
      };
    };
  };
  systemd.user.services.spotifyd = {
    Install.WantedBy = lib.mkForce [ "sway-session.target" ];
  };
}
