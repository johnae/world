{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.supertonic-tts;
  inherit (lib) mkIf mkOption mkEnableOption types concatStringsSep;
in {
  options.services.supertonic-tts = {
    enable = mkEnableOption "Supertonic text-to-speech for Home Assistant";

    port = mkOption {
      type = types.port;
      default = 10202;
      description = "Wyoming port Home Assistant connects to.";
    };

    voice = mkOption {
      type = types.enum ["F1" "F2" "F3" "F4" "F5" "M1" "M2" "M3" "M4" "M5"];
      default = "F5";
      description = "Preset style used when a request names no voice.";
    };

    language = mkOption {
      type = types.str;
      default = "sv";
      description = "One of the 31 languages Supertonic 3 covers.";
    };

    speed = mkOption {
      type = types.float;
      default = 1.05;
      description = "Speaking rate. The SDK's own default is 1.05.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the wyoming port. Not needed when Home Assistant reaches it over tailscale.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.supertonic-tts = {
      description = "Wyoming text-to-speech via Supertonic 3";
      wantedBy = ["multi-user.target"];
      after = ["network-online.target"];
      wants = ["network-online.target"];

      serviceConfig = {
        ExecStart = concatStringsSep " " [
          (lib.getExe pkgs.wyoming-supertonic)
          "--uri tcp://0.0.0.0:${toString cfg.port}"
          "--voice ${cfg.voice}"
          "--language ${cfg.language}"
          "--speed ${toString cfg.speed}"
        ];
        Restart = "on-failure";
        RestartSec = 5;
        DynamicUser = true;
        StateDirectory = "supertonic-tts";
        ## The SDK pulls its ONNX graphs from Hugging Face on first run and
        ## caches them under HOME.
        Environment = [
          "HF_HOME=/var/lib/supertonic-tts"
          "HOME=/var/lib/supertonic-tts"
        ];
        ## Same ordering problem the ASR service hit: StateDirectory does not
        ## order the unit against the impermanence bind mount.
        RequiresMountsFor = "/var/lib/private/supertonic-tts";
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [cfg.port];

    environment.persistence."/keep".directories = lib.mkIf config.ephemeralRoot [
      "/var/lib/private/supertonic-tts"
    ];
  };
}
