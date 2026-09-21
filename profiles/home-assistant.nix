{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) head splitString;
  cfg = config.services.home-assistant;
  ## Devices that can't resolve ha.9000.dev (voice satellites, webhook
  ## callbacks from IoT bridges) need a plain LAN address to talk back to.
  lanAddress = head (splitString "/" (head config.systemd.network.networks."10-wan".address));
in {
  services.home-assistant = {
    enable = true;
    configDir = "/var/lib/hass";
    openFirewallForComponents = true;

    extraComponents = [
      ## discovery and core plumbing
      "zeroconf"
      "ssdp"
      "dhcp"
      "usb"
      "mobile_app"
      "webhook"
      "stream"
      "media_source"
      "history"
      "logbook"
      "energy"
      "sun"
      "met"
      "my"
      "isal"

      ## bridges to the ecosystems we're migrating off
      "smartthings"
      "alexa_devices"
      "cast"

      ## local-first devices
      "hue"
      "samsungtv"
      "wake_on_lan"
      "dlna_dmr"
      "nuki"
      "mqtt"
      "esphome"

      ## energy
      "tibber"

      ## matter controller; thread runs on the S90D's border router
      "matter"
      "thread"

      ## voice
      "assist_pipeline"
      "conversation"
      "intent"
      "wyoming"
      "tts"
      "stt"

      "prometheus"
    ];

    customComponents = [
      pkgs.home-assistant-custom-components.local_openai
      pkgs.home-assistant-custom-components.tibber_local
      pkgs.home-assistant-custom-components.tuya_local
      pkgs.ha-easee
      pkgs.ha-kia-uvo
    ];

    config = {
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = "Europe/Stockholm";
        country = "SE";
        currency = "SEK";
        internal_url = "http://${lanAddress}:8123";
        external_url = "https://ha.9000.dev";
      };
      ## Without these HA rejects everything arriving through nginx.
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = ["127.0.0.1" "::1"];
      };
      prometheus.namespace = "hass";
      recorder.purge_keep_days = 30;
    };
  };

  ## 8123 on the LAN for satellites and bridge callbacks, mDNS and SSDP
  ## inbound so discovery finds Hue, the Samsung screens and the plugs.
  networking.firewall.allowedTCPPorts = [8123];
  networking.firewall.allowedUDPPorts = [5353 1900];

  environment.persistence."/keep".directories = [cfg.configDir];

  services.restic.backups.remote = {
    paths = [cfg.configDir];
    ## The recorder database is huge, changes constantly, and a live copy
    ## would be torn anyway. .storage holds the parts worth keeping.
    extraBackupArgs = ["--exclude=${cfg.configDir}/home-assistant_v2.db*"];
  };
}
