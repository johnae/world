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
      ## Everything default_config already pulls in (discovery, mobile_app,
      ## history, energy, the assist pipeline) is deliberately absent here -
      ## listing a component only installs it, `config` below is what makes
      ## Home Assistant actually set it up.

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
      "wyoming"

      "met"
      "isal"
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
      ## Does real work despite looking like boilerplate: HA only sets up a
      ## component that is a key here, is one of `defaultIntegrations`, or has
      ## a config entry. Without this, discovery (zeroconf/ssdp/dhcp/usb),
      ## mobile_app, history and the assist pipeline are installed but never
      ## started, and the companion app reports the server has no mobile_app.
      default_config = {};

      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = "Europe/Stockholm";
        country = "SE";
        currency = "SEK";
        internal_url = "http://${lanAddress}:8123";
        external_url = "https://ha.9000.dev";
      };
      ## No `http:` here on purpose. Since 2026.x HA migrates that block into
      ## .storage once, then ignores the YAML forever and raises a repair issue
      ## if it's still present (removed outright in 2027.2). Worse, the migrated
      ## config lands as a *pending* trial that an admin has to promote within
      ## five minutes or HA reverts and restarts - unpromotable on a fresh
      ## instance, where no admin exists yet. So the reverse proxy settings
      ## (use_x_forwarded_for, trusted_proxies 127.0.0.1 + ::1) are a one-time
      ## UI step after onboarding, and live in .storage from then on.
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
