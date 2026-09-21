{
  config,
  hostName,
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
      "mqtt"
      "esphome"

      ## turned up by discovery on the LAN once default_config was in place
      "sonos"
      "brother"
      "ipp"
      "elgato"

      ## energy
      "tibber"

      ## matter controller; thread runs on the S90D's border router
      "matter"
      "thread"

      ## voice; google_translate backs the TTS entry onboarding creates, until
      ## piper takes over
      "wyoming"
      "google_translate"

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

  ## default_config brings the bluetooth integration along, which talks to
  ## bluetoothd over d-bus. Without bluez there is no such service, so the
  ## adapter just sits there unused and HA logs a permissions error about it
  ## on every start.
  hardware.bluetooth.enable = true;

  age.secrets = {
    mosquitto-hass = {
      rekeyFile = ../secrets/${hostName}/mosquitto-hass.age;
      generator.script = "alnum";
    };
    mosquitto-nuki = {
      rekeyFile = ../secrets/${hostName}/mosquitto-nuki.age;
      generator.script = "alnum";
    };
  };

  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "0.0.0.0";
        port = 1883;
        users = {
          hass = {
            passwordFile = config.age.secrets.mosquitto-hass.path;
            acl = ["readwrite #"];
          };
          nuki = {
            passwordFile = config.age.secrets.mosquitto-nuki.path;
            acl = [
              "readwrite nuki/#"
              ## The lock publishes its own discovery config here, and the
              ## prefix is hardcoded in its firmware. Without write access it
              ## connects happily and then never shows up in Home Assistant.
              "readwrite homeassistant/#"
            ];
          };
        };
      }
    ];
  };

  ## 8123 for voice satellites and webhook callbacks, 1883 for the Nuki, and
  ## mDNS/SSDP inbound so discovery finds Hue, the Samsung screens and the
  ## plugs.
  networking.firewall.allowedTCPPorts = [8123 1883];
  networking.firewall.allowedUDPPorts = [5353 1900];

  ## Ownership spelled out because impermanence creates these under /keep
  ## before the service's own `createHome` would have chowned them, and a
  ## root-owned bind mount leaves the daemon unable to write its own state.
  environment.persistence."/keep".directories = [
    {
      directory = cfg.configDir;
      user = "hass";
      group = "hass";
      mode = "0700";
    }
    {
      directory = config.services.mosquitto.dataDir;
      user = "mosquitto";
      group = "mosquitto";
      mode = "0700";
    }
  ];

  services.restic.backups.remote = {
    paths = [cfg.configDir];
    ## The recorder database is huge, changes constantly, and a live copy
    ## would be torn anyway. .storage holds the parts worth keeping.
    extraBackupArgs = ["--exclude=${cfg.configDir}/home-assistant_v2.db*"];
  };
}
