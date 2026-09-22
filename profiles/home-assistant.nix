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

      ## energy. No local counterpart: the Pulse P1 sold in Sweden talks only
      ## to Tibber's cloud, and the LAN integrations all read a Pulse *Bridge*,
      ## which the P1 doesn't have. Local per-phase current needs its own meter.
      "tibber"

      ## matter controller; thread runs on the S90D's border router
      "matter"
      "thread"

      ## voice; google_translate backs the TTS entry onboarding creates, until
      ## piper takes over
      "wyoming"
      "google_translate"

      ## the music-assistant server on this host. Installs its client library;
      ## the server advertises _mass._tcp so discovery offers the config entry.
      "music_assistant"

      "met"
      "isal"
      "prometheus"
    ];

    customComponents = [
      pkgs.home-assistant-custom-components.local_openai
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

      ## Home Assistant's own editors write these three files, and nothing
      ## reads them unless configuration.yaml says so - default_config pulls in
      ## neither automation, script nor scene. Without these a script built in
      ## the UI lands on disk and silently never becomes an entity, which also
      ## means the conversation agent never gets it as a tool.
      automation = "!include automations.yaml";
      ## Labelled domain keys: `cv.domain_key` splits a top-level key on its
      ## first space, so both of these are the `script` domain and get merged.
      ## Scripts that belong in version control live here; the ones built in
      ## the UI keep their own file.
      "script manual" = {
        vaderprognos = {
          alias = "Väderprognos kommande dagar";
          description = "Väderprognos för de kommande dagarna. Anropa alltid detta när någon frågar om vädret framåt i tiden - om det kommer att regna, snöa, blåsa, bli varmare eller kallare någon av de närmaste dagarna. Svarar per dygn med datum, väderläge, högsta och lägsta temperatur samt nederbörd i millimeter. GetLiveContext ger bara vädret just nu och kan inte besvara frågor om framtiden.";
          mode = "single";
          sequence = [
            {
              action = "weather.get_forecasts";
              target.entity_id = "weather.forecast_hem";
              data.type = "daily";
              response_variable = "prognos";
            }
            {
              stop = "";
              response_variable = "prognos";
            }
          ];
        };

        nyheter = {
          alias = "Senaste nyheterna";
          description = "De senaste nyheterna från Omni. Anropa detta när någon frågar vad som har hänt, vad som är på gång i världen eller vill veta dagens nyheter. Svarar med rubrik, ingress och tidpunkt för var och en.";
          mode = "single";
          fields.antal = {
            description = "Hur många nyheter som ska hämtas.";
            default = 5;
            selector.number = {
              min = 1;
              max = 10;
              mode = "box";
            };
          };
          sequence = [
            {
              variables.svar.nyheter = ''
                {% set items = (state_attr('sensor.omni', 'item') or [])[:antal | int(5)] %}
                {% set ns = namespace(out = []) %}
                {% for i in items %}
                  {% set ns.out = ns.out + [{'rubrik': i.title, 'ingress': i.description, 'tid': i.pubDate}] %}
                {% endfor %}
                {{ ns.out }}
              '';
            }
            {
              stop = "";
              response_variable = "svar";
            }
          ];
        };

        las_upp_nyheterna = {
          alias = "Läs upp nyheterna";
          description = "Läser upp de senaste nyhetsrubrikerna högt på en högtalare. Anropa detta när någon vill höra nyheterna uppspelade i ett rum i stället för att få dem som svar.";
          mode = "single";
          fields = {
            hogtalare = {
              description = "Entitets-id för högtalaren nyheterna ska läsas upp på, till exempel media_player.kok.";
              required = true;
              selector.entity.filter.domain = "media_player";
            };
            antal = {
              description = "Hur många rubriker som ska läsas upp.";
              default = 5;
              selector.number = {
                min = 1;
                max = 10;
                mode = "box";
              };
            };
          };
          sequence = [
            {
              action = "tts.speak";
              target.entity_id = "tts.piper";
              data = {
                media_player_entity_id = "{{ hogtalare }}";
                ## Named explicitly: the wyoming tts entity has no default
                ## options, so leaving this out sends no voice at all and
                ## piper answers in whatever its --voice flag happens to be.
                options.voice = "sv_SE-alma-medium";
                message = ''
                  Senaste nytt från Omni.
                  {{ (state_attr('sensor.omni', 'item') or [])[:antal | int(5)]
                     | map(attribute='title') | join('. ') }}.
                '';
              };
            }
          ];
        };
      };
      "script ui" = "!include scripts.yaml";
      scene = "!include scenes.yaml";

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
      ## Omni publishes no documented API, but rss.xml is live and carries the
      ## full item list. The rest platform runs XML through xmltodict before
      ## templating, so the feed arrives as plain nested data.
      sensor = [
        {
          platform = "rest";
          name = "Omni";
          resource = "https://omni.se/rss.xml";
          scan_interval = 900;
          ## The state is capped at 255 characters, so it carries the count and
          ## the articles themselves ride along as an attribute.
          value_template = "{{ value_json.rss.channel.item | count }}";
          json_attributes_path = "$.rss.channel";
          json_attributes = ["item"];
        }
      ];

      prometheus.namespace = "hass";
      recorder = {
        purge_keep_days = 30;
        ## ~200 kB of headlines every 15 minutes, none of it worth a history.
        exclude.entities = ["sensor.omni"];
      };
    };
  };

  ## default_config brings the bluetooth integration along, which talks to
  ## bluetoothd over d-bus. Without bluez there is no such service, so the
  ## adapter just sits there unused and HA logs a permissions error about it
  ## on every start.
  hardware.bluetooth.enable = true;

  ## The Nuki Ultra rejects long MQTT passwords with a bare "error code 89"
  ## and no hint that length is the problem; the stock `alnum` generator's 48
  ## characters trip it. 20 alphanumerics is still ~119 bits.
  age.generators.alnum20 = {pkgs, ...}: "${pkgs.pwgen}/bin/pwgen -s 20 1";

  age.secrets = {
    mosquitto-hass = {
      rekeyFile = ../secrets/${hostName}/mosquitto-hass.age;
      generator.script = "alnum";
    };
    mosquitto-nuki = {
      rekeyFile = ../secrets/${hostName}/mosquitto-nuki.age;
      generator.script = "alnum20";
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

  ## `!include` of a file that isn't there stops Home Assistant from starting,
  ## and the editors only create these the first time something is saved. Seed
  ## them empty; tmpfiles leaves a file that already exists alone.
  systemd.tmpfiles.settings."10-home-assistant" = let
    seed = argument: {
      f = {
        inherit argument;
        user = "hass";
        group = "hass";
        mode = "0644";
      };
    };
  in {
    "${cfg.configDir}/automations.yaml" = seed "[]";
    "${cfg.configDir}/scripts.yaml" = seed "{}";
    "${cfg.configDir}/scenes.yaml" = seed "[]";
  };

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
