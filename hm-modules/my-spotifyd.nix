{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.my-spotifyd;

  toConfig = generators.toINI {
    mkKeyValue = key: value:
      let
        inherit (builtins) toString typeOf;
        conversions = {
          bool = v: if v then "true" else "false";
          int = v: toString v;
          float = v: toString v;
          string = v: ''"${v}"'';
        };
        value' = conversions.${typeOf value} value;
      in
      "${key}=${value'}";
  };

  configFile = pkgs.writeText "spotifyd.conf" ''
    ${toConfig cfg.settings}
  '';

in
{
  options.services.my-spotifyd = {
    enable = mkEnableOption "SpotifyD connect";

    package = mkOption {
      type = types.package;
      default = pkgs.spotifyd;
      defaultText = literalExample "pkgs.spotifyd";
      example =
        literalExample "(pkgs.spotifyd.override { withKeyring = true; })";
      description = ''
        The <literal>spotifyd</literal> package to use.
        Can be used to specify extensions.
      '';
    };

    settings = mkOption {
      type = types.attrsOf (types.attrsOf (types.either types.str types.int));
      default = { };
      description = "Configuration for spotifyd";
      example = literalExample ''
        {
          global = {
            username = "Alex";
            password = "foo";
            device_name = "nix";
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.spotifyd = {
      Unit = {
        Description = "spotify daemon";
        Documentation = "https://github.com/Spotifyd/spotifyd";
      };

      Install.WantedBy = [ "default.target" ];

      Service = {
        ExecStart =
          "${cfg.package}/bin/spotifyd --no-daemon --config-path ${configFile}";
        Restart = "always";
        RestartSec = 12;
      };
    };
  };
}
