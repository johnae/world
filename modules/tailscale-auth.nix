{
  config,
  options,
  pkgs,
  lib,
  ...
}: let
  inherit
    (lib // builtins)
    types
    mkOption
    mkEnableOption
    mkIf
    concatStringsSep
    mapAttrsToList
    isBool
    isList
    toString
    mapAttrs
    ;

  tsAuth = config.services.tailscale.auth;
  tsAuthScript = pkgs.writeShellScript "tsauth" ''
    EXTRA_ARGS="--hostname $(hostname)"
    if [ -e /etc/unique-hostname ]; then
      EXTRA_ARGS="--hostname $(cat /etc/unique-hostname)"
    fi
    ${pkgs.tailscale}/bin/tailscale up ${concatStringsSep " " tsAuth.args} $EXTRA_ARGS
  '';
in {
  options.services.tailscale.auth = {
    enable = mkEnableOption "tailscale automatic auth service";
    args = mkOption {
      type = types.attrsOf (types.oneOf [(types.listOf types.str) types.str types.number types.bool]);
      apply = value:
        mapAttrsToList (name: value: "--${name}${value}") (
          mapAttrs (
            _: value:
              if isBool value
              then
                if value
                then "=true"
                else "=false"
              else if isList value
              then " ${concatStringsSep "," value}"
              else " ${toString value}"
          )
          value
        );
    };
  };

  config = mkIf tsAuth.enable {
    systemd.services.tailscale-auth = {
      description = "Tailscale automatic authentication";
      wantedBy = ["tailscaled.service"];
      after = ["tailscaled.service"];
      restartTriggers = [tsAuthScript];
      serviceConfig = {
        Type = "oneshot";
        RestartSec = 10;
        Restart = "on-failure";
        RemainAfterExit = "yes";
        ExecStart = tsAuthScript;
      };
    };
    systemd.paths.tailscale-socket = {
      wantedBy = ["tailscaled.service"];
      pathConfig = {
        PathExists = "/var/run/tailscale/tailscaled.sock";
        Unit = "tailscale-auth.service";
      };
    };
  };
}
