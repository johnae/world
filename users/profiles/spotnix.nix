{ pkgs, config, ... }:
let
  home = config.home;
in
{
  systemd.user.services.spotnix = {
    Unit = {
      Description = "Spotify for UNIX";
      Wants = [ "spotifyd.service" ];
      After = [ "sway-session.target" "spotifyd.service" ];
      BindsTo = "sway-session.target";
    };
    Service = {
      ExecStart = ''
        ${pkgs.stdenv.shell} -c 'RUST_LOG=info CLIENT_ID="$(head -1 /run/secrets/spotnix)" CLIENT_SECRET="$(tail -1 /run/secrets/spotnix)" REDIRECT_URI="http://localhost:8182/spotnix" ${pkgs.spotnix}/bin/spotnix -d ${home.extraConfig.hostName} -s $XDG_RUNTIME_DIR/spotnix_status -i $XDG_RUNTIME_DIR/spotnix_input -o $XDG_RUNTIME_DIR/spotnix_output -r 10'
      '';
      Restart = "always";
      RestartSec = 3;
    };
    Install = {
      WantedBy = [ "sway-session.target" ];
    };
  };
}
