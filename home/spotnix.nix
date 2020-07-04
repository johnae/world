{ pkgs, config, lib, options }:
let
  hostname = lib.removeSuffix "\n" (builtins.readFile /etc/hostname);
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
        ${pkgs.stdenv.shell} -c 'CLIENT_ID="$(${pkgs.pass}/bin/pass web/spotify.com/spotnix | head -1)" CLIENT_SECRET="$(${pkgs.pass}/bin/pass web/spotify.com/spotnix | tail -1)" REDIRECT_URI="http://localhost:8182/spotnix" ${pkgs.spotnix}/bin/spotnix -d ${hostname} -e $XDG_RUNTIME_DIR/spotnix_event -i $XDG_RUNTIME_DIR/spotnix_input -o $XDG_RUNTIME_DIR/spotnix_output -r 10'
      '';
      Restart = "always";
      RestartSec = 3;
    };
    Install = {
      WantedBy = [ "sway-session.target" ];
    };
  };
}
