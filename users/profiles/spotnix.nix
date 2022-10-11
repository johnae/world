{
  hostName,
  pkgs,
  ...
}: let
  runSpotnix = pkgs.writeShellApplication {
    name = "run-spotnix";
    runtimeInputs = with pkgs; [coreutils spotnix];
    text = ''
      RUST_LOG=info \
      CLIENT_ID="$(head -1 /run/agenix/spotnix)" \
      CLIENT_SECRET="$(tail -1 /run/agenix/spotnix)" \
      REDIRECT_URI="http://localhost:8182/spotnix" \
      exec spotnix -d ${hostName} -s "$XDG_RUNTIME_DIR"/spotnix_status -i "$XDG_RUNTIME_DIR"/spotnix_input -o "$XDG_RUNTIME_DIR"/spotnix_output -r 10
    '';
  };
in {
  systemd.user.services.spotnix = {
    Unit = {
      Description = "Spotify for UNIX";
      Wants = ["spotifyd.service"];
      After = ["sway-session.target" "spotifyd.service"];
      BindsTo = "sway-session.target";
    };
    Service = {
      ExecStart = "${runSpotnix}/bin/run-spotnix";
      Restart = "always";
      RestartSec = 3;
    };
    Install = {
      WantedBy = ["sway-session.target"];
    };
  };
}
