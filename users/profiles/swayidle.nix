{pkgs, ...}: let
  swaylockEffects = pkgs.writeShellApplication {
    name = "swaylock-effects";
    runtimeInputs = [pkgs.swaylock-effects];
    text = ''
      exec swaylock \
       --screenshots \
       --indicator-radius 100 \
       --indicator-thickness 7 \
       --effect-blur 15x3 \
       --effect-greyscale \
       --ring-color ffffff \
       --ring-clear-color baffba \
       --ring-ver-color bababa \
       --ring-wrong-color ffbaba \
       --key-hl-color bababa \
       --line-color ffffffaa \
       --inside-color ffffffaa \
       --inside-ver-color bababaaa \
       --line-ver-color bababaaa \
       --inside-clear-color baffbaaa \
       --line-clear-color baffbaaa \
       --inside-wrong-color ffbabaaa \
       --line-wrong-color ffbabaaa \
       --separator-color 00000000 \
       --grace 2 \
       --fade-in 0.2
    '';
  };
in {
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 180;
        command = "${swaylockEffects}/bin/swaylock-effects";
      }
      # {
      #   timeout = 300;
      #   command = "${pkgs.niri}/bin/niri"
      # }
      #
    ];
    events = [
      {
        event = "before-sleep";
        command = "${swaylockEffects}/bin/swaylock-effects";
      }
      {
        event = "lock";
        command = "${swaylockEffects}/bin/swaylock-effects";
      }
    ];
  };
}
