{
  stdenv,
  blur,
  bash,
  jq,
  grim,
  sway,
  swaylock,
  mkStrictShellScript,
  lib,
}:
mkStrictShellScript {
  name = "swaylock-dope";
  src = ./swaylock-dope;
  substitutions = {inherit stdenv blur bash jq grim sway swaylock;};
  meta = {
    platforms = lib.platforms.linux;
  };
}
