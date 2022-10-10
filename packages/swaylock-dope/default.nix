{
  stdenv,
  blur,
  bash,
  jq,
  grim,
  coreutils,
  findutils,
  sway,
  swaylock,
  writeShellApplication,
  lib,
}:
writeShellApplication {
  name = "swaylock-dope";
  text = builtins.readFile ./swaylock-dope;
  runtimeInputs = [blur jq grim coreutils findutils sway swaylock];
}
