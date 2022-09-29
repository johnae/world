{
  bash,
  wl-clipboard,
  writeShellApplication,
  lib,
}:
writeShellApplication {
  name = "xclip";
  text = builtins.readFile ./xclip.sh;
  runtimeInputs = [bash wl-clipboard];
}
