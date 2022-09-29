{
  bash,
  gnugrep,
  socat,
  writeShellApplication,
  lib,
}:
writeShellApplication {
  name = "netns-dbus-proxy";
  text = builtins.readFile ./netns-dbus-proxy.sh;
  runtimeInputs = [bash gnugrep socat];
}
