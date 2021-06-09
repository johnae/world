{ stdenv, lib, bash, gnugrep, socat, mkStrictShellScript }:

mkStrictShellScript {
  name = "netns-dbus-proxy";
  src = ./netns-dbus-proxy.sh;
  substitutions = { inherit bash gnugrep socat; };
}
