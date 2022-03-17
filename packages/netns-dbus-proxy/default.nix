{
  bash,
  gnugrep,
  socat,
  mkStrictShellScript,
  lib,
}:
mkStrictShellScript {
  name = "netns-dbus-proxy";
  src = ./netns-dbus-proxy.sh;
  substitutions = {inherit bash gnugrep socat;};
  meta = {
    platforms = lib.platforms.linux;
  };
}
