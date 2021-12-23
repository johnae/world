{ bash, wl-clipboard, mkStrictShellScript, lib }:
let
  wl-copy = "${wl-clipboard}/bin/wl-copy";
  wl-paste = "${wl-clipboard}/bin/wl-paste";
in
mkStrictShellScript {
  name = "xclip";
  src = ./xclip.sh;
  substitutions = { inherit bash wl-copy wl-paste; };
  meta = {
    license = lib.licenses.gpl3;
    platforms = lib.platforms.linux;
  };
}
