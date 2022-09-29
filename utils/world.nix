{
  writeShellApplication,
  writeText,
  just,
  statix,
  nushell,
  pixiecore,
  gnugrep,
  gnused,
  findutils,
  buildEnv,
}: let
  world = writeShellApplication {
    name = "world";
    runtimeInputs = [just nushell];
    text = ''
      just -f ${../Justfile} -d "$(pwd)" "$@"
    '';
  };

  pixieboot = writeShellApplication {
    name = "pixieboot";
    runtimeInputs = [gnugrep pixiecore];
    text = ''
      _WORLD_HELP=''${_WORLD_HELP:-}
      if [ -n "$_WORLD_HELP" ]; then
        echo start pixiecore for automated network installation
        exit 0
      fi
      echo Hey, you may need to turn off the firewall for this to work
      nix build .#pxebooter -o /tmp/netboot
      n="$(realpath /tmp/netboot)"
      init="$(grep -ohP 'init=\S+' "$n/netboot.ipxe")"
      sudo pixiecore boot "$n/bzImage" "$n/initrd" \
        --cmdline "$init loglevel=4" \
        --debug --dhcp-no-bind --port 64172 --status-port 64172
    '';
  };

  lint = writeShellApplication {
    name = "lint";
    runtimeInputs = [statix];
    text = ''
      _WORLD_HELP=''${_WORLD_HELP:-}
      if [ -n "$_WORLD_HELP" ]; then
        echo lint all nix files
        exit 0
      fi
      shopt -s globstar
      action="''${1:-}"
      shift
      if [ -z "$action" ]; then
         statix check
      else
         statix "$action" "$@"
      fi
    '';
  };
in {inherit pixieboot lint world;}
