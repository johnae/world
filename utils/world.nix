{
  writeShellScriptBin,
  writeStrictShellScriptBin,
  statix,
  pixiecore,
  gnugrep,
  gnused,
  findutils,
}: let
  pixieboot = writeStrictShellScriptBin "pixieboot" ''
    export PATH=${gnugrep}/bin:$PATH
    _WORLD_HELP=''${_WORLD_HELP:-}
    if [ -n "$_WORLD_HELP" ]; then
      echo start pixiecore for automated network installation
      exit 0
    fi
    echo Hey, you may need to turn off the firewall for this to work
    nix build .#pxebooter -o /tmp/netboot
    n="$(realpath /tmp/netboot)"
    init="$(grep -ohP 'init=\S+' "$n/netboot.ipxe")"
    sudo ${pixiecore}/bin/pixiecore boot "$n/bzImage" "$n/initrd" \
      --cmdline "$init loglevel=4" \
      --debug --dhcp-no-bind --port 64172 --status-port 64172
  '';

  lint = writeShellScriptBin "lint" ''
    export PATH=${statix}/bin:$PATH
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
in {inherit pixieboot lint;}
