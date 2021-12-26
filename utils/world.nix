{ writeShellScriptBin, writeStrictShellScriptBin, nix-linter, pixiecore, gnugrep, gnused, findutils }:

let

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
    export PATH=${nix-linter}/bin:${gnugrep}/bin:${gnused}/bin:${findutils}/bin:$PATH
    _WORLD_HELP=''${_WORLD_HELP:-}
    if [ -n "$_WORLD_HELP" ]; then
      echo lint all nix files
      exit 0
    fi
    shopt -s globstar
    # shellcheck disable=SC2016
    lintout="$(mktemp lintout.XXXXXXX)"
    trap 'rm -f $lintout' EXIT
    nix-linter -W no-FreeLetInFunc -W no-SetLiteralUpdate ./**/*.nix | \
      grep -v 'Unused argument `final`' | \
      grep -v 'Unused argument `prev`' | \
      grep -Ev 'Unused argument `plugins` at \.\/packages\/.*' | \
      grep -Ev 'Unused argument `isNixOS` at \.\/packages\/.*' | \
      grep -Ev 'Unused argument `enableXWayland` at \.\/packages\/.*' | \
      grep -Ev 'Unused argument `inputs` at \.\/packages\/.*'
      > "$lintout"
    cat "$lintout"
    if [ -s "$lintout" ]; then
      exit 1
    fi
  '';

in

{ inherit pixieboot lint; }
