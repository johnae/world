{ buildEnv, writeShellScriptBin, writeStrictShellScriptBin, nix-linter, pixiecore, gnugrep, gnused, findutils, hostname }:

let

  world-pixieboot = writeStrictShellScriptBin "world-pixieboot" ''
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

  world-repl = writeStrictShellScriptBin "world-repl" ''
    export PATH=${hostname}/bin:$PATH
    _WORLD_HELP=''${_WORLD_HELP:-}
    if [ -n "$_WORLD_HELP" ]; then
      echo start a nix repl in host context
      exit 0
    fi
    host="$(hostname)"
    trap 'rm -f ./nix-repl.nix' EXIT
    cat<<EOF>./nix-repl.nix
    (builtins.getFlake (toString ./.)).nixosConfigurations.$host
    EOF
    nix repl ./nix-repl.nix
  '';

  world-help = writeStrictShellScriptBin "world-help" ''
    export PATH=${gnugrep}/bin:${gnused}/bin:${findutils}/bin:$PATH
    _WORLD_HELP=''${_WORLD_HELP:-}
    if [ -n "$_WORLD_HELP" ]; then
      echo this help text
      exit 0
    fi
    cat<<HELP
      Available sub commands:

    HELP
    export _WORLD_HELP=yes
    # shellcheck disable=SC2086
    for cmd in $(printf '%s\n' ''${PATH//:/\/* } | xargs -n 1 basename | \
      grep -E '^world-' | sed 's|world-||g'); do
    # shellcheck disable=SC1087
      cat<<HELP
          $cmd
             - $(world-$cmd)
    HELP
    done
  '';

  world-lint = writeShellScriptBin "world-lint" ''
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

  world = writeShellScriptBin "world" ''
    cmd=''${1:-help}
    if [ "$cmd" = "help" ]; then
      ${world-help}/bin/world-help
      exit 0
    fi
    name="$(basename "$0")"
    subcmd="$name-$cmd"
    shift
    if ! command -v "$subcmd" > /dev/null; then
      echo Unknown command "\"$cmd\""
      ${world-help}/bin/world-help
      exit 1
    fi
    "$subcmd" "$@"
  '';

in

buildEnv {
  name = "world";
  paths = [
    world
    world-help
    world-pixieboot
    world-lint
    world-repl
  ];
}
