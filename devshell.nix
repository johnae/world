{ writeShellScriptBin, writeStrictShellScriptBin, mkDevShell, nix-linter, agenix, age-plugin-yubikey, rage, pixiecore }:

let

  world-pixieboot = writeStrictShellScriptBin "world-pixieboot" ''
    echo Hey, you may need to turn off the firewall for this to work
    nix build .#pxebooter -o /tmp/netboot
    n="$(realpath /tmp/netboot)"
    init="$(grep -ohP 'init=\S+' "$n/netboot.ipxe")"
    sudo ${pixiecore}/bin/pixiecore boot "$n/bzImage" "$n/initrd" \
      --cmdline "$init loglevel=4" \
      --debug --dhcp-no-bind --port 64172 --status-port 64172
  '';

  world-repl = writeStrictShellScriptBin "world-repl" ''
    host="$(hostname)"
    trap 'rm -f ./nix-repl.nix' EXIT
    cat<<EOF>./nix-repl.nix
    (builtins.getFlake (toString ./.)).nixosConfigurations.$host
    EOF
    nix repl ./nix-repl.nix
  '';

  world-help = writeStrictShellScriptBin "world-help" ''
    cat<<HELP
      Hello, world! Here's some things to do:
        help                                          -  this help output
        lint                                          -  lint nix expressions
        repl                                          -  bit of a hacky way to get a repl (flakes are experimental still)
        pixieboot                                     -  start a pxebooter where all defined hosts in this repo are installable
    HELP
  '';

  world-lint = writeShellScriptBin "world-lint" ''
    shopt -s globstar
    # shellcheck disable=SC2016
    lintout="$(mktemp lintout.XXXXXXX)"
    trap 'rm -f $lintout' EXIT
    ${nix-linter}/bin/nix-linter -W no-FreeLetInFunc -W no-SetLiteralUpdate ./**/*.nix | \
      grep -v 'Unused argument `final`' | \
      grep -v 'Unused argument `prev`' | \
      grep -v 'Unused argument `plugins`' | \
      grep -v 'Unused argument `isNixOS`' \
      > "$lintout"
    cat "$lintout"
    if [ -s "$lintout" ]; then
      exit 1
    fi
  '';

  world = writeShellScriptBin "world" ''
    export PATH=${world-pixieboot}/bin:${world-help}/bin:${world-pixieboot}/bin:${world-repl}/bin:${world-lint}/bin:$PATH
    cmd=''${1:-}
    if [ -z "$cmd" ]; then
      echo Command expected
      echo
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
  '';in

mkDevShell {
  name = "world";
  packages = [ nix-linter rage agenix age-plugin-yubikey world ];
  intro = ''

    Hello, world!

  '';
}
