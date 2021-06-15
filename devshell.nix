{ writeText, writeStrictShellScriptBin, mkDevShell, ripgrep, agenix, age-plugin-yubikey, pixiecore }:

let

  world-update-cargo-vendor-sha = writeStrictShellScriptBin "world-update-cargo-vendor-sha" ''
    if [ -z "$1" ]; then
        echo "USAGE: $0 <attribute>"
        echo "EXAMPLE: $0 ripgrep"
        exit 1
    fi

    cd packages

    attr="$1"
    path="$attr"/default.nix
    sed -i 's|cargoSha256.*|cargoSha256 = "0000000000000000000000000000000000000000000000000000";|' "$path"

    log="$(mktemp nix-rustbuild-log-"$attr".XXXXXXX)"
    trap 'rm -f $log' EXIT

    nix build .#"$attr" 2>&1 | tee "$log" || true
    cargoSha256="$(grep 'got:.*sha256.*' "$log" | awk '{print $NF}')"
    echo Setting cargoSha256 for "$attr" to "$cargoSha256"
    sed -i "s|cargoSha256.*|cargoSha256 = \"$cargoSha256\";|" "$path"
  '';

  world-update-all-cargo-vendor-shas = writeStrictShellScriptBin "world-update-all-cargo-vendor-shas" ''
    for rpkg in $(${ripgrep}/bin/rg -l cargoSha256 packages/ | awk -F'/' '{print $2}'); do
      ${world-update-cargo-vendor-sha}/bin/world-update-cargo-vendor-sha "$rpkg"
    done
  '';

  world-update-fixed-output-derivation-sha = writeStrictShellScriptBin "world-update-fixed-output-derivation-sha" ''
    if [ -z "$1" ]; then
        echo "USAGE: $0 <attribute>"
        echo "EXAMPLE: $0 argocd-ui"
        exit 1
    fi

    cd packages

    attr="$1"
    path="$attr"/default.nix
    what=outputHash;
    if grep -q "vendorSha256" "$path"; then
      what="vendorSha256"
    fi

    sed -i "s|$what =.*|$what = \"0000000000000000000000000000000000000000000000000000\";|" "$path"

    log="$(mktemp nix-fixed-output-drv-log-"$attr".XXXXXXX)"
    trap 'rm -f $log' EXIT

    nix build .#"$attr" 2>&1 | tee "$log" || true
    hash="$(grep 'got:.*sha256.*' "$log" | awk '{print $NF}')"
    echo Setting "$what" for "$attr" to "$hash"
    sed -i "s|$what =.*|$what = \"$hash\";|" "$path"
  '';

  world-update-all-fixed-output-derivation-shas = writeStrictShellScriptBin "world-update-all-fixed-output-derivation-shas" ''
    for fopkg in $(${ripgrep}/bin/rg -l "outputHash|vendorSha256" packages/ | awk -F'/' '{print $2}'); do
      ${world-update-fixed-output-derivation-sha}/bin/world-update-fixed-output-derivation-sha "$fopkg"
    done
  '';

  world-pixieboot = writeStrictShellScriptBin "world-pixieboot" ''
    echo Hey, you may need to turn off the firewall for this to work
    nix build .#nixosConfigurations.pxebooter -o /tmp/netboot
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
        repl                                          -  bit of a hacky way to get a repl (flakes are experimental still)
        pixieboot                                     -  start a pxebooter where all defined hosts in this repo are installable
        update-cargo-vendor-sha <package>             -  updates the cargoSha256 of a rust package
        update-all-cargo-vendor-shas                  -  updates the cargoSha256 of all rust packages
        update-fixed-output-derivation-sha <package>  -  updates the outputHash of a fixed output drv
        update-all-fixed-output-derivation-shas       -  updates the outputHash of all fixed output drvs

    HELP
  '';

  world = writeStrictShellScriptBin "world" ''
    export PATH=${world-pixieboot}/bin:${world-update-all-cargo-vendor-shas}/bin:${world-update-all-fixed-output-derivation-shas}/bin:${world-update-cargo-vendor-sha}/bin:${world-update-fixed-output-derivation-sha}/bin:${world-help}/bin:${world-pixieboot}/bin:${world-repl}/bin:$PATH
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
  packages = [ agenix age-plugin-yubikey world ];
  intro = ''

    Hello, world!

  '';
}
