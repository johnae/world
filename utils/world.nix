{
  writeShellScriptBin,
  writeStrictShellScriptBin,
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
  justfile = writeText "justfile" ''
    set shell := ["${nushell}/bin/nu", "-c"]

    default:
      @${just}/bin/just --list -f {{justfile()}} -d {{invocation_directory()}}

    # search for packages
    search query:
      nix search nixpkgs {{query}} --json | from json | transpose | flatten | select pname version description

    # open a shell with given packages available
    shell +args:
      nix shell (echo {{args}} | each { |it| $"nixpkgs#($it)" })

    # garbage collect the system
    gc:
      nix-collect-garbage -d

    # upgrade the system using given flake ref
    upgrade flake="github:johnae/world":
      rm -rf ~/.cache/nix/fetcher-cache-v1.sqlite*
      nixos-rebuild boot --flake {{flake}} --use-remote-sudo -L
      if (echo initrd kernel kernel-modules | all { |it| (readlink $"/run/booted-system/($it)") != (readlink $"/nix/var/nix/profiles/system/($it)") }) { echo The system must be rebooted for the changes to take effect } else { nixos-rebuild switch --flake {{flake}} --use-remote-sudo -L }
  '';

  world = writeShellScriptBin "world" ''
    ${just}/bin/just -f ${justfile} -d "$(pwd)" "$@"
  '';

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
in {inherit pixieboot lint world;}
