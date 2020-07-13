{ nixpkgs ? import <nixpkgs> { } }:
let
  SOPS_PGP_FP = "782517BE26FBB0CC5DA3EFE59D91E5C4D9515D9E";

  latestRelease = nixpkgs.writeStrictShellScriptBin "latest-release" ''
    export PATH=${nixpkgs.curl}/bin:${nixpkgs.jq}/bin:$PATH
    REPO=''${1:-}
    curl -sS https://api.github.com/repos/"$REPO"/releases | \
             jq -r 'map(select(.tag_name | contains("rc") | not) | select(.tag_name != null)) | max_by(.tag_name | [splits("[-.a-zA-Z+]")] | map(select(length > 0)) | map(tonumber)) | .tag_name'
  '';

  world-help = nixpkgs.writeStrictShellScriptBin "world-help" ''
    cat<<HELP
      Hello, world! Here's some things to do:

        help                                 -  this help output
        build <host>                         -  build the given host
        repl                                 -  bit of a hacky way to get a repl (flakes are experimental still)
        installer <host>                     -  build an iso image installer for the given host
        update                               -  update the current host (first build it, then update the profile and switch to new config)
        update-remote <host> [reboot]        -  update the given remote host (first build it, then update the profile remotely and switch to new config and maybe reboot)
        package <package>                    -  build a package available under nixpkgs
        update-pkgs                          -  update all packages (except those outside flake control)
        update-bin-pkg <pkgpath> <repo> <dlname> [version]
                                             -  updates a binary package, eg. one that is precompiled generally,
                                                only works with github releases
    HELP
  '';

  world-build = nixpkgs.writeStrictShellScriptBin "world-build" ''
    host="$1"
    shift
    path=.#nixosConfigurations."$host".config.system.build.toplevel
    echo Building host "$host" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix build --no-link "$@" "$path" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix path-info "$@" "$path"
  '';

  world-installer = nixpkgs.writeStrictShellScriptBin "world-installer" ''
    host="$1"
    shift
    path=.#isoConfigurations."$host".config.system.build.isoImage
    echo Building iso image for host "$host" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix build --no-link "$@" "$path" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix path-info "$@" "$path"
  '';

  world-vm-installer = nixpkgs.writeStrictShellScriptBin "world-vm-installer" ''
    export PATH=${nixpkgs.qemu}/bin:${nixpkgs.e2fsprogs}/bin:$PATH
    host="$1"
    shift
    disk="$host"-qemu.img
    if [ -e "$disk" ]; then
      echo Booting system from "$disk"
      qemu-system-x86_64 -enable-kvm -smp 2 -m 1024 -hda "$disk" \
         -drive if=pflash,format=raw,readonly,file=${nixpkgs.OVMF.fd}/FV/OVMF_CODE.fd \
         -drive if=pflash,format=raw,readonly,file=${nixpkgs.OVMF.fd}/FV/OVMF_VARS.fd \
         -smbios type=2 \
         -net user,hostfwd=tcp::10022-:22 -net nic
    else
      if [ ! -e boot.iso ]; then
        isoDrv="$(${world-installer}/bin/world-installer "$host" "$@")"
        isoPath="$isoDrv/iso/nixos-$(echo "$isoDrv" | awk -F'nixos-' '{print $2}')"
        ln -s "$isoPath" boot.iso
      else
        isoPath=boot.iso
      fi
      echo Booting iso from "$isoPath"
      qemu-img create -f qcow2 "$disk" 20G
      chattr +C "$disk"
      qemu-system-x86_64 -enable-kvm -smp 2 -boot d -cdrom "$isoPath" -m 1024 -hda "$disk" \
         -drive if=pflash,format=raw,readonly,file=${nixpkgs.OVMF.fd}/FV/OVMF_CODE.fd \
         -drive if=pflash,format=raw,readonly,file=${nixpkgs.OVMF.fd}/FV/OVMF_VARS.fd \
         -smbios type=2 \
         -net user,hostfwd=tcp::10022-:22 -net nic
    fi
  '';

  world-package = nixpkgs.writeStrictShellScriptBin "world-package" ''
    pkg="$1"
    shift
    path=.#nixpkgs."$pkg"
    echo Building package "$pkg" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix build --no-link "$@" "$path" 1>&2
    ${nixpkgs.nixFlakes}/bin/nix path-info "$@" "$path"
  '';

  world-update = nixpkgs.writeStrictShellScriptBin "world-update" ''
    host="$(${nixpkgs.hostname}/bin/hostname)"

    profile=/nix/var/nix/profiles/system

    if [ -d "${nixpkgs.inputs.secrets}/$host/root" ]; then
    roottmp="$(mktemp -d /tmp/roottmp.XXXXXXXX)"
    trap 'sudo rm - rf "$roottmp"' EXIT
    cp - a ${nixpkgs.inputs.secrets}/"$host"/root/* "$roottmp"/
    for file in $(${nixpkgs.fd}/bin/fd . --type f "$roottmp");
    do
      echo Decrypting "$file"
      ${nixpkgs.sops}/bin/sops -d -i "$file"
    done
    sudo chown -R root:root "$roottmp"/*
      sudo cp -a "$roottmp"/* /
    fi

    pathToConfig="$(${world-build}/bin/world-build "$host" "$@")"

    echo Updating system profile
    sudo -E nix-env -p "$profile" --set "$pathToConfig"

    echo Switching to new configuration
    if ! sudo "$pathToConfig"/bin/switch-to-configuration switch; then
            echo "warning: error(s) occurred while switching to the new configuration"
            exit 1
    fi
  '';

  world-update-remote = nixpkgs.writeStrictShellScriptBin "world-update-remote" ''
    host=''${1:-}
    reboot=''${2:-}
    after_update=

    if [ -z "$host" ]; then
      echo Please provide the host as the first argument
      exit 1
    fi

    if [ -n "$reboot" ]; then
      after_update="sudo shutdown -r now"
    fi

    profile=/nix/var/nix/profiles/system
    pathToConfig="$(${world-build}/bin/world-build "$host" "$@")"

    export NIX_SSHOPTS="-T -o RemoteCommand=none"

    echo Copying closure to remote
    ${nixpkgs.nixFlakes}/bin/nix-copy-closure "$host" "$pathToConfig"

    if [ -d "${nixpkgs.inputs.secrets}/$host/root" ]; then
      roottmp="$(mktemp -d /tmp/roottmp.XXXXXXXX)"
      trap 'sudo rm -rf "$roottmp"' EXIT
      cp -a ${nixpkgs.inputs.secrets}/"$host"/root/* "$roottmp"/
      for file in $(${nixpkgs.fd}/bin/fd . --type f "$roottmp"); do
        echo Decrypting "$file"
        ${nixpkgs.sops}/bin/sops -d -i "$file"
      done
      scp -r "$roottmp" "$host:roottmp"
    fi

    ## below requires sudo without password on remote, also requires an ssh config
    ## where the given hosts are configured so they can be accessed via their
    ## names
    # shellcheck disable=SC2087
    ssh "$host" -t -o RemoteCommand=none nix-shell -p bash --run bash <<SSH

    if [ -d roottmp ]; then
      sudo chown -R root:root roottmp/*
      sudo cp -a roottmp/* /
      sudo rm -rf roottmp
    fi

    sudo nix-env -p '$profile' --set '$pathToConfig'
    echo Updating system profile

    echo Switching to new configuration
    if ! sudo '$pathToConfig'/bin/switch-to-configuration switch; then
        echo "warning: error(s) occurred while switching to the new configuration" >&2
        exit 1
    fi

    $after_update

    SSH
  '';

  world-update-pkgs = nixpkgs.writeStrictShellScriptBin "world-update-pkgs" ''
    for pkg in $(${nixpkgs.jq}/bin/jq -r '.nodes | keys[] | select(. != "root")' flake.lock); do
      ${nixpkgs.nixFlakes}/bin/nix flake update --update-input "$pkg" "$@"
    done
  '';

  world-update-bin-pkg = nixpkgs.writeStrictShellScriptBin "world-update-bin-pkg" ''
    export PATH=${latestRelease}/bin:$PATH
    _pkgpath=''${1:-}
    _repo=''${2:-}
    _dlname=''${3:-}
    _version=''${4:-}
    if [ -z "$_version" ]; then
      _version="$(latest-release "$_repo")"
    fi
    _url=https://github.com/"$_repo"/releases/download/"$_version"/"$_dlname"
    cat <<EOF>"$_pkgpath"/metadata.nix
    { url = "$_url"; version = "$_version"; sha256 = "$(${nixpkgs.nixFlakes}/bin/nix-prefetch-url "$_url")"; }
    EOF
  '';

  world-repl = nixpkgs.writeStrictShellScriptBin "world-repl" ''
    host="$(${nixpkgs.hostname}/bin/hostname)"
    trap 'rm -f ./nix-repl.nix' EXIT
    cat<<EOF>./nix-repl.nix
    (builtins.getFlake (toString ./.)).nixosConfigurations.$host
    EOF
    ${nixpkgs.nixFlakes}/bin/nix repl ./nix-repl.nix
  '';

  world = nixpkgs.writeStrictShellScriptBin "world" ''
    unset NIX_PATH NIXPKGS_CONFIG
    NIXPKGS_ALLOW_UNFREE=1
    export NIXPKGS_ALLOW_UNFREE
    export PATH=${nixpkgs.git}/bin:${world-build}/bin:${world-package}/bin:${world-update}/bin:${world-update-remote}/bin:${world-update-pkgs}/bin:${world-update-bin-pkg}/bin:${world-help}/bin:${world-installer}/bin:${world-vm-installer}/bin:${world-repl}/bin:$PATH

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
  '';

in
nixpkgs.mkShell {
  buildInputs =
    [ world nixpkgs.sops nixpkgs.moreutils nixpkgs.nixFlakes ];

  inherit SOPS_PGP_FP;
  NIX_CONF_DIR =
    let
      nixConf = nixpkgs.writeTextDir "opt/nix.conf" ''
        experimental-features = nix-command flakes ca-references
        ## below allows the use of builtins.exec - for secrets decryption
        allow-unsafe-native-code-during-evaluation = true
      '';
    in
    "${nixConf}/opt";
}
