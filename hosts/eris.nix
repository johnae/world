{ hostName, userName, config, lib, pkgs, inputs, ... }:
let
  secrets = (builtins.exec [
    "${pkgs.sops}/bin/sops"
    "-d"
    "${inputs.secrets}/${hostName}/meta.nix"
  ]) { inherit pkgs userName; };

  sshExtraConfig =
    let
      secretHosts = builtins.exec [
        "${pkgs.sops}/bin/sops"
        "-d"
        "${pkgs.inputs.secrets}/ssh-hosts/hosts.nix"
      ];
      builders = builtins.exec [
        "${pkgs.sops}/bin/sops"
        "-d"
        "${pkgs.inputs.secrets}/builders/hosts.nix"
      ];
      buildHosts = lib.filterAttrs
        (n: v:
          builtins.any (e: e.hostName == v.hostname) builders
        )
        secretHosts;
    in
    lib.concatStringsSep "\n"
      (lib.mapAttrsToList
        (
          name: value:
            ''
              Host ${name} ${value.hostname}
                HostName ${value.hostname}
                StrictHostKeyChecking accept-new
            ''
        )
        buildHosts);

  fwsvccfg = config.systemd.services.firewall;
  fwcfg = config.networking.firewall;
in
with lib; {
  imports = [
    ../profiles/desktop.nix
    ../modules/state.nix
    secrets
  ];

  services.tailscale.enable = true;

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  system.activationScripts.preStateSetup =
    let
      uid = builtins.toString
        secrets.users.extraUsers."${userName}".uid;
    in
    ''
      [ -d "/keep/home" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home
      [ -d "/keep/home/${userName}" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home/${userName}
      [ -d "/keep/home/${userName}/Downloads" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home/${userName}/Downloads
      chown ${uid}:${uid} /keep/home/${userName}
      chown ${uid}:${uid} /keep/home/${userName}/Downloads
    '';

  environment.state."/keep" =
    let
      uid = builtins.toString
        secrets.users.extraUsers."${userName}".uid;
    in
    {
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/iwd"
        "/var/lib/wireguard"
        "/var/lib/systemd/coredump"
        "/var/lib/containers"
        "/var/lib/tailscale"
        "/root"
      ];
      files = [
        "/etc/machine-id"
      ];
      users.${uid} = {
        directories = [
          "/home/${userName}/Downloads"
          "/home/${userName}/Documents"
          "/home/${userName}/Development"
          "/home/${userName}/Photos"
          "/home/${userName}/Pictures"
          "/home/${userName}/Games"
          "/home/${userName}/Sync"
          "/home/${userName}/.local/share/direnv"
          "/home/${userName}/.local/share/password-store"
          "/home/${userName}/.local/share/fish"
          "/home/${userName}/.local/share/containers"
          "/home/${userName}/.local/share/Steam"
          "/home/${userName}/.local/share/vulkan"
          "/home/${userName}/.mail"
          "/home/${userName}/.cargo"
          "/home/${userName}/.cache/mu"
          "/home/${userName}/.cache/nix"
          "/home/${userName}/.cache/nix-index"
          "/home/${userName}/.mozilla/firefox/default"
          "/home/${userName}/.gnupg"
          "/home/${userName}/.config/gcloud"
          "/home/${userName}/.emacs.d"
        ];
        files = [
          "/home/${userName}/.kube/config"
          "/home/${userName}/.ssh/known_hosts"
          "/home/${userName}/.spotify_token_cache.json"
        ];
      };
    };

  environment.systemPackages = [
    pkgs.man-pages
    pkgs.bmon
    pkgs.iftop
    pkgs.file
    pkgs.cacert
    pkgs.openssl
    pkgs.curl
    pkgs.gnupg
    pkgs.lsof
    pkgs.usbutils
    pkgs.mkpasswd
    pkgs.glib
    pkgs.powertop
    pkgs.socat
    pkgs.nmap
    pkgs.iptables
    pkgs.bridge-utils
    pkgs.dnsmasq
    pkgs.dhcpcd
    pkgs.dhcp
    pkgs.bind
    pkgs.pciutils
    pkgs.zip
    pkgs.wget
    pkgs.unzip
    pkgs.hdparm
    pkgs.libsysfs
    pkgs.htop
    pkgs.jq
    pkgs.binutils
    pkgs.psmisc
    pkgs.tree
    pkgs.ripgrep
    pkgs.vim
    pkgs.git
    pkgs.fish
    pkgs.tmux
    pkgs.blueman
    pkgs.pavucontrol
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.fd
    pkgs.wireguard
    pkgs.iwd
    pkgs.du-dust
    pkgs.hyperfine
    pkgs.procs
    pkgs.sd
    pkgs.bottom
  ];

  sleepManagement = {
    enable = true;
    wakeCommands = ''
      ${pkgs.procps}/bin/pkill gammastep
    '';
  };

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;

  users.groups = {
    "${userName}".gid = 1337;
    scard.gid = 1050;
  };
  users.users."${userName}" = {
    shell = pkgs.fish;
    extraGroups = [ "scard" ];
  };

  programs.sway.enable = true;
  programs.ssh.extraConfig = sshExtraConfig;
  programs.steam.enable = true;

  home-manager.useUserPackages = true;
  home-manager.users."${userName}" = { ... }: {
    imports = [
      ../home/home.nix
    ];

    home.username = userName;
    home.extraConfig.hostname = hostName;

  };

  hardware.cpu.intel.updateMicrocode = lib.mkForce false;
  hardware.cpu.amd.updateMicrocode = true;
  hardware.video.hidpi.enable = lib.mkDefault true;

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-amd" "k10temp" "nct6775" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=8G" "mode=755" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@nix" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/keep" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    neededForBoot = true;
    options =
      [ "subvol=@keep" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 32;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  boot.initrd.luks.devices = {
    cryptkey = { device = "/dev/disk/by-label/cryptkey"; };

    encrypted_root = {
      device = "/dev/disk/by-label/encrypted_root";
      keyFile = "/dev/mapper/cryptkey";
    };

    encrypted_swap = {
      device = "/dev/disk/by-label/encrypted_swap";
      keyFile = "/dev/mapper/cryptkey";
    };
  };
}
