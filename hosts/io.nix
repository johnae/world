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

  nixos-hardware = inputs.nixos-hardware;

  xps9300 = {
    imports = [
      #"${nixos-hardware}/common/cpu/intel/kaby-lake"
      "${nixos-hardware}/common/pc/laptop"
    ];

    boot.kernelParams = [ "mem_sleep_default=deep" ];
    boot.blacklistedKernelModules = [ "psmouse" ];
    services.throttled.enable = lib.mkDefault true;
    services.thermald.enable = true;
  };

  fwsvccfg = config.systemd.services.firewall;
  fwcfg = config.networking.firewall;
in
with lib; {
  imports = [
    ../profiles/laptop.nix
    ../modules/state.nix
    xps9300
    secrets
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
    wireguard.interfaces.vpn.postSetup = ''
      printf "nameserver 193.138.218.74" | ${pkgs.openresolv}/bin/resolvconf -a vpn -m 0
    '';
  };

  systemd.services.firewall-private = {
    inherit (fwsvccfg) wantedBy reloadIfChanged;
    wants = [ "wireguard-vpn.service" ];
    unitConfig = {
      inherit (fwsvccfg.unitConfig) ConditionCapability DefaultDependencies;
    };
    path = [ fwcfg.package ];
    description = fwsvccfg.description + " in netns private";
    after = fwsvccfg.after ++ [ "wireguard-vpn.service" ];
    serviceConfig = with fwsvccfg.serviceConfig; {
      inherit Type RemainAfterExit;
      ExecStart = "${pkgs.iproute}/bin/ip netns exec private " + (lib.last (lib.splitString "@" ExecStart));
      ExecReload = "${pkgs.iproute}/bin/ip netns exec private " + (lib.last (lib.splitString "@" ExecReload));
      ExecStop = "${pkgs.iproute}/bin/ip netns exec private " + (lib.last (lib.splitString "@" ExecStop));
    };
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
        "/var/lib/docker"
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
          "/home/${userName}/Sync"
          "/home/${userName}/.gnupg/private-keys-v1.d"
          "/home/${userName}/.local/share/direnv"
          "/home/${userName}/.local/share/password-store"
          "/home/${userName}/.local/share/fish"
          "/home/${userName}/.mail"
          "/home/${userName}/.cargo"
          "/home/${userName}/.cache/mu"
          "/home/${userName}/.cache/nix"
          "/home/${userName}/.mozilla/firefox/default"
        ];
        files = [
          "/home/${userName}/.gnupg/pubring.kbx"
          "/home/${userName}/.gnupg/sshcontrol"
          "/home/${userName}/.gnupg/trustdb.gpg"
          "/home/${userName}/.gnupg/random_seed"
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
    pkgs.ytop
  ];

  ## trying to fix bluetooth disappearing after suspend
  sleepManagement = {
    enable = true;
    sleepCommands = ''
      ${pkgs.libudev}/bin/systemctl stop bluetooth && ${pkgs.kmod}/bin/modprobe -r btusb
    '';
    wakeCommands = ''
      ${pkgs.kmod}/bin/modprobe btusb && ${pkgs.libudev}/bin/systemctl start bluetooth
    '';
  };
  ## end fix

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
  users.extraUsers."${userName}" = {
    shell = pkgs.fish;
    extraGroups = [ "scard" ];
  };

  programs.sway.enable = true;
  programs.ssh.extraConfig = sshExtraConfig;

  home-manager.useUserPackages = true;
  home-manager.users."${userName}" = { ... }: {
    imports = [
      ../home/home.nix
    ];

    home.username = userName;
    home.extraConfig.hostname = hostName;

    wayland.windowManager.sway.config.output = {
      "eDP-1" = {
        scale = "2.0";
        pos = "0 0";
      };
    };

  };

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=2G" "mode=755" ];
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

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
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
