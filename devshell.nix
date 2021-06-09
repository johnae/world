{ writeText, writeStrictShellScriptBin, mkDevShell, agenix, pixiecore }:

let

  netboot-installer = writeText "netboot-installer" ''
    {host}:
    let
      bootSystem = import <nixpkgs/nixos> {
        # system = ...;

        configuration = { config, pkgs, lib, ... }: with lib; {
          imports = [
              <nixpkgs/nixos/modules/installer/netboot/netboot-minimal.nix>
          ];

          nix = {
            trustedUsers = [ "root" ];
            extraOptions = '''
              experimental-features = nix-command flakes ca-references
            ''';
            package = pkgs.nixUnstable;
          };
          boot.kernelPackages = pkgs.linuxPackages_latest;
          ## Some useful options for setting up a new system
          services.getty.autologinUser = mkForce "root";
          # Enable sshd which gets disabled by netboot-minimal.nix
          systemd.services.sshd.wantedBy = mkOverride 0 [ "multi-user.target" ];
          users.users.root.openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ==" ];

          environment.etc."profile.local".text = '''
            if [ -z "$_INSTALLER_HAS_RUN" ]; then
              _INSTALLER_HAS_RUN=y
              export _INSTALLER_HAS_RUN
              echo will install "''${host}" now
              echo should format disks here
              echo nixos-install --flake github:johnae/world#''${host} --no-root-passwd
            else
              echo installer has already been run
            fi
            bash
          ''';
        };
      };

      pkgs = import <nixpkgs> {};
    in
      pkgs.symlinkJoin {

        name = "netboot";
        paths = with bootSystem.config.system.build; [
          netbootRamdisk
          kernel
          netbootIpxeScript
        ];
        preferLocalBuild = true;
      }
  '';

  pixieboot = writeStrictShellScriptBin "pixieboot" ''
    echo Hey, you may need to turn off the firewall for this to work
    nix build .#nixosConfigurations.pxebooter -o /tmp/netboot
    n="$(realpath /tmp/netboot)"
    init="$(grep -ohP 'init=\S+' "$n/netboot.ipxe")"
    sudo ${pixiecore}/bin/pixiecore boot "$n/bzImage" "$n/initrd" \
      --cmdline "$init loglevel=4" \
      --debug --dhcp-no-bind --port 64172 --status-port 64172
  '';
in

mkDevShell {
  name = "world";
  packages = [ agenix pixiecore pixieboot ];
  intro = ''

    Hello, world!

  '';
}
