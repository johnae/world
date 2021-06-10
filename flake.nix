{
  description = "John's NixOS configurations";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    packages = {
      url = "path:./packages";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
      inputs.nix-misc.follows = "nix-misc";
    };
    agenix = {
     url = "github:ryantm/agenix";
     inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      inherit (nixpkgs.lib) genAttrs filterAttrs mkOverride makeOverridable mkIf
        hasSuffix mapAttrs mapAttrs' removeSuffix nameValuePair nixosSystem
        mkForce mapAttrsToList splitString concatStringsSep last hasAttr;
      inherit (builtins) substring pathExists fromTOML readFile listToAttrs filter;

      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: genAttrs supportedSystems (system: f system);
      pkgs = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-misc.overlay
          inputs.nur.overlay
          inputs.agenix.overlay
        ] ++ mapAttrsToList (_: value: value) inputs.packages.overlays;
      });

      hosts = mapAttrs (hostname: config: {
        specialArgs.users = config.users;
        configuration.imports = (map (item:
          if pathExists (toString (./. + "/${item}")) then
            (./. + "/${item}")
          else (./. + "/${item}.nix")
        ) config.imports) ++ (mapAttrsToList (name: value: ./. + "/users/${name}") config.users.users);
      }) (fromTOML (readFile ./hosts.toml));

      systemConfig = hostName: host:
        let
          system = "x86_64-linux";
        in
        makeOverridable nixosSystem {
          inherit system;
          specialArgs = {
            pkgs = pkgs.${system};
            inherit hostName inputs;
            userProfiles = import ./users/profiles.nix { lib = inputs.nixpkgs.lib; };
          } // host.specialArgs;
          modules = [
            #{ system.configurationRevision = mkIf (self ? rev) self.rev; }
            { system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}"; }
            { nixpkgs = { pkgs = pkgs.${system}; }; }
            inputs.nixpkgs.nixosModules.notDetected
            inputs.home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.age
            host.configuration
          ];
        };

      toNixosConfig = hostName: hostConf: systemConfig hostName hostConf;

      toPxeBootSystemConfig = hostName:
        let
          system = "x86_64-linux";
        in
          let bootSystem = makeOverridable nixosSystem {
            inherit system;
            specialArgs = {
              pkgs = pkgs.${system};
              inherit hostName inputs;
            };
            modules = [
              { system.configurationRevision = mkIf (self ? rev) self.rev; }
              { system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}"; }
              { nixpkgs = { pkgs = pkgs.${system}; }; }
              inputs.nixpkgs.nixosModules.notDetected
              ({ config, modulesPath, pkgs, lib, ... }: {
                 imports = [
                   "${modulesPath}/installer/netboot/netboot-minimal.nix"
                   ./cachix.nix
                 ];
                 nix = {
                   trustedUsers = [ "root" ];
                   extraOptions = ''
                     experimental-features = nix-command flakes ca-references
                   '';
                   package = pkgs.nixUnstable;
                 };
                 environment.systemPackages = with pkgs; [
                   git curl yj jq skim
                 ];
                 boot.zfs.enableUnstable = true;
                 boot.kernelPackages = pkgs.linuxPackages_latest;
                 services.getty.autologinUser = mkForce "root";
                 hardware.video.hidpi.enable = true;
                 # Enable sshd which gets disabled by netboot-minimal.nix
                 systemd.services.sshd.wantedBy = mkOverride 0 [ "multi-user.target" ];
                 users.users.root.openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ==" ];

                 environment.etc."profile.local".text = ''
                   attempt=0
                   max_attempts=5
                   wait_secs=3
                   while ! curl -sf --connect-timeout 5 --max-time 5 http://www.google.com > /dev/null; do
                     if [ "$attempt" -ge "$max_attempts" ]; then
                       echo Fail - no internet, tried "$max_attempts" times over $((max_attempts * wait_secs)) seconds
                       exit 1
                     fi
                     sleep "$wait_secs"
                   done
                   if [ -z "$_INSTALLER_HAS_RUN" ]; then
                     _INSTALLER_HAS_RUN=y
                     export _INSTALLER_HAS_RUN
                     echo should format disks here
                     git clone https://github.com/johnae/world /tmp/world
                     cd /tmp/world
                     git checkout new-world
                     echo 'Which config should be installed?'
                     host="$(cat hosts.toml | yj -tj | jq -r '. | keys | .[]' | sk)"
                     nix build .#"$host"-diskformat
                     ./result/bin/diskformat | tee -a diskformat.log
                     mount
                     nixos-install --flake .#"$host" --no-root-passwd --impure | tee -a nixos-install.log
                   else
                     echo installer has already been run
                   fi
                   bash
                 '';
              })
            ];
          };
          in
           pkgs.${system}.symlinkJoin {
             name = "netboot";
             paths = with bootSystem.config.system.build; [
               netbootRamdisk
               kernel
               netbootIpxeScript
             ];
             preferLocalBuild = true;
          };

      toDiskFormatter = hostName: config:
        inputs.nixpkgs.lib.nameValuePair "${hostName}-diskformat" (
          pkgs.x86_64-linux.callPackage ./utils/diskformat.nix {
            inherit hostName config;
          }
        );

      hostConfigurations = mapAttrs toNixosConfig hosts;

      nixosConfigurations = hostConfigurations
                            //
                            {
                              pxebooter = toPxeBootSystemConfig "pxebooter";
                            };

      diskFormatters = mapAttrs' toDiskFormatter hostConfigurations;

    in
    {
      devShell = forAllSystems (system:
        pkgs.${system}.callPackage ./devshell.nix { }
      );
      inherit nixosConfigurations;
      packages.x86_64-linux = diskFormatters // (mapAttrs (name: value: pkgs.x86_64-linux.${name} ) (filterAttrs (name: _: (hasAttr name pkgs.x86_64-linux) && nixpkgs.lib.isDerivation pkgs.x86_64-linux.${name}) inputs.packages.overlays));
    };
}
