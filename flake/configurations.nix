{
  inputs,
  self,
  withSystem,
  ...
}: let
  inherit
    (inputs.nixpkgs.lib // builtins)
    filterAttrs
    foldl'
    makeOverridable
    mapAttrs'
    mapAttrsToList
    mkForce
    mkIf
    nixosSystem
    readDir
    replaceStrings
    substring
    splitString
    ;

  inherit (inputs.nix-darwin.lib) darwinSystem;

  nixSettings = {
    nix.registry.nixpkgs = {flake = inputs.nixpkgs;};
    nix.registry.world = {flake = inputs.self;};
  };
  mapSystems = dir: mapAttrsToList (name: _: name) (filterAttrs (_: type: type == "directory") (readDir dir));

  mapConfigurations = dir:
    foldl' (
      hosts: system:
        hosts
        // (mapAttrs' (
          filename: _: let
            name = replaceStrings [".nix"] [""] filename;
          in {
            inherit name;
            value = {
              inherit system;
              hostconf = dir + "/${system}/${filename}";
            };
          }
        ) (builtins.readDir (dir + "/${system}")))
    ) {};

  mapOutputs = dir: mapConfigurations dir (mapSystems dir);

  defaultModules = [
    nixSettings
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.disko.nixosModules.disko
    inputs.jovian.nixosModules.jovian
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.microvm.nixosModules.host
    inputs.nixpkgs.nixosModules.notDetected
    ../modules/default.nix
  ];

  darwinDefaultModules = [
    nixSettings
    inputs.agenix.darwinModules.age
    inputs.home-manager.darwinModules.home-manager
    inputs.mac-app-util.darwinModules.default
    ../modules/default-darwin.nix
  ];

  homeConfigurations = mapAttrs' (
    name: conf: let
      inherit (conf) system hostconf;
      nameParts = splitString "@" name;
      username = builtins.head nameParts;
      hostName = builtins.head (builtins.tail nameParts);
    in {
      inherit name;
      value = withSystem system ({pkgs, ...}:
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {inherit inputs username hostName system;};
          modules =
            [
              {
                home = {
                  inherit username;
                  stateVersion = "25.05"; # set and bump intentionally
                };
                programs.home-manager.enable = true;
                home.packages = [
                  pkgs.world
                ];
              }
              ../users/modules/chromiums.nix
              ../users/modules/git-auto-sync.nix
              ../users/modules/kubie.nix
              ../users/modules/theme.nix
              ../users/modules/userinfo.nix
            ]
            ++ [
              hostconf
            ];
        });
    }
  ) (mapOutputs ../configurations/home);

  darwinConfigurations = mapAttrs' (
    name: conf: let
      inherit (conf) system hostconf;
      adminUser = {
        name = "johnaxele";
        uid = 501;
        gid = 20;
        userinfo = {
          email = "john@insane.se";
          altEmail = "johnxele@spotify.com";
          fullName = "John Axel Eriksson";
          githubUser = "johnae";
          gitlabUser = "johnae";
          devRemote = "icarus";
        };
      };
    in {
      inherit name;
      value = withSystem system ({pkgs, ...}:
        makeOverridable darwinSystem {
          inherit system;
          specialArgs = {
            hostName = name;
            tailnet = "tail68e9c";
            inherit adminUser;
            inherit self;
            inherit inputs;
          };
          modules =
            [
              {inherit adminUser;}
              {
                nixpkgs.pkgs = pkgs;
                nixpkgs.hostPlatform = system;
                system.stateVersion = 5;
                environment.systemPackages = [
                  pkgs.world
                ];
              }
            ]
            ++ darwinDefaultModules
            ++ [
              hostconf
            ];
        });
    }
  ) (mapOutputs ../configurations/darwin);

  nixosConfigurations = mapAttrs' (
    name: conf: let
      inherit (conf) system hostconf;
      adminUser = {
        name = "john";
        uid = 1337;
        gid = 1337;
        userinfo = {
          email = "john@insane.se";
          altEmail = "john@9000.dev";
          fullName = "John Axel Eriksson";
          githubUser = "johnae";
          gitlabUser = "johnae";
          devRemote = "icarus";
        };
      };
    in {
      inherit name;
      value = withSystem system ({pkgs, ...}:
        makeOverridable nixosSystem {
          inherit system;
          specialArgs = {
            hostName = name;
            tailnet = "tail68e9c";
            inherit adminUser;
            inherit self;
            hostConfigurations =
              mapAttrs' (name: conf: {
                inherit name;
                value = conf.config;
              })
              nixosConfigurations;
            inherit inputs;
          };
          modules =
            [
              {
                inherit adminUser;
              }
              {
                system.configurationRevision = mkIf (self ? rev) self.rev;
                system.nixos.versionSuffix = mkForce "git.${substring 0 11 inputs.nixpkgs.rev}";
                nixpkgs.pkgs = pkgs;
                environment.systemPackages = [
                  pkgs.world
                  inputs.noctalia.packages.${system}.default
                ];
              }
            ]
            ++ defaultModules
            ++ [
              hostconf
            ];
        });
    }
  ) (mapOutputs ../configurations/nixos);
in {
  flake = {
    inherit nixosConfigurations darwinConfigurations homeConfigurations;
  };
}
