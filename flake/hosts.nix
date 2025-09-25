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
    ;

  inherit (inputs.nix-darwin.lib) darwinSystem;

  nixSettings = {
    nix.registry.nixpkgs = {flake = inputs.nixpkgs;};
    nix.registry.world = {flake = inputs.self;};
  };
  mapSystems = dir: mapAttrsToList (name: _: name) (filterAttrs (_: type: type == "directory") (readDir dir));
  mapHosts = foldl' (
    hosts: system:
      hosts
      // (mapAttrs' (
        filename: _: let
          name = replaceStrings [".nix"] [""] filename;
        in {
          inherit name;
          value = {
            inherit system;
            hostconf = ../hosts + "/${system}/${filename}";
          };
        }
      ) (builtins.readDir ../hosts/${system}))
  ) {};

  mapMicrovms = foldl' (
    hosts: system:
      hosts
      // (mapAttrs' (
        filename: _: let
          name = replaceStrings [".nix"] [""] filename;
        in {
          inherit name;
          value = {
            inherit system;
            hostconf = ../microvms + "/${system}/${filename}";
          };
        }
      ) (builtins.readDir ../microvms/${system}))
  ) {};

  mapMacs = foldl' (
    hosts: system:
      hosts
      // (mapAttrs' (
        filename: _: let
          name = replaceStrings [".nix"] [""] filename;
        in {
          inherit name;
          value = {
            inherit system;
            hostconf = ../darwin + "/${system}/${filename}";
          };
        }
      ) (builtins.readDir ../darwin/${system}))
  ) {};

  defaultModules = [
    nixSettings
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.microvm.nixosModules.host
    inputs.lanzaboote.nixosModules.lanzaboote
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
  ) (mapMacs (mapSystems ../darwin));

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
                ];
              }
            ]
            ++ defaultModules
            ++ [
              hostconf
            ];
        });
    }
  ) ((mapHosts (mapSystems ../hosts)) // (mapMicrovms (mapSystems ../microvms)));
in {
  flake = {
    inherit nixosConfigurations darwinConfigurations;
  };
}
