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

  defaultModules = [
    nixSettings
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.microvm.nixosModules.host
    inputs.nixpkgs.nixosModules.notDetected
    ../modules/default.nix
  ];

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
          devRemote = "orion";
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
    inherit nixosConfigurations;
  };
}
