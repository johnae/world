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

  adminUsers = import ../users/admin-users.nix;

  nixSettings = {
    nix.registry.nixpkgs = {flake = inputs.nixpkgs;};
    nix.registry.world = {flake = inputs.self;};
  };

  rekeyConfig = {
    age.rekey.masterIdentities = [
      ../secrets/yubikey-identity-1.pub
    ];
    age.rekey.extraEncryptionPubkeys = [
      "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
      "age14v5luk6ulvq8memqrmh5sw2pmp37cwzpra8sv5fzwt2zpu6hqyqsusvdgn"
    ];
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
    rekeyConfig
    inputs.agenix.nixosModules.age
    inputs.agenix-rekey.nixosModules.default
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
    rekeyConfig
    inputs.agenix.darwinModules.age
    inputs.agenix-rekey.darwinModules.default
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
              inputs.agenix.homeManagerModules.age
              inputs.agenix-rekey.homeManagerModules.default
              rekeyConfig
              {
                age.identityPaths = ["/home/${username}/.ssh/id_ed25519"];
                age.rekey = {
                  storageMode = "local";
                  localStorageDir = ../secrets/rekeyed + "/${username}-${hostName}";
                };
              }
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
              ../users/modules/default.nix
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
      adminUser = adminUsers.darwin;
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
      adminUser = adminUsers.nixos;
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
