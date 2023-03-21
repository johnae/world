{
  inputs,
  lib,
  withSystem,
  self,
  ...
}: let
  inherit
    (inputs.nixpkgs.lib // builtins)
    mapAttrs'
    replaceStrings
    fromTOML
    readDir
    readFile
    pathExists
    mapAttrs
    attrByPath
    filterAttrsRecursive
    makeOverridable
    nixosSystem
    mkIf
    mkForce
    substring
    ;
in rec {
  flake = let
    hostConfigurations = mapAttrs' (
      filename: _: let
        name = replaceStrings [".toml"] [""] filename;
      in {
        inherit name;
        value = fromTOML (readFile (../hosts + "/${filename}"));
      }
    ) (readDir ../hosts);

    nixosConfig = hostName: config: let
      fileOrDir = path: let
        basePath = toString (../. + "/${path}");
      in
        if pathExists basePath
        then basePath
        else "${basePath}.nix";

      hostConf = config.config;
      profiles = map fileOrDir hostConf.profiles;

      ## Somewhat hacky way of making it seem as if we're
      ## giving home-manager the profiles to load - that
      ## can't actually happen within a module though. I.e
      ## in a module you can't add imports coming from an option.
      userProfiles = mapAttrs (
        _: user: let
          profiles = attrByPath ["profiles"] {} user;
        in
          map fileOrDir profiles
      ) (attrByPath ["home-manager" "users"] {} hostConf);

      modules = [../modules];

      inherit (config) system;

      cfg =
        filterAttrsRecursive (
          name: _:
            name != "profiles"
        )
        hostConf;
    in
      withSystem system (ctx @ {pkgs, ...}:
        makeOverridable nixosSystem {
          inherit system;
          specialArgs = {
            inherit hostName inputs userProfiles;
            hostConfiguration = cfg;
            hostConfigurations = mapAttrs (_: conf: conf.config) hostConfigurations;
          };

          modules = [
            {
              system.configurationRevision = mkIf (self ? rev) self.rev;
              system.nixos.versionSuffix = mkForce "git.${substring 0 11 inputs.nixpkgs.rev}";
              nixpkgs.pkgs = pkgs;
            }
            (
              {pkgs, ...}: {
                environment.systemPackages = [pkgs.world];
              }
            )
            inputs.nixpkgs.nixosModules.notDetected
            inputs.home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.age
            {
              imports = modules ++ profiles;
            }
          ];
        });

    nixosConfigurations = mapAttrs nixosConfig hostConfigurations;
  in {
    inherit nixosConfigurations hostConfigurations;
  };

  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    diskFormatter = hostName: config: pkgs: {
      name = "${hostName}-diskformat";
      value = pkgs.callPackage ../utils/diskformat.nix {
        inherit hostName config;
      };
    };
  in {
    packages = mapAttrs' (hostName: config: diskFormatter hostName config pkgs) flake.nixosConfigurations;
  };
}
