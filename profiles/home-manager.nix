{hostName, config, ...}:

let
  inherit (builtins) mapAttrs;
in
{
  home-manager.extraSpecialArgs = { inherit hostName; };
  home-manager.sharedModules = [
    ../users/modules/userinfo.nix
    ../users/modules/theme.nix
  ];

  home-manager.users = mapAttrs (user: conf:
    { ... }:
    {
      imports = [ ../users/profiles/home.nix ] ++ conf.profiles;
      home.stateVersion = "21.05";
      home.username = user;
    }
  ) config.home;
}
