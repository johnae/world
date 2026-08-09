{
  config,
  lib,
  ...
}:
## This just auto-creates /nix/var/nix/{profiles,gcroots}/per-user/<USER>
## for all extraUsers setup on the system. Without this home-manager refuses
## to run on boot when setup as a nix module and the user has yet to install
## anything through nix.
  let
    inherit (lib) attrNames filterAttrs foldr mkEnableOption mkIf;
    cfg = config.services.nix-dirs;
    users = filterAttrs (_: value: value.isNormalUser) config.users.extraUsers;
    nix-dirs-services = foldr (
      user: svcs:
        svcs
        // {
          "nix-dirs-${user}" = let
            beforeUnits = ["home-manager-${user}.service"];
          in {
            description = "Ensure nix dirs per-user for ${user} are present";
            enable = true;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = "yes";
            };
            script = ''
              mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${user}
              chown ${user} /nix/var/nix/{profiles,gcroots}/per-user/${user}
            '';
            before = beforeUnits;
            wantedBy = beforeUnits;
          };
        }
    ) {} (attrNames users);
  in {
    options.services.nix-dirs = {
      enable =
        mkEnableOption
        "ensures nix-dirs underneath /nix/var/nix/{profiles,gcroots} are created on boot. Home-manager really requires these to work";
    };

    config = mkIf cfg.enable {systemd.services = nix-dirs-services;};
  }
