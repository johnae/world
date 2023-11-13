{
  adminUser,
  pkgs,
  ...
}: {
  nix.settings.trusted-users = [adminUser.name];
  users = {
    users.${adminUser.name} = {
      inherit (adminUser) uid;
      shell = pkgs.nushell;
      isNormalUser = true;
      hashedPassword = "$6$RG3QgdVr6A5s524$FRVNXOYWYCNXf6cH/gV4u7g6sffffznBnbXZGHysuWVHGYPpeLfuDuXEJHK.KUrkVCfPXFTTsCLJaj2Q6ATkC1";
      openssh.authorizedKeys.keys = [
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDpauQBOftphOeuF2TaBHGQSAAAABHNzaDo="
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
      ];
      extraGroups = ["wheel" "docker" "video" "audio" "kvm" "libvirtd"];
    };
  };
}
