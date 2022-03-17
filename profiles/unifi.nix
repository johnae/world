{pkgs, ...}: {
  environment.state."/keep".directories = ["/var/lib/unifi"];
  services.unifi.enable = true;
  services.unifi.unifiPackage = pkgs.unifi;
  services.unifi.mongodbPackage = pkgs.mongodb-4_2;
  services.unifi.openFirewall = true;
  systemd.services.unifi.unitConfig.RequiresMountsFor = "/var/lib/unifi";
  networking.firewall.allowedTCPPorts = [8443];
  users.users.unifi.group = "unifi";
  users.users.unifi.isSystemUser = true;
  users.groups.unifi = {};
}
