{lib, ...}:
{
  networking.nameservers = lib.mkForce [ "192.168.104.100" ];
}
