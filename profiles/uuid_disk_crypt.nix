{lib, ...}: {
  boot.initrd.luks.devices = {
    cryptkey.keyFile = lib.mkDefault "/sys/class/dmi/id/product_uuid";
  };
}
