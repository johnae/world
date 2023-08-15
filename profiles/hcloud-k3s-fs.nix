{
  fileSystems."/data" = {
    encrypted = {
      label = "cryptdata";
      keyFile = "/sys/class/dmi/id/product_uuid";
      enable = true;
      blkDev = "/dev/sdb";
    };
    device = "/dev/mapper/data";
    fsType = "ext4";
    autoFormat = true;
  };
  fileSystems."/var/lib/rancher" = {
    device = "/data/rancher";
    fsType = "none";
    autoFormat = false;
    options = ["bind"];
  };
  fileSystems."/var/lib/kubelet" = {
    device = "/data/kubelet";
    fsType = "none";
    autoFormat = false;
    options = ["bind"];
  };
}
