{
  fileSystems."/data" = {
    encrypted = {
      label = "data";
      keyFile = "/sys/class/dmi/id/product_uuid";
      enable = true;
      blkDev = "/dev/sdb";
    };
    device = "/dev/mapper/data";
    fsType = "ext4";
    autoFormat = true;
  };
  fileSystems."/etc/rancher" = {
    device = "/data/etc-rancher";
    fsType = "none";
    autoFormat = false;
    options = ["bind"];
  };
  fileSystems."/var/lib/tailscale" = {
    device = "/data/tailscale";
    fsType = "none";
    autoFormat = false;
    options = ["bind"];
  };
  fileSystems."/var/lib/cni" = {
    device = "/data/cni";
    fsType = "none";
    autoFormat = false;
    options = ["bind"];
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
