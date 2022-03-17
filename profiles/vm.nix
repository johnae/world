{lib, ...}: {
  ## these must be present when running via libvirtd/qemu
  boot.initrd.kernelModules = [
    "virtio_pci"
    "virtio_scsi"
    "virtio_blk"
    "virtio_balloon"
    "virtio_console"
  ];
  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-label/cryptkey";
      keyFile = lib.mkForce null; ## by forcing this to null, we also force setting an actual password for the encrypted drive, seems something is still wonky when booting a vm basically
    };
  };
}
