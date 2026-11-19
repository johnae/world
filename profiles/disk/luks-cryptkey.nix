{pkgs, ...}: {
  ## The cryptkey volume holds a raw keyfile with no filesystem on it. Since
  ## systemd 262, 99-systemd.rules sets SYSTEMD_READY=0 on dm-crypt devices
  ## without a detected superblock, so dev-mapper-cryptkey.device never shows
  ## up and every volume keyed off it hangs in the initrd. This rule sorts
  ## after 99-systemd.rules and marks it ready again.
  boot.initrd.services.udev.packages = [
    (pkgs.writeTextDir "lib/udev/rules.d/99-zz-cryptkey-ready.rules" ''
      SUBSYSTEM=="block", ENV{DM_NAME}=="cryptkey", ENV{SYSTEMD_READY}="1"
    '')
  ];
}
