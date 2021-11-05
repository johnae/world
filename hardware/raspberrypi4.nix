{inputs, pkgs, lib, ...}:
{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  boot.loader.generic-extlinux-compatible.enable = false;
  ## must use a specific kernel, the lastest doesn't work as of 2021-11-04
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };

  fileSystems."/".options = [ "defaults" "size=4G" "mode=755" ];

  networking.usePredictableInterfaceNames = lib.mkForce true;

  boot.kernelParams = [
    "8250.nr_uarts=1"
    "console=ttyS0115200n8"
    "console=ttyAMA0115200n8"
    "cma=128M"
    "console=tty1"
    "boot.shell_on_fail"
  ];

  boot.initrd.luks.devices.cryptkey.keyFile = "/sys/firmware/devicetree/base/serial-number";

  boot.initrd.availableKernelModules = [ "hid_generic" "usbhid" "uas" ];

  boot.initrd.luks.cryptoModules = [
    "aes"
    "aes_generic"
    "blowfish"
    "twofish"
    "serpent"
    "cbc"
    "xts"
    "lrw"
    "sha1"
    "sha256"
    "sha512"
    "af_alg"
    "algif_skcipher"
    "xchacha12"
    "xchacha20"
    "adiantum"
  ];

  cryptsetup.luksFormat.extraParams = "-v --type luks2 --sector-size 4096 --hash sha256 --key-size 256 --use-urandom --cipher xchacha12,aes-adiantum-plain64 --align-payload=2048";

}
