{
  lib,
  pkgs,
  ...
}: {
  services.jellyfin.enable = true;
  services.jellyfin.openFirewall = true;
  hardware.graphics = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.intel-vaapi-driver
      pkgs.libva-vdpau-driver
      pkgs.libvdpau-va-gl
      pkgs.rocm-opencl-icd
      pkgs.rocm-opencl-runtime
    ];
  };
  systemd.services.jellyfin.serviceConfig.PrivateDevices = lib.mkForce false;
}
