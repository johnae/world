{pkgs, ...}: {
  services.jellyfin.enable = true;
  hardware.opengl = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.vaapiIntel
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
      pkgs.rocm-opencl-icd
      pkgs.rocm-opencl-runtime
      pkgs.amdvlk
    ];
  };
  systemd.services.jellyfin.serviceConfig.PrivateDevices = lib.mkForce false;
}
