{lib, ...}: {
  imports = [
    ./workstation.nix
  ];

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend-then-hibernate";
  };
  services.disable-usb-wakeup.enable = true;
  programs.light.enable = true;
  services.upower.enable = true;
  services.tlp.enable = true;
  services.tlp.settings = {
    CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
    CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
    CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
    RADEON_DPM_PERF_LEVEL_ON_AC = "auto";
    RADEON_DPM_PERF_LEVEL_ON_BAT = "auto";
    RADEON_POWER_PROFILE_ON_AC = "auto";
    RADEON_POWER_PROFILE_ON_BAT = "auto";
    USB_AUTOSUSPEND = "1";
  };

  boot.kernel.sysctl = {
    "vm.dirty_writeback_centisecs" = lib.mkDefault 1500;
    "vm.laptop_mode" = lib.mkDefault 5;
    "vm.swappiness" = lib.mkDefault 1;
  };
}
