{
  zramSwap.enable = true;
  boot.kernel.sysctl."vm.swappiness" = 133; # with zram it makes sense to swap more aggressively
}
