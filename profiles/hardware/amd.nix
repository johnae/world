{
  hardware.cpu.amd.updateMicrocode = true;
  boot.kernelModules = ["kvm-amd" "k10temp" "nct6775"];
  boot.kernelParams = ["amd_pstate=passive"];
}
