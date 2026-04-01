{
  inputs,
  self,
  ...
}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      packages = let
        buildKubevirtImage = name: let
          config = self.nixosConfigurations.${name}.config;
          qcow2 = import "${inputs.nixpkgs}/nixos/lib/make-disk-image.nix" {
            inherit pkgs lib config;
            format = "qcow2";
            partitionTableType = "legacy+gpt";
            diskSize = "auto";
            additionalSpace = "4096M";
            memSize = 2048;
          };
          containerDisk = pkgs.dockerTools.buildImage {
            name = "${name}-containerdisk";
            tag = "latest";
            copyToRoot = pkgs.runCommand "containerdisk-root" {} ''
              mkdir -p $out/disk
              cp ${qcow2}/nixos.qcow2 $out/disk/disk.qcow2
            '';
          };
        in {
          "${name}-qcow2" = qcow2;
          "${name}-containerdisk" = containerDisk;
        };
      in
        buildKubevirtImage "vega";
    };
}
