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
            additionalSpace = "1024M";
            memSize = 2048;
          };
          # streamLayeredImage, not buildImage: buildImage materialises the
          # layer and the image tarball in the store, so a ~10G disk ends
          # up written three times (root, layer, tar.gz). The builders do
          # durable writes at ~48 MB/s, which made that the single biggest
          # cost of a CI run. This produces a script that writes the
          # archive to stdout instead, so only the qcow2 is stored.
          containerDisk = pkgs.dockerTools.streamLayeredImage {
            name = "${name}-containerdisk";
            tag = "latest";
            contents = pkgs.runCommand "containerdisk-root" {} ''
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
