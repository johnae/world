{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages = {
      kubevirt-operator = pkgs.runCommand "kubevirt-operator.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-operator} $out/kubevirt-operator.yaml
      '';
      kubevirt-cr = pkgs.runCommand "kubevirt-cr.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cr} $out/kubevirt-cr.yaml
      '';
      kubevirt-cdi-cr = pkgs.runCommand "kubevirt-cdi-cr.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cdi-cr} $out/kubevirt-cdi-cr.yaml
      '';
      kubevirt-cdi-operator = pkgs.runCommand "kubevirt-cdi-operator.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cdi-operator} $out/kubevirt-cdi-operator.yaml
      '';
    };
  };
}
