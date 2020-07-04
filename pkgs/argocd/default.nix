{ stdenv, lib, buildGoModule, packr, inputs }:
buildGoModule rec {
  pname = "argocd";
  commit = inputs.argo-cd.version;
  version = inputs.argo-cd.version;

  src = inputs.argo-cd;

  vendorSha256 = "sha256-TUWLW/AsxeJ84jcvYm3ECs1do5hy2ZaFvtW3bmEkUsQ=";

  nativeBuildInputs = [ packr ];

  CGO_ENABLED = 0;

  buildFlagsArray = ''
    -ldflags=
     -X github.com/argoproj/argo-cd/common.version=${version}
     -X github.com/argoproj/argo-cd/common.buildDate=unknown
     -X github.com/argoproj/argo-cd/common.gitCommit=${commit}
     -X github.com/argoproj/argo-cd/common.gitTreeState=clean
  '';

  # run packr to embed assets
  preBuild = ''
    packr
  '';

  meta = with stdenv.lib; {
    description = "Argo CD is a declarative, GitOps continuous delivery tool for Kubernetes";
    homepage = "https://github.com/argoproj/argo";
    license = licenses.asl20;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
