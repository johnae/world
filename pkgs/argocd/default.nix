{ stdenv, lib, buildGoModule, packr, inputs }:
let
  commit = inputs.argo-cd.rev;
  version = builtins.replaceStrings
    [ "\n" "\r" "\t" ] [ "" "" "" ]
    (builtins.readFile "${inputs.argo-cd}/VERSION");
in
buildGoModule {
  pname = "argocd";
  inherit version commit;

  src = inputs.argo-cd;

  vendorSha256 = "sha256-y8hrQTPwYBXq0maMiON6iKda3kBse0XNjLvmvrgzWQw=";

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

  meta = {
    description = "Argo CD is a declarative, GitOps continuous delivery tool for Kubernetes";
    homepage = "https://github.com/argoproj/argo";
    license = stdenv.lib.licenses.asl20;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
