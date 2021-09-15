{ lib, rustPlatform, llvmPackages, sqlite, installShellFiles, inputs }:

rustPlatform.buildRustPackage rec {
  pname = "innernet";
  version = inputs.innernet.rev;

  src = inputs.innernet;

  cargoSha256 = "sha256-9NiGeh0PrEKdkJ3+T6sekmOayBIQ++c01q2HUgP50H8=";

  nativeBuildInputs = with llvmPackages; [
    llvm
    clang
    installShellFiles
  ];
  buildInputs = [ sqlite ];

  LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";

  postInstall = ''
    installManPage doc/innernet-server.8.gz
    installManPage doc/innernet.8.gz
    installShellCompletion doc/innernet.completions.{bash,fish,zsh}
    installShellCompletion doc/innernet-server.completions.{bash,fish,zsh}
  '';

  meta = with lib; {
    description = "A private network system that uses WireGuard under the hood";
    homepage = "https://github.com/tonarino/innernet";
    license = licenses.mit;
  };
}
