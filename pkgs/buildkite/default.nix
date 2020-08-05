{ stdenv, lib, buildGoModule, makeWrapper, coreutils, git, openssh, bash, gnused, gnugrep, inputs }:
buildGoModule {
  pname = "buildkite-agent";
  version = inputs.buildkite.rev;

  src = inputs.buildkite;

  vendorSha256 = "sha256-5m/jRpVSN0N0EJSbRRmpygsYxOVOjNlhB74DMn6gp5I=";

  buildInputs = [ makeWrapper ];

  postInstall = ''
    # Fix binary name
    mv $out/bin/{agent,buildkite-agent}

    # These are runtime dependencies
    wrapProgram $out/bin/buildkite-agent \
      --prefix PATH : '${stdenv.lib.makeBinPath [ openssh git coreutils gnused gnugrep ]}' \
      --set BUILDKITE_HOOKS_PATH ${./hooks}
  '';

  meta = {
    description = "Build runner for buildkite.com";
    longDescription = ''
      The buildkite-agent is a small, reliable, and cross-platform build runner
      that makes it easy to run automated builds on your own infrastructure.
      It’s main responsibilities are polling buildkite.com for work, running
      build jobs, reporting back the status code and output log of the job,
      and uploading the job's artifacts.
    '';
    homepage = "https://buildkite.com/docs/agent";
    license = stdenv.lib.licenses.mit;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
    platforms = stdenv.lib.platforms.unix;
  };
}
