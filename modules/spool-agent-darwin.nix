# A hyperspool agent as a launchd daemon on macOS.
#
# The launchd job runs as root only to keep the binary and the token out of
# the agent user's reach: it fetches the published `spool` into a root-owned
# directory, checks it against the signed manifest, and starts the agent as
# its own user with the token on stdin. Jobs run as that user, so they can
# neither read the token nor replace the binary.
#
# With HYPERSPOOL_AGENT_UPDATE_URL set the agent exits once a newer build is
# published (after its current job); launchd starts it again and the fetch
# installs the new binary.
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption mkIf types concatStringsSep;
  cfg = config.services.spool-agent;
  bin = "${cfg.dataDir}/bin/spool";
  home = "${cfg.dataDir}/home";
in {
  options.services.spool-agent = {
    enable = mkEnableOption "a hyperspool agent";

    id = mkOption {
      type = types.str;
      description = "The agent id the token was minted for.";
    };

    server = mkOption {
      type = types.str;
      default = "https://api.hyperspool.dev";
    };

    capabilities = mkOption {
      type = types.listOf types.str;
      default = ["shell"];
      description = "What the agent advertises; the token caps it.";
    };

    tokenFile = mkOption {
      type = types.str;
      description = "A root-readable file holding the agent token.";
    };

    downloadUrl = mkOption {
      type = types.str;
      default = "https://get.9000.dev/darwin/spool-arm64";
      description = "The published binary; its manifest is this plus `.sha256`.";
    };

    user = mkOption {
      type = types.str;
      default = "spool-agent";
    };

    uid = mkOption {
      type = types.int;
      default = 532;
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/spool-agent";
    };

    runtimePackages = mkOption {
      type = types.listOf types.package;
      default = [];
      description = "On the agent's PATH, and so on its jobs' unless a pipeline sets its own.";
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = {
      inherit (cfg) uid;
      gid = cfg.uid;
      inherit home;
      shell = "/usr/bin/false";
      description = "hyperspool agent";
      isHidden = true;
    };
    users.groups.${cfg.user} = {
      gid = cfg.uid;
      members = [cfg.user];
    };
    users.knownUsers = [cfg.user];
    users.knownGroups = [cfg.user];

    launchd.daemons.spool-agent = {
      path = cfg.runtimePackages ++ [pkgs.coreutils pkgs.curl];
      script = ''
        set -u
        install -d -o root -g wheel -m 755 "${cfg.dataDir}" "${cfg.dataDir}/bin"
        install -d -o ${cfg.user} -g ${cfg.user} -m 700 "${home}"

        # Refresh the binary from the manifest. The installed binary checks
        # the manifest's signature; the very first fetch has nothing to
        # check it with, so it takes the manifest's checksum on trust. Any
        # failure keeps what is installed, so a restart always starts.
        url="${cfg.downloadUrl}"
        man=$(mktemp); sig=$(mktemp); tmp=$(mktemp)
        trap 'rm -f "$man" "$sig" "$tmp"' EXIT
        if curl -fsSL "$url.sha256" -o "$man" && curl -fsSL "$url.sha256.minisig" -o "$sig"; then
          if [ -x "${bin}" ]; then
            want=$("${bin}" agent verify-manifest "$man" "$sig" || true)
          else
            want=$(cut -d' ' -f1 "$man")
          fi
          if [ -z "$want" ]; then
            echo "spool-agent: manifest did not verify; keeping the current binary" >&2
          elif [ ! -x "${bin}" ] || [ "$(sha256sum "${bin}" | cut -d' ' -f1)" != "$want" ]; then
            if curl -fsSL "$url" -o "$tmp" && [ "$(sha256sum "$tmp" | cut -d' ' -f1)" = "$want" ]; then
              install -o root -g wheel -m 755 "$tmp" "${bin}"
              echo "spool-agent: installed $want" >&2
            else
              echo "spool-agent: download or checksum failed; keeping the current binary" >&2
            fi
          fi
        else
          echo "spool-agent: manifest fetch failed; keeping the current binary" >&2
        fi
        [ -x "${bin}" ] || { echo "spool-agent: no binary yet" >&2; exit 1; }
        rm -f "$man" "$sig" "$tmp"
        trap - EXIT

        # sudo opens nothing: the shell, as root, hands it the token on stdin.
        exec /usr/bin/sudo -u ${cfg.user} -H /usr/bin/env \
          PATH="$PATH" \
          HYPERSPOOL_AGENT_UPDATE_URL="$url.sha256" \
          HYPERSPOOL_AGENT_CAPABILITIES="${concatStringsSep "," cfg.capabilities}" \
          "${bin}" agent run "${cfg.server}" "${cfg.id}" --token-file - < "${cfg.tokenFile}"
      '';
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
        # A failed start (no network at boot) is tried again, not hammered.
        ThrottleInterval = 30;
        ProcessType = "Interactive";
        StandardOutPath = "/var/log/spool-agent.log";
        StandardErrorPath = "/var/log/spool-agent.log";
      };
    };
  };
}
