#!/usr/bin/env bash
# world CI router. Runs on a push to main and on pull requests (see
# .cryo/README.md to apply).
#
# It posts the commit's `pending` status, fetches .cryo/ci.yaml at the
# commit being built, and spawns it. The final success/failure comes
# from the shared `ci-status` handler, which watches the spawned run's
# lifecycle event and reads the `repo:` + `sha:` tags set below - so a
# build that fails after dispatch turns the commit red instead of
# leaving the dispatch-time green.
#
# Two event shapes reach here. A push carries the commit in `after` and
# the branch in `ref`; a pull_request carries neither and keeps them
# under `pull_request.head`. Pushing to a PR branch emits both events,
# but the handler's `--when` admits pushes only for main, so each build
# has exactly one trigger.
#
# world is a public repo, so the pipeline comes over plain https with no
# credential and no clone; the build jobs do their own checkout.
set -euo pipefail
export PATH="/opt/toolchain/bin:/nix/var/nix/profiles/default/bin:/root/.nix-profile/bin:$PATH"

# The run's canonical input, whole - a multi-commit push body is well
# past anything an environment could hold.
cp "${CRYO_INPUT_FILE:?}" /tmp/cryo-input.json

# Resolve a dotted key path against the input, or empty if absent.
get() {
  python3 - "$1" <<'PY'
import json, sys
cur = json.load(open('/tmp/cryo-input.json'))
for k in sys.argv[1].split('.'):
    cur = cur.get(k) if isinstance(cur, dict) else None
print(cur if cur is not None else '')
PY
}
# First non-empty of several key paths: prefer the manifest-lifted
# top-level field, fall back to the raw github push body.
first() { local v; for k in "$@"; do v="$(get "$k")"; [ -n "$v" ] && { printf '%s' "$v"; return; }; done; }

# The manifest lifts sha/ref only for a push (`sha` comes from `after`),
# so a pull_request falls through to its head.
sha="$(first sha payload.body.after payload.body.pull_request.head.sha)"
ref="$(first ref payload.body.ref payload.body.pull_request.head.ref)"
repo_full="$(first repo payload.body.repository.full_name)"
pr="$(get payload.body.pull_request.number)"
echo "world-ci: repo=${repo_full} sha=${sha} ref=${ref}${pr:+ pr=#${pr}}"

[ -n "$sha" ] || { echo "no sha in input; nothing to build" >&2; exit 0; }
# A branch delete pushes the null sha. There is no tree to build and the
# statuses API would reject it.
[ "$sha" != "0000000000000000000000000000000000000000" ] || {
  echo "branch delete; nothing to build" >&2; exit 0
}

# $3 is an optional run id; with one, the check on GitHub links to that
# run's page instead of being a dead end.
post_status() {
  local state="$1" desc="$2" run="${3:-}" target=null
  [ -n "$run" ] && target="\"https://app.cryosleep.io/runs/${run}\""
  curl -sS --retry 3 --retry-delay 3 -o /dev/null \
    -X POST \
    -H "Authorization: Bearer ${GITHUB_STATUS_TOKEN:?}" \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/repos/${repo_full}/statuses/${sha}" \
    -d "{\"state\":\"${state}\",\"context\":\"cryosleep\",\"description\":\"${desc}\",\"target_url\":${target}}"
}

# Any failure before the build is dispatched shows red on the commit.
trap 'post_status failure "CI dispatch failed"' ERR

post_status pending "Build dispatched"

# The pipeline as it existed at this commit, so a push builds the
# definition it shipped with.
curl -sS --retry 3 --retry-delay 3 -fL \
  "https://raw.githubusercontent.com/${repo_full}/${sha}/.cryo/ci.yaml" \
  > /tmp/world-ci.yaml

# Spawn carries no structured input, so pin a minimal shaped one into the
# document; ci.yaml reads input.sha and input.ref from it. Only the
# scalars - not the raw body, where a quote in a commit message would
# break the YAML.
spawn_input="$(python3 - "$sha" "$ref" "$repo_full" <<'PY'
import json, sys
print(json.dumps({"sha": sys.argv[1], "ref": sys.argv[2], "repo": sys.argv[3]}))
PY
)"
{ printf 'input: %s\n' "${spawn_input}"
  cat /tmp/world-ci.yaml
} > /tmp/world-run.yaml

# The statuses API wants the full sha, so no truncation here: `ci-status`
# reads these two tags straight off the run's lifecycle event.
child="$(cryo spawn /tmp/world-run.yaml \
  --tag "sha:${sha}" --tag "repo:${repo_full}" --tag world \
  ${pr:+--tag "pr:${pr}"})"
echo "spawned world-ci run: ${child}"

trap - ERR
# Stay pending until that run terminates.
post_status pending "Build running" "${child}"
