# world CI on cryosleep

world builds on [cryosleep](https://cryosleep.io). It is the only CI
that gates a merge: `cryosleep` is the required status check, so the
build here is what auto-merge waits for.

```
push  ->  webhook  ->  world.push  ->  router.sh  ->  world-ci run
                                          |               |
                                    pending status    final status
                                                      (ci-status handler)
```

- `ci.yaml` is the pipeline: lint, flake check, and every
  `x86_64-linux` package. The router fetches it **at the pushed
  commit**, so a push builds the definition it shipped with.
- `router.sh` runs on the server as an event handler. It posts the
  commit's `pending` status and spawns the pipeline, tagging the run
  with `repo:` and `sha:`.
- The final green or red comes from the shared `ci-status` handler
  (`cryosleep/ci/status.sh` in the cloud9k repo), which reads those two
  tags off the run's lifecycle event.

Builds need an agent advertising the `build` capability with nix
installed. Package builds push to `nixcache.9000.dev` when the
`NCPS_UPLOAD_KEY` project secret is set, and skip the push when it
isn't. Disk images (`*-qcow2`, `*-containerdisk`, `*-diskformat`) are
built but never pushed: `nix copy` sends a path's whole closure, and
with `streamLayeredImage` the containerdisk output is a small script
whose closure holds the ~10G qcow2.

## Applying the wiring

The pipeline is versioned here; the two server-side pieces are applied
by hand and only change when this file says so.

```sh
# The webhook endpoint. Prints the ingest URL and signing secret once -
# paste both into Settings -> Webhooks (content type application/json,
# push events).
cryo event-hook set world --manifest github --key "$(openssl rand -hex 20)"

# Route main pushes and pull requests to the pipeline. Pushing to a PR
# branch emits both a push and a synchronize; admitting pushes only for
# main means each build has exactly one trigger. The second clause skips
# changes that are only markdown (a PR carries no `commits`, so it
# passes).
cryo handler set world-ci .cryo/router.sh --as script \
  --on world.push,world.pull_request.opened,world.pull_request.synchronize,world.pull_request.reopened \
  --when '(has(event.payload.body.pull_request) || event.payload.body.ref == "refs/heads/main") && (!has(event.payload.body.commits) || size(event.payload.body.commits) == 0 || event.payload.body.commits.exists(c, (c.added + c.modified + c.removed).exists(p, !p.endsWith(".md"))))' \
  --credential GITHUB_STATUS_TOKEN=github-status
```

The webhook must send both `push` and `pull_request`. Pull requests from
forks are rejected at ingest: the `github` manifest only trusts one whose
author is an OWNER, MEMBER or COLLABORATOR and whose head is not a fork.

Check the pipeline before pushing it:

```sh
cryo check .cryo/ci.yaml
```

## Running one by hand

```sh
cryo submit .cryo/ci.yaml --follow
```

A manual submit carries no input, so the clone stays on the default
branch. To build a specific commit, prepend an `input:` line the way the
router does.

## The nightly updater

`update.yaml` bumps the flake inputs on the `automatic-updates` branch,
has a model repair whatever the update broke, opens a PR, and arms
GitHub's auto-merge. It does not merge anything itself: auto-merge waits
for the required `cryosleep` check, which is `ci.yaml` building the PR
like any other. The updater proposes; the build decides.

`repair-prompt.md` is the instruction the model gets. It is a separate
file so it shows up in review — an instruction that decides what may
change in your system config is worth reading.

Authentication is the GitHub App (app 1073609, installation 57780546).
Its installation token lasts an hour, so the job mints one per run from
the App private key in the `GITHUB_APP_KEY` project secret. There is no
App credential kind in cryosleep yet, hence the JWT exchange in bash.

It runs nightly at 00:00 UTC:

```sh
cryo schedule create --cron '0 0 0 * * *' --tz UTC .cryo/update.yaml --name world-update
```

No `--requires` on the schedule: a YAML pipeline takes its capabilities
per job, from `requires:` in the body, and passing the flag is a 400
rather than a silent no-op. Cron rather than an interval so the run
lands at a fixed hour instead of drifting by however long the previous
one took.

Two secrets it reads, both project-scoped so a scheduled run gets them
without a credential flag:

- `GITHUB_APP_KEY` — the App's PEM. Required.
- `ANTHROPIC_API_KEY` — optional. Without it the PR is opened
  unrepaired rather than the run failing.
