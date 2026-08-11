# world CI on cryosleep

world builds on [cryosleep](https://cryosleep.io) alongside the GitHub
Actions workflow in `.github/workflows/ci.yaml`. Both run; neither
depends on the other.

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
installed. Package builds push to `insane.cachix.org` when the
`CACHIX_SIGNING_KEY` project secret is set, and skip the push when it
isn't.

## Applying the wiring

The pipeline is versioned here; the two server-side pieces are applied
by hand and only change when this file says so.

```sh
# The webhook endpoint. Prints the ingest URL and signing secret once -
# paste both into Settings -> Webhooks (content type application/json,
# push events).
cryo event-hook set world --manifest github --key "$(openssl rand -hex 20)"

# Route a push to the pipeline. The guard skips pushes that only touch
# markdown.
cryo handler set world-ci .cryo/router.sh --as script \
  --on world.push \
  --when '!has(event.payload.body.commits) || size(event.payload.body.commits) == 0 || event.payload.body.commits.exists(c, (c.added + c.modified + c.removed).exists(p, !p.endsWith(".md")))' \
  --credential GITHUB_STATUS_TOKEN=github-status
```

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
