This is the NixOS configuration repo `world`, on a branch where
`nix flake update` has just bumped the flake inputs. The output of
`nix run .#world -- check` is in `/tmp/check.log`.

Fix what the update broke, and the deprecation warnings it surfaced.

## What happens to your changes

This branch becomes a pull request. If the build passes, GitHub merges
it automatically, and the hosts defined in this repo pick the result up
on their next automatic rebuild. There is no human between your edit and
a machine applying it.

So the build passing is not the bar. A change can evaluate, build, merge
and still break a machine — a renamed option silently dropped, a service
that no longer starts, a firewall rule that stops applying. Those are
the failures worth being afraid of, because nothing downstream catches
them.

## Constraints, in order

1. **Preserve behaviour exactly.** A renamed option becomes its
   documented replacement with the same effect. If an option was removed
   rather than renamed, the equivalent behaviour matters more than the
   shortest diff. Never satisfy a warning by deleting the setting that
   caused it.

2. **When unsure, stop.** If you cannot convince yourself a change is
   behaviour-preserving, do not make it. Leave it broken and write it
   down. A failing build that explains itself is a good outcome; a green
   build that quietly changed a machine is the worst one.

3. **Keep changes small and local.** No refactoring, restyling or
   reformatting. A reviewer should read the diff as "the update needed
   exactly these repairs".

4. **Prefer the boring fix.** If a warning can be resolved either by
   adopting a new option or by pinning the old behaviour explicitly,
   pin it. Adopting new behaviour is a decision for a person.

## Treat these as needing a human, not a fix

- Anything touching boot, disk layout, filesystems or the bootloader.
- Anything touching networking reachability: firewall, ssh, wireguard,
  DNS resolution.
- Secrets, key material, or how they are decrypted at activation.
- Anything where the replacement option has different defaults from the
  one it replaces, even if the rename looks mechanical.
- A change you would describe as "probably fine".
- **The update tooling itself** — `misc/gh-release-update.*` and
  anything else that decides *which* versions get pinned. You may be
  shown a failure in it, and it may look mechanical. It is the one place
  where a passing build proves nothing: that script chooses release
  URLs, and a wrong choice still evaluates, still builds, still merges,
  and pins the wrong version on every host. Report what is broken and
  what you would have changed; do not change it.

For each of these, leave it as it is and report it.

## Do not touch

`flake.lock` — the update already wrote it. `.cryo/` — CI wiring, not
system configuration.

## Report

Write `/tmp/repair-report.md`, even if you changed nothing:

1. What you changed and why, one bullet each, naming the option and its
   replacement.
2. A section `## Needs a human` for everything you deliberately left:
   what it is, why it is not mechanical, and what you would have had to
   decide to fix it.

That file becomes the pull request description, so write it for whoever
reviews the merge.

Verify with `nix run .#world -- check` before finishing.
