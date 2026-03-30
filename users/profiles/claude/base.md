## Responses

- Concise and direct. Fewer than 4 lines unless detail requested.
- No preamble/postamble ("Here's what I'll do...").
- Batch tool calls and run commands in parallel when possible.

## Code Comments

Comments are good when they add value. Do NOT add comments on obvious code - that
detracts from reading. Public APIs and complex logic should be documented. Comments
must always be accurate - misleading comments are worse than none.

## Files

Always ensure a trailing newline in both new files and files you edit.

## Version Control

Use **jujutsu (jj)**, never raw git. Detect `.jj` directory first, fall back to `.git`
only if there is not `.jj` directory.

### Changeset Awareness (CRITICAL)

Before ANY code change, description update, or `jj new`: run `jj log --limit 3` and
`jj status` first.

- **Know where you are** before making changes. Don't blindly create changesets or
  overwrite descriptions.
- **Empty changeset?** Just use it. Never stack empty on empty.
- **Has meaningful description?** Refine, don't replace.
- **Changes belong with current work?** Just add them, no new changeset needed.

### Commit Format

Conventional commits: `type(scope): description` (imperative mood, max 80 chars).
Types: `feat`, `fix`, `chore`, `docs`, `refactor`, `test`, `perf`, `ci`.
Never mention iteration numbers, plan references, or gitignored files in descriptions.
If only gitignored files changed, don't create a changeset or update description.
`jeriksson-local/` and `local/` tend to both be gitignored.

## Iteration Workflow

1. Check current changeset state. Describe orphan changesets from previous work.
2. Work through tasks. One changeset per logical unit (small, self-contained).
3. When a changeset is complete: run the code-simplifier agent on it before moving on.
   Do NOT create a new changeset for the simplifier - it runs on the current one.
   Do NOT update the description after code-simplifier runs.
4. `jj new` for the next piece of work (or docs updates if needed).
5. Review test coverage for the changes. Add tests where missing.

## Testing

TDD preferred: write tests first, implement to pass.
For state transitions and decision points, test ALL outcomes - not just happy path.

## Dev Environment

Uses [devenv](https://devenv.sh). Environment loaded via `CLAUDE_ENV_FILE`, so run
commands directly. Use the devenv MCP server when interacting with devenv config.
When there is a devenv.nix in a project, that project uses devenv basically. Then rely
on devenv for adding additional tooling.

## Build System

Giant is the build tool in repos with `giant.yaml`/`giant.jsonnet`. Always prefer giant
over raw cargo/go/etc commands - it handles dependency graphs and caching.
Use `giant docs <topic> --plain` for giant documentation.

## Tips

Use exa and context7 for searching in addition to standard web search. If instructed to do
research for example, exa is a good mcp tool. For up-to-date api documentation, context7 can
be very helpful.

## MCP Tools

- **Context7**: `resolve-library-id` then `query-docs` for library docs.
- **Exa**: Web searches optimized for precise results.
- **Devenv**: For devenv configuration changes.
