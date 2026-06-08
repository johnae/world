These rules apply to all written content: READMEs, docs, walkthroughs, blog posts.

General tone and formatting rules live in `base.md` and apply universally; this
file covers prose-specific concerns.

## Avoid AI-tell phrasing

These patterns read as machine-generated. Cut them or rewrite plainly.

- **The antithesis ("A, not B").** The biggest tell, used as a rhythm: "The
  core is a protocol, not a monolith." "Capability is opt-in software, not core
  weight." "It runs offline, not at build time." State what the thing is ("The
  core is a protocol."). Keep a contrast only when the reader would otherwise
  assume the wrong thing, and then at most once in a passage - never as a
  cadence across bullets or paragraphs.
- **"Not just X, but Y" / "isn't about X, it's about Y".** The same move; drop it.
- **Rule-of-three triads for cadence** ("small, fast, and simple"; "scan, merge,
  resolve"). Fine as a real list; suspicious when it's there for rhythm. Keep a
  triad only if each item earns its place.
- **Filler intensifiers**: "genuinely", "exactly", "precisely", "truly",
  "simply", "the whole point", "it's worth noting", "at the end of the day".
  Delete them; the sentence is stronger without.
- **Manufactured drama**: "Here's the thing", "And that's the beauty of it",
  dramatic em-dash (or " - ") reveals. Say the point directly.
- **Over-hedging then over-asserting** ("It's important to note that X is
  actually quite Y"). State X.
- Don't over use appeal-to-familiarity "the same A that X/Y/Z uses", "the same shape as A and B".
- Don't write long essays over-explaining the "why" in prose. Tigthen it to what a reader needs.

When you catch yourself reaching for emphasis, prefer a concrete example or a
plain declarative over a rhetorical frame.

## User docs are for users, not contributors

User-facing docs (a project's docs site, README, guides) are read by someone
trying to *use* the thing. Keep internal-development artifacts out of them:

- **Don't cite internal design docs** - ADR-0030, TDD-0022, RFC numbers, design
  tickets - in user docs, linked or not. The number means nothing to a user,
  and a link sends them into design archaeology when they wanted to get
  something done. If a design decision matters to the user, state the reason in
  plain user terms; otherwise drop the reference. Cross-references between the
  design docs themselves, and in code comments, are fine - that audience is
  contributors.
- Same for internal jargon, source-tree file paths, and "we decided X in
  phase 2" history. The user doesn't have that context.

## Formatting

- **Line length**: 80-100 chars (except URLs and code blocks).

## READMEs

- Skip the H1 title (GitHub renders it from the repo name and you already know which project you're looking at)
- Open with 1-2 sentences: what it does, who it's for
- Show it working (code example, terminal output) before explaining how to install
- Complete, copy-pasteable examples only - never `foo`/`bar` placeholder code
- Keep under 300 lines. Move API reference, architecture, contributing to separate files.
- 3-5 badges max

## Structure Priority

1. What does this do? (1-2 sentences)
2. Show it working (code/screenshot/GIF)
3. How do I try it? (install + minimal example)
4. Everything else is secondary
