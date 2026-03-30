These rules apply to all written content: READMEs, docs, walkthroughs, blog posts.

## Tone

- Conversational - like talking to a colleague, not writing a textbook
- Direct - get to the point, no throat-clearing
- Honest - don't oversell, say what it does
- No emdashes (use normal -), no marketing buzzwords, no "revolutionary"

## Formatting

- **Bold**: sparingly, for genuine emphasis only. Never bold every other word.
- **Italics**: avoid entirely - hard to read, rarely adds value. Use `code` for
  technical terms and bold for emphasis instead.
- **Emojis**: fine in moderation (section headings). Never in code examples or
  as bullet substitutes.
- **Line length**: 80-100 chars (except URLs and code blocks).
- **Code blocks**: always specify the language.

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
