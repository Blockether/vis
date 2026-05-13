# Autoresearch ideas backlog — Vis × SWE-bench Lite × glm-5.1

Append freely; prune as tried.

## Prompt-level
- Tighten the system prompt: drop tool descriptions for tools unused on the current task (post-classification).
- Inject a short "minimal-edit" reminder ahead of the problem statement.
- Repo-map preamble: pre-compute and inject `git ls-files` + top-level README excerpt.
- Two-pass: planner SCI eval produces a plan var, executor consumes it. Share via SQLite.

## Agent loop
- Reject empty patches and re-prompt with `"your previous turn produced no diff — try again, focused on file X"`.
- Cap journal at K turns of context; offload older entries to SQLite.
- Detect "thrash" (same tool call ≥3×) and force a stop-and-think eval.

## Tool surface
- Add a `repo-grep` tool backed by ripgrep with sane defaults; replace generic shell-grep usage.
- Add a `read-symbol` tool that uses tree-sitter to fetch a function/class by name across the repo.
- Replace large `cat`-style reads with windowed reads + auto-summary.

## SCI runtime
- Pre-warm a fresh SCI image with the file index already loaded.
- Persist named vars to SQLite at end-of-turn; reload on resume.

## glm-5.1 specifics
- Probe whether glm-5.1 follows XML vs. Markdown tool-format better; A/B the system prompt skeleton.
- Try a shorter "thinking budget" hint in the prompt — glm models sometimes ramble.
- Tune temperature 0 vs. 0.2 (held constant per session — switch only between sessions).

## Diff hygiene
- Strip trailing whitespace + EOF-newline normalize before returning the patch (some SWE-bench tests fail on cosmetic deltas).
- Detect when the agent edited a test file and reject the diff (we're forbidden from touching tests).
