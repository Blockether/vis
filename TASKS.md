# Tasks

## Board

### Backlog

- T-002
- T-003

### Done

- T-001

### Carol

- 

### Alex

- 

## Status values

- `backlog`
- `in-progress`
- `blocked`
- `review`
- `done`

## Tasks

### T-001 — Remove prompt-control tag protocol and canonicalize `ctx`

**Status:** `done`

**Outcome:** Prompt-control tag protocol fully retired. Per-turn dynamic state lives in `ctx` under `:session` / `:llm-provider` / `:project` / `:extensions` / `:defs`. Static prose lives in `;; -- TAG --` comment sections. Per-iteration trailer carries prior REPL observations plus `;; ctx = <edn>` for fresh state. Provider failures emit `;; llm-provider-error = <edn>` and thread `[:llm-provider :error]` into next-iteration `ctx`. Regression tests in `test/com/blockether/vis/internal/ctx_test.clj` and `extensions/common/vis-foundation-core/test/.../transcript_test.clj` assert the retired wrappers stay out of live prompt/trailer paths.

### T-002 — Strengthen tool prompts from codebase source

**Status:** `backlog`

**Intro:** Improve tool-facing prompt text by updating source code in Vis, not by ad-hoc prompt patching.

**Rationale:** Tool instructions must be maintained where tools are defined. Stronger prompt copy should live in codebase-owned extension prompt surfaces so runtime behavior and docs stay aligned.

**Acceptance criteria:**

- Tool prompt weaknesses identified from current runtime prompt output.
- Prompt text strengthened in relevant Vis source namespace(s).
- Foundation/tool prompt ownership boundaries preserved.
- No extension-specific prose added to core engine prompt.
- Runtime prompt output confirms updated tool guidance.
- Matching tests updated or added.

### T-003 — Queue TUI messages while prior message processes

**Status:** `backlog`

**Intro:** Preserve user messages entered in TUI while previous message is still processing.

**Rationale:** Current behavior can lose second message when first message is still processing, with no visible feedback. User input should never vanish silently. TUI should either enqueue message or show explicit blocked/busy state.

**Acceptance criteria:**

- Reproduce lost-message behavior through TUI-safe diagnostic or integration path.
- Second message entered during processing is queued or visibly rejected with durable feedback.
- User sees processing/queued state clearly.
- No silent message loss.
- Regression test covers concurrent/rapid message submission path.
