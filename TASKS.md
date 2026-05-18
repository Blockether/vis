# Tasks

## Board

### Backlog

- T-001
- T-002
- T-003

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

### T-001 — Remove XML tags and canonicalize `ctx`

**Status:** `backlog`

**Intro:** Replace XML-tagged context prompt shape with one canonical `ctx` model across Vis.

**Rationale:** Prompt context should have one source of truth. `ctx` should be canonical, routed through Vart, and used as foundation for next turns. Prompt must not imply separate `previous turn context` path or competing context concepts.

**Acceptance criteria:**

- XML tags removed from context prompt surfaces.
- Only canonical `ctx` concept remains in Vis prompt/context flow.
- All context flows through Vart path.
- Prompt copy states clearly: current `ctx` is foundation for next turns.
- No `previous turn context` prompt concept remains unless explicitly mapped into canonical `ctx`.
- Regression test or diagnostic verifies canonical `ctx` construction.

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
