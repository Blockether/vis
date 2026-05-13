# TASKS_CONTEXT.md

## Prompt/context hygiene finding — duplicated `turn_system_prompt`

Investigation of conversation `20b36024-57c6-4d77-913a-d0811ee07faf`, slow turn iteration 2 (`98d97440-94c4-4de5-89d6-5c789b55cc02`) found that Vis is duplicating static prompt material into the per-iteration user context.

### Evidence

The provider message array already contains:

- core system prompt as a system message
- extension/environment prompt as a system message
- current user request

But the final per-iteration user trailer also includes `<current_turn_context>`, and inside it:

```text
turn_system_prompt: "<system_prompt>\nλVis — Clojure SCI harness ... ...<+7594 chars>"
turn_active_extensions: [...docs/symbols...]
```

So `turn_system_prompt` is effectively a second copy/truncated echo of the already-sent system prompt. This is prompt bloat and can also defeat/provider-cache less effectively because it appears in dynamic user-role context.

### Classification

**P0/P1 prompt assembly / context hygiene bug.**

`<current_turn_context>` should carry live coordinates and state facts only: ids, turn/iteration positions, current title, maybe previous persisted iteration id. It should not re-embed static system prompt text or verbose extension documentation.

### Proposed fix

In `src/com/blockether/vis/internal/prompt.clj`, `current-turn-context-block` currently includes:

```clojure
"turn_system_prompt: " (context-value-str (or turn-system-prompt "")) "\n"
"turn_active_extensions: " (context-value-str (active-extensions-context-value active-extensions)) "\n"
```

Change this so dynamic context does **not** include full prompt/doc payloads. Options:

1. Remove `turn_system_prompt` entirely, or replace with a hash/version:
   ```text
   turn_system_prompt_sha256: <hash>
   ```
2. Replace verbose `turn_active_extensions` with a compact alias/kind list, e.g.:
   ```clojure
   [{:alias v :kind "foundation"}
    {:alias z :kind "language"}
    {:alias exa :kind "search"}]
   ```
   No docs, no full symbol lists unless explicitly requested by a tool.

### Related context bloat found

- `<journal>` included unrelated cross-turn raw evidence from prior turns.
- `<bindings>` repeated large values already visible in `<journal>`.
- Prior assistant preserved-thinking/content replay was broader than needed for the current task.
- `vis run --full-trace-stream ... "Siema"` showed ~3.8k input tokens baseline for a trivial greeting; manageable but high, and title-setting pressure appears too eager.

Primary actionable first cut: **stop duplicating `turn_system_prompt` inside `<current_turn_context>`.**

## Validation with `vis run --persist --provider zai-coding-plan --model glm-5.1 --json "Siema"`

Ran a persisted CLI probe because plain `vis run --json` is ephemeral (`conversation-id: null`) and does not persist `llm_user_prompt` to the main DB.

Command:

```bash
vis run --persist --provider zai-coding-plan --model glm-5.1 --json "Siema"
```

Persisted conversation: `9b9be6af-4917-4e02-bf47-03fe46b9e8cd`.

DB inspection of `conversation_turn_iteration.llm_user_prompt` confirms the duplication still happens for Z.ai / GLM 5.1:

- iteration 1 prompt chars: `15321`, tokens: `3785`, cached: `960`
- message array contained:
  - msg 0 system: core system prompt (`3223` chars)
  - msg 1 system: extension/environment prompt (`6342` chars)
  - msg 2 user: current user message (`52` chars)
  - msg 3 user: per-iteration trailer (`5085` chars)
- the per-iteration trailer contained:

```text
turn_system_prompt: "<system_prompt>\nλVis — Clojure SCI harness with a recursive eval loop..."
turn_active_extensions: [...]
```

Therefore the bug is not OpenAI/Codex-specific and not a TUI-only persistence artifact. It is in shared prompt assembly (`current-turn-context-block`) and affects `vis run --persist` with `zai-coding-plan/glm-5.1` too.

Important nuance: plain `vis run --json` does **not** expose the prompt in JSON output; it only exposes result/trace/tokens. To inspect actual prompt, use `--persist` and query `conversation_turn_iteration.llm_user_prompt`, or add explicit debug instrumentation.
