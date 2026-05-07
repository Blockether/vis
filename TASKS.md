# Vis Tasks

Current source of truth for runnable benchmark/backlog work:

```text
bench/opus/tasks.jsonl
bench/opus/cli-tasks.jsonl
autoresearch.md
```

`TASKS.md` is now only a human index. Detailed task prompts live in JSONL so Pi/Vis autoresearch can run them directly and Opus can judge observed artifacts.

## Removed from this backlog

- Native workspace manager / worktree materialization / attached repos / multirepo prompt work: another owner is handling it.
- Completed proof-plan backlog and `PROOF.md`: the useful Tasks 28-34 intent/deferred-intent backbone is folded into `CLJEXT1-clojure-test-classpath-tools` as self-contained baseline.
- Old proof/provenance migration checklist: superseded by the implemented attestation-ledger/intents backbone and the Opus benchmark tasks.
- Duplicated item-by-item historical map: replaced by runnable task IDs below.

## Autoresearch task map

### Substrate / measurement

| ID | Purpose |
|---|---|
| `S0-hi` | Opus smoke; verifies Pi and Vis can answer and captures Vis internal `duration-ms`. |
| `C1-provider-path` | Verify correct Vis Opus provider path: `anthropic-coding-plan/claude-opus-4-7`. |
| `C2-vis-duration-metric` | Ensure benchmark uses `result.json ."duration-ms" / 1000`, not Vis CLI wall time. |
| `C3-structural-edit-api` | Verify agents know `z/patch` is preferred Clojure structural-edit API. |

### CLI/TUI speed

In `bench/opus/cli-tasks.jsonl`:

| ID | Purpose |
|---|---|
| `VCLI-root-help` | Cold root help speed. |
| `VCLI-providers-list` | Provider command startup/discovery speed. |
| `VTUI-help` | TUI help fast path; should avoid full Lanterna runtime. |
| `VTUI-open-missing-conversation` | Headless-safe proxy for TUI opening speed with disposable DB. |

### High-priority real implementation tasks

| ID | Purpose |
|---|---|
| `CTX1-context-contract-compact-proof-safe` | Reduce prompt/context bloat without hiding proof, intents, audit, tool evidence, or loaded skill bodies. |
| `PRES1-presentation-render-contract` | Shared presentation/render contract for Markdown/details/tool/system/provider/audit/Mermaid rendering. |
| `EXTV2-extension-contract` | Extension v2 slots: config, toggles, renderers, backgrounds, lifecycle hooks, custom provider descriptors. |
| `CLJEXT1-clojure-test-classpath-tools` | Clojure `z/` helpers for clojure.test/lazytest, deps aliases, classpath/deps/JAR/source inspection. Includes Tasks 28-34 intent backbone as baseline. |
| `PYEXT1-python-structured-edit-extension` | Python language extension with tested structured editing. Depends on CLJEXT1. |
| `GAME1-python-snake` | Pi vs Vis implement Python Snake in fixture repo; Opus judges correctness/playability/tests/context/speed. |

### Secondary product tasks

| ID | Purpose |
|---|---|
| `GIT1-single-repo-checkpoints` | Single-repo Git checkpoint/time-travel linked to provenance. Explicitly excludes native workspace/multirepo/submodule scope. |
| `BGNOTIFY1-background-process-agent-end-notify` | Background process lifecycle, bounded output, patch preconditions, agent-end notifications, OSC 777 adapter. |
| `PROVIDER1-provider-visibility-custom-provider` | Provider trace normalization, provider-error diagnostics, custom OpenAI-compatible provider config. |
| `GUIDANCE1-project-guidance-docs-ux` | Compact project guidance/docs/UX cleanup; README stays tiny; Ctrl+Y stays unbound. |

## Autoresearch rules

- Run exact task prompts from JSONL.
- Hard implementation tasks require paired isolated worktrees and Opus judge over diffs, tests, logs, timings, tokens, tool-call metrics, and proof/trace metrics.
- Vis benchmark DBs are disposable under `target/vis-bench/**`; never delete `~/.vis` automatically.
- If Vis has an obvious substrate error, fix it before treating the result as benchmark signal.
- Context, speed, correctness, tool-call reliability, and proof/honesty all matter.
- Do not claim Vis beats Pi from one task; use suite rollup.
