# Vis Tasks

Human backlog. Old benchmark harness was removed; keep this file as the lightweight task index until a new planning format exists.

## Removed from this backlog

- Native workspace manager / worktree materialization / attached repos / multirepo prompt work: another owner is handling it.
- Completed proof-plan backlog and `PROOF.md`: the useful Tasks 28-34 intent/deferred-intent backbone is baseline behavior now.
- Old proof/provenance migration checklist: superseded by the implemented attestation-ledger/intents backbone.
- Duplicated item-by-item historical map: replaced by the task clusters below.

## High-priority implementation tasks

| ID | Purpose |
|---|---|
| `CTX1-context-contract-compact-proof-safe` | Reduce prompt/context bloat without hiding proof, intents, audit, tool evidence, or loaded skill bodies. |
| `PRES1-presentation-render-contract` | Shared presentation/render contract for Markdown/details/tool/system/provider/audit/Mermaid rendering. |
| `EXTV2-extension-contract` | Extension v2 slots: config, toggles, renderers, backgrounds, lifecycle hooks, custom provider descriptors. |
| `CLJEXT1-clojure-test-classpath-tools` | Clojure `z/` helpers for clojure.test/lazytest, deps aliases, classpath/deps/JAR/source inspection. |
| `PYEXT1-python-structured-edit-extension` | Python language extension with tested structured editing. Depends on CLJEXT1. |
| `GAME1-python-snake` | Python Snake fixture task for future manual comparison or demo work. |

## Secondary product tasks

| ID | Purpose |
|---|---|
| `GIT1-single-repo-checkpoints` | Single-repo Git checkpoint/time-travel linked to provenance. Explicitly excludes native workspace/multirepo/submodule scope. |
| `BGNOTIFY1-background-process-agent-end-notify` | Background process lifecycle, bounded output, patch preconditions, agent-end notifications, OSC 777 adapter. |
| `PROVIDER1-provider-visibility-custom-provider` | Provider trace normalization, provider-error diagnostics, custom OpenAI-compatible provider config. |
| `GUIDANCE1-project-guidance-docs-ux` | Compact project guidance/docs/UX cleanup; README stays tiny; Ctrl+Y stays unbound. |
