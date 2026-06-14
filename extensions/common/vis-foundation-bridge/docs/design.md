# Bridge Vis Extension Design

## Purpose

The Bridge extension exposes Bridge's verification coordinator to the Vis
agent loop. Vis remains responsible for conversation, workspace inspection,
edits, channels, providers, and final-answer discipline. Bridge remains
responsible for project profiles, verification policy, changed-file impact,
evidence obligations, evidence command execution, receipts, and convergence.

This extension is only an adapter between those systems.

## Exposed API

The model-facing namespace is `br/`.

Mutation tools:

- `(br/init)` bootstraps Bridge in the current workspace. If Bridge is already
  configured, it returns the discovered profile path instead of failing.
- `(br/run-evidence id)` runs one configured evidence command and writes the
  Bridge receipt.

Observation tools:

- `(br/profile)` returns the active Bridge profile summary. When no profile is
  configured, it returns discovery state and the next setup step instead of
  failing.
- `(br/check)` runs Bridge's verification status check for the current
  workspace. When no profile is configured, it returns
  `{:configured? false :status "unconfigured" ...}`.
- When configured, `br/check` returns Bridge's **canonical status summary**
  (`:summary-version` 1, produced by `bridge.api/check` — the same shape as
  `bb bridge check --format summary`): `:counts`, `:required-obligations`
  (flattened, failed first), `:recommended-obligations`,
  `:evidence-receipts`, and `:next-action`, plus the Vis envelope keys
  `:configured?`, `:profile-path`, and `:policy-path`. The extension adds no
  flattening of its own — summary semantics live in the Bridge kernel.
- `(br/next)` returns the next suggested Bridge action, expressed as `br/*`
  extension operations instead of shell commands.
- Bridge does not emit advisory `:turn.iteration/start` hook tasks. In the DAG
  model, Bridge obligations should be surfaced as structured graph state owned
  by Bridge, not as dismissible model instructions such as `vis.bridge/next`.
  When Bridge is unconfigured, the workspace is normal and no standing setup
  hint is emitted. `br_init()` discoverability comes from the static extension
  prompt instead.
- `(br/list-evidence)` lists configured evidence commands. When no profile is
  configured, it returns an empty command list plus setup guidance.
- `(br/run-evidence id)` remains a failure when no profile exists, but the
  error is concise and actionable rather than stack-oriented.

The extension also exposes Bridge policy `:bridge-path-sandbox` rules through
Vis `:ext/protected-paths`. When policy enforcement is enabled, Bridge
`path-pattern` entries become workspace-relative protected globs for the
foundation editing tools. Directory patterns such as `.bridge/` are expanded
to subtree globs such as `.bridge/**`; relative policy paths are resolved
against the normalized Bridge profile root.

All tools accept an optional opts map where relevant:

```clojure
{:profile "path/to/profile.edn"
 :policy "path/to/verification-policy.yaml"
 :changed-files ["src/foo.clj"]
 :subject "core"
 :out-dir ".bridge/ephemeral/evidence"
 :out ".bridge/ephemeral/evidence/unit.yaml"
 :timeout-seconds 300
 :dry-run? true}
```

`br/run-evidence` supports `:dry-run? true` to return the execution plan
without running a command or writing a receipt.

## Runtime Flow

1. The agent reproduces and inspects through normal Vis tools.
2. If the repo is new to Bridge, the agent calls `(br/init)`.
3. The agent edits through `v/patch`.
4. The agent calls `(br/check)`.
5. Bridge maps changed files to subsystems and policy obligations.
6. The agent can call `(br/next)` to get the next recommended action as a
   `br/*` operation.
7. The extension may also emit a native Vis hint that reminds the model to
   inspect Bridge state via `(br/next)` without automatically escalating into
   evidence execution.
8. When needed, the agent calls `(br/run-evidence id)` for a configured
   command.
9. The agent calls `(br/check)` again and reports clear status or remaining
   obligations in the final answer.

## Profile Discovery

The extension resolves profile paths relative to the active Vis workspace
root, then checks:

1. `.bridge/profile.edn`
2. `.bridge/persistent/profile.edn`

Callers can override discovery with `{:profile "..."}`.

## Boundaries

Vis-owned:

- user interaction and channels
- model routing and provider selection
- prompt assembly
- filesystem edits
- transcript and reproduction artifacts

Bridge-owned:

- `.bridge/profile.edn`
- `.bridge/verification-policy.yaml`
- subsystem and requirement matching
- evidence command plans
- evidence execution and receipts
- convergence and completeness state

Extension-owned:

- profile discovery from the active workspace
- converting Bridge library calls into plain Vis tool envelopes
- emitting advisory native Vis hints that point to `br/*` operations
- registering op tags and prompt guidance
- translating Bridge path sandbox policy into Vis protected-path declarations

The extension consumes Bridge exclusively through `bridge.api` (Bridge's
public library contract, pinned by git SHA; see `bridge/docs/api.md`
upstream). No other `bridge.*` namespace is required anywhere in this
extension — needing one is the signal to grow the upstream contract
instead.

## Non-goals

- No dedicated Bridge channel yet.
- No duplicate policy engine inside Vis.
- No direct storage poking into Vis or Bridge internals.
- No automatic evidence execution without an explicit `br/run-evidence` call.
