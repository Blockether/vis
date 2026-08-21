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
- In a non-repository parent that contains Git repositories, bare `(br/init)`
  refuses to guess. Pass `{"root": "/abs/path/to/project"}`; initialization
  still targets exactly one project.
- `(br/run-evidence id)` runs one configured evidence command and writes the
  Bridge receipt. Candidate evidence accepts `"is_index" true`, or pinned
  `"tree"` and `"frontier"` values.

Observation tools:

- `(br/profile)` returns the active Bridge profile summary. When no profile is
  configured, it returns discovery state and the next setup step instead of
  failing.
- `(br/check)` runs Bridge's verification status check for the current
  workspace. When no profile is configured, it returns
  `{:configured? false :status "unconfigured" ...}`.
- Bare `(br/check)` keeps iterative working-snapshot semantics. Pass
  `{"is_index": true}` for the exact staged candidate, or
  `{"tree": sha, "frontier": sha}` for a pinned candidate. A clear candidate
  is recorded only when `"is_approve": true`.
- When configured, `br/check` returns Bridge's **canonical status summary**
  (`:summary-version` 2, produced by `bridge.api/check` — the same shape as
  `bb bridge check --format json`): `:counts`, `:required-obligations`
  (flattened, failed first), `:recommended-obligations`,
  `:evidence-receipts`, and `:next-action`, plus the Vis envelope keys
  `:configured?`, `:profile-path`, and `:policy-path`. The extension adds no
  flattening of its own — summary semantics live in the Bridge kernel.
- There is no separate `br/next` wrapper. Callers inspect the canonical
  `:next-action` returned by `br/check`.
- The extension emits no proactive next-action hint. Bridge status remains an
  explicit pull through `br/check`.
- `(br/list-evidence)` lists configured evidence commands. When no profile is
  configured, it returns an empty command list plus setup guidance.
- `(br/run-evidence id)` remains a failure when no profile exists, but the
  error is concise and actionable rather than stack-oriented.

The extension also enforces Bridge policy `:bridge-path-sandbox` rules through
the Vis `:fs/access` gate hook. When policy enforcement is enabled, Bridge
`path-pattern` entries become workspace-relative globs the extension itself
matches — for the foundation editing tools and for the Python sandbox's own
filesystem alike. Directory patterns such as `.bridge/` are expanded
to subtree globs such as `.bridge/**`; relative policy paths are resolved
against the normalized Bridge profile root. In a multi-repository workspace,
rules from every discovered Bridge project are combined, with nested project
roots ordered before ancestors and then lexically.
If a discovered profile or policy is malformed, ordinary project paths remain
usable while that project's `.bridge/**` stays denied with the exact validation
field path in both the `br/*` tool failure and filesystem refusal. A broken
governance file must not crash unrelated filesystem reads or silently disable
protection of Bridge-owned state.

All tools accept an optional opts map where relevant:

```clojure
{"root" "/abs/path/to/project"
 "profile" "/abs/path/to/.bridge/profile.yaml"
 "policy" "/abs/path/to/verification-policy.yaml"
 "changed_files" ["src/foo.clj"]
 "is_index" true
 "tree" "candidate-tree-or-commit"
 "frontier" "approved-base-tree-or-commit"
 "is_approve" true
 "subject" "core"
 "out_dir" ".bridge/ephemeral/evidence"
 "out" ".bridge/ephemeral/evidence/unit.yaml"
 "timeout_seconds" 300
 "is_dry_run" true}
```

`br/run-evidence` supports `"is_dry_run" true` to return the execution plan
without running a command or writing a receipt. Candidate selection is mutually
exclusive with `"changed_files"`.

## Session Context

The extension contributes only Bridge project identity to the live Vis session
object:

```python
session["env"]["bridge"] = {
    "projects": [
        {"root": "/abs/project", "profile_path": "/abs/project/.bridge/profile.yaml"}
    ],
    # Present only when bare br/* operations have a safe default:
    "default_profile_path": "/abs/project/.bridge/profile.yaml",
    # Present only when the bounded repository scan stopped early:
    "discovery_truncated": True,
}
```

All keys at this boundary are strings. The entire `bridge` slice is omitted
when no configured project is discovered. It intentionally contains no status,
obligations, receipts, or next action: those are fresh, explicit outputs of
`br/check`, not ambient session state.

## Runtime Flow

1. The agent reproduces and inspects through normal Vis tools.
2. If the repo is new to Bridge, the agent calls `(br/init)` or supplies an
   explicit root in a multi-repository parent.
3. The agent edits through `v/patch`.
4. The agent calls `(br/check)`, passing an explicit profile when the session
   has no safe default.
5. Bridge maps changed files to subsystems and policy obligations.
6. The agent inspects `:next-action` in the returned status.
7. When needed, the agent calls `(br/run-evidence id)` for a configured
   command.
8. The agent calls `(br/check)` again and reports clear status or remaining
   obligations in the final answer.

A commit surface calls `internal.git/commit!`.
That generic Vis operation owns Git syntax and the exact
tree invariant: it resolves the effective repository after Git-global options,
rejects index-mutating commit flags/pathspecs, computes T0 with `write-tree`,
dispatches `:git/commit`, rechecks T0 immediately before Git, and asserts
`HEAD^{tree} == T0` afterward.

The Bridge extension contributes a declarative lifecycle-owned around hook:

```clojure
{:op :git/commit
 :phase :around
 :fn bridge-commit-gate}
```

The hook receives only semantic context (`:root`, `:candidate-tree`, and
`:index-preserving?`). It runs an exact Bridge index check with explicit
approval, compares Bridge's candidate with T0, and either invokes the inner
operation or refuses with the next evidence action. It never parses Git
arguments and returns no magic candidate field. The commit caller has no Bridge-specific
knowledge.

Required evidence is never run as a side effect of commit. Git options such as
`-a` (including combined short forms), commit pathspecs, and
`--pathspec-from-file` are refused by Vis; stage the intended tree first.
Git-global options such as `git -C other-repo commit` are resolved before the
semantic hook, so Bridge checks that repository rather than the original
workspace. Ambiguous multi-project discovery fails closed.

Repository hooks and CI are optional, independent project-policy adapters; Vis
commit enforcement does not depend on either. Enabling the unrestricted shell
surface is a deliberate escape from Vis-managed Git operations (Python
`subprocess` routes through that same shell surface).

## Profile Discovery

The extension reuses Vis's cached, bounded Git repository inventory beneath the
primary workspace root. The inventory scans at most 64 repositories and skips
known VCS metadata, cache, vendor, and build roots. Dot-prefixed repositories
remain discoverable. `refresh()` invalidates the inventory; profile files inside
already-known repositories are probed on every contribution and become visible
immediately.

For the active root and each discovered Git root, the extension checks:

1. `.bridge/profile.yaml`
2. `.bridge/profile.yml`
3. `.bridge/persistent/profile.yaml`
4. `.bridge/persistent/profile.yml`

Selection is deterministic and conservative:

1. An explicit `{"profile": path}` wins.
2. Otherwise a profile at the active workspace root wins.
3. Otherwise exactly one discovered profile is the default only when repository
   discovery completed.
4. Multiple candidates, or a truncated scan without an active-root profile,
   produce a structured ambiguity error with candidates and require an explicit
   profile.

Bridge remains single-project per operation. The extension has no hidden
"current project" and no aggregate check verdict. Cross-repository work runs
one `br/check` for each touched profile. Additional attached filesystem roots
are not scanned in this first integration; their profiles remain available
through the explicit selector.

## Boundaries

Vis-owned:

- user interaction and channels
- model routing and provider selection
- prompt assembly
- filesystem edits
- transcript and reproduction artifacts

Bridge-owned:

- `.bridge/profile.yaml`
- `.bridge/verification-policy.yaml`
- subsystem and requirement matching
- evidence command plans
- evidence execution and receipts
- convergence and completeness state

Extension-owned:

- Git-root-based Bridge project discovery below the primary workspace
- safe default selection and structured ambiguity reporting
- string-keyed project identity in `session["env"]["bridge"]`
- converting Bridge library calls into plain Vis tool envelopes
- registering op tags and prompt guidance
- enforcing Bridge path sandbox policy through the `:fs/access` gate hook
- declaring the fail-closed semantic `:git/commit` around hook

The extension consumes Bridge exclusively through `bridge.api` (Bridge's
public library contract, pinned by release version; see `bridge/docs/api.md`
upstream). No other `bridge.*` namespace is required anywhere in this
extension — needing one is the signal to grow the upstream contract
instead.

## Non-goals

- No dedicated Bridge channel yet.
- No duplicate policy engine inside Vis.
- No direct storage poking into Vis or Bridge internals.
- No automatic evidence execution without an explicit `br/run-evidence` call.
- No aggregate multi-project verdict or implicit mutable current-project state.
- No Git argument parsing or Git-specific result protocol in the Bridge
  extension.
- No claim to intercept the deliberately unrestricted shell or external Git
  clients.
