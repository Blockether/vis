# Workspaces work

Extracted from `TASKS.md`. This is the dedicated plan for native workspaces.

## Current implementation status

Basic worktree slice plus first workspace-context plumbing is implemented. This is
**not** full native workspace lifecycle yet.

Implemented:

- `src/com/blockether/vis/internal/workspace.clj`
  - `create-worktree!`
  - `workspace-status`
  - `workspace-roots`
- Real `git worktree add` for main-repo materialization.
- Runtime state outside the source checkout:
  - `~/.vis/workspaces.edn`
- Materialized worktrees under:
  - `~/.vis/workspaces/<repo-id>/<workspace-id>/`
- TUI built-in command:
  - `/worktree [branch]`
  - creates the worktree
  - opens a new TUI tab
  - stores `:workspace/root` on tab state
  - passes active workspace into the next turn env
  - shows a success notification
- Prompt/runtime binding:
  - bounded `<workspace>` prompt block injected when `:workspace/root` is active
  - extension symbol calls bind active workspace root dynamically
  - extension prompt/environment callbacks bind active workspace root dynamically
- Workspace-root aware surfaces:
  - foundation editing tools: `v/cat`, `v/rg`, `v/glob`, `v/patch`, `v/bash`, and related path helpers
  - Clojure language tools: `z/patch`, `z/xref*`, `z/lsp*`, `z/repair*`
  - environment snapshot and `v/git`
  - project guidance / repo-local skills discovery
  - TUI footer git status
  - TUI file picker, file mentions, and external opener relative paths
- Tests:
  - `test/com/blockether/vis/internal/workspace_test.clj`
  - `test/com/blockether/vis/internal/workspace_context_test.clj`
  - TUI slash-command suggestion expectations include `/worktree [<branch>]`
  - TUI command palette expectations include `New Worktree`
  - workspace-root regression coverage exists for prompt/env, extension symbols, editing bash cwd, `v/git`, footer git, AGENTS.md, and repo skills

Deliberately **not** implemented in this slice:

- JVM process cwd mutation.
- Full workspace lifecycle API: `create-workspace!`, `open-workspace`, `refresh-workspace!`, `destroy-workspace!`.
- Submodule reconciliation.
- Attached repo catalog/materialization.
- Drift detection.
- Dirty-workspace cleanup/destroy safeguards.
- SCI `v/workspace*` symbols.
- Full audit/rewire of every non-turn UI helper that still assumes process cwd.

Important runtime note:

- One TUI runs inside one JVM. Java cwd / `user.dir` is process-global, so
  changing it for one tab would affect all tabs and tools. Native workspace
  binding stores workspace root on tab/conversation state and passes explicit
  cwd to tools/subprocesses instead of mutating process cwd.

Remaining cwd audit items from the first grep pass:

- CLI/config defaults intentionally use invocation cwd and need a separate policy decision.
- Voice/Telegram provider process launches are service-control helpers, not workspace tools; leave unless a workspace-facing use appears.
- Clojure extension activation still checks process cwd, but actual Clojure tools use workspace root once active.

Checklist interpretation for the basic slice:

- [x] Add runtime workspace state store outside the repo.
- [x] Implement main repo materialization with `git worktree add`.
- [~] Create internal workspace manager module and submodules listed above.
  - Basic `com.blockether.vis.internal.workspace` exists.
  - Catalog/main-repo/submodules/attached-repos/reconciler namespaces do not.
- [~] Add `create/open/refresh/destroy/status/roots` public interface.
  - Present: `create-worktree!`, `workspace-status`, `workspace-roots`.
  - Missing: exact `create-workspace!`, `open-workspace`, `refresh-workspace!`,
    `destroy-workspace!`.
- [~] Add regression tests for worktree creation, submodule reconciliation,
  attached repo validation, and ignored nested repo rejection.
  - Present: real worktree creation regression.
  - Missing: submodule, attached repo, ignored nested repo regressions.

## Before this work

Do these first or lock their contracts enough that workspace code does not churn:

- [ ] Finish the presentation/render contract enough to render a bounded workspace prompt block consistently.
- [ ] Finish the extension contract v2 enough to expose workspace config, symbols, background refresh, and prompt contributions through one seam.
- [ ] Confirm install/state root policy: `~/.local/share/vis`, `~/.local/state/vis`, launcher path, and config records.
- [ ] Confirm manifest/catalog file locations and versioning before implementing readers.
- [ ] Define safety policy for dirty workspaces before destructive refresh/destroy operations.
- [ ] Add or confirm test coverage for existing repository/environment discovery namespaces touched by workspace work.

## Workspace implementation plan

Covers: **Ref C, 6, 12, 18, 19**

### Rationale

Vis already detects git state and repositories, but Ref C changes the goal: workspace support is not only discovery and prompt reporting. Vis needs real, isolated, tool-visible workspaces that external tools can operate on normally.

Hard requirements from Ref C:

- LSP servers, grep, test runners, compilers, formatters, and file watchers must see normal on-disk files.
- Main repo writable workspaces use Git worktrees.
- Submodules stay Git-native.
- `.gitignore`d nested repos are never implicitly used.
- Non-submodule nested repos that matter must be declared as attached repos.
- Workspace runtime state lives outside the source checkout.
- One deep workspace manager owns all materialization and cleanup.

### Proposal

Create a deep workspace manager:

```clojure
com.blockether.vis.internal.workspace
com.blockether.vis.internal.workspace.catalog
com.blockether.vis.internal.workspace.main-repo
com.blockether.vis.internal.workspace.submodules
com.blockether.vis.internal.workspace.attached-repos
com.blockether.vis.internal.workspace.reconciler
```

Public interface:

```clojure
(create-workspace! opts)
(open-workspace id)
(refresh-workspace! id)
(destroy-workspace! id)
(workspace-status id)
(workspace-roots id)
```

The manager hides:

- `git worktree add` for main repo materialization;
- submodule sync/update;
- attached repo checkout/symlink materialization;
- isolated vs shared attached repo policy;
- drift detection;
- cleanup;
- runtime state persistence.

### Child kinds

Every child path is owned by exactly one kind:

```clojure
:main       ;; main repo worktree
:submodule  ;; Git-native submodule from .gitmodules and checked-out commit
:attached   ;; explicit nested repo not tracked as submodule
```

Main repo:

```bash
git worktree add <workspace-root> <ref>
```

Submodules:

```bash
git submodule sync --recursive
git submodule update --init --recursive
```

Attached repos:

```clojure
{:path "vendor/private-sdk"
 :kind :attached
 :attached/id "private-sdk"
 :required? true
 :workspace-mode :isolated
 :write-mode :read-write
 :materialization :checkout-at-path
 :ref {:mode :follow-branch :branch "main"}
 :indexing :include}
```

Attached repo modes:

- `:isolated` - default for writable code; each parent workspace gets its own nested checkout.
- `:shared` - opt-in for read-mostly/shared resources; mutation leakage is accepted by configuration.

### Static manifest and attached repo catalog

Workspace shape is project purpose, not inferred from host leftovers.

Static manifest example:

```clojure
{:version 1
 :main {:default-branch "main"}
 :children [{:path "libs/parser"
             :kind :submodule
             :required? true
             :indexing :include}
            {:path "vendor/private-sdk"
             :kind :attached
             :attached/id "private-sdk"
             :required? true
             :workspace-mode :isolated
             :write-mode :read-write
             :materialization :checkout-at-path
             :ref {:mode :follow-branch :branch "main"}
             :indexing :include}]}
```

Attached repo catalog example:

```clojure
{:version 1
 :attached {"private-sdk"
            {:source {:type :git
                      :url "git@github.com:acme/private-sdk.git"}
             :cache {:mirror-name "private-sdk.git"}
             :checkout-default :worktree}}}
```

### Runtime state outside the repo

Materialized workspaces, mirrors, and metadata live outside the main repository checkout, for example:

```text
~/.local/state/vis/repos/<repo-id>/workspaces/<workspace-id>
~/.local/state/vis/repos/<repo-id>/mirrors/<mirror-name>.git
~/.local/state/vis/workspaces/<workspace-id>.edn
```

Runtime state example:

```clojure
{:workspace/id "ws-123"
 :workspace/repo-id "vis"
 :workspace/root "/Users/me/.local/state/vis/repos/vis/workspaces/ws-123"
 :workspace/state :ready
 :main {:ref {:type :branch :name "feature/native-workspaces"}
        :head "9f8e7d6"}
 :children {"libs/parser"
            {:kind :submodule
             :path "libs/parser"
             :state :ready
             :head "abc123"
             :dirty? false}
            "vendor/private-sdk"
            {:kind :attached
             :attached/id "private-sdk"
             :path "vendor/private-sdk"
             :workspace-mode :isolated
             :state :ready
             :head "def456"
             :dirty? false}}}
```

### SCI symbols under `v/`

```clojure
(v/workspace)
(v/workspaces)
(v/workspace-status id)
(v/workspace-roots id)
(v/create-workspace! opts)
(v/open-workspace id)
(v/refresh-workspace! id)
(v/destroy-workspace! id)
(v/repositories)
(v/git-summary)
```

### Prompt block

```text
<workspace>
id: ws-123
root: /Users/me/.local/state/vis/repos/vis/workspaces/ws-123
repo-id: vis
state: ready
main:
  branch: feature/native-workspaces
  head: 9f8e7d6
children:
  - path: libs/parser
    kind: submodule
    state: ready
    dirty: false
  - path: vendor/private-sdk
    kind: attached
    mode: isolated
    state: ready
    dirty: false
</workspace>
```

Installer target layout:

```text
~/.local/share/vis      repo clone
~/.local/state/vis      workspace runtime state, mirrors, materialized workspaces
~/.local/bin/vis        launcher symlink/script
~/.vis/config.edn       runtime config
```

Config records:

```edn
{:vis/install-root "/Users/me/.local/share/vis"
 :vis/state-root "/Users/me/.local/state/vis"
 :vis/original-source "https://github.com/Blockether/vis.git"}
```

### Invariants

- [ ] Every declared child path is owned by exactly one kind: main, submodule, or attached.
- [ ] Every `:required? true` child exists before LSP starts.
- [ ] Writable attached repos default to `:isolated`.
- [ ] Shared attached repos are opt-in and assumed cross-workspace mutable unless marked read-only.
- [ ] `.gitignore`d nested repos are never implicitly used.
- [ ] Workspace manager is the only module allowed to materialize child repos.
- [ ] Runtime state never lives inside the source checkout.

### Acceptance tasks

- [ ] Create internal workspace manager module and submodules listed above.
- [ ] Add static workspace manifest reader/validator.
- [ ] Add attached repo catalog reader/validator.
- [x] Add runtime workspace state store outside the repo.
- [x] Implement main repo materialization with `git worktree add`.
- [ ] Implement submodule reconciliation with `git submodule sync/update --recursive`.
- [ ] Implement attached repo materialization for `:isolated` checkout-at-path.
- [ ] Implement attached repo materialization for `:shared` symlink/shared-path, opt-in only.
- [ ] Add drift detection for main repo, submodules, and attached repos.
- [ ] Add cleanup/destroy semantics, including dirty workspace safeguards.
- [ ] Add `create/open/refresh/destroy/status/roots` public interface.
- [ ] Add `v/workspace`, `v/workspaces`, `v/workspace-status`, `v/workspace-roots` symbols.
- [ ] Add `v/create-workspace!`, `v/open-workspace`, `v/refresh-workspace!`, `v/destroy-workspace!` symbols if safe for SCI.
- [ ] Detect multirepositories only as candidates; require manifest for attached repos that matter.
- [x] Render bounded workspace prompt summary.
- [ ] Include dirty/clean, changes, stash count, stale/ahead/behind where available.
- [ ] Cache bounded status scans; never block prompt assembly indefinitely.
- [ ] Add installer script for `~/.local/share/vis`, `~/.local/state/vis`, and `~/.local/bin/vis`.
- [ ] Record install root/state root/original source in config/system prompt.
- [x] Add regression tests for worktree creation.
- [ ] Add regression tests for submodule reconciliation, attached repo validation, and ignored nested repo rejection.

### Completed next-slice workspace tab/context plumbing

- [x] Store workspace root on TUI tab state.
- [x] Bind active workspace into conversation/env for a turn.
- [x] Inject bounded `<workspace>` prompt block automatically.
- [x] Run available tools/subprocesses with explicit workspace-root cwd.
- [x] Route grep/LSP/Clojure language tools from workspace root where available.
- [x] Route `v/git` / environment snapshot from workspace root.
- [x] Route project guidance and repo-local skill discovery from workspace root.
- [x] Route TUI footer git status from active tab workspace root.
- [x] Avoid process-global cwd/user.dir mutation.
- [x] Route non-turn TUI file picker and external opener relative paths from active tab workspace root.

### Open questions

- [ ] Should shared attached repos default to `:indexing :exclude`?
- [ ] Should attached repo refs support tags, or only pinned SHA and branch-following?
- [ ] Should attached repo materialization prefer nested worktrees over lightweight clones in all cases?
- [ ] How should dirty attached repos affect refresh and destroy semantics?
- [ ] Do we need a lockfile or lease mechanism for concurrent workspace reconciliation?

### Considered alternatives

1. Pure in-memory filesystem.
   - Rejected: external tools need real files, real paths, and normal watcher semantics.
2. FUSE or mounted virtual filesystem.
   - Rejected: platform-specific complexity and watcher/save/rename risk without clear advantage over real worktrees.
3. Implicit `.gitignore`d nested repos.
   - Rejected: host-local ignored directories make workspaces non-reproducible and hard to diagnose.
4. Keep cwd-only model.
   - Too weak for conversation/branch isolation.

### Consequences

- LSP, grep, test runners, compilers, and formatters see a normal filesystem.
- Main repo uses Git-native worktrees.
- Submodules preserve Git semantics.
- Ignored nested repositories become explicit and reproducible.
- One module owns workspace complexity and improves locality.
- Workspace creation depends on Git availability and repository hygiene.
- Attached repo manifests and lifecycle machinery become first-class.
- Cleanup and drift detection become operational concerns.

---

## After this work

Do these after native workspaces are real and tested:

- [ ] Build Git checkpoints/time travel on top of workspace ids and roots.
- [ ] Add checkpoint aggregates spanning main repo, submodules, and attached repos.
- [ ] Wire conversation switching to workspace open/refresh semantics.
- [ ] Add user-facing restore flows for conversations only, files only, and both together.
- [ ] Add docs for manifest authoring, attached repo catalog, workspace lifecycle, and troubleshooting dirty/drifted workspaces.
- [ ] Run full regression suite plus real-process tests for multiprocess SQLite and Git workspace materialization.
