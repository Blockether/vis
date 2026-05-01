# ADR: Native workspaces via Git worktrees, submodules, and explicit attached repos

- Status: Draft
- Date: 2026-05-01

## Context

Vis needs a workspace model that feels virtual to the agent while remaining fully visible to external tooling:

- LSP servers must see normal on-disk files.
- Search, formatters, test runners, and compilers must operate on real paths.
- Git submodules must keep their native semantics.
- Nested repositories hidden behind `.gitignore` must not silently leak host-specific state into workspaces.

A purely in-memory filesystem fails the tooling-compatibility test. A FUSE-style virtual filesystem adds platform and watcher complexity without solving the core requirement better than a normal materialized workspace.

The architectural problem is therefore not "how to fake a filesystem", but "how to give each conversation or branch an isolated, tool-visible workspace with predictable nested-repository behavior".

## Decision

Vis will treat **Git worktrees as a native workspace primitive**.

Each workspace will be a real directory on disk composed from three child kinds:

1. **Main repo** — materialized as a Git worktree.
2. **Submodules** — reconciled by Git from the main repo commit.
3. **Attached repos** — explicit runtime-managed nested repositories not tracked by the main repo.

### 1. Main repo

The main project root for a workspace is created via `git worktree add`.

This is the default materialization mechanism for any writable workspace the agent or LSP will operate on.

### 2. Submodules

Submodules remain Git-native. On workspace create or refresh, Vis will run:

```bash
git submodule sync --recursive
git submodule update --init --recursive
```

The workspace manager does not invent submodule placement or ref selection. Those stay owned by `.gitmodules` and the checked-out superproject commit.

### 3. Attached repos

Any nested repository that matters to workspace correctness but is **not** tracked as a submodule must be declared explicitly as an **attached repo**.

Attached repos exist because Git worktrees do not manage `.gitignore`d repo-in-repo directories. Leaving those directories implicit would make workspaces non-reproducible and host-dependent.

Attached repos support two modes:

- **`:isolated`** — default for writable code. Each parent workspace gets its own nested checkout.
- **`:shared`** — opt-in for read-mostly or intentionally shared resources.

The default materialization for isolated attached repos is a real checkout at the final nested path inside the parent workspace, typically backed by a local bare mirror plus `git worktree add`.

Shared attached repos may be materialized by symlink or mount, but only when cross-workspace mutation is acceptable.

### 4. Explicit manifest

Workspace shape will be defined by project intent, not inferred from host leftovers.

Vis will maintain:

- a **static manifest** in the repo describing required child repos and their modes,
- a **runtime state record** outside the repo describing what was materialized for each workspace.

### 5. Runtime state outside the repo

Materialized workspaces, mirrors, and runtime metadata live outside the main repository checkout.

This avoids:

- recursive indexing of workspace farms,
- Git confusion from nested sibling worktrees,
- accidental inclusion in searches and status output,
- workspace state polluting project source.

### 6. Workspace manager as one deep module

Vis will concentrate all workspace materialization logic behind one module interface, responsible for:

- create workspace,
- open workspace,
- refresh workspace,
- destroy workspace,
- report workspace roots and status.

The module hides the implementation complexity of Git worktrees, submodule reconciliation, attached repo materialization, drift detection, and cleanup.

## Data model

### Static intent

Example project manifest:

```clojure
{:version 1

 :main
 {:default-branch "main"}

 :children
 [{:path "libs/parser"
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
   :ref {:mode :follow-branch
         :branch "main"}
   :indexing :include}

  {:path "vendor/internal-docs"
   :kind :attached
   :attached/id "internal-docs"
   :required? false
   :workspace-mode :shared
   :write-mode :read-only
   :materialization :symlink
   :ref {:mode :pinned
         :rev "a1b2c3d4"}
   :indexing :exclude}]}
```

Example attached repo catalog:

```clojure
{:version 1

 :attached
 {"private-sdk"
  {:source {:type :git
            :url "git@github.com:acme/private-sdk.git"}
   :cache {:mirror-name "private-sdk.git"}
   :checkout-default :worktree}

  "internal-docs"
  {:source {:type :git
            :url "git@github.com:acme/internal-docs.git"}
   :cache {:mirror-name "internal-docs.git"}
   :checkout-default :shared-path}}}
```

### Runtime state

Example per-workspace record:

```clojure
{:workspace/id "ws-123"
 :workspace/repo-id "vis"
 :workspace/root "/Users/me/.local/state/vis/repos/vis/workspaces/ws-123"
 :workspace/state :ready

 :main
 {:ref {:type :branch :name "feature/native-workspaces"}
  :head "9f8e7d6"}

 :children
 {"libs/parser"
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
   :dirty? false}

  "vendor/internal-docs"
  {:kind :attached
   :attached/id "internal-docs"
   :path "vendor/internal-docs"
   :workspace-mode :shared
   :state :ready
   :head "a1b2c3d4"}}}
```

## Invariants

1. Every declared child path is owned by exactly one kind: main, submodule, or attached.
2. Every `:required? true` child must exist before LSP starts.
3. Writable attached repos default to `:isolated`.
4. Shared attached repos are opt-in and assumed cross-workspace mutable unless marked read-only.
5. `.gitignore`d nested repos are never implicitly used; if they matter, they must be declared.
6. The workspace manager is the only module allowed to materialize child repos.

## Consequences

### Positive

- LSP, grep, test runners, and formatters see a normal filesystem.
- The main repo uses Git's native worktree model instead of a custom virtual filesystem.
- Submodules preserve native Git semantics.
- Ignored nested repositories become explicit and reproducible.
- One module owns workspace complexity, improving locality.
- Workspaces become inspectable, debuggable, and recoverable with normal shell tools.

### Negative

- Workspace creation now depends on Git availability and repository hygiene.
- Attached repos require new manifest and lifecycle machinery.
- Shared attached repos can leak mutations across workspaces when chosen incorrectly.
- Cleanup and drift detection become first-class operational concerns.
- Large nested repos may require indexing and watcher policy tuning.

## Rejected alternatives

### Pure in-memory filesystem

Rejected because external tools need real files, real paths, and normal watcher semantics.

### FUSE or mounted virtual filesystem

Rejected because it adds platform-specific complexity, watcher edge cases, and save/rename semantics risk without improving the core workflow over real worktrees.

### Implicit `.gitignore`d nested repos

Rejected because host-local ignored directories make workspaces non-reproducible and hard to diagnose.

## Implementation notes

A future implementation should likely split into these internal modules:

- workspace catalog,
- main repo adapter,
- submodule adapter,
- attached repo adapter,
- workspace reconciler,
- tool surface.

Callers should only need a small interface such as:

```clojure
(create-workspace! opts)
(open-workspace id)
(refresh-workspace! id)
(destroy-workspace! id)
(workspace-status id)
(workspace-roots id)
```

## Open questions

1. Should shared attached repos default to `:indexing :exclude`?
2. Should attached repo refs support only pinned SHA and branch-following, or also tags?
3. Should attached repo materialization prefer nested worktrees over lightweight clones in all cases?
4. How should dirty attached repos affect workspace refresh and destroy semantics?
5. Do we need a lockfile or lease mechanism for concurrent workspace reconciliation?
