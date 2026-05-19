# Vis Workspace Specification

Status: proposal
Scope: trunk-native development model — unified branch / worktree / session-binding / TUI viewport
Goal: every coding episode in Vis is a workspace. The session is the durable chat thread; workspaces are the git-backed episodes of work it walks through. Tabs disappear as a concept.

---

## 1. Motivation

Vis today models three loosely-coupled things:

- `session` — a persisted chat thread in SQLite.
- `workspace` (in `internal/workspace.clj`) — a `git worktree` materialised under `~/.vis/workspaces/...`.
- `:workspace-tabs` (in `channel-tui/state.clj`) — a TUI carrier that pins a `{:session, :workspace?}` pair to a tab.

This makes the tab the primary object, the session a child of the tab, and the workspace an optional sibling. It contradicts how the user actually works: trunk development. You sit on `main`, you bounce off into a branch, you talk about the change, you accept or discard, you return to `main`. The branch, the worktree, the chat that happened on it, and the on-screen viewport are facets of one thing — a workspace.

This spec collapses that.

```text
Repo
 └─ Session  (durable chat thread; many per repo)
     ├─ active_workspace_id  → workspace currently bound to this session (nullable = trunk)
     └─ message stream       → each message tagged with workspace_id-at-emission
        ├─ workspace W1 episode  (msgs 1..40)   [merged]
        ├─ trunk episode         (msgs 41..47)
        ├─ workspace W2 episode  (msgs 48..)    [active]
        └─ …
```

---

## 2. Vocabulary

| Term | Meaning |
|---|---|
| **Workspace** | One record. One branch. One worktree directory. One row in `workspace`. The unit of trunk-native work. |
| **Trunk** | `active_workspace_id IS NULL` on the session. The repo's default branch in its primary checkout (`repo-root`). Not a row. |
| **Session** | Durable chat thread for a repo. Has zero or one active workspace at any instant; walks through many over its lifetime. |
| **Episode** | Contiguous run of messages in a session sharing the same `workspace_id`. Derived, not stored. |
| **Spawn** | Create a new workspace from the current binding. Cuts a branch, materialises a worktree, switches the session's `active_workspace_id` to it. |
| **Merge** | Land the workspace's branch into trunk (the repo's default branch). Transitions `:active → :merging → :merged`. |
| **Discard** | Remove the worktree and (optionally) branch without merging. Transitions to `:discarded`. |
| **Inheritable** | EDN bag of session-scoped state copied onto a newly spawned workspace (model, settings, goal, …). |
| **Workspace strip** | Header row in the TUI listing active workspaces for the repo. Replaces the old `workspace-tabs` UI concept. The word *tab* is removed from app-db, handler names, and UI copy. |

Banned words from this point forward in code and prose: **tab**, **session-tab**, **workspace-tab**.

---

## 3. Invariants

1. **One entity, four facets.** A workspace IS a branch IS a worktree IS a session-binding IS a strip slot. They are never independent records.
2. **Trunk is not a row.** `active_workspace_id IS NULL` ⇒ session operates at `repo-root` on the repo's default branch.
3. **One active workspace per session at a time.** Sessions never multiplex; they switch.
4. **Workspace lifetime ≥ session lifetime.** Closing a session does not delete or merge its workspaces. They remain `:active` and may be reattached by any future session in the same repo.
5. **Branch uniqueness per repo.** `UNIQUE(repo_id, branch)`. If a branch already has a workspace row, spawn reuses it.
6. **Merge target is always trunk.** `parent_workspace_id` records lineage; it never changes the merge target. Stacked merges are out of scope.
7. **No mutation of JVM `user.dir`.** Tool cwd resolves via `(workspace/cwd)`, which reads `*workspace-root*`. This binding is set per turn by the channel from `session.active_workspace_id`.
8. **Persistence in SQLite.** `~/.vis/workspaces.edn` is a legacy artefact, imported once on boot, then renamed `.bak`. No EDN runtime state.
9. **Workspace utilities stay in `src/com/blockether/vis/internal/workspace.clj`.** Public surface re-exports through `src/com/blockether/vis/core.clj`. Extensions never `:require` the `internal` namespace.

---

## 4. Schema

Inline edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`. No V2 migrations (see `AGENTS.md`).

```sql
CREATE TABLE workspace (
  id                  TEXT PRIMARY KEY,           -- ULID
  repo_id             TEXT NOT NULL,
  repo_root           TEXT NOT NULL,              -- canonical primary-checkout path
  branch              TEXT NOT NULL,
  root                TEXT NOT NULL,              -- canonical worktree path
  parent_workspace_id TEXT REFERENCES workspace(id),
  state               TEXT NOT NULL,              -- active | merging | merged | discarded
  inheritable         TEXT,                       -- EDN blob; copied at spawn
  head_at_create      TEXT,                       -- repo HEAD when worktree was added
  created_at          TEXT NOT NULL,
  merged_at           TEXT,
  discarded_at        TEXT,
  UNIQUE(repo_id, branch)
);

CREATE INDEX workspace_by_repo_state ON workspace(repo_id, state);

ALTER TABLE session ADD COLUMN active_workspace_id TEXT REFERENCES workspace(id);

ALTER TABLE message ADD COLUMN workspace_id TEXT REFERENCES workspace(id);
CREATE INDEX message_by_session_workspace ON message(session_id, workspace_id);
```

Rationale:

- **Workspace independent of session.** `workspace` has no `session_id`. Sessions reference workspaces, never vice versa. This implements invariant 4.
- **`message.workspace_id` is the audit trail.** Without it you cannot reconstruct which branch a code edit was emitted from. NULL on a message ≡ emitted in trunk.
- **`UNIQUE(repo_id, branch)`.** Implements invariant 5.
- **No trunk row.** Implements invariant 2. The repo's default branch never appears in `workspace`.
- **HoneySQL only** in app code per `AGENTS.md`; raw SQL strings limited to migration DDL.

---

## 5. Lifecycle

```text
                  spawn!
       ┌────────────────────────────┐
       ▼                            │
   ┌────────┐  merge!   ┌────────┐  │
   │ active │──────────▶│ merging│──┘  (atomic: git merge, then commit row)
   └────────┘           └────────┘
       │                    │
       │ discard!           │ merge complete
       ▼                    ▼
   ┌──────────┐         ┌────────┐
   │ discarded│         │ merged │
   └──────────┘         └────────┘
```

- `:active` — created, worktree on disk, branch in repo, may be bound to a session.
- `:merging` — `git merge` in flight on trunk. Short-lived; intolerant to crashes is acceptable because the merge is idempotent on retry.
- `:merged` — branch landed in trunk. Worktree may be cleaned up; row retained for transcript references.
- `:discarded` — worktree removed, branch optionally deleted. Row retained for transcript references.

`:merged` and `:discarded` workspaces never reappear in the strip but remain queryable by id for transcript rendering.

---

## 6. Public API

Namespace: `com.blockether.vis.internal.workspace`. Re-exports through `com.blockether.vis.core` so extensions never reach into `internal`.

### 6.1 Lookup / status

```clojure
(workspace/list-active repo-id)            ;; → [workspace ...]  states #{:active :merging}
(workspace/list-all    repo-id)            ;; → [workspace ...]
(workspace/get          ws-id)              ;; → workspace | nil
(workspace/active-for-session session-id)  ;; → workspace | nil  (nil ⇒ trunk)
(workspace/status      ws-id)              ;; → enriched with :git/branch :git/head :git/dirty?
(workspace/trunk-info  repo-id)            ;; → {:branch :head :repo-root}
```

### 6.2 Mutations

```clojure
(workspace/spawn!
  {:from           ws-or-nil      ;; nil = spawn from trunk
   :repo-root      path-or-nil    ;; defaults to discovered repo of `from` or cwd
   :branch         "vis/feat-x"   ;; optional; generated as "vis/<id>" if absent
   :inherit        #{:model :settings :goal}  ;; keys to copy from caller's inheritable
   :session-id     id-or-nil})    ;; if provided, sets that session's active_workspace_id
;; → new workspace record

(workspace/merge!
  {:workspace-id   ws-id
   :strategy       :ff-only | :no-ff | :rebase  ;; default :no-ff
   :delete-branch? true})
;; → {:workspace ... :merge {:exit :out :err}}

(workspace/discard!
  {:workspace-id   ws-id
   :delete-branch? true
   :force?         true})        ;; force-removes the worktree even if dirty
;; → workspace record (state :discarded)

(workspace/attach! session-id ws-id)   ;; set session.active_workspace_id
(workspace/detach! session-id)         ;; clear → trunk
```

### 6.3 Cwd binding

```clojure
workspace/*workspace-root*       ;; dynamic var, canonical path or nil
(workspace/cwd)                  ;; java.io.File at *workspace-root* or repo-root or user.dir
(workspace/workspace-root v)     ;; canonicalise root from env-or-map-or-string
```

Channels rebind `*workspace-root*` once per turn from the active workspace before any tool fires. This is the only legal way to change cwd for the engine.

### 6.4 Hooks

```clojure
(workspace/register-hook! :on-spawn   (fn [ws] ...))
(workspace/register-hook! :on-merge   (fn [ws result] ...))
(workspace/register-hook! :on-discard (fn [ws] ...))
(workspace/register-hook! :on-attach  (fn [session-id ws] ...))
(workspace/register-hook! :on-detach  (fn [session-id ws] ...))
```

- Synchronous, fired after the state commit.
- Each hook is sandboxed with `try/catch`; exceptions are logged, never propagate.
- Hooks may dispatch back to the channel via `vis/notify!` or extension-specific surfaces. They must not block.
- Extensions register hooks in their `:ext/start` lifecycle and remove them in `:ext/stop`.

### 6.5 Re-exports in `core.clj`

```clojure
(def workspace-spawn!          workspace/spawn!)
(def workspace-merge!          workspace/merge!)
(def workspace-discard!        workspace/discard!)
(def workspace-attach!         workspace/attach!)
(def workspace-detach!         workspace/detach!)
(def workspace-list-active     workspace/list-active)
(def workspace-list-all        workspace/list-all)
(def workspace-get             workspace/get)
(def workspace-active-for-session workspace/active-for-session)
(def workspace-status          workspace/status)
(def workspace-trunk-info      workspace/trunk-info)
(def workspace-cwd             workspace/cwd)
(def workspace-register-hook!  workspace/register-hook!)
(def ^:dynamic *workspace-root* nil)  ;; alias via root-binder util, not a re-`def`
```

---

## 7. TUI surface

### 7.1 App-db (`extensions/channels/vis-channel-tui/src/.../state.clj`)

```clojure
;; OLD
{:workspace-tabs      [{:id :tab-1 :label "..." :workspace {...} :workspace/root "..." :active? true} ...]
 :active-workspace-id :tab-1
 :workspaces          {:tab-1 {:session ... :messages [...] :input ...} ...}}

;; NEW
{:workspaces          {<ws-id> {:input ...
                                :input-history [...]
                                :input-history-index ...
                                :input-history-draft ...
                                :scroll ...
                                :pastes {...}
                                :paste-counter N
                                :detail-expansions {...}
                                :mouse-selection ...}
                       ...}    ;; UI-local cache only — no :session, no :messages
 :active-workspace-id <ws-id-or-nil>}
```

Derived (recomputed on render; not stored):

```clojure
(workspace-strip db repo-id)
;; → [{:workspace/id ... :label "branch" :active? bool :state :active|:merging :diff-stat {:+ ... :- ... :files ...}}
;;    ...
;;    {:workspace/id nil :label "main" :active? bool :trunk? true}]
```

Trunk is rendered as the leftmost strip entry with id `nil`. The `[+]` spawn affordance is always the rightmost entry.

### 7.2 Event renames

| Old event id                            | New event id                  |
|-----------------------------------------|-------------------------------|
| `:add-workspace-tab`                    | `:spawn-workspace`            |
| `:select-workspace-tab-index`           | `:select-workspace-index`     |
| `:select-workspace-tab-session-id`      | `:select-workspace-by-session`|

New events:

```clojure
[:spawn-workspace      {:from <ws-id-or-nil> :branch nil-or-str :inherit set}]
[:merge-active]                                    ;; merge active workspace into trunk
[:discard-active   {:delete-branch? true}]
[:attach-workspace <ws-id>]                        ;; switch active workspace
[:detach-workspace]                                ;; return session to trunk
[:workspace-merged    <ws-id> <summary-ir>]        ;; fired by hook → injects system msg
[:workspace-discarded <ws-id>]
```

### 7.3 Keybindings (additions)

| Key | Action |
|---|---|
| `Ctrl+B` | Open "Spawn workspace" dialog (branch hint, inherit flags) |
| `Ctrl+,`/`Ctrl+.` | Cycle workspaces in strip (prev / next) |
| `Ctrl+\` | Switch to trunk |
| `Ctrl+Enter` (in diff view) | Merge active workspace |
| `Ctrl+Bksp` (in diff view) | Discard active workspace |

`Ctrl+Y` remains unbound per `AGENTS.md`.

### 7.4 Header workspace strip

A header contributor renders the strip:

```text
┌────────────────────────────────────────────────────────────┐
│  main │ vis/feat-x ●  │ vis/spike-y    │ + │   header rhs  │
└────────────────────────────────────────────────────────────┘
   trunk    active        active           spawn
```

The `●` marks the workspace bound to the current session. Only one entry carries it.

### 7.5 Diff header contributor

When `active_workspace_id` is non-nil, a second contributor renders below the strip:

```text
vis/feat-x  ▲ 3 ▼ 0   files 4   +120 −33   comments 2   [diff] [accept] [discard]
```

- Stats from `git -C <root> diff --stat <trunk-branch>..HEAD` plus `git status --porcelain`.
- Refresh: filesystem watcher on `root` if available; otherwise poll on focus + every 5s while loading is false.
- `[diff]` opens a dialog with per-file diff; per-line comments persist on the workspace row's `inheritable` blob under `:comments {file → [{line text inst}]}`.
- `[accept]` ≡ `:merge-active`; `[discard]` ≡ `:discard-active`.

### 7.6 Spawn dialog

Triggered from `Ctrl+B`, the `[+]` strip button, or an LLM tool call.

Fields:

- Branch name. Default `vis/<id>`. Validate against `git check-ref-format`.
- Spawn-from. Default current binding (trunk if none).
- Inherit checkboxes — `model`, `settings`, `goal`, etc. Driven by which extensions register `:workspace/inheritable-keys`.
- Confirm → `(vis/workspace-spawn! ...)` → dispatch `:attach-workspace` with the returned id.

### 7.7 Acceptance UX

On `:workspace-merged` the `:on-merge` hook in the TUI channel:

1. Detaches the session: `[:detach-workspace]`.
2. Appends a system bubble to the session transcript summarising the merge (file count, +/−, branch, head). The summary IR is provided by the hook payload.
3. Offers three follow-ups via inline buttons:
   - **Clear** — keep session on trunk; no further action.
   - **New workspace (preserve vars)** — open spawn dialog with current `inheritable` defaulted in.
   - **Switch to <other-active-ws>** — picker over remaining `:active` workspaces in the repo.

---

## 8. Cwd binding contract

For every iteration of every turn:

1. Channel resolves `(vis/workspace-active-for-session <session-id>)`.
2. Channel binds `(binding [workspace/*workspace-root* (workspace/workspace-root ws)] ...)` around the iteration.
3. Tool implementations resolve paths against `(workspace/cwd)` only. No direct reads of `(System/getProperty "user.dir")`.
4. After the iteration loop, the binding unwinds.

Audit obligation: `rg '(System/getProperty "user.dir")|\(io/file "\."\)' src extensions` returns zero call sites outside `workspace.clj`.

---

## 9. Spawn / merge / discard semantics

### 9.1 spawn!

1. Discover `repo-root` (from `:from` workspace or current git repo).
2. Compute `repo-id`, mint ULID, branch name (`vis/<id>` if blank).
3. If `(repo-id, branch)` exists with state `:active`, return that record (idempotent reuse).
4. Otherwise:
   - If branch exists locally: `git worktree add <root> <branch>`.
   - Else: `git worktree add -b <branch> <root> HEAD`.
5. Insert row with `:state :active`, `:head_at_create`.
6. If `:session-id` provided, `attach! session-id <new-id>`.
7. Fire `:on-spawn` hook.
8. Return record.

### 9.2 merge!

1. Load workspace; require `:state :active`. Otherwise error.
2. Transition row to `:merging`.
3. In the **trunk worktree** (`repo-root`):
   - `git fetch` if remote configured.
   - `git checkout <trunk-branch>` (best-effort; preserves user-stash if dirty).
   - `git merge <opts> <workspace.branch>`.
4. On success: transition `:merging → :merged`, set `merged_at`. Optionally `git worktree remove`. If `:delete-branch?`, delete branch.
5. On failure: leave row in `:merging` for retry; surface conflict info to channel.
6. Fire `:on-merge` hook with `{:workspace ws :merge {:exit :out :err}}`.

### 9.3 discard!

1. Require `:state :active` (or `:merging` if `:force?`).
2. `git worktree remove <root>` (or `--force` if `:force?`).
3. If `:delete-branch?`, `git branch -D <branch>` from `repo-root`.
4. Transition row to `:discarded`, set `discarded_at`.
5. Fire `:on-discard` hook.

---

## 10. Migration

On first boot after this lands:

1. Run `V1__schema.sql` (or reset, per dev policy in `AGENTS.md`).
2. For each row in `~/.vis/workspaces.edn`:
   - Compute `repo_id` from `:workspace/repo-root`.
   - Insert `workspace` row with `:state :active`, copying `:branch`, `:root`, `:repo-root`, `:head_at_create` from `:main.head`.
   - Skip if `(repo_id, branch)` already present.
3. Rename `~/.vis/workspaces.edn` to `~/.vis/workspaces.edn.bak`.
4. Existing `session` rows: `active_workspace_id` defaults NULL (trunk). User reattaches manually if desired.
5. Existing `message` rows: `workspace_id` defaults NULL (treated as trunk-episode for transcript rendering).

The migration is run once by the persistence extension's startup routine. Idempotent: if `.edn.bak` exists, it is skipped.

---

## 11. LLM tools (foundation)

Added in `extensions/common/vis-foundation`:

| Tool | Purpose |
|---|---|
| `v/workspace.spawn`   | Spawn a workspace from current binding. Branch name optional. |
| `v/workspace.merge`   | Merge current active workspace into trunk. |
| `v/workspace.discard` | Drop current active workspace. |
| `v/workspace.list`    | Enumerate active workspaces in the repo. |
| `v/workspace.diff`    | Diff stat + porcelain for the active workspace. |
| `v/workspace.switch`  | Attach the session to an existing workspace by id or branch. |

All return plain Clojure maps per the foundation tool contract (`AGENTS.md` § "Tool results are PLAIN CLOJURE DATA"). They internally call the core re-exports — never `internal/workspace.clj` directly.

---

## 12. Out of scope (for v1)

- Stacked workspaces (parent-merges-into-child workflows).
- Remote PR/MR integration. `merge!` is local-only; opening a PR is a downstream extension.
- Conflict-resolution UI. v1 surfaces the conflict text and stops; the user resolves in their editor.
- Per-line comments persistence beyond `inheritable[:comments]`. A first-class comments table is deferred.
- Cross-repo workspaces. One workspace belongs to exactly one repo.

---

## 13. Execution order

1. **Schema + import.** Edit `V1__schema.sql` inline. Write the EDN import path. No behaviour change yet.
2. **`internal/workspace.clj` → SQLite-backed.** Replace EDN read/write with persistence-extension calls. Keep public API stable on the surface; callers (only `channel-tui/state.clj`, `internal/loop.clj`) keep compiling.
3. **`core.clj` re-exports.** Add `workspace-*` aliases. Extensions migrate to `vis/workspace-*`.
4. **Naming sweep.** Rename `:workspace-tabs` → derived strip; rename event ids; remove `tab` token from app-db and handler names; update tests.
5. **Hooks API.** `register-hook!` + dispatch points in `spawn!` / `merge!` / `discard!` / `attach!` / `detach!`.
6. **TUI spawn dialog + strip.** `Ctrl+B`, `[+]` slot, key cycling.
7. **Diff header contributor.** Stat refresh, `[diff]/[accept]/[discard]` buttons.
8. **Merge flow + post-merge UX.** Hook-driven transcript injection, follow-up actions.
9. **LLM tools.** `v/workspace.*` in foundation.
10. **Cwd audit.** Eliminate `user.dir` reads outside `workspace.clj`. Add regression test.

Each step ships with tests per `AGENTS.md` (every src ns has a matching test ns; new public behaviour gets real coverage).

---

## 14. Decisions locked from design discussion

| # | Decision |
|---|---|
| a | Trunk has no row; `active_workspace_id IS NULL` ≡ trunk. |
| b | Detach on session close leaves workspaces `:active`; reattachable by any future session. |
| c | Existing messages backfill `workspace_id = NULL` (trunk episode). |
| d | Header strip name in code: `workspace-strip`. |
| e | Strip lists all `:active`/`:merging` workspaces in the repo, regardless of which session created them. |
| f | Files stay in `src/com/blockether/vis/internal/workspace.clj`; public surface re-exported through `core.clj`. |
| g | Session : workspace = 1 : N over time; 1 : 1 at any instant. |
| h | Merge target is always trunk. `parent_workspace_id` is lineage metadata only. |
| i | Branch ≡ worktree ≡ session-binding ≡ strip slot ≡ workspace. The word *tab* is removed from the codebase. |
