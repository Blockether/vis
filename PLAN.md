# Workspace Refactor — Implementation Plan

Source of truth: this file. Where it diverges from `WORKSPACE_SPEC.md`,
this file wins (overrides called out inline).

---

## 1. Locked decisions

| # | Decision | Note |
|---|---|---|
| 1 | **Session ↔ Workspace = 1 : 1, sztywno** | Overrides SPEC §3 invariant 4 (workspace lifetime ≥ session). Each session has exactly one workspace; each workspace has exactly one session. Closing a session is closing its workspace's chat surface; the workspace row remains queryable. |
| 2 | **Trunk is a real row** | Overrides SPEC §3 invariant 2 (trunk = NULL). `workspace.kind ∈ {trunk, branch}`. Trunk row has `root = repo_root`, no worktree directory created, merge/discard disabled. |
| 3 | **Each session-on-trunk = its own trunk-workspace row** | No row reuse. Cheap (metadata only). Keeps 1:1 clean — no special-case. |
| 4 | **Drop EDN entirely** | `~/.vis/workspaces.edn` deleted (best-effort) on first boot. **No import.** Existing SQLite DB also dropped (V1 reset). |
| 5 | **Flat `workspace` table** | Single table, no soul/state split. State transitions in-place on a single mutable row. Overrides vis idiom but matches SPEC §4 + simpler. |
| 6 | **Spawn = semi-automatic via slash command** | New chat created in TUI → trunk-workspace auto-created. `/workspace` (no args) in TUI = create new session + new branch-workspace + switch to it. Branch name and worktree path both auto-minted (decision 15). |
| 7 | **Apply / Discard = slash commands** | `/apply-workspace-to-trunk`, `/discard-workspace-soft`, `/discard-workspace-hard` in TUI. Keybinding accelerators deferred post-v1. |
| 8 | **LLM tools: only `v/workspace.diff`** | Cuts SPEC §11 down to one tool. Spawn / merge / discard / switch / list all belong to the user via TUI / slash. Agent sees its own workspace and a diff view of it. |
| 9 | **SCI ctx cache key = workspace_id** | Equivalent to `session_state_id` because 1:1. Naming aligns to user-facing concept. |
| 10 | **No optional bags in v1 schema** | `inheritable`, `metadata` columns deleted. Only `commit_id` (HEAD sha at worktree create) remains for git audit. Future extensibility goes through new columns when concretely needed. |
| 11 | **TUI: tab terminology banned in code, visual unchanged** | `:workspace-tabs` → `:workspaces` etc.; all event names and handlers sweep. The header strip renders **exactly as today's tabs do** — only labels and routing change. |
| 12 | **TUI shows active only; finished workspaces invisible in strip** | Header strip lists `state ∈ {active, merging}`. Merged + discarded persist in DB for transcript references but never appear in any list, panel, or overlay. No `/finished` command, no Active/Finished split. |
| 13 | **Trunk = current git branch at trunk-workspace creation time** | Captured once on insert. Subsequent branch changes in the repo don't mutate the trunk-workspace row. |
| 14 | **No-git folder: hard refuse** | Vis without a discoverable git repo prints user error and exits. No `kind='bare_cwd'` fallback. Simpler invariants. |
| 15 | **Branch names + worktree paths are auto-generated** | User never types a branch name. Spawn always mints `vis/<short-id>` and `~/.vis/workspaces/<repo-id>/<workspace-id>/`. Removes a footgun + a dialog. |
| 16 | **Slash commands renamed for clarity** | `/workspace` (spawn) · `/apply-workspace-to-trunk` (merge) · `/discard-workspace-soft` (worktree only) · `/discard-workspace-hard` (worktree + branch). No `/finished`. |

---

## 2. Schema V1

Drop old SQLite DB. Edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` in place — no V2 (per `AGENTS.md` and decision 4).

```sql
CREATE TABLE workspace (
  id                   TEXT PRIMARY KEY NOT NULL,
  repo_id              TEXT NOT NULL,
  repo_root            TEXT NOT NULL,

  kind                 TEXT NOT NULL CHECK (kind IN ('trunk','branch')),
  branch               TEXT,
  root                 TEXT NOT NULL,

  parent_workspace_id  TEXT REFERENCES workspace(id) ON DELETE SET NULL,

  state                TEXT NOT NULL
                       CHECK (state IN ('active','merging','merged','discarded')),

  commit_id            TEXT,                         -- repo HEAD sha at worktree creation

  created_at           INTEGER NOT NULL,
  merged_at            INTEGER,
  discarded_at         INTEGER,

  CHECK (kind = 'trunk' OR branch IS NOT NULL),
  CHECK (kind = 'branch' OR state = 'active')        -- trunk can't merge/discard
);

CREATE UNIQUE INDEX uq_workspace_repo_branch
  ON workspace(repo_id, branch) WHERE kind = 'branch';

CREATE INDEX idx_workspace_repo_state
  ON workspace(repo_id, state);

-- session_state gains workspace pin (NOT NULL, 1:1 with session_state)
ALTER TABLE session_state ADD COLUMN workspace_id TEXT NOT NULL
  REFERENCES workspace(id) ON DELETE RESTRICT;

CREATE UNIQUE INDEX uq_session_state_workspace
  ON session_state(workspace_id);
```

Note: because V1 is a fresh recreate, `ALTER TABLE` above is rewritten as
inline column in the modified `session_state` `CREATE TABLE` statement — no
real ALTER runs. The SPEC shows it as ALTER for readability; the migration
file gets the inline form.

---

## 3. `workspace.clj` — public API

Namespace: `com.blockether.vis.internal.workspace`. Re-exports from
`com.blockether.vis.core` as `workspace-*`.

### Lookup

```clojure
(workspace/get          ws-id)                ;; → workspace map | nil
(workspace/list-active  repo-id)              ;; → vec  states #{:active :merging}
(workspace/list-finished repo-id)             ;; → vec  states #{:merged :discarded}
(workspace/for-session  session-state-id)     ;; → workspace map (always present, 1:1)
(workspace/status       ws-id)                ;; → enriched with :git/branch :git/head :git/dirty?
(workspace/trunk-info   repo-id)              ;; → {:branch :head :repo-root}  (live git)
```

### Mutations

```clojure
(workspace/ensure-trunk! {:session-state-id sid})
;; → trunk workspace row, inserted if no row pinned to sid.
;;   repo-root is auto-discovered (decision 14: hard refuse if no git).

(workspace/spawn-branch! {:from              ws-or-nil   ;; current binding; nil → derive from cwd
                          :session-state-id  sid})       ;; the new session's state
;; → new workspace row (kind=branch).
;;   Branch name auto-minted as "vis/<short-id>" (decision 15).
;;   Worktree auto-materialised at "~/.vis/workspaces/<repo-id>/<workspace-id>/".
;;   repo-root derived from :from workspace, or from current git discovery.

(workspace/apply-to-trunk! {:workspace-id   ws-id
                            :strategy       :no-ff
                            :delete-branch? true})
;; trunk-kind → refuse. Otherwise:
;;   :active → :merging → git merge into trunk branch → :merged.
;;   On :delete-branch?: `git branch -D <branch>` from repo-root after success.

(workspace/discard! {:workspace-id   ws-id
                     :delete-branch? true     ;; true ≡ "hard", false ≡ "soft"
                     :force?         false})  ;; allow removal of dirty worktree
;; trunk-kind → refuse. Otherwise:
;;   1. `git worktree remove <root>` (with --force if :force?)
;;   2. If :delete-branch?, `git branch -D <branch>` from repo-root
;;   3. Row transitions to :discarded; root path freed on disk.
```

### Cwd binding (unchanged from current shape)

```clojure
workspace/*workspace-root*       ;; dynamic var, always bound during a turn
(workspace/cwd)                  ;; java.io.File at *workspace-root*
(workspace/workspace-root v)     ;; canonicalise from env-or-map-or-string
```

**Drop the `user.dir` fallback** in `cwd` — assert `*workspace-root*` is
bound. Channel layer guarantees it (see §5).

### Hooks

```clojure
(workspace/register-hook! :on-spawn   (fn [ws] ...))
(workspace/register-hook! :on-merge   (fn [ws result] ...))
(workspace/register-hook! :on-discard (fn [ws] ...))
```

Sync, try/catch, non-blocking. Attach/detach hooks omitted in v1 (no
attach concept under 1:1).

---

## 4. `persistance.clj` + sqlite backend

Adds backend ops. Names follow existing `store-*` / `db-*` style.

```
db-workspace-insert!         row → row
db-workspace-update-state!   id, state, [merged_at | discarded_at]
db-workspace-get             id → row
db-workspace-list-by-repo    repo-id, state-set → [row ...]
db-workspace-for-session     session-state-id → row
db-session-state-set-workspace!  session-state-id, workspace-id
```

`com.blockether.vis.internal.persistance` exposes the facade vars;
`com.blockether.vis.ext.persistance_sqlite.core` implements HoneySQL ops.

---

## 5. `loop.clj` + `extension.clj` wiring

### Session start

In `run-iteration-phase` (or its setup):

1. Resolve `repo-root` (git discover; **error if no git**).
2. If `session_state.workspace_id` is NULL (new session): call
   `workspace/ensure-trunk!` and pin its id to `session_state.workspace_id`.
3. Load the workspace row, derive `:workspace/root`, `:workspace/id`,
   `:workspace/kind`, `:workspace/branch`.
4. Merge into `environment`. Assert `:workspace/root` is non-blank.

Remove the mixed `select-keys [:workspace/root :workspace/id … :workspace]`
shape in `loop.clj:3268`. New shape: pure `:workspace/*` namespaced keys,
never a bare `:workspace`.

### SCI cache

Rekey cache from `session-id` → `workspace-id` (decision 9). Since 1:1
each session_state has exactly one workspace_id; no behavioural change
besides the key.

### Extension wrapper

`extension.clj` already wraps tool calls with
`binding [workspace/*workspace-root* ...]`. Drop the
`(or *workspace-root* user.dir)` fallback in `workspace/cwd` and assert.
Add an upstream assertion at the binding site that env has a non-blank
`:workspace/root`. Tools that misuse `(io/file ".")` get fixed in the
audit pass (§7).

---

## 6. TUI surface

### App-db rename (decision 11)

```
:workspace-tabs       → :workspaces           (vec, ordered)
:active-workspace-id   stays
:workspaces (map)     → :workspace-locals     (per-ws UI cache: input, scroll, pastes…)
```

Event sweep:

| Old | New |
|---|---|
| `:add-workspace-tab`            | `:create-workspace` |
| `:select-workspace-tab-index`   | `:select-workspace-index` |
| `:select-workspace-tab-session-id` | `:select-workspace-by-session` |
| (new) | `:apply-workspace-to-trunk`, `:discard-workspace-soft`, `:discard-workspace-hard`, `:workspace-applied`, `:workspace-discarded` |

Banned identifiers in code (lint at the end): `tab`, `session-tab`,
`workspace-tab`. Header / dialog visuals remain identical to today's
strip — only labels (`Workspaces` not `Tabs`) and routing change.

### Strip visibility (decision 12)

Header strip lists `state ∈ {active, merging}` only. Finished workspaces
(merged + discarded) stay in DB for transcript references but never
appear in any list, panel, or overlay. **No `/finished` command. No
toggle. No history view.**

### Slash commands (decisions 6, 7, 16)

Registered in `channel-tui`:

```
/workspace                       workspace/spawn-branch!  +  new session  +  switch
                                 (no args; branch + worktree both auto)
/apply-workspace-to-trunk        workspace/apply-to-trunk!  on active branch-workspace
/discard-workspace-soft          workspace/discard! :delete-branch? false
/discard-workspace-hard          workspace/discard! :delete-branch? true
```

Trunk-workspace recipients of `/apply-workspace-to-trunk` and
`/discard-workspace-*` → user-error toast.

---

## 7. Audit + cleanup (decision: along the way)

Sweep these consumer call sites for `user.dir` reads or `:workspace/root`
fallbacks left over from the old shape:

```
src/com/blockether/vis/internal/file_picker.clj
src/com/blockether/vis/internal/external_opener.clj
src/com/blockether/vis/internal/theme.clj
src/com/blockether/vis/internal/provider_limits.clj
src/com/blockether/vis/internal/git.clj
```

Acceptance: `rg '(System/getProperty "user.dir")|\(io/file "\."\)' src extensions`
returns zero hits outside `workspace.clj`.

Also: `git/workspace-status` (working-tree porcelain) → rename to
`git/working-tree-status` to end the name collision with
`workspace/status`.

---

## 8. Foundation LLM tool

In `extensions/common/vis-foundation`, add **one** tool:

```clojure
v/workspace.diff
;; → {:branch :head :stat {:files :+ :-} :porcelain [...]}
;; Calls workspace/status + git diff --stat against trunk branch.
```

Spec §11's `spawn / merge / discard / list / switch` are NOT added. User-only.

---

## 9. Execution order

Each step is a separate commit. Tests stay red until step 10.

1. **Schema rewrite** — edit `V1__schema.sql` to add `workspace` table and
   inline `workspace_id` into `session_state`. Add startup hook that
   best-effort deletes `~/.vis/workspaces.edn`. Drop existing SQLite DB
   (one-shot in `bin/dev` boot path).
2. **Persistance backend** — `db-workspace-*` + `db-session-state-set-workspace!`
   in sqlite ext + facade vars in `persistance.clj`.
3. **`workspace.clj` rewrite** — replace EDN code with DB facade calls.
   New API surface (§3). Hooks registry. Drop `user.dir` fallback in
   `cwd`. Re-exports from `core.clj`.
4. **`loop.clj`/`extension.clj` wiring** — ensure-trunk on session start,
   SCI cache rekey to workspace-id, assert workspace-root binding, clean
   up the mixed `select-keys` shape.
5. **Foundation tool** — `v/workspace.diff`.
6. **TUI naming sweep** — `:workspaces`/`:workspace-locals`, event
   renames, kill the word "tab". Header strip filters to
   `state ∈ {active, merging}` — finished workspaces invisible.
7. **Slash commands** — `/workspace`, `/apply-workspace-to-trunk`,
   `/discard-workspace-soft`, `/discard-workspace-hard` in `channel-tui`.
8. **Cwd / collision audit** — sweep §7, rename
   `git/workspace-status` → `git/working-tree-status`.
9. **Test migration** — fix everything that broke; new tests for
   workspace lifecycle, trunk auto-creation, 1:1 invariant,
   slash-command parsing.

Single push at the end of step 9 (or batched per step if midway review
is desired — caller's choice).

---

## 10. Test plan

New / migrated test namespaces:

```
src/com/blockether/vis/internal/workspace.clj
  test/com/blockether/vis/internal/workspace_test.clj
    - ensure-trunk! idempotent per session-state
    - spawn-branch! auto-mints "vis/<short-id>" branch + worktree path
    - apply-to-trunk! refuses trunk
    - apply-to-trunk! :active → :merging → :merged transitions atomic
    - discard! refuses trunk; force? removes dirty worktree
    - discard! :delete-branch? true ≡ hard, false ≡ soft
    - hooks fire post-commit; exceptions swallowed
    - for-session always returns a workspace

extensions/persistance/.../core_test.clj
    - workspace insert / update-state round-trip
    - session_state.workspace_id NOT NULL enforced
    - UNIQUE(repo_id, branch) only for kind='branch'

extensions/channels/vis-channel-tui/.../state_test.clj
    - rename sweep compiles (no :workspace-tabs leftover)
    - :create-workspace dispatch
    - header strip shows only state ∈ {active, merging}

extensions/common/vis-foundation/.../tools_test.clj
    - v/workspace.diff returns stat + porcelain
```

Regression test for audit (§7): grep-style assertion in a meta-test that
no source file outside `workspace.clj` references `user.dir`.

---

## 11. Risks / known unknowns

- **Git merge UX under conflicts.** Step 4 (loop wiring) shouldn't see
  conflicts directly; apply-to-trunk is invoked from slash command in
  step 7. v1: on conflict, leave `:merging`, surface stderr to TUI
  toast, user resolves in editor, retries `/apply-workspace-to-trunk`.
  No in-TUI conflict resolver.
- **Worktree on case-insensitive FS (macOS default).** Branch names
  differing only in case will collide on disk even though git
  distinguishes them. Documented; not solved in v1.
- **Long-running merge blocking TUI.** Run merge in a future; TUI shows
  spinner; subsequent slash commands rejected until merge resolves.
