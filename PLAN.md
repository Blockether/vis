# PLAN.md — Workspaces, slash commands, cross-channel intents

Status: design, not yet implemented. Single source of truth for the
workspace + slash redesign discussed across iterations of the
2026-05 thread. Supersedes ad-hoc PLAN.md references scattered
through the codebase (`extensions/.../core.clj`,
`extensions/.../environment/*.clj`, `extensions/persistance/.../core.clj`,
TUI `header.clj` / `screen.clj` / `chat.clj`); those references
remain valid — this doc is what they point at.

Shape disagreements between this doc and live code are bugs;
this doc wins. Shape disagreements between this doc and
`extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`
are bugs; the schema wins. CTX_REDESIGN.md is partially superseded —
this doc owns the `:session/workspace` subtree shape (canonical key
set, sourcing rules) since CTX_REDESIGN.md still documents the
legacy `:git/*` shape. Engine source of truth: `ctx_spec.clj`.

No V2 / V3 migration files. AGENTS.md is law: inline V1 edits.

## Live-state verification (2026-05-22, vis @ ec2d2987)

All claims below were cross-validated against a freshly restarted
nREPL (`bin/dev`), live schema, and source-code reads. Findings
below drive the kill-list in §3a.

| claim | verified by | result |
|---|---|---|
| `(ctx-engine/empty-ctx)` workspace shape | nREPL eval | `{:session/workspace {:vcs/kind :none}}` — VCS-agnostic, NO branch/head pre-stamped |
| `ctx_spec.clj` workspace shape | source read | canonical `:vcs/{kind,branch,trunk,head,dirty?,stats}`; `:git/*` aliases marked deprecated |
| `s/valid? :session/workspace` open-schema behaviour | nREPL eval (6 inputs) | accepts `{}`, `#:git{}`, `#:vcs{}`, mixed, **even `{:foobar 1}`** — only `:vcs/kind` enum is strict (rejects `:svn`) |
| `prompt.clj` advertises to model | source read | `:vcs/*` (matches spec) |
| `workspace/status` returns | source read | `:git/branch :git/head :git/dirty?` (LEGACY) |
| consumers of `workspace/status` outside `core.clj` re-export | ripgrep | **zero** — dead API for production paths, safe to rewrite/kill |
| `workspace/trunk-info` on vis repo | nREPL eval | `:branch "main"` (lucky; HEAD is on main) |
| `workspace/trunk-info` on `/tmp/vis-trunk-test` (HEAD=feature, base=master, no origin) | nREPL eval | returns `:branch "feature"` — **BUG B1**, should return `"master"` |
| `detect-trunk-branch` proposed logic (origin/HEAD → main → master → current) | nREPL prototype | `main` on vis, `master` on test repo with master base — confirmed |
| `git symbolic-ref --short refs/remotes/origin/HEAD` on vis | shell | returns `origin/main` (must strip `origin/` prefix) |
| `:session/workspace` stamping path in production | ripgrep `assoc.*:session/workspace` | **nothing stamps it**; only `empty-ctx` initial value survives the whole session. `ctx_engine.clj:934` TODO comment confirms |
| `apply-to-trunk!` (file-copy) call sites | ripgrep | `core.clj:90` re-export + TUI `screen.clj:2429` palette only — safe to kill after TUI rewrite |
| `spawn-branch!` dirty/ignored copy | source read | does NOT mirror dirty/ignored; only `git worktree add ... HEAD` — §4.4 rewrites |
| slash registry on engine side | ripgrep | does NOT exist |
| **TUI hardcoded slash tree** | source read (`dialogs.clj`, `screen.clj`) | `palette-commands` vec + `slash-only-commands` vec + `:tui.slot/commands` channel-contribution slot |
| **Telegram hardcoded slash tree** | source read (`bot.clj:992-1024`) | `parse-command` + `handle-command!` with hardcoded case for `/start /help /status /model /models /reasoning /verbosity /voice /cancel /restart /export` — EXACTLY the parallel-words anti-pattern the new registry kills |
| Voice extension's TUI command surface | source read (`foundation_voice/core.clj:716`, `input.clj:149`) | registers `{:tui.slot/commands [...]}` with `:id :voice/toggle-recording :palette? false :run-fn ...` — TUI-only; Telegram's `/voice` is hardcoded SEPARATELY in `bot.clj:1015` |
| `register-cmd!` (CLI subcommand tree) | source read | exists in `registry.clj`; orthogonal — for `vis x y z` process subcommands, NOT for in-session `/cmd` intents. Stays. |
| `:tui.slot/header-row` consumers | ripgrep | `header.clj:219` only — real UI slot, NOT slash. Stays as channel-specific surface. |

---

## 0. Vocabulary

| term | meaning | user-facing? |
|---|---|---|
| **workspace** | Vis row pinned 1:1 to `session_state`. Owns kind + branch + root + state. | yes — everywhere |
| **kind** | `:trunk` (root = repo_root, no worktree on disk) or `:branch` (managed worktree) | yes — strip, list, CTX |
| **worktree** | git filesystem checkout. Implementation detail of `:branch`-kind workspaces. | NO. Never surfaced. |
| **branch** | git branch ref (`vis/<short-id>` or trunk branch). Visible as secondary label. | yes |
| **trunk** | repo's default branch resolved via `origin/HEAD`, fallback `main` → `master` → current. | yes |
| **label** | optional explicit name for a workspace. Default tracks `session_state.title`. | yes |
| **session** | `session_soul` + chain of `session_state`s. One soul, N states (forks). | yes |
| **slash** | `/word ...` user input. Engine-level intent, channel-agnostic. | yes |

Display rule, one place, every channel:

```clojure
(defn display-label [{:keys [title label]}]
  (let [t (or title "Untitled")]
    (if (and (some? label) (not= label t))
      (str t " — " label)
      t)))
```

Sample: `Auth bcrypt refactor` (no override); `Refactor logging — frontend` (override).
`Untitled` for fresh session.

---

## 1. Schema (inline V1 — extends existing tables)

Current `V1__schema.sql` already defines `workspace`, `session_state`,
`session_soul`. We extend by:

```sql
-- workspace gains two optional columns
ALTER TABLE workspace ADD COLUMN label TEXT;
ALTER TABLE workspace ADD COLUMN last_focused_at_ms INTEGER;

-- new table: per-repo "last active workspace" pointer
CREATE TABLE repo_focus (
  repo_id        TEXT PRIMARY KEY NOT NULL,
  workspace_id   TEXT NOT NULL
                 REFERENCES workspace(id) ON DELETE CASCADE,
  updated_at_ms  INTEGER NOT NULL
);
```

But because AGENTS.md forbids new migration files AND existing V1
already has the `workspace` table, the inline edit is:

1. Edit `workspace` table block in `V1__schema.sql` to add
   `label TEXT` and `last_focused_at_ms INTEGER` between
   `commit_id TEXT` and `created_at INTEGER NOT NULL`.
2. Insert `CREATE TABLE repo_focus (...)` block after
   `idx_workspace_repo_state`.
3. Bump persistance row projection (`row->workspace` in
   `extensions/persistance/vis-persistance-sqlite/src/.../core.clj`)
   to surface `:label` and `:last-focused-at-ms`.

No constraints are dropped. Existing rows materialise with
`label = NULL` and `last_focused_at_ms = NULL`; readers treat both
as fallback paths (label → session.title; last_focused → created_at).

### Schema invariants reaffirmed

(All present in V1; listed for cross-validation.)

| invariant | enforced by |
|---|---|
| `workspace.kind ∈ {trunk, branch}` | CHECK |
| `kind=trunk OR branch IS NOT NULL` | CHECK |
| `kind=branch OR state=active` | CHECK |
| `state ∈ {active, merging, merged, discarded}` | CHECK |
| Unique `(repo_id, branch)` for branch-kind only | partial UNIQUE INDEX |
| Each `session_state` pins exactly one workspace | `uq_session_state_workspace` UNIQUE |

### What we do NOT add

- `commits_ahead_cached` — derived on demand from git, not persisted.
- `merge_resolve_session_id` — merge-resolve sub-session uses
  the existing `session_state.parent_state_id` + a flag column;
  decision deferred to §7.
- `tui_workspace_tab` — wycofane. Tabs are a TUI projection of
  `workspace/list-active(repo-id)`; no TUI-specific table.

---

## 2. Trunk detection

Replace `(current-branch repo-root)` everywhere "trunk branch" is
meant. Add to `workspace.clj`:

```clojure
(defn- detect-trunk-branch
  "Discover the repo's default branch. Order:
     1. origin/HEAD symbolic-ref  (most reliable)
     2. main  (local existence)
     3. master (local existence)
     4. current branch  (degenerate; lone-branch repo)
   Returns a plain branch name (no refs/ prefix)."
  [repo-root]
  (or (try
        (-> (git! repo-root ["symbolic-ref" "--short" "refs/remotes/origin/HEAD"])
            (str/replace #"^origin/" ""))
        (catch Throwable _ nil))
      (when (local-branch-exists? repo-root "main") "main")
      (when (local-branch-exists? repo-root "master") "master")
      (current-branch repo-root)))
```

`workspace/trunk-info` returns `{:repo-root :branch :head}` using
`detect-trunk-branch` for `:branch`. All `apply!` / `ff-apply!`
operations use this as the FF target.

Call-site audit:
- `ensure-trunk!` — currently uses `(current-branch repo-root)` for
  `:branch`. Switch to `detect-trunk-branch` so a fresh session
  spawned while user happened to be on `feat/foo` still pins
  trunk-kind to `main`, not `feat/foo`. **Behaviour change.**
  Mitigation: existing trunk rows keep their stored `branch`
  field; only new rows pick up the corrected logic.
- `apply-to-trunk!` — has no concept of "trunk branch" yet; it
  copies files. The new `ff-apply!` (§5) MUST use
  `detect-trunk-branch` for `git merge --ff-only` target.

---

## 3a. Kill-list (legacy code removed in this redesign)

This redesign is a HARD CUTOVER. No transition window, no deprecation
period. Everything in this list is deleted in the same PR that lands
the new surface; tests on the new path go in first.

| # | what dies | where | replacement |
|---|---|---|---|
| K1 | `:git/branch :git/trunk :git/head :git/dirty? :git/stats :git/file-stats` spec defs | `ctx_spec.clj:331-346` | canonical `:vcs/*` only. `:vcs/stats` keeps the file-stats map shape under a `:vcs/file-stats` name (key rename in same PR). |
| K2 | `:git/*` opt keys in `::workspace` spec | `ctx_spec.clj:351` | drop from opt-key vec; only `:vcs/*` remain |
| K3 | `workspace/status` `:git/*` output | `workspace.clj:286-288` | rewrite to emit `:vcs/branch :vcs/head :vcs/dirty?`. Or kill entirely once stamping path (§6.3) covers the same data — verified zero consumers outside `core.clj:86` re-export. |
| K4 | `workspace/apply-to-trunk!` (file-copy) | `workspace.clj:367+` | replaced by `workspace/ff-apply!` (§4.5) |
| K5 | `workspace-apply-to-trunk!` re-export | `core.clj:90` | `workspace-ff-apply!` re-export |
| K6 | TUI `palette-commands` entries: `:workspace :apply-workspace-to-trunk :discard-workspace-{soft,hard}` | `dialogs.clj:2131-2134` + `screen.clj` dispatch | engine slash registry; palette pulls live from `(vis/registered-slashes)` (§9.3) |
| K7 | TUI `slash-only-commands` hardcoded vec | `screen.clj:311-315` | engine slash registry feeds `menu-commands` directly |
| K8 | TUI `:tui.slot/commands` channel-contribution slot | `screen.clj:296` consumer + `extension.clj:704` doc + voice registration in `foundation_voice/core.clj:716` | engine slash registry. After voice migrates, NO consumer remains — retire the slot. (`:tui.slot/header-row` stays; that's a genuinely UI-only surface.) |
| K9 | Telegram `parse-command` + `handle-command!` hardcoded slash tree | `bot.clj:992-1024` | engine `slash/dispatch` called from `handle-user-text!` before `vis/send!` (§10). Each `/start /help /status /model /models /reasoning /verbosity /voice /cancel /restart /export` migrates to one `register-slash!` call. |
| K10 | Voice extension's TUI-only `voice-input-tui-commands` + `:tui.slot/commands` | `foundation_voice/core.clj:716-718`, `input.clj:149-155` | `(vis/register-slash! {:slash/name "voice" :slash/requires #{:channel} :slash/availability-fn (fn [{:keys [channel/id]}] (#{:tui :telegram} id))})` — ONE registration, both channels. Same for any future channel (web). |
| K11 | CTX_REDESIGN.md workspace section using `:git/*` | doc | rewrite to canonical `:vcs/*` shape; point at PLAN.md §8 for sourcing rules |
| K12 | `HANDOFF.md` and `TODO.md` stale workspace plan items | docs | audit + cross-link to PLAN.md after step 12 |

### K-list philosophy

- No `:git/*` keys in CTX. Period. A git-detected workspace still
  emits `:vcs/kind :git`; consumers branch on that. `:git/*` was a
  transition fiction; the spec already calls it deprecated; the
  cutover removes it.
- No channel-private slash trees. ONE engine registry, every channel
  reads from it. Voice extension stops registering twice. Telegram
  stops parsing slashes itself. TUI stops hardcoding the palette.
- `workspace/apply-to-trunk!` was always wrong (file-copy ignores
  commit history). New `ff-apply!` is git-native FF. We do not keep
  the old call site behind a flag; we delete it.
- `:tui.slot/commands` survived only because slash was TUI-centric.
  After cutover, slot has zero consumers → retired. Other slots
  (`:tui.slot/header-row`) stay because they describe real TUI UI,
  not slash semantics.

## 3. Slash registry — fourth global registry (cross-channel, not TUI-only)

**Cross-channel surface.** Slash commands are an engine-level concept,
rendered by every channel that has a user-input surface — TUI today,
Telegram today, Web soon (no `web` channel yet — confirmed by
`extensions/channels/` ls), Discord / Slack / API later. There is
exactly one registry; channels read from it. There are NO
channel-private slash trees, NO duplicate registrations.

**Cross-channel implication.** Telegram's `setMyCommands` API is fed
from the SAME registry (see §10 step 2). The Web channel, when it
lands, will pull its `/`-menu from the SAME registry. A user typing
`/workspace apply` in any channel hits the exact same handler with
the same ctx contract.

**Telegram already-existing CLI subcommands.** `vis channels telegram
approve` (and the like) are CLI subcommands registered through the
ORTHOGONAL `register-cmd!` API. Those are launch-time admin ops, NOT
in-session slash intents — they keep their own surface. See §13 row
`telegram bot.clj` for the audit.

Lives in `src/com/blockether/vis/internal/registry.clj`, peer with
`channel-registry`, `provider-registry`, `command-registry`.

```clojure
;; spec
(s/def :slash/name       non-blank-string?)
(s/def :slash/parent     (s/coll-of non-blank-string? :kind vector?))
(s/def :slash/doc        non-blank-string?)
(s/def :slash/usage      non-blank-string?)
(s/def :slash/run-fn     ifn?)
(s/def :slash/requires   (s/coll-of #{:session :workspace :channel} :kind set?))
(s/def :slash/availability-fn ifn?)
(s/def :slash/subcommands (s/coll-of non-blank-string? :kind vector?))
(s/def ::slash
  (s/keys :req [:slash/name]
    :opt [:slash/parent :slash/doc :slash/usage :slash/run-fn
          :slash/requires :slash/availability-fn :slash/subcommands]))

;; atom: vec preserves registration order, dedup on [parent name]
(defonce slash-registry (atom []))

(defn register-slash!   [spec])
(defn deregister-slash! [parent name])
(defn registered-slashes [])
(defn slash-by-path     [path])           ; e.g. ["workspace" "apply"]
(defn slash-children    [parent-path])
```

Engine is 100% generic. NO hardcoded knowledge of `/workspace`.
Workspace ops live in a foundation extension (§6) that registers
through this API exactly like any third-party extension would.

Public re-exports in `core.clj`:
```clojure
(def register-slash!     registry/register-slash!)
(def deregister-slash!   registry/deregister-slash!)
(def registered-slashes  registry/registered-slashes)
(def slash-by-path       registry/slash-by-path)
```

### Dispatch — `src/com/blockether/vis/internal/slash.clj` (NEW)

```clojure
(ns com.blockether.vis.internal.slash
  "Channel-agnostic slash dispatch. Each channel's input layer calls
   `dispatch` BEFORE forwarding the message to the LLM. When the
   return map has :handled? true, the channel must NOT round-trip
   the LLM for this turn.")

(defn parse [text]
  ;; Greedy longest-prefix match against registry tree. Returns
  ;; {:path ["workspace" "apply"] :args ["--hard"]} or nil.
  )

(defn dispatch
  "ctx carries channel/id, session/id?, workspace/id?, db-info,
   reply!, publish-event!. Returns:
     {:handled? true  :result …}
     {:handled? true  :error …  :reason :unknown}
     {:handled? true  :error …  :reason :requires-failed :missing #{:session}}
     {:handled? true  :error …  :reason :unavailable}
     {:handled? false}"
  [ctx text])
```

Engine wiring (`internal/loop.clj`): at turn start, before building
the user message + CTX render, call `(slash/dispatch ctx user-msg)`.
On `:handled? true`, persist a synthetic iteration row carrying the
slash result (so transcript shows the action), skip the LLM round-trip,
finalize the turn.

### Slash result envelope (cross-channel)

```clojure
{:slash/status :ok | :error
 :slash/title  "Workspace applied"
 :slash/body   "vis/abc12345 -> main (FF, 3 commits)"
 :slash/actions [{:label "Close workspace" :slash "/workspace discard"}
                 {:label "Keep working"    :slash :dismiss}]}
```

Channel renderers:
- **TUI**: modal/inline notification with action buttons mapped to
  slash dispatches.
- **Telegram**: reply message with inline keyboard; button taps
  re-enter `slash/dispatch` with the action's `:slash` field.
- **CLI/REPL/log**: plain text, actions printed as suggested
  follow-up commands.

Trailer pin (CTX): each handled slash adds one entry to the current
iter's pin with `:tag :user-slash`. See §6.4.

---

## 4. Workspace core API (refactor of `src/com/blockether/vis/internal/workspace.clj`)

### 4.1 Existing fns kept

- `*workspace-root*` dynamic var
- `workspace-root`, `cwd`, `normalize-root`
- `delete-legacy-edn!`
- `register-hook!`
- `get`, `list-active`, `list-finished`, `for-session`, `status`
- `ensure-trunk!` (updated to use `detect-trunk-branch`)
- `discard!`

### 4.2 Existing fns deprecated / removed

- `apply-to-trunk!` — file-copy based, replaced by `ff-apply!`.
  Kept as fallback handle? **Decision: remove**. The new flow
  is `commit!` (explicit) + `ff-apply!`. Dirty-on-worktree never
  reaches trunk without a commit.

### 4.3 New fns

```clojure
(defn detect-trunk-branch [repo-root])           ; §2

(defmacro with-repo-lock [repo-id & body])        ; §4.7

(defn spawn-branch!
  "Spawn :branch workspace. Branch (vis/<short>) and worktree path
   auto-minted. Copies trunk worktree state INCLUDING dirty tracked,
   untracked, and gitignored files. Real walk of repo_root excluding
   .git/.

   Opts: :from (workspace map; nil = derive)
         :session-state-id (optional pin)
         :label (optional)
   Fires :on-spawn hook."
  [db-info opts])

(defn commit!
  "git add -A && git commit -m <message> inside the worktree.
   Refuses :trunk-kind. Refuses empty diff (returns :nothing-to-commit).
   Updates workspace.commit_id to new HEAD.
   Returns {:status :ok | :nothing-to-commit
            :sha :message :workspace}."
  [db-info {:keys [workspace-id message]}])

(defn ff-apply!
  "Fast-forward merge workspace branch onto trunk:
     1. with-repo-lock
     2. auto-stash trunk if dirty (workspace files already mirror
        the trunk state we want — stash is just for FF cleanliness)
     3. git -C repo-root merge --ff-only vis/<id>
     4. auto-pop stash
     5. transition workspace state -> :merged
   Returns {:status :ok :sha :branch} on success.
   Returns {:status :ff-failed :reason …} on conflict — engine
   passes this to start-merge-resolve! (§7).
   Refuses :trunk-kind."
  [db-info {:keys [workspace-id]}])

(defn set-label!
  "Set workspace.label. Empty string / nil clears it."
  [db-info {:keys [workspace-id label]}])

(defn focus!
  "Touch workspace.last_focused_at_ms and upsert repo_focus row.
   Cheap; channel calls this whenever user switches active tab /
   selects a workspace."
  [db-info workspace-id])

(defn workspace-with-session
  "Return {:workspace :session-state} hydrated for rendering.
   Single fetch; channel layer never N+1's."
  [db-info workspace-id])

(defn list-active-with-sessions
  "Like list-active but each entry is the {:workspace :session-state}
   pair. Sorted by last_focused_at_ms DESC NULLS LAST,
   then created_at DESC."
  [db-info repo-id])

(defn last-focused
  "Return the workspace id from repo_focus for repo-id, or nil."
  [db-info repo-id])

(defn start-merge-resolve!                       ; §7
  [db-info {:keys [workspace-id parent-session-id channel-id]}])
```

### 4.4 Spawn rewrite — dirty + ignored copy

Order matters (cross-validated against existing `spawn-branch!`):

```clojure
;; 1. mint id + branch + worktree path
(let [ws-id  (str (UUID/randomUUID))
      branch (str "vis/" (subs ws-id 0 8))
      wt     (worktree-root rid ws-id)]
  ;; 2. with-repo-lock prevents concurrent git mutations
  (with-repo-lock rid
    ;; 3. ensure parent dir
    (.mkdirs (.getParentFile (io/file wt)))
    ;; 4. git worktree add -b vis/<id> <wt> HEAD
    (if (local-branch-exists? repo-root branch)
      (git! repo-root ["worktree" "add" wt branch])
      (git! repo-root ["worktree" "add" "-b" branch wt "HEAD"]))
    ;; 5. mirror trunk → worktree, excluding .git/
    ;;    (worktree now has tracked@HEAD + .git link; rsync overwrites)
    (mirror-tree! repo-root wt {:exclude #{".git"}})
    ;; 6. insert workspace row + pin to session-state (if provided)
    ...
    ;; 7. fire :on-spawn hook
    ))
```

`mirror-tree!` is a Java NIO `Files.walk` that:
- preserves symlinks as symlinks (no target deref)
- preserves file mtime + perms
- skips `.git/` recursively (top-level only; nested git submodules
  remain untouched — git already handles them via worktree add)
- never reads/writes outside `repo-root` / target

Result: worktree is bit-identical to trunk including `.env`,
`node_modules/`, `target/`, untracked files, uncommitted edits.
This is the explicit user requirement: workspace must "just work"
without re-running `npm install` / `clojure -P` / whatever.

### 4.5 FF apply

```clojure
(defn ff-apply! [db-info {:keys [workspace-id]}]
  (let [ws        (get db-info workspace-id)
        _         (when (= :trunk (:kind ws)) (throw …))
        repo-root (:repo-root ws)
        wt        (:root ws)
        branch    (:branch ws)
        trunk     (detect-trunk-branch repo-root)]
    (with-repo-lock (:repo-id ws)
      ;; transition active|merging -> merging
      (p/db-workspace-update-state! db-info workspace-id :merging)
      (let [trunk-dirty? (not (str/blank? (git! repo-root ["status" "--porcelain"])))
            stashed?     (when trunk-dirty?
                           (git! repo-root ["stash" "push" "-u"
                                            "-m" (str "vis-ff-" workspace-id)])
                           true)]
        (try
          ;; switch trunk to its detected branch (idempotent)
          (git! repo-root ["checkout" trunk])
          ;; FF only
          (git! repo-root ["merge" "--ff-only" branch])
          (let [head (git! repo-root ["rev-parse" "HEAD"])]
            (p/db-workspace-update-state! db-info workspace-id :merged)
            {:status :ok :sha head :branch branch})
          (catch Throwable t
            ;; FF failed (non-ancestor, conflict-by-merge attempt)
            ;; Workspace stays in :merging — engine spawns merge-resolve.
            {:status :ff-failed
             :reason (or (ex-message t) (str t))
             :workspace ws
             :trunk trunk})
          (finally
            (when stashed?
              (try (git! repo-root ["stash" "pop"]) (catch Throwable _ nil)))))))))
```

### 4.6 Slash handler bridge

The foundation-workspace extension (§6) wraps each core fn in a
slash handler that:
- destructures slash ctx → `{:keys [session/id db-info channel/id reply!]}`
- looks up `(workspace/for-session db-info session/id)`
- calls the core fn
- builds a `:slash/*` envelope (§3) and returns

Core fns never know about slash. They take `db-info + opts`,
return canonical data. Pure separation.

### 4.7 Repo lock

```clojure
(defn- repo-lock-file ^File [repo-id]
  (io/file (System/getProperty "user.home") ".vis" "locks"
           (str repo-id ".lock")))

(defmacro with-repo-lock [repo-id & body]
  `(let [f# (repo-lock-file ~repo-id)]
     (.mkdirs (.getParentFile f#))
     (with-open [ch# (java.nio.channels.FileChannel/open
                       (.toPath f#)
                       (into-array java.nio.file.OpenOption
                         [java.nio.file.StandardOpenOption/CREATE
                          java.nio.file.StandardOpenOption/WRITE]))]
       (let [lk# (.lock ch#)]
         (try ~@body
              (finally (.release lk#)))))))
```

Cross-process advisory. Holds during `spawn-branch!`, `commit!`,
`ff-apply!`, `discard!`. Read paths (`status`, `list-*`, `trunk-info`)
don't acquire — they're read-only and tolerate races.

---

## 5. Slash command tree — `/workspace …`

Registered by `vis-foundation-workspace` extension (§6).

```
/workspace              (parent)
/workspace new       [label?]              spawn :branch from trunk
/workspace commit    <message…>            git add -A + commit (worktree)
/workspace apply                           FF onto trunk; on conflict -> merge-resolve
/workspace discard   [--hard]              soft = worktree only; hard = + branch -D
/workspace list                            show all active in this repo
/workspace switch    <selector>            id | label | index | session-title
/workspace label     [text|--clear]        set/clear explicit label
```

`requires` per command:

| command | requires | availability |
|---|---|---|
| new      | `#{:session}` | always (spawn for current session's repo) |
| commit   | `#{:session}` | `(= :branch (:kind ws))` |
| apply    | `#{:session}` | `(= :branch (:kind ws))` and `(not= :merged (:state ws))` |
| discard  | `#{:session}` | `(= :branch (:kind ws))` and `(#{:active :merging} (:state ws))` |
| list     | `#{:channel}` (session optional) | always |
| switch   | `#{:channel}` (session optional) | always |
| label    | `#{:session}` | always |

`new` on a branch-kind session: **refuse** with "you are in workspace
X; use /workspace apply or /workspace discard first, or /workspace switch
to a different one". (Avoids fork-from-branch ambiguity.)

`commit` with empty diff → `{:status :nothing-to-commit}` envelope,
notify "nothing to commit".

`apply` with zero commits-ahead → refuse "no commits to apply; /commit first".

`discard` of the active workspace → on success, `focus!` jumps to
last_focused workspace in repo (or trunk-kind ensure if none left).

---

## 6. Foundation extension — `vis-foundation-workspace` (NEW)

Mirrors `vis-foundation-git` shape:

```
extensions/common/vis-foundation-workspace/
  deps.edn
  resources/META-INF/vis-extension/vis.edn        ;; {workspace {:nses [...]}}
  src/com/blockether/vis/ext/foundation_workspace/core.clj
  src/com/blockether/vis/ext/foundation_workspace/handlers.clj
  src/com/blockether/vis/ext/foundation_workspace/ctx.clj
  test/com/blockether/vis/ext/foundation_workspace/core_test.clj
  test/com/blockether/vis/ext/foundation_workspace/handlers_test.clj
  test/com/blockether/vis/ext/foundation_workspace/ctx_test.clj
```

### 6.1 `core.clj` — registration

```clojure
(vis/register-extension!
  (vis/extension
    {:ext/name        "foundation-workspace"
     :ext/description "Workspace + slash command surface for /workspace … intents."
     :ext/requires    ["foundation-git"]
     :ext/owner       "vis"
     :ext/version     "0.1.0"}))

(doseq [spec slash-specs] (vis/register-slash! spec))
(vis/register-ctx-renderer! :session/workspace #'ctx/render-workspace-block)
;; Read-only :v/workspace-* ops for the model. No mutators.
(doseq [op read-ops] (vis/register-op! op))
```

### 6.2 `handlers.clj` — slash → core

Each handler is `(fn [ctx])` returning slash envelope. Pure:
they don't touch UI, don't talk to channels directly; the
`:reply!` ifn in ctx is the channel's render hook.

```clojure
(defn handle-workspace-new
  [{:keys [session/id db-info slash/args]}]
  (let [ws       (workspace/for-session db-info id)
        label    (str/join " " args)
        spawned  (workspace/spawn-branch! db-info
                   {:from ws
                    :session-state-id id
                    :label (when-not (str/blank? label) label)})]
    {:slash/status :ok
     :slash/title  "Workspace created"
     :slash/body   (str (workspace/display-label
                          {:title (some-> spawned :session-state :title)
                           :label (:label spawned)})
                     " · " (:branch spawned))
     :slash/actions []}))
```

And similarly for the other 6 commands.

### 6.3 `ctx.clj` — CTX block

```clojure
(defn render-workspace-block
  "Return the :session/workspace map for CTX. Pure; takes a fully
   hydrated workspace+session-state pair."
  [{:keys [workspace session-state]}]
  (let [repo-root (:repo-root workspace)
        trunk     (workspace/detect-trunk-branch repo-root)
        status    (workspace/status db-info (:id workspace))
        commits   (commits-ahead-of repo-root (:root workspace) trunk)]
    {:workspace/id     (:id workspace)
     :workspace/kind   (:kind workspace)
     :workspace/label  (:label workspace)
     :session/id       (:session_soul_id session-state)
     :session/state-id (:id session-state)
     :session/title    (:title session-state)
     :session/fork-of  (when-let [p (:parent_state_id session-state)]
                         {:soul (:session_soul_id session-state)
                          :parent-state p})
     :git/branch       (:branch workspace)
     :git/trunk        trunk
     :git/head         (:head status)
     :git/dirty?       (:dirty? status)
     :git/commits-ahead commits
     :git/ff-possible? (ff-possible? repo-root (:branch workspace) trunk)
     :git/stats        (git-stats repo-root (:root workspace) trunk)}))
```

`commits-ahead-of`, `ff-possible?`, `git-stats` are jgit / sh helpers
in this extension. Self-contained; not in core `workspace.clj`.

### 6.4 Slash trailer pin

Handler also produces a trailer entry for the engine's per-iter pin:

```clojure
{:scope "tN/iN/fK"
 :tag   :user-slash
 :src   "/workspace apply"
 :result {:slash/status :ok :workspace-id … :sha …}}
```

Engine appends this to current iter's pin like any other form. Model
sees on next render that user slashed `/workspace apply`. Distinct
`:tag` so the model can filter.

### 6.5 Read-only SCI ops for the model

```clojure
(:v/workspace-status     {:tag :observation})   ; current pinned ws
(:v/workspace-list       {:tag :observation})   ; all active in repo
(:v/workspace-commits    {:tag :observation})   ; commits-ahead detail
```

No mutators. Mutations go through slash (user explicit) or through
the model's `(satisfy-hint! …)` flow proposing a slash to the user.
Reason: the model should never silently apply a workspace to trunk;
that's a user-side commitment.

---

## 7. Merge-resolve sub-session

Triggered by `ff-apply!` returning `:ff-failed`.

### 7.1 Bootstrap

```clojure
(defn start-merge-resolve!
  [db-info {:keys [workspace-id parent-session-id channel-id]}]
  (let [ws (get db-info workspace-id)]
    (with-repo-lock (:repo-id ws)
      ;; Attempt the real merge (without --ff-only) so the conflict
      ;; state lands in the trunk index.
      (try (git! (:repo-root ws) ["merge" (:branch ws)])
        (catch Throwable _ nil))
      ;; Fork the parent session to a NEW session_state pinned to the
      ;; SAME workspace as the parent (NOT a new workspace — we need
      ;; the trunk-side conflict state visible). Mark it merge-resolve.
      ;;
      ;; This is the ONE exception to "fork = new workspace"; merge
      ;; resolve sessions piggyback on the parent's workspace because
      ;; they're operating on the trunk side of the same merge.
      ...
      ;; Publish event so channels render the overlay/badge.
      (vis/publish-channel-event! channel-id
        {:type :session/merge-resolve-started
         :parent-session parent-session-id
         :sub-session    sub-id
         :workspace      workspace-id
         :conflicts      (parse-conflicts (:repo-root ws))}))))
```

### 7.2 Sub-session capabilities

The sub-session gets a different SCI binding map: standard core +
git ops PLUS the `merge/*` op family, registered with availability
gated on `(= :merge-resolve (-> session :tags))` (or whatever marker
we settle on; see §7.4):

```clojure
(merge/status)             ; conflict file list + branches
(merge/conflict <path>)    ; per-file conflict structure
(merge/accept-ours   <path>)
(merge/accept-theirs <path>)
(merge/mark-resolved <path>)  ; after manual v/patch
(merge/continue {:message "…"})  ; git commit, end sub-session
(merge/abort)              ; git merge --abort, end sub-session
```

Sub-session prompt is different: a system prompt section explains
the task, the available verbs, and the success criterion
(`merge/continue` or `merge/abort`).

### 7.3 Channel UX (referencing §3 envelope)

Engine publishes `:session/merge-resolve-started` and
`:session/merge-resolve-finished` events. Each channel decides
rendering:

- **TUI**: header badge `MERGING <parent-label> ← <child-label>`,
  input box stays normal, focus auto-switches to sub-session.
- **Telegram**: bot sends "Merge conflict, trying to resolve…" with
  occasional progress updates; final "Merged" or "Aborted".
- **CLI/REPL**: stdout stream.

### 7.4 Session marking — schema decision

Option A: add `session_state.tags TEXT` column (comma-sep) or
`session_state.kind TEXT` enum.

Option B: rely on a workspace flag — add
`workspace.is_merge_resolve INTEGER DEFAULT 0` + parent linkage.

**Decision: B.** Inline V1 add:
```sql
ALTER TABLE workspace ADD COLUMN is_merge_resolve INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspace ADD COLUMN parent_workspace_id_resolve TEXT
  REFERENCES workspace(id) ON DELETE SET NULL;
```
Wait — `parent_workspace_id` already exists for spawn lineage. Reuse
it for merge-resolve linkage too? They're orthogonal concepts. Keep
separate to avoid overloading.

Actually simpler: merge-resolve sub-session pins to parent's workspace
(§7.1). The flag lives on `session_state`, not `workspace`. Revised:

```sql
ALTER TABLE session_state ADD COLUMN merge_resolve_parent_id TEXT
  REFERENCES session_state(id) ON DELETE SET NULL;
```

When non-NULL, this `session_state` is a merge-resolve sub-session
for the referenced parent. SCI binding lookup checks this column to
decide whether to register `merge/*` ops.

---

## 8. CTX shape — `:session/workspace` (VCS-agnostic, canonical)

Live engine (`ctx_spec.clj`, `ctx_engine/empty-ctx`, `prompt.clj`)
already settled on the `:vcs/*` namespace as canonical, with kind-
specific aliases (`:git/*`, `:hg/*`, `:jj/*`) optional and additive.
Workspace identity + session linkage keys live in their own
namespaces (`:workspace/*`, `:session/*`). CTX_REDESIGN.md still
documents `:git/*`; that doc is now stale on this point and must be
updated in the same PR that lands §6 (foundation-workspace).

```clojure
:session/workspace
  ;; VCS-agnostic block.  :vcs/kind is the discriminator the model
  ;; reads first; everything else is OPTIONAL at the spec level.
  ;; Empty / non-VCS sessions render {:vcs/kind :none}.
  {;; --- identity (always present once workspace pin exists) ---
   :workspace/id        "01HXYZ..."
   :workspace/kind      :branch | :trunk                  ; Vis kind, NOT git
   :workspace/label     "frontend"                        ; nil unless set

   ;; --- session linkage ---
   :session/id          "01ABCD..."                       ; soul id
   :session/state-id    "01EFGH..."                       ; state id (fork-aware)
   :session/title       "Auth bcrypt refactor"            ; nil -> rendered "Untitled"
   :session/fork-of     {:soul "..." :parent-state "..."} ; nil for root state

   ;; --- VCS surface (canonical :vcs/* keys) ---
   :vcs/kind            :git                               ; #{:git :hg :jj :fossil :none}
   :vcs/branch          "vis/abc12345"
   :vcs/trunk           "main"                             ; resolved per §2
   :vcs/head            "def567..."                        ; HEAD sha (or rev id for hg/jj)
   :vcs/dirty?          true
   :vcs/stats           {"src/auth.clj" {:added 5 :removed 2}}  ; vs trunk

   ;; --- workspace-derived VCS state (canonical :vcs/* keys) ---
   :vcs/commits-ahead   [{:sha "abc..." :message "introduce emit-event"}
                         {:sha "def..." :message "rewire log/2"}]
   :vcs/ff-possible?    true | false | :unknown

   ;; --- git-specific extras (additive aliases; only when :vcs/kind = :git) ---
   :git/branch          "vis/abc12345"   ;; same as :vcs/branch; emitted as convenience
   :git/trunk           "main"           ;; same as :vcs/trunk
   :git/head            "def567..."
   :git/dirty?          true
   :git/stats           {…}}
```

### Stamping rules — who fills this map

`empty-ctx` returns `{:vcs/kind :none}`. The workspace foundation
extension (§6) stamps the rest at the start of every turn, via the
engine's pre-turn CTX hook surface. The stamping function:

1. Looks up `(workspace/for-session db-info session-state-id)`.
2. Looks up `(session-state-by-id db-info session-state-id)`.
3. If no workspace pin yet → emits `{:vcs/kind :none}` (engine
   default; matches `empty-ctx`).
4. If workspace pinned → emits the full identity + session-linkage +
   `:vcs/*` keys above, plus `:git/*` mirrors when `:vcs/kind = :git`.
5. `:vcs/commits-ahead` and `:vcs/ff-possible?` are cached at
   stamp time (per-turn) and NOT recomputed for every render of the
   same turn.

The legacy `workspace/status` fn (returns `:git/*` keys directly)
stays as a low-level helper but its callers are updated to consume
`:vcs/*` from the stamped block, not call `status` directly.

Deprecation: `:git/*` aliases ship for one release cycle alongside
`:vcs/*`. After all consumers migrate (audited list lives in §13),
remove the legacy keys from the stamped block — `ctx_spec.clj`
already marks them transitional.

### Hg / jj / fossil readiness

A non-git VCS detector emits `{:vcs/kind :hg :vcs/branch "default"
:vcs/head "<rev>" :vcs/dirty? false :vcs/stats {…}}` plus optional
`:hg/*` aliases. No git-specific assumption in CTX. The slash
foundation extension is git-only initially; a future hg/jj extension
registers its own `/hg ...` or extends `/workspace ...` semantics.
Either way, the CTX shape supports it from day one.

---

## 9. TUI surface changes

### 9.1 Header strip (`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/header.clj`)

- Strip continues to filter on `#{:active :merging}` (current PLAN.md
  decision 12 — unchanged).
- Strip pulls from `workspace/list-active-with-sessions` so each
  entry already has session_state hydrated.
- Per tab label = `(workspace/display-label …)` (one line, see §0).
- Active marker = reverse-video, no glyph.
- Secondary info (kind, branch, ahead count, dirty) goes into the
  detail panel (under the strip) or footer when active; NOT in
  the strip itself.

### 9.2 Command palette (`dialogs.clj` `palette-commands`)

Remove:
- `:workspace`
- `:apply-workspace-to-trunk`
- `:discard-workspace-soft`
- `:discard-workspace-hard`

These are now slash-only (§5). Palette still contains:
- new session, fork session, switch session
- providers, settings
(any other non-workspace entries kept as-is)

### 9.3 Slash overlay (`screen.clj` `menu-commands`, `command_suggest.clj`)

Today `menu-commands` is `(concat palette-commands slash-only-commands extension-commands)`.

Replace with `(concat palette-commands (vis/registered-slashes) extension-commands)`.

`command_suggest.clj` already builds `:slash/...` entries — extend
its input format to accept the registry's `:slash/name :slash/parent
:slash/usage :slash/doc :slash/subcommands` shape directly. Lossless
mapping; existing fuzzy-match logic carries over.

### 9.4 Tab restoration

On TUI start:
1. `(workspace/discover-repo-root)` (existing).
2. `repo-id ← (workspace/repo-id-for repo-root)`.
3. `tabs ← (workspace/list-active-with-sessions db-info repo-id)`.
4. `active-id ← (or (workspace/last-focused db-info repo-id)
                    (first tabs))`.
5. For each tab, find the workspace's pinned `session_state` (already
   in the hydrated map). Open that session.

On workspace switch (mouse / key / `/workspace switch`):
`(workspace/focus! db-info ws-id)` then switch app-db `active-workspace-id`.

### 9.5 Switch-session dialog

Replace current rows with hydrated workspace+session pairs:

```
  Auth bcrypt refactor       branch · vis/abc · 3 ahead    2m ago
  Ctx redesign step 3        branch · vis/def · dirty      5m ago
  Refactor logging — fronted trunk  · main                  1h ago
  Untitled (v2 fork)         trunk  · main                  3h ago
```

Sort: last_focused_at_ms DESC, then created_at DESC.

---

## 10. Telegram surface changes

`extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/bot.clj`:

1. On each incoming text message, call `(slash/dispatch ctx text)` BEFORE
   forwarding to LLM. `:handled? true` → render envelope as reply
   (with inline keyboard for `:slash/actions`), DO NOT enter LLM round-trip.

2. Add bot command menu entries via Telegram's `setMyCommands` API for
   discoverability: `/workspace`, `/help`. (Granular `/workspace apply`
   etc. are typed; Telegram's menu doesn't render nested commands.)

3. When `:session/merge-resolve-started` event fires, bot posts a
   status message "Merge conflict detected, attempting to resolve…"
   and pins it. Resolve-finished event unpins and posts result.

---

## 11. Engine loop integration (`src/com/blockether/vis/internal/loop.clj`)

At the very top of a turn, after building the env but before the
LLM request:

```clojure
(when-let [result (slash/dispatch
                    {:channel/id   channel-id
                     :session/id   session-id
                     :workspace/id (:id active-workspace)
                     :db-info      db-info
                     :reply!       channel-reply-fn
                     :publish-event! channel-publish-fn}
                    user-message)]
  (when (:handled? result)
    (persist-slash-iteration! db-info session-id result)
    (publish-channel-result! channel-id result)
    (return-early-from-turn!)))
```

`persist-slash-iteration!` writes a synthetic `session_turn_iteration`
row marking this turn as resolved without an LLM round-trip:
- `code` = the slash text
- `result` = the envelope (Nippy)
- `tag` = `:user-slash`
The next turn's CTX render includes this iteration in the trailer.

---

## 12. Implementation sequence

Each step is one commit, green under `./verify.sh --quick`. Full
`./verify.sh` before steps 3, 6, 9, 12. Hard-cutover policy: every
step's commit deletes the legacy entries from §3a's K-list that
belong to that step. No transitional flags.

1. **Schema inline V1**: add `label`, `last_focused_at_ms` to `workspace`;
   add `repo_focus` table; add `session_state.merge_resolve_parent_id`.
2. **Persistance fns**: extend `row->workspace`; add `db-workspace-update-label!`,
   `db-workspace-touch-focus!`, `db-repo-focus-get`, `db-repo-focus-set!`.
3. **`workspace.clj` core refactor + KILL legacy**:
   - new fns: `detect-trunk-branch`, `with-repo-lock`, `mirror-tree!`,
     `spawn-branch!` rewrite, `commit!`, `ff-apply!`, `set-label!`,
     `focus!`, `workspace-with-session`, `list-active-with-sessions`,
     `last-focused`, `display-label`, `start-merge-resolve!` skeleton.
   - KILL `apply-to-trunk!` body (K4).
   - KILL `workspace-apply-to-trunk!` re-export in `core.clj` (K5).
   - REWRITE `workspace/status` to emit `:vcs/*` keys instead of
     `:git/*` (K3); or kill `status` entirely if §6.3 stamping covers
     every consumer — verified zero callers outside `core.clj:86`.
4. **`ctx_spec.clj` KILL `:git/*` aliases**:
   - delete `:git/branch :git/trunk :git/head :git/dirty?` specs (K1).
   - rename `:git/file-stats` → `:vcs/file-stats`; update `:vcs/stats`
     to reference the new name (K1).
   - delete `:git/*` opt-keys from `::workspace` (K2).
   - update `prompt.clj` to advertise only `:vcs/*` (already does;
     verify no `:git/*` leaks).
5. **Slash registry**: extend `internal/registry.clj` with `slash-registry`
   atom + register/deregister/lookup; add `internal/slash.clj` with
   parser + dispatch; export from `core.clj`. Spec for `::slash`
   (§3). No slash handlers yet — just the surface.
6. **`vis-foundation-workspace` extension + foundation-voice migration**:
   - new `extensions/common/vis-foundation-workspace/` package; registers
     workspace slash tree, CTX stamping fn, read-only SCI ops.
   - MIGRATE `vis-foundation-voice` from `:tui.slot/commands` to
     `(vis/register-slash! …)` with `requires #{:channel}` and
     `availability-fn` accepting `:tui` AND `:telegram` (K10).
   - DELETE `voice-input-tui-commands` + `:ext/channel-contributions`
     map entry from `foundation_voice/core.clj`.
7. **Engine loop integration**: slash dispatch at turn start in
   `loop.clj`; synthetic iteration persistence; channel result
   publication. CTX `:session/workspace` stamping wired through
   foundation-workspace pre-turn hook.
8. **TUI channel rewrite + KILL TUI slash legacy**:
   - DELETE `:workspace :apply-workspace-to-trunk
     :discard-workspace-{soft,hard}` from `dialogs.clj`
     `palette-commands` (K6).
   - DELETE `slash-only-commands` vec from `screen.clj` (K7).
   - REPLACE `menu-commands` body with `(concat palette-commands
     (vis/registered-slashes))` (K6+K7+K8).
   - DELETE `extension-commands` fn + `:tui.slot/commands` consumer
     (K8). Header-row slot stays.
   - REWRITE strip to consume `list-active-with-sessions` +
     `display-label`.
   - REWRITE switch-session dialog with hydrated workspace+session
     rows.
   - Tab restoration via `last-focused` + per-repo focus.
   - On switch, call `workspace/focus!`.
9. **Telegram channel rewrite + KILL Telegram slash legacy**:
   - DELETE `parse-command` + `handle-command!` from `bot.clj`
     (K9). The 11 hardcoded commands migrate one-by-one to
     `register-slash!` calls (each lands in the appropriate
     foundation extension or stays inside `vis-channel-telegram`
     for telegram-specific ones).
   - `handle-user-text!` calls `(slash/dispatch ctx text)` before
     `vis/send!`; on `:handled? true`, render envelope as reply
     (with inline keyboard for `:slash/actions`).
   - Bot's `setMyCommands` fed from `(vis/registered-slashes)`
     filtered by availability + `:channel/id :telegram`.
10. **Merge-resolve sub-session**: real implementation. Spawn
    sub-session, register `merge/*` ops gated on
    `session_state.merge_resolve_parent_id`, sub-session prompt,
    completion event flow.
11. **Docs sync + KILL stale docs**:
    - REWRITE CTX_REDESIGN.md workspace section to `:vcs/*` shape
      (K11). Point at PLAN.md §8 for sourcing rules.
    - Audit `HANDOFF.md`, `TODO.md`, `TASKS.md`, `CHANGELOG.md` for
      `:git/branch` / `:git/trunk` / `apply-to-trunk!` mentions
      tied to the redesigned surface; remove or rewrite (K12).
12. **Cleanup pass**: remove every `PLAN.md decision N` comment
    whose referent has changed (audit list in §13). Verify ripgrep
    for `:git/branch :git/trunk apply-to-trunk slash-only-commands
    tui.slot/commands` returns ZERO hits in `src/` and `extensions/`.

---

## 13. Cross-validation against live code

Inventory of every `PLAN.md` reference in source. Each line lists the
file, what the comment claims, and whether this doc still says the
same. Mismatches are work-items for the relevant step above.

| location | claim | status under this doc |
|---|---|---|
| `extensions/common/vis-foundation-core/.../environment/core.clj:55` | "channel rebinds `*workspace-root*` per turn (PLAN.md §5)" | unchanged — still true |
| `extensions/common/vis-foundation-core/.../environment/agents.clj:35` | "treat active workspace root as repo root (PLAN.md §3)" | unchanged |
| `extensions/common/vis-foundation-core/.../environment/agents.clj:162` | "channel rebinds `*workspace-root*` per turn (PLAN.md §5)" | unchanged |
| `extensions/common/vis-foundation-git/.../core.clj:7` | "per turn, PLAN.md §5" | unchanged |
| `extensions/persistance/vis-persistance-sqlite/.../core.clj:532` | "Workspace - trunk-native work units (PLAN.md §2-§4)" | unchanged; §4 now owns the API |
| `extensions/persistance/vis-persistance-sqlite/.../core.clj:669` | "enforcing the 1:1 invariant (PLAN.md §1 decision 1)" | unchanged — see §0 |
| `extensions/persistance/vis-persistance-sqlite/.../core.clj:919` | "exactly one workspace (1:1, PLAN.md decision 1)" | unchanged |
| `extensions/persistance/vis-persistance-sqlite/test/.../test_helpers.clj:72` | "workspace per call (PLAN.md decision 1 — session_state.workspace_id …)" | unchanged |
| `extensions/channels/vis-channel-tui/.../header.clj:81` | "PLAN.md decision 12 — header strip shows live workspaces only" | unchanged; §9.1 reaffirms |
| `extensions/channels/vis-channel-tui/.../header.clj:100` | "PLAN.md decision 1; active entry label tracks session" | unchanged; §9.1 makes display rule explicit |
| `extensions/channels/vis-channel-tui/.../screen.clj:1794` | "PLAN.md decision 1 — each fork gets its own workspace" | unchanged |
| `extensions/channels/vis-channel-tui/.../screen.clj:2385` | "PLAN.md decision 6 — /workspace spawns a fresh vis/<short-id> branch …" | replaced — §5 now owns this; § 12 step 8 removes the inline palette path |
| `extensions/channels/vis-channel-tui/.../render.clj:1426` | "shell/meta to 2 (observation/mutation) per PLAN.md §2.1" | unrelated to this redesign; leave alone |
| `extensions/channels/vis-channel-tui/.../chat.clj:399` | "PLAN.md decision 1, decision 6 …" | unchanged in spirit; §9.4 covers tab restore |
| `extensions/common/vis-foundation-core/test/.../transcript_test.clj:508` | "No UUID leaks in user/LLM-facing surfaces (PLAN.md § 2.9 + § 2.10)" | unrelated; leave |
| `extensions/common/vis-foundation-core/test/.../transcript_test.clj:535` | "PLAN.md § 2.9" | unrelated; leave |
| `HANDOFF.md:96`, `:120`, `:143` | references CTX_REDESIGN.md hints rollout | unrelated to workspaces |

After step 12, no remaining `PLAN.md decision N` references should be
stale. Step 12's commit message must call out which references it
removes or updates.

---

## 14. Out of scope

- Multi-workspace coordination (one session reaching into another).
- Cross-repo workspaces (one workspace spanning two git repos).
- Mercurial / jujutsu / fossil. `:git/*` keys are namespaced so
  `:hg/*` / `:jj/*` can coexist later without renaming.
- Workspace garbage collection (`:merged`/`:discarded` rows beyond
  some age). Today they're query-only via `list-finished`; persistance
  decides retention policy out-of-band.
- Network sync (workspace state across machines). All state is local
  SQLite + local worktrees.

---

## 14a. Bug list (live verification 2026-05-22)

All discovered by nREPL eval / source reads. Each is fixed by the
implementation sequence step listed in `fix-in` column.

| # | symptom | root cause | fix-in |
|---|---|---|---|
| B1 | `workspace/trunk-info` returns current branch instead of repo's default branch | `trunk-info` calls `current-branch` not `detect-trunk-branch` | §12 step 3 |
| B2 | `workspace/status` emits `:git/*` keys; CTX expects `:vcs/*` canonical | legacy; predates `:vcs/*` migration in `ctx_spec.clj` | §12 step 3 (rewrite) or K3 (kill) |
| B3 | `:session/workspace` block is NEVER stamped by anything; empty-ctx default `{:vcs/kind :none}` survives the whole session | no extension owns the stamping hook; CTX engine TODO comment in `ctx_engine.clj:934` flags this | §12 step 6 |
| B4 | `apply-to-trunk!` is file-copy, ignores commit history of the workspace branch | original implementation predates the FF design | K4 + §12 step 3 |
| B5 | `spawn-branch!` does NOT mirror dirty / untracked / ignored files from trunk; user runs `npm install` again after `/workspace new` | original `git worktree add` only checks out `HEAD` | §12 step 3 |
| B6 | No engine slash registry; TUI hardcodes workspace ops in `palette-commands` + `slash-only-commands`; Telegram parses slashes in its own `handle-command!` | feature never built | K6+K7+K8+K9 + §12 steps 5 + 7 + 9 |
| B7 | CTX_REDESIGN.md documents legacy `:git/*` shape; conflicts with `ctx_spec.clj` canonical `:vcs/*` | doc drift | K11 + §12 step 11 |
| B8 | `command_suggest.clj` only handles per-channel palette entries; cannot render arbitrary registry entries | TUI-specific design predating engine registry | K7+K8 + §12 step 8 |
| B9 | Voice extension registers TUI-only slash; Telegram has parallel hardcoded `/voice`; web (when it lands) will need a third registration | per-channel slot pattern (`:tui.slot/commands`) instead of engine-level surface | K10 + §12 step 6 (foundation-voice migration block in same PR) |
| B10 | `s/valid? :session/workspace` accepts `{:foobar 1}` — schema is open by design but lacks any closed-schema variant for engine-stamped CTX block | tradeoff baked in (extensions add keys freely); not a real bug, but worth noting in renderer guarantees | informational; §6.3 stamping fn produces a known closed shape, schema validation stays loose |

## 15. Open items (must be resolved during implementation, not before)

- **Q1**: TUI active marker — reverse-video only, or also a glyph?
  Default: reverse-video only.
- **Q2**: Slash autocomplete progressive vs full-tree on first `/`?
  Default: full-tree fuzzy match; user types more to narrow.
- **Q3**: Telegram inline keyboards for `:slash/actions` — fixed
  set or arbitrary? Default: arbitrary; bot maps button.callback_data
  to slash text and re-enters `slash/dispatch`.
- **Q4**: Trunk-kind workspace with `commit!` — refuse cleanly (§5)
  but what about user genuinely wanting to commit on trunk? Default:
  refuse; user uses `git commit` directly or spawns a workspace.
- **Q5**: `apply` when worktree itself is dirty (uncommitted) —
  refuse "commit your worktree first" or auto-commit? Default:
  refuse, force explicit `/workspace commit` first.
