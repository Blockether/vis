# Drafts

A **draft** is an isolated workspace for trying a change without editing your
real working directory. Work in the draft, then choose what happens next:

| Decision | Command | Result |
| --- | --- | --- |
| Keep it | `/draft apply` | Copy the draft's changes into your real directories and consume the draft. |
| Keep it for later | `/draft stash` | Return to trunk while retaining the draft for `/draft resume`. |
| Throw it away | `/draft abandon` | Discard the draft permanently and return to trunk. |

Drafts are opt-in. A session starts on **trunk**, where it edits your real files
directly. Nothing is isolated until you explicitly create or resume a draft.

> **The safe default is stash.** Apply changes your real files. Abandon cannot be
> undone. Stash does neither.

## Quick start

Start from the current workspace tree, make a change, and land it:

```text
/draft new safer-parser
Please refactor the parser and run its tests.
/draft
/draft apply
```

Park unfinished work and return later:

```text
/draft stash
/draft list
/draft resume safer-parser
```

Switch between two drafts from the slash interface:

```text
# while inside draft A
/draft stash
/draft resume feature-b
```

There is deliberately no destructive "switch" hidden in that flow: the current
draft is parked before another one is entered.

## The mental model

Vis uses several related nouns. They describe different things:

| Term | Meaning |
| --- | --- |
| **Project** | A gateway-owned, cross-channel collection of sessions (the TUI's project/tab set). A project may be bound to a workspace root, but it organizes conversations; it does not isolate files. |
| **Repo root** (`repo_root`) | The real root directory from which a workspace was derived. Active drafts are listed per repo root. |
| **Workspace** | The persisted filesystem location currently pinned to a session, including its primary root and any extra filesystem roots. |
| **Trunk** | A live workspace that points at your real directories. This is the default. |
| **Draft** | An isolated workspace with a baseline and a merge-back target. |

A project can contain several sessions. Each session is pinned to one workspace.
Each workspace is either trunk or a draft. Draft identity and listing are based
on the repo root, not on project membership.

The runtime also exposes detected repository/language metadata under
`session.env.project`. That metadata is not the gateway **Project** record above;
the shared name is historical. In this feature, **project organizes sessions;
workspace selects files; draft isolates a workspace**.

This is why the gateway routes are under `/workspace`: resuming a draft changes
where a session's tools run. It does not move the session to another project.

## Why drafts exist

Drafts make speculative agent work cheap to reject:

- **Isolation before commitment.** Refactors, generated files, and deletions occur
  in the isolated copy until you apply.
- **An explicit boundary.** Apply, stash, and abandon make the transition back to
  real files visible rather than implicit.
- **Named, resumable work.** Labels make parked work discoverable across turns and
  channel reconnects.
- **One current location.** Every tool call sees one coherent root instead of a
  mixture of several working copies.

A draft is **not** a Git branch, commit, or stash. Vis does not create commits or
manage your Git history, and apply is not a three-way Git merge.

## Lifecycle and invariants

```text
                         /draft apply    (land, consume)
                       ┌──────────────────────────────────┐
                       │                                  ▼
trunk ── new/clean/blank ──► draft ── /draft stash ──► trunk
  ▲                    │                         │
  │                    └── /draft abandon ───────┘
  │                              (destroy)
  └──────────── /draft resume <label> ────────── parked draft
```

The important invariants are:

1. A session is pinned to exactly one current workspace.
2. A repo may have many active drafts, but a session can be inside only one.
3. "Stashed" is not a second copy or a Git stash. It is an active draft that the
   session has left intact on disk.
4. A draft can be pinned to only one session at a time. Another session cannot
   resume it until the first session leaves it.
5. Applied and abandoned drafts are no longer active and disappear from
   `/draft list`.

The TUI footer marks an isolated workspace as `DRAFT`; the session navigator shows
its label in the Draft column. `C-x e` opens the dedicated draft picker. Bare
`/draft` reports the same state and the current changed-file count.

## Starting a draft

### `/draft new <label>`

Creates and enters a draft forked from the **current workspace tree**. This is a
filesystem snapshot of the root Vis is using, not an instruction to check out a
clean Git `HEAD`. Existing working-tree content — including your uncommitted
changes — is therefore the source of the draft.

The fork is **gitignore-aware**. Whatever your repository ignores is not copied
into the draft, and neither are regenerable artifact directories at any depth
(`node_modules`, `target`, `dist`, `build`, `coverage`, `__pycache__`,
virtualenvs, Yarn Berry's `.yarn/cache` and `.yarn/unplugged`, and framework
caches such as `.next`, `.turbo`, or `.vite`) — even when your repository
happens to track one. A file git tracks despite an ignore
rule is still copied, and the repository itself (`.git`) is always copied.

That is what makes forking a large repository fast: this repository clones about
2,400 entries instead of about 20,000. Anything the draft needs but did not
receive is regenerable — run the install or build inside the draft.

Because those trees were never copied, Vis never reads their absence from the
draft as a deletion: `/draft apply` leaves your real `node_modules`, build
output, and ignored local files (`.env` and friends) exactly where they are.

A label is required. Vis may add a numeric suffix when the corresponding draft
directory name already exists; use the displayed label when resuming it.

### `/draft blank <label>`

Creates and enters an empty draft. No files from the real root are copied in.
This is useful for a spike, a fresh module, or generated output that should not
see the existing tree.

A blank draft has one special apply rule: because it never saw the original
files, it can add or overwrite files on trunk, but it cannot infer that an
absent trunk file was deleted. Applying a blank draft therefore does not delete
pre-existing files merely because they are absent from the draft.

### `/draft clean <label>`

Creates and enters a draft seeded from the **last commit**. Vis forks the tree as
usual and then rewinds the copy to `HEAD`: every committed file is present at its
committed content, and your uncommitted work — modified tracked files and new
untracked ones — stays in your real project and is never copied in. As with
`/draft new`, ignored and regenerable trees are not copied either, so a clean
draft may have to regenerate its dependencies or build output.

The repository must already have a commit; otherwise the command reports that
there is nothing to start a clean draft from. The paths the seed left out are
recorded, so applying the draft later never deletes the uncommitted files it
never received.

### Before the first command

A newly created session needs a persisted session state before slash handlers can
pin a draft. If Vis says `session not ready yet`, send one normal message and
retry.

## Working inside a draft

Once entered, file tools, searches, shell commands, and the agent's cwd resolve
to the draft root. Relative paths cannot silently fall back to trunk.

Configured `workspace.filesystem` catalog entries remain available after `/reload`.
By default only the active workspace root is forked, so those extra roots are
**shared**: the draft reads and writes the real directory. Give a root a `draft`
policy in `vis.yml` when that is not what you want:

| `draft` | Inside a draft |
| --- | --- |
| `shared` (default) | The draft works on the real root directly. |
| `copy-only` | The root is forked per draft; `/draft apply` never lands the copy back. |
| `copy-and-apply` | The root is forked per draft, and apply lands the copy into the real root. |
| `not-allowed` | The root is withheld from a drafted session on read and write. |

```yaml
# vis.yml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
      draft: copy-and-apply
    - id: production-notes
      path: ~/production-notes
      draft: not-allowed
```

The policy is enforced by Vis's own path guard, so it holds whether or not the
process jail is enabled, and a copy policy whose fork fails withholds the root
instead of quietly writing through to the real tree. `/draft abandon` releases
the extra clones with the draft. The session's own project root is always forked
and always landed by apply; `draft` only governs the *other* catalog roots.

Use bare `/draft` at any time to check whether the session is on trunk or in a
draft.

## Leaving a draft

### Apply: keep the changes

`/draft apply` copies files changed since the draft baseline, plus inferred
deletions, into the corresponding real directories. It then consumes the draft
and returns the session to trunk. The landed files remain **uncommitted** for you
to review and commit normally.

> **Apply is a replacement operation, not a conflict-resolving merge.** If the
> same real file changed after the draft was created, apply can overwrite it; an
> inferred deletion can remove it. Review concurrent trunk work before applying.

Isolation protects trunk **until apply**. It cannot undo an apply afterward; use
Git or another backup if you need that recovery boundary.

### Stash: park the changes

`/draft stash` returns the session to trunk without changing the draft's files or
active state. The clone remains available to `/draft list` and `/draft resume`.
Use this to pause work, inspect trunk, or move to another draft.

Closing a client view, disconnecting, or restarting the gateway does not turn a
stash into an abandon. The gateway persists the record and the working copy on
the host. A draft remains active until it is applied or abandoned.

### Abandon: destroy the changes

`/draft abandon [reason]` marks the draft discarded and returns to trunk. Clone
reclamation happens asynchronously, but the decision is immediate: the draft
cannot be resumed. The optional reason is retained in history.

There is no restore command for an abandoned draft. If there is any doubt, use
`/draft stash` instead.

## List, resume, and switch

`/draft list` shows active drafts for the current repo, newest first, and marks
the current one. Applied and abandoned drafts are intentionally absent.

`/draft resume <label>` matches the displayed label exactly. With no label it
lists the available choices. The slash command refuses to resume while the
session is already inside a draft; stash, apply, or abandon first.

### TUI draft picker

Press **`C-x e`** or choose **Switch Draft…** from the command palette to open
the large, searchable draft manager. It reads and mutates the canonical gateway
state; no draft list exists only in the TUI.

Trunk is pinned in its own section above the draft list instead of being mixed in
as if it were another draft. A smaller italic line explains that trunk is your real
repository and that switching there safely stashes the current draft. Each draft
also gets a secondary line showing whether it is active or parked and where its
isolated copy lives. While filtering, only matching locations remain; Trunk keeps
its separate section when it matches and otherwise gets out of the way.

The current location is selected by default, so opening the manager and pressing
Enter is a safe no-op. The controls follow the other searchable TUI pickers:

- **Enter** — switch to Trunk or the selected draft. Selecting Trunk stashes the
  current draft; selecting a parked draft makes the gateway stash-then-resume;
- **`C-n`** — name, create, and enter a new isolated draft. If another draft is
  current, the gateway stashes it first;
- **`C-d`** — abandon the selected current or parked draft after an explicit
  destructive confirmation; and
- **typing / Backspace / ↑ / ↓** — filter and navigate with the same
  case-insensitive substring behavior as the command palette and other pickers.
  Plain letters, including uppercase `N` and `D`, always belong to the query.

Apply remains an explicit `/draft apply` command because landing files into trunk
has a wider effect than picker management. The manager refuses every workspace
mutation while the current session has a running or queued turn, so an agent
cannot change roots mid-turn.

Resume also refuses a draft that:

- is no longer active;
- is not a draft workspace;
- belongs to another repository;
- is currently pinned to another session; or
- cannot be identified uniquely by the supplied label.

The direct gateway resume API—and therefore the TUI picker—accepts the stable
`workspace_id` and automatically stashes the caller's current draft before
switching. The slash command stays conservative and requires the two explicit
steps shown above.

## Gateway ownership and persistence

Drafts are a canonical gateway feature, not TUI-local state. The daemon owns the
workspace records, coordinates the on-disk clones, and executes workspace
mutations. Consequently:

- TUI and web clients attached to the same session see the same current workspace;
- sessions rooted in the same repo see the same active draft list;
- a client reconnect does not lose a draft; and
- only one session can pin a particular draft at a time.

Production draft storage defaults to `~/.vis/drafts` (overridable with the
`vis.drafts.dir` JVM system property). Treat that directory as Vis-managed data:
use apply, stash, and abandon rather than moving clone directories manually.

The slash UI and HTTP API are two control surfaces over the same gateway-owned
workspace state. The HTTP surface exposes list, create, stash, resume, and
abandon; apply remains an explicit slash command.

## Provider and privacy boundary

**Recognized draft control commands are local control-plane operations.** Vis
executes `/draft …` without an LLM call, charges no model
tokens for the command, and does not send the command text or result to a
provider—either immediately or as later provider context. They remain visible in
the local transcript for audit. The TUI suppresses the model/provider footer on
their result because no model produced it.

This guarantee applies to the **control command**, not to work performed in the
draft. Ordinary prompts and agent turns inside a draft use the selected provider
normally and may include repository content needed for that work. A draft is a
filesystem-isolation boundary, not a provider-privacy boundary.

Registered slash commands are local. An unknown `/something` is also returned as
a local unknown-command result unless it matches a prompt template; a matching
template runs as a normal provider turn. Absolute paths and other prose beginning
with `/` are not necessarily slash commands. Do not treat an arbitrary leading
slash as a privacy boundary.

If a recognized draft command itself displays a provider/model footer, that is a
channel rendering bug. It is not evidence that the command invoked the model.

## Requirements

Drafts require a workspace backend that can provide isolation, rollback,
merge-back, and retained revisions for the current root. Vis never silently
falls back to editing trunk when those capabilities are unavailable.

The bundled `workspace-rift` backend clones **copy-on-write**, so the filesystem
under the workspace decides whether drafts work at all. There is no draft switch
in `vis.yml`: a copy-on-write filesystem *is* the configuration.

| Platform | What a draft clone needs |
| --- | --- |
| macOS | APFS (`clonefile`). The default system volume qualifies. |
| Linux, including WSL2 | **btrfs.** Creating a draft turns the source directory into a btrfs subvolume and snapshots it. ext4, xfs, and zfs cannot clone, so drafts are unavailable on them. A btrfs mount over one subdirectory is enough — the whole disk need not be btrfs. |
| A Windows drive seen from WSL2 (`/mnt/c`, drvfs/9p) | Nothing to clone — keep the repository inside the Linux filesystem. |
| Native Windows | No copy-on-write workspace backend. |

**Two trees must qualify, not just the repository:**

- the **workspace root** the session works in, and
- the **draft store** — `~/.vis/drafts` by default, overridable with the
  `vis.drafts.dir` JVM system property on the gateway.

On Linux/WSL2 the practical setup is **one btrfs filesystem holding both — and it
may be mounted over a single directory.** Converting the whole disk is never
required: `mkfs.btrfs` a loopback image (WSL2 alternative: `wsl --mount --vhd
<file.vhdx>` a btrfs-formatted disk), mount it at something like `/srv/vis`, and
keep the repository *and* the draft store inside that one mount.

```bash
# once, as root: a btrfs image mounted over a single directory
fallocate -l 60G /var/lib/vis-btrfs.img
mkfs.btrfs /var/lib/vis-btrfs.img
mkdir -p /srv/vis
mount -o loop,compress=zstd /var/lib/vis-btrfs.img /srv/vis   # persist via /etc/fstab
mkdir -p /srv/vis/code /srv/vis/drafts
git clone <repo> /srv/vis/code/<repo>          # repository on the btrfs mount
vis-agent -J-Dvis.drafts.dir=/srv/vis/drafts   # store on the SAME mount
```

**Two btrfs mounts are not one filesystem.** A clone never crosses a filesystem
boundary, so a btrfs repository plus an ext4 `~/.vis/drafts` — or a second
loopback image — still leaves drafts unavailable. Everything else on the machine
can stay ext4.

Vis does not guess from the mount table: the backend probes the actual pair of
directories by cloning a throwaway directory from the root into the store, so
what it reports for your layout is measured, not assumed.

The Linux sandbox packages (`bubblewrap`, `passt`) are a **separate** concern:
they confine model-spawned child processes and neither enable nor block drafts.
See [Process sandbox and gateway egress](sandbox.md).

A **linked Git worktree** (a `.git` *file* pointing at another directory) is also
not forkable; run the session from the main worktree.

If creation fails with `No workspace backend can create an isolated draft here`,
the session remains on trunk and the command result carries the backend
capability matrix plus a platform-specific hint naming the filesystem it needs.
Fix that, then retry.

## Troubleshooting

| Message or symptom | Meaning | What to do |
| --- | --- | --- |
| `Already in draft '…'` | The session already has an isolated current workspace. | Apply, stash, or abandon it first. |
| `No stashed drafts to resume` | No active draft exists for the current repo. | Create one with `/draft new <label>`. |
| `No stashed draft named '…'` | Labels match exactly and the draft may be under another repo root. | Run `/draft list` and copy the displayed label. |
| `Draft is in use by another session` | Another session is pinned to that workspace. | Leave it from that session before resuming here. |
| A draft vanished from the list | It was applied or abandoned, or the session now points at a different repo root. | Check the root and local transcript history. |
| Trunk changed while a draft was parked | Apply has no automatic conflict resolver. | Review both trees and reconcile before applying. |
| `No workspace backend can create an isolated draft here` | The workspace root or the draft store is not on a copy-on-write filesystem, or the source is a linked Git worktree. | See [Requirements](#requirements): APFS on macOS, btrfs on Linux/WSL2 — for the root **and** `~/.vis/drafts`. |

## Command reference

| Command | Valid from | Effect |
| --- | --- | --- |
| `/draft` | trunk or draft | Show the current workspace state. |
| `/draft new <label>` | trunk | Fork the current tree (uncommitted changes included), enter the new draft. |
| `/draft clean <label>` | trunk | Fork, rewind the copy to the last commit, enter it — uncommitted work stays in the repo. |
| `/draft blank <label>` | trunk | Create an empty draft and enter it. |
| `/draft apply` | draft | Land changes into real directories, consume draft, return to trunk. |
| `/draft stash` | draft | Keep the draft active, return to trunk. |
| `/draft list` | trunk or draft | List active drafts for the current repo. |
| `/draft resume <label>` | trunk | Enter an active draft by displayed label. |
| `/draft abandon [reason]` | draft | Permanently discard the draft, return to trunk. |

## Gateway HTTP API

All routes are session-scoped and use the gateway's normal authentication rules.
They return canonical JSON with string keys.

| Method + path | Request | Result |
| --- | --- | --- |
| `GET /v1/sessions/:sid/workspace/drafts` | — | `{"drafts": [...]}` for the session's current repo. |
| `POST /v1/sessions/:sid/workspace/drafts` | `{"label":"…","blank":false,"clean":false}` | Stash any current draft, create an isolated draft from trunk, and enter it. `clean` seeds from the committed HEAD; `blank` seeds nothing. |
| `DELETE /v1/sessions/:sid/workspace/drafts/:workspace-id` | `{"reason":"…"}` | Permanently abandon a current or parked draft; rejects drafts from another repo or in use elsewhere. |
| `POST /v1/sessions/:sid/workspace/stash` | `{}` | Park the current draft; on trunk this is an idempotent no-op. |
| `POST /v1/sessions/:sid/workspace/resume` | `{"workspace_id":"…"}` | Stash any current draft, then enter the target. |

A draft list item has this shape:

```json
{
  "workspace_id": "…",
  "label": "safer-parser",
  "root": "/host/path/to/the/isolated/copy",
  "repo_root": "/host/path/to/the/real/repo",
  "fork_ms": 1784620000000,
  "is_current": false
}
```

Resume and abandon return HTTP `409` with a typed error when the target is not an
active draft in the session's current repo—for example, after apply/abandon or
while another session pins it. The gateway client exposes the corresponding
`list-drafts`, `create-draft!`, `stash-draft!`, `resume-draft!`, and
`abandon-draft!` operations.
