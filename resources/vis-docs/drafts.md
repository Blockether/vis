# Drafts

A draft is an isolated working copy of the current repository. The session
works inside it. `draft_approve()` commits the work and merges it into the
repository's local default branch, updating that branch's checkout when one
exists.

Only the agent manages drafts. There is no draft slash command, picker or menu
in the TUI or Companion app.

## Sandbox tools

| Tool | Effect |
|---|---|
| `draft_create("name")` | Open a draft and move the session into it. Pending trunk changes come along; `draft_create("name", clean=True)` seeds from `HEAD` and leaves them behind. One draft at a time. |
| `draft_status()` | Report the draft branch, `target_branch`, draft commits the target lacks (`ahead`) and pending paths (`pending`). |
| `draft_approve()` | Commit pending work and merge the draft into the local default branch. `draft_approve("subject")` sets the commit subject. The draft stays open for further work. |
| `draft_discard()` | Return to the original checkout and remove the draft working copy. Approved work stays on the default branch; unapproved changes are lost. A merged draft branch may be removed. |

Inside the turn that opens or discards a draft, sandbox confinement changes at
once. `session["workspace"]` and `project_root_path` follow from the next block.
While in a draft, `session["workspace"]["draft"]` includes `label`, `backend`,
`branch`, `target_branch`, `approved_ahead` and `pending_paths`.

## Approval

The target is the local branch named by `origin/HEAD`. Without that symbolic
ref, Vis selects `main`, then `master`. If the target does not exist locally,
approval refuses rather than selecting an arbitrary branch. Vis does not
switch the branch in your original checkout or fetch from or push to a remote.

Approval stages changed and non-ignored untracked paths, excluding backend
bookkeeping, and commits them on `vis/<name>`. The subject defaults to
`draft(<name>): approve`; new commits include `Vis-Session` and `Vis-Draft`
trailers. Existing draft commits are merged even if no paths are pending.
The result includes `branch`, `target_branch`, `commit` and `files`.
`nothing-to-approve` means the target already contains the draft and there are
no pending changes.

When the target has advanced independently, Vis merges its history inside the
draft and creates a merge commit, then fast-forwards the target. Otherwise it
fast-forwards directly to the draft commit. A linked checkout of the target is
updated in place; an unchecked-out target is updated without changing the
original checkout.

The target checkout does not need to be clean. Approval preserves staged,
unstaged and untracked changes outside the paths being updated. Git refuses a
fast-forward that would overwrite local work, leaving the target unchanged and
the draft commit available for retry. Changes copied by `draft_create()` remain
in the original checkout and can still overlap the draft's updates. Resolve
those overlapping changes before retrying; unrelated changes can stay in place.
Vis does not stash, force-update or overwrite local changes automatically.

A merge conflict leaves the target unchanged and the draft commit intact;
Vis aborts its attempted merge in the draft. Resolve the conflicting changes
there and retry `draft_approve()`. Failed commits or extension vetoes are
reported as failures, not successful approvals. An existing merge, rebase,
cherry-pick or revert in the draft must be finished or aborted before approval.

## Backends

| Backend | How the draft is made | Where it lands |
|---|---|---|
| `worktree` | `git worktree add` on a new `vis/<name>` branch from `HEAD`; pending changes are applied as a patch | Commit on the shared draft branch, then merge into the local default branch. |
| `rift` | A copy-on-write clone of the directory (Rift) | Commit in the clone, fetch the draft branch into the original repository, then merge into its local default branch. |

`worktree` needs a git repository with at least one commit; `rift` works in any
directory but needs the Rift native library. The `draft_backend` toggle chooses
between them:

| Value | Meaning |
|---|---|
| `auto` (default) | `worktree` when the repository allows it, else `rift`. |
| `worktree`, `rift` | Only that backend; `draft_create` refuses when it is unavailable. |
| `off` | No drafts. `draft_create` explains why. |

Set it from the Settings dialog or in `~/.vis/config.yml`:

```yaml
toggles:
  draft_backend: worktree
```

Draft working copies live under `~/.vis/drafts/`. Additional roots follow their
own `draft` policy from the `filesystem_roots` configuration (see
[Configuration](configuration.md#jail-filesystem-and-network)).

## Hooks for extensions

Every create, approve and discard goes through the `draft/create`,
`draft/approve` and `draft/discard` operations, so a Python extension can guard
or observe them with `vis.OpHook`. A `before` hook returning `vis.block(reason)`
stops the operation. Both the draft commit and any merge commit also cross
`git/commit`; Git's own hooks are not bypassed. See
[Extending Vis](extending.md#op-hooks).

## See also

- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
