# Drafts

A draft gives a session its own isolated working copies of one or more repositories. Each copy is a
Git worktree or a copy-on-write clone, separate from your current checkout. **Drafts are
experimental and off by default.**

## When to use

- **You want Vis to try a change without editing the files you are working on.** Vis
  makes the change and runs its checks in its own copy. [Enable
  drafts](#enable-drafts) explains what that protection covers.
- **You want to read the whole diff before anything reaches your branch.** [Review
  the changes](#review-changes-before-approval), then [approve](#approval) them or
  ask Vis to discard the draft.
- **One change spans several repositories.** [Include each
  repository](#work-in-another-repository) in the draft without switching projects.
- **A session is working in the wrong draft.** Ask Vis to [return it to its original
  checkout](#recover-a-session-opened-in-the-wrong-draft).
- **Your team needs its own check before a draft is approved.** An extension can
  [guard approvals with a hook](#hooks-for-extensions).
- **Your own program must turn on drafts or read the draft state.** Use [Drafts API](drafts-api.md).

Read-only questions and analysis do not need a draft.

## Enable drafts

Open **Settings → Experimental → Draft backend** in the TUI or Companion app and
choose `auto`, `worktree` or `rift`. Choose `off` to disable automatic drafts and
new draft creation. Vis preserves any choice you have already saved.

When enabled, Vis starts each change-making task in a session-owned draft without a
separate request. This includes code, tests, documentation and configuration. Vis
keeps edits and checks in the draft. If no backend can create one, Vis reports the
blocker instead of editing your current checkout. Original project repositories stay
read-only to sandbox writers. Select every repository you need to change.

Draft write protection does not need the jail. While drafts are enabled, the file tools of Vis
refuse writes to the original repositories, with or without the jail. The jail adds OS-level
confinement for shell children and the Python sandbox. Without it, a raw shell command can still
reach your checkout. If you change the draft backend while a Python context is open, start a new
turn. Vis then rebuilds that context safely, and Python variables do not carry over to it.

## Ask Vis to work in a draft

> Fix the parser and run the tests, then show me the draft's diff.
> Do not commit or push until I approve.

The setting controls whether drafts are enabled. Only the agent creates, approves
and discards them. There is no draft-management menu or slash command in the TUI
or Companion app. You control those actions through the conversation.

**Approval commits and may push.** `draft_approve()` commits the draft's changes,
fast-forwards the default branch, restores unrelated local work and pushes to
`origin` when configured. Enabling drafts does not authorize commits or pushes:
Vis needs your request or permission in your project instructions. Ask to review
first to keep the draft unapproved. Approval leaves it open for further work.

When your task is complete, Vis checks the draft. It must have no pending changes, and all its
commits must be merged into the target branch. Any required push must have succeeded. Vis then
removes the completed draft without asking again. For your next task, Vis starts a fresh draft from
the latest `origin` target branch. With no remote configured, it starts from the committed local
target.

Discarding an unfinished draft can lose unapproved changes or unmerged commits,
so Vis asks for confirmation first. Drafts awaiting review or a required push stay
open. Work already merged stays on the target branch.

## Sandbox tools

| Tool | Effect |
|---|---|
| `draft_create("name", clean=None, roots=None)` | Open a draft from committed `HEAD`. To select repositories, pass a nonempty list of authorized root Paths. The first one is primary. Omit `roots` to use the project and configured copy policies. `clean=False` also copies pending source changes. One draft group at a time. |
| `draft_status()` | Report each repository, target branch, unpublished commits and pending paths, plus task-only review counts. |
| `draft_diff(...)` | Attach a reviewable diff of the draft's changes and return a checkpoint for the next task. No commit, approval or push. |
| `draft_sync(action="start", message=None, roots=None)` | Fetch and merge target history inside the draft. Resolve reported conflicts, then use `action="continue"`. To undo this synchronization, use `action="abort"`. It can create commits, so it needs commit permission. |
| `draft_approve()` | Commit pending work, fast-forward each default branch and publish to origin if configured. `draft_approve("subject")` sets the subject. The draft stays open. |
| `draft_discard()` | Return to the original checkout and remove the draft working copy. Approved work stays on the default branch. Unapproved changes are lost. A merged draft branch can be removed. |

Opening or discarding a draft updates the session's workspace immediately. Use
`session["workspace"]` and the Path bindings from the next block, when Python's
writable paths are refreshed.
While in a draft, `session["workspace"]["draft"]` includes `label`, `backend`,
`branch`, `target_branch`, `approved_ahead` and `pending_paths`.

The TUI footer shows the draft name and task-review `CHANGES`, plus `PENDING`
paths and `UNMERGED` commits for repositories included in approval. Approval
reduces the latter counts to zero once the changes land locally. It does not
reset the review baseline or close the draft. These counts do not confirm a
successful push. Discarding switches the footer back to the original checkout.
The footer refreshes in the background and at the end of a turn.

## Recover a session opened in the wrong draft

Ask Vis to check the draft state and return the session to its original checkout.
If the session inherited another draft's directory, `draft_status()` reports
`recovery_required`. `draft_discard()` then returns the session to the source
checkout without removing that draft or its files. The result names the
`preserved_root`.

If Vis cannot find the directory's ownership record, it will not guess what to
delete. Use `/cd /path/to/original-checkout` to return the session to a checkout
you recognize. The unrecognized directory and its contents stay untouched.

## Work in another repository

You can isolate a change across added repositories without switching projects or
changing their configured draft policies:

> Update the parser and its client together in a draft. Leave both original
> checkouts unchanged, and show me each repository's diff before approval.

When your configuration exposes `sibling_path`, select both repositories:

```python
print(draft_create("parser-and-client", roots=[project_root_path, sibling_path]))
```

The first entry becomes the primary repository: from the next block,
`project_root_path` points to its draft. Added-root Path variables follow their
working copies too. To work only in the sibling, use `roots=[sibling_path]`.
Omitting `roots` preserves the project's configured copy policies.

Every selected source must be an existing, authorized repository root that your session can read and
write. You can select a `shared` root without changing your configuration. You cannot select
read-only, `copy-only` or `not-allowed` roots, or roots that overlap filesystem deny rules. Vis
refuses empty lists, duplicates and nested selections. It does not guess. Roots that you do not
select keep their configured policies.

Draft review and approval do not include shared roots. A session has one active draft group at a
time. Approval needs authorization for all participating repositories. Discarding returns to the
original project.

## Review changes before approval

Ask Vis to attach the draft's diff, or to include a diff with each completed task's
implementation report. Open it in Companion or the TUI to read file changes and
comment on them. Sending a review round does not approve the draft or publish code.

A diff compares immutable snapshots. Vis takes the first snapshot after it creates the draft. So
changes copied with `clean=False` are part of the starting point, not new implementation work. Later
changes to the original checkout cannot change that baseline. Worktree and Rift drafts both use this
review path. It does not change the Git index, branches or commits of either working copy.

Copied source work is excluded from the task-only review, **not from approval**.
It remains in the draft and can overlap pending changes in the source checkout.
Approval refuses those overlaps. Copying is not permission to overwrite them.
Status distinguishes task review counts from pending Git changes.

By default the diff covers all changes since the draft began. A checkpoint lets the
next diff show only the following task, including later edits to the same files.
The final cumulative diff still uses the original starting point. File contents,
additions and deletions come from the draft. Comments are saved separately and
never rewrite the patch. Ignored untracked files and backend bookkeeping are not
review material.

### Diff tool reference

`draft_diff(filename=None, since=None)` attaches `DIFF-<draft-label>.json` for one
repository. Its result includes the attachment descriptor, a `checkpoint` snapshot
tree ID and an `empty` flag. With several repositories, it creates separate
`-repo-1.json`, `-repo-2.json` attachments. Each document identifies its source
repository, so identical file paths cannot be confused. The result includes an
`attachments` list and a `checkpoint` map keyed by source repository path.

Keep the complete checkpoint value for the next task. Use a different filename
for each task and one stable filename for revisions of that task's diff:

```python
first = draft_diff(filename="DIFF-search-task-1.json")
print(first)
# After completing the next task:
second = draft_diff(filename="DIFF-search-task-2.json", since=first["checkpoint"])
print(second)
# The whole implementation, still measured from the draft's starting point:
print(draft_diff(filename="DIFF-search.json"))
```

A checkpoint must have been recorded by this draft. Multi-repository checkpoints
must contain every member. Missing, foreign or invalid members are refused before
any attachments are created. Snapshot IDs are not Git commits.
A diff with no changes still records its checkpoint and clearly reports that it is
empty. The tool does not approve the draft, commit, push or run an implementation.

The snapshot store uses Git even for a Rift directory without Git history. It is
private to the draft and removed when the draft is discarded. A draft created
without a saved baseline cannot supply a trustworthy historical diff. Vis refuses
rather than using today's original checkout as yesterday's starting point.

## Approval

The target is the local branch named by `origin/HEAD`. Without that symbolic ref, Vis selects
`main`, then `master`. If the target does not exist locally, approval refuses. It does not pick
another branch. Vis never switches the branch in the original checkout.

If origin is configured, approval fetches the target from it. The draft must contain both the local
target and that fetched commit. If it does not, approval refuses with `:draft/sync-required`.

Then ask Vis to run `draft_sync()` with commit permission. It saves pending draft work and merges
the target history inside each draft. It does not approve or push. Approval itself never merges
target history.

If synchronization reports conflicts, edit the reported files in the affected
draft, then call `draft_sync(action="continue")`. To cancel that owned merge, use
`draft_sync(action="abort")`. An optional `roots=[...]` restricts synchronization
to selected participants. Vis refuses unrelated in-progress Git operations rather
than taking ownership of them. Do not bypass a refusal with raw Git recovery.

Pending draft work is committed on `vis/<name>`, excluding backend bookkeeping.
The subject defaults to `draft(<name>): approve`. Commits include `Vis-Session`
and `Vis-Draft` trailers. Local landing is fast-forward only. A linked target
checkout is updated in place. An unchecked-out branch is updated by compare-and-swap.

The target checkout can be dirty. Overlapping paths are conservatively refused
before any stash or landing, even when edits affect different lines in one file.
Unrelated staged, unstaged and untracked work is saved in an approval-owned stash,
then restored with `--index`. Existing user stashes are preserved. Ignored files
are not stashed and cannot be overwritten by landing.

Push to origin happens only after successful restoration, without force. With no
origin, approval is local only. The result includes `published`, `branch`,
`target_branch`, `commit` and `files`. `nothing-to-approve` means the draft and local
target already match with no pending changes. It still retries publication.

Publication across repositories is **not atomic**. Preflight checks cover all
participants, but a later commit, restoration or push can fail after an earlier
repository lands. Inspect the per-repository results: successfully landed history
is not rolled back, and the draft stays pinned for recovery and retry. Already
published repositories do not need new approval commits on retry.

If restoration fails, Vis does not push that repository and keeps its saved stash. Approval stays
blocked while recovery is needed. Keep the stash and follow the reported blocker. Do not delete the
stash to force a retry.

If the push fails after landing, Vis has already restored your local work. Synchronize if needed,
then retry approval. Normal Git push rules reject the push if the remote branch moved. Failed
commits and extension vetoes are failures, not successful approvals.

## Backends

| Backend | How the draft is made | Where it lands |
|---|---|---|
| `worktree` | `git worktree add` from `HEAD`. By default, pending work is not included. | Fast-forward the shared target, restore local work, then push to origin if configured. |
| `rift` | Copy-on-write clone, reset to committed `HEAD` by default | Fetch the draft branch into the original repository, then use the same approval flow. |

`worktree` needs a git repository with at least one commit. `rift` works in any
directory with `clean=False` but needs the Rift native library. Clean drafts
require Git history. The `draft_backend` toggle chooses between them:

| Value | Meaning |
|---|---|
| `auto` | Require drafts for changes. Use `worktree` when the repository allows it, else `rift`. |
| `worktree`, `rift` | Require drafts that use only that backend. `draft_create` refuses when the backend is unavailable. |
| `off` (default) | No automatic draft workflow or draft creation. `draft_create` explains why. |

Set it in **Settings → Experimental**, or in `~/.vis/config.yml`:

```yaml
toggles:
  draft_backend: worktree
```

Draft working copies live under `~/.vis/drafts/`. Other roots follow their own `draft` policy from
the `filesystem_roots` configuration. See
[Configuration](configuration.md#jail-filesystem-and-network). Copy-only dependency roots keep their
source bytes, even with `clean=True`. They are never approval targets. An explicit selection cannot
make a copy-only root part of the approval.

## Hooks for extensions

Every create, synchronization, approval and discard goes through `draft/create`,
`draft/sync`, `draft/approve` and `draft/discard`, so a Python extension can guard
or observe them with `vis.OpHook`. A `before` hook returning `vis.block(reason)`
stops the operation. Each approval-created draft commit also crosses
`git/commit`. Git's own hooks are not bypassed. See
[Extension API](extension-api.md#op-hooks).

## See also

- [Drafts API](drafts-api.md) — turn on drafts and read the draft state from your own program.
- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
