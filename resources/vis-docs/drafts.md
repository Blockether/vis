# Drafts

A draft gives a session its own working copies of one or more repositories, separate from
your current checkout. **Drafts are off by default.** To enable them, open
**Settings → Sandbox → Draft backend** in the TUI or Companion app and choose
`auto`, `worktree` or `rift`. Choose `off` to disable automatic drafts and new
draft creation. Vis preserves any choice you have already saved.

When enabled, Vis starts each change-making task in a session-owned draft without
a separate request. This includes code, tests, documentation and configuration;
read-only questions and analysis do not need a draft. Vis keeps edits and checks
in the draft. If no backend can create one, Vis reports the blocker instead of
editing your current checkout. Original project repositories stay read-only to
sandbox writers; select every repository you need to change.

Draft write protection does not require the jail: while drafts are enabled, Vis'
file tools refuse writes to the original repositories whether or not the jail is
on. The jail adds OS-level confinement for shell children and the Python sandbox,
so with it off a raw shell command can still reach your checkout. If you change the draft
backend while a Python context is open, start a new turn so Vis can rebuild that
context safely. Python variables are not silently migrated across that boundary.

## Ask Vis to work in a draft

> Fix the parser and run the tests, then show me the draft's diff.
> Do not commit or push until I approve.

The setting controls whether drafts are enabled. Only the agent creates, approves
and discards them; there is no draft-management menu or slash command in the TUI
or Companion app. You control those actions through the conversation.

**Approval commits and may push.** `draft_approve()` commits the draft's changes,
fast-forwards the default branch, restores unrelated local work and pushes to
`origin` when configured. Enabling drafts does not authorize commits or pushes:
Vis needs your request or permission in your project instructions. Ask to review
first to keep the draft unapproved. Approval leaves it open for further work.

Once your task is complete, Vis checks that the draft has no pending changes, all
its commits are merged into the target branch, and any required push succeeded.
It then removes the completed draft without asking again. For your next task, Vis
starts a fresh draft from the latest `origin` target branch, or the committed
local target when no remote is configured.

Discarding an unfinished draft can lose unapproved changes or unmerged commits,
so Vis asks for confirmation first. Drafts awaiting review or a required push stay
open. Work already merged stays on the target branch.

## Sandbox tools

| Tool | Effect |
|---|---|
| `draft_create("name", clean=None, roots=None)` | Open a draft from committed `HEAD`. Select repositories with a nonempty list of authorized root Paths; the first is primary. Omit `roots` to use the project and configured copy policies. `clean=False` copies pending source changes too. One draft group at a time. |
| `draft_status()` | Report each repository, target branch, unpublished commits and pending paths, plus task-only review counts. |
| `draft_diff(...)` | Attach a reviewable diff of the draft's changes and return a checkpoint for the next task. No commit, approval or push. |
| `draft_sync(action="start", message=None, roots=None)` | Fetch and merge target history inside the draft. Resolve reported conflicts, then use `action="continue"`, or use `action="abort"` to undo the owned synchronization. May create commits; needs commit permission. |
| `draft_approve()` | Commit pending work, fast-forward each default branch and publish to origin if configured. `draft_approve("subject")` sets the subject. The draft stays open. |
| `draft_discard()` | Return to the original checkout and remove the draft working copy. Approved work stays on the default branch; unapproved changes are lost. A merged draft branch may be removed. |

Opening or discarding a draft updates the session's workspace immediately. Use
`session["workspace"]` and the Path bindings from the next block, when Python's
writable paths are refreshed.
While in a draft, `session["workspace"]["draft"]` includes `label`, `backend`,
`branch`, `target_branch`, `approved_ahead` and `pending_paths`.

The TUI footer shows the draft name and task-review `CHANGES`, plus `PENDING`
paths and `UNMERGED` commits for repositories included in approval. Approval
reduces the latter counts to zero once the changes land locally; it does not
reset the review baseline or close the draft. These counts do not confirm a
successful push. Discarding switches the footer back to the original checkout.
The footer refreshes in the background and at the end of a turn.

## Recover a session opened in the wrong draft

Ask Vis to check the draft state and return the session to its original checkout.
If the session inherited another draft's directory, `draft_status()` reports
`recovery_required`. `draft_discard()` then returns the session to the source
checkout without removing that draft or its files; the result names the
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

Every selected source must be an existing authorized read/write repository root
available to your session. A `shared` root can be selected without changing your
configuration. Read-only, `copy-only` and `not-allowed` roots cannot be selected,
nor can roots that overlap filesystem deny rules. Empty lists, duplicates and
nested selections are refused rather than guessed. Unselected roots retain their
configured policies; shared roots are not included in draft review or approval.
A session has one active draft group at a time. Approval needs authorization for
all participating repositories; discarding returns to the original project.

## Review changes before approval

Ask Vis to attach the draft's diff, or to include a diff with each completed task's
implementation report. Open it in Companion or the TUI to read file changes and
comment on them. Sending a review round does not approve the draft or publish code.

A diff compares immutable snapshots. The first snapshot is captured after the draft
is created, so changes copied with `clean=False` are part of its starting point,
not new implementation work. Later changes to the original checkout cannot alter
that baseline. Both worktree and Rift drafts use this review path; it does not
modify either working copy's Git index, branches or commits.

Copied source work is excluded from the task-only review, **not from approval**.
It remains in the draft and can overlap pending changes in the source checkout.
Approval refuses those overlaps; copying is not permission to overwrite them.
Status distinguishes task review counts from pending Git changes.

By default the diff covers all changes since the draft began. A checkpoint lets the
next diff show only the following task, including later edits to the same files.
The final cumulative diff still uses the original starting point. File contents,
additions and deletions come from the draft; comments are saved separately and
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
must contain every member; missing, foreign or invalid members are refused before
any attachments are created. Snapshot IDs are not Git commits.
A diff with no changes still records its checkpoint and clearly reports that it is
empty. The tool does not approve the draft, commit, push or run an implementation.

The snapshot store uses Git even for a Rift directory without Git history. It is
private to the draft and removed when the draft is discarded. A draft created
without a saved baseline cannot supply a trustworthy historical diff; Vis refuses
rather than using today's original checkout as yesterday's starting point.

## Approval

The target is the local branch named by `origin/HEAD`. Without that symbolic
ref, Vis selects `main`, then `master`. If the target does not exist locally,
approval refuses rather than selecting an arbitrary branch. Vis never switches
the branch in the original checkout.

Approval fetches the target from origin when configured. The draft must contain
both the local target and that fetched commit. Otherwise approval refuses with
`:draft/sync-required`. Ask Vis to run `draft_sync()` with commit permission. It
saves pending draft work and merges the target history inside each draft; it does
not approve or push. Approval itself never merges target history.

If synchronization reports conflicts, edit the reported files in the affected
draft, then call `draft_sync(action="continue")`. To cancel that owned merge, use
`draft_sync(action="abort")`. An optional `roots=[...]` restricts synchronization
to selected participants. Vis refuses unrelated in-progress Git operations rather
than taking ownership of them. Do not bypass a refusal with raw Git recovery.

Pending draft work is committed on `vis/<name>`, excluding backend bookkeeping.
The subject defaults to `draft(<name>): approve`; commits include `Vis-Session`
and `Vis-Draft` trailers. Local landing is fast-forward only. A linked target
checkout is updated in place; an unchecked-out branch is updated by compare-and-swap.

The target checkout can be dirty. Overlapping paths are conservatively refused
before any stash or landing, even when edits affect different lines in one file.
Unrelated staged, unstaged and untracked work is saved in an approval-owned stash,
then restored with `--index`. Existing user stashes are preserved. Ignored files
are not stashed and cannot be overwritten by landing.

Push to origin happens only after successful restoration, without force. With no
origin, approval is local only. The result includes `published`, `branch`,
`target_branch`, `commit` and `files`. `nothing-to-approve` means the draft and local
target already match with no pending changes; it still retries publication.

Publication across repositories is **not atomic**. Preflight checks cover all
participants, but a later commit, restoration or push can fail after an earlier
repository lands. Inspect the per-repository results: successfully landed history
is not rolled back, and the draft stays pinned for recovery and retry. Already
published repositories do not need new approval commits on retry.

If restoration fails, no push is attempted for that repository and its saved stash
is retained. Approval remains blocked while recovery is needed; preserve the stash
and follow the reported blocker rather than deleting it to force a retry. If push
fails after landing, local work has already been restored. Synchronize if needed,
then retry approval. Remote movement is rejected by normal Git push. Failed commits
and extension vetoes are failures, not successful approvals.

## Backends

| Backend | How the draft is made | Where it lands |
|---|---|---|
| `worktree` | `git worktree add` from `HEAD`; pending work is excluded by default | Fast-forward the shared target, restore local work, then push to origin if configured. |
| `rift` | Copy-on-write clone, reset to committed `HEAD` by default | Fetch the draft branch into the original repository, then use the same approval flow. |

`worktree` needs a git repository with at least one commit; `rift` works in any
directory with `clean=False` but needs the Rift native library. Clean drafts
require Git history. The `draft_backend` toggle chooses between them:

| Value | Meaning |
|---|---|
| `auto` | Require drafts for changes; use `worktree` when the repository allows it, else `rift`. |
| `worktree`, `rift` | Require drafts using only that backend; `draft_create` refuses when it is unavailable. |
| `off` (default) | No automatic draft workflow or draft creation. `draft_create` explains why. |

Set it from the Settings dialog or in `~/.vis/config.yml`:

```yaml
toggles:
  draft_backend: worktree
```

Draft working copies live under `~/.vis/drafts/`. Additional roots follow their
own `draft` policy from the `filesystem_roots` configuration (see
[Configuration](configuration.md#jail-filesystem-and-network)). Copy-only dependency
roots preserve source bytes even with `clean=True`; they are never approval targets.
Explicit selection cannot upgrade a copy-only root to an approving participant.

## Hooks for extensions

Every create, synchronization, approval and discard goes through `draft/create`,
`draft/sync`, `draft/approve` and `draft/discard`, so a Python extension can guard
or observe them with `vis.OpHook`. A `before` hook returning `vis.block(reason)`
stops the operation. Each approval-created draft commit also crosses
`git/commit`; Git's own hooks are not bypassed. See
[Extension API](extension-api.md#op-hooks).

## See also

- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
