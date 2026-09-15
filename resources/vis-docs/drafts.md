# Drafts

A draft gives a session its own working copy of your repository, separate from
your current checkout. With `draft_backend` set to `auto` (the default),
`worktree` or `rift`, Vis starts each change-making task in a session-owned draft
without a separate request. This includes code, tests, documentation and
configuration; read-only questions and analysis do not need a draft.

Vis keeps edits and checks in the draft. If no backend can create one, Vis reports
the blocker instead of editing your current checkout. Set the toggle to `off` to
disable this workflow and draft creation.

## Ask Vis to work in a draft

> Fix the parser and run the tests, then show me the draft's diff.
> Do not commit or push until I approve.

Only the agent manages drafts; there is no draft menu or slash command in the
TUI or Companion app. You control the workflow through the conversation.

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
| `draft_create("name", root=None)` | Open a draft from committed `HEAD` of the current project or a selected read/write root. `clean=False` explicitly copies pending source changes; those may overlap on approval. One draft at a time. |
| `draft_status()` | Report the draft branch, `target_branch`, draft commits the target lacks (`ahead`) and pending paths (`pending`). |
| `draft_diff(...)` | Attach a reviewable diff of the draft's changes and return a checkpoint for the next task. No commit, approval or push. |
| `draft_approve()` | Commit pending work, fast-forward the default branch and publish to origin if configured. `draft_approve("subject")` sets the subject. The draft stays open. |
| `draft_discard()` | Return to the original checkout and remove the draft working copy. Approved work stays on the default branch; unapproved changes are lost. A merged draft branch may be removed. |

Inside the turn that opens or discards a draft, sandbox confinement changes at
once. `session["workspace"]` and `project_root_path` follow from the next block.
While in a draft, `session["workspace"]["draft"]` includes `label`, `backend`,
`branch`, `target_branch`, `approved_ahead` and `pending_paths`.

## Work in another repository

You can isolate a change in an added repository without switching projects or
changing its configured draft policy:

> Fix the parser in the sibling repository in its own draft. Leave this project's
> checkout unchanged, and show me the diff before approval.

Vis selects that repository with the `root` argument. For example, when your
configuration exposes `sibling_path`:

```python
print(draft_create("parser-fix", root=sibling_path))
```

From the next block, `project_root_path` points to the selected repository's draft.
Use it for edits and checks. `draft_diff()` reviews that draft, `draft_approve()`
lands in that repository's default branch, and `draft_discard()` returns the session
to its original project. The original project's commits and pending files stay
unchanged. Approval still needs authorization for the selected repository.

The source must be an existing read/write root available to your session. A
`shared` root can be selected without changing your configuration. Read-only,
`copy-only` and `not-allowed` roots cannot be selected, nor can roots that overlap
filesystem deny rules: making a writable copy must not bypass those restrictions.
Other roots keep their configured policies. Finish the current draft before
selecting another source; a session has one active draft at a time.

## Review changes before approval

Ask Vis to attach the draft's diff, or to include a diff with each completed task's
implementation report. Open it in Companion or the TUI to read file changes and
comment on them. Sending a review round does not approve the draft or publish code.

A diff compares immutable snapshots. The first snapshot is captured after the draft
is created, so changes copied with `clean=False` are part of its starting point,
not new implementation work. Later changes to the original checkout cannot alter
that baseline. Both worktree and Rift drafts use this review path; it does not
modify either working copy's Git index, branches or commits.

By default the diff covers all changes since the draft began. A checkpoint lets the
next diff show only the following task, including later edits to the same files.
The final cumulative diff still uses the original starting point. File contents,
additions and deletions come from the draft; comments are saved separately and
never rewrite the patch. Ignored untracked files and backend bookkeeping are not
review material.

### Diff tool reference

`draft_diff(filename=None, since=None)` attaches `DIFF-<draft-label>.json` by default.
Its result includes the attachment descriptor, a `checkpoint` snapshot tree ID and
an `empty` flag. Use a different filename for each task and keep one stable filename
for revisions of that task's diff:

```python
first = draft_diff(filename="DIFF-search-task-1.json")
print(first)
# After completing the next task:
second = draft_diff(filename="DIFF-search-task-2.json", since=first["checkpoint"])
print(second)
# The whole implementation, still measured from the draft's starting point:
print(draft_diff(filename="DIFF-search.json"))
```

A checkpoint must have been recorded by this draft. Snapshot IDs are not Git commits.
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
`:draft/sync-required`: merge or rebase onto the reported commit in the draft,
resolve conflicts, then retry. Approval itself never merges target history.
An existing merge, rebase, cherry-pick or revert must be finished or aborted.

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

If restoration fails, no push is attempted and the saved stash is retained.
Recover and verify local work manually, then drop that approval stash. Further
approval is blocked while an approval stash remains. If push fails, the error
explicitly reports that landing succeeded locally. Local work is already restored;
fetch, synchronize the draft if necessary and retry. A rejected push does not roll
back local history. Remote movement after fetch is rejected by normal Git push.
Failed commits and extension vetoes are failures, not successful approvals.

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
| `auto` (default) | Require drafts for changes; use `worktree` when the repository allows it, else `rift`. |
| `worktree`, `rift` | Require drafts using only that backend; `draft_create` refuses when it is unavailable. |
| `off` | No automatic draft workflow or draft creation. `draft_create` explains why. |

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
stops the operation. Each approval-created draft commit also crosses
`git/commit`; Git's own hooks are not bypassed. See
[Extension API](extension-api.md#op-hooks).

## See also

- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
