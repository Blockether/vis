# Drafts

A draft gives a session its own working copy of your repository, separate from
your current checkout. Ask Vis to use one when you want to review a change before
bringing it back to the default branch.

## Ask Vis to work in a draft

> Create a draft for the parser fix. Make the change and run the tests, then show
> me the diff before approving it.

Only the agent manages drafts; there is no draft menu or slash command in the
TUI or Companion app. You control the workflow through the conversation.

**Approval commits and may push.** `draft_approve()` commits the draft's changes,
fast-forwards the default branch, restores unrelated local work and pushes to
`origin` when configured. Ask to review first if you do not want those actions yet.
Approval leaves the draft open for further work.

Discarding a draft removes its unapproved changes and returns the session to the
original checkout. Work already approved stays on the default branch. Ask Vis to
confirm what will be lost before discarding.

## Sandbox tools

| Tool | Effect |
|---|---|
| `draft_create("name")` | Open a draft from committed `HEAD` and move the session into it. `clean=False` explicitly copies pending trunk changes; those may overlap on approval. One draft at a time. |
| `draft_status()` | Report the draft branch, `target_branch`, draft commits the target lacks (`ahead`) and pending paths (`pending`). |
| `draft_approve()` | Commit pending work, fast-forward the default branch and publish to origin if configured. `draft_approve("subject")` sets the subject. The draft stays open. |
| `draft_discard()` | Return to the original checkout and remove the draft working copy. Approved work stays on the default branch; unapproved changes are lost. A merged draft branch may be removed. |

Inside the turn that opens or discards a draft, sandbox confinement changes at
once. `session["workspace"]` and `project_root_path` follow from the next block.
While in a draft, `session["workspace"]["draft"]` includes `label`, `backend`,
`branch`, `target_branch`, `approved_ahead` and `pending_paths`.

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
stops the operation. Each approval-created draft commit also crosses
`git/commit`; Git's own hooks are not bypassed. See
[Extension API](extension-api.md#op-hooks).

## See also

- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
