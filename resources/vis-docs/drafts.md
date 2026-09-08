# Drafts

A draft is an isolated working copy of the current repository. The session
works inside it; the checkout you started from (the trunk) is not touched until
the work is approved. Approving a draft lands its work as a commit on a
`vis/<name>` branch that the trunk repository can see; discarding it removes
the working copy and leaves any approved commits on that branch.

Only the agent manages drafts. There is no slash command, picker or menu for
them in the TUI or the Companion app: you ask for isolated work, the agent opens
a draft, and you review the `vis/<name>` branch in your repository afterwards.

## Sandbox tools

| Tool | Effect |
|---|---|
| `draft_create("name")` | Open a draft and move the session into it. Pending trunk changes come along; `draft_create("name", clean=True)` seeds from `HEAD` and leaves them behind. One draft at a time. |
| `draft_status()` | Whether the session is in a draft and, if so, its backend, branch, approved commits the trunk lacks (`ahead`) and paths that still differ from the branch (`pending`). |
| `draft_approve()` | Land the draft as one commit on `vis/<name>`; `draft_approve("subject")` sets the subject. Every changed and untracked path is staged. The draft stays open, so later work can be approved again. |
| `draft_discard()` | Leave the draft and remove its working copy. Approved commits stay on the branch; unapproved changes are lost. |

Inside the turn that opens or discards a draft, the sandbox is confined to the
new root at once and `session["workspace"]` / `project_root_path` follow from
the next block on. `session["workspace"]["draft"]` is present while the session
is in a draft: `label`, `backend`, `branch`, `approved_ahead` and
`pending_paths`.

The commit subject defaults to `draft(<name>): approve`. Every approval commit
carries `Vis-Session: <session id>` and `Vis-Draft: <name>` trailers, so the
branch history names the session that produced it. Review, merge or delete the
branch from the trunk checkout as with any other branch.

## Backends

| Backend | How the draft is made | Where it lands |
|---|---|---|
| `worktree` | `git worktree add` on a new `vis/<name>` branch from `HEAD`; pending changes are applied as a patch | The branch is shared with the trunk repository. |
| `rift` | A copy-on-write clone of the directory (Rift) | The clone commits on `vis/<name>` and the trunk repository fetches that branch. |

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
or observe them with `vis.OpHook`. A `before` hook that returns `vis.block(reason)`
stops the operation; the agent sees the reason. See
[Extending Vis](extending.md#op-hooks).

## HTTP

`POST /v1/sessions/:sid/workspace/drafts/:workspace-id/approve` with an optional
JSON body `{"message": "..."}` approves one draft and answers
`{"approval": {...}, "workspace": {...}}`. The `vis-agent` Python client exposes
it as `post_session_workspace_draft_approve`. The workspace routes that list,
create, stash, resume and abandon drafts serve the same client; no Vis user
interface calls them.

## See also

- [Configuration](configuration.md) — the `draft_backend` toggle and the `draft` policy of extra roots.
- [Extending Vis](extending.md) — op hooks on `draft/create`, `draft/approve` and `draft/discard`.
