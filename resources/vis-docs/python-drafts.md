# Drafts in Python

Turn on drafts, read the draft state of a session and control draft work from a Python program. The
agent in the session still creates, approves and discards each draft.

## When to use

- **A setup script must protect the checkouts of a project.** [Turn on drafts](#turn-on-drafts) for
  that project.
- **A dashboard must show whether a session works in a draft, and what changed.** [Read the draft
  state](#read-the-draft-state).
- **A batch job must make a change that a person reviews first.** [Ask for work in a
  draft](#ask-for-work-in-a-draft), then [approve or discard the
  draft](#approve-or-discard-a-draft) after the review.
- **A session works in the wrong directory.** [Return to the original
  checkout](#return-to-the-original-checkout).

To learn how drafts isolate changes, read [Drafts](drafts.md). To send the same requests from another
language, read [Drafts over HTTP](http-drafts.md).

## Before you start

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

Drafts are experimental. Approval commits and can push, so your program must have the permission of
the person who owns the repository.

## Operations

Typed calls check the answers and return records. Generated methods return the JSON answer of one
request. [Drafts over HTTP](http-drafts.md) lists the same operations as HTTP requests.

| Task | Typed call | Generated method |
|---|---|---|
| Turn on drafts | — | `post_settings(body=...)` |
| Read the draft state | — | `get_session_workspace(sid)` |
| Ask for work in a draft | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Approve or discard a draft | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Return to the original checkout | — | `patch_session_workspace_root(sid, body=...)` |

## Turn on drafts

```python
client.post_settings(
    body={"scope": "project", "target_id": project_id, "id": "draft_backend", "action": "value", "value": "auto"}
)
```

The `draft_backend` setting is `off`, `auto`, `worktree` or `rift`. [Enable drafts](drafts.md#enable-drafts)
explains each backend. [Configuration in Python](python-configuration.md#change-one-setting) explains
the scopes.

## Read the draft state

```python
workspace = client.get_session_workspace(session.id)["workspace"]
print(workspace["is_draft"], workspace["root"], workspace["repo_root"])
print(workspace.get("branch"), workspace.get("draft_changes"))
```

The answer has the `root` of the session, the `repo_root` of its project and the `git` state. In a
draft, it also has the draft `branch`, the `repositories`, the `draft_changes` and the
`working_changes`. These summaries refresh in the background. A summary that is not ready yet is
missing, never zero.

`recovery_required` is `true` when the session opened a directory of another draft. `draft_error`
then explains the problem.

## Ask for work in a draft

```python
turn = session.send("Fix the parser and run the tests. Do not commit or push until I approve.")
print(turn.wait(timeout=1800)["status"])
```

With drafts on, the agent starts each change in a draft of its own. Your files stay unchanged.

## Approve or discard a draft

```python
session.send("Show me the diff of the draft.").wait(timeout=300)
session.send("Approve the draft.").wait(timeout=600)
```

Approval commits the changes, moves the target branch forward and pushes when `origin` is
configured. To drop the changes, send `Discard the draft.` instead. Unapproved changes are then lost.
[Approval](drafts.md#approval) explains the checks before a commit.

## Return to the original checkout

```python
workspace = client.patch_session_workspace_root(session.id, body={"path": "/Users/me/projects/app"})
print(workspace["workspace"]["root"])
```

This call does the same as `/cd`. It moves the session to another directory and returns the new
workspace. The directory of the draft and its files stay unchanged.

## See also

- [Drafts](drafts.md) — backends, approval and recovery.
- [Drafts over HTTP](http-drafts.md) — the same operations as HTTP requests.
- [Python SDK basics](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
