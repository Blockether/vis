# Drafts API

Turn on drafts, read the draft state of a session and control draft work from your own program. The
agent in the session still creates, approves and discards each draft.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

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

To learn how drafts isolate changes, read [Drafts](drafts.md). To send the same requests from
another language, select **HTTP** at the top of the page.

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

</div>

Drafts are experimental. Approval commits and can push, so your program must have the permission of
the person who owns the repository.

## Operations

<div data-variant="python">

Typed calls check the answers and return records. Generated methods return the JSON answer of one
request.

| Task | Typed call | Generated method |
|---|---|---|
| Turn on drafts | — | `post_settings(body=...)` |
| Read the draft state | — | `get_session_workspace(sid)` |
| Ask for work in a draft | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Approve or discard a draft | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Return to the original checkout | — | `patch_session_workspace_root(sid, body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Turn on drafts | `POST /v1/settings` |
| Read the draft state | `GET /v1/sessions/{sid}/workspace` |
| Ask for work in a draft | `POST /v1/sessions/{sid}/turns` |
| Approve or discard a draft | `POST /v1/sessions/{sid}/turns` |
| Return to the original checkout | `PATCH /v1/sessions/{sid}/workspace/root` |

</div>

## Turn on drafts

<div data-variant="python">

```python
client.post_settings(
    body={"scope": "project", "target_id": project_id, "id": "draft_backend", "action": "value", "value": "auto"}
)
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' --data "{\"scope\": \"project\",
  \"target_id\": \"$PROJECT_ID\", \"id\": \"draft_backend\", \"action\": \"value\", \"value\": \"auto\"}"
```

</div>

The `draft_backend` setting is `off`, `auto`, `worktree` or `rift`. [Enable drafts](drafts.md#enable-drafts)
explains each backend. [Configuration API](configuration-api.md#change-one-setting) explains
the scopes.

## Read the draft state

<div data-variant="python">

```python
workspace = client.get_session_workspace(session.id)["workspace"]
print(workspace["is_draft"], workspace["root"], workspace["repo_root"])
print(workspace.get("branch"), workspace.get("draft_changes"))
```

The answer has the `root` of the session, the `repo_root` of its project and the `git` state. In a
draft, it also has the draft `branch`, the `repositories`, the `draft_changes` and the
`working_changes`. These summaries refresh in the background. A summary that is not ready yet is
missing, never zero.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace"
```

The answer has `workspace` with the `root` of the session, the `repo_root` of its project, `is_draft`
and the `git` state. In a draft, it also has the draft `branch`, the `repositories`, the
`draft_changes` and the `working_changes`. These summaries refresh in the background. A summary that
is not ready yet is missing, never zero.

</div>

`recovery_required` is `true` when the session opened a directory of another draft. `draft_error`
then explains the problem.

## Ask for work in a draft

<div data-variant="python">

```python
turn = session.send("Fix the parser and run the tests. Do not commit or push until I approve.")
print(turn.wait(timeout=1800)["status"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Fix the parser and run the tests. Do not commit or push until I approve.", "idempotency_key": "parser-1"}'
```

</div>

With drafts on, the agent starts each change in a draft of its own. Your files stay unchanged.

## Approve or discard a draft

<div data-variant="python">

```python
session.send("Show me the diff of the draft.").wait(timeout=300)
session.send("Approve the draft.").wait(timeout=600)
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Approve the draft.", "idempotency_key": "parser-approve-1"}'
```

</div>

Approval commits the changes, moves the target branch forward and pushes when `origin` is
configured. To drop the changes, send `Discard the draft.` instead. Unapproved changes are then lost.
[Approval](drafts.md#approval) explains the checks before a commit.

## Return to the original checkout

<div data-variant="python">

```python
workspace = client.patch_session_workspace_root(session.id, body={"path": "/Users/me/projects/app"})
print(workspace["workspace"]["root"])
```

This call does the same as `/cd`. It moves the session to another directory and returns the new
workspace. The directory of the draft and its files stay unchanged.

</div>

<div data-variant="http">

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace/root" \
  -H 'content-type: application/json' --data '{"path": "/Users/me/projects/app"}'
```

This request does the same as `/cd`. It moves the session to another directory and returns the new
`workspace`. The directory of the draft and its files stay unchanged.

</div>

## See also

- [Drafts](drafts.md) — backends, approval and recovery.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
