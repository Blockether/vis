# Drafts over HTTP

Turn on drafts, read the draft state of a session and control draft work with HTTP requests from any
language. The agent in the session still creates, approves and discards each draft.

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

To learn how drafts isolate changes, read [Drafts](drafts.md). To use typed calls from Python, read
[Drafts in Python](python-drafts.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

Drafts are experimental. Approval commits and can push, so your program must have the permission of
the person who owns the repository.

## Operations

The HTTP API has the same operations as the [Python SDK](python-drafts.md).

| Task | Method and path |
|---|---|
| Turn on drafts | `POST /v1/settings` |
| Read the draft state | `GET /v1/sessions/{sid}/workspace` |
| Ask for work in a draft | `POST /v1/sessions/{sid}/turns` |
| Approve or discard a draft | `POST /v1/sessions/{sid}/turns` |
| Return to the original checkout | `PATCH /v1/sessions/{sid}/workspace/root` |

## Turn on drafts

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' --data "{\"scope\": \"project\",
  \"target_id\": \"$PROJECT_ID\", \"id\": \"draft_backend\", \"action\": \"value\", \"value\": \"auto\"}"
```

The `draft_backend` setting is `off`, `auto`, `worktree` or `rift`. [Enable drafts](drafts.md#enable-drafts)
explains each backend. [Configuration over HTTP](http-configuration.md#change-one-setting) explains
the scopes.

## Read the draft state

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace"
```

The answer has `workspace` with the `root` of the session, the `repo_root` of its project, `is_draft`
and the `git` state. In a draft, it also has the draft `branch`, the `repositories`, the
`draft_changes` and the `working_changes`. These summaries refresh in the background. A summary that
is not ready yet is missing, never zero.

`recovery_required` is `true` when the session opened a directory of another draft. `draft_error`
then explains the problem.

## Ask for work in a draft

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Fix the parser and run the tests. Do not commit or push until I approve.", "idempotency_key": "parser-1"}'
```

With drafts on, the agent starts each change in a draft of its own. Your files stay unchanged.

## Approve or discard a draft

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Approve the draft.", "idempotency_key": "parser-approve-1"}'
```

Approval commits the changes, moves the target branch forward and pushes when `origin` is
configured. To drop the changes, send `Discard the draft.` instead. Unapproved changes are then lost.
[Approval](drafts.md#approval) explains the checks before a commit.

## Return to the original checkout

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace/root" \
  -H 'content-type: application/json' --data '{"path": "/Users/me/projects/app"}'
```

This request does the same as `/cd`. It moves the session to another directory and returns the new
`workspace`. The directory of the draft and its files stay unchanged.

## See also

- [Drafts](drafts.md) — backends, approval and recovery.
- [Drafts in Python](python-drafts.md) — the same operations as typed Python calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
