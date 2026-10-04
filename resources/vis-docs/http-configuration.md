# Configuration over HTTP

Read and change Vis settings with HTTP requests from any language. The routes change the same
settings as the Settings views, for the gateway, a project, a group or one session.

## When to use

- **A setup script must turn on features for a new project.** [Change one
  setting](#change-one-setting) for that project.
- **Several settings must change together or not at all.** [Change many
  settings](#change-many-settings) in one batch with a revision check.
- **You must know which value applies to a session, and why.** [Read settings](#read-settings) with
  a context session.
- **You edited an extension and the gateway must load the new code.** [Reload
  extensions](#reload-extensions).

To learn the settings and the configuration files, read [Configuration](configuration.md). To use
typed calls from Python, read [Configuration in Python](python-configuration.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

## Operations

| Task | Method and path |
|---|---|
| Read the settings of a target | `GET /v1/settings` |
| Read one setting | `GET /v1/settings/{id}` |
| Change one setting | `POST /v1/settings` |
| Change many settings at once | `PATCH /v1/settings` |
| Reload extensions | `POST /v1/extensions/reload` |

## How settings requests work

Your program reads and changes the same settings as the Settings views. The [Python
SDK](python-configuration.md) sends the same JSON and gets the same answers.

A request names its target with `scope` and `target_id`. The `scope` is `global`, `project`, `group`
or `session`. Without a scope, a request uses the global settings, never the session that you see.
A scope other than `global` needs a `target_id`.

The catalog of a target has its `revision` and its `groups`. Each group has `toggles`, the setting
rows. A row has `scopes`, `scope`, `source` and `is_override`. A boolean row has `enabled`, and
other rows have `value`. The `revision` is a hash of the saved values of the target.

Add `context_session_id` to a read to mark the rows that a more specific scope decides for that
session. Such a row has `overridden_by` with the deciding `scope` and its `enabled` or `value`. An
unknown session, or a session outside the target, marks no rows. Vis does not refuse a write to a
marked row, because the global value still applies to other projects. Clients lock the row only for
the open session.

An extension group in the catalog has `extension` with `name`, `origin` and `status`. The `origin`
is `built_in`, `global` or `project`, and the `status` is `loaded`, `stale` or `failed`. A `global`
or `project` extension also has `path`. A `stale` or `failed` extension has `error`. A failed
extension that never loaded has a group with no rows.

| Status | Meaning |
|---|---|
| `400` | A value, a scope or a batch is not valid. A batch error names the setting `id`. Access errors also list `field_errors`. |
| `404` | The target, the context session or the setting ID does not exist. |
| `409` | The `revision` of a batch is stale. Read the catalog again. |

## Read settings

```bash
vis_api "$VIS_GATEWAY_URL/v1/settings?scope=session&target_id=$SESSION_ID"
vis_api "$VIS_GATEWAY_URL/v1/settings/agent_name"
```

`GET /v1/sessions/{sid}` also returns the resolved `agent_name` of a session.

## Change one setting

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"id": "agent_name", "action": "value", "value": "Ada"}'
```

The `action` is `value`, `toggle`, `cycle` or `inherit`. A `value` action needs `value`, also when
the value is `false`. `inherit` removes one key at this scope, not a parent map. The answer is the
changed row.

## Change many settings

Save the batch as `changes.json`, with the `revision` from your last read:

```json
{
  "scope": "session",
  "target_id": "<session-id>",
  "revision": "<revision from GET>",
  "changes": [
    {"id": "refusal_fallback", "action": "value", "value": false},
    {"id": "provider_fallback", "action": "inherit"}
  ]
}
```

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data @changes.json
```

A batch has 1 to 256 changes for one target. Each change has a unique `id` and the action `value` or
`inherit`. Vis applies the whole batch or none of it. Optional `channel` and `context_session_id`
work as in a read. The answer is the full catalog with its new `revision`.

## Reload extensions

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/extensions/reload" -H 'content-type: application/json' \
  --data '{"scope": "global"}'
```

Vis runs the extension files of the target again. A global target reloads machine extensions only.
Reading the catalog never runs extension code. An older gateway without this route answers 404.

## See also

- [Configuration](configuration.md) — every setting, configuration file and scope.
- [Configuration in Python](python-configuration.md) — the same operations as typed Python calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
