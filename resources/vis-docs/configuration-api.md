# Configuration API

Read and change Vis settings from your own program. The program changes the same settings as the
Settings views, for the gateway, a project, a group or one session.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A setup script must turn on features for a new project.** [Change one
  setting](#change-one-setting) for that project.
- **Several settings must change together or not at all.** [Change many
  settings](#change-many-settings) in one batch with a revision check.
- **You must know which value applies to a session, and why.** [Read settings](#read-settings) with
  a context session.
- **You edited an extension and the gateway must load the new code.** [Reload
  extensions](#reload-extensions).

To learn the settings and the configuration files, read [Configuration](configuration.md). To send
the same requests from another language, select **HTTP** at the top of the page.

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples are calls on that `client`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

</div>

## Operations

<div data-variant="python">

`GatewayClient` has one method for each settings route.

| Task | Method |
|---|---|
| Read the settings of a target | `get_settings(query=...)` |
| Read one setting | `get_setting(id, query=...)` |
| Change one setting | `post_settings(body=...)` |
| Change many settings at once | `patch_settings(body=...)` |
| Reload extensions | `post_extensions_reload(body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the settings of a target | `GET /v1/settings` |
| Read one setting | `GET /v1/settings/{id}` |
| Change one setting | `POST /v1/settings` |
| Change many settings at once | `PATCH /v1/settings` |
| Reload extensions | `POST /v1/extensions/reload` |

</div>

## How settings requests work

<div data-variant="python">

The methods send and receive the JSON of the HTTP routes. You give a body or a query as a Python
`dict`. To read the scopes, the catalog fields and the status codes, select **HTTP** at the top of
the page. A failed request raises `GatewayError` with that status.

</div>

<div data-variant="http">

Your program reads and changes the same settings as the Settings views. The Python SDK sends the
same JSON and gets the same answers.

A request names its target with `scope` and `target_id`. The `scope` is `global`, `project`, `group`
or `session`. Without a scope, a request uses the global settings, never the session that you see.
A scope other than `global` needs a `target_id`.

The catalog of a target has its `revision` and its `groups`. Each group has `toggles`, the setting
rows. A row has `scopes`, `scope`, `source` and `is_override`. A boolean row has `enabled`, and
other rows have `value`. The `revision` is a hash of the saved values of the target.

A row can have `children`: the rows that belong under it, in display order. A child row has
`parent` with the `id` of its parent row. Rows nest at any depth. For example, the Council room
rows are children of the `council` row. When you read or write one setting, its row has no
`children`. Keep the cached children when you replace a row.

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

</div>

## Read settings

<div data-variant="python">

```python
catalog = client.get_settings(query={"scope": "session", "target_id": session_id})
def show(rows, depth=0):
    for row in rows:
        value = row.get("enabled", row.get("value"))
        print("  " * depth + row["id"], value, row["source"])
        show(row.get("children", []), depth + 1)

for group in catalog["groups"]:
    show(group["toggles"])

print(client.get_setting("agent_name")["value"])
```

`get_session(session_id)` also returns the resolved `agent_name` of a session.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/settings?scope=session&target_id=$SESSION_ID"
vis_api "$VIS_GATEWAY_URL/v1/settings/agent_name"
```

`GET /v1/sessions/{sid}` also returns the resolved `agent_name` of a session.

</div>

## Change one setting

<div data-variant="python">

```python
client.post_settings(body={"id": "agent_name", "action": "value", "value": "Ada"})
client.post_settings(
    body={"scope": "session", "target_id": session_id, "id": "refusal_fallback", "action": "toggle"}
)
```

The `action` is `value`, `toggle`, `cycle` or `inherit`. A `value` action needs `value`, also when
the value is `False`. `inherit` removes one key at this scope, not a parent map. The answer is the
changed row.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"id": "agent_name", "action": "value", "value": "Ada"}'
```

The `action` is `value`, `toggle`, `cycle` or `inherit`. A `value` action needs `value`, also when
the value is `false`. `inherit` removes one key at this scope, not a parent map. The answer is the
changed row.

</div>

## Change many settings

<div data-variant="python">

```python
catalog = client.get_settings(query={"scope": "session", "target_id": session_id})
client.patch_settings(
    body={
        "scope": "session",
        "target_id": session_id,
        "revision": catalog["revision"],
        "changes": [
            {"id": "refusal_fallback", "action": "value", "value": False},
            {"id": "provider_fallback", "action": "inherit"},
        ],
    }
)
```

</div>

<div data-variant="http">

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

</div>

A batch has 1 to 256 changes for one target. Each change has a unique `id` and the action `value` or
`inherit`. Vis applies the whole batch or none of it. Optional `channel` and `context_session_id`
work as in a read. The answer is the full catalog with its new `revision`.

## Reload extensions

<div data-variant="python">

```python
result = client.post_extensions_reload(body={"scope": "project", "target_id": project_id})
print(result["loaded"], result["failed"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/extensions/reload" -H 'content-type: application/json' \
  --data '{"scope": "global"}'
```

</div>

Vis runs the extension files of the target again. A global target reloads machine extensions only.
Reading the catalog never runs extension code. An older gateway without this route answers 404.

## See also

- [Configuration](configuration.md) — every setting, configuration file and scope.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
