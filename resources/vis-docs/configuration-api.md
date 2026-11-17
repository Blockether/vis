# Configuration API

Read and change Vis settings from your own program. A program changes the same settings as the
Settings views, for the gateway, a project, a group, or one session.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A setup script must turn on features for a new project.** [Change one
  setting](#change-one-setting) for that project.
- **Several settings must change together or not at all.** [Change many
  settings](#change-many-settings) in one batch with a revision check.
- **You must know which value applies to a session, and why.** [Read settings](#read-settings) with
  a context session.
- **A deployment script must give its gateway a name that people know.** [Name the
  machine](#name-the-machine).
- **Nobody must rename the agent that you serve under your own name.** [Keep one agent
  name](#keep-one-agent-name).
- **You edited an extension, and the gateway must load the new code.** [Reload
  extensions](#reload-extensions).

For the settings and the configuration files, read [Configuration](configuration.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task) shows. The examples call that `client`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

</div>

## Operations

<div data-variant="python">

| Task | Method |
|---|---|
| Read the settings of a target | `get_settings(query=...)` |
| Read one setting | `get_setting(id, query=...)` |
| Change one setting | `post_settings(body=...)` |
| Change many settings at once | `patch_settings(body=...)` |
| Read or save the machine name | `machine_name()`, `set_machine_name(name)` |
| Read, save, or lock the agent name | `agent_name()`, `set_agent_name(name, locked=...)` |
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

Each method sends and receives the JSON of its HTTP route. Give a body or a query as a Python
`dict`. A failed request raises `GatewayError` with the status of the answer. For the scopes, the
catalog fields, and the status codes, select **HTTP** at the top of the page.

</div>

<div data-variant="http">

A request names its target with `scope` and `target_id`. The `scope` is `global`, `project`,
`group`, or `session`, and each scope except `global` needs a `target_id`. Without a `scope`, a
request uses the global settings, never the session that you see.

The catalog of a target has `groups` and a `revision`, a hash of its saved values. Each group has
`toggles`, which are the setting rows. A row has `scopes`, `scope`, `source`, and `is_override`. A
boolean row has `enabled`, and each other row has `value`.

A row can have `children`, the rows that belong under it, in display order. A child row has
`parent` with the `id` of its parent row, and rows nest at any depth. The row of a one-setting read
or write has no `children`, so keep the cached children when you replace a row.

To mark the rows that a more specific scope decides for a session, add `context_session_id` to a
read. A marked row has `overridden_by` with the deciding `scope` and its `enabled` or `value`. An
unknown session, or a session outside the target, marks no rows. Vis accepts a write to a marked
row. Clients lock the row only for the open session.

An extension group has `extension` with `name`, `origin`, and `status`. The `origin` is `built_in`,
`global`, or `project`. The `status` is `loaded`, `stale`, or `failed`. A `global` or `project`
extension also has `path`, and a `stale` or `failed` extension has `error`. A failed extension that
never loaded has a group with no rows.

| Status | Meaning |
|---|---|
| `400` | A value, a scope, or a batch is not valid. A batch error names the setting `id`. Access errors also list `field_errors`. |
| `404` | The target, the context session, or the setting ID does not exist. |
| `409` | The `revision` of a batch is stale. Read the catalog again. A change to a fixed agent name also gets this status. |

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

`get_session(session_id)` also returns the resolved `agent_name` of a session. A client that
connects again reads the current name there.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/settings?scope=session&target_id=$SESSION_ID"
vis_api "$VIS_GATEWAY_URL/v1/settings/agent_name"
```

`GET /v1/sessions/{sid}` also returns the resolved `agent_name` of a session. A client that
connects again reads the current name there.

</div>

## Change one setting

<div data-variant="python">

```python
client.post_settings(body={"id": "agent_name", "action": "value", "value": "Ada"})
client.post_settings(
    body={"scope": "session", "target_id": session_id, "id": "refusal_fallback", "action": "toggle"}
)
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"id": "agent_name", "action": "value", "value": "Ada"}'
```

</div>

The `action` is `value`, `toggle`, `cycle`, or `inherit`. A `value` action needs `value`, also for
a false value. `inherit` removes one key at this scope, not a parent map. The answer is the changed
row.

The agent name is a `string` row in the `general` group. A name that breaks the [name
rules](configuration.md#coding-agent-name) gets status 400, and the saved name does not change.

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

Put the batch in `changes.json`, with the `revision` of your last read:

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

A batch holds 1 to 256 changes for one target, and Vis applies all of them or none. Each change has
a unique `id` and the action `value` or `inherit`. The optional `channel` and `context_session_id`
work as in a read. The answer is the full catalog with its new `revision`.

## Name the machine

<div data-variant="python">

```python
client.set_machine_name("Studio Mac")
print(client.machine_name())
client.set_machine_name(None)
```

`machine_name()` returns the name in effect, or `None` for a machine without a name.
`set_machine_name(None)` removes the saved name. Each call returns the name in effect after the
change. Both calls use `get_setting` and `post_settings` with the setting `machine_name`.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "machine_name", "action": "value", "value": "Studio Mac"}'
vis_api "$VIS_GATEWAY_URL/v1/settings/machine_name?scope=global"
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "machine_name", "action": "inherit"}'
```

The `value` of the answer is the name in effect, or an empty string for a machine without a name.
The `inherit` action removes the saved name. `GET /healthz` reports the name in effect as
`machine_name` and needs no token. The field is absent for a machine without a name.

</div>

When `VIS_MACHINE_NAME` is set on the gateway, that variable gives the name in effect. Read
[Machine name](configuration.md#machine-name).

## Keep one agent name

<div data-variant="python">

```python
client.set_agent_name("Acme", locked=True)
print(client.agent_name())
client.set_agent_name("Acme", locked=False)
```

`locked=True` saves the name and locks it. `locked=False` saves the name and removes the lock.
Without `locked`, the call is a rename, and the lock does not change. The gateway refuses the
rename of a locked name with `GatewayError`, status 409.

Each call returns the name in effect.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "agent_name", "action": "value", "value": "Acme", "is_locked": true}'
vis_api "$VIS_GATEWAY_URL/v1/settings/agent_name?scope=global"
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "agent_name", "action": "value", "value": "Acme", "is_locked": false}'
```

`"is_locked": true` saves the name and locks it. `"is_locked": false` saves the name and removes
the lock. Without `is_locked`, the request is a rename, and the lock does not change. The gateway
answers the rename of a locked name with 409 and the error type `setting-locked`.

The row of a fixed name has `lock_reason`, a sentence that tells a person why the name cannot
change.

</div>

When `VIS_AGENT_NAME` is set on the gateway, that variable gives the name in effect, and the
gateway refuses each change with 409. Read [One fixed name](configuration.md#one-fixed-name).

## Reload extensions

<div data-variant="python">

```python
result = client.post_extensions_reload(body={"scope": "project", "target_id": project_id})
for scope in result["scopes"]:
    print(scope["scope"], scope["dirs"], scope["loaded"], scope["failed"], scope["extensions"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/extensions/reload" -H 'content-type: application/json' \
  --data '{"scope": "global"}'
```

</div>

A global target reloads only machine extensions. A read of the catalog never runs extension code.

The answer has the totals `loaded` and `failed`, and a `scopes` list with one entry for each scanned
scope. An entry has `scope`, which is `global` or `project`, the scanned `dirs`, and `loaded` and
`failed` for that scope. Its `extensions` list names the loaded extensions.

## See also

- To learn each setting, configuration file, and scope, read [Configuration](configuration.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python
  SDK](python-sdk.md).
- For authentication, the OpenAPI document, and gateway errors, read [HTTP API](http-api.md).
