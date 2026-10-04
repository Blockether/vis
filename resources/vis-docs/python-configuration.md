# Configuration in Python

Read and change Vis settings from a Python program. The methods change the same settings as the
Settings views, for the gateway, a project, a group or one session.

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
the same requests from another language, read [Configuration over HTTP](http-configuration.md).

## Before you start

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples are calls on that `client`.

## Operations

`GatewayClient` has one method for each settings route.

| Task | Method |
|---|---|
| Read the settings of a target | `get_settings(query=...)` |
| Read one setting | `get_setting(id, query=...)` |
| Change one setting | `post_settings(body=...)` |
| Change many settings at once | `patch_settings(body=...)` |
| Reload extensions | `post_extensions_reload(body=...)` |

## How settings requests work

The methods send and receive the JSON of the HTTP routes. You give a body or a query as a Python
`dict`. [How settings requests work](http-configuration.md#how-settings-requests-work) explains the
scopes, the catalog fields and the status codes. A failed request raises `GatewayError` with that
status.

## Read settings

```python
catalog = client.get_settings(query={"scope": "session", "target_id": session_id})
for group in catalog["groups"]:
    for row in group["toggles"]:
        print(row["id"], row.get("enabled", row.get("value")), row["source"])

print(client.get_setting("agent_name")["value"])
```

`get_session(session_id)` also returns the resolved `agent_name` of a session.

## Change one setting

```python
client.post_settings(body={"id": "agent_name", "action": "value", "value": "Ada"})
client.post_settings(
    body={"scope": "session", "target_id": session_id, "id": "refusal_fallback", "action": "toggle"}
)
```

The `action` is `value`, `toggle`, `cycle` or `inherit`. A `value` action needs `value`, also when
the value is `False`. `inherit` removes one key at this scope, not a parent map. The answer is the
changed row.

## Change many settings

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

A batch has 1 to 256 changes for one target. Each change has a unique `id` and the action `value` or
`inherit`. Vis applies the whole batch or none of it. Optional `channel` and `context_session_id`
work as in a read. The answer is the full catalog with its new `revision`.

## Reload extensions

```python
result = client.post_extensions_reload(body={"scope": "project", "target_id": project_id})
print(result["loaded"], result["failed"])
```

Vis runs the extension files of the target again. A global target reloads machine extensions only.
Reading the catalog never runs extension code. An older gateway without this route answers 404.

## See also

- [Configuration](configuration.md) — every setting, configuration file and scope.
- [Configuration over HTTP](http-configuration.md) — the same operations as HTTP requests.
- [Python SDK basics](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
