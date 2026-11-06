# Sounds API

Read and change the sounds that the gateway plays. A sound plays when a turn finishes, when Vis
waits for your answer and when a turn fails or stops. The values are the settings of the **Sounds**
section in the Companion app and the TUI.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A setup script must turn on sounds.** [Change the sounds](#change-the-sounds) with one call.
- **An extension must use its own sound files.** Use `vis.sounds` in the extension, as in [Change
  the sounds](#change-the-sounds).
- **A tool must show the current sounds.** [Read the sounds](#read-the-sounds).

To learn which events play a sound and where it plays, read [Sounds](sounds.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client`. Inside an
extension, `vis.sounds` is the same API.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

</div>

## Operations

<div data-variant="python">

`client.sounds` returns a `Sounds` object. Its calls use the generated settings methods.

| Task | Typed call | Generated method |
|---|---|---|
| Read the sounds | `client.sounds.get()` | `get_setting(id)` |
| Change the sounds | `client.sounds.set(...)` | `post_settings(body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the sounds | `GET /v1/settings/{id}?scope=global` |
| Change the sounds | `POST /v1/settings` |

</div>

The settings are `sounds` (on or off), `sound_finish`, `sound_input` and `sound_error`. A sound
value is empty for the built-in sound, `off` for no sound, or a file path on the gateway computer.

## Read the sounds

<div data-variant="python">

```python
sounds = client.sounds.get()
print(sounds.is_enabled, sounds.finish, sounds.input, sounds.error)
```

`get()` returns a `SoundSettings` object. An empty cue value means the built-in sound.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/settings/sounds?scope=global"
vis_api "$VIS_GATEWAY_URL/v1/settings/sound_finish?scope=global"
```

The `sounds` answer has `enabled`. A cue answer has `value`, which is empty for the built-in sound.

</div>

## Change the sounds

<div data-variant="python">

```python
from pathlib import Path

sounds = client.sounds.set(
    enabled=True,
    finish=Path("~/Sounds/done.wav"),
    error="off",
)
print(sounds)
client.sounds.set(finish=None)
```

Arguments that you omit do not change. `None` gives a cue its built-in sound again. A path can start
with `~`, which the gateway expands on its own computer.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sounds", "action": "value", "value": true}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sound_finish", "action": "value", "value": "~/Sounds/done.wav"}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sound_finish", "action": "inherit"}'
```

The `inherit` action gives a cue its built-in sound again.

</div>

The gateway checks a path only when it plays the sound. A path that names no file plays nothing
and writes a warning to the gateway log.

## See also

- [Sounds](sounds.md) — which events play a sound, and where it plays.
- [Configuration API](configuration-api.md) — read and change all settings.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
