# Sounds API

This API reads and changes the sound settings of the gateway. The values are the settings of the
**Sounds** section in the Companion app and the TUI.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A setup script must turn on sounds.** [Turn on sounds and choose a
  preset](#turn-on-sounds-and-choose-a-preset) with one call.
- **An extension must use its own sound files.** [Save your own preset](#save-your-own-preset)
  with `vis.sounds` in the extension.
- **A tool must play a sound before a person saves it.** [Preview a sound](#preview-a-sound).
- **A tool must show the current sounds.** [Read the presets](#read-the-presets).

To learn which events play a sound and where the sound plays, read [Sounds](sounds.md).

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
| Read the presets | `client.sounds.get()` | `get_setting(id)` |
| Turn on sounds or choose a preset | `client.sounds.set(...)` | `post_settings(body=...)` |
| Save your own preset | `client.sounds.save_preset(...)` | `post_settings(body=...)` |
| Delete your own preset | `client.sounds.delete_preset(name)` | `post_settings(body=...)` |
| Preview a sound | `client.sounds.preview(slot, value)` | `post_setting_preview(id, body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the presets | `GET /v1/settings/{id}?scope=global` |
| Change sounds and presets | `POST /v1/settings` |
| Preview a sound | `POST /v1/settings/{id}/preview` |

</div>

A preset has one value for each slot: `finish`, `input` for the question sound, and `error`. A slot
value is one of these:

- An empty value, for no sound.
- The name of a built-in preset, for the sound of that preset. The built-in presets are `classic`,
  `glass`, `soft`, `minimal`, and `bright`.
- A file path on the gateway computer.

## Read the presets

<div data-variant="python">

```python
sounds = client.sounds.get()
print(sounds.is_enabled, sounds.active)
for preset in sounds.presets:
    print(preset.name, preset.is_builtin, preset.finish, preset.input, preset.error)
```

`get()` returns a `SoundSettings` object. `presets` lists the built-in presets, then your own.
`preset` is the active preset, and `custom` lists only your own presets.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/settings/sounds?scope=global"
vis_api "$VIS_GATEWAY_URL/v1/settings/sound_presets?scope=global"
```

The `sounds` answer has `enabled`. The `sound_presets` answer has `value`, with `active` and your
`custom` presets. Its `editor_options` list the slots and the built-in presets.

</div>

## Turn on sounds and choose a preset

<div data-variant="python">

```python
sounds = client.sounds.set(enabled=True, preset="glass")
print(sounds.active)
```

An argument that you omit changes nothing. A preset name matches in any case. An unknown name
raises `ValueError` and changes nothing.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sounds", "action": "value", "value": true}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sound_presets", "action": "value",
           "value": {"active": "glass", "custom": []}}'
```

The value replaces all presets. Send your `custom` presets again with each change.

</div>

## Save your own preset

<div data-variant="python">

```python
from pathlib import Path

sounds = client.sounds.save_preset(
    "My mix",
    finish="glass",
    input=Path("~/Sounds/question.wav"),
    error=None,
    activate=True,
)
print(sounds.preset)
client.sounds.delete_preset("My mix")
```

`save_preset()` replaces a preset that has the same name. `None` or an empty value plays no sound
in that slot. After you delete the active preset, the first built-in preset plays.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings" -H 'content-type: application/json' \
  --data '{"scope": "global", "id": "sound_presets", "action": "value",
           "value": {"active": "My mix", "custom": [{"name": "My mix",
             "slots": {"finish": "glass", "input": "~/Sounds/question.wav", "error": ""}}]}}'
```

</div>

The gateway checks each file path when you save. For a path that names no file, the gateway returns
an error with a message and saves nothing. The gateway expands a leading `~` on its own computer.

## Preview a sound

<div data-variant="python">

```python
played = client.sounds.preview("input", "glass")
print(played)
```

`preview()` plays one slot value on the gateway computer and saves nothing. It returns `False`
when the value is empty, when another sound plays, or when the computer has no sound program.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/settings/sound_presets/preview" \
  -H 'content-type: application/json' --data '{"slot": "input", "value": "glass"}'
```

The answer is `{"is_played": true}` when a sound starts.

</div>

## See also

- To learn which events play a sound and where the sound plays, read [Sounds](sounds.md).
- To read and change all settings, read [Configuration API](configuration-api.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python SDK](python-sdk.md).
