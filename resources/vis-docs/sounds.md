# Sounds

Vis can play a short sound when a turn finishes, when it waits for your answer and when a turn
fails or stops. Then you can work in another window and come back when Vis needs you.

## When to use

- **You do not want to watch a long turn.** [Turn on sounds](#turn-on-sounds) and wait for the
  finish sound.
- **You want to know at once when Vis asks you a question.** The question sound plays when a run
  [waits for your answer](#which-events-play-a-sound).
- **You prefer your own sounds, or fewer sounds.** [Use your own sound
  files](#use-your-own-sound-files), or turn one sound off.
- **You get no sound.** Read [Troubleshooting](#troubleshooting).

To change sounds from a program or an extension, read [Sounds API](sounds-api.md). For banners on
your phone or desktop, read [Configuration](configuration.md).

## Turn on sounds

Sounds are off by default.

1. Open the settings in the Companion app or the TUI.
2. Find the **Sounds** section.
3. Turn on **Sounds**.

The next turn that finishes plays the finish sound. Vis has three built-in sounds. They are short
interface sounds from the Kenney Interface Sounds pack, which has the CC0 license.

## Which events play a sound

| Sound | Setting | Plays when |
|---|---|---|
| Finish | **Finish sound** | A turn that you sent finishes. |
| Question | **Question sound** | A run stops and waits for your answer in a form. |
| Error | **Error sound** | A turn fails or stops, also when you stop it. |

Only your own turns play a sound. Subagent turns are silent, so a team of subagents does not play
a sound for each child. Council messages that wake the lead agent and automation runs are also
silent.

One sound plays at a time. When two sessions finish together, you hear one sound.

## Use your own sound files

Each sound setting takes one of these values:

| Value | Result |
|---|---|
| Empty | Vis plays its built-in sound. |
| `off` | Vis plays no sound for this event. |
| A file path, such as `~/Sounds/done.wav` | Vis plays this file. |

Use short WAV files. Every platform can play WAV. macOS can also play MP3, M4A and AIFF files.
A sound stops after 10 seconds.

You can also put the values in `~/.vis/state.yml`:

```yaml
toggles:
  sounds: true
  sound_finish: ~/Sounds/done.wav
  sound_input: ""
  sound_error: "off"
```

## Where the sound plays

The gateway plays the sound on its own computer. This is the computer that runs Vis. When you use a
gateway on a server, the sound plays on the server, not on your computer.

Vis uses the sound program of the computer:

| System | Program |
|---|---|
| macOS | `afplay` |
| Linux | `pw-play`, `paplay` or `aplay`, the first one that is installed |
| Windows | PowerShell with `Media.SoundPlayer`, for WAV files only |

## Troubleshooting

- **No sound plays.** Make sure that **Sounds** is on and that the sound setting is not `off`. On
  Linux, install PipeWire, PulseAudio or ALSA tools.
- **Your own file does not play.** Make sure that the path names a file on the gateway computer.
  Vis then writes a `Sound file not found` warning to the gateway log, which [Logging](logging.md)
  explains.
- **You hear the sound on the wrong computer.** The gateway plays it. Read [Where the sound
  plays](#where-the-sound-plays).

## See also

- [Sounds API](sounds-api.md) — read and change the sound settings from Python or with HTTP requests.
- [Configuration](configuration.md) — find all settings and the `state.yml` file.
- [Council](council.md) — subagents and the lead agent.
