# Sounds

Vis can play a short sound when a turn finishes, when it waits for your answer and when a turn
fails or stops. Then you can work in another window and come back when Vis needs you.

## When to use

- **You do not want to watch a long turn.** [Turn on sounds](#turn-on-sounds) and wait for the
  finish sound.
- **You want to know at once when Vis asks you a question.** The question sound plays when a run
  [waits for your answer](#which-events-play-a-sound).
- **You want different sounds.** [Choose a preset](#choose-a-preset) from the built-in presets.
- **You want your own sounds, or fewer sounds.** [Make your own preset](#make-your-own-preset).
- **You get no sound.** Read [Troubleshooting](#troubleshooting).

To change sounds from a program or an extension, read [Sounds API](sounds-api.md). For banners on
your phone or desktop, read [Configuration](configuration.md).

## Turn on sounds

Sounds are off by default.

1. Open the settings in the Companion app or the TUI.
2. Find the **Sounds** section.
3. Turn on **Sounds**.

The next turn that finishes plays the finish sound of the **Classic** preset.

## Which events play a sound

A preset gives one sound to each of three events:

| Sound | Plays when |
|---|---|
| Finish | A turn that you sent finishes. |
| Question | A run stops and waits for your answer in a form. |
| Error | A turn fails or stops, also when you stop it. |

Only your own turns play a sound. Subagent turns are silent, so a team of subagents does not play
a sound for each child. Council messages that wake the lead agent and automation runs are also
silent.

One sound plays at a time. When two sessions finish together, you hear one sound.

## Choose a preset

Vis has five built-in presets: **Classic**, **Glass**, **Soft**, **Minimal** and **Bright**. Each
one has its own finish, question and error sound. The sounds come from the Kenney Interface Sounds
pack, which has the CC0 license.

1. In the **Sounds** section, open **Sound preset**.
2. Select a preset. Use the play control next to a sound to hear it first.
3. Save. The next sound uses the new preset.

## Make your own preset

Your own preset has a name and one value for each sound:

| Value | Result |
|---|---|
| Empty | No sound for this event. |
| A built-in preset, such as **Glass** | The sound of that preset for this event. |
| A file path, such as `~/Sounds/done.wav` | This file. |

So you can keep the Glass finish sound, use your own file for questions and turn off the error
sound.

1. In **Sound preset**, add a preset and give it a name.
2. Set the three sounds. Play each one to check it.
3. Make the preset active and save.

The name cannot be the name of a built-in preset, and two presets cannot have the same name. Case
does not matter. You can keep up to 50 presets. Vis refuses a file path that names no file on the
gateway computer, and then saves nothing.

Use short WAV files. Every platform can play WAV. macOS can also play MP3, M4A and AIFF files.
A sound stops after 10 seconds.

You can also put the presets in `~/.vis/state.yml`:

```yaml
toggles:
  sounds: true
  sound_presets:
    active: My mix
    custom:
      - name: My mix
        slots:
          finish: glass
          input: ~/Sounds/question.wav
          error: ""
```

The slot names are `finish`, `input` (the question sound) and `error`.

## Where the sound plays

The gateway plays the sound on its own computer. This is the computer that runs Vis. When you use a
gateway on a server, the sound plays on the server, not on your computer. The play control in the
settings also plays the sound on the gateway computer.

Vis uses the sound program of the computer:

| System | Program |
|---|---|
| macOS | `afplay` |
| Linux | `pw-play`, `paplay` or `aplay`, the first one that is installed |
| Windows | PowerShell with `Media.SoundPlayer`, for WAV files only |

## Troubleshooting

- **No sound plays.** Make sure that **Sounds** is on and that the sound of the active preset is not
  empty. On Linux, install PipeWire, PulseAudio or ALSA tools.
- **Your own file stopped playing.** The file moved after you saved the preset. Vis then writes a
  `Sound file not found` warning to the gateway log, which [Logging](logging.md) explains. Set the
  path again.
- **You hear the sound on the wrong computer.** The gateway plays it. Read [Where the sound
  plays](#where-the-sound-plays).

## See also

- [Sounds API](sounds-api.md) — choose and save presets from Python or with HTTP requests.
- [Configuration](configuration.md) — find all settings and the `state.yml` file.
- [Council](council.md) — subagents and the lead agent.
