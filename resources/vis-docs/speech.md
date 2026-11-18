# Speech and voices

Vis speaks text and transcribes recordings with speech engines that run on your gateway. You can
add your own voice from a short recording and use it like a built-in voice.

## When to use

- **You want spoken audio for a podcast, a summary, or a voice message.** Read [Speak
  text](#speak-text).
- **You want a voice that sounds like you or like a colleague.** Read [Add your own
  voice](#add-your-own-voice).
- **You want to change or remove a voice that you added.** Read [Change or remove a
  voice](#change-or-remove-a-voice).
- **You want to check what a recording says.** Read [Transcribe a
  recording](#transcribe-a-recording).
- **You want the voice models to use less memory.** Read [Memory use](#memory-use).

To make these calls from a program or an extension, read [Speech API](speech-api.md). To find the
settings in the Companion app, read [Machine settings](configuration.md#machine-settings).

## Speech engines

The first use of an engine downloads its model, and the download can take some minutes.

| Engine | Use | Voices |
|---|---|---|
| `vibevoice-local` | Natural speech for long text, with a seed for each take | Built-in voices, such as `emma` and `carter` |
| `qwen3-tts-local` | Expressive speech in ten languages | Built-in voices, such as `ryan` and `serena`, and voices that you add |
| `pocket-tts-local` | Fast speech with your own voices | Built-in voices and voices that you add |
| `piper-local` | Small and fast speech | Built-in voices |
| `parakeet-local` | Default transcription engine, for short and long recordings | None |
| `vibevoice-asr-local` | Alternative transcription engine | None |

Select the default engines in the **General** section of the Companion app settings.

The `qwen3-tts-local` model is 3.7 GB. On a CPU, it speaks at about the speed of playback.

## Memory use

A loaded model stays in the memory of the gateway. Three settings in the **General** section
control this memory.

| Setting | Default | Effect |
|---|---|---|
| **Voice model memory limit** | 4 GB | When the loaded models are larger than the limit, the gateway unloads the least recently used model. |
| **Unload idle voice models** | 5 minutes | The gateway unloads a model that nothing used for this time. The next request loads the model again in some seconds. |
| **Preload the voice model** | Off | When the setting is on, the gateway keeps the transcription model while the model is idle. |

The gateway does not unload a model that a request uses, or the model that it used last. The limit
compares the sizes of the model files. The gateway process uses more memory than that.

## Speak text

Send the text, with an optional voice and an optional engine. Short text comes back at once. Long
text runs as a job, and the client waits for the audio. The audio is a mono WAV file, at 24 kHz for
most engines.

`vibevoice-local` and `qwen3-tts-local` give a different take each time. Give a `seed` to get the
same take again.

## Add your own voice

Record only people who agree to the use of their voice.

1. Record 3 to 30 seconds of one person who speaks clearly, without music or other voices.
2. Add the recording with a name.
3. Add what the recording says as its transcript. The transcript makes the voice closer to the
   recording.

`pocket-tts-local` learns the voice from the recording. The voice gets an ID from its name, and
scripts and settings use this ID. Vis keeps the voices that you add in `~/.vis/voices` on the
gateway computer. Every program that uses this gateway can use them.

For the voices that you add, `qwen3-tts-local` downloads a second model of about 1.8 GB on first
use. On a CPU, one second of speech takes about 1 to 2 seconds.

For the best copy of a voice with `qwen3-tts-local`, do these steps:

1. Use 5 to 10 seconds of calm reading, and end the recording at the end of a sentence.
2. Give the exact transcript. If the speech starts with words of the recording, cut the recording
   again.
3. Use speech in one of the ten languages of the model. Other languages, such as Polish, get an
   accent.

## Change or remove a voice

You can change the name, the language, and the transcript of a voice that you added. The ID stays
the same. To replace the audio, add a new recording with the same name.

When you remove a voice, Vis deletes its recording. You cannot remove a built-in voice.

## Transcribe a recording

Send a 16-bit PCM WAV recording. The engine returns the text and its time segments. Use
`parakeet-local` for all recordings. It decodes a long recording in parts.

## See also

- To speak, transcribe, and manage voices from Python or with HTTP requests, read [Speech
  API](speech-api.md).
- To use `vis.speech` in the tools of an extension, read [Extension API](extension-api.md).
- To find where the Companion app keeps the speech settings, read
  [Configuration](configuration.md).
