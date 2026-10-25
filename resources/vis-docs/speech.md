# Speech and voices

Vis can speak text and transcribe recordings with speech engines that run on your gateway. You can
also add your own voice from a short recording and use it like a built-in voice.

## When to use

- **You want spoken audio for a podcast, a summary or a voice message.** Read [Speak
  text](#speak-text).
- **You want a voice that sounds like you or like a colleague.** [Add your own
  voice](#add-your-own-voice) from a short recording.
- **You want to change or remove a voice that you added.** Read [Change or remove a
  voice](#change-or-remove-a-voice).
- **You want to check what a recording says.** Read [Transcribe a recording](#transcribe-a-recording).

To make these calls from a program or an extension, read [Speech API](speech-api.md). To choose
the engines in the app, read [Configuration](configuration.md).

## Speech engines

The gateway runs each engine on its own computer. The first use of an engine downloads its model.
The download can take some minutes.

| Engine | Use | Voices |
|---|---|---|
| `vibevoice-local` | Natural speech for long text, with a seed for each take | Built-in voices, such as `emma` and `carter` |
| `qwen3-tts-local` | Expressive speech in ten languages | Built-in voices, such as `ryan` and `serena`, and voices that you add |
| `pocket-tts-local` | Fast speech with your own voices | Built-in voices and voices that you add |
| `piper-local` | Small and fast speech | Built-in voices |
| `parakeet-local` | Fast transcription of short recordings | — |
| `vibevoice-asr-local` | Transcription of long recordings | — |

The **Voice** section of the Companion settings selects the default engines.

The `qwen3-tts-local` model is 3.7 GB, and Vis downloads it on first use. On a CPU, it speaks at about
the speed of playback.

## Speak text

A request sends the text, an optional voice and an optional engine. Short text comes back at once.
Long text runs as a job, and the client waits for its audio. The answer is usually a 24 kHz mono WAV
file.

Engines such as `vibevoice-local` and `qwen3-tts-local` sample noise. Give a `seed` to get the same take again. Give
another seed to get a different take of the same text.

## Add your own voice

Record 3 to 30 seconds of one person who speaks clearly, without music or other voices. Then add the
recording with a name. `pocket-tts-local` learns the voice from it.

- Add what the recording says as its transcript. The transcript improves the voice.
- Record only people who agree to the use of their voice.
- Vis keeps the voices that you add in `~/.vis/voices` on the gateway computer. Every program that
  uses this gateway can use them.

The voice gets an ID from its name. Scripts and settings use this ID.

`qwen3-tts-local` also speaks the voices that you add. For these voices, Vis downloads a second
model of about 1.8 GB on first use. On a CPU, one second of speech takes about 1 to 2 seconds.
A transcript makes the voice closer to the recording.

For the best copy of a voice with `qwen3-tts-local`, use these steps:

1. Use 5 to 10 seconds of calm reading, and end the recording at the end of a sentence.
2. Give the exact transcript. If a sentence starts with words of the recording, cut the recording again.
3. Use speech in one of the ten languages of the model. Other languages, such as Polish, get an accent.

## Change or remove a voice

You can change the name, the language and the transcript of a voice that you added. The ID stays
the same, so scripts that use the voice continue to work. To replace the audio, add a new recording
with the same name.

When you remove a voice, Vis deletes its recording. You cannot remove a built-in voice.

## Transcribe a recording

Send a 16-bit PCM WAV recording. The engine returns the text and its time segments. Use
`vibevoice-asr-local` for recordings that are longer than a few sentences.

## See also

- [Speech API](speech-api.md) — speak, transcribe and manage voices from Python or with HTTP requests.
- [Extension API](extension-api.md) — use `vis.speech` in the tools of an extension.
- [Configuration](configuration.md) — choose the default speech engines.
