# Speech API

Speak text, transcribe recordings, and add, change, or remove your own voices from a program or
from an extension. The gateway runs the speech engines.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A script must turn text into audio.** Read [Speak text](#speak-text).
- **A program must add a voice from a recording.** Read [Add a voice](#add-a-voice), then [List the
  voices](#list-the-voices).
- **A voice has a wrong name, language, or transcript.** Read [Change a voice](#change-a-voice).
- **A voice is no longer necessary.** Read [Remove a voice](#remove-a-voice).
- **A program must check what an audio file says.** Read [Transcribe audio](#transcribe-audio).
- **An extension tool must speak or manage voices.** Read [Use speech in an
  extension](#use-speech-in-an-extension).

To learn what each engine does and how to record a good voice, read [Speech and voices](speech.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and its
`speech` property.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

</div>

The first use of an engine downloads its model. Until the model is ready, a request gets status
425.

## Operations

<div data-variant="python">

A typed call waits for its gateway job and returns a typed record. A generated method sends one
request and returns its answer.

| Task | Typed call | Generated method |
|---|---|---|
| List the voices | `client.speech.voices(engine)` | `client.routes.get_speech_voices(query=...)` |
| Add a voice | `client.speech.add_voice(recording, name, ...)` | `client.routes.post_speech_voices(content=..., query=...)` |
| Change a voice | `client.speech.update_voice(voice_id, ...)` | `client.routes.patch_speech_voice(voice_id, body=...)` |
| Remove a voice | `client.speech.remove_voice(voice_id)` | `client.routes.delete_speech_voice(voice_id)` |
| Speak text | `client.speech.speak(text, ...)` | `client.routes.post_speech(body=...)`, `client.routes.get_speech_job(job_id)`, `client.routes.get_speech_job_audio(job_id)` |
| Transcribe audio | `client.speech.transcribe(audio, ...)` | `client.routes.post_voice(content=...)`, `client.routes.get_voice_job(job_id)` |
| Prepare an engine | `client.speech.model(engine, prepare=True)` | `client.routes.post_speech_model(query=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| List the voices | `GET /v1/speech/voices` |
| Add a voice | `POST /v1/speech/voices` |
| Change a voice | `PATCH /v1/speech/voices/{voice_id}` |
| Remove a voice | `DELETE /v1/speech/voices/{voice_id}` |
| Speak text | `POST /v1/speech` |
| Transcribe audio | `POST /v1/voice` |
| Prepare an engine | `POST /v1/speech/model` |

Each route takes an optional `engine` query parameter, for example `?engine=pocket-tts-local`.

</div>

## Speak text

<div data-variant="python">

```python
audio = client.speech.speak(
    "The Great Wall is not one wall.", voice="emma", engine="vibevoice-local", seed=7
)
audio.save("out/line.wav")
```

The call returns a `SpeechAudio` record with the `content` bytes and their `media_type`. The same
`seed` gives the same take again. The call raises `VisTimeout` after `timeout` seconds.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/speech?engine=vibevoice-local" -H 'content-type: application/json' \
  --data '{"text": "The Great Wall is not one wall.", "voice": "emma", "seed": 7}' -o line.wav -w '%{http_code}\n'
```

Short text gets status 200 and the audio. Longer text gets status 202 and a job. Read the job with
`GET /v1/speech/jobs/{job_id}` until `is_done` is true. Then get the audio from
`GET /v1/speech/jobs/{job_id}/audio`.

</div>

## List the voices

<div data-variant="python">

```python
for voice in client.speech.voices("pocket-tts-local"):
    print(voice.id, voice.label, voice.language, voice.is_imported)
```

Each `Voice` record has an `id`, a `label`, and its `engine`. A voice that you added has
`is_imported` set to true.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/speech/voices?engine=pocket-tts-local"
```

The answer has the `engine` and its `voices`. A voice that you added has `is_imported` set to true.

</div>

## Add a voice

<div data-variant="python">

```python
voice = client.speech.add_voice(
    "recordings/anna.wav", "Anna", language="en", transcript="Hello, this is Anna."
)
print(voice.id)
```

The recording is a path or bytes. A recording that the engine cannot use raises `GatewayError` with
status 400. If a voice has the same ID, the call replaces the voice and its audio.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/speech/voices?engine=pocket-tts-local&name=Anna&lang=en" \
  -H 'content-type: audio/wav' --data-binary @recordings/anna.wav
```

The body is the recording. The `name`, `lang`, and `text` query parameters describe it. The answer
has the new `voice`. A recording that the engine cannot use gets status 400.

</div>

## Change a voice

<div data-variant="python">

```python
voice = client.speech.update_voice("anna", name="Anna (studio)", transcript=None)
```

An argument that you omit keeps its value. `None` or an empty string removes the value. The ID
stays the same. A voice that you did not add raises `GatewayError` with status 404.

</div>

<div data-variant="http">

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/speech/voices/anna?engine=pocket-tts-local" \
  -H 'content-type: application/json' --data '{"name": "Anna (studio)", "text": ""}'
```

The body has `name`, `language`, or `text`. A key that you omit keeps its value. An empty value
removes it. A voice that you did not add gets status 404.

</div>

## Remove a voice

<div data-variant="python">

```python
removed = client.speech.remove_voice("anna")
```

The call returns false when the engine has no voice with this ID.

</div>

<div data-variant="http">

```bash
vis_api -X DELETE "$VIS_GATEWAY_URL/v1/speech/voices/anna?engine=pocket-tts-local"
```

A voice that does not exist gets status 404.

</div>

## Transcribe audio

<div data-variant="python">

```python
transcript = client.speech.transcribe("out/line.wav", engine="parakeet-local")
print(transcript.text)
```

The audio must be a 16-bit PCM WAV file. The `Transcript` record has the `text` and its `segments`.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/voice?engine=parakeet-local" \
  -H 'content-type: audio/wav' --data-binary @line.wav
```

The answer has status 202 and a job. Read the job with `GET /v1/voice/jobs/{job_id}` until
`is_done` is true. The finished job has the `text`.

</div>

## Use speech in an extension

<div data-variant="python">

```python
from blockether.vis import extension as vis


class Narrator:
    def say(self, text: str, path: str) -> str:
        """Speak `text` and save the audio at `path`."""
        return str(vis.speech.speak(text, voice="emma").save(path))
```

`vis.speech` has the same methods as `client.speech`. Add, change, or remove voices only while a
tool call runs. Background work of an extension can list voices, speak, and transcribe.

</div>

<div data-variant="http">

An extension uses the Python `vis.speech` object.

</div>

## See also

- To choose an engine, record a good voice, and learn about voice IDs, read [Speech and
  voices](speech.md).
- For the declarations and host operations of extension tools, read [Extension
  API](extension-api.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python
  SDK](python-sdk.md).
- For authentication, the OpenAPI document, and gateway errors, read [HTTP API](http-api.md).
