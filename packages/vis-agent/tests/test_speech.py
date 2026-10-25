"""The Speech API: voices, synthesis and transcription over the canonical routes."""

import json
from urllib.parse import parse_qs, urlsplit

import pytest
from blockether.vis.engine import GatewayClient, GatewayError, Speech, Voice
from test_client import compatible, endpoint

WAV = b"RIFF\x24\x00\x00\x00WAVEfmt "


def respond(method, path, raw):
    if result := compatible(method, path, raw):
        return result
    url = urlsplit(path)
    query = {k: v[0] for k, v in parse_qs(url.query).items()}
    route = url.path
    if route == "/v1/speech/voices" and method == "GET":
        return 200, {
            "engine": {"id": query.get("engine", "piper-local")},
            "voices": [
                {"id": "alba", "label": "Alba (English)", "language": "en"},
                {"id": "my-voice", "label": "My voice", "is_imported": True},
            ],
        }
    if route == "/v1/speech/voices" and method == "POST":
        assert raw == WAV
        return 201, {
            "voice": {
                "id": "anna",
                "label": query["name"],
                "language": query.get("lang"),
                "clip_text": query.get("text"),
                "is_imported": True,
            }
        }
    if route == "/v1/speech/voices/anna" and method == "PATCH":
        body = json.loads(raw)
        return 200, {
            "voice": {
                "id": "anna",
                "label": body.get("name", "Anna"),
                "is_imported": True,
            }
        }
    if route.startswith("/v1/speech/voices/") and method == "DELETE":
        if route.endswith("/anna"):
            return 200, {"is_forgotten": True}
        return 404, {"error": {"code": "voice-not-found", "message": "missing"}}
    if route == "/v1/speech" and method == "POST":
        body = json.loads(raw)
        if len(body["text"]) < 20:
            return 200, WAV, "audio/wav"
        return 202, {"id": "vj_1", "is_done": False, "phase": "queued"}
    if route == "/v1/speech/jobs/vj_1":
        return 200, {
            "id": "vj_1",
            "is_done": True,
            "phase": "done",
            "engine": "vibevoice-local",
        }
    if route == "/v1/speech/jobs/vj_1/audio":
        return 200, WAV + b"long", "audio/wav"
    if route == "/v1/voice" and method == "POST":
        return 202, {"id": "vj_2", "is_done": False, "phase": "queued"}
    if route == "/v1/voice/jobs/vj_2":
        return 200, {
            "id": "vj_2",
            "is_done": True,
            "phase": "done",
            "text": "Hello there.",
            "engine": "parakeet-local",
        }
    return 404, {"error": {"code": "missing", "message": "missing"}}


def test_voices_are_listed_with_their_engine():
    with endpoint(respond) as (url, calls):
        speech = GatewayClient(url).speech
        assert isinstance(speech, Speech)
        voices = speech.voices("pocket-tts-local")
        assert voices[0] == Voice("alba", "Alba (English)", "pocket-tts-local", "en")
        assert voices[1].is_imported
        assert calls[-1][1] == "/v1/speech/voices?engine=pocket-tts-local"


def test_a_voice_is_added_updated_and_removed(tmp_path):
    clip = tmp_path / "anna.wav"
    clip.write_bytes(WAV)
    with endpoint(respond) as (url, calls):
        speech = GatewayClient(url).speech
        voice = speech.add_voice(
            clip, "Anna", language="en", transcript="Hi, I am Anna."
        )
        assert (voice.id, voice.label, voice.clip_text) == (
            "anna",
            "Anna",
            "Hi, I am Anna.",
        )
        assert voice.engine == "pocket-tts-local"
        assert "engine=pocket-tts-local" in calls[-1][1]
        assert speech.update_voice("anna", name="Anna B").label == "Anna B"
        assert json.loads(calls[-1][3]) == {"name": "Anna B"}
        speech.update_voice("anna", language=None)
        assert json.loads(calls[-1][3]) == {"language": None}
        assert speech.remove_voice("anna") is True
        assert speech.remove_voice("ghost") is False
    with pytest.raises(ValueError):
        speech.update_voice("anna")
    with pytest.raises(ValueError):
        speech.add_voice(WAV, " ")


def test_speak_returns_inline_audio_and_waits_for_a_job():
    with endpoint(respond) as (url, calls):
        speech = GatewayClient(url).speech
        short = speech.speak("Hi.", voice="emma", engine="vibevoice-local", seed=7)
        assert (short.content, short.media_type, short.seed) == (WAV, "audio/wav", 7)
        assert json.loads(calls[-1][3]) == {"text": "Hi.", "voice": "emma", "seed": 7}
        long = speech.speak("A longer line that becomes a gateway job.", voice="emma")
        assert long.content == WAV + b"long"
        assert long.engine == "vibevoice-local"
    with pytest.raises(ValueError):
        speech.speak("Hi.", seed=-1)


def test_transcribe_waits_for_the_job(tmp_path):
    with endpoint(respond) as (url, calls):
        heard = GatewayClient(url).speech.transcribe(WAV, engine="parakeet-local")
        assert (heard.text, heard.engine) == ("Hello there.", "parakeet-local")
        assert calls[0][1] == "/v1/voice?engine=parakeet-local"


def test_saved_audio_creates_its_directory(tmp_path):
    with endpoint(respond) as (url, _):
        audio = GatewayClient(url).speech.speak("Hi.")
    target = audio.save(tmp_path / "out" / "hi.wav")
    assert target.read_bytes() == WAV


def test_gateway_refusals_keep_their_status():
    def refuse(method, path, raw):
        if result := compatible(method, path, raw):
            return result
        return 409, {"error": {"code": "voice-import-unsupported", "message": "no"}}

    with endpoint(refuse) as (url, _):
        with pytest.raises(GatewayError) as caught:
            GatewayClient(url).speech.add_voice(WAV, "Anna", engine="piper-local")
    assert caught.value.status == 409
