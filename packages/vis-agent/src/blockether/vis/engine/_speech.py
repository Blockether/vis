"""Speech API: voices, synthesis and transcription through the gateway engines.

Use `ExecutionLayer.speech` from the SDK, or `vis.speech` inside an extension.
Both send the same canonical gateway requests.
"""

from __future__ import annotations

import time
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from ._client import GatewayError, ProtocolError, VisTimeout

if TYPE_CHECKING:
    from ._client import ExecutionLayer

CLONING_ENGINE = "pocket-tts-local"
"""The engine that learns a voice from a recording."""

_UNSET: Any = object()
_POLL_SECONDS = 0.25


@dataclass(frozen=True, slots=True)
class Voice:
    """One voice that a speaking engine can use."""

    id: str
    label: str
    engine: str
    language: str | None = None
    is_imported: bool = False
    clip_text: str | None = None
    license: str | None = None


@dataclass(frozen=True, slots=True)
class SpeechAudio:
    """Audio that an engine spoke, as the bytes of one file."""

    content: bytes
    media_type: str
    engine: str | None
    voice: str | None
    seed: int | None

    def save(self, path: str | Path) -> Path:
        """Write the audio to `path`, create missing parents and return the path."""
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(self.content)
        return target


@dataclass(frozen=True, slots=True)
class Transcript:
    """The text that a transcription engine heard in a recording."""

    text: str
    engine: str | None
    segments: tuple[dict[str, Any], ...] = ()


def _voice(entry: Any, engine: str) -> Voice:
    if not isinstance(entry, dict) or not isinstance(entry.get("id"), str):
        raise ProtocolError("invalid voice entry")
    return Voice(
        id=entry["id"],
        label=str(entry.get("label") or entry["id"]),
        engine=engine,
        language=entry.get("language"),
        is_imported=bool(entry.get("is_imported")),
        clip_text=entry.get("clip_text"),
        license=entry.get("license"),
    )


def _object(reply: Any, what: str) -> dict[str, Any]:
    """The JSON object of a gateway answer, or ProtocolError for another shape."""
    if not isinstance(reply, dict):
        raise ProtocolError(f"invalid {what} answer")
    return reply


def _audio_bytes(audio: str | Path | bytes) -> bytes:
    if isinstance(audio, bytes):
        return audio
    return Path(audio).expanduser().read_bytes()


def _media_type(headers: dict[str, str]) -> str:
    for key, value in headers.items():
        if key.lower() == "content-type" and value:
            return value.split(";")[0].strip()
    return "audio/wav"


def _engine_query(engine: str | None, **extra: str | None) -> dict[str, str] | None:
    query = {k: v for k, v in {"engine": engine, **extra}.items() if v is not None}
    return query or None


class Speech:
    """Manage voices, speak text and transcribe audio with the gateway engines.

    Without `engine`, a call uses the engine that the gateway selected. Voice changes
    use the cloning engine `pocket-tts-local` by default. Long work runs as a gateway
    job; these methods wait for it and return the result.
    """

    def __init__(self, layer: ExecutionLayer):
        self._layer = layer

    def voices(self, engine: str | None = None) -> list[Voice]:
        """List the voices of `engine`, imported voices included."""
        reply = _object(
            self._layer.get_speech_voices(query=_engine_query(engine)), "voices"
        )
        name = str((reply.get("engine") or {}).get("id") or engine or "")
        return [_voice(entry, name) for entry in reply.get("voices") or []]

    def add_voice(
        self,
        recording: str | Path | bytes,
        name: str,
        *,
        language: str | None = None,
        transcript: str | None = None,
        engine: str = CLONING_ENGINE,
    ) -> Voice:
        """Import a recording of one speaker as a voice and return it.

        Use 3 to 30 seconds of clean speech. WAV works everywhere; the gateway converts
        other containers when it can. `transcript` is what the clip says and improves
        the clone. A voice with the same id is replaced, so this also replaces the audio
        of an existing voice. Raises `GatewayError` 400 for an unusable recording and
        409 for an engine that cannot learn voices.
        """
        if not name or not name.strip():
            raise ValueError("name must not be blank")
        reply = _object(
            self._layer.post_speech_voices(
                content=_audio_bytes(recording),
                query=_engine_query(engine, name=name, lang=language, text=transcript),
            ),
            "voice",
        )
        return _voice(reply.get("voice"), engine)

    def update_voice(
        self,
        voice_id: str,
        *,
        name: str | None = _UNSET,
        language: str | None = _UNSET,
        transcript: str | None = _UNSET,
        engine: str = CLONING_ENGINE,
    ) -> Voice:
        """Change the label, language or clip transcript of an imported voice.

        An omitted argument keeps its value; `None` or "" removes it. The id stays the
        same. Raises `GatewayError` 404 when the engine has no such imported voice.
        """
        body: dict[str, Any] = {
            key: value
            for key, value in (
                ("name", name),
                ("language", language),
                ("text", transcript),
            )
            if value is not _UNSET
        }
        if not body:
            raise ValueError("give name, language or transcript")
        reply = _object(
            self._layer.patch_speech_voice(
                voice_id, body=body, query=_engine_query(engine)
            ),
            "voice",
        )
        return _voice(reply.get("voice"), engine)

    def remove_voice(self, voice_id: str, *, engine: str = CLONING_ENGINE) -> bool:
        """Delete an imported voice. Return False when the engine has no such voice."""
        try:
            self._layer.delete_speech_voice(voice_id, query=_engine_query(engine))
        except GatewayError as error:
            if error.status == 404:
                return False
            raise
        return True

    def voice_sample(self, voice_id: str, *, engine: str | None = None) -> bytes:
        """Return a short audio sample of a voice. Raises `GatewayError` 404 without one."""
        return self._layer.get_speech_voice_sample(
            voice_id, query=_engine_query(engine)
        ).content

    def model(self, engine: str | None = None, *, prepare: bool = False) -> dict:
        """Return the readiness of a speaking engine; `prepare=True` starts its download."""
        query = _engine_query(engine)
        if prepare:
            return _object(self._layer.post_speech_model(query=query), "model")
        return _object(self._layer.get_speech_model(query=query), "model")

    def speak(
        self,
        text: str,
        *,
        voice: str | None = None,
        engine: str | None = None,
        seed: int | None = None,
        timeout: float = 600,
    ) -> SpeechAudio:
        """Speak `text` and return the audio, usually 24 kHz mono WAV.

        `seed` selects another take of the same text on engines that sample noise,
        such as `vibevoice-local` and `qwen3-tts-local`. The gateway speaks at most
        20000 characters at once. Raises `GatewayError` 425 while the engine model is
        not ready and `VisTimeout` when the job takes longer than `timeout` seconds.
        """
        if not text or not text.strip():
            raise ValueError("text must not be blank")
        if seed is not None and not (type(seed) is int and 0 <= seed <= 2147483647):
            raise ValueError("seed must be an integer from 0 to 2147483647")
        body: dict[str, Any] = {"text": text}
        if voice is not None:
            body["voice"] = voice
        if seed is not None:
            body["seed"] = seed
        response = self._layer._request(
            "POST", "/v1/speech", body=body, query=_engine_query(engine)
        )
        if response.status == 200:
            return SpeechAudio(
                response.content, _media_type(response.headers), engine, voice, seed
            )
        job = self._wait("speech", response.json(), timeout)
        audio = self._layer.get_speech_job_audio(job["id"])
        return SpeechAudio(
            audio.content,
            _media_type(audio.headers),
            job.get("engine") or engine,
            voice,
            seed,
        )

    def transcribe(
        self,
        audio: str | Path | bytes,
        *,
        engine: str | None = None,
        timeout: float = 600,
    ) -> Transcript:
        """Transcribe a 16-bit PCM WAV recording and return its text.

        Engines include `parakeet-local` and `vibevoice-asr-local`. Raises `GatewayError`
        400 for audio that is not WAV and `VisTimeout` after `timeout` seconds.
        """
        job = self._layer.post_voice(
            content=_audio_bytes(audio), query=_engine_query(engine)
        )
        job = self._wait("voice", job, timeout)
        segments = job.get("segments") or ()
        return Transcript(
            str(job.get("text") or ""),
            job.get("engine") or engine,
            tuple(s for s in segments if isinstance(s, dict)),
        )

    def _wait(self, direction: str, job: Any, timeout: float) -> dict:
        if not isinstance(job, dict) or not isinstance(job.get("id"), str):
            raise ProtocolError(f"invalid {direction} job")
        get = (
            self._layer.get_speech_job
            if direction == "speech"
            else self._layer.get_voice_job
        )
        deadline = time.monotonic() + timeout
        while not job.get("is_done"):
            if time.monotonic() > deadline:
                raise VisTimeout(f"{direction} job {job['id']} is still running")
            time.sleep(_POLL_SECONDS)
            job = get(job["id"])
        if job.get("phase") == "failed":
            raise RuntimeError(str(job.get("error") or f"{direction} job failed"))
        return job
