"""The Sounds API: the machine sound settings over the settings routes."""

import json
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

import pytest
from blockether.vis.engine import GatewayClient, Sounds, SoundSettings
from test_client import compatible, endpoint


def settings_gateway():
    rows = {"sounds": False, "sound_finish": "", "sound_input": "", "sound_error": ""}
    writes = []

    def respond(method, path, raw):
        if result := compatible(method, path, raw):
            return result
        url = urlsplit(path)
        query = {k: v[0] for k, v in parse_qs(url.query).items()}
        if method == "GET" and url.path.startswith("/v1/settings/"):
            id = url.path.rsplit("/", 1)[1]
            assert query == {"scope": "global"}
            key = "enabled" if id == "sounds" else "value"
            return 200, {"id": id, key: rows[id]}
        if method == "POST" and url.path == "/v1/settings":
            body = json.loads(raw)
            writes.append(body)
            rows[body["id"]] = body["value"] if body["action"] == "value" else ""
            return 200, {"id": body["id"]}
        return 404, {"error": {"code": "missing", "message": "missing"}}

    return respond, writes


def test_sounds_are_read_and_changed_as_global_settings(tmp_path):
    respond, writes = settings_gateway()
    own = tmp_path / "done.wav"
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        assert isinstance(sounds, Sounds)
        assert sounds.get() == SoundSettings(
            is_enabled=False, finish="", input="", error=""
        )
        changed = sounds.set(enabled=True, finish=own, error="off")
        assert changed == SoundSettings(
            is_enabled=True, finish=str(own), input="", error="off"
        )
        assert writes == [
            {"scope": "global", "id": "sounds", "action": "value", "value": True},
            {
                "scope": "global",
                "id": "sound_finish",
                "action": "value",
                "value": str(own),
            },
            {"scope": "global", "id": "sound_error", "action": "value", "value": "off"},
        ]
        writes.clear()
        assert sounds.set(finish=None).finish == ""
        assert writes == [
            {"scope": "global", "id": "sound_finish", "action": "inherit"}
        ]


def test_bad_sound_values_write_nothing():
    respond, writes = settings_gateway()
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        with pytest.raises(TypeError):
            sounds.set(enabled="yes")
        with pytest.raises(TypeError):
            sounds.set(input=3)
        with pytest.raises(ValueError):
            sounds.set(input="a.wav\nb.wav")
        with pytest.raises(TypeError):
            sounds.set(finish=Path("a.wav"), error=b"x")
    assert writes == []
