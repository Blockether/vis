"""The Sounds API: sound presets over the machine settings routes."""

import copy
import json
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

import pytest
from blockether.vis import _contracts
from blockether.vis.engine import GatewayClient, SoundPreset, Sounds, SoundSettings
from test_client import compatible, endpoint

FIXTURE = json.loads((_contracts._DATA / "fixtures/setting-presets.json").read_text())
BUILTIN = tuple(
    SoundPreset(
        name=b["id"], finish=b["id"], input=b["id"], error=b["id"], is_builtin=True
    )
    for b in FIXTURE["row"]["editor_options"]["builtin"]
)


def settings_gateway(value=None):
    row = dict(
        copy.deepcopy(FIXTURE["row"]),
        value=value or {"active": "classic", "custom": []},
    )
    state = {"sounds": False}
    writes, previews = [], []

    def respond(method, path, raw):
        if result := compatible(method, path, raw):
            return result
        url = urlsplit(path)
        query = {k: v[0] for k, v in parse_qs(url.query).items()}
        if method == "GET" and url.path == "/v1/settings/sounds":
            assert query == {"scope": "global"}
            return 200, {"id": "sounds", "enabled": state["sounds"]}
        if method == "GET" and url.path == "/v1/settings/sound_presets":
            assert query == {"scope": "global"}
            return 200, row
        if method == "POST" and url.path == "/v1/settings":
            body = json.loads(raw)
            writes.append(body)
            if body["id"] == "sounds":
                state["sounds"] = body["value"]
            else:
                row["value"] = body["value"]
            return 200, {"id": body["id"]}
        if method == "POST" and url.path == FIXTURE["preview_path"]:
            body = json.loads(raw)
            previews.append(body)
            return 200, {"is_played": body["value"] != ""}
        return 404, {"error": {"code": "missing", "message": "missing"}}

    return respond, writes, previews


def test_reads_builtin_and_saved_presets_from_the_fixture_row():
    respond, _, _ = settings_gateway(FIXTURE["row"]["value"])
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        assert isinstance(sounds, Sounds)
        settings = sounds.get()
    mix = SoundPreset(
        name="My mix", finish="~/Sounds/done.wav", input="glass", error=""
    )
    assert settings == SoundSettings(
        is_enabled=False, active="My mix", presets=BUILTIN + (mix,)
    )
    assert settings.preset == mix
    assert settings.custom == (mix,)


def test_saves_activates_replaces_and_deletes_a_preset(tmp_path):
    respond, writes, _ = settings_gateway()
    own = tmp_path / "done.wav"
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        saved = sounds.save_preset(
            "Mine", finish=own, input="glass", error=None, activate=True
        )
        assert saved.active == "Mine"
        assert writes[-1] == {
            "scope": "global",
            "id": "sound_presets",
            "action": "value",
            "value": {
                "active": "Mine",
                "custom": [
                    {
                        "name": "Mine",
                        "slots": {"finish": str(own), "input": "glass", "error": ""},
                    }
                ],
            },
        }
        # The same name without regard to case replaces the preset and keeps it active.
        replaced = sounds.save_preset(" mine ", finish="soft")
        assert replaced.active == "mine"
        assert replaced.custom == (
            SoundPreset(name="mine", finish="soft", input="", error=""),
        )
        sounds.save_preset("Other")
        assert sounds.set(enabled=True, preset="GLASS").active == "glass"
        assert writes[-2] == {
            "scope": "global",
            "id": "sounds",
            "action": "value",
            "value": True,
        }
        assert [p.name for p in sounds.get().custom] == ["mine", "Other"]
        sounds.set(preset="Other")
        after = sounds.delete_preset("other")
        assert after.active == BUILTIN[0].name
        assert [p.name for p in after.custom] == ["mine"]
        # A repeated delete changes nothing.
        count = len(writes)
        with pytest.raises(ValueError):
            sounds.delete_preset("other")
        assert len(writes) == count


def test_preview_plays_one_slot_value_without_a_save():
    respond, writes, previews = settings_gateway()
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        assert sounds.preview("input", "glass") is True
        assert sounds.preview("error", None) is False
    assert previews == [FIXTURE["preview_request"], {"slot": "error", "value": ""}]
    assert writes == []


def test_bad_values_write_nothing():
    respond, writes, previews = settings_gateway()
    with endpoint(respond) as (url, _):
        sounds = GatewayClient(url).sounds
        with pytest.raises(TypeError):
            sounds.set(enabled="yes")
        with pytest.raises(ValueError):
            sounds.set(enabled=True, preset="nope")
        with pytest.raises(ValueError):
            sounds.save_preset("Glass")
        with pytest.raises(ValueError):
            sounds.save_preset("  ")
        with pytest.raises(ValueError):
            sounds.save_preset("A", input="a.wav\nb.wav")
        with pytest.raises(TypeError):
            sounds.save_preset("A", finish=Path("a.wav"), error=b"x")
        with pytest.raises(ValueError):
            sounds.preview("volume", "glass")
    assert writes == []
    assert previews == []
