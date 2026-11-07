"""Sounds API: the sounds that the gateway machine plays for your turns.

Use `ExecutionLayer.sounds` from the SDK, or `vis.sounds` inside an extension. Both
change the same machine settings that Settings > Sounds shows in the Companion app and
the TUI, so a change from one place shows in all of them.

The gateway plays a sound in three slots: when a turn that you wait on finishes
(`finish`), when Vis waits for your answer (`input`) and when a turn fails or stops
(`error`). Subagent turns are silent. A preset gives one sound to each slot, and one
preset is active. Vis has built-in presets, and you can save your own. The gateway
process plays the sound, so a gateway on another machine plays it there.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from ._client import ProtocolError

if TYPE_CHECKING:
    from ._client import ExecutionLayer

SLOTS = ("finish", "input", "error")
"""The slots of a preset, in the order that settings show them."""

SILENT = ""
"""The slot value that plays no sound."""

_ENABLED = "sounds"
_PRESETS = "sound_presets"
_GLOBAL = {"scope": "global"}
_UNSET: Any = object()


@dataclass(frozen=True, slots=True)
class SoundPreset:
    """One preset: a name and one sound for each slot.

    A slot value is `""` for no sound, the name of a built-in preset for the sound of
    that preset in this slot, or the path of a sound file on the gateway machine (`~`
    expands there). Use WAV files: every platform player reads them. A built-in preset
    gives its own name to each slot.
    """

    name: str
    finish: str
    input: str
    error: str
    is_builtin: bool = False


@dataclass(frozen=True, slots=True)
class SoundSettings:
    """The sound settings of the gateway machine."""

    is_enabled: bool
    active: str
    presets: tuple[SoundPreset, ...]

    @property
    def preset(self) -> SoundPreset | None:
        """The active preset, or None when no preset has the active name."""
        return _find(self.presets, self.active)

    @property
    def custom(self) -> tuple[SoundPreset, ...]:
        """Your own presets, in saved order."""
        return tuple(p for p in self.presets if not p.is_builtin)


def _folded(name: str) -> str:
    return name.strip().lower()


def _find(presets: tuple[SoundPreset, ...], name: str) -> SoundPreset | None:
    return next((p for p in presets if _folded(p.name) == _folded(name)), None)


def _row(reply: Any, id: str) -> dict[str, Any]:
    if not isinstance(reply, dict) or reply.get("id") != id:
        raise ProtocolError(f"invalid {id} setting answer")
    return reply


def _name(name: Any) -> str:
    if not isinstance(name, str):
        raise TypeError("a preset name must be text")
    name = name.strip()
    if not name or "\n" in name or "\r" in name:
        raise ValueError("a preset name must be one line of text")
    return name


def _slot_value(slot: str, value: Any) -> str:
    """The wire value of one slot. None is no sound."""
    if value is None:
        return SILENT
    if isinstance(value, os.PathLike):
        value = os.fspath(value)
    if not isinstance(value, str):
        raise TypeError(f"{slot} must be a path, a built-in preset name, '' or None")
    if "\n" in value or "\r" in value:
        raise ValueError(f"{slot} must be one line")
    return value.strip()


def _custom_wire(preset: SoundPreset) -> dict[str, Any]:
    return {
        "name": preset.name,
        "slots": {slot: getattr(preset, slot) for slot in SLOTS},
    }


class Sounds:
    """Read and change the sounds that the gateway machine plays."""

    def __init__(self, layer: ExecutionLayer):
        self._layer = layer

    def get(self) -> SoundSettings:
        """Return the current sound settings with every preset."""
        enabled = _row(self._layer.get_setting(_ENABLED, query=_GLOBAL), _ENABLED)
        row = _row(self._layer.get_setting(_PRESETS, query=_GLOBAL), _PRESETS)
        value = row.get("value")
        options = row.get("editor_options")
        if not isinstance(value, dict) or not isinstance(options, dict):
            raise ProtocolError(f"invalid {_PRESETS} setting answer")
        builtin = tuple(
            SoundPreset(
                name=str(entry["id"]),
                finish=str(entry["id"]),
                input=str(entry["id"]),
                error=str(entry["id"]),
                is_builtin=True,
            )
            for entry in options.get("builtin", [])
        )
        custom = tuple(
            SoundPreset(
                name=str(entry["name"]),
                finish=str(entry.get("slots", {}).get("finish", SILENT)),
                input=str(entry.get("slots", {}).get("input", SILENT)),
                error=str(entry.get("slots", {}).get("error", SILENT)),
            )
            for entry in value.get("custom", [])
        )
        return SoundSettings(
            is_enabled=bool(enabled.get("enabled")),
            active=str(value.get("active", "")),
            presets=builtin + custom,
        )

    def set(self, *, enabled: bool = _UNSET, preset: str = _UNSET) -> SoundSettings:
        """Turn sounds on or off, choose the active preset, and return all settings.

        `preset` is the name of a built-in or saved preset, without regard to case.
        Arguments that you omit do not change.
        """
        if enabled is not _UNSET and type(enabled) is not bool:
            raise TypeError("enabled must be a bool")
        current: SoundSettings | None = None
        chosen: SoundPreset | None = None
        if preset is not _UNSET:
            current = self.get()
            chosen = _find(current.presets, _name(preset))
            if chosen is None:
                raise ValueError(f"no preset is called {preset.strip()}")
        if enabled is not _UNSET:
            self._layer.post_settings(
                body={**_GLOBAL, "id": _ENABLED, "action": "value", "value": enabled}
            )
        if current is not None and chosen is not None:
            self._write(chosen.name, current.custom)
        return self.get()

    def save_preset(
        self,
        name: str,
        *,
        finish: str | os.PathLike[str] | None = SILENT,
        input: str | os.PathLike[str] | None = SILENT,
        error: str | os.PathLike[str] | None = SILENT,
        activate: bool = False,
    ) -> SoundSettings:
        """Save your own preset, or replace the saved preset with this name.

        Each slot takes a file path on the gateway machine, the name of a built-in
        preset for its sound, or `""` or None for no sound. The gateway refuses a path
        that names no file, and then saves nothing. With `activate`, the preset plays
        from now on.
        """
        name = _name(name)
        if type(activate) is not bool:
            raise TypeError("activate must be a bool")
        preset = SoundPreset(
            name=name,
            finish=_slot_value("finish", finish),
            input=_slot_value("input", input),
            error=_slot_value("error", error),
        )
        current = self.get()
        existing = _find(current.presets, name)
        if existing is not None and existing.is_builtin:
            raise ValueError(f"a built-in preset is already called {existing.name}")
        custom = tuple(preset if p is existing else p for p in current.custom) + (
            () if existing else (preset,)
        )
        replaced = existing is not None and _folded(current.active) == _folded(name)
        self._write(name if activate or replaced else current.active, custom)
        return self.get()

    def delete_preset(self, name: str) -> SoundSettings:
        """Delete your own preset. When it was active, the first built-in preset plays."""
        current = self.get()
        existing = _find(current.custom, _name(name))
        if existing is None:
            raise ValueError(f"no saved preset is called {name.strip()}")
        builtin = [p for p in current.presets if p.is_builtin]
        active = current.active
        if _folded(active) == _folded(existing.name):
            active = builtin[0].name if builtin else ""
        self._write(active, tuple(p for p in current.custom if p is not existing))
        return self.get()

    def preview(self, slot: str, value: str | os.PathLike[str] | None) -> bool:
        """Play one slot value on the gateway machine now, without a save.

        Returns True when a sound starts, and False for no sound, a sound that plays
        already or a machine without a player.
        """
        if slot not in SLOTS:
            raise ValueError(f"slot must be one of {', '.join(SLOTS)}")
        reply = self._layer.post_setting_preview(
            _PRESETS, body={"slot": slot, "value": _slot_value(slot, value)}
        )
        played = reply.get("is_played") if isinstance(reply, dict) else None
        if not isinstance(played, bool):
            raise ProtocolError("invalid preview answer")
        return played

    def _write(self, active: str, custom: tuple[SoundPreset, ...]) -> None:
        self._layer.post_settings(
            body={
                **_GLOBAL,
                "id": _PRESETS,
                "action": "value",
                "value": {
                    "active": active,
                    "custom": [_custom_wire(p) for p in custom],
                },
            }
        )
