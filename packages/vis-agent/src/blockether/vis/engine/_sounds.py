"""Sounds API: the sounds that the gateway machine plays for your turns.

Use `ExecutionLayer.sounds` from the SDK, or `vis.sounds` inside an extension. Both
change the same machine settings that Settings > Sounds shows in the Companion app and
the TUI, so a change from one place shows in all of them.

The gateway plays a cue when a turn that you wait on finishes (`finish`), when Vis waits
for your answer (`input`) and when a turn fails or stops (`error`). Subagent turns are
silent. The gateway process plays the sound, so a gateway on another machine plays it
there.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from ._client import ProtocolError

if TYPE_CHECKING:
    from ._client import ExecutionLayer

CUES = ("finish", "input", "error")
"""The cues, in the order that settings show them."""

OFF = "off"
"""The cue value that plays no sound."""

_ENABLED = "sounds"
_CUE_SETTINGS = {cue: f"sound_{cue}" for cue in CUES}
_GLOBAL = {"scope": "global"}
_UNSET: Any = object()


@dataclass(frozen=True, slots=True)
class SoundSettings:
    """The sound settings of the gateway machine.

    A cue value is `""` for the bundled sound, `"off"` for no sound, or the path of a
    sound file on the gateway machine. Use WAV files: every platform player reads them.
    """

    is_enabled: bool
    finish: str
    input: str
    error: str


def _row(reply: Any, id: str) -> dict[str, Any]:
    if not isinstance(reply, dict) or reply.get("id") != id:
        raise ProtocolError(f"invalid {id} setting answer")
    return reply


def _cue_value(cue: str, value: Any) -> dict[str, Any]:
    """The settings write for one cue. None goes back to the bundled sound."""
    if value is None:
        return {"action": "inherit"}
    if isinstance(value, os.PathLike):
        value = os.fspath(value)
    if not isinstance(value, str):
        raise TypeError(f"{cue} must be a path, '', 'off' or None")
    if "\n" in value or "\r" in value:
        raise ValueError(f"{cue} must be one line")
    return {"action": "value", "value": value.strip()}


class Sounds:
    """Read and change the sounds that the gateway machine plays."""

    def __init__(self, layer: ExecutionLayer):
        self._layer = layer

    def get(self) -> SoundSettings:
        """Return the current sound settings."""
        enabled = _row(self._layer.get_setting(_ENABLED, query=_GLOBAL), _ENABLED)
        cues = {
            cue: str(
                _row(self._layer.get_setting(id, query=_GLOBAL), id).get("value") or ""
            )
            for cue, id in _CUE_SETTINGS.items()
        }
        return SoundSettings(is_enabled=bool(enabled.get("enabled")), **cues)

    def set(
        self,
        *,
        enabled: bool = _UNSET,
        finish: str | os.PathLike[str] | None = _UNSET,
        input: str | os.PathLike[str] | None = _UNSET,
        error: str | os.PathLike[str] | None = _UNSET,
    ) -> SoundSettings:
        """Change the given settings and return all of them.

        `enabled` turns every cue on or off. A cue takes a file path on the gateway
        machine (`~` expands there), `"off"` for no sound, or `None` (or `""`) for the
        bundled sound. Arguments that you omit do not change. The gateway checks a path
        when it plays the cue: a path that names no file logs a warning and plays nothing.
        """
        writes: list[dict[str, Any]] = []
        if enabled is not _UNSET:
            if type(enabled) is not bool:
                raise TypeError("enabled must be a bool")
            writes.append({"id": _ENABLED, "action": "value", "value": enabled})
        for cue, value in (("finish", finish), ("input", input), ("error", error)):
            if value is not _UNSET:
                writes.append({"id": _CUE_SETTINGS[cue], **_cue_value(cue, value)})
        for write in writes:
            self._layer.post_settings(body={**_GLOBAL, **write})
        return self.get()
