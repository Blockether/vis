"""The canonical Vis contract documents and extension host protocol."""

import json
from collections.abc import Callable, Mapping
from pathlib import Path
from typing import Any, Protocol, runtime_checkable

__all__ = [
    "CONFIG",
    "CONTENT",
    "CONTRACT",
    "GATEWAY",
    "LIVE",
    "OPS",
    "PROVIDER",
    "SHELL",
    "SURFACE",
    "TEST_RUNNER",
    "TOGGLE",
    "VERSION",
    "VIEW",
    "Host",
    "check_host",
    "op",
    "refusal",
]


_DATA = Path(__file__).with_name("data")
if not _DATA.is_dir():
    _DATA = Path(__file__).resolve().parents[3] / "resources" / "vis-contract"


def _load_document(name):
    return json.loads((_DATA / f"{name}.json").read_text(encoding="utf-8"))


_host = _load_document("python-host")
CONTRACT = {
    "version": _host["version"],
    "ops": _host["ops"],
    "shell": _host["shell"],
    "live": _host["live"],
    **{
        name.replace("-", "_"): _load_document(name)
        for name in (
            "gateway",
            "view",
            "content",
            "config",
            "toggle",
            "provider",
            "surface",
            "test-runner",
        )
    },
}
"""All canonical contract documents, keyed as the public Python API expects."""

GATEWAY = CONTRACT["gateway"]
"""Canonical routes, headers, events, envelopes and replay semantics."""

VERSION = CONTRACT["version"]
"""Bumped whenever an op is added, removed or re-shaped."""

OPS = {entry["name"]: entry for entry in CONTRACT["ops"]}
"""Every declared op by name, in document order."""

SHELL = CONTRACT["shell"]
"""The `shell` verb's lifecycle grammar: `default_op`, `handle_ops`, `spawn_ops`."""

LIVE = CONTRACT["live"]
"""The `live` operation vocabulary and flush interval."""

VIEW = CONTRACT["view"]
"""The closed View vocabulary — lifecycle kinds, semantic nodes and their bounds."""

CONTENT = CONTRACT["content"]
"""Canonical content vocabulary."""

CONFIG = CONTRACT["config"]
"""Canonical configuration vocabulary."""

TOGGLE = CONTRACT["toggle"]
"""Canonical toggle vocabulary."""

PROVIDER = CONTRACT["provider"]
"""Canonical provider-limits vocabulary."""

SURFACE = CONTRACT["surface"]
"""Canonical language-surface vocabulary."""

TEST_RUNNER = CONTRACT["test_runner"]
"""Canonical test-runner vocabulary."""


def op(name):
    """The declared op called `name`, or None."""
    return OPS.get(name)


def refusal(name):
    """The refusal message for an op unavailable outside Vis, if any."""
    return OPS.get(name, {}).get("refusal")


@runtime_checkable
class Host(Protocol):
    """Operations every injected or outside `vis` host must implement."""

    def state_get(self, key: str) -> Any:
        """Read one value out of the extension's durable state."""

    def state_put(self, key: str, value: Any) -> Any:
        """Write one JSON value into the extension's durable state."""

    def state_del(self, key: str) -> Any:
        """Drop one key from the extension's durable state."""

    def state_keys(self) -> Any:
        """List every key the extension's durable state holds."""

    def log(self, level: str, message: str) -> Any:
        """Emit one engine log line at a level."""

    def notify(self, text: str, level: str) -> Any:
        """Show one notification on the user's channel."""

    def shell(self, options: Mapping[str, Any]) -> Mapping[str, Any]:
        """Run one shell op — the grammar is [[SHELL]] — and answer the result shape."""

    def jailed_shell(self, options: Mapping[str, Any]) -> Mapping[str, Any]:
        """Run one shell op inside the workspace jail."""

    def jailed_shell_session(self, options: Mapping[str, Any]) -> Mapping[str, Any]:
        """Run one shell op inside a persistent jailed session."""

    def request_input(
        self,
        request_json: str,
        validator_arities_json: str,
        run_validator: Callable[[str, str], str],
    ) -> str:
        """Ask the human, and block until the answer settles or is cancelled."""

    def live(self, envelope_json: str) -> str:
        """Open, patch, read or close one live view — the grammar is [[LIVE]]."""

    def reveal_secret(self, handle: str) -> Any:
        """Resolve a `vis-secret:` handle to its plaintext."""

    def forget_secret(self, handle: str) -> Any:
        """Drop the plaintext a secret handle stands for."""

    def declare_env(self, declarations_json: str) -> str:
        """Resolve the environment variables the extension declared."""


def check_host(host):
    """Refuse a host that does not answer every op the contract declares.

    Answers the host, so a constructor can `return check_host(built)` — the point is
    that an incomplete host fails where it is BUILT, naming the ops it is missing,
    instead of halfway through somebody's extension.
    """
    missing = [name for name in OPS if not callable(getattr(host, name, None))]
    if missing:
        raise TypeError(
            "vis contract v{} declares host ops this host does not answer: {}".format(
                VERSION, ", ".join(missing)
            )
        )
    return host
