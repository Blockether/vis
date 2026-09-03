"""The Vis extension contract, as data — and the host protocol it declares.

Vis runs an extension's Python inside its own sandbox, with the host seeded into
the module the extension imports. This package is the DECLARATION of that
boundary, published on its own so it can be read where Vis is not: gateway
routes and events, the ops a host answers, shell/View/content/toggle/provider-limits
vocabularies, and [[Host]], the protocol an implementation is checked against.

`contract.json` beside this file is the validated portable contract aggregate.
Its source JSON documents and JSON Schemas live in the `vis-contract` artifact.

Read the ops:

    import vis_contract

    for name, op in vis_contract.OPS.items():
        print(name, op["arity"], op["outside"])

Check a host you built:

    vis_contract.check_host(my_host)   # raises TypeError naming what is missing
"""

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


def _load_contract():
    with open(Path(__file__).with_name("contract.json"), encoding="utf-8") as fh:
        return json.load(fh)


CONTRACT = _load_contract()
"""The whole rendered document: gateway, host ops and portable vocabularies."""

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
    """What an op that cannot be served outside a Vis process says when it refuses.

    None for every op that has an honest local meaning — the contract only writes a
    refusal for the ops it declares `outside == "refuse"`.
    """
    return OPS.get(name, {}).get("refusal")


@runtime_checkable
class Host(Protocol):
    """The whole boundary between the `vis` module and whatever is hosting it.

    Inside Vis the engine seeds an object with these calls bound to the live agent;
    installed from PyPI, `vis-agent`'s `_outside` answers the same names the way
    each op's `outside` says it behaves with no agent in the room. A third host —
    a test double, another editor, a CI harness — is a class with these methods and
    nothing else, and [[check_host]] is how it proves it.

    The method list is the document's, not this file's: `test_contract.py` fails
    when a name, an arity or an op stops matching `contract.json`.
    """

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
