"""Session-owned application callables; only declarations and JSON cross the wire."""

from __future__ import annotations

import asyncio
import hashlib
import inspect
import json
import math
import threading
from collections.abc import Mapping
from dataclasses import fields, is_dataclass

import blockether.vis.extension as vis

from ._client import GatewayError, ProtocolError

_MAX_BYTES = 1024 * 1024
_MAX_CALLS = 4096
_MAX_RECEIPT_BYTES = 16 * 1024 * 1024


def _json_value(value, *, depth=0):
    """Copy public result data without object loading, default execution or repr()."""
    if depth > 64:
        raise ValueError("callback result nesting exceeds 64 levels")
    if value is None or isinstance(value, (str, bool, int)):
        return value
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ValueError("callback results require finite numbers")
        return value
    if is_dataclass(value) and not isinstance(value, type):
        value = {field.name: getattr(value, field.name) for field in fields(value)}
    if isinstance(value, Mapping):
        if not all(isinstance(key, str) for key in value):
            raise TypeError("callback result object keys must be strings")
        return {key: _json_value(item, depth=depth + 1) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_value(item, depth=depth + 1) for item in value]
    raise TypeError(f"unsupported callback result type: {type(value).__name__}")


def _json_text(value):
    text = json.dumps(
        value, allow_nan=False, ensure_ascii=True, separators=(",", ":"), sort_keys=True
    )
    if len(text.encode("utf-8")) > _MAX_BYTES:
        raise ValueError("callback payload exceeds 1 MiB")
    return text


def _failure(error):
    try:
        message = str(error)[:4096]
    except Exception:
        message = type(error).__name__
    return {
        "status": "failure",
        "error": {"type": type(error).__name__, "message": message},
    }


class ClientExtensions:
    """Hold one Agent's declarations and invocation receipts on its calling thread."""

    def __init__(self, declarations=()):
        self.declarations = tuple(declarations)
        self.manifest = []
        self._functions = {}
        self._completed = {}
        self._receipt_bytes = 0
        self._thread = threading.get_ident()
        self._draining = False
        self.started = False
        names, aliases = set(), set()
        for extension in self.declarations:
            if not isinstance(extension, vis.Extension):
                raise TypeError("Agent extensions must be Extension declarations")
            unsupported = [
                name
                for name in (
                    "activation",
                    "ctx",
                    "env",
                    "providers",
                    "op_hooks",
                    "network_filters",
                    "slash_commands",
                )
                if getattr(extension, name)
            ]
            if callable(extension.prompt):
                unsupported.append("prompt")
            if unsupported:
                raise ValueError(
                    "client extensions do not support host-only fields: "
                    + ", ".join(unsupported)
                )
            if not extension.symbols:
                raise ValueError(
                    "client extensions require at least one exported symbol"
                )
            if extension.name in names or extension.alias in aliases:
                raise ValueError("duplicate client extension name or alias")
            names.add(extension.name)
            aliases.add(extension.alias)
            symbols = []
            for declaration in extension.symbols:
                spec = declaration._spec()
                if spec["marker"] == "namespace":
                    entries = [
                        {**item, "name": item["contract"]["name"]}
                        for item in spec["methods"]
                    ]
                else:
                    entries = [spec]
                for item in entries:
                    name = item["name"]
                    if any(
                        name == other
                        or name.startswith(other + ".")
                        or other.startswith(name + ".")
                        for other in self._functions
                    ):
                        raise ValueError(f"client symbol collision: {name}")
                    if not item.get("activity"):
                        raise ValueError(
                            f"client symbol {name} requires an explicit Activity"
                        )
                    self._functions[name] = item["fn"]
                    symbols.append(
                        {
                            key: value
                            for key, value in item.items()
                            if key not in ("fn", "marker")
                        }
                    )
            manifest = {
                "name": extension.name,
                "description": extension.description,
                "alias": extension.alias,
                "symbols": symbols,
            }
            for key in ("prompt", "version", "kind"):
                value = getattr(extension, key)
                if value is not None:
                    manifest[key] = value
            self.manifest.append(manifest)
        if len(self._functions) > 256 or len(self.manifest) > 32:
            raise ValueError(
                "client extensions are limited to 32 extensions and 256 symbols"
            )
        _json_text({"extensions": self.manifest})

    def _check_thread(self):
        if threading.get_ident() != self._thread:
            raise RuntimeError("client extensions must run on their SDK calling thread")

    def dispatch(self, call, publish):
        self._check_thread()
        if (
            not isinstance(call, dict)
            or set(call) != {"id", "name", "args", "kwargs"}
            or not isinstance(call["id"], str)
            or not call["id"]
            or not isinstance(call["name"], str)
            or not isinstance(call["args"], list)
            or not isinstance(call["kwargs"], dict)
            or not all(isinstance(key, str) for key in call["kwargs"])
        ):
            raise ProtocolError("invalid client extension call")
        fingerprint = hashlib.sha256(_json_text(call).encode("utf-8")).digest()
        previous = self._completed.get(call["id"])
        if previous is not None:
            if previous[0] != fingerprint:
                raise ProtocolError(
                    "client call id was reused with different arguments"
                )
            return previous[1]
        if (
            len(self._completed) >= _MAX_CALLS
            or self._receipt_bytes >= _MAX_RECEIPT_BYTES
        ):
            return _failure(
                RuntimeError("Agent client callback limit reached; create a new Agent")
            )
        token = vis._activity_publisher.set(publish)
        try:
            try:
                fn = self._functions.get(call["name"])
                if fn is None:
                    raise ValueError("unknown client extension symbol")
                value = fn(*call["args"], **call["kwargs"])
                if inspect.isawaitable(value):
                    try:
                        asyncio.get_running_loop()
                    except RuntimeError:

                        async def resolve():
                            return await value

                        value = asyncio.run(resolve())
                    else:
                        if inspect.iscoroutine(value):
                            value.close()
                        raise RuntimeError(
                            "async client callbacks require a calling thread without a running event loop"
                        )
                result = {"status": "success", "result": _json_value(value)}
                _json_text(result)
            except Exception as error:
                result = _failure(error)
            except BaseException as error:
                result = _failure(error)
                self._completed[call["id"]] = (fingerprint, result)
                self._receipt_bytes += len(_json_text(result))
                raise
            # Retain the receipt BEFORE delivery: retrying a lost ACK must not
            # invoke application code a second time. Never evict executed IDs.
            self._completed[call["id"]] = (fingerprint, result)
            self._receipt_bytes += len(_json_text(result))
            return result
        finally:
            vis._activity_publisher.reset(token)

    def drain(self, client, session_id):
        self._check_thread()
        if self._draining:
            return
        self._draining = True
        try:
            batch = client.get_session_client_calls(session_id)
            if (
                not isinstance(batch, dict)
                or set(batch) != {"calls"}
                or not isinstance(batch["calls"], list)
                or len(batch["calls"]) > 64
            ):
                raise ProtocolError("invalid client extension call batch")
            for call in batch["calls"]:
                # Dispatch validates the entire shape before application code.
                if not isinstance(call, dict) or not isinstance(call.get("id"), str):
                    raise ProtocolError("invalid client extension call")

                def publish(presentation, call_id=call["id"]):
                    client.post_session_client_call_activity(
                        session_id, call_id, body=presentation
                    )
                    return True

                result = self.dispatch(call, publish)
                try:
                    client.post_session_client_call_result(
                        session_id, call["id"], body=result
                    )
                except GatewayError as error:
                    if error.status != 410 or error.code != "client_call_gone":
                        raise
        finally:
            self._draining = False

    def clear(self):
        self.declarations = ()
        self.manifest.clear()
        self._functions.clear()
        self._completed.clear()
        self._receipt_bytes = 0
