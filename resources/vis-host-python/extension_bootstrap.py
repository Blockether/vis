"""Injector — build the `blockether.vis.extension` module for ONE extension context.

The module BODY is `packages/vis-agent/src/blockether/vis/extension.py`, the same file
that PyPI ships as `vis-agent`. `internal.python-extensions/bootstrap-python` reads it
from the classpath and prepends it here as `_vis_body`. So the repository holds exactly
one copy of the extension API, and the sandbox runs the code that an author can `pip
install` and read.

This fragment does only what the package cannot do for itself:

- It seeds `_host` with the host callables that the engine installed into this session.
- It executes the body into a module dict of its own, so the globals of the extension
  file stay clean.
- It registers the module in `sys.modules`, so `import blockether.vis.extension as vis`
  works.

`_host` has one attribute per callable that the engine installed. The outside host
implements the same interface in `blockether.vis._outside`. Boundary tests compare the
actual implementations, without a separate operation catalog.
"""

# ── The host's handle on this extension's Python callables ───────────────────
#
# A callable cannot cross to Clojure: what crosses is JSON text. So every
# callable the registration carries is SEALED here — kept in this module under
# an id, and replaced in the payload by `{"__vis_callable__": id}` — and the
# host invokes it later by that id. The seal is recursive because a spec nests
# callables inside dicts and lists (a provider's `auth_fn`, a symbol's `fn`).

import asyncio as _vis_asyncio
import dataclasses as _vis_dataclasses
import inspect as _vis_inspect
import json as _vis_json
import types as _vis_types

import vis_sdk as _vis_sdk

# The host prepends _vis_body with the canonical SDK source.
_vis_callables = {}
_vis_call_seq = [0]


def __vis_seal__(value):
    """Seal callables and public result data for the process boundary."""
    if callable(value):
        _vis_call_seq[0] += 1
        cid = f"c{_vis_call_seq[0]}"
        _vis_callables[cid] = value
        return {"__vis_callable__": cid}
    if isinstance(value, dict):
        return {str(k): __vis_seal__(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [__vis_seal__(v) for v in value]
    if _vis_dataclasses.is_dataclass(value) and not isinstance(value, type):
        attrs = {f.name: getattr(value, f.name) for f in _vis_dataclasses.fields(value)}
    else:
        attrs = getattr(value, "__dict__", None)
    if isinstance(attrs, dict):
        markers = {}
        if "__vis_sequence_field__" in vars(type(value)):
            field = vars(type(value))["__vis_sequence_field__"]
            if (
                not isinstance(field, str)
                or not field.isidentifier()
                or field.startswith("_")
                or field not in attrs
            ):
                raise ValueError("sequence field must name a public serialized field")
            if type(attrs[field]) not in (list, tuple):
                raise TypeError(
                    f"sequence field {field!r} must contain a built-in list or tuple"
                )
            markers["__vis_sequence_field__"] = field
        sealed = {
            str(k): __vis_seal__(v)
            for k, v in attrs.items()
            if not str(k).startswith("_")
        }
        for name in _vis_mod._derived_names(type(value)):
            # A property that fails stays out of the record; the tool still answers.
            try:
                sealed[name] = __vis_seal__(getattr(value, name))
            except Exception:
                pass
        # A model's own `__str__` text becomes the record's `str` in the sandbox.
        text = _vis_mod._derived_text(value)
        if text is not None:
            markers["__vis_text__"] = text
        return {
            **markers,
            "__vis_object__": type(value).__name__,
            "__vis_attrs__": sealed,
        }
    return value


def __vis_registration__():
    """The sealed registration spec, or None when the file registered nothing."""
    reg = _vis_mod.__dict__.get("_registration")
    return None if reg is None else __vis_seal__(reg["spec"])


def __vis_unseal_host__(value):
    """Answer `value` with every host-callback marker replaced by the host tool.

    The mirror of `__vis_seal__`: a Clojure fn cannot cross either, so the host
    binds it as an ordinary tool in this session and sends the name it bound.
    That name is a global here, which is what makes `printer('...')` — a
    provider's `auth(printer)` — an ordinary call for the extension author.
    """
    if isinstance(value, dict):
        if len(value) == 1 and "__vis_callback__" in value:
            return globals()[value["__vis_callback__"]]
        return {k: __vis_unseal_host__(v) for k, v in value.items()}
    if isinstance(value, list):
        return [__vis_unseal_host__(v) for v in value]
    return value


def __vis_call__(cid, args_json, kwargs_json):
    """Invoke a sealed callable, await its completion, then seal the public result.

    A record argument arrives as its class name and fields; the SDK rebuilds the
    records the callable's annotations declare before the call. An argument that
    does not fit its record fails before the call and names the sha256 of the
    loaded extension source (Blockether/vis#427).
    """
    fn = _vis_callables[cid]
    try:
        args, kwargs = _vis_mod._call_arguments(
            fn,
            __vis_unseal_host__(_vis_json.loads(args_json)),
            __vis_unseal_host__(_vis_json.loads(kwargs_json)),
        )
    except _vis_mod._ArgumentMismatch as error:
        # Name the loaded source, so a caller can tell stale code from a bad value.
        source = globals().get("__vis_source_sha__") or "unknown"
        raise TypeError(f"{error} (extension source sha256 {source[:12]})") from None
    result = fn(*args, **kwargs)
    if _vis_inspect.isawaitable(result):

        async def resolve():
            return await result

        result = _vis_asyncio.run(resolve())
    return __vis_seal__(result)


def __vis_member__(op):
    """Answer `op` with every Python callable in its arguments SEALED.

    A callable cannot cross to Clojure — JSON text crosses — so an argument
    that is one is kept here under an id and sent as its marker, exactly as a
    registration's callables are. `vis.ask` hands the host a validator this
    way, and the host calls it back by that id while the extension is parked
    inside the very call that passed it.
    """

    def call(*args):
        return op(*[__vis_seal__(a) for a in args])

    return call


# Every `__vis_host_*__` name the host injected becomes a member of `_host`,
# under that name with the marker trimmed: `__vis_host_jailed_shell__` is
# `_host.jailed_shell`. The list is NOT written out here on purpose — it lived in
# this file once, so a door added in the host meant a release of this library
# before the host could use it, for a name this file only ever passed through.
_vis_mod = _vis_sdk.install(
    _vis_body,  # noqa: F821 — supplied by the host with the canonical SDK source.
    _vis_types.SimpleNamespace(
        **{
            _vis_name[len("__vis_host_") : -len("__")]: __vis_member__(_vis_door)
            for _vis_name, _vis_door in list(globals().items())
            if _vis_name.startswith("__vis_host_")
            and _vis_name.endswith("__")
            and callable(_vis_door)
        }
    ),
)
