"""The Vis extension API.

The engine injects the host declared by `vis-contract`; an installed wheel uses
`blockether.vis._outside` for local behavior, terminal prompts, and explicit jail refusals.
"""

from __future__ import annotations
import __future__

import ast
import builtins
import inspect
import json
import math
import sys
import time
from collections.abc import Callable, Mapping, Sequence
from collections.abc import MutableMapping as _MutableMapping
from contextlib import contextmanager
from dataclasses import MISSING, dataclass, field, fields, is_dataclass
from os import PathLike
from types import FunctionType, MappingProxyType, MethodType, ModuleType, UnionType
from typing import (
    Annotated,
    Any,
    ClassVar,
    Literal,
    Optional,
    Protocol,
    TypeAlias,
    Union,
    get_args,
    get_origin,
    runtime_checkable,
)


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
        """Run one canonical shell operation and return its result shape."""

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
        """Open, patch, read or close a live view.

        `state` accepts timeout_ms (0..86400000) and after_seq (nonnegative).
        A positive timeout blocks until the sequence differs or the view closes.
        An unchanged timeout returns is_open=True, timed_out=True, without view.
        Ordinary state reads and changed waits return the current view with seq.
        """

    def activity(self, presentation: dict[str, Any]) -> bool:
        """Replace the running symbol's headline, summary, content and sections."""

    def reveal_secret(self, handle: str) -> Any:
        """Resolve a `vis-secret:` handle to its plaintext."""

    def forget_secret(self, handle: str) -> Any:
        """Drop the plaintext a secret handle stands for."""

    def declare_env(self, declarations_json: str) -> str:
        """Resolve the environment variables the extension declared."""


try:
    _host  # noqa: B018, F821 — the host seeds this into the module dict before exec.
except NameError:  # Installed from PyPI: no host in the room, so bring one.
    from blockether.vis import _outside as outside

    _host = outside.host


# Hosted SDK code has no package-loader dependency; cross-language contract tests
# pin this vocabulary to activity.json, just as they pin the live-node table.
_ACTIVITY_PRESENTERS = (
    "generic",
    "shell",
    "tests",
    "patch",
    "observation",
    "lint",
    "repl",
    "format",
    "list",
)


def _bounded_text(value, limit, name):
    if not isinstance(value, str) or len(value) > limit:
        raise ValueError(f"{name} must be text of at most {limit} characters")


def _wire_value(value):
    if isinstance(value, Mapping):
        return {key: _wire_value(item) for key, item in value.items()}
    if isinstance(value, (tuple, list)):
        return [_wire_value(item) for item in value]
    return value


def _freeze_config(value):
    if isinstance(value, Mapping):
        if not all(isinstance(key, str) for key in value):
            raise TypeError("provider JSON keys must be strings")
        return MappingProxyType(
            {key: _freeze_config(item) for key, item in value.items()}
        )
    if isinstance(value, (tuple, list)):
        return tuple(_freeze_config(item) for item in value)
    if value is None or type(value) in (str, bool, int):
        return value
    if type(value) is float and math.isfinite(value):
        return value
    raise TypeError("provider data must contain portable JSON values")


class _ActivityBlock:
    __slots__ = ()
    type: ClassVar[str]

    def to_wire(self) -> dict[str, Any]:
        """Return a fresh canonical Activity content block, with no host lifecycle fields."""
        return {
            "type": self.type,
            **{
                field.name: _wire_value(getattr(self, field.name))
                for field in fields(self)
                if getattr(self, field.name) is not None
            },
        }


@dataclass(frozen=True, slots=True)
class ActivityText(_ActivityBlock):
    """Plain Activity text, never a model message or a question."""

    text: str
    type: ClassVar[str] = "text"

    def __post_init__(self):
        _bounded_text(self.text, 16384, "Activity text")


@dataclass(frozen=True, slots=True)
class ActivityHeading(ActivityText):
    """Heading for a section of this invocation's current content."""

    type: ClassVar[str] = "heading"


@dataclass(frozen=True, slots=True)
class ActivityMarkdown(ActivityText):
    """Markdown content for this invocation."""

    type: ClassVar[str] = "markdown"


@dataclass(frozen=True, slots=True)
class ActivityCode(ActivityText):
    """Code with an optional language hint."""

    language: str | None = None
    type: ClassVar[str] = "code"

    def __post_init__(self):
        ActivityText.__post_init__(self)
        if self.language is not None:
            _bounded_text(self.language, 256, "Activity language")


@dataclass(frozen=True, slots=True)
class ActivityDiff(ActivityCode):
    """A diff, not engine-observed file-change evidence."""

    type: ClassVar[str] = "diff"


@dataclass(frozen=True, slots=True)
class ActivityTable(_ActivityBlock):
    """A bounded rectangular table; input lists are snapshotted as tuples."""

    columns: tuple[str, ...]
    rows: tuple[tuple[str, ...], ...]
    type: ClassVar[str] = "table"

    def __post_init__(self):
        if (
            not isinstance(self.columns, (tuple, list))
            or not 1 <= len(self.columns) <= 16
        ):
            raise ValueError("Activity table requires 1..16 columns")
        if not isinstance(self.rows, (tuple, list)) or len(self.rows) > 200:
            raise ValueError("Activity table allows at most 200 rows")
        for column in self.columns:
            _bounded_text(column, 256, "Activity column")
        for row in self.rows:
            if not isinstance(row, (tuple, list)) or len(row) != len(self.columns):
                raise ValueError("Activity table row width must match its columns")
            for cell in row:
                _bounded_text(cell, 256, "Activity cell")
        object.__setattr__(self, "columns", tuple(self.columns))
        object.__setattr__(self, "rows", tuple(tuple(row) for row in self.rows))


@dataclass(frozen=True, slots=True)
class ActivityFile(_ActivityBlock):
    """Reference an existing attachment, never an external media URL."""

    attachment_id: str
    label: str
    type: ClassVar[str] = "file"

    def __post_init__(self):
        _bounded_text(self.attachment_id, 256, "Activity attachment id")
        if not self.attachment_id.strip():
            raise ValueError("Activity attachment id must not be blank")
        _bounded_text(self.label, 256, "Activity label")


@dataclass(frozen=True, slots=True)
class ActivityImage(ActivityFile):
    """Display an existing image attachment."""

    type: ClassVar[str] = "image"


@dataclass(frozen=True, slots=True)
class ActivityVideo(ActivityFile):
    """Display an existing video attachment."""

    type: ClassVar[str] = "video"


@dataclass(frozen=True, slots=True)
class ActivityAudio(ActivityFile):
    """Display an existing audio attachment."""

    type: ClassVar[str] = "audio"


@dataclass(frozen=True, slots=True)
class ActivityProgress(_ActivityBlock):
    """Progress with paired value/total, or an indeterminate indicator when omitted."""

    label: str
    value: int | float | None = None
    total: int | float | None = None
    type: ClassVar[str] = "progress"

    def __post_init__(self):
        _bounded_text(self.label, 256, "Activity label")
        if self.value is None and self.total is None:
            return
        if (
            type(self.value) not in (int, float)
            or type(self.total) not in (int, float)
            or not math.isfinite(self.value)
            or not math.isfinite(self.total)
            or not 0 <= self.value <= self.total
            or self.total <= 0
        ):
            raise ValueError(
                "Activity progress requires finite 0 <= value <= total and total > 0"
            )


ActivityBlock: TypeAlias = (
    ActivityText
    | ActivityHeading
    | ActivityMarkdown
    | ActivityCode
    | ActivityDiff
    | ActivityTable
    | ActivityFile
    | ActivityImage
    | ActivityVideo
    | ActivityAudio
    | ActivityProgress
)


@dataclass(frozen=True, slots=True)
class ActivitySection:
    """A visible headline and one-line summary; only content waits behind disclosure."""

    headline: str
    summary: str
    content: tuple[ActivityBlock, ...] = ()

    def __post_init__(self):
        for name in ("headline", "summary"):
            value = getattr(self, name)
            _bounded_text(value, 512, f"Activity {name}")
            if len(value.encode("utf-8")) > 512 or any(
                ord(c) < 32 or ord(c) in (127, 8232, 8233) for c in value
            ):
                raise ValueError(
                    f"Activity {name} must be one line of at most 512 UTF-8 bytes"
                )
        if not self.headline:
            raise ValueError("Activity headline must not be empty")
        if (
            not isinstance(self.content, (tuple, list))
            or len(self.content) > 32
            or any(not isinstance(block, ActivityBlock) for block in self.content)
        ):
            raise TypeError("Activity content must contain at most 32 typed blocks")
        object.__setattr__(self, "content", tuple(self.content))

    def to_wire(self) -> dict[str, Any]:
        """Return fresh portable data, without engine-owned lifecycle fields."""
        return {
            "headline": self.headline,
            "summary": self.summary,
            "content": [block.to_wire() for block in self.content],
        }


@dataclass(frozen=True, slots=True)
class ActivityPresentation(ActivitySection):
    """One atomic symbol presentation: up to 8 sections, 32 total blocks and 32 KiB."""

    sections: tuple[ActivitySection, ...] = ()

    def __post_init__(self):
        ActivitySection.__post_init__(self)
        if (
            not isinstance(self.sections, (tuple, list))
            or len(self.sections) > 8
            or any(type(section) is not ActivitySection for section in self.sections)
        ):
            raise TypeError(
                "Activity sections must contain at most 8 non-nested ActivitySections"
            )
        object.__setattr__(self, "sections", tuple(self.sections))
        if (
            len(self.content) + sum(len(section.content) for section in self.sections)
            > 32
        ):
            raise ValueError("Activity presentation exceeds 32 total blocks")
        if (
            len(
                json.dumps(
                    self.to_wire(), ensure_ascii=False, separators=(",", ":")
                ).encode()
            )
            > 32768
        ):
            raise ValueError("Activity presentation exceeds 32 KiB")

    def to_wire(self) -> dict[str, Any]:
        value = ActivitySection.to_wire(self)
        if self.sections:
            value["sections"] = [section.to_wire() for section in self.sections]
        return value


@dataclass(frozen=True, slots=True)
class Activity:
    """Human-facing symbol presentation; the engine owns identity, timing and outcome.

    Declare Activity on every exported callable, including each object method.
    Write understandable English for people, not Python identifiers or object reprs.
    Labels and headlines use sentence case ("Read file", "Run tests"), preserving
    proper names and acronyms. Summaries explain the target, useful counts or outcome.
    Use render or publish_activity for selected content; return values stay independent.

    show_start=False makes a fast operation end-only: no running row or start callback.
    Use it for quick reads, patches and local lookups. Keep show_start=True for work
    people wait for, such as tests, network requests or transfers. Internal start/end
    tracking still preserves ordering, timing, errors and cancellation. Published
    content is retained but stays hidden until an end-only invocation settles.
    The final presentation must stand alone: name the target and outcome, including
    empty results or failures reported as return values. A normally returned failed
    workflow is still a failed workflow; "Completed" alone is not enough. Label
    partial lists and excerpts, and retain useful counts, errors and changes.

    render is an optional synchronous callback receiving phase, args, kwargs,
    result and error as keyword arguments; it returns an ActivityPresentation
    (or None to keep the current presentation). It runs on success and failure,
    and on start only when show_start=True. Use publish_activity for intermediate
    stages of long-running tools. Rendering failures never change returns or errors.
    """

    presenter: str = "generic"
    label: str | None = None
    render: Callable[..., ActivityPresentation | None] | None = None
    show_start: bool = True

    def __post_init__(self):
        if not isinstance(self.show_start, bool):
            raise TypeError("Activity show_start must be a boolean")
        if self.render is not None and (
            not callable(self.render) or inspect.iscoroutinefunction(self.render)
        ):
            raise TypeError("Activity render must be a synchronous callable")
        if self.presenter not in _ACTIVITY_PRESENTERS:
            raise ValueError("unknown Activity presenter")
        if self.label is not None and (
            not isinstance(self.label, str)
            or not self.label.strip()
            or len(self.label) > 96
            or "\n" in self.label
            or "\r" in self.label
        ):
            raise ValueError(
                "Activity label must be one nonblank line of at most 96 characters"
            )


def _activity_spec(activity):
    if activity is None:
        return None
    if not isinstance(activity, Activity):
        raise TypeError("activity must be a vis.Activity declaration")
    return {
        "presenter": activity.presenter,
        "show_start": activity.show_start,
        **({"label": activity.label} if activity.label is not None else {}),
    }


def publish_activity(presentation: ActivityPresentation) -> bool:
    """Replace headline, summary, content and sections without authoring lifecycle.

    Headline and summary stay visible when collapsed. Content uses typed text,
    Markdown, table, code, diff, attachment and progress blocks. Sections group
    multiple results with one blank line between them. To clear content, publish
    the same headline/summary with empty content. None from render keeps the last
    snapshot. Returns False outside an invocation or when publication is rejected.
    """
    try:
        if not isinstance(presentation, ActivityPresentation):
            return False
        return bool(_host.activity(presentation.to_wire()))
    except Exception:
        return False


def _activity_call(fn, activity):
    _activity_spec(activity)
    if activity is None or activity.render is None:
        return fn

    def render(phase, args, kwargs, result=None, error=None):
        try:
            presentation = activity.render(
                phase=phase, args=args, kwargs=kwargs, result=result, error=error
            )
            if presentation is not None:
                publish_activity(presentation)
        except Exception:
            pass  # Presentation must not alter the operation's result or error.

    if inspect.iscoroutinefunction(fn):

        async def invoke(*args, **kwargs):
            if activity.show_start:
                render("start", args, kwargs)
            try:
                result = await fn(*args, **kwargs)
            except BaseException as error:
                render("failure", args, kwargs, error=error)
                raise
            render("success", args, kwargs, result=result)
            return result
    else:

        def invoke(*args, **kwargs):
            if activity.show_start:
                render("start", args, kwargs)
            try:
                result = fn(*args, **kwargs)
            except BaseException as error:
                render("failure", args, kwargs, error=error)
                raise
            render("success", args, kwargs, result=result)
            return result

    from functools import wraps

    return wraps(fn)(invoke)


_registration = {"spec": None}


@dataclass(frozen=True, slots=True, kw_only=True)
class Extension:
    """A pure extension declaration. Only register() binds its environment and host.

    Collection inputs are copied into tuples. Symbols require an explicit alias;
    each child must be its corresponding SDK declaration, never a marker dict.
    """

    name: str
    description: str
    version: str | None = None
    kind: str | None = None
    alias: str | None = None
    activation: Callable[..., Any] | None = None
    symbols: Sequence[Symbol] = ()
    prompt: str | Callable[..., Any] | None = None
    slash_commands: Sequence[SlashCommand] = ()
    op_hooks: Sequence[OpHook] = ()
    ctx: Callable[..., Any] | None = None
    providers: Sequence[Provider] = ()
    network_filters: Sequence[NetworkFilter] = ()
    env: Sequence[str] = ()

    def __post_init__(self):
        if not isinstance(self.name, str) or not self.name.strip():
            raise ValueError("vis.Extension requires name=<non-empty string>")
        if not isinstance(self.description, str) or not self.description.strip():
            raise ValueError("vis.Extension requires description=<non-empty string>")
        if self.symbols and (not isinstance(self.alias, str) or not self.alias.strip()):
            raise ValueError(
                "vis.Extension requires alias=<string> when symbols are declared"
            )
        for name in ("version", "kind", "alias"):
            if getattr(self, name) is not None and not isinstance(
                getattr(self, name), str
            ):
                raise TypeError(f"vis.Extension {name} must be a string")
        for name in ("ctx", "activation"):
            if getattr(self, name) is not None and not callable(getattr(self, name)):
                raise ValueError(f"vis.Extension {name} must be a callable")
        if (
            self.prompt is not None
            and not isinstance(self.prompt, str)
            and not callable(self.prompt)
        ):
            raise TypeError("vis.Extension prompt must be text or a callable")
        for name, cls in (
            ("symbols", Symbol),
            ("slash_commands", SlashCommand),
            ("op_hooks", OpHook),
            ("providers", Provider),
            ("network_filters", NetworkFilter),
        ):
            value = getattr(self, name)
            if not isinstance(value, (tuple, list)) or not all(
                isinstance(item, cls) for item in value
            ):
                raise TypeError(
                    f"vis.Extension {name} must contain {cls.__name__} declarations"
                )
            object.__setattr__(self, name, tuple(value))
        if not isinstance(self.env, (tuple, list)):
            raise ValueError(
                "vis.Extension env must be a list or tuple of environment variable names"
            )
        for name in self.env:
            if (
                not isinstance(name, str)
                or not name
                or name[0].isdigit()
                or not all(c.isalnum() or c == "_" for c in name)
            ):
                raise ValueError(
                    "vis.Extension env entries must be environment variable names"
                )
        object.__setattr__(self, "env", tuple(self.env))

    def _spec(self):
        children = {
            "symbols",
            "slash_commands",
            "op_hooks",
            "providers",
            "network_filters",
        }
        return {
            field.name: (
                [item._spec() for item in getattr(self, field.name)]
                if field.name in children
                else list(self.env)
                if field.name == "env"
                else getattr(self, field.name)
            )
            for field in fields(self)
        }


def register(extension: Extension) -> None:
    """Register one typed declaration and resolve its declared environment in this context.

    Construction is pure; registration is the sole host boundary. A failure before
    completion leaves the context unregistered. No constructor registers itself.
    """
    if not isinstance(extension, Extension):
        raise TypeError("vis.register requires an Extension declaration")
    if _registration["spec"] is not None:
        raise ValueError(
            "vis.register() may only be called once per file; "
            f"extension {_registration['spec']['name']!r} is already registered. "
            "Keep a single registration in the entrypoint. "
            "If this happened during an import, the entrypoint may be shadowing "
            "a package or module with the same name. "
            "Rename the entrypoint (e.g. demo.py -> demo_bridge.py); "
            "the public alias can stay unchanged."
        )
    spec = extension._spec()
    import os

    resolved = json.loads(_host.declare_env(json.dumps(spec["env"]))) or {}
    for name, value in resolved.items():
        os.environ[str(name)] = str(value)
    _registration["spec"] = spec


def host_env(name, default=None):
    # Value of a host environment variable DECLARED in vis.Extension(env=[...]).
    # Undeclared names always return `default` -- declaring is the only way in.
    import os as _os

    v = _os.environ.get(str(name))
    return default if v is None else v


def _annotation_target(fn):
    """Follow the signature's wrapper chain while preserving method binding."""
    target = inspect.unwrap(fn, stop=lambda f: hasattr(f, "__signature__"))
    if inspect.ismethod(fn) and inspect.isfunction(target):
        return MethodType(target, fn.__self__)
    return target


def _inert_signature(fn):
    fn = _annotation_target(fn)
    # Even annotationlib.Format.STRING can execute deferred annotations in 3.14.
    # Inspect a structural clone; future/stringized annotations need no clone.
    target = fn.__func__ if inspect.ismethod(fn) else fn
    if (
        inspect.isfunction(target)
        and getattr(target, "__annotate__", None) is not None
        and not target.__code__.co_flags & __future__.annotations.compiler_flag
    ):
        clone = FunctionType(
            target.__code__,
            target.__globals__,
            target.__name__,
            target.__defaults__,
            target.__closure__,
        )
        clone.__kwdefaults__ = target.__kwdefaults__
        clone.__annotations__ = {}
        actual = MethodType(clone, fn.__self__) if inspect.ismethod(fn) else clone
        signature = inspect.signature(actual, eval_str=False)
        unknown = "deferred annotation; use from __future__ import annotations"
        return signature.replace(
            parameters=[
                p.replace(annotation=unknown) for p in signature.parameters.values()
            ],
            return_annotation=unknown,
        )
    return inspect.signature(fn, eval_str=False)


def _annotation_name(node, namespace):
    """Resolve names statically; never import modules or run annotation expressions."""
    if isinstance(node, ast.Name):
        return namespace.get(
            node.id, vars(builtins).get(node.id, inspect.Signature.empty)
        )
    if isinstance(node, ast.Attribute):
        parent = _annotation_name(node.value, namespace)
        if isinstance(parent, ModuleType) or inspect.isclass(parent):
            return vars(parent).get(node.attr, inspect.Signature.empty)
    return inspect.Signature.empty


def _contract_type(annotation, namespace, seen=()):
    if annotation is inspect.Signature.empty or annotation is Any:
        return {"kind": "any", "name": "Any"}
    if annotation is None or annotation is type(None):
        return {"kind": "null", "name": "None"}
    if isinstance(annotation, str):
        if annotation in seen:
            return {"kind": "reference", "name": annotation}
        seen = (*seen, annotation)
        try:
            node = ast.parse(annotation, mode="eval").body
        except (SyntaxError, ValueError):
            return {"kind": "unresolved", "name": annotation}
        return _contract_ast(node, namespace, seen)
    origin = get_origin(annotation)
    args = get_args(annotation)
    if origin is Annotated:
        result = _contract_type(args[0], namespace, seen)
        descriptions = [value for value in args[1:] if type(value) is str]
        if descriptions:
            result["description"] = "\n".join(descriptions)
        return result
    if origin in (Union, UnionType):
        return {
            "kind": "union",
            "name": "union",
            "arguments": [_contract_type(a, namespace, seen) for a in args],
        }
    if origin is Literal:
        return {
            "kind": "literal",
            "name": "Literal",
            "values": [a for a in args if type(a) in (str, int, bool, type(None))],
        }
    if origin is tuple and len(args) == 2 and args[1] is Ellipsis:
        return {
            "kind": "generic",
            "name": "tuple",
            "variadic": True,
            "arguments": [_contract_type(args[0], namespace, seen)],
        }
    if origin is not None:
        return {
            "kind": "generic",
            "name": getattr(origin, "__name__", "generic"),
            "arguments": [_contract_type(a, namespace, seen) for a in args],
        }
    if inspect.isclass(annotation):
        name = annotation.__name__
        if annotation in seen:
            return {"kind": "reference", "name": name}
        if is_dataclass(annotation):
            module = sys.modules.get(annotation.__module__)
            scope = vars(module) if module else namespace
            doc = inspect.getdoc(annotation) or ""
            result = {"kind": "record", "name": name, "fields": []}
            if doc and not doc.startswith(name + "("):
                result["description"] = doc
            for item in fields(annotation):
                if not item.name.startswith("_"):
                    has_default = (
                        item.default is not MISSING
                        or item.default_factory is not MISSING
                    )
                    result["fields"].append(
                        {
                            "name": item.name,
                            "type": _contract_type(
                                item.type, scope, (*seen, annotation)
                            ),
                            "required": not has_default,
                            "has_default": has_default,
                            "default_is_none": item.default is None,
                        }
                    )
            return result
        return {
            "kind": "scalar"
            if annotation in (str, int, float, bool, bytes)
            else "opaque",
            "name": name,
        }
    return {"kind": "unresolved", "name": type(annotation).__name__}


def _contract_ast(node, namespace, seen):
    if isinstance(node, ast.Constant):
        if node.value is None:
            return _contract_type(None, namespace, seen)
        if isinstance(node.value, str):
            return _contract_type(node.value, namespace, seen)
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.BitOr):
        return {
            "kind": "union",
            "name": "union",
            "arguments": [
                _contract_ast(n, namespace, seen) for n in (node.left, node.right)
            ],
        }
    if isinstance(node, ast.Subscript):
        base = _annotation_name(node.value, namespace)
        nodes = node.slice.elts if isinstance(node.slice, ast.Tuple) else [node.slice]
        if base is Annotated and nodes:
            result = _contract_ast(nodes[0], namespace, seen)
            descriptions = [
                n.value
                for n in nodes[1:]
                if isinstance(n, ast.Constant) and type(n.value) is str
            ]
            if descriptions:
                result["description"] = "\n".join(descriptions)
            return result
        if base is Literal:
            return {
                "kind": "literal",
                "name": "Literal",
                "values": [
                    n.value
                    for n in nodes
                    if isinstance(n, ast.Constant)
                    and type(n.value) in (str, int, bool, type(None))
                ],
            }
        if base in (Union, Optional):
            arguments = [_contract_ast(n, namespace, seen) for n in nodes]
            if base is Optional:
                arguments.append(_contract_type(None, namespace, seen))
            return {"kind": "union", "name": "union", "arguments": arguments}
        if base is not inspect.Signature.empty and (
            get_origin(base) is not None
            or base in (list, tuple, dict, set, frozenset, Sequence, Mapping, PathLike)
        ):
            actual = get_origin(base) or base
            if (
                actual is tuple
                and len(nodes) == 2
                and isinstance(nodes[1], ast.Constant)
                and nodes[1].value is Ellipsis
            ):
                return {
                    "kind": "generic",
                    "name": "tuple",
                    "variadic": True,
                    "arguments": [_contract_ast(nodes[0], namespace, seen)],
                }
            return {
                "kind": "generic",
                "name": actual.__name__,
                "arguments": [_contract_ast(n, namespace, seen) for n in nodes],
            }
    resolved = _annotation_name(node, namespace)
    if resolved is not inspect.Signature.empty:
        return _contract_type(resolved, namespace, seen)
    return {"kind": "unresolved", "name": ast.unparse(node)}


def _callable_contract(fn, name, tag, doc):
    # No evaluation or imports are needed to derive the portable description.
    signature = _inert_signature(fn)
    target = _annotation_target(fn)
    namespace = getattr(target, "__globals__", {})
    parameters, safe = [], []
    for item in signature.parameters.values():
        has_default = item.default is not inspect.Parameter.empty
        parameters.append(
            {
                "name": item.name,
                "kind": item.kind.name.lower(),
                "required": not has_default
                and item.kind not in (item.VAR_POSITIONAL, item.VAR_KEYWORD),
                "has_default": has_default,
                "default_is_none": item.default is None,
                "type": _contract_type(item.annotation, namespace),
            }
        )
        safe.append(
            item.replace(
                annotation=inspect.Parameter.empty,
                default=(None if item.default is None else ...)
                if has_default
                else inspect.Parameter.empty,
            )
        )
    return {
        "version": 1,
        "name": name,
        "tag": tag,
        "description": doc,
        "signature": str(
            signature.replace(
                parameters=safe, return_annotation=inspect.Signature.empty
            )
        )[1:-1].replace("=Ellipsis", "=..."),
        "parameters": parameters,
        "returns": _contract_type(signature.return_annotation, namespace),
    }


def _contract_type_text(spec):
    arguments = spec.get("arguments", [])
    if spec["kind"] in ("unresolved", "opaque"):
        return f"{spec['name']} ({spec['kind']})"
    if spec["kind"] == "union":
        return " | ".join(_contract_type_text(a) for a in arguments)
    if arguments:
        return (
            spec["name"]
            + "["
            + ", ".join(_contract_type_text(a) for a in arguments)
            + (", ..." if spec.get("variadic") else "")
            + "]"
        )
    return spec["name"]


def _contract_field_docs(spec, prefix=""):
    for item in spec.get("fields", []):
        typ = item["type"]
        name = prefix + item["name"]
        note = typ.get("description", "")
        yield f"- {name}: {_contract_type_text(typ)}" + (f" — {note}" if note else "")
        yield from _contract_field_docs(typ, name + ".")
    for argument in spec.get("arguments", []):
        yield from _contract_field_docs(argument, prefix)


def _contract_doc(contract):
    lines = (
        [contract["description"], "", "Parameters:"]
        if contract["parameters"]
        else [contract["description"]]
    )
    for item in contract["parameters"]:
        typ = item["type"]
        note = typ.get("description", "")
        lines.append(
            f"- {item['name']}: {_contract_type_text(typ)}"
            + (f" — {note}" if note else "")
        )
        lines.extend(_contract_field_docs(typ, item["name"] + "."))
    result = contract["returns"]
    lines.extend(["", "Returns: " + _contract_type_text(result)])
    if result.get("description"):
        lines.append(result["description"])
    lines.extend(_contract_field_docs(result))
    return "\n".join(lines)


def _symbol_spec(fn, name, tag, is_hidden, activity=None):
    if not callable(fn):
        raise ValueError("vis.Symbol(fn, ...) requires a callable")
    public_name = name or fn.__name__
    if (
        not isinstance(public_name, str)
        or not public_name.isidentifier()
        or public_name.startswith("_")
    ):
        raise ValueError(
            f"vis.Symbol name must be a public Python identifier, got {public_name!r}"
        )
    if tag not in ("observation", "mutation"):
        raise ValueError(f"vis.Symbol tag must be observation or mutation, got {tag!r}")
    doc = inspect.getdoc(fn)
    if not doc or not doc.strip():
        raise ValueError(
            "vis.Symbol: {} needs a docstring - it becomes the model-facing doc()".format(
                getattr(fn, "__name__", "?")
            )
        )
    contract = _callable_contract(fn, public_name, tag, doc)
    params = [
        p["name"]
        for p in contract["parameters"]
        if p["kind"] in ("positional_only", "positional_or_keyword")
    ]
    varargs = any(p["kind"] == "var_positional" for p in contract["parameters"])
    return {
        "marker": "symbol",
        "fn": _activity_call(fn, activity),
        "name": public_name,
        "tag": tag,
        "hidden": bool(is_hidden),
        "doc": _contract_doc(contract),
        "contract": contract,
        "params": params,
        "varargs": varargs,
        **({"activity": _activity_spec(activity)} if activity is not None else {}),
    }


def method(fn=None, *, tag="observation", is_hidden=False, activity=None):
    """Declare per-method tool metadata for an object exported by vis.Symbol()."""
    if tag not in ("observation", "mutation"):
        raise ValueError(f"vis.method tag must be observation or mutation, got {tag!r}")
    _activity_spec(activity)

    def _mark(actual):
        declared = (
            actual.__func__
            if isinstance(actual, (staticmethod, classmethod))
            else actual
        )
        if not callable(declared):
            raise ValueError("vis.method(...) requires a callable method")
        declared.__vis_symbol_tag__ = tag
        declared.__vis_symbol_hidden__ = bool(is_hidden)
        declared.__vis_symbol_activity__ = activity
        return actual

    return _mark if fn is None else _mark(fn)


def _public_members(obj):
    """Return public attributes without invoking arbitrary descriptors."""
    candidates = {}
    for cls in reversed(type(obj).__mro__):
        for name, raw in vars(cls).items():
            if name.startswith("_"):
                continue
            declared = (
                raw.__func__ if isinstance(raw, (staticmethod, classmethod)) else raw
            )
            candidates[name] = (
                "method" if inspect.isroutine(declared) else "value",
                declared,
            )
    try:
        own = vars(obj)
    except TypeError:
        own = {}
    for name, value in own.items():
        if not name.startswith("_"):
            candidates[name] = ("value", value)
    rows = []
    for name, (kind, raw) in candidates.items():
        if kind == "method":
            rows.append((name, getattr(obj, name), raw))
        elif callable(raw):
            rows.append((name, raw, raw))
        else:
            rows.append((name, raw, None))
    return rows


def _is_namespace_object(value):
    return (
        value is not None
        and type(value).__module__ != "builtins"
        and not inspect.ismodule(value)
        and not inspect.isclass(value)
        and not callable(value)
    )


def _object_symbol_specs(obj, path, tag, is_hidden, seen):
    object_id = id(obj)
    previous = seen.get(object_id)
    if previous is not None:
        raise ValueError(
            f"vis.Symbol(object, ...): public attribute {path!r} repeats the object "
            f"already exposed at {previous!r}; cycles and repeated references are not "
            "supported"
        )
    seen[object_id] = path
    specs = []
    for member_name, value, declared in _public_members(obj):
        member_path = f"{path}.{member_name}"
        if callable(value):
            raw = declared or value
            method_tag = getattr(raw, "__vis_symbol_tag__", tag)
            method_hidden = getattr(raw, "__vis_symbol_hidden__", is_hidden)
            spec = _symbol_spec(
                value,
                member_name,
                method_tag,
                method_hidden,
                getattr(raw, "__vis_symbol_activity__", None),
            )
            spec["name"] = member_path.split(".", 1)[1]
            spec["contract"]["name"] = member_path
            specs.append(spec)
        elif _is_namespace_object(value):
            specs.extend(_object_symbol_specs(value, member_path, tag, is_hidden, seen))
        else:
            raise ValueError(
                f"vis.Symbol(object, ...): public attribute {member_path!r} has unsupported "
                f"value of type {type(value).__name__}; expose callable methods, nest a "
                "capability object, or make the attribute private"
            )
    return specs


@dataclass(frozen=True, slots=True)
class Symbol:
    """Expose a function or object namespace. Object Activity belongs on each @method."""

    fn: Callable[..., Any] | object
    name: str | None = None
    tag: Literal["observation", "mutation"] = "observation"
    is_hidden: bool = False
    activity: Activity | None = None

    def __post_init__(self):
        if type(self.is_hidden) is not bool:
            raise TypeError("vis.Symbol is_hidden must be a boolean")
        self._spec()  # Pure validation; adapters are installed only by register().

    @property
    def contract(self) -> dict[str, Any]:
        """Fresh portable tool description; no callable, default values or host access.

        Namespace members carry their full public names. Strings in Annotated
        describe meaning; unresolved annotations remain explicit, never evaluated.
        This is documentation, not runtime argument or result validation.
        """
        spec = self._spec()
        if spec["marker"] == "namespace":
            return {
                "version": 1,
                "name": spec["name"],
                "members": [item["contract"] for item in spec["methods"]],
            }
        return spec["contract"]

    def _spec(self):
        if inspect.isroutine(self.fn):
            return _symbol_spec(
                self.fn, self.name, self.tag, self.is_hidden, self.activity
            )
        if self.tag not in ("observation", "mutation"):
            raise ValueError("vis.Symbol tag must be observation or mutation")
        if self.activity is not None:
            raise ValueError(
                "declare object Activity on each vis.method(), not its namespace"
            )
        if (
            not self.name
            or not isinstance(self.name, str)
            or not self.name.isidentifier()
            or self.name.startswith("_")
        ):
            raise ValueError(
                "vis.Symbol(object, ...) requires name=<public Python identifier>"
            )
        methods = _object_symbol_specs(self.fn, self.name, self.tag, self.is_hidden, {})
        if not methods:
            raise ValueError(
                "vis.Symbol(object, ...) requires at least one public method"
            )
        return {"marker": "namespace", "name": self.name, "methods": methods}


@dataclass(frozen=True, slots=True)
class SlashCommand:
    """A user-facing slash command, not a model-facing tool."""

    name: str
    run: Callable[..., Any]
    doc: str | None = None
    usage: str | None = None

    def __post_init__(self):
        if not isinstance(self.name, str) or not self.name.strip():
            raise ValueError("vis.SlashCommand requires name=<non-empty string>")
        if not callable(self.run):
            raise ValueError("vis.SlashCommand requires a callable run")
        for name in ("doc", "usage"):
            if getattr(self, name) is not None and not isinstance(
                getattr(self, name), str
            ):
                raise TypeError(f"vis.SlashCommand {name} must be text")

    def _spec(self):
        return {
            "marker": "slash",
            "name": self.name,
            "run": self.run,
            "doc": self.doc,
            "usage": self.usage,
        }


GATE_OPS = ("fs_access",)


@dataclass(frozen=True, slots=True)
class OpHook:
    """An operation observer or a fail-closed gate; never mix both kinds in one hook."""

    ops: Sequence[str]
    fn: Callable[..., Any]
    phase: Literal["before", "after"] = "before"

    def __post_init__(self):
        if self.phase not in ("before", "after"):
            raise ValueError("vis.OpHook phase must be before or after")
        if not callable(self.fn):
            raise ValueError("vis.OpHook requires a callable fn")
        if (
            not isinstance(self.ops, (tuple, list))
            or not self.ops
            or not all(isinstance(op, str) and op.strip() for op in self.ops)
        ):
            raise ValueError("vis.OpHook requires a non-empty ops list")
        gates = [op for op in self.ops if op in GATE_OPS]
        if gates and len(gates) != len(self.ops):
            raise ValueError(
                "vis.OpHook: a gate cannot share a hook with ordinary operations"
            )
        if gates and self.phase != "before":
            raise ValueError("vis.OpHook: a gate is asked before the operation runs")
        object.__setattr__(self, "ops", tuple(self.ops))

    def _spec(self):
        return {
            "marker": "op_hook",
            "ops": list(self.ops),
            "fn": self.fn,
            "phase": self.phase,
        }


@dataclass(frozen=True, slots=True)
class NetworkFilter:
    """Request/response policy at the host's network-filter boundary."""

    fn: Callable[..., Any]

    def __post_init__(self):
        if not callable(self.fn):
            raise ValueError("vis.NetworkFilter requires a callable")

    def _spec(self):
        return {"marker": "network_filter", "fn": self.fn}


# Provider vocabularies are pinned to the canonical provider/config JSON in tests.
ProviderJSON: TypeAlias = (
    str
    | int
    | float
    | bool
    | None
    | Mapping[str, "ProviderJSON"]
    | Sequence["ProviderJSON"]
)
ProviderAPIStyle: TypeAlias = Literal[
    "anthropic",
    "anthropic-messages",
    "anthropic_messages",
    "claude",
    "messages",
    "openai",
    "openai-chat",
    "openai_chat",
    "openai-compatible",
    "openai_compatible",
    "openai-compatible-chat",
    "openai_compatible_chat",
    "chat",
    "chat-completions",
    "chat_completions",
    "openai-responses",
    "openai_responses",
    "openai-compatible-responses",
    "openai_compatible_responses",
    "responses",
    "gemini",
    "google",
    "google-gemini",
    "google_gemini",
]
ProviderLimitStatus: TypeAlias = Literal[
    "error", "ok", "unauthenticated", "unknown-provider", "unsupported"
]
ProviderLimitScope: TypeAlias = Literal["account", "model", "plan", "workspace"]
ProviderLimitKind: TypeAlias = Literal[
    "credits", "rate", "requests", "sessions", "tokens", "usd"
]
ProviderWindowKind: TypeAlias = Literal["calendar", "lifetime", "rolling"]
ProviderWindowUnit: TypeAlias = Literal[
    "day", "hour", "minute", "month", "week", "year"
]
ProviderLimitPrecision: TypeAlias = Literal["derived", "estimate", "exact", "unknown"]
ProviderLimitSource: TypeAlias = Literal["derived", "local", "provider-api", "static"]


def _provider_text(value, name, required=False):
    if value is None and not required:
        return
    if not isinstance(value, str) or (required and not value.strip()):
        raise ValueError(f"{name} must be {'nonblank ' if required else ''}text")


def _provider_bool(value, name):
    if type(value) is not bool:
        raise TypeError(f"{name} must be a boolean")


def _provider_enum(value, vocabulary, name):
    if value is not None and (
        not isinstance(value, str) or value not in get_args(vocabulary)
    ):
        raise ValueError(
            f"invalid {name}"
            if name != "api_style"
            else "provider api_style names no wire dialect"
        )


def _provider_number(value, name, integer=False, minimum=None):
    if value is None:
        return
    if (
        type(value) not in ((int,) if integer else (int, float))
        or (isinstance(value, float) and not math.isfinite(value))
        or (minimum is not None and value < minimum)
    ):
        raise ValueError(f"invalid {name}")


def _provider_mapping(value, name, headers=False):
    if value is None:
        return None
    if not isinstance(value, Mapping):
        raise TypeError(f"{name} must be a mapping")
    if headers and any(not isinstance(v, str) for v in value.values()):
        raise TypeError(f"{name} values must be strings")
    return _freeze_config(value)


def _provider_extra(record):
    extra = _provider_mapping(record.extra, "extra")
    if extra is not None and {key.replace("-", "_") for key in extra} & {
        f.name for f in fields(record)
    }:
        raise ValueError("provider extra fields must not shadow declared fields")
    object.__setattr__(record, "extra", extra)


def _provider_rows(value, row_type, name):
    if (
        not isinstance(value, Sequence)
        or isinstance(value, (str, bytes))
        or any(not isinstance(row, row_type) for row in value)
    ):
        raise TypeError(f"{name} must contain {row_type.__name__} values")
    return tuple(value)


def _provider_wire(value):
    if isinstance(value, _ProviderValue):
        return value.to_wire()
    if isinstance(value, (tuple, list)):
        return [_provider_wire(row) for row in value]
    return _wire_value(value)


class _ProviderValue:
    __slots__ = ()

    def to_wire(self) -> dict[str, ProviderJSON]:
        """Return fresh host data; declared optional fields are omitted, not null."""
        value = {
            f.name: _provider_wire(getattr(self, f.name))
            for f in fields(self)
            if f.name != "extra" and getattr(self, f.name) is not None
        }
        value.update(_wire_value(getattr(self, "extra", None) or {}))
        return value


@dataclass(frozen=True, slots=True, kw_only=True)
class ProviderPreset(_ProviderValue):
    """Endpoint defaults, not credentials. Opaque API payload keys remain unchanged.

    extra carries additional router settings as JSON, never overriding named fields.
    Use enrich_models_fn for typed model metadata beyond default model names.
    """

    base_url: str | None = None
    api_style: ProviderAPIStyle | None = None
    default_models: Sequence[str] = ()
    responses_path: str | None = None
    llm_headers: Mapping[str, str] | None = field(default=None, repr=False)
    extra_body: Mapping[str, ProviderJSON] | None = None
    is_hidden: bool | None = None
    extra: Mapping[str, ProviderJSON] | None = field(default=None, repr=False)

    def __post_init__(self):
        _provider_text(self.base_url, "base_url")
        _provider_text(self.responses_path, "responses_path")
        _provider_enum(self.api_style, ProviderAPIStyle, "api_style")
        models = _provider_rows(self.default_models, str, "default_models")
        for model in models:
            _provider_text(model, "default_models", required=True)
        object.__setattr__(self, "default_models", models)
        if self.is_hidden is not None:
            _provider_bool(self.is_hidden, "is_hidden")
        object.__setattr__(
            self,
            "llm_headers",
            _provider_mapping(self.llm_headers, "llm_headers", headers=True),
        )
        object.__setattr__(
            self, "extra_body", _provider_mapping(self.extra_body, "extra_body")
        )
        _provider_extra(self)


@dataclass(frozen=True, slots=True)
class ProviderCredential(_ProviderValue):
    """A usable credential; return None when absent. Tokens/headers are excluded from repr."""

    token: str = field(repr=False)
    api_url: str | None = None
    api_style: ProviderAPIStyle | None = None
    responses_path: str | None = None
    llm_headers: Mapping[str, str] | None = field(default=None, repr=False)
    source: str | None = None

    def __post_init__(self):
        _provider_text(self.token, "token", required=True)
        for name in ("api_url", "responses_path", "source"):
            _provider_text(getattr(self, name), name)
        _provider_enum(self.api_style, ProviderAPIStyle, "api_style")
        object.__setattr__(
            self,
            "llm_headers",
            _provider_mapping(self.llm_headers, "llm_headers", headers=True),
        )


@dataclass(frozen=True, slots=True)
class ProviderStatus(_ProviderValue):
    """Connection verdict, separate from usage limits. extra is display-only JSON metadata."""

    is_authenticated: bool
    error: str | None = None
    source: str | None = None
    provider_id: str | None = None
    status: str | None = None
    base_url: str | None = None
    label: str | None = None
    config_path: str | None = None
    extra: Mapping[str, ProviderJSON] | None = field(default=None, repr=False)

    def __post_init__(self):
        _provider_bool(self.is_authenticated, "is_authenticated")
        for name in (
            "error",
            "source",
            "provider_id",
            "status",
            "base_url",
            "label",
            "config_path",
        ):
            _provider_text(getattr(self, name), name)
        _provider_extra(self)


@dataclass(frozen=True, slots=True)
class ProviderModel(_ProviderValue):
    """Model metadata returned by enrichment; extra preserves additional router fields."""

    name: str
    context: int | None = None
    is_tool_call: bool | None = None
    is_image_input: bool | None = None
    extra: Mapping[str, ProviderJSON] | None = None

    def __post_init__(self):
        _provider_text(self.name, "model name", required=True)
        _provider_number(self.context, "context", integer=True, minimum=1)
        for name in ("is_tool_call", "is_image_input"):
            if getattr(self, name) is not None:
                _provider_bool(getattr(self, name), name)
        _provider_extra(self)


@dataclass(frozen=True, slots=True)
class ProviderLimitWindow(_ProviderValue):
    """The canonical calendar, rolling or lifetime window for one limit."""

    kind: ProviderWindowKind
    unit: ProviderWindowUnit | None = None
    size: int | None = None
    resets_at_ms: int | None = None

    def __post_init__(self):
        _provider_text(self.kind, "window kind", required=True)
        _provider_enum(self.kind, ProviderWindowKind, "window kind")
        _provider_enum(self.unit, ProviderWindowUnit, "window unit")
        _provider_number(self.size, "window size", integer=True, minimum=1)
        _provider_number(self.resets_at_ms, "resets_at_ms", integer=True)


@dataclass(frozen=True, slots=True)
class ProviderLimit(_ProviderValue):
    """One canonical usage row; finite measurements retain their original precision."""

    id: str
    label: str
    scope: ProviderLimitScope
    kind: ProviderLimitKind
    precision: ProviderLimitPrecision
    source: ProviderLimitSource
    is_unlimited: bool = False
    used: int | float | None = None
    limit: int | float | None = None
    remaining: int | float | None = None
    window: ProviderLimitWindow | None = None
    subject: Mapping[str, ProviderJSON] | None = None
    note: str | None = None

    def __post_init__(self):
        for name in ("id", "label", "scope", "kind", "precision", "source"):
            _provider_text(getattr(self, name), name, required=True)
        for name, vocabulary in (
            ("scope", ProviderLimitScope),
            ("kind", ProviderLimitKind),
            ("precision", ProviderLimitPrecision),
            ("source", ProviderLimitSource),
        ):
            _provider_enum(getattr(self, name), vocabulary, name)
        _provider_bool(self.is_unlimited, "is_unlimited")
        for name in ("used", "limit", "remaining"):
            _provider_number(getattr(self, name), name)
        if self.window is not None and not isinstance(self.window, ProviderLimitWindow):
            raise TypeError("window must be a ProviderLimitWindow")
        _provider_text(self.note, "note")
        object.__setattr__(self, "subject", _provider_mapping(self.subject, "subject"))


@dataclass(frozen=True, slots=True)
class ProviderError(_ProviderValue):
    """A limits failure, not an exception carrying a credential."""

    type: str
    message: str
    data: Mapping[str, ProviderJSON] | None = field(default=None, repr=False)

    def __post_init__(self):
        _provider_text(self.type, "error type", required=True)
        _provider_text(self.message, "error message", required=True)
        object.__setattr__(self, "data", _provider_mapping(self.data, "error data"))


@dataclass(frozen=True, slots=True, kw_only=True)
class ProviderLimits(_ProviderValue):
    """Usage snapshot; the host fills provider_id and fetched_at_ms when omitted."""

    status: ProviderLimitStatus = "ok"
    limits: Sequence[ProviderLimit] = ()
    rpm: int | None = None
    tpm: int | None = None
    note: str | None = None
    error: ProviderError | None = None
    provider_id: str | None = None
    fetched_at_ms: int | None = None

    def __post_init__(self):
        _provider_text(self.status, "limits status", required=True)
        _provider_enum(self.status, ProviderLimitStatus, "limits status")
        object.__setattr__(
            self, "limits", _provider_rows(self.limits, ProviderLimit, "limits")
        )
        for name in ("rpm", "tpm"):
            _provider_number(getattr(self, name), name, integer=True, minimum=0)
        _provider_number(self.fetched_at_ms, "fetched_at_ms", integer=True)
        _provider_text(self.note, "note")
        if self.provider_id is not None:
            _provider_text(self.provider_id, "provider_id", required=True)
        if self.error is not None and not isinstance(self.error, ProviderError):
            raise TypeError("error must be a ProviderError")

    def to_wire(self) -> dict[str, ProviderJSON]:
        value = _ProviderValue.to_wire(self)
        value["dynamic"] = {"limits": value.pop("limits")}
        if "note" in value:
            value["dynamic"]["note"] = value.pop("note")
        value["static"] = {
            key: value.pop(key) for key in ("rpm", "tpm") if key in value
        }
        return value


def _provider_callback(name, fn):
    if fn is None:
        return None
    if (
        not callable(fn)
        or inspect.iscoroutinefunction(fn)
        or inspect.iscoroutinefunction(fn.__call__)
    ):
        raise TypeError(f"vis.Provider {name} must be a synchronous callable")
    arity = {
        "refresh_token_fn": 1,
        "auth_fn": 1,
        "enrich_models_fn": 2,
        "on_selected_fn": 1,
    }.get(name, 0)
    try:
        signature = inspect.signature(fn)
        try:
            signature.bind(*([None] * arity))
        except TypeError:
            if name != "refresh_token_fn":
                raise
            signature.bind()
            arity = 0
    except (TypeError, ValueError):
        raise TypeError(f"vis.Provider {name} has an incompatible signature") from None

    def invoke(*args):
        # Decide arity BEFORE invocation. A body failure must never run a callback twice.
        if name == "refresh_token_fn":
            if len(args) > 1:
                raise TypeError("refresh_token_fn takes at most one rejected token")
            args = (args[0] if args else None,) if arity else ()
        result = fn(*args)
        if inspect.isawaitable(result):
            if inspect.iscoroutine(result):
                result.close()
            raise TypeError(f"vis.Provider {name} returned an awaitable")
        if result is None or name in ("logout_fn", "on_selected_fn"):
            return None
        expected = {
            "get_token_fn": ProviderCredential,
            "detect_fn": ProviderCredential,
            "refresh_token_fn": ProviderCredential,
            "status_fn": ProviderStatus,
            "limits_fn": ProviderLimits,
        }.get(name)
        if expected is not None:
            if not isinstance(result, expected):
                raise TypeError(
                    f"vis.Provider {name} must return {expected.__name__} or None"
                )
            return result.to_wire()
        if name == "enrich_models_fn":
            return [
                row.to_wire() for row in _provider_rows(result, ProviderModel, name)
            ]
        if name == "auth_prompt_fn":
            return (
                result
                if isinstance(result, str)
                else list(_provider_rows(result, str, name))
            )
        if name == "auth_fn" and not isinstance(result, (bool, str)):
            raise TypeError("vis.Provider auth_fn must return text, a boolean or None")
        return result

    return invoke


@dataclass(frozen=True, slots=True)
class Provider:
    """Pure provider declaration; register adapts typed callbacks to the host protocol.

    Credential reads are passive; only auth_fn may initiate login. Callbacks are
    synchronous and may run without a session. Refresh accepts zero arguments or
    one rejected token (None if unknown), chosen without retrying callback errors.
    Enrichment/selection inputs remain JSON mappings owned by the router/config,
    not a second SDK schema for those domains. Callback outputs use typed records.
    """

    id: str
    label: str
    preset: ProviderPreset | None = None
    is_managed: bool = False
    get_token_fn: Callable[[], ProviderCredential | None] | None = None
    detect_fn: Callable[[], ProviderCredential | None] | None = None
    status_fn: Callable[[], ProviderStatus | None] | None = None
    logout_fn: Callable[[], None] | None = None
    limits_fn: Callable[[], ProviderLimits | None] | None = None
    refresh_token_fn: (
        Callable[[str | None], ProviderCredential | None]
        | Callable[[], ProviderCredential | None]
        | None
    ) = None
    auth_fn: Callable[[Callable[[str], None]], str | bool | None] | None = None
    auth_prompt_fn: Callable[[], Sequence[str] | str | None] | None = None
    enrich_models_fn: (
        Callable[
            [Mapping[str, ProviderJSON], Mapping[str, ProviderJSON]],
            Sequence[ProviderModel] | None,
        ]
        | None
    ) = None
    on_selected_fn: Callable[[Mapping[str, ProviderJSON]], None] | None = None

    def __post_init__(self):
        _provider_text(self.id, "provider id", required=True)
        _provider_text(self.label, "provider label", required=True)
        _provider_bool(self.is_managed, "is_managed")
        if self.preset is not None and not isinstance(self.preset, ProviderPreset):
            raise TypeError("vis.Provider preset must be a ProviderPreset")
        for f in fields(self):
            if f.name.endswith("_fn"):
                _provider_callback(f.name, getattr(self, f.name))

    def _spec(self):
        return {
            "marker": "provider",
            **{
                f.name: _provider_callback(f.name, getattr(self, f.name))
                if f.name.endswith("_fn")
                else _provider_wire(getattr(self, f.name))
                for f in fields(self)
            },
        }


def ok(title, body=None, data=None):
    return {
        "marker": "slash_result",
        "status": "ok",
        "title": str(title),
        "body": body,
        "data": data,
    }


def err(title, body=None, data=None):
    return {
        "marker": "slash_result",
        "status": "error",
        "title": str(title),
        "body": body,
        "data": data,
    }


def block(reason):
    return {"marker": "block", "reason": str(reason)}


def strings_of(value):
    out = []

    def walk(v):
        if isinstance(v, str):
            out.append(v)
        elif isinstance(v, dict):
            for k, x in v.items():
                walk(k)
                walk(x)
        elif isinstance(v, (list, tuple, set)):
            for x in v:
                walk(x)

    walk(value)
    return out


class _State(_MutableMapping):
    """The extension's durable store, as a mapping.

    A real `MutableMapping`, so `pop`, `setdefault`, `update`, `clear`, `keys`,
    `items`, `values`, `len` and iteration mean what they mean on a dict, and
    comparing one to a dict compares contents. Five methods used to be the whole
    surface: `vis.state.pop(key)` was an AttributeError, and `list(vis.state)`
    fell through to the old sequence protocol and asked the host for the key `0`.

    A read is one host call for the key it names — never a copy of the store;
    only iteration and `len` ask for the key list. A key written as `None` is
    absent, because no host can tell a stored JSON null from a key nobody wrote.
    """

    def _keys(self):
        # Inside Vis the host answers a host-marshalled list; outside it, a real one.
        return [str(key) for key in _host.state_keys()]

    def get(self, key, default=None):
        # The mixin would go through `__getitem__` and catch the KeyError it just
        # made; asking the host once is the whole read.
        value = _host.state_get(str(key))
        return default if value is None else value

    def __getitem__(self, key):
        value = _host.state_get(str(key))
        if value is None:
            raise KeyError(key)
        return value

    def __setitem__(self, key, value):
        _host.state_put(str(key), value)

    def __delitem__(self, key):
        # `state_del` forgives a key that was never there, so this read is the
        # KeyError a mapping owes its caller — and what `pop` reports.
        if _host.state_get(str(key)) is None:
            raise KeyError(key)
        _host.state_del(str(key))

    def __contains__(self, key):
        return _host.state_get(str(key)) is not None

    def __iter__(self):
        return iter(self._keys())

    def __len__(self):
        return len(self._keys())


state = _State()


def log(level, msg):
    _host.log(str(level), str(msg))


def notify(text, level="info"):
    _host.notify(str(text), str(level))


def _shell_options(name, opts):
    if not isinstance(opts, dict):
        raise TypeError(
            f"{name} takes one options map — use {name}({{'command': 'ls'}})"
        )
    return opts


class Shell(dict):
    # A SHELL RESULT IS A LIVE HANDLE — the SAME contract the model's sandbox gets.
    # `vis.shell`, `vis.jailed_shell` and `vis.jailed_shell_session` all answer this
    # dict-with-methods, so an extension drives a process on the object the call
    # returned (`sh.logs()`, `sh.wait(30)`, `sh.type('y')`, `sh.stop()`) instead of
    # hand-authoring `{'op': 'logs', 'id': …}` maps. It IS a dict — `sh['exit']`,
    # `json.dumps(sh)`, `{**sh}` all behave — and every op answers the one shell
    # result shape, so no key can KeyError.
    def __init__(self, raw, call):
        dict.__init__(self, dict(raw or {}))
        self._vis_call = call

    def _vis_op(self, opts):
        return Shell(self._vis_call(dict(opts, id=self.get("id"))), self._vis_call)

    def logs(self, offset=None, limit=None):
        # A NEGATIVE offset reads the last n LINES; a positive one is a byte cursor.
        opts = {"op": "logs"}
        if offset is not None:
            opts["offset"] = int(offset)
        if limit is not None:
            opts["limit"] = int(limit)
        return self._vis_op(opts)

    def type(self, text, is_enter=True):
        return self._vis_op(
            {"op": "send", "text": str(text), "is_enter": bool(is_enter)}
        )

    def stop(self):
        return self._vis_op({"op": "stop"})

    def wait(self, seconds=120):
        # ONE wait, in the HOST: `{'op': 'wait'}` runs the bounded poll loop that the
        # sandbox handle also calls, so an extension and the model can never disagree
        # about when a wait ends or what it accumulated.
        return self._vis_op({"op": "wait", "seconds": int(seconds)})


def _shell_call(name):
    def call(opts):
        return getattr(_host, name)(_shell_options(name, opts))

    return call


def shell(opts):
    # Trusted extensions get the same unrestricted process boundary as subprocess.
    call = _shell_call("shell")
    return Shell(call(opts), call)


def jailed_shell(opts):
    # Strictly re-read the latest merged on-disk config at each process spawn.
    call = _shell_call("jailed_shell")
    return Shell(call(opts), call)


def jailed_shell_session(opts):
    # Explicitly use the invoking session's immutable policy snapshot.
    call = _shell_call("jailed_shell_session")
    return Shell(call(opts), call)


class _Fs:
    # The extension's own filesystem, performed by the RUNTIME in C.
    #
    # Under a jail the interpreter confines Python to the session's roots, and the
    # files an extension owns are not the project's - `~/.config/gh`, a cache, a
    # checkout it maintains. `open()` is refused there; these are not, because the
    # bytes move in C where the audit hook does not stand. It is the filesystem
    # counterpart of `shell`: a capability an extension was GIVEN, not a policy the
    # session inherits.
    #
    # The model's sandbox cannot borrow it. The runtime asks WHAT IT WAS ASKED TO
    # RUN, which no Python can forge - taking another session's globals out of
    # `sys.modules` and `exec`ing into them, which defeats any check made on the
    # calling frame, changes nothing here.
    #
    # `read` answers bytes; `read_text` decodes. `write` takes str or bytes.

    @staticmethod
    def _door():
        import _vis_fs

        return _vis_fs

    @staticmethod
    def read(path):
        return _Fs._door().read(str(path))

    @staticmethod
    def read_text(path, encoding="utf-8"):
        return _Fs._door().read(str(path)).decode(encoding)

    @staticmethod
    def write(path, data, append=False):
        return _Fs._door().write(str(path), data, bool(append))

    @staticmethod
    def list(path):
        return _Fs._door().list(str(path))

    @staticmethod
    def copy(src, dst):
        return _Fs._door().copy(str(src), str(dst))

    @staticmethod
    def move(src, dst):
        return _Fs._door().move(str(src), str(dst))

    @staticmethod
    def remove(path):
        return bool(_Fs._door().remove(str(path)))

    @staticmethod
    def mkdir(path):
        return bool(_Fs._door().mkdir(str(path)))

    @staticmethod
    def stat(path):
        return _Fs._door().stat(str(path))

    @staticmethod
    def exists(path):
        return _Fs._door().stat(str(path)) is not None


fs = _Fs()


class Answer:
    # The outcome of `vis.ask(...)`. Truthy only when the human submitted.
    # `values` is keyed by each field's `name` and always carries every field; a
    # `password` field holds an opaque `vis-secret:` handle, never plaintext.
    def __init__(self, raw):
        raw = raw or {}
        self.is_submitted = bool(raw.get("is_submitted"))
        self.reason = str(raw.get("reason") or "cancelled")
        self.request_id = raw.get("request_id")
        self.values = dict(raw.get("values") or {})

    def __bool__(self):
        return self.is_submitted

    def __contains__(self, name):
        return str(name) in self.values

    def __getitem__(self, name):
        return self.values[str(name)]

    def get(self, name, default=None):
        v = self.values.get(str(name))
        return default if v is None else v

    def reveal(self, name):
        # Plaintext behind a password field's handle — trusted side only.
        return reveal(self.values.get(str(name)))

    def __repr__(self):
        return (
            f"Answer(is_submitted={self.is_submitted!r}, reason={self.reason!r}, "
            f"fields={sorted(self.values)!r})"
        )


def _validator_arity(fn):
    # How a validator wants to be CALLED: 2 for (value, every value), 1 for the
    # value alone, and None when it can take neither. The shape is judged here,
    # at `vis.ask`, instead of blowing up in front of the human on submit - a
    # `lambda: None` is a bug in the extension, not a bad answer.
    import inspect

    try:
        params = list(inspect.signature(fn).parameters.values())
    except (TypeError, ValueError):
        # A builtin with no introspectable signature: assume the common shape.
        return 1
    positional = 0
    required = 0
    for p in params:
        if p.kind in (p.POSITIONAL_ONLY, p.POSITIONAL_OR_KEYWORD):
            positional += 1
            if p.default is p.empty:
                required += 1
        elif p.kind is p.VAR_POSITIONAL:
            return 2
        elif p.kind is p.KEYWORD_ONLY and p.default is p.empty:
            return None
    if positional < 1 or required > 2:
        return None
    return 2 if positional >= 2 else 1


def _field_specs(fields, validators):
    # Canonicalize a field TREE to snake_case string keys and pull the `validate`
    # callables out of it.
    #
    # A validator is a FUNCTION, and a function is not JSON, so it never leaves
    # this process: every field's callables are popped out of the spec and kept
    # in `validators` by field name. The host is told only HOW MANY each field
    # declared and is handed a callback it re-enters on the thread the human
    # submitted on. Groups nest, so this walks the whole tree.
    if not isinstance(fields, (list, tuple)) or not fields:
        raise TypeError("a form needs a non-empty list of field specs")

    def one(f):
        if not isinstance(f, dict):
            raise TypeError("each field spec is a dict of snake_case string keys")
        spec = {str(k): v for k, v in f.items()}
        checks = spec.pop("validate", None)
        if checks is not None:
            if callable(checks):
                checks = [checks]
            if (
                not isinstance(checks, (list, tuple))
                or not checks
                or not all(callable(c) for c in checks)
            ):
                raise TypeError(
                    "validate is a function, or a list of functions, taking the "
                    "value (and optionally every value) and answering None or a "
                    "message string"
                )
            for c in checks:
                if _validator_arity(c) is None:
                    raise TypeError(
                        "a validate function takes the value, or the value and "
                        "every value - this one takes neither"
                    )
            name = str(spec.get("name") or spec.get("id") or "").strip()
            if not name:
                raise TypeError("a field with validate needs a name")
            validators[name] = list(checks)
        children = spec.get("fields")
        if isinstance(children, (list, tuple)):
            spec["fields"] = [one(c) for c in children]
        return spec

    return [one(f) for f in fields]


def _request_spec(title, fields, options, validators):
    # The request object the host receives. Dialog options first, then the title
    # and the field tree, so neither can be shadowed by an option key.
    request = {str(k): v for k, v in options.items()}
    request["title"] = str(title)
    request["fields"] = _field_specs(fields, validators)
    return request


def ask(title, fields, **options):
    # Pause and ask the human for typed values, then BLOCK until they answer.
    #
    #   answer = vis.ask('Deploy', [
    #       {'name': 'env', 'label': 'Target', 'type': 'select',
    #        'description': 'Where this deploy lands.',
    #        'options': ['staging', 'prod'], 'is_required': True},
    #       {'name': 'token', 'label': 'Deploy token', 'type': 'password'},
    #   ], description='Pick a target', timeout_ms=120000)
    #   if answer:
    #       deploy(answer['env'], answer.reveal('token'))
    #
    # EVERY key is a snake_case STRING: 'is_required', 'max_length',
    # 'timeout_ms'. A camelCase or kebab-case key is REFUSED with an error that
    # names the right spelling — it is never accepted and quietly ignored.
    #
    # Field keys: name (keys the answer in `values`), label (shown above the
    # input), description (the italic line under that label), type, default,
    # is_required, placeholder, options, min_length, max_length, validate, and
    # min/max/step for a 'range' field (defaults 0/100/1 — it answers with a
    # NUMBER). An 'otp' is a one-time code in digit boxes: min_length/max_length
    # say how many (default 6, at most 12), digits only, paste fills the boxes.
    # validate is a FUNCTION, or a list of them, run by the host when the human
    # CONFIRMS the form — never while they type. Each one takes the coerced value
    # (and, if it declares a second parameter, the dict of every value) and
    # answers None/True when the value is fine or the error message as a string;
    # False means 'is not valid' and a raise becomes 'could not be validated: …'.
    # The first message wins, the field shows it, and it disappears the moment
    # that field is touched again — the next confirmation checks it afresh.
    # A blank value is never validated: that is is_required's only job.
    #
    #       {'name': 'port', 'label': 'Port',
    #        'validate': lambda v: None if v.isdigit() else 'must be digits'},
    #       {'name': 'confirm', 'type': 'password', 'validate':
    #        lambda v, values: None if v == values['pw'] else 'must match Password'},
    # A 'group' is not a field at all — it LAYS OUT: 'fields' (its children) and
    # 'direction' ('column' stacks, the default; 'row' side by side). Groups
    # nest, need no name, and never appear in `values`, which stays flat.
    # Layout and value keys never mix: 'default'/'is_required'/'validate' on a
    # group, or 'fields'/'direction' on an answerable field, are both REFUSED.
    # Field types: plaintext, password, multiline, select, multiselect,
    # checkbox, range, otp. Two node types answer nothing: 'group' is the node
    # above them, and 'heading'/'paragraph' are pure DECORATION carrying only
    # 'text' — a section title and the prose under it, so a long form reads like
    # a page instead of a list. Decoration has no name and never lands in
    # `values`; giving one a name, a default or a validate is REFUSED.
    # Dialog options: description (prose under the title explaining
    # what the whole ask is about — it wraps), submit_label, cancel_label,
    # is_cancellable, timeout_ms.
    #
    # A dialog either has a DEADLINE or it has none. `timeout_ms` is the wait in
    # milliseconds: 5 minutes when the key is absent, and 0 to wait
    # INDEFINITELY, for as long as the human takes. Nothing is capped, so a
    # stated wait is the wait you get.
    # A cancelled, timed-out or unanswered request returns a falsey Answer
    # whose `reason` says which — it never raises. `reason == 'timeout'` is the
    # deadline running out: the dialog closes on every surface and you resume
    # with one clear fixed outcome instead of a half-open form nobody can
    # answer, which is exactly what `timeout_ms=0` refuses to do.
    # `reason == 'undeliverable'` means no surface was mounted to show the
    # dialog: the host logged an error and gave up at once instead of parking
    # you — even an indefinite ask cannot wait on a human who was never asked.
    import json

    validators = {}
    request = _request_spec(title, fields, options, validators)

    def _run(name, index, value_json, values_json):
        # One validator, one value, one verdict. A raise is deliberately NOT
        # caught: the host turns it into 'could not be validated: …' rather than
        # accepting a value the extension refused to judge.
        check = validators[str(name)][int(index)]
        if _validator_arity(check) == 2:
            verdict = check(json.loads(value_json), json.loads(values_json))
        else:
            verdict = check(json.loads(value_json))
        if verdict is None or verdict is True:
            return json.dumps(None)
        if verdict is False:
            return json.dumps(False)
        return json.dumps(verdict if isinstance(verdict, str) else str(verdict))

    answer_json = _host.request_input(
        json.dumps(request),
        json.dumps({k: len(v) for k, v in validators.items()}),
        _run,
    )
    return Answer(json.loads(answer_json))


# -- Form builders ------------------------------------------------------------
# One helper per node type, named exactly like Clojure's
# `com.blockether.vis.view`: the type IS the function and the name is
# POSITIONAL, so a misspelled type is a NameError on the spot instead of a
# refused request, and every other key stays the snake_case spelling `ask`
# documents.
#
#   answer = vis.ask('Deploy', [
#       vis.heading('Target'),
#       vis.paragraph('Staging pages nobody.'),
#       vis.row(vis.select('env', ['staging', 'prod'], label='Where',
#                          is_required=True),
#               vis.slider('canary', min=0, max=100, step=5)),
#       vis.password('token', label='Deploy token', is_required=True),
#   ])
#
# A builder is a plain dict, so a form stays printable and can be assembled in a
# loop. Nothing here talks to the host: a builder shapes a dict, and `vis.ask`
# is what carries it to the engine that judges it.


def _node(type_name, name, spec):
    node = {str(k): v for k, v in spec.items()}
    node["type"] = type_name
    node["name"] = str(name)
    return node


def plaintext(name, **spec):
    # One typed line, answered as a string.
    return _node("plaintext", name, spec)


def password(name, **spec):
    # A masked line, answered as an opaque 'vis-secret:' handle: `reveal` it on
    # the trusted side, never log it.
    return _node("password", name, spec)


def multiline(name, **spec):
    # A text box, answered as a string that keeps its newlines.
    return _node("multiline", name, spec)


def select(name, options, **spec):
    # Choose exactly ONE of `options` (plain strings, or `vis.option(...)`
    # pairs); answered as the chosen value.
    spec["options"] = list(options)
    return _node("select", name, spec)


def multiselect(name, options, **spec):
    # Choose ANY of `options`; answered as a list, empty when nothing is ticked.
    spec["options"] = list(options)
    return _node("multiselect", name, spec)


def checkbox(name, **spec):
    # One box, answered as a bool. `is_required=True` means it must end up
    # TICKED, not merely present.
    return _node("checkbox", name, spec)


def slider(name, **spec):
    # A number on a track: min / max / step, 0 / 100 / 1 by default, answered as
    # a NUMBER. It is `range` on the wire; the builder is `slider` so it never
    # shadows the builtin.
    return _node("range", name, spec)


def otp(name, **spec):
    # A one-time code in digit boxes: min_length / max_length say how many
    # (6 by default, 12 at most), digits only, paste fills the boxes. A code is a
    # credential, so it answers with a `vis-secret:` handle like a password.
    return _node("otp", name, spec)


def option(value, label=None):
    # One entry of a select / multiselect: the value that is ANSWERED and the
    # words shown for it. Given no label, the value shows itself.
    return {"value": value} if label is None else {"value": value, "label": label}


def _group(direction, fields):
    # A group answers nothing and never appears in `values`, which stays flat
    # however deep the tree goes. A LEADING STRING is the group's id: a live view
    # patches by id and `after=` names a group too, so the same builder serves a
    # form (no id needed) and a live view (an id required).
    group = {"type": "group", "direction": direction}
    if fields and isinstance(fields[0], str):
        group["id"] = fields[0]
        fields = fields[1:]
    group["fields"] = list(fields)
    return group


def row(*fields):
    # Lay these nodes out side by side: `vis.row(vis.text("host"), vis.text("port"))`
    # on a form, `vis.row("reading", vis.table("hosts", ...), vis.status("why"))`
    # in a live view.
    return _group("row", fields)


def column(*fields):
    # Stack these nodes, the default arrangement: worth saying out loud inside a
    # `row`.
    return _group("column", fields)


def heading(text, live_text=None, **spec):
    """Form heading, or live ``heading(id, text, level=1..6)``."""
    if live_text is not None:
        return _live_node(
            "heading", text, dict(spec, text=str(live_text), level=spec.get("level", 2))
        )
    if spec:
        raise TypeError("live heading options require both id and text")
    return {"type": "heading", "text": str(text)}


def paragraph(text, live_text=None, **spec):
    """Form prose, or live ``paragraph(id, text)`` with inline Markdown."""
    if live_text is not None:
        return _live_node("paragraph", text, dict(spec, text=str(live_text)))
    if spec:
        raise TypeError("live paragraph options require both id and text")
    return {"type": "paragraph", "text": str(text)}


def reveal(handle):
    # Resolve an opaque `vis-secret:` handle to its plaintext, or None when the
    # handle is unknown or already forgotten. Never log or return the result.
    if not handle:
        return None
    return _host.reveal_secret(str(handle))


def forget(handle):
    # Drop the plaintext behind a handle as soon as it is no longer needed.
    if not handle:
        return False
    return bool(_host.forget_secret(str(handle)))


# -- Live views ---------------------------------------------------------------
# A form PAUSES a run to collect values; a live view REPORTS on work already
# running. Nothing here blocks: a push crosses to the engine, every mounted
# surface repaints, and the extension carries straight on. The human watches it
# move and may stop it; the model reads the finished picture as DATA.
#
#   with vis.live('CI', [vis.status('now', 'Polling'),
#                        vis.table('jobs', columns=[vis.table_column('job', 'Job'),
#                                                   vis.table_column('state', 'State')])]) as view:
#       for job in poll():
#           view['jobs'].upsert(job.id, [job.name, job.state], tone=job.tone)
#       view['now'].set('Finished', tone='ok')
#
# Nodes are addressed BY ID, because a view with two tables has no "the" table.

_FLUSH_MS = 100
# How long a handle may coalesce pushes before one has to cross. Mirrors
# `:live/flush-ms` in the contract document, which `python_host_test` reads back:
# a host round trip per written line would park the extension on the journal
# writer once per line, so the batching is part of the contract.

_MAX_BATCH = 200
# The most items one coalesced push carries, under every per-patch bound the
# engine declares (500 log lines, 200 table rows). A hot loop coalesces into
# whole patches instead of being refused for writing one too big.


class Interrupted(Exception):
    """The live view this handle drives is no longer open.

    Raised by the next push after the human stopped watching — Escape in the
    terminal, Stop in the app — so an unattended loop ends by itself. A loop
    that would rather finish its own work reads `view.is_interrupted` instead
    and decides.

    `note` is the comment the person left with the stop, when they left one: the
    reason it is being stopped, in their words.
    """

    def __init__(self, view_id, reason=None, note=None):
        self.view_id = view_id
        self.reason = reason
        self.note = note
        ended = f" ({reason})" if reason else ""
        because = f": {note}" if note else ""
        super().__init__(f"live view {view_id} is no longer open{ended}{because}")


class _Node:
    """One node of a live view, addressed by its id."""

    def __init__(self, view, node_id, type_name):
        self._view = view
        self.node_id = str(node_id)
        self.type = str(type_name)

    def __repr__(self):
        return f"<vis {self.type} {self.node_id!r}>"

    def _op(self, name, **payload):
        payload["op"] = name
        payload["node_id"] = self.node_id
        return self._view._push({k: v for k, v in payload.items() if v is not None})


class _KeyedNode(_Node):
    """A node holding items the extension addresses by id: it can drop them."""

    def remove(self, *item_ids):
        # Ids as arguments or as one iterable, because a caller with a list
        # should not have to spread it.
        ids = (
            list(item_ids[0])
            if len(item_ids) == 1 and not isinstance(item_ids[0], str)
            else list(item_ids)
        )
        return self._op("remove", item_ids=[str(i) for i in ids])

    def clear(self):
        return self._op("clear")


class TextNode(_Node):
    """A paragraph, heading or code block, replaced in place."""

    def set(self, text, **spec):
        return self._op("set", text=str(text), **spec)


class Spinner(_Node):
    def set(self, text=None, *, variant=None, is_active=None):
        return self._op("set", text=text, variant=variant, is_active=is_active)


class Button(_Node):
    def set(self, label=None, *, is_disabled=None):
        return self._op("set", label=label, is_disabled=is_disabled)


class Status(_Node):
    def set(self, text, tone=None, detail=None, label=None):
        # One line saying what is happening RIGHT NOW.
        return self._op("set", text=str(text), tone=tone, detail=detail, label=label)


class Progress(_Node):
    def set(self, value=None, done=None, total=None, label=None):
        # A fraction (0..1), or the counts to make one. Neither is
        # INDETERMINATE, which is a real state and not zero.
        return self._op("set", value=value, done=done, total=total, label=label)


class Stat(_Node):
    def set(self, stat_id, value_text, label=None, tone=None):
        # One counter of the strip, upserted by id.
        return self._op(
            "append",
            stats=[
                _live_item(
                    stat_id,
                    {"value_text": str(value_text), "label": label, "tone": tone},
                )
            ],
        )

    def clear(self):
        return self._op("clear")

    def remove(self, *item_ids):
        return _KeyedNode.remove(self, *item_ids)


class Steps(_Node):
    def set(self, step_id, tone=None, label=None, detail=None, value=None):
        # One step of the checklist, upserted by id: the same call marks it
        # running and later done.
        return self._op(
            "append",
            steps=[
                _live_item(
                    step_id,
                    {"tone": tone, "label": label, "detail": detail, "value": value},
                )
            ],
        )

    def clear(self):
        return self._op("clear")

    def remove(self, *item_ids):
        return _KeyedNode.remove(self, *item_ids)


class Log(_Node):
    def write(self, *lines):
        # Lines as arguments or as one iterable. A log is UNBOUNDED: every line
        # reaches the view's record, and the window is only what a surface holds.
        given = lines[0] if len(lines) == 1 and not isinstance(lines[0], str) else lines
        return self._op("append", lines=[str(line) for line in given])

    def clear(self):
        # A PHOTOGRAPH, not a scroll: the record starts over with the window, so a
        # pane rewritten in place never offers earlier lines nothing can serve.
        return self._op("clear")


class Table(_KeyedNode):
    def upsert(self, row_id, cells, tone=None, branch=None):
        # ONE verb for "new row" and "row changed": a scan loop writing a live
        # table does not know which it is, and the id is the address either way.
        return self._op(
            "append",
            rows=[
                _live_item(
                    row_id,
                    {
                        "cells": [_cell(c) for c in cells],
                        "tone": tone,
                        "branch": branch,
                    },
                )
            ],
        )

    def select(self, *item_ids):
        # Selection is shared engine state. A surface writes it and the extension can
        # observe it through `LiveView.state()` on its next update.
        return self._op("set", selected_ids=[str(item_id) for item_id in item_ids])


class Link(_KeyedNode):
    def add(self, link_id, label, target, target_kind=None, tone=None):
        # A pointer the human can open: a URL, a path, or an attachment id.
        return self._op(
            "append",
            links=[
                _live_item(
                    link_id,
                    {
                        "label": str(label),
                        "target": str(target),
                        "target_kind": target_kind,
                        "tone": tone,
                    },
                )
            ],
        )


_LIVE_NODES = {
    "paragraph": TextNode,
    "heading": TextNode,
    "code": TextNode,
    "spinner": Spinner,
    "button": Button,
    "status": Status,
    "progress": Progress,
    "stat": Stat,
    "steps": Steps,
    "log": Log,
    "table": Table,
    "link": Link,
}
# The typed handle each node type answers. The engine owns the type table
# (`view.spec/live-node-types`, rendered into the contract document);
# `python_host_test` fails when this one names a type that is not in it.


def _cell(value):
    return "" if value is None else str(value)


def _live_item(item_id, spec):
    item = {"id": str(item_id)}
    item.update({k: v for k, v in spec.items() if v is not None})
    return item


def _batch_size(op):
    return max((len(v) for k, v in op.items() if isinstance(v, list)), default=1)


class LiveView:
    """A live view the human WATCHES, driven by the extension that opened it.

    `vis.live(...)` mounts one and answers this handle. Nodes are addressed by
    id — `view['jobs']`, or `view.node('jobs')` — and each answers the typed
    handle its own type declares. The view-level shortcuts (`view.status(...)`,
    `view.log(...)`, `view.row(...)`) resolve to the one node of that type and
    raise naming the candidate ids when the view holds several, so an ambiguous
    call fails where it was written instead of quietly patching the wrong table.

    Pushes are BATCHED: ops buffer and cross on the next push after `flush_ms`,
    when a coalesced push fills, and always before the view is read or closed.
    `with view.batch():` groups one logical picture explicitly, including structural
    add/drop operations. Repeated writes to the same row or node collapse into the
    last one, so a per-row progress counter costs one wire row per tick rather than
    one per write.

    Closing is the point: `close()` answers either the structured verdict or the
    compact `model_result` the extension chose. Used as a context manager the
    view closes itself — `completed` on the way out, and `failed` carrying the
    error when the body raised, because a run that died mid-way still owes the
    model what happened.
    """

    def __init__(self, request, flush_ms=None):
        import json

        self._json = json
        self._flush_ms = _FLUSH_MS if flush_ms is None else max(0, int(flush_ms))
        self._buffer = []
        self._batch_depth = 0
        self._nodes = {}
        self._order = []
        self._is_open = False
        self._result = None
        # None is NEVER, and never is longer than any window: the first push and the
        # first read cross at once, so a human sees the view move the moment work
        # starts. A zero would not — `time.monotonic()` counts from an arbitrary
        # origin, so on a freshly booted machine it reads a few seconds, which is
        # INSIDE the window and would swallow the very first op.
        self._last_flush = None
        self._last_read = None
        self._seq = 0
        answer = self._call({"op": "open", "view": request})
        self.view_id = str(answer.get("view_id") or "")
        self._settle(answer)

    # -- the host seam --------------------------------------------------------

    def _call(self, envelope):
        return self._json.loads(_host.live(self._json.dumps(envelope)))

    def _settle(self, answer):
        """Learn from every answer whether the view is still open."""
        self._is_open = bool(answer.get("is_open"))
        view = answer.get("view")
        if isinstance(view, dict):
            self._seq = view.get("seq", 0)
            self._learn(view.get("nodes"))
        elif answer.get("seq") == self._seq + 1:
            self._seq = answer["seq"]
        if not self._is_open and answer.get("result"):
            self._result = answer["result"]
        return answer

    def _learn(self, nodes):
        if not isinstance(nodes, list):
            return
        self._nodes = {}
        self._order = []
        self._index(nodes)

    def _index(self, nodes):
        # A group is LAYOUT: it holds no items and takes no op, so a row lends its
        # place in the order to the nodes it arranges and never answers itself.
        for node in nodes:
            if not isinstance(node, dict) or not node.get("id"):
                continue
            children = node.get("fields")
            if isinstance(children, list):
                self._index(children)
            else:
                self._nodes[str(node["id"])] = str(node.get("type") or "")
                self._order.append(str(node["id"]))

    def _refuse_closed(self):
        raise Interrupted(self.view_id, self.reason, self.note)

    @contextmanager
    def batch(self):
        """Send one complete picture for a related group of node changes.

        Node handles still coalesce exactly as usual, but neither the leading edge nor
        structural add/drop operations cross the host seam until the outermost batch
        ends. Reads and close remain explicit flush points.
        """
        self._batch_depth += 1
        try:
            yield self
        finally:
            self._batch_depth -= 1
            if self._batch_depth == 0:
                self.flush()

    def _flush_shape_change(self):
        """Flush an add/drop immediately, unless an explicit batch owns it."""
        if self._batch_depth == 0:
            self.flush()

    # -- pushing --------------------------------------------------------------

    def _push(self, op):
        # Leading edge, then a window: the first op after a quiet stretch crosses
        # at once, and whatever arrives within `flush_ms` of it rides the next
        # push, read or close. A loop reporting every iteration costs one host
        # call per window instead of one per iteration.
        if not self._is_open:
            self._refuse_closed()
        self._coalesce(op)
        if self._batch_depth == 0 and (
            self._is_full() or self._since_ms(self._last_flush) >= self._flush_ms
        ):
            self.flush()
        return self

    def _since_ms(self, stamp):
        """Milliseconds since a stamp, and forever when there has not been one."""
        if stamp is None:
            return float("inf")
        return (time.monotonic() - stamp) * 1000.0

    def _is_full(self):
        return len(self._buffer) >= _MAX_BATCH or any(
            _batch_size(op) >= _MAX_BATCH for op in self._buffer
        )

    def _coalesce(self, op):
        """Fold this op into the last one addressing the same node, if it folds.

        Only the LAST op for that node is a candidate, so nothing reorders past
        a `clear` or a `remove` that stands between them.
        """
        for earlier in reversed(self._buffer):
            if earlier.get("node_id") != op.get("node_id"):
                continue
            if earlier["op"] == op["op"] == "set":
                earlier.update(op)
                return
            if earlier["op"] == op["op"] == "append" and _merge_append(earlier, op):
                return
            break
        self._buffer.append(op)

    def flush(self):
        """Send everything buffered. Called for you before any read or close."""
        if not self._buffer:
            return self
        ops, self._buffer = self._buffer, []
        if not self._is_open:
            self._refuse_closed()
        answer = self._settle(
            self._call({"op": "patch", "view_id": self.view_id, "patch": {"ops": ops}})
        )
        self._last_flush = time.monotonic()
        if not self._is_open:
            self._refuse_closed()
        return answer and self

    # -- reading --------------------------------------------------------------

    def state(self):
        """What the view looks like right now, as the surfaces paint it."""
        self.flush()
        answer = self._settle(self._call({"op": "state", "view_id": self.view_id}))
        self._last_read = time.monotonic()
        return answer.get("view")

    def sleep(self, seconds):
        """Block until the view changes, ends, or `seconds` elapse.

        One host wait replaces periodic state reads. An unchanged timeout returns
        no view payload. Returns True on change or close, False on timeout.
        Nonpositive durations do not flush or call the host.
        """
        import math

        seconds = float(seconds)
        if not math.isfinite(seconds) or seconds > 86400:
            raise ValueError("sleep duration must be finite and at most 86400 seconds")
        if seconds <= 0:
            return False
        self.flush()
        if not self._is_open:
            return True
        answer = self._settle(
            self._call(
                {
                    "op": "state",
                    "view_id": self.view_id,
                    "after_seq": self._seq,
                    "timeout_ms": math.ceil(seconds * 1000),
                }
            )
        )
        self._last_read = time.monotonic()
        return not answer.get("timed_out", False)

    @property
    def is_interrupted(self):
        """True once the human stopped watching.

        Asks the engine at most once per flush window, so a compute loop can
        poll it every iteration and still cost one host call per tick.
        """
        if self._is_open and self._since_ms(self._last_read) >= self._flush_ms:
            try:
                self.state()
            except Interrupted:
                pass
        return not self._is_open

    def _result_field(self, key):
        """One structured-verdict field, absent for a compact model result."""
        return self._result.get(key) if isinstance(self._result, dict) else None

    @property
    def reason(self):
        """Why the view ended, or None while open or after a compact result."""
        return self._result_field("reason")

    @property
    def is_from_human(self):
        """True when a PERSON ended it, rather than the run itself or a deadline.

        A view is always stoppable — nothing is asked of the human, so nothing is
        left unanswered by stopping it — and this is how the run finds out that is
        what happened.
        """
        return bool(self._result_field("is_from_human"))

    @property
    def note(self):
        """The comment the human left with their stop, or None.

        The stop always lands; the note says WHY in their own words, and the same
        words reach the model in the verdict.
        """
        return self._result_field("note")

    @property
    def result(self):
        """The structured verdict or compact model result, once ended."""
        return self._result

    # -- nodes ----------------------------------------------------------------

    def node(self, node_id):
        """The typed handle for one node, by id."""
        name = str(node_id)
        type_name = self._nodes.get(name)
        if not type_name:
            known = ", ".join(self._order) or "no nodes"
            raise KeyError(f"this view has no node {name!r} — it has {known}")
        return _LIVE_NODES[type_name](self, name, type_name)

    def __getitem__(self, node_id):
        return self.node(node_id)

    def __contains__(self, node_id):
        return str(node_id) in self._nodes

    def __iter__(self):
        return iter(list(self._order))

    def _only(self, type_name, verb):
        ids = [i for i in self._order if self._nodes.get(i) == type_name]
        if len(ids) == 1:
            return self.node(ids[0])
        if not ids:
            raise KeyError(
                f"view.{verb}() needs a {type_name} node and this view has none"
            )
        raise KeyError(
            f"view.{verb}() is ambiguous: this view has {len(ids)} {type_name} nodes — "
            f"address one by id ({', '.join(ids)})"
        )

    def status(self, text, tone=None, detail=None):
        return self._only("status", "status").set(text, tone=tone, detail=detail)

    def progress(self, value=None, done=None, total=None):
        return self._only("progress", "progress").set(
            value=value, done=done, total=total
        )

    def stat(self, stat_id, value_text, label=None, tone=None):
        return self._only("stat", "stat").set(
            stat_id, value_text, label=label, tone=tone
        )

    def step(self, step_id, tone=None, label=None, detail=None, value=None):
        return self._only("steps", "step").set(
            step_id, tone=tone, label=label, detail=detail, value=value
        )

    def write(self, *lines):
        return self._only("log", "write").write(*lines)

    def row(self, row_id, cells, tone=None, branch=None):
        return self._only("table", "row").upsert(
            row_id, cells, tone=tone, branch=branch
        )

    def link(self, link_id, label, target, target_kind=None, tone=None):
        return self._only("link", "link").add(
            link_id, label, target, target_kind=target_kind, tone=tone
        )

    # -- shape ----------------------------------------------------------------

    def add(self, node, after=None):
        """Add a whole node to a running view — a scan that discovers a seventh
        device should not have to have declared it."""
        op = {"op": "add-node", "node_spec": node}
        if after is not None:
            op["after"] = str(after)
        self._push(op)
        self._flush_shape_change()
        self._index([node])
        node_id = str(node.get("id"))
        # Adding a ROW hands back the view: layout takes no op, and the nodes it
        # arranged are addressable by their own ids.
        return self.node(node_id) if node_id in self._nodes else self

    def drop(self, node_id):
        """Drop a whole node, its items with it."""
        self._push({"op": "remove-node", "node_id": str(node_id)})
        self._flush_shape_change()
        self._nodes.pop(str(node_id), None)
        self._order = [i for i in self._order if i != str(node_id)]
        return self

    # -- ending ---------------------------------------------------------------

    def close(
        self,
        reason=None,
        summary=None,
        error=None,
        artifact_id=None,
        selection_snapshots=None,
        model_result=None,
    ):
        """End the view and answer the result the model reads.

        ``model_result`` is an optional compact string returned instead of the
        full structured verdict. The finished picture and close metadata remain
        in the durable artifact and on human-facing close events.

        ``selection_snapshots`` are finished pictures keyed by a selectable table and
        its selected rows. They are sealed only into the artifact record, so a
        reopened run can still switch rows without keeping its extension alive.

        Closing twice is a no-op answering the first result: a `finally` that
        closes what an interrupt already closed must not overwrite the reason
        the human chose.
        """
        if not self._is_open:
            return self._result
        try:
            self.flush()
        except Interrupted:
            return self._result
        ending = {
            "reason": reason,
            "summary": summary,
            "error": error,
            "artifact_id": artifact_id,
            "selection_snapshots": selection_snapshots,
            "model_result": model_result,
        }
        answer = self._settle(
            self._call(
                {
                    "op": "close",
                    "view_id": self.view_id,
                    "ending": {k: v for k, v in ending.items() if v is not None},
                }
            )
        )
        self._is_open = False
        self._result = answer.get("result") or self._result
        return self._result

    def __enter__(self):
        return self

    def __exit__(self, kind, error, traceback):
        if error is None:
            self.close()
        else:
            # The run died mid-way and still owes the model the picture the
            # human was watching, with the reason it stopped.
            self.close(reason="failed", error=str(error) or kind.__name__)
        return False

    def __repr__(self):
        state = "open" if self._is_open else (self.reason or "closed")
        return f"<vis live view {self.view_id!r} {state}>"


def _merge_append(earlier, op):
    """Fold `op`'s items into `earlier`, or answer False when they do not fold."""
    keys = [k for k in earlier if k not in ("op", "node_id")]
    other = [k for k in op if k not in ("op", "node_id")]
    if len(keys) != 1 or keys != other:
        return False
    key = keys[0]
    if key == "lines":
        earlier[key] = list(earlier[key]) + list(op[key])
        return True
    merged = {}
    for item in list(earlier[key]) + list(op[key]):
        # A repeated id keeps its POSITION and takes the newest values, which is
        # exactly what the engine's upsert does.
        merged[item.get("id")] = item
    earlier[key] = list(merged.values())
    return True


def live(title, nodes, **options):
    """Open a live view and answer the handle that drives it.

    View options: description, source, session_id, channel_ids, plus `flush_ms`
    for the batching window. EVERY key is a snake_case string, exactly as
    `vis.ask` documents. There is no cancellable flag: a human can always stop
    watching, and the verdict says they did (`is_from_human`) and why (`note`).
    plus `flush_ms` for the batching window. EVERY key is a snake_case string,
    exactly as `vis.ask` documents.

    The view is mounted at once and nothing blocks — use it as a context
    manager so it closes itself:

        with vis.live('Deploy', [vis.steps('plan', steps=[...])]) as view:
            view['plan'].set('build', tone='running')

    Closing answers the verdict: `is_completed`, `reason`, the finished picture
    as data, and whatever `summary` the extension chose to end with.
    """
    flush_ms = options.pop("flush_ms", None)
    request = {str(k): v for k, v in options.items()}
    request["title"] = str(title)
    request["nodes"] = list(nodes)
    return LiveView(request, flush_ms=flush_ms)


# -- Extension live-view test harness -----------------------------------------


class _LiveRecorder:
    """An isolated in-memory live host for testing any extension live view.

    ``live`` records only envelopes emitted by the extension. ``host_live`` and
    the ``select``/``close`` helpers simulate surface or human actions against the
    same materialized view without publishing fixture data into a Vis session.
    """

    _COLLECTION = {"stat": "stats", "steps": "steps", "table": "rows", "link": "links"}

    def __init__(self, inner, view_id="test-live-view"):
        import threading

        self._condition = threading.Condition()
        self._inner = inner
        self.said = []
        self.view_id = str(view_id)
        self._view = None
        self._result = None
        self._seq = 0

    def __getattr__(self, name):
        return getattr(self._inner, name)

    @staticmethod
    def _copy(value):
        import json

        return json.loads(json.dumps(value))

    @staticmethod
    def _nodes(nodes):
        for node in nodes or []:
            yield node
            yield from _LiveRecorder._nodes(node.get("fields"))

    def node(self, node_id):
        """Return one materialized node by id, at any depth."""
        if self._view is None:
            raise AssertionError("no test live view is open")
        return next(
            node
            for node in self._nodes(self._view.get("nodes"))
            if node["id"] == node_id
        )

    @staticmethod
    def _upsert(old, incoming):
        positions = {item["id"]: index for index, item in enumerate(old)}
        result = _LiveRecorder._copy(old)
        for item in incoming:
            item = _LiveRecorder._copy(item)
            if item["id"] in positions:
                at = positions[item["id"]]
                result[at] = {**result[at], **item}
            else:
                positions[item["id"]] = len(result)
                result.append(item)
        return result

    def _materialize(self, view):
        view = self._copy(view)
        for node in self._nodes(view.get("nodes")):
            kind = node.get("type")
            if kind == "log":
                node.setdefault("lines", [])
                node.setdefault("window_lines", 2000)
                node["total_lines"] = len(node["lines"])
            elif kind == "heading":
                node.setdefault("level", 2)
            elif kind == "spinner":
                node.setdefault("text", "Working")
                node.setdefault("variant", "braille")
                node.setdefault("is_active", True)
            elif kind == "button":
                node["clicks"] = 0
                node.setdefault("is_disabled", False)
            elif kind == "table":
                node.setdefault("rows", [])
                node.setdefault("max_rows", 5000)
                node.setdefault("order", "insertion")
            elif kind in self._COLLECTION:
                items = node.setdefault(self._COLLECTION[kind], [])
                if kind == "link":
                    for item in items:
                        item.setdefault("target_kind", "url")
        return view

    def _parent(self, node_id):
        def find(nodes):
            for index, node in enumerate(nodes or []):
                if node.get("id") == node_id:
                    return nodes, index
                found = find(node.get("fields"))
                if found is not None:
                    return found
            return None

        return find(self._view.get("nodes"))

    def _apply(self, op):
        action = op["op"]
        if action == "add-node":
            node = self._materialize({"nodes": [op["node_spec"]]})["nodes"][0]
            found = (
                self._parent(op.get("after")) if op.get("after") is not None else None
            )
            siblings, at = (
                (self._view["nodes"], len(self._view["nodes"]))
                if found is None
                else found
            )
            siblings.insert(at + (1 if found is not None else 0), node)
            return
        if action == "remove-node":
            found = self._parent(op["node_id"])
            if found is None:
                raise AssertionError(f"no test live node {op['node_id']!r}")
            found[0].pop(found[1])
            return

        node = self.node(op["node_id"])
        if action == "set":
            node.update(
                {k: self._copy(v) for k, v in op.items() if k not in ("op", "node_id")}
            )
        elif action == "clear":
            key = "lines" if node["type"] == "log" else self._COLLECTION[node["type"]]
            node[key] = []
            if node["type"] == "log":
                # `live/apply-clear`: a cleared log starts its RECORD over too.
                node["total_lines"] = 0
        elif action == "remove":
            key = self._COLLECTION[node["type"]]
            removed = set(op.get("item_ids") or [])
            node[key] = [item for item in node[key] if item.get("id") not in removed]
        elif action == "append" and node["type"] == "log":
            lines = self._copy(op.get("lines") or [])
            node["total_lines"] += len(lines)
            node["lines"] = (node["lines"] + lines)[-node["window_lines"] :]
        elif action == "append":
            key = self._COLLECTION[node["type"]]
            incoming = self._copy(op.get(key) or [])
            if node["type"] == "link":
                for item in incoming:
                    item.setdefault("target_kind", "url")
            node[key] = self._upsert(node[key], incoming)
        else:
            raise AssertionError(f"unsupported test live op: {op!r}")

    def picture(self):
        """Return the terminal, layout-free picture surfaces and the model read."""
        picture = self._copy(self._view)
        picture.pop("seq", None)
        leaves = []
        for node in self._nodes(picture.get("nodes")):
            if node.get("type") == "group":
                continue
            node.pop("selected_ids", None)
            node.pop("is_selectable", None)
            leaves.append(node)
        picture["nodes"] = leaves
        return picture

    def host_live(self, envelope_json):
        """Apply an envelope without recording it as extension output."""
        import json

        envelope = json.loads(envelope_json)
        with self._condition:
            if envelope.get("op") == "state" and envelope.get("timeout_ms", 0) > 0:
                changed = self._condition.wait_for(
                    lambda: (
                        self._result is not None
                        or self._seq != envelope.get("after_seq")
                    ),
                    timeout=envelope["timeout_ms"] / 1000,
                )
                if not changed:
                    return json.dumps(
                        {"view_id": self.view_id, "is_open": True, "timed_out": True}
                    )
            answer = self._dispatch_live(envelope)
            if envelope.get("op") in {"patch", "close"}:
                self._condition.notify_all()
            return answer

    def _dispatch_live(self, envelope):
        import json

        action = envelope.get("op")
        if action == "open":
            self._view = self._materialize(envelope["view"])
            self._result = None
            self._seq = 0
            self._view["seq"] = self._seq
            answer = {"view_id": self.view_id, "is_open": True, "view": self._view}
        elif self._result is not None:
            answer = {"view_id": self.view_id, "is_open": False, "result": self._result}
        elif action == "patch":
            for op in (envelope.get("patch") or {}).get("ops") or []:
                self._apply(op)
            self._seq += 1
            self._view["seq"] = self._seq
            answer = {"view_id": self.view_id, "is_open": True, "seq": self._seq}
        elif action == "state":
            answer = {"view_id": self.view_id, "is_open": True, "view": self._view}
        elif action == "close":
            ending = envelope.get("ending") or {}
            reason = ending.get("reason") or "completed"
            verdict = {
                "view_id": self.view_id,
                "reason": reason,
                "is_completed": reason == "completed",
                "is_from_human": False,
                "view": self.picture(),
                "elided": [],
            }
            verdict.update(
                {
                    k: self._copy(v)
                    for k, v in ending.items()
                    if k not in verdict and k != "model_result"
                }
            )
            self._result = ending.get("model_result", verdict)
            answer = {"view_id": self.view_id, "is_open": False, "result": self._result}
        else:
            raise AssertionError(f"unsupported test live envelope: {envelope!r}")
        return json.dumps(answer)

    def live(self, envelope_json):
        """Record and apply one envelope emitted by the extension."""
        import json

        self.said.append(json.loads(envelope_json))
        return self.host_live(envelope_json)

    def activate(self, node_id):
        """Simulate an accepted operator press and wake state() waiters."""
        import json

        with self._condition:
            node = self.node(node_id)
            if (
                self._result is not None
                or node.get("type") != "button"
                or node.get("is_disabled")
            ):
                raise AssertionError(
                    "only an enabled button in an open view can be activated"
                )
            return json.loads(
                self.host_live(
                    json.dumps(
                        {
                            "op": "patch",
                            "patch": {
                                "ops": [
                                    {
                                        "op": "set",
                                        "node_id": node_id,
                                        "clicks": node.get("clicks", 0) + 1,
                                    }
                                ]
                            },
                        }
                    )
                )
            )

    def select(self, node_id, selected_ids):
        """Simulate a surface selecting rows without recording extension output."""
        import json

        return json.loads(
            self.host_live(
                json.dumps(
                    {
                        "op": "patch",
                        "view_id": self.view_id,
                        "patch": {
                            "ops": [
                                {
                                    "op": "set",
                                    "node_id": str(node_id),
                                    "selected_ids": [str(one) for one in selected_ids],
                                }
                            ]
                        },
                    }
                )
            )
        )

    def close(self, reason="interrupted", **ending):
        """Simulate an external close and return its terminal result."""
        import json

        answer = json.loads(
            self.host_live(
                json.dumps(
                    {
                        "op": "close",
                        "view_id": self.view_id,
                        "ending": {**ending, "reason": str(reason)},
                    }
                )
            )
        )
        return answer["result"]

    def ops(self):
        """Return recorded envelopes with their run-specific view id removed."""
        return [{k: v for k, v in one.items() if k != "view_id"} for one in self.said]

    def patched(self):
        """Return every patch op emitted by the extension, in order."""
        return [
            op
            for one in self.said
            if one.get("op") == "patch"
            for op in (one.get("patch") or {}).get("ops") or []
        ]


def _assert_tree(actual, expected, path="view"):
    """Compare a nested golden and report the exact leaf that moved."""
    assert (path, type(expected)) == (path, type(actual))
    if isinstance(expected, dict):
        assert (path, sorted(expected)) == (path, sorted(actual))
        for key, value in expected.items():
            _assert_tree(actual[key], value, f"{path}.{key}")
    elif isinstance(expected, list):
        assert (path, len(expected)) == (path, len(actual))
        for index, value in enumerate(expected):
            _assert_tree(actual[index], value, f"{path}[{index}]")
    else:
        assert (path, expected) == (path, actual)


class _Testing:
    """Reusable helpers for extension tests; no fixture reaches the real live host."""

    LiveRecorder = _LiveRecorder
    assert_tree = staticmethod(_assert_tree)


testing = _Testing()


# -- Live view builders -------------------------------------------------------
# One helper per node type, named exactly like Clojure's
# `com.blockether.vis.view`, and the ID IS POSITIONAL because every op an
# extension writes later addresses that id. A builder is a plain dict: nothing
# here talks to the host, and `vis.live` is what carries it to the engine that
# judges it.
#
# Layout is the FORM's own vocabulary, not a second one: `vis.row("reading",
# table, status)` stands its nodes side by side and `vis.column(...)` stacks them,
# with a live group taking the id every op addresses. It is DECLARED once and no
# op carries it, so a layout never rearranges itself while a human is reading it.


def _live_node(type_name, node_id, spec):
    node = {str(k): v for k, v in spec.items() if v is not None}
    node["type"] = type_name
    node["id"] = str(node_id)
    return node


def disclosure(node_id, label, *nodes, default_expanded=False):
    """A collapsible column. Local choices survive updates; receipts start collapsed."""
    return _live_node(
        "group",
        node_id,
        {
            "label": str(label),
            "direction": "column",
            "fields": list(nodes),
            "is_collapsible": True,
            "default_expanded": default_expanded,
        },
    )


def code(node_id, text, *, language=None, **spec):
    """Literal code; whitespace is retained and content is never executed."""
    return _live_node("code", node_id, dict(spec, text=str(text), language=language))


def spinner(node_id, text="Working", *, variant="braille", is_active=True, **spec):
    """A braille, dots, line or pulse indicator. Receipts never animate."""
    return _live_node(
        "spinner",
        node_id,
        dict(spec, text=str(text), variant=variant, is_active=is_active),
    )


def button(node_id, label, *, is_disabled=False, **spec):
    """An operator action. Accepted presses increment ``clicks`` in ``view.state()``.

    No callback or code crosses the wire. The producer decides how to respond.
    Disabled buttons and completed receipts cannot be activated.
    """
    return _live_node(
        "button", node_id, dict(spec, label=str(label), is_disabled=is_disabled)
    )


def status(node_id, text=None, **spec):
    # One line saying what is happening right now: text, tone, detail, label.
    return _live_node("status", node_id, dict(spec, text=text))


def progress(node_id, **spec):
    # A bar: value (0..1), or done/total. Neither is indeterminate.
    return _live_node("progress", node_id, spec)


def stat(node_id, stats=None, **spec):
    # A strip of counters, each `{'id': ..., 'label': ..., 'value_text': ...}`.
    return _live_node("stat", node_id, dict(spec, stats=stats))


def steps(node_id, steps=None, **spec):
    # A checklist, each `{'id': ..., 'label': ..., 'tone': ...}`.
    return _live_node("steps", node_id, dict(spec, steps=steps))


def output(node_id, **spec):
    """Retained output with an independent disclosure, collapsed by default.

    ``default_expanded=True`` opens an active log initially. Updates preserve the
    local choice; completion starts a collapsed receipt. Hiding never clears lines.
    ``window_lines`` bounds only the hot window, not the durable record.
    """
    return _live_node("log", node_id, spec)


def table(node_id, columns=None, **spec):
    # Rows keyed by id. `order` declares how they paint — 'insertion' (the
    # default), 'newest-first', or {'by': 'duration', 'dir': 'desc'}.
    # `is_selectable=True` makes rows controls; `selected_ids` is shared state.
    return _live_node("table", node_id, dict(spec, columns=columns))


def table_column(column_id, label=None, **spec):
    # One declared column: the id a cell sits under, its header, its align.
    return _live_item(column_id, dict(spec, label=label))


def table_row(row_id, cells, **spec):
    # One declared row: the id every later upsert addresses, and its cells.
    return _live_item(row_id, dict(spec, cells=[_cell(c) for c in cells]))


def link(node_id, links=None, **spec):
    # Pointers a human can open: url, path or attachment.
    return _live_node("link", node_id, dict(spec, links=links))
