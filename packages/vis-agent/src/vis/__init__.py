"""The Vis extension API — everything an extension file reaches for.

`import vis` inside a Vis extension gets this module FROM THE HOST: the engine
builds it, seeds `_host` with an object whose attributes are the ops named by
`packages/vis-contract/resources/vis-contract/python-host.edn` — the `Host`
protocol `vis-contract` declares — execs this source into it and registers it in
`sys.modules`, so every call below reaches the live agent.

Installed from PyPI (`pip install vis-agent`) there is no Vis process to seed
anything, so `vis._outside` answers each op the way the contract document says it
behaves outside the sandbox: durable state becomes a JSON file, `log`/`notify`
become stderr lines, `ask` becomes a terminal prompt, `shell` becomes a
local subprocess, and the two jailed shells refuse by name because nothing out
here can enforce a jail. That is what makes an extension importable, unit-testable
and runnable with no agent in the room — the same file, one host or the other.
"""

try:
    _host  # noqa: B018, F821 — the host seeds this into the module dict before exec.
except NameError:  # Installed from PyPI: no host in the room, so bring one.
    from vis import _outside as outside

    _host = outside.host

import inspect
import time
from collections.abc import MutableMapping as _MutableMapping

_registration = {"spec": None}


def extension(
    name=None,
    description=None,
    version=None,
    kind=None,
    alias=None,
    activation=None,
    symbols=None,
    prompt=None,
    slash_commands=None,
    op_hooks=None,
    ctx=None,
    providers=None,
    network_filters=None,
    env=None,
):
    if _registration["spec"] is not None:
        raise ValueError("vis.extension() may only be called once per file")
    if not name or not isinstance(name, str):
        raise ValueError("vis.extension(...) requires name=<non-empty string>")
    if not description or not isinstance(description, str):
        raise ValueError("vis.extension(...) requires description=<non-empty string>")
    if symbols and not alias:
        raise ValueError(
            "vis.extension(...) requires alias=<string> when symbols= is declared"
        )
    if ctx is not None and not callable(ctx):
        raise ValueError(
            "vis.extension(...) ctx= must be a callable (env) -> dict of session contributions"
        )
    # DECLARED HOST ENV. The extension names the environment variables it
    # needs; the host resolves each one in the ONE order every Vis surface uses
    # (an `environment:` declaration -> the workspace's .env, then .env.local ->
    # the environment that started Vis) and injects ONLY the resolved values
    # into this context's os.environ. No blanket passthrough of the host
    # environment: an undeclared variable stays invisible to extension code.
    # The PROJECT's own names are the deliberate exception and always arrive --
    # anything under `environment:` or assigned in the workspace's .env -- so
    # those are written once and read from Python without every extension
    # repeating the list.
    if env is not None:
        if isinstance(env, str) or not hasattr(env, "__iter__"):
            raise ValueError(
                "vis.extension(...) env= must be a list of environment variable names"
            )
        env = [str(n) for n in env]
        for n in env:
            if not n or not all(c.isalnum() or c == "_" for c in n) or n[0].isdigit():
                raise ValueError(
                    f"vis.extension(...) env= entries must be environment variable names, got {n!r}"
                )
    # Resolve NOW, not at registration: the host hands back only the values it
    # could resolve, so `os.environ[...]` works for the rest of this file (module
    # level included) and for every later detect/status callback.
    import json as _json
    import os as _os

    for _k, _v in (
        _json.loads(_host.declare_env(_json.dumps(env or []))) or {}
    ).items():
        _os.environ[str(_k)] = str(_v)
    _registration["spec"] = {
        "name": name,
        "description": description,
        "version": version,
        "kind": kind,
        "alias": alias,
        "activation": activation,
        "symbols": list(symbols or []),
        "prompt": prompt,
        "slash_commands": list(slash_commands or []),
        "op_hooks": list(op_hooks or []),
        "ctx": ctx,
        "providers": list(providers or []),
        "network_filters": list(network_filters or []),
        "env": list(env or []),
    }


def host_env(name, default=None):
    # Value of a host environment variable DECLARED in vis.extension(env=[...]).
    # Undeclared names always return `default` -- declaring is the only way in.
    import os as _os

    v = _os.environ.get(str(name))
    return default if v is None else v


def _kwargs_dict(x):
    # The folded-kwargs map crosses the host boundary as a FOREIGN hash map, not a
    # Python dict, so duck-type it (keys + item access) instead of isinstance().
    if isinstance(x, (str, bytes, bytearray, list, tuple)) or not hasattr(x, "keys"):
        return None
    try:
        pairs = {k: x[k] for k in list(x.keys())}
    except Exception:
        return None
    if not pairs:
        return None
    for k in pairs:
        if not isinstance(k, str) or not k.isidentifier():
            return None
    return pairs


def _kwargs_call(fn):
    # KEYWORD ARGUMENTS for a Python-backed tool. Host tool callables are
    # positional-only proxies, so the sandbox folds a caller's **kwargs into ONE
    # TRAILING DICT positional: `mytool('g', want_json=True)` reaches Python as
    # ('g', {'want_json': True}) and every keyword parameter silently keeps its
    # default. Re-expand that trailing map whenever THIS signature binds it by
    # keyword; a genuine dict positional (no such parameter, a positional-only
    # slot, non-identifier keys) fails the bind and passes through untouched.
    try:
        sig = inspect.signature(fn)
    except (TypeError, ValueError):
        return fn

    def _call(*args):
        if args:
            kw = _kwargs_dict(args[-1])
            if kw is not None:
                head = args[:-1]
                try:
                    sig.bind(*head, **kw)
                except TypeError:
                    pass
                else:
                    return fn(*head, **kw)
        return fn(*args)

    _call.__name__ = getattr(fn, "__name__", "symbol")
    _call.__doc__ = fn.__doc__
    return _call


def symbol(fn, name=None, tag="observation", is_hidden=False):
    if not callable(fn):
        raise ValueError("vis.symbol(fn, ...) requires a callable")
    if tag not in ("observation", "mutation"):
        raise ValueError(f"vis.symbol tag must be observation or mutation, got {tag!r}")
    doc = inspect.getdoc(fn)
    if not doc or not doc.strip():
        raise ValueError(
            "vis.symbol: {} needs a docstring - it becomes the model-facing doc()".format(
                getattr(fn, "__name__", "?")
            )
        )
    params, varargs = [], False
    for p in inspect.signature(fn).parameters.values():
        if p.kind == inspect.Parameter.VAR_POSITIONAL:
            varargs = True
        elif p.kind in (
            inspect.Parameter.POSITIONAL_ONLY,
            inspect.Parameter.POSITIONAL_OR_KEYWORD,
        ):
            params.append(p.name)
    return {
        "marker": "symbol",
        "fn": _kwargs_call(fn),
        "name": name or fn.__name__,
        "tag": tag,
        "hidden": bool(is_hidden),
        "doc": doc,
        "params": params,
        "varargs": varargs,
    }


def slash(name, run, doc=None, usage=None):
    if not name or not isinstance(name, str):
        raise ValueError("vis.slash(name, run, ...) requires name=<non-empty string>")
    if not callable(run):
        raise ValueError("vis.slash(name, run, ...) requires a callable run")
    return {"marker": "slash", "name": name, "run": run, "doc": doc, "usage": usage}


GATE_OPS = ("fs_access",)


def op_hook(ops, fn, phase="before"):
    # A GATE op is ASKED, never wrapped: the hook receives that gate's own ctx
    # (fs_access -> {'operation', 'path'}), returns vis.block(reason) to REFUSE,
    # and an error inside it refuses too, because a boundary fails closed. There
    # is nothing to be before or after when the operation has not been allowed
    # yet, so mixing a gate into an ordinary hook is a mistake named here rather
    # than a hook that silently never runs.
    if phase not in ("before", "after"):
        raise ValueError(f"vis.op_hook phase must be before or after, got {phase!r}")
    if not callable(fn):
        raise ValueError("vis.op_hook(ops, fn, ...) requires a callable fn")
    ops = [str(o) for o in (ops or [])]
    if not ops:
        raise ValueError("vis.op_hook requires a non-empty ops list")
    gates = [o for o in ops if o in GATE_OPS]
    if gates and len(gates) != len(ops):
        raise ValueError(
            f"vis.op_hook: {gates[0]!r} is a gate and is asked, not wrapped, so it cannot "
            f"share a hook with {[o for o in ops if o not in GATE_OPS]!r}"
        )
    if gates and phase != "before":
        raise ValueError(
            f"vis.op_hook: {gates[0]!r} is a gate, asked before the operation runs, so "
            f"phase={phase!r} means nothing"
        )
    return {"marker": "op_hook", "ops": ops, "fn": fn, "phase": phase}


def network_filter(fn):
    if not callable(fn):
        raise ValueError("vis.network_filter(fn) requires a callable")
    return {"marker": "network_filter", "fn": fn}


def provider(
    id,
    label,
    preset=None,
    is_managed=False,
    get_token_fn=None,
    detect_fn=None,
    status_fn=None,
    logout_fn=None,
    limits_fn=None,
    refresh_token_fn=None,
    auth_fn=None,
    auth_prompt_fn=None,
    enrich_models_fn=None,
    on_selected_fn=None,
):
    if not id or not isinstance(id, str):
        raise ValueError("vis.provider(...) requires id=<non-empty string>")
    if not label or not isinstance(label, str):
        raise ValueError("vis.provider(...) requires label=<non-empty string>")
    for slot, f in (
        ("get_token_fn", get_token_fn),
        ("detect_fn", detect_fn),
        ("status_fn", status_fn),
        ("logout_fn", logout_fn),
        ("limits_fn", limits_fn),
        ("refresh_token_fn", refresh_token_fn),
        ("auth_fn", auth_fn),
        ("auth_prompt_fn", auth_prompt_fn),
        ("enrich_models_fn", enrich_models_fn),
        ("on_selected_fn", on_selected_fn),
    ):
        if f is not None and not callable(f):
            raise ValueError(f"vis.provider {slot}= must be callable or None")
    return {
        "marker": "provider",
        "id": id,
        "label": label,
        "preset": dict(preset or {}),
        # MANAGED: the runtime issues this provider's credential, so vis never
        # asks for an API key and never needs an "Add provider" step for it.
        "is_managed": bool(is_managed),
        "get_token_fn": get_token_fn,
        "detect_fn": detect_fn,
        "status_fn": status_fn,
        "logout_fn": logout_fn,
        "limits_fn": limits_fn,
        "refresh_token_fn": refresh_token_fn,
        "auth_fn": auth_fn,
        "auth_prompt_fn": auth_prompt_fn,
        "enrich_models_fn": enrich_models_fn,
        "on_selected_fn": on_selected_fn,
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
        # Inside Vis the host answers a polyglot list; outside it, a real one.
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
# `com.blockether.vis.human-input`: the type IS the function and the name is
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


def heading(text):
    # A section title. Pure decoration: no name, no value, never focusable.
    return {"type": "heading", "text": str(text)}


def paragraph(text):
    # Prose under a title, wrapped. Pure decoration, exactly like `heading`.
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
        return self._op("clear")


class Table(_KeyedNode):
    def upsert(self, row_id, cells, tone=None):
        # ONE verb for "new row" and "row changed": a scan loop writing a live
        # table does not know which it is, and the id is the address either way.
        return self._op(
            "append",
            rows=[
                _live_item(row_id, {"cells": [_cell(c) for c in cells], "tone": tone})
            ],
        )

    def focus(self, *item_ids):
        # Focus is shared engine state. A surface writes it and the extension can
        # observe it through `LiveView.state()` on its next update.
        return self._op("set", focused_ids=[str(item_id) for item_id in item_ids])


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
    "status": Status,
    "progress": Progress,
    "stat": Stat,
    "steps": Steps,
    "log": Log,
    "table": Table,
    "link": Link,
}
# The typed handle each node type answers. The engine owns the type table
# (`human-input.spec/live-node-types`, rendered into the contract document);
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
    A repeated write to the same row or the same node collapses into the last
    one, so a per-row progress counter costs one wire row per tick rather than
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
            self._learn(view.get("nodes"))
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

    # -- pushing --------------------------------------------------------------

    def _push(self, op):
        # Leading edge, then a window: the first op after a quiet stretch crosses
        # at once, and whatever arrives within `flush_ms` of it rides the next
        # push, read or close. A loop reporting every iteration costs one host
        # call per window instead of one per iteration.
        if not self._is_open:
            self._refuse_closed()
        self._coalesce(op)
        if self._is_full() or self._since_ms(self._last_flush) >= self._flush_ms:
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

    def row(self, row_id, cells, tone=None):
        return self._only("table", "row").upsert(row_id, cells, tone=tone)

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
        self.flush()
        self._index([node])
        node_id = str(node.get("id"))
        # Adding a ROW hands back the view: layout takes no op, and the nodes it
        # arranged are addressable by their own ids.
        return self.node(node_id) if node_id in self._nodes else self

    def drop(self, node_id):
        """Drop a whole node, its items with it."""
        self._push({"op": "remove-node", "node_id": str(node_id)})
        self.flush()
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
        focus_snapshots=None,
        model_result=None,
    ):
        """End the view and answer the result the model reads.

        ``model_result`` is an optional compact string returned instead of the
        full structured verdict. The finished picture and close metadata remain
        in the durable artifact and on human-facing close events.

        ``focus_snapshots`` are finished pictures keyed by a focusable table and
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
            "focus_snapshots": focus_snapshots,
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
    the ``focus``/``close`` helpers simulate surface or human actions against the
    same materialized view without publishing fixture data into a Vis session.
    """

    _COLLECTION = {"stat": "stats", "steps": "steps", "table": "rows", "link": "links"}

    def __init__(self, inner, view_id="test-live-view"):
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
        leaves = []
        for node in self._nodes(picture.get("nodes")):
            if node.get("type") == "group":
                continue
            node.pop("focused_ids", None)
            node.pop("is_focusable", None)
            leaves.append(node)
        picture["nodes"] = leaves
        return picture

    def host_live(self, envelope_json):
        """Apply an envelope without recording it as extension output."""
        import json

        envelope = json.loads(envelope_json)
        action = envelope.get("op")
        if action == "open":
            self._view = self._materialize(envelope["view"])
            self._result = None
            self._seq = 0
            answer = {"view_id": self.view_id, "is_open": True, "view": self._view}
        elif self._result is not None:
            answer = {"view_id": self.view_id, "is_open": False, "result": self._result}
        elif action == "patch":
            for op in (envelope.get("patch") or {}).get("ops") or []:
                self._apply(op)
            self._seq += 1
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
                "summary": ending.get("summary"),
                "view": self.picture(),
                "elided": {},
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

    def focus(self, node_id, focused_ids):
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
                                    "focused_ids": [str(one) for one in focused_ids],
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
# `com.blockether.vis.human-input`, and the ID IS POSITIONAL because every op an
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
    # Streamed lines. It is `log` on the wire; the builder is `output` so it
    # never shadows `vis.log`, the engine log line — the same reason `slider`
    # builds a `range` field. Unbounded: `window_lines` is only how much of it a
    # surface holds hot, and the view's record keeps every line.
    return _live_node("log", node_id, spec)


def table(node_id, columns=None, **spec):
    # Rows keyed by id. `order` declares how they paint — 'insertion' (the
    # default), 'newest-first', or {'by': 'duration', 'dir': 'desc'}.
    # `is_focusable=True` makes rows controls; `focused_ids` is shared state.
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
