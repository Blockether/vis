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


class _State:
    def get(self, key, default=None):
        v = _host.state_get(str(key))
        return default if v is None else v

    def __getitem__(self, key):
        v = _host.state_get(str(key))
        if v is None:
            raise KeyError(key)
        return v

    def __setitem__(self, key, value):
        _host.state_put(str(key), value)

    def __delitem__(self, key):
        _host.state_del(str(key))

    def __contains__(self, key):
        return _host.state_get(str(key)) is not None


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


def row(*fields):
    # Lay these nodes out side by side. A group answers nothing and never
    # appears in `values`, which stays flat however deep the tree goes.
    return {"type": "group", "direction": "row", "fields": list(fields)}


def column(*fields):
    # Stack these nodes, the default arrangement: worth saying out loud inside a
    # `row`.
    return {"type": "group", "direction": "column", "fields": list(fields)}


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
