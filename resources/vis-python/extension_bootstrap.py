import sys as _vis_sys, types as _vis_types

_vis_body = """
import inspect

_registration = {'spec': None}

def extension(name=None, description=None, version=None, kind=None, alias=None,
              activation=None, symbols=None, prompt=None, slash_commands=None,
              op_hooks=None, ctx=None, providers=None, network_filters=None):
    if _registration['spec'] is not None:
        raise ValueError('vis.extension() may only be called once per file')
    if not name or not isinstance(name, str):
        raise ValueError('vis.extension(...) requires name=<non-empty string>')
    if not description or not isinstance(description, str):
        raise ValueError('vis.extension(...) requires description=<non-empty string>')
    if symbols and not alias:
        raise ValueError('vis.extension(...) requires alias=<string> when symbols= is declared')
    if ctx is not None and not callable(ctx):
        raise ValueError('vis.extension(...) ctx= must be a callable (env) -> dict of session contributions')
    _registration['spec'] = {
        'name': name, 'description': description, 'version': version,
        'kind': kind, 'alias': alias, 'activation': activation,
        'symbols': list(symbols or []), 'prompt': prompt,
        'slash_commands': list(slash_commands or []),
        'op_hooks': list(op_hooks or []), 'ctx': ctx,
        'providers': list(providers or []),
        'network_filters': list(network_filters or []),
    }

def _kwargs_dict(x):
    # The folded-kwargs map crosses the host boundary as a FOREIGN hash map, not a
    # Python dict, so duck-type it (keys + item access) instead of isinstance().
    if isinstance(x, (str, bytes, bytearray, list, tuple)) or not hasattr(x, 'keys'):
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
    _call.__name__ = getattr(fn, '__name__', 'symbol')
    _call.__doc__ = fn.__doc__
    return _call

def symbol(fn, name=None, tag='observation', is_hidden=False, schema=None,
           description=None, result=None, is_native_tool=False, render=None,
           color_role=None):
    if not callable(fn):
        raise ValueError('vis.symbol(fn, ...) requires a callable')
    if tag not in ('observation', 'mutation'):
        raise ValueError('vis.symbol tag must be observation or mutation, got %r' % (tag,))
    doc = inspect.getdoc(fn)
    if not doc or not doc.strip():
        raise ValueError('vis.symbol: %s needs a docstring - it becomes the model-facing doc()'
                         % (getattr(fn, '__name__', '?'),))
    params, varargs = [], False
    for p in inspect.signature(fn).parameters.values():
        if p.kind == inspect.Parameter.VAR_POSITIONAL:
            varargs = True
        elif p.kind in (inspect.Parameter.POSITIONAL_ONLY,
                        inspect.Parameter.POSITIONAL_OR_KEYWORD):
            params.append(p.name)
    label = name or getattr(fn, '__name__', '?')
    is_native = bool(is_native_tool) or schema is not None
    if is_native:
        if not isinstance(schema, dict):
            raise ValueError('vis.symbol: native tool %s requires schema=<JSON Schema dict>'
                             % (label,))
        if schema.get('type') != 'object':
            raise ValueError('vis.symbol: native tool %s schema root must be type object'
                             % (label,))
        for union in ('oneOf', 'anyOf', 'allOf'):
            if union in schema:
                raise ValueError('vis.symbol: native tool %s schema root must not use %s; '
                                 'nested property unions are allowed' % (label, union))
        for slot, text in (('description', description), ('result', result)):
            if not text or not isinstance(text, str) or not text.strip():
                raise ValueError('vis.symbol: native tool %s requires %s=<non-empty string>'
                                 % (label, slot))
            if 'Raw result:' in text:
                raise ValueError('vis.symbol: %s= must not carry the reserved label '
                                 'Raw result: - put the bare contract in result=' % (slot,))
    elif description is not None or result is not None:
        raise ValueError('vis.symbol: description=/result= describe a NATIVE tool; '
                         'pass schema= as well (%s)' % (label,))
    if render is not None and not callable(render):
        raise ValueError('vis.symbol render= must be a callable (result) -> dict with '
                         'summary and optional body')
    if color_role is not None and not isinstance(color_role, str):
        raise ValueError('vis.symbol color_role= must be a string, e.g. search')
    return {'marker': 'symbol', 'fn': _kwargs_call(fn), 'name': name or fn.__name__, 'tag': tag,
            'hidden': bool(is_hidden), 'is_native_tool': is_native,
            'schema': schema, 'description': description, 'result': result,
            'render': render, 'color_role': color_role,
            'doc': doc, 'params': params, 'varargs': varargs}

def slash(name, run, doc=None, usage=None):
    if not name or not isinstance(name, str):
        raise ValueError('vis.slash(name, run, ...) requires name=<non-empty string>')
    if not callable(run):
        raise ValueError('vis.slash(name, run, ...) requires a callable run')
    return {'marker': 'slash', 'name': name, 'run': run, 'doc': doc, 'usage': usage}

def op_hook(ops, fn, phase='before'):
    if phase not in ('before', 'after'):
        raise ValueError('vis.op_hook phase must be before or after, got %r' % (phase,))
    if not callable(fn):
        raise ValueError('vis.op_hook(ops, fn, ...) requires a callable fn')
    ops = [str(o) for o in (ops or [])]
    if not ops:
        raise ValueError('vis.op_hook requires a non-empty ops list')
    return {'marker': 'op_hook', 'ops': ops, 'fn': fn, 'phase': phase}

def network_filter(fn):
    if not callable(fn):
        raise ValueError('vis.network_filter(fn) requires a callable')
    return {'marker': 'network_filter', 'fn': fn}

def provider(id, label, preset=None, get_token_fn=None, detect_fn=None,
             status_fn=None, logout_fn=None, limits_fn=None, refresh_token_fn=None,
             auth_fn=None, auth_prompt_fn=None, enrich_models_fn=None,
             on_selected_fn=None):
    if not id or not isinstance(id, str):
        raise ValueError('vis.provider(...) requires id=<non-empty string>')
    if not label or not isinstance(label, str):
        raise ValueError('vis.provider(...) requires label=<non-empty string>')
    for slot, f in (('get_token_fn', get_token_fn), ('detect_fn', detect_fn),
                    ('status_fn', status_fn), ('logout_fn', logout_fn),
                    ('limits_fn', limits_fn), ('refresh_token_fn', refresh_token_fn),
                    ('auth_fn', auth_fn), ('auth_prompt_fn', auth_prompt_fn),
                    ('enrich_models_fn', enrich_models_fn),
                    ('on_selected_fn', on_selected_fn)):
        if f is not None and not callable(f):
            raise ValueError('vis.provider %s= must be callable or None' % (slot,))
    return {'marker': 'provider', 'id': id, 'label': label,
            'preset': dict(preset or {}), 'get_token_fn': get_token_fn,
            'detect_fn': detect_fn, 'status_fn': status_fn, 'logout_fn': logout_fn,
            'limits_fn': limits_fn, 'refresh_token_fn': refresh_token_fn,
            'auth_fn': auth_fn, 'auth_prompt_fn': auth_prompt_fn,
            'enrich_models_fn': enrich_models_fn, 'on_selected_fn': on_selected_fn}

def ok(title, body=None, data=None):
    return {'marker': 'slash_result', 'status': 'ok', 'title': str(title),
            'body': body, 'data': data}

def err(title, body=None, data=None):
    return {'marker': 'slash_result', 'status': 'error', 'title': str(title),
            'body': body, 'data': data}

def block(reason):
    return {'marker': 'block', 'reason': str(reason)}

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
        v = _host['state_get'](str(key))
        return default if v is None else v
    def __getitem__(self, key):
        v = _host['state_get'](str(key))
        if v is None:
            raise KeyError(key)
        return v
    def __setitem__(self, key, value):
        _host['state_put'](str(key), value)
    def __delitem__(self, key):
        _host['state_del'](str(key))
    def __contains__(self, key):
        return _host['state_get'](str(key)) is not None

state = _State()

def log(level, msg):
    _host['log'](str(level), str(msg))

def notify(text, level='info'):
    _host['notify'](str(text), str(level))

def shell(opts):
    # The public extension shell has exactly one call shape: one options map.
    # Process commands live in {'commands': ['...']}; lifecycle-only calls use
    # the same map, e.g. shell({'op': 'logs', 'id': 'dev'}).
    if not isinstance(opts, dict):
        raise TypeError(
            "shell takes one options map — use shell({'commands': ['ls']})"
        )
    return _host['jailed_shell'](opts)

class Answer:
    # The outcome of `vis.ask(...)`. Truthy only when the human submitted.
    # `values` is keyed by each field's `name` and always carries every field; a
    # `password` field holds an opaque `vis-secret:` handle, never plaintext.
    def __init__(self, raw):
        raw = raw or {}
        self.is_submitted = bool(raw.get('is_submitted'))
        self.reason = str(raw.get('reason') or 'cancelled')
        self.request_id = raw.get('request_id')
        self.values = dict(raw.get('values') or {})

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
        return 'Answer(is_submitted=%r, reason=%r, fields=%r)' % (
            self.is_submitted,
            self.reason,
            sorted(self.values),
        )

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
    # is_required, placeholder, options, max_length.
    # Field types: plaintext, password, multiline, select, multiselect,
    # checkbox. Dialog options: description (prose under the title explaining
    # what the whole ask is about — it wraps), submit_label, cancel_label,
    # is_cancellable, timeout_ms (default 5 min, capped at 1 hour).
    # A cancelled, timed-out or unanswered request returns a falsey Answer
    # whose `reason` says which — it never raises. `reason == 'undeliverable'`
    # means no surface was mounted to show the dialog: the host logged an error
    # and gave up at once instead of parking you until the timeout.
    import json
    if not isinstance(fields, (list, tuple)) or not fields:
        raise TypeError('ask needs a non-empty list of field specs')
    specs = []
    for f in fields:
        if not isinstance(f, dict):
            raise TypeError('each field spec is a dict of snake_case string keys')
        specs.append(dict((str(k), v) for k, v in f.items()))
    request = dict((str(k), v) for k, v in options.items())
    request['title'] = str(title)
    request['fields'] = specs
    return Answer(json.loads(_host['request_input'](json.dumps(request))))

def reveal(handle):
    # Resolve an opaque `vis-secret:` handle to its plaintext, or None when the
    # handle is unknown or already forgotten. Never log or return the result.
    if not handle:
        return None
    return _host['reveal_secret'](str(handle))

def forget(handle):
    # Drop the plaintext behind a handle as soon as it is no longer needed.
    if not handle:
        return False
    return bool(_host['forget_secret'](str(handle)))

# Compatibility for extensions written before the public `vis.shell` spelling.
jailed_shell = shell
"""

_vis_mod = _vis_types.ModuleType("vis")
_vis_mod.__dict__["_host"] = {
    "state_get": __vis_host_state_get__,
    "state_put": __vis_host_state_put__,
    "state_del": __vis_host_state_del__,
    "log": __vis_host_log__,
    "notify": __vis_host_notify__,
    "jailed_shell": __vis_host_jailed_shell__,
    "request_input": __vis_host_request_input__,
    "reveal_secret": __vis_host_reveal_secret__,
    "forget_secret": __vis_host_forget_secret__,
}
exec(compile(_vis_body, "<vis-bootstrap>", "exec"), _vis_mod.__dict__)
_vis_sys.modules["vis"] = _vis_mod


def __vis_registration__():
    return _vis_sys.modules["vis"].__dict__["_registration"]["spec"]
