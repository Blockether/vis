# vis sandbox YAML-compat shim.
#
# The agent sandbox ships no CPython PyYAML wheel. This shim publishes a
# PyYAML-compatible yaml module whose load/dump DELEGATE to the pure-Clojure
# YAMLStar loader (org.yamlstar/yamlstar), bridged as the host callables
# __vis_yaml_load__ / __vis_yaml_load_all__ / __vis_yaml_dump__ /
# __vis_yaml_dump_all__ (looked up in globals() at CALL time, so the shim
# self-adapts). Published into sys.modules so `import yaml` works, and stapled
# onto builtins so yaml.safe_load(...) needs no import. YAMLStar is a YAML 1.2
# loader and is always SAFE, so PyYAML Loader=/Dumper= kwargs are accepted for
# signature compatibility and ignored.


def __vis_install_yaml_compat__():
    import sys
    import types

    _MISSING = (
        "vis: the YAMLStar backend is not bound in this sandbox "
        "(__vis_yaml_load__ missing) - cannot parse or emit YAML."
    )

    class YAMLError(Exception):
        pass

    def _realize(x):
        # Foreign polyglot proxies (ProxyHashMap/ProxyArray from the YAMLStar
        # bridge) -> REAL python dict/list so isinstance(_, dict), {**_} and
        # json.dumps(_) behave like PyYAML. Native values pass through. NOT
        # __vis_pyify__, which would stamp an 'op'-keyed YAML doc as a tool card.
        isf = globals().get("__vis_is_foreign__")
        if isf is None or not isf(x):
            return x
        if hasattr(x, "keys"):
            try:
                return {_k: _realize(_v) for _k, _v in x.items()}
            except Exception:
                return x
        try:
            return [_realize(_e) for _e in x]
        except Exception:
            return x

    def _call(name, arg):
        fn = globals().get(name)
        if fn is None:
            raise YAMLError(_MISSING)
        # The bridge returns a 2-item envelope [ok, payload]: [True, data] on
        # success, [False, message] on any host error. Turning a parse failure
        # into DATA lets the shim raise a catchable YAMLError (a raw host
        # exception would NOT be caught by Python `except Exception`).
        env = fn(arg)
        if not env[0]:
            raise YAMLError(env[1])
        return _realize(env[1])

    def _text(stream):
        # PyYAML accepts a str/bytes or a file-like object exposing .read().
        if hasattr(stream, "read"):
            stream = stream.read()
        if isinstance(stream, (bytes, bytearray)):
            stream = bytes(stream).decode("utf-8")
        return stream if stream is not None else ""

    def load(stream, Loader=None):
        return _call("__vis_yaml_load__", _text(stream))

    def load_all(stream, Loader=None):
        for d in _call("__vis_yaml_load_all__", _text(stream)) or []:
            yield d

    def safe_load(stream):
        return load(stream)

    def full_load(stream):
        return load(stream)

    def unsafe_load(stream):
        return load(stream)

    def safe_load_all(stream):
        return load_all(stream)

    def full_load_all(stream):
        return load_all(stream)

    def unsafe_load_all(stream):
        return load_all(stream)

    def _emit(bridge_name, value, stream):
        text = _call(bridge_name, value)
        if stream is None:
            return text
        stream.write(text)
        return None

    def dump(data, stream=None, Dumper=None, **kwargs):
        return _emit("__vis_yaml_dump__", data, stream)

    def dump_all(documents, stream=None, Dumper=None, **kwargs):
        return _emit("__vis_yaml_dump_all__", list(documents), stream)

    def safe_dump(data, stream=None, **kwargs):
        return _emit("__vis_yaml_dump__", data, stream)

    def safe_dump_all(documents, stream=None, **kwargs):
        return _emit("__vis_yaml_dump_all__", list(documents), stream)

    def _sentinel(name):
        # Loader/Dumper stand-ins so `yaml.load(s, Loader=yaml.SafeLoader)` and
        # `yaml.dump(x, Dumper=yaml.SafeDumper)` type-check; the safe YAMLStar
        # backend ignores which one is passed.
        return type(name, (object,), {})

    mod = types.ModuleType("yaml")
    mod.__doc__ = (
        "vis PyYAML-compatible shim backed by YAMLStar (pure-Clojure YAML 1.2)."
    )
    mod.__version__ = "6.0-yamlstar"
    mod.YAMLError = YAMLError
    mod.load = load
    mod.load_all = load_all
    mod.safe_load = safe_load
    mod.full_load = full_load
    mod.unsafe_load = unsafe_load
    mod.safe_load_all = safe_load_all
    mod.full_load_all = full_load_all
    mod.unsafe_load_all = unsafe_load_all
    mod.dump = dump
    mod.dump_all = dump_all
    mod.safe_dump = safe_dump
    mod.safe_dump_all = safe_dump_all
    for _n in (
        "BaseLoader",
        "SafeLoader",
        "FullLoader",
        "Loader",
        "UnsafeLoader",
        "CLoader",
        "CSafeLoader",
        "CFullLoader",
        "BaseDumper",
        "SafeDumper",
        "Dumper",
        "CDumper",
        "CSafeDumper",
    ):
        setattr(mod, _n, _sentinel(_n))
    sys.modules["yaml"] = mod

    # Autoload: staple onto builtins so yaml.safe_load(...) works in every
    # python_execution block WITHOUT an explicit `import yaml` (mirrors json/os).
    try:
        import builtins as _b

        _b.yaml = mod
    except Exception:
        pass


__vis_install_yaml_compat__()
del __vis_install_yaml_compat__
