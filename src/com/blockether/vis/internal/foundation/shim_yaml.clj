(ns com.blockether.vis.internal.foundation.shim-yaml
  "Built-in sandbox SHIM: a PyYAML-compatible `yaml` module for the model's
   Python sandbox, backed by the pure-Clojure YAMLStar loader
   (`org.yamlstar/yamlstar`). No CPython PyYAML wheel ships in the sandbox; this
   extension contributes a `:ext/sandbox-shims` entry that
   `env-python/build-agent-context` installs into every sandbox Context (main +
   every `sub_loop` fork): the host bridge callables are wired onto the globals,
   then the Python preamble publishes a `yaml` module into `sys.modules` (so
   `import yaml` works) and staples it onto builtins (so `yaml.safe_load(...)`
   works with NO import).

   This is the reference example of the sandbox-shim mechanism: a host / JVM
   capability surfaced to sandbox Python as a real importable module, with the
   engine staying completely generic about which shims exist."
  (:require [com.blockether.vis.core :as vis]
            [yamlstar.core :as yamlstar]))

(def ^:private yaml-compat-shim-src
  "Pure-Python preamble that publishes a PyYAML-compatible `yaml` module backed
   by the pure-Clojure YAMLStar loader (`org.yamlstar/yamlstar`). All parse/emit
   work is DELEGATED to the host callables `__vis_yaml_load__` /
   `__vis_yaml_load_all__` / `__vis_yaml_dump__` / `__vis_yaml_dump_all__` (bound
   from `yaml-bridge-bindings`), looked up in `globals()` at CALL time so the
   shim is backend-agnostic. Published into `sys.modules` under `yaml` (so
   `import yaml` finds it) AND stapled onto builtins (so `yaml.safe_load(...)`
   works with NO import). INLINED here so it ships in-jar with no separate `.py`
   resource. Installed once per sandbox context (main + every sub_loop fork),
   BEFORE the baseline snapshot so its `__vis_*` names are filtered out of the
   model-visible live-vars view. Python uses single-quoted string literals
   throughout so this Clojure string needs no escaping."
  "# vis sandbox YAML-compat shim.
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
        'vis: the YAMLStar backend is not bound in this sandbox '
        '(__vis_yaml_load__ missing) - cannot parse or emit YAML.'
    )

    class YAMLError(Exception):
        pass

    def _realize(x):
        # Foreign polyglot proxies (ProxyHashMap/ProxyArray from the YAMLStar
        # bridge) -> REAL python dict/list so isinstance(_, dict), {**_} and
        # json.dumps(_) behave like PyYAML. Native values pass through. NOT
        # __vis_pyify__, which would stamp an 'op'-keyed YAML doc as a tool card.
        isf = globals().get('__vis_is_foreign__')
        if isf is None or not isf(x):
            return x
        if hasattr(x, 'keys'):
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
        if hasattr(stream, 'read'):
            stream = stream.read()
        if isinstance(stream, (bytes, bytearray)):
            stream = bytes(stream).decode('utf-8')
        return stream if stream is not None else ''

    def load(stream, Loader=None):
        return _call('__vis_yaml_load__', _text(stream))

    def load_all(stream, Loader=None):
        for d in (_call('__vis_yaml_load_all__', _text(stream)) or []):
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
        return _emit('__vis_yaml_dump__', data, stream)

    def dump_all(documents, stream=None, Dumper=None, **kwargs):
        return _emit('__vis_yaml_dump_all__', list(documents), stream)

    def safe_dump(data, stream=None, **kwargs):
        return _emit('__vis_yaml_dump__', data, stream)

    def safe_dump_all(documents, stream=None, **kwargs):
        return _emit('__vis_yaml_dump_all__', list(documents), stream)

    def _sentinel(name):
        # Loader/Dumper stand-ins so `yaml.load(s, Loader=yaml.SafeLoader)` and
        # `yaml.dump(x, Dumper=yaml.SafeDumper)` type-check; the safe YAMLStar
        # backend ignores which one is passed.
        return type(name, (object,), {})

    mod = types.ModuleType('yaml')
    mod.__doc__ = 'vis PyYAML-compatible shim backed by YAMLStar (pure-Clojure YAML 1.2).'
    mod.__version__ = '6.0-yamlstar'
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
    for _n in ('BaseLoader', 'SafeLoader', 'FullLoader', 'Loader', 'UnsafeLoader',
               'CLoader', 'CSafeLoader', 'CFullLoader',
               'BaseDumper', 'SafeDumper', 'Dumper', 'CDumper', 'CSafeDumper'):
        setattr(mod, _n, _sentinel(_n))
    sys.modules['yaml'] = mod

    # Autoload: staple onto builtins so yaml.safe_load(...) works in every
    # run_python block WITHOUT an explicit `import yaml` (mirrors json/os).
    try:
        import builtins as _b
        _b.yaml = mod
    except Exception:
        pass

__vis_install_yaml_compat__()
del __vis_install_yaml_compat__
")

(defn- yaml-envelope
  "Run thunk `f`, returning the 2-vector the YAML shim expects: [true result] on
   success, [false message] on any Throwable. Errors are returned as DATA (not
   thrown) so a malformed-YAML failure crosses the boundary as a catchable
   `yaml.YAMLError` rather than a raw host `PolyglotException` (GraalPy does not
   route host exceptions through Python's `except Exception`)."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- yaml-bridge-bindings
  "Host callables (pure-Clojure YAMLStar) the YAML-compat shim delegates to.
   Wrapped by `wrap-ifn` at install time, so args marshal Python->Clojure and
   results Clojure->Python across the STRINGS-ONLY boundary: Python hands a YAML
   string in and gets native dict/list/str back; on dump it hands native data
   and gets a YAML string. Map keys land as strings (the boundary contract),
   which is the common case; malformed YAML surfaces as an exception the shim
   maps to `yaml.YAMLError`."
  []
  {"__vis_yaml_load__" (fn [s]
                         (yaml-envelope #(yamlstar/load s)))
   "__vis_yaml_load_all__" (fn [s]
                             (yaml-envelope #(vec (yamlstar/load-all s))))
   "__vis_yaml_dump__" (fn [v]
                         (yaml-envelope #(yamlstar/dump v)))
   "__vis_yaml_dump_all__" (fn [v]
                             (yaml-envelope #(yamlstar/dump-all (vec v))))})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-yaml"
     :ext/description
     "Sandbox shim: a PyYAML-compatible `yaml` module (import yaml / yaml.safe_load) backed by the pure-Clojure YAMLStar 1.2 loader. No pip, no native wheel."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "yaml"
       :shim/description
       "PyYAML-compatible `yaml` module backed by YAMLStar (pure-Clojure YAML 1.2)."
       :shim/bindings yaml-bridge-bindings
       :shim/preamble yaml-compat-shim-src}]}))

(vis/register-extension! vis-extension)
