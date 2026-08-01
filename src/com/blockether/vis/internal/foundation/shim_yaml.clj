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
     "Sandbox PyYAML `yaml` load/dump subset (including `safe_load` and `*_all`) via pure-Clojure YAMLStar 1.2. No pip/wheel."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "yaml"
       :shim/imports ["yaml"]
       :shim/description
       "PyYAML-compatible `yaml` via pure-Clojure YAMLStar (YAML 1.2). Limits: map keys return as strings; no custom tags/`!!python` or arbitrary-object (de)serialization."
       :shim/bindings yaml-bridge-bindings
       :shim/source "vis-shims/yaml.py"}]}))

(vis/register-extension! vis-extension)
