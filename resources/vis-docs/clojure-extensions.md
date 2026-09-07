# Clojure extensions

A Clojure extension is a library on the classpath that registers tools, sandbox
shims, providers, channels, language packs or slash commands. It compiles into
the binary and ships with a [distribution](distributions.md). This page is the
reference for the manifest, the declaration, tools and the documentation the
model reads. For project-local tools written in Python, see
[Extending Vis](extending.md).

## Loading

A distribution ships one manifest, `resources/META-INF/vis/manifest.edn`:

```clojure
{:initialization
 [com.blockether.vis.internal.foundation.core/register!
  {:register com.acme.ext.weather.core/register!
   :apropos  "META-INF/vis/apropos/weather.edn"}
  {:register    com.acme.ext.onnx.core/register!
   :is-optional true
   :because     "the native runtime ships per platform and may be absent"}]}
```

At startup Vis calls each `:register` symbol once, in order. There is no
classpath scan: a jar does nothing until the manifest names it. A map entry
also names the `:apropos` resource that carries the pack's documents. A
required initializer that fails throws; `:is-optional true` logs the failure
instead and must carry `:because`. The same vector is the native-image root
set, so runtime initialization and the native build cannot drift apart.

To get on the classpath, add the extension to `deps.edn` and its `register!`
entry to the manifest, then rebuild with `vis-agent update --rebuild` for a
native binary.

## Layout

```text
my-extension/
├── deps.edn
├── src/com/acme/ext/weather/core.clj
└── resources/
    ├── META-INF/vis/apropos/weather.edn          ; symbol and page records
    ├── META-INF/native-image/com.acme/weather/   ; only for reflective libraries
    │   └── reachability-metadata.json
    └── vis-docs/weather.md                       ; optional page
```

`com.blockether.vis.core` is the public entry point. Engine internals are not
an extension API; declarations that cross a process boundary use the JSON
documents published by `com.blockether/vis-contract`.

## The declaration

`vis/extension` validates the map and fills defaults; `vis/register-extension!`
adds it to the registry.

| Key | Purpose |
| --- | --- |
| `:ext/name`, `:ext/description` | identity and the one line shown in listings |
| `:ext/kind` | section label: `"foundation"`, `"language"`, `"channel"`, `"provider"`, … |
| `:ext/engine` | `{:ext.engine/alias 'weather :ext.engine/symbols [...]}`, the sandbox surface |
| `:ext/activation-fn` | `(fn [env] -> boolean)`, once per turn; falsy hides the extension that turn |
| `:ext/prompt-fn` | `(fn [env] -> string)`, dynamic prompt text; never a copy of a docstring |
| `:ext/ctx-fn` | `(fn [env] -> map)` merged into the model's `session` |
| `:ext/sandbox-shims` | host-backed Python modules; see [Sandbox shims](#sandbox-shims) |
| `:ext/slash-commands` | user commands |
| `:ext/doctor-fn` | health checks for `vis-agent doctor` |
| `:ext/settings`, `:ext/env` | declared settings and environment variables |
| `:ext/cli`, `:ext/language-tools`, `:ext/hooks`, `:ext/op-hooks`, `:ext/network-filters`, `:ext/attachment-storage`, `:ext/channel-contributions`, `:ext/theme`, `:ext/channels`, `:ext/providers`, `:ext/workspace-backends` | declarative registrations applied on register and undone on unregister |

The complete list is the `::extension` spec in
`com.blockether.vis.internal.extension.core`. Read a first-party extension of
the same kind as the reference implementation.

## Tools

A tool is a `defn` wrapped with `vis/symbol` and listed under
`:ext.engine/symbols`:

```clojure
(defn- lookup-fn
  "Read live weather when current conditions are required. ONE city."
  [city]
  (extension/success {:result {:city city :summary "sunny, 21°C"}}))

(def lookup-symbol
  (vis/symbol #'lookup-fn {:symbol 'lookup :tag :observation}))
```

- Pass the var, never a bare function: its docstring and arglists become the
  page `doc("weather_lookup")` renders.
- The Python name is `<alias>_<symbol>` in snake_case. Kebab-case folds to
  snake_case, and a trailing `?` or `!` is dropped.
- `:tag` is required: `:observation` for reads, `:mutation` for writes.
- Arguments arrive as plain values; a Python dict becomes a map with keyword
  keys. Use arities for optional arguments.
- Return `extension/success {:result value}`. Throw, or return
  `extension/failure {:result nil :error {:message "…" :hint "…"}}`, to fail;
  the model sees a Python exception. Map keys convert from kebab to snake case.

Further `vis/symbol` options: `:before-fn` (for example, inject the turn's
`env` as the first argument), `:hidden?`, and the four rendering keys below.

## What the model reads

`python_execution` is the only tool a provider receives a schema for. Every
symbol is a bare Python name inside that sandbox, so the model discovers it
with `apropos(pattern)` and reads its contract with `doc(name)`. Both are
rendered from the symbol entry, and writing a tool is writing them.

`doc("grep")` renders:

```text
# grep  ·  callable                                       <- the sandbox name

grep(options, **kwargs)                                   <- :call, else the real arglists
Keys: query · paths · include · exclude · is_regex …      <- :params

FIND WHERE something is — the codebase-wide search …      <- :description, or the docstring

Raw result: Text, not a map: line 1 summarizes …          <- :result
```

| Entry key | Holds | Must not hold |
| --- | --- | --- |
| docstring or `:description` | routing, preconditions, side effects, result semantics | the signature; the page prints it once already |
| `:result` | the raw result shape, appended as `Raw result: …` | workflow prose |
| `:params` | one `{:name "paths" :required? true :note "…"}` per options-dict key, rendered as `Keys: paths (REQUIRED) · …` | positional arguments |
| `:call` | the positional shape when it differs from the arglists, such as `{:pos ["repository"] :opt-pos ["opts"] :rest :always}` | a shape that contradicts an arity |
| `:ext/prompt-fn` | availability, routing, catalogs | anything `doc(name)` already answers |

`apropos` applies one regular expression to symbol names only and answers
items in manifest order:

```python
apropos(r"^(patch|token-optimization)$")
# [AproposItem(type='tool', name='patch',
#              body='Apply EVERY anchored edit for one file in a single atomic write — prose, c…'),
#  AproposItem(type='doc', name='token-optimization', body='…')]

apropos(r"^pandas\.read_csv$")
# [AproposItem(type='function', name='pandas.read_csv',
#              body='Read a CSV file into a DataFrame. Ignores dtype and parse_dates …')]
```

`type` is `function`, `class`, `module`, `tool`, `doc` or `skill`; `name` is
the handle `doc()` reads; `body` is the first 100 characters of the text. The
search never scores, sorts or reads bodies, so:

1. Choose a stable, searchable name; a module member uses its dotted address.
2. Never write the call into the prose.
3. Declare every options-dict key in `:params` once, spelled as the model types it.
4. State the result shape in `:result`, naming the exact keys.
5. Open the description with a concrete one-line preview.

A symbol with neither a docstring nor `:description` has no page. After adding
one, read `apropos(r"^weather_lookup$")` and `print(doc("weather_lookup"))`
the way the model meets them.

## Sandbox shims

A shim publishes a host-backed Python API into the model's sandbox: a familiar
Python façade whose work is done by Clojure callables. Packages from an index
need no shim; the prebound `ls(...)` is one, shipped as the built-in
`foundation.shim-ls` extension through this same path.

```clojure
{:shim/name     "ls"
 ;; Exact top-level modules a caller may import, and exact names callable with
 ;; no import. The build harvests their Python docstrings into the apropos resource.
 :shim/imports  []
 :shim/globals  ["ls"]
 ;; Extra doctrine no single public name owns.
 :shim/docs     "Host-backed, ignore-aware workspace directory listing."
 ;; Host callables the Python delegates to, wired onto the sandbox globals
 ;; before the source evaluates. Return errors as data the façade turns into
 ;; a Python exception.
 :shim/bindings (fn [] {"__vis_list_directories__" list-directories})
 ;; Classpath resource holding the real .py file; there is no inline form.
 :shim/source   "vis-shims/ls.py"}
```

- The source publishes its module into `sys.modules` or staples names onto
  `builtins`. Built-in shims live in `resources/vis-shims/`; ship yours on
  your classpath and, for a native image, include it with
  `-H:IncludeResources=<prefix>/.*`.
- A missing `:shim/source` throws at load. Each source evaluates lazily on the
  first import or first touch of a global.
- Shims install before the sandbox baseline snapshot, so bridge names are
  hidden from the model's variable view. A shim that throws is logged and
  skipped.
- Every name in `:shim/globals` must be documented in the Python source;
  contract tests reject undocumented names. `apropos-resource-test/regenerate!`
  refreshes the harvested resource.

## Slash commands and activation

```clojure
:ext/slash-commands
[{:slash/name   "weather"
  :slash/doc    "Show current weather."
  :slash/usage  "/weather <city>"
  :slash/run-fn (fn [ctx]
                  {:slash/status :ok
                   :slash/title  "Sunny in Oslo"
                   :slash/data   {:city "Oslo"}})}]
```

`:slash/status` is `:ok` or `:error`; `:slash/data` is optional.

`:ext/activation-fn` hides an extension that cannot work in the current
workspace, at no prompt cost:

```clojure
(defn- activation-fn [env]
  (boolean (some-> (:workspace/root env) (io/file "package.json") .isFile)))
```

Asking the human from Clojure uses `vis/request-human-input!`; see
[Asking the human](human-input.md#clojure-builders).

## Documentation pages

A page is a Markdown resource plus one record in the apropos file the manifest
names:

```clojure
;; resources/META-INF/vis/apropos/weather.edn
[{:name "weather" :kind "doc" :resource "vis-docs/weather.md"}]
```

`doc("weather")` reads the page. The docs site additionally needs an entry in
`resources/vis-docs/site.edn`, the only place that titles, groups and orders a
page; a page the site never navigates to fails the render.

## Complete example

```clojure
(ns com.acme.ext.weather.core
  "Weather lookups under the `weather_` alias."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension.core :as extension]))

(defn- lookup-fn
  "Implementation for a current-conditions lookup."
  [city]
  (extension/success {:result {:city (str city) :summary "sunny, 21°C"}}))

(def ^:private symbols
  [(vis/symbol
     #'lookup-fn
     {:symbol 'lookup
      :tag :observation
      :description "Read live weather when current conditions are required. ONE city."
      :result "Object with string `city` and string `summary`."
      :call {:pos ["city"]}})])

(def vis-extension
  (vis/extension
   {:ext/name        "weather"
    :ext/description "Current-conditions weather lookups for the model."
    :ext/version     "0.1.0"
    :ext/kind        "integration"
    :ext/engine      {:ext.engine/alias 'weather
                      :ext.engine/symbols symbols}}))

(defn register!
  []
  (vis/register-extension! vis-extension))
```

Add `com.acme.ext.weather.core/register!` to the manifest, rebuild, and the
model can call `weather_lookup("Oslo")`.

## Native image rules

- No `defrecord`, `deftype` or `gen-class` in sandbox-facing code; the build
  refuses them. Use maps and functions.
- Reachability metadata travels in your jar as
  `META-INF/native-image/<group>/<artifact>/reachability-metadata.json`. Add
  only what your extension uniquely needs; never copy a library's own config.
- Generate it with the tracing agent
  (`java -agentlib:native-image-agent=config-merge-dir=<dir> …`), then strip
  Clojure-internal noise.
- Resources read with `io/resource` at runtime need a resource entry; the agent
  captures only what the trace touched.

See [Building the native binary](jvm-native-image.md) for the build itself.

## Testing

Vis uses [Lazytest](https://github.com/NoahTheDuke/lazytest). Test tool
functions against the envelope contract:

```clojure
(ns com.acme.ext.weather.core-test
  (:require
   [com.blockether.vis.internal.extension.core :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe lookup-test
  (it "returns a canonical success envelope"
    (let [result (@#'com.acme.ext.weather.core/lookup-fn "Oslo")]
      (expect (extension/envelope-success? result))
      (expect (= "Oslo" (:city (:result result)))))))
```

Before shipping, run `format_code`, `clojure -M:lint` and the affected tests.

## See also

- [Extending Vis](extending.md) — Python extensions, reloadable in place.
- [Python sandbox](python-sandbox.md) — the interpreter a shim publishes into.
- [Distributions](distributions.md) — building and shipping a custom binary.
- [Building the native binary](jvm-native-image.md) — GraalVM constraints and metadata.
