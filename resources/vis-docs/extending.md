# Clojure extensions

Vis is a small core plus **extensions**. There are two flavors: dynamic [Python extensions](python-extensions.md) you drop into `.vis/extensions/`, and **Clojure extensions** — this page — that compile into the binary and reach every surface Vis has.

Clojure extensions are libraries that register tools, providers, channels, language packs, and slash commands. Everything beyond the engine loop ships this way: the git surface, the TUI, the web channel, the Clojure and Python language packs are all extensions living on the classpath. Writing your own follows the same recipe they do.

> Looking for something lighter? [Python extensions](python-extensions.md) are single `.py` files in `.vis/extensions/` — project-local tools, prompt fragments, slash commands and op-hook guards with no rebuild and in-place `/reload`. Clojure extensions (this page) are the full-surface path: channels, providers, persistence backends, TUI, CLI.

An extension does three things:

1. **Declares itself** in a spec map built with `vis/extension` and registered at namespace load with `vis/register-extension!`.
2. **Exposes tools** — ordinary Clojure functions that surface as bare async Python functions in the model's sandbox (`git_diff(...)`, `weather_lookup(...)`).
3. **Teaches the model** about them through a short prompt fragment, injected only while the extension is active.

## How extensions load

Discovery is classpath-wide and manifest-driven. Each extension jar ships **one resource**:

```
resources/META-INF/vis-extension/vis.edn
```

```clojure
{weather {:nses [com.acme.ext.weather.core]}}
```

At startup Vis scans every `META-INF/vis-extension/vis.edn` on the classpath and `require`s each namespace listed under `:nses` exactly once. Your namespace's top-level `(vis/register-extension! …)` fires during that require — that's the whole registration protocol. A namespace that throws during load doesn't crash Vis; the failure is surfaced as a warning to both the user and the model.

Getting on the classpath:

- **JVM / source runs** — add the extension to `deps.edn` like any Clojure dep (the first-party extensions use `:local/root` entries in Vis's own `deps.edn`).
- **Native binary** — extensions compile into the image. Add the dep, rebuild with `vis native` (see [Custom distributions](distributions.md)), and mind the [native-image rules](#native-image-rules) below.

## Anatomy

```
my-extension/
├── deps.edn
├── src/com/acme/ext/weather/core.clj
└── resources/
    ├── META-INF/vis-extension/vis.edn                      ; discovery manifest
    ├── META-INF/native-image/com.acme/weather/             ; only if you pull in
    │   └── reachability-metadata.json                      ;   reflective libs
    └── vis-docs/                                           ; optional doc pages
        ├── vis-docs.edn
        └── weather.md
```

```clojure
;; deps.edn
{:paths ["src" "resources"]
 :deps  {com.blockether/vis {:local/root "../vis"}}}   ; or a released coordinate
```

## The extension spec

`vis/extension` validates the map and fills defaults; `vis/register-extension!` puts it in the registry.

| Key | What it is |
| --- | --- |
| `:ext/name` | Unique name string, e.g. `"weather"`. |
| `:ext/description` | One-liner shown in `vis extensions list` and to the model in its extensions snapshot. |
| `:ext/version` `:ext/author` `:ext/owner` `:ext/license` | Plain metadata strings. |
| `:ext/kind` | Categorical bucket used as a section label: `"foundation"`, `"git"`, `"language"`, `"channel"`, `"provider"`, … |
| `:ext/activation-fn` | `(fn [env] -> boolean)`, called **once per turn**. Falsy hides every symbol and the prompt fragment for that turn. Defaults to always-on. |
| `:ext/engine` | `{:ext.engine/alias 'weather :ext.engine/symbols [...]}` — the sandbox surface (below). |
| `:ext/prompt-fn` | `(fn [env] -> string)` — the model-facing usage text for your tools. |
| `:ext/ctx-fn` | `(fn [env] -> map)` — structured per-turn context contributed into the model's `session` dict. |
| `:ext/sandbox-shims` | Vec of Python **shim** specs — host-backed modules published into the model's Python sandbox (below). |
| `:ext/slash-commands` | Vec of slash-command specs (below). |
| `:ext/doctor-fn` | `(fn [env] -> [checks])` — health checks for `vis doctor`. |
| `:ext/settings` `:ext/env` | Declared settings / environment variables (configurable via `~/.vis/config.edn` `:environment`). |

Channels, providers, persistence backends, and workspace backends register through their own keys (`:ext/channels`, `:ext/providers`, `:ext/persistance`, `:ext/workspace-backends`) — read a first-party extension of the matching kind as the reference implementation.

## Tools: sandbox symbols

A tool is a Clojure `defn` wrapped with `vis/symbol` and listed under `:ext.engine/symbols`:

```clojure
(defn- lookup-fn
  "await weather_lookup(city)
Returns {\"city\", \"summary\"} — current conditions for a city."
  [city]
  (extension/success {:result {:city city :summary "sunny, 21°C"}}))

(def lookup-symbol
  (vis/symbol #'lookup-fn {:symbol 'lookup :tag :observation}))
```

The rules:

- **Pass the var** (`#'lookup-fn`), never a bare fn — the docstring and arglists come from var metadata and become the model's `doc("weather_lookup")` text. Write the docstring in the `await name(args)` + return-shape style shown above; it's the only manual the model gets.
- **Naming.** The Python name is `<alias>_<symbol>` in snake_case: alias `'weather` + symbol `'lookup` → `weather_lookup`. Kebab-case folds to snake_case, and a trailing `?`/`!` is stripped (`refresh!` → `refresh`).
- **`:tag` is required**: `:observation` for pure reads, `:mutation` for anything that writes.
- **Arguments** arrive as plain values; a Python dict of options becomes a Clojure map with keyword keys (`weather_lookup("Oslo", {"units": "metric"})` → `[city {:units "metric"}]`). Use multiple arities for optional args.
- **Return an envelope.** `extension/success {:result value}` on success; on failure either throw (`ex-info` is converted for you) or return `extension/failure {:result nil :error {:message "…" :hint "…"}}`. The model sees only the `:result` payload — map keys convert kebab→snake automatically — and failures surface as normal Python exceptions.
- Envelope constructors live in `com.blockether.vis.internal.extension` (`success` / `failure`); the spec/registration API is `com.blockether.vis.core` (aliased `vis`).

Useful `vis/symbol` opts beyond `:symbol` and `:tag`: `:before-fn` (e.g. inject the turn's `env` as the first argument), `:render` + `:color-role` (custom TUI result card), `:hidden?` (bind but don't advertise).

## Sandbox shims and autoloads

The agent writes **Python**, but its sandbox ships only the pure-stdlib — no
pip, no native wheels. A **shim** lets your extension publish a *host-backed*
Python module into every sandbox (the main session and every `sub_loop` fork):
the familiar Python API is a thin façade whose real work is DELEGATED across the
boundary to Clojure/JVM callables you supply. This is exactly how `import yaml`
(backed by the pure-Clojure YAMLStar loader) and `import matplotlib.pyplot`
(backed by a Java2D PNG renderer) work — both ship as built-in shim extensions
(`foundation.shim-yaml`, `foundation.shim-matplotlib`), and the engine installs
them through the SAME generic path any extension uses.

List one or more shim specs under `:ext/sandbox-shims`:

```clojure
{:shim/name        "yaml"
 :shim/description "PyYAML-compatible module backed by YAMLStar."
 ;; Host callables the preamble delegates to — a `{py-name -> fn}` map (or a
 ;; 0-arg fn returning one). Each is wired onto the sandbox globals as a Python
 ;; callable (args marshalled Python->Clojure, result back) BEFORE the preamble
 ;; evals. Return a 2-vec envelope `[true payload]` / `[false message]` so a
 ;; failure crosses the boundary as a catchable Python exception.
 :shim/bindings    (fn [] {"__vis_yaml_load__" (fn [s] (try [true (yamlstar/load s)]
                                                        (catch Throwable t [false (str t)])))})
 ;; Python source eval'd into the sandbox: publish your module into
 ;; `sys.modules` (so `import yaml` finds it) and optionally staple it onto
 ;; `builtins` (autoload — `yaml.safe_load(...)` with no import). Use
 ;; single-quoted Python string literals so the Clojure string needs no escaping.
 :shim/preamble    "import sys, types\n..."}
```

Installed BEFORE the sandbox's baseline snapshot, so your `__vis_*` bridge names
and published module are hidden from the model's live-vars view. Install is
best-effort: a shim that throws is logged and skipped — it never breaks the
sandbox. Shims are a Clojure-extension capability (they need host callables);
drop-in Python extensions contribute tools/prompts/slash/hooks instead.


## The prompt fragment

`:ext/prompt-fn` returns the text that teaches the model your surface. It rides in a labeled `;; -- EXTENSION <alias> --` block only while the extension is active. Keep it dense and imperative — signatures, return shapes, one-line semantics:

```
weather_ surface active. Bare async Python functions:
  weather_lookup(city)  or  weather_lookup(city, {"units": ..})
    -> {city, summary}
doc("weather_lookup") for full opts.
```

Follow the house style: snake_case call forms, `->` return shapes, no prose paragraphs. The foundation-git extension's prompt is the canonical example.

## Activation

`:ext/activation-fn` gates the whole extension per turn. Use it to hide tools that can't work in the current workspace — foundation-git activates only when the workspace root sits inside a git repository:

```clojure
(defn- activation-fn [env]
  (boolean (some-> (:workspace/root env) io/file git-core/in-repository?)))
```

An inactive extension costs zero prompt tokens.

## Slash commands

User-facing `/commands` (TUI and web) are data too:

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

Return `{:slash/status :ok | :error, :slash/title "…"}` plus optional `:slash/data`.

## Shipping doc pages

Any extension can add pages to Vis's embedded docs — the same corpus the `/docs` site renders and the model reads through its `vis_docs` tool. Drop markdown under `resources/vis-docs/` with a manifest:

```clojure
;; resources/vis-docs/vis-docs.edn
{:pages [{:file "weather.md" :title "Weather" :section "Extensions" :order 50}]}
```

Every `vis-docs/vis-docs.edn` on the classpath is discovered — no central registry to edit. Ask a running Vis about your extension and it reads the page you shipped.

## Complete minimal example

`src/com/acme/ext/weather/core.clj`:

```clojure
(ns com.acme.ext.weather.core
  "Weather lookups under the `weather_` alias."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]))

(defn- lookup-fn
  "await weather_lookup(city)
Returns {\"city\", \"summary\"} — current conditions for a city."
  [city]
  (extension/success {:result {:city (str city) :summary "sunny, 21°C"}}))

(def ^:private symbols
  [(vis/symbol #'lookup-fn {:symbol 'lookup :tag :observation})])

(def ^:private prompt-text
  (str "weather_ surface active. Bare async Python functions:\n"
       "  weather_lookup(city) -> {city, summary}\n"
       "doc(\"weather_lookup\") for details."))

(def vis-extension
  (vis/extension
   {:ext/name        "weather"
    :ext/description "Current-conditions weather lookups for the model."
    :ext/version     "0.1.0"
    :ext/kind        "integration"
    :ext/engine      {:ext.engine/alias 'weather
                      :ext.engine/symbols symbols}
    :ext/prompt-fn   (fn [_env] prompt-text)}))

(vis/register-extension! vis-extension)
```

`resources/META-INF/vis-extension/vis.edn`:

```clojure
{weather {:nses [com.acme.ext.weather.core]}}
```

Add the dep, restart Vis, and the model can call `weather_lookup("Oslo")`.

## Native image rules

The native binary compiles extensions ahead of time, which brings a few hard constraints (see [JVM & native-image](jvm-native-image.md) for the background):

- **No `defrecord` / `deftype` / `gen-class`** in sandbox-facing code — the build refuses them (`validate-no-banned-defs!`). Plain maps and functions only.
- **Reachability metadata travels inside your jar**: `resources/META-INF/native-image/<group>/<artifact>/reachability-metadata.json` — the unified format only, never the legacy `reflect-config.json` family. Only add entries for reflection/resources **your extension uniquely pulls in**; never duplicate a library's own config.
- **Generate it with the tracing agent**, not by hand: run your code paths under `java -agentlib:native-image-agent=config-merge-dir=<your-artifact-dir> …`, then strip Clojure-internal noise.
- Resources your extension reads at runtime via `io/resource` (templates, assets) need a resource glob in that metadata — the agent only captures what the trace actually touched.

## Testing and verification

Vis uses [lazytest](https://github.com/NoahTheDuke/lazytest); test tool functions directly against the envelope contract:

```clojure
(ns com.acme.ext.weather.core-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe lookup-test
  (it "returns a canonical success envelope"
    (let [result (@#'com.acme.ext.weather.core/lookup-fn "Oslo")]
      (expect (extension/envelope-success? result))
      (expect (= "Oslo" (:city (:result result)))))))
```

Before shipping, run `clojure -M:format check`, `clojure -M:lint src extensions test build.clj`, and the relevant tests.
