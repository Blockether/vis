(ns com.blockether.vis.internal.foundation.shim-ruff
  "Built-in sandbox SHIM: a `ruff` module (and `vis-agent python -m ruff` CLI) for the
   model's Python sandbox, backed by the SAME in-process ruff the Python
   language pack's `format_code`/`lint_code` use — the `com.blockether/ruff` FFI
   over a first-party cdylib. No pip, no `ruff` binary on PATH, and it survives
   into the native image.

   Two surfaces, one engine:

     - `import ruff` -> `ruff.format_str(src)`, `ruff.check_str(src)`,
       `ruff.format_file(path)`, `ruff.check_file(path)`, `ruff.config_for(path)`,
       `ruff.version()`.
     - `vis-agent python -m ruff check|format [paths]` -> the module's `console_main`
       is the `-m` entry point (sandbox shims have no `__file__`, so `runpy`
       cannot execute them; `main-clj/python-module-runner-src` calls
       `console_main(sys.argv[1:])` instead).

   Ruff's OWN configuration discovery is authoritative: for every file we ask
   the host for the nearest `.ruff.toml` / `ruff.toml` / `pyproject.toml` with a
   `[tool.ruff]` table, walking up from that file, and hand it back to ruff. A
   tree with no configuration gets ruff's built-in defaults (E4, E7, E9, F,
   line-length 88) — exactly what the real `ruff` CLI does — and `check` says so
   on stderr so the user knows to add one.

   Not supported (the FFI is a one-source-one-call linter/formatter, not the
   whole CLI): `--fix`, `--watch`, `--output-format`, `--show-settings`, and
   config `exclude`/`extend-exclude` walking (the shim's own walk skips the
   usual noise directories instead). `per-file-ignores` DO apply — they are
   matched by ruff itself against the path we pass."
  (:require [com.blockether.ruff :as ruff]
            [com.blockether.vis.core :as vis]))


(defn- envelope
  "Run `f`, returning the 2-vector the ruff shim expects: `[true result]` on
   success, `[false message]` on any Throwable. Errors cross the boundary as
   DATA so the Python side can raise a catchable `ruff.RuffError` instead of a
   raw host `PolyglotException` (GraalPy does not route host exceptions through
   Python's `except Exception`)."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- blank->nil
  [s]
  (let
    [s (some-> s
               str)]
    (when-not (or (nil? s) (= "" s)) s)))

(defn- diagnostic->py
  "One ruff diagnostic as a STRING-keyed map — the boundary marshals that to a
   plain Python dict."
  [d]
  {"code" (:code d)
   "message" (:message d)
   "row" (:row d)
   "col" (:col d)
   "end_row" (:end-row d)
   "end_col" (:end-col d)
   "is_fixable" (boolean (:is-fixable d))})

(defn- ruff-bridge-bindings
  "Host callables the `ruff` shim delegates to. Scalars only across the
   boundary (no option dicts): every argument is a string/int, so the marshalling
   is total. `line_length` 0 means \"whatever the configuration says\"."
  []
  {"__vis_ruff_format__" (fn [code path config line-length]
                           (envelope #(ruff/format (str code)
                                                   {:path (blank->nil path)
                                                    :config (blank->nil config)
                                                    :line-length (when (pos? (long (or line-length
                                                                                       0)))
                                                                   (long line-length))})))
   "__vis_ruff_lint__" (fn [code path config line-length select ignore preview]
                         (envelope #(mapv diagnostic->py
                                          (ruff/lint (str code)
                                                     {:path (blank->nil path)
                                                      :config (blank->nil config)
                                                      :line-length (when (pos? (long (or line-length
                                                                                         0)))
                                                                     (long line-length))
                                                      :select (blank->nil select)
                                                      :ignore (blank->nil ignore)
                                                      :preview (boolean preview)}))))
   "__vis_ruff_config__" (fn [path]
                           (envelope #(ruff/config-file (str path))))
   "__vis_ruff_version__" (fn []
                            (envelope #(ruff/version)))})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-ruff"
     :ext/description
     (str "Sandbox in-process `ruff` formatter/linter: importable module plus `vis-agent python "
          "-m ruff check|format`, backed by com.blockether/ruff. "
          "Honours nearest ruff.toml/.ruff.toml/pyproject.toml config; no pip/PATH binary.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "ruff"
       :shim/imports ["ruff"]
       :shim/description
       (str "In-process Ruff, no pip/PATH: `python -m ruff check|format <paths>` (`format`: "
            "--check/--diff); import API: format_/check_{str,file}, config_for, version. "
            "Finds nearest .ruff.toml, ruff.toml, or pyproject.toml [tool.ruff] per file and "
            "honors per-file-ignores; otherwise uses/reports E4,E7,E9,F defaults. "
            "Not supported: --fix, --watch, --output-format, --show-settings, config exclude "
            "walking.")
       :shim/bindings ruff-bridge-bindings
       :shim/source "vis-shims/ruff.py"}]}))

(vis/register-extension! vis-extension)
