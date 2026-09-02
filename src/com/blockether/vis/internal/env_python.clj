(ns com.blockether.vis.internal.env-python
  "The agent's action substrate: an embedded CPython the model writes Python for.

   A SESSION here is a Python module namespace inside the one interpreter
   `com.blockether.vis-python-runtime` starts for the process, named by a string —
   the value callers carry as `:python-context`. Sessions keep separate globals
   and share every imported module, so the second sandbox costs almost nothing.

   Three rules shape everything below. **One dialect crosses the boundary:**
   JSON, in both directions — host to guest is a `json.loads` of a literal this
   namespace renders, guest to host is `python-host`'s envelope. **The guest's
   Python is a FILE**, never a string built here: the runtime ships the sandbox
   runtime, the auto-imports, the network guard, the proxy environment and the
   process redirect as modules it imports, and Vis' own guest code lives under
   `resources/vis-guest/`. **The boundary is C, not Python:** the filesystem
   roots, the process surface and the thread cap are policy the runtime holds
   behind an audit hook, so no guard here is a security boundary — the ones here
   are ergonomics.

   Public surface used by the loop:

     create-python-context / dispose-python-context! / set-python-binding! /
     bind-and-bump! / count-top-level-forms / validate-no-banned-defs! /
     run-python-block / persist-session-defs! / restore-session-defs! /
     forget-session-defs! / SYSTEM_VAR_NAMES / system-var-sym? / boundary-view /
     ctx->python-str / bind-ctx!"
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python-host :as python-host]
            [com.blockether.vis.internal.python-runtime :as python-runtime]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

(defn boundary-violation!
  "Throw on a keyword/symbol trying to cross the Clojure->Python boundary.
   The boundary is STRINGS-ONLY: every map that crosses (tool results, ctx,
   verb payloads) is built with string keys and string enum values at the
   SOURCE — there is no silent keyword->string conversion, so a keyword here
   means a producer bug, not data. `path` is the key path down from the value
   handed to `->py`, so the offending producer field is nameable.

   The message ALWAYS says WHERE: an empty path means the offending key sits on
   the value handed to the boundary itself, and a bare `:result`/`:success?`
   there is vis' own internal envelope — the producer must hand Python the
   PAYLOAD (`:result`), never the envelope that wraps it. Without that clause a
   report reads `non-string-key :result` with no location at all."
  [kind x path]
  (throw (ex-info (str "STRINGS-ONLY boundary violation: "
                       (name kind)
                       " "
                       (pr-str x)
                       (if (seq path)
                         (str " at path " (pr-str (vec path)))
                         (when (= :non-string-key kind) " at the TOP-LEVEL map key"))
                       " cannot cross Clojure->Python. Build boundary maps with"
                       " string keys and stringify enum values at the source."
                       (when (and (= :non-string-key kind) (contains? #{:result :success?} x))
                         (str " This looks like vis' INTERNAL result envelope:"
                              " hand Python the payload under :result, never the envelope.")))
                  {:vis/boundary-violation kind :value x :path (vec path)})))

(defn normalize-dict-key
  "Model-input hygiene at the ONE inbound conversion: a dict key spelled
   `\":target\"` is still a STRING (the model drifting into colon
   spelling while reading keyword-heavy source), so strip the single leading
   colon when an identifier char follows and the call just works — no
   lecture, no failure. Data keys are untouched: line numbers start with a
   digit, paths with a letter or `/`, neither with `:`. Produces
   strings, never keywords."
  ^String [^String s]
  (if (and (> (count s) 1)
           (= \: (.charAt s 0))
           (let [c (.charAt s 1)]
             (or (Character/isLetter c) (= \_ c))))
    (subs s 1)
    s))

(defn- key->py
  "Map key -> the Python-side dict key. STRINGS-ONLY: a string key passes
   verbatim; anything else (keyword, symbol, number, ...) is a producer bug
   and throws `boundary-violation!`."
  ^String [k path]
  (if (string? k) k (boundary-violation! :non-string-key k path)))

(defn- leaf->py
  "LEAF (non-collection) conversion shared by `->py` (the real boundary) and
   `boundary-view` (the no-context test mirror) — one fn so the mirror can
   never drift from the boundary again:

   - keywords/symbols are FORBIDDEN — strings-only boundary; throw with the
     key path so the producer that leaked one is directly nameable.
   - UUIDs (workspace/session ids in ctx) and java.time instants have no
     Python analog: the bridge marshals strings, numbers, booleans and
     collections, so anything else would cross as an opaque host handle.
     Stringify so the rendered ctx and the live dict both read as plain str.
   - `java.util.Date` is what nippy hands back for every persisted `#inst`
     (session/turn created_at) and it is NOT a Temporal, so the Temporal
     branch misses it and it would reach the bridge as an unconvertible
     host object (session 9c829d10, `list_sessions()`). ISO-8601 string
     instead.
   - numbers, booleans and strings hand straight across; every other leaf
     must become one of them HERE, because the bridge refuses what it
     cannot name."
  [x path]
  (cond (keyword? x) (boundary-violation! :keyword-value x path)
        (symbol? x) (boundary-violation! :symbol-value x path)
        (instance? java.util.Date x) (str (.toInstant ^java.util.Date x))
        (or (instance? java.util.UUID x) (instance? java.time.temporal.Temporal x)) (str x)
        :else x))

(defn boundary-view
  "What a plain-data Clojure value LOOKS LIKE after the CPython round trip —
   the mechanical composition of `->py` then `->clj` without a Python context.
   STRINGS-ONLY: string map keys pass VERBATIM, sets/seqs -> vectors,
   UUID/Temporal/Date leaves -> ISO strings. A keyword/symbol anywhere (key or
   value, any depth) throws `boundary-violation!` exactly like the real
   boundary — fix the producer fixture, never catch it. Idempotent.

   Every tool result the model sees in production (serialized structurally
   by `ctx-renderer/render-form-value`) has already crossed this boundary,
   so assertions about what the model reads MUST be written against THIS
   shape. Tests feed `(boundary-view raw-result)` to pin that contract
   without booting CPython."
  ([x] (boundary-view x []))
  ([x path]
   (cond (map? x) (into {}
                        (map (fn [[k v]]
                               ;; mirror the REAL round trip: `key->py` guards the
                               ;; outbound key, `normalize-dict-key` is what `->clj`
                               ;; does to it on the way back in.
                               (let [pk (normalize-dict-key (key->py k path))]
                                 [pk (boundary-view v (conj path pk))])))
                        x)
         (or (vector? x) (seq? x) (set? x)) (mapv #(boundary-view % path) x)
         ;; Leaves convert through the SAME fn the real boundary uses — this
         ;; mirror had drifted (Dates/UUIDs/Temporals passed through raw here
         ;; while `->py` stringified them), which let a test assert a shape the
         ;; model never actually sees.
         :else (leaf->py x path))))

(defn sym->py-name
  "Clojure tool/binding symbol -> a Python-LEGAL global name. Purely mechanical:
   `/` and `-` fold to `_` (alias fold + kebab->snake); a trailing `!` (mutation
   marker) is dropped; a trailing `?` (predicate) becomes an `is_` prefix. So
   `git/status` -> `git_status`, `git/commit!` -> `git_commit`, `search/web` ->
   `search_web`, `file-exists` -> `file_exists`. FULL SNAKE:
   this is how the agent reaches the tools — `git_status()` calls `git/status`.

   A tiny compatibility alias layer may additionally expose selected historical
   short names (currently `find_files`/`find` for `grep`), but the snake name remains
   canonical."
  ^String [sym]
  (let [s
        (str sym)

        pred?
        (str/ends-with? s "?")

        base
        (-> s
            (str/replace "?" "")
            (str/replace "!" "")
            (str/replace "/" "_")
            (str/replace "-" "_"))]

    (if pred? (str "is_" base) base)))

(defn- py-aliases-for-sym
  "Additional Python names intentionally accepted for a Clojure tool symbol.
   Keep tiny: aliases are prompt/API compatibility, not another naming scheme."
  [sym]
  (case sym
    grep
    ["find_files" "find"]

    find_files
    ["find"]

    ;; `grep` is canonical; `find_files`/`find` stay as compatibility aliases.
    ;; Keep the older symbol branch while contexts/extensions may still expose it.
    []))

(defn python-binding-names
  "Canonical Python global plus intentional compatibility aliases for `sym`.
   Used by provider/native discovery to deduplicate the same capability."
  [sym]
  (into [(sym->py-name sym)] (py-aliases-for-sym sym)))

(defn- python-string-literal
  ^String [x]
  (str "\""
       (-> (str x)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\"")
           (str/replace "\n" "\\n")
           (str/replace "\r" "\\r")
           (str/replace "\t" "\\t"))
       "\""))

(defn- python-number-literal
  ^String [x]
  (cond (and (instance? Double x) (Double/isNaN ^double x)) "nan"
        (and (instance? Double x) (Double/isInfinite ^double x)) (if (neg? ^double x) "-inf" "inf")
        (and (instance? Float x) (Float/isNaN ^float x)) "nan"
        (and (instance? Float x) (Float/isInfinite ^float x)) (if (neg? ^float x) "-inf" "inf")
        (instance? java.math.BigDecimal x) (.toPlainString ^java.math.BigDecimal x)
        (instance? clojure.lang.Ratio x) (str "(" (numerator x) " / " (denominator x) ")")
        :else (str x)))

(declare python-literal*)

(defn- python-map-literal
  ^String [m indent width path]
  (if (empty? m)
    "{}"
    (let [items
          (mapv (fn [[k v]]
                  (let [ks (key->py k path)]
                    (str (python-string-literal ks)
                         ": "
                         (python-literal* v (inc (long indent)) width (conj path ks)))))
                m)

          inline
          (str "{" (str/join ", " items) "}")]

      (if (and (not (str/includes? inline "\n")) (<= (+ (long indent) (count inline)) (long width)))
        inline
        (str "{\n"
             (str/join ",\n" (map #(str (apply str (repeat (inc (long indent)) " ")) %) items))
             "\n"
             (apply str (repeat indent " "))
             "}")))))

(defn- python-list-literal
  ^String [xs indent width path]
  (if (empty? xs)
    "[]"
    (let [items
          (mapv #(python-literal* % (inc (long indent)) width path) xs)

          inline
          (str "[" (str/join ", " items) "]")]

      (if (and (not (str/includes? inline "\n")) (<= (+ (long indent) (count inline)) (long width)))
        inline
        (str "[\n"
             (str/join ",\n" (map #(str (apply str (repeat (inc (long indent)) " ")) %) items))
             "\n"
             (apply str (repeat indent " "))
             "]")))))

(defn- python-literal*
  ^String [x indent width path]
  (cond (instance? java.util.Map x) (python-map-literal x indent width path)
        (or (vector? x) (seq? x) (set? x)) (python-list-literal x indent width path)
        :else (let [v (leaf->py x path)]
                (cond (nil? v) "None"
                      (true? v) "True"
                      (false? v) "False"
                      (string? v) (python-string-literal v)
                      (number? v) (python-number-literal v)
                      (char? v) (python-string-literal v)
                      ;; Boundary producers should emit plain data. Keep an unexpected
                      ;; host leaf deterministic and executable instead of leaking a
                      ;; process-specific <JavaObject ... at 0x...> pseudo-literal.
                      :else (python-string-literal (str v))))))

(defn ctx->python-str
  "Render plain boundary data as a deterministic, executable Python literal.

   This is deliberately a pure JVM serializer: rendering never enters CPython,
   never waits behind a session's GIL, and needs no process-global printer
   lock. It mirrors the Clojure->Python boundary (string-only map
   keys, list-like collections, ISO strings for Date/UUID/Temporal) and keeps
   insertion order plus the historical 100-column layout."
  ^String [data]
  (python-literal* data 0 100 []))

;; =============================================================================
;; The wire: one JSON literal in, one JSON string out
;; =============================================================================

(defn py-json-literal
  "`data` as a Python expression that evaluates to it — `json.loads(\"…\")`.

   This is the ONLY way a host value reaches the guest. There is no `putMember`
   over a C ABI that carries text, and interpolating a repr would make every
   value a parsing question; JSON is the one dialect this boundary speaks.

   Public because two other host namespaces cross the same boundary: JSON
   escapes `/` as `\\/`, which Python keeps VERBATIM, so JSON text pasted
   straight into Python source silently corrupts every path in it."
  ^String [data]
  (str "__import__('json').loads(" (pr-str (json/write-json-str data)) ")"))

(defn- exec!
  "Run `code` in `session` for its side effects, answering nil.

   Best-effort by design at every call site that seeds: a hiccup in a discovery
   surface must never be the reason a block fails."
  [session ^String code]
  (try (runtime/exec! session code) nil (catch Throwable _ nil)))

(defn- read-json
  "Guest JSON text as Clojure data, `nil` when it is blank or unreadable."
  [^String text]
  (when-not (str/blank? text) (try (json/read-json text :key-fn keyword) (catch Throwable _ nil))))

(defn- guest-value
  "The value of guest EXPRESSION `expr` in `session`, as Clojure data.

   `runtime/run` renders it as JSON, so what comes back is what a tool result
   looks like in the other direction: strings, numbers, vectors, string-keyed
   maps."
  [session ^String expr]
  (try (read-json (runtime/run session expr)) (catch Throwable _ nil)))

(defn- seed-json!
  "Merge `m` into the guest dict `dict-name` with `setdefault` semantics — the
   FIRST source to claim a key keeps it, which is what makes a tool's registered
   contract win over a later, weaker one."
  [session ^String dict-name m]
  (when (seq m)
    (exec! session
           (str "for __vis_k__, __vis_v__ in "
                (py-json-literal m)
                ".items():\n"
                "    globals().setdefault('" dict-name
                "', {}).setdefault(__vis_k__, __vis_v__)\n" "del __vis_k__, __vis_v__"))))

(defn- update-json!
  "Overwrite `m`'s keys in the guest dict `dict-name` — the seeding twin for
   metadata that ARRIVES LATE and must win, like a tool's doc after its binding."
  [session ^String dict-name m]
  (when (seq m)
    (exec! session
           (str "globals().setdefault('" dict-name "', {}).update(" (py-json-literal m) ")"))))

(defn- keywordize-facts
  "The guest's facts map with keyword keys, whichever way the envelope decoded
   them, and its `:names` as strings."
  [facts]
  (let [m (into {}
                (map (fn [[k v]]
                       [(keyword (str/replace (if (keyword? k) (name k) (str k)) "_" "-")) v]))
                (or facts {}))]
    (assoc m :names (mapv str (or (:names m) [])))))

(def BANNED_DEF_HEADS
  "Python constructs refused pre-eval — belt-and-suspenders against the obvious
   sandbox-escape footguns on top of the interpreter's own confinement."
  #{"exec" "eval" "compile" "__import__"})

(def ^:private protected-baseline-names
  "Python globals the agent may CALL but must not rebind. Rebinding output, tool, or
   parser-helper names would shadow the persistent session substrate."
  #{"print" "println" "apropos" "doc" "defs" "gather" "__vis_count_forms__" "__vis_banned_name__"})

(def PROCESS_SURFACE
  "THE sentences about this sandbox's process surface — written ONCE, here, and
   said verbatim by every surface that has to say them:

     - the `sandbox-shims` prompt block (`prompt/sandbox-shims-prompt-block`);
     - the POSIX refusal (`vis-shims/posix.py`), when `subprocess` / `os.system` /
       `os.popen` is called;
     - a live handle that cannot be driven (`__VisShell__.__vis_op__` in
       `vis-python/async_runtime.py`);
     - the corpus entry named `shell` (`env-python/create-python-context`), when the
       shell tools are off and `apropos(\"shell\")` / `doc(\"shell\")` would otherwise
       answer with silence.

   Every copy of one fact drifts, and the copy the model reads at the moment it
   is blocked is the one that must be right. Composed, never concatenated ad hoc:
   `ban` is the rule and is all the PROMPT says (invocation grammar belongs to the
   `shell` symbol's own docs, not to a supplemental block); `ban` + `use` is what a
   call site says, because there the model is already writing the call; `off` is
   the toggle state, and it names BOTH doors so silence is never read as
   \"`subprocess` might still work\"; `off` + `extension` is what DISCOVERY says,
   because a model that reads `off` alone concludes nothing in this product can
   start a process. The toggle closes the MODEL's door only: an installed Python
   extension's `vis.shell` is wired unconditionally in
   `python-extensions/build-context` and no toggle gates it.

   Reaches Python as the `__vis_process_surface__` global (see
   `install-process-surface!`), so no `.py` file carries a copy."
  {"ban" (str "`subprocess`, `os.system` and `os.popen` never spawn in the vis sandbox "
              "— every process starts through the sandbox's own `shell` verb, which "
              "owns the jail, the log file and the handle. There is no shell TOOL.")
   "use" (str "Use `sh = await shell('npm test')`; the handle it returns drives "
              "it with `logs()`, `wait()`, `type()` and `stop()`, and every answer "
              "already carries that shell's status.")
   "off" (str "Shell commands are DISABLED in this vis sandbox: `shell` is not bound, "
              "a live handle cannot be driven, and `subprocess`, `os.system` and "
              "`os.popen` all raise — nothing here can start a process. Turn on 'Shell "
              "commands' in the settings dialog.")
   "extension" (str
                 "That toggle closes the MODEL's door only: an installed Python extension "
                 "keeps its own trusted process boundary, so `vis.shell({'command': 'npm test'})` "
                 "in extension code still spawns and answers the same handle.")})

(def AUTO_IMPORTED_PYTHON_NAMES
  "Python names installed into builtins for every `python_execution` context.
   This is the model-facing inventory; keep it synchronized with
   `auto-imports-python` and its real-context regression test."
  ["json" "shlex" "re" "hashlib" "glob" "os" "sys" "collections" "Counter" "pathlib" "Path"
   "textwrap" "base64" "math" "socket" "builtins" "time" "datetime"])

(def ^:private default-denied-domains
  "Hosts ALWAYS blocked when the sandbox has network — even under an `*` allowlist.
   Cloud-metadata endpoints are the classic SSRF target (credentials / instance
   identity), so they are denied by default; config `:network/denied-domains`
   ADDS to this set, never removes from it. The metadata IP is enforced at the
   `connect()` level too (see `network-guard-python`) so a raw-IP socket can't
   sidestep DNS to reach it."
  ["169.254.169.254" "metadata.google.internal" "metadata.goog" "metadata"])

;; =============================================================================
;; Sandbox bindings
;; =============================================================================

(defn- protected-names-for-bindings
  [custom-bindings]
  (set (concat protected-baseline-names
               (map (fn [[sym _]]
                      (first (str/split (sym->py-name sym) #"\." 2)))
                    (or custom-bindings {})))))

(defn- install-protected-names!
  "Declare which globals the block may SHADOW but never overwrite for the
   session. The runtime reads this list when it wraps a block."
  [session custom-bindings]
  (exec! session
         (str "globals()['__vis_protected_names__'] = sorted(set("
              "globals().get('__vis_protected_names__') or []) | set("
              (py-json-literal (vec (sort (protected-names-for-bindings custom-bindings))))
              "))")))

(defn- add-protected-names!
  [session names]
  (when (seq names)
    (exec! session
           (str "globals()['__vis_protected_names__'] = sorted(set("
                "globals().get('__vis_protected_names__') or []) | set("
                (py-json-literal (vec (sort (set (map str names)))))
                "))"))))

(defn set-python-binding!
  "Bind `sym` -> `val` in `session`'s globals.

   A FUNCTION becomes a host tool: the name is registered for this session in
   [[python-host]] and installed by the runtime as a deferred callable, so
   `await tool(...)` and `gather(tool(...), …)` work exactly like the tools the
   context was built with. A dotted name publishes one declared method through a
   capability namespace instead of exposing the extension's raw object. Anything
   else is DATA and crosses as JSON."
  [session sym val]
  (let [nm
        (sym->py-name sym)

        aliases
        (py-aliases-for-sym sym)

        names
        (cons nm aliases)

        protected
        (map #(first (str/split % #"\." 2)) names)]

    (add-protected-names! session protected)
    (if (fn? val)
      (python-host/install-tools! session
                                  (into {}
                                        (map (fn [n]
                                               [n val]))
                                        names))
      (exec! session
             (str "for __vis_n__ in "
                  (py-json-literal (vec names))
                  ":\n"
                  "    globals()[__vis_n__] = " (py-json-literal val)
                  "\n" "del __vis_n__")))
    nil))

(defn- set-python-binding-meta!
  "Record one piece of model-facing metadata for `sym` (and its aliases) in the
   guest table `dict-name`, then RE-STAMP those names so the bound callable
   carries it: a tool deferred before its metadata arrived only gets it here."
  [session sym dict-name text]
  (when (and session (string? text))
    (let [names (cons (sym->py-name sym) (py-aliases-for-sym sym))]
      (update-json! session
                    dict-name
                    (into {}
                          (map (fn [n]
                                 [n text]))
                          names))
      (exec! session
             (str "if '__vis_stamp_tools__' in globals():\n"
                  "    __vis_stamp_tools__("
                  (py-json-literal (vec names))
                  ")")))))

(defn set-python-binding-doc!
  "The model-facing description of `sym`, what in-sandbox `doc(name)` prints."
  [session sym text]
  (set-python-binding-meta! session sym "__vis_docs__" text))

(defn set-python-binding-signature!
  "The declared parameter list of `sym`, what `inspect.signature` reports through
   the deferred wrapper's `__wrapped__`."
  [session sym signature]
  (set-python-binding-meta! session sym "__vis_sigs__" signature))

(defn set-python-binding-keys!
  "The options-dict vocabulary of `sym` — which keys the dict must carry and
   which it may omit — printed by `doc(name)` under the call line."
  [session sym keys-text]
  (set-python-binding-meta! session sym "__vis_keys__" keys-text))

(defn remove-python-binding!
  "Remove `sym` from `session` entirely, including dotted namespace members and
   every discovery metadata table."
  [session sym]
  (let [names (cons (sym->py-name sym) (py-aliases-for-sym sym))]
    (exec! session
           (str "for __vis_n__ in " (py-json-literal (vec names))
                ":\n" "    if '.' in __vis_n__:\n"
                "        __vis_remove_dotted_tool__(__vis_n__)\n" "    else:\n"
                "        globals().pop(__vis_n__, None)\n"
                "    for __vis_table__ in ('__vis_docs__', '__vis_sigs__', '__vis_keys__'):\n"
                "        globals().get(__vis_table__, {}).pop(__vis_n__, None)\n"
                "del __vis_n__, __vis_table__"))
    nil))

(defn bind-and-bump!
  "Set `sym` -> `val` in the env's sandbox."
  [env sym val]
  (set-python-binding! (:python-context env) sym val))

(defn bind-ctx!
  "Bind the standing context as the guest dict `session` — the same projection
   the renderer prints, so the live dict and the wire's structural deltas agree."
  [session data]
  (exec! session (str "globals()['session'] = " (py-json-literal data))))

(defn seed-cli-runtime!
  "Seed a standalone `vis-agent python` CLI session with script `argv` (bound to
   `sys.argv`) and, when non-empty, an `env` map merged into `os.environ`.

   This is what gives the CLI real-`python` semantics: unlike the agent sandbox,
   whose environment is scrubbed because the human never sees it, the CLI
   forwards the caller's own."
  [session {:keys [argv env]}]
  (when (some? argv)
    (exec! session
           (str "import sys as __vis_sys__\n"
                "__vis_sys__.argv = list(" (py-json-literal (vec argv))
                ")\n" "del __vis_sys__")))
  (when (seq env)
    (exec! session
           (str "import os as __vis_os__\n"
                "__vis_os__.environ.update(" (py-json-literal env)
                ")\n" "del __vis_os__")))
  session)

;; =============================================================================
;; Block validation (top-level statement count + banned constructs)
;; =============================================================================

(defn count-top-level-forms
  "Number of top-level Python statements in `code`, counted by the session's own
   parser. Comment- or whitespace-only blocks answer 0.

   Source the parser REFUSES throws that SyntaxError: an empty block and an
   unparseable one are different outcomes, and swallowing the refusal here would
   answer 0 for both — reporting `nothing to execute` for a real syntax error."
  [session code]
  (long (or (read-json (runtime/run session (str "__vis_count_forms__(" (pr-str (str code)) ")")))
            0)))

(defn validate-no-banned-defs!
  "Throws `:vis/banned-def-head` when `code` references a banned construct
   ([[BANNED_DEF_HEADS]]). A parse failure is silent — the run that follows
   surfaces a clean syntax error with its line and column."
  [session code]
  (when-let [head (guest-value session
                               (str "__vis_banned_name__("
                                    (pr-str (str code))
                                    ", "
                                    (py-json-literal (vec BANNED_DEF_HEADS))
                                    ")"))]
    (throw (ex-info (str "Block uses `" head
                         "` which is banned in the Python sandbox " "(sandbox-escape footgun).")
                    {:type :vis/banned-def-head :head (str head)}))))

;; =============================================================================
;; Discovery: apropos / doc
;; =============================================================================

(def ^:private unharvested-shim-page
  "What a door carries when the generated capability index never saw it — an
   extension declares its own shims, and only this repository's are harvested. It
   keeps the name searchable; `doc()` reads the live module instead of printing
   this."
  "sandbox shim capability")
(defn- sandbox-corpus
  "Merge what a SESSION can reach with the ordered document corpus. Live
   contracts win name collisions; documents retain corpus order.

   `facts` is what only the guest knows — its callable names, the registered
   `__vis_docs__`/`__vis_calls__`/`__vis_sigs__`/`__vis_kinds__`/`__vis_keys__`
   tables and the gists of its own `def`s — gathered there and merged here, in
   ONE call. Documents are read LIVE on the call that asks, so a skill edited
   mid-session answers with what it says now."
  [{:keys [names docs calls sigs kinds keys def-docs def-calls]}]
  (let [document-entries
        (doc-corpus/entries)

        documents
        (into {} (map (juxt :name identity)) document-entries)

        shims
        (try (extension/sandbox-shims) (catch Throwable _ nil))

        shim-names
        (into #{}
              (comp (mapcat #(concat (:shim/imports %) (:shim/globals %)))
                    (filter string?)
                    (remove str/blank?))
              shims)

        shim-pages
        (reduce (fn [m shim]
                  (let [page (str (:shim/docs shim))]
                    (if (str/blank? page)
                      m
                      (reduce #(assoc %1 %2 page)
                              m
                              (filter string?
                                      (concat (:shim/imports shim) (:shim/globals shim)))))))
                {}
                shims)

        ordered-names
        (distinct (concat names
                          (sort (map str (clojure.core/keys docs)))
                          (sort shim-names)
                          (map :name document-entries)))]

    (into
      []
      (map
        (fn [nm]
          (let [document
                (get documents nm)

                registered
                (str (get docs nm ""))

                own
                (if (seq registered) registered (str (:text document)))

                page
                (str (get shim-pages nm ""))

                whole
                (if (and (seq page) (not (str/includes? own page)))
                  (str/join "\n\n" (remove str/blank? [own page]))
                  own)

                text
                (cond (seq whole) whole
                      ;; A door the generated apropos resources never saw
                      ;; — an extension declares its own — keeps its NAME
                      ;; searchable; `doc()` reads the live module.
                      (contains? shim-names nm) unharvested-shim-page
                      :else (str (get def-docs nm "")))

                call
                (or (not-empty (str (get calls nm)))
                    (not-empty (str (:call document)))
                    (when-let [sig (get sigs nm)]
                      (str nm "(" sig ")"))
                    (not-empty (str (get def-calls nm))))]

            (cond-> {:name nm
                     :text text
                     :kind (or (get kinds nm)
                               (:kind document)
                               (when (contains? shim-names nm) "module")
                               (if (and (str/blank? (str (get docs nm ""))) (nil? document))
                                 "local"
                                 "tool"))}
              (seq (str call))
              (assoc :call call)

              (seq (str (get keys nm)))
              (assoc :params (str (get keys nm)))))))
      ordered-names)))

(defn- apropos-rows
  "The rows `apropos(pattern)` prints: the corpus filtered by the pattern, with
   the session's own `def`s left out — those are what `defs()` lists."
  [facts pattern]
  (let [corpus (into [] (remove #(= "local" (str (:kind %)))) (sandbox-corpus facts))]
    (into []
          (comp (remove #(str/starts-with? (str (:name %)) "_"))
                (map (fn [hit]
                       {"name" (:name hit)
                        "kind" (str (:kind hit "tool"))
                        "body" (doc-corpus/body-text (:text hit))})))
          (doc-corpus/search corpus (str pattern)))))

(defn- doc-text
  "The page `doc(target)` prints. `live` is the prose the GUEST already read off
   the object itself — the only thing the host cannot see — so a dotted member
   and an extension's own module answer from the live object, and everything
   else from the corpus."
  [facts target live]
  (let [es (sandbox-corpus facts)]
    (if (str/blank? (str target))
      (doc-corpus/index-text es)
      (let [wanted (doc-corpus/normalize-name target)
            hit (or (first (filter #(= (str target) (:name %)) es))
                    (first (filter #(= wanted (doc-corpus/normalize-name (:name %))) es)))]

        (if hit
          (let [page (doc-corpus/entry-text hit
                                            (when (contains? (set (:names facts)) (str (:name hit)))
                                              "callable"))]
            (cond
              (and (= "local" (str (:kind hit))) (str/blank? (str (:text hit))))
              (str
                page
                "This session defined it and it carries no docstring, so there is "
                "nothing to print. `defs(\""
                (:name hit)
                "\")` returns its source; a "
                "docstring's first line becomes its `defs()` gist and the whole of it "
                "becomes this page. Session-local helpers are listed by `defs()`, not `apropos`.")
              (and (= "module" (str (:kind hit)))
                   (contains? #{"" unharvested-shim-page} (str (:text hit))))
              (if (str/blank? (str live)) page (str live))
              :else page))
          (if (str/blank? (str live)) (doc-corpus/miss-text es target) (str live)))))))

(def ^:private introspection-doc
  "The pages of the two discovery verbs and the three runtime verbs beside them.
   They are installed by the ENGINE rather than the extension registry, so this
   is the only place their contract can come from."
  {'apropos
   "apropos(pattern='') -> [AproposItem(type, name, body)]. REGULAR-EXPRESSION FILTER over every SYMBOL name this session can reach. The pattern is applied with Clojure `re-find`, so `numpy\\..*` finds NumPy members and an invalid expression is an error. Results preserve corpus order and are never ranked or capped. With no argument it lists every public symbol. `type` is function · class · module · tool · doc · skill; `name` is exactly what `doc()` reads; `body` is the opening of the symbol's own text. Pass a row directly to `doc(item)` for the whole document. Session-local `def`s are listed by `defs()`, not here."
   'doc
   "doc(target) -> str. RETRIEVE one symbol whole: an `AproposItem` a search answered with, a function name, a Vis documentation slug or a skill name — case, whitespace and a trailing `.md` do not matter, and a callable wins a name collision. What comes back is what the target IS: a function's or class's own docstring, a module's, a whole documentation page, a whole `SKILL.md`. A DOTTED target reads ONE MEMBER live off the object itself — `doc(\"pandas.read_csv\")` prints that member's signature and docstring. `doc()` with no argument prints the curated index of the verbs a session starts from. A skill is one of these documents and nothing more: reading it is the whole of using it."
   'gather
   "gather(*awaitables) -> list. Concurrently run independent deferred tool calls/awaitables on a bounded host pool; results preserve input order. One list/tuple works. Use `await gather(...)`; keep dependent calls sequential. All settle before failure reports every failing slot index."
   'defs
   "defs(name=None) -> str. The FUNCTIONS this session defined, as text: `defs()` lists each one's name, signature, block, length and the first line of its DOCSTRING; `defs(name)` returns that one's source, so a helper is refined by editing what it already says instead of being re-pasted from memory. WRITE that docstring: its first line is the gist this listing prints and the whole of it is the helper's own `doc(name)` page. Session-local definitions are deliberately absent from `apropos`; find them with `defs()`. A `def` outlives the block and the turn, and is re-created in a fresh sandbox after a gateway restart — a restored one is marked `(restored)`. Imported functions and engine internals are never listed. When the same helper recurs across sessions it has outgrown the sandbox: propose a Python extension (`doc(\"extending\")`)."
   'fold-session
   "fold_session(key, gist=None) -> str. Collapse SETTLED steps: prior turns and the current turn only through its last completed iteration; live/future steps cannot fold. The key is a STRING: \"t2/i5\" one step · \"t2\" a whole turn · \"t2/i1-i56\" a range · \"-t2/i56\" everything through it · \"t2/i5-\" everything since it · comma-separate several, disjoint ranges included (\"t1/i61-i98, t3/i111-i135, t4\" is ONE fold); a token that is not a step key, or that matches no settled step, is refused by name. Folding changes rendering, not storage; there is no destructive unfold command, and a folded step is not re-readable inline — its GIST is what survives. With introspection on, `s = await read_session()` and filter `['transcript']['turns'][...]['iterations'][...]['blocks']`. A broader newer fold supersedes fully covered breadcrumbs; equal scope keeps newer. Partial overlaps remain separate."})

(def ^:private introspection-signatures
  {'apropos "pattern=''"
   'doc "target=None"
   'gather "*awaitables"
   'defs "name=None"
   'fold-session "key, gist=None"})

(defn- install-introspection!
  "Wire `apropos` and `doc` into `session` and seed the contracts of the five
   verbs the engine itself owns.

   The two verbs live on BOTH sides on purpose: the guest half gathers what only
   it knows and is the one that can read prose off a live object, the host half
   holds the corpus. They meet in one host call, because a host tool runs while
   the block waits inside it and cannot ask the interpreter anything."
  [session]
  (python-host/install-tools! session
                              {"__vis_apropos__" (fn [facts pattern]
                                                   (apropos-rows (keywordize-facts facts) pattern))
                               "__vis_doc__" (fn [facts target live]
                                               (doc-text (keywordize-facts facts) target live))})
  (exec! session "import vis_introspection\nvis_introspection.install(globals())")
  (doseq [[sym text] introspection-doc]
    (set-python-binding-doc! session sym text))
  (doseq [[sym signature] introspection-signatures]
    (set-python-binding-signature! session sym signature)))

;; =============================================================================
;; Sessions
;; =============================================================================

(def ^:private guest-source-dir
  "Where Vis stages its OWN guest Python for the interpreter to import.

   The runtime imports what it ships from its own manifest; this is the twin for
   the modules Vis owns (`resources/vis-guest/`). They are STAGED as files, not
   exec'd as text, so a traceback inside one names a file and a line."
  (delay (let [dir (io/file (System/getProperty "user.home") ".vis" "python" "vis-guest")]
           (io/make-parents (io/file dir "keep"))
           (.mkdirs dir)
           (doseq [nm ["vis_introspection.py" "vis_autoinstall.py"]]
             (when-let [res (io/resource (str "vis-guest/" nm))]
               (let [target (io/file dir nm)
                     source (slurp res)]

                 (when-not (and (.isFile target) (= source (slurp target))) (spit target source)))))
           (.getAbsolutePath dir))))

(defonce ^:private interpreter-started (atom false))
(defonce ^:private interpreter-lock (Object.))

(defn ensure-interpreter!
  "Start the process's ONE interpreter, with Vis' own guest modules on the path.
   Idempotent: the runtime's own `initialize!` is, and so is this.

   Public because a session is not the only thing that needs the interpreter up:
   a Python EXTENSION loads at startup, before any sandbox exists, and the first
   of the two to arrive is the one that starts it. The interpreter itself may
   not be on this machine yet — `python-runtime/ensure-library!` is what fetches
   it, and it costs nothing once it has.

   A caller that arrives second WAITS, and that is the whole point of the lock:
   a flag set before the work is done let the second caller return to a Python
   that was still inside `Py_Initialize` and confine it, and the audit hook then
   refused the interpreter's OWN startup — `getpath` raising OSError, `error
   evaluating path`, and every session after it failing in `vispython_exec`.
   Measured on a gateway prewarming its api and tui sessions together. The flag
   is set only after a start SUCCEEDS, so a machine that could not fetch the
   interpreter this time gets to try again rather than serving a dead one."
  []
  (when-not @interpreter-started
    (locking interpreter-lock
      (when-not @interpreter-started
        (python-runtime/ensure-library!)
        (runtime/initialize! {:source-paths [@guest-source-dir]})
        (runtime/logs! (fn [ndjson]
                         (doseq [line (str/split-lines (str ndjson))]
                           (when-not (str/blank? line)
                             (tel/log! {:level :debug :id ::python-runtime} line)))))
        (reset! interpreter-started true))))
  nil)

(defonce ^:private session-counter (atom 0))

(defn- new-session-name
  "A fresh namespace name for one sandbox. Sessions share the interpreter, so
   the name is what keeps two sandboxes out of each other's globals."
  ^String []
  (str "vis_sandbox_" (swap! session-counter inc)))

(defn- confine!
  "Point the process's filesystem policy at what THIS session may read and
   write. Confinement is the interpreter's, not a session's — the C policy is
   process state — so a second sandbox in one process REPLACES it. That is the
   open design question the migration plan names; today a gateway serves one
   workspace at a time."
  [roots-fn]
  (when roots-fn
    (try (let [roots (vec (distinct (map str (roots-fn))))]
           (runtime/confine! roots
                             roots
                             (str "Refused: the sandbox filesystem is confined to this "
                                  "session's roots.")))
         (catch Throwable t (tel/log! {:level :warn :id ::confine-failed :error t}) nil))))

(defn- install-network!
  "Install the in-guest network policy for `session`: the domain guard when the
   jail is on and there is a restriction to enforce, and the proxy environment
   when egress routes through the gateway.

   The CAPABILITY comes first and is not Python at all: with this session's
   network off the runtime's audit hook refuses every socket, name lookup and
   connection, so nothing seeded here can be rebound to get one back. Inside a
   session that HAS egress the domain guard is LEGIBILITY — a refusal in Python
   instead of a socket timeout — and the boundary deciding where a request may go
   is the gateway proxy, which sees the request. Both are files the runtime ships,
   configured by globals seeded before they run."
  [session {:keys [enabled? jail-enabled? allowed-domains denied-domains proxy-port ca-file]}]
  (let [net?
        (boolean enabled?)

        allowed
        (vec allowed-domains)

        denied
        (into default-denied-domains denied-domains)

        allow-all?
        (or (empty? allowed) (some #(= "*" (str %)) allowed))

        guard?
        (and net? jail-enabled? (or (seq denied) (not allow-all?)))

        ;; Under a proxy the guest must always reach loopback: the proxy itself
        ;; enforces the real host, verb and path policy.
        guard-allowed
        (if proxy-port (into allowed ["127.0.0.1" "::1" "localhost"]) allowed)]

    (try (runtime/network! net? "Refused: this session was granted no network access.")
         (catch Throwable t (tel/log! {:level :warn :id ::network-capability-failed :error t})))
    (when guard?
      (exec! session
             (str "globals()['__vis_allowed_domains__'] = "
                  (py-json-literal guard-allowed)
                  "\n"
                  "globals()['__vis_denied_domains__'] = "
                  (py-json-literal (vec denied))))
      (try (runtime/install-module! session "network_guard") (catch Throwable _ nil)))
    (when-not guard?
      ;; The guard's policy holder and its socket wrapper are PROCESS state — one
      ;; interpreter serves every session — so a session that enforces nothing has
      ;; to clear the holder. Left in place, the previous session's allowlist
      ;; answers first and reports a domain refusal for a session whose sockets
      ;; the capability layer had already refused outright.
      (try (exec! session "import builtins; builtins.__vis_net_policy__ = None")
           (catch Throwable t (tel/log! {:level :warn :id ::network-policy-not-cleared :error t}))))
    (when (and net? proxy-port)
      (exec! session
             (str "globals()['__vis_proxy_url__'] = 'http://127.0.0.1:"
                  proxy-port
                  "'\n"
                  "globals()['__vis_ca_file__'] = "
                  (py-json-literal (str (or ca-file "")))))
      (try (runtime/install-module! session "proxy_env") (catch Throwable _ nil)))))

(defn- install-network-probe!
  "The dev network-filter loop: `network_probe([method,] url)` and
   `network_filter(fn)` over a SYNTHETIC request — no socket, nothing sent."
  [session]
  (when-let [report-fn (requiring-resolve
                         'com.blockether.vis.internal.python-extensions/net-probe-report)]
    (python-host/install-tools! session
                                {"__vis_net_probe__" (fn [method target headers body]
                                                       (report-fn method target headers body))})
    (try (runtime/install-module! session "network_probe") (catch Throwable _ nil))))

(defn- pip-install!
  "Install the distribution `spec` for the sandbox, answering whether it landed.

   The HOST fetches it, because the guest may neither spawn a process nor route
   its own egress — so this is a door with its own policy and not a hole in the
   one the block runs under: only a plain distribution name, only when this
   session has network at all, and only ever a WHEEL (the runtime's `pip` passes
   `--only-binary=:all:`, since an sdist would run its own `setup.py` on the
   host, outside every boundary here). Never throws: a refusal is `false` and
   the guest sees the ordinary `ModuleNotFoundError`."
  [network-enabled? spec]
  (boolean (and network-enabled?
                (string? spec)
                (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,63}" spec)
                (try (zero? (long (:exit (python-runtime/pip-install! [spec]) 1)))
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::pip-install-failed :spec spec :error t})
                       false)))))

(defn- install-autoinstall!
  "Let this session's first `import numpy` fetch numpy.

   Vis used to answer that import with a reimplementation of its own; the
   interpreter is the real one now, so the honest answer is the real wheel. The
   finder is Vis' own guest module and goes LAST on `sys.meta_path`, so it only
   ever sees a name neither the standard library nor an installed package could
   resolve."
  [session network-opts]
  (try (python-host/install-sync-tools! session
                                        {"__vis_pip_install__"
                                         (partial pip-install! (boolean (:enabled? network-opts)))})
       (exec! session "import vis_autoinstall; vis_autoinstall.install(__vis_pip_install__)")
       (catch Throwable t (tel/log! {:level :warn :id ::autoinstall-failed :error t}) nil)))

(defn- install-shims!
  "Give `session` every registered sandbox shim: its host bindings first, then
   its Python.

   A shim is a DOOR — `ls`, `attach`, `nippy` and `ruff` are Vis
   capabilities the host performs, not packages anyone can pip install — so the
   bindings go in as SYNCHRONOUS host tools and the source is exec'd in the
   session's own globals, where each shim's `__vis_install_<name>__` staples its
   module or its global on. Anything a wheel can serve is not a shim: the
   interpreter is a real CPython and `import numpy` fetches numpy."
  [session]
  (doseq [shim (try (extension/sandbox-shims) (catch Throwable _ nil))]
    (try (let [declared (:shim/bindings shim)
               bindings (if (map? declared) declared (when (ifn? declared) (declared)))]

           (when (seq bindings) (python-host/install-doors! session bindings))
           (exec! session (extension/shim-src shim)))
         (catch Throwable t
           (tel/log! {:level :warn :id ::shim-install-failed :shim (:shim/name shim) :error t})))))

(defn- guest-stdin-text
  "What the guest's `sys.stdin` reads for a context built with `stdin`.

   Descriptor 0 belongs to the HOST process, so an agent block that reaches for
   it blocks on a terminal nobody is typing into — and with one interpreter
   thread serving every session, that is the whole process. So the sandbox's
   stdin is EMPTY (`\"\"`), and a stray `input()` answers `EOFError` instead of
   hanging. `System/in` is the one caller that genuinely owns descriptor 0 —
   the human running `vis-agent python` — and keeps it (`nil`). Any other
   stream is read here and handed over as its text."
  [stdin]
  (cond (nil? stdin) ""
        (identical? stdin System/in) nil
        :else (slurp stdin)))

(defn create-python-context
  "Equip ONE session with the sandbox and answer
   `{:python-context :sandbox-ns :initial-ns-keys}`.

   `custom-bindings` is `{symbol value}` — a function becomes a host tool, any
   other value crosses as data. `roots-fn` answers the directories the guest may
   read and write; without it the interpreter keeps whatever policy the process
   already had. `network-opts` carries the egress rules.

   The order matters and is the same order it has always been: the runtime
   first, then the names the block may not overwrite, then the tools, then the
   contracts those tools carry, then discovery, then policy — so every `__vis_*`
   name and every seeded module is already BASELINE when the member snapshot is
   taken, and the model's live-vars view shows only what its own blocks made."
  [custom-bindings roots-fn network-opts stdin _stderr _gate-fn]
  (ensure-interpreter!)
  (let [session (new-session-name)]
    (confine! roots-fn)
    (runtime/stdin! (guest-stdin-text stdin))
    (runtime/install-runtime! session)
    ;; `println` is the sandbox's historical second spelling of Python `print`, not a
    ;; host tool that callers must remember to inject. Seed it before protected-name
    ;; discovery so a block may shadow it locally but can never replace it for the session.
    (exec! session "globals().setdefault('println', print)")
    (try (runtime/install-module! session "auto_imports") (catch Throwable _ nil))
    (install-protected-names! session custom-bindings)
    ;; Tools first as ONE registration — the guest gets every name in one pass,
    ;; and the contracts below stamp the wrappers that pass leaves behind.
    (let [tools (into {}
                      (mapcat (fn [[sym val]]
                                (when (fn? val)
                                  (map (fn [nm]
                                         [nm val])
                                       (cons (sym->py-name sym) (py-aliases-for-sym sym))))))
                      (or custom-bindings {}))]
      (when (seq tools) (python-host/install-tools! session tools)))
    ;; …and the DATA bindings, which cross as JSON like every other value.
    (doseq [[sym val] (or custom-bindings {})
            :when (not (fn? val))]

      (set-python-binding! session sym val))
    ;; The contracts: description, signature and options vocabulary, keyed by
    ;; the same Python names the bindings above wired.
    (try (let [by-py-name (fn [sym->text]
                            (into {}
                                  (mapcat (fn [[sym _]]
                                            (when-let [d (get sym->text sym)]
                                              (map (fn [nm]
                                                     [nm d])
                                                   (cons (sym->py-name sym)
                                                         (py-aliases-for-sym sym))))))
                                  (or custom-bindings {})))]
           (update-json! session "__vis_docs__" (by-py-name (extension/sandbox-symbol-docs)))
           (update-json! session "__vis_sigs__" (by-py-name (extension/sandbox-symbol-signatures)))
           (update-json! session "__vis_keys__" (by-py-name (extension/sandbox-symbol-keys))))
         (catch Throwable _ nil))
    ;; With the shell tools off the `shell` verb is simply not bound, and a
    ;; shell-shaped question would answer with silence — which reads as "no
    ;; process can be started in this product". It can; the toggle closes the
    ;; MODEL's door only.
    (when-not (some (fn [[sym _]]
                      (= "shell" (sym->py-name sym)))
                    (or custom-bindings {}))
      (seed-json! session
                  "__vis_docs__"
                  {"shell"
                   (str (get PROCESS_SURFACE "off") " " (get PROCESS_SURFACE "extension"))}))
    (install-shims! session)
    (install-introspection! session)
    (exec! session "__vis_stamp_tools__()")
    (install-network! session network-opts)
    (install-network-probe! session)
    (install-autoinstall! session network-opts)
    {:python-context session
     :sandbox-ns :python
     :initial-ns-keys (set (map str (or (guest-value session "sorted(globals())") [])))}))

(defonce
  ^:private
  ^{:doc
    "Every session name this process has dropped.

   A session is a module namespace the interpreter creates on FIRST USE, so a
   name that ran once and was disposed would quietly come back as an EMPTY
   namespace with no doors in it. Remembering the name is what makes that a
   refusal instead."}
  disposed-sessions
  (atom #{}))

(defn dispose-python-context!
  "Drop `session`: its guest namespace, the host bindings it could call, and the
   right to run in it ever again.

   A namespace is a reference cycle through every function defined in it, so the
   interpreter has to be told; the host half has to go with it or every closure
   the session captured outlives it."
  [session]
  (when session
    (swap! disposed-sessions conj session)
    (python-host/forget-session! session)
    (try (runtime/close-session! session) (catch Throwable _ nil))
    nil))

;; =============================================================================
;; Session lifecycle the loop drives (dispose / probe / drain / interrupt)
;; =============================================================================

(defn context-enterable?
  "Can the loop still run guest code in this environment?

   False once the session was disposed or retired, or when the environment
   carries no session at all. There is no half-usable state to probe for: a
   namespace either exists in the one interpreter or it does not."
  [environment]
  (boolean (when-let [session (:python-context environment)]
             (not (contains? @disposed-sessions session)))))

(def ^:private guest-budget-ms
  "How long a between-turns guest call may hold the TURN thread.

   One interpreter, one GIL: a call into the guest waits for whatever is running
   there. Both callers below run on the turn thread — the collect between turns,
   the defs snapshot one line before the turn's outcome is persisted — so a block
   that parks holding the GIL would hold the turn with it: no terminal ever
   reaches the durable row, the turn stays `:running` in every listing, and the
   next cancel is refused as `:not-running`. Neither call is worth a turn."
  5000)

(defn- within-budget
  "`thunk`'s value, or nil when it did not come back inside [[guest-budget-ms]].

   The abandoned thread finishes on its own time and its answer is dropped: a
   guest call cannot be cancelled, so the only thing to give up is the WAIT."
  [thunk]
  (let [pending
        (future (try (thunk) (catch Throwable _ nil)))

        answer
        (deref pending (long guest-budget-ms) ::over-budget)]

    (when-not (= answer ::over-budget) answer)))

(defn interrupt-guest!
  "Ask the interpreter to raise `KeyboardInterrupt` in the thread running guest
   code, answering whether it landed.

   Bytecode-level, like CPython's own interrupt: a spinning `while True:` unwinds
   and the session survives; a thread blocked in a host call or inside C does not
   see it until it returns. False means the caller must retire the environment
   instead. One interpreter runs one block at a time, so `session` names WHOSE
   block the loop meant to stop and the interrupt reaches the thread running it."
  [session]
  (boolean (when session (try (runtime/interrupt!) (catch Throwable _ false)))))

(defn- prose-leading-syntax-hint
  "When a `:python/syntax` failure came from a reply that OPENED with PROSE — the
   recurring 'the model answered in Markdown' bug — return an actionable directive
   to prepend to the raw CPython message; else nil.

   The whole reply is run as one Python program, so a leading sentence/heading is
   itself a SyntaxError. CPython's message points at whatever mangled token trips
   first — an apostrophe (`I've` → unterminated string), a `×`/em-dash (invalid
   character), or an orphaned `)` (the matching `(` got swallowed by a quote-pair).
   Those messages read like unicode/typo bugs, so they get MISDIAGNOSED (and svar
   gets blamed). This converts them into one clear cause.

   Detection is high-precision: take the first non-blank, non-`#`-comment line; if
   it does NOT parse as Python on its own AND reads like a sentence (markdown
   marker, or 3+ space-separated word runs), it's prose. A genuine code line with a
   typo elsewhere parses fine alone → no hint, raw error preserved."
  [python-context code]
  (let [first-real (->> (str/split-lines code)
                        (map str/trim)
                        (remove str/blank?)
                        (remove #(str/starts-with? % "#"))
                        first)]
    (when (and (seq first-real)
               (try (count-top-level-forms python-context first-real)
                    false ; parses alone → real code
                    (catch Throwable _
                      (boolean (or (re-find #"^(#{1,6}\s|[-*]\s|>\s)" first-real) ; heading/bullet/quote
                                   (re-find #"\*\*" first-real)                   ; **bold**
                                   (re-find #"[A-Za-z]{2,}\s+[A-Za-z]{2,}\s+[A-Za-z]{2,}"
                                            first-real)))))) ; sentence
      (str "Your reply opened with PROSE, not Python. The engine runs your ENTIRE "
           "reply as one Python program, so the narration itself is the syntax error "
           "(this is NOT a unicode, typo, or svar problem). Put ALL narration in `#` "
           "comments above the code; the reply must START "
           "with runnable Python. Original parser error: "))))

(def ^:dynamic *auto-repair-brackets?*
  "When true, a bracket-balance syntax hint ALSO appends `repair-bracket-balance`'s
   single-candidate suggested fix. OFF by default: the walker only DIAGNOSES; the
   auto-fix stays gated behind this flag until proven safe in the wild."
  false)

(defn- render-source-context
  "Babashka-style source excerpt for an eval failure: a numbered ±2-line window of
   `code` around the 1-based `line`, with a caret run under the offending span
   (`col`/`end-col`, 0-based offsets into the detabbed line — tabs collapse to one
   space so 1 char == 1 caret column). Returns nil when `line` is out of range, so
   a positionless failure leaves the raw message untouched."
  [code line col end-col]
  (let [lines
        (vec (str/split-lines (str code)))

        n
        (count lines)]

    (when (and line (<= 1 (long line) n))
      (let [detab
            (fn [s]
              (str/replace s "\t" " "))

            i0
            (dec (long line))

            lo
            (max 0 (- i0 2))

            hi
            (min (dec n) (+ i0 2))

            width
            (count (str (inc hi)))

            sb
            (StringBuilder.)]

        (doseq [idx (range lo (inc hi))]
          (let [pfx (str (format (str "%" width "d") (inc (long idx))) ": ")
                txt (detab (nth lines idx))]

            (.append sb pfx)
            (.append sb txt)
            (.append sb "\n")
            (when (= idx i0)
              (let [c0 (if (and col (<= 0 (long col) (count txt))) (long col) 0)
                    end (if (and end-col (> (long end-col) c0)) (long end-col) (inc c0))
                    ;; Snap the caret start off leading whitespace: a `co_positions`
                    ;; quirk reports the ENCLOSING handler's column for a
                    ;; `raise … from …` inside an `except`, landing the caret start
                    ;; in the indentation gutter. Advance to the first non-space
                    ;; within the span so the caret always begins on real code (a
                    ;; no-op when the reported column already points at a token).
                    c (or (first (filter #(and (< (long %) end) (not= \space (nth txt %)))
                                         (range c0 (min end (count txt)))))
                          c0)
                    end* (min (long end) (count txt))
                    pad (+ (count pfx) (long c))
                    span (max 1 (- end* (long c)))]

                (.append sb (apply str (repeat pad \space)))
                (.append sb (apply str (repeat span \^)))
                (.append sb "\n")))))
        (str/trimr (str sb))))))

(def ^:private repeat-breaker-threshold
  "Consecutive identical (code, error) failures before the loop breaker fires.
   Two retries is normal recovery; the third is a loop."
  3)

(defn- diagnosis-hint
  "The sentence a `parse-diagnose` answer carries, whatever shape it came in."
  [d]
  (cond (string? d) (not-empty (str/trim d))
        (map? d) (not-empty (str/trim (str (or (:hint d) (:message d) ""))))
        :else nil))

;; =============================================================================
;; Running one block
;; =============================================================================

(def ^:private block-failure-memory
  "session -> [code message count]: the LAST failing block, so a model repeating
   the identical failure is told to change approach instead of looping."
  (atom {}))

(defn- note-block-failure!
  "Record this failure and answer how many times in a row it has now happened."
  [session code message]
  (let [[_ _ n] (get (swap! block-failure-memory (fn [m]
                                                   (let [[prev-code prev-msg cnt] (get m session)]
                                                     (assoc m
                                                       session (if (and (= prev-code code)
                                                                        (= prev-msg message))
                                                                 [code message
                                                                  (inc (long (or cnt 1)))]
                                                                 [code message 1])))))
                     session)]
    (long (or n 1))))

(defn- clear-block-failures!
  "A clean block ENDS the streak, so the breaker only counts CONSECUTIVE
   identical failures."
  [session]
  (swap! block-failure-memory dissoc session)
  nil)

(defn- guest-error-position
  "Where the failure happened in the block's own source, as `[line col]`.

   The runtime stashes the raised exception for exactly this lookup and computes
   the position from the deepest USER frame — walking the traceback while the
   guest is still inside its `except` is what used to replace a model's real
   error with an internal fault."
  [session]
  (let [pos (guest-value session "__vis_err_pos_now__()")]
    (when (and (sequential? pos) (first pos))
      [(long (first pos)) (when (second pos) (long (second pos)))
       (when (nth pos 2 nil) (long (nth pos 2 nil)))])))

(defn- syntax-error-position
  "`[line nil]` read out of a stringified SyntaxError, which CPython ends with
   `(<file>, line N)`.

   A source the parser refused never ran, so there is no traceback to walk and
   `__vis_err_pos_now__` has nothing to answer: the location printed in the
   message is the only position that failure has."
  [^String message]
  (when-let [[_ line] (re-find #"line (\d+)\)\s*$" message)]
    [(parse-long line) nil nil]))

(defn- char-column
  "A guest BYTE column as a CHARACTER column into `line-text`, or nil.

   CPython reports a code object's columns as UTF-8 byte offsets; every excerpt
   and caret here counts characters, so a line with a multi-byte glyph before
   the failing token would point past that token."
  [^String line-text col]
  (when col
    (if (str/blank? (str line-text))
      (long col)
      (loop [i
             0

             seen
             0]

        (cond (>= seen (long col)) i
              (>= i (count (str line-text))) i
              :else (recur (inc i)
                           (+ seen (alength (util/utf8 (subs (str line-text) i (inc i)))))))))))

(defn map-python-error
  "Map what a block RAISED into the engine's op-error shape.

   `:phase` is `:python/syntax` for a parse failure, `:python/host` when a host
   tool is what failed, else `:python/runtime`; `:line`/`:column` come from the
   guest position when there is one. The recurring syntax classes keep their
   actionable hint: a non-ASCII character in code position, a prose-leading
   reply, and — through `parse-diagnose` — an unbalanced quote or bracket."
  [session ^String raised code]
  (let [base
        (-> (str raised)
            str/trim
            ;; The interpreter wraps what Python raised in its own
            ;; `vis-python: ` frame. The model is owed the RAISED
            ;; error, and the class name below is what classifies it.
            (str/replace #"^vis-python:\s+" ""))

        syntax?
        (str/starts-with? base "SyntaxError")

        ;; IndentationError and TabError ARE SyntaxErrors — CPython names the
        ;; subclass, and a block that mis-indents never ran either.
        indent?
        (boolean (re-find #"^(IndentationError|TabError)" base))

        host?
        (str/starts-with? base "VisToolError")

        ;; A host tool's failure is the HOST's: the model is owed the tool's own
        ;; message, and the engine is owed the ex-info the tool threw. Read the
        ;; data BEFORE the position, which releases the stashed exception.
        tool-data
        (when host?
          (try (edn/read-string (str (guest-value session "__vis_err_host_data__()")))
               (catch Throwable _ nil)))

        tool-message
        (when host? (str/replace-first base #"^VisToolError:\s*" ""))

        pos
        (or (when (or syntax? indent?) (syntax-error-position base)) (guest-error-position session))

        prose-hint
        (when syntax? (prose-leading-syntax-hint session code))

        ;; Prose is the ROOT cause when the reply OPENS with narration, so a
        ;; leading sentence is reported as PROSE and never as "a stray glyph".
        non-ascii?
        (boolean (and syntax? (not prose-hint) (re-find #"invalid character" base)))

        quote-hint
        (when (and syntax? (not prose-hint) (not non-ascii?))
          (diagnosis-hint (try (parse-diagnose/diagnose-quote-balance code)
                               (catch Throwable _ nil))))

        bracket-hint
        (when (and syntax? (not prose-hint) (not non-ascii?) (not quote-hint))
          (diagnosis-hint (try (parse-diagnose/diagnose-bracket-balance code)
                               (catch Throwable _ nil))))

        ;; The confinement refuses in the interpreter itself, naming the operation
        ;; and whether it wanted to WRITE — the one denial the model can act on.
        denied-write?
        (boolean (re-find #"vis sandbox: .* is outside the writable roots" base))

        denied-root?
        (boolean (re-find #"vis sandbox: .* is outside the (readable|writable) roots" base))

        ;; `NameError: name 'X' is not defined` for a TOOL is usually an
        ;; extension toggled OFF — the engine removes its symbols while inactive,
        ;; so the call fails with nothing pointing at the toggle.
        undefined-name
        (when (and (not host?) (not syntax?))
          (second (re-find #"name '([^']+)' is not defined" base)))

        hint
        (cond prose-hint prose-hint
              non-ascii? (str
                           "A non-ASCII character leaked into CODE position - it is only "
                           "legal inside a \"...\" string or a `#` comment. This is almost always "
                           "a smart em-dash, en-dash, curly quote, or multiplication sign that you "
                           "meant as prose. Replace it with plain ASCII, or move that whole line "
                           "into a `#` comment. Original parser error: ")
              quote-hint (str quote-hint " Original parser error: ")
              bracket-hint (str bracket-hint " Original parser error: ")
              denied-root?
              (str "Sandbox policy denied "
                   (if denied-write? "file-write" "file-read")
                   ": the resource is outside approved filesystem roots. "
                   "Use grep({\"query\": q, \"context\": 4}) or cat(path, start, end) to read, "
                   "patch(path, edits) to edit, repl_eval(language, code) for project code, "
                   "or ask the USER to add the path to workspace.filesystem in vis.yml "
                   "and run /reload. Original error: ")
              undefined-name
              (str "`"
                   undefined-name
                   "` is not defined. If it's a TOOL you expected, it is "
                   "likely an extension that is inactive — its symbols are removed while off. Run "
                   "`apropos(\""
                   undefined-name
                   "\")`; if it isn't listed, ask the USER to enable "
                   "it and do NOT retry the name. If it's a variable, define it first. "
                   "Original error: ")
              indent? (str
                        "Python is INDENTATION-sensitive: a block (after def / if / for / with / "
                        "a trailing `:`) must be indented consistently (4 spaces), and a top-level "
                        "statement must start at column 0. Re-indent that region. Original error: ")
              :else nil)

        ;; CPython reports a code object's columns as UTF-8 BYTE offsets, while the
        ;; excerpt below counts CHARACTERS — a multi-byte glyph earlier on the line
        ;; would otherwise push the caret past the token it points at.
        line-text
        (when pos (nth (str/split-lines (str code)) (dec (long (first pos))) nil))

        source-context
        (when (and code pos (not host?))
          (render-source-context code
                                 (first pos)
                                 (char-column line-text (second pos))
                                 (char-column line-text (nth pos 2 nil))))

        repeats
        (note-block-failure! session code base)

        breaker
        (when (>= (long repeats) (long repeat-breaker-threshold))
          (str "This is failure "
               repeats
               " in a row with the SAME error. "
               "Stop repeating it: change the approach, use a different tool, "
               "or tell the USER what is blocking you.\n\n"))

        msg
        (if host?
          tool-message
          (str breaker
               (cond-> (if hint (str hint base) base)
                 source-context
                 (str "\n\n" source-context))))]

    {:message msg
     :data (cond-> {:phase (cond host? :python/host
                                 (or syntax? indent?) :python/syntax
                                 :else :python/runtime)}
             pos
             (assoc :line
               (first pos) :column
               (if-let [c (second pos)]
                 (inc (long c))
                 1))

             non-ascii?
             (assoc :non-ascii-in-code? true)

             prose-hint
             (assoc :prose-leading? true)

             quote-hint
             (assoc :unbalanced-quote? true)

             bracket-hint
             (assoc :unbalanced-bracket? true)

             denied-root?
             (assoc :sandbox-denied? true)

             undefined-name
             (assoc :name-undefined?
               true :undefined-name
               undefined-name)

             (>= (long repeats) (long repeat-breaker-threshold))
             (assoc :repeated-failures repeats)

             (map? tool-data)
             (merge tool-data))}))

(defn- empty-block-error
  "Op-error when `code` has NO top-level statements — only comments or
   whitespace. A parse failure is NOT empty: it falls through so the run
   surfaces the precise syntax error instead."
  [session code]
  (when (zero? (long (try (count-top-level-forms session code) (catch Throwable _ -1))))
    {:message (str "Empty block — nothing to execute. The code is only comments or "
                   "whitespace, so this iteration produces no evidence. Write at least "
                   "one statement, and print() what you want back.")
     :data {:phase :python/empty-block :empty-block? true}}))

(defn run-python-block
  "Run one Python `code` block in `session` as ONE whole-block coroutine,
   answering the FLAT sum-typed outcome:

     {:stdout <printed>}   ; SUCCESS — what the block print()ed
     {:error  <op-error>}  ; FAILURE — the raised error IS the result

   A block that printed nothing carries NEITHER key: its own value is discarded,
   so `print()` is the only way anything comes back. Either outcome may carry
   `:attachments`, the artifacts the block produced.

   The runtime AST-wraps the block in an `async def`, auto-settles every bare
   tool-call statement at every depth and drives it as a single coroutine.

   A session this process already disposed is REFUSED: the interpreter would
   otherwise mint the namespace again, empty, and run the block without one of
   its doors."
  [session code & [_opts]]
  (when (contains? @disposed-sessions session)
    (throw (ex-info (str "python session " session " was disposed")
                    {:type :vis/session-disposed :session session})))
  (if-let [err (empty-block-error session code)]
    {:forms [{:source code :error err}] :error err}
    (let [sink (atom [])
          outbox-seen (atom #{})]

      (with-bindings {#'extension/*current-form-idx* 0
                      #'mpl-capture/*attachment-sink* sink
                      #'mpl-capture/*outbox-seen* outbox-seen}
        ;; The doors run on the interpreter's own threads, so the bindings above
        ;; travel to them explicitly - see `python-host/conveying`.
        (python-host/conveying session
                               (let [outcome (or (read-json (runtime/run-block session code)) {})
                                     out (not-empty (str/trim-newline (str (:stdout outcome))))
                                     raised (:error outcome)
                                     attachments (mpl-capture/drain sink)]

                                 (when-not raised (clear-block-failures! session))
                                 (cond-> {}
                                   (and out (not (str/blank? out)))
                                   (assoc :stdout (str (:stdout outcome)))

                                   raised
                                   (assoc :error (map-python-error session raised code))

                                   attachments
                                   (assoc :attachments attachments))))))))

(def SYSTEM_VAR_NAMES "Sandbox-owned symbols hidden from user live-var listings." '#{session})

(defn system-var-sym? [sym] (contains? SYSTEM_VAR_NAMES sym))

(def ^:private session-defs-max-bytes
  "Cap on ONE session's persisted helper source. Helpers are small; anything past
   this is a runaway generator, not a toolbox, and is neither written nor read."
  262144)

(def ^:private last-session-defs
  "session-id -> the snapshot last written, so an unchanged toolbox re-writes nothing."
  (atom {}))

(defn persist-session-defs!
  "Write this session's own `def`s beside the session, for a LATER process.

   Globals persist across turns because the interpreter does — but the
   interpreter dies with the PROCESS. Restart the gateway and every helper the
   session refined is gone while the transcript still shows it, so the next call
   is a NameError against code the model can still read. Best effort; answers the
   file when it wrote one."
  [session session-id]
  (when (and session session-id)
    (try (let [src
               (within-budget #(guest-value session "__vis_defs_snapshot__()"))

               f
               (io/file (paths/sandbox-defs-file (str session-id)))]

           (cond
             ;; Over budget: the snapshot never came back, which says nothing
             ;; about the toolbox on disk — leave what is there alone.
             (nil? src) nil
             (str/blank? (str src))
             (do (swap! last-session-defs dissoc session-id) (when (.exists f) (.delete f)) nil)
             (> (count (str src)) (long session-defs-max-bytes)) nil
             (= (str src) (get @last-session-defs session-id)) nil
             :else (do (io/make-parents f)
                       (spit f (str src))
                       (swap! last-session-defs assoc session-id (str src))
                       f)))
         (catch Throwable _ nil))))

(defn forget-session-defs!
  "Drop `session-id`'s entry from the in-memory dedup memo. The on-disk snapshot
   deliberately SURVIVES — it is what a later process restores from."
  [session-id]
  (when session-id (swap! last-session-defs dissoc session-id) nil))

(defn restore-session-defs!
  "Re-create the helper definitions an EARLIER process persisted for
   `session-id`, answering how many are live afterwards.

   The restored source is registered as a real block, so `defs(\"name\")` and
   `inspect.getsource` read it back exactly like a local one, and it goes
   through the same rewrite so it RUNS like one."
  [session session-id]
  (when (and session session-id)
    (try (let [f (io/file (paths/sandbox-defs-file (str session-id)))]
           (when (and (.isFile f) (<= (.length f) (long session-defs-max-bytes)))
             (let [src (slurp f)
                   n (guest-value session (str "__vis_restore_defs__(" (pr-str src) ")"))]

               (swap! last-session-defs assoc session-id src)
               (when (number? n) (long n)))))
         (catch Throwable e
           (tel/log! {:level :debug :id ::restore-session-defs-failed :error e})
           nil))))
