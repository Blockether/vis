(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / START_REPL dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes stable bare tool names and dispatches to the
  active handler for the requested/current language. REPL lifecycle is resource
  backed: `repl` creates a language-owned session resource and `repl_stop`
  stops one by id. Live REPLs also surface in the ctx `resources` block."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.environment.core :as environment]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]))

(defn- normalize-language
  [x]
  ;; STRINGS-ONLY: dispatch on a lowercase language STRING. Registrations
  ;; declare `:language "clojure"` (a string) at the source — there is NO
  ;; colon-strip tolerance; a keyword registered here would surface as an
  ;; unmatched ":clojure" handler immediately, which is the point.
  (some-> x
          str
          str/lower-case))

(defn- language-scan
  "Embedded scan data in tests/legacy callers, otherwise the cached scan for the
   dynamically bound workspace root. The production tool env does not carry the
   model-facing session digest, so dispatch must consult the environment source."
  [env]
  (or (:env/languages env)
      (:languages env)
      (try (:languages (environment/snapshot)) (catch Throwable _ nil))))

(defn- env-language
  [env]
  (let [scan (language-scan env)]
    (or (normalize-language (get-in env [:env/project :primary_language]))
        (normalize-language (get-in env [:project :primary_language]))
        (normalize-language (:primary scan))
        (some->> (:languages scan)
                 (map #(normalize-language (or (:language %) (:name %) %)))
                 (remove nil?)
                 first))))

(defn- active-extensions
  [env]
  (or (some-> env
              :active-extensions
              deref
              seq)
      (some-> env
              :extensions
              deref
              seq)
      (extension/registered-extensions)))

(defn- registered-handlers
  [env capability]
  (->> (active-extensions env)
       (mapcat :ext/language-tools)
       (keep (fn [entry]
               (let
                 [language
                  (normalize-language (:language entry))

                  f
                  (get entry capability)]

                 (when f
                   (assoc entry
                     :language language
                     :handler f)))))))

(def ^:private capability->tool
  "language-tool key -> the facade verb shown in the capability matrix."
  {:format-fn "format_code"
   :lint-fn "lint_code"
   :test-fn "run_tests"
   :repl-eval-fn "repl_eval"
   :start-repl-fn "repl"})

(def ^:private tool-order ["format_code" "lint_code" "run_tests" "repl_eval" "repl"])

(defn capability-data
  "STRUCTURED capability map for the ACTIVE language packs:
   `{\"clojure\" [\"format\" \"test\" \"repl_eval\" \"repl\"], \"python\" [...]}`
   — nil when none active. Recomputed every turn from active-extensions, so it
   GAINS a language the moment its pack activates (e.g. a .py file appears).
   INTERNAL: it feeds the EXTENSIONS prompt block and the `resources` ctx
   projection. It is NOT shipped in the model's `session` dict — that was a
   verbatim duplicate of the prompt block's LANGUAGE TOOLS lines."
  [env]
  (let
    [by-lang (reduce (fn [m cap]
                       (reduce (fn [m h]
                                 (update m (:language h) (fnil conj #{}) (capability->tool cap)))
                               m
                               (registered-handlers env cap)))
                     {}
                     (keys capability->tool))]
    (when (seq by-lang)
      (into (sorted-map)
            (for [[lang tools] by-lang]
              [lang (vec (filter tools tool-order))])))))

(defn capability-matrix
  "AUTO capability matrix for the system prompt — the active packs' facade verbs
   + a CERTAIN statement of when each is the tool. nil when no pack is active.
     LANGUAGE TOOLS (active packs; language first):
       clojure : format_code · run_tests · repl_eval · repl
       python  : repl_eval · repl"
  [env]
  (when-let [data (capability-data env)]
    (str
      "LANGUAGE TOOLS (active packs; language first):\n"
      (str/join "\n"
                (for [[lang tools] data]
                  (str "  " lang " : " (str/join " · " tools))))
      (when (contains? data "clojure")
        (str
          "\n  clojure run_tests NEVER starts a REPL: with none running it shells the project's"
          " own test command in a CLEAN JVM, so the run always sees the code on disk. When THIS"
          " session already has a REPL up for that project it REUSES it — and that path reloads"
          " the namespaces it RUNS but NEVER their dependencies, so a changed PRODUCTION ns still"
          " serves the Vars already loaded: `repl_eval` `(require 'my.prod.ns :reload)` for each"
          " one you edited, or `repl` stop and let the clean JVM run them."
          "\n  clojure lint_code runs clj-kondo + `general` REFLECTION/BOXED-MATH checks;"
          " whole-project lint (omit code/paths) includes both; no separate reflection check.")))))

(defn- language-like? [x] (and (string? x) (re-matches #"[A-Za-z][A-Za-z0-9_-]*" x)))

(defn- coerce-opts
  [arg]
  (cond (nil? arg) {}
        (map? arg) arg
        :else {:arg arg}))

(defn- opts-language [opts] (get opts "language"))

(def ^:private language-aliases
  "Grammar VARIANTS that share a base language's TOOLING. `tsx`/`jsx` are distinct
   tree-sitter grammars (the distinction matters for PARSING), but Bun/Node run
   them exactly like their base language, so a tool request for a variant resolves
   to the base family's handler when no exact handler is registered. The
   `.mjs/.cjs/.mts/.cts` module variants already collapse in the workspace scan,
   but a caller can still name one explicitly."
  {"tsx" "typescript"
   "mts" "typescript"
   "cts" "typescript"
   "jsx" "javascript"
   "mjs" "javascript"
   "cjs" "javascript"})

(defn- alias-of [lang] (get language-aliases lang))

(defn- scanned-languages
  "The workspace scan's languages in FILE-COUNT order (most files first),
   normalized to lowercase strings, nil-free."
  [env]
  (->> (:languages (language-scan env))
       (keep #(normalize-language (or (:language %) (:name %) %)))))

(defn- candidate-languages
  "Ordered, DISTINCT languages to try when resolving a handler, each followed by
   its family alias so a variant (`tsx`) can fall back to its base (`typescript`):

     1. an EXPLICIT `language` opt is the whole intent — try only it (+ alias), so
        an explicit unsupported language still errors rather than silently picking
        another pack;
     2. otherwise the workspace PRIMARY language, then every OTHER scanned language
        in file-count order.

   Step 2 is the heuristic that makes a bare `repl_eval`/`run_tests`/`format_code`
   work in a repo whose top language is DATA (a json/yaml-heavy TS app, or vis
   itself): the dominant data language has no pack, so we fall through to the
   first REAL code language a pack can actually handle."
  [env explicit]
  (->> (if explicit [explicit] (cons (env-language env) (scanned-languages env)))
       (mapcat (fn [l]
                 [l (alias-of l)]))
       (remove nil?)
       distinct
       vec))

(defn- choose-handler
  [env capability opts]
  (let
    [handlers
     (vec (registered-handlers env capability))

     by-lang
     (group-by :language handlers)

     explicit
     (normalize-language (opts-language opts))

     ;; First candidate resolving to EXACTLY one handler wins; a candidate
     ;; matching several is genuinely ambiguous and stops the search there.
     picked
     (some (fn [l]
             (let [ms (get by-lang l)]
               (cond (= 1 (count ms)) {:handler (first ms)}
                     (seq ms) {:ambiguous l :matches ms})))
           (candidate-languages env explicit))]

    (cond (empty? handlers) (throw (ex-info
                                     (str "No language extension registered for " (name capability))
                                     {:type :language-surface/no-handler :capability capability}))
          (:handler picked) (:handler picked)
          (:ambiguous picked)
          (throw (ex-info (str
                            "Multiple language handlers match "
                            (:ambiguous picked)
                            "; pass the language as first arg, e.g. repl_eval with language first")
                          {:type :language-surface/ambiguous-language
                           :language (:ambiguous picked)
                           :capability capability
                           :available (vec (keep :language (:matches picked)))}))
          ;; No candidate matched, but exactly one pack is active and the caller named
          ;; no language — use it (single-pack convenience).
          (and (nil? explicit) (= 1 (count handlers))) (first handlers)
          explicit (throw (ex-info (str "No " (name capability) " handler for language " explicit)
                                   {:type :language-surface/no-language-handler
                                    :language explicit
                                    :capability capability
                                    :available (vec (keep :language handlers))}))
          :else (throw (ex-info
                         (str
                           "Multiple language handlers match current workspace"
                           "; pass the language as first arg, e.g. repl_eval with language first")
                         {:type :language-surface/ambiguous-language
                          :language nil
                          :capability capability
                          :available (vec (keep :language handlers))})))))

(defn- parse-language-call
  [args]
  (case (count args)
    0
    {:opts {} :payload {}}

    1
    (let [arg (first args)]
      {:opts (coerce-opts arg) :payload arg})

    2
    (let [[language payload] args]
      (if (language-like? language)
        {:opts (assoc (coerce-opts payload) "language" language) :payload payload}
        (throw (ex-info "Expected language as first arg, e.g. repl_eval(language, ...)."
                        {:type :language-surface/bad-args :got args}))))

    (throw (ex-info "Expected (arg) or (language, arg)."
                    {:type :language-surface/bad-args :got args}))))

(defn- dispatch!
  "Run `capability`'s handler for the chosen language. `post` (default: the raw
   result) sees the CHOSEN handler alongside its result, so a caller can stamp
   handler-derived facts — the language that actually ran — without re-resolving
   it."
  ([env capability args]
   (dispatch! env
              capability
              args
              (fn [_handler result]
                result)))
  ([env capability args post]
   (let
     [{:keys [opts payload]}
      (parse-language-call args)

      handler
      (choose-handler env capability opts)]

     ;; A live environment may predate a process-jail namespace reload. Refresh the
     ;; session binding immediately before a handler that may spawn a child: Clojure
     ;; run_tests shells the project's own test command whenever no REPL is up.
     (when (#{:test-fn :repl-eval-fn} capability) (vis/prepare-session-jail! env))
     (post handler ((:handler handler) env payload)))))

(def ^:private repl-ops #{"status" "start" "stop" "connect"})

;; `restart` is GONE — not an op, not in the schema, not a silent alias for
;; start. It was a stop+start pretending to be one atomic step: it tore down the
;; REPL the turn was standing on, and when the relaunch hung (a slow cold boot,
;; a wedged JVM) the caller was left with NO repl and no usable error. Stop and
;; start are two decisions; the agent makes them separately.
;;
;; The token stays RECOGNIZED as an op purely so a stale `repl(lang, "restart")`
;; is refused loudly instead of being parsed as a REPL *id* and silently
;; starting something.
(def ^:private removed-repl-ops #{"restart"})

(defn- repl-op? [x] (and (string? x) (or (contains? repl-ops x) (contains? removed-repl-ops x))))

(defn- reject-removed-op!
  "Fail closed on an op that no longer exists, naming the two calls that replace it."
  [op]
  (when (contains? removed-repl-ops op)
    (throw (ex-info (str "repl op " (pr-str op)
                         " was REMOVED. Stop the REPL, then start a new one"
                         " — two calls, so a failed start cannot masquerade as a live REPL.")
                    {:type :language-surface/removed-op
                     :got op
                     :allowed (vec (sort repl-ops))
                     :examples ["repl('clojure', 'stop', {'cwd': 'extensions/foo'})"
                                "repl('clojure', 'start', {'cwd': 'extensions/foo'})"]}))))

(defn- start-repl-payload
  [args]
  (let
    [[language more]
     (if (and (seq args) (language-like? (first args)) (not (repl-op? (first args))))
       [(first args) (next args)]
       [nil args])]
    (case (count more)
      0
      {:language language :id nil :op "start" :opts {}}

      1
      (let [arg (first more)]
        (cond (nil? arg) {:language language :id nil :op "start" :opts {}}
              (map? arg) {:language (or language (opts-language arg))
                          :id (or (get arg "id") (get arg "repl_id"))
                          :op (or (get arg "op") "start")
                          :opts (dissoc arg "op")}
              :else {:language language :id nil :op arg :opts nil}))

      2
      (let [[a b] more]
        (if (map? b)
          {:language (or language (opts-language b))
           :id (when-not (repl-op? a) a)
           :op (if (repl-op? a) a "start")
           :opts b}
          {:language language :id a :op b :opts nil}))

      3
      (let [[id op opts] more]
        {:language (or language (opts-language opts)) :id id :op op :opts opts})

      (throw
        (ex-info
          "repl expects (language?), (language, opts), (language, op, opts), or (language, id, op, opts)."
          {:type :language-surface/bad-args
           :got args
           :examples ["repl('clojure')" "repl('clojure', {'op': 'start', 'cwd': 'extensions/foo'})"
                      "repl('clojure', 'status')"
                      "repl('clojure', 'main', 'stop', {'cwd': 'extensions/foo'})"]})))))

(defn repl-stop
  "Stop a REPL by session resource id. This is the REPL-specific wrapper around resource_stop(id)."
  [env id]
  ;; `stop-resource!` returns an INTERNAL keyword-keyed map ({:result :stopped
  ;; :id ...}); project it to a strings-only model payload (enum value stringified
  ;; at the source) so nothing keyword crosses the boundary.
  (let [{:keys [result id message]} (vis/stop-resource! (:session-id env) id)]
    (extension/success {:result {"result" (name result)
                                 "id" (str id)
                                 ;; TOTAL: `message` is nil rather than absent, so
                                 ;; r["message"] reads on EVERY stop instead of
                                 ;; KeyErroring on the clean path.
                                 "message" message}})))

(defn- dispatch-start-repl!
  [env args]
  (let [{:keys [language id op opts]} (start-repl-payload args)]
    (reject-removed-op! op)
    (if (and (= "stop" op) id)
      ;; By-id stop is a generic session-resource op — no pack dispatch needed,
      ;; and it works even when the owning language pack is gone.
      (repl-stop env id)
      (let
        [dispatch-opts (cond-> (coerce-opts opts)
                         language
                         (assoc "language" language)

                         id
                         (assoc "id" id))
         handler (choose-handler env :start-repl-fn dispatch-opts)
         opts (cond-> (or opts {})
                id
                (assoc "id" id))]

        ;; Refresh from the live env at the process boundary. This also repairs the
        ;; registry after a process-jail namespace reload without weakening fail-closed
        ;; handling for missing session identity or policy.
        (when (= "start" op) (vis/prepare-session-jail! env))
        ((:handler handler) env op opts)))))

(defn- repl-resources
  [env language]
  (let [lang (normalize-language language)]
    ;; `list-resources` returns string-keyed DATA maps with string enum VALUES
    ;; ("kind" "nrepl", "status" "up"), so filter on strings.
    (->> (vis/list-resources (:session-id env))
         (filter #(let [kind (str (get % "kind"))]

                    (or (= "repl" kind) (= "nrepl" kind) (str/ends-with? kind "repl"))))
         (filter #(or (nil? lang) (= lang (normalize-language (get % "language")))))
         vec)))

(defn repl-status
  "List REPL resources, optionally filtered by language or id."
  ([env] (repl-status env nil))
  ([env arg]
   (let
     [opts
      (coerce-opts arg)

      lang
      (or (opts-language opts) (when (language-like? arg) arg))

      id
      (or (get opts "id") (get opts "repl_id"))]

     (extension/success {:result {"resources" (cond->> (repl-resources env lang)
                                                id
                                                (filter #(= (str id) (get % "id")))

                                                true
                                                vec)}}))))

;; Call-selection helpers — what a CALL asked for, read off its input map. A pack
;; reports what it RAN; only the call knows what was SELECTED, so `run_tests`
;; stamps `test-target` into the result metadata.

(defn- input-list
  "Comma-joined non-blank entries of list-ish call key `k`, else nil — how a
   multi-value selection reads on one line. A bare string is ONE entry, never a
   sequence of characters."
  [input k]
  (let
    [xs (let [v (get input k)]
          (cond (nil? v) nil
                (sequential? v) v
                :else [v]))]
    (some->> xs
             (keep #(some-> %
                            str
                            str/trim
                            not-empty))
             seq
             (str/join ", "))))

;; Every spelling a call can select WITH: `paths` in every language, plus the
;; namespace / var vocabulary the clojure pack resolves (`clojure -M:test`'s own
;; `--namespace` / `--var`). A selection rendered as "full suite" would make a
;; one-namespace run read like the whole workspace.
(def ^:private selection-keys ["paths" "ns" "nses" "namespace" "namespaces" "var" "vars" "only"])

(defn- test-target
  "WHAT a run_tests call selected, as one line: its selector entries — files,
   directories, `<path>::<test-name>` node ids, or the namespaces a clojure call
   named — else the whole suite. The pending card and the finished headline read
   the SAME string off the call, because a runner reports only what it RAN — two
   runs that selected different tests must never render the same summary."
  [input]
  (or (some->> selection-keys
               (keep (partial input-list input))
               seq
               (str/join ", "))
      "full suite"))

(defn format-code
  "Format through a pack: `format_code(language,arg)`; omit `language` only for paths-based inference. Source/`{\"code\":...}` returns changed + char-delta, never text. `{\"paths\":[...]}` (always a list) recursively formats files/dirs in place and returns per-file changes, never text. Omit code/paths for default source paths recursively."
  [env & args]
  (dispatch! env :format-fn args))

(defn lint-code
  "Lint through a pack: `lint_code(language,arg)`; omit `language` only for file/workspace inference. Source/`{\"code\":...}` lints a snippet; `{\"paths\":[...]}` (always a list) lints disk. Omit code/paths for defaults. Returns findings and severity counts."
  [env & args]
  (dispatch! env :lint-fn args))

(defn run-tests
  "Run through a pack: `run_tests(language,arg)`. `arg` is a path string or map: `paths` (files, directories, or `<path>::<test-name>` node ids — the selector every language shares; `::<test-name>` alone finds that test wherever it lives) selects; clojure ALSO takes `ns` / `nses` (a namespace name, or `ns/var` for one test) and resolves it the same way, and runs `*_test.cljs` through the project's own shadow-cljs build (`build` names which one when several could); `include` / `exclude` narrow by metadata tag; `cwd` chooses the project; `environment` picks the python backend (`project` for the project interpreter's own pytest, else the hermetic sandbox). List selectors stay lists, even one. Omit `arg` for all tests."
  [env & args]
  (let
    [started-at
     (System/nanoTime)

     ;; A pack reports what it RAN; only the CALL knows what was ASKED FOR, so
     ;; stamp the selection here or the headline cannot tell two runs apart.
     target
     (test-target (or (first (filter map? args)) {}))]

    (dispatch! env
               :test-fn
               args
               (fn [handler envelope]
                 ;; Language handlers return extension envelopes. Complete and time
                 ;; the PUBLIC payload; metadata added beside :result gets unwrapped.
                 (if (and (map? envelope) (contains? envelope :result))
                   (update
                     envelope
                     :result
                     (fn [result]
                       (let [completed (contract/complete-test-result (:language handler) result)]
                         (if (map? completed)
                           (assoc completed
                             "target" (or (get result "target") target)
                             "ms" (quot (- (System/nanoTime) started-at) 1000000))
                           completed))))
                   envelope)))))

(defn repl-eval
  "Eval in an already-running project REPL: `repl_eval(language,arg)`. Pass `language` first; `arg` may set `id`/`repl_id`, `cwd` (root default), and `timeout_ms`."
  [env & args]
  (dispatch! env :repl-eval-fn args))

(defn start-repl
  "Start a language REPL resource: `repl(language,{op,cwd,id,...})`. Pass `language` first; `op` defaults to `start`. There is no restart: `stop`, then `start`."
  [env & args]
  (dispatch-start-repl! env args))

(defn connect-repl
  "Attach to an external running REPL: `repl_connect(language,{port|build,host?,cwd?})`. A `build` attaches to the project's `shadow-cljs watch` and selects it, making eval ClojureScript. Registers it for eval, tests, and context but never owns or kills its process; stop only detaches."
  [env & args]
  (let
    [[language more]
     (if (and (seq args) (language-like? (first args))) [(first args) (next args)] [nil args])

     opts
     (coerce-opts (first more))

     dispatch-opts
     (cond-> opts
       language
       (assoc "language" language))

     handler
     (choose-handler env :start-repl-fn dispatch-opts)]

    ((:handler handler) env "connect" opts)))

(def format-symbol
  (vis/symbol
    #'format-code
    {:symbol 'format_code
     :result
     (str
       "String-keyed `op` result. Code/file: `changed` plus optional `chars,path,formatter,repaired`; "
       "batch: `files,by-cwd,formatters`. No source text.")
     :description
     (str
       "Format through the active pack — `format_code({\"paths\": [\"src\"]})`, or "
       "`format_code(\"python\", {\"code\": src})`. `language` leads the call and is optional: it is "
       "inferred from the paths and the workspace, and needed only when several packs match. `paths` "
       "(ALWAYS a list) formats files and directories in place, recursively, and answers per-file "
       "changes; `code` formats one snippet and answers `changed` + char delta, NEVER the text. Omit "
       "both to format the pack's default source paths.")
     ;; NAME(language, {payload}) — optional leading `language`, the rest a
     ;; pure options dict (always emitted so the payload stays a map).
     :params [{:name "language"} {:name "paths"} {:name "code"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def lint-symbol
  (vis/symbol
    #'lint-code
    {:symbol 'lint_code
     :result
     (str
       "String-keyed `op` object: `language`, severity counts, `files,findings,providers,by-cwd`; "
       "stdin adds `snippet`, paths add `targets`. Findings use `file,row,col,level,type,message` "
       "and optional `provider`.")
     :description
     (str
       "Lint through the active pack, editing nothing — `lint_code({\"paths\": [\"src\"]})`, or "
       "`lint_code(\"python\", {\"code\": src})`. `language` leads the call and is optional: it is "
       "inferred from the paths and the workspace, and needed only when several packs match. `paths` "
       "(ALWAYS a list) lints disk, `code` lints one snippet, and omitting both lints the pack's "
       "defaults across the workspace. Answers findings plus severity counts.")
     :params [{:name "language"} {:name "paths"} {:name "code"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :observation}))

(def test-symbol
  (vis/symbol
    #'run-tests
    {:symbol 'run_tests
     :result
     (str
       "String-keyed, stamped with `op`; absent fields mean not applicable. The verdict is `is_pass`. "
       "Counts: `total`, `pass`, `fail`, `errored` (the erroring subset of `fail`), `skipped`, "
       "`selected`. `failures` carries ONE ROW PER FAULT — `test`, `type` (`fail` or `error`), "
       "`message`, plus `ns` (Clojure) or `file` (Python) — so a red run already names what to "
       "open. `output` is the runner's own text; `ns`, `target`, `framework`, `mode`, `ms` say "
       "what ran and how. A REPL that could not serve answers `repl_wedged`, `repl_unusable`, "
       "`recovered` and a `hint`; `timed_out`, `error`, `exit` carry the rest.")
     :description
     (str
       "Run the pack's tests — `run_tests({\"paths\": [\"test/foo_test.clj\"]})`, or "
       "`run_tests(\"python\", {\"paths\": [...]})`. `language` leads the call and is optional "
       "(inferred from the paths and the workspace). Prefer the smallest target: `paths` is the "
       "shared selector and each entry is a file, a directory, or `<path>::<test-name>` for a "
       "single test; clojure also takes `ns` — a namespace name, or `ns/var` for one test. "
       "`include`/`exclude` narrow by tag, `cwd` chooses the project. Omit `paths` to run everything.")
     :params [{:name "language"} {:name "paths"}
              {:name "ns" :note "clojure — namespace or `ns/var`"} {:name "include"}
              {:name "exclude"} {:name "cwd"}]
     :call {:lead-opt "language" :rest :always}
     ;; run_tests can exceed the generic Python eval watchdog; dispatch it
     ;; directly in Clojure so the language pack's own timeout budget wins.
     :inject-env? true
     :tag :mutation}))

(def repl-eval-symbol
  (vis/symbol
    #'repl-eval
    {:symbol 'repl_eval
     :result (str
               "Pack-defined string-keyed object stamped with `op`; fields may be absent. Clojure: "
               "`code,repl,value/values,out,err,status,ns,ms,timed_out,ex,root_ex`; Python/Bun: "
               "`code,ok,out,err,value,data,type,exc`. No UI `transcript` or `content`.")
     :description
     (str
       "Evaluate `code` in an already-running project REPL — "
       "`repl_eval({\"language\": \"clojure\", \"code\": \"(+ 1 1)\"})`. `code` is REQUIRED and "
       "`language` may lead the call; `id`/`repl_id` picks one of several REPLs, `cwd` its project, "
       "`timeout_ms` its budget. Lifecycle — start, status, stop — is `repl`. A REPL attached to a "
       "shadow-cljs `build` evaluates ClojureScript inside that build's JS runtime and the result "
       "names the `build` it landed in.")
     :params [{:name "language"} {:name "code" :required? true} {:name "id" :note "or `repl_id`"}
              {:name "cwd"} {:name "timeout_ms"}]
     :call {:lead-opt "language" :rest :always}
     ;; repl_eval's own `timeout_ms` can exceed the generic Python eval
     ;; watchdog (DEFAULT_EVAL_TIMEOUT_MS, five minutes); dispatch it directly in
     ;; Clojure so the language pack's own timeout budget wins (parity with
     ;; run_tests above).
     :inject-env? true
     :tag :mutation}))

(def start-repl-symbol
  (vis/symbol
    #'start-repl
    {:symbol 'repl
     :result
     (str
       "String-keyed result stamped with `op`; never a `{resources: [...]}` list. Status: Clojure "
       "`result,id,cwd,status`, Python/Bun `cwd,status`. Start/connect may add "
       "`running,port,pid,cmd,tool,aliases,external,host,log,message`, and a shadow-cljs attachment "
       "adds `build,target,dialect,runtime`; stop by id returns `{result,id,message}`.")
     :description
     (str
       "REPL lifecycle — `repl(\"clojure\", {\"op\": \"status\"})`, or `repl({\"language\": ..., "
       "\"op\": \"start\"})`. `op` is `start` (the default) | `status` | `stop` | `connect`; `cwd` "
       "chooses the project and `id` the REPL a `stop` ends. "
       "Nothing lists live REPLs for you: `status` is the only answer — reuse `up`, "
       "recheck `starting`, start when absent/down/failed. There is NO restart op: a wedged REPL is "
       "`stop` then `start`. `status` reports that "
       "directory's state; `stop` ends a managed REPL; `connect` attaches an external REPL by port and only detaches it. "
       "`connect` with `build` attaches to the `shadow-cljs watch` running under `cwd` — it publishes "
       "its own port, so `port` is optional — and SELECTS that build, so later `repl_eval` is "
       "ClojureScript in its JS runtime. It rides BESIDE the managed JVM REPL for the same `cwd`, each "
       "under its own id (`nrepl:~/proj` and `nrepl:~/proj#app`); `stop` with `build` detaches only it.")
     :params [{:name "language"} {:name "op" :note "start | status | stop | connect"} {:name "cwd"}
              {:name "id" :note "which REPL a stop ends"} {:name "port"} {:name "host"}
              {:name "build" :note "shadow-cljs build to attach + select (ClojureScript)"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def connect-repl-symbol
  (vis/symbol
    #'connect-repl
    {:symbol 'repl_connect
     :result (str
               "String-keyed and stamped with `op` — the shape `repl`'s own `connect` answers: "
               "`result,id,cwd,status` plus `running,port,host,external,message` when known, and "
               "`build,target,dialect,runtime` for a shadow-cljs build.")
     :description
     (str
       "Attach an external running REPL — `repl_connect(\"clojure\", {\"port\": 56428})`. `port` names "
       "it, `host` (default localhost) and `cwd` say where it lives. `build` instead attaches to the "
       "`shadow-cljs watch` under `cwd` — it publishes its own port, so `port` is optional there — and "
       "selects that build, making `repl_eval` ClojureScript in its JS runtime. Vis registers it for "
       "eval, tests and context but never owns or kills it, so stopping it only detaches.")
     :params [{:name "language"} {:name "port" :note "or `build`"} {:name "host"} {:name "cwd"}
              {:name "build" :note "shadow-cljs build to attach + select"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def repl-stop-symbol
  (vis/symbol
    #'repl-stop
    {:symbol 'repl_stop
     :result
     "String-keyed `{result, id, message}` stamped with `op`; an external REPL is only detached."
     :description
     (str
       "Stop the managed REPL you started, by the exact `id` `repl` answered with — after verification, "
       "so nothing is left running. The id is REQUIRED. An external REPL attached by `repl_connect` "
       "is only detached, never killed.")
     ;; repl_stop(id) — one positional id, unlike the language-led verbs above.
     :call {:pos ["id"]}
     :inject-env? true
     :tag :mutation}))

(def symbols
  [format-symbol lint-symbol test-symbol repl-eval-symbol start-repl-symbol connect-repl-symbol
   repl-stop-symbol])

(defn prompt
  "The language-facade reference: the AUTO capability matrix (active packs only)
   + the bare facade verbs. nil when no language pack is active, so a non-coding
   or single-language workspace carries nothing extra. Each verb's own docstring
   holds its args/return; `language` is explicit only when several packs match."
  [env]
  (capability-matrix env))
