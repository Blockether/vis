(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / REPL-LIFECYCLE dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes stable bare tool names and dispatches to the
  active handler for the requested/current language. REPL lifecycle is resource
  backed: `repl_start` creates a language-owned session resource, `repl_status`
  reports it and `repl_stop` ends one. Live REPLs also surface in the ctx
  `resources` block."
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
               (let [language
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
   :start-repl-fn "repl_start"})

(def ^:private tool-order ["format_code" "lint_code" "run_tests" "repl_eval" "repl_start"])

(defn capability-data
  "STRUCTURED capability map for the ACTIVE language packs:
   `{\"clojure\" [\"format\" \"test\" \"repl_eval\" \"repl_start\"], \"python\" [...]}`
   — nil when none active. Recomputed every turn from active-extensions, so it
   GAINS a language the moment its pack activates (e.g. a .py file appears).
   INTERNAL: it feeds the EXTENSIONS prompt block and the `resources` ctx
   projection. It is NOT shipped in the model's `session` dict — that was a
   verbatim duplicate of the prompt block's LANGUAGE TOOLS lines."
  [env]
  (let [by-lang (reduce (fn [m cap]
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
       clojure : format_code · run_tests · repl_eval · repl_start
       python  : repl_eval · repl_start"
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
          " own test command in a CLEAN JVM, so the run sees the code on disk. When THIS"
          " session already has one for that project it REUSES it — that path reloads"
          " the namespaces it RUNS but NEVER their dependencies, so a changed PRODUCTION ns"
          " still serves the Vars already loaded: `repl_eval` `(require 'my.prod.ns :reload)` for"
          " each one you edited, or `repl_stop` and let a clean JVM run them."
          "\n  clojure lint_code runs clj-kondo + `general` REFLECTION/BOXED-MATH checks;"
          " whole-project lint (omit code/paths) does both; no separate reflection tool.")))))

(defn- language-like? [x] (and (string? x) (re-matches #"[A-Za-z][A-Za-z0-9_-]*" x)))

(defn- coerce-opts
  [arg]
  (cond (nil? arg) {}
        (map? arg) arg
        :else {:arg arg}))

;; `root` and `project` are aliases of `cwd` — the directory a call acts in. Resolve
;; them HERE, once, for every verb. Multiple spellings may repeat one directory, but
;; different directories are refused rather than silently reduced to one.
(defn- directory->cwd
  "The call's `root`/`project` directory aliases normalized to `cwd`."
  [m]
  (if-not (map? m)
    m
    (let [named
          (into {}
                (keep (fn [k]
                        (when-let [v (some-> (get m k)
                                             str
                                             str/trim
                                             not-empty)]
                          [k v])))
                ["cwd" "root" "project"])

          dirs
          (set (vals named))]

      (when (> (count dirs) 1)
        (throw (ex-info (str "A call names ONE directory; `cwd`, `root`, and `project` disagree: "
                             (pr-str named))
                        {:type :language-surface/bad-args :directories named})))
      (cond-> (dissoc m "root" "project")
        (seq dirs)
        (assoc "cwd" (first dirs))))))

(defn- path->paths
  "The singular `path` selector normalized into the shared `paths` list."
  [m]
  (if-not (and (map? m) (contains? m "path"))
    m
    (let [path
          (get m "path")

          paths
          (get m "paths")

          paths
          (cond (nil? paths) []
                (sequential? paths) paths
                :else [paths])]

      (cond-> (dissoc m "path")
        (some? path)
        (assoc "paths" (vec (cons path paths)))))))

(defn- normalize-call-map
  "Normalize directory and selector aliases before any language pack sees a call."
  [x]
  (-> x
      directory->cwd
      path->paths))

(defn- env-at-cwd
  "For format/lint, make their workspace-relative discovery begin at selected `cwd`."
  [env opts]
  (if-let [cwd (some-> (get opts "cwd")
                       str
                       str/trim
                       not-empty)]
    (let [^java.io.File base (java.io.File. ^String (str (or (:workspace/root env) ".")))
          ^java.io.File selected (java.io.File. ^String cwd)]

      (assoc env
        :workspace/root (str
                          (if (.isAbsolute selected) selected (java.io.File. base ^String cwd)))))
    env))

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
  (let [handlers
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
    (let [arg (normalize-call-map (first args))]
      {:opts (coerce-opts arg) :payload arg})

    2
    (let [[language raw]
          args

          payload
          (normalize-call-map raw)]

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
   (let [{:keys [opts payload]}
         (parse-language-call args)

         handler
         (choose-handler env capability opts)

         call-env
         (if (#{:format-fn :lint-fn} capability) (env-at-cwd env opts) env)]

     ;; A live environment may predate a process-jail namespace reload. Refresh the
     ;; session binding immediately before a handler that may spawn a child.
     (when (#{:test-fn :repl-eval-fn} capability) (vis/prepare-session-jail! env))
     (post handler ((:handler handler) call-env payload)))))

;; An `op` STRING is GONE from this surface: every lifecycle step is its own
;; verb — `repl_start`, `repl_status`, `repl_stop`, `repl_connect` — so the call
;; itself says what it did, in the transcript and in a refusal. `restart` is
;; gone with it: it was a stop+start pretending to be one atomic step, tearing
;; down the REPL the turn was standing on, and when the relaunch hung the caller
;; was left with NO repl and no usable error. Stop and start are two decisions.
(defn- opts-id [opts] (or (get opts "id") (get opts "repl_id")))

(defn- repl-call
  "Split a lifecycle call into `[id language opts]`.

   A leading string is a REPL *id* when it names one — a live resource of THIS
   session, or a string no language could be spelled as (`nrepl:~/proj`), so a
   stale id still takes the by-id path and answers `not-found` instead of acting
   on some pack's REPL. Otherwise it is the language. An `id`/`repl_id` inside
   the options reads the same way, so a pack's own REPL LABEL (`{\"id\": \"worker\"}`)
   still reaches the pack.

   What follows is ONE options MAP. A bare string where the map belongs is
   REFUSED: `repl_start(\"clojure\", \"extensions/foo\")` used to be swallowed and
   start a REPL at the workspace ROOT instead."
  [env args]
  (let [live-ids
        (into #{} (map #(str (get % "id"))) (vis/list-resources (:session-id env)))

        id?
        (fn [x]
          (and (string? x) (or (not (language-like? x)) (contains? live-ids x))))

        [lead more]
        (if (seq args) [(first args) (next args)] [nil nil])

        lead-id
        (when (id? lead) lead)

        language
        (when (and (nil? lead-id) (language-like? lead)) lead)

        more
        (if (or lead-id language) more (seq args))

        opts
        (first more)]

    (when-not (or (nil? opts) (map? opts))
      (throw (ex-info (str "A REPL lifecycle call is (language?, {options}); " (pr-str opts)
                           " is not an options map — name what it selects:"
                           " {\"cwd\": ...}, {\"id\": ...}, {\"port\": ...}.")
                      {:type :language-surface/bad-args
                       :got args
                       :examples ["repl_start('clojure', {'cwd': 'extensions/foo'})"
                                  "repl_status('python')" "repl_stop('nrepl:~/proj')"]})))
    (when (next more)
      (throw (ex-info "A REPL lifecycle call takes at most (language?, {options})."
                      {:type :language-surface/bad-args :got args})))
    (let [opts (directory->cwd (or opts {}))]
      [(or lead-id
           (let [oid (opts-id opts)]
             (when (id? oid) oid))) language opts])))

(defn- dispatch-repl!
  "Run the active pack's REPL-lifecycle handler for `op` (start/status/stop/connect)."
  [env op language opts]
  (let [dispatch-opts
        (cond-> opts
          language
          (assoc "language" language))

        handler
        (choose-handler env :start-repl-fn dispatch-opts)]

    ;; Refresh from the live env at the process boundary. This also repairs the
    ;; registry after a process-jail namespace reload without weakening
    ;; fail-closed handling for missing session identity or policy.
    (when (= "start" op) (vis/prepare-session-jail! env))
    ((:handler handler) env op opts)))

(defn- dispatch-repl-call!
  [env op args]
  (let [[_ language opts] (repl-call env args)]
    (dispatch-repl! env op language opts)))

(defn- repl-resources
  "Every live REPL resource of THIS session, optionally narrowed to one language.
   A pack answers for ONE directory, so a REPL under another `cwd` — or a
   shadow-cljs attachment riding beside the JVM one under its own id — exists
   only here."
  [env language]
  (let [lang (normalize-language language)]
    ;; `list-resources` returns string-keyed DATA maps with string enum VALUES
    ;; ("kind" "nrepl", "status" "up"), so filter on strings.
    (->> (vis/list-resources (:session-id env))
         (filter #(let [kind (str (get % "kind"))] (or (= "repl" kind)
                                                       (= "nrepl" kind)
                                                       (str/ends-with? kind "repl"))))
         (filter #(or (nil? lang) (= lang (normalize-language (get % "language")))))
         vec)))

(defn repl-stop
  "Stop a REPL: `repl_stop(id)` with the exact id `repl_start`/`repl_status`
   answered with. NOTHING is required — `language` is inferred and `cwd` (or
   `project`, the SAME key) defaults to the WORKSPACE ROOT, so a bare
   `repl_stop()` ends the inferred pack's REPL there, which is rarely the one
   you meant. `build` (clojure) detaches just that shadow-cljs attachment."
  [env & args]
  (let [[id language opts] (repl-call env args)]
    (if id
      ;; By-id stop is a generic session-resource op — no pack dispatch needed,
      ;; and it works even when the owning language pack is gone.
      ;; `stop-resource!` returns an INTERNAL keyword-keyed map ({:result :stopped
      ;; :id ...}); project it to a strings-only model payload (enum value stringified
      ;; at the source) so nothing keyword crosses the boundary.
      (let [{:keys [result id message]} (vis/stop-resource! (:session-id env) id)]
        (extension/success {:result {"result" (name result)
                                     "id" (str id)
                                     ;; TOTAL: `message` is nil rather than absent, so
                                     ;; r["message"] reads on EVERY stop instead of
                                     ;; KeyErroring on the clean path.
                                     "message" message}}))
      (dispatch-repl! env "stop" language opts))))

(defn repl-status
  "Report REPL state: `repl_status(language,{cwd})` — the pack's answer for that
   project PLUS `resources`, every live REPL of this session whatever directory it
   runs in. Nothing is required; `cwd` — or `project`, the same key — chooses the
   project and defaults to the workspace root. A REPL id answers for that one REPL
   alone."
  [env & args]
  (let [[id language opts]
        (repl-call env args)

        rows
        (cond->> (repl-resources env language)
          id
          (filterv #(= (str id) (str (get % "id")))))]

    (if id
      (extension/success {:result {"id" (str id)
                                   ;; TOTAL: an id nothing answers to reads
                                   ;; "unknown" instead of an absent key.
                                   "status" (or (some-> (first rows)
                                                        (get "status"))
                                                "unknown")
                                   "resources" rows}})
      (update (dispatch-repl! env "status" language opts)
              :result
              (fn [result]
                (if (map? result) (assoc result "resources" rows) result))))))

;; Call-selection helpers — what a CALL asked for, read off its input map. A pack
;; reports what it RAN; only the call knows what was SELECTED, so `run_tests`
;; stamps `test-target` into the result metadata.

(defn- input-list
  "Comma-joined non-blank entries of list-ish call key `k`, else nil — how a
   multi-value selection reads on one line. A bare string is ONE entry, never a
   sequence of characters."
  [input k]
  (let [xs (let [v (get input k)]
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

;; Every spelling a call can select WITH: singular `path` or plural `paths` in every
;; language, plus the namespace / var vocabulary the clojure pack resolves
;; (`clojure -M:test`'s own `--namespace` / `--var`). A selection rendered as "full
;; suite" would make a one-namespace run read like the whole workspace.
(def ^:private selection-keys
  ["path" "paths" "ns" "nses" "namespace" "namespaces" "var" "vars" "only"])

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
  "Format through a pack: `format_code(language,arg)`; omit `language` only for paths-based inference. Source/`{\"code\":...}` returns changed + char-delta, never text. `{\"paths\":[...]}` (always a list) recursively formats files/dirs in place and returns per-file changes, never text. Omit code/paths for default source paths recursively. python also takes ruff's own `line_length` and `config`."
  [env & args]
  (dispatch! env :format-fn args))

(defn lint-code
  "Lint through a pack: `lint_code(language,arg)`; omit `language` only for file/workspace inference. Source/`{\"code\":...}` lints a snippet; `{\"paths\":[...]}` (always a list) lints disk. Omit code/paths for defaults. Returns findings and severity counts. python also takes ruff's own `select`, `ignore`, `line_length` and `config` for this call."
  [env & args]
  (dispatch! env :lint-fn args))

(defn run-tests
  "Run through a pack: `run_tests(language,arg)`. NOTHING is required: omit `arg` to
   run every test, and `language` is inferred from the paths and the workspace. `arg`
   is a path string or a map: `paths` (files, directories, or `<path>::<test-name>`
   node ids — the selector every language shares; `::<test-name>` alone finds that
   test wherever it lives) selects; clojure ALSO takes `ns` / `nses` (a namespace
   name, or `ns/var` for one test) and resolves it the same way, and runs
   `*_test.cljs` through the project's own shadow-cljs build (`build` names which one
   when several could); `include` / `exclude` narrow by metadata tag; `cwd` — or
   `project`, the SAME key — chooses the project and defaults to the WORKSPACE ROOT,
   which is where a relative `paths` entry resolves; `runner` picks the python backend
   (`\"project\"` for the project interpreter's own pytest, else the hermetic sandbox).
   `aliases` (clojure) adds EXTRA deps.edn aliases to that clean-JVM command —
   `clojure -M:test:<name>`, `:test` always kept — for a project whose tests need
   more than `:test` declares; they cannot reach an already-running REPL, and a
   run that reused one says so on its note.
   List selectors stay lists, even one."
  [env & args]
  (let [started-at
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
                   (update envelope
                           :result
                           (fn [result]
                             (let [completed (contract/complete-test-result (:language handler)
                                                                            result)]
                               (if (map? completed)
                                 (assoc completed
                                   "target" (or (get result "target") target)
                                   "ms" (quot (- (System/nanoTime) started-at) 1000000))
                                 completed))))
                   envelope)))))

(defn repl-eval
  "Eval in an already-running project REPL: `repl_eval(language,{code,cwd,id,timeout_ms})`.
   `code` is the one REQUIRED key; `language` may lead the call; `cwd` — or `project`, the
   same key — chooses the project (default the workspace root), `id`/`repl_id` picks one of
    several REPLs and `timeout_ms` its budget. Clojure also reads `ns` (the namespace the
    form is read in) and `port`/`host`, which dial an nREPL directly instead of a REPL this
    session owns."
  [env & args]
  (dispatch! env :repl-eval-fn args))

(defn repl-start
  "Start a language REPL resource: `repl_start(language,{cwd,id,aliases,env})`.
   NOTHING is required: `language` is inferred and `cwd` — or `project`, the same
   key — chooses the project, defaulting to the WORKSPACE ROOT. `id` LABELS a
   second REPL in one project (python/bun; a clojure id is derived from its
   `cwd`), `aliases` is clojure-only and `env` belongs to THIS REPL. Neither
   `port` nor `build` starts anything here — attaching to a process already
   running is `repl_connect`. There is no restart: `repl_stop`, then `repl_start`."
  [env & args]
  (dispatch-repl-call! env "start" args))

(defn connect-repl
  "Attach to an external running REPL: `repl_connect(language,{port|build,host?,cwd?})`.
   CLOJURE only — Vis owns the python and bun runtimes and those packs refuse to
   attach. `port` is REQUIRED unless `build` names a shadow-cljs build — that
   attaches to the project's `shadow-cljs watch` and selects it, making eval
   ClojureScript; `host` defaults to localhost and `cwd` — or `project`, the same
   key — says where it lives. Registers it for eval, tests, and context but never
   owns or kills its process; stop only detaches."
  [env & args]
  (dispatch-repl-call! env "connect" args))

(def format-symbol
  (vis/symbol
    #'format-code
    {:symbol 'format_code
     :result
     (str
       "String-keyed `op` result. Code/file: `changed` plus optional `chars,path,formatter,repaired`; "
       "batch: `files,formatters`. No source text.")
     :description
     (str
       "Format through the active pack — `format_code({\"path\": \"src/a.clj\"})`, or "
       "`format_code(\"python\", {\"paths\": [\"src\"]})`. `language` leads the call and is optional: it is "
       "inferred from paths and workspace. `cwd`, `root`, and `project` select the project directory. `path` "
       "formats one file or directory; `paths` formats a list recursively, in place, and answers per-file "
       "changes. `code` formats one snippet and answers `changed` + char delta, NEVER the text. Omit "
       "all selectors to format the pack's default source paths. python also takes ruff's own "
       "knobs: `line_length` overrides the discovered config, `config` pins one file.")
     ;; NAME(language, {payload}) — optional leading `language`, the rest a
     ;; pure options dict (always emitted so the payload stays a map).
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "cwd" :note "or `root`/`project`; default workspace ROOT"}
              {:name "root" :note "alias of `cwd` project directory"}
              {:name "path" :note "one file or directory"}
              {:name "paths" :note "list of files or directories"}
              {:name "code" :note "one snippet instead of `paths`"}
              {:name "line_length" :note "python — overrides ruff config"}
              {:name "config" :note "python — pin one ruff config"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation
     :presenter :format}))

(def lint-symbol
  (vis/symbol
    #'lint-code
    {:symbol 'lint_code
     :result
     (str
       "String-keyed `op` object: `language`, severity counts, `files,findings,providers`; "
       "stdin adds `snippet`, paths add `targets`. Findings use `file,row,col,level,type,message` "
       "and optional `provider`.")
     :description
     (str
       "Lint through the active pack, editing nothing — `lint_code({\"path\": \"src/a.clj\"})`, or "
       "`lint_code(\"python\", {\"paths\": [\"src\"]})`. `language` leads the call and is optional: it is "
       "inferred from paths and workspace. `cwd`, `root`, and `project` select the project directory. `path` "
       "lints one disk target, `paths` lints a list, `code` lints one snippet, and omitting all selectors "
       "lints the pack's defaults across the workspace. Answers findings plus severity counts. python also "
       "takes ruff's own knobs for this call: `select`/`ignore` choose rules, `line_length` "
       "overrides the discovered config, `config` pins one file.")
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "cwd" :note "or `root`/`project`; default workspace ROOT"}
              {:name "root" :note "alias of `cwd` project directory"}
              {:name "path" :note "one file or directory"}
              {:name "paths" :note "list of files or directories"}
              {:name "code" :note "one snippet instead of `paths`"}
              {:name "select" :note "python — ruff rules to keep"}
              {:name "ignore" :note "python — ruff rules to drop"}
              {:name "line_length" :note "python — overrides ruff config"}
              {:name "config" :note "python — pin one ruff config"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :observation
     :presenter :lint}))

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
       "open. `output` is the runner's own text; `runner`, `ns`, `target`, `framework`, `mode`, `ms` "
       "say what ran and how. A REPL that could not serve answers `repl_wedged`, `repl_unusable`, "
       "`recovered` and a `hint`; `timed_out`, `error`, `exit` carry the rest.")
     :description
     (str
       "Run the pack's tests — `run_tests({\"path\": \"test/foo_test.clj\"})`, or "
       "`run_tests(\"python\", {\"paths\": [...]})`. `language` leads the call and is optional "
       "(inferred from the paths and the workspace). Prefer the smallest target: `path` selects one and "
       "`paths` selects a list; each target is a file, a directory, or `<path>::<test-name>` for a "
       "single test; clojure also takes `ns` — a namespace name, or `ns/var` for one test. "
       "`include`/`exclude` (clojure) narrow by metadata tag; `cwd`, `root`, and `project` are the "
       "SAME directory and default to the WORKSPACE ROOT, which is where a "
       "relative `paths` entry resolves; `runner` (python) selects `project` — the "
       "interpreter's own pytest "
       "— over the hermetic sandbox. `aliases` (clojure) ADDS deps.edn aliases to the clean-JVM "
       "`clojure -M:test:<name>` when `:test` alone does not carry the classpath the tests need; "
       "a run that REUSED a REPL cannot apply them and says so. "
       "NOTHING is required: omit `path` and `paths` to run everything.")
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "path" :note "one test target"}
              {:name "paths" :note "list; omit to run every test"}
              {:name "ns" :note "clojure — namespace, or `ns/var`"}
              {:name "include" :note "clojure — keep these metadata tags"}
              {:name "exclude" :note "clojure — drop these metadata tags"}
              {:name "cwd" :note "or `root`/`project`; default workspace ROOT"}
              {:name "root" :note "alias of `cwd` project directory"}
              {:name "build" :note "clojure — shadow-cljs build id"}
              {:name "aliases" :note "clojure — EXTRA deps.edn aliases"}
              {:name "runner" :note "python — \"project\" for its pytest"}]
     :call {:lead-opt "language" :rest :always}
     ;; run_tests can exceed the generic Python eval watchdog; dispatch it
     ;; directly in Clojure so the language pack's own timeout budget wins.
     :inject-env? true
     :tag :mutation
     :presenter :tests}))

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
       "`language` may lead the call; nothing else is: `id`/`repl_id` picks one of several "
       "REPLs, `cwd` — or `project`, the SAME key — its project directory (default the "
       "WORKSPACE ROOT, not the project you meant), "
       "`timeout_ms` its budget. Lifecycle is `repl_start` / `repl_status` / `repl_stop`. A REPL attached to a "
       "shadow-cljs `build` evaluates ClojureScript inside that build's JS runtime and the result "
       "names the `build` it landed in. Clojure also reads `ns` — the namespace the form is "
       "read in — and `port`/`host`, which dial an nREPL DIRECTLY instead of a REPL this "
       "session owns.")
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "code" :required? true} {:name "id" :note "or `repl_id`, among several"}
              {:name "cwd" :note "or `project`; default workspace ROOT"}
              {:name "ns" :note "clojure — read the form here"}
              {:name "port" :note "clojure — dial that nREPL directly"}
              {:name "host" :note "clojure — with `port`; default localhost"}
              {:name "timeout_ms" :note "this eval's budget"}]
     :call {:lead-opt "language" :rest :always}
     :presenter :repl
     ;; repl_eval's own `timeout_ms` can exceed the generic Python eval
     ;; watchdog (DEFAULT_EVAL_TIMEOUT_MS, five minutes); dispatch it directly in
     ;; Clojure so the language pack's own timeout budget wins (parity with
     ;; run_tests above).
     :inject-env? true
     :tag :mutation}))

(def repl-start-symbol
  (vis/symbol
    #'repl-start
    {:symbol 'repl_start
     :result
     (str
       "String-keyed result stamped with `op` — the SAME shape in every language: `result` "
       "(`started` | `already-running` | `starting` | `failed` | `no-launcher`), `id`, `cwd`, "
       "`status`, plus `running,port,pid,cmd,tool,aliases,env,log,message` when known — a failed "
       "start adds `exit` and `log_tail`, the same two keys whatever launched it — and "
       "`build,target,dialect,runtime` for a shadow-cljs attachment.")
     :description
     (str
       "Start a project REPL — `repl_start(\"clojure\")`, or "
       "`repl_start({\"language\": \"python\", \"cwd\": \"extensions/foo\"})`. NOTHING is required: "
       "`language` is inferred from the workspace and named only when several packs match. "
       "`cwd` chooses the project — spell it `cwd` or `project`, they are the SAME key — and "
       "omitting it starts the REPL at the WORKSPACE ROOT, not at the project you meant "
       "(the typescript pack refuses a bare start at a monorepo root for exactly that reason). "
       "Neither `port` nor `build` starts anything here: attaching to a process already running "
       "is `repl_connect`. "
       "`aliases` (clojure) ADDS deps.edn aliases to the `:dev` + `:test` every managed REPL "
       "already boots with — name the extra ones a project needs on its classpath; they never "
       "replace the defaults, and a live REPL keeps the aliases it started with. "
       "Nothing lists live REPLs for you: `repl_status` is the only answer — reuse `up`, "
       "recheck `starting`, start when absent/down/failed. A LIVE REPL IS REUSED, never replaced: "
       "a second start answers `already-running` in every language, because that process' state is "
       "the work you are standing on. There is NO restart: a wedged REPL is `repl_stop` then "
       "`repl_start`. "
       "`env` carries THIS REPL's own variables over the project's — a literal for a switch, a "
       "source map ({\"keychain\"|\"env\"|\"dotenv\"|\"command\": …}) for a secret, null to unset "
       "— and that env BELONGS to the REPL: a start naming a different one is refused by the keys "
       "that differ, since there is no restart. `repl_eval` never takes `env`: a live process' "
       "environment is its own.")
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "cwd" :note "or `project`; default workspace ROOT"}
              {:name "id" :note "python/bun — label a second REPL"}
              {:name "aliases" :note "clojure — EXTRA deps.edn aliases"}
              {:name "env" :note "THIS REPL's variables, over the project's"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def repl-status-symbol
  (vis/symbol
    #'repl-status
    {:symbol 'repl_status
     :result
     (str
       "String-keyed and stamped with `op`. The project REPL answers the same keys in every "
       "language — `result,id,cwd,status` — and a running one adds `pid,cmd,running,port,build` and its `env` key "
       "NAMES (never values). `resources` lists EVERY live REPL of this session — `id`, `kind`, "
       "`language`, `status`, `label` — including ones under another `cwd`. Asking by id answers "
       "`id,status,resources`, `status` `unknown` when nothing has that id.")
     :description
     (str
       "State of the project's REPL — `repl_status(\"clojure\")`, or "
       "`repl_status({\"language\": \"python\", \"cwd\": \"extensions/foo\"})`. Nothing else lists "
       "live REPLs: this is the only answer — reuse `up`, recheck `starting`, `repl_start` when "
       "absent/down/failed. `cwd` — or `project`, the same key — chooses the project; "
       "`resources` beside it names every REPL this "
       "session owns, so one running under another directory is never invisible. An `id` — leading "
       "or in the options — reports that REPL alone.")
     :params [{:name "language" :note "inferred; name it when ambiguous"}
              {:name "cwd" :note "or `project`; default workspace ROOT"}
              {:name "id" :note "reports that REPL alone"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :observation}))

(def connect-repl-symbol
  (vis/symbol
    #'connect-repl
    {:symbol 'repl_connect
     :result (str "String-keyed and stamped with `op`: `result,id,cwd,status` plus "
                  "`running,port,host,external,message` when known, and "
                  "`build,target,dialect,runtime` for a shadow-cljs build.")
     :description
     (str
       "Attach an external running REPL — `repl_connect(\"clojure\", {\"port\": 56428})`. CLOJURE only: "
       "Vis owns the python and bun runtimes, and those packs refuse to attach. `port` names "
       "it — REQUIRED unless `build` does — `host` (default localhost) and `cwd` (or `project`, "
       "the same key) say where it lives. `build` instead attaches to the "
       "`shadow-cljs watch` under `cwd` — it publishes its own port, so `port` is optional there — and "
       "selects that build, making `repl_eval` ClojureScript in its JS runtime. It rides BESIDE the "
       "managed JVM REPL for the same `cwd`, each under its own id (`nrepl:~/proj` and "
       "`nrepl:~/proj#app`). Vis registers it for eval, tests and context but never owns or kills it, "
       "so stopping it only detaches.")
     :params [{:name "language" :note "clojure — the only pack that attaches"}
              {:name "port" :required? true :note "or `build` names one"}
              {:name "host" :note "default localhost"}
              {:name "cwd" :note "or `project`; default workspace ROOT"}
              {:name "build" :note "clojure — shadow-cljs build attached"}]
     :call {:lead-opt "language" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def repl-stop-symbol
  (vis/symbol
    #'repl-stop
    {:symbol 'repl_stop
     :result
     (str "String-keyed `{result, id, cwd, status}` stamped with `op` — `result` is `stopped`, "
          "`not-managed` or `detached`, and an external REPL is only detached.")
     :description
     (str
       "Stop a REPL after verification, so nothing is left running — `repl_stop(id)` with the exact "
       "id `repl_start`/`repl_status` answered with. NOTHING is required: `language` is inferred and "
       "`cwd` (or `project`, the same key) defaults to the WORKSPACE ROOT, so a bare `repl_stop()` "
       "ends the inferred pack's REPL there — rarely the one you meant. Name it: `repl_stop(id)`, or "
       "`repl_stop(\"clojure\", {\"cwd\": \"…\"})` for the pack's REPL under a directory. A REPL "
       "attached by `repl_connect` is only detached, never killed. `build` detaches just that "
       "shadow-cljs attachment.")
     ;; repl_stop(id) — one positional id, or the language-led form the other
     ;; lifecycle verbs take.
     :params [{:name "id" :note "the exact id `repl_status` answered"}
              {:name "language" :note "inferred; name it when ambiguous"}
              {:name "cwd" :note "or `project`; default workspace ROOT"}
              {:name "build" :note "clojure — detaches that attachment only"}]
     :call {:lead-opt "id" :rest :always}
     :inject-env? true
     :tag :mutation}))

(def symbols
  [format-symbol lint-symbol test-symbol repl-eval-symbol repl-start-symbol repl-status-symbol
   connect-repl-symbol repl-stop-symbol])

(defn prompt
  "The language-facade reference: the AUTO capability matrix (active packs only)
   + the bare facade verbs. nil when no language pack is active, so a non-coding
   or single-language workspace carries nothing extra. Each verb's own docstring
   holds its args/return; `language` is explicit only when several packs match."
  [env]
  (capability-matrix env))
