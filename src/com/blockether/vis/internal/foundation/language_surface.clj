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
            [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [com.blockether.vis.internal.strutil :as strutil]))

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
     LANGUAGE TOOLS (active packs; call via the facade, language first):
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
          "\n  clojure REPL tests reuse the managed REPL and execute its already-loaded Vars;"
          " they do NOT reload namespaces automatically. After edits `(require 'my.ns :reload)`"
          " every changed production and test namespace, or tests may exercise stale code;"
          " prefer a FRESH REPL (stop, then start) over `:reload-all`."
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
     ;; session binding immediately before a handler that may spawn a child. Clojure
     ;; repl_eval can auto-start its managed nREPL when none is running.
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

;; =============================================================================
;; Native op-card renderers — `:result` → `{:summary :body}`. The result arrives
;; string-keyed snake_case (strings-only boundary), the injected env gone.
;; Renderers read string keys but still RETURN the keyword `{:summary :body}` IR.
;; Defensive: language results vary per pack, so every access is nil-safe.
;; =============================================================================

(defn- fence [label s] (when (seq (str s)) (str (when label (str label ":\n")) (strutil/fenced s))))

(defn- md-cell
  "Sanitize a value for ONE markdown table cell: escape `\\`/`|` and collapse every
   whitespace run (incl. newlines) to a single space, so a multi-line form or a value
   carrying a pipe can never break the row's column structure."
  [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "|" "\\|")
      (str/replace #"\s+" " ")
      str/trim))

(defn- md-table
  "A GitHub-flavored markdown table from `headers` (column labels) and `rows` (each a
   same-arity seq of cells). nil when there are no rows, so an empty result drops out
   of the card body; the TUI renders the markdown as a boxed grid."
  [headers rows]
  (when (seq rows)
    (let
      [line (fn [cells]
              (str "| " (str/join " | " cells) " |"))]
      (str/join "\n"
                (concat [(line headers) (line (repeat (count headers) "---"))]
                        (map (fn [row]
                               (line (map md-cell row)))
                             rows))))))

(defn- render-format-result
  "format_code → `` `path` (changed) `` when writing a file (the FORMAT_CODE
   badge already names the tool), a per-file roll-up when several `paths` were
   formatted, else the formatted text as a code block."
  [r]
  (if-let [files (get r "files")]
    (let
      [n (count files)
       changed (or (get r "changed") 0)
       by-cwd (get r "by-cwd")]

      {:summary (str n " file" (when (not= 1 n) "s") " — " changed " changed")
       :body (fence
               nil
               (if (map? by-cwd)
                 ;; grouped: each directory prefix written ONCE, its files indented
                 (str/join
                   "\n"
                   (mapcat (fn [[dir entries]]
                             (cons (str dir "/")
                                   (for [[base v] (sort-by key entries)]
                                     (str "  " base
                                          " " (if (get v "changed") "(changed)" "(no change)")))))
                           (sort-by key by-cwd)))
                 (str/join
                   "\n"
                   (for [f files]
                     (str (get f "path") " " (if (get f "changed") "(changed)" "(no change)"))))))})
    (let
      [changed (get r "changed")
       note (if changed "(changed)" "(no change)")
       delta (get r "chars")
       mag (when (and changed (number? delta) (not (zero? (long delta))))
             (str " (" (if (pos? (long delta)) "+" "-") (Math/abs (long delta)) " chars)"))
       label (str note mag)]

      (if-let [path (get r "path")]
        {:summary (str "`" path "` " label)}
        {:summary label}))))

(defn- findings->table
  "Lint `findings` → a markdown table the LINT_CODE card renders as a boxed grid:
   one row per finding with its `file`, `row:col`, `level`, `provider`, and
   `message`. nil when there are no findings."
  [findings]
  (md-table ["file" "at" "level" "provider" "message"]
            (for [f findings]
              [(str (get f "file")) (str (get f "row") ":" (get f "col")) (str (get f "level"))
               (str (get f "provider")) (str (get f "message"))])))

(defn- render-lint-result
  "lint_code → `` `path` — clean `` / `N targets — E errors, W warnings` headline
   (the LINT_CODE badge already names the tool, and the headline names the linted
   target(s) — the file/dir path(s) when given, else `snippet` for a stdin lint or
   `N files` for a bare workspace lint). A stdin lint also renders the exact snippet
   in a fenced body, followed by the findings table when present."
  [r]
  (let
    [errors
     (long (or (get r "error") 0))

     warnings
     (long (or (get r "warning") 0))

     infos
     (long (or (get r "info") 0))

     findings
     (get r "findings")

     snippet
     (get r "snippet")

     clean?
     (and (zero? errors) (zero? warnings) (zero? infos))

     targets
     (get r "targets")

     n
     (get r "files")

     head
     (cond (seq targets) (if (= 1 (count targets))
                           (str "`" (first targets)
                                "`" (when (and n (> (long n) 1)) (str " (" n " files)")))
                           (str (count targets)
                                " targets"
                                (when (and n (> (long n) (count targets))) (str " (" n " files)"))))
           (some? snippet) "snippet"
           n (if (= 1 n) "snippet" (str n " files")))

     body
     (not-empty (str/join "\n\n"
                          (keep identity [(fence "snippet" snippet) (findings->table findings)])))]

    {:summary (not-empty (when head
                           (str head
                                (if clean?
                                  " — clean"
                                  (str " — "
                                       errors
                                       " error"
                                       (when (not= 1 errors) "s")
                                       ", "
                                       warnings
                                       " warning"
                                       (when (not= 1 warnings) "s")
                                       (when (pos? infos) (str ", " infos " info")))))))
     :body body}))

(defn- failures->table
  "Structured test `failures` → a markdown table the RUN_TESTS card renders as a
   boxed grid centred on the expectation-vs-reality comparison: one row per fault
   with `ns/test` and its `file:line`, then `expected`/`actual` columns whenever a
   fault carries them. The generic `message` (e.g. lazytest's `Expectation failed`)
   only earns its own column when some fault leans on it as its SOLE signal — an
   error or plain assertion with no expected/actual pair — so a clean expectation
   failure reads purely as expected vs actual. nil when there are no faults, so a
   passing run stays body-less."
  [fails]
  (let
    [keep?
     (fn [x]
       (and x (not (str/blank? (str x))) (not= "nil" (str x))))

     exp?
     (boolean (some #(keep? (get % "expected")) fails))

     act?
     (boolean (some #(keep? (get % "actual")) fails))

     ;; The message is worth a column only when some fault has NO expected/actual
     ;; pair, so it is that row's only signal (a thrown error, a bare assertion).
     msg?
     (boolean (some #(and (keep? (get % "message"))
                          (not (keep? (get % "expected")))
                          (not (keep? (get % "actual"))))
                    fails))

     headers
     (cond-> ["test" "at"]
       msg?
       (conj "message")

       exp?
       (conj "expected")

       act?
       (conj "actual"))

     rows
     (for [f fails]
       (let
         [nm (str (get f "ns") (when (keep? (get f "test")) (str "/" (get f "test"))))
          at (if (keep? (get f "file"))
               (str (get f "file") (when (keep? (get f "line")) (str ":" (get f "line"))))
               "")]

         (cond-> [nm at]
           msg?
           (conj (str (get f "message")))

           exp?
           (conj (str (get f "expected")))

           act?
           (conj (str (get f "actual"))))))]

    (md-table headers rows)))

(defn- render-test-result
  "run_tests → `<ns> — pass/total (Nms)` headline (the RUN_TESTS badge already
   names the tool, so no redundant `tests` word or success glyph — only a
   leading `✗` flags a failure). Many namespaces collapse to `<first> +N more`
   so the headline stays one tidy line, and the CLI-fallback `:note` rides
   after a ` · ` instead of being fused into the run detail; the run output on
   failure, or the error text when the run itself could not produce a result. A
   failing run NEVER renders blank — with neither output nor error we surface
   the raw result so the user always sees *something* went wrong, never an
   empty card."
  [r]
  (let
    [pass
     (get r "pass")

     fail
     (get r "fail")

     total
     (get r "total")

     error
     (get r "error")

     ok
     (and (not error)
          (cond (number? fail) (zero? (long fail))
                (some? (get r "is_pass")) (boolean (get r "is_pass")) ; CLI fallback: exit-code verdict
                :else (boolean (get r "pass"))))

     parts
     (some-> (get r "ns")
             str
             str/trim
             not-empty
             (str/split #"\s+"))

     ns-disp
     (cond (empty? parts) nil
           (> (count parts) 1) (str (first parts) " +" (dec (count parts)) " more")
           :else (first parts))

     detail
     (or (not-empty (str (get r "output")))
         (not-empty (str error))
         (when-not ok (str "no test result returned — " (pr-str r))))]

    {:summary (str (when-not ok "✗ ")
                   ns-disp
                   (when total
                     (str " — " pass
                          "/" total
                          " passed" (when (and (number? fail) (pos? (long fail)))
                                      (str ", " fail " failed"))))
                   (when (and (not ok) (not total)) " — error")
                   (when-let [ms (get r "ms")]
                     (str " (" ms "ms)"))
                   (when (get r "note") (str " · " (get r "note"))))
     :body (when-not ok
             (or (some-> (get r "failures")
                         seq
                         failures->table)
                 (fence nil detail)))}))

(defn- short-error
  "First line of an error headline, trimmed to its class before the `:` — e.g.
   `NullPointerException: null` → `NullPointerException`. Capped so a long
   message never blows out the one-line summary badge."
  [s]
  (let
    [head
     (-> (str s)
         str/split-lines
         first
         (or "")
         str/trim)

     cls
     (-> head
         (str/split #":" 2)
         first
         str/trim)]

    (subs cls 0 (min 60 (count cls)))))

(def ^:private repl-form-inline-max
  "Display-width budget for the evaluated FORM on the collapsed chip. A form wider
   than this (or any multi-line form) is too long to ride inline, so it's clipped
   on the chip and promoted to its own FORM section when expanded."
  56)

(defn- one-line
  "Collapse `s` to a single trimmed line — every run of whitespace (incl. newlines)
   becomes one space. nil/blank → nil."
  [s]
  (not-empty (str/trim (str/replace (str s) #"\s+" " "))))

(defn- clip-chip
  "Clip `s` to `n` display chars with a trailing ellipsis, so one long form or value
   never blows out the single-line collapsed summary."
  [s n]
  (let [s (str s)]
    (if (> (count s) (long n)) (str (subs s 0 (max 0 (dec (long n)))) "…") s)))

(defn- sect
  "One labeled body SECTION — a bold uppercase header over a fenced monospace block,
   the shape the collapsed/expanded repl_eval card stacks (RESULT / STDOUT / …). nil
   when there's nothing to show, so an empty section drops out of the join."
  ([label s] (sect label s nil))
  ([label s lang]
   (let [s (str/trimr (str s))]
     (when (seq s) (str "**" label "**\n" (strutil/fenced s lang))))))

(defn- render-repl-eval-result
  "repl_eval → a collapsed/expanded op-card modeled on the GIT band (the REPL badge
   names the tool). The COLLAPSED chip carries the evaluated FORM (clipped) plus a
   value/error/timeout PREVIEW so a run reads at a glance — `(+ 1 1)  ⇒ 2`, `(/ 1 0)  ✗
   ArithmeticException`, `(Thread/sleep 999999)  ⧖ timed out after 30000ms`. EXPANDED,
   the body stacks labeled sections, each fenced and separated by one blank line:
     - FORM    — the evaluated form; shown when multi-line / too wide to sit on the
                 chip, and ALWAYS on a timeout so the timed-out code is visible;
     - RESULT  — the non-nil value; ERROR replaces it on failure;
     - STDOUT  — :out when non-blank;
     - STDERR  — :err when non-blank;
     - TIMEOUT — a note (with the deadline in ms) when the eval blew the deadline.
   :error_message/:trace/:error_data come enriched from the nREPL client; we fall
   back to the raw :err one-liner when no structured trace was captured. A TIMEOUT is
   detected from a timed_out flag / a timeout status and renders like a first-class
   outcome (its own preview + FORM + any partial stdout/stderr)."
  [r]
  (let
    [code
     (not-empty (str (get r "code")))

     value
     (get r "value")

     out
     (get r "out")

     err
     (get r "err")

     emsg
     (not-empty (str (get r "error_message")))

     trace
     (get r "trace")

     edata
     (get r "error_data")

     timed-out?
     (boolean (or (get r "timed_out") (some #{"timeout"} (get r "status"))))

     ms
     (get r "ms")

     error?
     (boolean (or emsg
                  (not-empty (str (get r "ex")))
                  (not-empty (str (get r "root_ex")))
                  (some #{"eval-error"} (get r "status"))))

     long-form?
     (boolean (and code
                   (or (str/includes? code "\n") (> (count code) (long repl-form-inline-max)))))

     ;; The form on the chip: single-lined + clipped. Short → it's the whole
     ;; story; long → it's a teaser and the full form leads the expanded body.
     form-chip
     (some-> code
             one-line
             (clip-chip repl-form-inline-max))

     value-preview
     (or (one-line value) "nil")

     show-result?
     (not= "nil" value-preview)

     preview
     (cond timed-out? (str "⧖ timed out" (when ms (str " after " ms "ms")))
           error? (str "✗ " (if emsg (short-error emsg) "error"))
           :else (str "⇒ " (clip-chip value-preview repl-form-inline-max)))

     summary
     (not-empty (str (when form-chip (str form-chip "  ")) preview))

     error-body
     (str/join "\n"
               (remove str/blank?
                 [(or emsg (str err)) (when (seq trace) (str/join "\n" trace))
                  (when (seq (str edata)) (str "ex-data: " edata))]))

     ;; Fixed section order; each gate matches the design. ERROR stands in for
     ;; RESULT on failure and sits LAST, after any captured stdout. On a TIMEOUT
     ;; the FORM is ALWAYS shown (so the timed-out code is visible), followed by
     ;; whatever partial STDOUT/STDERR was captured and a closing TIMEOUT note.
     sections
     (cond timed-out? [(sect "FORM" code "clojure") (sect "STDOUT" out) (sect "STDERR" err)
                       (sect "TIMEOUT"
                             (str "Evaluation timed out"
                                  (when ms (str " after " ms "ms"))
                                  ". The form was still running when the deadline was reached."))]
           error? [(when long-form? (sect "FORM" code "clojure")) (sect "STDOUT" out)
                   (sect "ERROR" error-body)]
           :else [(when long-form? (sect "FORM" code "clojure"))
                  (when show-result? (sect "RESULT" value "clojure")) (sect "STDOUT" out)
                  (sect "STDERR" err)])

     body
     (->> sections
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) (str "\n" body))}))

(defn- render-repl-status-result
  "repl_status → `N REPLs: id (status), …`."
  [r]
  (let [res (get r "resources")]
    {:summary
     (str (count res)
          " REPL"
          (when (not= 1 (count res)) "s")
          (when (seq res)
            (str ": " (str/join ", " (map #(str (get % "id") " (" (get % "status") ")") res)))))}))

(defn- render-repl-start-result
  "repl → lifecycle headline plus startup failure/log details when present."
  [r]
  (cond (contains? r "resources") (render-repl-status-result r)
        (#{"stopped" "detached"} (get r "result")) {:summary (str (get r "result")
                                                                  (when-let [id (get r "id")]
                                                                    (str " " id)))}
        :else (let
                [status
                 (or (get r "status") "ready")

                 failed?
                 (or (= "failed" status) (= "failed" (get r "result")))

                 prefix
                 (if failed? "✗ " "")

                 summary
                 (str prefix
                      (or (get r "id") (get r "language") "")
                      " "
                      status
                      (when-let [p (get r "port")]
                        (str " :" p)))

                 log-tail
                 (get r "log_tail")

                 sections
                 [(when-let [m (get r "message")]
                    (str "MESSAGE\n" m))
                  (when-let [exit (get r "exit")]
                    (str "EXIT\n" exit))
                  (when-let [log (get r "log")]
                    (str "LOG\n" log))
                  (when-let [cmd (seq (get r "cmd"))]
                    (str "CMD\n" (str/join " " (map str cmd))))
                  (when (seq log-tail) (str "LOG TAIL\n" (str/join "\n" log-tail)))]

                 body
                 (->> sections
                      (remove nil?)
                      (str/join "\n\n"))]

                {:summary summary :body (when (seq body) (str "\n" body))})))

(defn- render-repl-stop-result
  "repl_stop → `stopped <id>`."
  [r]
  {:summary (str "stopped"
                 (when-let [id (get r "id")]
                   (str " " id)))})

(defn format-code
  "Format through a pack: `format_code(language,arg)`; omit `language` only for paths-based inference. Source/`{\"code\":...}` returns changed + char-delta, never text. `{\"paths\":[...]}` (always a list) recursively formats files/dirs in place and returns per-file changes, never text. Omit code/paths for default source paths recursively."
  [env & args]
  (dispatch! env :format-fn args))

(defn lint-code
  "Lint through a pack: `lint_code(language,arg)`; omit `language` only for file/workspace inference. Source/`{\"code\":...}` lints a snippet; `{\"paths\":[...]}` (always a list) lints disk. Omit code/paths for defaults. Returns findings and severity counts."
  [env & args]
  (dispatch! env :lint-fn args))

(defn run-tests
  "Run through a pack: `run_tests(language,arg)`. `arg` is a module string or map: `namespaces`/`paths` choose loading/discovery; `only`, `include`, `exclude`, and `filter` narrow tests; `cwd` chooses the project; `runner` picks a pack backend (python: `graalpy` default, or `project` for the project interpreter's own pytest). List selectors stay lists, even one. Omit `arg` for all tests."
  [env & args]
  ;; Park outside the generic 30s native wall. Language packs own the test budget.
  (let [started-at (System/nanoTime)]
    (extension/run-outside-tool-wall
      env
      #(dispatch!
         env
         :test-fn
         args
         (fn [handler envelope]
           ;; Language handlers return extension envelopes. Complete and time
           ;; the PUBLIC payload; metadata added beside :result gets unwrapped.
           (if (and (map? envelope) (contains? envelope :result))
             (update envelope
                     :result
                     (fn [result]
                       (let [completed (contract/complete-test-result (:language handler) result)]
                         (if (map? completed)
                           (assoc completed "ms" (quot (- (System/nanoTime) started-at) 1000000))
                           completed))))
             envelope))))))

(defn repl-eval
  "Eval in an already-running project REPL: `repl_eval(language,arg)`. Pass `language` first; `arg` may set `id`/`repl_id`, `cwd` (root default), and `timeout_ms`."
  [env & args]
  (dispatch! env :repl-eval-fn args))

(defn start-repl
  "Start a language REPL resource: `repl(language,{op,cwd,id,...})`. Pass `language` first; `op` defaults to `start`. There is no restart: `stop`, then `start`."
  [env & args]
  (dispatch-start-repl! env args))

(defn connect-repl
  "Attach to an external running REPL: `repl_connect(language,{port,host?,cwd?})`. Registers it for eval, tests, and context but never owns or kills its process; stop only detaches."
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
     :native-tool? true
     :result
     (str
       "String-keyed `op` result. Code/file: `changed` plus optional `chars,path,formatter,repaired`; "
       "batch: `files,by-cwd,formatters`. No source text.")
     :description "Format via the active pack."
     ;; NAME(language, {payload}) — optional leading `language`, the rest a
     ;; pure options dict (always emitted so the payload stays a map).
     :call {:lead-opt "language" :rest :always}
     :render render-format-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"language" {:type "string" :minLength 1}
       "code" {:type "string"}
       "paths"
       {:type "array"
        :items {:type "string" :minLength 1}
        :minItems 1
        :description
        "Directories recursively; exclusive with `code`; OMIT both for default source paths."}}
      :required []
      :additionalProperties false}
     :inject-env? true
     :tag :mutation}))

(def lint-symbol
  (vis/symbol
    #'lint-code
    {:symbol 'lint_code
     :native-tool? true
     :result
     (str
       "String-keyed `op` object: `language`, severity counts, `files,findings,providers,by-cwd`; "
       "stdin adds `snippet`, paths add `targets`. Findings use `file,row,col,level,type,message` "
       "and optional `provider`.")
     :description "Lint code/paths without edits."
     :render render-lint-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"language" {:type "string" :minLength 1}
                           "code" {:type "string" :description "Source; exclusive with `paths`."}
                           "paths" {:type "array"
                                    :items {:type "string" :minLength 1}
                                    :minItems 1
                                    :description "files/dirs; OMIT for defaults."}}
              :required []
              :additionalProperties false}
     :inject-env? true
     :tag :observation}))

(def test-symbol
  (vis/symbol
    #'run-tests
    {:symbol 'run_tests
     :native-tool? true
     :result
     (str
       "String-keyed, stamped with `op`; absent fields mean not applicable. Carries execution metadata, "
       "counts/details, output, timeout, and REPL-recovery diagnostics.")
     :description
     "Run pack tests; prefer the smallest target: `namespaces` beats `paths`, `only`/`filter`/tags narrow."
     :call {:lead-opt "language" :rest :always}
     ;; run_tests can exceed the generic Python eval watchdog; dispatch it
     ;; directly in Clojure so the language pack's own timeout budget wins.
     :handler (fn [env input]
                (run-tests env input))
     :render render-test-result
     :color-role :tool-color/test
     :schema {:type "object"
              :properties
              {"language" {:type "string" :minLength 1}
               "namespaces" {:type "array"
                             :items {:type "string" :minLength 1}
                             :description "Modules; OMIT/[] discovers all `*_test`."}
               "paths" {:type "array"
                        :items {:type "string" :minLength 1}
                        :description "Dirs/files; OMIT/[] uses root; non-empty miss errors."}
               "only" {:type "array" :items {:type "string"} :description "Qualified vars."}
               "include" {:type "array" :items {:type "string"} :description "Required tags."}
               "exclude" {:type "array" :items {:type "string"} :description "Skipped tags."}
               "cwd" {:type "string" :description "Project dir; root default."}
               "filter" {:type "string" :description "Name filter if supported."}
               "environment"
               {:type "string"
                :enum ["project"]
                :description
                "Execution environment when supported; `project` uses the project's managed toolchain and dependencies."}}
              :required ["language"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation}))

(def repl-eval-symbol
  (vis/symbol
    #'repl-eval
    {:symbol 'repl_eval
     :native-tool? true
     :result (str
               "Pack-defined string-keyed object stamped with `op`; fields may be absent. Clojure: "
               "`code,repl,value/values,out,err,status,ns,ms,timed_out,ex,root_ex`; Python/Bun: "
               "`code,ok,out,err,value,data,type,exc`. No UI `transcript` or `content`.")
     :description "Eval in an `up` project REPL; use `repl` for lifecycle."
     :call {:lead-opt "language" :rest :always}
     ;; repl_eval's own `timeout_ms` can exceed the generic Python eval
     ;; watchdog (DEFAULT_EVAL_TIMEOUT_MS, 120s); dispatch it directly in
     ;; Clojure so the language pack's own timeout budget wins (parity with
     ;; run_tests above).
     :handler (fn [env input]
                (repl-eval env input))
     :render render-repl-eval-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties {"language" {:type "string" :minLength 1}
                           "code" {:type "string" :minLength 1}
                           "id" {:type "string" :minLength 1 :description "REPL id."}
                           "cwd" {:type "string" :description "Project dir; root default."}
                           "timeout_ms"
                           {:type "integer" :minimum 1 :description "ms; default 30000."}}
              :required ["language" "code"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation}))

(def start-repl-symbol
  (vis/symbol
    #'start-repl
    {:symbol 'repl
     :native-tool? true
     :result
     (str
       "String-keyed result stamped with `op`; never a `{resources: [...]}` list. Status: Clojure "
       "`result,id,cwd,status`, Python/Bun `cwd,status`. Start/connect may add "
       "`running,port,pid,cmd,tool,aliases,external,host,log,message`; stop by id returns "
       "`{result,id,message}`.")
     :description
     (str
       "REPL lifecycle. Check `session[\"resources\"][\"repls\"][language][cwd]` first: reuse `up`, recheck "
       "`starting`, start when absent/down/failed. There is NO restart op: a wedged REPL is `stop` "
       "then `start`. `status` reports that "
       "directory's state; `stop` ends a managed REPL; `connect` attaches an external REPL by port and only detaches it.")
     :call {:lead-opt "language" :rest :always}
     :render render-repl-start-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"language" {:type "string" :minLength 1}
               "op" {:type "string"
                     :enum ["start" "connect" "stop" "status"]
                     :description "Default `start`."}
               "id" {:type "string" :minLength 1 :description "Exact id for stop."}
               "cwd" {:type "string" :minLength 1 :description "Dir (`.` default); connect key."}
               "port" {:type "integer" :description "Connect port."}
               "host" {:type "string" :description "Connect host; localhost default."}
               "aliases" {:type "array" :items {:type "string"}}}
              :required ["language"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation}))

(def connect-repl-symbol
  (vis/symbol
    #'connect-repl
    {:symbol 'repl_connect
     :native-tool? false
     :description
     "Attach an external running REPL; Vis registers it but never owns or kills it, so stop only detaches."
     :call {:lead-opt "language" :rest :always}
     :render render-repl-start-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"language" {:type "string" :minLength 1}
               "port" {:type "integer" :minimum 1 :maximum 65535 :description "External REPL port."}
               "host" {:type "string" :description "Host; localhost default."}
               "cwd" {:type "string" :description "Project dir; root default; attachment key."}}
              :required ["language" "port"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation}))

(def repl-stop-symbol
  (vis/symbol
    #'repl-stop
    {:symbol 'repl_stop
     :native-tool? false
     :description
     "After verification, stop by exact id the managed REPL you started. External REPLs are only detached, never killed."
     ;; repl_stop(id) — one positional id. (lint_code intentionally has NO
     ;; :call: its fn takes the whole input dict, so the generic form fits.)
     :call {:pos ["id"]}
     :render render-repl-stop-result
     :color-role :tool-color/delete
     :schema {:type "object"
              :properties {"id" {:type "string" :minLength 1 :description "Exact resource id."}}
              :required ["id"]
              :additionalProperties false}
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
