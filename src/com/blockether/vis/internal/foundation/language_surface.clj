(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / START_REPL dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes stable bare tool names and dispatches to the
  active handler for the requested/current language. REPL lifecycle is resource
  backed: `repl_start` creates a language-owned session resource and `repl_stop`
  stops one by id. Live REPLs also surface in the ctx `resources` block."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]))

(defn- normalize-language
  [x]
  ;; STRINGS-ONLY: dispatch on a lowercase language STRING. Registrations
  ;; declare `:language "clojure"` (a string) at the source — there is NO
  ;; colon-strip tolerance; a keyword registered here would surface as an
  ;; unmatched ":clojure" handler immediately, which is the point.
  (some-> x
          str
          str/lower-case))

(defn- env-language
  [env]
  (or (normalize-language (get-in env [:env/project :primary_language]))
      (normalize-language (get-in env [:project :primary_language]))
      (some->> (get-in env [:env/languages :languages])
               (map #(normalize-language (or (:language %) (:name %) %)))
               (remove nil?)
               first)))

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
   GAINS a language the moment its pack activates (e.g. a .py file appears). Goes
   in ctx (`session[\"language_tools\"]`) so the model can read it programmatically
   AND it always reflects the current turn."
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
     LANGUAGE TOOLS (active packs; call via the facade, language first):
       clojure : format_code · run_tests · repl_eval · repl_start
       python  : repl_eval · repl_start"
  [env]
  (when-let [data (capability-data env)]
    (str
      "LANGUAGE TOOLS (active packs; call via the facade, language first):\n"
      (str/join "\n"
                (for [[lang tools] data]
                  (str "  " lang " : " (str/join " · " tools))))
      ;; CERTAIN: name when each verb IS the tool, so it's not ambiguous.
      "\n  → To RUN or VERIFY code in a listed language you MUST use repl_eval(language, code): it executes in the PROJECT interpreter (its modules + installed deps; globals persist across calls), which your own sandbox CANNOT import — do NOT importlib/open a project file. (Pure-stdlib scratch compute may run in your own sandbox.) Run the project's tests with run_tests(language); tidy hand-written source with format_code — it accepts either a raw code string (returns a lean changed? + char-delta ack) or a {\"path\": file} map (formats that file IN PLACE and returns a LEAN ack — which file + changed? — NOT the file's text, so don't print it back). ALWAYS pass the `language` as the FIRST arg to repl_eval/run_tests/repl_start (e.g. repl_eval(\"clojure\", code)) — do NOT rely on workspace inference; being explicit avoids running in the wrong pack in a mixed repo. Only format_code/lint_code may omit it for the {\"path\": file} form (then it's inferred).")))

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
  (->> (get-in env [:env/languages :languages])
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
  [env capability args]
  (let [{:keys [opts payload]}
        (parse-language-call args)

        handler
        (choose-handler env capability opts)]

    ((:handler handler) env payload)))

(def ^:private repl-ops #{"status" "start" "stop" "restart"})

(defn- repl-op? [x] (and (string? x) (contains? repl-ops x)))

(defn- start-repl-payload
  [args]
  (let [[language more]
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
                          :op "start"
                          :opts arg}
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
          "repl_start expects (language?), (language, opts), (language, op, opts), or (language, id, op, opts)."
          {:type :language-surface/bad-args
           :got args
           :examples ["repl_start('clojure')"
                      "repl_start('clojure', {'id': 'main', 'aliases': ['dev']})"
                      "repl_start('clojure', 'status')"
                      "repl_start('clojure', 'main', 'restart', {'dir': 'extensions/foo'})"]})))))

(defn- dispatch-start-repl!
  [env args]
  (let [{:keys [language id op opts]}
        (start-repl-payload args)

        dispatch-opts
        (cond-> (coerce-opts opts)
          language
          (assoc "language" language)

          id
          (assoc "id" id))

        handler
        (choose-handler env :start-repl-fn dispatch-opts)

        opts
        (cond-> (or opts {})
          id
          (assoc "id" id))]

    ((:handler handler) env op opts)))

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
   (let [opts
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

(defn repl-stop
  "Stop a REPL by session resource id. This is the REPL-specific wrapper around resource_stop(id)."
  [env id]
  ;; `stop-resource!` returns an INTERNAL keyword-keyed map ({:result :stopped
  ;; :id ...}); project it to a strings-only model payload (enum value stringified
  ;; at the source) so nothing keyword crosses the boundary.
  (let [{:keys [result id message]} (vis/stop-resource! (:session-id env) id)]
    (extension/success {:result (cond-> {"result" (name result) "id" (str id)}
                                  message
                                  (assoc "message" message))})))

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

;; =============================================================================
;; Native op-card renderers — `:result` → `{:summary :body}`. The result arrives
;; string-keyed snake_case (strings-only boundary), the injected env gone.
;; Renderers read string keys but still RETURN the keyword `{:summary :body}` IR.
;; Defensive: language results vary per pack, so every access is nil-safe.
;; =============================================================================

(defn- fence [label s] (when (seq (str s)) (str (when label (str label ":\n")) "```\n" s "\n```")))

(defn- render-format-result
  "format_code → `` `path` (changed) `` when writing a file (the FORMAT_CODE
   badge already names the tool), a per-file roll-up when several `paths` were
   formatted, else the formatted text as a code block."
  [r]
  (if-let [files (get r "files")]
    (let [n (count files)
          changed (or (get r "changed") 0)]

      {:summary (str n " file" (when (not= 1 n) "s") " — " changed " changed")
       :body (fence nil
                    (str/join "\n"
                              (for [f files]
                                (str (get f "path")
                                     " "
                                     (if (get f "changed") "(changed)" "(no change)")))))})
    (let [changed (get r "changed")
          note (if changed "(changed)" "(no change)")
          delta (get r "chars")
          mag (when (and changed (number? delta) (not (zero? (long delta))))
                (str " (" (if (pos? (long delta)) "+" "-") (Math/abs (long delta)) " chars)"))
          label (str note mag)]

      (if-let [path (get r "path")]
        {:summary (str "`" path "` " label)}
        {:summary label}))))

(defn- render-lint-result
  "lint_code → `` `path` — clean `` / `N targets — E errors, W warnings` headline
   (the LINT_CODE badge already names the tool, and the headline names the linted
   target(s) — the file/dir path(s) when given, else `snippet` for a stdin lint or
   `N files` for a bare workspace lint); the findings (`file:row:col level message`)
   in the body."
  [r]
  (let [errors
        (long (or (get r "error") 0))

        warnings
        (long (or (get r "warning") 0))

        infos
        (long (or (get r "info") 0))

        findings
        (get r "findings")

        clean?
        (and (zero? errors) (zero? warnings) (zero? infos))

        lines
        (for [f findings]
          (str (get f "file")
               ":" (get f "row")
               ":" (get f "col")
               " " (get f "level")
               ": " (get f "message")))

        targets
        (get r "targets")

        n
        (get r "files")

        head
        (cond (= 1 (count targets)) (str "`" (first targets)
                                         "`" (when (and n (> (long n) 1)) (str " (" n " files)")))
              (seq targets) (str (count targets)
                                 " targets"
                                 (when (and n (> (long n) (count targets))) (str " (" n " files)")))
              n (if (= 1 n) "snippet" (str n " files")))]

    {:summary (not-empty (str head
                              (when head
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
     :body (when (seq lines) (fence nil (str/join "\n" lines)))}))

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
  (let [pass
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
                   (contains? r "pass?") (boolean (get r "pass?")) ; CLI fallback: exit-code verdict
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
     :body (when-not ok (fence nil detail))}))

(defn- short-error
  "First line of an error headline, trimmed to its class before the `:` — e.g.
   `NullPointerException: null` → `NullPointerException`. Capped so a long
   message never blows out the one-line summary badge."
  [s]
  (let [head
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
     (when (seq s) (str "**" label "**\n```" (or lang "") "\n" s "\n```")))))

(defn- render-repl-eval-result
  "repl_eval → a collapsed/expanded op-card modeled on the GIT band (the REPL badge
   names the tool). The COLLAPSED chip carries the evaluated FORM (clipped) plus a
   value/error PREVIEW so a run reads at a glance — `(+ 1 1)  ⇒ 2`, `(/ 1 0)  ✗
   ArithmeticException`. EXPANDED, the body stacks labeled sections, each fenced and
   separated by one blank line:
     - FORM   — only when the form is multi-line / too wide to sit on the chip;
     - RESULT — the non-nil value; ERROR replaces it on failure;
     - STDOUT — `:out` when non-blank;
     - STDERR — `:err` when non-blank (success path only; on failure the stream IS
                the stacktrace, surfaced under ERROR).
   `:error_message`/`:trace`/`:error_data` come enriched from the nREPL client; we
   fall back to the raw `:err` one-liner when no structured trace was captured."
  [r]
  (let [code
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
        (if error?
          (str "✗ " (if emsg (short-error emsg) "error"))
          (str "⇒ " (clip-chip value-preview repl-form-inline-max)))

        summary
        (not-empty (str (when form-chip (str form-chip "  ")) preview))

        error-body
        (str/join "\n"
                  (remove str/blank?
                    [(or emsg (str err)) (when (seq trace) (str/join "\n" trace))
                     (when (seq (str edata)) (str "ex-data: " edata))]))

        ;; Fixed section order; each gate matches the design. ERROR stands in for
        ;; RESULT on failure and sits LAST, after any captured stdout.
        sections
        (if error?
          [(when long-form? (sect "FORM" code "clojure")) (sect "STDOUT" out)
           (sect "ERROR" error-body)]
          [(when long-form? (sect "FORM" code "clojure"))
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
  "repl_start → lifecycle headline plus startup failure/log details when present."
  [r]
  (if (contains? r "resources")
    (render-repl-status-result r)
    (let [status
          (or (get r "status") "ready")

          failed?
          (or (= "failed" status) (= "failed" (get r "result")))

          prefix
          (if failed? "✗ REPL " "REPL ")

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
  "repl_stop → `stopped REPL <id>`."
  [r]
  {:summary (str "stopped REPL"
                 (when-let [id (get r "id")]
                   (str " " id)))})

(defn format-code
  "Format source using a language extension. Pass `language` FIRST when you know it — format_code(language, arg); it may be omitted only for the {\"path\": file} form (then inferred from the file/workspace).
   `arg` is either a raw code string / {\"code\": ...} (returns a lean changed? + char-delta ack, not the text), a {\"path\": file} map (formats that ONE file IN PLACE and returns a LEAN ack — which file + changed? — NOT the file's text, so don't print it back), or a {\"paths\": [file-or-dir …]} map (formats MANY paths IN PLACE — a DIRECTORY is walked RECURSIVELY for source files — returning a per-file changed roll-up). Omit all of code/path/paths to format the workspace's default source paths recursively. The payload is passed through to the language handler verbatim."
  [env & args]
  (dispatch! env :format-fn args))

(defn lint-code
  "Lint source using a language extension. Pass `language` FIRST when you know it —
   lint_code(language, arg); inferred from the file/workspace only when omitted. `arg` is a raw code string / {\"code\": ...}
   (lints the snippet), a {\"path\": file} or {\"paths\": […]} map (lints those on
   disk), or nothing (lints the workspace's default source paths). Returns the
   linter's findings + severity counts."
  [env & args]
  (dispatch! env :lint-fn args))

(defn run-tests
  "Run tests using a language extension. ALWAYS pass the language FIRST — run_tests(language, arg). `arg` selects what to run: a namespace/module string (e.g. run_tests(\"clojure\", \"my.app.core-test\")), or a dict — {\"namespaces\": [\"a-test\" \"b-test\"]} (alias :ns) to run several, {\"paths\": [\"test\" ...]} to discover *_test namespaces under dirs/files, plus optional {\"only\": [...] :include/:exclude [tags]} selectors. Omit arg to run the whole suite."
  [env & args]
  ;; Wall-clock the whole run so the RUN_TESTS card can headline how long it
  ;; took (parity with repl_eval's `(Nms)`); language handlers don't time
  ;; themselves, and this captures dispatch + run end-to-end.
  (let [start
        (System/currentTimeMillis)

        result
        (dispatch! env :test-fn args)]

    (if (map? result) (assoc result :ms (- (System/currentTimeMillis) start)) result)))

(defn repl-eval
  "Evaluate code in a language REPL. ALWAYS pass the language FIRST — repl_eval(language, arg). `arg` may include `id`/`repl_id` to target a registered REPL resource, and `dir` to target/auto-start the REPL in a subdirectory (e.g. a monorepo app dir) — REQUIRED when the code must run under that dir's config (tsconfig, package.json); defaults to the workspace root."
  [env & args]
  (dispatch! env :repl-eval-fn args))

(defn start-repl
  "Start/manage a language REPL resource. ALWAYS pass the language FIRST — repl_start(language, opts); opts may include `id` and language-specific options."
  [env & args]
  (dispatch-start-repl! env args))

(def format-symbol
  (vis/symbol
    #'format-code
    {:symbol 'format_code
     :native-tool? true
     ;; NAME(language, {payload}) — optional leading `language`, the rest a
     ;; pure options dict (always emitted so the payload stays a map).
     :call {:lead-opt "language" :rest :always}
     :render render-format-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"language"
       {:type "string"
        :description
        "Language pack (e.g. \"clojure\"); pass it first — inferred from the file/workspace only when omitted."}
       "code" {:type "string"
               :description
               "Source to format (returns a lean changed? + char-delta ack, not the text)."}
       "path"
       {:type "string"
        :description
        "Format this ONE file IN PLACE (returns a lean ack, not the text). Mutually exclusive with code. A directory is walked RECURSIVELY for source files."}
       "paths"
       {:type "array"
        :items {:type "string"}
        :description
        "Format MANY paths IN PLACE (returns a per-file changed roll-up). A DIRECTORY is walked RECURSIVELY for source files. Mutually exclusive with code/path; OMIT all to format the workspace's default source paths recursively."}}
      :required []}
     :before-fn inject-env
     :tag :mutation}))

(def lint-symbol
  (vis/symbol
    #'lint-code
    {:symbol 'lint_code
     :native-tool? true
     :render render-lint-result
     :color-role :tool-color/read
     :schema
     {:type "object"
      :properties
      {"language"
       {:type "string"
        :description
        "Language pack (e.g. \"clojure\"); pass it first — inferred from the file/workspace only when omitted."}
       "code" {:type "string"
               :description
               "Source to lint (returns findings). Mutually exclusive with path/paths."}
       "path" {:type "string" :description "Lint this file on disk."}
       "paths" {:type "array"
                :items {:type "string"}
                :description
                "Lint these files/dirs. OMIT all to lint the workspace's default source paths."}}
      :required []}
     :before-fn inject-env
     :tag :observation}))

(def test-symbol
  (vis/symbol
    #'run-tests
    {:symbol 'run_tests
     :native-tool? true
     :call {:lead-opt "language" :rest :always}
     ;; run_tests can exceed the generic Python eval watchdog; dispatch it
     ;; directly in Clojure so the language pack's own timeout budget wins.
     :handler (fn [env input]
                (run-tests env input))
     :render render-test-result
     :color-role :tool-color/test
     :schema
     {:type "object"
      :properties
      {"language" {:type "string"
                   :description
                   "Language pack (e.g. \"clojure\") — REQUIRED; ALWAYS pass it as the first arg."}
       "namespaces"
       {:type "array"
        :items {:type "string"}
        :description
        "Test namespaces/modules to run (e.g. [\"my.app.core-test\"]). OMIT (or pass []) to run every *_test namespace in the workspace."}
       "paths"
       {:type "array"
        :items {:type "string"}
        :description
        "Dirs/files to discover *_test namespaces under. OMIT (or pass []) to default to the workspace root; an explicit non-empty path that yields no tests is an error."}
       "only" {:type "array"
               :items {:type "string"}
               :description "Restrict to these fully-qualified test vars."}
       "include"
       {:type "array" :items {:type "string"} :description "Only run tests carrying these tags."}
       "exclude"
       {:type "array" :items {:type "string"} :description "Skip tests carrying these tags."}
       "dir"
       {:type "string"
        :description
        "Directory to run the test command in (e.g. a monorepo app dir). Defaults to the workspace root."}
       "filter" {:type "string"
                 :description "Test-name filter, for packs that support it (e.g. `bun test -t`)."}}
      :required ["language"]}
     :before-fn inject-env
     :tag :mutation}))

(def repl-eval-symbol
  (vis/symbol
    #'repl-eval
    {:symbol 'repl_eval
     :native-tool? true
     :call {:lead-opt "language" :rest :always}
     ;; repl_eval's own `timeout_ms` can exceed the generic Python eval
     ;; watchdog (DEFAULT_EVAL_TIMEOUT_MS, 120s); dispatch it directly in
     ;; Clojure so the language pack's own timeout budget wins (parity with
     ;; run_tests above).
     :handler (fn [env input]
                (repl-eval env input))
     :render render-repl-eval-result
     :color-role :tool-color/shell
     :schema
     {:type "object"
      :properties
      {"language" {:type "string"
                   :description
                   "Language pack (e.g. \"clojure\") — REQUIRED; ALWAYS pass it as the first arg."}
       "code" {:type "string" :description "Source to evaluate in the language REPL."}
       "id" {:type "string" :description "Target a specific registered REPL resource by id."}
       "dir"
       {:type "string"
        :description
        "Directory to run the REPL in (e.g. a monorepo app dir like \"apps/api\") — the REPL auto-starts there and picks up THAT dir's config (tsconfig/package.json). Defaults to the workspace root."}
       "timeout_ms" {:type "integer" :description "Eval timeout in milliseconds (default 30000)."}}
      :required ["language" "code"]}
     :before-fn inject-env
     :tag :mutation}))

(def start-repl-symbol
  (vis/symbol
    #'start-repl
    {:symbol 'repl_start
     :native-tool? true
     :call {:lead-opt "language" :rest :always}
     :render render-repl-start-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"language"
               {:type "string"
                :description
                "Language pack (e.g. \"clojure\") — REQUIRED; ALWAYS pass it as the first arg."}
               "id" {:type "string" :description "Resource id for the REPL (default per language)."}
               "dir" {:type "string" :description "Directory to start the REPL in."}
               "aliases" {:type "array"
                          :items {:type "string"}
                          :description "Build-tool aliases to activate (e.g. deps.edn :dev)."}}
              :required ["language"]}
     :before-fn inject-env
     :tag :mutation}))

(def repl-stop-symbol
  (vis/symbol #'repl-stop
              {:symbol 'repl_stop
               :native-tool? true
               ;; repl_stop(id) — one positional id. (lint_code intentionally has NO
               ;; :call: its fn takes the whole input dict, so the generic form fits.)
               :call {:pos ["id"]}
               :render render-repl-stop-result
               :color-role :tool-color/delete
               :schema {:type "object"
                        :properties {"id" {:type "string"
                                           :description "Session resource id of the REPL to stop."}}
                        :required ["id"]}
               :before-fn inject-env
               :tag :mutation}))

(def symbols
  [format-symbol lint-symbol test-symbol repl-eval-symbol start-repl-symbol repl-stop-symbol])

(defn prompt
  "The language-facade reference: the AUTO capability matrix (active packs only)
   + the bare facade verbs. nil when no language pack is active, so a non-coding
   or single-language workspace carries nothing extra. Each verb's own docstring
   holds its args/return; `language` is explicit only when several packs match."
  [env]
  (when-let [matrix (capability-matrix env)]
    (str
      matrix
      "\n"
      "  facade (language-first, or inferred): format_code · lint_code · run_tests · repl_eval · repl_start · repl_stop")))
