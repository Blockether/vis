(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces:

     1. The system prompt — written once, cached per-conversation.
        `assemble-system-prompt` joins the minimal core prompt + the
        environment block + each active extension's prompt fragment.

     2. The trailing user message — rebuilt every iteration. Two slots:
          <recent>      last RECENT_KEEP_ITERS iterations, code + result,
                        addressable as iN.K. The model's working memory.
          <var_index>   user-defined `(def ...)` bindings in the SCI env.
        Plus one optional [system_nudge] line when the model executes the
        same expression twice.

   Everything else — plan, breadcrumbs, attempts ledger, recent_thought,
   PRIOR_TURN digest, vars_archive, plan-validation nudges — was deleted.
   The two slots above plus the SYSTEM vars (`QUERY` `ANSWER` `REASONING`)
   bound in SCI cover the same ground without the projection drift that
   produced the previous read-loop pathology."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Hard cap on a single value's pr-str when shown to the model in <recent>."
  6000)

(def ^:const RECENT_KEEP_ITERS
  "Number of iterations carried in <recent>. 3 gives the model a real
   moving window: it can compare what it just read with what it read
   two iterations ago WITHOUT making another tool call."
  3)

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) " …") s)))

(defn- strip-sandbox-ns [s]
  (str/replace (str s) #"\bsandbox/" ""))

(defn- realize-value [v]
  (cond
    (nil? v) nil
    (map? v) v
    (vector? v) v
    (string? v) v
    :else v))

(defn safe-pr-str
  "Bounded pr-str. Used in <recent> rendering."
  ([v] (safe-pr-str v {}))
  ([v {:keys [max-chars print-length print-level]
       :or {max-chars MAX_RESULT_DISPLAY_CHARS
            print-length 64
            print-level 6}}]
   (try
     (binding [*print-length* print-length
               *print-level*  print-level]
       (let [s (strip-sandbox-ns (pr-str v))]
         (if (> (count s) max-chars)
           (str (subs s 0 max-chars) " …<+" (- (count s) max-chars) " chars>")
           s)))
     (catch Throwable t
       (str "<unprintable: " (.getMessage t) ">")))))

(defn truncated-pr-str
  "Wrapper used by <recent>. Returns [bounded-str truncated?]."
  [v]
  (let [bounded   (safe-pr-str v {:max-chars MAX_RESULT_DISPLAY_CHARS})
        truncated? (boolean (re-find #" …<\+\d+ chars>$" bounded))]
    [bounded truncated?]))

;; =============================================================================
;; <recent> — last RECENT_KEEP_ITERS iterations of code + results
;; =============================================================================

(defn- format-recent-block
  "Render the last RECENT_KEEP_ITERS iterations with iN.K addressable ids.
   `expressions-by-iteration` is a seq of `[iteration-position [exprs]]`
   pairs, oldest-first."
  [expressions-by-iteration]
  (let [kept (take-last RECENT_KEEP_ITERS (or expressions-by-iteration []))]
    (when (seq kept)
      (let [lines (for [[iteration-position exprs] kept
                        [k expr] (map-indexed vector exprs)
                        :let [{:keys [code error result stdout stderr execution-time-ms]} expr]]
                    (let [code-str      (str/trim (or code ""))
                          stdout-suffix (when-not (str/blank? stdout)
                                          (str " :stdout " (pr-str (truncate stdout 600))))
                          stderr-suffix (when-not (str/blank? stderr)
                                          (str " :stderr " (pr-str (truncate stderr 600))))
                          time-ms       (or execution-time-ms 0)
                          slow-suffix   (when (> time-ms 5000)
                                          (str " (" time-ms "ms)"))
                          value-part    (if error
                                          (str "ERROR: " (truncate error 600))
                                          (let [v (realize-value result)
                                                [value-str truncated?] (truncated-pr-str v)]
                                            (str value-str
                                              (when truncated? " :truncated? true"))))]
                      (str "  i" iteration-position "." (inc k) "  " code-str " → " value-part
                        (or slow-suffix "")
                        (or stdout-suffix "")
                        (or stderr-suffix ""))))]
        (when (seq lines)
          (str "<recent>\n" (str/join "\n" lines) "\n</recent>"))))))

;; =============================================================================
;; <var_index> — read/cache the current SCI sandbox shape
;; =============================================================================

(defn read-var-index-str
  "Lazily build (and cache) the <var_index> body for the active env.
   Returns nil when the env has no SCI context (test fixtures)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [var-index-atom (or (:var-index-atom environment)
                           (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @var-index-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx) [:namespaces 'sandbox])
              idx         (env/build-var-index
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)]
          (swap! var-index-atom assoc :index idx :revision current-revision)
          idx)))))

;; =============================================================================
;; Repetition warning (the only nudge that survived the cull)
;; =============================================================================

(def ^:private REPETITION_THRESHOLD 1)

(defn- repetition-warning
  "Emit a single [system_nudge] line when the SAME (code, error?) was seen
   in the previous iteration's expressions. Threshold of 1 means: one
   repeat is enough; the model should change strategy."
  [call-counts-atom previous-expressions]
  (when (and call-counts-atom (seq previous-expressions))
    (let [keys* (mapv (fn [{:keys [code error]}]
                        (if error
                          [:error-only (str/trim (str error))]
                          [:code-only (str/trim (str code))]))
                  previous-expressions)
          max-count (swap! call-counts-atom
                      (fn [m]
                        (reduce (fn [acc k] (update acc k (fnil inc 0)))
                          (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen REPETITION_THRESHOLD)
        "[system_nudge] You repeated the same expression. Either consult <recent> for the previous result, or change strategy."))))

;; =============================================================================
;; Iteration context — the trailing user message
;; =============================================================================

(defn build-iteration-context
  "Assemble the per-iteration trailing user message.

   Two slots:
     <recent>      — last RECENT_KEEP_ITERS iterations, code + result.
     <var_index>   — `(def ...)` bindings in the SCI env.

   Plus one [system_nudge] line if the model is repeating itself.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`. Computed once
        per query; threaded through every iteration. Each extension's
        :ext/nudge-fn is consulted (rare).

   Optional:
     `:expressions-by-iteration`, `:call-counts-atom`."
  [environment {:keys [expressions-by-iteration call-counts-atom active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [recent-block (format-recent-block expressions-by-iteration)
        last-iteration-expressions (some-> expressions-by-iteration last second)
        var-index-str (read-var-index-str environment)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        rep-nudge (repetition-warning call-counts-atom last-iteration-expressions)
        ext-nudges (when (seq active-extensions)
                     (let [ctx {:environment environment
                                :previous-expressions last-iteration-expressions}]
                       (into []
                         (keep (fn [ext]
                                 (when-let [nudge-fn (:ext/nudge-fn ext)]
                                   (try
                                     (let [result (nudge-fn ctx)]
                                       (when (and (string? result) (not (str/blank? result)))
                                         result))
                                     (catch Throwable t
                                       (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                                       nil)))))
                         active-extensions)))
        nudges (cond-> []
                 rep-nudge          (conj rep-nudge)
                 (seq ext-nudges)   (into ext-nudges))
        nudges-block (when (seq nudges) (str/join "\n" nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [recent-block var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn assemble-initial-messages
  [{:keys [system-prompt initial-user-content history-messages]}]
  (vec
    (concat
      (when system-prompt [{:role "system" :content system-prompt}])
      (or history-messages [])
      (when initial-user-content [{:role "user" :content initial-user-content}]))))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

;; =============================================================================
;; System prompt
;; =============================================================================

(def CORE_SYSTEM_PROMPT
  "Clojure agent. Think = write Clojure in SCI sandbox.

Each turn emit JSON:
  :code    ONE source string. Multiple top-level forms OK; runtime evals each in order.
           Each form -> one iN.K result. `\"\"` = noop.
  :answer  optional. Emit when done -> turn ends.

After eval you get fresh user msg:
  <recent>     last few iters' forms + results. Addressable iN.K (iter N, form K, 1-indexed).
               Shown = form's return val. `(def x ...)` returns the var, NOT the bound value.
               To SEE a tool result, call inline. Don't wrap reads in `def` unless crossing iters.
  <var_index>  your `(def name val)` bindings still alive. Strings verbatim up to ~8000 chars.

SCI vars bound by name: `QUERY` (current req), `ANSWER` (prev turn), `REASONING` (last iter thinking).

Rules:
  • Real Clojure. `let` / `do` / threading inside one form when steps depend.
  • `def` / `defn` only to KEEP values across iters.
  • Inspect = RETURN from form (last expr of `do`) -> appears in <recent>. No print, no def: return.
  • Need prior tool result? Read <recent> or your bound var. Don't re-fetch.")

(defn build-system-prompt
  "Core system prompt: agent rules + optional caller addendum.

   The `<environment>` block (cwd, OS, git facts, languages,
   monorepo shape) is NOT assembled here. It is rendered by the
   `vis-common-environment` extension's `:ext/prompt` fragment, so
   the runtime no longer hardcodes any environment text. Drop the
   jar, drop the block."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    (when (and system-prompt (not (str/blank? system-prompt)))
      (str "\n\nINSTRUCTIONS:\n" system-prompt "\n"))))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a query."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean ((:ext/activation-fn ext) environment))
                  (catch Throwable t
                    (tel/log! {:level :error :id ::ext-activation-error
                               :data {:ext (:ext/namespace ext)
                                      :error (ex-message t)}}
                      (str "Extension '" (:ext/namespace ext) "' activation-fn threw"))
                    false)))
        exts))))

(defn- render-extension-prompt-block
  [environment ext]
  (try
    (let [canonical (extension/render-prompt ext)
          extra-fn  (:ext/prompt ext)
          extra     (when extra-fn (extra-fn environment))
          body      (->> [canonical extra]
                      (filter #(and (string? %) (not (str/blank? %))))
                      (str/join "\n"))]
      (when-not (str/blank? body)
        (if-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
          (str "[namespace: " alias-sym " → " ns-sym "]\n" body)
          body)))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-prompt-error
                 :data {:ext (:ext/namespace ext) :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' prompt rendering failed"))
      nil)))

(defn assemble-system-prompt
  "Build the full system prompt: core agent rules + active-extension prompts.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [base   (build-system-prompt {:system-prompt system-prompt})
        ext-ps (seq (keep #(render-extension-prompt-block environment %) active-extensions))]
    (if ext-ps
      (str base "\n\n" (str/join "\n\n" ext-ps))
      base)))
