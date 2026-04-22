(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.format
  "Result formatting.

   Renders execution results as compact per-block receipts for:
   - LLM feedback in the next iteration's <journal>
   - Error-recovery / iteration-accumulation path
   - Prior-thinking chain and cross-query handover"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.shared :as rt-shared
    :refer [realize-value truncate]]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:const SLOW_EXECUTION_MS 5000)

(def ^:const EXECUTION_SAFETY_CAP_CHARS
  "Cap for one value rendering in a journal entry."
  rt-shared/MAX_RESULT_DISPLAY_CHARS)

(def ^:const EXECUTION_STDERR_CHARS 2000)

(def ^:const HANDOVER_KEEP_LAST
  "How many of the previous turn's most-recent iterations carry over
   into the new turn's handover block."
  2)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- formatted-str-of
  "Return the canonical formatted string for a tool-produced value, or nil."
  [v]
  (when (instance? clojure.lang.IObj v)
    (let [m (meta v)]
      (or (:rlm/formatted m)
          (when-let [f (:rlm/format m)]
            (try (f v) (catch Throwable _ nil)))))))

(defn- truncated-pr-str
  "pr-str v + truncate-at-cap flag."
  [v]
  (let [s (rt-shared/strip-sandbox-ns (pr-str v))]
    (if (> (count s) EXECUTION_SAFETY_CAP_CHARS)
      [(truncate s EXECUTION_SAFETY_CAP_CHARS) true]
      [s false])))

(defn- error-hint
  "Returns a specialized hint for a known error, or nil."
  [error-msg]
  (when error-msg
    (let [e (str error-msg)]
      (cond
        (re-find #"Unable to resolve symbol: (\S+)" e)
        (let [[_ sym] (re-find #"Unable to resolve symbol: (\S+)" e)]
          (str "'" sym "' is not defined. (def " sym " ...) or check spelling."))

        (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)
        (let [[_ n target] (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)]
          (cond
            (str/includes? target "PersistentVector")
            (str "Vectors take 1 arg (index). Use (nth v idx) or (subvec v start end), not (v " (str/join " " (repeat (parse-long n) "x")) ").")
            :else
            (str "Function expects different arity than " n ". Check with (doc fn-name).")))

        (str/includes? e "cannot be cast to clojure.lang.IFn")
        "You're calling a non-function. Bare (1 2 3) calls 1 as fn. Use '(1 2 3) for list literals."

        (str/includes? e "Nested fn")
        "Nested #() is illegal. Rewrite inner #() as (fn [x] ...)."

        (re-find #"unbound fn: #'sandbox/(\S+)" e)
        (let [[_ sym] (re-find #"unbound fn: #'sandbox/(\S+)" e)]
          (str "'" sym "' was declared but its defn failed. Fix the defn above first."))

        (str/includes? e "LazySeq")
        "conj/peek/pop need a vector, not a lazy seq. Wrap with (vec ...) first."

        (str/includes? e "NullPointerException")
        "Something is nil unexpectedly. Debug with (prn suspect-value) to find which value is nil."

        (re-find #"recur.*tail" e)
        "recur must be the last expression in a loop/fn body."

        (and (str/includes? e "Unmatched delimiter")
          (str/includes? e "}"))
        "Map literals with \\} or \\{ as keys/values confuse the reader. Use (hash-map \\) \\( \\] \\[ \\} \\{) instead of {\\} \\{ ...}."

        (and (str/includes? e "EOF while reading")
          (str/includes? e "match {"))
        "If your map contains bracket char literals like \\} \\{, use (hash-map ...) instead of a map literal."

        :else nil))))

;; ---------------------------------------------------------------------------
;; format-executions (used by error-recovery / format-executions callers)
;; ---------------------------------------------------------------------------

(defn format-executions
  "Formats executions as compact per-block receipts for LLM feedback."
  [executions]
  (str/join "\n"
    (map (fn [{:keys [code error result stdout repaired? execution-time-ms]}]
           (let [code-str (str/trim (or code ""))
                  hint (when error (error-hint error))
                  var-surrogate? (and (map? result) (contains? result :rlm/var-id))
                  val-part (cond
                             error
                             (str "ERROR: " error
                               (when hint (str " :hint " (pr-str hint))))

                             (fn? result)
                             (str "ERROR: " code-str " is a function object. Call it: (" code-str ")")

                             var-surrogate?
                             (let [{var-name :rlm/var-id bound :rlm/var-value} result
                                   pre (formatted-str-of bound)
                                   [vs _] (if pre [pre false] (truncated-pr-str (realize-value bound)))]
                               (str "*" var-name "* = " vs))

                             (instance? clojure.lang.Var result)
                             (let [^clojure.lang.Var var-obj result
                                   var-name (name (.sym var-obj))
                                   raw-bound (.getRawRoot var-obj)
                                   pre (formatted-str-of raw-bound)
                                   [vs _] (if pre [pre false] (truncated-pr-str (realize-value raw-bound)))]
                               (str "*" var-name "* = " vs))

                             :else
                             (let [pre (formatted-str-of result)
                                   [vs _] (if pre [pre false] (truncated-pr-str (realize-value result)))]
                               vs))
                 stdout-part (when-not (str/blank? stdout)
                               (str " :stdout " (pr-str stdout)))
                 warning-part (when repaired?
                                " :warning \"auto-repaired delimiters\"")
                 time-ms (or execution-time-ms 0)
                 slow-part (when (> time-ms SLOW_EXECUTION_MS)
                             (str " (" time-ms "ms SLOW)"))]
             (str code-str " → " val-part
               (or slow-part "")
               (or stdout-part "")
               (or warning-part ""))))
      executions)))

;; ---------------------------------------------------------------------------
;; format-execution-results (journal block for next iteration)
;; ---------------------------------------------------------------------------

(defn format-execution-results
  "Formats the previous iteration's executions as the `<journal>` block."
  [executions _iteration]
  (when (seq executions)
    (str "<journal>\n"
      (str/join "\n"
        (map-indexed
          (fn [idx {:keys [code error result stdout stderr execution-time-ms]}]
            (let [code-str      (str/trim (or code ""))
                  stdout-suffix (when-not (str/blank? stdout)
                                  (str " :stdout " (pr-str stdout)))
                  stderr-suffix (when-not (str/blank? stderr)
                                  (str " :stderr " (pr-str (truncate stderr EXECUTION_STDERR_CHARS))))
                  time-ms       (or execution-time-ms 0)
                  slow-suffix   (when (> time-ms SLOW_EXECUTION_MS)
                                  (str " (" time-ms "ms SLOW)"))
                  var-surrogate? (and (map? result)
                                   (contains? result :rlm/var-id))
                  value-part
                  (cond
                    error
                    (str "ERROR: " (truncate error 400))

                    (fn? result)
                    "ERROR: Result is a function, not a value"

                    var-surrogate?
                    (let [{var-name :rlm/var-id bound :rlm/var-value} result
                          pre-formatted (formatted-str-of bound)
                          bound* (realize-value bound)
                          [value-str truncated?]
                          (if pre-formatted
                            [pre-formatted false]
                            (truncated-pr-str bound*))]
                      (str "*" var-name "* = " value-str
                        (when truncated? " :truncated? true")))

                    (instance? clojure.lang.Var result)
                    (let [^clojure.lang.Var var-obj result
                          var-name (name (.sym var-obj))
                          raw-bound (.getRawRoot var-obj)
                          pre-formatted (formatted-str-of raw-bound)
                          bound (realize-value raw-bound)
                          [value-str truncated?]
                          (if pre-formatted
                            [pre-formatted false]
                            (truncated-pr-str bound))]
                      (str "*" var-name "* = " value-str
                        (when truncated? " :truncated? true")))

                    :else
                    (let [pre-formatted (formatted-str-of result)
                          v (realize-value result)
                          [value-str truncated?]
                          (if pre-formatted
                            [pre-formatted false]
                            (truncated-pr-str v))]
                       (str value-str
                         (when truncated? " :truncated? true"))))]
              (str "  [" (inc idx) "] " code-str " → " value-part
                (or slow-suffix "")
                (or stdout-suffix "")
                (or stderr-suffix ""))))
          executions))
      "\n</journal>")))

;; ---------------------------------------------------------------------------
;; Prior thinking chain + cross-query handover
;; ---------------------------------------------------------------------------

(defn format-prior-thinking-chain
  "Compose the `<prior_thinking>` block showing iteration reasonings,
   oldest first. Returns the joined block string, or nil."
  [iterations]
  (let [entries (->> iterations
                  (keep (fn [{:keys [iteration thinking]}]
                          (when (and (string? thinking)
                                  (not (str/blank? thinking)))
                            (str "[iter " iteration "] " (str/trim thinking))))))]
    (when (seq entries)
      (str/join "\n\n" entries))))

(defn format-prior-turn-handover
  "Compose the cross-query handover shown at the TOP of a new turn's prompt.
   Returns nil when there are no previous iterations."
  [{:keys [iterations final-answer]}]
  (let [kept (take-last HANDOVER_KEEP_LAST iterations)]
    (when (seq kept)
      (let [thinking-lines (->> kept
                             (keep (fn [{:keys [iteration thinking]}]
                                     (when (and (string? thinking)
                                             (not (str/blank? thinking)))
                                       (str "  [iter " iteration "] "
                                         (str/trim thinking))))))
            final-line (when (and (string? final-answer)
                               (not (str/blank? final-answer)))
                         (str "  [final answer] " (str/trim final-answer)))]
        (str/join "\n"
          (concat ["[prior turn]"]
            thinking-lines
            (when final-line [final-line])
            ["[new query]"]))))))
