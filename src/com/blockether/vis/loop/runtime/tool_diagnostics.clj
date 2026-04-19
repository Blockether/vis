(ns com.blockether.vis.loop.runtime.tool-diagnostics
  "Cross-conversation telemetry for the tool subsystem.

   Two call sites contribute samples into a process-global atom:

   * `record-activation-check!` — fired from the per-turn activation pass in
     `query/core.clj` every time a tool's `:activation-fn` is evaluated.
   * `record-activation-error!` — same call site, when the activation-fn
     throws. Prior to this being tracked the error was silently coerced to
     `false` by the caller, so inactive-due-to-bug looked identical to
     inactive-by-design.
   * `record-execution!` — fired from `runtime/core.clj`'s `execute-tool`
     after every wrapped-tool invocation, with wall-clock duration and an
     error flag.

   The atom is **process-global on purpose**. Tool identity is stable across
   conversations (same symbol, same wrapper); aggregating per-env would just
   scatter the signal and defeat the 'is this tool healthy across my whole
   session' question the `vis doctor` command is answering.

   Storage is last-writer-wins on `:last-*` fields (intentionally cheap;
   concurrency racing on those is fine because the cumulative counters are
   what the report reads) plus monotonically-accumulated totals.

   Nothing in this namespace performs I/O. Rendering is pure and takes a
   pre-merged tool seq so callers control where the live activation pass
   happens (the `vis doctor` CLI runs a fresh pass at report time; test code
   can feed a synthetic seq)."
  (:require [clojure.string :as str]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private diagnostics (atom {}))

(defn- empty-record
  "Zeroed record for a freshly-seen tool symbol. Keeping the shape explicit
   means callers never have to guard against `nil` when reading totals."
  [sym]
  {:sym                   sym
   ;; Activation counters
   :activation-checks     0      ;; total calls to record-activation-check!
   :activation-active     0      ;; subset where active? was true
   :activation-errors     0      ;; activation-fn threw
   :last-active?          nil
   :last-activation-ns    nil
   :last-activation-error nil    ;; {:message str :class str}
   :total-activation-ns   0
   :max-activation-ns     0
   ;; Execution counters
   :executions            0      ;; total calls to record-execution!
   :execution-errors      0
   :last-execution-ns     nil
   :total-execution-ns    0      ;; only successful executions
   :max-execution-ns      0})

(defn- update-record
  "Apply a fn to a tool's record, creating the empty record on first touch."
  [m sym f]
  (update m sym (fn [d] (f (or d (empty-record sym))))))

;; =============================================================================
;; Writers (hot path — keep allocation-light)
;; =============================================================================

(defn record-activation-check!
  "Record one evaluation of a tool's `:activation-fn`. `elapsed-ns` is the
   measured duration; `active?` is the boolean result (truthy coerced)."
  [sym active? elapsed-ns]
  (swap! diagnostics update-record sym
    (fn [d]
      (let [active? (boolean active?)]
        (-> d
          (update :activation-checks inc)
          (cond-> active? (update :activation-active inc))
          (assoc :last-active?       active?
                 :last-activation-ns elapsed-ns
                 :last-activation-error nil)
          (update :total-activation-ns + elapsed-ns)
          (update :max-activation-ns max elapsed-ns))))))

(defn record-activation-error!
  "Record an exception thrown by a tool's `:activation-fn`. Still bumps
   `:activation-checks` so the ratio active/checks stays meaningful, and
   still accumulates timing — a slow-failing activation-fn is a bug worth
   surfacing."
  [sym ^Throwable t elapsed-ns]
  (swap! diagnostics update-record sym
    (fn [d]
      (-> d
        (update :activation-checks inc)
        (update :activation-errors inc)
        (assoc :last-active?          false
               :last-activation-ns    elapsed-ns
               :last-activation-error {:message (.getMessage t)
                                       :class   (.getName (class t))})
        (update :total-activation-ns + elapsed-ns)
        (update :max-activation-ns max elapsed-ns)))))

(defn record-execution!
  "Record one wrapped-tool invocation. `error?` = true means the tool threw
   or returned an error envelope. Successful runs contribute to
   `:total-execution-ns`; errored runs don't (mean exec time stays an honest
   'how long does this tool take when it works' number)."
  [sym elapsed-ns error?]
  (swap! diagnostics update-record sym
    (fn [d]
      (let [error? (boolean error?)]
        (-> d
          (update :executions inc)
          (assoc :last-execution-ns elapsed-ns)
          (update :max-execution-ns max elapsed-ns)
          (cond->
            error?       (update :execution-errors inc)
            (not error?) (update :total-execution-ns + elapsed-ns)))))))

;; =============================================================================
;; Readers
;; =============================================================================

(defn get-diagnostics
  "Return a snapshot of the diagnostics atom. Stable shape — consumers can
   rely on every key in `empty-record` being present once a tool has been
   seen at least once."
  []
  @diagnostics)

(defn reset-diagnostics!
  "Wipe all telemetry. Intended for tests and for long-running server
   processes that want a fresh slate per deploy."
  []
  (reset! diagnostics {}))

(defn clear-tool!
  "Drop telemetry for a single tool symbol (e.g. when a tool is
   unregistered). Silent no-op if the symbol was never recorded."
  [sym]
  (swap! diagnostics dissoc sym))

;; =============================================================================
;; Derived statistics
;; =============================================================================

(defn- mean-ns
  "Mean duration, or nil if denominator is zero. Returning `nil` keeps the
   formatter's 'no data' branch a single check."
  [total n]
  (when (and total n (pos? n))
    (/ (double total) (double n))))

(defn stats-for
  "Convenience view over a single tool's record with derived means. Useful
   for tests and for adapter code that wants one call to pull a summary."
  [sym]
  (when-let [d (get @diagnostics sym)]
    (assoc d
      :mean-activation-ns (mean-ns (:total-activation-ns d) (:activation-checks d))
      :mean-execution-ns  (mean-ns (:total-execution-ns d)
                                    (max 0 (- (:executions d) (:execution-errors d)))))))

;; =============================================================================
;; Rendering
;; =============================================================================

(defn- ns->ms-str
  "Format a nanosecond duration as a compact millisecond string. `nil` →
   `\"  —  \"` so the column stays aligned."
  [ns]
  (if (and ns (number? ns))
    (format "%6.2fms" (/ (double ns) 1e6))
    "    —   "))

(defn- rpad
  "Right-pad `s` with spaces to `width` columns. If `s` is already longer,
   leave it alone — we'd rather see the full name than truncate silently."
  [s width]
  (let [s (str s)
        pad (- width (count s))]
    (if (pos? pad)
      (str s (str/join (repeat pad \space)))
      s)))

(defn- ruler
  "Horizontal rule the same width as `s`, using the Unicode box-drawing
   heavy line. Purely cosmetic."
  [s]
  (str/join (repeat (count (str s)) \u2500)))

(defn- format-tool-line
  "Render one tool's row: status icon, name, reason, live activation cost,
   and — when present — execution stats and activation-error trailer."
  [{:keys [sym group active? activation-ms activation-doc]} stats]
  (let [icon     (if active? "\u2713" "\u2717")
        reason   (if active?
                   "active"
                   (str "inactive \u2014 "
                     (or activation-doc "activation-fn returned false")))
        act-ms   (when activation-ms (format " (%.2fms)" (double activation-ms)))
        exec     (when (and stats (pos? (long (:executions stats 0))))
                   (let [n       (:executions stats)
                         errs    (:execution-errors stats)
                         mean    (:mean-execution-ns stats)
                         max-ns  (:max-execution-ns stats)]
                     (str "  \u2502 runs=" n
                       (when (pos? errs) (str " errs=" errs))
                       (when mean   (str " mean=" (str/trim (ns->ms-str mean))))
                       (when (and max-ns (pos? (long max-ns)))
                         (str " max=" (str/trim (ns->ms-str max-ns)))))))
        err-tail (when-let [e (and stats (:last-activation-error stats))]
                   (str "\n      \u2514 activation error: " (:class e) ": " (:message e)))]
    (str "  " icon " " (rpad sym 30) reason (or act-ms "") (or exec "") (or err-tail ""))))

(defn format-doctor-report
  "Human-readable doctor report.

   `tools` is a seq of maps describing the *current* registry state, each:
     {:sym 'search-documents
      :group \"documents\"
      :active? true
      :activation-ms 0.42
      :activation-doc \"requires indexed docs\"}
   The live activation pass is the caller's responsibility — this fn is
   pure. It merges the caller-supplied tool list with the cumulative
   telemetry from `get-diagnostics` so the report shows BOTH 'is this tool
   active right now' AND 'how has it behaved across all past runs'.

   Grouping is by `:group` (alphabetical), tools within a group sorted by
   name. Missing group → \"Other\"."
  [tools]
  (let [diag     (get-diagnostics)
        by-group (group-by #(or (:group %) "Other") tools)
        groups   (sort-by key by-group)
        sb       (StringBuilder.)]
    (doseq [[group group-tools] groups]
      (.append sb (str "\n  " group "\n"))
      (.append sb (str "  " (ruler group) "\n"))
      (doseq [t (sort-by (comp str :sym) group-tools)]
        (let [stats (get diag (:sym t))
              stats (when stats
                      (assoc stats
                        :mean-execution-ns
                        (mean-ns (:total-execution-ns stats)
                          (max 0 (- (:executions stats)
                                   (:execution-errors stats))))))]
          (.append sb (format-tool-line t stats))
          (.append sb "\n"))))
    (str sb)))

(defn summary-line
  "One-line headline for the top of the doctor output.

   Example: `42 tools registered — 37 active, 5 inactive; 128 executions (3 errors)`"
  [tools]
  (let [n        (count tools)
        active   (count (filter :active? tools))
        inactive (- n active)
        diag     (vals (get-diagnostics))
        execs    (reduce + 0 (map :executions diag))
        errs     (reduce + 0 (map :execution-errors diag))]
    (str n " tools registered \u2014 "
      active " active, " inactive " inactive"
      (when (pos? execs)
        (str "; " execs " execution" (when (not= 1 execs) "s")
          (when (pos? errs) (str " (" errs " error" (when (not= 1 errs) "s") ")")))))))
