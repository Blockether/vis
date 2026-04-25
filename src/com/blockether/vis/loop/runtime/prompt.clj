(ns com.blockether.vis.loop.runtime.prompt
  "System prompt assembly and per-iteration nudge composers.

   Built-in nudges (budget, var-index overflow, repetition) live here.
   Extensions can contribute additional per-iteration nudges via
   `:ext/nudge-fn` — see `collect-extension-nudges`."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel]))

(defn build-system-prompt
  [{:keys [system-prompt]}]
  (or system-prompt ""))

;; ---------------------------------------------------------------------------
;; Nudges — system nudge composers used by iteration prompt assembly
;; ---------------------------------------------------------------------------

(def ^:private BUDGET_WARNING_WINDOW 2)
(def ^:private VAR_INDEX_OVERFLOW_THRESHOLD 150)
(def ^:private REPETITION_THRESHOLD 3)

(defn budget-warning
  "Warn when we are close to the iteration budget edge."
  [{:keys [iteration current-max-iterations]}]
  (let [iter (long (or iteration 0))
        max-iters (long (or current-max-iterations 0))
        remaining (- max-iters (inc iter))]
    (when (<= remaining BUDGET_WARNING_WINDOW)
      (str "[system_nudge] Iteration budget nearly exhausted (remaining="
        (max 0 remaining)
        "). If you can finalize safely, do it now."))))

(defn var-index-overflow
  "Warn when many vars are visible in <var_index>."
  [user-var-count]
  (let [n (long (or user-var-count 0))]
    (when (> n VAR_INDEX_OVERFLOW_THRESHOLD)
      (str "[system_nudge] <var_index> is large (" n
        " vars). Prefer :forget for scratch vars to reduce prompt noise."))))

(defn repetition-warning
  "Detect repeated code/result pairs and emit a nudge.
   Also updates `call-counts-atom` in-place."
  [call-counts-atom prev-expressions]
  (when (and call-counts-atom (seq prev-expressions))
    (let [keys* (mapv (fn [{:keys [code error result]}]
                        (if error
                          [:error-only (str/trim (str error))]
                          [(str/trim (str code)) (pr-str result)]))
                  prev-expressions)
          max-count (swap! call-counts-atom
                      (fn [m]
                        (reduce (fn [acc k] (update acc k (fnil inc 0)))
                          (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen REPETITION_THRESHOLD)
        "[system_nudge] You are repeating the same expression pattern. Change strategy (different tool, different query, or inspect intermediate vars)."))))

;; ---------------------------------------------------------------------------
;; Extension nudges
;; ---------------------------------------------------------------------------

(defn collect-extension-nudges
  "Invoke every registered extension's `:ext/nudge-fn` with `ctx` and
   collect non-nil string results. Each extension's nudge-fn receives
   the same nudge context map (see EXTENSION.md § Nudge Context).

   Extensions that have no `:ext/nudge-fn` or whose `:ext/activation-fn`
   returns falsy for the current environment are silently skipped.

   A throwing nudge-fn is caught, logged, and skipped — one broken
   extension must never take down the iteration pipeline."
  [extensions ctx]
  (when (seq extensions)
    (into []
      (keep (fn [ext]
              (when-let [nudge-fn (:ext/nudge-fn ext)]
                (try
                  (let [active? ((:ext/activation-fn ext) (:environment ctx))]
                    (when active?
                      (let [result (nudge-fn ctx)]
                        (when (and (string? result) (not (str/blank? result)))
                          result))))
                  (catch Throwable t
                    (tel/log! {:level :warn
                               :id    ::extension-nudge-failed
                               :data  {:ext  (:ext/namespace ext)
                                       :error (ex-message t)}
                               :msg   (str "Extension nudge-fn threw for '"
                                        (:ext/namespace ext) "' — skipping")})
                    nil)))))
      extensions)))
