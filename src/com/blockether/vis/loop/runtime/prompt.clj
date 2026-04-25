(ns com.blockether.vis.loop.runtime.prompt
  "DEPRECATED — nudges and system prompt now live in iteration/core.clj.
   This namespace exists only for backward compat with loop/core.clj (old monolith).
   Do NOT add new code here."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel]))

(def ^:private BUDGET_WARNING_WINDOW 2)
(def ^:private REPETITION_THRESHOLD 3)

(defn budget-warning
  [{:keys [iteration current-max-iterations]}]
  (let [iter (long (or iteration 0))
        max-iters (long (or current-max-iterations 0))
        remaining (- max-iters (inc iter))]
    (when (<= remaining BUDGET_WARNING_WINDOW)
      (str "[system_nudge] Iteration budget nearly exhausted (remaining="
        (max 0 remaining) "). If you can finalize safely, do it now."))))

(defn var-index-overflow
  [user-var-count]
  (let [n (long (or user-var-count 0))]
    (when (> n 150)
      (str "[system_nudge] <var_index> is large (" n " vars). Prefer :forget for scratch vars."))))

(defn repetition-warning
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
        "[system_nudge] You are repeating the same expression pattern. Change strategy."))))

(defn collect-extension-nudges
  [extensions ctx]
  (when (seq extensions)
    (into []
      (keep (fn [ext]
              (when-let [nudge-fn (:ext/nudge-fn ext)]
                (try
                  (when ((:ext/activation-fn ext) (:environment ctx))
                    (let [result (nudge-fn ctx)]
                      (when (and (string? result) (not (str/blank? result)))
                        result)))
                  (catch Throwable t
                    (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                    nil)))))
      extensions)))
