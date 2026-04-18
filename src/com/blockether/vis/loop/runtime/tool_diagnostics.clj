(ns com.blockether.vis.loop.runtime.tool-diagnostics
  "Tool diagnostics: timing for activation, load, and execution.
   Cross-conversation — stored in a process-global atom, not per-env."
  (:require [clojure.string :as str]))

;; Process-global tool diagnostics. Survives across conversations.
(defonce ^:private diagnostics (atom {}))

(defn record-activation-check!
  "Record how long an activation-fn took to evaluate."
  [sym active? elapsed-ns]
  (swap! diagnostics update sym
    (fn [d]
      (-> (or d {:sym sym :activation-checks 0 :executions 0})
        (update :activation-checks inc)
        (assoc :last-activation-ns elapsed-ns
               :last-active? active?)))))

(defn record-execution!
  "Record a tool execution timing."
  [sym elapsed-ns error?]
  (swap! diagnostics update sym
    (fn [d]
      (let [d (or d {:sym sym :activation-checks 0 :executions 0})]
        (-> d
          (update :executions inc)
          (assoc :last-execution-ns elapsed-ns)
          (cond->
            error? (update :errors (fnil inc 0))
            (not error?) (update :total-execution-ns (fnil + 0) elapsed-ns)))))))

(defn get-diagnostics
  "Returns diagnostics map: {sym -> {:sym :activation-checks :executions ...}}"
  []
  @diagnostics)

(defn reset-diagnostics! [] (reset! diagnostics {}))

(defn format-table
  "Format diagnostics as a table string.
   Returns a string with aligned columns."
  []
  (let [data (sort-by (comp str key) @diagnostics)
        fmt-ns (fn [ns] (when ns (format "%.2fms" (/ (double ns) 1e6))))
        rows (mapv (fn [[sym d]]
                     [(str sym)
                      (str (:activation-checks d 0))
                      (or (fmt-ns (:last-activation-ns d)) "—")
                      (if (:last-active? d) "✓" "✗")
                      (str (:executions d 0))
                      (or (fmt-ns (:last-execution-ns d)) "—")
                      (str (:errors d 0))])
               data)
        header ["Tool" "Activ#" "ActTime" "Active?" "Exec#" "ExecTime" "Errors"]
        all-rows (cons header rows)
        widths (reduce (fn [ws row]
                         (mapv (fn [w cell] (max w (count cell))) ws row))
                 (vec (repeat (count header) 0))
                 all-rows)
        pad (fn [s w] (str s (apply str (repeat (- w (count s)) " "))))
        sep (str/join " | " (map #(apply str (repeat % "-")) widths))]
    (str/join "\n"
      (concat
        [(str/join " | " (map pad header widths))
         sep]
        (map #(str/join " | " (map pad % widths)) rows)))))
