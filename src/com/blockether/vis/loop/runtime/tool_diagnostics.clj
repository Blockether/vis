(ns com.blockether.vis.loop.runtime.tool-diagnostics
  "Tool diagnostics: timing for activation and execution.
   Cross-conversation — stored in a process-global atom, not per-env."
  (:require [clojure.string :as str]))

(defonce ^:private diagnostics (atom {}))

(defn record-activation-check!
  [sym active? elapsed-ns]
  (swap! diagnostics update sym
    (fn [d]
      (-> (or d {:sym sym :activation-checks 0 :executions 0})
        (update :activation-checks inc)
        (assoc :last-activation-ns elapsed-ns
               :last-active? active?)))))

(defn record-execution!
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

(defn get-diagnostics [] @diagnostics)
(defn reset-diagnostics! [] (reset! diagnostics {}))

(defn format-doctor-report
  "Format a human-readable doctor report from tool registry + diagnostics.
   `tools` is a seq of {:sym :group :activation-doc :active? :activation-ms}."
  [tools]
  (let [sb       (StringBuilder.)
        by-group (group-by :group tools)
        sorted   (sort-by key by-group)]
    (doseq [[group group-tools] sorted]
      (.append sb (str "\n  " (or group "Other") "\n"))
      (.append sb (str "  " (apply str (repeat (count (or group "Other")) "─")) "\n"))
      (doseq [t (sort-by :sym group-tools)]
        (let [icon   (if (:active? t) "✓" "✗")
              sym    (str (:sym t))
              pad    (apply str (repeat (max 1 (- 30 (count sym))) " "))
              reason (if (:active? t)
                       (str "active")
                       (str "inactive — " (or (:activation-doc t) "activation-fn returned false")))
              timing (when (:activation-ms t)
                       (format " (%.2fms)" (double (:activation-ms t))))]
          (.append sb (str "  " icon " " sym pad reason (or timing "") "\n")))))
    (str sb)))
