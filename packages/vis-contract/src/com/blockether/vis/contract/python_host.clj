(ns com.blockether.vis.contract.python-host
  "Python host operations loaded from the validated JSON contract."
  (:require [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(def ^:private source (delay (document/load! "python-host")))

(defn- op->engine
  [op]
  (cond-> {:op/name (get op "name")
           :op/global (get op "global")
           :op/arity (get op "arity")
           :op/summary (get op "summary")
           :op/outside (keyword "outside" (get op "outside"))}
    (contains? op "refusal")
    (assoc :op/refusal (get op "refusal"))))

(defn ops "Every declared host op, in document order." [] (mapv op->engine (get @source "ops")))
(defn op "The op named `name`, or nil." [name] (first (filter #(= name (:op/name %)) (ops))))
(defn op-names "Host operation names, in document order." [] (mapv :op/name (ops)))
(defn host-globals "Host globals the engine binds, in document order." [] (mapv :op/global (ops)))
(defn version "The Python host contract version." [] (get @source "version"))

(defn shell-vocabulary
  "The shell operation lifecycle grammar."
  []
  (let [value (get @source "shell")]
    {:shell/default-op (get value "default_op")
     :shell/spawn-ops (get value "spawn_ops")
     :shell/handle-ops (get value "handle_ops")}))

(defn live-vocabulary
  "The live View operation lifecycle grammar."
  []
  (let [value (get @source "live")]
    {:live/default-op (get value "default_op")
     :live/spawn-ops (get value "spawn_ops")
     :live/handle-ops (get value "handle_ops")
     :live/flush-ms (get value "flush_ms")}))
