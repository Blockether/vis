(ns com.blockether.vis.contract.python-host
  "Python host operations loaded from the validated JSON contract."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.config :as config]
            [com.blockether.vis.contract.content :as content]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway]
            [com.blockether.vis.contract.provider :as provider]
            [com.blockether.vis.contract.surface :as surface]
            [com.blockether.vis.contract.test-runner :as test-runner]
            [com.blockether.vis.contract.toggle :as toggle]
            [com.blockether.vis.contract.view :as view]))

(set! *warn-on-reflection* true)

(def ^:private source (delay (document/load! "python-host")))

(defn- op->engine
  [op]
  (cond-> {:op/name (get op "name")
           :op/global (get op "global")
           :op/arity (get op "arity")
           :op/summary (get op "summary")
           :op/outside (keyword (get op "outside"))}
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


(defn- op->json
  [{:op/keys [name global arity summary outside refusal]}]
  (cond-> (array-map "name" name
                     "global" global
                     "arity" arity
                     "summary" summary
                     "outside" (clojure.core/name outside))
    refusal
    (assoc "refusal" refusal)))


(defn package-document
  "The portable `contract.json`: gateway semantics, host ops, verb grammars and
   contract-owned View, content, toggle and provider-limits vocabularies."
  []
  (array-map "version" (version)
             "ops" (mapv op->json (ops))
             "shell"
             (let [{:shell/keys [default-op spawn-ops handle-ops]} (shell-vocabulary)]
               (array-map "default_op" default-op "spawn_ops" spawn-ops "handle_ops" handle-ops))
             "live" (let [{:live/keys [default-op spawn-ops handle-ops flush-ms]} (live-vocabulary)]
                      (array-map "default_op" default-op
                                 "spawn_ops" spawn-ops
                                 "handle_ops" handle-ops
                                 "flush_ms" flush-ms))
             "gateway" (gateway/package-document)
             "view" (view/package-document)
             "content" (content/package-document)
             "config" (config/package-document)
             "toggle" (toggle/package-document)
             "provider" (provider/package-document)
             "surface" (surface/package-document)
             "test_runner" (test-runner/package-document)))

(def package-document-path
  "Where the rendered document is checked in, from the repository root."
  "packages/vis-contract/python/src/vis_contract/contract.json")

(def package-document-paths
  "The language-neutral generator input and its byte-identical Python wheel copy."
  ["packages/vis-contract/contract.json" package-document-path])

(defn package-document-json
  "[[package-document]] as the checked-in file's exact bytes."
  []
  (-> (json/write-json-str (package-document) {:indent-str "  "})
      (str/replace #" +\n" "\n")
      (str "\n")))

(defn write-package-document!
  "Re-render every [[package-document-paths]] copy from its owning contract data."
  []
  (let [body (package-document-json)]
    (doseq [path package-document-paths]
      (spit path body))
    package-document-paths))
