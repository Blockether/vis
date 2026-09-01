(ns com.blockether.vis.contract.python-host
  "The Python host contract as DATA: `resources/vis-contract/python-host.edn`.

   Everything the `vis` Python module can ask its host to do is one entry in that
   document — the host global the engine binds, how many positional arguments
   the callable takes, and what the op does when there is no Vis host at all. The
   engine derives the names it binds from here, the injected host is built from
   here, and the package's outside-the-sandbox host derives its behavior from
   here, so a new host call is added to the document and nowhere else.

   This project is `com.blockether/vis-contract` and requires NO Vis namespace, so
   an extension can compile against the declaration without the engine. [[package-document]]
   ships as the root language-neutral generator input and `vis_contract/contract.json`.

   View, canonical-content, toggle and provider-limits vocabularies come from their
   own contract EDN documents; package rendering therefore needs no engine input.

   The document is validated the moment it is read: a malformed contract is a
   broken build, not a runtime surprise inside somebody's extension.
   `contract.python-host-test` is what fails when a reader drifts."
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.contract.content :as content]
            [com.blockether.vis.contract.gateway :as gateway]
            [com.blockether.vis.contract.provider :as provider]
            [com.blockether.vis.contract.toggle :as toggle]
            [com.blockether.vis.contract.view :as view]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(s/def :op/name (s/and non-blank-string? #(re-matches #"[a-z][a-z0-9_]*" %)))
(s/def :op/global (s/and non-blank-string? #(re-matches #"__vis_host_[a-z0-9_]+__" %)))
;; An op that takes nothing is still an op: enumeration asks the host for the
;; whole key list and passes no argument at all.
(s/def :op/arity (s/int-in 0 4))
(s/def :op/summary non-blank-string?)
(s/def :op/outside #{:outside/local :outside/prompt :outside/refuse})
(s/def :op/refusal non-blank-string?)

;; The `shell` verb's lifecycle grammar. `:shell/default-op` is the op an options
;; map without one means, so it has to be one of the spawn ops, and no op may both
;; spawn and drive a handle.
(s/def :shell/default-op non-blank-string?)
(s/def :shell/spawn-ops
  (s/and (s/coll-of non-blank-string? :kind vector? :distinct true) not-empty))
(s/def :shell/handle-ops
  (s/and (s/coll-of non-blank-string? :kind vector? :distinct true) not-empty))
(s/def :contract/shell
  (s/and (s/keys :req [:shell/default-op :shell/spawn-ops :shell/handle-ops])
         #(contains? (set (:shell/spawn-ops %)) (:shell/default-op %))
         #(not-any? (set (:shell/handle-ops %)) (:shell/spawn-ops %))))
;; The `live` verb's grammar, read exactly like the shell's: one spawn op that
;; MOUNTS a view and answers its id, handle ops that drive the view that id names,
;; and the window the packaged handle may coalesce pushes inside before one has to
;; cross the host boundary.
(s/def :live/default-op non-blank-string?)
(s/def :live/spawn-ops (s/and (s/coll-of non-blank-string? :kind vector? :distinct true) not-empty))
(s/def :live/handle-ops
  (s/and (s/coll-of non-blank-string? :kind vector? :distinct true) not-empty))
(s/def :live/flush-ms pos-int?)
(s/def :contract/live
  (s/and (s/keys :req [:live/default-op :live/spawn-ops :live/handle-ops :live/flush-ms])
         #(contains? (set (:live/spawn-ops %)) (:live/default-op %))
         #(not-any? (set (:live/handle-ops %)) (:live/spawn-ops %))))
;; A refusal is REQUIRED exactly when the op refuses, and meaningless otherwise:
;; the reason an author reads in the traceback is part of the contract, not of the
;; implementation that happens to raise.
(s/def :contract/op
  (s/and (s/keys :req [:op/name :op/global :op/arity :op/summary :op/outside] :opt [:op/refusal])
         #(= (contains? % :op/refusal) (= :outside/refuse (:op/outside %)))
         #(= (:op/global %) (str "__vis_host_" (:op/name %) "__"))))

;; `:min-count` on `s/coll-of` counts through a boxed `inc`; `not-empty` keeps the
;; conformed value a vector and the reflection check quiet.
(s/def :contract/ops (s/and (s/coll-of :contract/op :kind vector? :distinct true) not-empty))
(s/def :contract/version pos-int?)
(s/def :contract/python-host
  (s/keys :req [:contract/version :contract/ops :contract/shell :contract/live]))

(def ^:private resource-path "vis-contract/python-host.edn")

(def ^:private document
  "The parsed, validated contract. Read from the classpath — embedded in the native
   image by build.clj's blanket `-H:IncludeResources=.*\\.edn$`, which is what
   covers every `vis-contract/*.edn`; a contract in any other format would need
   its own pattern."
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the Python host contract is missing from the classpath: "
                                 resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/python-host parsed)
        (throw (ex-info (str resource-path " is not a valid Python host contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/python-host parsed)})))
      parsed)))

(defn ops "Every declared host op, in document order." [] (:contract/ops @document))

(defn op "The op named `name`, or nil." [name] (first (filter #(= name (:op/name %)) (ops))))

(defn op-names
  "The op names the packaged `vis` module's `_host` object answers, in document order."
  []
  (mapv :op/name (ops)))

(defn host-globals
  "The `__vis_host_*` globals the engine binds, in document order."
  []
  (mapv :op/global (ops)))

(defn version
  "The contract version. Bumped when an op is added, removed or re-shaped."
  []
  (:contract/version @document))

(defn shell-vocabulary
  "The `shell` verb's lifecycle grammar: the op an options map without one means,
   the ops that SPAWN a process, and the ops that drive the handle one answered."
  []
  (:contract/shell @document))

(defn live-vocabulary
  "The `live` verb's lifecycle grammar: the op an options map without one means,
   the op that MOUNTS a view, the ops that drive the view it answered, and the
   window a handle may coalesce pushes inside."
  []
  (:contract/live @document))
;; ---------------------------------------------------------------------------
;; The document the PACKAGE reads
;;
;; `python/src/vis_contract/contract.json` is this document plus the contract-owned
;; View vocabulary, rendered for Python and JavaScript readers that have no EDN or
;; JVM. It is checked in because a wheel installed from PyPI has no repository to
;; read. `python_package_test` fails on drift and names [[write-package-document!]]
;; as the fix.


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
             "toggle" (toggle/package-document)
             "provider" (provider/package-document)))

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
