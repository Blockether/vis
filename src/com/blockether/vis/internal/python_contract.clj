(ns com.blockether.vis.internal.python-contract
  "The Python host contract as DATA: `resources/vis-contract/python-host.edn`.

   Everything the `vis` Python module can ask its host to do is one entry in that
   document — the polyglot global the engine binds, how many positional arguments
   the callable takes, and what the op does when there is no Vis host at all. The
   engine derives the names it binds from here, the packaged module derives its
   `_host` dict from here, and the package's outside-the-sandbox host derives its
   behavior from here, so a new host call is added to the document and nowhere
   else. `python_contract_test` is what fails when a reader drifts.

   The document is validated the moment it is read: a malformed contract is a
   broken build, not a runtime surprise inside somebody's extension."
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.human-input.spec :as hi]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(s/def :op/name (s/and non-blank-string? #(re-matches #"[a-z][a-z0-9_]*" %)))
(s/def :op/global (s/and non-blank-string? #(re-matches #"__vis_host_[a-z0-9_]+__" %)))
(s/def :op/arity (s/int-in 1 4))
(s/def :op/summary non-blank-string?)
(s/def :op/outside #{:outside/local :outside/prompt :outside/refuse})
(s/def :op/refusal non-blank-string?)

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
(s/def :contract/python-host (s/keys :req [:contract/version :contract/ops]))

(def ^:private resource-path "vis-contract/python-host.edn")

(def ^:private document
  "The parsed, validated contract. Read from the classpath — embedded in the native
   image by build.clj's `-H:IncludeResources=vis-contract/.*`."
  (delay
    (let
      [resource
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
  "The `_host` dict keys the packaged `vis` module builds, in document order."
  []
  (mapv :op/name (ops)))

(defn host-globals
  "The `__vis_host_*` polyglot globals the engine binds, in document order."
  []
  (mapv :op/global (ops)))

(defn version
  "The contract version. Bumped when an op is added, removed or re-shaped."
  []
  (:contract/version @document))

;; ---------------------------------------------------------------------------
;; The document the PACKAGE reads
;;
;; `packages/vis-agent/src/vis/contract.json` is this document plus the human-input
;; vocabulary, rendered for a Python reader that has no EDN and no JVM. It is
;; CHECKED IN because a wheel installed from PyPI has no repository to read, and
;; GENERATED because [[hi/field-types]] and friends are the one definition of that
;; vocabulary -- `doc("human-input")` forbids a second copy, so the package gets a
;; rendering, never a transcription. `python_contract_test` fails on drift and
;; names [[write-package-document!]] as the fix.

(defn- op->json
  [{:op/keys [name global arity summary outside refusal]}]
  (cond->
    (array-map "name" name
               "global" global
               "arity" arity
               "summary" summary
               "outside" (clojure.core/name outside))
    refusal
    (assoc "refusal" refusal)))

(defn package-document
  "The contract as `vis/contract.json`: snake_case string keys, ops in document
   order, and the closed human-input vocabulary the outside host prompts with."
  []
  (array-map "version" (version)
             "ops" (mapv op->json (ops))
             "human_input" (array-map "field_types" (vec (sort (keys hi/field-types)))
                                      "text_types" (mapv clojure.core/name (sort hi/text-types))
                                      "choice_types" (mapv clojure.core/name (sort hi/choice-types))
                                      "secret_types" (mapv clojure.core/name (sort hi/secret-types))
                                      "decor_types" (vec (sort (keys hi/decor-types)))
                                      "group_type" hi/group-type-name
                                      "group_directions" (vec (sort (keys hi/group-directions)))
                                      "otp" (array-map "length" (:length hi/otp-defaults)
                                                       "ceiling" (:ceiling hi/otp-defaults))
                                      "range" (array-map "min" (:min hi/range-defaults)
                                                         "max" (:max hi/range-defaults)
                                                         "step" (:step hi/range-defaults))
                                      "secret_handle_prefix" hi/secret-handle-prefix)))

(def package-document-path
  "Where the rendered document is checked in, from the repository root."
  "packages/vis-agent/src/vis/contract.json")

(defn package-document-json
  "[[package-document]] as the checked-in file's exact bytes."
  []
  (str (json/write-json-str (package-document) {:indent-str "  "}) "\n"))

(defn write-package-document!
  "Re-render [[package-document-path]]. Run me after changing the contract or the
   human-input vocabulary; `python_contract_test` is what notices you did not."
  ([] (write-package-document! package-document-path))
  ([path] (spit path (package-document-json)) path))
