(ns com.blockether.vis.contract.python-host
  "The Python host contract as DATA: `resources/vis-contract/python-host.edn`.

   Everything the `vis` Python module can ask its host to do is one entry in that
   document — the polyglot global the engine binds, how many positional arguments
   the callable takes, and what the op does when there is no Vis host at all. The
   engine derives the names it binds from here, the injected host is built from
   here, and the package's outside-the-sandbox host derives its behavior from
   here, so a new host call is added to the document and nowhere else.

   This project is `com.blockether/vis-contract` and requires NO Vis namespace, so
   an extension can compile against the declaration without the engine. Its PyPI
   half ships [[package-document]] as `vis_contract/contract.json`.

   The human-input vocabulary is the one part this project does not own:
   `internal.human-input.spec` declares it and PASSES it to [[package-document]],
   because a closed vocabulary with two definitions is exactly the bug a contract
   exists to prevent.

   The document is validated the moment it is read: a malformed contract is a
   broken build, not a runtime surprise inside somebody's extension.
   `contract.python-host-test` is what fails when a reader drifts."
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(s/def :op/name (s/and non-blank-string? #(re-matches #"[a-z][a-z0-9_]*" %)))
(s/def :op/global (s/and non-blank-string? #(re-matches #"__vis_host_[a-z0-9_]+__" %)))
(s/def :op/arity (s/int-in 1 4))
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
(s/def :contract/python-host (s/keys :req [:contract/version :contract/ops :contract/shell]))

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
  "The op names the packaged `vis` module's `_host` object answers, in document order."
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

(defn shell-vocabulary
  "The `shell` verb's lifecycle grammar: the op an options map without one means,
   the ops that SPAWN a process, and the ops that drive the handle one answered."
  []
  (:contract/shell @document))
;; ---------------------------------------------------------------------------
;; The document the PACKAGE reads
;;
;; `python/src/vis_contract/contract.json` is this document plus the human-input
;; vocabulary, rendered for a Python reader that has no EDN and no JVM. It is
;; CHECKED IN because a wheel installed from PyPI has no repository to read, and
;; GENERATED because the engine's `human-input.spec` holds the one definition of
;; that vocabulary -- `doc("human-input")` forbids a second copy, so the package
;; gets a rendering, never a transcription. `python_package_test` fails on drift
;; and names [[write-package-document!]] as the fix.

(s/def :human-input/strings
  (s/and (s/coll-of non-blank-string? :kind vector? :distinct true) not-empty))
(s/def :human-input/field-types :human-input/strings)
(s/def :human-input/text-types :human-input/strings)
(s/def :human-input/choice-types :human-input/strings)
(s/def :human-input/secret-types :human-input/strings)
(s/def :human-input/decor-types :human-input/strings)
(s/def :human-input/group-type non-blank-string?)
(s/def :human-input/group-directions :human-input/strings)
(s/def :human-input/otp (s/keys :req-un [:human-input/length :human-input/ceiling]))
(s/def :human-input/length pos-int?)
(s/def :human-input/ceiling pos-int?)
(s/def :human-input/range (s/keys :req-un [:human-input/min :human-input/max :human-input/step]))
(s/def :human-input/min number?)
(s/def :human-input/max number?)
(s/def :human-input/step number?)
(s/def :human-input/secret-handle-prefix non-blank-string?)
;; The vocabulary the ENGINE hands in. Specced here because the contract is what
;; the package trusts: a surface that drifts is caught rendering the document, not
;; by an extension author reading a field type Python has never heard of.
(s/def :contract/human-input
  (s/keys :req-un [:human-input/field-types :human-input/text-types :human-input/choice-types
                   :human-input/secret-types :human-input/decor-types :human-input/group-type
                   :human-input/group-directions :human-input/otp :human-input/range
                   :human-input/secret-handle-prefix]))

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

(defn- human-input->json
  [{:keys [field-types text-types choice-types secret-types decor-types group-type group-directions
           otp range secret-handle-prefix]
    :as vocabulary}]
  (when-not (s/valid? :contract/human-input vocabulary)
    (throw (ex-info "the human-input vocabulary handed to the contract is not one"
                    {:type :vis/contract-invalid
                     :explain (s/explain-str :contract/human-input vocabulary)})))
  (array-map "field_types" field-types
             "text_types" text-types
             "choice_types" choice-types
             "secret_types" secret-types
             "decor_types" decor-types
             "group_type" group-type
             "group_directions" group-directions
             "otp" (array-map "length" (:length otp) "ceiling" (:ceiling otp))
             "range" (array-map "min" (:min range) "max" (:max range) "step" (:step range))
             "secret_handle_prefix" secret-handle-prefix))

(defn package-document
  "The contract as `vis_contract/contract.json`: snake_case string keys, ops in
   document order, and the closed human-input vocabulary the outside host prompts
   with. `human-input` comes from the namespace that OWNS that vocabulary --
   `(com.blockether.vis.internal.human-input.spec/contract-vocabulary)`."
  [human-input]
  (array-map "version" (version)
             "ops" (mapv op->json (ops))
             "shell"
             (let [{:shell/keys [default-op spawn-ops handle-ops]} (shell-vocabulary)]
               (array-map "default_op" default-op "spawn_ops" spawn-ops "handle_ops" handle-ops))
             "human_input" (human-input->json human-input)))

(def package-document-path
  "Where the rendered document is checked in, from the repository root."
  "packages/vis-contract/python/src/vis_contract/contract.json")

(defn package-document-json
  "[[package-document]] as the checked-in file's exact bytes."
  [human-input]
  (str (json/write-json-str (package-document human-input) {:indent-str "  "}) "\n"))

(defn write-package-document!
  "Re-render [[package-document-path]]. Run me after changing the contract or the
   human-input vocabulary; `python_package_test` is what notices you did not."
  ([human-input] (write-package-document! human-input package-document-path))
  ([human-input path] (spit path (package-document-json human-input)) path))
