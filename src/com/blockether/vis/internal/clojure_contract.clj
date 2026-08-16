(ns com.blockether.vis.internal.clojure-contract
  "The Clojure host contract as DATA: `resources/vis-contract/clojure-host.edn`.

   `com.blockether.vis.internal.python-contract` declares what a PYTHON extension may
   ask the host for. This is its symmetric half for CLOJURE extensions, which have no
   sandbox at all: an extension is a deps project with the whole engine on its
   classpath, so the facade `com.blockether.vis.core` is a promise nothing enforced.

   The document names the facade, where extension code lives, and the FROZEN set of
   internal namespaces extensions already require past it. This namespace reads and
   validates the document and re-derives the real coupling from the tree, so
   `clojure_contract_test` can fail on drift in either direction."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- namespace-symbol?
  "A namespace NAME is a simple symbol with dots in it — `qualified-symbol?` asks for
   a slash and would refuse every entry in this document."
  [x]
  (and (symbol? x) (nil? (namespace x)) (str/includes? (str x) ".")))

(s/def :contract/facade namespace-symbol?)
(s/def :contract/extension-roots (s/and (s/coll-of string? :kind vector? :distinct true) not-empty))
(s/def :debt/production (s/coll-of namespace-symbol? :kind set?))
(s/def :debt/test (s/coll-of namespace-symbol? :kind set?))
(s/def :contract/internal-debt (s/keys :req [:debt/production :debt/test]))
(s/def :contract/version pos-int?)
(s/def :contract/clojure-host
  (s/keys :req [:contract/version :contract/facade :contract/extension-roots
                :contract/internal-debt]))

(def ^:private resource-path "vis-contract/clojure-host.edn")

(def ^:private document
  "The parsed, validated contract, read from the classpath like its Python twin."
  (delay
    (let
      [resource
       (io/resource resource-path)

       _
       (when-not resource
         (throw (ex-info (str "the Clojure host contract is missing from the classpath: "
                              resource-path)
                         {:type :vis/contract-missing :resource resource-path})))

       parsed
       (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/clojure-host parsed)
        (throw (ex-info (str resource-path " is not a valid Clojure host contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/clojure-host parsed)})))
      parsed)))

(defn facade "The one namespace an extension is meant to require." [] (:contract/facade @document))

(defn extension-roots
  "Directories, relative to the repository root, that hold extension projects."
  []
  (:contract/extension-roots @document))

(defn internal-debt
  "The frozen coupling: `{:debt/production #{ns} :debt/test #{ns}}`."
  []
  (:contract/internal-debt @document))

(defn version
  "The contract version. Bumped when the shape of the document changes."
  []
  (:contract/version @document))

;; ---------------------------------------------------------------------------
;; What the TREE actually does
;;
;; The engine has no runtime hook to ask "which namespaces did this extension
;; require" -- an extension is compiled by its own deps project. So the coupling is
;; read off the source: every `[com.blockether.vis.internal.x ...]` libspec, which is
;; the only shape the repository's own style writes a require in.

(def ^:private source-extensions #{"clj" "cljc" "cljs"})

(def ^:private libspec-pattern #"\[com\.blockether\.vis\.internal\.([a-zA-Z0-9.*+!?<>=_-]+)")

(defn- source-file?
  [^java.io.File file]
  (and (.isFile file)
       (contains? source-extensions
                  (str/lower-case (or (second (re-find #"\.([^.]+)$" (.getName file))) "")))))

(defn- files-under
  [^java.io.File dir]
  (mapcat (fn [^java.io.File file]
            (cond (.isDirectory file) (if (contains? #{"target" "classes" ".cpcache"}
                                                     (.getName file))
                                        []
                                        (files-under file))
                  (source-file? file) [file]
                  :else []))
          (or (.listFiles dir) [])))

(defn- scope-of
  "`:debt/production` for a file under a `src` directory, `:debt/test` for one under
   `test`, nil for anything else (dev scratch, generated code)."
  [^java.io.File file]
  (let [path (str/replace (.getPath file) "\\" "/")]
    (cond (str/includes? path "/src/") :debt/production
          (str/includes? path "/test/") :debt/test
          :else nil)))

(defn internal-requires
  "The internal namespaces extensions under `root` really require, as
   `{:debt/production {ns #{path}} :debt/test {ns #{path}}}`. Every namespace is a
   symbol; the paths are what a failing test names as the place to fix."
  [root]
  (reduce (fn [acc ^java.io.File file]
            (if-let [scope (scope-of file)]
              (let [text (try (slurp file) (catch Exception _ ""))]
                (reduce (fn [acc [_ nsname]]
                          (update-in acc
                                     [scope (symbol (str "com.blockether.vis.internal." nsname))]
                                     (fnil conj #{})
                                     (.getPath file)))
                        acc
                        (re-seq libspec-pattern text)))
              acc))
          {:debt/production {} :debt/test {}}
          (mapcat #(files-under (io/file root %)) (extension-roots))))
