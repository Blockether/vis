(ns com.blockether.vis.contract.clojure-host
  "The executable dependency contract for Clojure hosts and first-party consumers.

   `resources/vis-contract/clojure-host.edn` names the present facade, source roots,
   and the FROZEN debt on the final SDK-only graph. Debt is counted by source file:
   adding another forbidden require or another hand-written wire literal changes the
   count and fails the suite; deleting one requires shrinking the document.

   The scanner lives in `com.blockether/vis-contract`, not the engine. It reads only
   declarations and source text, requires no Vis implementation namespace, and can
   therefore guard the same boundary from an extension repository or an SDK build."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import (java.io File PushbackReader)
           (java.nio.file Path)))

(set! *warn-on-reflection* true)

(defn- namespace-symbol?
  "A namespace NAME is a simple symbol with dots in it — `qualified-symbol?` asks for
   a slash and would refuse every entry in this document."
  [x]
  (and (symbol? x) (nil? (namespace x)) (str/includes? (str x) ".")))

(s/def :contract/facade namespace-symbol?)
(s/def :contract/extension-roots (s/and (s/coll-of string? :kind vector? :distinct true) not-empty))
(s/def :contract/javascript-wire-files
  (s/and (s/coll-of string? :kind vector? :distinct true) not-empty))
(s/def :debt/count pos-int?)
(s/def :debt/namespaces (s/map-of namespace-symbol? :debt/count))
(s/def :debt/production :debt/namespaces)
(s/def :debt/test :debt/namespaces)
(s/def :debt/layers :debt/namespaces)
(s/def :contract/internal-debt (s/keys :req [:debt/production :debt/test :debt/layers]))
(s/def :debt/wire-kind #{:event :header :import :protocol :route})
(s/def :debt/wire-value (s/and string? not-empty))
(s/def :debt/wire-key (s/tuple :debt/wire-kind :debt/wire-value))
(s/def :debt/wire-counts (s/map-of :debt/wire-key :debt/count))
(s/def :debt/javascript :debt/wire-counts)
(s/def :debt/python :debt/wire-counts)
(s/def :contract/wire-debt (s/keys :req [:debt/javascript :debt/python]))
(s/def :contract/version pos-int?)
(s/def :contract/clojure-host
  (s/keys :req [:contract/version :contract/facade :contract/extension-roots
                :contract/javascript-wire-files :contract/internal-debt :contract/wire-debt]))

(def ^:private resource-path "vis-contract/clojure-host.edn")

(def ^:private document
  "The parsed, validated contract, read from the classpath like its Python twin."
  (delay
    (let [resource
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

(defn facade
  "The temporary facade named by the current Clojure host contract."
  []
  (:contract/facade @document))

(defn extension-roots
  "Directories, relative to the repository root, that hold extension projects."
  []
  (:contract/extension-roots @document))

(defn javascript-wire-files
  "Companion protocol modules whose hand-written literals are frozen for SDK extraction."
  []
  (:contract/javascript-wire-files @document))

(defn internal-debt
  "Forbidden Clojure dependencies counted by source file in each migration scope."
  []
  (:contract/internal-debt @document))

(defn wire-debt
  "Hand-written JavaScript/Python protocol literals and forbidden imports, by file count."
  []
  (:contract/wire-debt @document))

(defn version
  "The contract version. Bumped when the shape of the document changes."
  []
  (:contract/version @document))

;; ---------------------------------------------------------------------------
;; What the Clojure tree actually does

(def ^:private ignored-directories #{".cpcache" "classes" "node_modules" "target"})
(def ^:private clojure-source-extensions #{"clj" "cljc" "cljs"})
(def ^:private clojure-source-roots
  ["src" "packages/vis-contract/src" "packages/vis-sdk/clojure/src" "extensions"])

(defn- file-extension
  [^File file]
  (some->> (.getName file)
           (re-find #"\.([^.]+)$")
           second
           str/lower-case))

(defn- files-under
  [^File file wanted?]
  (cond (and (.isDirectory file) (not (contains? ignored-directories (.getName file))))
        (mapcat #(files-under % wanted?) (or (.listFiles file) []))
        (and (.isFile file) (wanted? file)) [file]
        :else []))

(defn- relative-path
  [root ^File file]
  (let [^Path root-path
        (.toPath (.getCanonicalFile (io/file root)))

        ^Path file-path
        (.toPath (.getCanonicalFile file))]

    (str/replace (str (.relativize root-path file-path)) "\\" "/")))

(defn- namespace-prefix?
  [prefix nsname]
  (let [candidate (str nsname)]
    (or (= prefix candidate) (str/starts-with? candidate (str prefix ".")))))

(defn- vis-namespace? [nsname] (namespace-prefix? "com.blockether.vis" nsname))
(defn- contract-namespace? [nsname] (namespace-prefix? "com.blockether.vis.contract" nsname))
(defn- core-namespace? [nsname] (namespace-prefix? "com.blockether.vis.internal.core" nsname))
(defn- server-namespace?
  [nsname]
  (namespace-prefix? "com.blockether.vis.internal.gateway.server" nsname))
(defn- sdk-namespace? [nsname] (namespace-prefix? "com.blockether.vis.sdk" nsname))

(defn- read-ns-form
  [^File file]
  (with-open [reader (PushbackReader. (io/reader file))]
    (binding [*read-eval* false]
      (read {:eof nil
             :features (if (= "cljs" (file-extension file)) #{:cljs} #{:clj})
             :read-cond :allow}
            reader))))

(defn- require-clause?
  [form]
  (and (seq? form) (contains? #{:require :require-macros} (first form))))

(defn- required-namespaces
  [ns-form]
  (->> (drop 2 ns-form)
       (filter require-clause?)
       (mapcat #(tree-seq coll? seq %))
       (filter namespace-symbol?)
       (filter vis-namespace?)
       set))

(defn- extension-owner [path] (second (re-find #"^(extensions/[^/]+/[^/]+)/(?:src|test)/" path)))

(defn- extension-scope
  [path]
  (case (second (re-find #"^extensions/[^/]+/[^/]+/(src|test)/" path))
    "src"
    :debt/production

    "test"
    :debt/test

    nil))

(defn- normalized-namespace [nsname] (str/replace (str nsname) "-" "_"))

(defn- source-record
  [root ^File file]
  (let [form (read-ns-form file)]
    (when (= 'ns (first form))
      (let [path (relative-path root file)]
        {:namespace (second form)
         :owner (extension-owner path)
         :path path
         :requires (required-namespaces form)
         :scope (extension-scope path)}))))

(defn- source-records
  [root]
  (->> clojure-source-roots
       (mapcat #(files-under (io/file root %)
                             (fn [^File file]
                               (contains? clojure-source-extensions (file-extension file)))))
       (keep #(source-record root %))))

(defn- allowed-layer-dependency?
  [source dependency]
  (cond (contract-namespace? source) (contract-namespace? dependency)
        (core-namespace? source) (or (contract-namespace? dependency)
                                     (core-namespace? dependency)
                                     (= 'com.blockether.vis.internal.util dependency))
        (server-namespace? source) (or (contract-namespace? dependency)
                                       (core-namespace? dependency)
                                       (server-namespace? dependency)
                                       (= 'com.blockether.vis.internal.util dependency))
        (sdk-namespace? source) (or (contract-namespace? dependency) (sdk-namespace? dependency))
        :else true))

(defn dependency-violations
  "Forbidden Clojure dependencies under `root`, as
   `{:debt/production {dependency #{paths}} :debt/test ... :debt/layers ...}`.
   Consumer code may require its own pack or the Clojure SDK. Contract, Core,
   gateway-server and SDK namespaces obey the final layer graph immediately."
  [root]
  (let [records
        (source-records root)

        owner-by-namespace
        (into {}
              (keep (fn [{:keys [namespace owner]}]
                      (when owner [(normalized-namespace namespace) owner])))
              records)]

    (reduce (fn [found {:keys [namespace owner path requires scope]}]
              (reduce (fn [found dependency]
                        (let [same-owner?
                              (and owner
                                   (= owner
                                      (get owner-by-namespace (normalized-namespace dependency))))

                              consumer-violation?
                              (and scope (not same-owner?) (not (sdk-namespace? dependency)))

                              layer-violation?
                              (not (allowed-layer-dependency? namespace dependency))]

                          (cond-> found
                            consumer-violation?
                            (update-in [scope dependency] (fnil conj #{}) path)

                            layer-violation?
                            (update-in [:debt/layers dependency] (fnil conj #{}) path))))
                      found
                      requires))
            {:debt/production {} :debt/test {} :debt/layers {}}
            records)))

(defn- location-counts
  [locations]
  (into {}
        (map (fn [[scope dependencies]]
               [scope
                (into {}
                      (map (fn [[dependency paths]]
                             [dependency (count paths)]))
                      dependencies)]))
        locations))

(defn dependency-debt
  "[[dependency-violations]] reduced to the exact source-file counts frozen in the document."
  [root]
  (location-counts (dependency-violations root)))

;; ---------------------------------------------------------------------------
;; What JavaScript and Python consumers spell by hand

(def ^:private javascript-source-extensions #{"js" "jsx" "ts" "tsx"})
(def ^:private javascript-import-pattern #"(?:from\s+|import\s*)[\"']([^\"']+)[\"']")
(def ^:private python-import-pattern
  #"(?m)^\s*(?:from\s+([A-Za-z_]\w*(?:\.\w+)*)\s+import|import\s+([A-Za-z_]\w*(?:\.\w+)*))")
(def ^:private route-pattern #"[\"'`](/(?:healthz|v1|tui)(?:[^\"'`\s])*)[\"'`]")
(def ^:private event-pattern
  #"[\"'`]((?:turn|session|queue|subscription|view|content|block|iteration|provider|voice|speech|fleet)\.[a-z0-9_.-]+)[\"'`]")
(def ^:private header-pattern
  #"(?i)[\"'`]((?:authorization|content-type|accept|last-event-id|x-vis-[a-z0-9-]+))[\"'`]")
(def ^:private protocol-pattern #"\b(APP_PROTOCOL|MIN_GATEWAY_PROTOCOL)\s*=\s*(\d+)")

(defn- protocol-literals
  [text]
  (concat (map (fn [[_ value]]
                 [:route value])
               (re-seq route-pattern text))
          (map (fn [[_ value]]
                 [:event value])
               (re-seq event-pattern text))
          (map (fn [[_ value]]
                 [:header (str/lower-case value)])
               (re-seq header-pattern text))
          (map (fn [[_ name value]]
                 [:protocol (str name "=" value)])
               (re-seq protocol-pattern text))))

(defn- javascript-values
  [text]
  (concat (protocol-literals text)
          (for [[_ import]
                (re-seq javascript-import-pattern text)

                :when (or (= "@blockether/vis-contract" import)
                          (str/starts-with? import "@blockether/vis-contract/"))]

            [:import import])))

(defn- python-values
  [path text]
  (concat (protocol-literals text)
          (when (str/starts-with? path "extensions/")
            (for [[_ from direct]
                  (re-seq python-import-pattern text)

                  :let [import
                        (or from direct)]
                  :when (or (= "vis_contract" import) (str/starts-with? import "vis_contract."))]

              [:import import]))))

(defn- add-wire-file
  [found language path values]
  (reduce (fn [found wire-key]
            (update-in found [language wire-key] (fnil conj #{}) path))
          found
          (distinct values)))

(defn consumer-wire-violations
  "Hand-written protocol literals in the named Companion modules, plus direct contract
   imports by JavaScript/Python consumers. Results retain source paths for diagnostics."
  [root]
  (let [javascript-files
        (keep (fn [path]
                (let [file (io/file root path)]
                  (when (.isFile file) [path file])))
              (javascript-wire-files))

        javascript-consumer-files
        (files-under (io/file root "apps/vis-companion/src")
                     (fn [^File file]
                       (and (contains? javascript-source-extensions (file-extension file))
                            (not (str/includes? (.getName file) ".test."))
                            (not (str/includes? (.getName file) ".stories.")))))

        python-files
        (mapcat #(files-under (io/file root %)
                              (fn [^File file]
                                (= "py" (file-extension file))))
                ["extensions" "packages/vis-agent/src" "packages/vis-sdk/python/src"])

        with-javascript-literals
        (reduce (fn [found [path ^File file]]
                  (add-wire-file found :debt/javascript path (javascript-values (slurp file))))
                {:debt/javascript {} :debt/python {}}
                javascript-files)

        with-javascript-imports
        (reduce (fn [found ^File file]
                  (let [path
                        (relative-path root file)

                        imports
                        (remove #(not= :import (first %)) (javascript-values (slurp file)))]

                    (add-wire-file found :debt/javascript path imports)))
                with-javascript-literals
                javascript-consumer-files)]

    (reduce (fn [found ^File file]
              (let [path (relative-path root file)]
                (add-wire-file found :debt/python path (python-values path (slurp file)))))
            with-javascript-imports
            python-files)))

(defn wire-debt-counts
  "[[consumer-wire-violations]] reduced to source-file counts for exact debt comparison."
  [root]
  (location-counts (consumer-wire-violations root)))
