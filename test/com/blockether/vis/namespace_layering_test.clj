(ns com.blockether.vis.namespace-layering-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.extension :as ext]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.io File PushbackReader]))

(def ^:private facade 'com.blockether.vis.core)

(def ^:private runtime-facade-lookup
  "A quoted facade reference handed to a runtime loader, e.g.
   `(requiring-resolve 'com.blockether.vis.core/current-config)`."
  #"\((?:requiring-resolve|resolve|ns-resolve|find-ns|the-ns|require)\s+'\[?com\.blockether\.vis\.core[\s/\]\)]")

(defn- internal-sources
  []
  (->> (file-seq (io/file "src/com/blockether/vis/internal"))
       (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".clj")))
       sort))

(defn- ns-form
  [^File file]
  (with-open [reader (PushbackReader. (io/reader file))]
    (binding [*read-eval* false]
      (read reader))))

(defn- libspec-names
  "The namespaces one `:require` argument names, prefix lists expanded."
  [spec]
  (cond (symbol? spec) [spec]
        (and (sequential? spec) (symbol? (first spec)))
        (let [[head & more] spec]
          (if (or (empty? more) (keyword? (first more)))
            [head]
            (for [child more
                  :let [child-name (if (sequential? child) (first child) child)]]

              (symbol (str head "." child-name)))))
        :else []))

(defn- required-namespaces
  [form]
  (for [clause
        (rest form)

        :when (and (seq? clause) (= :require (first clause)))
        spec
        (rest clause)

        ns-sym
        (libspec-names spec)]

    ns-sym))

(deftest internal-namespaces-never-load-the-facade-test
  (testing "engine namespaces call owners or the authoring API, never the public facade"
    (let [offenders (->> (internal-sources)
                         (keep (fn [^File file]
                                 (when (or (some #{facade} (required-namespaces (ns-form file)))
                                           (re-find runtime-facade-lookup (slurp file)))
                                   (.getPath file))))
                         vec)]
      (is (= [] offenders)
          (str "Internal namespaces loading " facade ": " (str/join ", " offenders))))))

(def ^:private authored
  (ext/extension {:ext/name "test.layering-authoring"
                  :ext/description "Authoring API fixture."
                  :ext/prompt-fn "Probe."}))

(def ^:private facade-authored
  (vis/extension
    {:ext/name "test.layering-facade" :ext/description "Facade fixture." :ext/prompt-fn "Probe."}))

(deftest facade-re-exports-the-authoring-api-test
  (testing "every authoring function is the same value through either namespace"
    (doseq [sym '[symbol value render-prompt register-extension! register-toggle!]]
      (is (identical? @(ns-resolve 'com.blockether.vis.extension sym) @(ns-resolve facade sym))
          (str sym))))
  (testing "both `extension` macros stamp the namespace that declared the extension"
    (is (= '[com.blockether.vis.namespace-layering-test] (:ext/source-nses authored)))
    (is (= '[com.blockether.vis.namespace-layering-test] (:ext/source-nses facade-authored)))))
