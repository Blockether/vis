(ns com.blockether.vis.ext.bridge.languages.clojure
  "Clojure extractor backed by external `clojure-lsp dump`.

   The external process path avoids classpath/protocol clashes in the Vis JVM
   while reusing clojure-lsp/clj-kondo's mature semantic analysis."
  (:require
   [babashka.process :as process]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.bridge.schema :as schema])
  (:import
   (java.io File)
   (java.net URI)))

(def ^:private default-analysis-edn "{:type :project-only}")
(def ^:private default-output-edn "{:format :edn :filter-keys [:source-paths :analysis :dep-graph]}")

(defn command
  "Return the clojure-lsp command Bridge should invoke."
  ([] (command nil))
  ([opts]
   (or (:command opts)
     (some-> (System/getenv "BRIDGE_CLOJURE_LSP_CMD") str/trim not-empty)
     "clojure-lsp")))

(defn- sh
  [& args]
  (apply process/sh {:out :string :err :string :continue true} args))

(defn executable-status
  "Check whether external `clojure-lsp` is available. Returns data suitable for
   doctor output and `bridge/clojure-lsp-status`."
  ([] (executable-status nil))
  ([opts]
   (let [cmd (command opts)]
     (try
       (let [which (sh "command" "-v" cmd)]
         (if (zero? (:exit which))
           (let [version (sh cmd "--version")]
             {:available? true
              :command cmd
              :path (str/trim (:out which))
              :version (str/trim (or (:out version) ""))
              :exit (:exit version)
              :stderr (str/trim (or (:err version) ""))})
           {:available? false
            :command cmd
            :stderr (str/trim (or (:err which) ""))
            :exit (:exit which)}))
       (catch Throwable t
         {:available? false
          :command cmd
          :error (or (ex-message t) (str t))})))))

(defn available?
  "True when external `clojure-lsp` can be invoked."
  ([] (available? nil))
  ([opts]
   (true? (:available? (executable-status opts)))))

(defn- root-file
  ^File [project-root]
  (.getCanonicalFile (io/file (or project-root "."))))

(defn- uri->path
  [project-root uri]
  (try
    (let [root-path (.toPath (root-file project-root))
          file-path (.toPath (io/file (URI. (str uri))))]
      (str (.relativize root-path file-path)))
    (catch Throwable _
      (str uri))))

(defn- ns-qname [x]
  (str (:name x)))

(defn- var-qname [x]
  (str (:ns x) "/" (:name x)))

(defn- usage-target [x]
  (when (and (:to x) (:name x))
    (str (:to x) "/" (:name x))))

(defn- symbol-kind [x]
  (let [defined-by (str (:defined-by x))]
    (cond
      (str/ends-with? defined-by "/defn") "function"
      (str/ends-with? defined-by "/defmacro") "macro"
      (str/ends-with? defined-by "/defprotocol") "protocol"
      (str/ends-with? defined-by "/defrecord") "record"
      :else "var")))

(defn- namespace-node [project-root x]
  (schema/node
    {:kind :module
     :language "clojure"
     :name (str (:name x))
     :qualified-name (ns-qname x)
     :path (uri->path project-root (:uri x))
     :line-start (:row x)
     :line-end (or (:end-row x) (:row x))
     :metadata {:bucket (:bucket x)}}))

(defn- var-node [project-root x]
  (schema/node
    {:kind :symbol
     :language "clojure"
     :name (str (:name x))
     :qualified-name (var-qname x)
     :path (uri->path project-root (:uri x))
     :line-start (:row x)
     :line-end (or (:end-row x) (:row x))
     :visibility (if (:private x) :private :public)
     :metadata (assoc (select-keys x [:defined-by :fixed-arities :varargs-min-arity :test :macro :deprecated])
                 :symbol-kind (symbol-kind x))}))

(defn- requires-edge [project-root x]
  (schema/edge
    {:edge-kind :imports
     :source (str (:from x))
     :target (str (:name x))
     :path (uri->path project-root (:uri x))
     :language "clojure"
     :line (:row x)
     :resolved? true
     :metadata (select-keys x [:alias :refer])}))

(defn- usage-edge [project-root x]
  (when-let [target (usage-target x)]
    (schema/edge
      {:edge-kind :uses
       :source (str (:from x))
       :target target
       :path (uri->path project-root (:uri x))
       :language "clojure"
       :line (:row x)
       :column (:col x)
       :resolved? (not (:external? x))
       :metadata (select-keys x [:name :to :refer :alias :arity])})))

(defn- dep-edge [ns-name dep-name dep-count]
  (schema/edge
    {:edge-kind :imports
     :source (str ns-name)
     :target (str dep-name)
     :path "<clojure-lsp-dep-graph>"
     :language "clojure"
     :resolved? true
     :metadata {:count dep-count
                :source :clojure-lsp/dep-graph}}))

(defn- file-analysis->facts
  [project-root [_uri file-analysis]]
  {:nodes (vec (concat
                 (map #(namespace-node project-root %) (:namespace-definitions file-analysis))
                 (map #(var-node project-root %) (:var-definitions file-analysis))))
   :edges (vec (concat
                 (map #(requires-edge project-root %) (:namespace-usages file-analysis))
                 (keep #(usage-edge project-root %) (:var-usages file-analysis))))})

(defn- dep-graph-edges [dep-graph]
  (vec
    (mapcat (fn [[ns-name {:keys [dependencies]}]]
              (map (fn [[dep-name dep-count]]
                     (dep-edge ns-name dep-name dep-count))
                dependencies))
      dep-graph)))

(defn dump-project
  "Run external `clojure-lsp dump --raw` and return parsed EDN.

   Opts: `:project-root`, `:command`, `:analysis`, `:output`, `:timeout-ms`.
   `:analysis` and `:output` may be EDN strings or maps."
  ([] (dump-project nil))
  ([opts]
   (let [opts (or opts {})
         cmd (command opts)
         project-root (str (.getPath (root-file (:project-root opts))))
         analysis (if (string? (:analysis opts))
                    (:analysis opts)
                    (pr-str (or (:analysis opts) (edn/read-string default-analysis-edn))))
         output (if (string? (:output opts))
                  (:output opts)
                  (pr-str (or (:output opts) (edn/read-string default-output-edn))))
         result (process/sh {:out :string
                             :err :string
                             :continue true
                             :timeout (or (:timeout-ms opts) 120000)}
                  cmd "dump" "--raw"
                  "--project-root" project-root
                  "--analysis" analysis
                  "--output" output)]
     (if (zero? (:exit result))
       (edn/read-string (:out result))
       (throw (ex-info "clojure-lsp dump failed"
                {:type :bridge.clojure-lsp/dump-failed
                 :command cmd
                 :project-root project-root
                 :exit (:exit result)
                 :stdout (:out result)
                 :stderr (:err result)}))))))

(defn extract-project
  "Extract normalized Bridge facts for a Clojure project using external
   `clojure-lsp dump`."
  ([] (extract-project nil))
  ([opts]
   (let [opts (or opts {})
         project-root (:project-root opts)
         dump (or (:dump opts) (dump-project opts))
         file-facts (map #(file-analysis->facts project-root %) (:analysis dump))
         nodes (vec (mapcat :nodes file-facts))
         edges (vec (concat (mapcat :edges file-facts)
                      (dep-graph-edges (:dep-graph dump))))]
     (schema/extract-result
       {:nodes nodes
        :edges edges
        :diagnostics []
        :stats {:language "clojure"
                :project-root (str (.getPath (root-file project-root)))
                :source-path-count (count (:source-paths dump))
                :namespace-count (count (:dep-graph dump))
                :node-count (count nodes)
                :edge-count (count edges)
                :backend :clojure-lsp/external-cli}}))))
