(ns com.blockether.vis.ext.lang-clojure.lsp
  "clojure-lsp planning tools for the `z/` alias.

   These tools intentionally expose dry-run / plan surfaces first:
   diagnostics, rename edits, and clean-ns edits. They run against files on
   disk and do not need the target application JVM to be the same process."
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.api :as lsp]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md])
  (:import
   (java.io File StringWriter)
   (java.net URI)))

(defn- safe-path
  ^File [p]
  (let [cwd        (fs/cwd)
        raw-path   (fs/path (str p))
        resolved   (if (.isAbsolute raw-path)
                     raw-path
                     (fs/path cwd (str p)))
        normalized (.normalize (.toAbsolutePath resolved))
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure.lsp/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- root-file
  ([] (root-file nil))
  ([opts]
   (safe-path (or (:project-root opts) "."))))

(defn- rel-path
  [p]
  (let [cwd (.normalize (.toAbsolutePath (fs/path (fs/cwd))))
        f   (if (instance? File p) p (File. (str p)))
        raw (.toPath ^File f)
        abs (.normalize (.toAbsolutePath (if (.isAbsolute raw)
                                           raw
                                           (fs/path (fs/cwd) (str p)))))]
    (if (.startsWith abs cwd)
      (str (.relativize cwd abs))
      (str p))))

(defn- uri->path
  [uri]
  (try
    (rel-path (File. (URI. uri)))
    (catch Throwable _
      (str uri))))

(defn- normalize-edit
  [{:keys [uri] :as edit}]
  (cond-> edit
    uri (assoc :path (uri->path uri))))

(defn- normalize-diagnostic
  [uri diagnostic]
  (assoc diagnostic :path (uri->path uri)))

(defn- normalize-diagnostics
  [diagnostics]
  (->> diagnostics
    (mapcat (fn [[uri rows]]
              (map #(normalize-diagnostic uri %) rows)))
    vec))

(defn- capture-lsp
  [f]
  (let [out (StringWriter.)]
    (binding [*out* out
              *err* out]
      (let [result (f)]
        {:result result
         :output (str out)}))))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- tool-success
  [{:keys [op result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result result
       :info (merge {:op op
                     :target {:kind :project
                              :resolved (rel-path ".")}
                     :started-at-ms t
                     :finished-at-ms t
                     :duration-ms 0}
               info)})))

(defn- tool-failure-on-error
  [op]
  (fn [err _env _f _args]
    (let [t (now-ms)]
      {:result (extension/failure
                 {:result nil
                  :info {:op op
                         :target {:kind :project
                                  :resolved (rel-path ".")}
                         :started-at-ms t
                         :finished-at-ms t
                         :duration-ms 0}
                  :throwable err})})))

(defn- option-files
  [opts]
  (some->> (:filenames opts)
    (mapv safe-path)))

(defn- diagnostics
  ([] (diagnostics nil))
  ([opts]
   (let [opts (or opts {})
         {:keys [result output]} (capture-lsp
                                   #(lsp/diagnostics
                                      (cond-> {:project-root (root-file opts)}
                                        (:namespace opts) (assoc :namespace (:namespace opts))
                                        (:filenames opts) (assoc :filenames (option-files opts))
                                        (:settings opts) (assoc :settings (:settings opts)))))]
     (tool-success
       {:op :z/diagnostics
        :result (normalize-diagnostics (:diagnostics result))
        :info {:result-code (:result-code result)
               :diagnostic-count (count (normalize-diagnostics (:diagnostics result)))
               :lsp-output output}}))))

(defn- rename-plan
  [from to & [opts]]
  (let [opts (or opts {})
        {:keys [result output]} (capture-lsp
                                  #(lsp/rename!
                                     (cond-> {:project-root (root-file opts)
                                              :from from
                                              :to to
                                              :dry? true}
                                       (:settings opts) (assoc :settings (:settings opts)))))
        edits (mapv normalize-edit (:edits result))]
    (tool-success
      {:op :z/rename-plan
       :result {:from from
                :to to
                :edits edits
                :changed-paths (->> edits (map :path) distinct vec)}
       :info {:result-code (:result-code result)
              :edit-count (count edits)
              :lsp-output output}})))

(defn- clean-ns-plan
  ([] (clean-ns-plan nil))
  ([opts]
   (let [opts (or opts {})
         {:keys [result output]} (capture-lsp
                                   #(lsp/clean-ns!
                                      (cond-> {:project-root (root-file opts)
                                               :dry? true}
                                        (:namespace opts) (assoc :namespace (:namespace opts))
                                        (:filenames opts) (assoc :filenames (option-files opts))
                                        (:ns-exclude-regex opts) (assoc :ns-exclude-regex (:ns-exclude-regex opts))
                                        (:settings opts) (assoc :settings (:settings opts)))))
         edits (mapv normalize-edit (:edits result))]
     (tool-success
       {:op :z/clean-ns-plan
        :result {:edits edits
                 :changed-paths (->> edits (map :path) distinct vec)}
        :info {:result-code (:result-code result)
               :edit-count (count edits)
               :lsp-output output}}))))

(defn- render-tool-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (md/p "Tool" (md/code (get-in tool-result [:info :op])) "failed:" (get-in tool-result [:error :message]))
    (md/join
      (md/p (md/code (get-in tool-result [:info :op])) "returned" (if (sequential? (:result tool-result))
                                                                    (count (:result tool-result))
                                                                    "a") "result(s).")
      (md/code-block "clojure" (pr-str (:result tool-result))))))

(defn- lsp-symbol
  [sym f doc arglists examples]
  (vis/symbol sym f
    {:doc doc
     :arglists arglists
     :examples examples
     :result-spec ::extension/tool-result
     :render-fn render-tool-result
     :on-error-fn (tool-failure-on-error (keyword "z" (name sym)))}))

(def diagnostics-symbol
  (lsp-symbol 'diagnostics diagnostics
    "Return clojure-lsp diagnostics for the project or selected files. Opts: {:project-root p, :filenames [...], :namespace [...], :settings {...}}."
    '([] [opts])
    ["(z/diagnostics)"
     "(z/diagnostics {:filenames [\"src/foo.clj\"]})"]))

(def rename-plan-symbol
  (lsp-symbol 'rename-plan rename-plan
    "Dry-run clojure-lsp semantic rename. Returns changed paths and old/new text edits; does not write."
    '([from to] [from to opts])
    ["(z/rename-plan 'old.ns/foo 'old.ns/bar)"]))

(def clean-ns-plan-symbol
  (lsp-symbol 'clean-ns-plan clean-ns-plan
    "Dry-run clojure-lsp clean-ns. Returns changed paths and old/new text edits; does not write."
    '([] [opts])
    ["(z/clean-ns-plan {:filenames [\"src/foo.clj\"]})"]))

(def symbols
  [diagnostics-symbol rename-plan-symbol clean-ns-plan-symbol])
