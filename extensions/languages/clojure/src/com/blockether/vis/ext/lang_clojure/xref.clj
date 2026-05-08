(ns com.blockether.vis.ext.lang-clojure.xref
  "Agent-facing Clojure cross-reference tools.

   clj-xref supplies the semantic graph: who calls what, namespace deps,
   protocol implementations, dead vars, and bounded code neighborhoods.
   rewrite-clj supplies the source zipper bridge so xref rows can become
   z/patch-compatible locator rows. The public surface stays under the
   `z/` alias through SDK symbol entries in this namespace."
  (:require
   [babashka.fs :as fs]
   [clj-xref.core :as xref]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [rewrite-clj.zip :as z])
  (:import
   (java.io File)))

;; =============================================================================
;; Path/project handling
;; =============================================================================

(def ^:private source-root-names #{"src" "test"})
(def ^:private source-exts #{"clj" "cljc" "cljs"})
(def ^:private ignored-scan-dirs #{".git" ".hg" ".svn" "target" "node_modules" ".cpcache" ".clj-kondo"})
(def ^:private source-root-scan-limit 20000)

(defonce ^:private xref-state
  (atom nil))

(defn- safe-path
  ^File [p]
  (let [cwd        (workspace-context/cwd)
        raw-path   (fs/path (str p))
        resolved   (if (.isAbsolute raw-path)
                     raw-path
                     (fs/path cwd (str p)))
        normalized (.normalize (.toAbsolutePath resolved))
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure.xref/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- rel-path
  [p]
  (let [cwd-file (workspace-context/cwd)
        cwd (.normalize (.toAbsolutePath (fs/path cwd-file)))
        f   (if (instance? File p) p (File. (str p)))
        raw (.toPath ^File f)
        abs (.normalize (.toAbsolutePath (if (.isAbsolute raw)
                                           raw
                                           (fs/path cwd-file (str p)))))]
    (if (.startsWith abs cwd)
      (str (.relativize cwd abs))
      (str p))))

(defn- path-exists-dir? [p]
  (let [f (safe-path p)]
    (and (.exists f) (.isDirectory f))))

(defn- file-extension [^File f]
  (when-let [n (some-> f .getName)]
    (when-let [idx (str/last-index-of n ".")]
      (subs n (inc idx)))))

(defn- clojure-source-file? [^File f]
  (and (.isFile f)
    (contains? source-exts (file-extension f))))

(defn- scan-children [^File dir]
  (->> (or (seq (.listFiles dir)) [])
    (remove (fn [^File f]
              (and (.isDirectory f)
                (contains? ignored-scan-dirs (.getName f)))))))

(defn- source-root-with-clojure? [^File dir]
  (loop [stack (seq (scan-children dir))
         seen  0]
    (cond
      (or (nil? stack) (>= seen source-root-scan-limit)) false
      (clojure-source-file? (first stack)) true
      (.isDirectory ^File (first stack))
      (recur (concat (scan-children (first stack)) (next stack)) (inc seen))
      :else
      (recur (next stack) (inc seen)))))

(defn- discover-source-roots
  []
  (let [root (safe-path ".")]
    (loop [stack (seq (scan-children root))
           seen  0
           out   []]
      (cond
        (or (nil? stack) (>= seen source-root-scan-limit))
        (let [roots (->> out distinct sort vec)]
          (if (seq roots) roots (filterv path-exists-dir? ["src" "test"])))

        (and (.isDirectory ^File (first stack))
          (contains? source-root-names (.getName ^File (first stack)))
          (source-root-with-clojure? (first stack)))
        (recur (next stack) (inc seen) (conj out (rel-path (first stack))))

        (.isDirectory ^File (first stack))
        (recur (concat (scan-children (first stack)) (next stack)) (inc seen) out)

        :else
        (recur (next stack) (inc seen) out)))))

(defn- coerce-paths
  [paths]
  (let [paths (or (seq paths) (discover-source-roots))]
    (mapv (comp rel-path safe-path) paths)))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- tool-success
  [{:keys [op result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result result
       :info (merge {:op             op
                     :target         {:kind :project
                                      :resolved (rel-path ".")}
                     :started-at-ms  t
                     :finished-at-ms t
                     :duration-ms    0}
               info)})))

(defn- tool-failure-on-error
  [op]
  (fn [err _env _f _args]
    (let [t (now-ms)]
      {:result (extension/failure
                 {:result nil
                  :info {:op             op
                         :target         {:kind :project
                                          :resolved (rel-path ".")}
                         :started-at-ms  t
                         :finished-at-ms t
                         :duration-ms    0}
                  :throwable err})})))

;; =============================================================================
;; Analysis/cache
;; =============================================================================

(defn- analyze-db!
  [{:keys [paths kondo-config project] :as opts}]
  (let [paths (coerce-paths paths)
        started (System/nanoTime)
        db (xref/analyze paths (cond-> {}
                                 project (assoc :project project)
                                 kondo-config (assoc :kondo-config kondo-config)))
        elapsed-ms (/ (- (System/nanoTime) started) 1e6)
        state {:db db
               :paths paths
               :project project
               :analyzed-at-ms (now-ms)
               :elapsed-ms elapsed-ms
               :opts (select-keys opts [:kondo-config :project])}]
    (reset! xref-state state)
    state))

(defn- current-state
  ([] (current-state nil))
  ([opts]
   (if (or (:force? opts) (:paths opts) (nil? @xref-state))
     (analyze-db! opts)
     @xref-state)))

(defn- db [opts]
  (:db (current-state opts)))

(defn- xref-analyze!
  ([] (xref-analyze! nil))
  ([opts]
   (let [{:keys [db paths elapsed-ms analyzed-at-ms]} (analyze-db! (or opts {}))]
     (tool-success
       {:op :z/xref-analyze!
        :result {:paths paths
                 :vars (count (:vars db))
                 :refs (count (:refs db))
                 :namespaces (count (:namespaces db))
                 :elapsed-ms elapsed-ms}
        :info {:paths paths
               :analyzed-at-ms analyzed-at-ms}}))))

(defn- xref-refresh!
  ([] (xref-refresh! nil))
  ([opts]
   (xref-analyze! (assoc (or opts {}) :force? true))))

;; =============================================================================
;; Normalization
;; =============================================================================

(defn- location-keys->row-cols
  [m]
  (cond-> m
    (:file m) (assoc :path (rel-path (:file m))
                :file (rel-path (:file m)))
    (:line m) (assoc :row (:line m))
    (:end-line m) (assoc :end-row (:end-line m))))

(defn- normalize-var-row [row]
  (location-keys->row-cols row))

(defn- normalize-ref-row [row]
  (location-keys->row-cols row))

(defn- normalize-edge [[from to]]
  {:from from :to to})

(defn- bounded
  [xs opts]
  (let [limit (long (or (:limit opts) 100))
        xs    (vec xs)]
    {:rows (subvec xs 0 (min limit (count xs)))
     :total-count (count xs)
     :limit limit
     :truncated? (> (count xs) limit)}))

(defn- query-success
  [op result opts extra-prov]
  (let [{:keys [paths analyzed-at-ms]} (current-state opts)]
    (tool-success
      {:op op
       :result result
       :info (merge {:paths paths
                     :analyzed-at-ms analyzed-at-ms}
               extra-prov)})))

(defn- ref-query
  [op f sym opts]
  (let [{:keys [rows total-count limit truncated?]} (bounded (mapv normalize-ref-row (f (db opts) sym)) opts)]
    (query-success op rows opts {:symbol sym
                                 :count (count rows)
                                 :total-count total-count
                                 :limit limit
                                 :truncated? truncated?})))

(defn- who-calls
  ([sym] (who-calls sym nil))
  ([sym opts] (ref-query :z/who-calls xref/who-calls sym opts)))

(defn- calls-who
  ([sym] (calls-who sym nil))
  ([sym opts] (ref-query :z/calls-who xref/calls-who sym opts)))

(defn- who-references
  ([sym] (who-references sym nil))
  ([sym opts] (ref-query :z/who-references xref/who-references sym opts)))

(defn- who-macroexpands
  ([sym] (who-macroexpands sym nil))
  ([sym opts] (ref-query :z/who-macroexpands xref/who-macroexpands sym opts)))

(defn- who-implements
  ([sym] (who-implements sym nil))
  ([sym opts] (ref-query :z/who-implements xref/who-implements sym opts)))

(defn- who-dispatches
  ([sym] (who-dispatches sym nil))
  ([sym opts] (ref-query :z/who-dispatches xref/who-dispatches sym opts)))

(defn- ns-vars
  ([ns-sym] (ns-vars ns-sym nil))
  ([ns-sym opts]
   (let [{:keys [rows total-count limit truncated?]} (bounded (mapv normalize-var-row (xref/ns-vars (db opts) ns-sym)) opts)]
     (query-success :z/ns-vars rows opts {:namespace ns-sym
                                          :count (count rows)
                                          :total-count total-count
                                          :limit limit
                                          :truncated? truncated?}))))

(defn- ns-deps
  ([ns-sym] (ns-deps ns-sym nil))
  ([ns-sym opts]
   (let [rows (sort (xref/ns-deps (db opts) ns-sym))]
     (query-success :z/ns-deps (vec rows) opts {:namespace ns-sym
                                                :count (count rows)}))))

(defn- ns-dependents
  ([ns-sym] (ns-dependents ns-sym nil))
  ([ns-sym opts]
   (let [rows (sort (xref/ns-dependents (db opts) ns-sym))]
     (query-success :z/ns-dependents (vec rows) opts {:namespace ns-sym
                                                      :count (count rows)}))))

(defn- unused-vars
  ([] (unused-vars nil))
  ([opts]
   (let [{:keys [rows total-count limit truncated?]} (bounded (mapv normalize-var-row (xref/unused-vars (db opts) opts)) opts)]
     (query-success :z/unused-vars rows opts {:count (count rows)
                                              :total-count total-count
                                              :limit limit
                                              :truncated? truncated?}))))

(defn- call-graph
  ([sym] (call-graph sym nil))
  ([sym opts]
   (let [edges (->> (xref/call-graph (db opts) sym opts)
                 (map normalize-edge)
                 (sort-by (juxt (comp str :from) (comp str :to)))
                 vec)]
     (query-success :z/call-graph edges opts {:symbol sym
                                              :depth (or (:depth opts) 3)
                                              :direction (or (:direction opts) :outgoing)
                                              :count (count edges)}))))

(defn- apropos
  ([pattern] (apropos pattern nil))
  ([pattern opts]
   (let [{:keys [rows total-count limit truncated?]} (bounded (mapv normalize-var-row (xref/apropos (db opts) pattern)) opts)]
     (query-success :z/apropos rows opts {:pattern (str pattern)
                                          :count (count rows)
                                          :total-count total-count
                                          :limit limit
                                          :truncated? truncated?}))))

;; =============================================================================
;; Zipper locator bridge
;; =============================================================================

(defn- span<=
  [[[sr sc] [er ec]] [[outer-sr outer-sc] [outer-er outer-ec]]]
  (and (or (> sr outer-sr)
         (and (= sr outer-sr) (>= sc outer-sc)))
    (or (< er outer-er)
      (and (= er outer-er) (<= ec outer-ec)))))

(defn- locator-row
  [zloc]
  (when (z/sexpr-able? zloc)
    (try
      (let [v (z/sexpr zloc)]
        {:tag     (z/tag zloc)
         :value   v
         :locator (pr-str v)
         :source  (z/string zloc)
         :span    (z/position-span zloc)})
      (catch Throwable _ nil))))

(defn- source-locators
  [source]
  (loop [loc (z/of-string source {:track-position? true})
         out []]
    (if (z/end? loc)
      out
      (recur (z/next loc)
        (if-let [row (locator-row loc)]
          (conj out row)
          out)))))

(defn- with-locator-row-context
  [path rows]
  (mapv (fn [idx row]
          (assoc row
            :path path
            :index idx))
    (range)
    rows))

(defn- row-span [row]
  (when-let [row-num (or (:line row) (:row row))]
    (let [col-num (or (:col row) 1)]
      [[row-num col-num]
       [(or (:end-line row) (:end-row row) row-num)
        (or (:end-col row) col-num)]])))

(defn- symbol-local-name [sym]
  (when (symbol? sym)
    (symbol (name sym))))

(defn- wanted-symbols [row]
  (->> [(:name row)
        (:local-name row)
        (:to row)
        (:from row)
        (:method row)
        (symbol-local-name (:name row))
        (symbol-local-name (:to row))
        (symbol-local-name (:from row))]
    (filter symbol?)
    set))

(defn- locator-for-row*
  [row]
  (let [path       (rel-path (or (:path row) (:file row)))
        f          (safe-path path)
        rows       (with-locator-row-context path (source-locators (slurp f)))
        span       (row-span row)
        wanted     (wanted-symbols row)
        candidates (cond->> rows
                     span (filter #(span<= (:span %) span))
                     (seq wanted) (filter #(contains? wanted (:value %))))]
    (when-let [hit (first candidates)]
      (assoc hit :xref-row (select-keys row [:kind :from :to :name :local-name :method :line :col :end-line :end-col :row :end-row :path :file])))))

(defn- locator-for-ref
  [row]
  (if-let [hit (locator-for-row* row)]
    (query-success :z/locator-for-ref hit nil {:row (select-keys row [:kind :from :to :name :line :col :path :file])})
    (throw (ex-info "Could not resolve xref row to a zipper locator"
             {:type :ext.lang-clojure.xref/locator-not-found
              :row row}))))

(defn- locators-for-symbol
  ([sym] (locators-for-symbol sym nil))
  ([sym opts]
   (let [d          (db opts)
         definition (get (:vars-by-name d) sym)
         refs       (xref/who-references d sym)
         rows       (cond-> [] definition (conj definition) true (into refs))
         locators   (keep locator-for-row* rows)
         {:keys [rows total-count limit truncated?]} (bounded locators opts)]
     (query-success :z/locators-for-symbol rows opts {:symbol sym
                                                      :count (count rows)
                                                      :total-count total-count
                                                      :limit limit
                                                      :truncated? truncated?}))))

(defn- definition
  ([sym] (definition sym nil))
  ([sym opts]
   (let [row     (some-> (get (:vars-by-name (db opts)) sym) normalize-var-row)
         locator (when row (locator-for-row* row))]
     (query-success :z/definition
       (when row
         {:symbol sym
          :definition row
          :locator locator})
       opts
       {:symbol sym
        :found? (boolean row)}))))

(defn- call-sites
  ([sym] (call-sites sym nil))
  ([sym opts]
   (let [refs (mapv normalize-ref-row (xref/who-calls (db opts) sym))
         rows (mapv (fn [row]
                      (assoc row :locator (locator-for-row* row)))
                refs)
         {:keys [rows total-count limit truncated?]} (bounded rows opts)]
     (query-success :z/call-sites rows opts {:symbol sym
                                             :count (count rows)
                                             :total-count total-count
                                             :limit limit
                                             :truncated? truncated?}))))

;; =============================================================================
;; Agent context selection
;; =============================================================================

(defn- context-for
  ([sym] (context-for sym nil))
  ([sym opts]
   (let [d            (db opts)
         definition   (some-> (get (:vars-by-name d) sym) normalize-var-row)
         callers      (mapv normalize-ref-row (xref/who-calls d sym))
         callees      (mapv normalize-ref-row (xref/calls-who d sym))
         callee-files (into #{} (comp (map :to)
                                  (map #(get (:vars-by-name d) %))
                                  (filter some?)
                                  (map :file)
                                  (map rel-path))
                        callees)
         caller-files (into #{} (comp (map :file) (filter some?) (map rel-path)) callers)
         self-file    (some-> definition :file rel-path)
         files        (->> (set/union caller-files callee-files (if self-file #{self-file} #{}))
                        sort
                        vec)]
     (query-success :z/context-for
       {:symbol sym
        :definition definition
        :files files
        :callers callers
        :callees callees}
       opts
       {:symbol sym
        :file-count (count files)
        :caller-count (count callers)
        :callee-count (count callees)}))))

;; =============================================================================
;; Rendering and symbol entries
;; =============================================================================

(defn- render-tool-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (md/p "Tool" (md/code (get-in tool-result [:info :op])) "failed:" (get-in tool-result [:error :message]))
    (md/join
      (md/p (md/code (get-in tool-result [:info :op])) "returned" (if (sequential? (:result tool-result))
                                                                    (count (:result tool-result))
                                                                    "a") "result(s).")
      (md/code-block "clojure" (pr-str (:result tool-result))))))

(defn- xref-symbol
  [sym f doc arglists examples]
  (vis/symbol sym f
    {:doc doc
     :arglists arglists
     :examples examples
     :result-spec ::extension/tool-result
     :render-fn render-tool-result
     :on-error-fn (tool-failure-on-error (keyword "z" (name sym)))}))

(def xref-analyze-symbol
  (xref-symbol 'xref-analyze! xref-analyze!
    "Analyze Clojure source roots with clj-xref and cache the semantic graph for later z/ xref queries. Opts: {:paths [...], :kondo-config {...}, :project string}."
    '([] [opts])
    ["(z/xref-analyze!)"
     "(z/xref-analyze! {:paths [\"src\" \"test\"]})"]))

(def xref-refresh-symbol
  (xref-symbol 'xref-refresh! xref-refresh!
    "Force-refresh the cached clj-xref semantic graph. Same opts as z/xref-analyze!."
    '([] [opts])
    ["(z/xref-refresh!)"]))

(def who-calls-symbol
  (xref-symbol 'who-calls who-calls
    "Return call sites of symbol `sym` from the cached clj-xref graph. Opts include {:limit n}; first call auto-analyzes default source roots."
    '([sym] [sym opts])
    ["(z/who-calls 'my.ns/f)"]))

(def calls-who-symbol
  (xref-symbol 'calls-who calls-who
    "Return vars called by symbol `sym` from the cached clj-xref graph."
    '([sym] [sym opts])
    ["(z/calls-who 'my.ns/f)"]))

(def who-references-symbol
  (xref-symbol 'who-references who-references
    "Return all references to symbol `sym` from the cached clj-xref graph."
    '([sym] [sym opts])
    ["(z/who-references 'my.ns/f)"]))

(def who-macroexpands-symbol
  (xref-symbol 'who-macroexpands who-macroexpands
    "Return macro expansion sites for macro symbol `sym`."
    '([sym] [sym opts])
    ["(z/who-macroexpands 'my.ns/m)"]))

(def who-implements-symbol
  (xref-symbol 'who-implements who-implements
    "Return implementations of protocol symbol `sym`."
    '([sym] [sym opts])
    ["(z/who-implements 'my.ns/Protocol)"]))

(def who-dispatches-symbol
  (xref-symbol 'who-dispatches who-dispatches
    "Return defmethod dispatch sites for multimethod symbol `sym`."
    '([sym] [sym opts])
    ["(z/who-dispatches 'my.ns/multi)"]))

(def ns-vars-symbol
  (xref-symbol 'ns-vars ns-vars
    "Return var definitions in namespace `ns-sym`."
    '([ns-sym] [ns-sym opts])
    ["(z/ns-vars 'my.ns)"]))

(def ns-deps-symbol
  (xref-symbol 'ns-deps ns-deps
    "Return namespaces that `ns-sym` depends on."
    '([ns-sym] [ns-sym opts])
    ["(z/ns-deps 'my.ns)"]))

(def ns-dependents-symbol
  (xref-symbol 'ns-dependents ns-dependents
    "Return namespaces that depend on `ns-sym`."
    '([ns-sym] [ns-sym opts])
    ["(z/ns-dependents 'my.ns)"]))

(def unused-vars-symbol
  (xref-symbol 'unused-vars unused-vars
    "Return vars defined but never referenced. Opts: {:include-private? true, :limit n}."
    '([] [opts])
    ["(z/unused-vars)"
     "(z/unused-vars {:include-private? true :limit 50})"]))

(def call-graph-symbol
  (xref-symbol 'call-graph call-graph
    "Return transitive call graph edges for `sym`. Opts: {:depth n :direction :outgoing|:incoming}."
    '([sym] [sym opts])
    ["(z/call-graph 'my.ns/f {:depth 2})"]))

(def apropos-symbol
  (xref-symbol 'apropos apropos
    "Find vars whose fully-qualified name matches string or regex `pattern`."
    '([pattern] [pattern opts])
    ["(z/apropos \"process\")"]))

(def locator-for-ref-symbol
  (xref-symbol 'locator-for-ref locator-for-ref
    "Convert an xref row returned by z/who-calls, z/calls-who, z/who-references, etc. into a z/patch-compatible zipper locator row."
    '([row])
    ["(z/locator-for-ref (first (:result (z/who-calls 'my.ns/f))))"]))

(def locators-for-symbol-symbol
  (xref-symbol 'locators-for-symbol locators-for-symbol
    "Return z/patch-compatible zipper locator rows for a symbol definition and references known to clj-xref."
    '([sym] [sym opts])
    ["(z/locators-for-symbol 'my.ns/f)"]))

(def definition-symbol
  (xref-symbol 'definition definition
    "Return the definition row and z/patch-compatible locator for fully-qualified symbol `sym`."
    '([sym] [sym opts])
    ["(z/definition 'my.ns/f)"]))

(def call-sites-symbol
  (xref-symbol 'call-sites call-sites
    "Return call sites of `sym`, enriched with z/patch-compatible :locator rows."
    '([sym] [sym opts])
    ["(z/call-sites 'my.ns/f)"]))

(def context-for-symbol
  (xref-symbol 'context-for context-for
    "Return a bounded agent context neighborhood for `sym`: definition, caller refs, callee refs, and relevant files."
    '([sym] [sym opts])
    ["(z/context-for 'my.ns/f)"]))

(def symbols
  [xref-analyze-symbol
   xref-refresh-symbol
   who-calls-symbol
   calls-who-symbol
   who-references-symbol
   who-macroexpands-symbol
   who-implements-symbol
   who-dispatches-symbol
   ns-vars-symbol
   ns-deps-symbol
   ns-dependents-symbol
   unused-vars-symbol
   call-graph-symbol
   apropos-symbol
   locator-for-ref-symbol
   locators-for-symbol-symbol
   definition-symbol
   call-sites-symbol
   context-for-symbol])
