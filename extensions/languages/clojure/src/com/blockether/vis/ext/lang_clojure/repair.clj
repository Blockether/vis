(ns com.blockether.vis.ext.lang-clojure.repair
  "Range-scoped Clojure source repair for the `z/` alias.

   Reuses Vis' existing runtime repair strategy:
     1. parinfer indent-mode for delimiter balance;
     2. parse-diagnose quote rebalance for odd unescaped double-quotes.

   Public tools work on files and on rewrite-clj ranges (`:span` rows from
   z/locators/z/symbols/xref locator bridges)."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md]
   [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [edamame.core :as edamame])
  (:import
   (com.oakmac.parinfer Parinfer ParinferResult)
   (java.io File)))

(def ^:private edamame-opts
  {:all true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn- parse-ok?
  [^String src]
  (try
    (edamame/parse-string-all src edamame-opts)
    true
    (catch Throwable _ false)))

(defn- parse-error-message
  [^String src]
  (try
    (edamame/parse-string-all src edamame-opts)
    nil
    (catch Throwable t (ex-message t))))

(defn parinfer-rebalance
  "Return parinfer-repaired `source` iff the result differs and parses. Pure."
  ^String [^String source]
  (try
    (let [^ParinferResult r (Parinfer/indentMode source nil nil nil false)]
      (when (.success r)
        (let [rebalanced (.text r)]
          (when (and rebalanced
                  (not= rebalanced source)
                  (parse-ok? rebalanced))
            rebalanced))))
    (catch Throwable _ nil)))

(defn quote-rebalance
  "Return quote-repaired `source` iff Vis' parse-diagnose repair finds one. Pure."
  ^String [^String source]
  (parse-diagnose/try-quote-rebalance source parse-ok?))

(defn repair-source
  "Repair a Clojure source string using Vis' existing parse repair order.

   Returns:
     {:changed? bool
      :parseable-before? bool
      :parseable-after? bool
      :engine nil|:parinfer|:quote
      :source fixed-or-original
      :error parse-error-message-or-nil
      :diagnostic optional-quote-diagnostic}"
  [^String source]
  (let [source (or source "")]
    (if (parse-ok? source)
      {:changed? false
       :parseable-before? true
       :parseable-after? true
       :engine nil
       :source source}
      (if-let [fixed (parinfer-rebalance source)]
        {:changed? true
         :parseable-before? false
         :parseable-after? true
         :engine :parinfer
         :source fixed
         :error nil}
        (if-let [fixed (quote-rebalance source)]
          {:changed? true
           :parseable-before? false
           :parseable-after? true
           :engine :quote
           :source fixed
           :error nil
           :diagnostic (parse-diagnose/diagnose-quote-balance source)}
          {:changed? false
           :parseable-before? false
           :parseable-after? false
           :engine nil
           :source source
           :error (parse-error-message source)
           :diagnostic (parse-diagnose/diagnose-quote-balance source)})))))

;; =============================================================================
;; Path/range handling
;; =============================================================================

(defn- safe-path
  ^File [p]
  (let [cwd        (workspace-context/cwd)
        resolved   (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure.repair/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file!
  [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.lang-clojure.repair/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.lang-clojure.repair/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path
  [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace-context/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

(defn- line-starts
  [^String source]
  (let [lines (str/split source #"\n" -1)]
    (->> lines
      (reductions (fn [acc line] (+ acc (count line) 1)) 0)
      vec)))

(defn- offset-of
  [^String source [row col]]
  (let [starts (line-starts source)
        line-idx (dec (long row))
        col-idx  (dec (long col))]
    (when-not (<= 0 line-idx (dec (count starts)))
      (throw (ex-info "Range row outside source"
               {:type :ext.lang-clojure.repair/range-row-outside-source
                :row row})))
    (let [off (+ (nth starts line-idx) (max 0 col-idx))]
      (when-not (<= 0 off (count source))
        (throw (ex-info "Range col outside source"
                 {:type :ext.lang-clojure.repair/range-col-outside-source
                  :row row :col col})))
      off)))

(defn- normalize-range
  [range]
  (when-not (and (vector? range)
              (= 2 (count range))
              (every? #(and (vector? %) (= 2 (count %))) range))
    (throw (ex-info "Range must be [[start-row start-col] [end-row end-col]]"
             {:type :ext.lang-clojure.repair/invalid-range
              :range range})))
  range)

(defn- range-slice
  [source range]
  (let [[[sr sc :as start] [er ec :as end]] (normalize-range range)
        start-off (offset-of source start)
        end-off   (offset-of source end)]
    (when (> start-off end-off)
      (throw (ex-info "Range start must be <= range end"
               {:type :ext.lang-clojure.repair/invalid-range-order
                :range range})))
    {:range [[sr sc] [er ec]]
     :start start-off
     :end end-off
     :source (subs source start-off end-off)}))

(defn- replace-slice
  [source {:keys [start end]} replacement]
  (str (subs source 0 start) replacement (subs source end)))

(defn- whole-file-range
  [^String source]
  (let [lines (str/split source #"\n" -1)
        row   (max 1 (count lines))
        col   (inc (count (last lines)))]
    [[1 1] [row col]]))

(defn- coerce-range-request
  ([edit]
   (cond
     (map? edit)
     {:path (:path edit)
      :range (or (:range edit) (:span edit))
      :dry-run? (boolean (:dry-run? edit))}

     :else
     (throw (ex-info "repair-range expects a map or path/range args"
              {:type :ext.lang-clojure.repair/invalid-request
               :request edit}))))
  ([path range]
   {:path path :range range :dry-run? false})
  ([path range opts]
   (merge {:path path :range range :dry-run? false} opts)))

;; =============================================================================
;; Tool envelopes
;; =============================================================================

(defn- now-ms [] (System/currentTimeMillis))

(defn- tool-success
  [{:keys [op path result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result result
       :info (merge {:op op
                     :target {:kind :file
                              :resolved path}
                     :started-at-ms t
                     :finished-at-ms t
                     :duration-ms 0}
               info)})))

(defn- tool-failure-on-error
  [op]
  (fn [err _env _f args]
    (let [path (or (:path (first args)) (first args) ".")
          t    (now-ms)]
      {:result (extension/failure
                 {:result nil
                  :info {:op op
                         :target {:kind :file :requested (str path)}
                         :started-at-ms t
                         :finished-at-ms t
                         :duration-ms 0}
                  :throwable err})})))

(defn- repair-selection!
  [{:keys [op path range dry-run?]
    :or {op :z/repair-range}}]
  (when-not path
    (throw (ex-info "repair-range requires :path"
             {:type :ext.lang-clojure.repair/missing-path})))
  (when-not range
    (throw (ex-info "repair-range requires :range or :span"
             {:type :ext.lang-clojure.repair/missing-range
              :path path})))
  (let [file       (ensure-existing-file! (safe-path path))
        path       (rel-path file)
        before-all (slurp file)
        selection  (range-slice before-all range)
        repaired   (repair-source (:source selection))
        after-all  (if (:changed? repaired)
                     (replace-slice before-all selection (:source repaired))
                     before-all)]
    (when (and (:changed? repaired) (not dry-run?))
      (spit file after-all))
    (tool-success
      {:op op
       :path path
       :result {:path path
                :range (:range selection)
                :dry-run? (boolean dry-run?)
                :changed? (:changed? repaired)
                :engine (:engine repaired)
                :parseable-before? (:parseable-before? repaired)
                :parseable-after? (:parseable-after? repaired)
                :error (:error repaired)
                :diagnostic (:diagnostic repaired)}
       :info {:files [{:path path
                       :range (:range selection)
                       :dry-run? (boolean dry-run?)
                       :changed? (:changed? repaired)
                       :engine (:engine repaired)
                       :before (:source selection)
                       :after (:source repaired)}]}})))

(defn- repair-range
  ([edit]
   (repair-selection! (coerce-range-request edit)))
  ([path range]
   (repair-selection! (coerce-range-request path range)))
  ([path range opts]
   (repair-selection! (coerce-range-request path range opts))))

(defn- repair-locator
  ([locator-row]
   (repair-locator locator-row nil))
  ([locator-row opts]
   (repair-selection!
     (assoc (coerce-range-request (merge locator-row opts))
       :op :z/repair-locator))))

(defn- repair-file
  ([path] (repair-file path nil))
  ([path opts]
   (let [file   (ensure-existing-file! (safe-path path))
         source (slurp file)]
     (repair-selection!
       (merge {:op :z/repair-file
               :path path
               :range (whole-file-range source)}
         opts)))))

;; =============================================================================
;; Rendering/symbols
;; =============================================================================

(defn- preview-text
  [s]
  (let [s (str s)]
    (if (> (count s) 2000)
      (str (subs s 0 2000) "\n...<+" (- (count s) 2000) " chars>")
      s)))

(defn- render-repair-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (md/p "Tool" (md/code (get-in tool-result [:info :op])) "failed:" (get-in tool-result [:error :message]))
    (let [file (first (get-in tool-result [:info :files]))]
      (md/join
        (md/p "Repair" (md/code (:path file))
          "range" (md/code (pr-str (:range file)))
          "changed?" (md/code (pr-str (:changed? file)))
          "engine" (md/code (pr-str (:engine file))))
        (when (:changed? file)
          (md/join
            (md/code-block "clojure" (preview-text (:before file)))
            (md/code-block "clojure" (preview-text (:after file)))))))))

(def repair-range-symbol
  (vis/symbol 'repair-range repair-range
    {:doc "Repair a Clojure/EDN file range using Vis' parse repair pipeline. Accepts {:path p :range [[sr sc] [er ec]]} or a locator row with :span. Writes by default; pass :dry-run? true to preview."
     :arglists '([edit] [path range] [path range opts])
     :examples ["(z/repair-range {:path \"src/foo.clj\" :range [[10 1] [14 2]]})"
                "(z/repair-range (assoc locator-row :dry-run? true))"]
     :result-spec ::extension/tool-result
     :render-fn render-repair-result
     :on-error-fn (tool-failure-on-error :z/repair-range)}))

(def repair-locator-symbol
  (vis/symbol 'repair-locator repair-locator
    {:doc "Repair the source range described by a z/ locator row. The row must include :path and :span (or :range). Same behavior as z/repair-range; pass opts like {:dry-run? true}."
     :arglists '([locator-row] [locator-row opts])
     :examples ["(z/repair-locator locator-row {:dry-run? true})"
                "(z/repair-locator (:result (z/locator-for-ref ref-row)))"]
     :result-spec ::extension/tool-result
     :render-fn render-repair-result
     :on-error-fn (tool-failure-on-error :z/repair-locator)}))

(def repair-file-symbol
  (vis/symbol 'repair-file repair-file
    {:doc "Repair a whole Clojure/EDN file using Vis' parse repair pipeline. Writes by default; pass {:dry-run? true} to preview."
     :arglists '([path] [path opts])
     :examples ["(z/repair-file \"src/foo.clj\" {:dry-run? true})"]
     :result-spec ::extension/tool-result
     :render-fn render-repair-result
     :on-error-fn (tool-failure-on-error :z/repair-file)}))

(def symbols
  [repair-range-symbol repair-locator-symbol repair-file-symbol])
