(ns com.blockether.vis.ext.lang-clojure.patch
  "Clojure/EDN zipper patching.

   Surface:

     (z/patch edits)          ; zipper patch, v/patch-shaped maps

   `z/patch` keeps the v/patch-shaped edit map but interprets `:search`
   as a Clojure/EDN locator form, not raw text. String locators are parsed
   as Clojure source before matching. `:replace` is likewise a replacement
   form/source snippet. The matching and replacement happen through
   rewrite-clj zippers so comments/spacing around the edited form survive.

   Hard guard: every path stays inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [rewrite-clj.parser :as parser]
   [rewrite-clj.zip :as z])
  (:import
   (java.io File)))

;; =============================================================================
;; Path safety (mirrors vis-foundation; kept local so this extension
;; has no implicit dependency on the editing extension's internals).
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.
  (let [cwd        (workspace-context/cwd)
        resolved   (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.lang-clojure/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.lang-clojure/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace-context/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- path->target
  [requested kind]
  (try
    (let [f (safe-path requested)]
      {:requested (str requested)
       :resolved  (rel-path f)
       :absolute  (.getPath f)
       :kind      kind})
    (catch Throwable _
      {:requested (str requested)
       :resolved  nil
       :absolute  nil
       :kind      kind})))

(defn- tool-success
  [{:keys [op path kind result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result     result
       :info (merge {:op             op
                     :target         (path->target path kind)
                     :started-at-ms  t
                     :finished-at-ms t
                     :duration-ms    0}
               info)})))

(defn- first-edit-path
  [args]
  (let [edits (first args)
        edit  (cond
                (string? edits) {:path edits}
                (map? edits) edits
                :else (first edits))]
    (or (:path edit) ".")))

(defn- tool-failure-on-error
  [op]
  (fn [err _env _f args]
    (let [path (first-edit-path args)
          t    (now-ms)]
      {:result (extension/failure
                 {:result     nil
                  :info {:op             op
                         :target         (path->target path :file)
                         :started-at-ms  t
                         :finished-at-ms t
                         :duration-ms    0}
                  :throwable  err})})))

;; =============================================================================
;; Zipper patching
;; =============================================================================

(def ^:private patch-required-keys #{:path :search :replace})

(defn- parse-source-node
  [role source]
  (try
    (parser/parse-string source)
    (catch Throwable t
      (throw (ex-info (str "z/patch " role " string must be parseable Clojure/EDN source")
               {:type :ext.lang-clojure/invalid-patch-source
                :role role
                :source source}
               t)))))

(defn- locator-row?
  [x]
  (and (map? x)
    (contains? x :value)
    (some #(contains? x %) [:span :source :locator :tag])))

(defn- row-edit->patch-edit
  [edit]
  (if (and (locator-row? edit)
        (contains? edit :path)
        (contains? edit :replace)
        (not (contains? edit :search)))
    (assoc edit :search (select-keys edit [:path :index :tag :value :locator :source :span]))
    edit))

(defn- coerce-patch-edits
  [edits]
  (let [edits (if (map? edits) [edits] edits)]
    (when-not (sequential? edits)
      (throw (ex-info "z/patch expects a map or vector of edit maps"
               {:type :ext.lang-clojure/invalid-patch-edits
                :got  (type edits)})))
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "z/patch edit must be a map"
                       {:type :ext.lang-clojure/invalid-patch-edit
                        :edit edit})))
            (let [edit    (row-edit->patch-edit edit)
                  missing (seq (remove #(contains? edit %) patch-required-keys))]
              (when missing
                (throw (ex-info "z/patch edit missing required keys"
                         {:type :ext.lang-clojure/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit})))
              (update edit :path str)))
      edits)))

(defn- parse-locator-source
  [role s]
  (when (str/blank? s)
    (throw (ex-info (str "z/patch " role " locator string must be non-blank")
             {:type :ext.lang-clojure/invalid-patch-search})))
  (z/sexpr (z/of-node (parse-source-node role s))))

(defn- locator-target
  [search]
  (cond
    (string? search)
    {:value (parse-locator-source ":search" search)}

    (locator-row? search)
    {:value (:value search)
     :span  (:span search)}

    :else
    {:value search}))

(defn- replacement-value
  [replace]
  (cond
    (string? replace)
    (parse-source-node ":replace" replace)

    (locator-row? replace)
    (if-let [source (:source replace)]
      (parse-source-node ":replace" source)
      (:value replace))

    :else replace))

(defn- matching-loc?
  [{:keys [value span]} zloc]
  (and (z/sexpr-able? zloc)
    (try
      (if span
        (= span (z/position-span zloc))
        (= value (z/sexpr zloc)))
      (catch Throwable _ false))))

(defn- count-matches
  [zloc target]
  (loop [loc zloc
         n   0]
    (if-let [hit (z/find loc z/next #(matching-loc? target %))]
      (recur (z/next hit) (inc n))
      n)))

(defn- replace-one
  [zloc target replacement]
  (if-let [hit (z/find zloc z/next #(matching-loc? target %))]
    (z/replace hit replacement)
    zloc))

(defn- locator-row-edit?
  [edit]
  (locator-row? (:search edit)))

(defn- locator-row-span-start
  [edit]
  (get-in edit [:search :span 0]))

(defn- locator-row-edit-order
  "Span-specific locator rows are positions in the original file.
   Apply them from bottom to top so earlier replacements do not shift
   later spans and break multi-edit patches discovered from z/locators."
  [edits]
  (if (every? locator-row-edit? edits)
    (sort-by locator-row-span-start #(compare %2 %1) edits)
    edits))

(defn- zipper-patch-source
  [source search replace]
  (let [zloc        (z/of-string source {:track-position? true})
        target      (locator-target search)
        replacement (replacement-value replace)
        matches     (count-matches zloc target)]
    (when-not (= 1 matches)
      (throw (ex-info (str "z/patch :search locator must match exactly once; matched "
                        matches " time(s)")
               {:type :ext.lang-clojure/patch-search-not-unique
                :matches matches
                :search search
                :locator target})))
    (z/root-string (replace-one zloc target replacement))))

(defn- plan-path-edits
  [path edits]
  (let [file   (ensure-existing-file! (safe-path path))
        before (slurp file)
        after  (reduce
                 (fn [current {:keys [search replace]}]
                   (try
                     (zipper-patch-source current search replace)
                     (catch clojure.lang.ExceptionInfo e
                       (throw (ex-info (ex-message e)
                                (assoc (ex-data e) :path path)
                                e)))))
                 before
                 (locator-row-edit-order edits))]
    {:file file
     :path (rel-path file)
     :before before
     :after after}))

(defn- patch-plan
  [edits]
  (let [edits      (coerce-patch-edits edits)
        path-order (distinct (map :path edits))
        by-path    (group-by :path edits)]
    (mapv #(plan-path-edits % (get by-path %)) path-order)))

(defn- search-preview-for-check
  [search]
  (let [s (str search)]
    (if (<= (count s) 180)
      s
      (str (subs s 0 180) "...<+" (- (count s) 180) " chars>"))))

(defn- check-single-edit
  "Dry-run one zipper edit against `current` source. Returns a tuple
   [next-source check-map failure-map-or-nil]. `next-source` is the
   original `current` if the edit fails so subsequent checks in the
   same file see the same baseline as the failing edit."
  [path current edit-index {:keys [search replace]}]
  (let [zloc      (z/of-string current {:track-position? true})
        [target target-err]
        (try [(locator-target search) nil]
          (catch Throwable e [nil e]))
        matches   (cond target-err 0
                    :else      (count-matches zloc target))
        check     {:edit-index     edit-index
                   :path           path
                   :matches        matches
                   :search-preview (search-preview-for-check search)
                   :locator-error  (some-> target-err ex-message)}
        valid?    (and (not target-err) (= 1 matches))]
    (if valid?
      [(z/root-string
         (replace-one zloc target (replacement-value replace)))
       check
       nil]
      [current check check])))

(defn- patch-analysis
  [edits]
  (let [edits      (coerce-patch-edits edits)
        path-order (distinct (map :path edits))
        by-path    (group-by :path edits)
        indexed    (into {} (map-indexed (fn [i e] [(System/identityHashCode e) i]) edits))]
    (loop [paths    path-order
           checks   []
           failures []]
      (if-let [path (first paths)]
        (let [file    (ensure-existing-file! (safe-path path))
              rel     (rel-path file)
              before  (slurp file)
              ordered (locator-row-edit-order (get by-path path))
              [_ pcs pfs]
              (reduce
                (fn [[current cs fs] edit]
                  (let [idx (get indexed (System/identityHashCode edit))
                        [next-src ck fl] (check-single-edit rel current idx edit)]
                    [next-src (conj cs ck) (cond-> fs fl (conj fl))]))
                [before [] []]
                ordered)]
          (recur (next paths)
            (into checks pcs)
            (into failures pfs)))
        {:checks   checks
         :failures failures
         :valid?   (empty? failures)}))))

(defn patch-check
  "Dry-run zipper patch edits without writing. Returns
   {:valid? :checks :failures} mirroring v/patch-check semantics."
  [edits]
  (let [{:keys [checks failures valid?]} (patch-analysis edits)]
    {:valid?   valid?
     :checks   checks
     :failures failures}))

(defn- patch-check-tool
  "Preflight zipper patches without writing. Use before z/patch when locators may be stale or multi-edit risk is high; mirrors v/patch-check."
  [edits]
  (let [out (patch-check edits)]
    (tool-success
      {:op     :z/patch-check
       :path   (or (:path (first (:checks out))) ".")
       :kind   :file
       :result out
       :info   {:valid?        (:valid? out)
                :edit-count    (count (:checks out))
                :failure-count (count (:failures out))}})))

(defn- journal-render-patch-check
  [result]
  (str "z/patch-check — " (pr-str result)))

(defn- channel-render-patch-check
  [result]
  (let [{:keys [valid? checks failures]} (or result {})]
    (md/join
      (md/p (if valid? "All zipper edits valid." (str (count failures) " zipper edit(s) failed.")))
      (when (seq checks)
        (md/code-block "text"
          (str/join "\n"
            (map (fn [{:keys [edit-index path matches locator-error]}]
                   (str "#" edit-index " " path " matches=" matches
                     (when locator-error (str " locator-error=" locator-error))))
              checks)))))))

(defn- write-plans!
  [plans]
  (doseq [{:keys [file after]} plans]
    (spit file after))
  plans)

(defn patch-safe
  [edits]
  (-> edits patch-plan vec write-plans!))

(defn- patch-file
  "Canonical zipper patch for Clojure/EDN files. Same input shape as v/patch: one edit map or vector of maps with required keys `:path`, `:search`, `:replace`. `:search` is a locator form/source snippet and must match exactly once before any write. Tool result envelope."
  [edits]
  (let [plans (patch-safe edits)]
    (tool-success
      {:op :z/patch
       :path (or (:path (first plans)) ".")
       :kind :file
       :result (mapv #(select-keys % [:path]) plans)
       :info {:files (mapv (fn [{:keys [path before after]}]
                             {:path path
                              :changed? (not= before after)
                              :before before
                              :after after
                              :lines-before (count (str/split-lines before))
                              :lines-after (count (str/split-lines after))})
                       plans)}})))

;; =============================================================================
;; Rendering
;; =============================================================================

(def ^:private journal-preview-chars 3000)

(defn- preview-text
  [s]
  (let [s (str s)]
    (if (> (count s) journal-preview-chars)
      (str (subs s 0 journal-preview-chars)
        "\n...<+" (- (count s) journal-preview-chars) " chars>")
      s)))

(defn- journal-render-patch-result
  [_result]
  ;; z/patch result is the per-file path map; the diff data lives on :info
  ;; which renderers no longer see. Single-line confirmation.
  "z/patch — wrote zipper edit(s) (full diff visible in channel render)")

(defn- channel-render-patch-result
  [result]
  (let [files (if (sequential? result) result [result])]
    (md/join
      (md/p "Patched" (count files) "Clojure file(s).")
      (md/ul (map (fn [{:keys [path]}] (md/code path)) files)))))

;; =============================================================================
;; Locator discovery
;; =============================================================================

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

(defn- apply-locator-filters
  [rows {:keys [symbol source-contains name limit] :or {limit 10}}]
  (let [sym-filter (or symbol name)
        rows       (cond->> rows
                     sym-filter (filter #(= sym-filter (:value %)))
                     source-contains (filter #(str/includes? (str (:source %)) (str source-contains))))
        rows       (vec rows)
        limit      (max 1 (long limit))]
    {:rows (subvec rows 0 (min limit (count rows)))
     :total-count (count rows)
     :limit limit
     :truncated? (> (count rows) limit)}))

(defn- locators-file
  "List Clojure/EDN zipper locators in a file. Defaults to 10 rows; pass opts like {:symbol 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (locators-file path nil))
  ([path opts]
   (let [f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         all   (with-locator-row-context path (source-locators (slurp f)))
         {:keys [rows total-count limit truncated?]} (apply-locator-filters all (or opts {}))]
     (tool-success
       {:op :z/locators
        :path path
        :kind :file
        :result rows
        :info {:count (count rows)
               :total-count total-count
               :limit limit
               :truncated? truncated?
               :filters (select-keys (or opts {}) [:symbol :source-contains :limit])}}))))

(defn- symbols-file
  "List symbol zipper locators in a Clojure/EDN file. Defaults to 10 rows; pass opts like {:name 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (symbols-file path nil))
  ([path opts]
   (let [f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         all   (->> (with-locator-row-context path (source-locators (slurp f)))
                 (filterv #(symbol? (:value %))))
         {:keys [rows total-count limit truncated?]} (apply-locator-filters all (or opts {}))]
     (tool-success
       {:op :z/symbols
        :path path
        :kind :file
        :result rows
        :info {:count (count rows)
               :total-count total-count
               :limit limit
               :truncated? truncated?
               :filters (select-keys (or opts {}) [:name :symbol :source-contains :limit])}}))))

(defn- locator-for-symbol-file
  "Return the first symbol zipper locator row for `sym` without dumping the whole namespace."
  [path sym]
  (let [out (symbols-file path {:symbol sym :limit 2})]
    (assoc out
      :result (first (:result out))
      :info (assoc (:info out)
              :op :z/locator-for-symbol
              :symbol sym))))

(defn- journal-render-locators
  [result]
  (let [rows (vec (or result []))]
    (str (count rows) " zipper locator(s)\n"
      (str/join "\n"
        (map-indexed
          (fn [idx {:keys [path tag locator span]}]
            (str (inc idx) ". " path " " (pr-str span) " " tag " "
              (preview-text (or locator ""))))
          (take 8 rows)))
      (when (> (count rows) 8)
        (str "\n… (" (- (count rows) 8) " more; bind result and slice)")))))

(defn- channel-render-locators
  [result]
  (let [rows (vec (or result []))]
    (md/join
      (md/p "Found" (count rows) "zipper locator(s).")
      (when (seq rows)
        (md/code-block "text"
          (str/join "\n\n"
            (map-indexed
              (fn [idx {:keys [path tag locator source span]}]
                (str (inc idx) ". " path " " (pr-str span) " " tag "\n"
                  (preview-text (or source locator ""))))
              (take 8 rows))))))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def patch-check-symbol
  (vis/symbol #'patch-check-tool
    {:sym 'patch-check

     :result-spec ::extension/tool-result
     :journal-render-fn journal-render-patch-check
     :channel-render-fn channel-render-patch-check
     :on-error-fn (tool-failure-on-error :z/patch-check)}))

(def patch-symbol
  (vis/symbol #'patch-file
    {:sym 'patch

     :result-spec ::extension/tool-result
     :journal-render-fn journal-render-patch-result
     :channel-render-fn channel-render-patch-result
     :on-error-fn (tool-failure-on-error :z/patch)}))

(def locators-symbol
  (vis/symbol #'locators-file
    {:sym 'locators

     :result-spec ::extension/tool-result
     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locators)}))

(def symbols-symbol
  (vis/symbol #'symbols-file
    {:sym 'symbols

     :result-spec ::extension/tool-result
     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/symbols)}))

(def locator-for-symbol-symbol
  (vis/symbol #'locator-for-symbol-file
    {:sym 'locator-for-symbol

     :result-spec ::extension/tool-result
     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locator-for-symbol)}))

(def z-prompt
  "`z/` Clojure/EDN zipper patching:
  Use (z/patch {:path p :search locator :replace replacement}) or vector of same maps. Same map shape as v/patch. :search is parsed Clojure/EDN and must match once.
  Dry-run with (z/patch-check edits) when locators may be stale or multi-edit risk is high; returns {:valid? :checks :failures} mirroring v/patch-check.
  Find targets with (z/locator-for-symbol path 'foo), (z/locators path {:symbol 'foo}), (z/locators path {:source-contains <text> :limit 20}), or (z/symbols path {:name 'foo}). Rows include :path/:index/:span and become edits by adding :replace. If a row identifies the target, patch immediately: (z/patch (assoc row :replace \"<new source>\")); do not re-preview unless patch fails. Repair with z/repair-range, z/repair-locator, or z/repair-file. Full rewrite-clj.zip API is under z/, including z/subedit->.
Examples: (z/patch [{:path \"src/foo.clj\" :search \"old-sym\" :replace \"new-sym\"}])
          (z/patch [{:path \"src/foo.clj\" :search \"(def x 1)\" :replace \"(def x 2)\"}])")
