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
  [{:keys [op path kind result provenance presentation]}]
  (let [t (now-ms)]
    (cond->
      (extension/success
        {:result     result
         :provenance (merge {:op             op
                             :target         (path->target path kind)
                             :started-at-ms  t
                             :finished-at-ms t
                             :duration-ms    0}
                       provenance)})
      presentation (assoc :presentation presentation))))

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
                  :provenance {:op             op
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

(defn- write-plans!
  [plans]
  (doseq [{:keys [file after]} plans]
    (spit file after))
  plans)

(defn patch-safe
  [edits]
  (-> edits patch-plan vec write-plans!))

(defn- patch-file
  "Zipper patch for Clojure/EDN source. Same input shape as v/patch:
   one edit map or a vector of {:path :search :replace} maps."
  [edits]
  (let [plans (patch-safe edits)]
    (tool-success
      {:op :z/patch
       :path (or (:path (first plans)) ".")
       :kind :file
       :result (mapv #(select-keys % [:path]) plans)
       :provenance {:files (mapv (fn [{:keys [path before after]}]
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
(def ^:private diff-context-lines 3)

(defn- preview-text
  [s]
  (let [s (str s)]
    (if (> (count s) journal-preview-chars)
      (str (subs s 0 journal-preview-chars)
        "\n…<+" (- (count s) journal-preview-chars) " chars>")
      s)))

(defn- split-preserve-trailing-empty
  [s]
  (if (nil? s)
    []
    (str/split s #"\n" -1)))

(defn- diff-edit-chunks
  [a b]
  (loop [prefix []
         left   a
         right  b]
    (cond
      (= left right)
      [{:type :equal :lines prefix}]

      (and (seq left) (seq right) (= (first left) (first right)))
      (recur (conj prefix (first left)) (rest left) (rest right))

      :else
      (let [leftv  (vec left)
            rightv (vec right)
            shared-suffix (loop [n 0]
                            (if (and (< n (count leftv))
                                  (< n (count rightv))
                                  (= (nth leftv (- (count leftv) (inc n)))
                                    (nth rightv (- (count rightv) (inc n)))))
                              (recur (inc n))
                              n))
            left-core  (subvec leftv 0 (- (count leftv) shared-suffix))
            right-core (subvec rightv 0 (- (count rightv) shared-suffix))
            suffix     (subvec leftv (- (count leftv) shared-suffix))]
        (cond-> []
          (seq prefix) (conj {:type :equal :lines prefix})
          (seq left-core) (conj {:type :delete :lines left-core})
          (seq right-core) (conj {:type :insert :lines right-core})
          (seq suffix) (conj {:type :equal :lines suffix}))))))

(defn- trim-context
  [chunks]
  (let [vec-chunks (vec chunks)
        last-idx   (dec (count vec-chunks))]
    (->> vec-chunks
      (map-indexed
        (fn [idx {:keys [type lines] :as chunk}]
          (if (and (= type :equal) (> (count lines) (* 2 diff-context-lines)))
            (cond
              (= idx 0)
              (assoc chunk :lines (subvec (vec lines) (- (count lines) diff-context-lines)))

              (= idx last-idx)
              (assoc chunk :lines (subvec (vec lines) 0 diff-context-lines))

              :else
              {:type :equal-gap
               :lines [(str "... " (- (count lines) (* 2 diff-context-lines)) " unchanged line(s) ...")]})
            chunk)))
      (remove #(and (= :equal (:type %)) (empty? (:lines %)))))))

(defn- unified-diff-text
  [path before after]
  (let [before-lines (split-preserve-trailing-empty before)
        after-lines  (split-preserve-trailing-empty after)
        chunks       (trim-context (diff-edit-chunks before-lines after-lines))
        body         (->> chunks
                       (mapcat (fn [{:keys [type lines]}]
                                 (case type
                                   :equal (map #(str " " %) lines)
                                   :equal-gap lines
                                   :delete (map #(str "-" %) lines)
                                   :insert (map #(str "+" %) lines))))
                       (str/join "\n"))]
    (preview-text
      (str "--- a/" path "\n"
        "+++ b/" path "\n"
        "@@\n"
        body))))

(defn- tool-error-text
  [tool-result]
  (let [op   (get-in tool-result [:provenance :op])
        path (get-in tool-result [:provenance :target :requested])
        err  (:error tool-result)]
    (md/p "Tool" (md/code op) "failed"
      (when path (str "for " (md/code path)))
      ":"
      (or (:message err) (pr-str err)))))

(defn- render-patch-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (tool-error-text tool-result)
    (let [files (get-in tool-result [:provenance :files])]
      (md/join
        (md/p "Patched" (count files) "Clojure file(s). Each :search locator matched exactly once before any write.")
        (for [{:keys [path changed? before after]} files
              :let [diff-txt (when changed? (unified-diff-text path before after))]]
          (md/join
            (md/p (md/code path) (when (false? changed?) "(no change)"))
            (when diff-txt
              (md/code-block "diff" diff-txt))))))))

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
        :presentation {:kind :clojure/locators}
        :provenance {:count (count rows)
                     :total-count total-count
                     :limit limit
                     :truncated? truncated?
                     :filters (select-keys (or opts {}) [:symbol :source-contains :limit])}}))))

(defn- symbols-file
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
        :presentation {:kind :clojure/locators}
        :provenance {:count (count rows)
                     :total-count total-count
                     :limit limit
                     :truncated? truncated?
                     :filters (select-keys (or opts {}) [:name :symbol :source-contains :limit])}}))))

(defn- locator-for-symbol-file
  [path sym]
  (let [out (symbols-file path {:symbol sym :limit 2})]
    (assoc out
      :result (first (:result out))
      :provenance (assoc (:provenance out)
                    :op :z/locator-for-symbol
                    :symbol sym))))

(defn- render-locators-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (tool-error-text tool-result)
    (let [rows       (vec (:result tool-result))
          shown      (take 8 rows)
          one?       (= 1 (count rows))
          row-lines  (->> shown
                       (map-indexed
                         (fn [idx {:keys [path tag locator source span]}]
                           (str (inc idx) ". " path " " (pr-str span) " " tag "\n"
                             (preview-text (or source locator ""))))))
          patch-hint (when one?
                       (str "Patch hint:\n"
                         "(z/patch [{:path \"" (:path (first rows))
                         "\" :search <locator-row> :replace <new-source>}])"))]
      (md/join
        (md/p "Found" (count rows) "zipper locator(s)."
          (when (get-in tool-result [:provenance :truncated?])
            " Narrow filters before patching."))
        (when (seq row-lines)
          (md/code-block "text" (str/join "\n\n" row-lines)))
        (when patch-hint
          (md/code-block "clojure" patch-hint))))))

(defn- render-locators-kind
  [{:keys [value]}]
  (render-locators-result {:tool-result {:success? true
                                         :result (vec (or value []))}}))

(def rendering-kind-fns
  {:clojure/locators render-locators-kind})

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def patch-symbol
  (vis/symbol 'patch patch-file
    {:doc "Canonical zipper patch for Clojure/EDN files. Same input shape as v/patch: one edit map or vector of maps with required keys `:path`, `:search`, `:replace`. `:search` is a locator form/source snippet and must match exactly once before any write. Tool result envelope."
     :arglists '([edits])
     :examples ["(z/patch [{:path \"src/x.clj\" :search \"old\" :replace \"new\"}])"
                "(z/patch {:path \"deps.edn\" :search \":old\" :replace \":new\"})"]
     :result-spec ::extension/tool-result
     :render-fn render-patch-result
     :on-error-fn (tool-failure-on-error :z/patch)}))

(def locators-symbol
  (vis/symbol 'locators locators-file
    {:doc "List Clojure/EDN zipper locators in a file. Defaults to 10 rows; pass opts like {:symbol 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
     :arglists '([path] [path opts])
     :examples ["(z/locators \"src/foo.clj\")"
                "(z/locators \"src/foo.clj\" {:symbol 'foo :limit 20})"]
     :result-spec ::extension/tool-result
     :render-fn render-locators-result
     :on-error-fn (tool-failure-on-error :z/locators)}))

(def symbols-symbol
  (vis/symbol 'symbols symbols-file
    {:doc "List symbol zipper locators in a Clojure/EDN file. Defaults to 10 rows; pass opts like {:name 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
     :arglists '([path] [path opts])
     :examples ["(z/symbols \"src/foo.clj\")"
                "(z/symbols \"src/foo.clj\" {:name 'foo :limit 20})"]
     :result-spec ::extension/tool-result
     :render-fn render-locators-result
     :on-error-fn (tool-failure-on-error :z/symbols)}))

(def locator-for-symbol-symbol
  (vis/symbol 'locator-for-symbol locator-for-symbol-file
    {:doc "Return the first symbol zipper locator row for `sym` without dumping the whole namespace."
     :arglists '([path sym])
     :examples ["(z/locator-for-symbol \"src/foo.clj\" 'foo)"]
     :result-spec ::extension/tool-result
     :render-fn render-locators-result
     :on-error-fn (tool-failure-on-error :z/locator-for-symbol)}))

(def z-prompt
  "`z/` Clojure/EDN zipper patching:
  Use (z/patch {:path p :search locator :replace replacement}) or vector of same maps. Same map shape as v/patch. :search is parsed Clojure/EDN and must match once.
  Find targets with (z/locator-for-symbol path 'foo), (z/locators path {:symbol 'foo}), (z/locators path {:source-contains <text> :limit 20}), or (z/symbols path {:name 'foo}). Rows include :path/:index/:span and become edits by adding :replace. If a row identifies the target, patch immediately: (z/patch (assoc row :replace \"<new source>\")); do not re-preview unless patch fails. Repair with z/repair-range, z/repair-locator, or z/repair-file. Full rewrite-clj.zip API is under z/, including z/subedit->.
Examples: (z/patch [{:path \"src/foo.clj\" :search \"old-sym\" :replace \"new-sym\"}])
          (z/patch [{:path \"src/foo.clj\" :search \"(def x 1)\" :replace \"(def x 2)\"}])")
