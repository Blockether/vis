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
  (let [cwd        (fs/cwd)
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
  (let [cwd (.toAbsolutePath (fs/path (fs/cwd)))
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
  [{:keys [op path kind result provenance]}]
  (let [t (now-ms)]
    (extension/success
      {:result     result
       :provenance (merge {:op             op
                           :target         (path->target path kind)
                           :started-at-ms  t
                           :finished-at-ms t
                           :duration-ms    0}
                     provenance)})))

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
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))]
              (when missing
                (throw (ex-info "z/patch edit missing required keys"
                         {:type :ext.lang-clojure/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit}))))
            (update edit :path str))
      edits)))

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
      (and (= value (z/sexpr zloc))
        (or (nil? span)
          (= span (z/position-span zloc))))
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

(defn- patch-plan
  [edits]
  (let [edits (coerce-patch-edits edits)]
    (loop [remaining edits
           states {}]
      (if-let [{:keys [path search replace]} (first remaining)]
        (let [file    (ensure-existing-file! (safe-path path))
              before  (or (get-in states [path :before]) (slurp file))
              current (or (get-in states [path :after]) before)
              after   (try
                        (zipper-patch-source current search replace)
                        (catch clojure.lang.ExceptionInfo e
                          (throw (ex-info (ex-message e)
                                   (assoc (ex-data e) :path path)
                                   e))))]
          (recur (next remaining)
            (assoc states path {:file file
                                :path (rel-path file)
                                :before before
                                :after after})))
        (vals states)))))

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

(defn- locators-file
  [path]
  (let [f    (ensure-existing-file! (safe-path path))
        rows (source-locators (slurp f))]
    (tool-success
      {:op :z/locators
       :path path
       :kind :file
       :result rows
       :provenance {:count (count rows)}})))

(defn- symbols-file
  [path]
  (let [f    (ensure-existing-file! (safe-path path))
        rows (->> (source-locators (slurp f))
               (filterv #(symbol? (:value %))))]
    (tool-success
      {:op :z/symbols
       :path path
       :kind :file
       :result rows
       :provenance {:count (count rows)}})))

(defn- render-locators-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (tool-error-text tool-result)
    (let [rows (:result tool-result)
          shown (take 40 rows)
          body (->> shown
                 (map (fn [{:keys [tag locator source span]}]
                        (str (pr-str span) " " tag " " locator " <- " source)))
                 (str/join "\n"))]
      (md/join
        (md/p "Found" (count rows) "zipper locator(s).")
        (when (seq shown)
          (md/code-block "text" (preview-text body)))))))

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
    {:doc "List Clojure/EDN zipper locators in a file. Tool result rows include :tag, :value, :locator, :source, and :span."
     :arglists '([path])
     :examples ["(z/locators \"src/foo.clj\")"]
     :result-spec ::extension/tool-result
     :render-fn render-locators-result
     :on-error-fn (tool-failure-on-error :z/locators)}))

(def symbols-symbol
  (vis/symbol 'symbols symbols-file
    {:doc "List symbol zipper locators in a Clojure/EDN file. Tool result rows include :value, :locator, :source, and :span."
     :arglists '([path])
     :examples ["(z/symbols \"src/foo.clj\")"]
     :result-spec ::extension/tool-result
     :render-fn render-locators-result
     :on-error-fn (tool-failure-on-error :z/symbols)}))

(def z-prompt
  "`z/` Clojure/EDN zipper patching:
  Use (z/patch {:path p :search locator :replace replacement}) or vector of same maps. Same map shape as v/patch. :search is a parsed Clojure/EDN locator, not raw text; exactly one match required before write.
  Discover locators with (z/locators path) or symbols only with (z/symbols path). Full rewrite-clj.zip API is also under z/, including z/subedit->.
Examples: (z/patch [{:path \"src/foo.clj\" :search \"old-sym\" :replace \"new-sym\"}])
          (z/patch [{:path \"src/foo.clj\" :search \"(def x 1)\" :replace \"(def x 2)\"}])")
