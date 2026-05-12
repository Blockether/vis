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

   [com.blockether.vis.internal.workspace :as workspace]
   [rewrite-clj.node :as node]
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
  (let [cwd        (workspace/cwd)
        resolved   (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure/path-escape :path (str p)})))
    (.toFile normalized)))

(def ^:private clojure-file-extensions
  "File extensions whose contents `z/` zipper tools can safely parse via
   rewrite-clj. Anything else (Markdown, SQL, YAML, JSON, shell) breaks
   the zipper parser at runtime on perfectly-valid non-Clojure bytes
   (regression: ANALYSIS.md §1 — z/patch on AGENTS.md crashed on Unicode
   math symbols inside a fenced code block)."
  #{"clj" "cljc" "cljs" "edn"})

(defn- file-extension
  "Lowercased extension of the path's basename, without the dot. Empty
   string when the basename has no `.`."
  [path]
  (let [s    (str path)
        base (.getName (java.io.File. s))
        idx  (.lastIndexOf base ".")]
    (if (pos? idx)
      (str/lower-case (subs base (inc idx)))
      "")))

(defn- ensure-clojure-file-ext! [path]
  (let [ext (file-extension path)]
    (when-not (contains? clojure-file-extensions ext)
      (throw (ex-info
               (str "z/ tools only operate on Clojure/EDN files ("
                 (str/join ", " (sort (map #(str "." %) clojure-file-extensions)))
                 "). Got '" path "' (extension " (pr-str ext)
                 "). Use v/patch / v/write for plain-text files.")
               {:type      :ext.lang-clojure/non-clojure-file
                :path      (str path)
                :extension ext
                :allowed   clojure-file-extensions})))))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.lang-clojure/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.lang-clojure/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace/cwd)))
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

(defn- op->tool-sym
  [op]
  (some-> op name symbol))

(defn- tool-success
  [{:keys [op path kind result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result   result
       :op       op
       :metadata (merge {:tool           {:sym (op->tool-sym op)}
                         :extension      {:namespace 'com.blockether.vis.ext.lang-clojure.core}
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

(defn- error-hint
  [err]
  (case (:type (ex-data err))
    :ext.lang-clojure/patch-search-not-unique
    "Use z/forms or z/locators to pick one row, then patch with that locator row's :span. Avoid plain :search strings when forms repeat."

    :ext.lang-clojure/invalid-patch-source
    "Use parseable Clojure/EDN source for :search/:replace, or patch a locator row returned by z/forms/z/locators."

    :ext.lang-clojure/non-clojure-file
    "Use v/patch or v/write for non-Clojure files; z/ only parses .clj/.cljc/.cljs/.edn."

    :ext.lang-clojure/file-not-found
    "Check the :path relative to the current Vis workspace before retrying."

    :ext.lang-clojure/path-escape
    "Use a path inside the current Vis workspace; z/ refuses traversal outside cwd."

    nil))

(defn- tool-failure-on-error
  [op]
  (fn [err _env _f args]
    (let [path  (first-edit-path args)
          t     (now-ms)
          error (cond-> (extension/normalize-error err)
                  (error-hint err) (assoc :hint (error-hint err)))]
      {:result (extension/failure
                 {:result   nil
                  :op       op
                  :metadata {:tool           {:sym (op->tool-sym op)}
                             :extension      {:namespace 'com.blockether.vis.ext.lang-clojure.core}
                             :target         (path->target path :file)
                             :started-at-ms  t
                             :finished-at-ms t
                             :duration-ms    0
                             :exception-data (ex-data err)}
                  :error    error})})))

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

(defn source
  "Parse exact Clojure/EDN source into a rewrite-clj node. Use inside z/patch :replace when formatting, comments, reader macros, or multi-form source bytes matter. Example: (z/patch (assoc row :replace (z/source \"(def x 1)\")))."
  [s]
  (parse-source-node "source" s))

(defn lit
  "Coerce a Clojure value into a rewrite-clj node. Use inside z/patch :replace when you mean data, not source text. Symbols stay symbols; strings become string literals. Example: (z/patch (assoc row :replace (z/lit \"actual string literal\")))."
  [x]
  (node/coerce x))

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
              (let [edit (update edit :path str)]
                (ensure-clojure-file-ext! (:path edit))
                edit)))
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

    (node/node? search)
    {:value (node/sexpr search)}

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
    (str (if valid? "All zipper edits valid." (str (count failures) " zipper edit(s) failed.")) "\n\n"
      (when (seq checks)
        (str "```text\n"
          (str/join "\n"
            (map (fn [{:keys [edit-index path matches locator-error]}]
                   (str "#" edit-index " " path " matches=" matches
                     (when locator-error (str " locator-error=" locator-error))))
              checks))
          "\n```")))))

(defn- write-plans!
  [plans]
  (doseq [{:keys [file after]} plans]
    (spit file after))
  plans)

(defn patch-safe
  [edits]
  (-> edits patch-plan vec write-plans!))

(defn- patch-file-result
  [plans]
  (letfn [(prefix-count [a b]
            (loop [i 0]
              (if (and (< i (count a))
                    (< i (count b))
                    (= (nth a i) (nth b i)))
                (recur (inc i))
                i)))
          (suffix-count [a b prefix]
            (loop [i 0]
              (if (and (< (+ prefix i) (count a))
                    (< (+ prefix i) (count b))
                    (= (nth a (- (count a) i 1))
                      (nth b (- (count b) i 1))))
                (recur (inc i))
                i)))
          (hunk [before after]
            (let [before-lines (vec (str/split-lines before))
                  after-lines  (vec (str/split-lines after))
                  prefix       (prefix-count before-lines after-lines)
                  suffix       (suffix-count before-lines after-lines prefix)
                  before-end   (- (count before-lines) suffix)
                  after-end    (- (count after-lines) suffix)
                  ctx          2
                  ctx-start    (max 0 (- prefix ctx))]
              (when (not= before after)
                {:start-line     (inc ctx-start)
                 :context-before (subvec before-lines ctx-start prefix)
                 :removed        (subvec before-lines prefix before-end)
                 :added          (subvec after-lines prefix after-end)
                 :context-after  (subvec after-lines after-end
                                   (min (count after-lines) (+ after-end ctx)))
                 :removed-count  (- before-end prefix)
                 :added-count    (- after-end prefix)})))]
    (let [files (mapv (fn [{:keys [path before after]}]
                        (cond-> {:path path
                                 :changed? (not= before after)
                                 :before before
                                 :after after
                                 :lines-before (count (str/split-lines before))
                                 :lines-after (count (str/split-lines after))}
                          (not= before after) (assoc :hunks [(hunk before after)])))
                  plans)]
      {:files files
       :preflight {:checked? true
                   :message "z/patch validates every edit matches exactly once before any write"}
       :total-files (count files)
       :total-changes (count (filter :changed? files))})))

(defn- patch-file
  "Canonical zipper patch for Clojure/EDN files. Same input shape as v/patch: one edit map or vector of maps with required keys `:path`, `:search`, `:replace`. `:search` is a locator row/span or locator form/source snippet and must match exactly once before any write. Tool result envelope returns the changed file diffs in :result."
  [edits]
  (let [plans (patch-safe edits)
        result (patch-file-result plans)]
    (tool-success
      {:op :z/patch
       :path (or (:path (first plans)) ".")
       :kind :file
       :result result
       :info {:file-count (:total-files result)
              :change-count (:total-changes result)}})))

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

(defn- patch-result-files
  [result]
  (vec (or (:files result)
         (when (sequential? result) result)
         (when result [result]))))

(defn- journal-render-patch-result
  [result]
  (let [files   (patch-result-files result)
        changed (count (filter :changed? files))]
    (str "z/patch — " changed "/" (count files) " file(s) changed; preflight exact-match OK"
      (when (seq files)
        (str "\n"
          (str/join "\n"
            (map (fn [{:keys [path changed? lines-before lines-after hunks]}]
                   (let [{:keys [start-line removed-count added-count]} (first hunks)]
                     (str "- " path " " (if changed? "changed" "unchanged")
                       " lines " lines-before "→" lines-after
                       (when changed?
                         (str " hunk@" start-line " -" removed-count " +" added-count)))))
              (take 8 files))))))))

(defn- channel-render-patch-result
  [result]
  (letfn [(render-lines [{:keys [context-before removed added context-after]}]
            (str/join "\n"
              (concat
                (map #(str " " %) context-before)
                (map #(str "-" %) removed)
                (map #(str "+" %) added)
                (map #(str " " %) context-after))))
          (render-file [{:keys [path changed? hunks]}]
            (if-not changed?
              (str "### `" path "` — unchanged")
              (let [{:keys [start-line] :as hunk} (first hunks)]
                (str "### `" path "` — changed\n\n"
                  "```diff\n"
                  "@@ line " start-line " @@\n"
                  (preview-text (render-lines hunk))
                  "\n```"))))]
    (let [files (patch-result-files result)]
      (str "Patched " (count files)
        " Clojure file(s). z/patch preflight validated exact matches before writing.\n\n"
        (str/join "\n\n" (map render-file (take 6 files)))))))

;; =============================================================================
;; Locator discovery
;; =============================================================================

(def ^:private definition-heads
  '#{def defonce defn defn- defmacro defmulti defmethod defrecord deftype defprotocol definterface definterface+ defstruct})

(defn- compact-source
  ([s] (compact-source s 180))
  ([s n]
   (let [s (-> (str s)
             (str/replace #"\s+" " ")
             str/trim)]
     (if (> (count s) n)
       (str (subs s 0 n) "…<+" (- (count s) n) " chars>")
       s))))

(defn- form-head
  [v]
  (when (seq? v) (first v)))

(defn- form-kind
  [v]
  (let [head (form-head v)]
    (cond
      (= 'ns head) :ns
      (contains? definition-heads head) (keyword (name head))
      (seq? v) :form
      (symbol? v) :symbol
      (keyword? v) :keyword
      (vector? v) :vector
      (map? v) :map
      (set? v) :set
      :else :literal)))

(defn- form-name
  [v]
  (let [head (form-head v)]
    (cond
      (= 'ns head) (second v)
      (contains? definition-heads head) (second v)
      :else nil)))

(defn- defn-arities
  [v]
  (when (and (seq? v) (#{'defn 'defn- 'defmacro} (first v)))
    (let [body (drop-while #(not (or (vector? %) (seq? %))) (drop 2 v))]
      (cond
        (vector? (first body)) #{(count (first body))}
        (seq? (first body)) (->> body
                              (filter seq?)
                              (map first)
                              (filter vector?)
                              (map count)
                              set)
        :else nil))))

(defn- docstring?
  [v]
  (and (seq? v)
    (contains? definition-heads (first v))
    (string? (some #(when (string? %) %) (drop 2 v)))))

(defn- form-digest
  [v source]
  (let [head (form-head v)
        name (form-name v)]
    (cond
      (= 'ns head) (str "(ns " name " …)")
      (#{'def 'defonce} head) (str "(" head " " name " …)")
      (#{'defn 'defn- 'defmacro} head) (str "(" head " " name " " (or (seq (defn-arities v)) "…") " …)")
      (= 'defmethod head) (str "(defmethod " name " " (nth v 2 nil) " …)")
      (contains? definition-heads head) (str "(" head " " name " …)")
      :else (compact-source source))))

(defn- locator-row
  [zloc]
  (when (z/sexpr-able? zloc)
    (try
      (let [v      (z/sexpr zloc)
            source (z/string zloc)]
        (cond-> {:tag            (z/tag zloc)
                 :kind           (form-kind v)
                 :name           (form-name v)
                 :digest         (form-digest v source)
                 :source-preview (compact-source source)
                 :value          v
                 :locator        (pr-str v)
                 :source         source
                 :span           (z/position-span zloc)}
          (seq (defn-arities v)) (assoc :arities (defn-arities v))
          (docstring? v) (assoc :doc? true)))
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

(defn- source-top-level-locators
  [source]
  (loop [loc (z/of-string source {:track-position? true})
         out []]
    (if (or (nil? loc) (z/end? loc))
      out
      (recur (z/right loc)
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

(defn- filter-match?
  [expected actual]
  (cond
    (nil? expected) true
    (instance? java.util.regex.Pattern expected) (boolean (re-find expected (str actual)))
    :else (= expected actual)))

(defn- apply-locator-filters
  [rows {:keys [symbol source-contains name kind limit] :or {limit 50}}]
  (let [rows  (cond->> rows
                symbol (filter #(= symbol (:value %)))
                name (filter #(filter-match? name (or (:name %) (:value %))))
                kind (filter #(= kind (:kind %)))
                source-contains (filter #(str/includes? (str (:source %)) (str source-contains))))
        rows  (vec rows)
        limit (max 1 (long limit))]
    {:rows (subvec rows 0 (min limit (count rows)))
     :total-count (count rows)
     :limit limit
     :truncated? (> (count rows) limit)}))

(defn- locator-source-rows
  [source opts]
  (if (or (= :all (:depth opts)) (:symbol opts))
    (source-locators source)
    (source-top-level-locators source)))

(defn- locators-file
  "List semantic Clojure/EDN zipper locator rows in a file. Defaults to top-level forms (limit 50); pass {:depth :all} for every descendant, {:kind :defn}, {:name 'foo}, {:symbol 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (locators-file path nil))
  ([path opts]
   (ensure-clojure-file-ext! path)
   (let [opts  (or opts {})
         f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         all   (with-locator-row-context path (locator-source-rows (slurp f) opts))
         {:keys [rows total-count limit truncated?]} (apply-locator-filters all opts)]
     (tool-success
       {:op :z/locators
        :path path
        :kind :file
        :result rows
        :info {:count (count rows)
               :total-count total-count
               :limit limit
               :truncated? truncated?
               :filters (select-keys opts [:depth :kind :name :symbol :source-contains :limit])}}))))

(defn forms-file
  "List top-level Clojure/EDN forms as semantic locator rows. Defaults to limit 50; pass filters like {:kind :defn}, {:name 'foo}, {:name #\"^format-\"}, {:source-contains \"swap!\"}. Rows can become z/patch edits by adding :replace."
  ([path] (forms-file path nil))
  ([path opts]
   (let [out (locators-file path (assoc (or opts {}) :depth :top))]
     (assoc out
       :symbol :z/forms
       :metadata (assoc (:metadata out)
                   :op :z/forms
                   :tool {:sym 'forms})))))

(defn- symbols-file
  "List symbol zipper locator rows in a Clojure/EDN file. Defaults to 50 rows; pass opts like {:name 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (symbols-file path nil))
  ([path opts]
   (ensure-clojure-file-ext! path)
   (let [opts  (or opts {})
         f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         all   (->> (with-locator-row-context path (source-locators (slurp f)))
                 (filterv #(symbol? (:value %))))
         {:keys [rows total-count limit truncated?]} (apply-locator-filters all opts)]
     (tool-success
       {:op :z/symbols
        :path path
        :kind :file
        :result rows
        :info {:count (count rows)
               :total-count total-count
               :limit limit
               :truncated? truncated?
               :filters (select-keys opts [:name :symbol :source-contains :limit])}}))))

(defn- locator-for-symbol-file
  "Return the first symbol zipper locator row for `sym` without dumping the whole namespace."
  [path sym]
  (let [out (symbols-file path {:symbol sym :limit 2})]
    (assoc out
      :symbol :z/locator-for-symbol
      :result (first (:result out))
      :metadata (assoc (:metadata out)
                  :op :z/locator-for-symbol
                  :tool {:sym 'locator-for-symbol}
                  :symbol sym))))

(defn- locator-label
  [{:keys [kind name digest source-preview locator]}]
  (str (or (some-> kind name) "form")
    (when name (str " " name))
    " — " (or digest source-preview locator "")))

(defn- render-locator-line
  [idx {:keys [path span tag] :as row}]
  (str (inc idx) ". " path " " (pr-str span) " " (or (:kind row) tag) " "
    (locator-label row)))

(defn- journal-render-locators
  [result]
  (let [rows (vec (if (sequential? result) result (when result [result])))]
    (str (count rows) " zipper locator(s)\n"
      (str/join "\n" (map-indexed render-locator-line (take 12 rows)))
      (when (> (count rows) 12)
        (str "\n… (" (- (count rows) 12) " more; bind result and slice)")))))

(defn- channel-render-locators
  [result]
  (let [rows (vec (if (sequential? result) result (when result [result])))]
    (str "Found " (count rows) " zipper locator(s).\n\n"
      (when (seq rows)
        (str "```text\n"
          (str/join "\n"
            (map-indexed render-locator-line (take 20 rows)))
          "\n```\n\n"
          "Patch by adding :replace to the chosen row: `(z/patch (assoc row :replace \"<new source>\"))`.")))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} inspect-locator
  "Return a compact, serializable summary for a rewrite-clj zipper loc or a z/ locator row. Use this instead of relying on raw zloc #object printing in journals."
  [x]
  (cond
    (and (map? x) (:span x))
    (select-keys x [:path :span :tag :kind :name :digest :source-preview :arities :doc?])

    (z/sexpr-able? x)
    (select-keys (locator-row x) [:span :tag :kind :name :digest :source-preview :arities :doc?])

    :else
    {:tag (some-> x class .getName)
     :value-preview (compact-source (pr-str x))}))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} render-inspect
  [result]
  (str "z/inspect — " (pr-str result)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} render-node
  [result]
  (if (node/node? result)
    (str "rewrite-clj node\n"
      ":source " (pr-str (node/string result)) "\n"
      ":sexpr  " (pr-str (try (node/sexpr result)
                           (catch Throwable _ :not-sexpr-able))))
    (str "rewrite-clj node — " (pr-str result))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def source-symbol
  (vis/symbol #'source {:raw? true}))

(def lit-symbol
  (vis/symbol #'lit {:raw? true}))

(def patch-check-symbol
  (vis/symbol #'patch-check-tool
    {:sym 'patch-check

     :journal-render-fn journal-render-patch-check
     :channel-render-fn channel-render-patch-check
     :on-error-fn (tool-failure-on-error :z/patch-check)}))

(def patch-symbol
  (vis/symbol #'patch-file
    {:sym 'patch

     :journal-render-fn journal-render-patch-result
     :channel-render-fn channel-render-patch-result
     :on-error-fn (tool-failure-on-error :z/patch)}))

(def locators-symbol
  (vis/symbol #'locators-file
    {:sym 'locators

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locators)}))

(def forms-symbol
  (vis/symbol #'forms-file
    {:sym 'forms

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/forms)}))

(def symbols-symbol
  (vis/symbol #'symbols-file
    {:sym 'symbols

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/symbols)}))

(def locator-for-symbol-symbol
  (vis/symbol #'locator-for-symbol-file
    {:sym 'locator-for-symbol

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locator-for-symbol)}))

(def inspect-symbol
  (vis/symbol #'inspect-locator
    {:sym 'inspect

     :journal-render-fn render-inspect
     :channel-render-fn render-inspect}))

(def z-prompt
  "`z/` strategy for Clojure/EDN edits.\nCombine discovery rows with one patch: z/forms for top-level defs/ns, z/locators {:depth :all} for nested forms, z/symbols for symbol sites; choose the row by :span/:digest and add :replace. Prefer data replacements for structural changes; use z/source only when comments/formatting/reader syntax must be exact. Use z/patch-check for risky batches, z/repair-* after parse damage, and v/patch for non-Clojure text.")
