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
   directory (Vis workspace cwd); `..` traversal is rejected before any I/O."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.path :as lang-path]
   [com.blockether.vis.internal.extension :as extension]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as parser]
   [rewrite-clj.zip :as z]))

;; =============================================================================
;; Shared path safety
;; =============================================================================

(def ^:private safe-path lang-path/safe-path)
(def ^:private ensure-clojure-file-ext! lang-path/ensure-clojure-file-ext!)
(def ^:private ensure-existing-file! lang-path/ensure-existing-file!)
(def ^:private rel-path lang-path/rel-path)

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
       :metadata (merge {:tool           {:symbol (op->tool-sym op)}
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
                  :metadata {:tool           {:symbol (op->tool-sym op)}
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
    (not (node/node? x))
    (some #(contains? x %) [:span :source :locator :tag :digest :value])))

(defn- row-edit->patch-edit
  [edit]
  (if (and (locator-row? edit)
        (contains? edit :path)
        (contains? edit :replace)
        (not (contains? edit :search)))
    (assoc edit :search (select-keys edit [:path :index :tag :kind :value :locator :source :span :digest :sexpr-able? :include-hidden?]))
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
    (cond-> (select-keys search [:span :source :tag :digest :value :locator :sexpr-able? :include-hidden?])
      (not (contains? search :value)) (dissoc :value))

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

(defn- safe-z
  [f zloc]
  (try
    [(f zloc) nil]
    (catch Throwable t [nil t])))

(defn- target-move
  [target]
  (if (:include-hidden? target) z/next* z/next))

(defn- target-root
  [source target]
  ((if (:include-hidden? target) z/of-string* z/of-string)
   source
   {:track-position? true}))

(defn- matching-loc?
  [target zloc]
  (let [[span]   (safe-z z/position-span zloc)
        [tag]    (safe-z z/tag zloc)
        [source] (safe-z z/string zloc)]
    (cond
      (:span target)
      (and (= (:span target) span)
        (or (not (contains? target :tag)) (= (:tag target) tag))
        (or (not (contains? target :source)) (= (:source target) source)))

      (:source target)
      (and (= (:source target) source)
        (or (not (contains? target :tag)) (= (:tag target) tag)))

      (contains? target :value)
      (and (z/sexpr-able? zloc)
        (try (= (:value target) (z/sexpr zloc))
          (catch Throwable _ false)))

      :else false)))

(defn- count-matches
  [zloc target]
  (let [move (target-move target)]
    (loop [loc zloc
           n   0]
      (if-let [hit (z/find loc move #(matching-loc? target %))]
        (recur (move hit) (inc n))
        n))))

(defn- replace-one
  [zloc target replacement]
  (if-let [hit (z/find zloc (target-move target) #(matching-loc? target %))]
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
  (let [target      (locator-target search)
        zloc        (target-root source target)
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
   [next-source check-map failure-map-or-nil warning-maps]. `next-source`
   is the original `current` if the edit fails so subsequent checks in
   the same file see the same baseline as the failing edit."
  [path current edit-index {:keys [search replace]}]
  (let [[target target-err]
        (try [(locator-target search) nil]
          (catch Throwable e [nil e]))
        zloc      (when-not target-err (target-root current target))
        matches   (cond target-err 0
                    :else      (count-matches zloc target))
        [replacement replacement-err]
        (try [(replacement-value replace) nil]
          (catch Throwable e [nil e]))
        valid?    (and (not target-err) (not replacement-err) (= 1 matches))
        next-src  (when valid?
                    (z/root-string (replace-one zloc target replacement)))
        changed?  (when valid? (not= current next-src))
        warnings  (cond-> []
                    (and valid? (not changed?))
                    (conj {:type :ext.lang-clojure/patch-no-op
                           :message "replacement renders identical source; z/patch would leave the file unchanged"
                           :edit-index edit-index
                           :path path}))
        check     (cond-> {:edit-index       edit-index
                           :path             path
                           :matches          matches
                           :changed?         (boolean changed?)
                           :search-preview   (search-preview-for-check search)
                           :locator-error    (some-> target-err ex-message)
                           :replacement-error (some-> replacement-err ex-message)}
                    (seq warnings) (assoc :warnings warnings))]
    (if valid?
      [next-src check nil warnings]
      [current check check warnings])))

(defn- patch-analysis
  [edits]
  (let [edits      (coerce-patch-edits edits)
        path-order (distinct (map :path edits))
        by-path    (group-by :path edits)
        indexed    (into {} (map-indexed (fn [i e] [(System/identityHashCode e) i]) edits))]
    (loop [paths    path-order
           checks   []
           failures []
           warnings []]
      (if-let [path (first paths)]
        (let [file    (ensure-existing-file! (safe-path path))
              rel     (rel-path file)
              before  (slurp file)
              ordered (locator-row-edit-order (get by-path path))
              [_ pcs pfs pws]
              (reduce
                (fn [[current cs fs ws] edit]
                  (let [idx (get indexed (System/identityHashCode edit))
                        [next-src ck fl warn] (check-single-edit rel current idx edit)]
                    [next-src (conj cs ck) (cond-> fs fl (conj fl)) (into ws warn)]))
                [before [] [] []]
                ordered)]
          (recur (next paths)
            (into checks pcs)
            (into failures pfs)
            (into warnings pws)))
        {:checks   checks
         :failures failures
         :warnings warnings
         :valid?   (empty? failures)}))))

(defn patch-check
  "Dry-run zipper patch edits without writing. Returns
   {:valid? :checks :failures} mirroring v/patch-check semantics."
  [edits]
  (let [{:keys [checks failures warnings valid?]} (patch-analysis edits)]
    {:valid?   valid?
     :checks   checks
     :failures failures
     :warnings warnings}))

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
                :failure-count (count (:failures out))
                :warning-count (count (:warnings out))}})))

(defn- journal-render-patch-check
  [result]
  (str "z/patch-check — " (pr-str result)))

(defn- channel-render-patch-check
  [result]
  (let [{:keys [valid? checks failures warnings]} (or result {})]
    (str (if valid? "All zipper edits valid." (str (count failures) " zipper edit(s) failed."))
      (when (seq warnings)
        (str " " (count warnings) " warning(s)."))
      "\n\n"
      (when (seq checks)
        (str "```text\n"
          (str/join "\n"
            (map (fn [{:keys [edit-index path matches changed? locator-error replacement-error warnings]}]
                   (str "#" edit-index " " path " matches=" matches " changed=" changed?
                     (when locator-error (str " locator-error=" locator-error))
                     (when replacement-error (str " replacement-error=" replacement-error))
                     (when (seq warnings) " WARNING no-op")))
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

(defn- tag-kind
  [tag]
  (case tag
    :comment :comment
    :whitespace :whitespace
    :newline :whitespace
    :uneval :reader-discard
    :forms :forms
    tag))

(defn- locator-row
  [zloc]
  (let [[tag tag-err]       (safe-z z/tag zloc)
        [source source-err] (safe-z z/string zloc)
        [span span-err]     (safe-z z/position-span zloc)
        sexpr-able?         (try (boolean (z/sexpr-able? zloc)) (catch Throwable _ false))
        [v sexpr-err]       (if sexpr-able?
                              (safe-z z/sexpr zloc)
                              [nil nil])]
    (when (and (not tag-err) (not source-err) (not span-err) span)
      (cond-> {:tag            tag
               :kind           (if sexpr-err (tag-kind tag) (form-kind v))
               :name           (when-not sexpr-err (form-name v))
               :digest         (if sexpr-err (compact-source source) (form-digest v source))
               :source-preview (compact-source source)
               :source         source
               :span           span
               :sexpr-able?    sexpr-able?}
        (and sexpr-able? (not sexpr-err)) (assoc :value v :locator (pr-str v))
        (and sexpr-able? sexpr-err) (assoc :value-error (ex-message sexpr-err))
        (and (not sexpr-err) (seq (defn-arities v))) (assoc :arities (defn-arities v))
        (and (not sexpr-err) (docstring? v)) (assoc :doc? true)))))

(defn- parse-context
  [{:keys [include-hidden? depth]}]
  {:create          (if include-hidden? :of-string* :of-string)
   :move            (if include-hidden? :next* (if (= :top depth) :right :next))
   :track-position? true
   :position        "1-based row/col, end-col exclusive"
   :sexpr           "best-effort; :value/:locator omitted or :value-error set when rewrite-clj sexpr is unsupported/misleading"
   :hidden?         (boolean include-hidden?)})

(defn- source-locators
  ([source] (source-locators source nil))
  ([source opts]
   (let [include-hidden? (:include-hidden? opts)
         move            (if include-hidden? z/next* z/next)]
     (loop [loc ((if include-hidden? z/of-string* z/of-string) source {:track-position? true})
            out []]
       (if (or (nil? loc) (z/end? loc))
         out
         (recur (move loc)
           (if-let [row (locator-row loc)]
             (conj out (assoc row :include-hidden? (boolean include-hidden?)))
             out)))))))

(defn- source-top-level-locators
  [source]
  (loop [loc (z/of-string source {:track-position? true})
         out []]
    (if (or (nil? loc) (z/end? loc))
      out
      (recur (z/right loc)
        (if-let [row (locator-row loc)]
          (conj out (assoc row :include-hidden? false))
          out)))))

(defn- with-locator-row-context
  [path rows parse]
  (mapv (fn [idx row]
          (assoc row
            :path path
            :index idx
            :parse parse))
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
  (if (or (= :all (:depth opts)) (:symbol opts) (:include-hidden? opts))
    (source-locators source opts)
    (source-top-level-locators source)))

(defn- locators-file
  "List semantic Clojure/EDN zipper locator rows in a file. Defaults to top-level forms (limit 50); pass {:depth :all} for every descendant, {:kind :defn}, {:name 'foo}, {:symbol 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (locators-file path nil))
  ([path opts]
   (ensure-clojure-file-ext! path)
   (let [opts  (or opts {})
         f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         parse (parse-context opts)
         all   (with-locator-row-context path (locator-source-rows (slurp f) opts) parse)
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
               :parse parse
               :filters (select-keys opts [:depth :kind :name :symbol :source-contains :limit :include-hidden?])}}))))

(defn forms-file
  "List top-level Clojure/EDN forms as semantic locator rows. Defaults to limit 50; pass filters like {:kind :defn}, {:name 'foo}, {:name #\"^format-\"}, {:source-contains \"swap!\"}. Rows can become z/patch edits by adding :replace."
  ([path] (forms-file path nil))
  ([path opts]
   (let [out (locators-file path (assoc (or opts {}) :depth :top))]
     (assoc out
       :symbol :z/forms
       :metadata (assoc (:metadata out)
                   :op :z/forms
                   :tool {:symbol 'forms})))))

(defn- symbols-file
  "List symbol zipper locator rows in a Clojure/EDN file. Defaults to 50 rows; pass opts like {:name 'foo}, {:source-contains \"foo\"}, or {:limit 20}. Rows can become z/patch edits by adding :replace."
  ([path] (symbols-file path nil))
  ([path opts]
   (ensure-clojure-file-ext! path)
   (let [opts  (or opts {})
         f     (ensure-existing-file! (safe-path path))
         path  (rel-path f)
         parse (parse-context (assoc opts :depth :all))
         all   (->> (with-locator-row-context path (source-locators (slurp f) opts) parse)
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
               :parse parse
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
                  :tool {:symbol 'locator-for-symbol}
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
  (vis/symbol #'source
    {:raw? true
     :journal-render-fn render-node
     :channel-render-fn render-node}))

(def lit-symbol
  (vis/symbol #'lit
    {:raw? true
     :journal-render-fn render-node
     :channel-render-fn render-node}))

(def patch-check-symbol
  (vis/symbol #'patch-check-tool
    {:symbol 'patch-check

     :journal-render-fn journal-render-patch-check
     :channel-render-fn channel-render-patch-check
     :on-error-fn (tool-failure-on-error :z/patch-check)}))

(def patch-symbol
  (vis/symbol #'patch-file
    {:symbol 'patch

     :journal-render-fn journal-render-patch-result
     :channel-render-fn channel-render-patch-result
     :on-error-fn (tool-failure-on-error :z/patch)}))

(def locators-symbol
  (vis/symbol #'locators-file
    {:symbol 'locators

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locators)}))

(def forms-symbol
  (vis/symbol #'forms-file
    {:symbol 'forms

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/forms)}))

(def symbols-symbol
  (vis/symbol #'symbols-file
    {:symbol 'symbols

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/symbols)}))

(def locator-for-symbol-symbol
  (vis/symbol #'locator-for-symbol-file
    {:symbol 'locator-for-symbol

     :journal-render-fn journal-render-locators
     :channel-render-fn channel-render-locators
     :on-error-fn (tool-failure-on-error :z/locator-for-symbol)}))

(def inspect-symbol
  (vis/symbol #'inspect-locator
    {:symbol 'inspect

     :journal-render-fn render-inspect
     :channel-render-fn render-inspect}))

(def z-prompt
  "`z/` strategy for Clojure/EDN edits.\nCombine discovery rows with one patch: z/forms for top-level defs/ns, z/locators {:depth :all} for nested forms, z/symbols for symbol sites; choose the row by :span/:digest and add :replace. Prefer data replacements for structural changes; use z/source only when comments/formatting/reader syntax must be exact. Use z/patch-check for risky batches, z/repair-* after parse damage, and v/patch for non-Clojure text.")
