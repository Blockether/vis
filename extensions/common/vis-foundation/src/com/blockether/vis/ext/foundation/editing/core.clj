(ns com.blockether.vis.ext.foundation.editing.core
  "Filesystem tools exposed under the `v/` alias in the SCI sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (v/cat path)            ; -> {:path :offset :total-lines :truncated-by :lines ...}
        (v/ls path)             ; -> nested {:name :path :type :size :children} tree
        (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
        (v/rg spec)            ; -> {:hits :truncated-by}; spec = {:all|:any [...] :paths [...]}

   2. Cwd-safe wrappers over the babashka.fs file API. `v/patch` is
      the canonical text edit surface:

        (v/cat path)
        (v/patch [{:path path :search old :replace new}])
        (v/create-dirs path)
        (v/glob root pattern)
        (v/copy src dest)
        (v/move src dest)
        (v/delete path)
        (v/delete-if-exists path)
        (v/exists? path)
        (v/bash command)
        (v/bash command opts)
        (v/cwd)
        (v/parent path)
        (v/file-name path)
        (v/extension path)
        (v/relativize from to)

   Clojure/EDN zipper patching (`z/patch`, same map shape as
   `v/patch`) lives in the `vis-language-clojure` extension under
   `extensions/languages/clojure/`.

   Hard guard: every path must stay inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.io File InputStream Reader Writer)
   (java.util.concurrent TimeUnit)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 50)
(def ^:private default-list-depth 5)
(def ^:private default-bash-timeout-ms 30000)
(def ^:private default-bash-max-output-chars 20000)
(def ^:private journal-render-chars 3000)

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.
  (let [cwd (workspace/cwd)
        resolved (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.foundation.editing/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.foundation.editing/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.foundation.editing/path-is-dir :path (.getPath f)})))
  f)

(defn- ensure-existing-dir! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "Directory not found: " (.getPath f))
             {:type :ext.foundation.editing/dir-not-found :path (.getPath f)})))
  (when-not (.isDirectory f)
    (throw (ex-info (str "Path is a file, not a directory: " (.getPath f))
             {:type :ext.foundation.editing/path-is-file :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

(defn- display-path [p]
  (let [cwd  (.normalize (.toAbsolutePath (fs/path (workspace/cwd))))
        path (.normalize (.toAbsolutePath (fs/path p)))]
    (if (= path cwd)
      "."
      (str (.relativize cwd path)))))

(defn- ensure-parent-dirs! [^File f]
  (when-let [parent (.getParentFile f)]
    (.mkdirs parent))
  f)

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

;; Engine contract lives in `com.blockether.vis.internal.extension`:
;;   `extension/op-tag`          - canonical op-keyword -> :op.tag/... value.
;;   `extension/op-presentation` - `:info` metadata `{:tag ...}` embedded in tool envelopes.
;; The iteration loop's mutating-call gate inlines
;; `(= :op.tag/action (extension/op-tag op-kw))` directly
;; (no separate `side-effect-op?` helper).
;; Editing used to keep its own copies; they were thin shims and crossed
;; the abstraction boundary (color-role lived here too). Use the engine
;; functions directly.

(defn- tool-success
  [{:keys [op path kind result info]}]
  (let [t (now-ms)]
    (extension/success
      {:result   result
       :op       op
       :metadata (merge {:target         (path->target path kind)
                         :started-at-ms  t
                         :finished-at-ms t
                         :duration-ms    0}
                   info)})))

(defn- tool-failure-on-error
  [op kind _render-fn]
  (fn [err _env _f args]
    (let [bash?        (= :v/bash op)
          bash-opts    (when bash? (second args))
          path         (if bash? (or (:cwd bash-opts) ".") (first args))
          target       (path->target path kind)
          interrupted? (instance? InterruptedException err)
          t            (now-ms)
          error        (when interrupted?
                         {:message (str (name op)
                                     " interrupted while running; operation was cancelled.")})]
      {:result (extension/failure
                 {:result    nil
                  :op        op
                  :metadata  (cond-> {:target         target
                                      :started-at-ms  t
                                      :finished-at-ms t
                                      :duration-ms    0}
                               bash?
                               (assoc :command (first args)
                                 :cwd path
                                 :opts (dissoc bash-opts :stdin))
                               interrupted?
                               (assoc :interrupted? true
                                 :status :interrupted))
                  :error     error
                  :throwable (when-not error err)})})))

;; =============================================================================
;; .gitignore (cheap, lazy)
;; =============================================================================

(defn- load-ignore-node ^IgnoreNode [^File root]
  (let [gi (io/file root ".gitignore")]
    (when (.exists gi)
      (let [n (IgnoreNode.)]
        (with-open [in (io/input-stream gi)]
          (.parse n in))
        n))))

(defn- ignored? [^IgnoreNode node ^File f ^File root]
  (when node
    (let [rel (str (.relativize (.toPath root) (.toPath f)))
          dir? (.isDirectory f)
          result (.isIgnored node rel dir?)]
      (= IgnoreNode$MatchResult/IGNORED result))))

;; =============================================================================
;; cat
;; =============================================================================

(defn- unsupported-cat-opts!
  [opts]
  (when (some? opts)
    (throw (ex-info "v/cat reads the whole file; bind the result and slice with plain Clojure for ranges"
             {:type :ext.foundation.editing/invalid-cat-opts
              :opts opts}))))

(defn- read-file
  "Read the whole text file as pure structured data.

   Returns :path, :offset, :total-lines, :truncated-by, and raw :lines.
   No prose footer, no line-number prefix, no display limit. Bind the
   result and slice with `subvec`/`get-in` for ranges."
  ([path] (read-file path nil))
  ([path opts]
   (unsupported-cat-opts! opts)
   (let [f         (ensure-existing-file! (safe-path path))
         all-lines (str/split-lines (slurp f))]
     {:path         (rel-path f)
      :offset       1
      :total-lines  (count all-lines)
      :truncated-by :end-of-file
      :lines        (vec all-lines)})))

;; =============================================================================
;; ls
;; =============================================================================

(defn- file->entry [^File f]
  {:name (.getName f)
   :path (rel-path f)
   :type (if (.isDirectory f) :dir :file)
   :size (if (.isDirectory f) nil (.length f))
   :hidden? (.isHidden f)})

(defn- visible-children [^File f {:keys [hidden? respect-gitignore? ignore-node root]}]
  (when (.isDirectory f)
    (let [kids (->> (.listFiles f)
                 (remove (fn [^File c]
                           (and (not hidden?) (.isHidden c))))
                 (remove (fn [^File c]
                           (and respect-gitignore?
                             (ignored? ignore-node c root)))))]
      (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %)) kids))))

(defn- file->tree [^File f opts depth]
  (let [base (file->entry f)]
    (if (and (pos? depth) (.isDirectory f))
      (assoc base :children
        (mapv #(file->tree % opts (dec depth))
          (visible-children f opts)))
      base)))

(defn- list-files
  ([path] (list-files path nil))
  ([path opts]
   (let [{:keys [depth hidden? respect-gitignore?]
          :or {depth default-list-depth hidden? false respect-gitignore? true}} (or opts {})
         f (safe-path path)
         _ (when-not (.exists f)
             (throw (ex-info (str "Path not found: " (.getPath f))
                      {:type :ext.foundation.editing/path-not-found})))
         opts* {:hidden? hidden?
                :respect-gitignore? respect-gitignore?
                :ignore-node (when respect-gitignore? (load-ignore-node f))
                :root f}]
     (file->tree f opts* depth))))

;; =============================================================================
;; rg
;; =============================================================================

(defn- coerce-rg-spec
  "Coerce the single public v/rg spec map. Regex syntax is never
   interpreted. Exactly one of :all or :any is required. Every public
   collection field is a vector; no shorthand arities or scalar paths."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "v/rg takes one spec map: {:all [...] :paths [...]}."
             {:type :ext.foundation.editing/invalid-rg-spec
              :got  (type spec)})))
  (let [allowed-keys #{:all :any :paths :include :exclude :hidden? :respect-gitignore?}
        unknown-keys (seq (remove allowed-keys (keys spec)))
        _ (when unknown-keys
            (throw (ex-info "v/rg spec has unknown keys."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :unknown (vec unknown-keys)})))
        has-all? (contains? spec :all)
        has-any? (contains? spec :any)
        _ (when (= has-all? has-any?)
            (throw (ex-info "v/rg spec must use exactly one of :all or :any."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        vector-of-strings (fn [k default]
                            (let [v (if (contains? spec k) (get spec k) default)]
                              (when-not (and (vector? v) (seq v) (every? string? v))
                                (throw (ex-info "v/rg spec fields must be non-empty vectors of strings."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              (when-not (every? #(not (str/blank? %)) v)
                                (throw (ex-info "v/rg spec string values must be non-blank."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              v))
        op (if has-all? :all :any)
        needles (vector-of-strings op nil)
        paths (vector-of-strings :paths ["."])
        include (when (contains? spec :include)
                  (vector-of-strings :include nil))
        exclude (when (contains? spec :exclude)
                  (vector-of-strings :exclude nil))]
    {:op op
     :needles needles
     :paths paths
     :include (or include [])
     :exclude (or exclude [])
     :hidden? (boolean (:hidden? spec))
     :respect-gitignore? (get spec :respect-gitignore? true)}))

(defn- grep-files
  "Search with the one public v/rg spec map. Exactly one of :all or
   :any is required. :paths defaults to the current directory.
   :include/:exclude are glob vectors. Acquisition has a private hard
   cap; bind the result and slice for tighter display."
  [spec]
  (let [{:keys [op needles paths include exclude hidden? respect-gitignore?]} (coerce-rg-spec spec)
        limit default-grep-limit
        glob-matcher (fn [pattern]
                       (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                         (str "glob:" pattern)))
        include-matchers (mapv glob-matcher include)
        exclude-matchers (mapv glob-matcher exclude)
        match-globs? (fn [matchers ^File f]
                       (let [rel (rel-path f)
                             name (.getName f)
                             rel-path (fs/path rel)
                             name-path (fs/path name)]
                         (boolean
                           (some #(or (.matches % rel-path)
                                    (.matches % name-path))
                             matchers))))
        include-file? (fn [^File f]
                        (and (or (empty? include-matchers)
                               (match-globs? include-matchers f))
                          (not (match-globs? exclude-matchers f))))
        roots (->> paths
                (mapv (fn [p]
                        (let [f (safe-path p)]
                          (when-not (.exists f)
                            (throw (ex-info (str "Path not found: " (.getPath f))
                                     {:type :ext.foundation.editing/path-not-found
                                      :path p})))
                          (.getCanonicalFile f))))
                (sort-by (fn [^File f]
                           [(count (iterator-seq (.iterator (.toPath f))))
                            (.getPath f)]))
                (reduce (fn [acc ^File f]
                          (if (some (fn [^File parent]
                                      (.startsWith (.toPath f) (.toPath parent)))
                                acc)
                            acc
                            (conj acc f)))
                  []))
        matches-line? (fn [line]
                        (case op
                          :all (every? #(str/includes? line %) needles)
                          :any (boolean (some #(str/includes? line %) needles))))
        walk (fn walk [ignore-node root ^File f]
               (cond
                 (and (not hidden?) (.isHidden f)) []
                 (and respect-gitignore? (ignored? ignore-node f root)) []
                 (.isDirectory f) (mapcat #(walk ignore-node root %)
                                    (or (.listFiles f) (into-array File [])))
                 (and (.isFile f) (include-file? f)) [f]
                 :else []))
        files (->> roots
                (mapcat (fn [root]
                          (let [ignore-node (when respect-gitignore?
                                              (load-ignore-node root))]
                            (walk ignore-node root root))))
                (sort-by rel-path))
        hits (atom [])
        seen (atom #{})
        capped? (atom false)]
    (doseq [^File f files :while (not @capped?)]
      (try
        (with-open [r (io/reader f)]
          (loop [line-no 1
                 lines (line-seq r)]
            (when-let [line (first lines)]
              (when (matches-line? line)
                (let [hit {:path (rel-path f)
                           :line line-no
                           :text (subs line 0 (min 400 (count line)))}
                      hit-key [(:path hit) (:line hit) (:text hit)]]
                  (when-not (contains? @seen hit-key)
                    (swap! seen conj hit-key)
                    (swap! hits conj hit)
                    (when (>= (count @hits) limit)
                      (reset! capped? true)))))
              (when-not @capped?
                (recur (inc line-no) (rest lines))))))
        (catch Throwable _ nil)))
    {:hits (vec @hits)
     :truncated-by (if @capped? :internal-cap :end-of-results)}))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{:path :search :replace})

(defn- coerce-patch-edits
  [edits]
  (let [edits (if (map? edits) [edits] edits)]
    (when-not (sequential? edits)
      (throw (ex-info "v/patch expects a map or vector of edit maps"
               {:type :ext.foundation.editing/invalid-patch-edits
                :got  (type edits)})))
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "v/patch edit must be a map"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))]
              (when missing
                (throw (ex-info "v/patch edit missing required keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit}))))
            (update edit :path str))
      edits)))

(defn- occurrence-count
  [^String haystack ^String needle]
  (when (str/blank? needle)
    (throw (ex-info "v/patch :search must be non-blank"
             {:type :ext.foundation.editing/invalid-patch-search})))
  (loop [idx 0 n 0]
    (let [hit (str/index-of haystack needle idx)]
      (if (nil? hit)
        n
        (recur (+ hit (count needle)) (inc n))))))

(def ^:private patch-search-preview-chars 180)

(defn- search-preview
  [s]
  (let [s (str s)]
    (if (<= (count s) patch-search-preview-chars)
      s
      (str (subs s 0 patch-search-preview-chars)
        "...<+" (- (count s) patch-search-preview-chars) " chars>"))))

(defn- patch-analysis
  [edits]
  (let [edits (coerce-patch-edits edits)]
    (loop [idx 0
           remaining edits
           states {}
           checks []
           failures []]
      (if-let [{:keys [path search replace]} (first remaining)]
        (let [file    (ensure-existing-file! (safe-path path))
              rel     (rel-path file)
              before  (or (get-in states [path :before]) (slurp file))
              current (or (get-in states [path :after]) before)
              search  (str search)
              replace (str replace)
              matches (occurrence-count current search)
              check   {:edit-index idx
                       :path rel
                       :matches matches
                       :search-preview (search-preview search)}]
          (if (= 1 matches)
            (recur (inc idx)
              (next remaining)
              (assoc states path {:file file
                                  :path rel
                                  :before before
                                  :after (str/replace-first current
                                           (re-pattern (java.util.regex.Pattern/quote search))
                                           (java.util.regex.Matcher/quoteReplacement replace))})
              (conj checks check)
              failures)
            (recur (inc idx)
              (next remaining)
              states
              (conj checks check)
              (conj failures check))))
        {:plans (vals states)
         :checks checks
         :failures failures
         :valid? (empty? failures)}))))

(defn- patch-failure-message
  [failures]
  (let [{:keys [edit-index path matches]} (first failures)]
    (if (= 1 (count failures))
      (str "v/patch edit " edit-index " failed in " path
        "; matched " matches " time(s)")
      (str "v/patch " (count failures) " edits failed; first edit " edit-index
        " in " path " matched " matches " time(s)"))))

(defn- patch-plan
  [edits]
  (let [{:keys [plans failures checks]} (patch-analysis edits)]
    (when (seq failures)
      (throw (ex-info (patch-failure-message failures)
               {:type :ext.foundation.editing/patch-search-not-unique
                :failures failures
                :checks checks})))
    plans))

(defn patch-check
  [edits]
  (let [{:keys [checks failures valid?]} (patch-analysis edits)]
    {:valid? valid?
     :checks checks
     :failures failures}))

(defn patch-safe
  [edits]
  (let [plans (vec (patch-plan edits))]
    (doseq [{:keys [file after]} plans]
      (spit file after))
    (mapv #(select-keys % [:path :before :after]) plans)))

(defn- create-dirs-safe [path]
  (let [f (safe-path path)]
    (fs/create-dirs f)
    (rel-path f)))

(defn- glob-matcher
  [pattern]
  (.getPathMatcher (java.nio.file.FileSystems/getDefault)
    (str "glob:" pattern)))

(defn- glob-matchers
  [pattern scope]
  (cond-> [(glob-matcher pattern)]
    (and (= scope :recursive)
      (str/starts-with? pattern "**/"))
    (conj (glob-matcher (subs pattern 3)))))

(defn- glob-scope
  [pattern opts]
  (let [scope (:scope opts)]
    (cond
      (nil? scope)
      (if (or (str/includes? pattern "/")
            (str/includes? pattern "**"))
        :recursive
        :children)

      (#{:children :recursive} scope)
      scope

      :else
      (throw (ex-info "v/glob :scope must be :children or :recursive"
               {:type :ext.foundation.editing/invalid-glob-opts
                :scope scope
                :opts opts})))))

(defn- visible-path?
  [^File f {:keys [hidden? respect-gitignore? ignore-node root]}]
  (and (or hidden? (not (.isHidden f)))
    (or (not respect-gitignore?)
      (not (ignored? ignore-node f root)))))

(defn- visible-descendants
  [^File root opts]
  (letfn [(walk [^File f]
            (when (visible-path? f opts)
              (if (.isDirectory f)
                (let [kids (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %))
                             (or (.listFiles f) []))]
                  (into [] (mapcat walk) kids))
                [f])))]
    (vec (mapcat walk (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %))
                        (or (.listFiles root) []))))))

(defn- glob-safe
  ([root pattern]
   (glob-safe root pattern nil))
  ([root pattern opts]
   (let [root-file (ensure-existing-dir! (safe-path root))
         {:keys [hidden? respect-gitignore?]
          :or {hidden? false respect-gitignore? true}} (or opts {})
         scope    (glob-scope pattern opts)
         opts*    {:hidden? hidden?
                   :respect-gitignore? respect-gitignore?
                   :ignore-node (when respect-gitignore? (load-ignore-node root-file))
                   :root root-file}
         matchers (glob-matchers pattern scope)
         rel*     (fn [^File f]
                    (str (.relativize (.toPath root-file) (.toPath f))))
         matches? (fn [^File f]
                    (let [p (java.nio.file.Paths/get (rel* f) (make-array String 0))]
                      (some #(.matches ^java.nio.file.PathMatcher % p) matchers)))
         candidates (case scope
                      :children (visible-children root-file opts*)
                      :recursive (visible-descendants root-file opts*))]
     {:paths (->> candidates
               (filter matches?)
               (mapv display-path))
      :scope scope})))

(defn- copy-safe
  ([src dest]
   (copy-safe src dest nil))
  ([src dest opts]
   (let [src-file  (safe-path src)
         dest-file (safe-path dest)]
     (ensure-parent-dirs! dest-file)
     (fs/copy src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- move-safe
  ([src dest]
   (move-safe src dest nil))
  ([src dest opts]
   (let [src-file  (safe-path src)
         dest-file (safe-path dest)]
     (ensure-parent-dirs! dest-file)
     (fs/move src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- delete-safe [path]
  (fs/delete (safe-path path))
  true)

(defn- delete-if-exists-safe [path]
  (fs/delete-if-exists (safe-path path)))

(defn- exists-safe? [path]
  (fs/exists? (safe-path path)))

(defn- coerce-bash-opts [opts]
  (when (and (some? opts) (not (map? opts)))
    (throw (ex-info "v/bash opts must be a map"
             {:type :ext.foundation.editing/invalid-bash-opts :opts opts})))
  (let [timeout-ms       (long (or (:timeout-ms opts) default-bash-timeout-ms))
        max-output-chars (long (or (:max-output-chars opts) default-bash-max-output-chars))
        cwd              (or (:cwd opts) ".")
        stdin            (:stdin opts)]
    (when-not (pos? timeout-ms)
      (throw (ex-info "v/bash :timeout-ms must be a positive integer"
               {:type :ext.foundation.editing/invalid-bash-opts
                :opt  :timeout-ms :got (:timeout-ms opts)})))
    (when-not (pos? max-output-chars)
      (throw (ex-info "v/bash :max-output-chars must be a positive integer"
               {:type :ext.foundation.editing/invalid-bash-opts
                :opt  :max-output-chars :got (:max-output-chars opts)})))
    (when (and (some? stdin) (not (string? stdin)))
      (throw (ex-info "v/bash :stdin must be a string when provided"
               {:type :ext.foundation.editing/invalid-bash-opts
                :opt  :stdin :got (type stdin)})))
    {:timeout-ms       timeout-ms
     :max-output-chars max-output-chars
     :cwd              cwd
     :stdin            stdin}))

(defn- read-stream-limited
  [^InputStream in max-output-chars]
  (let [limit (long max-output-chars)]
    (with-open [^Reader reader (io/reader in)]
      (let [^StringBuilder out (StringBuilder.)
            ^chars buffer (char-array 4096)]
        (loop [total 0]
          (let [n (.read reader buffer 0 (alength buffer))]
            (if (neg? n)
              {:text       (str out)
               :chars      total
               :truncated? (> total limit)}
              (do
                (let [appended (.length out)
                      remaining (- limit appended)
                      take-n (int (max 0 (min n remaining)))]
                  (when (pos? take-n)
                    (.append out buffer 0 take-n)))
                (recur (+ total n))))))))))

(defn- write-stdin-and-close!
  [^Process process stdin]
  (with-open [^Writer writer (io/writer (.getOutputStream process))]
    (when (some? stdin)
      (.write writer ^String stdin))))

(defn- bash-warnings
  [exit stderr]
  (cond-> []
    (and (zero? (long exit))
      (string? stderr)
      (re-find #"(?m)^Traceback \(most recent call last\):" stderr))
    (conj {:type :stderr-traceback-with-zero-exit
           :message "stderr contains a Python Traceback even though bash exited 0; a swallowed script error may have occurred. v/bash runs with set -euo pipefail; inspect the child command for swallowed failures."})))

(defn- run-bash-safe
  ([command]
   (run-bash-safe command nil))
  ([command opts]
   (when-not (string? command)
     (throw (ex-info "v/bash command must be a string"
              {:type :ext.foundation.editing/invalid-bash-command
               :got (type command)})))
   (when (str/blank? command)
     (throw (ex-info "v/bash command must be non-blank"
              {:type :ext.foundation.editing/invalid-bash-command})))
   (when (and (re-find #"(?i)\.(clj|cljc|cljs|edn)(['\"\s]|$)" command)
           (or (re-find #"(?i)\bpython3?\b|\bperl\b|\bruby\b" command)
             (re-find #"(?i)\bsed\s+-i\b" command))
           (re-find #"(?is)(write_text|open\s*\([^)]*['\"]w|perl\s+-p?i|ruby\s+-p?i|sed\s+-i|>\s*[^\n]*\.(clj|cljc|cljs|edn))" command))
     (throw (ex-info "Refusing v/bash shell edit of Clojure/EDN source; use z/patch after z/locators or z/symbols."
              {:type :ext.foundation.editing/bash-clojure-source-edit-blocked
               :reason :use-z-patch
               :command-preview (subs command 0 (min 240 (count command)))})))
   (let [{:keys [cwd timeout-ms max-output-chars stdin]} (coerce-bash-opts opts)
         cwd-file (ensure-existing-dir! (safe-path cwd))
         cwd-rel (display-path cwd-file)
         argv (java.util.ArrayList. ^java.util.Collection ["/usr/bin/env" "bash" "-lc" command])
         pb (doto (ProcessBuilder. ^java.util.List argv)
              (.directory cwd-file))
         started (now-ms)
         ^Process process (.start pb)
         stdout-f (future (read-stream-limited (.getInputStream process) max-output-chars))
         stderr-f (future (read-stream-limited (.getErrorStream process) max-output-chars))
         stdin-f (future (write-stdin-and-close! process stdin))]
     (try
       (let [done? (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
             _ (when-not done?
                 (.destroyForcibly process)
                 (.waitFor process 1000 TimeUnit/MILLISECONDS))
             finished (now-ms)
             stdout @stdout-f
             stderr @stderr-f
             exit (if done? (.exitValue process) 124)
             _ (try @stdin-f (catch Throwable _ nil))]
         {:command command
          :cwd cwd-rel
          :exit exit
          :timed-out? (not done?)
          :timeout-ms timeout-ms
          :duration-ms (long (max 0 (- finished started)))
          :stdout (:text stdout)
          :stderr (:text stderr)
          :stdout-chars (:chars stdout)
          :stderr-chars (:chars stderr)
          :stdout-truncated? (:truncated? stdout)
          :stderr-truncated? (:truncated? stderr)
          :warnings (bash-warnings exit (:text stderr))})
       (catch InterruptedException e
         (.destroyForcibly process)
         (future-cancel stdout-f)
         (future-cancel stderr-f)
         (future-cancel stdin-f)
         (throw e))))))

(defn- bounded-render-text
  [s]
  (let [s (str s)]
    (if (> (count s) journal-render-chars)
      (str (subs s 0 journal-render-chars)
        "\n...<+" (- (count s) journal-render-chars) " chars>")
      s)))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-tool
  "Read the whole text file into `:lines`. `v/cat` has no display or pagination opts; the journal renders a bounded preview (first 50 + last 50 lines) and the full vector stays bound for slicing. Returns {:path :offset :total-lines :truncated-by :lines}."
  ([path]
   (let [out (read-file path)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result out
        :info {:lines-returned (count (:lines out))
               :offset (:offset out)
               :total-lines (:total-lines out)
               :truncated-by (:truncated-by out)}
        :presentation {:kind :source
                       :path (:path out)
                       :line-key :lines
                       :offset (:offset out)}}))))

(defn- ls-tool
  "Preview directory tree. Returns nested tree payload."
  ([path]
   (ls-tool path nil))
  ([path opts]
   (let [out (list-files path opts)]
     (tool-success
       {:op :v/ls
        :path path
        :kind :dir
        :result out
        :info {:depth (:depth opts)
               :hidden? (:hidden? opts)
               :respect-gitignore? (get opts :respect-gitignore? true)}
        :presentation {:kind :tree}}))))

(defn- rg-tool
  "Search file contents with one spec-map grammar: (v/rg {:all [...] :paths [...]}) or (v/rg {:any [...] :paths [...]}). Exactly one of :all/:any. :paths defaults to [\".\"]. All collection fields are vectors. Optional filters: :include and :exclude glob vectors, plus :hidden? and :respect-gitignore?. Unknown keys throw. Acquisition has a private hard cap; bind the result and slice for display. Returns {:hits [...] :truncated-by ...}. Use `v/glob` for path matching."
  [spec]
  (let [{:keys [paths include exclude] :as coerced} (coerce-rg-spec spec)
        out (grep-files spec)]
    (tool-success
      {:op :v/rg
       :path (if (= 1 (count paths))
               (first paths)
               ".")
       :kind :dir
       :result out
       :info {:spec spec
              :query-op (:op coerced)
              :paths paths
              :include include
              :exclude exclude
              :hit-count (count (:hits out))
              :truncated-by (:truncated-by out)}
       :presentation {:kind :search-hits
                      :row-keys [:path :line :text]}})))

(defn- patch-tool
  "Canonical exact text patch. Takes one edit map or a vector of maps with required keys `:path`, `:search`, `:replace`. Every `:search` must match exactly once in the current file; all edits validate before any write. Returns changed path summaries."
  [edits]
  (let [plans (patch-safe edits)]
    (tool-success
      {:op :v/patch
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

(defn- patch-check-tool
  "Preflight canonical exact text patches without writing. Returns match counts for each edit plus bounded search previews; use before v/patch when searches may be stale or multi-edit risk is high."
  [edits]
  (let [out (patch-check edits)]
    (tool-success
      {:op :v/patch-check
       :path (or (:path (first (:checks out))) ".")
       :kind :file
       :result out
       :info {:valid? (:valid? out)
              :edit-count (count (:checks out))
              :failure-count (count (:failures out))}})))

(defn- create-dirs-tool
  "Ensure dir exists. Tool result."
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :v/create-dirs
       :path path
       :kind :dir
       :result out
       :info {:created? (not before)
              :already-existed? before}})))

(defn- glob-tool
  "Path matching. Returns matching cwd-relative path strings. Simple patterns like `*` and `*.clj` match immediate children; recursive patterns like `**/*.clj` walk descendants. Opts may include `:scope :children|:recursive`, `:hidden?`, and `:respect-gitignore?`."
  ([root pattern]
   (glob-tool root pattern nil))
  ([root pattern opts]
   (let [{:keys [paths scope]} (glob-safe root pattern opts)]
     (tool-success
       {:op :v/glob
        :path root
        :kind :dir
        :result paths
        :info {:pattern pattern
               :scope scope
               :match-count (count paths)
               :opts opts}}))))

(defn- copy-tool
  "Copy path. Tool result."
  ([src dest]
   (copy-tool src dest nil))
  ([src dest opts]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :v/copy
        :path dest
        :kind :path
        :result out
        :info {:src (path->target src :path)
               :dest (path->target dest :path)
               :opts opts}}))))

(defn- move-tool
  "Move/rename path. Tool result."
  ([src dest]
   (move-tool src dest nil))
  ([src dest opts]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :v/move
        :path dest
        :kind :path
        :result out
        :info {:src (path->target src :path)
               :dest (path->target dest :path)
               :opts opts}}))))

(defn- delete-tool
  "Delete path. Tool result."
  [path]
  (delete-safe path)
  (tool-success
    {:op :v/delete
     :path path
     :kind :path
     :result nil
     :info {:deleted? true}}))

(defn- delete-if-exists-tool
  "Delete path if present. Tool result."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :v/delete-if-exists
       :path path
       :kind :path
       :result deleted?
       :info {:deleted? deleted?}})))

(defn- exists-tool
  "Existence check. Returns boolean."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :v/exists?
       :path path
       :kind :path
       :result exists?
       :info {:exists? exists?}})))

(defn- strict-bash-command
  [command]
  (str "set -euo pipefail\n" command))

(defn- bash-tool-result
  [op command opts]
  (let [out (run-bash-safe (strict-bash-command command) opts)]
    (tool-success
      {:op op
       :path (:cwd out)
       :kind :dir
       :result (assoc out :strict? true :original-command command)
       :info {:command command
              :cwd (:cwd out)
              :exit (:exit out)
              :status (if (:timed-out? out) :timeout :done)
              :timed-out? (:timed-out? out)
              :timeout-ms (:timeout-ms out)
              :duration-ms (:duration-ms out)
              :stdout-truncated? (:stdout-truncated? out)
              :stderr-truncated? (:stderr-truncated? out)
              :warnings (:warnings out)
              :opts (dissoc opts :stdin)
              :strict? true
              :strict-command (:command out)}
       :presentation {:kind :diagnostic}})))

(defn- bash-tool
  "Run bounded `/usr/bin/env bash -lc` in worktree with `set -euo pipefail` prepended. Returns shell payload map with :stdout, :stderr, :exit, :warnings, etc. Refuses shell-driven Clojure/EDN source edits; use z/patch for those. Emits :warnings when stderr looks like a swallowed failure."
  ([command]
   (bash-tool command nil))
  ([command opts]
   (bash-tool-result :v/bash command opts)))

;; =============================================================================
;; Structured renderers
;; =============================================================================

;; Inline markdown string-builder helpers (replaced the v/ DSL).
(defn- md-code       ^String [s] (str "`" s "`"))
(defn- md-code-block ^String [lang body] (str "```" (or lang "") "\n" body "\n```"))
(defn- md-p          ^String [& parts]
  (str/join " " (filter some? (map str parts))))
(defn- md-join       ^String [& parts]
  (str/join "\n\n" (filter some? (map str parts))))
(defn- md-ul         ^String [items]
  (str/join "\n" (map #(str "- " %) (filter some? items))))

(defn- render-edn-block
  ([value]
   (render-edn-block :channel value))
  ([surface value]
   (let [text (bounded-render-text (pr-str value))]
     (case surface
       :journal text
       (md-code-block "edn" text)))))

(defn- tree-entry-line
  [depth {:keys [name path type size] :as entry}]
  (let [indent (apply str (repeat depth "  "))
        label  (or name path (pr-str (dissoc entry :children)))]
    (str indent "- " label
      (when type (str " (" (clojure.core/name type) ")"))
      (when size (str " " size "B")))))

(defn- tree-lines
  ([entry] (tree-lines 0 entry))
  ([depth entry]
   (cond
     (map? entry)
     (cons (tree-entry-line depth entry)
       (mapcat #(tree-lines (inc depth) %) (:children entry)))

     (sequential? entry)
     (mapcat #(tree-lines depth %) entry)

     :else
     [(str (apply str (repeat depth "  ")) "- " (pr-str entry))])))

(def ^:private journal-head-tail-lines
  "How many leading and trailing lines a bounded reader (v/cat) shows in
   <journal>. The full value stays bound to the model's def; this slice is
   only the working-memory peek."
  50)

(defn- numbered-line-block
  "Format a sub-vector of source lines with absolute 1-based line numbers.
   `start-line` is the 1-based line number of `(first lines)`."
  [start-line lines]
  (->> lines
    (map-indexed (fn [idx line] (str (+ start-line idx) ": " line)))
    (str/join "\n")))

(defn- read-more-hint
  "Tail line on every bounded read renderer. Tells the model exactly how to
   reach the values that didn't fit."
  [bound-name]
  (str "… (bound to `" bound-name "`; slice raw payload fields with subvec/get-in to see more)"))

;; ---------------------------------------------------------------------------
;; Per-symbol renderers
;;
;; Engine contract:
;;   journal-render-fn -> (fn [result] string)
;;   channel-render-fn -> (fn [result] string)
;;
;; `result` is the raw payload returned to SCI. Engine handles
;; `:success? false` separately - error fns are optional and fall back to
;; `default-{journal,channel}-error-text`.
;; ---------------------------------------------------------------------------

(defn- journal-render-cat
  "v/cat journal preview: first 50 + last 50 lines, total counts, read-more
   hint. Plaintext for token-budgeted <journal>."
  [result]
  (let [{:keys [path lines total-lines offset truncated-by]} result
        n   (count lines)
        head (vec (take journal-head-tail-lines lines))
        tail (vec (drop (max (- n journal-head-tail-lines) 0) lines))
        head-block (numbered-line-block (inc offset) head)
        tail-block (numbered-line-block
                     (+ offset (max 0 (- n journal-head-tail-lines)) 1)
                     tail)
        elided (max 0 (- n (* 2 journal-head-tail-lines)))]
    (str "v/cat " path " — " n " line(s), total " total-lines
      ", truncated-by " (name (or truncated-by :none)) "\n"
      head-block
      (when (pos? elided) (str "\n… (" elided " line(s) elided)\n"))
      (when (pos? elided) tail-block)
      "\n" (read-more-hint "<your binding>"))))

(defn- channel-render-cat
  [result]
  (let [{:keys [path lines]} result
        body (numbered-line-block (inc (:offset result)) (vec lines))]
    (md-join
      (md-p "Read" (md-code path) "—" (count lines) "line(s).")
      (md-code-block "text" (bounded-render-text body)))))

(defn- journal-render-ls
  [result]
  (str "v/ls " (:path result) " — "
    (count (:children result)) " top-level entries\n"
    (bounded-render-text (str/join "\n" (tree-lines result)))))

(defn- channel-render-ls
  [result]
  (md-p "Directory tree of" (md-code (:path result)) "-"
    (count (:children result)) "top-level entries."))

(defn- journal-render-rg
  [result]
  (let [hits (or (:hits result) [])
        n    (count hits)
        shown (vec (take 20 hits))
        body (->> shown
               (map (fn [{:keys [path line text]}]
                      (str path ":" line " " text)))
               (str/join "\n"))]
    (str "v/rg — " n " hit(s), truncated-by "
      (name (or (:truncated-by result) :none)) "\n"
      body
      (when (> n (count shown))
        (str "\n… (" (- n (count shown)) " more; bind hits and slice)")))))

(defn- channel-render-rg
  [result]
  (let [hits (or (:hits result) [])]
    (md-join
      (md-p "Searched —" (count hits) "hit(s), truncated-by"
        (md-code (name (or (:truncated-by result) :none))) ".")
      (when (seq hits)
        (md-code-block "text"
          (bounded-render-text
            (str/join "\n"
              (map (fn [{:keys [path line text]}]
                     (str path ":" line " " text)) hits))))))))

(defn- journal-render-patch
  [_result]
  ;; Patch result is the per-file path map; the interesting data lives on
  ;; :info (which the journal renderer does not see). Keep the journal
  ;; entry minimal - one line confirming a write happened.
  "v/patch — wrote edit(s) (full diff visible in channel render)")

(defn- channel-render-patch
  [result]
  ;; `result` here is the [{:path ...}] vec, but the rich diff data lives on
  ;; the tool-result `:info` map which the contract does not expose to
  ;; renderers. Show the per-file paths; full diff is recoverable via
  ;; (get-in tool-result [:info :files]) when the model binds the result.
  (let [files (if (sequential? result) result [result])]
    (md-join
      (md-p "Patched" (count files) "file(s).")
      (md-ul (map (fn [{:keys [path]}] (md-code path)) files)))))

(defn- journal-render-patch-check
  [result]
  (str "v/patch-check — " (pr-str result)))

(defn- channel-render-patch-check
  [result]
  (render-edn-block :channel result))

(defn- journal-render-create-dirs
  [result]
  (str "v/create-dirs — ensured " result))

(defn- channel-render-create-dirs
  [result]
  (md-p "Ensured dir" (md-code result) "."))

(defn- journal-render-glob
  [result]
  (let [matches (vec (or result []))
        n (count matches)
        shown (vec (take 40 matches))]
    (str "v/glob — " n " match(es)\n"
      (str/join "\n" shown)
      (when (> n (count shown))
        (str "\n… (" (- n (count shown)) " more; bind result and slice)")))))

(defn- channel-render-glob
  [result]
  (let [matches (vec (or result []))]
    (md-join
      (md-p "Glob —" (count matches) "match(es).")
      (when (seq matches)
        (md-code-block "text" (bounded-render-text (str/join "\n" matches)))))))

(defn- journal-render-copy
  [result]
  (str "v/copy — wrote " result))

(defn- channel-render-copy
  [result]
  (md-p "Copied to" (md-code result) "."))

(defn- journal-render-move
  [result]
  (str "v/move — wrote " result))

(defn- channel-render-move
  [result]
  (md-p "Moved to" (md-code result) "."))

(defn- journal-render-delete
  [result]
  (str "v/delete — " (pr-str result)))

(defn- channel-render-delete
  [result]
  (md-p "Deleted." (md-code (pr-str result))))

(defn- journal-render-delete-if-exists
  [result]
  (str "v/delete-if-exists — "
    (if result "deleted" "already absent")))

(defn- channel-render-delete-if-exists
  [result]
  (if result
    (md-p "Deleted.")
    (md-p "Already absent.")))

(defn- journal-render-exists?
  [result]
  (str "v/exists? — " (pr-str result)))

(defn- channel-render-exists?
  [result]
  (md-p "Exists?" (md-code (pr-str result))))

(defn- journal-render-bash
  [result]
  (let [{:keys [command exit duration-ms stdout stderr stdout-truncated? stderr-truncated?]} result
        head (fn [s n] (let [s (or s "")]
                         (if (<= (count s) n) s
                           (str (subs s 0 n) "…<+" (- (count s) n) " chars>"))))]
    (str "v/bash — exit " exit ", " duration-ms "ms"
      (when stdout-truncated? " (stdout truncated)")
      (when stderr-truncated? " (stderr truncated)")
      "\ncmd: " (head command 200)
      (when-not (str/blank? stdout)
        (str "\nstdout: " (head stdout 600)))
      (when-not (str/blank? stderr)
        (str "\nstderr: " (head stderr 600))))))

(defn- channel-render-bash
  [result]
  (let [{:keys [command cwd exit timed-out? timeout-ms duration-ms stdout stderr
                stdout-truncated? stderr-truncated? warnings]} result
        warning-text (when (seq warnings)
                       (md-join "warnings:"
                         (md-ul (map :message warnings))))
        out (cond-> []
              (seq warning-text)
              (conj warning-text)
              (not (str/blank? stdout))
              (conj (md-join "stdout:" (md-code-block "text" (bounded-render-text stdout))))
              (not (str/blank? stderr))
              (conj (md-join "stderr:" (md-code-block "text" (bounded-render-text stderr)))))]
    (md-join
      (md-p "Ran bash in" (md-code cwd) "- exit" (md-code exit) "," duration-ms "ms"
        (when timed-out? (str ", timed out after " timeout-ms "ms"))
        (when (or stdout-truncated? stderr-truncated?) ", output truncated")
        (when (seq warnings) (str ", " (count warnings) " warning(s)"))
        ".")
      (md-p "Command:" (md-code command))
      (when (seq out) (apply md-join out)))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Symbol declarations.
;;
;; Each underlying `xxx-tool` defn carries the canonical docstring + arglists
;; on its var. `vis/symbol` reads them straight from the var meta - the
;; SCI sandbox sees the same text the prompt-listing renders.
;; `:sym` overrides the var name (`cat-tool` -> `cat`) for the model-facing
;; surface; everything else (examples, render-fn, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(def cat-symbol
  (vis/symbol #'cat-tool
    {:sym 'cat

     :journal-render-fn journal-render-cat
     :channel-render-fn channel-render-cat
     :on-error-fn (tool-failure-on-error :v/cat :file nil)}))

(def ls-symbol
  (vis/symbol #'ls-tool
    {:sym 'ls

     :journal-render-fn journal-render-ls
     :channel-render-fn channel-render-ls
     :on-error-fn (tool-failure-on-error :v/ls :dir nil)}))

(def rg-symbol
  (vis/symbol #'rg-tool
    {:sym 'rg

     :journal-render-fn journal-render-rg
     :channel-render-fn channel-render-rg
     :on-error-fn (tool-failure-on-error :v/rg :dir nil)}))

(def patch-symbol
  (vis/symbol #'patch-tool
    {:sym 'patch

     :journal-render-fn journal-render-patch
     :channel-render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :v/patch :file nil)}))

(def patch-check-symbol
  (vis/symbol #'patch-check-tool
    {:sym 'patch-check

     :journal-render-fn journal-render-patch-check
     :channel-render-fn channel-render-patch-check
     :on-error-fn (tool-failure-on-error :v/patch-check :file nil)}))

(def create-dirs-symbol
  (vis/symbol #'create-dirs-tool
    {:sym 'create-dirs

     :journal-render-fn journal-render-create-dirs
     :channel-render-fn channel-render-create-dirs
     :on-error-fn (tool-failure-on-error :v/create-dirs :dir nil)}))

(def glob-symbol
  (vis/symbol #'glob-tool
    {:sym 'glob

     :journal-render-fn journal-render-glob
     :channel-render-fn channel-render-glob
     :on-error-fn (tool-failure-on-error :v/glob :dir nil)}))

(def copy-symbol
  (vis/symbol #'copy-tool
    {:sym 'copy

     :journal-render-fn journal-render-copy
     :channel-render-fn channel-render-copy
     :on-error-fn (tool-failure-on-error :v/copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
    {:sym 'move

     :journal-render-fn journal-render-move
     :channel-render-fn channel-render-move
     :on-error-fn (tool-failure-on-error :v/move :path nil)}))

(def delete-symbol
  (vis/symbol #'delete-tool
    {:sym 'delete

     :journal-render-fn journal-render-delete
     :channel-render-fn channel-render-delete
     :on-error-fn (tool-failure-on-error :v/delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol #'delete-if-exists-tool
    {:sym 'delete-if-exists

     :journal-render-fn journal-render-delete-if-exists
     :channel-render-fn channel-render-delete-if-exists
     :on-error-fn (tool-failure-on-error :v/delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol #'exists-tool
    {:sym 'exists?

     :journal-render-fn journal-render-exists?
     :channel-render-fn channel-render-exists?
     :on-error-fn (tool-failure-on-error :v/exists? :path nil)}))

(def bash-symbol
  (vis/symbol #'bash-tool
    {:sym 'bash

     :journal-render-fn journal-render-bash
     :channel-render-fn channel-render-bash
     :on-error-fn (tool-failure-on-error :v/bash :dir nil)}))

(defn available-editing-symbols
  []
  (cond-> [cat-symbol
           ls-symbol
           rg-symbol
           patch-symbol
           patch-check-symbol
           create-dirs-symbol
           glob-symbol
           copy-symbol
           move-symbol
           delete-symbol
           delete-if-exists-symbol
           exists?-symbol]
    (not (config/bash-disabled?))
    (into [bash-symbol])))

(defn available-editing-prompt
  []
  (str
    "`v/` strategy: combine v/rg/v/glob/v/ls to locate, v/cat to read, then bind raw payloads and slice with normal Clojure. "
    "Use v/patch for exact raw-text edits and v/patch-check when uniqueness is uncertain; use z/patch for Clojure/EDN when `z/` is active. "
    "Use path ops for filesystem moves/deletes/copies. "
    (if (config/bash-disabled?)
      "v/bash is disabled; stay in tools/REPL. "
      "Use v/bash only for process boundaries (git, verify, CLI); it runs pipefail and refuses shell Clojure/EDN edits. ")))

(def editing-symbols
  "Default editing symbol set for docs/tests. Runtime extension registration calls
   `available-editing-symbols` so project-local config can disable bash symbols
   before the sandbox is assembled."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests. Runtime prompt assembly calls
   `available-editing-prompt` so bash guidance matches project-local config."
  (available-editing-prompt))
