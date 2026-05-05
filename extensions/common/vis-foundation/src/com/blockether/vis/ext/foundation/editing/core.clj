(ns com.blockether.vis.ext.foundation.editing.core
  "Filesystem tools exposed under the `v/` alias in the SCI sandbox.

   Two layers:

   1. Structured helpers for preview / tree / search:

        (v/cat path)            ; -> {:path :offset :total-lines :truncated-by :lines ...}
        (v/preview value eql?)  ; -> selected projection + shape for journal/TUI
        (v/ls path)             ; -> nested {:name :path :type :size :children} tree
        (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
        (v/rg patterns path)    ; -> {:hits :truncated-by}; patterns = non-empty vec of literal substrings

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
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md])
  (:import
   (com.google.re2j Pattern)
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
(def ^:private diff-context-lines 3)

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.
  (let [cwd (fs/cwd)
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
  (let [cwd (.toAbsolutePath (fs/path (fs/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

(defn- display-path [p]
  (let [cwd  (.normalize (.toAbsolutePath (fs/path (fs/cwd))))
        path (.normalize (.toAbsolutePath (fs/path p)))]
    (if (= path cwd)
      "."
      (str (.relativize cwd path)))))

(defn- ensure-parent-dirs! [^File f]
  (when-let [parent (.getParentFile f)]
    (.mkdirs parent))
  f)

(def ^:private tool-result-spec ::extension/tool-result)

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

(defn- tool-failure-on-error
  [op kind _render-fn]
  (fn [err _env _f args]
    (let [path (first args)
          target (path->target path kind)
          t (now-ms)]
      {:result (extension/failure
                 {:result     nil
                  :provenance {:op             op
                               :target         target
                               :started-at-ms  t
                               :finished-at-ms t
                               :duration-ms    0}
                  :throwable  err})})))

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
    (throw (ex-info "v/cat reads the whole file; use v/preview EQL for display ranges"
             {:type :ext.foundation.editing/invalid-cat-opts
              :opts opts}))))

(defn- read-file
  "Read the whole text file as pure structured data.

   Returns :path, :offset, :total-lines, :truncated-by, and raw :lines.
   No prose footer, no line-number prefix, no display limit. Use
   `v/preview` EQL to project ranges into the journal/TUI."
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

(defn- compile-pattern
  "v/rg accepts ONLY a non-empty vector of literal substrings. Each
   element is matched LITERALLY (PCRE metacharacters are escaped via
   `Pattern/quote`); the elements are OR'd together internally. This
   removes every regex-DSL footgun (`\\|`, `\\.`, `\\d`, ...) by making
   regex syntax unrepresentable on the input side. For genuine regex
   needs, drop to `(re-seq #\"...\" (slurp f))` in the SCI sandbox."
  [p]
  (cond
    (and (vector? p) (seq p) (every? string? p))
    (Pattern/compile (str/join "|" (map #(Pattern/quote %) p)))

    (and (vector? p) (empty? p))
    (throw (ex-info "v/rg patterns vector must be non-empty."
             {:type :ext.foundation.editing/empty-patterns}))

    (vector? p)
    (throw (ex-info "v/rg patterns vector must contain only strings."
             {:type :ext.foundation.editing/non-string-in-patterns
              :got  (mapv type p)}))

    (string? p)
    (throw (ex-info
             (str "v/rg pattern must be a vector of literal strings, "
               "not a string. Wrap in a vector: [\"" p "\"]. For multiple "
               "alternatives use [\"a\" \"b\" \"c\"]. No regex DSL.")
             {:type :ext.foundation.editing/string-pattern-rejected
              :got  p}))

    (instance? java.util.regex.Pattern p)
    (throw (ex-info
             (str "v/rg pattern must be a vector of literal strings, "
               "not a regex literal. For #\"foo|bar\" use [\"foo\" \"bar\"]. "
               "For genuine regex needs use (re-seq #\"...\" (slurp f)).")
             {:type :ext.foundation.editing/regex-pattern-rejected}))

    :else
    (throw (ex-info "v/rg pattern must be a non-empty vector of strings."
             {:type :ext.foundation.editing/invalid-pattern-type
              :got  (type p)}))))

(defn- grep-files
  "Walk `path`, OR-grep against the literal-substring vector
   `pattern`, return pure structured data:

     {:hits [{:path \"src/x.clj\" :line 42 :text \"...\"} ...]
      :truncated-by :end-of-results | :limit}

   No heterogeneous capped-sentinel inside `:hits`. Model decides
   what to do with `:truncated-by :limit` (re-issue with bigger
   `:limit`, narrow `:patterns`, etc.) from the keyword alone."
  ([pattern path] (grep-files pattern path nil))
  ([pattern path opts]
   (let [{:keys [limit hidden? respect-gitignore?]
          :or {limit default-grep-limit hidden? false respect-gitignore? true}} (or opts {})
         pat   (compile-pattern pattern)
         root  (safe-path path)
         _     (when-not (.exists root)
                 (throw (ex-info (str "Path not found: " (.getPath root))
                          {:type :ext.foundation.editing/path-not-found})))
         ignore-node (when respect-gitignore? (load-ignore-node root))
         walk (fn walk [^File f]
                (cond
                  (and (not hidden?) (.isHidden f)) []
                  (and respect-gitignore? (ignored? ignore-node f root)) []
                  (.isDirectory f) (mapcat walk (.listFiles f))
                  (.isFile f) [f]
                  :else []))
         hits (atom [])
         capped? (atom false)]
     (try
       (doseq [^File f (walk root) :while (not @capped?)]
         (with-open [r (io/reader f)]
           (loop [line-no 1
                  lines   (line-seq r)]
             (when-let [line (first lines)]
               (when (.find (.matcher pat line))
                 (swap! hits conj
                   {:path (rel-path f)
                    :line line-no
                    :text (subs line 0 (min 400 (count line)))})
                 (when (>= (count @hits) limit)
                   (reset! capped? true)))
               (when-not @capped?
                 (recur (inc line-no) (rest lines)))))))
       (catch Throwable _ nil))
     {:hits         (vec @hits)
      :truncated-by (if @capped? :limit :end-of-results)})))

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

(defn- patch-plan
  [edits]
  (let [edits (coerce-patch-edits edits)]
    (loop [remaining edits
           states {}]
      (if-let [{:keys [path search replace]} (first remaining)]
        (let [file (ensure-existing-file! (safe-path path))
              before (or (get-in states [path :before]) (slurp file))
              current (or (get-in states [path :after]) before)
              search (str search)
              replace (str replace)
              matches (occurrence-count current search)]
          (when-not (= 1 matches)
            (throw (ex-info (str "v/patch :search must match exactly once in " path
                              "; matched " matches " time(s)")
                     {:type :ext.foundation.editing/patch-search-not-unique
                      :path path
                      :matches matches
                      :search search})))
          (recur (next remaining)
            (assoc states path {:file file
                                :path (rel-path file)
                                :before before
                                :after (str/replace-first current
                                         (re-pattern (java.util.regex.Pattern/quote search))
                                         (java.util.regex.Matcher/quoteReplacement replace))})))
        (vals states)))))

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

(defn- run-bash-safe
  ([command]
   (run-bash-safe command nil))
  ([command opts]
   (when-not (string? command)
     (throw (ex-info "v/bash command must be a string"
              {:type :ext.foundation.editing/invalid-bash-command
               :got  (type command)})))
   (when (str/blank? command)
     (throw (ex-info "v/bash command must be non-blank"
              {:type :ext.foundation.editing/invalid-bash-command})))
   (let [{:keys [cwd timeout-ms max-output-chars stdin]} (coerce-bash-opts opts)
         cwd-file (ensure-existing-dir! (safe-path cwd))
         cwd-rel  (display-path cwd-file)
         argv     (java.util.ArrayList. ^java.util.Collection ["/usr/bin/env" "bash" "-lc" command])
         pb       (doto (ProcessBuilder. ^java.util.List argv)
                    (.directory cwd-file))
         started  (now-ms)
         ^Process process (.start pb)
         stdout-f (future (read-stream-limited (.getInputStream process) max-output-chars))
         stderr-f (future (read-stream-limited (.getErrorStream process) max-output-chars))
         stdin-f  (future (write-stdin-and-close! process stdin))
         done?    (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
         _        (when-not done?
                    (.destroyForcibly process)
                    (.waitFor process))
         finished (now-ms)
         stdout   @stdout-f
         stderr   @stderr-f
         _        (try @stdin-f (catch Throwable _ nil))]
     {:command           command
      :cwd               cwd-rel
      :exit              (if done? (.exitValue process) 124)
      :timed-out?        (not done?)
      :duration-ms       (long (max 0 (- finished started)))
      :stdout            (:text stdout)
      :stderr            (:text stderr)
      :stdout-chars      (:chars stdout)
      :stderr-chars      (:chars stderr)
      :stdout-truncated? (:truncated? stdout)
      :stderr-truncated? (:truncated? stderr)})))

;; =============================================================================
;; Preview EQL
;; =============================================================================

(declare bounded-render-text)

(defn- payload-map
  [value]
  (if (and (map? value) (extension/tool-result? value))
    (select-keys value [:result :stdout :stderr :error])
    value))

(defn- small-head
  [value]
  (let [value (payload-map value)]
    (cond
      (string? value) (bounded-render-text value)
      (map? value) (into {} (take 8 value))
      (vector? value) (vec (take 8 value))
      (sequential? value) (vec (take 8 value))
      (set? value) (set (take 8 value))
      :else value)))

(defn- range-param-map?
  [x]
  (and (map? x) (or (contains? x :from) (contains? x :to))))

(defn- validate-range-bound!
  [k v]
  (when (and (some? v) (or (not (integer? v)) (neg? v)))
    (throw (ex-info (str "v/preview range " k " must be a non-negative integer")
             {:type :ext.foundation.editing/invalid-preview-eql
              :key k
              :value v}))))

(defn- slice-value
  [value {:keys [from to] :as params}]
  (let [_ (validate-range-bound! :from from)
        _ (validate-range-bound! :to to)
        from (or from 0)]
    (cond
      (string? value)
      (let [n (count value)
            to (or to n)]
        (when (> from to)
          (throw (ex-info "v/preview range :from cannot be greater than :to"
                   {:type :ext.foundation.editing/invalid-preview-eql
                    :params params})))
        (subs value (min from n) (min to n)))

      (sequential? value)
      (let [v (vec value)
            n (count v)
            to (or to n)]
        (when (> from to)
          (throw (ex-info "v/preview range :from cannot be greater than :to"
                   {:type :ext.foundation.editing/invalid-preview-eql
                    :params params})))
        (subvec v (min from n) (min to n)))

      :else
      (throw (ex-info "v/preview range can only be applied to text or sequential values"
               {:type :ext.foundation.editing/invalid-preview-eql
                :params params
                :value-shape (extension/result-shape value)})))))

(declare apply-preview-eql)

(defn- field-range-entry?
  [entry]
  (and (vector? entry)
    (keyword? (first entry))
    (range-param-map? (second entry))
    (<= 2 (count entry) 3)))

(defn- apply-field-range-entry
  [value [k params nested-eql]]
  (let [field-value (when (map? value) (get value k))
        ranged      (slice-value field-value params)
        projected   (if (some? nested-eql)
                      (if (and (sequential? ranged) (not (string? ranged)))
                        (mapv #(apply-preview-eql % nested-eql) ranged)
                        (apply-preview-eql ranged nested-eql))
                      ranged)]
    {k projected}))

(defn- apply-map-entry
  [value k nested-eql]
  {k (apply-preview-eql (when (map? value) (get value k)) nested-eql)})

(defn- apply-preview-vector-eql
  [value preview-eql]
  (cond
    (= [:*] preview-eql)
    (if (map? value) value (small-head value))

    (map? value)
    (reduce
      (fn [acc entry]
        (cond
          (= :* entry) (merge acc value)
          (keyword? entry) (assoc acc entry (get value entry))
          (field-range-entry? entry) (merge acc (apply-field-range-entry value entry))
          (map? entry) (merge acc (apply-preview-eql value entry))
          :else (throw (ex-info "Invalid v/preview EQL entry"
                         {:type :ext.foundation.editing/invalid-preview-eql
                          :entry entry}))))
      {}
      preview-eql)

    (and (sequential? value) (not (string? value)))
    (mapv #(apply-preview-eql % preview-eql) value)

    :else
    (small-head value)))

(defn- apply-preview-eql
  [value preview-eql]
  (let [value (payload-map value)]
    (cond
      (nil? preview-eql) (small-head value)
      (keyword? preview-eql) (if (map? value) {preview-eql (get value preview-eql)} (small-head value))
      (vector? preview-eql) (apply-preview-vector-eql value preview-eql)
      (map? preview-eql) (reduce-kv
                           (fn [acc k nested-eql]
                             (merge acc (apply-map-entry value k nested-eql)))
                           {}
                           preview-eql)
      :else (throw (ex-info "Invalid v/preview EQL"
                     {:type :ext.foundation.editing/invalid-preview-eql
                      :preview-eql preview-eql})))))

(defn- presentation-kind-from-result
  [result]
  (cond
    (and (map? result) (vector? (get-in result [:result :hits]))
      (every? #(and (map? %) (contains? % :path) (contains? % :line) (contains? % :text))
        (get-in result [:result :hits])))
    :search-hits

    (and (map? result) (vector? (get-in result [:result :lines])))
    :source

    (string? result)
    :text

    :else
    :data))

(defn- preview-presentation
  [value projection]
  (or (:presentation value)
    {:kind (presentation-kind-from-result projection)}))

(defn- preview-tool
  ([value]
   (preview-tool value nil))
  ([value preview-eql]
   (let [projection (apply-preview-eql value preview-eql)
         source-shape (extension/result-shape value)
         projection-shape (extension/result-shape projection)
         t (now-ms)]
     (assoc (extension/success
              {:result projection
               :provenance {:op :v/preview
                            :started-at-ms t
                            :finished-at-ms t
                            :duration-ms 0}})
       :preview-eql preview-eql
       :presentation (preview-presentation value projection)
       :source-shape source-shape
       :projection-shape projection-shape))))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-tool
  ([path]
   (let [out (read-file path)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result out
        :provenance {:lines-returned (count (:lines out))
                     :offset (:offset out)
                     :total-lines (:total-lines out)
                     :truncated-by (:truncated-by out)}
        :presentation {:kind :source
                       :path (:path out)
                       :line-key :lines
                       :offset (:offset out)}}))))

(defn- ls-tool
  ([path]
   (ls-tool path nil))
  ([path opts]
   (let [out (list-files path opts)]
     (tool-success
       {:op :v/ls
        :path path
        :kind :dir
        :result out
        :provenance {:depth (:depth opts)
                     :hidden? (:hidden? opts)
                     :respect-gitignore? (get opts :respect-gitignore? true)}
        :presentation {:kind :tree}}))))

(defn- rg-tool
  ([patterns path]
   (rg-tool patterns path nil))
  ([patterns path opts]
   (let [out (grep-files patterns path opts)]
     (tool-success
       {:op :v/rg
        :path path
        :kind :dir
        :result out
        :provenance {:patterns patterns
                     :hit-count (count (:hits out))
                     :truncated-by (:truncated-by out)
                     :opts opts}
        :presentation {:kind :search-hits
                       :row-keys [:path :line :text]}}))))

(defn- patch-tool
  [edits]
  (let [plans (patch-safe edits)]
    (tool-success
      {:op :v/patch
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

(defn- create-dirs-tool
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :v/create-dirs
       :path path
       :kind :dir
       :result out
       :provenance {:created? (not before)
                    :already-existed? before}})))

(defn- glob-tool
  ([root pattern]
   (glob-tool root pattern nil))
  ([root pattern opts]
   (let [{:keys [paths scope]} (glob-safe root pattern opts)]
     (tool-success
       {:op :v/glob
        :path root
        :kind :dir
        :result paths
        :provenance {:pattern pattern
                     :scope scope
                     :match-count (count paths)
                     :opts opts}}))))

(defn- copy-tool
  ([src dest]
   (copy-tool src dest nil))
  ([src dest opts]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :v/copy
        :path dest
        :kind :path
        :result out
        :provenance {:src (path->target src :path)
                     :dest (path->target dest :path)
                     :opts opts}}))))

(defn- move-tool
  ([src dest]
   (move-tool src dest nil))
  ([src dest opts]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :v/move
        :path dest
        :kind :path
        :result out
        :provenance {:src (path->target src :path)
                     :dest (path->target dest :path)
                     :opts opts}}))))

(defn- delete-tool
  [path]
  (delete-safe path)
  (tool-success
    {:op :v/delete
     :path path
     :kind :path
     :result nil
     :provenance {:deleted? true}}))

(defn- delete-if-exists-tool
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :v/delete-if-exists
       :path path
       :kind :path
       :result deleted?
       :provenance {:deleted? deleted?}})))

(defn- exists-tool
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :v/exists?
       :path path
       :kind :path
       :result exists?
       :provenance {:exists? exists?}})))

(defn- bash-tool
  ([command]
   (bash-tool command nil))
  ([command opts]
   (let [out (run-bash-safe command opts)]
     (tool-success
       {:op :v/bash
        :path (:cwd out)
        :kind :dir
        :result out
        :provenance {:command command
                     :cwd (:cwd out)
                     :exit (:exit out)
                     :timed-out? (:timed-out? out)
                     :stdout-truncated? (:stdout-truncated? out)
                     :stderr-truncated? (:stderr-truncated? out)
                     :opts (dissoc opts :stdin)}
        :presentation {:kind :diagnostic}}))))

;; =============================================================================
;; Structured renderers
;; =============================================================================

(defn- bounded-render-text
  [s]
  (let [s (str s)]
    (if (> (count s) journal-render-chars)
      (str (subs s 0 journal-render-chars)
        "\n…<+" (- (count s) journal-render-chars) " chars>")
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
    (bounded-render-text
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

(defn- preview-shape-text
  [tool-result]
  (bounded-render-text
    (pr-str {:source-shape (:source-shape tool-result)
             :projection-shape (:projection-shape tool-result)})))

(defn- result-lines
  [result]
  (or (get-in result [:result :lines])
    (:lines result)
    (when (and (sequential? result) (every? string? result)) result)))

(defn- result-hits
  [result]
  (or (get-in result [:result :hits])
    (:hits result)))

(defn- render-preview-body
  [tool-result]
  (let [result (:result tool-result)
        kind   (get-in tool-result [:presentation :kind])]
    (cond
      (and (= :source kind) (seq (result-lines result)))
      (let [lines (vec (result-lines result))
            from  (or (some-> tool-result :preview-eql pr-str (re-find #":from\s+(\d+)") second parse-long) 0)
            body  (->> lines
                    (map-indexed (fn [idx line] (str (+ from idx 1) ": " line)))
                    (str/join "\n"))]
        (md/code-block "text" (bounded-render-text body)))

      (and (= :search-hits kind) (seq (result-hits result)))
      (let [body (->> (result-hits result)
                   (map (fn [{:keys [path line text]}]
                          (str "- `" path ":" line "` " text)))
                   (str/join "\n"))]
        (md/code-block "text" (bounded-render-text body)))

      :else
      (md/code-block "edn" (bounded-render-text (pr-str result))))))

(defn- render-preview
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (md/join
      (md/p "Preview." "Shape:")
      (md/code-block "edn" (preview-shape-text tool-result))
      (render-preview-body tool-result))))

(defn- render-ls
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [out (:result tool-result)]
      (md/p "Directory tree of" (md/code (:path out)) "-"
        (count (:children out)) "top-level entries."))))

(defn- render-patch
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [files (get-in tool-result [:provenance :files])]
      (md/join
        (md/p "Patched" (count files) "file(s). Each :search matched exactly once before any write. Read back only when exact persisted bytes matter or external writers may interfere.")
        (for [{:keys [path changed? before after]} files
              :let [diff-txt (when changed? (unified-diff-text path before after))]]
          (md/join
            (md/p (md/code path) (when (false? changed?) "(no change)"))
            (when diff-txt
              (md/code-block "diff" diff-txt))))))))

(defn- render-create-dirs
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [path (:result tool-result)]
      (md/p "Ensured dir" (md/code path) "."))))

(defn- render-glob
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [pattern (get-in tool-result [:provenance :pattern])
          root (get-in tool-result [:provenance :target :requested])
          scope (get-in tool-result [:provenance :scope])
          matches (:result tool-result)]
      (md/join
        (md/p "Glob" (md/code pattern) "in" (md/code root) "-" (count matches) "match(es)"
          (when scope (str "(" (md/code (name scope)) " scope)"))
          ".")
        (when (seq matches)
          (md/code-block "text" (bounded-render-text (str/join "\n" matches))))))))

(defn- render-copy
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [src (get-in tool-result [:provenance :src :requested])
          dest (:result tool-result)]
      (md/p "Copied" (md/code src) "->" (md/code dest) "."))))

(defn- render-move
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [src (get-in tool-result [:provenance :src :requested])
          dest (:result tool-result)]
      (md/p "Moved" (md/code src) "->" (md/code dest) "."))))

(defn- render-delete
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [path (get-in tool-result [:provenance :target :requested])]
      (md/p "Deleted" (md/code path) "."))))

(defn- render-delete-if-exists
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [path (get-in tool-result [:provenance :target :requested])
          deleted? (:result tool-result)]
      (if deleted?
        (md/p "Deleted" (md/code path) ".")
        (md/p "Already absent" (md/code path) ".")))))

(defn- render-exists?
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [path (get-in tool-result [:provenance :target :requested])
          exists? (:result tool-result)]
      (md/p "Exists?" (md/code path) "->" (pr-str exists?)))))

(defn- render-cat
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [out (:result tool-result)]
      (md/p "Read" (md/code (:path out)) "- returned"
        (count (:lines out)) "line(s), offset" (:offset out)
        ", total" (:total-lines out)
        ", truncated-by" (md/code (name (:truncated-by out)))
        ". Use" (md/code "v/preview") "to project lines into the journal/TUI."))))

(defn- render-rg
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [out      (:result tool-result)
          patterns (get-in tool-result [:provenance :patterns])
          target   (get-in tool-result [:provenance :target :requested])]
      (md/p "Searched" (md/code target) "for" (md/code (pr-str patterns)) "-"
        (count (:hits out)) "hit(s), truncated-by" (md/code (name (:truncated-by out)))
        ". Use" (md/code "v/preview") "to project selected hits into the journal/TUI."))))

(defn- render-bash
  [{:keys [tool-result]}]
  (if-not (:ok? tool-result)
    (tool-error-text tool-result)
    (let [{:keys [command cwd exit timed-out? duration-ms stdout stderr
                  stdout-truncated? stderr-truncated?]} (:result tool-result)
          out (cond-> []
                (not (str/blank? stdout))
                (conj (md/join "stdout:" (md/code-block "text" (bounded-render-text stdout))))
                (not (str/blank? stderr))
                (conj (md/join "stderr:" (md/code-block "text" (bounded-render-text stderr)))))]
      (md/join
        (md/p "Ran bash in" (md/code cwd) "- exit" (md/code exit) "," duration-ms "ms"
          (when timed-out? ", timed out")
          (when (or stdout-truncated? stderr-truncated?) ", output truncated")
          ".")
        (md/p "Command:" (md/code command))
        (when (seq out) (apply md/join out))))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def cat-symbol
  (vis/symbol 'cat cat-tool
    {:doc "Read the whole text file into `[:result :lines]`. `v/cat` has no display or pagination opts; use `v/preview` EQL to select ranges for journal/TUI display. Tool result; `:result` = {:path :offset :total-lines :truncated-by :lines}."
     :arglists '([path])
     :examples ["(:result (v/cat \"src/main.clj\"))"
                "(get-in (v/cat \"src/main.clj\") [:result :lines])"
                "(v/preview (v/cat \"src/main.clj\") {:result [[:lines {:from 40 :to 120}]]})"]
     :result-spec tool-result-spec
     :render-fn render-cat
     :on-error-fn (tool-failure-on-error :v/cat :file nil)}))

(def preview-symbol
  (vis/symbol 'preview preview-tool
    {:doc "Project a value for journal/TUI display with EQL. With no EQL, returns shape plus a tiny safe head. EQL supports keys, [:*], nested pulls, and field ranges like {:result [[:lines {:from 40 :to 120}]]}. Preserves source presentation metadata and provenance boundary."
     :arglists '([value] [value preview-eql])
     :examples ["(v/preview file {:result [[:lines {:from 40 :to 120}]]})"
                "(v/preview hits {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})"
                "(v/preview state)"]
     :result-spec tool-result-spec
     :render-fn render-preview}))

(def ls-symbol
  (vis/symbol 'ls ls-tool
    {:doc "Preview directory tree. Tool result; `:result` = nested tree."
     :arglists '([path] [path opts])
     :examples ["(:result (v/ls \".\"))"
                "(v/ls \"src\" {:depth 3})"]
     :result-spec tool-result-spec
     :render-fn render-ls
     :on-error-fn (tool-failure-on-error :v/ls :dir nil)}))

(def rg-symbol
  (vis/symbol 'rg rg-tool
    {:doc "Search file contents for literal substrings. Tool result; `:result` = {:hits [...] :truncated-by ...}. Use `v/glob` for path matching."
     :arglists '([patterns path] [patterns path opts])
     :examples ["(:result (v/rg [\"defn render\"] \"src\"))"
                "(get-in (v/rg [\"defn render\"] \"src\") [:result :hits])"
                "(v/rg [\"border-top\" \"draw-border\"] \"src\" {:limit 50})"]
     :result-spec tool-result-spec
     :render-fn render-rg
     :on-error-fn (tool-failure-on-error :v/rg :dir nil)}))

(def patch-symbol
  (vis/symbol 'patch patch-tool
    {:doc "Canonical exact text patch. Takes one edit map or a vector of maps with required keys `:path`, `:search`, `:replace`. Every `:search` must match exactly once in the current file; all edits validate before any write. Tool result envelope."
     :arglists '([edits])
     :examples ["(v/patch [{:path \"src/main.clj\" :search \"old\" :replace \"new\"}])"
                "(v/patch {:path \"README.md\" :search \"alpha\" :replace \"beta\"})"]
     :result-spec tool-result-spec
     :render-fn render-patch
     :on-error-fn (tool-failure-on-error :v/patch :file nil)}))

(def create-dirs-symbol
  (vis/symbol 'create-dirs create-dirs-tool
    {:doc "Ensure dir exists. Tool result."
     :arglists '([path])
     :examples ["(v/create-dirs \"target/tmp/cache\")"]
     :result-spec tool-result-spec
     :render-fn render-create-dirs
     :on-error-fn (tool-failure-on-error :v/create-dirs :dir nil)}))

(def glob-symbol
  (vis/symbol 'glob glob-tool
    {:doc "Path matching. Tool result; `:result` = matching cwd-relative path strings. Simple patterns like `*` and `*.clj` match immediate children; recursive patterns like `**/*.clj` walk descendants. Opts may include `:scope :children|:recursive`, `:hidden?`, and `:respect-gitignore?`."
     :arglists '([root pattern] [root pattern opts])
     :examples ["(:result (v/glob \"src\" \"*.clj\"))"
                "(:result (v/glob \"src\" \"**/*.clj\"))"
                "(v/glob \"resources\" \"*.edn\" {:scope :children :hidden? true})"]
     :result-spec tool-result-spec
     :render-fn render-glob
     :on-error-fn (tool-failure-on-error :v/glob :dir nil)}))

(def copy-symbol
  (vis/symbol 'copy copy-tool
    {:doc "Copy path. Tool result."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/copy \"a.txt\" \"backup/a.txt\")"]
     :result-spec tool-result-spec
     :render-fn render-copy
     :on-error-fn (tool-failure-on-error :v/copy :path nil)}))

(def move-symbol
  (vis/symbol 'move move-tool
    {:doc "Move/rename path. Tool result."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/move \"tmp.txt\" \"archive/tmp.txt\")"]
     :result-spec tool-result-spec
     :render-fn render-move
     :on-error-fn (tool-failure-on-error :v/move :path nil)}))

(def delete-symbol
  (vis/symbol 'delete delete-tool
    {:doc "Delete path. Tool result."
     :arglists '([path])
     :examples ["(v/delete \"tmp.txt\")"]
     :result-spec tool-result-spec
     :render-fn render-delete
     :on-error-fn (tool-failure-on-error :v/delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol 'delete-if-exists delete-if-exists-tool
    {:doc "Delete path if present. Tool result."
     :arglists '([path])
     :examples ["(v/delete-if-exists \"tmp.txt\")"]
     :result-spec tool-result-spec
     :render-fn render-delete-if-exists
     :on-error-fn (tool-failure-on-error :v/delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol 'exists? exists-tool
    {:doc "Existence check. Tool result; `:result` = boolean."
     :arglists '([path])
     :examples ["(:result (v/exists? \"src/main.clj\"))"]
     :result-spec tool-result-spec
     :render-fn render-exists?
     :on-error-fn (tool-failure-on-error :v/exists? :path nil)}))

(def bash-symbol
  (vis/symbol 'bash bash-tool
    {:doc "Run bounded `/usr/bin/env bash -lc` in worktree. Tool result envelope; shell fields live under :result, e.g. :result :stdout, :result :stderr, :result :exit. Do not read (:stdout run) or (:exit run)."
     :arglists '([command] [command opts])
     :examples ["(def run (v/bash \"./verify.sh --quick\"))"
                "(get-in run [:result :stdout])"
                "(get-in run [:result :exit])"
                "(v/bash \"git status --short\" {:timeout-ms 10000 :max-output-chars 8000})"]
     :result-spec tool-result-spec
     :render-fn render-bash
     :on-error-fn (tool-failure-on-error :v/bash :dir nil)}))

(def editing-symbols
  [cat-symbol
   preview-symbol
   ls-symbol
   rg-symbol
   patch-symbol
   create-dirs-symbol
   glob-symbol
   copy-symbol
   move-symbol
   delete-symbol
   delete-if-exists-symbol
   exists?-symbol
   bash-symbol])

(def editing-prompt
  "`v/` files: Use structured tools for discovery and reads: (v/cat path), (v/rg [lits] path opts?), (v/glob root pat opts?), (v/ls path opts?). `v/cat` reads the whole file into [:result :lines]; bind full reads/searches once, then use `v/preview` to project what enters <journal>/TUI: (v/preview file {:result [[:lines {:from 40 :to 120}]]}), (v/preview hits {:result [[:hits {:from 0 :to 12} [:path :line :text]]]}), (v/preview value). Every preview includes shape in the journal; every preview returned in a block is rendered. EQL supports [:*] for all map keys at one level; plain keywords are payload keys, so :from/:to are selectable keys and ranges use field params like [[:hits {:from 0 :to 12} [:path :line :text]]]. `v/silent!` is removed. `v/cat` has no opts; cat is full acquisition only. `v/glob` returns cwd-relative path strings under `:result`. Simple patterns like `*` and `*.clj` match immediate children; recursive patterns like `**/*.clj` walk descendants. Example child listing: (->> (v/glob \"src\" \"*.clj\") :result sort vec). Example recursive search: (->> (v/glob \"extensions\" \"**/*.clj\") :result sort vec). Use `:scope :children` or `:scope :recursive` when you want to force the behavior. Edit text with canonical (v/patch [{:path p :search old :replace new} ...]); every :search must match exactly once and all edits validate before write. Read back after writes only when exact persisted bytes matter, external writers may interfere, or user explicitly asks for verification; otherwise use the tool diff/result and avoid duplicate reads. Path ops: (v/create-dirs path), (v/copy src dest), (v/move src dest), (v/delete path), (v/delete-if-exists path), (v/exists? path).
`v/` shell: Use `v/bash` for process boundaries like git, verify.sh, CLI entrypoints, or external commands: (v/bash cmd {:cwd \".\" :timeout-ms 30000 :max-output-chars 20000 :stdin s}).
Tool results are envelopes and expose their payload under `:result`. Examples: (get-in (v/cat \"IDEAS.md\") [:result :lines]), (get-in (v/bash \"pwd\") [:result :stdout]), (-> (v/rg [\"needle\"] \"src\") :result :hits). Tools own presentation metadata; preview preserves it. Provenance/lifecycle metadata stays unchanged and remains the proof substrate. For Clojure/EDN source edits prefer z/patch when `z/` is active; it uses zipper locators. Use z/locators or z/symbols to discover locator snippets. Use v/patch for generic raw text.")
