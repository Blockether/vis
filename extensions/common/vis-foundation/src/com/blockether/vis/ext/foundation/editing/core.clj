(ns com.blockether.vis.ext.foundation.editing.core
  "Filesystem tools exposed under the `v/` alias in the SCI sandbox.

   Two layers:

   1. Structured helpers for preview / tree / search:

        (v/cat path)            ; -> {:path :offset :total-lines :truncated-by :lines}
        (v/cat path opts)       ; opts is {:offset N :limit M :char-limit C}
        (v/ls path)             ; -> nested {:name :path :type :size :children} tree
        (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
        (v/rg patterns path)    ; -> {:hits :truncated-by}; patterns = non-empty vec of literal substrings

   2. Thin cwd-safe wrappers over the babashka.fs file API, so the
      model acts via normal Clojure code instead of bespoke edit DSLs:

        (v/read-all-lines path)
        (v/write-lines path lines)
        (v/update-file path f & xs)
        (v/create-dirs path)
        (v/list-dir path)
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

   Clojure-specific structured editing (`z/zedit` plus the `z/`
   rewrite-clj zipper API) lives in the `vis-language-clojure`
   extension under `extensions/languages/clojure/`.

   Hard guard: every path must stay inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension])
  (:import
   (com.google.re2j Pattern)
   (java.io File InputStream Reader Writer)
   (java.util.concurrent TimeUnit)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 200)
(def ^:private default-read-char-limit 6000)
(def ^:private default-list-depth 5)
(def ^:private default-bash-timeout-ms 30000)
(def ^:private default-bash-max-output-chars 20000)

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
  [{:keys [op path kind result markdown provenance]}]
  (let [t (now-ms)]
    (extension/with-presentation
      (extension/success
        {:result     result
         :provenance (merge {:op             op
                             :target         (path->target path kind)
                             :started-at-ms  t
                             :finished-at-ms t
                             :duration-ms    0}
                       provenance)
         :markdown   markdown})
      {:journal :markdown
       :var-index :compact
       :transcript :full})))

(defn- default-error-markdown
  [op path err _target]
  (str "`" (name op) "` failed for `" (or path "<nil>") "` — `"
    (.getName (class err)) "`: " (or (ex-message err) "")))

(defn- tool-failure-on-error
  [op kind markdown-fn]
  (fn [err _env _f args]
    (let [path (first args)
          target (path->target path kind)
          t (now-ms)
          markdown ((or markdown-fn default-error-markdown) op path err target)]
      {:result (extension/with-presentation
                 (extension/failure
                   {:result     nil
                    :provenance {:op             op
                                 :target         target
                                 :started-at-ms  t
                                 :finished-at-ms t
                                 :duration-ms    0}
                    :markdown   markdown
                    :throwable  err})
                 {:journal :markdown
                  :var-index :compact
                  :transcript :full})})))

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

(defn- coerce-cat-opts [opts]
  (let [validate-positive (fn [k v]
                            (when (and (some? v)
                                    (or (not (integer? v)) (not (pos? v))))
                              (throw (ex-info (str "v/cat " k " must be a positive integer")
                                       {:type :ext.foundation.editing/invalid-cat-opts
                                        :opt  k :got v}))))]
    (cond
      (nil? opts) {:offset nil :limit nil :char-limit default-read-char-limit}

      (map? opts)
      (let [m {:offset     (:offset opts)
               :limit      (:limit opts)
               :char-limit (or (:char-limit opts) default-read-char-limit)}]
        (validate-positive :offset (:offset m))
        (validate-positive :limit  (:limit m))
        (validate-positive :char-limit (:char-limit m))
        m)

      (integer? opts)
      (do (validate-positive :offset opts)
        {:offset opts :limit nil :char-limit default-read-char-limit})

      :else (throw (ex-info (str "Invalid (v/cat) opts: " (pr-str opts))
                     {:type :ext.foundation.editing/invalid-cat-opts :opts opts})))))

(defn- take-lines-under-char-cap
  "Return `[selected truncated-by]` where `selected` is the prefix of
   `candidates` whose joined-with-newline length stays within
   `char-cap`, and `truncated-by` is one of:
     :char-limit   — char cap kicked in before consuming everything
     :line-limit   — we hit the requested `limit-arg` before EOF
     :end-of-file  — the slice exhausted the file (or limit met EOF)
   `total-lines` and `offset-zero` are the file size and the 0-based
   start of `candidates` so we can decide between :line-limit and
   :end-of-file when the limit happens to land exactly at EOF."
  [candidates char-cap limit-arg total-lines offset-zero]
  (let [;; +1 per line for the joining newline; matches str/join "\n".
        budget (long char-cap)]
    (loop [remaining candidates
           taken     []
           used      0]
      (if-not (seq remaining)
        ;; Exhausted candidates without hitting the char cap.
        (let [taken-count   (count taken)
              ;; Did we run out because limit-arg said so, or EOF?
              consumed-end? (>= (+ offset-zero taken-count) total-lines)]
          [taken (cond
                   consumed-end?               :end-of-file
                   (and limit-arg
                     (= taken-count limit-arg)) :line-limit
                   :else                       :end-of-file)])
        (let [line     (first remaining)
              new-used (+ used (count line) (if (zero? (count taken)) 0 1))]
          (if (> new-used budget)
            [taken :char-limit]
            (recur (rest remaining) (conj taken line) new-used)))))))

(defn- read-file
  "Read a file slice as pure structured data:

     {:path \"src/foo.clj\"
      :offset 1               ;; 1-based line number of (:lines 0)
      :total-lines 487
      :truncated-by :end-of-file | :line-limit | :char-limit
      :lines [\"line 1 text\" \"line 2 text\" ...]}

   No prose footer, no line-number prefix on each string. The model
   composes display text itself when it wants to:
     (str/join \"\\n\" (:lines result))                          ;; raw text
     (map-indexed (fn [i s] (str (+ i (:offset r)) \" \" s)) ...) ;; numbered

   Pagination is the model's iteration code, not a hint embedded in
   the result — `:offset`, `:total-lines` and `:truncated-by` carry
   everything needed to decide the next call."
  ([path] (read-file path nil))
  ([path opts]
   (let [{:keys [offset limit char-limit]} (coerce-cat-opts opts)
         f          (ensure-existing-file! (safe-path path))
         all-lines  (str/split-lines (slurp f))
         total      (count all-lines)
         offset-1   (max 1 (or offset 1))
         offset-0   (dec offset-1)
         past-eof?  (>= offset-0 total)
         candidates (if past-eof? () (drop offset-0 all-lines))
         capped     (if limit (take limit candidates) candidates)
         [selected truncated-by]
         (cond
           past-eof? [[] :end-of-file]
           :else     (take-lines-under-char-cap capped char-limit limit total offset-0))]
     {:path         (rel-path f)
      :offset       offset-1
      :total-lines  total
      :truncated-by truncated-by
      :lines        (vec selected)})))

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

(defn- read-all-lines-safe
  ([path]
   (vec (fs/read-all-lines (ensure-existing-file! (safe-path path)))))
  ([path opts]
   (vec (fs/read-all-lines (ensure-existing-file! (safe-path path))
          (or opts {})))))

(defn- write-lines-safe
  ([path lines]
   (write-lines-safe path lines nil))
  ([path lines opts]
   (let [f (safe-path path)]
     (ensure-parent-dirs! f)
     (fs/write-lines f lines (or opts {}))
     (rel-path f))))

(defn- update-file-safe
  [path & more]
  (let [file (ensure-existing-file! (safe-path path))]
    (if (map? (first more))
      (let [[opts f & xs] more]
        (apply fs/update-file file (or opts {}) f xs))
      (let [[f & xs] more]
        (apply fs/update-file file f xs)))))

(defn- create-dirs-safe [path]
  (let [f (safe-path path)]
    (fs/create-dirs f)
    (rel-path f)))

(defn- list-dir-safe
  ([path]
   (mapv display-path (fs/list-dir (safe-path path))))
  ([path glob-or-accept]
   (mapv display-path (fs/list-dir (safe-path path) glob-or-accept))))

(defn- glob-safe
  ([root pattern]
   (glob-safe root pattern nil))
  ([root pattern opts]
   (mapv display-path (fs/glob (safe-path root) pattern (or opts {})))))

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
;; Tool-result facades
;; =============================================================================

(defn- cat-tool
  ([path]
   (cat-tool path nil))
  ([path opts]
   (let [out (read-file path opts)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result out
        :markdown (str "Read `" (:path out) "` — returned "
                    (count (:lines out)) " line(s), offset " (:offset out)
                    ", total " (:total-lines out)
                    ", truncated-by `" (name (:truncated-by out)) "`.")
        :provenance {:lines-returned (count (:lines out))
                     :offset (:offset out)
                     :total-lines (:total-lines out)
                     :truncated-by (:truncated-by out)
                     :opts opts}}))))

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
        :markdown (str "Listed `" (:path out) "` as a " (name (:type out)) " tree.")
        :provenance {:depth (:depth opts)
                     :hidden? (:hidden? opts)
                     :respect-gitignore? (get opts :respect-gitignore? true)}}))))

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
        :markdown (str "Searched `" path "` for " (count patterns) " literal pattern(s) — "
                    (count (:hits out)) " hit(s), truncated-by `" (name (:truncated-by out)) "`.")
        :provenance {:patterns patterns
                     :hit-count (count (:hits out))
                     :truncated-by (:truncated-by out)
                     :opts opts}}))))

(defn- read-all-lines-tool
  ([path]
   (read-all-lines-tool path nil))
  ([path opts]
   (let [lines (if opts (read-all-lines-safe path opts) (read-all-lines-safe path))]
     (tool-success
       {:op :v/read-all-lines
        :path path
        :kind :file
        :result lines
        :markdown (str "Read `" path "` fully — " (count lines) " line(s).")
        :provenance {:lines (count lines)
                     :opts opts}}))))

(defn- write-lines-tool
  ([path lines]
   (write-lines-tool path lines nil))
  ([path lines opts]
   (let [before (when (fs/exists? (safe-path path)) (slurp (safe-path path)))
         out    (write-lines-safe path lines opts)
         after  (slurp (safe-path path))]
     (tool-success
       {:op :v/write-lines
        :path path
        :kind :file
        :result out
        :markdown (str "Wrote `" out "` — " (count lines) " line(s), changed? `" (not= before after) "`.")
        :provenance {:changed? (not= before after)
                     :lines-before (count (str/split-lines (or before "")))
                     :lines-after (count (str/split-lines after))
                     :opts opts}}))))

(defn- update-file-tool
  [path & more]
  (let [before (slurp (safe-path path))
        after  (apply update-file-safe path more)]
    (tool-success
      {:op :v/update-file
       :path path
       :kind :file
       :result after
       :markdown (str "Updated `" path "` — changed? `" (not= before after) "`, "
                   (count (str/split-lines before)) " -> " (count (str/split-lines after)) " line(s).")
       :provenance {:changed? (not= before after)
                    :lines-before (count (str/split-lines before))
                    :lines-after (count (str/split-lines after))}})))

(defn- create-dirs-tool
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :v/create-dirs
       :path path
       :kind :dir
       :result out
       :markdown (str "Ensured directory `" out "` exists — created? `" (not before) "`.")
       :provenance {:created? (not before)
                    :already-existed? before}})))

(defn- list-dir-tool
  ([path]
   (list-dir-tool path nil))
  ([path glob-or-accept]
   (let [out (if (some? glob-or-accept)
               (list-dir-safe path glob-or-accept)
               (list-dir-safe path))]
     (tool-success
       {:op :v/list-dir
        :path path
        :kind :dir
        :result out
        :markdown (str "Listed directory `" path "` — " (count out) " entr" (if (= 1 (count out)) "y" "ies") ".")
        :provenance {:entry-count (count out)
                     :filter glob-or-accept}}))))

(defn- glob-tool
  ([root pattern]
   (glob-tool root pattern nil))
  ([root pattern opts]
   (let [out (glob-safe root pattern opts)]
     (tool-success
       {:op :v/glob
        :path root
        :kind :dir
        :result out
        :markdown (str "Globbed `" root "` with `" pattern "` — " (count out) " match(es).")
        :provenance {:pattern pattern
                     :match-count (count out)
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
        :markdown (str "Copied `" src "` to `" out "`.")
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
        :markdown (str "Moved `" src "` to `" out "`.")
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
     :markdown (str "Deleted `" path "`.")
     :provenance {:deleted? true}}))

(defn- delete-if-exists-tool
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :v/delete-if-exists
       :path path
       :kind :path
       :result deleted?
       :markdown (if deleted?
                   (str "Deleted `" path "`.")
                   (str "`" path "` was already absent."))
       :provenance {:deleted? deleted?}})))

(defn- exists-tool
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :v/exists?
       :path path
       :kind :path
       :result exists?
       :markdown (if exists?
                   (str "`" path "` exists.")
                   (str "`" path "` does not exist."))
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
        :markdown (str "Ran bash in `" (:cwd out) "` — exit `" (:exit out) "`, "
                    (:duration-ms out) " ms"
                    (when (:timed-out? out) ", timed out") ".")
        :provenance {:command command
                     :cwd (:cwd out)
                     :exit (:exit out)
                     :timed-out? (:timed-out? out)
                     :stdout-truncated? (:stdout-truncated? out)
                     :stderr-truncated? (:stderr-truncated? out)
                     :opts (dissoc opts :stdin)}}))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def cat-symbol
  (vis/symbol 'cat cat-tool
    {:doc "Preview file slice. Tool result; `:result` = {:path :offset :total-lines :truncated-by :lines}. Use for browse, not whole-file edits."
     :arglists '([path] [path opts])
     :examples ["(:result (v/cat \"src/main.clj\"))"
                "(get-in (v/cat \"src/main.clj\") [:result :lines])"
                "(v/cat \"big.log\" {:offset 5000 :limit 200})"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/cat :file nil)}))

(def ls-symbol
  (vis/symbol 'ls ls-tool
    {:doc "Preview directory tree. Tool result; `:result` = nested tree."
     :arglists '([path] [path opts])
     :examples ["(:result (v/ls \".\"))"
                "(v/ls \"src\" {:depth 3})"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/ls :dir nil)}))

(def rg-symbol
  (vis/symbol 'rg rg-tool
    {:doc "Search file contents for literal substrings. Tool result. For path search use `v/glob`."
     :arglists '([patterns path] [patterns path opts])
     :examples ["(:result (v/rg [\"defn render\"] \"src\"))"
                "(get-in (v/rg [\"defn render\"] \"src\") [:result :hits])"
                "(v/rg [\"border-top\" \"draw-border\"] \"src\" {:limit 50})"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/rg :dir nil)}))

(def read-all-lines-symbol
  (vis/symbol 'read-all-lines read-all-lines-tool
    {:doc "Read whole text file. Tool result; `:result` = line vector."
     :arglists '([path] [path opts])
     :examples ["(:result (v/read-all-lines \"src/main.clj\"))"
                "(str/join \"\\n\" (:result (v/read-all-lines \"src/main.clj\")))"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/read-all-lines :file nil)}))

(def write-lines-symbol
  (vis/symbol 'write-lines write-lines-tool
    {:doc "Replace/create whole file. Tool result."
     :arglists '([path lines] [path lines opts])
     :examples ["(v/write-lines \"notes.txt\" [\"alpha\" \"beta\"])"
                "(v/write-lines \"src/main.clj\" (:result (v/read-all-lines \"src/main.clj\")))"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/write-lines :file nil)}))

(def update-file-symbol
  (vis/symbol 'update-file update-file-tool
    {:doc "Read-modify-write text file. Tool result; `:result` = new contents."
     :arglists '([path f & xs] [path opts f & xs])
     :examples ["(v/update-file \"README.md\" #(str % \"\\nnew line\\n\"))"
                "(v/update-file \"x.txt\" str/upper-case)"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/update-file :file nil)}))

(def create-dirs-symbol
  (vis/symbol 'create-dirs create-dirs-tool
    {:doc "Ensure dir exists. Tool result."
     :arglists '([path])
     :examples ["(v/create-dirs \"target/tmp/cache\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/create-dirs :dir nil)}))

(def list-dir-symbol
  (vis/symbol 'list-dir list-dir-tool
    {:doc "Flat directory listing. Tool result; `:result` = cwd-relative paths."
     :arglists '([path] [path glob-or-accept])
     :examples ["(:result (v/list-dir \"src\"))"
                "(v/list-dir \"src\" \"*.clj\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/list-dir :dir nil)}))

(def glob-symbol
  (vis/symbol 'glob glob-tool
    {:doc "Search paths, not contents. Tool result; `:result` = matching paths."
     :arglists '([root pattern] [root pattern opts])
     :examples ["(:result (v/glob \"src\" \"**.clj\"))"
                "(v/glob \"resources\" \"**.edn\" {:hidden true})"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/glob :dir nil)}))

(def copy-symbol
  (vis/symbol 'copy copy-tool
    {:doc "Copy path. Tool result."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/copy \"a.txt\" \"backup/a.txt\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/copy :path nil)}))

(def move-symbol
  (vis/symbol 'move move-tool
    {:doc "Move/rename path. Tool result."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/move \"tmp.txt\" \"archive/tmp.txt\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/move :path nil)}))

(def delete-symbol
  (vis/symbol 'delete delete-tool
    {:doc "Delete path. Tool result."
     :arglists '([path])
     :examples ["(v/delete \"tmp.txt\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol 'delete-if-exists delete-if-exists-tool
    {:doc "Delete path if present. Tool result."
     :arglists '([path])
     :examples ["(v/delete-if-exists \"tmp.txt\")"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol 'exists? exists-tool
    {:doc "Existence check. Tool result; `:result` = boolean."
     :arglists '([path])
     :examples ["(:result (v/exists? \"src/main.clj\"))"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/exists? :path nil)}))

(def bash-symbol
  (vis/symbol 'bash bash-tool
    {:doc "Run bounded `/usr/bin/env bash -lc` in worktree. Tool result; opts {:cwd :timeout-ms :max-output-chars :stdin}."
     :arglists '([command] [command opts])
     :examples ["(:result (v/bash \"pwd && ls\"))"
                "(v/bash \"grep -R needle src\" {:timeout-ms 10000 :max-output-chars 8000})"]
     :result-spec tool-result-spec
     :on-error-fn (tool-failure-on-error :v/bash :dir nil)}))

(def editing-symbols
  [cat-symbol
   ls-symbol
   rg-symbol
   read-all-lines-symbol
   write-lines-symbol
   update-file-symbol
   create-dirs-symbol
   list-dir-symbol
   glob-symbol
   copy-symbol
   move-symbol
   delete-symbol
   delete-if-exists-symbol
   exists?-symbol
   bash-symbol])

(def editing-prompt
  "`v/` file + shell tools:
  browse: (v/cat path opts?) (v/ls path opts?) (v/rg [literals] path opts?) (v/glob root pattern opts?)
  edit:   (v/read-all-lines path) (v/write-lines path lines opts?) (v/update-file path f & xs)
  paths:  (v/create-dirs path) (v/list-dir path) (v/copy src dest) (v/move src dest) (v/delete path) (v/delete-if-exists path) (v/exists? path)
  shell:  (v/bash cmd {:cwd \".\" :timeout-ms 30000 :max-output-chars 20000 :stdin s})
Use `v/cat` for preview, `v/read-all-lines` for full-file transforms, `v/glob` for paths, `v/rg` for contents. Clojure structure edits: `z/zedit`.")
