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
   [com.blockether.vis.core :as vis])
  (:import
   (com.google.re2j Pattern)
   (java.io File)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 200)
(def ^:private default-read-char-limit 6000)
(def ^:private default-list-depth 5)

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

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def cat-symbol
  (vis/symbol 'cat read-file
    {:doc (str "Read a file slice as pure structured data: "
            "{:path :offset :total-lines :truncated-by :lines}. "
            ":lines is a vec of raw line strings (no line-number "
            "prefix). :offset is the 1-based line number of (:lines 0). "
            ":truncated-by is :end-of-file | :line-limit | :char-limit. "
            "Default char-limit 6000; opts {:offset N :limit M :char-limit C}. "
            "Compose display text yourself: (str/join \"\\n\" (:lines r)).")
     :arglists '([path] [path opts])
     :examples ["(v/cat \"src/main.clj\")"
                "(:lines (v/cat \"src/main.clj\"))"
                "(v/cat \"big.log\" {:offset 5000 :limit 200})"
                "(str/join \"\\n\" (:lines (v/cat \"src/main.clj\")))"]}))

(def ls-symbol
  (vis/symbol 'ls list-files
    {:doc "List a directory tree. opts {:depth :hidden? :respect-gitignore?}."
     :arglists '([path] [path opts])
     :examples ["(v/ls \".\")"
                "(v/ls \"src\" {:depth 3})"]}))

(def rg-symbol
  (vis/symbol 'rg grep-files
    {:doc (str "Search files for any of N literal substrings (OR'd). "
            "`patterns` is a non-empty vector of strings; each element "
            "is matched LITERALLY (no regex). `path` is the search root. "
            "Returns {:hits [{:path :line :text} ...] :truncated-by :limit | :end-of-results}. "
            "opts {:limit :hidden? :respect-gitignore?}. "
            "For genuine regex needs use (re-seq #\"...\" (some file-text-source)).")
     :arglists '([patterns path] [patterns path opts])
     :examples ["(v/rg [\"defn render\"] \"src\")"
                "(:hits (v/rg [\"defn render\"] \"src\"))"
                "(v/rg [\"border-top\" \"draw-border\"] \"src\" {:limit 50})"]}))

(def read-all-lines-symbol
  (vis/symbol 'read-all-lines read-all-lines-safe
    {:doc "Read a whole text file as a vec of raw strings. Thin cwd-safe wrapper over babashka.fs/read-all-lines. opts currently supports :charset."
     :arglists '([path] [path opts])
     :examples ["(v/read-all-lines \"src/main.clj\")"
                "(str/join \"\\n\" (v/read-all-lines \"src/main.clj\"))"]}))

(def write-lines-symbol
  (vis/symbol 'write-lines write-lines-safe
    {:doc "Write a seqable of strings to path. Creates parent directories as needed. Returns the cwd-relative path string. Thin wrapper over babashka.fs/write-lines."
     :arglists '([path lines] [path lines opts])
     :examples ["(v/write-lines \"notes.txt\" [\"alpha\" \"beta\"])"
                "(v/write-lines \"src/main.clj\" (v/read-all-lines \"src/main.clj\"))"]}))

(def update-file-symbol
  (vis/symbol 'update-file update-file-safe
    {:doc "Update a text file by applying f to its old contents. Returns the new contents. Thin cwd-safe wrapper over babashka.fs/update-file."
     :arglists '([path f & xs] [path opts f & xs])
     :examples ["(v/update-file \"README.md\" #(str % \"\\nnew line\\n\"))"
                "(v/update-file \"x.txt\" {} str/upper-case)"]}))

(def create-dirs-symbol
  (vis/symbol 'create-dirs create-dirs-safe
    {:doc "Create a directory and any missing parents. Returns the cwd-relative path string."
     :arglists '([path])
     :examples ["(v/create-dirs \"target/tmp/cache\")"]}))

(def list-dir-symbol
  (vis/symbol 'list-dir list-dir-safe
    {:doc "List a directory as cwd-relative path strings. 2-arg form takes a glob string or accept fn, mirroring babashka.fs/list-dir."
     :arglists '([path] [path glob-or-accept])
     :examples ["(v/list-dir \"src\")"
                "(v/list-dir \"src\" \"*.clj\")"]}))

(def glob-symbol
  (vis/symbol 'glob glob-safe
    {:doc "Glob under root and return cwd-relative path strings. Thin cwd-safe wrapper over babashka.fs/glob."
     :arglists '([root pattern] [root pattern opts])
     :examples ["(v/glob \"src\" \"**.clj\")"
                "(v/glob \"resources\" \"**.edn\" {:hidden true})"]}))

(def copy-symbol
  (vis/symbol 'copy copy-safe
    {:doc "Copy src to dest. Creates parent directories for dest. Returns the cwd-relative dest path string."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/copy \"a.txt\" \"backup/a.txt\")"]}))

(def move-symbol
  (vis/symbol 'move move-safe
    {:doc "Move or rename src to dest. Creates parent directories for dest. Returns the cwd-relative dest path string."
     :arglists '([src dest] [src dest opts])
     :examples ["(v/move \"tmp.txt\" \"archive/tmp.txt\")"]}))

(def delete-symbol
  (vis/symbol 'delete delete-safe
    {:doc "Delete path. Returns true on success; throws when the path is missing or undeletable."
     :arglists '([path])
     :examples ["(v/delete \"tmp.txt\")"]}))

(def delete-if-exists-symbol
  (vis/symbol 'delete-if-exists delete-if-exists-safe
    {:doc "Delete path if it exists. Returns true when deleted, false when absent."
     :arglists '([path])
     :examples ["(v/delete-if-exists \"tmp.txt\")"]}))

(def exists?-symbol
  (vis/symbol 'exists? exists-safe?
    {:doc "True when path exists inside the workspace cwd."
     :arglists '([path])
     :examples ["(v/exists? \"src/main.clj\")"]}))

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
   exists?-symbol])

(def editing-prompt
  "`v/` = browse with helpers, act via normal Clojure code.

Browse / inspect:
  (v/cat path)                  -> paginated preview map {:path :offset :total-lines :truncated-by :lines}
  (v/ls path)                   -> directory tree map
  (v/rg [\"foo\" \"bar\"] path) -> CONTENT search inside files
  (v/glob root pattern)         -> PATH search for matching file names

Act via code (whole-file / path operations):
  (v/read-all-lines path)       -> [\"line\" ...]      ; code-first full-file read
  (v/write-lines path lines)    -> \"path\"            ; whole-file replace/create
  (v/update-file path f & xs)   -> new-string         ; read-modify-write
  (v/create-dirs path)          -> \"path\"
  (v/list-dir path)             -> [\"child\" ...]
  (v/copy src dest) / (v/move src dest)
  (v/delete path) / (v/delete-if-exists path)
  (v/exists? path)

Usage patterns:
  Browse a large file:
    (def preview (v/cat \"src/main.clj\" {:offset 1 :limit 80}))

  Rewrite a file via code:
    (v/write-lines \"notes.txt\" [\"alpha\" \"beta\"])

  Transform a file via code:
    (v/update-file \"README.md\" #(str % \"\\nNew section\\n\"))

  Find files vs find text:
    (v/glob \"src\" \"**.clj\")        ; which files?
    (v/rg [\"defn render\"] \"src\")  ; where in contents?

Rule of thumb:
  - `v/cat` = preview/browse
  - `v/read-all-lines` = full-file read for code
  - `v/write-lines` = whole-file replace/create
  - `v/update-file` = mutate old contents with a function
  - `v/glob` = path search
  - `v/rg` = content search

For structured Clojure edits use `(z/zedit path zfn)` (vis-language-clojure ext, alias `z/`).")
