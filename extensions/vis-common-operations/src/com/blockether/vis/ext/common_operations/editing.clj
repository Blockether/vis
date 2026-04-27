(ns com.blockether.vis.ext.common-operations.editing
  "Filesystem editing module of the `vis-common-operations` extension —
   read, list, grep, patch.

   This namespace owns the actual tool implementations and exposes
   `editing-symbols` for the aggregator (`com.blockether.vis.ext
   .common-operations.core`). It does NOT call `register-global!`; the
   sibling `core` namespace assembles every common-operations module
   into a single extension and registers it.

   Depends ONLY on `com.blockether/vis-extension` (the slim extension
   contract). The full vis runtime is intentionally not pulled in here."
  (:refer-clojure :exclude [read list])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext])
  (:import [com.google.re2j Pattern]
           [org.eclipse.jgit.ignore IgnoreNode]))

;; =============================================================================
;; Implementation fns (not exposed directly — wrapped via ext/symbol)
;; =============================================================================

(def ^:private default-grep-limit 200)
(def ^:private default-read-char-limit 1500)
(def ^:private default-respect-gitignore? true)

;; =============================================================================
;; Error rescue helpers
;; =============================================================================

(defn- absolute->relative
  "Strip CWD prefix from an absolute path to make it relative."
  [^String path]
  (let [cwd (System/getProperty "user.dir")]
    (if (str/starts-with? path cwd)
      (let [rel (subs path (inc (count cwd)))]
        (if (str/blank? rel) "." rel))
      ;; Not under CWD — try stripping leading /
      (str/replace path #"^/+" ""))))

(defn- rescue-path-args
  "on-error-fn: if the error is about a non-relative path, retry with
   the path made relative. Works for any tool whose first arg is a path."
  [err _env f args]
  (let [msg (ex-message err)]
    (if (and msg (or (str/includes? msg "not a relative path")
                   (str/includes? msg "Path escapes working directory")))
      (let [fixed-path (absolute->relative (str (first args)))]
        {:fn f :args (vec (cons fixed-path (rest args)))})
      {:error err})))

;; Regex meta characters that can be safely de-escaped (i.e. the LLM
;; threw an unhelpful backslash in front of a literal that didn't need
;; escaping in the first place). NOT included: alphanumeric escapes
;; like \d, \w, \s, \b — those are real character classes in RE2/J
;; and stripping them would break legitimate patterns.
(def ^:private safe-deescape-meta-chars
  #{\| \( \) \{ \} \[ \] \. \+ \* \? \^ \$ \- \/ \, \: \; \= \! \@ \#})

(defn- strip-bad-escape
  "Remove every occurrence of `\\X` from `pattern` where `X` is the
   offending char reported by RE2/J's \"invalid escape sequence: `\\X`\"
   message. Returns the fixed pattern, or nil if the offending escape
   isn't safe to strip (e.g. `\\1` backreference, `\\q` typo for `\\d`).

   Both the search pattern and the replacement go through
   `java.util.regex.{Pattern,Matcher}/quote` so meta characters like
   `$` or `(` don't get re-interpreted by `str/replace`."
  [^String pattern ^Character bad-char]
  (when (contains? safe-deescape-meta-chars bad-char)
    (str/replace pattern
      (re-pattern (str "\\\\" (java.util.regex.Pattern/quote (str bad-char))))
      (java.util.regex.Matcher/quoteReplacement (str bad-char)))))

(defn- extract-bad-escape-char
  "Pull the offending char from RE2/J's \"invalid escape sequence: `\\X`\"
   message. Returns the Character or nil."
  [^String msg]
  (when msg
    (when-let [m (re-find #"invalid escape sequence: `\\(.)`" msg)]
      (.charAt ^String (second m) 0))))

(defn- rescue-grep-args
  "on-error-fn: rescue common grep mistakes — bad regex escapes and
   absolute paths. Retries with fixed arguments, or surfaces the
   original error if nothing safe can be repaired.

   What this CAN fix:
     - RE2/J errors of the form \"invalid escape sequence: `\\X`\" where
       X is a non-alphanumeric punctuation char that the LLM
       over-escaped (e.g. `\\|`, `\\(`, `\\.`, `\\$`). Those backslashes
       are stripped and the call is retried with the cleaned pattern.
     - Absolute paths in the optional `path` arg — rewritten relative
       to CWD.

   What this CANNOT fix (and intentionally re-throws):
     - SCI/edamame parse errors (e.g. raw `\\|` in source code with no
       string escape). Those happen at code-eval time BEFORE the tool
       fn is ever invoked, so :on-error-fn never sees them. The
       iteration loop surfaces the parse error to the LLM, which
       self-corrects on the next turn.
     - Real character-class typos like `\\q` (probably meant `\\d`) or
       backreferences like `\\1`. Stripping the backslash would
       silently change meaning, so we leave them alone."
  [err _env f args]
  (let [msg       (ex-message err)
        data      (ex-data err)
        ;; compile-safe-pattern wraps RE2/J's message in :error.
        re2j-msg  (when data (some-> (:error data) str))
        bad-char  (or (extract-bad-escape-char re2j-msg)
                    (extract-bad-escape-char msg))
        path-err? (some #(and % (or (str/includes? % "not a relative path")
                                  (str/includes? % "Path escapes working directory")))
                    [msg re2j-msg])]
    (cond
      ;; RE2/J told us exactly which escape it choked on. Try a
      ;; conservative fix: strip the backslash only for known-safe
      ;; punctuation. Anything else (letters, digits) gets surfaced.
      bad-char
      (if-let [fixed (strip-bad-escape (str (first args)) bad-char)]
        (if (= fixed (str (first args)))
          {:error err}
          {:fn f :args (vec (cons fixed (rest args)))})
        {:error err})

      ;; Absolute path in the second arg.
      path-err?
      (let [fixed-args (if (>= (count args) 2)
                         (vec (cons (first args)
                                (cons (absolute->relative (str (second args)))
                                  (drop 2 args))))
                         args)]
        {:fn f :args fixed-args})

      :else {:error err})))

;; -----------------------------------------------------------------------------
;; Parse-error rescue (extension-wide :ext/on-parse-error-fn)
;;
;; The LLM occasionally writes a raw `\X` inside a string literal in
;; the SOURCE it emits — e.g. `(fs/grep-files "foo\|bar")` instead of
;; the correctly escaped `"foo\\|bar"`. Edamame rejects that with
;;
;;   "[line L, col C] Unsupported escape character: \X"
;;
;; Symbol-level :on-error-fn cannot help — the parse fails before any
;; tool fn is dispatched. The extension-wide :ext/on-parse-error-fn
;; hook (walked by `loop.runtime.….iteration.core/execute-code`) gets
;; the source string + error message and can return a rewritten
;; source. Here we double the offending backslash at the reported
;; line/col so the source becomes `"foo\\|bar"` and parses cleanly.
;; -----------------------------------------------------------------------------

(def ^:private parse-error-escape-re
  ;; edamame format: "[line 1, col 12] Unsupported escape character: \|"
  ;; Capture line, col, and the offending char.
  #"\[line (\d+), col (\d+)\] Unsupported escape character: \\(.)")

(defn- line-col->index
  "Translate edamame's 1-based [line, col] into a 0-based char index
   into `code`. Returns nil when the position is past the end."
  ^Long [^String code line col]
  (loop [i 0, ln 1, c 1]
    (cond
      (and (= ln line) (= c col)) i
      (>= i (.length code))       nil
      :else
      (let [ch (.charAt code i)]
        (if (= ch \newline)
          (recur (inc i) (inc ln) 1)
          (recur (inc i) ln       (inc c)))))))

(defn- safe-double-escape-char?
  "True when doubling the backslash in front of `ch` is a meaningful
   repair. Punctuation regex meta chars qualify; alphanumerics do
   NOT — those are most likely a real typo (`\\q` for `\\d`) and silently
   converting them into `\\\\q` (literal backslash + q in regex) would
   change meaning."
  [^Character ch]
  (contains? safe-deescape-meta-chars ch))

(defn- find-backslash-near
  "Edamame's [line, col] for an unsupported-escape error doesn't always
   point at the backslash itself — different versions land on the
   backslash, on the offending char immediately after it, or even one
   past that. Scan a tiny window around `idx` for the nearest `\\`
   that is followed by `bad-char`. Returns the index of the backslash,
   or nil."
  [^String code ^long idx ^Character bad-char]
  (let [n (.length code)]
    (some (fn [^long probe]
            (when (and (>= probe 0) (< probe n)
                    (= \\ (.charAt code probe))
                    (< (inc probe) n)
                    (= bad-char (.charAt code (inc probe))))
              probe))
      [idx (dec idx) (- idx 2) (inc idx)])))

(defn- rescue-parse-error
  "Symbol-level `:on-parse-error-fn` hook. Recovers from edamame's
   \"Unsupported escape character: \\X\" by doubling the lone backslash
   so the resulting Clojure string literal contains a real `\\X`. Returns
   the rewritten source, or nil to let the iteration loop surface the
   original error to the LLM.

   ctx keys: :code (string), :error (string), :sym (symbol),
   :environment (env map). Only :code and :error are read; :sym is
   ignored — every symbol mentioned in a broken form would benefit
   from the same string-literal repair."
  [{:keys [^String code ^String error]}]
  (when (and (string? code) (string? error))
    (when-let [m (re-find parse-error-escape-re error)]
      (let [line     (Long/parseLong (nth m 1))
            col      (Long/parseLong (nth m 2))
            bad-char (.charAt ^String (nth m 3) 0)]
        (when (safe-double-escape-char? bad-char)
          (when-let [idx (line-col->index code line col)]
            (when-let [bsl-idx (find-backslash-near code idx bad-char)]
              ;; Repair only the single offending site — if more bad
              ;; escapes lurk downstream the next parse will raise,
              ;; the rescue will fire again, and so on.
              (str (subs code 0 bsl-idx)
                "\\\\"
                (subs code (inc bsl-idx))))))))))

(defn- safe-path
  "Resolve path relative to CWD. Rejects traversal outside CWD."
  ^java.io.File [^String path]
  (let [cwd (System/getProperty "user.dir")
        f   (io/file cwd path)
        abs (.getCanonicalPath f)]
    (when-not (str/starts-with? abs cwd)
      (throw (ex-info (str "Path escapes working directory: " path)
               {:type :ext.common-operations.editing/path-traversal :path path :resolved abs})))
    f))

(defn- rel-path
  "Path of f relative to cwd. Canonicalizes the parent chain (so cwd-side
   symlinks resolve cleanly) but preserves the leaf name, so a symlink like
   CLAUDE.md -> AGENTS.md is reported as CLAUDE.md, not AGENTS.md."
  ^String [^java.io.File f]
  (let [cwd-path (.toPath (.getCanonicalFile (io/file (System/getProperty "user.dir"))))
        parent   (.getParentFile f)
        leaf     (.getName f)
        resolved (if (and parent (seq leaf))
                   (io/file (.getCanonicalFile parent) leaf)
                   (.getCanonicalFile f))]
    (str (.relativize cwd-path (.toPath resolved)))))

(defn- ensure-existing-file!
  ^java.io.File [path]
  (let [f (safe-path path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path)
               {:type :ext.common-operations.editing/not-found :path path})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path)
               {:type :ext.common-operations.editing/is-directory :path path})))
    f))

(defn- repo-root
  ^java.io.File []
  (loop [f (.getCanonicalFile (io/file (System/getProperty "user.dir")))]
    (cond
      (nil? f) nil
      (.exists (io/file f ".git")) f
      :else (recur (.getParentFile f)))))

(defn- load-ignore-node
  ^IgnoreNode [^java.io.File dir]
  (let [ignore-file (io/file dir ".gitignore")]
    (when (.isFile ignore-file)
      (with-open [in (io/input-stream ignore-file)]
        (doto (IgnoreNode.)
          (.parse in))))))

(defn- ignored-by-gitignore?
  "Check if a file or any of its ancestor directories is gitignored.
   Two-pass approach:
   1. Check the FULL relative path from root against root's .gitignore
      (catches path patterns like 'bench/data/').
   2. Walk each ancestor and check its NAME against its parent's
      .gitignore (catches simple patterns like '.clj-kondo/')."
  [^java.io.File f]
  (when-let [root (repo-root)]
    (let [root-canon (.getCanonicalFile root)
          root-path  (.getPath root-canon)
          target     (.getCanonicalFile f)]
      (when (str/starts-with? (.getPath target) root-path)
        (let [;; Full relative path from repo root
              rel-from-root (-> (str (.relativize (.toPath root-canon) (.toPath target)))
                              (str/replace java.io.File/separator "/"))
              ;; Pass 1: check full relative path against root .gitignore
              ;; This catches path patterns like 'bench/data/' and 'resources/docs/book/'
              root-node    (load-ignore-node root-canon)
              root-ignored (when (and root-node (not (str/blank? rel-from-root)))
                             (true? (.checkIgnored root-node rel-from-root (.isDirectory target))))]
          (if root-ignored
            true
            ;; Also check partial paths for intermediate dirs:
            ;; e.g. for bench/data/foo.json, check bench/data against root .gitignore
            (let [parts (str/split rel-from-root #"/")
                  intermediate-ignored
                  (when (and root-node (> (count parts) 1))
                    (some (fn [n]
                            (let [partial (str/join "/" (take n parts))]
                              (true? (.checkIgnored root-node partial true))))
                      (range 1 (count parts))))]
              (if intermediate-ignored
                true
                ;; Pass 2: walk ancestors checking each name against parent's .gitignore
                (loop [current target]
                  (let [parent (.getParentFile current)]
                    (cond
                      (nil? parent) false
                      (not (str/starts-with? (.getPath (.getCanonicalFile parent)) root-path)) false
                      :else
                      (let [parent-canon (.getCanonicalFile parent)
                            node         (load-ignore-node parent-canon)
                            nm           (.getName current)
                            is-dir?      (.isDirectory current)
                            ignored?     (when (and node (not (str/blank? nm)))
                                           (true? (.checkIgnored node nm is-dir?)))]
                        (if ignored?
                          true
                          (if (.equals parent-canon root-canon)
                            false
                            (recur parent)))))))))))))))

(defn- read-file
  "Read file contents with optional offset/limit (1-indexed lines).

   Default `(fs/read-file path)` returns a preview capped to 1500 chars.
   Use offset/limit to continue with more lines."
  ([path] (read-file path nil nil default-read-char-limit))
  ([path offset] (read-file path offset nil nil))
  ([path offset limit] (read-file path offset limit nil))
  ([path offset limit char-limit]
   (let [f        (ensure-existing-file! path)
         lines    (str/split-lines (slurp f))
         total    (count lines)
         off      (max 0 (dec (or offset 1)))
         lim      (or limit total)
         selected (take lim (drop off lines))
         numbered (map-indexed (fn [i line]
                                 (str (format "%4d" (+ off i 1)) "  " line))
                    selected)
         raw      (str/join "\n" numbered)
         capped?  (and char-limit (> (count raw) char-limit))
         content  (if capped? (subs raw 0 char-limit) raw)
         showing  (count selected)]
     (str content
       (when (or capped? (< (+ off showing) total))
         (str "\n\n["
           (cond
             capped?
             (str "preview capped at " char-limit " chars. Use offset=" (or offset 1)
               " limit=" (max 50 lim) " to continue")

             :else
             (str (- total off showing) " more lines. Use offset=" (+ off showing 1) " to continue"))
           "]"))))))

(defn- dir-size
  "Recursive byte-sum of every regular file under f. Symlinks and unreadable
   entries contribute 0; never throws."
  ^long [^java.io.File f]
  (try
    (reduce
      (fn [^long acc ^java.io.File child]
        (if (.isFile child)
          (+ acc (.length child))
          acc))
      0
      (file-seq f))
    (catch Exception _ 0)))

(defn- file->entry
  [^java.io.File f]
  {:name (.getName f)
   :path (rel-path f)
   :type (if (.isDirectory f) :dir :file)
   :size (if (.isDirectory f) (dir-size f) (.length f))
   :hidden? (.isHidden f)})

(def ^:private default-list-depth
  "Default tree depth for (fs/list-files): top level + one level of children."
  2)

(defn- depth->limit
  "Coerce a user-supplied depth arg into a long.

   - true        => unbounded (Long/MAX_VALUE)
   - false / nil => 1 (current level only)
   - integer n   => max(0, n)"
  ^long [d]
  (cond
    (true? d)    Long/MAX_VALUE
    (or (false? d) (nil? d)) 1
    (integer? d) (max 0 (long d))
    :else (throw (ex-info (str "Invalid depth: " (pr-str d))
                   {:type :ext.common-operations.editing/invalid-depth :depth d}))))

(defn- visible-children
  "Direct children of dir, filtered by hidden? + gitignore, sorted by path."
  [^java.io.File dir hidden? respect-gitignore?]
  (let [children (seq (.listFiles dir))
        visible  (filter #(or hidden? (not (.isHidden ^java.io.File %))) children)
        visible  (if respect-gitignore?
                   (remove ignored-by-gitignore? visible)
                   visible)]
    (sort-by rel-path visible)))

(defn- file->tree
  "Build a tree entry for f. Directory entries get :children when
   depth-left > 0, otherwise :children is omitted (the dir is a leaf)."
  [^java.io.File f ^long depth-left hidden? respect-gitignore?]
  (let [base (file->entry f)]
    (if (and (= :dir (:type base)) (pos? depth-left))
      (assoc base :children
        (mapv #(file->tree % (dec depth-left) hidden? respect-gitignore?)
          (visible-children f hidden? respect-gitignore?)))
      base)))

(defn- list-files
  "List files/dirs at path as a tree of
   {:name :path :type :size :hidden? :children}.

   Positional args only:
   - ()
   - (path)
   - (path depth)                          ; integer, true (unbounded), or false (1)
   - (path depth hidden?)
   - (path depth hidden? respect-gitignore?)

   Default depth is 2 (top level + one level of children).
   By default, .gitignore is respected."
  ([] (list-files "." default-list-depth false default-respect-gitignore?))
  ([path] (list-files path default-list-depth false default-respect-gitignore?))
  ([path depth] (list-files path depth false default-respect-gitignore?))
  ([path depth hidden?] (list-files path depth hidden? default-respect-gitignore?))
  ([path depth hidden? respect-gitignore?]
   (let [f         (safe-path path)
         depth-lim (depth->limit depth)]
     (when-not (.exists f)
       (throw (ex-info (str "Path not found: " path)
                {:type :ext.common-operations.editing/not-found :path path})))
     (when-not (.isDirectory f)
       (throw (ex-info (str "Not a directory: " path)
                {:type :ext.common-operations.editing/not-directory :path path})))
     (mapv #(file->tree % (dec depth-lim) hidden? respect-gitignore?)
       (visible-children f hidden? respect-gitignore?)))))

(defn- compile-safe-pattern
  [pattern]
  (try
    (Pattern/compile (str pattern))
    (catch Exception e
      (throw (ex-info (str "Invalid regex pattern: " pattern)
               {:type :ext.common-operations.editing/invalid-regex
                :pattern pattern
                :error (ex-message e)})))))

(def ^:private max-grep-line-chars
  "Max chars per matched line in grep results. Lines longer than this
   are truncated to prevent multi-KB JSON lines from flooding the journal."
  500)

(defn- grep-match->map
  [^java.io.File file idx line]
  (let [text (str/trim line)
        text (if (> (count text) max-grep-line-chars)
               (str (subs text 0 max-grep-line-chars) "...[truncated]")
               text)]
    {:path (rel-path file)
     :line (inc idx)
     :text text}))

(defn- grep-files
  "Search for pattern in files. Always returns structured maps.

   Positional args only:
   - (pattern)
   - (pattern path)
   - (pattern path limit)
   - (pattern path limit hidden?)
   - (pattern path limit hidden? respect-gitignore?)

   By default, .gitignore is respected."
  ([pattern] (grep-files pattern "." default-grep-limit false default-respect-gitignore?))
  ([pattern path] (grep-files pattern path default-grep-limit false default-respect-gitignore?))
  ([pattern path limit] (grep-files pattern path limit false default-respect-gitignore?))
  ([pattern path limit hidden?] (grep-files pattern path limit hidden? default-respect-gitignore?))
  ([pattern path limit hidden? respect-gitignore?]
   (let [f   (safe-path path)
         pat (compile-safe-pattern pattern)]
     (when-not (.exists f)
       (throw (ex-info (str "Path not found: " path)
                {:type :ext.common-operations.editing/not-found :path path})))
     (let [files   (if (.isDirectory f)
                     (filter #(and (.isFile ^java.io.File %)
                                (or hidden? (not (.isHidden ^java.io.File %))))
                       (file-seq f))
                     [f])
           files   (if respect-gitignore?
                     (remove ignored-by-gitignore? files)
                     files)
           matches (for [^java.io.File file files
                         :when (try (.isFile file) (catch Exception _ false))
                         :let [lines (try (str/split-lines (slurp file))
                                       (catch Exception _ nil))]
                         :when lines
                         [idx line] (map-indexed vector lines)
                         :when (.find (.matcher pat line))]
                     (grep-match->map file idx line))]
       (vec (take limit matches))))))

(def ^:private search-marker "<<<<<<< SEARCH")
(def ^:private split-marker "=======")
(def ^:private replace-marker ">>>>>>> REPLACE")

(defn- exact-match-count
  [content old-text]
  (count (re-seq (re-pattern (java.util.regex.Pattern/quote old-text)) content)))

(defn- apply-one-replacement
  [content {:keys [search replace index]} path]
  (cond
    ;; SEARCH empty => insertion. Allowed only when file is empty/new in practice.
    (= search "")
    (str replace content)

    :else
    (let [count (exact-match-count content search)]
      (cond
        (zero? count)
        (throw (ex-info (str "SEARCH block " index " not found in " path)
                 {:type :ext.common-operations.editing/patch-no-match
                  :path path
                  :block index
                  :search search}))

        (> count 1)
        (throw (ex-info (str "SEARCH block " index " matches " count
                          " times in " path ". Must be unique.")
                 {:type :ext.common-operations.editing/patch-ambiguous
                  :path path
                  :block index
                  :matches count
                  :search search}))

        :else
        (str/replace-first content
          (re-pattern (java.util.regex.Pattern/quote search))
          replace)))))

(defn- parse-search-replace-patch
  "Parse a Codex-style patch text with one or more blocks:

   <<<<<<< SEARCH
   old text
   =======
   new text
   >>>>>>> REPLACE"
  [patch-text]
  (loop [remaining (str patch-text)
         blocks []
         index 1]
    (let [remaining (str/triml remaining)]
      (if (str/blank? remaining)
        blocks
        (do
          (when-not (str/starts-with? remaining search-marker)
            (throw (ex-info "Invalid patch format: expected <<<<<<< SEARCH"
                     {:type :ext.common-operations.editing/patch-invalid-format
                      :block index})))
          (let [after-search (subs remaining (count search-marker))
                after-search (if (str/starts-with? after-search "\n")
                               (subs after-search 1)
                               after-search)
                split-idx (.indexOf after-search (str "\n" split-marker "\n"))]
            (when (neg? split-idx)
              (throw (ex-info "Invalid patch format: missing ======= separator"
                       {:type :ext.common-operations.editing/patch-invalid-format
                        :block index})))
            (let [search (subs after-search 0 split-idx)
                  after-split (subs after-search
                                (+ split-idx (count (str "\n" split-marker "\n"))))
                  replace-idx (.indexOf after-split (str "\n" replace-marker))]
              (when (neg? replace-idx)
                (throw (ex-info "Invalid patch format: missing >>>>>>> REPLACE terminator"
                         {:type :ext.common-operations.editing/patch-invalid-format
                          :block index})))
              (let [replace (subs after-split 0 replace-idx)
                    after-replace (subs after-split
                                    (+ replace-idx (count (str "\n" replace-marker))))
                    after-replace (if (str/starts-with? after-replace "\n")
                                    (subs after-replace 1)
                                    after-replace)]
                (recur after-replace
                  (conj blocks {:index index :search search :replace replace})
                  (inc index))))))))))

(defn- apply-search-replace-patch
  [path patch-text]
  (let [f        (safe-path path)
        existed? (.exists f)
        _        (when (and existed? (.isDirectory f))
                   (throw (ex-info (str "Path is a directory: " path)
                            {:type :ext.common-operations.editing/is-directory :path path})))
        original (if existed? (slurp f) "")
        blocks   (parse-search-replace-patch patch-text)
        _        (when (and (not existed?)
                         (some #(not= "" (:search %)) blocks))
                   (throw (ex-info (str "Cannot patch non-existent file " path
                                     " unless every SEARCH block is empty")
                            {:type :ext.common-operations.editing/patch-create-requires-empty-search
                             :path path})))
        patched  (reduce (fn [content block]
                           (apply-one-replacement content block path))
                   original blocks)]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f patched)
    {:path path
     :status :ok
     :op :patch-file
     :created? (not existed?)
     :applied (count blocks)}))

(defn- patch-file
  "Apply a Codex-style SEARCH/REPLACE patch to a file.

     (fs/patch-file path patch-text)

   `patch-text` contains one or more blocks:

     <<<<<<< SEARCH
     old text
     =======
     new text
     >>>>>>> REPLACE

   Create a new file by using an EMPTY SEARCH block:

     <<<<<<< SEARCH
     =======
     full file contents here
     >>>>>>> REPLACE"
  [path patch-text]
  (apply-search-replace-patch path patch-text))

;; =============================================================================
;; Extension definition
;; =============================================================================

(def read-file-symbol
  (ext/symbol 'read-file read-file
    {:doc "Read file contents with optional line offset/limit. Default path-only call returns a 1500-char preview."
     :arglists '([path] [path offset] [path offset limit])
     :examples ["(fs/read-file \"src/core.clj\")"
                "(fs/read-file \"big.log\" 100 50)"]
     :on-error-fn rescue-path-args}))

(def list-files-symbol
  (ext/symbol 'list-files list-files
    {:doc "List files/dirs as a tree of {:name :path :type :size :hidden? :children}. Default depth is 2 (top level + one level of children). Pass an integer for custom depth, true for unbounded, or false/0 for current level only. Directory :size is the recursive byte sum. Respects .gitignore by default. Positional args only."
     :arglists '([] [path] [path depth] [path depth hidden?] [path depth hidden? respect-gitignore?])
     :examples ["(fs/list-files)"
                "(fs/list-files \"src\")"
                "(fs/list-files \"src\" 3)"
                "(fs/list-files \"src\" true)"
                "(fs/list-files \"src\" 2 true)"
                "(fs/list-files \"src\" 2 true false)"]
     :on-error-fn rescue-path-args}))

(def grep-files-symbol
  (ext/symbol 'grep-files grep-files
    {:doc "Search files with RE2/J (linear-time regex, ReDoS-safe). Always returns structured maps. Respects .gitignore by default. Positional args only."
     :arglists '([pattern] [pattern path] [pattern path limit] [pattern path limit hidden?] [pattern path limit hidden? respect-gitignore?])
     :examples ["(fs/grep-files \"TODO\")"
                "(fs/grep-files \"defn\" \"src\")"
                "(fs/grep-files \"defn|defmacro\" \"src\" 50)"
                "(fs/grep-files \"defn\" \"src\" 50 true)"
                "(fs/grep-files \"defn\" \"src\" 50 true false)"]
     :on-error-fn       rescue-grep-args
     ;; Parse-time rescue: when the LLM emits e.g.
     ;;   (fs/grep-files \"foo\\|bar\")
     ;; — raw `\|` inside the string literal — edamame errors out
     ;; before any tool dispatch. The iteration loop notices the
     ;; broken form mentions `fs/grep-files`, calls THIS hook, and
     ;; retries with the doubled backslash. See `rescue-parse-error`.
     :on-parse-error-fn rescue-parse-error}))

(def patch-file-symbol
  (ext/symbol 'patch-file patch-file
    {:doc "Patch a file with a Codex-style SEARCH/REPLACE patch text. Use an empty SEARCH block to create a new file."
     :arglists '([path patch-text])
     :examples ["(fs/patch-file \"src/core.clj\" \"<<<<<<< SEARCH\nold\n=======\nnew\n>>>>>>> REPLACE\")"
                "(fs/patch-file \"new-file.txt\" \"<<<<<<< SEARCH\n=======\nhello\n>>>>>>> REPLACE\")"]
     :on-error-fn rescue-path-args}))

(def editing-symbols
  "Vector of `ext/symbol` definitions exported by this module. The
   sibling `core` namespace concatenates every module's symbol vector
   into the single `vis-common-operations` extension."
  [read-file-symbol
   list-files-symbol
   grep-files-symbol
   patch-file-symbol])

(def editing-prompt
  "Module-specific prompt fragment merged into the extension prompt by
   the `core` aggregator. Lives next to the symbols so a future
   reorganization moves both pieces together."
  "RULES:
- NEVER guess file paths. Always discover paths first with (fs/list-files) or (fs/grep-files pattern).
- There is NO write-file tool. Use fs/patch-file ALWAYS.
- To create a new file, use fs/patch-file with an EMPTY SEARCH block.
- Prefer the smallest unique SEARCH block that matches exactly once.")
