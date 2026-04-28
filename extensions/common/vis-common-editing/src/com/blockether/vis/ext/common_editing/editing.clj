(ns com.blockether.vis.ext.common-editing.editing
  "Filesystem editing module of the `vis-common-editing` extension —
   cat, ls, rg, patch.

   This namespace owns the actual tool implementations and exposes
   `editing-symbols` for the aggregator (`com.blockether.vis.ext
   .common-editing.core`). It does NOT call `register-global!`; the
   sibling `core` namespace assembles every common-editing module
   into a single extension and registers it.

   Depends on `com.blockether/vis-runtime` and uses the extension-author
   facade `com.blockether.vis-sdk.core`."
  (:refer-clojure :exclude [read list])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis-sdk.core :as ext])
  (:import [com.google.re2j Pattern]
           [java.math BigInteger]
           [org.eclipse.jgit.ignore IgnoreNode]))

;; =============================================================================
;; Implementation fns (not exposed directly — wrapped via ext/symbol)
;; =============================================================================

(def ^:private default-grep-limit 200)
(def ^:private default-read-char-limit 1500)
(def ^:private default-autobind-max-bytes (* 256 1024))
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
;; the SOURCE it emits — e.g. `(vis/rg "foo\|bar")` instead of
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
               {:type :ext.common-editing.editing/path-traversal :path path :resolved abs})))
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
               {:type :ext.common-editing.editing/not-found :path path})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path)
               {:type :ext.common-editing.editing/is-directory :path path})))
    f))

(defn- sha1
  ^String [^String text]
  (let [digest (java.security.MessageDigest/getInstance "SHA-1")
        bytes  (.digest digest (.getBytes text "UTF-8"))]
    (format "%040x" (BigInteger. 1 bytes))))

(defn- line-count
  ^long [^String text]
  (if (str/blank? text)
    0
    (count (str/split-lines text))))

(defn- file-content-for-autobind
  "Read full file content for autobind when the file is small enough.
   Returns nil for oversized files."
  [path]
  (let [file (ensure-existing-file! path)
        file-length (.length file)]
    (when (<= file-length default-autobind-max-bytes)
      (slurp file))))

(defn- read-file-autobind
  [{:keys [args]}]
  (let [path (str (first args))]
    (when-let [full-content (file-content-for-autobind path)]
      {:bindings
       [{:kind    :file
         :id      path
         :content full-content
         :doc     (str path " — " (count full-content) " chars / "
                    (line-count full-content) " lines")
         :tag     (sha1 full-content)}]})))

(defn- patch-autobind
  [{:keys [result]}]
  (let [files (or (:files result) [])]
    (when (seq files)
      {:bindings
       (vec
         (keep (fn [{:keys [path]}]
                 (when-let [full-content (file-content-for-autobind path)]
                   {:kind    :file
                    :id      path
                    :content full-content
                    :doc     (str path " — " (count full-content) " chars / "
                               (line-count full-content) " lines")
                    :tag     (sha1 full-content)}))
           files))})))

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

(defn- coerce-cat-opts
  "Normalize the optional second arg of (vis/cat path opts) into
   {:offset :limit :char-limit}. Accepts:

   - nil / absent  -> all defaults
   - a map         -> picked apart by key (unknown keys ignored)
   - an integer    -> {:offset n} (back-compat: positional offset)

   Anything else throws so a typo at call-site (e.g. passing a
   keyword like :all instead of an opts map) surfaces here."
  [opts]
  (cond
    (nil? opts)
    {:offset nil :limit nil :char-limit default-read-char-limit}

    (map? opts)
    {:offset     (get opts :offset)
     :limit      (get opts :limit)
     :char-limit (get opts :char-limit default-read-char-limit)}

    (integer? opts)
    {:offset opts :limit nil :char-limit default-read-char-limit}

    :else
    (throw (ex-info (str "Invalid (vis/cat) opts: " (pr-str opts)
                      ". Expected a map like {:offset 60 :limit 100} or an integer offset.")
             {:type :ext.common-editing.editing/invalid-cat-opts :opts opts}))))

(defn- read-file
  "Read file contents.

   Variadic shape, opts is a map. See cat-symbol :examples for
   concrete call patterns; in short:

     (vis/cat path)             ; preview capped to 1500 chars
     (vis/cat path opts)        ; opts is a map of {:offset :limit :char-limit}
     (vis/cat path offset)      ; back-compat: integer offset

   Default `(vis/cat path)` returns a preview capped to 1500 chars.
   Use opts {:offset N :limit M} to continue with more lines."
  ([path] (read-file path nil))
  ([path opts]
   (let [{:keys [offset limit char-limit]} (coerce-cat-opts opts)
         f        (ensure-existing-file! path)
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
  "Default tree depth for (vis/ls): top level + one level of children."
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
                   {:type :ext.common-editing.editing/invalid-depth :depth d}))))

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

(defn- path-not-found-message
  [path]
  (let [working-directory (System/getProperty "user.dir")]
    (str "Path not found: " path
      " (working directory: " working-directory "). "
      "Do not assume common folders like src/. Start with "
      "(vis/ls \".\").")))

(defn- coerce-ls-opts
  "Normalize the optional second arg of (vis/ls path opts) into
   {:depth :hidden? :respect-gitignore?}. Accepts:

   - nil / absent  -> all defaults
   - a map         -> picked apart by key (unknown keys ignored)
   - an integer    -> {:depth n} (back-compat: positional depth still works)
   - true / false  -> {:depth n} (true = unbounded, false = depth 1)

   Anything else throws so a typo at call-site (e.g. passing a
   keyword like :deep instead of an opts map) surfaces here, not
   three layers down."
  [opts]
  (cond
    (nil? opts)
    {:depth default-list-depth :hidden? false :respect-gitignore? default-respect-gitignore?}

    (map? opts)
    {:depth              (get opts :depth default-list-depth)
     :hidden?            (boolean (get opts :hidden? false))
     :respect-gitignore? (boolean (get opts :respect-gitignore? default-respect-gitignore?))}

    (or (integer? opts) (true? opts) (false? opts))
    {:depth opts :hidden? false :respect-gitignore? default-respect-gitignore?}

    :else
    (throw (ex-info (str "Invalid (vis/ls) opts: " (pr-str opts)
                      ". Expected a map like {:depth 3 :hidden? true} or an integer depth.")
             {:type :ext.common-editing.editing/invalid-ls-opts :opts opts}))))

(defn- list-files
  "List files/dirs at path as a tree of
   {:name :path :type :size :hidden? :children}.

   Variadic shape, ALL arguments optional. See ls-symbol :examples
   for concrete call patterns; in short:

     (vis/ls)              ; cwd, default depth (2)
     (vis/ls path)         ; same defaults at PATH
     (vis/ls path opts)    ; opts is a map of
                           ;   {:depth :hidden? :respect-gitignore?}
     (vis/ls path depth)   ; back-compat: integer / true / false

   Default depth is 2 (top level + one level of children); pass
   :depth true for an unbounded tree. By default .gitignore is
   respected."
  ([] (list-files "." nil))
  ([path] (list-files path nil))
  ([path opts]
   (let [{:keys [depth hidden? respect-gitignore?]} (coerce-ls-opts opts)
         f         (safe-path path)
         depth-lim (depth->limit depth)]
     (when-not (.exists f)
       (throw (ex-info (path-not-found-message path)
                {:type :ext.common-editing.editing/not-found :path path})))
     (when-not (.isDirectory f)
       (throw (ex-info (str "Not a directory: " path)
                {:type :ext.common-editing.editing/not-directory :path path})))
     (mapv #(file->tree % (dec depth-lim) hidden? respect-gitignore?)
       (visible-children f hidden? respect-gitignore?)))))

(defn- compile-safe-pattern
  [pattern]
  (try
    (Pattern/compile (str pattern))
    (catch Exception e
      (throw (ex-info (str "Invalid regex pattern: " pattern)
               {:type :ext.common-editing.editing/invalid-regex
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

(defn- coerce-rg-opts
  "Normalize the optional third arg of (vis/rg pattern path opts) into
   {:limit :hidden? :respect-gitignore?}. Accepts:

   - nil / absent  -> all defaults
   - a map         -> picked apart by key (unknown keys ignored)
   - an integer    -> {:limit n} (back-compat: positional limit)

   Anything else throws so a typo at call-site (e.g. passing a
   keyword like :all instead of an opts map) surfaces here."
  [opts]
  (cond
    (nil? opts)
    {:limit default-grep-limit :hidden? false :respect-gitignore? default-respect-gitignore?}

    (map? opts)
    {:limit              (get opts :limit default-grep-limit)
     :hidden?            (boolean (get opts :hidden? false))
     :respect-gitignore? (boolean (get opts :respect-gitignore? default-respect-gitignore?))}

    (integer? opts)
    {:limit opts :hidden? false :respect-gitignore? default-respect-gitignore?}

    :else
    (throw (ex-info (str "Invalid (vis/rg) opts: " (pr-str opts)
                      ". Expected a map like {:limit 50 :hidden? true} or an integer limit.")
             {:type :ext.common-editing.editing/invalid-rg-opts :opts opts}))))

(defn- grep-files
  "Search for pattern in files. Always returns structured maps.

   Variadic shape, opts is a map. See rg-symbol :examples for
   concrete call patterns; in short:

     (vis/rg pattern)               ; search cwd, default limit 200
     (vis/rg pattern path)          ; search PATH, default limit 200
     (vis/rg pattern path opts)     ; opts is a map of
                                    ;   {:limit :hidden? :respect-gitignore?}
     (vis/rg pattern path limit)    ; back-compat: integer limit

   By default, .gitignore is respected."
  ([pattern] (grep-files pattern "." nil))
  ([pattern path] (grep-files pattern path nil))
  ([pattern path opts]
   (let [{:keys [limit hidden? respect-gitignore?]} (coerce-rg-opts opts)
         f   (safe-path path)
         pat (compile-safe-pattern pattern)]
     (when-not (.exists f)
       (throw (ex-info (path-not-found-message path)
                {:type :ext.common-editing.editing/not-found :path path})))
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

(defn- exact-match-count
  [content old-text]
  (count (re-seq (re-pattern (java.util.regex.Pattern/quote old-text)) content)))

(defn- collapse-whitespace
  "Collapse every run of whitespace to a single space. Used by the
   patch-no-match diagnostic to detect search blocks that differ from
   the on-disk content only in indentation/inner whitespace."
  ^String [^String s]
  (str/replace s #"\s+" " "))

(defn- find-whitespace-only-near-match
  "When `search` doesn't match `content` exactly, but matches when both
   are whitespace-collapsed, return
     {:line N :hint \"whitespace differs\" :snippet S}
   where N is 1-based and S is the closest on-disk line. Returns nil
   when no such whitespace-only candidate exists.

   Strategy: take the first non-blank line of the search and look for
   on-disk lines whose whitespace-collapsed form equals it AND whose
   raw form differs (so we don't false-positive on lines that exactly
   matched a different SEARCH block earlier). For multi-line searches
   this anchors at the start; that's enough information for the LLM
   to re-emit with the right indentation."
  [^String content ^String search]
  (let [search-lines       (str/split-lines search)
        first-search-line  (some #(when-not (str/blank? %) %) search-lines)]
    (when first-search-line
      (let [head           (str/trim (collapse-whitespace first-search-line))
            content-lines  (str/split-lines content)
            matches        (keep-indexed
                             (fn [i line]
                               (when (and (= head (str/trim (collapse-whitespace line)))
                                       (not= line first-search-line))
                                 [(inc i) line]))
                             content-lines)]
        (when (seq matches)
          (let [[line-number snippet] (first matches)]
            {:line    line-number
             :hint    "whitespace differs"
             :snippet snippet}))))))

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
        (let [near (find-whitespace-only-near-match content search)
              base-msg (str "SEARCH block " index " not found in " path)
              msg  (if near
                     (str base-msg
                       ". Closest line: " (:line near)
                       " (whitespace differs). Re-emit with the on-disk indentation.")
                     base-msg)]
          (throw (ex-info msg
                   (cond-> {:type   :ext.common-editing.editing/patch-no-match
                            :path   path
                            :block  index
                            :search search}
                     near (assoc :near-match near)))))

        (> count 1)
        (throw (ex-info (str "SEARCH block " index " matches " count
                          " times in " path ". Must be unique.")
                 {:type :ext.common-editing.editing/patch-ambiguous
                  :path path
                  :block index
                  :matches count
                  :search search}))

        :else
        (str/replace-first content
          (re-pattern (java.util.regex.Pattern/quote search))
          replace)))))

(defn- apply-blocks-to-content
  [content blocks path]
  (reduce (fn [current-content block]
            (apply-one-replacement current-content block path))
    content
    blocks))

(def ^:private patch-shape-message
  "vis/patch takes ONE argument: a vector of {:path :search :replace} maps. Example: (vis/patch [{:path \"src/foo.clj\" :search \"old\" :replace \"new\"}]). For multi-line content, compose with (str \"line1\\n\" \"line2\\n\"). Empty :search means insert at start of (possibly new) file.")

(defn- normalize-vector-edit
  "Validate one entry of the canonical edit vector and produce a
   `{:path :blocks}` plan row. Each entry contributes exactly one
   SEARCH/REPLACE block; the block's `:index` is the entry's 1-based
   position in the vector so error messages tell the model exactly
   which entry failed."
  [position entry]
  (when-not (map? entry)
    (throw (ex-info (str "vis/patch entry at position " position
                      " is not a map. Got: " (pr-str entry) ". "
                      patch-shape-message)
             {:type :ext.common-editing.editing/patch-invalid-edit
              :position position :entry entry})))
  (let [{:keys [path search replace]} entry
        extra-keys (seq (disj (set (keys entry)) :path :search :replace))]
    (when extra-keys
      (throw (ex-info (str "vis/patch entry at position " position
                        " has unsupported keys: " (vec extra-keys)
                        ". Allowed keys are :path, :search, :replace. "
                        patch-shape-message)
               {:type :ext.common-editing.editing/patch-invalid-edit
                :position position :entry entry :extra-keys (vec extra-keys)})))
    (when-not (string? path)
      (throw (ex-info (str "vis/patch entry at position " position
                        " :path must be a string. Got: " (pr-str path) ". "
                        patch-shape-message)
               {:type :ext.common-editing.editing/patch-invalid-edit
                :position position :entry entry})))
    (when-not (string? search)
      (throw (ex-info (str "vis/patch entry at position " position
                        " :search must be a string (may be \"\" to insert at start of file). Got: "
                        (pr-str search) ". " patch-shape-message)
               {:type :ext.common-editing.editing/patch-invalid-edit
                :position position :entry entry})))
    (when-not (string? replace)
      (throw (ex-info (str "vis/patch entry at position " position
                        " :replace must be a string (may be \"\" to delete the matched search). Got: "
                        (pr-str replace) ". " patch-shape-message)
               {:type :ext.common-editing.editing/patch-invalid-edit
                :position position :entry entry})))
    {:path   path
     :blocks [{:index (inc position) :search search :replace replace}]}))

(defn- normalize-patch-plan
  "Canonical shape: ONE argument, a non-empty vector of
   `{:path :search :replace}` maps. Anything else throws a teaching
   error message that points the model at the canonical shape."
  [arguments]
  (when-not (and (= 1 (count arguments)) (vector? (first arguments)))
    (throw (ex-info (str "Invalid vis/patch call shape. " patch-shape-message
                      " Got: " (pr-str arguments))
             {:type :ext.common-editing.editing/patch-invalid-arity
              :args arguments})))
  (let [edit-vector (first arguments)]
    (when (empty? edit-vector)
      (throw (ex-info (str "vis/patch edit vector is empty. " patch-shape-message)
               {:type :ext.common-editing.editing/patch-invalid-arity
                :args arguments})))
    (mapv normalize-vector-edit (range) edit-vector)))

(defn- apply-patch-plan
  [patch-plan]
  (let [workspace
        (reduce (fn [state {:keys [path]}]
                  (if (contains? state path)
                    state
                    (let [file (safe-path path)
                          exists? (.exists file)
                          _ (when (and exists? (.isDirectory file))
                              (throw (ex-info (str "Path is a directory: " path)
                                       {:type :ext.common-editing.editing/is-directory :path path})))
                          content (if exists? (slurp file) "")]
                      (assoc state path {:file file
                                         :exists? exists?
                                         :original-content content
                                         :content content
                                         :applied 0}))))
          {}
          patch-plan)
        patched-workspace
        (reduce (fn [state {:keys [path blocks]}]
                  (let [{:keys [exists? content]} (get state path)
                        _ (when (and (not exists?)
                                  (some #(not= "" (:search %)) blocks))
                            (throw (ex-info (str "Cannot patch non-existent file " path
                                              " unless :search is \"\" (empty string means insert at start of new file).")
                                     {:type :ext.common-editing.editing/patch-create-requires-empty-search
                                      :path path})))
                        next-content (apply-blocks-to-content content blocks path)]
                    (assoc-in (assoc-in state [path :content] next-content)
                      [path :applied]
                      (+ (get-in state [path :applied] 0) (count blocks)))))
          workspace
          patch-plan)
        changed-paths
        (->> patched-workspace
          (filter (fn [[_ {:keys [original-content content]}]]
                    (not= original-content content)))
          (map first)
          vec)]
    ;; Disk write happens only after EVERY edit validated in-memory.
    (doseq [path changed-paths]
      (let [{:keys [file content]} (get patched-workspace path)]
        (when-let [parent (.getParentFile file)]
          (.mkdirs parent))
        (spit file content)))
    {:status :ok
     :op :patch
     :files (mapv (fn [path]
                    (let [{:keys [exists? applied content]} (get patched-workspace path)]
                      {:path path
                       :created? (not exists?)
                       :applied applied
                       :size (count content)}))
              changed-paths)
     :files-touched (count changed-paths)}))

(defn- patch
  "Apply a vector of edits across one or more files.

   Canonical shape:
     (vis/patch [{:path \"...\" :search \"...\" :replace \"...\"} ...])

   Each edit map MUST have exactly :path, :search, :replace (all
   strings). :search must match the file contents EXACTLY ONCE. Use
   :search \"\" to insert at the start of (possibly new) file.

   For multi-line content, compose with (str ...) so each line stays
   on its own physical line:
     :search (str \"(defn foo [x]\\n\" \"  (inc x))\")"
  [& arguments]
  (-> arguments
    normalize-patch-plan
    apply-patch-plan))

;; =============================================================================
;; Extension definition
;; =============================================================================

(def cat-symbol
  (ext/symbol 'cat read-file
    {:doc (str "Read file contents. Use the opts MAP for paging "
            "(non-default offset/limit) -- you almost never need "
            "anything past `(vis/cat path)` for a first look.")
     :arglists '([path] [path opts])
     :examples ["(vis/cat \"README.md\")"
                "(vis/cat \"path/to/file.clj\" {:offset 60 :limit 100})"
                "(vis/cat \"path/to/file.clj\" {:char-limit 4000})"]
     :on-error-fn rescue-path-args
     :autobind-fn read-file-autobind}))

(def ls-symbol
  (ext/symbol 'ls list-files
    {:doc (str "List files/dirs as a tree of "
            "{:name :path :type :size :hidden? :children}. "
            "All args optional. Use the opts MAP for non-default "
            "depth/hidden/gitignore settings -- you almost never "
            "need anything past `(vis/ls)` or `(vis/ls path)`.")
     :arglists '([] [path] [path opts])
     :examples ["(vis/ls)"
                "(vis/ls \".\")"
                "(vis/ls \"src\" {:depth 3})"
                "(vis/ls \".\" {:depth true :hidden? true})"]
     :on-error-fn rescue-path-args}))

(def rg-symbol
  (ext/symbol 'rg grep-files
    {:doc (str "Search files with RE2/J (linear-time regex, ReDoS-safe). "
            "Equivalent to ripgrep for this sandbox. Always returns "
            "structured maps. Use the opts MAP for non-default "
            "limit/hidden/gitignore settings.\n\n"
            "REGEX ESCAPING. Inside a Clojure string literal `\\|` "
            "is NOT a valid escape -- use a bare `|` for regex "
            "alternation: \"a|b|c\". For any pattern with quotes, "
            "brackets, or alternation, prefer the regex literal "
            "`#\"...\"` form -- it avoids string-escape headaches "
            "and renders the regex EXACTLY as the engine sees it.")
     :arglists '([pattern] [pattern path] [pattern path opts])
     :examples ["(vis/rg \"TODO\")"
                "(vis/rg \"TODO\" \"src\")"
                "(vis/rg #\"defn|defmacro\" \"src\" {:limit 50})"
                "(vis/rg #\"\\(let \\[\" \"src\")"]
     :on-error-fn       rescue-grep-args
     ;; Parse-time rescue: when the LLM emits e.g.
     ;;   (vis/rg \"foo\\|bar\")
     ;; — raw `\|` inside the string literal — edamame errors out
     ;; before any tool dispatch. The iteration loop notices the
     ;; broken form mentions `vis/rg`, calls THIS hook, and retries
     ;; with the doubled backslash. See `rescue-parse-error`.
     :on-parse-error-fn rescue-parse-error}))

(def patch-symbol
  (ext/symbol 'patch patch
    {:doc (str "Apply a vector of edits across one or more files. "
            "ONE shape, always:\n\n"
            "  (vis/patch [{:path P :search S :replace R} ...])\n\n"
            "Each map has EXACTLY :path, :search, :replace (all "
            "strings). :search must match the file contents EXACTLY "
            "ONCE -- pick the smallest unique snippet; whitespace and "
            "indentation must match the file. :search \"\" inserts at "
            "the start of (possibly new) file. For multi-line content, "
            "compose with (str \"line1\\n\" \"line2\\n\") so each line "
            "stays on its own physical line and the closing quote stays "
            "visible.")
     :arglists '([edits])
     :examples ["(vis/patch [{:path \"src/foo.clj\" :search \"old\" :replace \"new\"}])"
                "(vis/patch [{:path \"src/a.clj\" :search \"x\" :replace \"y\"} {:path \"src/b.clj\" :search \"foo\" :replace \"bar\"}])"
                "(vis/patch [{:path \"new.clj\" :search \"\" :replace (str \"(ns new)\\n\" \"(def x 1)\\n\")}])"]
     :on-error-fn rescue-path-args
     :autobind-fn patch-autobind}))

(def editing-symbols
  "Vector of `ext/symbol` definitions exported by this module. The
   sibling `core` namespace concatenates every module's symbol vector
   into the single `vis-common-editing` extension."
  [cat-symbol
   ls-symbol
   rg-symbol
   patch-symbol])

(def editing-prompt
  "Module-specific prompt fragment merged into the extension prompt by
   the `core` aggregator. Lives next to the symbols so a future
   reorganization moves both pieces together.

   This is a FLOW, not a list of admonitions. The model already knows
   not to invent paths; what it needs is a concrete recipe for how to
   compose the four tools so it doesn't re-read files or fan out one
   patch per edit. Keep this fragment short -- every line earns its
   place."
  "EDITING FLOW (compose, don't probe):

1. LOCATE.  (vis/ls path) finds files. (vis/rg pattern path) finds the
   exact line. Always pass a path -- do not run vis/rg over the whole repo.
   For regex with quotes/brackets/alternation, prefer #\"...\" over a
   string literal (no `\\|` headaches).
2. LOAD.    (vis/cat path) reads the file AND autobinds it to
   file/<munged-path>. Reuse that var with (subs file/x ...),
   (re-find #\"...\" file/x), (str/split-lines file/x). Re-reading the
   same path is a bug -- the var is already there.
3. EDIT.    (vis/patch [{:path :search :replace} ...]) -- ONE shape,
   ALWAYS a vector of maps. Single edit? Vector of one. Multi-file?
   More maps. Each :search must match the file EXACTLY ONCE. For
   multi-line content, compose with (str \"line1\\n\" \"line2\\n\") so
   each line stays on its own physical line and you can see the
   closing quote.
4. CREATE.  Empty :search means \"insert at start of (possibly new)
   file\":
     (vis/patch [{:path \"new.clj\" :search \"\" :replace \"...\"}]).

TUNING. ls / cat / rg take an OPTS MAP as their last arg:
  (vis/ls path {:depth :hidden? :respect-gitignore?})
  (vis/cat path {:offset :limit :char-limit})
  (vis/rg pattern path {:limit :hidden? :respect-gitignore?})
Skip the opts map 90% of the time; reach for it only when the
defaults don't fit (paging a huge file, scanning hidden dirs, etc.).")
