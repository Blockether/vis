(ns com.blockether.vis.ext.foundation.editing.core
  "Editing tools exposed under the `v/` alias in the SCI sandbox.

   Surface (intentionally small) — every tool returns pure
   structured data, no English prose smuggled into return values:

     (v/cat path)            ; -> {:path :offset :total-lines :truncated-by :lines}
     (v/cat path opts)       ; opts is {:offset N :limit M :char-limit C}
     (v/ls path)             ; -> nested {:name :path :type :size :children} tree
     (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
     (v/rg patterns path)    ; -> {:hits :truncated-by}; patterns = non-empty vec of literal substrings
     (v/edit path search replace)   ; -> {:path :bytes-before :bytes-after}
     (v/write path content)         ; -> {:path :bytes}

   Plus the babashka.fs surface bound under `fs/` (cwd, exists?, glob,
   parent, components, file-name, extension, expand-home, list-dir,
   relativize). Use the fs primitives for path math; reach for a
   higher-level v/ symbol when you also need to read or mutate the
   file's contents.

   Clojure-specific structured editing (`z/zedit` plus the `z/`
   rewrite-clj zipper API) lives in the `vis-language-clojure`
   extension under `extensions/languages/clojure/`.

   No patch DSL, no parse-error rescue stack. Read once,
   bind to a `(def ...)` if you need it across iterations, edit in
   one call.

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
;; edit / write
;; =============================================================================

(defn- write-file
  "Overwrite (or create) the file at `path` with `content`. Creates parent
   directories as needed."
  [path content]
  (let [f (safe-path path)]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f (str content))
    {:path (rel-path f) :bytes (count (str content))}))

(defn- match-context
  "Return up to `n` matches of `s` in `original` with line numbers and
   surrounding line context, so an ambiguous edit can be disambiguated
   without another v/cat round-trip."
  [original s n]
  (let [matches (loop [from 0 acc []]
                  (let [idx (.indexOf original s from)]
                    (cond
                      (neg? idx) acc
                      (>= (count acc) n) acc
                      :else (recur (+ idx (count s)) (conj acc idx)))))]
    (mapv (fn [idx]
            (let [pre  (subs original 0 idx)
                  line (inc (count (re-seq #"\n" pre)))
                  ;; pull a one-line slice around the match
                  start (or (some-> (str/last-index-of original "\n" idx) inc) 0)
                  end   (let [after-end (+ idx (count s))
                              nl        (str/index-of original "\n" after-end)]
                          (or nl (count original)))]
              {:line line
               :context (subs original start end)}))
      matches)))

(defn- char-offset-of-line
  [^String s line]
  (loop [i 0 idx 0]
    (cond
      (>= i (dec line)) idx
      (>= idx (count s)) idx
      :else (let [next-nl (.indexOf s "\n" idx)]
              (if (neg? next-nl) (count s) (recur (inc i) (inc next-nl)))))))

(defn- edit-file
  "Replace ONE occurrence of `search` with `replace` in `path`.

   3-arg form replaces the FIRST occurrence; throws when ambiguous.
   4-arg form takes opts — supported keys:
     :line N  pick the occurrence on or after line N (the same number
              `v/cat` printed). No ambiguity check; nearest match wins.

   When ambiguous (no :line given), the error embeds matched line
   numbers + surrounding context so the model can disambiguate without
   re-reading the file."
  ([path search replace] (edit-file path search replace nil))
  ([path search replace opts]
   (let [f (ensure-existing-file! (safe-path path))
         original (slurp f)
         s (str search)
         r (str replace)
         line (:line opts)]
     (when (str/blank? s)
       (throw (ex-info "v/edit :search must be non-blank. Use v/write to overwrite a whole file."
                {:type :ext.foundation.editing/blank-search})))
     (let [start-from (if line (char-offset-of-line original line) 0)
           first-idx (.indexOf original s start-from)
           second-idx (when (and (not line) (>= first-idx 0))
                        (.indexOf original s (+ first-idx (count s))))]
       (cond
         (neg? first-idx)
         (throw (ex-info (str "v/edit :search not found in " (rel-path f)
                           (when line (str " (searching from line " line " onward)")))
                  {:type :ext.foundation.editing/search-not-found
                   :path (rel-path f)
                   :search (subs s 0 (min 200 (count s)))}))
         (and second-idx (>= second-idx 0))
         (let [hits (match-context original s 5)]
           (throw (ex-info (str "v/edit :search appears " (count hits)
                             " times in " (rel-path f)
                             ". Either add surrounding lines until the match is unique, or pass {:line N} to pick the occurrence on or after that line. Hits: "
                             (pr-str (mapv (fn [h] (str "L" (:line h) ": " (str/trim (:context h)))) hits)))
                    {:type :ext.foundation.editing/search-ambiguous
                     :path (rel-path f)
                     :hits hits})))
         :else
         (let [updated (str (subs original 0 first-idx)
                         r
                         (subs original (+ first-idx (count s))))]
           (spit f updated)
           {:path (rel-path f)
            :bytes-before (count original)
            :bytes-after  (count updated)}))))))

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

(def edit-symbol
  (vis/symbol 'edit edit-file
    {:doc "Replace the FIRST occurrence of `search` with `replace` in `path`. Throws if `search` is missing or non-unique. Pass {:line N} as a 4th arg to pick the occurrence on or after line N when context-anchoring is hard."
     :arglists '([path search replace] [path search replace opts])
     :examples ["(v/edit \"src/main.clj\" \"(def OLD 1)\" \"(def NEW 2)\")"
                "(v/edit \"src/main.clj\" \"foo\" \"bar\" {:line 142})"]}))

(def write-symbol
  (vis/symbol 'write write-file
    {:doc "Overwrite or create a file with the given content. Creates parent directories as needed."
     :arglists '([path content])
     :examples ["(v/write \"new.txt\" \"hello\")"]}))

(def editing-symbols
  [cat-symbol ls-symbol rg-symbol edit-symbol write-symbol])

(def editing-prompt
  "`v/` = file I/O (5 tools). EVERY tool returns pure structured data — maps with explicit keys, never English-prose strings. Compose display text yourself when you need it.
  (v/cat path)             -> {:path :offset :total-lines :truncated-by :lines}. :lines = vec of raw line strings. Default char-limit 6000; opts {:offset :limit :char-limit}. Pagination: `(v/cat p {:offset (+ (:offset r) (count (:lines r)))})`.
  (v/ls path)              -> nested {:name :path :type :size :children} tree. opts {:depth :hidden? :respect-gitignore?}.
  (v/rg [\"a\" \"b\"] path)  -> {:hits [{:path :line :text} ...] :truncated-by :limit | :end-of-results}. patterns = non-empty vec of literal strings (no regex DSL). For real regex: (re-seq #\"...\" (str/join \"\\n\" (:lines (v/cat path)))).
  (v/edit path s r)        -> {:path :bytes-before :bytes-after}. Replace FIRST `s`->`r`. `s` must be unique. 4th arg {:line N} disambiguates by line.
  (v/write path content)   -> {:path :bytes}. Overwrite/create.

For structured Clojure edits use `(z/zedit path zfn)` (vis-language-clojure ext, alias `z/`).

Read once, reuse. Cross-iter? `(def x (v/cat ...))`. Identical re-reads run again on every call — bind once, reuse via the var.")
