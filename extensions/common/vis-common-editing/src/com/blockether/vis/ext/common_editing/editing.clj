(ns com.blockether.vis.ext.common-editing.editing
  "Editing tools exposed under the `vis/` alias in the SCI sandbox.

   Surface (intentionally small):

     (vis/cat path)            ; whole file as a string
     (vis/cat path opts)       ; opts is {:offset N :limit M :char-limit C}
     (vis/ls path)             ; non-recursive directory listing
     (vis/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
     (vis/rg pattern path)     ; ripgrep over a tree (re2j; ReDoS-safe)
     (vis/edit path search replace)   ; exact-match search/replace; :search must be unique
     (vis/write path content)         ; overwrite or create the file
     (vis/zedit path zfn)             ; rewrite-clj structured edit on a Clojure file

   Plus the babashka.fs surface bound under `fs/` (cwd, exists?, glob,
   parent, components, file-name, extension, expand-home, list-dir,
   relativize). Use the fs primitives for path math; reach for a
   higher-level vis/ symbol when you also need to read or mutate the
   file's contents.

   No autobind, no patch DSL, no parse-error rescue stack. Read once,
   bind to a `(def ...)` if you need it across iterations, edit in
   one call.

   Hard guard: every path must stay inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as sdk]
   [rewrite-clj.zip :as z])
  (:import
   (com.google.re2j Pattern PatternSyntaxException)
   (java.io File)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 200)
(def ^:private default-read-char-limit 6000)
(def ^:private default-list-depth 2)

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- ^File safe-path
  "Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
   the working directory."
  [p]
  (let [cwd (fs/cwd)
        resolved (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.common-editing/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.common-editing/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.common-editing/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (fs/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

;; =============================================================================
;; .gitignore (cheap, lazy)
;; =============================================================================

(defn- ^IgnoreNode load-ignore-node [^File root]
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
  (cond
    (nil? opts) {:offset nil :limit nil :char-limit default-read-char-limit}
    (map? opts) {:offset     (:offset opts)
                 :limit      (:limit opts)
                 :char-limit (or (:char-limit opts) default-read-char-limit)}
    (integer? opts) {:offset opts :limit nil :char-limit default-read-char-limit}
    :else (throw (ex-info (str "Invalid (vis/cat) opts: " (pr-str opts))
                   {:type :ext.common-editing/invalid-cat-opts :opts opts}))))

(defn- read-file
  ([path] (read-file path nil))
  ([path opts]
   (let [{:keys [offset limit char-limit]} (coerce-cat-opts opts)
         f (ensure-existing-file! (safe-path path))
         lines (str/split-lines (slurp f))
         total (count lines)
         off   (max 0 (dec (or offset 1)))
         lim   (or limit total)
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
             capped? (str "preview capped at " char-limit
                       " chars. Use offset=" (+ off 1)
                       " limit=" (max 50 lim) " to continue")
             :else (str (- total off showing) " more lines. Use offset="
                     (+ off showing 1) " to continue"))
           "]"))))))

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
                      {:type :ext.common-editing/path-not-found})))
         opts* {:hidden? hidden?
                :respect-gitignore? respect-gitignore?
                :ignore-node (when respect-gitignore? (load-ignore-node f))
                :root f}]
     (file->tree f opts* depth))))

;; =============================================================================
;; rg
;; =============================================================================

(defn- compile-pattern [p]
  (cond
    (instance? java.util.regex.Pattern p) (Pattern/compile (str p))
    (string? p) (try (Pattern/compile p)
                  (catch PatternSyntaxException e
                    (throw (ex-info (str "Invalid regex: " (ex-message e))
                             {:type :ext.common-editing/invalid-pattern}))))
    :else (throw (ex-info "rg pattern must be a string or regex" {:got (type p)}))))

(defn- grep-files
  ([pattern path] (grep-files pattern path nil))
  ([pattern path opts]
   (let [{:keys [limit hidden? respect-gitignore?]
          :or {limit default-grep-limit hidden? false respect-gitignore? true}} (or opts {})
         pat   (compile-pattern pattern)
         root  (safe-path path)
         _     (when-not (.exists root)
                 (throw (ex-info (str "Path not found: " (.getPath root))
                          {:type :ext.common-editing/path-not-found})))
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
     (cond-> (vec @hits)
       @capped? (conj {:capped? true :limit limit
                       :note (str "result truncated at " limit " matches")})))))

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
   without another vis/cat round-trip."
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
              `vis/cat` printed). No ambiguity check; nearest match wins.

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
       (throw (ex-info "vis/edit :search must be non-blank. Use vis/write to overwrite a whole file."
                {:type :ext.common-editing/blank-search})))
     (let [start-from (if line (char-offset-of-line original line) 0)
           first-idx (.indexOf original s start-from)
           second-idx (when (and (not line) (>= first-idx 0))
                        (.indexOf original s (+ first-idx (count s))))]
       (cond
         (neg? first-idx)
         (throw (ex-info (str "vis/edit :search not found in " (rel-path f)
                           (when line (str " (searching from line " line " onward)")))
                  {:type :ext.common-editing/search-not-found
                   :path (rel-path f)
                   :search (subs s 0 (min 200 (count s)))}))
         (and second-idx (>= second-idx 0))
         (let [hits (match-context original s 5)]
           (throw (ex-info (str "vis/edit :search appears " (count hits)
                             " times in " (rel-path f)
                             ". Either add surrounding lines until the match is unique, or pass {:line N} to pick the occurrence on or after that line. Hits: "
                             (pr-str (mapv (fn [h] (str "L" (:line h) ": " (str/trim (:context h)))) hits)))
                    {:type :ext.common-editing/search-ambiguous
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

(defn- zedit-file
  "rewrite-clj structured edit. `zfn` receives a zipper at the file root
   and must return a zipper. The new source is written back."
  [path zfn]
  (let [f (ensure-existing-file! (safe-path path))
        zloc (z/of-file f {:track-position? true})
        zout (zfn zloc)]
    (when (nil? zout)
      (throw (ex-info "zedit zfn must return a zipper, got nil"
               {:type :ext.common-editing/zedit-nil-result})))
    (spit f (z/root-string zout))
    {:path (rel-path f)}))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def cat-symbol
  (sdk/symbol 'cat read-file
    {:doc "Read a file. Returns line-numbered content. Default cap 6000 chars; opts {:offset :limit :char-limit}."
     :arglists '([path] [path opts])
     :examples ["(vis/cat \"src/main.clj\")"
                "(vis/cat \"big.log\" {:offset 5000 :limit 200})"]}))

(def ls-symbol
  (sdk/symbol 'ls list-files
    {:doc "List a directory tree. opts {:depth :hidden? :respect-gitignore?}."
     :arglists '([path] [path opts])
     :examples ["(vis/ls \".\")"
                "(vis/ls \"src\" {:depth 3})"]}))

(def rg-symbol
  (sdk/symbol 'rg grep-files
    {:doc "Search files with a regex (re2j). pattern is a string or #\"...\"; path is the search root. opts {:limit :hidden? :respect-gitignore?}."
     :arglists '([pattern path] [pattern path opts])
     :examples ["(vis/rg \"defn render\" \"src\")"
                "(vis/rg #\"foo|bar\" \"src\" {:limit 50})"]}))

(def edit-symbol
  (sdk/symbol 'edit edit-file
    {:doc "Replace the FIRST occurrence of `search` with `replace` in `path`. Throws if `search` is missing or non-unique. Pass {:line N} as a 4th arg to pick the occurrence on or after line N when context-anchoring is hard."
     :arglists '([path search replace] [path search replace opts])
     :examples ["(vis/edit \"src/main.clj\" \"(def OLD 1)\" \"(def NEW 2)\")"
                "(vis/edit \"src/main.clj\" \"foo\" \"bar\" {:line 142})"]}))

(def write-symbol
  (sdk/symbol 'write write-file
    {:doc "Overwrite or create a file with the given content. Creates parent directories as needed."
     :arglists '([path content])
     :examples ["(vis/write \"new.txt\" \"hello\")"]}))

(def zedit-symbol
  (sdk/symbol 'zedit zedit-file
    {:doc "Structured edit of a Clojure file using rewrite-clj. zfn receives a zipper and must return a zipper. Useful for adding requires, renaming syms, etc."
     :arglists '([path zfn])
     :examples ["(vis/zedit \"src/x.clj\" #(rewrite-clj.zip/edit % str/upper-case))"]}))

(def editing-symbols
  [cat-symbol ls-symbol rg-symbol edit-symbol write-symbol zedit-symbol])

(def editing-prompt
  "EDITING. Six tools, all under the `vis/` alias:

  (vis/cat path)              read a file (default cap 6000 chars; pass {:offset :limit :char-limit} for paging)
  (vis/ls path)               list a tree ({:depth :hidden? :respect-gitignore?})
  (vis/rg pattern path)       regex grep (re2j; ReDoS-safe)
  (vis/edit path s r)         replace FIRST occurrence of `s` with `r`. `s` must be unique.
                              Pass {:line N} as 4th arg to disambiguate by line number
                              when context-anchoring is hard - (vis/edit path s r {:line 1992}).
  (vis/write path content)    overwrite or create a file
  (vis/zedit path zfn)        rewrite-clj structured edit on a Clojure file

Babashka filesystem helpers are bound under `fs/`:
  (fs/cwd) (fs/exists? p) (fs/glob root pat) (fs/parent p)
  (fs/file-name p) (fs/extension p) (fs/components p)
  (fs/expand-home p) (fs/list-dir d) (fs/relativize a b)

Read once, then operate on the result. If you'll need a value across
iterations, `(def x (vis/cat \"foo\"))`. Repeating an identical call is
deduped automatically; non-identical re-reads of the same region waste
your turn budget.")
