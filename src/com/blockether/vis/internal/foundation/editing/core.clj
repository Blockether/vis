(ns com.blockether.vis.internal.foundation.editing.core
  "Filesystem tools exposed as bare symbols in the Python sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (cat path)            ; -> {:path :lines [[N text]…] :next-offset N? :truncated? B}
        (cat path n)          ; first n lines from line 1
        (cat path offset n)   ; n lines starting at line `offset` (1-based)
        (cat path :tail)      ; last 400 lines (tail)
        (cat path :tail n)    ; last n lines
        (ls path)             ; -> nested {:name :path :type :size :children} tree
        (ls path opts)        ; opts is {:depth :is_hidden :is_respect_gitignore}
        (rg spec)            ; -> {:hits :truncated-by}; spec = {:any [literal] :paths [src]}
                               ; OR {:all [lit1 lit2]}; no regex/query+opts shorthand

   2. Cwd-safe wrappers over the babashka.fs file API. `patch` is
      the canonical text edit surface:

        (cat path)
        (patch [{:path path :search old :replace new}])
        (create-dirs path)
        (copy src dest)
        (move src dest)
        (delete path)
        (delete-if-exists path)
        (exists? path)

   Hard guard: every path must stay inside the session's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (com.github.difflib DiffUtils UnifiedDiffUtils)
   (java.io File)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 250)
(def ^:private default-list-depth 10)
(def ^:private default-list-limit 3000)
(def ^:private render-preview-chars 3000)

;; cat pagination contract:
;;   `default-cat-limit`     - lines per window when the model omits `n`.
;;                             Industry parity — Claude Code / Roo Code use
;;                             2000 by default; Cline uses 1000.
;;   `max-cat-window-bytes`  - hard ceiling on a single window's bytes.
;;                             Doubles as the persistence-blob ceiling:
;;                             each call's result is Nippy-frozen into the
;;                             iteration's `forms` BLOB, bounded by this.
;;                             Not user-tunable; it is the storage contract.
;;
;; There is no `max-line-length` per-line cap (no 2000-char + `…<+N chars
;; truncated>` marker). Such a cap produces the same failure pattern
;; as the absent trailer/rg caps (see ctx_renderer.clj
;; header + conversation ccee2e1f-16ee-4acf-8d93-b4505034c0de): a
;; silent ellipsis makes the model perceive its own data as missing and
;; chase phantom roundtrips even on legitimate long source lines.
;; The structural defense is the per-window byte cap above — a single
;; pathological line is included whole (so the model sees actual data)
;; and the next iteration stops with `:truncated? true :next-offset N`,
;; which the model already knows how to page through.
(def ^:private default-cat-limit 2000)
(def ^:private max-cat-window-bytes 262144)

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.
  (when (str/blank? (str p))
    (throw (ex-info "Path is nil or blank — cat/rg/ls take a concrete path string; note rg returns a MAP, so use (:files r) or (map :path (:matches r)), not the rg result itself"
             {:type :ext.foundation.editing/blank-path :path p})))
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

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace/cwd)))
        p   (.toAbsolutePath (.toPath f))
        rel (str (.relativize cwd p))]
    (if (str/blank? rel) "." rel)))

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

;; =============================================================================
;; Extension protected paths
;; =============================================================================

(def ^:private protected-access-rank
  {:read-write 0
   :read-only  1
   :none       2})

(defn- extracted-paths
  [path-extractor args]
  (try
    (let [paths (path-extractor args)]
      (vec (remove nil?
             (if (sequential? paths) paths [paths]))))
    (catch Throwable _
      [])))

(defn- first-arg-paths
  [args]
  (when (seq args)
    [(first args)]))

(defn- first-two-arg-paths
  [args]
  (take 2 args))

(defn- patch-arg-paths
  [args]
  (let [edits (first args)
        edits (cond
                (map? edits) [edits]
                (sequential? edits) edits
                :else [])]
    (keep :path edits)))

(defn- write-arg-paths
  [args]
  (when-let [path (and (map? (first args)) (:path (first args)))]
    [path]))

(defn- rg-arg-paths
  [args]
  (let [spec (first args)]
    (when (map? spec)
      (let [paths (cond
                    (contains? spec :paths) (:paths spec)
                    (contains? spec :files) (:files spec)
                    :else nil)]
        (cond
          (nil? paths) ["."]
          (sequential? paths) paths
          :else [paths])))))

(defn- protected-target
  [path kind]
  (let [target (path->target path kind)]
    (when (:resolved target)
      target)))

(defn- protected-glob-matches?
  [glob rel]
  (let [matcher (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                  (str "glob:" glob))
        rel     (str/replace (str rel) (str (char 92)) "/")
        name    (last (str/split rel #"/+"))]
    (boolean
      (some (fn [candidate]
              (try
                (.matches matcher (fs/path candidate))
                (catch Throwable _
                  false)))
        (distinct [rel name])))))

(def ^:private glob-meta-chars
  #{\* \? \[ \] \{ \}})

(defn- glob-static-prefix
  [glob]
  (let [glob (str/replace (str glob) (str (char 92)) "/")
        idx  (first (keep-indexed (fn [idx ch]
                                    (when (contains? glob-meta-chars ch) idx))
                      glob))
        raw-prefix (if idx (subs glob 0 idx) glob)
        prefix (if (and idx (not (str/ends-with? raw-prefix "/")))
                 (let [slash-idx (.lastIndexOf ^String raw-prefix "/")]
                   (if (neg? slash-idx) "" (subs raw-prefix 0 slash-idx)))
                 raw-prefix)
        prefix (str/replace prefix #"/+$" "")]
    (if (str/blank? prefix) "." prefix)))

(defn- path-prefix?
  [ancestor path]
  (let [ancestor (str/replace (str ancestor) (str (char 92)) "/")
        path     (str/replace (str path) (str (char 92)) "/")]
    (or (= "." ancestor)
      (= ancestor path)
      (str/starts-with? path (str ancestor "/")))))

(defn- composite-path-target?
  [{:keys [kind absolute]}]
  (or (= :dir kind)
    (and (= :path kind)
      absolute
      (.isDirectory (io/file absolute)))))

(defn- protected-rule-matches?
  [target rule]
  (or (protected-glob-matches? (:glob rule) (:resolved target))
    (and (composite-path-target? target)
      (let [rel    (:resolved target)
            prefix (glob-static-prefix (:glob rule))]
        (or (path-prefix? prefix rel)
          (and (not= :read-write (:access rule))
            (path-prefix? rel prefix)))))))

(defn- rules-by-extension
  [rules]
  (->> (map-indexed vector rules)
    (reduce (fn [groups [idx rule]]
              (let [ext-name (:extension/name rule)]
                (-> groups
                  (update-in [ext-name :idx] #(or % idx))
                  (update-in [ext-name :rules] (fnil conj []) rule))))
      {})
    vals
    (sort-by :idx)
    (mapv :rules)))

(defn- first-matching-rule
  [target rules]
  (some (fn [rule]
          (when (protected-rule-matches? target rule)
            rule))
    rules))

(defn- more-restrictive-rule
  [best rule]
  (if (or (nil? best)
        (> (protected-access-rank (:access rule))
          (protected-access-rank (:access best))))
    rule
    best))

(defn- resolve-protected-access
  [rules target]
  (reduce
    (fn [best extension-rules]
      (if-let [match (first-matching-rule target extension-rules)]
        (more-restrictive-rule best match)
        best))
    nil
    (rules-by-extension rules)))

(defn- blocked-access?
  [access-intent access]
  (or (= :none access)
    (and (= :write access-intent) (= :read-only access))))

(defn- current-dir-read-ancestor-match?
  "True when a read op is targeting `.` (cwd) and the matched rule
   protects only a descendant of `.`. Reading cwd itself must stay
   usable for every observation tool (ls, rg, cat-on-dir,
   exists?, grep, …); hidden protected extension roots (for
   example `.bridge/**`) should not make cwd reads fail closed.

   Direct rules for `.` (a glob that literally matches `.`) still
   apply — those are explicit \"do not read cwd\" decisions and the
   bypass leaves them intact.

   This bypass is INTENTIONALLY read-only. Writes/mutations on `.`
   stay blocked because they cannot be filtered descendant-by-descendant
   the way a recursive search/list can.

   Tools that recurse into `.` (rg, ls, grep) remain responsible for
   skipping protected descendants in their own walk — the bypass only
   lets the operation start."
  [_op access-intent target rule]
  (and (= :read access-intent)
    (composite-path-target? target)
    (= "." (:resolved target))
    (not (protected-glob-matches? (:glob rule) (:resolved target)))))

(defn- protected-failure-row
  [{:keys [target intent glob access hint] ext-name :extension/name}]
  {:path (:resolved target)
   :requested (:requested target)
   :reason :path-protected
   :intent intent
   :access access
   :glob glob
   :extension ext-name
   :hint hint})

(defn- path-protected-failure
  [op kind access-intent blocked]
  (let [t          (now-ms)
        first-row  (first blocked)
        first-tgt  (:target first-row)
        first-hint (:hint first-row)
        failures   (mapv protected-failure-row blocked)]
    (extension/failure
      {:result nil
       :op op
       :metadata {:target first-tgt
                  :started-at-ms t
                  :finished-at-ms t
                  :duration-ms 0
                  :access-intent access-intent
                  :protected-paths failures}
       :error {:message (str op " blocked: " (:resolved first-tgt)
                          " is protected; use the owning extension API instead.")
               :type :ext.foundation.editing/path-protected
               :reason :path-protected
               :intent access-intent
               :hint first-hint
               :loop-hint first-hint
               :failures failures
               :kind kind}})))

(defn- path-protection-error-failure
  [op kind err]
  (let [t (now-ms)]
    (extension/failure
      {:result nil
       :op op
       :metadata {:target (path->target "." kind)
                  :started-at-ms t
                  :finished-at-ms t
                  :duration-ms 0}
       :error {:message "Protected path registry failed; refusing direct file operation."
               :type :ext.foundation.editing/path-protection-error
               :reason :path-protection-error
               :hint "Fix the extension's :ext/protected-paths callback before retrying direct file IO."
               :loop-hint "Fix the extension's :ext/protected-paths callback before retrying direct file IO."
               :cause (ex-message err)}})))

(defn- path-protected-before-fn
  [op kind access-intent path-extractor]
  (fn [env f args]
    (try
      (let [rules   (extension/active-protected-globs env)
            targets (keep #(protected-target % kind)
                      (extracted-paths path-extractor args))
            blocked (keep (fn [target]
                            (when-let [rule (resolve-protected-access rules target)]
                              (when (and (blocked-access? access-intent (:access rule))
                                      (not (current-dir-read-ancestor-match?
                                             op access-intent target rule)))
                                (assoc rule
                                  :target target
                                  :intent access-intent))))
                      targets)]
        (if (seq blocked)
          {:result (path-protected-failure op kind access-intent (vec blocked))}
          {:env env :fn f :args args}))
      (catch Throwable t
        {:result (path-protection-error-failure op kind t)}))))

;; Engine contract lives in `com.blockether.vis.internal.extension`:
;;   `extension/op-tag`          - canonical op-keyword -> :observation | :mutation value.
;;   `extension/op-presentation` - `:info` metadata `{:tag ...}` embedded in tool envelopes.
;; The iteration loop's final-answer gate rejects any registered extension op
;; in the same iteration as `(done ...)`; op tags remain mandatory for
;; audit/permission policy.
;; Editing keeps no copies of these; call the engine functions directly to
;; avoid thin shims that cross the abstraction boundary.

;; Op tags are carried INLINE on each `vis/symbol` opts map below.

(defn- tool-success
  "Build a successful tool envelope. The caller passes `:metadata` (per-op
   diagnostics like `:next-offset`, `:truncated?`, `:mode`, `:hit-count`,
   etc.) and this fn merges it onto the standard `:target` / timing fields
   that every envelope carries. Earlier this fn took the local key `:info`
   and merged it into `:metadata`, which was confusing — the caller side
   used `:info`, the envelope side called the same data `:metadata`. One
   name end-to-end."
  [{:keys [op path kind result metadata]}]
  (let [t (now-ms)]
    (extension/success
      {:result   result
       :op       op
       :metadata (merge {:target         (path->target path kind)
                         :started-at-ms  t
                         :finished-at-ms t
                         :duration-ms    0}
                   metadata)})))

(defn- tool-failure-on-error
  [op kind _render-fn]
  (fn [err _env _f args]
    (let [path         (first args)
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

(defn- validate-cat-args!
  [offset n]
  (when-not (and (integer? offset) (pos? offset))
    (throw (ex-info "cat offset must be a positive integer (1-based line number)."
             {:type :ext.foundation.editing/invalid-cat-args
              :offset offset})))
  (when-not (and (integer? n) (pos? n))
    (throw (ex-info "cat limit must be a positive integer line count."
             {:type :ext.foundation.editing/invalid-cat-args
              :limit n}))))

(defn- validate-cat-range!
  [start end]
  (when-not (and (integer? start) (integer? end)
              (pos? start) (pos? end)
              (<= start end))
    (throw (ex-info "cat :range/:ranges start/end must be positive ints with start <= end"
             {:type :ext.foundation.editing/invalid-cat-args
              :start start :end end}))))

(defn- normalize-cat-ranges
  [ranges]
  (let [pairs (cond
                (and (vector? ranges)
                  (= 2 (count ranges))
                  (every? integer? ranges))
                [ranges]

                (sequential? ranges)
                (vec ranges)

                :else
                (throw (ex-info "cat :ranges expects [[start end] ...]"
                         {:type :ext.foundation.editing/invalid-cat-args
                          :ranges ranges})))]
    (when (empty? pairs)
      (throw (ex-info "cat :ranges expects at least one range"
               {:type :ext.foundation.editing/invalid-cat-args
                :ranges ranges})))
    (mapv (fn [pair]
            (when-not (and (sequential? pair) (= 2 (count pair)))
              (throw (ex-info "cat :ranges entries must be [start end] pairs"
                       {:type :ext.foundation.editing/invalid-cat-args
                        :range pair})))
            (let [[start end] (vec pair)]
              (validate-cat-range! start end)
              [(long start) (long end)]))
      pairs)))

(defn- read-file
  "Read a window of a text file as pure structured data.

   Arities:
     (read-file path)              ; first `default-cat-limit` lines from line 1
     (read-file path n)            ; first n lines from line 1
     (read-file path offset n)     ; n lines starting at line `offset` (1-based)

   Returns
   `{:path :lines [[N text]…] :next-offset N? :eof? B :truncated? B
     :mtime EPOCH-MS :size BYTES}`.

   `:lines` is a vec of `[line-number, text]` tuples — line number first so
   the model destructures `[ln t]` without offset arithmetic. Each line's
   `text` is verbatim — no per-line character cap. A pathological single
   long line is included whole and the next iteration's byte cap will
   page the rest via `:next-offset`.
   `:next-offset` is nil at EOF, integer otherwise.
   `:eof? true` iff the window reached end-of-file (unambiguous; distinct
   from `:truncated?` which only fires when the window byte cap chopped
   the window short mid-file).
   `:mtime` and `:size` mirror `File.lastModified` / `File.length`; pass
   them as `:expected_mtime` / `:expected_size` on a subsequent
   `patch` / `write` to fail closed if the file changed since the read.
   Each call's `:lines` payload is bounded by `max-cat-window-bytes`; that
   is also the persistence-blob ceiling (one Nippy row per call).
   Streaming: never slurps the whole file. Lines outside the window are
   discarded after a single `.readLine` pass."
  ([path] (read-file path 1 default-cat-limit))
  ([path n] (read-file path 1 n))
  ([path offset n]
   (validate-cat-args! offset n)
   (let [f        (ensure-existing-file! (safe-path path))
         byte-cap (long max-cat-window-bytes)
         skip     (dec (long offset))
         limit    (long n)
         mtime    (.lastModified f)
         size     (.length f)]
     (with-open [^java.io.BufferedReader rdr (io/reader f)]
       (loop [skipped 0]
         (when (and (< skipped skip)
                 (some? (.readLine rdr)))
           (recur (inc skipped))))
       (loop [acc        (transient [])
              bytes-used 0
              read-count 0
              stop       nil]
         (cond
           stop
           (let [lines       (persistent! acc)
                 returned    (count lines)
                 eof?        (= stop :eof)
                 next-offset (when-not eof?
                               (+ (long offset) returned))]
             {:path        (rel-path f)
              :lines       lines
              :hashes      (patch/lines->hashes lines)
              :next-offset next-offset
              :eof?        eof?
              :truncated?  (= stop :bytes)
              :mtime       mtime
              :size        size})

           (>= read-count limit)
           (recur acc bytes-used read-count :limit)

           :else
           (let [raw (.readLine rdr)]
             (if (nil? raw)
               (recur acc bytes-used read-count :eof)
               (let [line-bytes (+ 1 (alength (.getBytes raw "UTF-8")))
                     new-bytes  (+ bytes-used line-bytes)
                     line-no    (+ (long offset) read-count)]
                 (if (and (pos? read-count) (> new-bytes byte-cap))
                   (recur acc bytes-used read-count :bytes)
                   (recur (conj! acc [line-no raw])
                     new-bytes
                     (inc read-count)
                     nil)))))))))))

(defn- hash-read-error-message
  "Human message for a `patch/resolve-hash-range` `:error` on the cat READ
   path — mirrors the patch hash-error copy and always points back to a
   fresh read for current `:hashes`."
  [{:keys [reason which hash lines from-line to-line stated-line found-lines]}]
  (case reason
    :hash-not-found
    (str "cat hash failed: " (name which) "-hash " (pr-str hash)
      " matches no line (the line changed or the file moved)."
      " Re-read with cat(path) or cat(path, {\"tail\": N}) for fresh `lineno:hash` anchors.")
    :hash-misplaced
    (str "cat hash failed: " (name which) "-hash " (pr-str hash)
      " says line " stated-line " but that content is at line(s) " (pr-str found-lines)
      " — stale/misattributed anchor. Re-read with cat(path) for fresh `lineno:hash` anchors.")
    :hash-ambiguous
    (str "cat hash failed: " (name which) "-hash " (pr-str hash)
      " matches " (count lines) " lines " (pr-str lines)
      " — a dup-line collision near that line. Use cat(path, {\"range\": [start, end]}) instead.")
    :hash-range-inverted
    (str "cat hash failed: to_hash line " to-line
      " precedes from_hash line " from-line ".")
    (str "cat :hash failed: " (pr-str reason))))

(defn- read-file-by-hash
  "Read the inclusive window between the lines hashed `from_hash`..`to_hash`
   (`to_hash` defaults to `from_hash` — a single line). Resolves the hashes
   against LIVE file content via `patch/resolve-hash-range`, so the read
   addresses lines BY CONTENT, not by drifting line numbers — the symmetric
   counterpart of `patch :from_hash`. Returns the same shape as `read-file`
   plus `:range [from-line to-line]`. Throws ex-info on a missing / ambiguous /
   inverted hash; the message points back to a fresh read."
  [path from_hash to_hash]
  (let [f       (ensure-existing-file! (safe-path path))
        content (slurp f)
        res     (patch/resolve-hash-range content (str from_hash)
                  (when to_hash (str to_hash)))]
    (if-let [err (:error res)]
      (throw (ex-info (hash-read-error-message err)
               (merge {:type :ext.foundation.editing/invalid-cat-args} err)))
      (let [{:keys [from-line to-line]} res
            n (inc (- (long to-line) (long from-line)))]
        (let [out (read-file path from-line n)]
          (assoc out :range [from-line to-line]
            :hashes (patch/lines->hashes (:lines out))))))))

(defn- read-file-ranges
  "Read several inclusive 1-based line ranges from one file. Result keeps both
   a flat `:lines` view for simple model filtering and per-range windows for
   channel display / diagnostics."
  [path ranges]
  (let [pairs   (normalize-cat-ranges ranges)
        windows (mapv (fn [[start end]]
                        (let [n   (inc (- end start))
                              out (read-file path start n)]
                          (assoc (select-keys out [:lines :next-offset :eof? :truncated?])
                            :range [start end])))
                  pairs)
        f       (ensure-existing-file! (safe-path path))]
    {:path        (rel-path f)
     :lines       (vec (mapcat :lines windows))
     :hashes      (patch/lines->hashes (vec (mapcat :lines windows)))
     :ranges      windows
     :next-offset nil
     :eof?        (every? :eof? windows)
     :truncated?  (boolean (some :truncated? windows))
     :mtime       (.lastModified f)
     :size        (.length f)}))

(defn- tail-file
  "Read the last n lines of a text file. Streams once via a fixed-size
   ring buffer (`java.util.ArrayDeque`), so memory stays bounded even for
   gigantic logs. After the scan, walks the kept window from the END to
   the start to honour `max-cat-window-bytes` — tail = most recent, so the
   byte cap fires by dropping older lines, not newer ones.

   Returns the same shape as `read-file`:
     {:path :lines [[N text]…] :next-offset nil :truncated? B}
   `:next-offset` is always nil — tail is a terminal request. `:truncated?`
   is true only when the byte cap dropped lines that would otherwise have
   fit inside the requested n; trimming older lines beyond n is the
   requested behaviour, not a truncation event."
  [path n]
  (when-not (pos-int? n)
    (throw (ex-info "tail n must be a positive integer"
             {:type :ext.foundation.editing/invalid-cat-args :limit n})))
  (let [n        (long n)
        f        (ensure-existing-file! (safe-path path))
        byte-cap (long max-cat-window-bytes)
        buf      (java.util.ArrayDeque.)
        mtime    (.lastModified f)
        size     (.length f)]
    (with-open [^java.io.BufferedReader rdr (io/reader f)]
      (loop [total 0]
        (let [raw (.readLine rdr)]
          (if (nil? raw)
            (let [kept     (vec (.toArray buf))
                  kept-cnt (count kept)
                  start    (inc (- (long total) kept-cnt))
                  ;; Walk kept from the END backwards, accumulating
                  ;; until the byte cap. Anything dropped off the front
                  ;; bumps `:truncated?`. Per-line text is verbatim —
                  ;; there is no per-line cap (see the
                  ;; `default-cat-limit` / `max-cat-window-bytes`
                  ;; header note up-file); a single pathological long
                  ;; line is included whole and the byte cap stops
                  ;; further accumulation.
                  [final-lines bytes-truncated?]
                  (loop [i          (dec kept-cnt)
                         bytes-used 0
                         acc        ()]
                    (if (neg? i)
                      [(vec acc) false]
                      (let [^String s (nth kept i)
                            lb        (+ 1 (alength (.getBytes s "UTF-8")))
                            nb        (+ bytes-used lb)]
                        (if (and (seq acc) (> nb byte-cap))
                          [(vec acc) true]
                          (recur (dec i) nb (cons s acc))))))
                  start-line   (+ (long start) (- kept-cnt (count final-lines)))
                  numbered     (mapv vector (iterate inc start-line) final-lines)]
              {:path        (rel-path f)
               :lines       numbered
               :hashes      (patch/lines->hashes numbered)
               :next-offset nil
               :eof?        true
               :truncated?  bytes-truncated?
               :mtime       mtime
               :size        size})
            (do
              (when (>= (.size buf) n) (.removeFirst buf))
              (.addLast buf raw)
              (recur (inc total)))))))))

;; =============================================================================
;; ls
;; =============================================================================

(defn- visible-children [^File f {:keys [is_hidden is_respect_gitignore ignore-node root]}]
  (when (.isDirectory f)
    (let [kids (->> (.listFiles f)
                 (remove (fn [^File c]
                           (and (not is_hidden) (.isHidden c))))
                 (remove (fn [^File c]
                           (and is_respect_gitignore
                             (ignored? ignore-node c root)))))]
      (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %)) kids))))

;; Flat-listing helper. The earlier nested `:children` shape made the
;; model walk the tree client-side (`tree-seq map? :children`) for the
;; most common questions (find a file, count files, filter by ext). Flat
;; output sidesteps that — every entry is a top-level row the model can
;; filter directly.
(defn- flat-entry
  "Project a `File` into the flat-list row shape used by ls.
   `:path` is workspace-relative; trailing `/` on dir paths is left to
   the renderer so the data shape stays uniform."
  [^File f]
  {:path (rel-path f)
   :type (if (.isDirectory f) :dir :file)
   :size (if (.isDirectory f) nil (.length f))})

(defn- collect-flat-entries
  "BFS-like (actually pre-order DFS) walk under `root` up to `max-depth`.
   Returns `{:entries [...] :truncated? B}`. Respects `:is_hidden` /
   `:is_respect_gitignore` like before. `:is_files_only` and `:is_dirs_only`
   are post-filters applied per entry (root is never emitted). Stops at
   `max-limit` entries and marks `:truncated? true`."
  [^File root opts* max-depth max-limit is_files_only is_dirs_only]
  (let [acc        (volatile! (transient []))
        truncated? (volatile! false)
        keep?      (fn [^File f]
                     (cond is_files_only (.isFile f)
                       is_dirs_only  (.isDirectory f)
                       :else       true))
        walk (fn walk [^File f cur-depth]
               (when-not @truncated?
                 (when (and (not= f root) (keep? f))
                   (vswap! acc conj! (flat-entry f))
                   (when (>= (count @acc) max-limit)
                     (vreset! truncated? true)))
                 ;; Descend strictly by depth budget. `:depth 0` means
                 ;; \"no descent at all\" — root's children aren't visited.
                 ;; `:depth 1` visits root's immediate children only.
                 (when (and (.isDirectory f)
                         (< cur-depth max-depth)
                         (not @truncated?))
                   (doseq [^File child (visible-children f opts*)
                           :while (not @truncated?)]
                     (walk child (inc cur-depth))))))]
    (walk root 0)
    {:entries (persistent! @acc)
     :truncated? @truncated?}))

(defn- entry-parent-dir
  "Workspace-relative parent directory of a flat entry path, or `root-rel`
   when the path has no slash (a direct child of the listed root)."
  [^String path root-rel]
  (let [i (str/last-index-of path "/")]
    (if i (subs path 0 i) root-rel)))

(defn- entry-base-name
  [^String path]
  (let [i (str/last-index-of path "/")]
    (if i (subs path (inc i)) path)))

(defn- group-entries-by-dir
  "Fold the pre-order flat `entries` into ONE group per directory, in
   first-seen (pre-order) order:
     [{:dir \"bin\" :files [{:name \"dev\" :size 2308} ...]} ...]

   The dir path is stated ONCE per group instead of repeating its full
   prefix on every file — that prefix duplication is the whole reason a
   flat ls bloats context. Every directory the walk covered appears as a
   group header (so the tree stays visible even for dirs that hold only
   subdirs); `root-rel` is the listed root and always leads. Files carry
   the raw `:size` int (nil for the implicit dir rows); display formats it."
  [entries root-rel]
  (let [order   (volatile! [])
        seen    (volatile! #{})
        files   (volatile! {})
        ensure! (fn [d]
                  (when-not (contains? @seen d)
                    (vswap! seen conj d)
                    (vswap! order conj d)
                    (vswap! files assoc d (transient []))))]
    (ensure! root-rel)
    (doseq [{:keys [path type size]} entries]
      (if (= type :dir)
        (ensure! path)
        (let [d (entry-parent-dir path root-rel)]
          (ensure! d)
          (vswap! files update d conj! {:name (entry-base-name path) :size size}))))
    (mapv (fn [d] {:dir d :files (persistent! (get @files d))}) @order)))

(defn- human-size
  "Compact human-readable byte count for DISPLAY only (the structured
   `:size` stays a raw int). nil → nil. Locale-ROOT formatting so the
   decimal separator is always `.` (never a locale comma like `2,3k`)."
  [n]
  (cond
    (nil? n)               nil
    (< (long n) 1024)      (str n)
    (< (long n) 1048576)   (String/format java.util.Locale/ROOT "%.1fk" (object-array [(/ (double n) 1024.0)]))
    :else                  (String/format java.util.Locale/ROOT "%.1fM" (object-array [(/ (double n) 1048576.0)]))))

(defn- list-files
  ;; Internal helper — always called with a real map (or nil).
  ;; The dual kwargs/map calling convention lives on the public
  ;; `ls-tool` wrapper; this stays positional to keep nil-forwarding
  ;; trivial.
  ([path] (list-files path nil))
  ([path opts]
   (let [{:keys [depth limit is_hidden is_respect_gitignore is_files_only is_dirs_only]
          :or {depth default-list-depth
               limit default-list-limit
               is_hidden false
               is_respect_gitignore true
               is_files_only false
               is_dirs_only  false}} (or opts {})
         _ (when (and is_files_only is_dirs_only)
             (throw (ex-info "ls :is_files_only and :is_dirs_only are mutually exclusive"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :opts opts})))
         _ (when-not (and (integer? depth) (not (neg? depth)))
             (throw (ex-info "ls :depth must be a non-negative integer"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :depth depth})))
         _ (when-not (and (integer? limit) (pos? limit))
             (throw (ex-info "ls :limit must be a positive integer"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :limit limit})))
         f (safe-path path)
         _ (when-not (.exists f)
             (throw (ex-info (str "Path not found: " (.getPath f))
                      {:type :ext.foundation.editing/path-not-found})))
         opts* {:is_hidden is_hidden
                :is_respect_gitignore is_respect_gitignore
                :ignore-node (when is_respect_gitignore (load-ignore-node f))
                :root f}
         {:keys [entries truncated?]}
         (collect-flat-entries f opts* depth limit is_files_only is_dirs_only)
         file-count (count (filter #(= :file (:type %)) entries))
         dir-count  (count (filter #(= :dir  (:type %)) entries))]
     {:path          (rel-path f)
      :absolute-path (.getAbsolutePath f)
      :root-type     (if (.isDirectory f) :dir :file)
      :groups        (group-entries-by-dir entries (rel-path f))
      :entry-count   (count entries)
      :file-count    file-count
      :dir-count     dir-count
      :truncated?    truncated?
      :depth         depth
      :limit         limit})))

;; =============================================================================
;; rg
;; =============================================================================

(defn- compile-needles
  "Pre-compile needles when `:is_regex true`; nil otherwise (literal mode).
   Bad patterns surface as a structured invalid-rg-spec error."
  [needles is_regex]
  (when is_regex
    (mapv (fn [n]
            (try (java.util.regex.Pattern/compile n)
              (catch java.util.regex.PatternSyntaxException e
                (throw (ex-info (str "rg :is_regex true — invalid regex pattern: "
                                  (pr-str n))
                         {:type :ext.foundation.editing/invalid-rg-spec
                          :pattern n
                          :reason (ex-message e)})))))
      needles)))

(defn- coerce-rg-spec
  "Coerce the single public rg spec map. Exactly one of :all or :any
   is required. Every public collection field is a vector; a bare string
   is also accepted and coerced to a 1-element vector (ripgrep/grep
   muscle-memory like a scalar :glob or :any). Accepts compatibility alias :files for :paths,
   but docs/prompt keep advertising canonical :paths only.

   Optional keys (all default to off):
     :limit       max hits / files / count entries (default 250)
     :context N   shorthand for :before N + :after N
     :before N    lines of context BEFORE each hit (content mode only)
     :after  N    lines of context AFTER  each hit (content mode only)
     :is_files_only true → return only distinct paths (no per-line hits)
     :is_counts     true → return per-file match counts (no per-line hits)
     :is_regex      true → interpret needles as java.util.regex patterns;
                  literal substring otherwise (default)."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "rg takes one spec map: {:all [...] :paths [...]}."
             {:type :ext.foundation.editing/invalid-rg-spec
              :got  (type spec)})))
  (let [allowed-keys #{:all :any :paths :path :files :include :glob :exclude :is_hidden :is_respect_gitignore
                       :limit :context :before :after :is_files_only :is_counts :is_regex}
        unknown-keys (seq (remove allowed-keys (keys spec)))
        _ (when unknown-keys
            (throw (ex-info "rg spec has unknown keys."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :unknown (vec unknown-keys)})))
        has-all? (contains? spec :all)
        has-any? (contains? spec :any)
        _ (when (= has-all? has-any?)
            (throw (ex-info "rg spec must use exactly one of :all or :any."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        vector-of-strings (fn [k default]
                            (let [raw (if (contains? spec k) (get spec k) default)
                                  ;; scalar-tolerant: a bare string coerces to a 1-vec, so
                                  ;; :glob "x" / :any "x" (ripgrep & grep muscle-memory) just
                                  ;; works instead of throwing. Vectors pass through unchanged.
                                  v   (if (string? raw) [raw] raw)]
                              (when-not (and (vector? v) (seq v) (every? string? v))
                                (throw (ex-info "rg spec fields must be non-empty vectors of strings."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              (when-not (every? #(not (str/blank? %)) v)
                                (throw (ex-info "rg spec string values must be non-blank."
                                         {:type :ext.foundation.editing/invalid-rg-spec
                                          :field k
                                          :got v})))
                              v))
        ;; :path is an undocumented singular alias for :paths (same muscle-
        ;; memory treatment as :glob for :include). Kept out of the docstring
        ;; / prompt on purpose; canonical stays :paths.
        _ (when (< 1 (count (filter #(contains? spec %) [:paths :files :path])))
            (throw (ex-info "rg spec must use only one of canonical :paths or its aliases (:files / :path)."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        path-key (cond (contains? spec :files) :files
                   (contains? spec :path)  :path
                   :else                   :paths)
        op (if has-all? :all :any)
        needles (vector-of-strings op nil)
        paths (vector-of-strings path-key ["."])
        ;; :glob is an undocumented ripgrep-muscle-memory alias for
        ;; :include. Kept out of the docstring on purpose; negation
        ;; (!pat) still goes through :exclude.
        _ (when (and (contains? spec :include) (contains? spec :glob))
            (throw (ex-info "rg spec must use only one of :include or :glob."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        include-key (cond (contains? spec :include) :include
                      (contains? spec :glob)    :glob
                      :else                     nil)
        include-raw (when include-key
                      (vector-of-strings include-key nil))
        exclude-raw (when (contains? spec :exclude)
                      (vector-of-strings :exclude nil))
        ;; ripgrep muscle-memory: a leading ! on an include/glob
        ;; pattern is a negation -> peel it into :exclude (sans !).
        ;; Kept undocumented alongside :glob; canonical path stays
        ;; :include / :exclude.
        bang? (fn [s] (str/starts-with? s "!"))
        include (filterv (complement bang?) (or include-raw []))
        exclude (into (vec (or exclude-raw []))
                  (comp (filter bang?) (map #(subs % 1)))
                  (or include-raw []))
        nonneg-int! (fn [k]
                      (when (contains? spec k)
                        (let [v (get spec k)]
                          (when-not (and (integer? v) (not (neg? v)))
                            (throw (ex-info (str "rg :" (name k) " must be a non-negative integer")
                                     {:type :ext.foundation.editing/invalid-rg-spec
                                      :field k :got v}))))))
        _ (run! nonneg-int! [:limit :context :before :after])
        limit-spec (:limit spec)
        _ (when (and limit-spec (not (pos? limit-spec)))
            (throw (ex-info "rg :limit must be a positive integer"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :field :limit :got limit-spec})))
        limit (or limit-spec default-grep-limit)
        context-shared (or (:context spec) 0)
        before-ctx (or (:before spec) context-shared)
        after-ctx  (or (:after  spec) context-shared)
        is_files_only (boolean (:is_files_only spec))
        is_counts     (boolean (:is_counts spec))
        _ (when (and is_files_only is_counts)
            (throw (ex-info "rg :is_files_only and :is_counts are mutually exclusive"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        _ (when (and (or is_files_only is_counts)
                  (or (pos? before-ctx) (pos? after-ctx)))
            (throw (ex-info "rg :before / :after / :context only apply to content mode (not :is_files_only / :is_counts)"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        is_regex (boolean (:is_regex spec))
        patterns (compile-needles needles is_regex)]
    {:op op
     :needles needles
     :paths paths
     :include (or include [])
     :exclude (or exclude [])
     :is_hidden (boolean (:is_hidden spec))
     :is_respect_gitignore (get spec :is_respect_gitignore true)
     :limit limit
     :before-ctx before-ctx
     :after-ctx  after-ctx
     :is_files_only is_files_only
     :is_counts is_counts
     :is_regex is_regex
     :patterns patterns}))

(defn- make-line-matcher
  "Return a `(fn [line] boolean)` predicate. Default mode is literal
   substring (`str/includes?`); `:is_regex true` uses pre-compiled
   `java.util.regex.Pattern` objects. `:all` means every needle must
   match the same line; `:any` means at least one."
  [op needles patterns is_regex]
  (cond
    (and (= op :all) is_regex)
    (fn [^String line] (every? #(re-find % line) patterns))

    (and (= op :any) is_regex)
    (fn [^String line] (boolean (some #(re-find % line) patterns)))

    (= op :all)
    (fn [^String line] (every? #(str/includes? line %) needles))

    :else
    (fn [^String line] (boolean (some #(str/includes? line %) needles)))))

;; There is no per-line text cap for rg hits. Such a cap (e.g. the
;; 500-char one Roo Code uses) has the same failure mode as a
;; trailer cap: the `…<+N chars>` marker makes the model
;; perceive its own data as missing and chase a phantom "full line"
;; via extra cat roundtrips, even on normal source lines that
;; happen to brush the cap. The model owns its data — see the
;; trailer truncation note in `internal/ctx_renderer.clj`.
;;
;; Realistic corpus exposure is bounded by:
;;   - the hit cap (`:truncated-by :limit`, default 250 hits)
;;   - the model's choice to use `:is_files_only` / `:is_counts` /
;;     `:exclude` when scanning minified or wide-line corpora
;; If a `:text` ever needs to be capped again, the cap MUST be
;; explicit at the spec layer (e.g. an opt-in `:max-text-chars`) so
;; the model controls it, NOT a silent renderer-side ellipsis.

(defn- search-file-content
  "Walk one file once, emit hits with optional context. Content-mode helper.
   Returns a vec of hit maps; an empty vec means no match. Hit `:text`,
   `:before`, and `:after` carry source bytes verbatim — no per-line cap
   (see the retirement note above for rationale)."
  [^File f matches? before-ctx after-ctx]
  (try
    (let [path  (rel-path f)
          lines (with-open [r (io/reader f)] (vec (line-seq r)))
          n     (count lines)
          before-ctx (long before-ctx)
          after-ctx  (long after-ctx)
          want-before? (pos? before-ctx)
          want-after?  (pos? after-ctx)]
      (loop [i 0
             out (transient [])]
        (cond
          (>= i n) (persistent! out)
          (matches? (nth lines i))
          (let [line-no (inc i)
                text    (nth lines i)
                hit (cond-> {:path path :line line-no :text text}
                      want-before?
                      (assoc :before
                        (mapv (fn [j] [(inc j) (nth lines j)])
                          (range (max 0 (- i before-ctx)) i)))
                      want-after?
                      (assoc :after
                        (mapv (fn [j] [(inc j) (nth lines j)])
                          (range (inc i) (min n (+ i after-ctx 1))))))]
            (recur (inc i) (conj! out hit)))
          :else (recur (inc i) out))))
    (catch Throwable _ [])))

(defn- file-has-any-hit?
  "Short-circuit: true on first matching line. Used by :is_files_only mode
   so we exit each file as fast as possible."
  [^File f matches?]
  (try
    (with-open [r (io/reader f)]
      (boolean (some matches? (line-seq r))))
    (catch Throwable _ false)))

(defn- count-hits-in-file
  "Total matching lines in `f`. Used by :is_counts mode — returns the
   real count regardless of the global :limit (since the limit caps
   FILE entries, not per-file lines)."
  [^File f matches?]
  (try
    (with-open [r (io/reader f)]
      (count (filter matches? (line-seq r))))
    (catch Throwable _ 0)))

(defn- rg-search
  "The rg search ENGINE: takes the public rg spec map and does the
   actual file scanning. The public `rg-tool` (= `rg`) wraps this with
   arity/kwargs handling + the LLM-facing result envelope. Three output
   modes, picked by
   `:is_files_only` / `:is_counts` / (default content).

   Returns one of:
     {:hits   [{:path :line :text :hash :before? :after?} ...] :truncated-by KW}  ;; content
   `:hash` is the `<lineno>:<hash>` anchor for that line — the same one `cat`
   emits in `:hashes` — so a hit is directly patchable via `{:from_hash <anchor>}`
   without a follow-up `cat`. Absent on blank lines.
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW}  ;; files-only
     {:counts [{:path P :count N} ...]                    :truncated-by KW}  ;; counts

   `:truncated-by` is `:limit` when the configured limit clamped the
   result, `:end-of-results` otherwise. Per-line `:text`, `:before`,
   and `:after` are verbatim — no per-line cap (see the per-line cap
   retirement note above)."
  [spec]
  (let [{:keys [op needles patterns paths include exclude is_hidden is_respect_gitignore
                limit before-ctx after-ctx is_files_only is_counts is_regex]} (coerce-rg-spec spec)
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
        matches? (make-line-matcher op needles patterns is_regex)
        walk (fn walk [ignore-node root ^File f]
               (cond
                 (and (not is_hidden) (.isHidden f)) []
                 (and is_respect_gitignore (ignored? ignore-node f root)) []
                 (.isDirectory f) (mapcat #(walk ignore-node root %)
                                    (or (.listFiles f) (into-array File [])))
                 (and (.isFile f) (include-file? f)) [f]
                 :else []))
        files (->> roots
                (mapcat (fn [root]
                          (let [ignore-node (when is_respect_gitignore
                                              (load-ignore-node root))]
                            (walk ignore-node root root))))
                (sort-by rel-path))]
    (cond
      is_files_only
      (let [out (atom [])
            capped? (atom false)]
        (doseq [^File f files :while (not @capped?)]
          (when (file-has-any-hit? f matches?)
            (swap! out conj (rel-path f))
            (when (>= (count @out) limit) (reset! capped? true))))
        {:files (vec @out)
         :truncated-by (if @capped? :limit :end-of-results)})

      is_counts
      (let [out (atom [])
            capped? (atom false)]
        (doseq [^File f files :while (not @capped?)]
          (let [c (count-hits-in-file f matches?)]
            (when (pos? c)
              (swap! out conj {:path (rel-path f) :count c})
              (when (>= (count @out) limit) (reset! capped? true)))))
        {:counts (vec @out)
         :truncated-by (if @capped? :limit :end-of-results)})

      :else
      (let [out (atom [])
            capped? (atom false)]
        (doseq [^File f files :while (not @capped?)]
          (let [hits (search-file-content f matches? before-ctx after-ctx)]
            (when (seq hits)
              ;; Attach the `lineno:hash` anchor to each hit so the model can
              ;; patch STRAIGHT from an rg result — the same addressing `cat`
              ;; emits in `:hashes`. The hit already carries :line and :text, so
              ;; this is a per-hit compute — no whole-file rehash (the old
              ;; file-wide-ordinal scheme that forced one is gone). Blank lines
              ;; carry no anchor.
              (doseq [hit hits :while (not @capped?)]
                (swap! out conj (cond-> hit
                                  (not (str/blank? (:text hit)))
                                  (assoc :hash (patch/line-anchor (:line hit) (:text hit)))))
                (when (>= (count @out) limit) (reset! capped? true))))))
        {:hits (vec @out)
         :truncated-by (if @capped? :limit :end-of-results)}))))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{:path :replace})
(def ^:private patch-locator-keys
  "Every edit needs EXACTLY ONE locator: `:search` (text match) or
   `:from_hash` (hashline — content-addressed by the per-line hash from
   `cat`). The hashline form re-resolves against LIVE content on every
   edit, so it stays correct under line drift (insertions above, or
   earlier edits in the same grouped batch) where raw line numbers would
   silently target the wrong line."
  #{:search :from_hash})
(def ^:private patch-optional-keys
  "Optional keys recognised on exact-replace edit maps.
   - :after / :before  positional anchors (string; first exact occurrence) [:search only]
   - :nth              :first | :last | :all | 1-based positive integer    [:search only]
   - :to_hash          end of a hashline range; defaults to :from_hash (single line)
   - :expected_mtime   epoch-ms; fail if file mtime differs (staleness guard)
   - :expected_size    bytes;    fail if file size differs (staleness guard)"
  #{:after :before :nth :to_hash :expected_mtime :expected_size})

(def ^:private patch-allowed-keys
  (set/union patch-required-keys patch-locator-keys patch-optional-keys))

(def ^:private patch-group-required-keys #{:path :edits})
(def ^:private patch-group-optional-keys #{:expected_mtime :expected_size})
(def ^:private patch-group-allowed-keys
  (set/union patch-group-required-keys patch-group-optional-keys))

(defn- grouped-patch-edit?
  [edit]
  (and (map? edit) (contains? edit :edits)))

(defn- expand-patch-group
  [{:keys [path edits expected_mtime expected_size] :as group}]
  (let [missing (seq (remove #(contains? group %) patch-group-required-keys))
        unknown (seq (remove patch-group-allowed-keys (keys group)))]
    (when missing
      (throw (ex-info "patch grouped edit missing required keys"
               {:type :ext.foundation.editing/invalid-patch-edit-group
                :missing (vec missing)
                :edit group})))
    (when unknown
      (throw (ex-info "patch grouped edit has unknown keys"
               {:type :ext.foundation.editing/invalid-patch-edit-group
                :unknown (vec unknown)
                :allowed (vec patch-group-allowed-keys)
                :edit group})))
    (when-not (sequential? edits)
      (throw (ex-info "patch grouped :edits must be a vector/seq of edit maps"
               {:type :ext.foundation.editing/invalid-patch-edit-group
                :edits edits})))
    (when (empty? edits)
      (throw (ex-info "patch grouped :edits must not be empty"
               {:type :ext.foundation.editing/invalid-patch-edit-group
                :edit group})))
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "patch grouped :edits entries must be maps"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (when (contains? edit :path)
              (throw (ex-info "patch grouped :edits inherit :path; do not repeat it per edit"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (cond-> (assoc edit :path path)
              (some? expected_mtime) (assoc :expected_mtime expected_mtime)
              (some? expected_size) (assoc :expected_size expected_size)))
      edits)))

(defn- normalize-patch-edits-input
  [edits]
  (let [edits (if (map? edits) [edits] edits)]
    (when-not (sequential? edits)
      (throw (ex-info "patch expects a map, grouped map, or vector of edit maps/groups"
               {:type :ext.foundation.editing/invalid-patch-edits
                :got  (type edits)})))
    (mapcat (fn [edit]
              (if (grouped-patch-edit? edit)
                (expand-patch-group edit)
                [edit]))
      edits)))

(defn- coerce-patch-edits
  [edits]
  (let [edits (normalize-patch-edits-input edits)]
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "patch edit must be a map"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))
                  locators (filter #(contains? edit %) patch-locator-keys)
                  unknown (seq (remove patch-allowed-keys (keys edit)))]
              (when missing
                (throw (ex-info "patch edit missing required keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit})))
              (when (not= 1 (count locators))
                (throw (ex-info "patch edit needs EXACTLY ONE of :search or :from_hash"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :locators (vec locators)
                          :edit edit})))
              (when (and (:to_hash edit) (not (:from_hash edit)))
                (throw (ex-info "patch :to_hash requires :from_hash"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :edit edit})))
              (when unknown
                (throw (ex-info "patch edit has unknown keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :unknown (vec unknown)
                          :allowed (vec patch-allowed-keys)
                          :edit edit}))))
            (let [nth-spec (:nth edit)]
              (when (and (some? nth-spec)
                      (not (or (#{:first :last :all} nth-spec)
                             (and (integer? nth-spec) (pos? nth-spec)))))
                (throw (ex-info "patch :nth must be :first, :last, :all or a positive integer"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :nth nth-spec :edit edit}))))
            (update edit :path str))
      edits)))

(defn- find-substring-positions
  "All 0-based char positions where `needle` appears in `haystack`.
   Throws on blank needle (would yield infinite matches)."
  [^String haystack ^String needle]
  (when (str/blank? needle)
    (throw (ex-info "patch :search must be non-blank"
             {:type :ext.foundation.editing/invalid-patch-search})))
  (loop [idx 0 acc []]
    (let [hit (str/index-of haystack needle idx)]
      (if (nil? hit)
        acc
        (recur (+ (long hit) (count needle)) (conj acc (long hit)))))))

(defn- filter-positions-by-anchors
  "Drop positions that fall outside `:after` / `:before` boundaries.
   `:after S` requires the match to start at-or-after the END of the first
   exact occurrence of S. `:before S` requires the match's end to land
   at-or-before the START of the first occurrence of S. Either anchor
   missing in the file fails the whole edit."
  [positions search-len ^String content {:keys [after before]}]
  (let [after-pos (when after (str/index-of content (str after)))
        before-pos (when before (str/index-of content (str before)))
        anchor-error (cond
                       (and after (nil? after-pos))   {:anchor :after  :value after}
                       (and before (nil? before-pos)) {:anchor :before :value before})]
    (if anchor-error
      {:anchor-error anchor-error}
      (let [floor   (if after  (+ (long after-pos) (count (str after)))  0)
            ceiling (if before (long before-pos) Long/MAX_VALUE)]
        {:positions (vec (filter (fn [^long p]
                                   (and (>= p floor)
                                     (<= (+ p (long search-len)) ceiling)))
                           positions))}))))

(defn- select-by-nth
  "Pick concrete positions to mutate given `:nth` semantics.
   nil / :first -> [first] when any; :last -> [last]; :all -> every position;
   1-based integer -> exactly that one or nil when out of range."
  [positions nth-spec]
  (let [positions (vec positions)
        n (count positions)]
    (cond
      (zero? n) nil
      (= :all nth-spec) positions
      (= :last nth-spec) [(peek positions)]
      (or (nil? nth-spec) (= :first nth-spec)) [(first positions)]
      (and (integer? nth-spec) (pos? nth-spec) (<= nth-spec n))
      [(nth positions (dec nth-spec))]
      :else nil)))

(defn- apply-substring-replacements
  "Splice `replace-text` into `content` at each `position`. Process from
   END to START so earlier offsets stay valid."
  [^String content positions search-len ^String replace-text]
  (loop [^String out content
         remaining (sort > positions)]
    (if-let [pos (first remaining)]
      (recur (str (subs out 0 pos)
               replace-text
               (subs out (+ (long pos) (long search-len))))
        (next remaining))
      out)))

(defn- compute-window-indent-delta
  "Even when a non-`:relative-indent` pass succeeded (e.g. `:trim`), the
   matched window may sit at a different absolute indent than the SEARCH
   block. Compute the delta so the `:replace` payload can be re-indented
   uniformly. Returns 0 when either side has no non-blank line."
  ^long [content-lines pattern line-start]
  (let [window (subvec content-lines line-start (+ (long line-start) (count pattern)))
        p-indent (#'patch/min-leading-indent pattern)
        w-indent (#'patch/min-leading-indent window)]
    (if (and p-indent w-indent)
      (- (long w-indent) (long p-indent))
      0)))

(defn- fuzzy-line-match
  "Line-based fuzzy fallback used only when exact substring search returns
   zero hits AND `:search` spans multiple lines. Runs the 5-pass matcher
   (exact -> rstrip -> trim -> unicode -> relative-indent) and converts
   the line hit back to char offsets. Returns `{:char-start :char-end
   :pass :indent-delta :line-start :line-end}` or nil.

   Indent-delta is computed for EVERY non-exact pass, not just
   `:relative-indent` — a `:trim` hit on a pattern authored at a
   different indentation still needs `:replace` shifted to match the
   file's actual indent."
  [^String content ^String search]
  (let [content-lines (patch/split-content-lines content)
        search-lines  (patch/split-content-lines search)]
    (when (and (>= (count search-lines) 2)
            (seq content-lines))
      (when-let [{:keys [start pass indent-delta]}
                 (patch/seek-sequence-with-pass content-lines search-lines 0 false)]
        (when (not= :exact pass)
          (let [line-end   (+ (long start) (count search-lines))
                char-start (patch/char-offset-at-line content start)
                char-end-raw (patch/char-offset-at-line content line-end)
                ;; If the matched window does NOT touch EOF and ended at a
                ;; newline (char-offset-at-line of a non-final line is one
                ;; past the preceding `\n`), keep the trailing newline
                ;; OUTSIDE the replaced region so the user's `:replace`
                ;; doesn't need to know about it.
                char-end   (if (and (< char-end-raw (count content))
                                 (pos? char-end-raw)
                                 (= \newline (.charAt content (dec char-end-raw))))
                             (dec char-end-raw)
                             char-end-raw)
                delta (or indent-delta
                        (compute-window-indent-delta content-lines search-lines start))]
            {:char-start char-start
             :char-end   char-end
             :pass       pass
             :indent-delta delta
             :line-start start
             :line-end   line-end}))))))

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping a trailing `\n` OUTSIDE the
   replaced region (mirrors `fuzzy-line-match`)."
  [^String content ^long line-start ^long line-end]
  (let [char-start   (patch/char-offset-at-line content line-start)
        char-end-raw (patch/char-offset-at-line content line-end)
        char-end     (if (and (< char-end-raw (count content))
                           (pos? char-end-raw)
                           (= \newline (.charAt content (dec char-end-raw))))
                       (dec char-end-raw)
                       char-end-raw)]
    [char-start char-end]))

(defn- ws-agnostic-match
  "Whitespace-agnostic fallback used only when exact substring search AND
   the line-structured `fuzzy-line-match` both return nothing. Folds both
   sides to a whitespace-free token stream (see `patch/ws-agnostic-line-span`)
   so a SEARCH block whose line breaks / indentation drifted from the file
   still locates. Applies ONLY when the token subsequence is unique
   (`:occurrences` = 1) — ambiguous hits fall through to no-match so the
   model picks an anchor. Returns the same shape as `fuzzy-line-match`."
  [^String content ^String search]
  (when-let [{:keys [line-start line-end occurrences]}
             (patch/ws-agnostic-line-span content search)]
    (when (= 1 occurrences)
      (let [[char-start char-end] (line-span->char-span content line-start line-end)
            window (subvec (patch/split-content-lines content) line-start line-end)
            delta  (compute-window-indent-delta
                     (patch/split-content-lines content)
                     (patch/split-content-lines search)
                     line-start)]
        ;; indent-delta only meaningful when search/window line counts match;
        ;; ws-agnostic can re-segment, so guard on equal length.
        {:char-start   char-start
         :char-end     char-end
         :pass         :ws-agnostic
         :indent-delta (if (= (count window) (count (patch/split-content-lines search)))
                         delta 0)
         :line-start   line-start
         :line-end     line-end}))))

(defn- adjust-replace-for-indent
  "For `:relative-indent` fuzzy hits, re-indent the user's `:replace`
   lines so they sit at the file's actual indentation rather than the
   indentation the SEARCH block was authored at. No-op for other passes."
  [^String replace-text ^long indent-delta]
  (if (zero? indent-delta)
    replace-text
    (let [lines (patch/split-content-lines replace-text)
          adjusted (patch/apply-indent-delta indent-delta lines)
          trailing-nl? (str/ends-with? replace-text "\n")]
      (str (str/join "\n" adjusted) (when trailing-nl? "\n")))))

(def ^:private patch-search-preview-chars 180)

(defn- search-preview
  [s]
  (let [s (str s)]
    (if (<= (count s) patch-search-preview-chars)
      s
      (str (subs s 0 patch-search-preview-chars)
        "...<+" (- (count s) patch-search-preview-chars) " chars>"))))

(defn- first-line-diff
  "First differing line between SEARCH and the file window it landed on,
   compared trim-insensitively. Returns `{:search S :file F :at N}` (N is
   the 1-based offset within the window) or nil when every line matches
   under trim (i.e. the only drift was whitespace/segmentation)."
  [search-lines window-lines]
  (loop [i 0]
    (cond
      (and (>= i (count search-lines)) (>= i (count window-lines))) nil
      (or (>= i (count search-lines)) (>= i (count window-lines)))
      {:at (inc i)
       :search (when (< i (count search-lines)) (nth search-lines i))
       :file   (when (< i (count window-lines)) (nth window-lines i))}
      (= (str/trim (nth search-lines i)) (str/trim (nth window-lines i)))
      (recur (inc i))
      :else {:at (inc i) :search (nth search-lines i) :file (nth window-lines i)})))

(defn- best-similarity-window
  "Slide a `(count search-lines)`-high window over `lines` and pick the
   window with the most trim-equal lines. Returns `{:line-start :score}`
   (0-based) or nil. Used as the last-resort near-miss locator when even
   the ws-agnostic token match cannot place the SEARCH block."
  [lines search-lines]
  (let [plen (count search-lines)
        n    (count lines)
        strim (mapv str/trim search-lines)]
    (when (and (pos? plen) (<= plen n))
      (let [end (- n plen)]
        (loop [i 0 best nil best-score -1]
          (if (> i end)
            (when (and best (pos? best-score)) {:line-start best :score best-score})
            (let [score (->> (range plen)
                          (filter #(= (nth strim %) (str/trim (nth lines (+ i %)))))
                          count)]
              (if (> score best-score)
                (recur (inc i) i score)
                (recur (inc i) best best-score)))))))))

(defn- nearest-match-context
  "For a failed edit, capture a small around-the-hit window so the model
   can see WHERE on disk its `:search` would have landed (or almost
   landed) and WHAT differs. Tries, in order: line-structured fuzzy match,
   the whitespace-agnostic token span, then a best-similarity window scan.
   Returns `{:line N :pass KW :context [[ln text]...] :diff {...}
   :occurrences M?}` or nil. Lines are 1-based; window is ±3 lines."
  [^String content ^String search]
  (let [lines        (patch/split-content-lines content)
        search-lines (patch/split-content-lines search)
        located      (or (when-let [h (fuzzy-line-match content search)]
                           {:line-start (:line-start h)
                            :line-end (:line-end h)
                            :pass (:pass h)
                            :indent-delta (:indent-delta h)})
                       (when-let [s (patch/ws-agnostic-line-span content search)]
                         {:line-start (:line-start s)
                          :line-end (:line-end s)
                          :pass :ws-agnostic
                          :occurrences (:occurrences s)})
                       (when-let [b (best-similarity-window lines search-lines)]
                         {:line-start (:line-start b)
                          :line-end (+ (long (:line-start b)) (count search-lines))
                          :pass :similarity}))]
    (when located
      (let [start-line (long (:line-start located))
            end-line   (long (:line-end located))
            ctx-from   (max 0 (- start-line 3))
            ctx-to     (min (count lines) (+ end-line 3))
            window     (subvec lines start-line (min (count lines) end-line))
            diff       (first-line-diff search-lines window)]
        (cond-> {:line (inc start-line)
                 :pass (:pass located)
                 :context (mapv (fn [i] [(inc i) (nth lines i)])
                            (range ctx-from ctx-to))}
          (:indent-delta located) (assoc :indent-delta (:indent-delta located))
          (:occurrences located)  (assoc :occurrences (:occurrences located))
          diff                    (assoc :diff diff))))))

;; -----------------------------------------------------------------------------
;; Per-path consecutive-failure tracker (Roo-style loop detector)
;;
;; A process-wide atom of `{absolute-path consecutive-fail-count}`. We bump
;; on every failed patch invocation that touched the path and reset to
;; zero when the same path's plan applies cleanly. Once the count crosses
;; `patch-fail-loop-threshold`, the error message escalates with a hard
;; "stop blind retry" hint that nudges the model out of the loop.
;; -----------------------------------------------------------------------------

(def ^:private patch-fail-counts (atom {}))
(def ^:private patch-fail-loop-threshold 3)

(defn- bump-patch-fail-count!
  ^long [^java.io.File file]
  (let [abs (.getAbsolutePath file)]
    (long (get (swap! patch-fail-counts update abs (fnil inc 0)) abs))))

(defn- clear-patch-fail-count!
  [^java.io.File file]
  (let [abs (.getAbsolutePath file)]
    (swap! patch-fail-counts dissoc abs)))

(defn- patch-loop-hint
  [^long n path]
  (when (>= n patch-fail-loop-threshold)
    (str "Consecutive patch failures on " path ": " n
      ". STOP retrying with similar search. Re-read the file (cat(path, {\"tail\": N})"
      " or with the offset shown above), then build ONE cohesive edit plan with"
      " from_hash..to_hash anchors or nth selection. If you are rewriting the"
      " whole file, switch to write(...).")))

;; -----------------------------------------------------------------------------
;; Staleness check: :expected_mtime / :expected_size
;; -----------------------------------------------------------------------------

(defn- staleness-check
  "Return nil when the file's on-disk mtime/size matches the edit's
   expectations (or no expectations were given), else a structured
   `:stale` failure carrying the actual vs. expected values."
  [^java.io.File file {:keys [expected_mtime expected_size]}]
  (let [actual-mtime (.lastModified file)
        actual-size  (.length file)]
    (cond
      (and (some? expected_mtime) (not= (long expected_mtime) actual-mtime))
      {:reason :stale-mtime :expected_mtime expected_mtime :actual-mtime actual-mtime
       :actual-size actual-size}

      (and (some? expected_size) (not= (long expected_size) actual-size))
      {:reason :stale-size :expected_size expected_size :actual-size actual-size
       :actual-mtime actual-mtime})))

;; -----------------------------------------------------------------------------
;; patch-analysis (rewritten)
;;
;; Per-edit pipeline:
;;   1. Coerce/validate edit map (anchors, :nth, mtime/size types).
;;   2. Read current file content (post-state if a prior edit hit the same path).
;;   3. mtime/size guard → :stale failure if mismatched.
;;   4. Exact substring search → vec of char positions.
;;   5. Filter by :after / :before anchors.
;;   6. Select target positions via :nth (:first | :last | :all | int).
;;   7. If no positions selected:
;;        a. zero exact AND search is multi-line → fuzzy line-based fallback
;;           (5 passes incl. relative-indent). On hit, apply with optional
;;           re-indent of `:replace`.
;;        b. otherwise → fail with structured diagnostics.
;;   8. Apply replacement(s) end-to-start, update post-state.
;;
;; All failures populate `:failures` with `:matches`, `:filtered-matches`,
;; `:reason`, optional `:nearest` (fuzzy candidate with ±3 line context),
;; and the original anchors so the surfaced ex-message stays actionable.
;; -----------------------------------------------------------------------------

(defn- resolve-edit-target
  "Resolve the edit's path to an existing file. Returns either
   `{:file F :rel R}` or `{:error {:reason RK :message MSG}}` so the
   caller folds path-level problems (escape, missing file, target is a
   dir) into the same structured failure stream as match-level
   problems. Keeps `patch-analysis` exception-free."
  [path]
  (try
    (let [file (safe-path path)]
      (cond
        (not (.exists file))   {:error {:reason :file-not-found
                                        :path (.getPath file)
                                        :message (str "File not found: " (.getPath file))}}
        (.isDirectory file)    {:error {:reason :path-is-dir
                                        :path (.getPath file)
                                        :message (str "Path is a directory, not a file: "
                                                   (.getPath file))}}
        :else                  {:file file :rel (rel-path file)}))
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [type] :as data} (ex-data e)]
        {:error {:reason (case type
                           :ext.foundation.editing/path-escape :path-escape
                           :path-error)
                 :message (ex-message e)
                 :data data}}))))

;; Hashline locator resolution lives in the reusable `patch` layer
;; (`patch/resolve-hash-edit`, `patch/indices-matching-hash`). The
;; `:from_hash`/`:to_hash` branch of `patch-analysis` calls straight into
;; it — no bespoke hash math in this channel/IO namespace.

(defn- patch-analysis
  "Resolve every edit to a char SPAN against the ORIGINAL per-file snapshot (the
   file as the model last read it), collect spans per path, then splice them all
   together bottom-up. Resolving against the original — never cumulatively — keeps
   hashline / ordinal anchors and line numbers valid across a multi-edit batch:
   an earlier edit can no longer drift a later edit's anchor. Overlapping spans
   in one file are a hard error (split into separate patches). Atomic: any failure
   means `patch-safe` writes nothing."
  [edits]
  (let [edits (coerce-patch-edits edits)
        ;; PHASE 1 — resolve each edit to span(s) against the file's ORIGINAL text.
        {:keys [origs spans checks failures]}
        (loop [idx 0, remaining edits, origs {}, spans {}, checks [], failures []]
          (if-let [{:keys [path search replace after before nth from_hash to_hash] :as edit}
                   (first remaining)]
            (let [resolved (resolve-edit-target path)]
              (if-let [path-error (:error resolved)]
                (let [check {:edit-index idx :path path :reason (:reason path-error)
                             :path-error path-error :search-preview (search-preview (str search))}]
                  (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                (let [file        (:file resolved)
                      rel         (:rel resolved)
                      seen?       (contains? origs path)
                      ;; ALWAYS the original snapshot — never the cumulative result.
                      current     (or (get origs path) (slurp file))
                      origs       (assoc origs path current)
                      search      (str search)
                      replace     (str replace)
                      stale       (when-not seen? (staleness-check file edit))]
                  (if from_hash
                    ;; ---- hashline locator (content-addressed by line hash) ----
                    (let [base-check {:edit-index idx :path rel :from_hash from_hash :to_hash (or to_hash from_hash)}]
                      (if stale
                        (let [check (assoc base-check :reason :stale :stale stale)]
                          (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                        (let [res (patch/resolve-hash-edit-span current from_hash to_hash replace)]
                          (if-let [err (:error res)]
                            (let [check (assoc base-check :reason (:reason err) :hash-error err)]
                              (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                            (let [span  {:start (:start res) :end (:end res) :replacement (:replacement res)
                                         :file file :path rel :edit-index idx}
                                  check (assoc base-check :pass :hashline :applied-positions [(:applied-line res)])]
                              (recur (inc idx) (next remaining) origs
                                (update spans path (fnil conj []) span)
                                (conj checks check) failures))))))
                    ;; ---- text locator (:search) ----
                    (let [all-positions (find-substring-positions current search)
                          anchor-result (filter-positions-by-anchors all-positions (count search) current
                                          {:after after :before before})
                          filtered-positions (:positions anchor-result)
                          selected (when (nil? (:anchor-error anchor-result))
                                     (select-by-nth filtered-positions nth))
                          base-check {:edit-index idx :path rel
                                      :matches (count all-positions)
                                      :filtered-matches (count (or filtered-positions []))
                                      :search-preview (search-preview search)
                                      :anchors (cond-> {}
                                                 after  (assoc :after (search-preview (str after)))
                                                 before (assoc :before (search-preview (str before)))
                                                 nth    (assoc :nth nth))}]
                      (cond
                        stale
                        (let [check (assoc base-check :reason :stale :stale stale)]
                          (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))

                        (:anchor-error anchor-result)
                        (let [check (assoc base-check :reason :anchor-not-found :anchor-error (:anchor-error anchor-result))]
                          (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))

                        (seq selected)
                        (let [slen      (count search)
                              new-spans (mapv (fn [pos] {:start pos :end (+ (long pos) slen) :replacement replace
                                                         :file file :path rel :edit-index idx})
                                          selected)
                              check     (assoc base-check :applied-positions (vec selected) :pass :exact)]
                          (recur (inc idx) (next remaining) origs
                            (update spans path (fnil into []) new-spans)
                            (conj checks check) failures))

                        ;; Zero or out-of-range -> try fuzzy if multi-line
                        :else
                        (if-let [fuzzy (and (zero? (count all-positions))
                                         (not after) (not before) (nil? nth)
                                         (or (fuzzy-line-match current search)
                                           (ws-agnostic-match current search)))]
                          (let [{:keys [char-start char-end pass indent-delta]} fuzzy
                                rewritten (adjust-replace-for-indent replace indent-delta)
                                matched-ends-with-nl? (and (> (long char-end) 0)
                                                        (= \newline (.charAt current (dec (long char-end)))))
                                rewritten (if (and matched-ends-with-nl? (not (str/ends-with? rewritten "\n")))
                                            (str rewritten "\n")
                                            rewritten)
                                span  {:start char-start :end char-end :replacement rewritten
                                       :file file :path rel :edit-index idx}
                                check (assoc base-check :pass pass :indent-delta indent-delta
                                        :applied-positions [char-start])]
                            (recur (inc idx) (next remaining) origs
                              (update spans path (fnil conj []) span)
                              (conj checks check) failures))
                          (let [reason (cond
                                         (and nth (pos? (count filtered-positions))) :nth-out-of-range
                                         (and (or after before)
                                           (pos? (count all-positions))
                                           (zero? (count filtered-positions)))
                                         :anchors-exclude-all-matches
                                         (zero? (count all-positions)) :no-match
                                         :else :ambiguous-no-anchor)
                                nearest (when (zero? (count all-positions))
                                          (nearest-match-context current search))
                                check (cond-> (assoc base-check :reason reason)
                                        nearest (assoc :nearest nearest))]
                            (recur (inc idx) (next remaining) origs spans
                              (conj checks check) (conj failures check))))))))))
            {:origs origs :spans spans :checks checks :failures failures}))
        ;; PHASE 2 — splice each file's spans into its ORIGINAL, bottom-up so
        ;; earlier offsets stay valid. Overlapping spans are a conflict.
        results (for [[path file-spans] spans]
                  (let [before (get origs path)
                        sorted (sort-by :start file-spans)
                        bad    (first (filter (fn [[a b]] (> (long (:end a)) (long (:start b))))
                                        (partition 2 1 sorted)))]
                    (if bad
                      {:failure {:edit-index (:edit-index (second bad)) :path path
                                 :reason :overlapping-edits
                                 :overlap (mapv :edit-index bad)}}
                      {:plan {:file (:file (first file-spans)) :path (:path (first file-spans))
                              :before before
                              :after (reduce (fn [c {:keys [start end replacement]}]
                                               (str (subs c 0 start) replacement (subs c end)))
                                       before (reverse sorted))}})))
        overlap-failures (vec (keep :failure results))
        plans (vec (keep :plan results))
        all-failures (into failures overlap-failures)]
    {:plans plans
     :checks checks
     :failures all-failures
     :valid? (empty? all-failures)}))

(defn- explain-failure
  [{:keys [edit-index path matches filtered-matches reason anchors nearest stale anchor-error
           hash-error]}]
  (let [head (str "edit " edit-index " in " path)]
    (case reason
      :hash-not-found (str head " failed: " (name (:which hash-error)) "-hash "
                        (pr-str (:hash hash-error)) " matches no line in the current file"
                        " (that line changed or the file moved). Use the EXACT `lineno:hash`"
                        " anchor cat printed; re-read with cat for fresh :hashes, then resend"
                        " the batch.")
      :hash-misplaced (str head " failed: " (name (:which hash-error)) "-hash "
                        (pr-str (:hash hash-error)) " says line " (:stated-line hash-error)
                        " but that content is at line(s) " (pr-str (:found-lines hash-error))
                        " — too far to be drift, so this looks like a stale/misattributed"
                        " anchor. Re-read with cat for fresh `lineno:hash` anchors before"
                        " editing (this guard is what stops an edit landing on the wrong line).")
      :overlapping-edits (str head " failed: this edit's target overlaps another edit"
                           " in the same file — two edits touch the same lines. Merge"
                           " them into ONE edit, or split into separate patch calls.")
      :hash-ambiguous (str head " failed: " (name (:which hash-error)) "-hash "
                        (pr-str (:hash hash-error)) " matches " (count (:lines hash-error))
                        " identical lines " (pr-str (:lines hash-error))
                        " near that line — use :search instead, that content is not unique.")
      :hash-range-inverted (str head " failed: :to_hash line " (:to-line hash-error)
                             " precedes :from_hash line " (:from-line hash-error) ".")
      :stale (str head
               " failed: file changed since :expected-" (name (:reason stale))
               " check (expected " (or (:expected_mtime stale) (:expected_size stale))
               ", actual " (or (:actual-mtime stale) (:actual-size stale))
               "). Re-read the file before retrying.")
      :anchor-not-found (str head " failed: "
                          (name (:anchor anchor-error))
                          " anchor not found in file ("
                          (pr-str (:value anchor-error)) ").")
      :nth-out-of-range (str head " failed: :nth=" (:nth anchors)
                          " exceeds available matches (" filtered-matches ").")
      :anchors-exclude-all-matches (str head " failed: :after/:before anchors exclude all "
                                     matches " exact match(es).")
      :no-match (cond-> (str head " failed: no exact match.")
                  nearest (str " Nearest candidate at line " (:line nearest)
                            " (pass " (name (:pass nearest)) ")"
                            (when-let [occ (:occurrences nearest)]
                              (when (> (long occ) 1)
                                (str ", " occ " ambiguous ws-agnostic hits")))
                            (when-let [d (:diff nearest)]
                              (str "; first drift at line " (:at d) ": SEARCH "
                                (pr-str (search-preview (str (:search d))))
                                " vs FILE "
                                (pr-str (search-preview (str (:file d))))))
                            " - inspect context above."))
      :ambiguous-no-anchor (str head " failed: matched " matches
                             " time(s) and no :after/:before/:nth selector.")
      (str head " failed."))))

(defn- patch-failure-message
  [failures]
  ;; patch is ATOMIC — a single failed edit rejects the WHOLE batch and writes
  ;; NOTHING, so the file is byte-for-byte unchanged. Say so up front: the model
  ;; must not assume a partial application and must not re-read to \"repair\" it.
  (let [atomic "patch made NO changes — it is atomic, so the whole batch was rejected and every file is UNCHANGED. Fix the edit below and resend the full batch. "]
    (if (= 1 (count failures))
      (str atomic (explain-failure (first failures)))
      (str atomic (count failures) " edits failed; first: "
        (explain-failure (first failures))))))

(defn- non-exact-passes-for-path
  "Pull the non-`:exact` fuzzy passes that fired against `rel-path` out
   of `:checks`, preserving edit order. Returns nil when every check on
   that path used `:exact` so the caller can omit the `:passes` key
   entirely (no `:exact` noise in the trailer)."
  [checks rel-path]
  (let [ps (->> checks
             (filter #(= rel-path (:path %)))
             (keep :pass)
             (remove #{:exact})
             vec)]
    (when (seq ps) ps)))

(defn- indent-delta-for-path
  "Pull the FIRST non-zero `:indent-delta` from a `:relative-indent`
   check for `rel-path`, if any. Used purely as an alarm signal so the
   model knows the renderer re-shifted its `:replace` payload."
  [checks rel-path]
  (some (fn [c]
          (when (and (= rel-path (:path c))
                  (= :relative-indent (:pass c))
                  (some? (:indent-delta c))
                  (not (zero? (long (:indent-delta c)))))
            (long (:indent-delta c))))
    checks))

(defn patch-safe
  "Apply exact-replace patch edits to the filesystem.

   Returns a structured map; **never throws on normal failure paths**
   (no-match, anchor-not-found, stale mtime, file not found, path
   escape, ambiguous selection). Reserves exceptions for genuinely
   unexpected errors (thread interrupt, disk full, etc.).

   Success shape:
     {:success? true
      :plans    [{:path :before :after :passes? :indent-delta?} ...]
      :checks   [<per-edit-check> ...]}

   `:passes` lists the non-`:exact` fuzzy passes that fired against this
   plan's path, in edit order. Absent when every match was byte-exact —
   no `:exact` noise in the trailer. Same for `:indent-delta`: present
   only when a `:relative-indent` pass auto-shifted `:replace`.

   Failure shape:
     {:success? false
      :failures [<failure-check-with-:consecutive-failures>]
      :checks   [<every-edit-check>]
      :loop-hint <string-or-nil>
      :message  <human-readable summary>}

   `patch-tool` projects the result into the standard tool-success /
   tool-failure envelope so the model sees `:reason`, `:loop-hint`,
   and per-edit diagnostics in `:error` without `try/catch`."
  [edits]
  (let [{:keys [plans failures checks]} (patch-analysis edits)]
    (if (seq failures)
      ;; Failure path: bump per-path loop counter, attach hint, return.
      (let [paths (->> failures (map :path) distinct)
            counts (into {}
                     (for [p paths]
                       (let [f (try (safe-path p) (catch Throwable _ nil))]
                         (when f [p (bump-patch-fail-count! f)]))))
            failures-with-count (mapv (fn [f]
                                        (let [n (get counts (:path f))]
                                          (cond-> f n (assoc :consecutive-failures n))))
                                  failures)
            hint (some (fn [[p n]] (patch-loop-hint n p)) counts)]
        {:success? false
         :failures failures-with-count
         :checks   checks
         :loop-hint hint
         :message  (cond-> (patch-failure-message failures-with-count)
                     hint (str "\n" hint))})
      ;; Success path: commit writes, clear counters, project plans.
      (let [plans (vec plans)]
        (doseq [{:keys [file after]} plans]
          (spit file after))
        (doseq [{:keys [file]} plans]
          (clear-patch-fail-count! file))
        {:success? true
         :plans (mapv (fn [{:keys [path before after]}]
                        (let [passes (non-exact-passes-for-path checks path)
                              idelta (indent-delta-for-path checks path)]
                          (cond-> {:path path :before before :after after}
                            passes (assoc :passes passes)
                            idelta (assoc :indent-delta idelta))))
                  plans)
         :checks checks}))))

;; =============================================================================
;; write — whole-file write primitive (create or overwrite)
;;
;; patch is great for surgical edits but awkward for full-file rewrites:
;; the model would otherwise read the file, use whole content as :search
;; and submit the new blob as :replace. write makes the common case
;; ergonomic: one tool, one map, atomic semantics.
;;
;; Shape (parity with patch result):
;;   {:success? true
;;    :plan   {:path :before :after :op}}
;;   {:success? false
;;    :failures [<failure-with-:reason>]
;;    :loop-hint <string-or-nil>
;;    :message  <human-readable>}
;;
;; The `:is_overwrite` knob defaults to true. `:expected_mtime` /
;; `:expected_size` provide the same staleness guard as patch — pair
;; them with (:mtime / :size) from a prior cat for atomic
;; read-modify-write on existing files.
;; =============================================================================

(def ^:private write-required-keys #{:path :content})
(def ^:private write-optional-keys
  #{:expected_mtime :expected_size :is_overwrite})
(def ^:private write-allowed-keys
  (set/union write-required-keys write-optional-keys))

(defn- coerce-write-args
  [args]
  (when-not (map? args)
    (throw (ex-info "write expects a single map argument"
             {:type :ext.foundation.editing/invalid-write-args
              :got  (type args)})))
  (let [missing (seq (remove #(contains? args %) write-required-keys))
        unknown (seq (remove write-allowed-keys (keys args)))]
    (when missing
      (throw (ex-info "write missing required keys"
               {:type :ext.foundation.editing/invalid-write-args
                :missing (vec missing)
                :args args})))
    (when unknown
      (throw (ex-info "write has unknown keys"
               {:type :ext.foundation.editing/invalid-write-args
                :unknown (vec unknown)
                :allowed (vec write-allowed-keys)
                :args args})))
    (when-not (string? (:content args))
      (throw (ex-info "write :content must be a string"
               {:type :ext.foundation.editing/invalid-write-args
                :got (type (:content args))}))))
  (update args :path str))

(defn write-safe
  "Whole-file write primitive: create a new file OR overwrite an
   existing one with `:content`. Returns a structured result; **never
   throws on normal failure paths** (file exists with is_overwrite false,
   stale mtime/size, path escape).

   Required keys: `:path`, `:content` (string).
   Optional keys:
     :is_overwrite       default true; when false and target exists
                       → :reason :exists
     :expected_mtime   staleness guard; mismatch → :reason :stale
     :expected_size    staleness guard; mismatch → :reason :stale

   Success shape:
     {:success? true
      :plan {:path :before :after :op}
      :checks [<check>]}

   Failure shape:
     {:success? false
      :failures [<failure-with-:reason>]
      :checks   [<check>]
      :loop-hint <string-or-nil>
      :message  <human-readable>}"
  [args]
  (let [args (coerce-write-args args)
        path (:path args)
        content (str (:content args))
        is_overwrite (if (contains? args :is_overwrite) (:is_overwrite args) true)
        expected_mtime (:expected_mtime args)
        expected_size  (:expected_size  args)
        resolved (try {:file (safe-path path) :rel (rel-path (safe-path path))}
                   (catch clojure.lang.ExceptionInfo e
                     {:error {:reason (case (:type (ex-data e))
                                        :ext.foundation.editing/path-escape :path-escape
                                        :path-error)
                              :message (ex-message e)
                              :data (ex-data e)}}))]
    (if-let [perr (:error resolved)]
      (let [check {:edit-index 0 :path path :reason (:reason perr) :path-error perr}
            file-for-counter (try (safe-path path) (catch Throwable _ nil))
            n (when file-for-counter (bump-patch-fail-count! file-for-counter))]
        {:success? false
         :failures [(cond-> check n (assoc :consecutive-failures n))]
         :checks   [check]
         :loop-hint (when (and file-for-counter n) (patch-loop-hint n path))
         :message  (str "write failed: " (:message perr))})
      (let [^java.io.File file (:file resolved)
            rel (:rel resolved)
            exists? (.exists file)
            is-dir? (and exists? (.isDirectory file))
            before  (when (and exists? (not is-dir?)) (slurp file))
            actual-mtime (when exists? (.lastModified file))
            actual-size  (when exists? (.length file))
            fail (cond
                   is-dir?
                   {:reason :path-is-dir
                    :message (str "write target is a directory: " rel)}

                   (and (not is_overwrite) exists?)
                   {:reason :exists
                    :path rel
                    :message (str "write refused: " rel
                               " already exists and :is_overwrite is false")}

                   (and exists? (some? expected_mtime)
                     (not= (long expected_mtime) (long actual-mtime)))
                   {:reason :stale
                    :stale  {:reason :stale-mtime
                             :expected_mtime expected_mtime
                             :actual-mtime actual-mtime
                             :actual-size actual-size}
                    :message (str "write refused: " rel
                               " mtime changed since :expected_mtime")}

                   (and exists? (some? expected_size)
                     (not= (long expected_size) (long actual-size)))
                   {:reason :stale
                    :stale  {:reason :stale-size
                             :expected_size expected_size
                             :actual-size actual-size
                             :actual-mtime actual-mtime}
                    :message (str "write refused: " rel
                               " size changed since :expected_size")})]
        (if fail
          (let [n (bump-patch-fail-count! file)]
            {:success? false
             :failures [(cond-> (assoc fail :edit-index 0 :path rel) n
                          (assoc :consecutive-failures n))]
             :checks   [(assoc fail :edit-index 0 :path rel)]
             :loop-hint (patch-loop-hint n rel)
             :message  (cond-> (:message fail)
                         (>= n patch-fail-loop-threshold)
                         (str "\n" (patch-loop-hint n rel)))})
          (do
            (ensure-parent-dirs! file)
            (spit file content)
            (clear-patch-fail-count! file)
            {:success? true
             :plan {:path rel
                    :before before
                    :after content
                    :op (if exists? :update :add)}
             :checks [{:edit-index 0 :path rel
                       :op (if exists? :update :add)
                       :existed? exists?}]}))))))

(defn- create-dirs-safe [path]
  (let [f (safe-path path)]
    (fs/create-dirs f)
    (rel-path f)))

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

(defn- bounded-render-text
  [s]
  (let [s (str s)]
    (if (> (count s) render-preview-chars)
      (str (subs s 0 render-preview-chars)
        "\n...<+" (- (count s) render-preview-chars) " chars>")
      s)))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-tool
  "Read a window of a text file.

   Default-first design: `(cat path)` reads up to `default-cat-limit`
   (2000) lines from line 1, which is the WHOLE FILE for almost every
   source file in a normal repo. Reach for explicit slicing only when
   the file is bigger than 2000 lines OR you specifically want a
   middle/tail section.

   Arities:
     (cat path)                       — first 2000 lines (default — use this).
     (cat path :range start end)      — INCLUSIVE 1-based line range [start, end].
                                          Pick when you know both endpoints
                                          (e.g., a rg hit + :context window).
     (cat path :ranges [[s e] ...])   — several inclusive ranges from one file;
                                          use this instead of repeated same-file cats.
     (cat path :hash H)               — the single line whose content hash is H.
     (cat path :hash H1 H2)           — INCLUSIVE window between the lines hashed
                                          H1..H2. Addresses BY CONTENT (the read
                                          counterpart of `patch :from_hash`/:to_hash`):
                                          re-read a region you kept by its stored
                                          hashes, drift-proof — no line numbers.
                                          A missing/dup hash errors back to cat.
     (cat path :tail)                 — LAST 2000 lines.
     (cat path :tail n)               — LAST n lines.

   Result shape:
     {:op :cat :path P :lines [[<line-number> <text>] …]
      :hashes {<line-number> <hash> …}
      :next-offset N? :eof? B :truncated? B :mtime :size}
   `:hashes` maps each line number to a stable 6-char content hash (also
   shown in the gutter as `<ln> <hash>│ text`). Feed a pair to
   `patch` as `{:from_hash H1 :to_hash H2 :replace R}` to edit the line
   range [H1..H2] without reconstructing the source text — anchors are
   content-addressed, so they survive line drift.
   `:lines` carries `[ln text]` tuples — destructure with `[n t]`; no
   offset arithmetic. Filter content with `(filter (fn [[_ t]] …) :lines)`.
   Each line's text is verbatim — no per-line cap. `:next-offset` is nil
   at EOF or for tail; integer otherwise — pass to a follow-up
   `(cat path :range next-offset …)` to paginate. `:truncated?` is
   true when the 256KB byte cap chopped the window; paginate regardless.

   There are no `(cat path n)` / `(cat path offset n)` arities; use
   `:range` instead so there is one line-count surface. Use:
     (cat path :range 1 N)          ;; first N lines
     (cat path :range offset (+ offset n -1))  ;; n lines from offset"
  ([path]
   (let [out (read-file path 1 default-cat-limit)]
     (tool-success
       {:op :cat
        :path path
        :kind :file
        :result (assoc out :op :cat)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)}
        :presentation {:kind :source :path (:path out) :line-key :lines}})))
  ([path arg]
   (cond
     ;; Python-native form: a single options dict, e.g.
     ;;   cat("p", {"range": [5, 10]})       cat("p", {"ranges": [[1,5],[20,25]]})
     ;;   cat("p", {"hash": H})              cat("p", {"hash": [H1, H2]})
     ;;   cat("p", {"tail": 100})            cat("p", {})  -> whole file
     ;; Delegated to the keyword arities below so internal Clojure callers
     ;; (which pass bare keyword args) keep working unchanged.
     (map? arg)
     (let [rng    (:range arg)
           ranges (:ranges arg)
           hsh    (:hash arg)
           tail   (:tail arg)]
       (cond
         rng            (cat-tool path :range (first rng) (second rng))
         ranges         (cat-tool path :ranges ranges)
         (vector? hsh)  (cat-tool path :hash (first hsh) (second hsh))
         (some? hsh)    (cat-tool path :hash hsh)
         (integer? tail) (cat-tool path :tail tail)
         (some? tail)   (cat-tool path :tail)
         :else          (cat-tool path)))

     (= arg :tail)
     (let [out (tail-file path default-cat-limit)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (assoc out :op :cat)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}
          :presentation {:kind :source :path (:path out) :line-key :lines}}))

     :else
     (throw (ex-info "cat options must be a dict, e.g. cat(path, {\"range\": [start, end]})"
              {:type :ext.foundation.editing/invalid-cat-args
               :got arg}))))
  ([path arg n]
   (case arg
     :tail
     (let [out (tail-file path n)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (assoc out :op :cat)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}
          :presentation {:kind :source :path (:path out) :line-key :lines}}))

     :ranges
     (let [out (read-file-ranges path n)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (assoc out :op :cat)
          :metadata {:truncated? (:truncated? out)
                     :ranges (mapv :range (:ranges out))}
          :presentation {:kind :source :path (:path out) :line-key :lines}}))

     :hash
     ;; (cat path :hash H) — the single line whose content hash is H,
     ;; addressed by content (the symmetric read for patch :from_hash).
     (let [out (read-file-by-hash path n nil)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (assoc out :op :cat)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                     :range (:range out)}
          :presentation {:kind :source :path (:path out) :line-key :lines}}))

     (throw (ex-info "cat options must use {\"tail\": N}, {\"ranges\": [[s, e], ...]}, or {\"hash\": H}; for one range use {\"range\": [start, end]}"
              {:type :ext.foundation.editing/invalid-cat-args
               :got arg}))))
  ([path mode start end]
   (case mode
     ;; (cat path :range start end) — INCLUSIVE start..end (both 1-based).
     :range
     (do
       (validate-cat-range! start end)
       (let [n (inc (- (long end) (long start)))
             out (read-file path start n)
             out (assoc out :hashes (patch/lines->hashes (:lines out)))]
         (tool-success
           {:op :cat
            :path path
            :kind :file
            :result (assoc out :op :cat)
            :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                       :range [start end]}
            :presentation {:kind :source :path (:path out) :line-key :lines}})))

     ;; (cat path :hash from_hash to_hash) — INCLUSIVE window between the
     ;; lines hashed from_hash..to_hash, addressed by content.
     :hash
     (let [out (read-file-by-hash path start end)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (assoc out :op :cat)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                     :range (:range out)}
          :presentation {:kind :source :path (:path out) :line-key :lines}}))

     (throw (ex-info "cat window must use {\"range\": [start, end]} or {\"hash\": [from, to]}"
              {:type :ext.foundation.editing/invalid-cat-args
               :got mode})))))

(defn- ls-tool
  "List a directory recursively, GROUPED BY DIRECTORY. Returns a dict:
     {\"path\": P,             # workspace-relative root path
      \"absolute_path\": AP,
      \"root_type\": \"dir\"|\"file\",
      \"groups\": [{\"dir\": \"bin\",
                  \"files\": [{\"name\": \"dev\", \"size\": 2308}, ...]}, ...],
      \"entry_count\": N, \"file_count\": F, \"dir_count\": D,
      \"truncated\": bool,     # True when limit clamped the walk
      \"depth\": N, \"limit\": N}

   Each directory is listed ONCE (its `dir` is workspace-relative) with its
   direct `files` (name + raw byte size). The dir TREE is the set of group
   `dir` headers — a dir that holds only sub-dirs still appears, with empty
   `files`. This grouping is why ls stays compact: the dir prefix is stated
   once per group, not repeated on every file. Default: recursive walk up to
   depth 10 and 3000 entries, pre-order (sub-dirs first, then files, both
   alphabetical).

   Reconstruct a full path as g[\"dir\"] + \"/\" + f[\"name\"]. Filter across
   groups with a comprehension:
     [g[\"dir\"]+\"/\"+f[\"name\"] for g in r[\"groups\"] for f in g[\"files\"] if f[\"name\"].endswith(\".py\")]
     [g[\"dir\"] for g in r[\"groups\"]]   # just the directory tree

   ls() (no args) and ls(\".\") both list the current directory
   (Pythonic, like os.listdir()). Options are a trailing dict with
   snake_case keys:
     ls(path, {\"depth\": 2, \"is_files_only\": True})

   Recognised options (any combination):
     depth N                 max recursion depth (default 10)
     limit N                 stop after N entries (default 3000; sets truncated True)
     is_files_only B         emit only file entries (no directories)
     is_dirs_only B          emit only directory entries
     is_hidden B             include dotfiles / dotdirs (default False)
     is_respect_gitignore B  default True"
  ([] (ls-tool "."))
  ([path & {:as opts}]
   (let [listing (list-files path opts)
         result  (assoc listing :op :ls)]
     (tool-success
       {:op :ls
        :path path
        :kind :dir
        :result result
        :metadata  {:depth (:depth listing)
                    :limit (:limit listing)
                    :entry-count (:entry-count listing)
                    :file-count  (:file-count listing)
                    :dir-count   (:dir-count listing)
                    :truncated?  (:truncated? listing)
                    :is_hidden (:is_hidden opts)
                    :is_respect_gitignore (get opts :is_respect_gitignore true)}
        :presentation {:kind :flat-list}}))))

(defn- rg-tool
  "File-content search. Three output modes — default is content (hits with
   optional context); `:is_files_only true` returns just distinct paths;
   `:is_counts true` returns per-file match counts.

   Call with a single options dict (snake_case string keys):
     rg({\"any\": [\"a\"], \"paths\": [\"src\"]})

   Recognised keys:
     {\"all\": [\"a\", \"b\"]      — AND: every needle on same line
      \"any\": [\"a\", \"b\"]      — OR: at least one needle on a line
      \"paths\": [\"src\"]         — search roots/files (default [\".\"])
      \"include\": [\"**/*.py\"]   — glob filters (list)
      \"exclude\": [\"**/test/**\"]
      \"is_hidden\": False, \"is_respect_gitignore\": True,
      \"limit\": 250            — cap hits / files / counts (default 250)
      \"context\": N            — N lines before AND after each hit (alias)
      \"before\": N             — lines before each hit
      \"after\": N              — lines after  each hit
      \"is_files_only\": False   — return only distinct paths (no line text)
      \"is_counts\": False       — return per-file match counts
      \"is_regex\": False}       — needles are java.util.regex patterns
   Exactly one of \"all\"/\"any\". Strings are literal substrings by default;
   pass \"is_regex\": True to treat them as full regex (e.g. \\bdef login\\b).

   Result shape varies by mode (the result dict always carries \"op\": \"rg\"
   plus a \"mode\" discriminator):
     content     (mode \"content\")     {\"matches\": [{\"path\": P, \"lines\": [[ln, text]...]} ...], \"hit_count\": N, \"file_count\": N, \"first_hit\": \"P:L\", ...}
                                     # path stated ONCE per file; \"lines\" tuples mirror cat's [ln, text] shape
     is_files_only (mode \"files-only\")  {\"files\": [...], \"file_count\": N ...}
     is_counts     (mode \"counts\")      {\"counts\": [...], \"file_count\": N ...}"
  [& args]
  ;; Accept either a single spec map OR inline kwargs. Manual dispatch
  ;; (instead of `& {:as spec}`) so that malformed input — a stray
  ;; positional string, an odd-length rest seq — routes through one
  ;; clean `:invalid-rg-spec` error instead of Clojure's raw
  ;; "No value supplied for key" destructure exception.
  (let [spec (cond
               (and (= 1 (count args)) (map? (first args)))
               (first args)

               (and (even? (count args)) (every? keyword? (take-nth 2 args)))
               (apply hash-map args)

               :else
               (throw (ex-info
                        "rg takes a single options dict, e.g. rg({\"any\": [\"x\"], \"paths\": [\"src\"]})."
                        {:type :ext.foundation.editing/invalid-rg-arity
                         :expected '([spec-map] [& kwargs])
                         :got args})))
        {:keys [paths include exclude is_files_only is_counts is_regex
                before-ctx after-ctx limit] :as coerced} (coerce-rg-spec spec)
        out (rg-search spec)
        mode (cond is_files_only :files-only
               is_counts     :counts
               :else       :content)
        shared {:op       :rg
                :mode         mode
                :truncated-by (:truncated-by out)
                :spec         spec
                :paths        paths
                :limit        limit
                :is_regex       is_regex}
        result (case mode
                 :content
                 (let [hits          (vec (:hits out))
                       ordered-paths (distinct (map :path hits))
                       by-path       (group-by :path hits)
                       ;; Group hits by file so the path is stated ONCE per
                       ;; file instead of once per hit. :lines folds each
                       ;; hit's context (:before / match / :after) into one
                       ;; line-sorted, de-duplicated [ln text] vec — the same
                       ;; tuple shape cat emits, so the model reads one
                       ;; structure, not a flat path-repeating wall.
                       matches       (mapv (fn [p]
                                             (let [hs    (get by-path p)
                                                   lines (->> hs
                                                           (mapcat (fn [{:keys [line text before after]}]
                                                                     (concat before [[line text]] after)))
                                                           (into (sorted-map))
                                                           (mapv (fn [[ln tx]] [ln tx])))]
                                               {:path p :lines lines}))
                                       ordered-paths)]
                   (assoc shared
                     :matches matches
                     :hit-count (count hits)
                     :file-count (count matches)
                     :first-hit (when (pos? (count hits))
                                  (let [{:keys [path line]} (nth hits 0)]
                                    (str path ":" line)))
                     :context (cond-> {}
                                (pos? before-ctx) (assoc :before before-ctx)
                                (pos? after-ctx)  (assoc :after  after-ctx))))
                 :files-only
                 (let [files (vec (:files out))]
                   (assoc shared
                     :files files
                     :file-count (count files)))
                 :counts
                 (let [counts (vec (:counts out))]
                   (assoc shared
                     :counts counts
                     :file-count (count counts)
                     :total-matches (reduce + 0 (map :count counts)))))]
    (tool-success
      {:op :rg
       :path (if (= 1 (count paths))
               (first paths)
               ".")
       :kind :dir
       :result result
       :metadata (cond-> {:spec spec
                          :query-op (:op coerced)
                          :paths paths
                          :include include
                          :exclude exclude
                          :mode mode
                          :truncated-by (:truncated-by out)}
                   (= mode :content)
                   (assoc :hit-count (:hit-count result))
                   (= mode :files-only)
                   (assoc :file-count (:file-count result))
                   (= mode :counts)
                   (assoc :file-count (:file-count result)
                     :total-matches (:total-matches result)))
       :presentation (case mode
                       :content    {:kind :search-grouped}
                       :files-only {:kind :search-files}
                       :counts     {:kind :search-counts})})))

(def ^:private patch-diff-context-lines 3)
(def ^:private patch-diff-max-render-lines 240)
(def ^:private patch-java-diff-max-lines 5000)

(defn- cap-diff-lines
  [lines]
  (let [lines   (vec lines)
        n       (count lines)
        shown-n (min n patch-diff-max-render-lines)
        shown   (subvec lines 0 shown-n)
        omitted (- n shown-n)]
    (cond-> shown
      (pos? omitted)
      (conj (str "... diff truncated; " omitted " line(s) omitted")))))

(defn- common-prefix-count
  [a b]
  (let [limit (min (count a) (count b))]
    (loop [i 0]
      (if (and (< i limit) (= (a i) (b i)))
        (recur (inc i))
        i))))

(defn- common-suffix-count
  [a b prefix-count]
  (let [a-count (count a)
        b-count (count b)
        limit   (- (min a-count b-count) prefix-count)]
    (loop [i 0]
      (if (and (< i limit)
            (= (a (- a-count i 1)) (b (- b-count i 1))))
        (recur (inc i))
        i))))

(defn- prefixed-diff-lines
  [prefix lines]
  (let [lines   (vec lines)
        n       (count lines)
        shown-n (min n patch-diff-max-render-lines)
        shown   (subvec lines 0 shown-n)
        omitted (- n shown-n)]
    (cond-> (mapv #(str prefix %) shown)
      (pos? omitted)
      (conj (str prefix "... (" omitted " line(s) omitted)")))))

(defn- compact-diff-lines
  "Linear fallback for very large files. It is a bounded preview, not a
   minimal diff: for normal-sized files `java-diff-utils` renders real
   unified hunks."
  [a b]
  (let [prefix-count (common-prefix-count a b)
        suffix-count (common-suffix-count a b prefix-count)
        a-count      (count a)
        b-count      (count b)
        a-change-end (- a-count suffix-count)
        b-change-end (- b-count suffix-count)
        pre-start    (max 0 (- prefix-count patch-diff-context-lines))
        post-end     (min a-count (+ a-change-end patch-diff-context-lines))
        pre-lines    (subvec a pre-start prefix-count)
        del-lines    (subvec a prefix-count a-change-end)
        add-lines    (subvec b prefix-count b-change-end)
        post-lines   (subvec a a-change-end post-end)
        before-skip  pre-start
        after-skip   (- a-count post-end)]
    (vec
      (concat
        ["--- before"
         "+++ after"]
        (when (pos? before-skip)
          [(str "... " before-skip " unchanged line(s) before")])
        (map #(str " " %) pre-lines)
        (prefixed-diff-lines "-" del-lines)
        (prefixed-diff-lines "+" add-lines)
        (map #(str " " %) post-lines)
        (when (pos? after-skip)
          [(str "... " after-skip " unchanged line(s) after")])))))

(defn- java-unified-diff-lines
  [a b]
  (let [patch (DiffUtils/diff a b)]
    (vec (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a patch patch-diff-context-lines))))

(defn- unified-diff-text
  "Unified diff preview for two file blobs. Normal-sized files use
   `java-diff-utils` for real hunks. Very large files use a linear bounded
   fallback to keep `patch` result rendering from becoming the slow path."
  [before after]
  (cond
    (= before after) nil
    (nil? before) (str "+++ (new file, "
                    (count (str/split-lines (or after ""))) " lines)")
    (nil? after)  (str "--- (deleted, "
                    (count (str/split-lines (or before ""))) " lines)")
    :else
    (let [a (vec (str/split-lines before))
          b (vec (str/split-lines after))
          diff-lines (if (and (<= (count a) patch-java-diff-max-lines)
                           (<= (count b) patch-java-diff-max-lines))
                       (java-unified-diff-lines a b)
                       (compact-diff-lines a b))]
      (str/join "\n" (cap-diff-lines diff-lines)))))

(defn- patch-result-file-summary
  "Build a per-file summary map that lives on `:result` of `patch` /
   `write`.

   Minimal shape — every key is necessary signal, no redundant counters:

     {:path     <rel-path>
      :op       :update | :add
      :changed? <bool>            — false on no-op edits
      :diff     <unified-diff>    — the WRITE evidence; omitted only
                                    when both before+after are nil
      :passes   [<pass-kw> ...]   — ONLY when a non-:exact fuzzy pass
                                    fired; absent = byte-exact match
      :indent-delta <n>}          — ONLY when :relative-indent fuzzy
                                    auto-shifted :replace by N spaces

   Line counts (`:lines-before` / `:lines-after` / `:delta-lines`) were
   intentionally dropped: the `:diff` carries the exact change and the
   scalars duplicated that information at the cost of trailer bloat."
  [{:keys [op path before after passes indent-delta]}]
  (let [diff-text (unified-diff-text before after)]
    (cond-> {:path     path
             :op       (or op :update)
             :changed? (not= before after)}
      diff-text     (assoc :diff diff-text)
      (seq passes)  (assoc :passes (vec passes))
      indent-delta  (assoc :indent-delta indent-delta))))

(defn- patch-tool
  "Surgical file editing. Input is either edit maps or grouped same-file maps.

   Every edit carries EXACTLY ONE locator: `:search` (text) or
   `:from_hash` (hashline). Both forms take `:replace`.

   Text edit:
     {:path P :search S :replace R
      :after \"context\"?  :before \"context\"?
      :nth :first|:last|:all|N?
      :expected_mtime MS?  :expected_size BYTES?}

   Hashline edit (content-addressed — no whitespace reconstruction):
     {:path P :from_hash H1 :to_hash H2? :replace R}
   H1/H2 are per-line anchors from the `cat` `:hashes` map / gutter
   (`<ln> <hash>│ text`). The range is the line carrying H1 through the
   line carrying H2 (inclusive); omit `:to_hash` for a single line. The
   anchors are re-resolved against LIVE content on every edit, so they
   stay correct under line drift (insertions above, or earlier edits in
   the same batch) — unlike raw line numbers. Each hash must match exactly
   one line; on a dup-line collision the edit fails (use `:search`).

   Grouped same-file map (preferred for several changes to one file):
     {:path P
      :edits [{:search S1 :replace R1}
              {:from_hash \"a3f2e9\" :to_hash \"9c1d04\" :replace R2}]}

   Replaces the FIRST occurrence by default. Use `:nth`, `:after` /
   `:before` anchors, or extend `:search` with context to target a
   specific occurrence. Multi-line `:search` gets fuzzy fallbacks: a
   5-pass line matcher (exact -> rstrip -> unicode -> relative-indent ->
   trim) and, when line structure itself drifted (a SEARCH block that
   joins/splits lines vs the file), a whitespace-agnostic token match
   (`:ws-agnostic`) that applies only when the token run is unique. A
   total miss reports the nearest candidate WITH the first differing line
   so you can fix `:search` in one shot instead of re-reading blind.

   Companion primitives — each does ONE thing, no overlap with patch:
     write    whole-file create or overwrite
     move     rename / move
     delete   delete (or delete-if-exists)

   The full plan is validated against the live filesystem before any
   write — a single failure aborts the entire batch and no file is
   touched."
  [edits]
  (let [result (patch-safe edits)]
    (if (:success? result)
      (let [plans     (:plans result)
            summaries (mapv patch-result-file-summary plans)]
        (tool-success
          {:op :patch
           :path (or (:path (first plans)) ".")
           :kind :file
           :result summaries
           :metadata  {:mode          :exact-replace
                       :file-count    (count summaries)
                       :changed-count (count (filter :changed? summaries))}}))
      ;; Failure: full structured `:error` map with `:reason`, per-edit
      ;; `:failures`, `:checks`, and the optional `:loop-hint` so the
      ;; model can read them as plain map keys (no try/catch needed).
      (let [first-failure (first (:failures result))]
        (extension/failure
          {:result   nil
           :op       :patch
           :metadata {:target {:requested (str (or (:path first-failure) "."))
                               :resolved nil
                               :absolute nil
                               :kind :file}
                      :mode :exact-replace
                      :started-at-ms (now-ms)
                      :finished-at-ms (now-ms)
                      :duration-ms 0}
           :error    {:message  (:message result)
                      :reason   (:reason first-failure)
                      :failures (:failures result)
                      :checks   (:checks result)
                      :loop-hint (:loop-hint result)
                      :mode     :exact-replace}})))))

(defn- write-tool
  "Whole-file write: create a new file OR overwrite an existing one.

   Args accept BOTH calling conventions (Clojure 1.11+ kwargs
   auto-coercion); both forms below are equivalent:
     (write {:path P :content S})
     (write :path P :content S)

     (write {:path P :content S :is_overwrite false})     fail if file exists
     (write {:path P :content S :expected_mtime MS})    staleness guard
     (write {:path P :content S :expected_size  BYTES}) staleness guard

   Returns the same per-file summary shape as `patch` (so the model
   reads `:diff`, `:changed?`, etc. with one mental model). `:op` is
   `:add` for new files and `:update` for overwrites. Failures land in
   the structured error envelope as `;; ! data {:reason …}` — no
   try/catch needed."
  [& {:as args}]
  (let [result (write-safe args)]
    (if (:success? result)
      (let [plan (:plan result)
            summary (patch-result-file-summary plan)]
        (tool-success
          {:op :write
           :path (:path plan)
           :kind :file
           :result [summary]
           :metadata  {:mode :write
                       :file-count 1
                       :changed-count (if (:changed? summary) 1 0)
                       :op (:op plan)}}))
      (let [first-failure (first (:failures result))]
        (extension/failure
          {:result   nil
           :op       :write
           :metadata {:target {:requested (str (or (:path first-failure)
                                                 (:path args)
                                                 "."))
                               :resolved nil
                               :absolute nil
                               :kind :file}
                      :mode :write
                      :started-at-ms (now-ms)
                      :finished-at-ms (now-ms)
                      :duration-ms 0}
           :error    {:message  (:message result)
                      :reason   (:reason first-failure)
                      :failures (:failures result)
                      :checks   (:checks result)
                      :loop-hint (:loop-hint result)
                      :mode     :write}})))))

(defn- create-dirs-tool
  "Ensure dir exists. Returns the canonical foundation map shape so the
   model destructures `(:path r)` / `(:created? r)` directly off the
   bound result."
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :create-dirs
       :path path
       :kind :dir
       :result {:op :create-dirs
                :path out
                :created? (not before)
                :already-existed? before}
       :metadata {:created? (not before)
                  :already-existed? before}})))

(defn- copy-tool
  "Copy path. Returns the canonical foundation map shape — `(:src r)` /
   `(:dest r)` / `(:path r)` (alias for dest).

   Opts accept BOTH calling conventions (Clojure 1.11+ kwargs
   auto-coercion):
     (copy src dest {:is_overwrite true})
     (copy src dest :is_overwrite true)"
  ([src dest & {:as opts}]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :copy
        :path dest
        :kind :path
        :result {:op :copy
                 :src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- move-tool
  "Move/rename path. Returns the canonical foundation map shape —
   `(:src r)` / `(:dest r)` / `(:path r)` (alias for dest).

   Opts accept BOTH calling conventions:
     (move src dest {:is_overwrite true})
     (move src dest :is_overwrite true)"
  ([src dest & {:as opts}]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :move
        :path dest
        :kind :path
        :result {:op :move
                 :src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- delete-tool
  "Delete `path`. Returns the map every foundation tool returns so the
   channel renderer (and `(def r (delete p))` consumers) can read
   `(:path r)` and `(:deleted? r)` straight off. Previously this
   returned `nil` and the channel preview painted `DELETE nil` —
   the same consistency bug already fixed in `exists?`."
  [path]
  (delete-safe path)
  (tool-success
    {:op :delete
     :path path
     :kind :path
     :result {:op :delete :path path :deleted? true}
     :metadata {:deleted? true}}))

(defn- delete-if-exists-tool
  "Delete `path` if present. Returns the map every foundation tool returns
   so destructuring (and the channel renderer) can read `(:path r)`
   and `(:deleted? r)` directly. Previously this returned a bare
   boolean which broke `(def r (delete-if-exists p))` consumers
   (same lesson as `exists?`)."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :delete-if-exists
       :path path
       :kind :path
       :result {:op :delete-if-exists :path path :deleted? deleted?}
       :metadata {:deleted? deleted?}})))

(defn- exists-tool
  "Filesystem existence check.

   Returns `{:op :exists? :path P :exists? B}` so the model
   destructures the same shape every foundation tool uses (consistent
   with `cat`, `ls`, `rg`, …). Earlier this returned a bare
   boolean, which broke `(def r (exists? P))` consumers that
   reached for `(:exists? r)` — a wholly reasonable assumption given
   the surrounding map-shaped foundation API. See conversation
   11d4f817-fbd1-43ab-a6b4-052c8557af0a turn 4 iter 1→2."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :exists?
       :path path
       :kind :path
       :result {:op :exists?
                :path   (str path)
                :exists? exists?}
       :metadata {:exists? exists?}})))

;; =============================================================================
;; Structured renderers
;; =============================================================================

;; Channel IR builders. No Markdown string round-trip on tool display.
(defn- ir-code [s] [:c {} (str s)])
(defn- ir-strong [s] [:strong {} (str s)])
(defn- ir-code-block [lang body] [:code (cond-> {} lang (assoc :lang lang)) (str body)])
(defn- ir-inline [x] (if (vector? x) x [:span {} (str x)]))
(defn- ir-p [& parts]
  (into [:p {}]
    (map ir-inline (filter some? parts))))
(defn- ir-root [& blocks]
  (into [:ir {}] (filter some? blocks)))

(defn- flat-entry-line
  "Render one flat ls row as `path/[trailing-slash-for-dirs] (Nb)`.
   Trailing slash is RENDERER convention (Roo / cline style) so directory
   rows read as filesystem paths; the underlying data shape carries
   `:type :dir` for programmatic discrimination instead."
  [{:keys [path type size]}]
  (let [dir? (= :dir type)]
    (str path
      (when dir? "/")
      (when (and (not dir?) (some? size)) (str "  (" size "B)")))))

;; Cat-body rendering routes entirely through the reusable hashline layer
;; in `patch` (`render-hashline-block` / `render-hashline-range-block`):
;; the gutter is `<hash>│ text`, hash-addressed, single source of truth for
;; whole-file, range and tail reads. Line numbers stay in the CAT summary
;; (`from=` / `next-offset=`) and range headers, not the per-line gutter.

;; ---------------------------------------------------------------------------
;; Per-symbol renderers
;;
;; Engine contract ({:summary :display}, Phase 1 hard cut):
;;   render-fn -> (fn [result] {:summary <ir-or-zones> :display <ir>})
;;
;; `result` is the raw payload returned to the sandbox. `:summary` is the
;; single badge row — a zone map {:left :center? :right?} when the result has
;; a natural label + right-anchored metric, else a one-paragraph IR whose
;; first [:strong …] is the label. `:display` is the full expanded IR body.
;; Engine handles `:success? false`
;; separately — error fns are optional and fall back to `default-error-result`
;; (which already returns the contract). The MODEL surface is the per-iteration
;; trailer (real values via pr-str); there is no second model-side render.
;; ---------------------------------------------------------------------------

(defn- channel-render-cat
  "Channel preview. Summary is a zone badge: `CAT` label, the path
   centered, the line count + pagination state anchored right. Display
   is the numbered-line block. Reads the plain map directly; no
   handle/deref."
  [{:keys [path next-offset truncated? lines ranges]}]
  (let [lines        (vec lines)
        ranges       (vec ranges)
        range-labels (mapv (fn [{:keys [range]}]
                             (let [[start end] range]
                               (str start "-" end)))
                       ranges)
        line-count   (count lines)
        first-ln     (ffirst lines)
        ;; Channel/TUI display is a HUMAN surface — line-number gutter,
        ;; not the model's `<hash>│` edit-anchor gutter. Humans navigate
        ;; cat output by line number; the hash anchors live in the
        ;; model-facing `:lines`/`:hashes` payload (Vis session ac065988).
        body         (if (seq ranges)
                       (patch/render-lineno-range-block ranges)
                       (patch/render-lineno-block lines))
        state        (cond
                       (seq ranges) (str "ranges=" (str/join "," range-labels)
                                      (when truncated? "  (byte-cap)"))
                       next-offset  (str "next-offset=" next-offset
                                      (when truncated? " (byte-cap)"))
                       truncated?   "(byte-cap)"
                       :else        "(eof)")]
    {:summary {:left   (ir-strong "CAT")
               :center (ir-code (or path "?"))
               :right  (str line-count " line" (when (not= 1 line-count) "s")
                         (when (and first-ln (empty? ranges)) (str "  from=" first-ln))
                         "  " state)}
     :display (ir-root
                (ir-code-block "text" (bounded-render-text body)))}))

(defn- ls-group-block
  "Render one ls dir-group as a header line + indented file rows:
     bin/
       dev  2.3k
       vis  4.5k
   Empty groups (a dir with only subdirs) render as the bare header."
  [{:keys [dir files]}]
  (let [header (str dir "/")]
    (if (seq files)
      (str header "\n"
        (str/join "\n"
          (map (fn [{:keys [name size]}]
                 (str "  " name (when (some? size) (str "  " (human-size size)))))
            files)))
      header)))

(defn- channel-render-ls
  "Channel preview. Summary is a zone badge: `LS` label, the path
   centered, file/dir counts anchored right. Display is GROUPED BY
   DIRECTORY — each dir is stated once as a header, its files indented
   beneath with a human-readable size. Same grouping the model sees in
   `:groups`, so the prefix-duplication that bloated the flat list is
   gone in both surfaces."
  [{:keys [path groups entry-count file-count dir-count truncated?
           depth limit]}]
  {:summary {:left   (ir-strong "LS")
             :center (ir-code (or path "?"))
             :right  (str entry-count " entr" (if (= 1 entry-count) "y" "ies")
                       "  files=" (or file-count 0)
                       "  dirs=" (or dir-count 0)
                       (when (and depth (not= depth 10)) (str "  depth=" depth))
                       (when truncated?
                         (str "  truncated" (when limit (str "=" limit)))))}
   :display (ir-root
              (when (seq groups)
                (ir-code-block "text"
                  (bounded-render-text
                    (str/join "\n" (map ls-group-block groups))))))})

(defn- render-rg-hit-block
  "Render one content-mode hit with optional :before / :after context.
   Context lines use a `  N│ text` gutter; the matched line uses `▶ N│ text`
   so the anchor stands out without diff symbols."
  [{:keys [path line text before after]}]
  (let [head (str path ":" line)
        ctx-line (fn [marker [ln t]] (str marker " " ln "│ " t))
        body-lines (concat
                     (map #(ctx-line " " %) (or before []))
                     [(ctx-line "▶" [line text])]
                     (map #(ctx-line " " %) (or after [])))]
    [head (str/join "\n" body-lines)]))

(defn- channel-render-rg
  "Channel preview — mode-aware with a zone badge. The label varies by
   mode (`RG` / `RG files` / `RG counts`), the match/file tally is
   anchored right. Content-mode display renders each hit with its
   `:before` / `:after` context (when present) so the body reads like a
   miniature grep -C output; `:files-only` shows distinct paths;
   `:counts` shows per-file totals."
  [{:keys [mode matches files counts truncated-by hit-count file-count
           total-matches]}]
  (case mode
    :files-only
    (let [n (or file-count (count files))]
      {:summary {:left  (ir-strong "RG files")
                 :right (str n " file" (when (not= 1 n) "s")
                          "  truncated-by=" (name (or truncated-by :none)))}
       :display (ir-root
                  (when (seq files)
                    (ir-code-block "text"
                      (bounded-render-text (str/join "\n" files)))))})

    :counts
    (let [n (or file-count (count counts))]
      {:summary {:left  (ir-strong "RG counts")
                 :right (str n " file" (when (not= 1 n) "s")
                          (when total-matches (str "  total=" total-matches))
                          "  truncated-by=" (name (or truncated-by :none)))}
       :display (ir-root
                  (when (seq counts)
                    (ir-code-block "text"
                      (bounded-render-text
                        (str/join "\n"
                          (map (fn [{:keys [path count]}] (format "%-50s %d" path count))
                            counts))))))})

    ;; default: :content (or unset). Grouped by file: the path is stated
    ;; ONCE as a header, then its matches as `-- range S-E --` windows with
    ;; the human line-number gutter (`<ln>│ <text>`) — IDENTICAL to cat's
    ;; multi-range render (patch/render-lineno-range-block). Gaps between
    ;; matched lines surface as new range headers, not a bare divider.
    (let [n  (or hit-count 0)
          fc (or file-count (count matches))]
      {:summary {:left  (ir-strong "RG")
                 :right (str fc " file" (when (not= 1 fc) "s")
                          " · " n " hit" (when (not= 1 n) "s")
                          "  truncated-by=" (name (or truncated-by :none)))}
       :display
       (if (seq matches)
         (ir-root
           (ir-code-block "text"
             (bounded-render-text
               (str/join "\n\n"
                 (map (fn [{:keys [path lines]}]
                        (str path "\n"
                          (patch/render-lineno-range-block
                            (patch/tuples->ranges lines))))
                   matches)))))
         (ir-root))})))

(defn- channel-render-patch
  "Channel preview: badge header + (capped) unified diff per file.
   Pure projection over the summary map (`patch-result-file-summary`).

   No line counts in the header — the diff itself carries the line-level
   change and adding scalars duplicated that information. The header
   instead surfaces structural fuzzy alarms: `:passes` (non-`:exact`
   fuzzy passes that fired) and `:indent-delta` (auto re-indent applied
   by `:relative-indent`). These signal \"verify the diff carefully\";
   their absence means byte-exact match."
  [result]
  (let [files   (if (sequential? result) result [result])
        changed (count (filter :changed? files))
        nf      (count files)]
    {:summary {:left  (ir-strong "PATCH")
               :right (str nf " file" (when (not= 1 nf) "s")
                        (when (pos? changed) (str "  changed=" changed)))}
     :display
     (apply ir-root
       (mapcat
         (fn [{:keys [path op diff changed? passes indent-delta]}]
           (let [header (str (name (or op :update))
                          " " (or path "?")
                          (when (seq passes)
                            (str " [fuzzy: " (str/join "," (map name passes)) "]"))
                          (when indent-delta
                            (str " [indentΔ " (if (pos? indent-delta) "+" "")
                              indent-delta "]"))
                          (when (false? changed?) " (no-op)"))]
             (cond-> [(ir-p (ir-code header))]
               diff (conj (ir-code-block "diff" (bounded-render-text diff))))))
         files))}))

;; ---------------------------------------------------------------------------
;; Mutation tool renderers — strict map shape only.
;;
;; Every foundation mutation tool returns `{:op K :path P ...}` (see
;; `create-dirs-tool` / `copy-tool` / `move-tool` / `delete-tool` /
;; `delete-if-exists-tool` / `exists-tool`). Renderers destructure
;; that map directly; bare strings, bare booleans, and nil are NOT
;; supported shapes — if they show up the renderer fails loudly so
;; the boundary bug surfaces at write time, not at paint time.
;; ---------------------------------------------------------------------------

(defn- channel-render-create-dirs
  [{:keys [path]}]
  {:summary {:left  (ir-strong "MKDIR")
             :right (ir-code path)}
   :display (ir-root (ir-p (ir-strong "MKDIR") "  " (ir-code path)))})

(defn- channel-render-copy
  [{:keys [src dest path]}]
  (let [target (or dest path)]
    {:summary (cond-> {:left  (ir-strong "COPY")
                       :right (ir-code target)}
                src (assoc :center (ir-code src)))
     :display (ir-root (ir-p (ir-strong "COPY")
                         (when src (str "  " src))
                         "  → " (ir-code target)))}))

(defn- channel-render-move
  [{:keys [src dest path]}]
  (let [target (or dest path)]
    {:summary (cond-> {:left  (ir-strong "MOVE")
                       :right (ir-code target)}
                src (assoc :center (ir-code src)))
     :display (ir-root (ir-p (ir-strong "MOVE")
                         (when src (str "  " src))
                         "  → " (ir-code target)))}))

(defn- channel-render-delete
  [{:keys [path]}]
  {:summary {:left  (ir-strong "DELETE")
             :right (ir-code path)}
   :display (ir-root (ir-p (ir-strong "DELETE") "  " (ir-code path)))})

(defn- channel-render-delete-if-exists
  [{:keys [path deleted?]}]
  (let [label (if deleted? "DELETE" "ABSENT")]
    {:summary {:left  (ir-strong label)
               :right (ir-code path)}
     :display (ir-root (ir-p (ir-strong label) "  " (ir-code path)))}))

(defn- channel-render-exists?
  [{:keys [path exists?]}]
  (let [label (if exists? "EXISTS" "MISSING")]
    {:summary {:left  (ir-strong label)
               :right (ir-code path)}
     :display (ir-root (ir-p (ir-strong label) "  " (ir-code path)))}))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Symbol declarations.
;;
;; Each underlying `xxx-tool` defn carries the canonical docstring + arglists
;; on its var. `vis/symbol` reads them straight from the var meta - the
;; Python sandbox sees the same text the prompt-listing renders.
;; `:symbol` overrides the var name (`cat-tool` -> `cat`) for the model-facing
;; surface; everything else (examples, render-fn, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(def cat-symbol
  (vis/symbol #'cat-tool
    {:symbol 'cat
     :before-fn (path-protected-before-fn :cat :file :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-cat
     :on-error-fn (tool-failure-on-error :cat :file nil)}))

(def ls-symbol
  (vis/symbol #'ls-tool
    {:symbol 'ls
     :before-fn (path-protected-before-fn :ls :dir :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-ls
     :on-error-fn (tool-failure-on-error :ls :dir nil)}))

(def rg-symbol
  (vis/symbol #'rg-tool
    {:symbol 'rg
     :before-fn (path-protected-before-fn :rg :dir :read rg-arg-paths)
     :tag :observation
     :render-fn channel-render-rg
     :on-error-fn (tool-failure-on-error :rg :dir nil)}))

(def patch-symbol
  (vis/symbol #'patch-tool
    {:symbol 'patch
     :before-fn (path-protected-before-fn :patch :file :write patch-arg-paths)
     :tag :mutation
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :patch :file nil)}))

(def write-symbol
  ;; write reuses the patch channel renderer because its `:result`
  ;; shape is the same single-file summary (just always 1-file long).
  (vis/symbol #'write-tool
    {:symbol 'write
     :before-fn (path-protected-before-fn :write :file :write write-arg-paths)
     :tag :mutation
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :write :file nil)}))

(def create-dirs-symbol
  (vis/symbol #'create-dirs-tool
    {:symbol 'create-dirs
     :before-fn (path-protected-before-fn :create-dirs :dir :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-create-dirs
     :on-error-fn (tool-failure-on-error :create-dirs :dir nil)}))

(def copy-symbol
  (vis/symbol #'copy-tool
    {:symbol 'copy
     :before-fn (path-protected-before-fn :copy :path :write first-two-arg-paths)
     :tag :mutation
     :render-fn channel-render-copy
     :on-error-fn (tool-failure-on-error :copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
    {:symbol 'move
     :before-fn (path-protected-before-fn :move :path :write first-two-arg-paths)
     :tag :mutation
     :render-fn channel-render-move
     :on-error-fn (tool-failure-on-error :move :path nil)}))

(def delete-symbol
  (vis/symbol #'delete-tool
    {:symbol 'delete
     :before-fn (path-protected-before-fn :delete :path :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-delete
     :on-error-fn (tool-failure-on-error :delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol #'delete-if-exists-tool
    {:symbol 'delete-if-exists
     :before-fn (path-protected-before-fn :delete-if-exists :path :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-delete-if-exists
     :on-error-fn (tool-failure-on-error :delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol #'exists-tool
    {:symbol 'exists?
     :before-fn (path-protected-before-fn :exists? :path :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-exists?
     :on-error-fn (tool-failure-on-error :exists? :path nil)}))

(defn available-editing-symbols
  []
  [cat-symbol
   ls-symbol
   rg-symbol
   patch-symbol
   write-symbol
   create-dirs-symbol
   copy-symbol
   move-symbol
   delete-symbol
   delete-if-exists-symbol
   exists?-symbol])

(defn available-editing-prompt
  []
  (str/join "\n"
    ["Editing tools — bare Python functions: cat / ls / rg / patch / write + copy / move / delete / exists. Canonical path only."
     ""
     "FLOW"
     "  LOCATE — pick by what you already know, cheapest first:"
     "    0. Already located it?      → it's a FACT. Re-patch by from_hash. DON'T grep/cat again."
     "    1. Know the path?           → scoped rg({\"path\": …}) + cat(…) batched in ONE reply."
     "    2. Know content, not file?  → rg({…, \"is_files_only\": True}) → {\"files\": [paths]}; cat each path."
     "       rg ALWAYS returns a DICT, never a list — content→{\"matches\": [{\"path\":…, \"lines\":…}]}, is_files_only→{\"files\": [...]}, is_counts→{\"counts\": …}. Iterate r[\"matches\"] / r[\"files\"], NEVER rg(…) itself."
     "    3. Tree unfamiliar?         → ls(…) for shape — ONCE, not per turn."
     "  Wide CONTENT grep is last resort, not default (dumps junk into context)."
     "  Read:   cat(path)  — whole by default; large files use one 400-500 line range:"
     "    cat(path, {\"range\": [start, end]})"
     "    cat(path, {\"ranges\": [[start, end], ...]})"
     "  cat rows render `HASH| text`; HASH anchors the line."
     "  PATCH STRATEGY — pick locator by intent, batch in one call:"
     "  from_hash = precise line/range, drift-safe — for a UNIQUE line (DEFAULT):"
     "    patch([{\"path\": P, \"from_hash\": H, \"replace\": R}])"
     "    patch([{\"path\": P, \"from_hash\": H1, \"to_hash\": H2, \"replace\": R}])  # range; anchor UNIQUE ends"
     "  Repeated-content line (a bare `}`, `})`, blank)? Its hash is AMBIGUOUS — never"
     "  bare-target it: use from_hash..to_hash on unique neighbours, or \"search\" with"
     "  enough surrounding lines to match uniquely."
     "  search = bulk/fuzzy (rename-all, dup/blank/repeated lines, multi-line context):"
     "    patch({\"path\": P, \"edits\": [{\"search\": S1, \"replace\": R1}]})"
     "    patch([{\"path\": P, \"search\": S, \"replace\": R, \"nth\": \"all\"}])  # every hit"
     "  Whole files:  write({\"path\": P, \"content\": S})"
     "  File ops: is_exists(path)  copy(src, dest)  move(src, dest)  delete(path)"
     ""
     "INVARIANTS"
     "  - Dicts (snake_case keys) for option-bearing tools. Don't assume paths; root-search before reading."
     "  - Don't re-cat after patch/write; the diff is the evidence."
     "  - Side effect in its own reply; paths stay inside the workspace root."]))

(def editing-symbols
  "Default editing symbol set for docs/tests."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests."
  (available-editing-prompt))
