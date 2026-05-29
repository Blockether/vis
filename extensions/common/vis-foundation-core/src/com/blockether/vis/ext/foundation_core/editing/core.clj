(ns com.blockether.vis.ext.foundation-core.editing.core
  "Filesystem tools exposed under the `v/` alias in the SCI sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (v/cat path)            ; -> {:path :lines [[N text]…] :next-offset N? :truncated? B}
        (v/cat path n)          ; first n lines from line 1
        (v/cat path offset n)   ; n lines starting at line `offset` (1-based)
        (v/cat path :tail)      ; last 400 lines (tail)
        (v/cat path :tail n)    ; last n lines
        (v/ls path)             ; -> nested {:name :path :type :size :children} tree
        (v/ls path opts)        ; opts is {:depth :hidden? :respect-gitignore?}
        (v/rg spec)            ; -> {:hits :truncated-by}; spec = {:any [literal] :paths [src]}
                               ; OR {:all [lit1 lit2]}; no regex/query+opts shorthand

   2. Cwd-safe wrappers over the babashka.fs file API. `v/patch` is
      the canonical text edit surface:

        (v/cat path)
        (v/patch [{:path path :search old :replace new}])
        (v/create-dirs path)
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

   Hard guard: every path must stay inside the session's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.editing.patch :as patch]
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

;; v/cat pagination contract:
;;   `default-cat-limit`     - lines per window when the model omits `n`.
;;                             Industry parity — Claude Code / Roo Code use
;;                             2000 by default; Cline uses 1000.
;;   `max-cat-window-bytes`  - hard ceiling on a single window's bytes.
;;                             Doubles as the persistence-blob ceiling:
;;                             each call's result is Nippy-frozen into the
;;                             iteration's `forms` BLOB, bounded by this.
;;                             Not user-tunable; it is the storage contract.
;;
;; The earlier `max-line-length` per-line cap (2000 chars + `…<+N chars
;; truncated>` marker) was retired. It produced the same failure pattern
;; as the trailer/rg caps removed alongside it (see ctx_renderer.clj
;; header + conversation ccee2e1f-16ee-4acf-8d93-b4505034c0de): a
;; silent ellipsis made the model perceive its own data as missing and
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
      (let [paths (:paths spec)]
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
   usable for every observation tool (v/ls, v/rg, v/cat-on-dir,
   v/exists?, v/grep, …); hidden protected extension roots (for
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
;; Editing used to keep its own copies; they were thin shims and crossed
;; the abstraction boundary (color-role lived here too). Use the engine
;; functions directly.

;; Op tags carried INLINE on each `vis/symbol` opts map below; the
;; old (extension/register-op! ...) doseq retired.

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
    (throw (ex-info "v/cat offset must be a positive integer (1-based line number)."
             {:type :ext.foundation.editing/invalid-cat-args
              :offset offset})))
  (when-not (and (integer? n) (pos? n))
    (throw (ex-info "v/cat limit must be a positive integer line count."
             {:type :ext.foundation.editing/invalid-cat-args
              :limit n}))))

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
   them as `:expected-mtime` / `:expected-size` on a subsequent
   `v/patch` / `v/write` to fail closed if the file changed since the read.
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
                  ;; the per-line cap was retired (see the
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

(defn- visible-children [^File f {:keys [hidden? respect-gitignore? ignore-node root]}]
  (when (.isDirectory f)
    (let [kids (->> (.listFiles f)
                 (remove (fn [^File c]
                           (and (not hidden?) (.isHidden c))))
                 (remove (fn [^File c]
                           (and respect-gitignore?
                             (ignored? ignore-node c root)))))]
      (sort-by (juxt #(if (.isDirectory %) 0 1) #(.getName %)) kids))))

;; Flat-listing helper. The earlier nested `:children` shape made the
;; model walk the tree client-side (`tree-seq map? :children`) for the
;; most common questions (find a file, count files, filter by ext). Flat
;; output sidesteps that — every entry is a top-level row the model can
;; filter directly.
(defn- flat-entry
  "Project a `File` into the flat-list row shape used by v/ls.
   `:path` is workspace-relative; trailing `/` on dir paths is left to
   the renderer so the data shape stays uniform."
  [^File f]
  {:path (rel-path f)
   :type (if (.isDirectory f) :dir :file)
   :size (if (.isDirectory f) nil (.length f))})

(defn- collect-flat-entries
  "BFS-like (actually pre-order DFS) walk under `root` up to `max-depth`.
   Returns `{:entries [...] :truncated? B}`. Respects `:hidden?` /
   `:respect-gitignore?` like before. `:files-only?` and `:dirs-only?`
   are post-filters applied per entry (root is never emitted). Stops at
   `max-limit` entries and marks `:truncated? true`."
  [^File root opts* max-depth max-limit files-only? dirs-only?]
  (let [acc        (volatile! (transient []))
        truncated? (volatile! false)
        keep?      (fn [^File f]
                     (cond files-only? (.isFile f)
                       dirs-only?  (.isDirectory f)
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

(defn- list-files
  ;; Internal helper — always called with a real map (or nil).
  ;; The dual kwargs/map calling convention lives on the public
  ;; `ls-tool` wrapper; this stays positional to keep nil-forwarding
  ;; trivial.
  ([path] (list-files path nil))
  ([path opts]
   (let [{:keys [depth limit hidden? respect-gitignore? files-only? dirs-only?]
          :or {depth default-list-depth
               limit default-list-limit
               hidden? false
               respect-gitignore? true
               files-only? false
               dirs-only?  false}} (or opts {})
         _ (when (and files-only? dirs-only?)
             (throw (ex-info "v/ls :files-only? and :dirs-only? are mutually exclusive"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :opts opts})))
         _ (when-not (and (integer? depth) (not (neg? depth)))
             (throw (ex-info "v/ls :depth must be a non-negative integer"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :depth depth})))
         _ (when-not (and (integer? limit) (pos? limit))
             (throw (ex-info "v/ls :limit must be a positive integer"
                      {:type :ext.foundation.editing/invalid-ls-opts
                       :limit limit})))
         f (safe-path path)
         _ (when-not (.exists f)
             (throw (ex-info (str "Path not found: " (.getPath f))
                      {:type :ext.foundation.editing/path-not-found})))
         opts* {:hidden? hidden?
                :respect-gitignore? respect-gitignore?
                :ignore-node (when respect-gitignore? (load-ignore-node f))
                :root f}
         {:keys [entries truncated?]}
         (collect-flat-entries f opts* depth limit files-only? dirs-only?)
         file-count (count (filter #(= :file (:type %)) entries))
         dir-count  (count (filter #(= :dir  (:type %)) entries))]
     {:path          (rel-path f)
      :absolute-path (.getAbsolutePath f)
      :root-type     (if (.isDirectory f) :dir :file)
      :entries       entries
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
  "Pre-compile needles when `:regex? true`; nil otherwise (literal mode).
   Bad patterns surface as a structured invalid-rg-spec error."
  [needles regex?]
  (when regex?
    (mapv (fn [n]
            (try (java.util.regex.Pattern/compile n)
              (catch java.util.regex.PatternSyntaxException e
                (throw (ex-info (str "v/rg :regex? true — invalid regex pattern: "
                                  (pr-str n))
                         {:type :ext.foundation.editing/invalid-rg-spec
                          :pattern n
                          :reason (ex-message e)})))))
      needles)))

(defn- coerce-rg-spec
  "Coerce the single public v/rg spec map. Exactly one of :all or :any
   is required. Every public collection field is a vector; no shorthand
   arities or scalar paths.

   Optional keys (all default to off):
     :limit       max hits / files / count entries (default 250)
     :context N   shorthand for :before N + :after N
     :before N    lines of context BEFORE each hit (content mode only)
     :after  N    lines of context AFTER  each hit (content mode only)
     :files-only? true → return only distinct paths (no per-line hits)
     :counts?     true → return per-file match counts (no per-line hits)
     :regex?      true → interpret needles as java.util.regex patterns;
                  literal substring otherwise (default)."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "v/rg takes one spec map: {:all [...] :paths [...]}."
             {:type :ext.foundation.editing/invalid-rg-spec
              :got  (type spec)})))
  (let [allowed-keys #{:all :any :paths :include :exclude :hidden? :respect-gitignore?
                       :limit :context :before :after :files-only? :counts? :regex?}
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
                  (vector-of-strings :exclude nil))
        nonneg-int! (fn [k]
                      (when (contains? spec k)
                        (let [v (get spec k)]
                          (when-not (and (integer? v) (not (neg? v)))
                            (throw (ex-info (str "v/rg :" (name k) " must be a non-negative integer")
                                     {:type :ext.foundation.editing/invalid-rg-spec
                                      :field k :got v}))))))
        _ (run! nonneg-int! [:limit :context :before :after])
        limit-spec (:limit spec)
        _ (when (and limit-spec (not (pos? limit-spec)))
            (throw (ex-info "v/rg :limit must be a positive integer"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :field :limit :got limit-spec})))
        limit (or limit-spec default-grep-limit)
        context-shared (or (:context spec) 0)
        before-ctx (or (:before spec) context-shared)
        after-ctx  (or (:after  spec) context-shared)
        files-only? (boolean (:files-only? spec))
        counts?     (boolean (:counts? spec))
        _ (when (and files-only? counts?)
            (throw (ex-info "v/rg :files-only? and :counts? are mutually exclusive"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        _ (when (and (or files-only? counts?)
                  (or (pos? before-ctx) (pos? after-ctx)))
            (throw (ex-info "v/rg :before / :after / :context only apply to content mode (not :files-only? / :counts?)"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        regex? (boolean (:regex? spec))
        patterns (compile-needles needles regex?)]
    {:op op
     :needles needles
     :paths paths
     :include (or include [])
     :exclude (or exclude [])
     :hidden? (boolean (:hidden? spec))
     :respect-gitignore? (get spec :respect-gitignore? true)
     :limit limit
     :before-ctx before-ctx
     :after-ctx  after-ctx
     :files-only? files-only?
     :counts? counts?
     :regex? regex?
     :patterns patterns}))

(defn- make-line-matcher
  "Return a `(fn [line] boolean)` predicate. Default mode is literal
   substring (`str/includes?`); `:regex? true` uses pre-compiled
   `java.util.regex.Pattern` objects. `:all` means every needle must
   match the same line; `:any` means at least one."
  [op needles patterns regex?]
  (cond
    (and (= op :all) regex?)
    (fn [^String line] (every? #(re-find % line) patterns))

    (and (= op :any) regex?)
    (fn [^String line] (boolean (some #(re-find % line) patterns)))

    (= op :all)
    (fn [^String line] (every? #(str/includes? line %) needles))

    :else
    (fn [^String line] (boolean (some #(str/includes? line %) needles)))))

;; Per-line text cap for v/rg hits was retired (was 500 chars,
;; mirroring Roo Code). The cap had the same failure mode as the
;; trailer cap before it: the `…<+N chars>` marker made the model
;; perceive its own data as missing and chase a phantom "full line"
;; via extra v/cat roundtrips, even on normal source lines that
;; happened to brush the cap. The model owns its data — see the
;; trailer truncation note in `internal/ctx_renderer.clj`.
;;
;; Realistic corpus exposure is bounded by:
;;   - the hit cap (`:truncated-by :limit`, default 250 hits)
;;   - the model's choice to use `:files-only?` / `:counts?` /
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
  "Short-circuit: true on first matching line. Used by :files-only? mode
   so we exit each file as fast as possible."
  [^File f matches?]
  (try
    (with-open [r (io/reader f)]
      (boolean (some matches? (line-seq r))))
    (catch Throwable _ false)))

(defn- count-hits-in-file
  "Total matching lines in `f`. Used by :counts? mode — returns the
   real count regardless of the global :limit (since the limit caps
   FILE entries, not per-file lines)."
  [^File f matches?]
  (try
    (with-open [r (io/reader f)]
      (count (filter matches? (line-seq r))))
    (catch Throwable _ 0)))

(defn- grep-files
  "Search with the public v/rg spec map. Three output modes, picked by
   `:files-only?` / `:counts?` / (default content).

   Returns one of:
     {:hits   [{:path :line :text :before? :after?} ...] :truncated-by KW}  ;; content
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW}  ;; files-only
     {:counts [{:path P :count N} ...]                    :truncated-by KW}  ;; counts

   `:truncated-by` is `:limit` when the configured limit clamped the
   result, `:end-of-results` otherwise. Per-line `:text`, `:before`,
   and `:after` are verbatim — no per-line cap (see the per-line cap
   retirement note above)."
  [spec]
  (let [{:keys [op needles patterns paths include exclude hidden? respect-gitignore?
                limit before-ctx after-ctx files-only? counts? regex?]} (coerce-rg-spec spec)
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
        matches? (make-line-matcher op needles patterns regex?)
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
                (sort-by rel-path))]
    (cond
      files-only?
      (let [out (atom [])
            capped? (atom false)]
        (doseq [^File f files :while (not @capped?)]
          (when (file-has-any-hit? f matches?)
            (swap! out conj (rel-path f))
            (when (>= (count @out) limit) (reset! capped? true))))
        {:files (vec @out)
         :truncated-by (if @capped? :limit :end-of-results)})

      counts?
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
          (doseq [hit (search-file-content f matches? before-ctx after-ctx)
                  :while (not @capped?)]
            (swap! out conj hit)
            (when (>= (count @out) limit) (reset! capped? true))))
        {:hits (vec @out)
         :truncated-by (if @capped? :limit :end-of-results)}))))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{:path :search :replace})
(def ^:private patch-optional-keys
  "Optional keys recognised on exact-replace edit maps.
   - :after / :before  positional anchors (string; first exact occurrence)
   - :nth              :first | :last | :all | 1-based positive integer
   - :expected-mtime   epoch-ms; fail if file mtime differs (staleness guard)
   - :expected-size    bytes;    fail if file size differs (staleness guard)"
  #{:after :before :nth :expected-mtime :expected-size})

(def ^:private patch-allowed-keys
  (set/union patch-required-keys patch-optional-keys))

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
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))
                  unknown (seq (remove patch-allowed-keys (keys edit)))]
              (when missing
                (throw (ex-info "v/patch edit missing required keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit})))
              (when unknown
                (throw (ex-info "v/patch edit has unknown keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :unknown (vec unknown)
                          :allowed (vec patch-allowed-keys)
                          :edit edit}))))
            (let [nth-spec (:nth edit)]
              (when (and (some? nth-spec)
                      (not (or (#{:first :last :all} nth-spec)
                             (and (integer? nth-spec) (pos? nth-spec)))))
                (throw (ex-info "v/patch :nth must be :first, :last, :all or a positive integer"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :nth nth-spec :edit edit}))))
            (update edit :path str))
      edits)))

(defn- find-substring-positions
  "All 0-based char positions where `needle` appears in `haystack`.
   Throws on blank needle (would yield infinite matches)."
  [^String haystack ^String needle]
  (when (str/blank? needle)
    (throw (ex-info "v/patch :search must be non-blank"
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

(defn- nearest-match-context
  "For a failed edit, capture a small around-the-hit window so the model
   can see WHERE on disk its `:search` would have landed (or almost
   landed). Returns `{:line N :pass KW :context [[ln text] ...]}` or nil.
   Lines are 1-based; window is ±3 lines around the hit."
  [^String content ^String search]
  (when-let [hit (fuzzy-line-match content search)]
    (let [lines      (patch/split-content-lines content)
          start-line (long (:line-start hit))
          end-line   (long (:line-end hit))
          ctx-from   (max 0 (- start-line 3))
          ctx-to     (min (count lines) (+ end-line 3))]
      {:line (inc start-line)
       :pass (:pass hit)
       :indent-delta (:indent-delta hit)
       :context (mapv (fn [i] [(inc i) (nth lines i)])
                  (range ctx-from ctx-to))})))

;; -----------------------------------------------------------------------------
;; Per-path consecutive-failure tracker (Roo-style loop detector)
;;
;; A process-wide atom of `{absolute-path consecutive-fail-count}`. We bump
;; on every failed v/patch invocation that touched the path and reset to
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
    (str "Consecutive v/patch failures on " path ": " n
      ". STOP retrying with similar :search. Re-read the file (v/cat path :tail"
      " or with the offset shown above), then build ONE cohesive edit plan with"
      " :after/:before anchors or :nth selection. If you are rewriting the"
      " whole file, switch to (v/write).")))

;; -----------------------------------------------------------------------------
;; Staleness check: :expected-mtime / :expected-size
;; -----------------------------------------------------------------------------

(defn- staleness-check
  "Return nil when the file's on-disk mtime/size matches the edit's
   expectations (or no expectations were given), else a structured
   `:stale` failure carrying the actual vs. expected values."
  [^java.io.File file {:keys [expected-mtime expected-size]}]
  (let [actual-mtime (.lastModified file)
        actual-size  (.length file)]
    (cond
      (and (some? expected-mtime) (not= (long expected-mtime) actual-mtime))
      {:reason :stale-mtime :expected-mtime expected-mtime :actual-mtime actual-mtime
       :actual-size actual-size}

      (and (some? expected-size) (not= (long expected-size) actual-size))
      {:reason :stale-size :expected-size expected-size :actual-size actual-size
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

(defn- patch-analysis
  [edits]
  (let [edits (coerce-patch-edits edits)]
    (loop [idx 0
           remaining edits
           states {}
           checks []
           failures []]
      (if-let [{:keys [path search replace after before nth] :as edit} (first remaining)]
        (let [resolved (resolve-edit-target path)]
          (if-let [path-error (:error resolved)]
            (let [check {:edit-index idx
                         :path path
                         :reason (:reason path-error)
                         :path-error path-error
                         :search-preview (search-preview (str search))}]
              (recur (inc idx) (next remaining) states
                (conj checks check) (conj failures check)))
            (let [file    (:file resolved)
                  rel     (:rel resolved)
                  before-text (or (get-in states [path :before]) (slurp file))
                  current (or (get-in states [path :after]) before-text)
                  search  (str search)
                  replace (str replace)
                  stale   (when-not (contains? states path)
                            (staleness-check file edit))
                  all-positions (find-substring-positions current search)
                  anchor-result (filter-positions-by-anchors all-positions (count search) current
                                  {:after after :before before})
                  filtered-positions (:positions anchor-result)
                  selected (when (nil? (:anchor-error anchor-result))
                             (select-by-nth filtered-positions nth))
                  base-check {:edit-index idx
                              :path rel
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
                  (recur (inc idx) (next remaining) states
                    (conj checks check) (conj failures check)))

                (:anchor-error anchor-result)
                (let [check (assoc base-check :reason :anchor-not-found
                              :anchor-error (:anchor-error anchor-result))]
                  (recur (inc idx) (next remaining) states
                    (conj checks check) (conj failures check)))

                (seq selected)
                (let [new-content (apply-substring-replacements current selected (count search) replace)
                      check       (assoc base-check :applied-positions (vec selected) :pass :exact)]
                  (recur (inc idx) (next remaining)
                    (assoc states path {:file file
                                        :path rel
                                        :before before-text
                                        :after new-content})
                    (conj checks check) failures))

                ;; Zero or out-of-range -> try fuzzy if multi-line
                :else
                (if-let [fuzzy (and (zero? (count all-positions))
                                 (not after) (not before) (nil? nth)
                                 (fuzzy-line-match current search))]
                  (let [{:keys [char-start char-end pass indent-delta]} fuzzy
                        rewritten (adjust-replace-for-indent replace indent-delta)
                        ;; Line-based fuzzy matches whole lines, so the
                        ;; substring being replaced may include the trailing
                        ;; `\n` of the last matched line. If the model's
                        ;; `:replace` did not include a trailing newline
                        ;; (typical when authoring a SEARCH block by hand),
                        ;; preserve the line boundary by reapplying it.
                        matched-ends-with-nl? (and (> char-end 0)
                                                (= \newline (.charAt current (dec char-end))))
                        replace-ends-with-nl? (str/ends-with? rewritten "\n")
                        rewritten (if (and matched-ends-with-nl?
                                        (not replace-ends-with-nl?))
                                    (str rewritten "\n")
                                    rewritten)
                        new-content (str (subs current 0 char-start)
                                      rewritten
                                      (subs current char-end))
                        check (assoc base-check :pass pass
                                :indent-delta indent-delta
                                :applied-positions [char-start])]
                    (recur (inc idx) (next remaining)
                      (assoc states path {:file file
                                          :path rel
                                          :before before-text
                                          :after new-content})
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
                    (recur (inc idx) (next remaining) states
                      (conj checks check) (conj failures check))))))))
        {:plans (vals states)
         :checks checks
         :failures failures
         :valid? (empty? failures)}))))

(defn- explain-failure
  [{:keys [edit-index path matches filtered-matches reason anchors nearest stale anchor-error]}]
  (let [head (str "edit " edit-index " in " path)]
    (case reason
      :stale (str head
               " failed: file changed since :expected-" (name (:reason stale))
               " check (expected " (or (:expected-mtime stale) (:expected-size stale))
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
                  nearest (str " Nearest fuzzy candidate at line " (:line nearest)
                            " (pass " (name (:pass nearest)) ") - inspect context above."))
      :ambiguous-no-anchor (str head " failed: matched " matches
                             " time(s) and no :after/:before/:nth selector.")
      (str head " failed."))))

(defn- patch-failure-message
  [failures]
  (if (= 1 (count failures))
    (str "v/patch " (explain-failure (first failures)))
    (str "v/patch " (count failures) " edits failed; first: "
      (explain-failure (first failures)))))

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
  "Apply exact-replace v/patch edits to the filesystem.

   Returns a structured map; **never throws on normal failure paths**
   (no-match, anchor-not-found, stale mtime, file not found, path
   escape, ambiguous selection). Reserves exceptions for genuinely
   unexpected errors (`SCI` interrupt, disk full, etc.).

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
;; v/write — whole-file write primitive (create or overwrite)
;;
;; v/patch is great for surgical edits but awkward for full-file rewrites:
;; the model would otherwise read the file, use whole content as :search
;; and submit the new blob as :replace. v/write makes the common case
;; ergonomic: one tool, one map, atomic semantics.
;;
;; Shape (parity with v/patch result):
;;   {:success? true
;;    :plan   {:path :before :after :op}}
;;   {:success? false
;;    :failures [<failure-with-:reason>]
;;    :loop-hint <string-or-nil>
;;    :message  <human-readable>}
;;
;; The `:overwrite?` knob defaults to true. `:expected-mtime` /
;; `:expected-size` provide the same staleness guard as v/patch — pair
;; them with (:mtime / :size) from a prior v/cat for atomic
;; read-modify-write on existing files.
;; =============================================================================

(def ^:private write-required-keys #{:path :content})
(def ^:private write-optional-keys
  #{:expected-mtime :expected-size :overwrite?})
(def ^:private write-allowed-keys
  (set/union write-required-keys write-optional-keys))

(defn- coerce-write-args
  [args]
  (when-not (map? args)
    (throw (ex-info "v/write expects a single map argument"
             {:type :ext.foundation.editing/invalid-write-args
              :got  (type args)})))
  (let [missing (seq (remove #(contains? args %) write-required-keys))
        unknown (seq (remove write-allowed-keys (keys args)))]
    (when missing
      (throw (ex-info "v/write missing required keys"
               {:type :ext.foundation.editing/invalid-write-args
                :missing (vec missing)
                :args args})))
    (when unknown
      (throw (ex-info "v/write has unknown keys"
               {:type :ext.foundation.editing/invalid-write-args
                :unknown (vec unknown)
                :allowed (vec write-allowed-keys)
                :args args})))
    (when-not (string? (:content args))
      (throw (ex-info "v/write :content must be a string"
               {:type :ext.foundation.editing/invalid-write-args
                :got (type (:content args))}))))
  (update args :path str))

(defn write-safe
  "Whole-file write primitive: create a new file OR overwrite an
   existing one with `:content`. Returns a structured result; **never
   throws on normal failure paths** (file exists with overwrite? false,
   stale mtime/size, path escape).

   Required keys: `:path`, `:content` (string).
   Optional keys:
     :overwrite?       default true; when false and target exists
                       → :reason :exists
     :expected-mtime   staleness guard; mismatch → :reason :stale
     :expected-size    staleness guard; mismatch → :reason :stale

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
        overwrite? (if (contains? args :overwrite?) (:overwrite? args) true)
        expected-mtime (:expected-mtime args)
        expected-size  (:expected-size  args)
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
         :message  (str "v/write failed: " (:message perr))})
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
                    :message (str "v/write target is a directory: " rel)}

                   (and (not overwrite?) exists?)
                   {:reason :exists
                    :path rel
                    :message (str "v/write refused: " rel
                               " already exists and :overwrite? is false")}

                   (and exists? (some? expected-mtime)
                     (not= (long expected-mtime) (long actual-mtime)))
                   {:reason :stale
                    :stale  {:reason :stale-mtime
                             :expected-mtime expected-mtime
                             :actual-mtime actual-mtime
                             :actual-size actual-size}
                    :message (str "v/write refused: " rel
                               " mtime changed since :expected-mtime")}

                   (and exists? (some? expected-size)
                     (not= (long expected-size) (long actual-size)))
                   {:reason :stale
                    :stale  {:reason :stale-size
                             :expected-size expected-size
                             :actual-size actual-size
                             :actual-mtime actual-mtime}
                    :message (str "v/write refused: " rel
                               " size changed since :expected-size")})]
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

   Default-first design: `(v/cat path)` reads up to `default-cat-limit`
   (2000) lines from line 1, which is the WHOLE FILE for almost every
   source file in a normal repo. Reach for explicit slicing only when
   the file is bigger than 2000 lines OR you specifically want a
   middle/tail section.

   Arities:
     (v/cat path)                       — first 2000 lines (default — use this).
     (v/cat path :range start end)      — INCLUSIVE 1-based line range [start, end].
                                          Pick when you know both endpoints
                                          (e.g., a v/rg hit + :context window).
     (v/cat path :tail)                 — LAST 2000 lines.
     (v/cat path :tail n)               — LAST n lines.

   Result shape:
     {:vis.op :v/cat :path P :lines [[<line-number> <text>] …]
      :next-offset N? :eof? B :truncated? B :mtime :size}
   `:lines` carries `[ln text]` tuples — destructure with `[n t]`; no
   offset arithmetic. Filter content with `(filter (fn [[_ t]] …) :lines)`.
   Each line's text is verbatim — no per-line cap. `:next-offset` is nil
   at EOF or for tail; integer otherwise — pass to a follow-up
   `(v/cat path :range next-offset …)` to paginate. `:truncated?` is
   true when the 256KB byte cap chopped the window; paginate regardless.

   The legacy `(v/cat path n)` and `(v/cat path offset n)` arities were
   removed because they competed with `:range`. Use:
     (v/cat path :range 1 N)          ;; first N lines (was (v/cat path N))
     (v/cat path :range offset (+ offset n -1))  ;; was (v/cat path offset n)"
  ([path]
   (let [out (read-file path 1 default-cat-limit)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result (assoc out :vis.op :v/cat)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)}
        :presentation {:kind :source :path (:path out) :line-key :lines}})))
  ([path arg]
   (when-not (= arg :tail)
     (throw (ex-info "v/cat 2-arity must use :tail; for head/range use (v/cat path :range start end)"
              {:type :ext.foundation.editing/invalid-cat-args
               :got arg})))
   (let [out (tail-file path default-cat-limit)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result (assoc out :vis.op :v/cat)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}
        :presentation {:kind :source :path (:path out) :line-key :lines}})))
  ([path arg n]
   (when-not (= arg :tail)
     (throw (ex-info "v/cat 3-arity must use :tail; for head/range use (v/cat path :range start end)"
              {:type :ext.foundation.editing/invalid-cat-args
               :got arg})))
   (let [out (tail-file path n)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result (assoc out :vis.op :v/cat)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}
        :presentation {:kind :source :path (:path out) :line-key :lines}})))
  ([path range-kw start end]
   ;; (v/cat path :range start end) — INCLUSIVE start..end (both 1-based).
   (when-not (= range-kw :range)
     (throw (ex-info "v/cat 4-arity must use :range as the second arg"
              {:type :ext.foundation.editing/invalid-cat-args
               :got range-kw})))
   (when-not (and (integer? start) (integer? end)
               (pos? start) (pos? end)
               (<= start end))
     (throw (ex-info "v/cat :range start/end must be positive ints with start <= end"
              {:type :ext.foundation.editing/invalid-cat-args
               :start start :end end})))
   (let [n (inc (- (long end) (long start)))
         out (read-file path start n)]
     (tool-success
       {:op :v/cat
        :path path
        :kind :file
        :result (assoc out :vis.op :v/cat)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                   :range [start end]}
        :presentation {:kind :source :path (:path out) :line-key :lines}}))))

(defn- ls-tool
  "List a directory as a FLAT recursive entry list. Returns a plain map:
     {:vis.op :v/ls
      :path          P            — workspace-relative root path
      :absolute-path AP
      :root-type     :dir|:file
      :entries       [{:path :type :size} ...]  — entries under root
      :entry-count N :file-count F :dir-count D
      :truncated?    B            — true when the :limit clamped the walk
      :depth N :limit N}

   Default behaviour: recursive walk up to `:depth 10` and 3000 entries.
   Paths are workspace-relative (same shape as v/cat / v/patch :path);
   `:type :dir` carries no trailing slash on the path — the discriminator
   IS the `:type` field. Sort order: depth-first pre-order; within each
   directory, sub-directories first, then files, both alphabetical.

   Filter directly on `:entries`:
     (filter #(str/ends-with? (:path %) \".py\") (:entries r))
     (filter #(= :file (:type %)) (:entries r))
   no `tree-seq` walk needed.

   `(v/ls path)` reads with defaults. Opts can be supplied either as
   a trailing map OR as inline kwargs — BOTH calling conventions
   work and are equivalent (Clojure 1.11+ kwargs auto-coercion):
     (v/ls path {:depth 2 :files-only? true})   ;; map form
     (v/ls path :depth 2 :files-only? true)     ;; kwargs form

   Recognised opts (any combination):
     :depth N            — max recursion depth (default 10)
     :limit N            — stop after N entries (default 3000;
                           sets :truncated? true)
     :files-only? B      — emit only file entries (no directories)
     :dirs-only?  B      — emit only directory entries
     :hidden? B          — include dotfiles / dotdirs (default false)
     :respect-gitignore? B  default true"
  ([path & {:as opts}]
   (let [listing (list-files path opts)
         result  (assoc listing :vis.op :v/ls)]
     (tool-success
       {:op :v/ls
        :path path
        :kind :dir
        :result result
        :metadata  {:depth (:depth listing)
                    :limit (:limit listing)
                    :entry-count (:entry-count listing)
                    :file-count  (:file-count listing)
                    :dir-count   (:dir-count listing)
                    :truncated?  (:truncated? listing)
                    :hidden? (:hidden? opts)
                    :respect-gitignore? (get opts :respect-gitignore? true)}
        :presentation {:kind :flat-list}}))))

(defn- rg-tool
  "File-content search. Three output modes — default is content (hits with
   optional context); `:files-only? true` returns just distinct paths;
   `:counts? true` returns per-file match counts.

   Spec accepts BOTH calling conventions (Clojure 1.11+ kwargs
   auto-coercion):
     (v/rg {:any [\"a\"] :paths [\"src\"]})    ;; map form
     (v/rg :any [\"a\"] :paths [\"src\"])      ;; kwargs form

   Recognised keys:
     {:all [\"a\" \"b\"]      — AND: every needle on same line
      :any [\"a\" \"b\"]      — OR: at least one needle on a line
      :paths [\"src\"]        — search roots (default [\".\"])
      :include [\"**/*.clj\"] — glob filters (vector)
      :exclude [\"**/test/**\"]
      :hidden? false :respect-gitignore? true
      :limit 250            — cap hits / files / counts (default 250)
      :context N            — N lines before AND after each hit (alias)
      :before  N            — lines before each hit
      :after   N            — lines after  each hit
      :files-only? false    — return only distinct paths
      :counts?     false    — return per-file match counts
      :regex?      false}   — needles are java.util.regex patterns
   Exactly one of :all/:any. Strings are literal substrings by default;
   pass :regex? true to treat them as full regex (e.g. `\\bdef login\\b`).

   Result shape varies by mode (the tool envelope's `:result` always
   carries `:vis.op :v/rg` plus a `:mode` discriminator):
     content     (:mode :content)     {:hits [...]  :hit-count N  ...}
     files-only? (:mode :files-only)  {:files [...] :file-count N ...}
     counts?     (:mode :counts)      {:counts [...] :file-count N ...}"
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
                        "v/rg takes either a single spec map or inline kwargs (e.g. (v/rg :any [\"x\"] :paths [\"src\"]))."
                        {:type :ext.foundation.editing/invalid-rg-arity
                         :expected '([spec-map] [& kwargs])
                         :got args})))
        {:keys [paths include exclude files-only? counts? regex?
                before-ctx after-ctx limit] :as coerced} (coerce-rg-spec spec)
        out (grep-files spec)
        mode (cond files-only? :files-only
               counts?     :counts
               :else       :content)
        shared {:vis.op       :v/rg
                :mode         mode
                :truncated-by (:truncated-by out)
                :spec         spec
                :paths        paths
                :limit        limit
                :regex?       regex?}
        result (case mode
                 :content
                 (let [hits (vec (:hits out))]
                   (assoc shared
                     :hits hits
                     :hit-count (count hits)
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
      {:op :v/rg
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
                       :content    {:kind :search-hits :row-keys [:path :line :text]}
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
   fallback to keep `v/patch` result rendering from becoming the slow path."
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
  "Build a per-file summary map that lives on `:result` of `v/patch` /
   `v/write`.

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
  "Surgical file editing. Single input shape: a vector of edit maps (a
   single map is auto-wrapped). Each edit map carries:

     {:path P :search S :replace R
      :after \"context\"?  :before \"context\"?
      :nth :first|:last|:all|N?
      :expected-mtime MS?  :expected-size BYTES?}

   Replaces the FIRST occurrence by default. Use `:nth`, `:after` /
   `:before` anchors, or extend `:search` with context to target a
   specific occurrence. Multi-line `:search` gets a 5-pass fuzzy
   fallback (exact -> rstrip -> unicode -> relative-indent -> trim)
   when zero exact matches land.

   Companion primitives — each does ONE thing, no overlap with v/patch:
     v/write    whole-file create or overwrite
     v/move     rename / move
     v/delete   delete (or v/delete-if-exists)

   The full plan is validated against the live filesystem before any
   write — a single failure aborts the entire batch and no file is
   touched."
  [edits]
  (let [result (patch-safe edits)]
    (if (:success? result)
      (let [plans     (:plans result)
            summaries (mapv patch-result-file-summary plans)]
        (tool-success
          {:op :v/patch
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
           :op       :v/patch
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
     (v/write {:path P :content S})
     (v/write :path P :content S)

     (v/write {:path P :content S :overwrite? false})     fail if file exists
     (v/write {:path P :content S :expected-mtime MS})    staleness guard
     (v/write {:path P :content S :expected-size  BYTES}) staleness guard

   Returns the same per-file summary shape as `v/patch` (so the model
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
          {:op :v/write
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
           :op       :v/write
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
  "Ensure dir exists. Returns the canonical `v/*` map shape so the
   model destructures `(:path r)` / `(:created? r)` directly off the
   bound result."
  [path]
  (let [before (fs/exists? (safe-path path))
        out    (create-dirs-safe path)]
    (tool-success
      {:op :v/create-dirs
       :path path
       :kind :dir
       :result {:vis.op :v/create-dirs
                :path out
                :created? (not before)
                :already-existed? before}
       :metadata {:created? (not before)
                  :already-existed? before}})))

(defn- copy-tool
  "Copy path. Returns the canonical `v/*` map shape — `(:src r)` /
   `(:dest r)` / `(:path r)` (alias for dest).

   Opts accept BOTH calling conventions (Clojure 1.11+ kwargs
   auto-coercion):
     (v/copy src dest {:overwrite? true})
     (v/copy src dest :overwrite? true)"
  ([src dest & {:as opts}]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :v/copy
        :path dest
        :kind :path
        :result {:vis.op :v/copy
                 :src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- move-tool
  "Move/rename path. Returns the canonical `v/*` map shape —
   `(:src r)` / `(:dest r)` / `(:path r)` (alias for dest).

   Opts accept BOTH calling conventions:
     (v/move src dest {:overwrite? true})
     (v/move src dest :overwrite? true)"
  ([src dest & {:as opts}]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :v/move
        :path dest
        :kind :path
        :result {:vis.op :v/move
                 :src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- delete-tool
  "Delete `path`. Returns the map every `v/*` tool returns so the
   channel renderer (and `(def r (v/delete p))` consumers) can read
   `(:path r)` and `(:deleted? r)` straight off. Previously this
   returned `nil` and the channel preview painted `DELETE nil` —
   same parity bug we already fixed in `v/exists?`."
  [path]
  (delete-safe path)
  (tool-success
    {:op :v/delete
     :path path
     :kind :path
     :result {:vis.op :v/delete :path path :deleted? true}
     :metadata {:deleted? true}}))

(defn- delete-if-exists-tool
  "Delete `path` if present. Returns the map every `v/*` tool returns
   so destructuring (and the channel renderer) can read `(:path r)`
   and `(:deleted? r)` directly. Previously this returned a bare
   boolean which broke `(def r (v/delete-if-exists p))` consumers
   (same lesson as `v/exists?`)."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :v/delete-if-exists
       :path path
       :kind :path
       :result {:vis.op :v/delete-if-exists :path path :deleted? deleted?}
       :metadata {:deleted? deleted?}})))

(defn- exists-tool
  "Filesystem existence check.

   Returns `{:vis.op :v/exists? :path P :exists? B}` so the model
   destructures the same shape every `v/*` tool uses (industry parity
   with `v/cat`, `v/ls`, `v/rg`, …). Earlier this returned a bare
   boolean, which broke `(def r (v/exists? P))` consumers that
   reached for `(:exists? r)` — a wholly reasonable assumption given
   the surrounding map-shaped `v/*` API. See conversation
   11d4f817-fbd1-43ab-a6b4-052c8557af0a turn 4 iter 1→2."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :v/exists?
       :path path
       :kind :path
       :result {:vis.op :v/exists?
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
  "Render one flat v/ls row as `path/[trailing-slash-for-dirs] (Nb)`.
   Trailing slash is RENDERER convention (Roo / cline style) so directory
   rows read as filesystem paths; the underlying data shape carries
   `:type :dir` for programmatic discrimination instead."
  [{:keys [path type size]}]
  (let [dir? (= :dir type)]
    (str path
      (when dir? "/")
      (when (and (not dir?) (some? size)) (str "  (" size "B)")))))

(defn- numbered-line-block
  "Format a vec of `[line-number, text]` tuples as `<ln>: <text>` lines."
  [tuples]
  (->> tuples
    (map (fn [[ln s]] (str ln ": " s)))
    (str/join "\n")))

;; ---------------------------------------------------------------------------
;; Per-symbol renderers
;;
;; Engine contract:
;;   render-fn -> (fn [result] [:ir ...])
;;
;; `result` is the raw payload returned to SCI. Engine handles
;; `:success? false` separately — error fns are optional and fall back to
;; `default-error-ir`. The MODEL surface is the per-iteration trailer
;; (real SCI values via pr-str); there is no second model-side render.
;; ---------------------------------------------------------------------------

(defn- channel-render-cat
  "Channel preview: numbered-line-block + badge header. Reads the
   plain map directly; no handle/deref."
  [{:keys [path next-offset truncated? lines]}]
  (let [lines      (vec lines)
        line-count (count lines)
        first-ln   (ffirst lines)
        body       (numbered-line-block lines)]
    (ir-root
      (ir-p (ir-strong "CAT")
        "  " (ir-code (or path "?"))
        "  " line-count " line" (when (not= 1 line-count) "s")
        (when first-ln (str "  from=" first-ln))
        (cond
          next-offset (str "  next-offset=" next-offset
                        (when truncated? "  (byte-cap)"))
          truncated?  "  (byte-cap)"
          :else       "  (eof)"))
      (ir-code-block "text" (bounded-render-text body)))))

(defn- channel-render-ls
  "Channel preview: flat path list with badge header. Directory
   rows get a trailing `/`, file rows get a `(NB)` size suffix.
   `:truncated?` surfaces an inline note. Pure projection of
   `(:entries r)`."
  [{:keys [path entries entry-count file-count dir-count truncated?
           depth limit]}]
  (ir-root
    (ir-p (ir-strong "LS")
      "  " (ir-code (or path "?"))
      "  " entry-count " entr" (if (= 1 entry-count) "y" "ies")
      "  files=" (or file-count 0)
      "  dirs=" (or dir-count 0)
      (when (and depth (not= depth 10)) (str "  depth=" depth))
      (when truncated?
        (str "  truncated" (when limit (str "=" limit)))))
    (when (seq entries)
      (ir-code-block "text"
        (bounded-render-text
          (str/join "\n" (map flat-entry-line entries)))))))

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
  "Channel preview — mode-aware with badge header. Content mode
   renders each hit with its `:before` / `:after` context (when
   present) so the trailer reads like a miniature grep -C output.
   `:files-only` shows distinct paths. `:counts` shows per-file
   totals."
  [{:keys [mode hits files counts truncated-by hit-count file-count
           total-matches]}]
  (case mode
    :files-only
    (ir-root
      (ir-p (ir-strong "RG files")
        "  " (or file-count (count files)) " file"
        (when (not= 1 (or file-count (count files))) "s")
        "  truncated-by=" (name (or truncated-by :none)))
      (when (seq files)
        (ir-code-block "text"
          (bounded-render-text (str/join "\n" files)))))

    :counts
    (ir-root
      (ir-p (ir-strong "RG counts")
        "  " (or file-count (count counts)) " file"
        (when (not= 1 (or file-count (count counts))) "s")
        (when total-matches (str "  total=" total-matches))
        "  truncated-by=" (name (or truncated-by :none)))
      (when (seq counts)
        (ir-code-block "text"
          (bounded-render-text
            (str/join "\n"
              (map (fn [{:keys [path count]}] (format "%-50s %d" path count))
                counts))))))

    ;; default: :content (or unset — legacy maps without :mode)
    (ir-root
      (ir-p (ir-strong "RG")
        "  " (or hit-count (count hits)) " hit"
        (when (not= 1 (or hit-count (count hits))) "s")
        "  truncated-by=" (name (or truncated-by :none)))
      (when (seq hits)
        (let [rendered (mapv render-rg-hit-block hits)
              any-context? (some #(or (seq (:before %)) (seq (:after %))) hits)]
          (if any-context?
            ;; Context-rich: one labelled block per hit.
            (apply ir-root
              (mapcat (fn [[head body]]
                        [(ir-p (ir-code head))
                         (ir-code-block "text" (bounded-render-text body))])
                rendered))
            ;; Plain: flat one-line-per-hit block, like the old renderer.
            (ir-code-block "text"
              (bounded-render-text
                (str/join "\n"
                  (map (fn [{:keys [path line text]}]
                         (str path ":" line " " text)) hits))))))))))

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
    (apply ir-root
      (ir-p (ir-strong "PATCH")
        "  " nf " file" (when (not= 1 nf) "s")
        (when (pos? changed) (str "  changed=" changed)))
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
        files))))

;; ---------------------------------------------------------------------------
;; Mutation tool renderers — strict map shape only.
;;
;; Every `v/*` mutation tool returns `{:vis.op K :path P ...}` (see
;; `create-dirs-tool` / `copy-tool` / `move-tool` / `delete-tool` /
;; `delete-if-exists-tool` / `exists-tool`). Renderers destructure
;; that map directly; bare strings, bare booleans, and nil are NOT
;; supported shapes — if they show up the renderer fails loudly so
;; the boundary bug surfaces at write time, not at paint time.
;; ---------------------------------------------------------------------------

(defn- channel-render-create-dirs
  [{:keys [path]}]
  (ir-root (ir-p (ir-strong "MKDIR") "  " (ir-code path))))

(defn- channel-render-copy
  [{:keys [src dest path]}]
  (let [target (or dest path)]
    (ir-root (ir-p (ir-strong "COPY")
               (when src (str "  " src))
               "  → " (ir-code target)))))

(defn- channel-render-move
  [{:keys [src dest path]}]
  (let [target (or dest path)]
    (ir-root (ir-p (ir-strong "MOVE")
               (when src (str "  " src))
               "  → " (ir-code target)))))

(defn- channel-render-delete
  [{:keys [path]}]
  (ir-root (ir-p (ir-strong "DELETE") "  " (ir-code path))))

(defn- channel-render-delete-if-exists
  [{:keys [path deleted?]}]
  (ir-root (ir-p (ir-strong (if deleted? "DELETE" "ABSENT"))
             "  " (ir-code path))))

(defn- channel-render-exists?
  [{:keys [path exists?]}]
  (ir-root (ir-p (ir-strong (if exists? "EXISTS" "MISSING"))
             "  " (ir-code path))))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Symbol declarations.
;;
;; Each underlying `xxx-tool` defn carries the canonical docstring + arglists
;; on its var. `vis/symbol` reads them straight from the var meta - the
;; SCI sandbox sees the same text the prompt-listing renders.
;; `:symbol` overrides the var name (`cat-tool` -> `cat`) for the model-facing
;; surface; everything else (examples, render-fn, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(def cat-symbol
  (vis/symbol #'cat-tool
    {:symbol 'cat
     :before-fn (path-protected-before-fn :v/cat :file :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-cat
     :on-error-fn (tool-failure-on-error :v/cat :file nil)}))

(def ls-symbol
  (vis/symbol #'ls-tool
    {:symbol 'ls
     :before-fn (path-protected-before-fn :v/ls :dir :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-ls
     :on-error-fn (tool-failure-on-error :v/ls :dir nil)}))

(def rg-symbol
  (vis/symbol #'rg-tool
    {:symbol 'rg
     :before-fn (path-protected-before-fn :v/rg :dir :read rg-arg-paths)
     :tag :observation
     :render-fn channel-render-rg
     :on-error-fn (tool-failure-on-error :v/rg :dir nil)}))

(def patch-symbol
  (vis/symbol #'patch-tool
    {:symbol 'patch
     :before-fn (path-protected-before-fn :v/patch :file :write patch-arg-paths)
     :tag :mutation
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :v/patch :file nil)}))

(def write-symbol
  ;; v/write reuses the v/patch channel renderer because its `:result`
  ;; shape is the same single-file summary (just always 1-file long).
  (vis/symbol #'write-tool
    {:symbol 'write
     :before-fn (path-protected-before-fn :v/write :file :write write-arg-paths)
     :tag :mutation
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :v/write :file nil)}))

(def create-dirs-symbol
  (vis/symbol #'create-dirs-tool
    {:symbol 'create-dirs
     :before-fn (path-protected-before-fn :v/create-dirs :dir :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-create-dirs
     :on-error-fn (tool-failure-on-error :v/create-dirs :dir nil)}))

(def copy-symbol
  (vis/symbol #'copy-tool
    {:symbol 'copy
     :before-fn (path-protected-before-fn :v/copy :path :write first-two-arg-paths)
     :tag :mutation
     :render-fn channel-render-copy
     :on-error-fn (tool-failure-on-error :v/copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
    {:symbol 'move
     :before-fn (path-protected-before-fn :v/move :path :write first-two-arg-paths)
     :tag :mutation
     :render-fn channel-render-move
     :on-error-fn (tool-failure-on-error :v/move :path nil)}))

(def delete-symbol
  (vis/symbol #'delete-tool
    {:symbol 'delete
     :before-fn (path-protected-before-fn :v/delete :path :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-delete
     :on-error-fn (tool-failure-on-error :v/delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol #'delete-if-exists-tool
    {:symbol 'delete-if-exists
     :before-fn (path-protected-before-fn :v/delete-if-exists :path :write first-arg-paths)
     :tag :mutation
     :render-fn channel-render-delete-if-exists
     :on-error-fn (tool-failure-on-error :v/delete-if-exists :path nil)}))

(def exists?-symbol
  (vis/symbol #'exists-tool
    {:symbol 'exists?
     :before-fn (path-protected-before-fn :v/exists? :path :read first-arg-paths)
     :tag :observation
     :render-fn channel-render-exists?
     :on-error-fn (tool-failure-on-error :v/exists? :path nil)}))

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
    ["`v/` editing tools. Canonical path only; strategies live as data."
     ""
     "CANONICAL FLOW"
     "  Discover repo shape first:"
     "    (v/ls \".\" {:depth 2})"
     "  Locate files from workspace root (default path is .):"
     "    (v/rg {:any [P] :files-only? true})"
     "  Open normal source files whole by default:"
     "    (v/cat path)"
     "  For known large files only, use one 400-500 line range:"
     "    (v/cat path :range start end)"
     "  Patch surgically, then use the returned diff as write evidence:"
     "    (v/patch [{:path P :search S :replace R}])"
     "  Create/replace whole files only when full content is known:"
     "    (v/write {:path P :content S})"
     ""
     "INVARIANTS"
     "  - One canonical call shape: maps for option-bearing tools."
     "  - Do not assume `src`; root search/listing first."
     "  - Do not re-cat after patch/write; diff is evidence."
     "  - Side effect in own iteration; done in next iteration."]))

(def editing-symbols
  "Default editing symbol set for docs/tests."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests."
  (available-editing-prompt))
