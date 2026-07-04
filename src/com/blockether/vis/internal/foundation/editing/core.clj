(ns com.blockether.vis.internal.foundation.editing.core
  "Filesystem tools exposed as bare symbols in the Python sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (cat path)            ; -> {:path :anchors {<N:hash> text…} :next-offset N? :truncated? B}
        (cat path n)          ; first n lines from line 1
        (cat path offset n)   ; n lines starting at line `offset` (1-based)
        (cat path :tail)      ; last 400 lines (tail)
        (cat path :tail n)    ; last n lines
        (ls path)             ; -> nested {:name :path :type :size :children} tree
        (ls path opts)        ; opts is {:depth :is_hidden :is_respect_gitignore}
        (rg query)           ; -> content hits; query = a term or list of terms (OR),
                               ; smart-case substring. Opts: paths/include/context/is_files_only

   2. Cwd-safe wrappers over the babashka.fs file API. `patch` is
      the canonical text edit surface:

        (cat path)
        (patch [{:path path :from_anchor anchor :replace new}])
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
   [com.blockether.fff :as fff]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
   [com.blockether.vis.internal.foundation.editing.outline :as outline]
   [com.blockether.vis.internal.foundation.editing.structural :as structural]
   [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
   [com.blockether.vis.internal.foundation.environment.core :as environment]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.git :as git]
   [com.blockether.vis.internal.paths :as paths]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (com.github.difflib DiffUtils UnifiedDiffUtils)
   (java.io File)
   (org.eclipse.jgit.ignore IgnoreNode IgnoreNode$MatchResult)))

;; Tools in this namespace (cat/patch/write/move/…) can execute DEFERRED on a
;; virtual thread that has entered the GraalPy polyglot Context — e.g. inside
;; `await gather(cat(a), cat(b))`. While on a context-entered thread, GraalVM's
;; HostAccess DENIES reflective Java calls (clojure.lang.Reflector → "Cannot
;; reflectively invoke …"). So every Java interop call here MUST compile to a
;; direct invokevirtual (type-hinted), never a reflective one. Keep this on.
(set! *warn-on-reflection* true)

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private default-grep-limit 250)
(def ^:private max-rg-result-bytes
  "Total-bytes ceiling on a content-mode rg result — pi DEFAULT_MAX_BYTES parity
   (50KB). Once the accumulated hit text crosses it, rg stops and marks
   `:truncated-by :bytes`, so a broad search returns a SMALL, useful slice
   instead of 250 fat hits the wire clip would then chop mid-structure."
  (* 50 1024))

(def ^:private rg-fff-scan-timeout-ms
  "Ceiling on how long `rg-fff-open` blocks for fff's initial scan (paths +
   content index) to COMPLETE before the instance is usable. wait-for-scan
   returns false on timeout; a half-built index silently under-reports
   grep/search hits, so past this ceiling we fail loud instead of searching a
   partial index."
  30000)

;; rg is fff-first: fff owns workspace discovery/ranking, this namespace only
;; re-reads returned candidate files to preserve exact line semantics + patch
;; anchors. We deliberately do NOT cache fff instances and disable fff's
;; on-disk mmap cache (`:enable-mmap-cache? false`): a cached/persisted
;; snapshot from a `:watch? false` instance silently goes stale and never sees
;; files written after its first scan — the `rg returns nothing that should not
;; be empty` bug (verified). A cold create + full scan of the whole repo is
;; ~11ms, so a FRESH instance per search is effectively free and always
;; current. Callers MUST close it (use `with-open`).
(defn- rg-fff-open
  "Create a FRESH fff instance scoped to `root`, blocking until its initial
   scan completes. The caller owns the instance and must close it."
  ^java.io.Closeable [^File root]
  (when-not (.isDirectory root)
    (throw (ex-info "rg fff index root must be a directory"
             {:type :ext.foundation.editing/invalid-rg-root
              :path (.getPath root)})))
  ;; Backstop for EVERY caller: a full fff scan of $HOME or a filesystem root
  ;; never finishes and hangs the tool (~30s wait-for-scan timeout, then a
  ;; useless partial index). Refuse fast; callers skip the root.
  (when (paths/pathological-index-root? root)
    (throw (ex-info "refusing to fff-index the home directory or a filesystem root"
             {:type :ext.foundation.editing/pathological-root
              :path (.getPath root)})))
  (let [k (.getCanonicalPath root)]
    (let [idx (try
                (fff/create {:base-path k
                             :watch? false
                             :ai-mode? true
                             :enable-content-indexing? true
                             :enable-mmap-cache? false})
                (catch Throwable t
                  (throw (ex-info (str "rg requires fff for directory search, but fff failed for " k)
                           {:type :ext.foundation.editing/fff-unavailable
                            :path k}
                           t))))]
      (when-not (fff/wait-for-scan idx rg-fff-scan-timeout-ms)
        (.close ^java.io.Closeable idx)
        (throw (ex-info "rg fff scan did not complete in time"
                 {:type :ext.foundation.editing/fff-scan-timeout
                  :path k
                  :timeout-ms rg-fff-scan-timeout-ms})))
      idx)))

(defn- rg-fff-candidate-files [roots needles files]
  (let [by-canon (into {}
                   (map (fn [^File f] [(.getCanonicalPath f) f]))
                   files)
        queries needles
        mode :plain
        rel-paths (fn [base items]
                    (keep (fn [{:keys [relative-path]}]
                            (some-> (io/file base relative-path)
                              .getCanonicalPath))
                      items))
        candidate-keys
        (->> roots
          (mapcat
            (fn [^File root]
              (cond
                (.isFile root)
                [(.getCanonicalPath root)]

                ;; Never fff-scan $HOME or a filesystem root — the walk never
                ;; finishes and hangs the tool. Skip this root; other (real
                ;; project) roots still search normally.
                (paths/pathological-index-root? root)
                nil

                :else
                (with-open [idx (rg-fff-open root)]
                  (let [base (.getCanonicalFile root)]
                      ;; doall: realize the lazy hits INSIDE with-open, before
                      ;; the fresh instance is closed.
                    (doall
                      (mapcat
                        (fn [query]
                          (let [path-items (:items (fff/search idx {:query query
                                                                    :page-size 1000}))
                                grep-items (:matches (fff/grep idx {:query query
                                                                    :mode mode
                                                                    :page-limit 1000
                                                                    :max-matches-per-file 1
                                                                    :time-budget-ms 1500}))]
                            (concat (rel-paths base path-items)
                              (rel-paths base grep-items))))
                        queries)))))))
          distinct)]
    (vec (keep by-canon candidate-keys))))

(def ^:private default-find-limit 50)
(def ^:private default-list-depth 10)
(def ^:private default-list-limit 3000)

;; cat pagination contract:
;;   `default-cat-limit`     - lines per window when the model omits `n`.
;;                             Industry parity — Claude Code / Roo Code use
;;                             2000 by default; Cline uses 1000.
;;   `max-cat-window-bytes`  - hard ceiling on a single window's bytes.
;;                             50KB — pi (@mariozechner/pi-coding-agent) parity:
;;                             whichever of lines/bytes is hit first ends the
;;                             window. Doubles as the persistence-blob ceiling:
;;                             each call's result is Nippy-frozen into the
;;                             iteration's `forms` BLOB, bounded by this.
;;                             Not user-tunable; it is the storage contract.
;;
;; There is no `max-line-length` per-line cap (no 2000-char + `…<+N chars
;; truncated>` marker). Such a cap produces the same failure pattern
;; as the absent trailer/rg caps (see ctx_renderer.clj header): a
;; silent ellipsis makes the model perceive its own data as missing and
;; chase phantom roundtrips even on legitimate long source lines.
;; The structural defense is the per-window byte cap above — a single
;; pathological line is included whole (so the model sees actual data)
;; and the next iteration stops with `:truncated? true :next-offset N`,
;; which the model already knows how to page through.
(def ^:private default-cat-limit 2000)
(def ^:private max-cat-window-bytes (* 50 1024)) ; 50KB — pi parity

;; =============================================================================
;; Path safety
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` and confine it to the union of ALLOWED ROOTS: the primary
  ;; workspace cwd plus any extra filesystem roots bound for this turn. Relative
  ;; paths resolve against the primary root; an absolute path is taken as-is so
  ;; it may land under an added filesystem root. The confinement check runs on
  ;; CANONICAL paths (symlinks resolved, e.g. macOS /tmp -> /private/tmp) so it
  ;; matches the canonical allowed roots AND a symlink that points outside every
  ;; root is rejected. `..` traversal that escapes all roots is rejected too.
  (when (str/blank? (str p))
    (throw (ex-info "Path is nil or blank - cat/rg/ls take a concrete path string; note rg returns a MAP, so use (:files r) or (keys (:matches r)), not the rg result itself"
             {:type :ext.foundation.editing/blank-path :path p})))
  (let [cwd       (workspace/cwd)
        canon     (fn ^java.nio.file.Path [x] (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))
        ^java.nio.file.Path cwd-canon (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd))))))
        ;; relative → under cwd; absolute → as-is. Canonical throughout so
        ;; symlinks (/tmp→/private/tmp) and `..` resolve before confinement.
        ^java.nio.file.Path canonical (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd (str p)))))))
        mappings  (workspace/filesystem-root-mappings)
        ;; The model addresses a context file by its REAL (trunk) path; remap it
        ;; transparently onto the rift clone where edits land. The remapped
        ;; target is ALWAYS under an allowed clone root, so confinement holds.
        ^java.nio.file.Path target
        (or
          (when (.startsWith canonical cwd-canon) canonical)
          (some (fn [{:keys [clone]}]
                  (let [^java.nio.file.Path cp (canon clone)] (when (.startsWith canonical cp) canonical)))
            mappings)
          (some (fn [{:keys [trunk clone]}]
                  (let [^java.nio.file.Path tp (canon trunk) ^java.nio.file.Path cp (canon clone)]
                    (when (and (not= tp cp) (.startsWith canonical tp))
                      (.resolve cp (.relativize tp canonical)))))
            mappings))]
    (when-not target
      (throw (ex-info (str "Path '" p "' escapes the allowed workspace roots")
               {:type :ext.foundation.editing/path-escape :path (str p)})))
    (.toFile target)))

(defn- ensure-existing-file! ^File [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.foundation.editing/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.foundation.editing/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  ;; Reverse of safe-path's remap so the address the model SEES round-trips:
  ;; a file under the primary cwd renders RELATIVE; a file under a context
  ;; CLONE renders as its REAL (trunk) absolute path — never the ~/.vis/drafts
  ;; clone path. Anything else falls back to the absolute path.
  (let [canon     (fn ^java.nio.file.Path [x] (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))
        ^java.nio.file.Path cwd-canon (canon (workspace/cwd))
        ^java.nio.file.Path p         (.toPath (.getCanonicalFile f))]
    (cond
      (.startsWith p cwd-canon)
      (let [rel (paths/unixify (.relativize cwd-canon p))]
        (if (str/blank? rel) "." rel))

      :else
      (or (some (fn [{:keys [trunk clone]}]
                  (let [^java.nio.file.Path cp (canon clone)
                        ^java.nio.file.Path tp (canon trunk)]
                    (when (.startsWith p cp)
                      (paths/unixify (.resolve tp (.relativize cp p))))))
            (workspace/filesystem-root-mappings))
        (str p)))))

(defn- resolve-search-roots
  "Resolve rg/find `paths` to canonical root Files. The DEFAULT/unscoped
   `[\".\"]` expands to the FULL allowed-roots set — the primary cwd PLUS
   every bound filesystem-root clone — so an unscoped search sweeps ALL
   filesystem roots, not just the primary. Explicit paths resolve through
   `safe-path` (confinement + trunk↔clone remap).

   A search is FORGIVING about a MISSING path: the model routinely lists
   speculative candidates (`[\"deps.edn\" \"vis.edn\" \"src\"]`) where one may not
   exist — those are SKIPPED so the search still runs over the paths that DO
   exist. Only when NONE of the given paths exist is it an error (a confinement
   violation from `safe-path` still propagates — that's not a miss)."
  [paths]
  (if (= paths ["."])
    (mapv io/file (workspace/allowed-roots))
    (let [existing (filterv #(.exists ^File %) (map safe-path paths))]
      (when (empty? existing)
        (throw (ex-info (str "None of these paths exist: " (str/join ", " paths))
                 {:type :ext.foundation.editing/path-not-found
                  :paths (vec paths)})))
      (mapv #(.getCanonicalFile ^File %) existing))))

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
  (let [a (first args)]
    (cond
      (map? a)    (when-let [path (:path a)] [path])
      (string? a) [a]
      :else       nil)))

(defn- find-arg-paths
  [args]
  (let [a (first args)
        opts (second args)
        spec (cond
               (map? a) a
               (map? opts) opts
               :else nil)
        paths (cond
                (contains? spec :paths) (:paths spec)
                (contains? spec :path)  (:path spec)
                :else nil)]
    (cond
      (nil? paths) ["."]
      (sequential? paths) paths
      :else [paths])))

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

(defn- plan-gated-before-fn
  "Path-protection for write-intent ops (patch / write / move / delete)."
  [op kind access path-extractor]
  (path-protected-before-fn op kind access path-extractor))

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
    ;; JGit's IgnoreNode matches gitignore patterns against `/`-separated
    ;; paths — a Windows `\` would never match, leaking ignored files.
    (let [rel (paths/unixify (.relativize (.toPath root) (.toPath f)))
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
              :anchors      (patch/lines->anchors lines)
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

(defn- anchor-read-error-message
  "Human message for a `patch/resolve-anchor-range` `:error` on the cat READ
   path - mirrors the patch hash-error copy and always points back to a
   fresh read for current `:anchors`."
  [{:keys [reason which hash lines from-line to-line stated-line found-lines anchor]}]
  (case reason
    :hashline-malformed
    (str "cat hash failed: " (name which) "_anchor " (pr-str anchor)
      " is not a `lineno:hash` anchor - hashline needs BOTH coordinates."
      " Re-read with cat(path) for fresh `lineno:hash` anchors.")
    :hashline-not-found
    (str "cat hash failed: " (name which) "_anchor hash " (pr-str hash)
      " matches no line (the line changed or the file moved)."
      " Re-read with cat(path) or cat(path, {\"tail\": N}) for fresh `lineno:hash` anchors.")
    :hashline-misplaced
    (str "cat hash failed: " (name which) "_anchor " (pr-str hash)
      " says line " stated-line " but that content is at line(s) " (pr-str found-lines)
      " - stale/misattributed anchor. Re-read with cat(path) for fresh `lineno:hash` anchors.")
    :hashline-ambiguous
    (str "cat hash failed: " (name which) "_anchor hash " (pr-str hash)
      " matches " (count lines) " lines " (pr-str lines)
      " near that line. Use cat(path, {\"range\": [start, end]}) instead.")
    :hashline-range-inverted
    (str "cat hash failed: to_anchor line " to-line
      " precedes from_anchor line " from-line ".")
    (str "cat :anchor failed: " (pr-str reason))))

(defn- read-file-by-anchor
  "Read the inclusive window between the lines hashed `from_anchor`..`to_anchor`
   (`to_anchor` defaults to `from_anchor` — a single line). Resolves the hashes
   against LIVE file content via `patch/resolve-anchor-range`, so the read
   addresses lines BY CONTENT, not by drifting line numbers — the symmetric
   counterpart of `patch :from_anchor`. Returns the same shape as `read-file`
   plus `:range [from-line to-line]`. Throws ex-info on a missing / ambiguous /
   inverted hash; the message points back to a fresh read."
  [path from_anchor to_anchor]
  (let [f       (ensure-existing-file! (safe-path path))
        content (slurp f)
        res     (patch/resolve-anchor-range content (str from_anchor)
                  (when to_anchor (str to_anchor)))]
    (if-let [err (:error res)]
      (throw (ex-info (anchor-read-error-message err)
               (merge {:type :ext.foundation.editing/invalid-cat-args} err)))
      (let [{:keys [from-line to-line]} res
            n (inc (- (long to-line) (long from-line)))]
        (as-> (read-file path from-line n) out
          (assoc out :range [from-line to-line]
            :anchors (patch/lines->anchors (:lines out))))))))

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
     :anchors      (patch/lines->anchors (vec (mapcat :lines windows)))
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
               :anchors      (patch/lines->anchors numbered)
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
      (sort-by (juxt #(if (.isDirectory ^File %) 0 1) #(.getName ^File %)) kids))))

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

(defn- check-interrupt!
  "Throw `InterruptedException` when the worker thread has been interrupted
   (e.g. by `cancel!` cancelling the turn's worker future). Long recursive
   directory walks poll this so Esc aborts them promptly instead of running
   the whole tree to completion - the symptom when `/fs add ..` widens the
   session onto a huge parent tree and the spinner hangs on 'cancelling'."
  []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "directory walk cancelled"))))

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
               (check-interrupt!)
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
;; find
;; =============================================================================

(def ^:private find-min-score
  "Minimum per-token match density (0.0–1.0) a fuzzy hit must reach to survive.
   fff's native matcher returns a full page of loose subsequence matches with no
   score of its own (e.g. query \"lmstudio\" matches 108/489 unrelated paths);
   below this floor a path is treated as noise and dropped."
  0.4)

(defn- find-norm
  "Lower-case `s` stripped to `[a-z0-9]` — separators and case removed so scoring
   compares bare identifier characters."
  ^String [s]
  (-> (or s "") str/lower-case (str/replace #"[^a-z0-9]" "")))

(defn- find-subseq-window
  "Length of the SMALLEST span in `hay` that contains `needle` as an ordered
   subsequence, or nil when `needle` is not a subsequence of `hay`. A tight span
   (few gaps) means a strong match; a sprawling one means scattered noise."
  [^String needle ^String hay]
  (let [n (count needle) h (count hay)]
    (when (pos? n)
      (loop [s 0 best nil]
        (if (< s h)
          (if (= (.charAt hay s) (.charAt needle 0))
            (let [end (loop [i (inc s) k 1]
                        (cond (= k n)                          (dec i)
                          (>= i h)                         nil
                          (= (.charAt hay i) (.charAt needle k)) (recur (inc i) (inc k))
                          :else                            (recur (inc i) k)))]
              (recur (inc s) (if (and end (or (nil? best) (< (- end s) best)))
                               (- end s) best)))
            (recur (inc s) best))
          (some-> best inc))))))

(defn- find-token-score
  "Best subsequence-window density of `token` against the file NAME (full weight)
   or the whole PATH (0.6 weight — a directory hit is weaker than a name hit).
   0.0 when the token is absent entirely."
  [^String token ^String path-norm ^String name-norm]
  (let [wp (find-subseq-window token path-norm)
        wf (find-subseq-window token name-norm)]
    (if (nil? wp)
      0.0
      (max (if wf (/ (double (count token)) wf) 0.0)
        (* 0.6 (/ (double (count token)) wp))))))

(defn- find-relevance
  "Order-INSENSITIVE relevance of `query` to `path`, in [0.0, 1.0]. Splits the
   query into alnum tokens, scores each by its tightest subsequence window
   (name-weighted), and takes the MIN so EVERY token must land somewhere — this
   is what separates the handful of genuine hits from fff's page of loose
   subsequence noise."
  [query path]
  (let [toks  (->> (str/split (str/lower-case (or query "")) #"[^a-z0-9]+")
                (remove str/blank?))
        pnorm (find-norm path)
        nnorm (find-norm (last (str/split (str path) #"/")))]
    (if (empty? toks)
      0.0
      (transduce (map #(find-token-score % pnorm nnorm)) min 1.0 toks))))

(defn- coerce-find-spec [args]
  (let [[a b] args
        spec (cond
               (and (= 1 (count args)) (string? a)) {:query a}
               (and (= 2 (count args)) (string? a) (map? b)) (assoc b :query a)
               (and (= 1 (count args)) (map? a)) a
               :else (throw (ex-info
                              "find_files takes find_files(query), find_files(query, opts), or find_files({\"query\": q, ...})."
                              {:type :ext.foundation.editing/invalid-find-args
                               :expected '([query] [query opts] [spec-map])
                               :got args})))
        allowed-keys #{:query :paths :path :limit :is_hidden :is_respect_gitignore}
        unknown-keys (seq (remove allowed-keys (keys spec)))]
    (when unknown-keys
      (throw (ex-info (str "find spec has unknown keys: "
                        (str/join ", " (map #(if (keyword? %) (name %) (str %)) unknown-keys))
                        ". Allowed: query, paths, limit, is_hidden, is_respect_gitignore.")
               {:type :ext.foundation.editing/invalid-find-args
                :unknown (vec unknown-keys)
                :allowed (vec (sort allowed-keys))})))
    (let [query (:query spec)
          _ (when-not (and (string? query) (not (str/blank? query)))
              (throw (ex-info "find :query must be a non-blank string"
                       {:type :ext.foundation.editing/invalid-find-args
                        :query query})))
          _ (when (and (contains? spec :paths) (contains? spec :path))
              (throw (ex-info "find spec must use only one of canonical :paths or alias :path."
                       {:type :ext.foundation.editing/invalid-find-args
                        :spec spec})))
          raw-paths (cond
                      (contains? spec :paths) (:paths spec)
                      (contains? spec :path)  (:path spec)
                      :else ["."])
          paths (cond
                  (string? raw-paths) [raw-paths]
                  (sequential? raw-paths) (vec raw-paths)
                  :else raw-paths)
          _ (when-not (and (vector? paths) (seq paths) (every? string? paths))
              (throw (ex-info "find :paths must be a non-empty vector of strings"
                       {:type :ext.foundation.editing/invalid-find-args
                        :paths raw-paths})))
          limit (or (:limit spec) default-find-limit)
          _ (when-not (and (integer? limit) (pos? limit))
              (throw (ex-info "find :limit must be a positive integer"
                       {:type :ext.foundation.editing/invalid-find-args
                        :limit limit})))]
      {:query query
       :paths paths
       :limit limit
       :is_hidden (boolean (:is_hidden spec))
       :is_respect_gitignore (get spec :is_respect_gitignore true)})))

(defn- find-scan
  "Scan `roots` for ONE `query` string via fff and keep candidates whose
   `find-relevance` (name-weighted, order-insensitive) clears `find-min-score`.
   Returns raw item maps carrying `:score`. A direct FILE root contributes
   itself at score 1.0. The single-query building block `find-search` runs
   once for the strict whole-query pass and once per token for the fallback."
  [roots query is_hidden is_respect_gitignore candidate-page]
  (->> roots
    (mapcat (fn [^File root]
              (cond
                (.isFile root)
                [{:path (rel-path root)
                  :file-name (.getName root)
                  :size (.length root)
                  :binary? false
                  :source :direct-file
                  :score 1.0}]

                ;; skip $HOME / filesystem roots (never indexable); other
                ;; roots still contribute results
                (paths/pathological-index-root? root)
                nil

                :else
                (with-open [idx (rg-fff-open root)]
                  (let [base (.getCanonicalFile root)]
                       ;; doall: realize hits INSIDE with-open, before the
                       ;; fresh instance is closed.
                    (doall
                      (->> (:items (fff/search idx {:query query
                                                    :page-size candidate-page}))
                        (keep (fn [{:keys [relative-path file-name git-status size modified frecency-score binary?]}]
                                (let [f     (io/file base relative-path)
                                      rel   (rel-path f)
                                      score (find-relevance query rel)]
                                  (when (and (>= score find-min-score)
                                          (or is_hidden (not (.isHidden f)))
                                          (or (not is_respect_gitignore)
                                            (not (ignored? (load-ignore-node base) f base))))
                                    {:path rel
                                     :file-name (or file-name (.getName f))
                                     :size size
                                     :modified modified
                                     :frecency-score frecency-score
                                     :git-status git-status
                                     :binary? (boolean binary?)
                                     :score score})))))))))))
    (distinct)
    vec))

(defn- find-fallback-tokens
  "Distinct alnum query tokens worth an independent per-token search: length
   ≥ 3 (a 1–2 char token matches everything) and NOT one of a few noise words
   that describe INTENT rather than a filename (`file`, `code`, `render`-style
   verbs are kept — they often ARE the name; only true glue words drop). Capped
   at the 5 LONGEST so a rambling query can't fan out into a dozen fff scans."
  [query]
  (let [glue #{"the" "and" "for" "with" "that" "this" "how" "what" "where"
               "into" "from" "was" "are" "any" "all" "not" "our" "you" "your"}]
    (->> (str/split (str/lower-case (or query "")) #"[^a-z0-9]+")
      (remove str/blank?)
      distinct
      (filter #(and (>= (count %) 3) (not (contains? glue %))))
      (sort-by (comp - count))
      (take 5)
      vec)))

(defn- find-search [args]
  (let [{:keys [query paths limit is_hidden is_respect_gitignore]} (coerce-find-spec args)
        roots (resolve-search-roots paths)
        ;; fff ranks genuine hits first but pads the page with loose subsequence
        ;; noise, so pull a WIDER candidate set than `limit` and let the relevance
        ;; filter below do the real cutting (a fresh fff scan is ~11ms).
        candidate-page (max limit 300)
        scan (fn [q] (find-scan roots q is_hidden is_respect_gitignore candidate-page))
        strict (scan query)
        tokens (find-fallback-tokens query)
        ;; RELAXED FALLBACK. `find-relevance` takes the MIN across query tokens,
        ;; so EVERY word must land in one path — a multi-word CONCEPT query
        ;; ("native tool call visualization render") is dropped the moment any
        ;; term is absent, even when a distinctive term is an exact filename
        ;; match (`render`). That is why such queries returned nothing. When the
        ;; strict pass is empty and the query has ≥2 usable tokens, search each
        ;; token on its own and surface files ranked by HOW MANY query terms
        ;; they match (coverage) then best term score. It stays a FILENAME tool
        ;; — it just stops requiring the whole sentence to be one filename.
        ;; Low-confidence fuzzy results are ranked, not exhaustive — a tight cap
        ;; keeps the card/model focused on the strongest candidates instead of a
        ;; page of loose single-term noise.
        fuzzy-limit (min limit 20)
        [ranked fuzzy?]
        (if (or (seq strict) (< (count tokens) 2))
          [strict false]
          (let [stem  (fn [it] (find-norm (str/replace (str (:file-name it)) #"\.[^.]*$" "")))
                by-path (reduce
                          (fn [m t]
                            (reduce (fn [m it]
                                      (update m (:path it)
                                        (fn [cur]
                                          (-> (or cur (assoc it :score 0.0 :terms #{}))
                                            (update :score max (:score it))
                                            (update :terms conj t)))))
                              m (scan t)))
                          {} tokens)
                ;; A term that IS the filename stem (`render` → `render.clj`) is a
                ;; bullseye — it must beat a 2-common-word loose match
                ;; (`native`+`tool` → `native-tool-handlers.md`), so it gets a
                ;; score bonus that ranks above raw term coverage.
                scored (map (fn [it]
                              (let [s (stem it)
                                    bull? (contains? (:terms it) s)]
                                (assoc it :rank-score (+ (double (:score it 0.0))
                                                        (if bull? 0.6 0.0)))))
                         (vals by-path))]
            [(->> scored
               (sort-by (fn [it] [(- (double (:rank-score it)))
                                  (- (count (:terms it)))          ;; then coverage
                                  (- (long (or (:frecency-score it) 0)))
                                  (:path it)]))
               vec)
             true]))
        items (if fuzzy?
                (vec (take fuzzy-limit (map #(dissoc % :rank-score) ranked)))
                (->> ranked
                  ;; strongest match first; frecency then path break ties.
                  (sort-by (fn [it] [(- (double (:score it 0.0)))
                                     (- (long (or (:frecency-score it) 0)))
                                     (:path it)]))
                  (take limit)
                  vec))
        ;; The query terms that actually landed a file (fuzzy pass only) — so
        ;; the card/model can see WHICH words matched, e.g. "render, native".
        matched-terms (when fuzzy?
                        (->> items (mapcat :terms) distinct (sort-by (comp - count)) vec))]
    (cond-> {:items (mapv #(dissoc % :terms) items)
             :item-count (count items)
             :paths (mapv :path items)
             :query query
             :searched-paths paths
             :limit limit
             :truncated-by (if (>= (count items) limit) :limit :end-of-results)}
      fuzzy?              (assoc :fuzzy true)
      (seq matched-terms) (assoc :matched-terms matched-terms))))

(defn- find-tool
  "Find files by NAME/PATH — fuzzy subsequence match over the file TREE (fff;
   bound as `find_files`, `find` is a back-compat alias). It matches FILENAMES
   and PATHS, NOT file contents. The `query` is a filename fragment or a couple
   of distinctive path words — SHORT, e.g.:
     await find_files(\"render\")
     await find_files(\"channel_tui render\", {\"paths\": [\"src\"], \"limit\": 20})
     await find_files({\"query\": \"editing/core\", \"paths\": [\".\"]})

   Do NOT pass a natural-language phrase describing what code DOES (e.g.
   \"native tool call visualization render\") — that describes CONTENT, matches
   no filename, and returns nothing. For text/symbols/error-strings INSIDE
   files use `rg`; use `find_files` only to locate a file when you half-know
   its name; `cat` once you know the path; `ls` for a literal directory listing.

   Returns {\"items\": [{\"path\": P, \"file_name\": N, ...}], \"paths\": [P...],
   \"item_count\", \"query\", \"searched_paths\", \"limit\"} — plus a \"hint\" when
   nothing matched."
  [& args]
  (let [{:keys [query searched-paths limit item-count truncated-by] :as out} (find-search args)
        ;; A 0-result find is almost always a MISUSE: the model passed a
        ;; content/concept phrase to a FILENAME matcher. Steer it — a bare
        ;; "0 matches" just makes it retry the same wrong query (the exact loop
        ;; the user hit). Only fires on empty so a normal hit set stays lean.
        multiword? (> (count (str/split (str/trim (str query)) #"\s+")) 2)
        out (cond-> out
              (zero? (long (or item-count 0)))
              (assoc :hint
                (str "No FILENAME/PATH matched \"" query "\". find_files matches file "
                  "NAMES, not contents. "
                  (if multiword?
                    (str "That query looks like CONTENT (a phrase describing code) — "
                      "search inside files with rg(\"a distinctive term\") instead, "
                      "or shorten this to a single filename fragment.")
                    "Try a shorter/different filename fragment, or rg(...) to search file contents."))))]
    (tool-success
      {:op :find_files
       :path (first searched-paths)
       :kind :dir
       :result out
       :metadata {:query query
                  :paths searched-paths
                  :limit limit
                  :item-count item-count
                  :truncated-by truncated-by}})))

;; =============================================================================
;; rg
;; =============================================================================

(defn- coerce-rg-spec
  "Coerce the public rg spec map into the search engine's shape.

   `query` IS the search — a string, or a LIST of terms matched as OR (a line
   containing ANY term). Matching is smart-case literal substring (see
   `make-line-matcher`). `any` is accepted as a back-compat alias for `query`
   (and a stray `all` is treated as OR too — the same-line AND mode was dropped;
   filter the hits in Python for the rare \"both terms\" case).

   Optional: `paths` (scope, default \".\"), `include` (globs — only files whose
   path/name matches), `context` N (lines of context around each hit),
   `is_files_only` (return the distinct matching file paths, no per-line hits).
   Unknown keys are ignored so a stray annotation never hard-fails the call."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "rg takes one spec map: {:query [...] :paths [...]}."
             {:type :ext.foundation.editing/invalid-rg-spec
              :got  (type spec)})))
  (let [vector-of-strings (fn [k raw]
                            (let [v (if (string? raw) [raw] raw)]  ;; scalar-tolerant
                              (when-not (and (vector? v) (seq v) (every? string? v))
                                (throw (ex-info "rg field must be a string or non-empty vector of strings."
                                         {:type :ext.foundation.editing/invalid-rg-spec :field k :got v})))
                              (when-not (every? #(not (str/blank? %)) v)
                                (throw (ex-info "rg string values must be non-blank."
                                         {:type :ext.foundation.editing/invalid-rg-spec :field k :got v})))
                              v))
        ;; `query` canonical; `any`/`all` are accepted aliases (all mean OR now).
        query-key (some #(when (contains? spec %) %) [:query :any :all])
        _ (when-not query-key
            (throw (ex-info "rg needs `query`: a term or a list of terms."
                     {:type :ext.foundation.editing/invalid-rg-spec :spec spec})))
        ;; A query TERM is a substring; a LIST is OR. Models overwhelmingly write
        ;; the OR list as ONE comma-joined string (`\"model, cycle\"`) — which,
        ;; matched literally, hits nothing. So split every term on commas into
        ;; separate OR needles: `\"a, b\"` and `[\"a, b\", c]` both become
        ;; `[a b …]`. (A rare literal-comma search loses out; the model's intent
        ;; is virtually always \"these separate terms\".)
        needles (let [ns (->> (vector-of-strings query-key (get spec query-key))
                           (mapcat #(str/split % #"\s*,\s*"))
                           (map str/trim)
                           (remove str/blank?)
                           vec)]
                  (when (empty? ns)
                    (throw (ex-info "rg query has no non-blank terms."
                             {:type :ext.foundation.editing/invalid-rg-spec :field query-key})))
                  ns)
        paths   (vector-of-strings :paths (get spec :paths ["."]))
        include (when (contains? spec :include)
                  (vector-of-strings :include (get spec :include)))
        nonneg-int! (fn [label v]
                      (when (and (some? v) (not (and (integer? v) (not (neg? v)))))
                        (throw (ex-info (str "rg " label " must be a non-negative integer")
                                 {:type :ext.foundation.editing/invalid-rg-spec :field label :got v}))))
        _ (nonneg-int! ":context" (:context spec))
        is_files_only (boolean (:is_files_only spec))
        ;; `context` is a CONTENT-mode concept — in files-only mode there are no
        ;; per-line hits to surround, so a stray `context` is simply IGNORED (never
        ;; a hard error: the model harmlessly set both, so honor `is_files_only`).
        context (if is_files_only 0 (or (:context spec) 0))]
    {:needles needles
     :paths paths
     :include (or include [])
     :is_hidden (boolean (:is_hidden spec))
     :is_respect_gitignore (get spec :is_respect_gitignore true)
     :limit default-grep-limit
     :context context
     :is_files_only is_files_only}))

(defn- has-upper?
  "True when `s` contains an uppercase letter — the smart-case trigger."
  [^String s]
  (boolean (some #(Character/isUpperCase ^char %) s)))

(defn- make-line-matcher
  "A `(fn [line] boolean)` — true when the line contains ANY needle (OR). SMART-
   CASE, the SAME rule the fff candidate pre-filter (`fff/grep :smart-case?`) uses,
   so the two never disagree: a needle with NO uppercase matches case-INSENSITIVELY
   (`rg(\"key\")` finds `Key`/`KEY`/`keymap`); a needle WITH an uppercase letter
   matches case-sensitively (you typed a capital on purpose). Plain literal
   substring — no regex, no per-line AND. \"Both terms\" is a Python filter on the
   hits; a pattern is the rare case you `re` over `:text` yourself."
  [needles]
  (let [grouped (group-by has-upper? needles)
        cs (vec (get grouped true))                    ;; has uppercase → case-sensitive
        ci (mapv str/lower-case (get grouped false))]  ;; no uppercase → case-insensitive
    (fn [^String line]
      (or (boolean (some #(str/includes? line %) cs))
        (and (seq ci)
          (let [low (str/lower-case line)]
            (boolean (some #(str/includes? low %) ci))))))))

;; rg hit/context text is kept FULL in the result value — never per-line
;; mutilated. The model sees it by printing what it needs (context is print-only;
;; there is no `r[...]` by-scope result store any more). The wire VIEW is bounded
;; by the non-destructive 64KB per-observation clip (loop/clip-form-repr). The hit
;; cap (default 250) and total-bytes budget bound result SIZE only — collected
;; hits stay full, with `:truncated-by` set so the model narrows.

(defn- hit-bytes
  "Rough char/byte size of a content-mode hit (text + context) for the rg
   total-bytes budget."
  ^long [hit]
  (long (+ (count (str (:text hit)))
          (reduce + 0 (map (comp count str second) (:before hit)))
          (reduce + 0 (map (comp count str second) (:after hit))))))

(defn- search-file-content
  "Walk one file once, emit hits with optional context. Content-mode helper.
   Returns a vec of hit maps; an empty vec means no match. `:text` and the
   `:before`/`:after` context are kept FULL — the value is the model's data,
   sliceable in Python via `r[...]`; only the wire VIEW is bounded downstream."
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
                ;; FULL text here: the hit's :text feeds `patch/line-anchor`
                ;; (the patch hash must match the real file line). Display
                ;; clipping happens AFTER the anchor is computed (see rg-search).
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

(defn- rg-search
  "The rg search ENGINE: takes the public rg spec map and does the
   actual file scanning. The public `rg-tool` (= `rg`) wraps this with
   arity/kwargs handling + the LLM-facing result envelope. Two output modes,
   picked by `:is_files_only` / (default content).

   Returns one of:
     {:hits   [{:path :line :text :anchor :before? :after?} ...] :truncated-by KW}  ;; content
   `:anchor` is the `<lineno>:<hash>` anchor for that line — the same one `cat`
   emits in `:anchors` — so a hit is directly patchable via `{:from_anchor <anchor>}`
   without a follow-up `cat`. Absent on blank lines.
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW}  ;; files-only

   `:truncated-by` is `:limit` (hit count), `:bytes` (total-bytes budget), or
   `:end-of-results`. Hit/context `:text` is kept FULL (sliceable in Python via
   `r[...]`); only the wire VIEW is bounded by the 64KB per-observation clip."
  [spec]
  (let [{:keys [needles paths include is_hidden is_respect_gitignore
                limit context is_files_only]} (coerce-rg-spec spec)
        before-ctx context
        after-ctx  context
        glob-matcher (fn [pattern]
                       (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                         (str "glob:" pattern)))
        include-matchers (mapv glob-matcher include)
        match-globs? (fn [matchers ^File f]
                       (let [rel (rel-path f)
                             name (.getName f)
                             rel-path (fs/path rel)
                             name-path (fs/path name)]
                         (boolean
                           (some (fn [^java.nio.file.PathMatcher m]
                                   (or (.matches m rel-path)
                                     (.matches m name-path)))
                             matchers))))
        include-file? (fn [^File f]
                        (or (empty? include-matchers)
                          (match-globs? include-matchers f)))
        roots (->> (resolve-search-roots paths)
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
        matches? (make-line-matcher needles)
        walk (fn walk [ignore-node root ^File f]
               (check-interrupt!)
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
                (sort-by rel-path)
                (rg-fff-candidate-files roots needles))]
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

      :else
      (let [out        (atom [])
            bytes-used (atom 0)
            cap-reason (atom nil)]     ;; nil | :limit | :bytes
        (doseq [^File f files :while (not @cap-reason)]
          (let [hits (search-file-content f matches? before-ctx after-ctx)]
            (when (seq hits)
              ;; Attach the `lineno:hash` anchor (patchable straight from the hit).
              ;; :text is kept FULL — it's the model's data, sliceable in Python
              ;; via r[...]; the wire VIEW is bounded by the 64KB observation clip.
              ;; Stop on the hit limit OR the total-bytes budget (whichever first).
              (doseq [hit hits :while (not @cap-reason)]
                (let [hit* (cond-> hit
                             (not (str/blank? (:text hit)))
                             (assoc :anchor (patch/line-anchor (:line hit) (:text hit))))]
                  (swap! out conj hit*)
                  (swap! bytes-used + (hit-bytes hit*))
                  (cond
                    (>= (count @out) limit)              (reset! cap-reason :limit)
                    (>= @bytes-used max-rg-result-bytes) (reset! cap-reason :bytes)))))))
        {:hits (vec @out)
         :truncated-by (or @cap-reason :end-of-results)}))))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{:path :replace})
(def ^:private patch-locator-keys
  "Every edit needs the `:from_anchor` locator (a `lineno:hash` hashline —
   content-addressed by the per-line hash `cat` prints). It re-resolves against
   LIVE content on every edit, so it stays correct under line drift (insertions
   above, or earlier edits in the same grouped batch) where raw line numbers
   would silently target the wrong line. (The old `:search` text matcher was
   removed — anchors only.)"
  #{:from_anchor})
(def ^:private patch-optional-keys
  "Optional keys recognised on an anchor edit map.
   - :to_anchor        end of a hashline range; defaults to :from_anchor (single line)
   - :expected_mtime   epoch-ms; fail if file mtime differs (staleness guard)
   - :expected_size    bytes;    fail if file size differs (staleness guard)
   - :atomic/:atomic?  multi-file escape flag (read by `mutation-atomic?` from
                       the RAW args before this validation; allowed here so a
                       documented `\"atomic\": True` edit isn't refused as an
                       unknown key)."
  #{:to_anchor :expected_mtime :expected_size :atomic :atomic?})

(def ^:private patch-allowed-keys
  (set/union patch-required-keys patch-locator-keys patch-optional-keys))

(def ^:private patch-group-required-keys #{:path :edits})
(def ^:private patch-group-optional-keys #{:expected_mtime :expected_size :atomic :atomic?})
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
            ;; The LLM-facing JSON schema marks :path as required, so callers
            ;; routinely echo an EMPTY :path (""/nil) into each grouped edit
            ;; just to satisfy it. Treat a blank per-edit :path as ABSENT and
            ;; only refuse a genuinely conflicting NON-EMPTY path — the group's
            ;; :path is assoc'd over it below regardless.
            (when-let [stated (some-> edit :path str str/trim not-empty)]
              (throw (ex-info "patch grouped :edits inherit :path; do not repeat it per edit"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :path stated
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
  "Normalize + validate the user's edit maps. Every edit is anchor-located:
   it must carry `:from_anchor` (and optionally `:to_anchor` for a range).
   A missing anchor, or an unknown key, throws."
  [edits]
  (let [edits (normalize-patch-edits-input edits)]
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "patch edit must be a map"
                       {:type :ext.foundation.editing/invalid-patch-edit
                        :edit edit})))
            (let [missing (seq (remove #(contains? edit %) patch-required-keys))
                  unknown (seq (remove patch-allowed-keys (keys edit)))]
              (when missing
                (throw (ex-info "patch edit missing required keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :missing (vec missing)
                          :edit edit})))
              ;; The removed text-matcher API (`search`/`nth`/grouped `edits`
              ;; carrying `search`) keeps getting re-hallucinated. Name it
              ;; explicitly so the model corrects to anchors in ONE step
              ;; instead of staring at a generic ":from_anchor missing".
              (when-let [legacy (seq (filter #(contains? edit %) #{:search :nth :replace_all}))]
                (throw (ex-info (str "patch is ANCHOR-ONLY — `"
                                  (str/join "`/`" (map name legacy))
                                  "` was removed; there is no text search/replace. "
                                  "cat the file, then pass `from_anchor` (a lineno:hash from the "
                                  "result's \"anchors\" map) — add `to_anchor` for a range — with `replace`.")
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :removed (vec legacy)
                          :edit edit})))
              (when-not (contains? edit :from_anchor)
                (throw (ex-info "patch edit needs a :from_anchor (lineno:hash from cat)"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :edit edit})))
              (when unknown
                (throw (ex-info "patch edit has unknown keys"
                         {:type :ext.foundation.editing/invalid-patch-edit
                          :unknown (vec unknown)
                          :allowed (vec patch-allowed-keys)
                          :edit edit}))))
            (update edit :path str))
      edits)))

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
      " from_anchor..to_anchor anchors or nth selection. If you are rewriting the"
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
;; Per-edit pipeline (anchor-only — there is no text search):
;;   1. Coerce/validate edit map (`:from_anchor` required, mtime/size types).
;;   2. Read current file content (always the ORIGINAL snapshot, not a prior
;;      edit's output, so anchors in one batch can't drift each other).
;;   3. mtime/size guard → :stale failure if mismatched.
;;   4. Resolve the `lineno:hash` anchor(s) to a char span (patch/resolve-anchor-
;;      edit-span): line locates, hash verifies.
;;   5. Apply replacement(s) end-to-start, update post-state.
;;
;; All failures populate `:failures` with `:reason` + the original anchors so
;; the surfaced ex-message stays actionable.
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
;; (`patch/resolve-anchor-edit`, `patch/indices-matching-hash`). The
;; `:from_anchor`/`:to_anchor` branch of `patch-analysis` calls straight into
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
          (if-let [{:keys [path replace from_anchor to_anchor] :as edit}
                   (first remaining)]
            (let [resolved (resolve-edit-target path)]
              (if-let [path-error (:error resolved)]
                (let [check {:edit-index idx :path path :reason (:reason path-error)
                             :path-error path-error}]
                  (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                (let [file        (:file resolved)
                      rel         (:rel resolved)
                      seen?       (contains? origs path)
                      ;; ALWAYS the original snapshot — never the cumulative result.
                      current     (or (get origs path) (slurp file))
                      origs       (assoc origs path current)
                      replace     (str replace)
                      stale       (when-not seen? (staleness-check file edit))]
                  (if from_anchor
                    ;; ---- hashline locator (content-addressed by line hash) ----
                    (let [base-check {:edit-index idx :path rel :from_anchor from_anchor :to_anchor (or to_anchor from_anchor)}]
                      (if stale
                        (let [check (assoc base-check :reason :stale :stale stale)]
                          (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                        (let [res (patch/resolve-anchor-edit-span current from_anchor to_anchor replace)]
                          (if-let [err (:error res)]
                            (let [check (assoc base-check :reason (:reason err) :hash-error err)]
                              (recur (inc idx) (next remaining) origs spans (conj checks check) (conj failures check)))
                            (let [span  {:start (:start res) :end (:end res) :replacement (:replacement res)
                                         :file file :path rel :edit-index idx}
                                  check (assoc base-check :pass :hashline :applied-positions [(:applied-line res)])]
                              (recur (inc idx) (next remaining) origs
                                (update spans path (fnil conj []) span)
                                (conj checks check) failures))))))
                    ;; ANCHOR-ONLY: the `:search`/`:replace` text matcher was
                    ;; removed. An edit with no `:from_anchor` cannot be located —
                    ;; re-read with `cat` and use the `lineno:hash` anchor.
                    (let [check {:edit-index idx :path rel :reason :missing-anchor}]
                      (recur (inc idx) (next remaining) origs spans
                        (conj checks check) (conj failures check)))))))
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
  [{:keys [edit-index path reason stale hash-error message path-error]}]
  ;; A failure that arrives with a precomputed :message (the syntax-error re-parse
  ;; guard) or a nested :path-error message (file-not-found / path-escape / dir)
  ;; already carries its full, actionable explanation — surface it verbatim rather
  ;; than dropping it into the generic default branch below, which only knows the
  ;; anchor-resolution `reason`s and would otherwise flatten it to "edit N in P
  ;; failed." (the exact wrong report this guards against).
  (or message
    (:message path-error)
    (let [head (str "edit " edit-index " in " path)]
      (case reason
        :hashline-malformed (str head " failed: " (name (:which hash-error)) "_anchor "
                              (pr-str (:anchor hash-error)) " is not a `lineno:hash` anchor"
                              " - every :from_anchor needs BOTH the line number AND the hash"
                              " (the bare-hash form is gone). Use the EXACT `lineno:hash` anchor"
                              " cat printed.")
        :hashline-line-out-of-range (str head " failed: " (name (:which hash-error)) "_anchor line "
                                      (:line hash-error) " is outside the file (it has "
                                      (:lines hash-error) " lines). Re-read with cat for fresh"
                                      " `lineno:hash` anchors, then resend the batch.")
        :hashline-not-found (str head " failed: " (name (:which hash-error)) "_anchor hash "
                              (pr-str (:hash hash-error)) " matches no line in the current file"
                              " (that line changed or the file moved — anchors go STALE after"
                              " any write/patch)."
                              (if-let [ca (:current-anchor hash-error)]
                                (str " Line " (:stated-line hash-error) " is NOW `" ca "`"
                                  (when-let [t (:current-text hash-error)]
                                    (str " = " (pr-str (cond-> (str t) (> (count (str t)) 80)
                                                         (-> (subs 0 80) (str " …"))))))
                                  ". Use that anchor if it's still your target; otherwise re-`cat`"
                                  " the exact lines for fresh :anchors, then resend the batch.")
                                (str " Re-`cat` the exact lines for fresh `lineno:hash` anchors,"
                                  " then resend the batch.")))
        :hashline-misplaced (str head " failed: " (name (:which hash-error)) "_anchor "
                              (pr-str (:hash hash-error)) " says line " (:stated-line hash-error)
                              " but that content is at line(s) " (pr-str (:found-lines hash-error))
                              " — too far to be drift, so this looks like a stale/misattributed"
                              " anchor. Re-read with cat for fresh `lineno:hash` anchors before"
                              " editing (this guard is what stops an edit landing on the wrong line).")
        :overlapping-edits (str head " failed: this edit's target overlaps another edit"
                             " in the same file — two edits touch the same lines. Merge"
                             " them into ONE edit, or split into separate patch calls.")
        :hashline-range-inverted (str head " failed: :to_anchor line " (:to-line hash-error)
                                   " precedes :from_anchor line " (:from-line hash-error) ".")
        :stale (str head
                 " failed: file changed since :expected-" (name (:reason stale))
                 " check (expected " (or (:expected_mtime stale) (:expected_size stale))
                 ", actual " (or (:actual-mtime stale) (:actual-size stale))
                 "). Re-read the file before retrying.")
        :missing-anchor (str head " failed: no :from_anchor — patch is anchor-only."
                          " Re-read with cat and use the `lineno:hash` anchor it prints.")
        (str head " failed.")))))

(defn- patch-failure-message
  [failures]
  ;; patch is ATOMIC — a single failed edit rejects the WHOLE batch and writes
  ;; NOTHING, so the file is byte-for-byte unchanged. Say so up front: the model
  ;; must not assume a partial application and must not re-read to \"repair\" it.
  (let [atomic "patch made NO changes — it is atomic, so the whole batch was rejected and every file is UNCHANGED. Fix the edit(s) below and resend the full batch. "]
    (if (= 1 (count failures))
      (str atomic (explain-failure (first failures)))
      ;; Show EVERY failing edit, not just the first — reporting only `first:`
      ;; hid the LATER edit that was the real problem and made the model fixate on
      ;; edit 0 (typically an unrelated require/import at the top of the batch).
      (str atomic (count failures) " edits failed:\n"
        (str/join "\n" (map (fn [f] (str "  • " (explain-failure f))) failures))))))

(defn- non-exact-passes-for-path
  "Pull the non-`:exact` fuzzy passes that fired against `rel-path` out
   of `:checks`, preserving edit order. Returns nil when every check on
   that path used `:exact` so the caller can omit the `:passes` key
   entirely (no `:exact` noise in the trailer)."
  [checks rel-path]
  (let [ps (->> checks
             (filter #(= rel-path (:path %)))
             (keep :pass)
             (remove #{:exact :hashline})
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

   `:passes` lists only meaningful non-anchor alarms that fired against this
   plan's path, in edit order. The ordinary `:hashline` anchor path is
   expected and therefore omitted from user/model-facing summaries. Same for
   `:indent-delta`: present only when a `:relative-indent` pass auto-shifted
   `:replace`.

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
      ;; Success path: RE-PARSE guard, then commit. Refuse only an edit that turns
      ;; a CLEANLY-parsing file into a BROKEN one — the same safety `struct_patch`/
      ;; `symbol_rename` already give, now on plain `patch` too, so the two verbs
      ;; stop differing on "which one is safe". TWO gates keep it false-positive-free:
      ;; (1) `outline/code-language` — a CURATED allowlist, so prose/markup/data the
      ;; pack over-eagerly recognizes (`.txt`→vimdoc, `.md`, `.csv`, `.log`) is never
      ;; considered; (2) before→after — a file that was already broken can still be
      ;; FIXED, and strict configs (json/yaml/toml) are protected without surprises.
      (let [plans (vec plans)
            syntax-fails
            (vec (keep (fn [{:keys [path before after]}]
                         (when-let [lang (outline/code-language path)]
                           (when (and (not (zipper/syntax-broken? lang (str before)))
                                   (zipper/syntax-broken? lang (str after)))
                             {:edit-index 0
                              :path path
                              :reason :syntax-error
                              :message (str "patch refused: this edit would leave " path
                                         " with a SYNTAX ERROR (unbalanced delimiters / a "
                                         "broken form) — it parsed cleanly before. NOTHING was "
                                         "written. Re-cat for fresh anchors and fix the "
                                         "replacement, or use struct_patch for a structural "
                                         "edit that can't break syntax.")})))
                   plans))]
        (if (seq syntax-fails)
          (let [counts (into {} (for [p (distinct (map :path syntax-fails))]
                                  (when-let [f (try (safe-path p) (catch Throwable _ nil))]
                                    [p (bump-patch-fail-count! f)])))
                fails  (mapv (fn [f] (let [n (get counts (:path f))]
                                       (cond-> f n (assoc :consecutive-failures n))))
                         syntax-fails)
                hint   (some (fn [[p n]] (patch-loop-hint n p)) counts)]
            {:success? false
             :failures fails
             :checks   (into (vec checks) fails)
             :loop-hint hint
             ;; The WHOLE-BATCH candidates (every planned file, applied but
             ;; unwritten) ride the refusal so a language pack's :around
             ;; op-hook can WHOLE-SOURCE-repair the broken ones and commit the
             ;; batch itself (fragment repair can't fix contextual imbalance —
             ;; a locally-balanced replacement that swallows/duplicates an
             ;; enclosing closer only shows up in the full file). Carried on
             ;; the RESULT, never inside `:error` — the model must not be fed
             ;; whole files in a failure message.
             :candidate-plans (mapv #(select-keys % [:path :before :after]) plans)
             :broken-paths (mapv :path fails)
             :message  (cond-> (patch-failure-message fails) hint (str "\n" hint))})
          (do
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
             :checks checks}))))))

;; =============================================================================
;; write — whole-file write primitive (create or overwrite)
;;
;; patch is great for surgical edits but awkward for full-file rewrites:
;; the model would otherwise have to anchor and replace every line. write
;; makes the common case ergonomic: one tool, one map, atomic semantics.
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
  ;; :atomic/:atomic? = the documented multi-file escape flag (read from raw
  ;; args by `mutation-atomic?`); allowed here so it isn't refused as unknown.
  #{:expected_mtime :expected_size :is_overwrite :atomic :atomic? :allow_dirty})
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
        allow_dirty (boolean (:allow_dirty args))
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

                   ;; A whole-file write over a file with UNCOMMITTED changes is
                   ;; how a truncated reconstruction silently wipes work. Refuse
                   ;; it: surgical edits belong in patch()/struct_patch().
                   (and exists? (not is-dir?) (not allow_dirty) (git/file-dirty? file))
                   {:reason :dirty
                    :path rel
                    :message (str "write refused: " rel " has UNCOMMITTED changes — a "
                               "whole-file write would clobber edits already in flight "
                               "(this is exactly how a truncated reconstruction wipes a "
                               "file). Make surgical changes with patch(...) or "
                               "struct_patch(...) instead, or commit/checkout " rel
                               " first. Pass allow_dirty=True to overwrite on purpose.")}

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

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-result->model
  "Shape an internal read result into the MODEL-facing form: the internal
   `:lines` (a vec of `[ln text]` tuples) becomes the model's `:anchors` — an
   ordered `{anchor text}` map (`patch/lines->anchor-map`, a line-ordered
   LinkedHashMap, the key IS the `patch :from_anchor`). The internal `:lines`
   tuple vector and the read-file `{ln anchor}` `:anchors` are both dropped;
   every `:ranges` window converts the same way. The internal read pipeline
   keeps working on tuples — this is the single boundary where the model
   payload is built."
  [out]
  (letfn [(->anchors [m] (-> m
                           (assoc :anchors (patch/lines->anchor-map (:lines m)))
                           (dissoc :lines)))]
    (cond-> out
      (contains? out :lines) ->anchors
      (seq (:ranges out))    (update :ranges (fn [ws] (mapv ->anchors ws))))))

(defn- cat-tool
  "Read a text-file window. `await cat(path)` reads the whole file (≤2000 lines)
   — slice only for bigger files or a middle/tail section. Options = a dict,
   snake_case keys:
     await cat(path, {\"range\": [start, end]})   # inclusive 1-based line range
     await cat(path, {\"ranges\": [[s, e], ...]})  # several windows in one call
     await cat(path, {\"anchor\": \"325:0e3\"})      # one line by its lineno:hash anchor
     await cat(path, {\"anchor\": [\"H1\", \"H2\"]})   # inclusive anchor range H1..H2
     await cat(path, {\"tail\": 200})              # last N lines (omit N → 2000)
   Returns {\"anchors\": {\"lineno:hash\": text, ...}, \"next_offset\", \"eof\",
   \"truncated\", \"mtime\", \"size\"}. \"anchors\" is the ONLY content key — an ORDERED
   {anchor: text} map; there is NO \"lines\"/\"text\" key (c[\"lines\"] KeyErrors).
   Each key IS the `patch` from_anchor — copy it straight into an edit.
   Not \"eof\"/\"truncated\" → paginate from \"next_offset\"."
  ([path]
   (let [out (read-file path 1 default-cat-limit)]
     (tool-success
       {:op :cat
        :path path
        :kind :file
        :result (cat-result->model out)
        :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)}})))
  ([path arg]
   (cond
     ;; Python-native form: a single options dict, e.g.
     ;;   cat("p", {"range": [5, 10]})       cat("p", {"ranges": [[1,5],[20,25]]})
     ;;   cat("p", {"anchor": A})            cat("p", {"anchor": [A1, A2]})
     ;;   cat("p", {"tail": 100})            cat("p", {})  -> whole file
     ;; Delegated to the keyword arities below so internal Clojure callers
     ;; (which pass bare keyword args) keep working unchanged.
     (map? arg)
     (let [rng    (:range arg)
           ranges (:ranges arg)
           anc    (:anchor arg)
           tail   (:tail arg)]
       (cond
         rng            (cat-tool path :range (first rng) (second rng))
         ranges         (cat-tool path :ranges ranges)
         (vector? anc)  (cat-tool path :anchor (first anc) (second anc))
         (some? anc)    (cat-tool path :anchor anc)
         (integer? tail) (cat-tool path :tail tail)
         (some? tail)   (cat-tool path :tail)
         :else          (cat-tool path)))

     (= arg :tail)
     (let [out (tail-file path default-cat-limit)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (cat-result->model out)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}}))

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
          :result (cat-result->model out)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}}))

     :ranges
     (let [out (read-file-ranges path n)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (cat-result->model out)
          :metadata {:truncated? (:truncated? out)
                     :ranges (mapv :range (:ranges out))}}))

     :anchor
     ;; (cat path :anchor A) — the single line carrying the `lineno:hash`
     ;; anchor A (the symmetric read for patch :from_anchor).
     (let [out (read-file-by-anchor path n nil)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (cat-result->model out)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                     :range (:range out)}}))

     (throw (ex-info "cat options must use {\"tail\": N}, {\"ranges\": [[s, e], ...]}, or {\"anchor\": A}; for one range use {\"range\": [start, end]}"
              {:type :ext.foundation.editing/invalid-cat-args
               :got arg}))))
  ([path mode start end]
   (case mode
     ;; (cat path :range start end) — INCLUSIVE start..end (both 1-based).
     :range
     (do
       (validate-cat-range! start end)
       (let [n (inc (- (long end) (long start)))
             out (read-file path start n)]
         (tool-success
           {:op :cat
            :path path
            :kind :file
            :result (cat-result->model out)
            :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                       :range [start end]}})))

     ;; (cat path :anchor from_anchor to_anchor) — INCLUSIVE window between the
     ;; lines anchored from_anchor..to_anchor, addressed by content.
     :anchor
     (let [out (read-file-by-anchor path start end)]
       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (cat-result->model out)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)
                     :range (:range out)}}))

     (throw (ex-info "cat window must use {\"range\": [start, end]} or {\"anchor\": [from, to]}"
              {:type :ext.foundation.editing/invalid-cat-args
               :got mode})))))

(defn- ls-tool
  "List a directory tree, grouped by directory.
     await ls()                 # current dir (like os.listdir)
     await ls(\".\")
     await ls(path, {\"depth\": 2, \"is_files_only\": True})

   Returns {\"groups\": [{\"dir\": D, \"files\": [{\"name\": N, \"size\": BYTES}]}],
   \"path\", \"entry_count\", \"file_count\", \"dir_count\", \"truncated\", \"depth\", \"limit\"}.
   Each dir is stated ONCE; rebuild a path as g[\"dir\"] + \"/\" + f[\"name\"].
     [g[\"dir\"]+\"/\"+f[\"name\"] for g in r[\"groups\"] for f in g[\"files\"]]

   Opts (snake_case): depth, limit, is_files_only, is_dirs_only, is_hidden,
   is_respect_gitignore (default True). Gotcha: \"truncated\": True means limit
   (default 3000) clamped the walk — narrow with depth/path, don't assume complete."
  ([] (ls-tool "."))
  ([path & {:as opts}]
   (let [listing (list-files path opts)]
     (tool-success
       {:op :ls
        :path path
        :kind :dir
        :result listing
        :metadata  {:depth (:depth listing)
                    :limit (:limit listing)
                    :entry-count (:entry-count listing)
                    :file-count  (:file-count listing)
                    :dir-count   (:dir-count listing)
                    :truncated?  (:truncated? listing)
                    :is_hidden (:is_hidden opts)
                    :is_respect_gitignore (get opts :is_respect_gitignore true)}}))))

(defn- rg-tool
  "Search file CONTENT — smart-case, loose. (For file NAMES use find_files; it's fuzzy.)
     await rg(\"request_timeout\")                    # one loose term
     await rg([\"TODO\", \"FIXME\"], paths=[\"src\"])     # a LIST is OR (a line with ANY term)
     await rg(\"login\", is_files_only=True)          # just the files that contain it
     await rg(\"handleClick\", context=3)             # ± lines around each hit

   `query` is a term or a list of terms (OR). Matching is SMART-CASE substring:
   a lowercase term matches ANY case — rg(\"key\") finds Key/KEY/keymap/keystroke,
   so you rarely need to list variants; a term WITH a capital is case-sensitive.
   OR is for several terms you have REASON to believe exist ([\"TODO\" \"FIXME\"]) —
   OR-ing pure GUESSES just multiplies ZERO. A no-hit rg means the term is wrong
   OR the thing is absent; either way more synonyms won't help — widen ONCE to the
   stem (scoped with paths/include) or read a file you already hold.
   Opts (snake_case): paths (default [\".\"]), include [\"**/*.py\"], context N,
   is_files_only. No regex / no AND — filter the hits in Python for those.

   Result:
     content:       {\"matches\": {path: {\"lineno:hash\": text}}, \"hit_count\", \"file_count\", \"first_hit\"}
     is_files_only: {\"files\": [...], \"file_count\"}
   With context the content value becomes
   {\"text\": match, \"before\": {anchor: text}, \"after\": {anchor: text}}.
   Gotcha: each \"matches\" key is a \"lineno:hash\" anchor — pass it AS patch from_anchor."
  [& args]
  ;; Accept either a single spec map OR inline kwargs. Manual dispatch
  ;; (instead of `& {:as spec}`) so that malformed input — a stray
  ;; positional string, an odd-length rest seq — routes through one
  ;; clean `:invalid-rg-spec` error instead of Clojure's raw
  ;; "No value supplied for key" destructure exception.
  (let [[a & more] args
        ;; A positional query is a string ("x") or a list of strings (["a" "b"]).
        query-arg? (fn [x] (or (string? x)
                             (and (sequential? x) (seq x) (every? string? x))))
        ->query    (fn [x] (if (string? x) [x] (vec x)))
        spec (cond
               ;; rg({...}) — a full spec map.
               (and (= 1 (count args)) (map? a)) a
               ;; rg("x") / rg(["a" "b"]) — bare query.
               (and (= 1 (count args)) (query-arg? a)) {:query (->query a)}
               ;; rg("x", {opts}) — query + an options MAP.
               (and (= 2 (count args)) (query-arg? a) (map? (first more)))
               (assoc (first more) :query (->query a))
               ;; rg("x", paths=[...], …) — query + trailing kwargs.
               (and (query-arg? a) (even? (count more)) (every? keyword? (take-nth 2 more)))
               (assoc (apply hash-map more) :query (->query a))
               ;; rg(query=[...], …) — pure kwargs.
               (and (even? (count args)) (every? keyword? (take-nth 2 args)))
               (apply hash-map args)

               :else
               (throw (ex-info
                        "rg takes a query, e.g. rg(\"x\") or rg([\"x\", \"y\"], paths=[\"src\"])."
                        {:type :ext.foundation.editing/invalid-rg-arity
                         :expected '([query] [query opts] [spec-map] [& kwargs])
                         :got args})))
        {:keys [needles paths include is_files_only context limit]} (coerce-rg-spec spec)
        out (rg-search spec)
        mode (if is_files_only :files-only :content)
        ;; NO `:spec` echo in the model-facing payload: echoing the input
        ;; map back taught models a phantom "spec" INPUT key (`rg({...,
        ;; "spec": {}})`). The spec stays host-side on `:metadata` below
        ;; for channel labels.
        ;; `:needles` = the parsed OR search terms, carried on the result so the
        ;; op-card HEADLINE can name WHAT was searched (a bare "N hits in M
        ;; files" is useless without it). Model-facing but harmless — it is the
        ;; query the model itself sent, echoed back as data it can re-read.
        shared {:mode         mode
                :needles      needles
                :truncated-by (:truncated-by out)
                :paths        paths
                :limit        limit}
        result (case mode
                 :content
                 (let [hits          (vec (:hits out))
                       ordered-paths (distinct (map :path hits))
                       by-path       (group-by :path hits)
                       ctx?          (pos? context)
                       ;; Grouped by file → each file is an ORDERED
                       ;; `{match-anchor → value}` map (a LinkedHashMap, so it
                       ;; serializes in line order). The path is stated ONCE (the
                       ;; key). WITHOUT a context window the value is the bare
                       ;; matched text; WITH one it is
                       ;; `{:text <match> :before {anchor→text} :after {anchor→text}}`
                       ;; so every match keeps its own before/after context and
                       ;; ALL lines (match + context) stay patchable by anchor key.
                       matches       (let [^java.util.LinkedHashMap mm (java.util.LinkedHashMap.)]
                                       (doseq [p ordered-paths]
                                         (let [^java.util.LinkedHashMap fm (java.util.LinkedHashMap.)]
                                           (doseq [{:keys [line text before after]} (get by-path p)]
                                             (.put fm (patch/line-anchor line text)
                                               (if ctx?
                                                 (cond-> {:text text}
                                                   (seq before) (assoc :before (patch/lines->anchor-map before))
                                                   (seq after)  (assoc :after  (patch/lines->anchor-map after)))
                                                 text)))
                                           (.put mm p fm)))
                                       mm)]
                   (assoc shared
                     :matches matches
                     :hit-count (count hits)
                     :file-count (count ordered-paths)
                     :first-hit (when (pos? (count hits))
                                  (let [{:keys [path line]} (nth hits 0)]
                                    (str path ":" line)))
                     :context (when (pos? context) {:before context :after context})))
                 :files-only
                 (let [files (vec (:files out))]
                   (assoc shared
                     :files files
                     :file-count (count files))))]
    (tool-success
      {:op :rg
       :path (if (= 1 (count paths))
               (first paths)
               ".")
       :kind :dir
       :result result
       :metadata (cond-> {:spec spec
                          :paths paths
                          :include include
                          :mode mode
                          :truncated-by (:truncated-by out)}
                   (= mode :content)
                   (assoc :hit-count (:hit-count result))
                   (= mode :files-only)
                   (assoc :file-count (:file-count result)))})))

(def ^:private patch-diff-context-lines 3)
(def ^:private patch-diff-max-render-lines 240)
(def ^:private patch-java-diff-max-lines 5000)

(defn- cap-diff-lines
  "Bound a rendered diff to `patch-diff-max-render-lines`, keeping a HEAD and a
   TAIL window rather than a plain head-cut. A pure head-cut let a
   deletion-heavy hunk fill the whole visible budget with `-` lines and bury
   the `+` replacement / trailing context below the cut, so a correct edit read
   as a catastrophic deletion. Head+tail keeps the additions and closing
   context visible."
  [lines]
  (let [lines (vec lines)
        n     (count lines)]
    (if (<= n patch-diff-max-render-lines)
      lines
      (let [tail-n  (quot patch-diff-max-render-lines 4)
            head-n  (- patch-diff-max-render-lines tail-n)
            omitted (- n head-n tail-n)]
        (vec (concat (subvec lines 0 head-n)
               [(str "... diff truncated; " omitted " line(s) omitted")]
               (subvec lines (- n tail-n))))))))

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
  "Edit files by ANCHOR (no text search/replace).
     await patch([{\"path\": P, \"from_anchor\": \"12:a3f2\", \"replace\": R}])
     await patch([{\"path\": P, \"from_anchor\": \"40:9c1d\", \"to_anchor\": \"44:7b02\", \"replace\": R}])

   Each anchor is a \"lineno:hash\" key from a fresh cat \"anchors\" map. The window
   is from_anchor..to_anchor inclusive; omit to_anchor for one line; \"replace\": \"\"
   deletes. Anchors re-resolve against live content, so they survive line drift
   within the batch. Optional per-edit \"expected_mtime\"/\"expected_size\" guards.

   Group several edits to one file: {\"path\": P, \"edits\": [{...}, {...}]}.
   Returns [{\"path\": P, \"op\": \"update\"|\"add\", \"changed\": bool, \"diff\": str}].
   Gotcha: a stale hash that sits FAR from its stated line aborts the WHOLE batch
   (nothing written) — re-cat for fresh anchors. For whole-file writes use write."
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
           :metadata (cond-> {:target {:requested (str (or (:path first-failure) "."))
                                       :resolved nil
                                       :absolute nil
                                       :kind :file}
                              :mode :exact-replace
                              :started-at-ms (now-ms)
                              :finished-at-ms (now-ms)
                              :duration-ms 0}
                      ;; Whole-batch candidates on a SYNTAX refusal — metadata
                      ;; only (the model-facing throw carries `:error` alone),
                      ;; so a language pack's :around op-hook can whole-source-
                      ;; repair the broken files and commit the batch itself.
                       (:candidate-plans result)
                       (assoc :candidate-plans (:candidate-plans result)
                         :broken-paths   (:broken-paths result)))
           :error    {:message  (:message result)
                      :reason   (:reason first-failure)
                      :failures (:failures result)
                      :checks   (:checks result)
                      :loop-hint (:loop-hint result)
                      :mode     :exact-replace}})))))

(defn- normalize-write-args
  "Accept write args EITHER as a single options map
   (`await write({\"path\": P, \"content\": S})`) OR positionally
   (`await write(P, S)` / `await write(P, S, {opts})`). Returns the
   canonical options map for `write-safe`."
  [args]
  (cond
    ;; single map → already canonical (also covers Clojure trailing-kwargs)
    (and (= 1 (count args)) (map? (first args)))
    (first args)
    ;; positional: path, content, optional trailing opts map
    (and (>= (count args) 2) (string? (first args)) (string? (second args)))
    (merge {:path (first args) :content (second args)}
      (let [extra (nth args 2 nil)] (when (map? extra) extra)))
    ;; legacy Clojure-style trailing kwargs (even k/v count)
    (and (pos? (count args)) (even? (count args)))
    (apply hash-map args)
    :else
    (throw (ex-info "write expects (path, content) or a single options map"
             {:type :ext.foundation.editing/invalid-write-args
              :got  (mapv type args)}))))

(defn- write-tool
  "Write a whole file — create or overwrite.
     await write(P, S)                                  # positional path, content
     await write({\"path\": P, \"content\": S})
     await write({\"path\": P, \"content\": S, \"is_overwrite\": False})   # fail if exists
     await write({\"path\": P, \"content\": S, \"expected_mtime\": MS})   # staleness guard

   Returns [{\"path\": P, \"op\": \"add\"|\"update\", \"changed\": bool, \"diff\": str}]
   (same per-file shape as patch, always one element).
   Gotcha: overwrites the whole file — use patch for surgical anchor edits.
   REFUSED on a file with uncommitted changes (:reason \"dirty\") — a whole-file
   write would clobber edits already in flight (e.g. a truncated reconstruction).
   For an existing file you're changing use patch(...)/struct_patch(...); write is
   for NEW files and clean overwrites. Override with allow_dirty=True."
  [& args]
  (let [result (write-safe (normalize-write-args args))]
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
       :result {:path out
                :created? (not before)
                :already-existed? before}
       :metadata {:created? (not before)
                  :already-existed? before}})))

(defn- copy-tool
  "Copy a path.
     await copy(src, dest)
     await copy(src, dest, {\"is_overwrite\": True})

   Returns {\"src\": src, \"dest\": dest, \"path\": dest}.
   Gotcha: without is_overwrite an existing dest fails."
  ([src dest & {:as opts}]
   (let [out (copy-safe src dest opts)]
     (tool-success
       {:op :copy
        :path dest
        :kind :path
        :result {:src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- move-tool
  "Move / rename a path.
     await move(src, dest)
     await move(src, dest, {\"is_overwrite\": True})

   Returns {\"src\": src, \"dest\": dest, \"path\": dest}.
   Gotcha: without is_overwrite an existing dest fails."
  ([src dest & {:as opts}]
   (let [out (move-safe src dest opts)]
     (tool-success
       {:op :move
        :path dest
        :kind :path
        :result {:src    src
                 :dest   dest
                 :path   out}
        :metadata {:src (path->target src :path)
                   :dest (path->target dest :path)
                   :opts opts}}))))

(defn- delete-tool
  "Delete a path.
     await delete(path)

   Returns {\"path\": path, \"deleted\": True}.
   Gotcha: errors if path is missing — use delete_if_exists to no-op instead."
  [path]
  (delete-safe path)
  (tool-success
    {:op :delete
     :path path
     :kind :path
     :result {:path path :deleted? true}
     :metadata {:deleted? true}}))

(defn- delete-if-exists-tool
  "Delete a path if it exists (no-op otherwise).
     await delete_if_exists(path)

   Returns {\"path\": path, \"deleted\": bool}.
   Gotcha: \"deleted\" is False when nothing was there — never raises on a missing path."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success
      {:op :delete-if-exists
       :path path
       :kind :path
       :result {:path path :deleted? deleted?}
       :metadata {:deleted? deleted?}})))

(defn- exists-tool
  "Check whether a path exists.
     await exists(path)

   Returns {\"path\": path, \"exists\": bool}.
   Gotcha: returns a dict, not a bare bool — read r[\"exists\"]."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success
      {:op :file-exists
       :path path
       :kind :path
       :result {:path   (str path)
                :exists? exists?}
       :metadata {:exists? exists?}})))

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
;; surface; everything else (examples, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(defn- outline-tool
  "Structural outline of a source file — a high-level, line-ranged skeleton via
   tree-sitter. Read this BEFORE cat: it maps each definition, nested by
   structure, to one line:
     <kind> <visibility> <name>  <signature>  @<start-anchor>..<end-anchor>
   with the first line of its doc string on an indented line below (when present).
   So you get kind (function/constant/…), visibility (public/private), the exact
   name, the arglist, the docstring gist, and the full start..end span — WITHOUT
   reading the body; cat just the range you need instead of the whole file. The
   <name> is the VERBATIM `struct_patch` target: copy it as-is (it is already
   clean — no `^:private`/type-hint noise), and pair it with <kind> when two defs
   share a name.
     await outline(path)
   Returns {\"skeleton\": \"...\", \"language\": \"...\"}. When a language has no
   structural outline yet, returns a note — fall back to cat(path)."
  [& args]
  ;; Accept outline("x") (positional) AND outline({"path":"x"}) — the native
  ;; tool-call path synthesizes the dict form.
  (let [a    (first args)
        path (cond (string? a) a
               (map? a)    (or (:path a) (get a "path"))
               :else       a)]
  ;; Resolve through safe-path (workspace-cwd confinement) like every other file
  ;; tool — file-skeleton's internal (slurp path) must NOT see a raw relative
  ;; path (that resolves against the JVM user.dir, not the workspace root, so a
  ;; nested `src/foo.clj` 404s while cat finds it).
    (let [f        (ensure-existing-file! (safe-path path))
          abs      (.getPath f)
          language (outline/detect-language abs)
          skeleton (when language (outline/file-skeleton abs (slurp f)))]
      (tool-success
        {:op :outline
         :path path
         :kind :file
         :result (cond
                   skeleton {:skeleton skeleton :language language :path path}
                   language {:language language
                             :path path
                             :note "No structural outline for this language yet — use cat(path)."}
                   :else    {:path path
                             :note "Unknown language — use cat(path)."})}))))

;; -----------------------------------------------------------------------------
;; Native-tool result renderers — `(result → markdown)`. The loop applies these
;; so a native tool's result shows as a clean card in BOTH the TUI and the web
;; (unified), surfacing only what matters — never the raw args+result dump. Tools
;; without a renderer fall back to a pretty-printed result (see the loop).
;;
;; CONTRACT: a renderer receives the tool's UNWRAPPED result value (the inner
;; `:result` the tool returns, e.g. cat's `{:path :anchors …}`) AFTER it has
;; round-tripped the GraalPy boundary — so keys are KEYWORDS in snake_case with a
;; trailing `?`/`!` stripped (`:exists?` → `:exists`), and even nested DATA keys
;; are keywordized (cat's anchor keys arrive as `:1:5ad`, not `"1:5ad"`). Write
;; renderers against THAT shape, not the tool's raw Clojure return.
;; -----------------------------------------------------------------------------

(defn- anchor-line-spans
  "Sorted contiguous line-number runs from cat anchor keys —
   `[[1 60] [370 405] …]`. nil when any key fails to parse, so the
   caller can fall back to a count-only summary instead of lying."
  [anchors]
  (let [nums (mapv (fn [k]
                     (parse-long (first (str/split (if (keyword? k) (name k) (str k)) #":"))))
               (keys anchors))]
    (when (and (seq nums) (every? some? nums))
      (reduce (fn [acc ^long x]
                (let [[a ^long b] (peek acc)]
                  (if (and b (= x (inc b)))
                    (conj (pop acc) [a x])
                    (conj acc [x x]))))
        [] (sort nums)))))

(defn- render-cat-result
  "cat → `{:summary :body}`: the summary is the path + the LINE SPANS read +
   line count (the op-card headline); the body is the numbered slice as a code
   block. `r` is the inner cat data `{:path :anchors}`; anchor keys are
   keywords like `:12:ab` (lineno via `name`).

   Spans exist so two adjacent ranged reads of the SAME file don't render as
   look-alike duplicate cards (session 128cefd8: `L1-60` then
   `L370-975 (6 ranges)` both showed only `app.css · N lines`). A single
   contiguous run shows just `L<a>-<b>` — the count is implied; multi-run
   shows the overall extent + run count + line total."
  [r]
  (let [line-no (fn [k] (first (str/split (if (keyword? k) (name k) (str k)) #":")))
        rows    (mapv (fn [[k v]] (str (format "%5s" (line-no k)) "  " v)) (:anchors r))
        n       (count rows)
        spans   (anchor-line-spans (:anchors r))
        span-str (fn [[a b]] (if (= a b) (str "L" a) (str "L" a "-" b)))
        loc     (cond
                  (nil? spans)        nil
                  (= 1 (count spans)) (span-str (first spans))
                  :else               (str "L" (ffirst spans) "-" (second (peek spans))
                                        " (" (count spans) " ranges)"))
        counted (str n " line" (when (not= 1 n) "s"))]
    {:summary (str "`" (:path r) "` · "
                (cond
                  (nil? loc)              counted
                  (= 1 (count spans))     loc
                  :else                   (str loc " · " counted)))
     :body    (when (seq rows) (str "\n```\n" (str/join "\n" rows) "\n```"))}))

(defn- render-exists-result
  "file_exists → `{:summary}` only (no body): the path + presence mark. `r` is
   `{:path :exists}` (the `?` is stripped by the boundary)."
  [r]
  {:summary (str "`" (:path r) "` " (if (:exists r) "exists ✓" "missing ✗"))})

(defn- kw->str
  "A round-tripped map KEY back to its string form, rebuilding a namespaced
   keyword (a path with `/`, e.g. `:src/foo.clj`) into the full path."
  [k]
  (cond
    (keyword? k) (if-let [ns (namespace k)] (str ns "/" (name k)) (name k))
    :else        (str k)))

(defn- rg-anchor-lineno
  "The leading line number from an `<lineno>:<hash>` anchor key (string form)."
  [k]
  (first (str/split (kw->str k) #":")))

(defn- rg-anchor-lineno-long
  "Numeric line number from an anchor key — for ORDERING. The result round-trips
   through GraalPy (`LinkedHashMap` → plain Clojure map), which drops insertion
   order, so the renderer must re-sort by line number itself."
  ^long [k]
  (try (Long/parseLong (rg-anchor-lineno k)) (catch Exception _ 0)))

(defn- rg-row
  "One `  <lineno>  <text>` gutter row for a match or a context line."
  [k txt]
  (str "  " (rg-anchor-lineno k) "  " (str/trimr (str txt))))

(defn- rg-hit-rows
  "Rows for ONE match anchor `k` → value `v`. Without a context window `v` is the
   bare matched line (a string). WITH one it is `{:text <match> :before {anchor→text}
   :after {anchor→text}}` — render the before-context, then the matched line, then
   the after-context, each as a line-numbered gutter row (sorted by line number)."
  [k v]
  (if (map? v)
    (let [ctx-rows (fn [m] (map (fn [[ck cv]] (rg-row ck cv))
                             (sort-by (comp rg-anchor-lineno-long key) m)))]
      (concat (ctx-rows (:before v))
        [(rg-row k (:text v))]
        (ctx-rows (:after v))))
    [(rg-row k v)]))

(defn- render-rg-result
  "rg → a `{:summary :body}` card for whichever MODE ran — each carries a DIFFERENT
   result shape, so a single content-only renderer (the old bug) showed
   `0 hits in N files` with no body for files-only:

     - content    `:matches` {path {anchor VALUE}} + `:hit_count`/`:file_count`
                  → `N hits in M files`, per-file matching lines.
     - files-only `:files [path…]` + `:file_count` (no per-line hits — that IS the
                  mode) → `M files`, the matching paths listed.

   Paths/anchors are keywords; a content VALUE is the bare matched line OR a
   `{:text :before :after}` context map (see `rg-hit-rows`)."
  [r]
  (let [fc    (or (:file_count r) 0)
        files-word (str fc " file" (when (not= 1 fc) "s"))
        ;; NAME what was searched on the headline — a bare "N hits in M files"
        ;; is useless without the term(s). Each OR needle is backtick-quoted
        ;; (the same chip style paths use); multiple terms join with " OR ".
        ;; `:needles` is snake-safe (no hyphen) across the render boundary.
        needles (seq (:needles r))
        query-chip (when needles
                     (str/join " OR " (map #(str "`" % "`") needles)))
        with-query (fn [tail] (if query-chip (str query-chip " · " tail) tail))]
    (cond
      ;; files-only — the matching FILES are the result; there are no per-line hits.
      (contains? r :files)
      {:summary (with-query files-word)
       :body    (when-let [files (seq (:files r))]
                  (str "\n```\n" (str/join "\n" (map #(str "  " (kw->str %)) files)) "\n```"))}
      ;; content (default) — per-line hits grouped by file.
      :else
      (let [hc    (or (:hit_count r) 0)
            files (for [[path hits] (:matches r)]
                    (str "`" (kw->str path) "`\n\n```\n"
                      (str/join "\n"
                        (mapcat (fn [[k v]] (rg-hit-rows k v))
                          (sort-by (comp rg-anchor-lineno-long key) hits)))
                      "\n```"))]
        {:summary (with-query (str hc " hit" (when (not= 1 hc) "s") " in " files-word))
         :body    (when (seq files) (str "\n" (str/join "\n\n" files)))}))))

(defn- render-patch-result
  "patch → `{:summary :body}`: the summary NAMES each file with its op
   (`update `path` · add `path`` …) so a single-file patch and a multi-file
   patch read the SAME way; only a large fan-out collapses to `first two +N
   more`. The body is the unified diff(s); for a SINGLE file it omits the
   per-file `op `path`` header (the summary already states it) and for
   MULTI-file it prefixes each diff with `op `path`` to disambiguate. `r` is a
   vector of per-file summaries `[{:path :op :changed :diff}]`."
  [r]
  (let [summaries (if (sequential? r) r [r])
        changed   (filterv :changed summaries)
        n         (count summaries)
        file-label (fn [{:keys [path op changed]}]
                     (str (if changed (str (name (or op :update)) " ") "(no change) ")
                       "`" path "`"))
        labels    (mapv file-label summaries)]
    {:summary (if (<= n 3)
                (str/join " · " labels)
                (str (str/join " · " (take 2 labels))
                  " · +" (- n 2) " more ("
                  (count changed) "/" n " changed)"))
     :body    (some->> (str/join "\n\n"
                         (for [{:keys [path op changed diff]} summaries]
                           (let [diff-block (when (and changed (seq (str diff)))
                                              (str "```diff\n" (str diff) "\n```"))]
                             (if (= n 1)
                               ;; single file: summary already names it — show just the diff
                               (or diff-block "")
                               (str (if changed (str (name (or op :update)) " ") "(no change) ") "`" path "`"
                                 (when diff-block (str "\n" diff-block)))))))
                not-empty
                       ;; leading blank = op-card BREATHE spacer (see tool-card-entries head-gap?)
                (str "\n"))}))

(defn- render-find-result
  "find → `{:summary :body}`: match-count summary + the ranked paths body. `r` is
   `{:paths [path…] :item_count :query}`."
  [r]
  (let [n (or (:item_count r) (count (:paths r)) 0)
        q (some-> (:query r) str not-empty)
        hint (some-> (:hint r) kw->str not-empty)
        ;; When the strict whole-query pass found nothing, find_files fell back
        ;; to per-TERM matching (see `find-search`). Name the terms that landed
        ;; so a fuzzy result reads honestly — `3 matches for "…" · terms: render,
        ;; native` — instead of implying an exact whole-query hit.
        terms (when (:fuzzy r)
                (seq (keep #(some-> % kw->str not-empty) (:matched_terms r))))]
    {:summary (str n " match" (when (not= 1 n) "es")
                (when q (str " for \"" q "\""))
                (when terms (str " · terms: " (str/join ", " terms))))
     :body    (cond
                (seq (:paths r))
                (str "\n```\n" (str/join "\n" (map #(str "  " (kw->str %)) (:paths r))) "\n```")
                ;; 0 results: show the steer (filename-vs-content) instead of a
                ;; blank card — the same hint the model reads, so the user sees
                ;; WHY it found nothing.
                hint (str "\n" hint))}))

(defn- render-outline-result
  "outline → `{:summary :body}`: a path headline (like cat) + the skeleton
   string (already anchored/nested). `r` is `{:skeleton str :language str :path str}`
   or a no-structure shape."
  [r]
  (let [loc (some-> (:path r) str not-empty (#(str "`" % "`")))]
    (if-let [sk (some-> (:skeleton r) kw->str not-empty)]
      {:summary (or loc "outline")
       :body    (str "\n```\n" sk "\n```")}
      {:summary (str (or loc "outline") " · no structural outline")})))

(defn- render-occurrences-result
  "occurrences → `{:summary :body}`: a `N · K defs in M files` headline (no leading
   'occurrences' word — the op-card badge already names it),
   then per file the DEFINITION(s) (kind/visibility/signature + span anchors) on their
   own lines and the use lines (derived from each use's anchor) compacted. `r` is
   wire-shaped: `{:name :files [{:path :occurrences [{:anchor :is_definition :kind
   :visibility :signature :end_anchor}]}] :count :definition_count}`."
  [r]
  (let [files (:files r)
        total (or (:count r) 0)
        defs  (or (:definition_count r) 0)
        fc    (count files)
        nm    (some-> (:name r) kw->str)]
    {:summary (str total
                (when (pos? defs) (str " · " defs " def" (when (not= 1 defs) "s")))
                " in " fc " file" (when (not= 1 fc) "s")
                (when nm (str " of `" nm "`")))
     :body (when (seq files)
             (str "\n" (str/join "\n\n"
                         (for [f files]
                           (let [occ (:occurrences f)
                                 ds  (filter :is_definition occ)
                                 us  (remove :is_definition occ)]
                             (str "`" (kw->str (:path f)) "`\n```\n"
                               (str/join "\n"
                                 (concat
                                   (for [d ds]
                                     (str "  def "
                                       (some-> (:kind d) kw->str)
                                       (when-let [v (:visibility d)] (str " " (kw->str v)))
                                       (when-let [s (:signature d)] (str "  " (kw->str s)))
                                       "  @" (kw->str (:anchor d)) ".." (kw->str (:end_anchor d))))
                                   (when (seq us)
                                     [(str "  used: " (str/join ", " (map #(rg-anchor-lineno (:anchor %)) us)))])))
                               "\n```"))))))}))

(defn- render-symbol-rename-result
  "symbol_rename → `{:summary :body}`: `renamed in N files` (+ any failures), then
   the changed paths. `r` is `{:files [{:path :changed}] :file_count :failed}`."
  [r]
  (let [files  (:files r)
        fc     (or (:file_count r) (count files))
        failed (:failed r)]
    {:summary (str "renamed in " fc " file" (when (not= 1 fc) "s")
                (when (seq failed) (str " · " (count failed) " failed")))
     :body    (when (seq files)
                (str "\n```\n" (str/join "\n" (map #(str "  " (kw->str (:path %))) files)) "\n```"))}))

(defn- render-ls-result
  "ls → `{:summary :body}`: entry-count summary + the directory entries body (dirs
   get a trailing `/`). `r` is `{:path :entries [{:type :name}] :entry_count}`."
  [r]
  (let [n     (or (:entry_count r) (count (:entries r)) 0)
        path  (some-> (:path r) kw->str)
        row   (fn [e] (str "  " (:name e) (when (= "dir" (some-> (:type e) name)) "/")))]
    {:summary (str n " entr" (if (= 1 n) "y" "ies") (when path (str " in `" path "`")))
     :body    (when (seq (:entries r))
                (str "\n```\n" (str/join "\n" (map row (:entries r))) "\n```"))}))

(defn- render-move-result
  "move → `{:summary}` only: `moved `src` → `dest``. `r` is `{:src :dest}`."
  [r]
  {:summary (str "moved `" (kw->str (:src r)) "` → `" (kw->str (:dest r)) "`")})

(defn- render-delete-result
  "delete → `{:summary}` only: `deleted `path`` (or a no-op note). `r` is
   `{:path :deleted}`."
  [r]
  {:summary (str (if (false? (:deleted r)) "nothing to delete at `" "deleted `")
              (kw->str (:path r)) "`")})

(defn- render-copy-result
  "copy → `{:summary}` only: `copied `src` → `dest``. `r` is `{:src :dest :path}`."
  [r]
  {:summary (str "copied `" (kw->str (:src r)) "` → `" (kw->str (:dest r)) "`")})

(defn- render-create-dirs-result
  "create_dirs → `{:summary}` only: created / already-existed note. `r` is
   `{:path :created :already_existed}`."
  [r]
  {:summary (str (if (:created r) "created dir `" "dir already exists `")
              (kw->str (:path r)) "`")})

(defn- render-sexpr-result
  "sexpr → `{:summary :body}`: a `<kind> @line..end_line` headline + the node's
   text as a code block. `r` is the zip shape `{:path :kind :line :end_line :text
   :children :can}`."
  [r]
  (let [kind (some-> (:kind r) kw->str)
        line (:line r)
        eol  (:end_line r)
        txt  (some-> (:text r) kw->str)]
    {:summary (str (or kind "node") (when line (str " @" line (when eol (str ".." eol)))))
     :body    (when (seq txt) (str "\n```\n" txt "\n```"))}))

;; -----------------------------------------------------------------------------
;; Conditional advertising — the tree-sitter structural editors are only useful
;; when the project actually contains code in a supported language. Gate them on
;; the (cached) project language scan so a docs/config/unsupported-language repo
;; isn't handed tools it can't use.
;; -----------------------------------------------------------------------------

(def ^:private structural-scan-languages
  "The `environment/languages` SCAN vocabulary names whose files tree-sitter can
   structurally edit. Mostly == `outline/code-languages`, but the SCAN names a few
   things differently — notably it rolls `sh`/`bash`/`zsh`/`fish` into `shell`,
   while tree-sitter calls it `bash` — so this is the reconciled set, NOT just
   `code-languages`. (Languages the scan doesn't recognize at all — e.g. `.elm`,
   `.jl` — simply don't appear, and `structural-supported?` fails OPEN on them.)"
  (conj outline/code-languages "shell"))

(defn structural-supported?
  "Whether the STRUCTURAL editors should be advertised for the current project:
   true when its language scan finds at least one file in a structurally-supported
   language. FAILS OPEN — a scan error, an empty/new repo, or an all-unrecognized
   tree all return true, so a useful editor is NEVER hidden on uncertainty. Only a
   project that scanned cleanly AND contains code, NONE of it structurally supported
   (a pure docs/config repo, or an unsupported-language project), returns false.
   `env` is ignored — the answer comes from the cached env snapshot, not per-call
   runtime state."
  [_env]
  (try
    (let [langs (get-in (environment/snapshot) [:languages :languages])]
      (if (seq langs)
        (boolean (some (fn [l] (contains? structural-scan-languages
                                 (some-> (:language l) str str/lower-case)))
                   langs))
        true))                    ;; nothing recognized → fail OPEN
    (catch Throwable _ true)))    ;; any failure → fail OPEN

(def outline-symbol
  (vis/symbol #'outline-tool
    {:symbol 'outline
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Structural skeleton of a CODE file via tree-sitter: every definition "
       "(kind, visibility, name, signature, doc-gist) with its start..end anchors, "
       "nested by structure — WITHOUT reading the bodies. Read it BEFORE `cat` to "
       "jump straight to the range you need (then `cat` that range), and to patch a "
       "whole def straight from its anchors.")
     :render render-outline-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Path to a source file."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :outline :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :outline :file nil)}))

(def cat-symbol
  (vis/symbol #'cat-tool
    {:symbol 'cat
     :native-tool? true
               ;; cat(path, {opts}) — path positional, the rest a Python options dict.
     :call {:pos ["path"] :rest :opt}
     :description
     (str "Read a file and get back its content as anchored lines "
       "(`lineno:hash` keys you can patch against). Optional slice: `range` "
       "[start,end] (1-based, inclusive), `ranges` [[s,e],…] for several windows, "
       "`anchor` A (one line) or [A1,A2] (inclusive span) by lineno:hash, or `tail` "
       "N (last N lines). Read GENEROUSLY — the whole region you'll touch — not "
       "tiny slices you then re-read.")
     :render render-cat-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path"   {:type "string" :description "File path (relative to a filesystem root or absolute under one)."}
                           "range"  {:type "array" :items {:type "integer"}
                                     :description "Optional [start,end] line range (1-based, inclusive)."}
                           "ranges" {:type "array" :items {:type "array" :items {:type "integer"}}
                                     :description "Optional several [[s,e],…] windows in one call."}
                           "anchor" {:description "Optional lineno:hash anchor — a string for ONE line, or [from,to] for an inclusive span."}
                           "tail"   {:type "integer" :description "Optional: read the last N lines (omit N → 2000)."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :cat :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :cat :file nil)}))

(def ls-symbol
  (vis/symbol #'ls-tool
    {:symbol 'ls
     :native-tool? true
               ;; ls(path, {opts}) — but a bare {opts} would bind to `path`, so when
               ;; opts are present force a leading path (default "."). Escape hatch:
               ;; returns RAW arg values (the engine renders them).
     :call (fn [input]
             (let [path (get input "path") opts (dissoc input "path")]
               (if (seq opts)
                 {:args [(or path ".") opts]}
                 {:args (if path [path] [])})))
     :description
     (str "List the entries of a directory `path` (default: the workspace root), grouped "
       "by directory. Opts: `depth` (recursion), `limit`, `is_files_only`, "
       "`is_dirs_only`, `is_hidden`, `is_respect_gitignore` (default true).")
     :render render-ls-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path"                 {:type "string" :description "Directory to list (default workspace root)."}
                           "depth"                {:type "integer" :description "Recursion depth (default shallow)."}
                           "limit"                {:type "integer" :description "Max entries before truncation (default 3000)."}
                           "is_files_only"        {:type "boolean" :description "List only files."}
                           "is_dirs_only"         {:type "boolean" :description "List only directories."}
                           "is_hidden"            {:type "boolean" :description "Include dotfiles."}
                           "is_respect_gitignore" {:type "boolean" :description "Honor .gitignore (default true)."}}
              :required []}
     :before-fn (path-protected-before-fn :ls :dir :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :ls :dir nil)}))

(def find-symbol
  (vis/symbol #'find-tool
    {:symbol 'find_files
     :native-tool? true
     :description
     (str "Typo-tolerant FUZZY file/path discovery (searches file NAMES/paths, not "
       "content — use rg for content). Use FIRST for vague names, concepts, unfamiliar modules. "
       "`query` fuzzy-matches the whole relative path, ranked by frecency; then `cat` "
       "the likely ones. Scope with `paths`.")
     :render render-find-result
     :color-role :tool-color/search
     :schema {:type "object"
              :properties {"query" {:type "string" :description "Fuzzy query — a name, concept, or partial path (matches the whole relative path)."}
                           "paths" {:type "array" :items {:type "string"} :description "Restrict the search to these paths."}}
              :required ["query"]}
     :before-fn (path-protected-before-fn :find_files :dir :read find-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :find_files :dir nil)}))

(def rg-symbol
  (vis/symbol #'rg-tool
    {:symbol 'rg
     :native-tool? true
     :description
     (str "Search file CONTENT (for file NAMES use find_files — it's fuzzy). `query` is a "
       "term or a LIST of terms matched as OR. SMART-CASE substring: a lowercase "
       "term matches any case (`rg(\"key\")` finds Key/KEY/keymap), a term with a "
       "capital is case-sensitive — so you rarely list variants. Scope with `paths` "
       "and `include` globs; `context` N adds surrounding lines; `is_files_only` "
       "returns just the files that contain a match. No regex / no AND — filter the "
       "hits in Python for those. A no-hit rg means the term is wrong OR the thing is "
       "absent — don't re-grep more synonyms; widen once to the stem or read a file you hold.")
     :render render-rg-result
     :color-role :tool-color/search
     :schema {:type "object"
              :properties {"query"    {:type "array" :items {:type "string"} :description "Term(s) to find — a line containing ANY matches (OR, smart-case substring). Pass a LIST [\"a\",\"b\"] (or a comma-separated string \"a, b\" — it's split into OR terms). Keep each term SHORT (an identifier/fragment), not a multi-word phrase. OR helps only when a term is REAL — a list of guessed synonyms just multiplies zero."}
                           "paths"    {:type "array" :items {:type "string"} :description "Restrict to these paths (default the whole tree)."}
                           "include"  {:type "array" :items {:type "string"} :description "Only files matching these globs, e.g. [\"**/*.clj\"]."}
                           "context"  {:type "integer" :description "Lines of context around each match."}
                           "is_files_only" {:type "boolean" :description "Return just the distinct files that contain a match, no per-line hits."}}
              :required ["query"]}
     :before-fn (path-protected-before-fn :rg :dir :read rg-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :rg :dir nil)}))

(def patch-symbol
  (vis/symbol #'patch-tool
    {:symbol 'patch
     :native-tool? true
               ;; patch(edits) OR patch({path, edits}) — carry a single-file top-level
               ;; `path` through so the {path, edits} form isn't dropped. One positional
               ;; arg either way. Escape hatch: returns the RAW value (engine renders).
     :call (fn [input]
             (if-let [p (get input "path")]
               {:args [{"path" p "edits" (get input "edits")}]}
               {:args [(get input "edits")]}))
     :description
     (str "Apply anchored edits. Each edit anchors to a `from_anchor` "
       "(a `lineno:hash` from a FRESH `cat`) — optionally a `to_anchor` for a span "
       "— and supplies `replace` text. ATOMIC: one bad anchor rejects the whole "
       "batch. Anchors go STALE after any write, so re-`cat` before editing again.\n"
       "SINGLE FILE: set top-level `path` and give each edit just "
       "{from_anchor, replace[, to_anchor]} (they inherit the path). "
       "MULTI FILE: omit top-level `path` and give each edit its own `path`.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"path"  {:type "string"
                                    :description "Single-file form: the file all `edits` apply to (then each edit omits `path`)."}
                           "edits" {:type "array"
                                    :description "Anchored edits. With top-level `path`: {from_anchor, replace[, to_anchor]}. Without: each item includes its own `path`."
                                    :items {:type "object"
                                            :properties {"path"        {:type "string" :description "File path (omit when top-level `path` is set)."}
                                                         "from_anchor" {:type "string" :description "lineno:hash from a fresh cat."}
                                                         "to_anchor"   {:type "string" :description "Optional end anchor for a span."}
                                                         "replace"     {:type "string" :description "Replacement text."}}
                                            :required ["from_anchor" "replace"]}}}
              :required ["edits"]}
     :before-fn (plan-gated-before-fn :patch :file :write patch-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :patch :file nil)}))

(def write-symbol
  ;; write reuses the patch channel renderer because its `:result`
  ;; shape is the same single-file summary (just always 1-file long).
  (vis/symbol #'write-tool
    {:symbol 'write
     :native-tool? true
     :description
     (str "Write a whole file — create or overwrite (per-file shape is patch's: "
       "[{`path`, `op`: add|update, `changed`, `diff`}]). Overwrites the ENTIRE "
       "file, so use patch/struct_patch for surgical edits. REFUSED on a file with "
       "uncommitted changes unless `allow_dirty`. For NEW files and clean overwrites.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"path"    {:type "string" :description "File path to create or overwrite."}
                           "content" {:type "string" :description "Full file content."}
                           "is_overwrite" {:type "boolean" :description "Overwrite an existing file (default true); false = fail if it exists."}
                           "allow_dirty"  {:type "boolean" :description "Allow writing a file with uncommitted git changes."}
                           "expected_mtime" {:type "integer" :description "Staleness guard: only write if the file's mtime matches this."}}
              :required ["path" "content"]}
     :before-fn (plan-gated-before-fn :write :file :write write-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :write :file nil)}))

(defn- struct-patch-tool
  "Structural edit via tree-sitter (every language). Locate the node EITHER by
   NAME or by a zipper PATH, then edit — the file is re-parsed and the write is
   REFUSED if it introduces a syntax error. This is the PREFERRED way to edit
   code; reach for patch(...) only for non-code text or unsupported languages.
     by name:  await struct_patch({\"path\": P, \"op\": \"rename\", \"target\": \"old\", \"code\": \"new\"})
     by path:  await struct_patch({\"path\": P, \"op\": \"replace\", \"at\": [2, 1], \"code\": S})
   ops (by NAME/`target`): replace | delete | insert_before | insert_after | append |
     add_doc | replace_doc | replace_node | rename | move_before | move_after.
     `delete` drops the named def entirely (= replace it with \"\"); it also works
     by PATH (`at`).
     `rename` rewrites identifier `target` to `code` EVERYWHERE it occurs — a
     syntax-safe global rename, far safer than a blind text replace_all.
     `move_before`/`move_after` RELOCATE the def `target` next to the def `anchor`
     (e.g. move a fn below a dependency it forward-references) — one step, no manual
     cut-and-paste:
       await struct_patch({\"path\": P, \"op\": \"move_after\", \"target\": \"helper\", \"anchor\": \"dep\"})
     `kind` (function/class/method/…) disambiguates same-named defs; `replace_node`
     swaps the UNIQUE sub-expr equal to `match` (scope with `target`).
   ops (by PATH/`at`): replace | insert_before | insert_after | append_child |
     prepend_child (append/prepend = inside the node, after last / before first
     child; delete = replace with \"\"). `at` is the named-child index path from
     sexpr(path); `nav` adds relative moves — the full clojure.zip vocabulary:
     down|d|b up|u|t left|l right|r first last next|n prev|p {child:i}
     {find:\"text\"} {find_kind:\"if_statement\"}. Navigate with sexpr(...) first,
     then edit the same path here.
   Locate targets with outline(path) / sexpr(path) / occurrences(name).
   Returns the [{\"path\", \"op\", \"changed\", \"diff\"}] shape as write."
  [& {:as args}]
  (let [path        (:path args)
        raw-op      (keyword (str/replace (name (or (:op args) :replace)) "_" "-"))
        ;; LENIENCY — do the obvious thing instead of erroring:
        ;;  • `delete` (by name OR path) = replace the located node with "" (there was
        ;;    no name-based delete op, so a model wanting to drop a dead def was stuck).
        ;;  • `replace_node` given a `target` but no `match` is really a name-based
        ;;    `replace` (the two are easy to confuse) — redirect instead of failing
        ;;    with "replaceNode requires both match and code".
        delete?     (= raw-op :delete)
        op          (cond
                      delete? :replace
                      (and (= raw-op :replace-node)
                        (str/blank? (str (:match args)))
                        (not (str/blank? (str (:target args))))) :replace
                      :else raw-op)
        code        (if delete? "" (:code args))
        new-content
        (if (contains? args :at)
          ;; PATH-based (the zipper): locate by named-child index path + moves.
          (let [lang   (or (zipper/detect-language path)
                         (throw (ex-info (str "Unknown language for " path " — use patch(...).")
                                  {:type :ext.foundation.editing/struct-unknown-language :path path})))
                source (slurp (safe-path path))
                nav    (zipper/navigate lang source (:at args) (:nav args))
                at     (if (:ok? nav)
                         (:path nav)
                         (throw (ex-info (get-in nav [:error :message] "navigation failed")
                                  {:type :ext.foundation.editing/struct-nav-error
                                   :reason (get-in nav [:error :reason])})))
                r      (zipper/edit lang source at op code)]
            (if (:ok? r)
              (:new-source r)
              (throw (ex-info (get-in r [:error :message] "structural edit failed")
                       {:type :ext.foundation.editing/struct-zip-error
                        :reason (get-in r [:error :reason]) :at at}))))
          ;; NAME/MATCH-based (the original StructuralApi surface).
          (structural/edit-source path (slurp (safe-path path))
            {:op op
             :target (:target args)
             :kind (some-> (:kind args) keyword)
             :code code
             :match (:match args)
             :anchor (:anchor args)}))
        ;; allow_dirty: a re-parsed structural edit is SAFE on a file with
        ;; uncommitted changes — the dirty-guard only blocks the raw `write`.
        result      (write-safe {:path path :content new-content :allow_dirty true})]
    (if (:success? result)
      (let [plan (:plan result)
            summary (patch-result-file-summary plan)]
        (tool-success
          {:op :struct_patch
           :path (:path plan)
           :kind :file
           :result [summary]
           :metadata {:mode :struct_patch
                      :file-count 1
                      :changed-count (if (:changed? summary) 1 0)
                      :op (:op plan)}}))
      (extension/failure
        {:result nil
         :op :struct_patch
         :metadata {:target {:requested (str path) :resolved nil :absolute nil :kind :file}
                    :mode :struct_patch}
         :error {:message (:message result)
                 :failures (:failures result)
                 :mode :struct_patch}}))))

(def struct-patch-symbol
  (vis/symbol #'struct-patch-tool
    {:symbol 'struct_patch
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Structural edit via tree-sitter (every language): locate a node by NAME "
       "(`target`) or by a zipper PATH (`at`/`nav` from sexpr), then edit — the file "
       "is RE-PARSED and the write REFUSED if it breaks syntax. PREFERRED over patch "
       "for code. ops by name: replace | insert_before | insert_after | append | "
       "add_doc | replace_doc | replace_node | rename (syntax-safe global) | "
       "move_before | move_after; `kind` disambiguates same-named defs. ops by path: "
       "replace | insert_before | insert_after | append_child | prepend_child. "
       "Returns the same [{`path`,`op`,`changed`,`diff`}] shape as patch/write.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"path"   {:type "string" :description "File to edit."}
                           "op"     {:type "string" :description "replace|delete|insert_before|insert_after|append|add_doc|replace_doc|replace_node|rename|move_before|move_after (by name) or replace|delete|insert_before|insert_after|append_child|prepend_child (by path). delete drops the located node. Default replace."}
                           "target" {:type "string" :description "Definition NAME to locate (name-based ops)."}
                           "code"   {:type "string" :description "Replacement/insertion source (or the new name for rename)."}
                           "kind"   {:type "string" :description "function/class/method/… — disambiguates same-named defs."}
                           "match"  {:type "string" :description "For replace_node: the unique sub-expr text to swap."}
                           "anchor" {:type "string" :description "For move_before/move_after: the def to relocate next to."}
                           "at"     {:type "array" :items {:type "integer"} :description "Named-child index path from sexpr(path) (path-based ops)."}
                           "nav"    {:type "array" :description "Relative zipper moves applied after `at` (strings or maps)."}}
              :required ["path"]}
     :before-fn (plan-gated-before-fn :struct_patch :file :write write-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :struct_patch :file nil)}))

;; -----------------------------------------------------------------------------
;; Structural ZIPPER — a language-neutral node cursor (tree-sitter), the synergy
;; partner to the name-based struct_patch ops: locate a def by name, then WALK
;; into it by path. Location = a vector of NAMED-child indices; relative moves
;; (down/up/next/prev) are path arithmetic. See editing.zipper.
;; -----------------------------------------------------------------------------

;; Move resolution now lives in editing.zipper/navigate (tree-aware: validates
;; boundaries, supports leftmost/rightmost/root + single-letter directions).

(defn- zip-clip [s n]
  (if (and (string? s) (> (count s) n)) (str (subs s 0 n) " …[clipped]") s))

(defn- zip-shape [r]
  {:path (:path r)
   :kind (:kind r)
   :line (:start-line r)
   :end_line (:end-line r)
   :named_child_count (:named-child-count r)
   :has_error (:has-error? r)
   :text (zip-clip (:text r) 2000)
   :sexp (zip-clip (:sexp r) 1200)
   :children (mapv (fn [c] {:idx (:idx c) :kind (:kind c) :head (zip-clip (:head c) 120)})
               (:children r))})

(defn- sexpr-tool
  "The tree-sitter ZIPPER cursor (clojure.zip / rewrite-clj vocabulary, any
   language). A node's location is a PATH = list of NAMED-child indices from the
   file root, so the cursor round-trips through async tool calls.
     await sexpr(path)                                    # root + its named children
     await sexpr(path, {\"at\": [2, 0]})                    # jump to an absolute path
     await sexpr(path, {\"at\": [2], \"nav\": [\"down\", \"right\"]})  # cursor moves
     await sexpr(path, {\"nav\": [{\"find\": \"my_fn\"}]})            # jump to a node by text
   nav moves — the full clojure.zip / rewrite-clj vocabulary (single-letter
   aliases): SIBLING/PARENT/CHILD down|d|b up|u|t left|l right|r leftmost|first
   rightmost|last root|home {\"child\": i}; DEPTH-FIRST next|n prev|p; SEARCH
   {\"find\": \"text\"} {\"find_kind\": \"if_statement\"}. Boundary / not-found moves
   FAIL CLOSED instead of going nowhere.
   Returns {\"path\", \"kind\", \"line\", \"end_line\", \"text\", \"sexp\",
   \"children\": [{\"idx\",\"kind\",\"head\"}], \"can\": {\"down\",\"up\",\"left\",\"right\",
   \"next\",\"prev\",\"index\",\"siblings\"}} — `can` shows which moves remain plus
   the cursor's `index` among its `siblings` (lefts = index, rights =
   siblings-1-index), so you navigate without probing. EDIT the node under the
   cursor with struct_patch({\"path\": P, \"op\": ..., \"at\": path})."
  [path & [opts]]
  (let [lang   (zipper/detect-language path)
        source (slurp (safe-path path))
        nav    (zipper/navigate lang source (:at opts) (:nav opts))]
    (if (:error nav)
      (extension/failure
        {:result nil :op :sexpr
         :metadata {:target {:requested (str path) :kind :file} :mode :sexpr}
         :error {:message (get-in nav [:error :message])
                 :reason (get-in nav [:error :reason]) :mode :sexpr}})
      (let [at (:path nav)
            r  (zipper/inspect lang source at)]
        (if (:error r)
          (extension/failure
            {:result nil :op :sexpr
             :metadata {:target {:requested (str path) :kind :file} :mode :sexpr}
             :error {:message (get-in r [:error :message])
                     :reason (get-in r [:error :reason]) :mode :sexpr}})
          (tool-success {:op :sexpr :path path :kind :file
                         :result (assoc (zip-shape r)
                                   :can (zipper/moves-available lang source at))}))))))

(def sexpr-symbol
  (vis/symbol #'sexpr-tool
    {:symbol 'sexpr
     :native-tool? true
               ;; sexpr(path, {opts}) — path positional, the rest a Python options dict.
     :call {:pos ["path"] :rest :opt}
     :active-fn structural-supported?
     :description
     (str "Read-only tree-sitter ZIPPER cursor (any language). A node's location is a "
       "PATH = named-child indices from the file root. Returns {`path`,`kind`,`line`,"
       "`end_line`,`text`,`sexp`,`children`,`can`} — `can` shows which moves remain. "
       "nav moves: down|d|b up|u|t left|l right|r first last next|n prev|p {child:i} "
       "{find:\"text\"} {find_kind:\"if_statement\"}. Get a PATH here, then edit it "
       "with struct_patch({path, op, at}).")
     :render render-sexpr-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Source file to navigate."}
                           "at"   {:type "array" :items {:type "integer"} :description "Absolute named-child index path to jump to."}
                           "nav"  {:type "array" :description "Relative cursor moves (strings or {find/child/find_kind} maps)."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :sexpr :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :sexpr :file nil)}))

;; sexpr_edit was FOLDED INTO struct_patch — which now takes a zipper `at`/`nav`
;; path as an alternative to a `target` name. ONE structural editor (locate by
;; name OR by path), so the model isn't choosing between two near-identical
;; mutation verbs. `sexpr` stays as the read-only navigator that produces paths.

(defn- occurrence->wire
  "One `structural/occurrences` entry → snake_case wire map. The `anchor` is the
   SOLE position (a `lineno:hash` patch handle — the line number lives in it, so no
   redundant `line`/`column`/byte fields, which unbounded would bloat the wire until
   it clips). A DEFINITION additionally carries its metadata + span (`end_anchor`)."
  [o]
  (cond-> {:anchor (:anchor o)}
    (:is-definition o) (assoc :is_definition true
                         :kind (:kind o) :visibility (:visibility o)
                         :signature (:signature o) :doc (:doc o)
                         :end_anchor (:end-anchor o))))

(defn- occurrences-tool
  "Every OCCURRENCE of an identifier across the project (or within `paths`), via
   tree-sitter — real identifier boundaries, never inside a bigger token / string
   / comment. The DEFINITION occurrences are MARKED with their kind, visibility,
   signature, doc, and full span; plain uses carry just location + a patch anchor:
     await occurrences(\"handle_click\")            # whole project
     await occurrences(\"foo\", paths=[\"src/api\"]) # scoped
   Result: {\"name\", \"files\": [{\"path\", \"occurrences\": [{\"anchor\"  # a use:
   the sole position, a `lineno:hash` patch handle
   , \"is_definition\": true, \"kind\", \"visibility\", \"signature\", \"doc\",
   \"end_anchor\"  # a DEFINITION: span = anchor..end_anchor, patch it directly
   }]}], \"count\", \"definition_count\", \"scanned\", \"failed\"}.
   ONE call answers both 'where is it defined' (filter is_definition in Python)
   AND 'where is it used'. Not truncated — loop/filter the structure in Python and
   print only what you need. Syntactic (no scope resolution): every same-named
   definition across the project is marked, so `definition_count` > 1 means the
   name is ambiguous — each carries its own path + signature to disambiguate."
  [& args]
  (let [[a & more] args
        spec (cond
               (and (= 1 (count args)) (string? a)) {:name a}
               (and (= 1 (count args)) (map? a)) a
               (and (= 2 (count args)) (string? a) (map? (first more))) (assoc (first more) :name a)
               (and (string? a) (even? (count more)) (every? keyword? (take-nth 2 more)))
               (assoc (apply hash-map more) :name a)
               (and (even? (count args)) (every? keyword? (take-nth 2 args))) (apply hash-map args)
               :else (throw (ex-info "occurrences takes occurrences(name) or occurrences(name, paths=[...])."
                              {:type :ext.foundation.editing/invalid-occurrences-args :got args})))
        name (:name spec)
        _ (when-not (and (string? name) (not (str/blank? name)))
            (throw (ex-info "occurrences needs a non-blank `name`."
                     {:type :ext.foundation.editing/invalid-occurrences-args :name name})))
        paths (let [p (or (:paths spec) ["."])] (if (string? p) [p] (vec p)))
        ;; rg prefilters the files that mention the name (smart-case would over-
        ;; match casing, but a definition keeps the name's case, so a case-
        ;; sensitive identifier still lands among these files); each is parsed.
        files (vec (or (:files (rg-search {:query [name] :is_files_only true :paths paths})) []))
        {:keys [per failed]}
        (reduce (fn [acc path]
                  (try
                    (let [occ (structural/occurrences path (slurp (safe-path path)) name)]
                      (cond-> acc
                        (seq occ) (update :per conj {:path path :occurrences (mapv occurrence->wire occ)})))
                    (catch Exception e
                      (update acc :failed conj {:path path :error (or (ex-message e) (str (class e)))}))))
          {:per [] :failed []} files)
        total (reduce + 0 (map #(count (:occurrences %)) per))
        defs  (reduce + 0 (map (fn [f] (count (filter :is_definition (:occurrences f)))) per))]
    (tool-success
      {:op :occurrences
       :kind :dir
       :result {:name name :files per :count total :definition_count defs
                :scanned (count files) :failed failed}
       :metadata {:name name :paths paths :count total :definition_count defs}})))

(def occurrences-symbol
  (vis/symbol #'occurrences-tool
    {:symbol 'occurrences
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Trace one identifier across the project via tree-sitter: every OCCURRENCE, "
       "with the DEFINITION(s) MARKED (`is_definition`, kind, visibility, signature, "
       "doc, and span `anchor`..`end_anchor`). Uses carry just location + a patch "
       "anchor. ONE call = both 'where defined' (filter is_definition) and 'where "
       "used'. Real identifier boundaries (not strings/comments/substrings); "
       "syntactic, so >1 definition means an ambiguous name (each has its path + "
       "signature). Scope with `paths`. Not truncated — filter in Python.")
     :render render-occurrences-result
     :color-role :tool-color/search
     :schema {:type "object"
              :properties {"name"  {:type "string" :description "Identifier to trace."}
                           "paths" {:type "array" :items {:type "string"} :description "Restrict to these paths (default whole project)."}}
              :required ["name"]}
     :tag :observation
     :on-error-fn (tool-failure-on-error :occurrences :dir nil)}))

(defn- symbol-rename-tool
  "Rename identifier `name` → `new_name` across the WHOLE project via tree-sitter
   — at real identifier boundaries (never a string / comment / larger token),
   RE-PARSED per file so a syntax-breaking rename is refused. For a Clojure
   NAMESPACE this is the cross-file ns rename: it rewrites the `(ns …)` form, every
   `:require`/`:use` target, and qualified `old.ns/sym` usages, while leaving local
   `:as` aliases intact (then move the defining file with move(old, new)). Returns
   {\"files\": [{\"path\", \"changed\"}], \"file_count\", \"failed\": [{\"path\",
   \"error\"}]}.
     await symbol_rename(\"foo.bar\", \"foo.baz\")     # ns or any symbol"
  [& args]
  (let [spec (cond
               (and (= 2 (count args)) (string? (first args)) (string? (second args)))
               {:name (first args) :new_name (second args)}
               (and (= 1 (count args)) (map? (first args))) (first args)
               (and (even? (count args)) (every? keyword? (take-nth 2 args))) (apply hash-map args)
               :else (throw (ex-info "symbol_rename takes symbol_rename(name, new_name)."
                              {:type :ext.foundation.editing/invalid-symbol-rename-args :got args})))
        name (:name spec)
        new_name (:new_name spec)
        _ (when-not (and (string? name) (not (str/blank? name)) (string? new_name) (not (str/blank? new_name)))
            (throw (ex-info "symbol_rename needs non-blank `name` and `new_name`."
                     {:type :ext.foundation.editing/invalid-symbol-rename-args :spec spec})))
        files (vec (or (:files (rg-search {:query [name] :is_files_only true})) []))
        out (reduce
              (fn [acc path]
                (try
                  (let [src  (slurp (safe-path path))
                        hits (structural/references path src name)]
                    (if (seq hits)
                      (let [renamed (structural/edit-source path src
                                      {:op :rename :target name :kind nil :code new_name :match nil})]
                        (write-safe {:path path :content renamed :allow_dirty true})
                        (update acc :changed conj path))
                      acc))
                  (catch Exception e
                    (update acc :failed conj {:path path :error (or (ex-message e) (str (class e)))}))))
              {:changed [] :failed []}
              files)]
    (tool-success
      {:op :symbol_rename
       :kind :dir
       :result {:files (mapv (fn [p] {:path p :changed true}) (:changed out))
                :file_count (count (:changed out))
                :failed (:failed out)}})))

(def symbol-rename-symbol
  (vis/symbol #'symbol-rename-tool
    {:symbol 'symbol_rename
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Rename an identifier `name` → `new_name` across the WHOLE project via "
       "tree-sitter — at real identifier boundaries (never a string/comment/larger "
       "token), re-parsed per file so a syntax-breaking rename is refused. For a "
       "Clojure NAMESPACE it's the cross-file ns rename (ns form + :require/:use + "
       "qualified usages; then move the defining file). Run `occurrences(name)` first "
       "to preview the blast radius.")
     :render render-symbol-rename-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"name"     {:type "string" :description "Current identifier / namespace."}
                           "new_name" {:type "string" :description "New identifier / namespace."}}
              :required ["name" "new_name"]}
     :tag :mutation
     :on-error-fn (tool-failure-on-error :symbol_rename :dir nil)}))

(def create-dirs-symbol
  (vis/symbol #'create-dirs-tool
    {:symbol 'create-dirs
     :native-tool? true
     :call {:pos ["path"]}
     :name "create_dirs"
     :description "Ensure a directory exists (creating parents), confined to filesystem roots. Returns {`path`, `created`, `already_existed`}."
     :render render-create-dirs-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Directory path to create."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :create-dirs :dir :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :create-dirs :dir nil)}))

(def copy-symbol
  (vis/symbol #'copy-tool
    {:symbol 'copy
     :native-tool? true
               ;; copy(src, dest, {opts}) — two positionals, the rest an options dict.
     :call {:pos ["src" "dest"] :rest :opt}
     :description "Copy a file or directory from `src` to `dest` (confined to filesystem roots). Without is_overwrite an existing dest fails."
     :render render-copy-result
     :color-role :tool-color/move
     :schema {:type "object"
              :properties {"src"  {:type "string" :description "Source path."}
                           "dest" {:type "string" :description "Destination path."}
                           "is_overwrite" {:type "boolean" :description "Overwrite an existing dest (default false)."}}
              :required ["src" "dest"]}
     :before-fn (path-protected-before-fn :copy :path :write first-two-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
    {:symbol 'move
     :native-tool? true
     :call {:pos ["src" "dest"]}
     :description "Move/rename a file or directory from `src` to `dest` (confined to filesystem roots)."
     :render render-move-result
     :color-role :tool-color/move
     :schema {:type "object"
              :properties {"src"  {:type "string" :description "Source path."}
                           "dest" {:type "string" :description "Destination path."}}
              :required ["src" "dest"]}
     :before-fn (path-protected-before-fn :move :path :write first-two-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :move :path nil)}))

(def delete-symbol
  (vis/symbol #'delete-tool
    {:symbol 'delete
     :native-tool? true
     :call {:pos ["path"]}
     :description "Delete a file or directory at `path` (confined to filesystem roots)."
     :render render-delete-result
     :color-role :tool-color/delete
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Path to delete."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :delete :path :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol #'delete-if-exists-tool
    {:symbol 'delete-if-exists
     :native-tool? true
     :call {:pos ["path"]}
     :name "delete_if_exists"
     :description "Delete a path if it exists, else no-op (never raises on a missing path). Returns {`path`, `deleted`: bool}."
     :render render-delete-result
     :color-role :tool-color/delete
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Path to delete if present."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :delete-if-exists :path :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :delete-if-exists :path nil)}))

(def file-exists-symbol
  (vis/symbol #'exists-tool
    {:symbol 'file-exists
     :native-tool? true
     :name "file_exists"
     :call {:pos ["path"]}
     :description "Check whether a file or directory `path` exists (confined to the filesystem roots)."
     :render render-exists-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Path to check."}}
              :required ["path"]}
     :before-fn (path-protected-before-fn :file-exists :path :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :file-exists :path nil)}))

(defn available-editing-symbols
  []
  [outline-symbol
   cat-symbol
   ls-symbol
   find-symbol
   rg-symbol
   patch-symbol
   write-symbol
   struct-patch-symbol
   sexpr-symbol
   occurrences-symbol
   symbol-rename-symbol
   create-dirs-symbol
   copy-symbol
   move-symbol
   delete-symbol
   delete-if-exists-symbol
   file-exists-symbol])

(defn- project-languages-line
  "One compact `Project languages: PRIMARY x · also y, z` line from the cached
   scan, or nil when nothing was detected. Orients the model on what the repo IS."
  []
  (try
    (let [langs   (get-in (environment/snapshot) [:languages :languages])
          primary (some-> (first langs) :language str not-empty)
          others  (->> (rest langs) (keep (comp not-empty str :language)) (take 5))]
      (when primary
        (str "Project languages: PRIMARY " primary
          (when (seq others) (str " · also " (str/join ", " others))) ".")))
    (catch Throwable _ nil)))

(defn available-editing-prompt
  "The editing extension's model-facing prompt. When the project contains NO
   structurally-supported code (`structural-supported?` false — e.g. a docs/config
   repo), the tree-sitter STRUCTURAL editors are neither advertised nor bound, so
   their tool names + strategy are OMITTED here too — the prompt stays consistent
   with what's actually callable, and the model is steered to anchor-based `patch`."
  []
  (let [struct? (structural-supported? nil)
        lang-line (project-languages-line)]
    (str/join "\n"
      (concat
        (keep identity
          [lang-line
           (str "Editing surface. All tools below are NATIVE (call directly — results come back as the tool result) AND also bound as Python symbols (usable inside python_execution): cat / find_files / rg / ls / patch / move / delete / copy / file_exists / write"
             (when struct? " / outline / struct_patch / sexpr / occurrences / symbol_rename")
             ". `doc(name)` gives any symbol's exact result shape + mechanics — read it instead of guessing. Canonical path only.")
           ""])
        (if struct?
          ["STRATEGY (λ phase; → produces; | alternatives; ¬ never; ✓ verify; structural-FIRST for code):"
           "  3 code lenses: outline = DEFINITIONS index (what's declared) | occurrences = every USE of a name across the repo + the definition(s) MARKED (is_definition; LEXICAL, not scope-resolved) | sexpr = NODE cursor (one sub-form)."
           "  λ inspect:"
           "    shape(file)   → outline(path)   # kind VISIBILITY name SIGNATURE + docstring, @anchor..end_anchor, NO body — read BEFORE cat on a def-bearing code file (skip it for data/config/mostly-side-effect files); the name is the VERBATIM struct_patch target"
           "    body(symbol)  → cat(path, {\"anchor\": [outline_anchor, outline_end_anchor]}) | sexpr(path, nav=[find(name)])   # one def's source"
           "    usages(name)  → occurrences(name)   # tree-sitter identifier hits across the repo, each anchor-carrying, definition(s) marked (filter is_definition) — NOT rg, NOT a per-file loop. LEXICAL: over-matches shadowed / unrelated same-named idents — scan the hits before acting on them."
           "    rename(name)  → struct_patch(path, op=rename) ONE file | symbol_rename(old, new) REPO-wide (Clojure ns: rewrites ns form + :require + qualified usages, keeps :as — then move the file)"
           "  λ edit  (structural by name/path, or patch by anchor — ALL code editors RE-PARSE and refuse a syntax break):"
           "    def(name)     → struct_patch(path, op, target=name, code)   # op ∈ replace|delete|insert_before|insert_after|append|add_doc|replace_doc|replace_node|rename; add kind=function/constant/… when two defs share a name"
           "    sub-def node  → sexpr(P, nav=…) to the exact node, then struct_patch at n[\"path\"]   # one arity, a cond branch, a form inside do/let, a #?(:clj) leg — the reach struct_patch-by-name CANNOT name"
           "    line | text   → patch([{\"path\": P, \"from_anchor\": \"lineno:hash\", \"replace\": R}])   # non-code text, or a config/markdown/string/comment line, or an unsupported language  (span: add \"to_anchor\")"
           "    new_file      → write(path, content)"
           "    refused? ladder: ambiguous name → add kind | node struct_patch can't name → sexpr | unsupported language/node → patch by anchor."
           "    ¬ (cat → rebuild → write)   # cat TRUNCATES large files — the #1 way work is lost"
           "    ✓ after editing CODE: repl_eval(language, …) to load + exercise the change; if that language has no repl, run_tests(language). The returned diff confirms the TEXT changed, NOT that the code is correct."]
          ;; No structurally-supported code in this project → anchor-based editing only.
          ["EDIT (no tree-sitter language detected here — structural editors are OFF):"
           "    line | text   → patch([{\"path\": P, \"from_anchor\": \"lineno:hash\", \"replace\": R}])   # anchor-only, atomic, re-parsed for known formats  (span: add \"to_anchor\"; delete: replace \"\")"
           "    new_file      → write(path, content)"
           "    ¬ (cat → rebuild → write)   # cat TRUNCATES large files — the #1 way work is lost"])
        [""
         "LOCATE — cheapest first: fresh anchors from THIS turn's cat/rg? use them in one patch batch (stale after any write/patch — re-cat before editing again). | know the path? cat(path) directly. | need file/module discovery? find_files(query) FIRST (fff fuzzy paths: vague names, typos, concepts). | know exact symbol/string/error? rg({\"any\": [\"literal\"]}) for line hits + patch anchors. | literal dir contents? ls(path). A no-hit rg = wrong term OR the thing is absent, NOT a cue to guess more synonyms — and don't re-grep a file you already hold, read it. Broad UNSCOPED grep dumps junk, but for a concept whose exact token you don't know ONE stem-grep SCOPED with paths/include beats several zero-hit guessed greps."
         ""
         "ESSENTIALS (full shapes + mechanics: doc(name)):"
         "  cat → its ONLY content key is c[\"anchors\"] = an ORDERED {\"lineno:hash\": text} map; there is NO \"lines\"/\"text\"/\"content\" key (c[\"lines\"] KeyErrors, the #1 mistake). To edit, pass a lineno:hash you see here as a patch from_anchor: patch([{\"path\": P, \"from_anchor\": \"lineno:hash\", \"replace\": R}]); span = add \"to_anchor\". Whole file by default; big files cat(path, {\"range\": [s, e]})."
         "  rg → ALWAYS a DICT, never a list: {\"matches\": {path: {\"lineno:hash\": text}}, \"hit_count\"} (every hit patchable by its anchor); is_files_only → {\"files\": [...]}. NEVER iterate rg(…) itself."
         "  patch → ANCHOR-ONLY (no search/replace, no \"nth\"); ATOMIC (one bad anchor → nothing changes, fix it and resend); anchors go STALE after ANY write (re-cat first). The returned diff IS your confirmation — don't re-cat to check. The anchor carries the LINE NUMBER, so repeated lines (a bare `}`, blanks) are NOT ambiguous. Delete = replace \"\"."
         "  write → a NEW file or deliberate full rewrite; REFUSED on a git-dirty file (allow_dirty=True). NEVER rebuild from cat output (truncation drops the tail)."]
        (when struct?
          ["  struct_patch / sexpr → tree-sitter, every language; doc(\"struct_patch\") / doc(\"sexpr\") for ops + the zipper nav vocabulary. File ops: exists / copy / move / delete."])
        [""
         "INVARIANTS"
         "  - Paths stay inside the workspace root."]))))

(def editing-symbols
  "Default editing symbol set for docs/tests."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests."
  (available-editing-prompt))
