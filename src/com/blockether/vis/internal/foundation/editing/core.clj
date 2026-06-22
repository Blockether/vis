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
        (rg spec)            ; -> {:hits :truncated-by}; spec = {:any [literal] :paths [src]}
                               ; OR {:all [lit1 lit2]}; no regex/query+opts shorthand

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
(def ^:private max-rg-result-bytes
  "Total-bytes ceiling on a content-mode rg result — pi DEFAULT_MAX_BYTES parity
   (50KB). Once the accumulated hit text crosses it, rg stops and marks
   `:truncated-by :bytes`, so a broad search returns a SMALL, useful slice
   instead of 250 fat hits the wire clip would then chop mid-structure."
  (* 50 1024))

;; Cached fff indexes keyed by canonical directory. rg is now fff-first:
;; fff owns workspace discovery/ranking, while this namespace only re-reads
;; returned candidate files to preserve exact line semantics and patch anchors.
(defonce ^:private rg-fff-indexes
  (atom {}))

(defn- rg-fff-index [^File root]
  (when-not (.isDirectory root)
    (throw (ex-info "rg fff index root must be a directory"
             {:type :ext.foundation.editing/invalid-rg-root
              :path (.getPath root)})))
  (let [k (.getCanonicalPath root)]
    (try
      (or (get @rg-fff-indexes k)
        (let [idx (fff/create {:base-path k
                               :watch? false
                               :ai-mode? true
                               :enable-content-indexing? true})]
          (fff/wait-for-scan idx 1500)
          (if-let [existing (get (swap! rg-fff-indexes
                                   (fn [m]
                                     (if (contains? m k)
                                       (do (.close ^java.io.Closeable idx) m)
                                       (assoc m k idx))))
                              k)]
            existing
            idx)))
      (catch Throwable t
        (throw (ex-info (str "rg requires fff for directory search, but fff failed for " k)
                 {:type :ext.foundation.editing/fff-unavailable
                  :path k}
                 t))))))

(defn- rg-fff-query [needles]
  (str/join " " needles))

(defn- rg-fff-candidate-files [roots op needles is-regex files]
  (let [by-canon (into {}
                   (map (fn [^File f] [(.getCanonicalPath f) f]))
                   files)
        queries (if (= op :any)
                  needles
                  (distinct (cons (rg-fff-query needles) needles)))
        mode (if is-regex :regex :plain)
        rel-paths (fn [base items]
                    (keep (fn [{:keys [relative-path]}]
                            (some-> (io/file base relative-path)
                              .getCanonicalPath))
                      items))
        candidate-keys
        (->> roots
          (mapcat
            (fn [^File root]
              (if (.isFile root)
                [(.getCanonicalPath root)]
                (let [idx (rg-fff-index root)
                      base (.getCanonicalFile root)]
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
                    queries)))))
          distinct)]
    (vec (keep by-canon candidate-keys))))

(def ^:private default-find-limit 50)
(def ^:private default-list-depth 10)
(def ^:private default-list-limit 3000)
(def ^:private render-preview-chars 3000)

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
;; as the absent trailer/rg caps (see ctx_renderer.clj
;; header + conversation ccee2e1f-16ee-4acf-8d93-b4505034c0de): a
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
  ;; workspace cwd plus any extra context roots bound for this turn. Relative
  ;; paths resolve against the primary root; an absolute path is taken as-is so
  ;; it may land under an added context root. The confinement check runs on
  ;; CANONICAL paths (symlinks resolved, e.g. macOS /tmp -> /private/tmp) so it
  ;; matches the canonical allowed roots AND a symlink that points outside every
  ;; root is rejected. `..` traversal that escapes all roots is rejected too.
  (when (str/blank? (str p))
    (throw (ex-info "Path is nil or blank - cat/rg/ls take a concrete path string; note rg returns a MAP, so use (:files r) or (keys (:matches r)), not the rg result itself"
             {:type :ext.foundation.editing/blank-path :path p})))
  (let [cwd       (workspace/cwd)
        canon     (fn [x] (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))
        cwd-canon (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd))))))
        ;; relative → under cwd; absolute → as-is. Canonical throughout so
        ;; symlinks (/tmp→/private/tmp) and `..` resolve before confinement.
        canonical (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd (str p)))))))
        mappings  (workspace/context-root-mappings)
        ;; The model addresses a context file by its REAL (trunk) path; remap it
        ;; transparently onto the rift clone where edits land. The remapped
        ;; target is ALWAYS under an allowed clone root, so confinement holds.
        target    (or
                    (when (.startsWith canonical cwd-canon) canonical)
                    (some (fn [{:keys [clone]}]
                            (let [cp (canon clone)] (when (.startsWith canonical cp) canonical)))
                      mappings)
                    (some (fn [{:keys [trunk clone]}]
                            (let [tp (canon trunk) cp (canon clone)]
                              (when (and (not= tp cp) (.startsWith canonical tp))
                                (.resolve cp (.relativize tp canonical)))))
                      mappings))]
    (when-not target
      (throw (ex-info (str "Path '" p "' escapes the allowed workspace roots")
               {:type :ext.foundation.editing/path-escape :path (str p)})))
    (.toFile target)))

(defn- ensure-existing-file! [^File f]
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
  (let [canon     (fn [x] (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))
        cwd-canon (canon (workspace/cwd))
        p         (.toPath (.getCanonicalFile f))]
    (cond
      (.startsWith p cwd-canon)
      (let [rel (str (.relativize cwd-canon p))]
        (if (str/blank? rel) "." rel))

      :else
      (or (some (fn [{:keys [trunk clone]}]
                  (let [cp (canon clone)]
                    (when (.startsWith p cp)
                      (str (.resolve (canon trunk) (.relativize cp p))))))
            (workspace/context-root-mappings))
        (str p)))))

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

(defn- mutation-atomic?
  "True when the model passed the `atomic` escape flag on this mutation call.
   Robust to keyword|string keys and to write's single arg-map vs patch's
   vec-of-edit-maps."
  [args]
  (let [a (first args)
        maps (cond (map? a) [a] (sequential? a) (filter map? a) :else [])]
    (boolean
      (some (fn [m] (or (:atomic m) (:atomic? m) (get m "atomic") (get m "atomic?")))
        maps))))

(defn- canonical-mutation-paths
  "Resolved (cwd-relative) distinct file paths this mutation call would touch —
   the unit the plan-gate counts, so `./b.clj` and `b.clj` are ONE file."
  [path-extractor args]
  (->> (extracted-paths path-extractor args)
    (map #(or (:resolved (path->target % :file)) (str %)))
    (remove nil?)
    distinct
    vec))

(defn- plan-gate-failure
  "Refusal envelope for the FORCING plan-gate — same shape as path-protected
   refusals so the loop surfaces it as a tool error the model reads and retries."
  [op kind msg]
  (let [t (now-ms)]
    (extension/failure
      {:result nil
       :op op
       :metadata {:started-at-ms t :finished-at-ms t :duration-ms 0}
       :error {:message msg
               :type :ext.foundation.editing/plan-required
               :reason :plan-required
               :hint msg
               :loop-hint msg
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
  "Compose path-protection (always) with the loop-injected FORCING plan-gate
   (`env :mutation-gate`, present only on write-intent content mutations). The
   gate is a POLICY CALLBACK — `{:op :paths :atomic?} -> refusal-string | nil` —
   so THIS layer stays decoupled from the ctx engine. Path-protection runs FIRST;
   the plan-gate only sees calls that cleared it. The callback records intent +
   the audit fact on the allow path; it returns a string ONLY to block."
  [op kind access path-extractor]
  (let [protect (path-protected-before-fn op kind access path-extractor)]
    (fn [env f args]
      (let [pre (protect env f args)]
        (if (contains? pre :result)
          pre
          (if-let [gate (:mutation-gate env)]
            (if-let [msg (gate {:op op
                                :paths (canonical-mutation-paths path-extractor args)
                                :atomic? (mutation-atomic? args)})]
              {:result (plan-gate-failure op kind msg)}
              pre)
            pre))))))

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

(defn- check-interrupt!
  "Throw `InterruptedException` when the worker thread has been interrupted
   (e.g. by `cancel!` cancelling the turn's worker future). Long recursive
   directory walks poll this so Esc aborts them promptly instead of running
   the whole tree to completion - the symptom when `/dir add ..` widens the
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
;; find
;; =============================================================================

(defn- coerce-find-spec [args]
  (let [[a b] args
        spec (cond
               (and (= 1 (count args)) (string? a)) {:query a}
               (and (= 2 (count args)) (string? a) (map? b)) (assoc b :query a)
               (and (= 1 (count args)) (map? a)) a
               :else (throw (ex-info
                              "find takes find(query), find(query, opts), or find({\"query\": q, ...})."
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

(defn- find-search [args]
  (let [{:keys [query paths limit is_hidden is_respect_gitignore]} (coerce-find-spec args)
        roots (mapv (fn [p]
                      (let [f (safe-path p)]
                        (when-not (.exists f)
                          (throw (ex-info (str "Path not found: " (.getPath f))
                                   {:type :ext.foundation.editing/path-not-found
                                    :path p})))
                        (.getCanonicalFile f)))
                paths)
        items (->> roots
                (mapcat (fn [^File root]
                          (if (.isFile root)
                            [{:path (rel-path root)
                              :file-name (.getName root)
                              :size (.length root)
                              :binary? false
                              :source :direct-file}]
                            (let [idx (rg-fff-index root)
                                  base (.getCanonicalFile root)]
                              (->> (:items (fff/search idx {:query query
                                                            :page-size limit}))
                                (keep (fn [{:keys [relative-path file-name git-status size modified frecency-score binary?] :as item}]
                                        (let [f (io/file base relative-path)]
                                          (when (and (or is_hidden (not (.isHidden f)))
                                                  (or (not is_respect_gitignore)
                                                    (not (ignored? (load-ignore-node base) f base))))
                                            (cond-> {:path (rel-path f)
                                                     :file-name (or file-name (.getName f))
                                                     :size size
                                                     :modified modified
                                                     :frecency-score frecency-score
                                                     :git-status git-status
                                                     :binary? (boolean binary?)}
                                              (:score item) (assoc :score (:score item))))))))))))
                (distinct)
                (take limit)
                vec)]
    {:items items
     :item-count (count items)
     :paths (mapv :path items)
     :query query
     :searched-paths paths
     :limit limit
     :truncated-by (if (>= (count items) limit) :limit :end-of-results)}))

(defn- find-tool
  "Fuzzy file/path discovery powered by fff.
     await find(\"workspace rift deps\")
     await find(\"renderer\", {\"paths\": [\"src\"], \"limit\": 20})
     await find({\"query\": \"lazy native download\", \"paths\": [\".\"]})

   Use find to locate likely files/modules when you do NOT know the exact path.
   Use rg for exact content/symbol/error-string search, cat once you know the path,
   and ls only when you need a literal directory listing.

   Returns {\"items\": [{\"path\": P, \"file_name\": N, ...}],
   \"paths\": [P...], \"item_count\", \"query\", \"searched_paths\", \"limit\"}."
  [& args]
  (let [{:keys [query searched-paths limit item-count truncated-by] :as out} (find-search args)]
    (tool-success
      {:op :find
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
     :context N   shorthand for :before N + :after N. Also accepts a
                  ripgrep-style map {:before N :after N} (same as setting
                  :before/:after directly).
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
  (let [allowed-keys #{:all :any :paths :path :files :include :glob :globs :exclude :excludes :is_hidden :is_respect_gitignore
                       :limit :context :before :after :is_files_only :is_counts :is_regex}
        unknown-keys (seq (remove allowed-keys (keys spec)))
        _ (when unknown-keys
            (throw (ex-info (str "rg spec has unknown keys: "
                              (str/join ", " (map #(if (keyword? %) (name %) (str %)) unknown-keys))
                              ". Allowed: all|any (exactly one), paths, include, exclude, limit,"
                              " context, before, after, is_files_only, is_counts, is_regex,"
                              " is_hidden, is_respect_gitignore.")
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :unknown (vec unknown-keys)
                      :allowed (vec (sort allowed-keys))})))
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
        ;; :glob / :globs are undocumented ripgrep-muscle-memory aliases
        ;; for :include (:excludes likewise for :exclude). Kept out of the
        ;; docstring on purpose; negation (!pat) still goes through :exclude.
        _ (when (< 1 (count (filter #(contains? spec %) [:include :glob :globs])))
            (throw (ex-info "rg spec must use only one of :include or its aliases (:glob / :globs)."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        include-key (cond (contains? spec :include) :include
                      (contains? spec :glob)    :glob
                      (contains? spec :globs)   :globs
                      :else                     nil)
        include-raw (when include-key
                      (vector-of-strings include-key nil))
        _ (when (and (contains? spec :exclude) (contains? spec :excludes))
            (throw (ex-info "rg spec must use only one of :exclude or :excludes."
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :spec spec})))
        exclude-key (cond (contains? spec :exclude)  :exclude
                      (contains? spec :excludes) :excludes
                      :else                      nil)
        exclude-raw (when exclude-key
                      (vector-of-strings exclude-key nil))
        ;; ripgrep muscle-memory: a leading ! on an include/glob
        ;; pattern is a negation -> peel it into :exclude (sans !).
        ;; Kept undocumented alongside :glob; canonical path stays
        ;; :include / :exclude.
        bang? (fn [s] (str/starts-with? s "!"))
        include (filterv (complement bang?) (or include-raw []))
        exclude (into (vec (or exclude-raw []))
                  (comp (filter bang?) (map #(subs % 1)))
                  (or include-raw []))
        nonneg-int! (fn [label v]
                      (when (some? v)
                        (when-not (and (integer? v) (not (neg? v)))
                          (throw (ex-info (str "rg " label " must be a non-negative integer")
                                   {:type :ext.foundation.editing/invalid-rg-spec
                                    :field label :got v})))))
        ;; :context is EITHER a shared non-negative integer (before == after) OR
        ;; a ripgrep-style map `{:before N :after N}` — the model naturally writes
        ;; the map form. Top-level :before / :after still override either way.
        ctx-spec (:context spec)
        _ (if (map? ctx-spec)
            (do (nonneg-int! ":context :before" (:before ctx-spec))
              (nonneg-int! ":context :after"  (:after  ctx-spec)))
            (nonneg-int! ":context" ctx-spec))
        _ (nonneg-int! ":before" (:before spec))
        _ (nonneg-int! ":after"  (:after  spec))
        _ (nonneg-int! ":limit"  (:limit  spec))
        limit-spec (:limit spec)
        _ (when (and limit-spec (not (pos? limit-spec)))
            (throw (ex-info "rg :limit must be a positive integer"
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :field :limit :got limit-spec})))
        limit (or limit-spec default-grep-limit)
        context-before (if (map? ctx-spec) (or (:before ctx-spec) 0) (or ctx-spec 0))
        context-after  (if (map? ctx-spec) (or (:after  ctx-spec) 0) (or ctx-spec 0))
        before-ctx (or (:before spec) context-before)
        after-ctx  (or (:after  spec) context-after)
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

;; rg hit/context text is kept FULL in the result value — never per-line
;; mutilated. The full line (and context) lives in `r["tN/iN/fN"]`, pickled and
;; rebound into the sandbox, so the model slices/indexes it in Python (e.g.
;; `r["tN/iN/fN"]["hits"][0]["text"][500:]`) instead of re-fetching. The wire
;; VIEW is bounded by the non-destructive 64KB per-observation clip
;; (loop/clip-form-repr), which points back to `r[...]`. The hit cap (default
;; 250) and total-bytes budget bound result SIZE only — collected hits stay
;; full, with `:truncated-by` set so the model narrows.

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
     {:hits   [{:path :line :text :anchor :before? :after?} ...] :truncated-by KW}  ;; content
   `:anchor` is the `<lineno>:<hash>` anchor for that line — the same one `cat`
   emits in `:anchors` — so a hit is directly patchable via `{:from_anchor <anchor>}`
   without a follow-up `cat`. Absent on blank lines.
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW}  ;; files-only
     {:counts [{:path P :count N} ...]                    :truncated-by KW}  ;; counts

   `:truncated-by` is `:limit` (hit count), `:bytes` (total-bytes budget), or
   `:end-of-results`. Hit/context `:text` is kept FULL (sliceable in Python via
   `r[...]`); only the wire VIEW is bounded by the 64KB per-observation clip."
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
                (rg-fff-candidate-files roots op needles is_regex))]
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
  [{:keys [edit-index path reason stale hash-error]}]
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
  #{:expected_mtime :expected_size :is_overwrite :atomic :atomic?})
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
   The key IS the patch from_anchor. Prefer edit(...)/edit_span(...): they copy
   the anchor plus cat's mtime/size into expected_mtime/expected_size guards.
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
     ;;   cat("p", {"hash": H})              cat("p", {"hash": [H1, H2]})
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

     (throw (ex-info "cat window must use {\"range\": [start, end]} or {\"hash\": [from, to]}"
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
  "Search file content.
     await rg({\"any\": [\"TODO\"], \"paths\": [\"src\"]})        # OR — any needle on a line
     await rg({\"all\": [\"def\", \"login\"]})                  # AND — all needles on same line
     await rg({\"any\": [\"login\"], \"is_files_only\": True})  # distinct paths only
     await rg({\"any\": [\"login\"], \"is_counts\": True})      # per-file counts

   Exactly one of \"all\"/\"any\". Opts (snake_case): paths (default [\".\"]),
   include [\"**/*.py\"], exclude, limit (default 250), context/before/after N,
   is_regex (default False = literal substring), is_hidden, is_respect_gitignore.

   Result depends on mode:
     content:        {\"matches\": {path: {\"lineno:hash\": text}}, \"hit_count\", \"file_count\", \"first_hit\"}
     is_files_only:  {\"files\": [...], \"file_count\"}
     is_counts:      {\"counts\": [{\"path\": P, \"count\": N}], \"total_matches\", \"file_count\"}
   With context/before/after the content value becomes
   {\"text\": match, \"before\": {anchor: text}, \"after\": {anchor: text}}.
   Gotcha: each \"matches\" key is a \"lineno:hash\" anchor — pass it AS patch from_anchor."
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
        ;; NO `:spec` echo in the model-facing payload: echoing the input
        ;; map back taught models a phantom "spec" INPUT key (`rg({...,
        ;; "spec": {}})`). The spec stays host-side on `:metadata` below
        ;; for channel labels.
        shared {:mode         mode
                :truncated-by (:truncated-by out)
                :paths        paths
                :limit        limit
                :is_regex       is_regex}
        result (case mode
                 :content
                 (let [hits          (vec (:hits out))
                       ordered-paths (distinct (map :path hits))
                       by-path       (group-by :path hits)
                       ctx?          (or (pos? before-ctx) (pos? after-ctx))
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
                     :total-matches (:total-matches result)))})))

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
  "Write a whole file — create or overwrite.
     await write({\"path\": P, \"content\": S})
     await write({\"path\": P, \"content\": S, \"is_overwrite\": False})   # fail if exists
     await write({\"path\": P, \"content\": S, \"expected_mtime\": MS})   # staleness guard

   Returns [{\"path\": P, \"op\": \"add\"|\"update\", \"changed\": bool, \"diff\": str}]
   (same per-file shape as patch, always one element).
   Gotcha: overwrites the whole file — use patch for surgical anchor edits."
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
      {:op :exists?
       :path path
       :kind :path
       :result {:path   (str path)
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

;; The MODEL sees `cat` as STRUCTURED data (no rendering) — the result map
;; serialized by `ctx-renderer/render-form-value`. The line-number gutter
;; (`<ln>│ text`, `patch/render-lineno-block`) is the HUMAN/channel display
;; surface only, used by `channel-render-cat` for the `:display` body.

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
  [{:keys [path next-offset truncated? anchors ranges]}]
  (let [;; the model result carries `:anchors`/`:ranges` as ordered
        ;; `{anchor text}` maps; convert back to `[ln text]` tuples for the
        ;; HUMAN line-number gutter.
        lines        (patch/anchor-map->tuples anchors)
        ranges       (mapv #(assoc % :lines (patch/anchor-map->tuples (:anchors %))) (vec ranges))
        range-labels (mapv (fn [{:keys [range]}]
                             (let [[start end] range]
                               (str start "-" end)))
                       ranges)
        line-count   (count lines)
        first-ln     (ffirst lines)
        ;; Channel/TUI display is a HUMAN surface — line-number gutter,
        ;; not the model's `<hash>│` edit-anchor gutter. Humans navigate
        ;; cat output by line number; the hash anchors live in the
        ;; model-facing `:lines`/`:anchors` payload (Vis session ac065988).
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

(defn- find-item-line
  [{:keys [path size git-status binary?]}]
  (str path
    (when (some? size) (str "  (" (human-size size) ")"))
    (when (and git-status (not= "clean" git-status)) (str "  " git-status))
    (when binary? "  binary")))

(defn- channel-render-find
  [{:keys [query items item-count searched-paths limit truncated-by]}]
  {:summary {:left   (ir-strong "FIND")
             :center (ir-code query)
             :right  (str (or item-count (count items)) " item" (when (not= 1 (or item-count (count items))) "s")
                       "  truncated-by=" (name (or truncated-by :none)))}
   :display (ir-root
              (ir-p "searched " (ir-code (str/join ", " searched-paths))
                (when limit (str "  limit=" limit)))
              (when (seq items)
                (ir-code-block "text"
                  (bounded-render-text
                    (str/join "\n" (map find-item-line items))))))})

(defn- rg-match-tuples
  "One match value from the grouped `:matches` map → `[[ln text]…]` tuples: its
   before-context, the match line, then its after-context. The value is either
   the bare match text (no context window) or `{:text :before :after}` with
   `:before`/`:after` as `{anchor→text}` maps."
  [anchor v]
  (let [ln (patch/anchor->line anchor)]
    (if (string? v)
      [[ln v]]
      (concat (patch/anchor-map->tuples (:before v))
        [[ln (:text v)]]
        (patch/anchor-map->tuples (:after v))))))

(defn- rg-matches->channel-groups
  "Grouped model-facing `:matches` (`{path → {anchor → value}}`) →
   `[{:path :lines [[ln text]…]}]` for the human channel render: each file's
   lines (match + context) line-sorted and de-duplicated."
  [matches]
  (mapv (fn [[path file-map]]
          {:path path
           :lines (->> file-map
                    (mapcat (fn [[a v]] (rg-match-tuples a v)))
                    (into (sorted-map))
                    (mapv (fn [[ln tx]] [ln tx])))})
    matches))

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
                   (rg-matches->channel-groups matches))))))
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
   :display (ir-root (ir-p (ir-code path)))})

(defn- channel-render-delete-if-exists
  [{:keys [path deleted?]}]
  (let [label (if deleted? "DELETE" "ABSENT")]
    {:summary {:left  (ir-strong label)
               :right (ir-code path)}
     :display (ir-root (ir-p (ir-code path)))}))

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

(def find-symbol
  (vis/symbol #'find-tool
    {:symbol 'find
     :before-fn (path-protected-before-fn :find :dir :read find-arg-paths)
     :tag :observation
     :render-fn channel-render-find
     :on-error-fn (tool-failure-on-error :find :dir nil)}))

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
     :before-fn (plan-gated-before-fn :patch :file :write patch-arg-paths)
     :tag :mutation
     :render-fn channel-render-patch
     :on-error-fn (tool-failure-on-error :patch :file nil)}))

(def write-symbol
  ;; write reuses the patch channel renderer because its `:result`
  ;; shape is the same single-file summary (just always 1-file long).
  (vis/symbol #'write-tool
    {:symbol 'write
     :before-fn (plan-gated-before-fn :write :file :write write-arg-paths)
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
   find-symbol
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
    ["Editing tools — bare Python functions: cat / find / rg / ls / patch / write + copy / move / delete / exists. Pure helpers: anchor / anchor_exact / edit / edit_span. Canonical path only."
     ""
     "FLOW"
     "  LOCATE — pick by what you already know, cheapest first:"
     "    0. Already located it?      → it's a FACT. Re-patch by from_anchor. DON'T grep/cat again."
     "    1. Know the path?           → cat(path) directly; use scoped rg({\"paths\": [dir], …}) only for exact content near it."
     "    2. Need file/module discovery? → find(query) FIRST. It is fff-powered fuzzy path discovery; returns {\"items\": [{\"path\": P, ...}], \"paths\": [P...]}. Use it for vague names, typos, concepts, and repo shape."
     "    3. Know exact content/symbol/error? → rg({\"any\": [\"literal\"]}) for exact line hits + patch anchors; cat the best files."
     "       rg ALWAYS returns a DICT, never a list. content→{\"matches\": {path: {\"lineno:hash\": text}}, \"hit_count\", \"file_count\"} — \"matches\" is a map KEYED BY PATH, each value a {anchor: text} map (every hit is patchable by that anchor). Iterate: for path, hits in r[\"matches\"].items(): for anchor, text in hits.items(): … . is_files_only→{\"files\": [paths]}; is_counts→{\"counts\": [{\"path\": P, \"count\": N}], \"total_matches\": T}. NEVER iterate rg(…) itself."
     "    4. Need literal dir contents? → ls(path). Do NOT use ls for fuzzy repo discovery; find is the discovery tool."
     "  Wide CONTENT grep is last resort, not default (dumps junk into context); prefer find → cat/rg."
     "  Read:   cat(path)  — whole by default; large files use one 400-500 line range:"
     "    cat(path, {\"range\": [start, end]})"
     "    cat(path, {\"ranges\": [[start, end], ...]})"
     "  cat RETURNS a dict whose ONLY content key is \"anchors\": an ORDERED map"
     "  {\"lineno:hash\": linetext} (e.g. {\"141:971\": \"  foo\"}), plus \"eof\"/\"truncated\"/\"mtime\"/\"size\"."
     "  There is NO \"lines\"/\"text\"/\"content\"/\"hashes\" key — c[\"lines\"] KeyErrors (the #1 mistake)."
     "  The KEY is the FULL `lineno:hash` anchor (BOTH coords; a bare hash like `971` is REJECTED)."
     "  Prefer helpers over loops: anchor(c, \"needle\") returns the unique matching anchor;"
     "  anchor_exact(c, \"full line\") matches exactly; pass nth=2 only for intentional duplicates."
     "  Build guarded patch maps with edit(c, P, \"needle\", R) or edit_span(c, P, \"start\", \"end\", R)."
     "  Those helpers copy cat mtime/size into expected_mtime/expected_size so stale anchors fail closed."
     "  Raw iteration still works: for anchor, t in c[\"anchors\"].items(): ...   (line number = anchor.split(\":\")[0])."
     "  PATCH is ANCHOR-ONLY — there is NO text search/replace; no {\"search\":…}, {\"edits\":…} or \"nth\"."
     "  Prefer patch over blind whole-file rewrites. patch is ATOMIC / all-or-nothing: a batch of"
     "  hunks to ONE file resolves against the file as you last read it (hunks don't shift each"
     "  other). RETURNS → every hunk applied, the diff is your confirmation (don't re-cat to"
     "  check). ERRORS → nothing changed, file exactly as before; fix the one failing hunk and resend."
     "  from_anchor (H) = the FULL `lineno:hash` anchor from cat's \"anchors\" (e.g. `141:971`, NEVER bare `971`):"
     "    patch([{\"path\": P, \"from_anchor\": H, \"replace\": R}])                       # one line"
     "    patch([{\"path\": P, \"from_anchor\": H1, \"to_anchor\": H2, \"replace\": R}])    # inclusive range H1..H2"
     "  Delete a line with \"replace\": \"\". Copy the WHOLE anchor cat gave you — never fabricate one"
     "  or reuse one from an earlier read; after ANY edit, re-cat for fresh anchors. Moved far from"
     "  its line? patch refuses (hash-misplaced) — re-cat and resend."
     "  Repeated-content lines (a bare `}`, a blank, a duplicated row) are NOT ambiguous: the anchor"
     "  carries the LINE NUMBER, so `205:971` and `141:971` are distinct — use the anchor for the line"
     "  you mean, or from_anchor..to_anchor to span a block. There is no `#N` ordinal."
     "  Whole files:  write({\"path\": P, \"content\": S})  — a NEW file or deliberate full rewrite; prefer patch for edits to existing files."
     "  File ops: is_exists(path)  copy(src, dest)  move(src, dest)  delete(path)"
     ""
     "INVARIANTS"
     "  - Paths stay inside the workspace root."]))

(def editing-symbols
  "Default editing symbol set for docs/tests."
  (available-editing-symbols))

(def editing-prompt
  "Default editing prompt for docs/tests."
  (available-editing-prompt))
