(ns com.blockether.vis.internal.foundation.editing.core
  "Filesystem tools exposed as bare symbols in the Python sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (cat path)            ; -> {:path :anchors {<N:hash> text…} :next-offset N? :truncated? B}
        (cat path n)          ; first n lines from line 1
        (cat path offset n)   ; n lines starting at line `offset` (1-based)
        (cat path :tail)      ; last 400 lines (tail)
        (cat path :tail n)    ; last n lines
        (ls path)             ; -> nested dict tree (name/path/type/size/children)
        (ls path opts)        ; opts keys: depth / is_hidden / is_respect_gitignore
        (rg query)           ; -> content hits; query = a term or list of terms (OR),
                               ; smart-case substring. Opts: paths/include/context/is_files_only

   2. Cwd-safe wrappers over the babashka.fs file API. `patch` is
      the canonical text edit surface:

        (cat path)
        (patch [edit-map])    ; keys: path / from_anchor / replace
        (create-dirs path)
        (copy src dest)
        (move src dest)
        (delete path)
        (delete-if-exists path)
        (exists? path)

   Hard guard: every path must stay inside the session's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.fff :as fff]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [com.blockether.vis.internal.foundation.editing.index :as index]
            [com.blockether.vis.internal.foundation.editing.structural :as structural]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            [com.blockether.vis.internal.foundation.environment.core :as environment]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.gitignore :as gitignore]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import (com.github.difflib DiffUtils UnifiedDiffUtils)
           (java.io File)))

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

(def ^:private rg-breadth-probe-limit
  "Max EXTRA files probed for breadth AFTER a content/files result hits its
   display cap. Past the cap rg keeps short-circuit-probing candidate files to
   report a true `total_file_count`; this bounds that tail so a hostile-needle
   full-tree scan (fff disabled) can't turn a truncated result into a whole-tree
   sweep. When the budget is exhausted `total_file_count` is a LOWER bound
   (`total_file_count_is_exact` false)."
  5000)

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

(def ^:private fff-scan-max-concurrency
  "Permit count for `fff-scan-semaphore`: the max number of FRESH fff index
   scans (rg / find_files / struct_occurrences — everything that goes through
   `rg-fff-open`) allowed to run at once. A fresh scan spins fff's own worker
   threads over the whole tree — cheap for a small repo (~11ms), but up to the
   `rg-fff-scan-timeout-ms` (~30s) ceiling for a large one — so an UNBOUNDED
   `gather(rg, rg, …)` of N searches could fan out into N simultaneous
   full-tree scans, N CPU-heavy scan groups grinding at once (the orphan-CPU
   shape). A small bound caps that blast radius while still overlapping enough
   scans to keep `gather` worthwhile. Cheap reads (cat / index) never spin an
   index and are NEVER bounded — they don't pass through here."
  4)

(defonce ^:private ^java.util.concurrent.Semaphore fff-scan-semaphore
  ;; FAIR (true) so a queued burst of scans drains in arrival order — no scan
  ;; starves behind a steady stream of later arrivals.
  (java.util.concurrent.Semaphore. fff-scan-max-concurrency true))

(defn- with-fff-scan-permit*
  "Run `thunk` holding ONE fff-scan permit: block (interruptibly) until a permit
   is free, then ALWAYS release it — even when `thunk` throws, or the waiting
   thread is interrupted (turn `cancel!` / eval timeout, which surfaces as an
   `InterruptedException` from `.acquire` and propagates, releasing nothing it
   never took). Guards ONLY the index BUILD (create + `wait-for-scan`);
   searching an already-scanned index is cheap and needs no permit, so the
   permit is dropped the moment the scan is ready — maximizing scan overlap."
  [thunk]
  (.acquire fff-scan-semaphore)
  (try (thunk) (finally (.release fff-scan-semaphore))))

(defn- rg-fff-open
  "Create a FRESH fff instance scoped to `root`, blocking until its initial
   scan completes. The caller owns the instance and must close it. The
   CPU-heavy build (create + scan) runs under `with-fff-scan-permit*`, so no
   more than `fff-scan-max-concurrency` fresh scans ever run at once — a
   `gather(rg, …)` fan-out queues past the bound instead of stampeding."
  ^java.io.Closeable [^File root]
  (when-not (.isDirectory root)
    (throw (ex-info "rg fff index root must be a directory"
                    {:type :ext.foundation.editing/invalid-rg-root :path (.getPath root)})))
  (with-fff-scan-permit*
    (fn []
      (let
        [k
         (.getCanonicalPath root)

         idx
         (try (fff/create {:base-path k
                           :watch? false
                           :ai-mode? true
                           :enable-content-indexing? true
                           :enable-mmap-cache? false})
              (catch Throwable t
                (throw (ex-info (str "rg requires fff for directory search, but fff failed for " k)
                                {:type :ext.foundation.editing/fff-unavailable :path k}
                                t))))]

        (when-not (fff/wait-for-scan idx rg-fff-scan-timeout-ms)
          (.close ^java.io.Closeable idx)
          (throw (ex-info "rg fff scan did not complete in time"
                          {:type :ext.foundation.editing/fff-scan-timeout
                           :path k
                           :timeout-ms rg-fff-scan-timeout-ms})))
        idx))))

(defn- rg-needle-hostile-to-fff?
  "fff's candidate pre-filter (fuzzy path + content grep) honors a needle as a
   REGEX/glob, so a needle with a quantifier/bracket char — e.g. an ear-muffed
   Clojure var `*workspace-root*`, `(defn foo`, `arr[0]` — matches NOTHING (or
   errors) and yields zero candidate files. With no fallback the literal
   `make-line-matcher` then never runs, so a query that SHOULD hit returns empty.
   `.`/`|`/`^`/`$` only OVER-match (still correct after the literal filter) and
   `.` is ubiquitous, so they stay on the fff fast path."
  [^String needle]
  (boolean (re-find #"[*+?(){}\[\]]" needle)))

(def ^:private rg-fff-enumerate-page-size 5000)

(defn- rg-fff-enumerate-all
  "The FULL file universe under `roots` via fff — the native, nested-`.gitignore`-aware
   index — INCLUDING dotfiles (the caller applies hidden/include filtering). Returns a
   File vec, deduped nowhere (roots are pre-deduped upstream). Pages through fff/search
   so a huge repo isn't silently truncated at one page.

   This is the hostile-needle fallback (a needle fff reads as regex/glob and can't grep):
   we still hand the literal `make-line-matcher` the correct git-respecting universe
   instead of a raw filesystem walk that would descend `.gitignored` trees (node_modules,
   target, …) at ~280× the cost."
  [roots]
  (vec
    (mapcat (fn [^File root]
              (if (.isFile root)
                [root]
                (with-open [idx (rg-fff-open root)]
                  (let [base (.getCanonicalFile root)]
                    (loop
                      [page-index 0
                       acc (transient [])]

                      (let
                        [{:keys [items total-matched]}
                         (fff/search
                           idx
                           {:query "" :page-index page-index :page-size rg-fff-enumerate-page-size})
                         acc (reduce (fn [a {:keys [relative-path]}]
                                       (cond-> a
                                         relative-path
                                         (conj! (io/file base relative-path))))
                                     acc
                                     items)]

                        (if (or (empty? items) (>= (count acc) (long (or total-matched 0))))
                          (persistent! acc)
                          (recur (inc page-index) acc))))))))
            roots)))

(defn- rg-hidden-below-root?
  "True when `f` sits under a HIDDEN segment BELOW one of `roots` (a dotdir/dotfile the
   default sweep hides). The root's OWN name is exempt — an explicit dot-root (`~/.vis`)
   is entered on purpose — mirroring the walk's `f == root` hidden-guard. Used only on the
   fff enumeration path, where fff surfaces dotfiles the manual walk skipped by descent."
  [roots ^File f]
  (let [fp (.toPath (.getCanonicalFile f))]
    (boolean (some (fn [^File root]
                     (let [rp (.toPath (.getCanonicalFile root))]
                       (when (.startsWith fp rp)
                         (some (fn [seg]
                                 (str/starts-with? (str seg) "."))
                               (iterator-seq (.iterator (.relativize rp fp)))))))
                   roots))))

(defn- rg-fff-candidate-files
  "Files under `roots` that MIGHT contain a needle, via fff — the fast, nested-
   `.gitignore`-aware universe rg then RE-VALIDATES with the literal `make-line-matcher`.
   fff-first: NO raw filesystem walk, so a `.gitignored` subtree is never descended.

   Normal needles → the union of fff fuzzy-PATH hits and fff native-GREP content hits
   (a superset of true matches; a path-only hit whose content doesn't match is harmlessly
   dropped downstream). A needle HOSTILE to fff (a quantifier/bracket char fff reads as
   regex/glob and would match nothing) → the FULL fff enumeration, so the literal matcher
   still sees every candidate. Returns a File vec, deduped by canonical path."
  [roots needles]
  (if (some rg-needle-hostile-to-fff? needles)
    (rg-fff-enumerate-all roots)
    (let
      [rel-files (fn [^File base items]
                   (keep (fn [{:keys [relative-path]}]
                           (some->> relative-path
                                    (io/file base)))
                         items))]
      (->> roots
           (mapcat
             (fn [^File root]
               (cond (.isFile root) [root]
                     :else (with-open [idx (rg-fff-open root)]
                             (let [base (.getCanonicalFile root)]
                               ;; doall: realize the lazy hits INSIDE with-open, before the fresh
                               ;; instance is closed.
                               (doall
                                 (mapcat
                                   (fn [query]
                                     (let
                                       [path-items
                                        (:items (fff/search idx {:query query :page-size 1000}))
                                        grep-items (:matches (fff/grep idx
                                                                       {:query query
                                                                        :mode :plain
                                                                        :page-limit 1000
                                                                        :max-matches-per-file 1
                                                                        :time-budget-ms 1500}))]

                                       (concat (rel-files base path-items)
                                               (rel-files base grep-items))))
                                   needles)))))))
           ;; dedup by canonical path, keep File objects
           (reduce (fn [acc ^File f]
                     (assoc acc (.getCanonicalPath f) f))
                   {})
           vals
           vec))))


(def ^:private default-find-limit 50)

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

(def ^:private temp-roots
  "System temp dirs (`/tmp` and the JVM `java.io.tmpdir`, e.g. `$TMPDIR`) the file
   tools may ALWAYS reach, independent of the workspace roots. Canonical (symlinks
   resolved, so macOS `/tmp` -> `/private/tmp`), computed once on first use; a
   non-existent/unresolvable entry is dropped."
  (delay (->> [(System/getProperty "java.io.tmpdir") "/tmp"]
              (keep (fn [s]
                      (when-not (str/blank? (str s))
                        (try (.toPath (.getCanonicalFile (java.io.File. ^String (str s))))
                             (catch Throwable _ nil)))))
              distinct
              vec)))

(def ^:private vis-always-roots
  "Dirs under `~/.vis` the file tools may ALWAYS reach, independent of the
   workspace roots: the Python-extension dir `~/.vis/extensions` (author/debug an
   extension in any project) and the log dir `~/.vis/logs` (read vis's own
   diagnostics). Canonical (symlinks resolved), computed once on first use;
   dropped when `user.home` is unset. Kept SEPARATE from `temp-roots`: a write
   here is NOT captured as a session attachment (only temp writes are), and the
   secret-bearing rest of `~/.vis` — `config.edn`, the session DB, `gateway/`
   tokens — stays OUT of reach."
  (delay (->> [".vis/extensions" ".vis/logs"]
              (keep (fn [^String sub]
                      (some-> (System/getProperty "user.home")
                              (java.io.File. sub))))
              (keep (fn [^java.io.File f]
                      (try (.toPath (.getCanonicalFile f)) (catch Throwable _ nil))))
              distinct
              vec)))

(defn- under-temp-root?
  "True when `f` canonicalizes under a system temp root (`/tmp`, `$TMPDIR`)."
  [^File f]
  (try (let [^java.nio.file.Path cp (.toPath (.getCanonicalFile f))]
         (boolean (some (fn [^java.nio.file.Path tr]
                          (.startsWith cp tr))
                        @temp-roots)))
       (catch Throwable _ false)))

(defn- capture-temp-write!
  "Stream a just-written TEMP file (under `/tmp` or `$TMPDIR`) to the DB as a
   `session_iteration_attachment` — the native-tool twin of the sandbox OUTBOX
   tap. A no-op for a non-temp path, or when no capture sink is bound (the file
   tool ran outside a driven block). NEVER throws — a capture must not break an
   edit."
  [^File f]
  (try (when (under-temp-root? f) (mpl-capture/record-file! (.toPath f))) (catch Throwable _ nil))
  nil)

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
    (throw
      (ex-info
        "Path is nil or blank - cat/rg/ls take a concrete path string; note rg returns a MAP, so use (:files r) or (keys (:matches r)), not the rg result itself"
        {:type :ext.foundation.editing/blank-path :path p})))
  (let
    [cwd
     (workspace/cwd)

     canon
     (fn ^java.nio.file.Path [x]
       (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))

     ^java.nio.file.Path cwd-canon
     (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd))))))

     ;; relative → under cwd; absolute → as-is. Canonical throughout so
     ;; symlinks (/tmp→/private/tmp) and `..` resolve before confinement.
     ^java.nio.file.Path canonical
     (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd (str p)))))))

     mappings
     (workspace/filesystem-root-mappings)

     ;; The model addresses a context file by its REAL (trunk) path; remap it
     ;; transparently onto the rift clone where edits land. The remapped
     ;; target is ALWAYS under an allowed clone root, so confinement holds.
     ^java.nio.file.Path target
     (or (when (.startsWith canonical cwd-canon) canonical)
         (some (fn [{:keys [clone]}]
                 (let [^java.nio.file.Path cp (canon clone)]
                   (when (.startsWith canonical cp) canonical)))
               mappings)
         (some (fn [{:keys [trunk clone]}]
                 (let
                   [^java.nio.file.Path tp
                    (canon trunk)

                    ^java.nio.file.Path cp
                    (canon clone)]

                   (when (and (not= tp cp) (.startsWith canonical tp))
                     (.resolve cp (.relativize tp canonical)))))
               mappings)
         ;; system temp dirs (/tmp, $TMPDIR) + the always-on vis dirs
         ;; (~/.vis/extensions, ~/.vis/logs) are ALWAYS reachable, independent of the
         ;; workspace roots — /tmp scratch and extension authoring just work.
         ;; LAST so an isolated draft's trunk↔clone remap still wins first.
         (some (fn [^java.nio.file.Path tr]
                 (when (.startsWith canonical tr) canonical))
               (concat @temp-roots @vis-always-roots)))]

    (when-not target
      (throw (ex-info (str "Path '" p "' escapes the allowed workspace roots")
                      {:type :ext.foundation.editing/path-escape :path (str p)})))
    (.toFile target)))

(defn- ensure-existing-file!
  ^File [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
                    {:type :ext.foundation.editing/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
                    {:type :ext.foundation.editing/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path
  [^File f]
  ;; Reverse of safe-path's remap so the address the model SEES round-trips:
  ;; a file under the primary cwd renders RELATIVE; a file under a context
  ;; CLONE renders as its REAL (trunk) absolute path — never the ~/.vis/drafts
  ;; clone path. Anything else falls back to the absolute path.
  (let
    [canon
     (fn ^java.nio.file.Path [x]
       (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))

     ^java.nio.file.Path cwd-canon
     (canon (workspace/cwd))

     ^java.nio.file.Path p
     (.toPath (.getCanonicalFile f))]

    (cond (.startsWith p cwd-canon) (let [rel (paths/unixify (.relativize cwd-canon p))]
                                      (if (str/blank? rel) "." rel))
          :else (or (some (fn [{:keys [trunk clone]}]
                            (let
                              [^java.nio.file.Path cp
                               (canon clone)

                               ^java.nio.file.Path tp
                               (canon trunk)]

                              (when (.startsWith p cp)
                                (paths/unixify (.resolve tp (.relativize cp p))))))
                          (workspace/filesystem-root-mappings))
                    (str p)))))


(defn- nearest-existing-dir
  "Climb `f` to its nearest ancestor that EXISTS as a directory AND still lies
   WITHIN the allowed roots. A file/path that is GONE resolves to the closest real
   directory above it — parent, then parent-of-parent, … — but the climb is BOUNDED
   by confinement: it NEVER ascends past the allowed roots (the primary cwd, the
   bound filesystem-root clones, and the always-on temp/vis dirs that `safe-path`
   admits). An ancestor `safe-path` would reject stops the climb, so a stale/typo'd
   path resolves to the nearest real directory INSIDE the workspace — at worst an
   allowed root itself — never a directory above the confinement boundary (e.g. `/`
   or a parent outside the workspace). Returns nil only if nothing in the confined
   chain exists."
  [^File f]
  (letfn [(confined? [^File d]
            ;; safe-path is the single source of truth for confinement: it accepts
            ;; a dir under any allowed root and throws :path-escape otherwise.
            (try (safe-path (.getPath d)) true (catch clojure.lang.ExceptionInfo _ false)))]
    (loop [^File cur f]
      (cond (nil? cur) nil
            (not (confined? cur)) nil
            (and (.exists cur) (.isDirectory cur)) cur
            :else (recur (.getParentFile cur))))))

(defn- resolve-search-roots
  "Resolve rg/find `paths` into `{:roots [File …] :resolutions [{…} …]}`.

   `:roots` are the canonical Files actually searched — a FILE root is searched as
   that ONE file, a DIRECTORY root is walked as a tree (ripgrep / Claude-Code /
   Codex semantics: `rg PATTERN a.clj src/` greps `a.clj` as a file and `src/` as
   a tree in one pass; the whole downstream already special-cases `.isFile`).

   A path that DOES NOT EXIST is NOT silently dropped: it CLIMBS to its nearest
   existing ANCESTOR directory (parent, then parent-of-parent, …) via
   `nearest-existing-dir`, so a stale path still searches the closest real place
   instead of finding nothing. The climb is NOT silent — each requested path is
   recorded in `:resolutions` as `{:requested :resolved :root :existed :climbed}`,
   so the caller reports `missing_paths` (what you named that was gone + where it
   searched instead). Honest middle ground: still productive (climbs like the
   original), never misleading (reports what it couldn't find).

   The DEFAULT/unscoped `[\".\"]` (or a BLANK/nil entry, which the model routinely
   tacks on, e.g. `[\".github\" \"\"]`) expands to the FULL allowed-roots set — the
   primary cwd PLUS every bound filesystem-root clone — and carries NO
   `:resolutions` (a default sweep names nothing, so nothing is reportable).

   Explicit paths resolve through `safe-path` (confinement + trunk↔clone remap); a
   confinement violation still propagates — that is not a miss."
  [paths]
  (let
    [paths (mapv #(let [s (str/trim (str %))]

                    (if (str/blank? s) "." s))
                 paths)]
    (if (some #{"."} paths)
      {:roots (mapv io/file (workspace/allowed-roots)) :resolutions []}
      (let
        [resolutions
         (mapv (fn [p]
                 (let [^File f (safe-path p)]
                   (if (.exists f)
                     {:requested p :resolved (rel-path f) :root f :existed true :climbed false}
                     (let [anc (nearest-existing-dir f)]
                       {:requested p
                        :resolved (when anc (rel-path anc))
                        :root anc
                        :existed false
                        :climbed (boolean anc)}))))
               paths)
         roots (into [] (comp (keep :root) (distinct)) resolutions)]

        {:roots roots :resolutions resolutions}))))

(defn- missing-search-paths
  "From `resolve-search-roots` `:resolutions`, the requested paths that did NOT
   exist — each `{\"requested\" p \"searched\" ancestor-dir}` (`searched` = the
   nearest existing directory the search climbed to, omitted when nothing in the
   chain existed). Empty when every named path was real (and always empty for the
   default `.` sweep). Surfaced identically on both `rg` and `find_files` as
   `missing_paths` so a stale/typo'd path is reported, never silently absorbed."
  [resolutions]
  (into []
        (comp (remove :existed)
              (map (fn [{:keys [requested resolved]}]
                     (cond-> {"requested" requested}
                       resolved
                       (assoc "searched" resolved)))))
        resolutions))

(defn- ensure-parent-dirs!
  [^File f]
  (when-let [parent (.getParentFile f)]
    (.mkdirs parent))
  f)

(defn- now-ms [] (System/currentTimeMillis))

(defn- path->target
  [requested kind]
  (try (let [f (safe-path requested)]
         {:requested (str requested) :resolved (rel-path f) :absolute (.getPath f) :kind kind})
       (catch Throwable _ {:requested (str requested) :resolved nil :absolute nil :kind kind})))

;; =============================================================================
;; Extension protected paths
;; =============================================================================

(def ^:private protected-access-rank {:read-write 0 :read-only 1 :none 2})

(defn- extracted-paths
  [path-extractor args]
  (try (let [paths (path-extractor args)]
         (vec (remove nil? (if (sequential? paths) paths [paths]))))
       (catch Throwable _ [])))

(defn- first-arg-paths [args] (when (seq args) [(first args)]))

(defn- first-two-arg-paths [args] (take 2 args))

(defn- patch-arg-paths
  [args]
  (let
    [edits
     (first args)

     edits
     (cond (map? edits) [edits]
           (sequential? edits) edits
           :else [])]

    (keep #(get % "path") edits)))

(defn- write-arg-paths
  [args]
  (let [a (first args)]
    (cond (map? a) (when-let [path (get a "path")]
                     [path])
          (string? a) [a]
          :else nil)))

(defn- find-arg-paths
  [args]
  (let
    [a
     (first args)

     opts
     (second args)

     spec
     (cond (map? a) a
           (map? opts) opts
           :else nil)

     paths
     (cond (contains? spec "paths") (get spec "paths")
           (contains? spec "path") (get spec "path")
           :else nil)]

    (cond (or (nil? paths) (and (sequential? paths) (empty? paths))) ["."]
          (sequential? paths) paths
          :else [paths])))

(defn- rg-arg-paths
  [args]
  (let [spec (first args)]
    (when (map? spec)
      (let
        [paths (cond (contains? spec "paths") (get spec "paths")
                     (contains? spec "files") (get spec "files")
                     :else nil)]
        (cond (or (nil? paths) (and (sequential? paths) (empty? paths))) ["."]
              (sequential? paths) paths
              :else [paths])))))

(defn- protected-target
  [path kind]
  (let [target (path->target path kind)]
    (when (:resolved target) target)))

(defn- protected-glob-matches?
  [glob rel]
  (let
    [matcher
     (.getPathMatcher (java.nio.file.FileSystems/getDefault) (str "glob:" glob))

     rel
     (str/replace (str rel) (str (char 92)) "/")

     name
     (last (str/split rel #"/+"))]

    (boolean (some (fn [candidate]
                     (try (.matches matcher (fs/path candidate)) (catch Throwable _ false)))
                   (distinct [rel name])))))

(def ^:private glob-meta-chars #{\* \? \[ \] \{ \}})

(defn- glob-static-prefix
  [glob]
  (let
    [glob
     (str/replace (str glob) (str (char 92)) "/")

     idx
     (first (keep-indexed (fn [idx ch]
                            (when (contains? glob-meta-chars ch) idx))
                          glob))

     raw-prefix
     (if idx (subs glob 0 idx) glob)

     prefix
     (if (and idx (not (str/ends-with? raw-prefix "/")))
       (let [slash-idx (.lastIndexOf ^String raw-prefix "/")]
         (if (neg? slash-idx) "" (subs raw-prefix 0 slash-idx)))
       raw-prefix)

     prefix
     (str/replace prefix #"/+$" "")]

    (if (str/blank? prefix) "." prefix)))

(defn- path-prefix?
  [ancestor path]
  (let
    [ancestor
     (str/replace (str ancestor) (str (char 92)) "/")

     path
     (str/replace (str path) (str (char 92)) "/")]

    (or (= "." ancestor) (= ancestor path) (str/starts-with? path (str ancestor "/")))))

(defn- composite-path-target?
  [{:keys [kind absolute]}]
  (or (= :dir kind) (and (= :path kind) absolute (.isDirectory (io/file absolute)))))

(defn- protected-rule-matches?
  [target rule]
  (or (protected-glob-matches? (:glob rule) (:resolved target))
      (and (composite-path-target? target)
           (let
             [rel
              (:resolved target)

              prefix
              (glob-static-prefix (:glob rule))]

             (or (path-prefix? prefix rel)
                 (and (not= :read-write (:access rule)) (path-prefix? rel prefix)))))))

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
          (when (protected-rule-matches? target rule) rule))
        rules))

(defn- more-restrictive-rule
  [best rule]
  (if (or (nil? best)
          (> (long (protected-access-rank (:access rule)))
             (long (protected-access-rank (:access best)))))
    rule
    best))

(defn- resolve-protected-access
  [rules target]
  (reduce (fn [best extension-rules]
            (if-let [match (first-matching-rule target extension-rules)]
              (more-restrictive-rule best match)
              best))
          nil
          (rules-by-extension rules)))

(defn- blocked-access?
  [access-intent access]
  (or (= :none access) (and (= :write access-intent) (= :read-only access))))

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
  (let
    [t
     (now-ms)

     first-row
     (first blocked)

     first-tgt
     (:target first-row)

     first-hint
     (:hint first-row)

     failures
     (mapv protected-failure-row blocked)]

    (extension/failure
      {:result nil
       :op op
       :metadata {:target first-tgt
                  :started-at-ms t
                  :finished-at-ms t
                  :duration-ms 0
                  :access-intent access-intent
                  :protected-paths failures}
       :error {:message (str op
                             " blocked: "
                             (:resolved first-tgt)
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
       :metadata {:target (path->target "." kind) :started-at-ms t :finished-at-ms t :duration-ms 0}
       :error {:message "Protected path registry failed; refusing direct file operation."
               :type :ext.foundation.editing/path-protection-error
               :reason :path-protection-error
               :hint
               "Fix the extension's :ext/protected-paths callback before retrying direct file IO."
               :loop-hint
               "Fix the extension's :ext/protected-paths callback before retrying direct file IO."
               :cause (ex-message err)}})))

(defn- path-protected-before-fn
  [op kind access-intent path-extractor]
  (fn [env f args]
    (try (let
           [rules
            (extension/active-protected-globs env)

            targets
            (keep #(protected-target % kind) (extracted-paths path-extractor args))

            blocked
            (keep (fn [target]
                    (when-let [rule (resolve-protected-access rules target)]
                      (when (and (blocked-access? access-intent (:access rule))
                                 (not
                                   (current-dir-read-ancestor-match? op access-intent target rule)))
                        (assoc rule
                          :target target
                          :intent access-intent))))
                  targets)]

           (if (seq blocked)
             {:result (path-protected-failure op kind access-intent (vec blocked))}
             {:env env :fn f :args args}))
         (catch Throwable t {:result (path-protection-error-failure op kind t)}))))

(defn- mutation-atomic?
  "True when a write/patch args vector carries the documented `atomic` escape
   flag - on the write opts map, or on ANY patch edit map."
  [args]
  (let [a (first args)]
    (boolean (cond (map? a) (get a "atomic")
                   (sequential? a) (some #(and (map? %) (get % "atomic")) a)
                   :else false))))

(defn- plan-required-failure
  "Failure envelope for a write-intent op the env's `:mutation-gate` refused.
   `refusal` is the gate's human-readable reason string."
  [op kind paths refusal]
  (let [t (now-ms)]
    (extension/failure {:result nil
                        :op op
                        :metadata {:target (path->target "." kind)
                                   :started-at-ms t
                                   :finished-at-ms t
                                   :duration-ms 0
                                   :paths paths}
                        :error {:message (str refusal)
                                :type :ext.foundation.editing/plan-required
                                :reason :plan-required
                                :hint (str refusal)
                                :loop-hint (str refusal)
                                :paths paths}})))

(defn- plan-gated-before-fn
  "Write-intent gate for patch / write / struct_patch. Path-protection runs
   FIRST (owner-API refusals always win); only AFTER it clears does this
   consult the env's OPTIONAL `:mutation-gate`. The gate receives
   `{:op :paths :atomic?}` and returns a refusal string to short-circuit with a
   `:plan-required` failure, or nil to pass through. No `:mutation-gate` on the
   env = pass through unchanged (the gate is opt-in)."
  [op kind access path-extractor]
  (let [protect (path-protected-before-fn op kind access path-extractor)]
    (fn [env f args]
      (let [out (protect env f args)]
        (if (contains? out :result)
          out
          (if-let [gate (:mutation-gate env)]
            (let
              [paths (extracted-paths path-extractor args)
               refusal (gate {:op op :paths paths :atomic? (mutation-atomic? args)})]

              (if (and (string? refusal) (not (str/blank? refusal)))
                {:result (plan-required-failure op kind paths refusal)}
                {:env env :fn f :args args}))
            out))))))

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
      {:result result
       :op op
       :metadata
       (merge {:target (path->target path kind) :started-at-ms t :finished-at-ms t :duration-ms 0}
              metadata)})))

(defn- tool-failure-on-error
  [op kind _render-fn]
  (fn [err _env _f args]
    (let
      [path
       (first args)

       target
       (path->target path kind)

       interrupted?
       (instance? InterruptedException err)

       t
       (now-ms)

       error
       (when interrupted?
         {:message (str (name op) " interrupted while running; operation was cancelled.")})]

      {:result (extension/failure
                 {:result nil
                  :op op
                  :metadata (cond->
                              {:target target :started-at-ms t :finished-at-ms t :duration-ms 0}
                              interrupted?
                              (assoc :interrupted?
                                true :status
                                :interrupted))
                  :error error
                  :throwable (when-not error err)})})))

;; =============================================================================
;; .gitignore (cheap, lazy)
;; =============================================================================

(defn- load-ignore-node [^File root] (gitignore/load-matcher root))

(defn- ignored?
  [node ^File f ^File root]
  (when node
    ;; Match gitignore patterns against `/`-separated paths — a Windows `\`
    ;; would never match, leaking ignored files.
    (let
      [rel
       (paths/unixify (.relativize (.toPath root) (.toPath f)))

       dir?
       (.isDirectory f)]

      (gitignore/ignored? node rel dir?))))

(defn- search-overlay-matchers
  "Compiled `:search` config overlay (issue #23), or nil when unconfigured.
   `{:include <matcher> :include-prefixes [static-prefix…] :exclude <matcher>}` —
   both matchers compiled by `gitignore/compile-rules`, so the two config lists
   speak `.gitignore` pattern syntax. `:include-prefixes` are the patterns'
   static path prefixes: a gitignored DIRECTORY that is an ancestor of a prefix
   must still be DESCENDED (git itself never descends an excluded dir, which is
   exactly why a `.gitignore` `!` negation cannot re-include these subtrees —
   the overlay can, but only if the walker opens the ancestors)."
  []
  (when-let [{:keys [include-gitignored-paths always-exclude]} (config/search-overlay)]
    {:include (gitignore/compile-rules include-gitignored-paths)
     :include-prefixes (mapv glob-static-prefix include-gitignored-paths)
     :exclude (gitignore/compile-rules always-exclude)}))

(defn- overlay-included?
  "True when `rel` falls under an `:include-gitignored-paths` pattern — or, for
   a DIRECTORY, when it is an ancestor of a pattern's static prefix (the walker
   must open `repositories/` before `repositories/**` can match anything). A
   prefix of `.` (pattern starts with a meta char, e.g. `**/vendored/`) opens
   every directory: any gitignored dir might hold a match."
  [{:keys [include include-prefixes]} ^String rel dir?]
  (boolean (or (gitignore/ignored? include rel dir?)
               (and dir?
                    (some (fn [^String p]
                            (or (= "." p) (path-prefix? rel p)))
                          include-prefixes)))))

(defn- search-excluded?
  "Issue #23 exclusion formula:

     excluded?(f) = always-exclude?(f) OR (gitignored?(f) AND NOT included?(f))

   `overlay` nil degrades to plain `ignored?` — unconfigured projects keep
   today's behavior bit-for-bit. `ignore-node` nil (gitignore opt-out) leaves
   only the `:always-exclude` guard."
  [ignore-node overlay ^File f ^File root]
  (let
    [rel
     (paths/unixify (.relativize (.toPath root) (.toPath f)))

     dir?
     (.isDirectory f)]

    (or (boolean (and overlay (gitignore/ignored? (:exclude overlay) rel dir?)))
        (boolean (and ignore-node
                      (gitignore/ignored? ignore-node rel dir?)
                      (not (and overlay (overlay-included? overlay rel dir?))))))))

;; =============================================================================
;; cat
;; =============================================================================

(defn- validate-cat-args!
  [offset n]
  (when-not (and (integer? offset) (pos? (long offset)))
    (throw (ex-info "cat offset must be a positive integer (1-based line number)."
                    {:type :ext.foundation.editing/invalid-cat-args :offset offset})))
  (when-not (and (integer? n) (pos? (long n)))
    (throw (ex-info "cat limit must be a positive integer line count."
                    {:type :ext.foundation.editing/invalid-cat-args :limit n}))))

(defn- validate-cat-range!
  [start end]
  (when-not (and (integer? start)
                 (integer? end)
                 (pos? (long start))
                 (pos? (long end))
                 (<= (long start) (long end)))
    (throw (ex-info "cat \"range\"/\"ranges\" start/end must be positive ints with start <= end"
                    {:type :ext.foundation.editing/invalid-cat-args :start start :end end}))))

(defn- normalize-cat-ranges
  [ranges]
  (let
    [pairs (cond (and (vector? ranges) (= 2 (count ranges)) (every? integer? ranges)) [ranges]
                 (sequential? ranges) (vec ranges)
                 :else (throw (ex-info "cat \"ranges\" expects [[start, end], ...]"
                                       {:type :ext.foundation.editing/invalid-cat-args
                                        :ranges ranges})))]
    (when (empty? pairs)
      (throw (ex-info "cat \"ranges\" expects at least one range"
                      {:type :ext.foundation.editing/invalid-cat-args :ranges ranges})))
    (mapv (fn [pair]
            (when-not (and (sequential? pair) (= 2 (count pair)))
              (throw (ex-info "cat \"ranges\" entries must be [start, end] pairs"
                              {:type :ext.foundation.editing/invalid-cat-args :range pair})))
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
   `:mtime` and `:size` mirror `File.lastModified` / `File.length`. Pass
   `:mtime` as `write`'s `:expected_mtime` guard before a whole-file overwrite;
   anchored `patch` edits verify their target content instead.
   Each call's `:lines` payload is bounded by `max-cat-window-bytes`; that
   is also the persistence-blob ceiling (one Nippy row per call).
   Streaming: never slurps the whole file. Lines outside the window are
   discarded after a single `.readLine` pass."
  ([path] (read-file path 1 default-cat-limit))
  ([path n] (read-file path 1 n))
  ([path offset n]
   (validate-cat-args! offset n)
   (let
     [f
      (ensure-existing-file! (safe-path path))

      byte-cap
      (long max-cat-window-bytes)

      skip
      (dec (long offset))

      limit
      (long n)

      mtime
      (.lastModified f)

      size
      (.length f)]

     (with-open [^java.io.BufferedReader rdr (io/reader f)]
       (loop [skipped 0]
         (when (and (< skipped skip) (some? (.readLine rdr))) (recur (inc skipped))))
       (loop
         [acc (transient [])
          bytes-used 0
          read-count 0
          stop nil]

         (cond stop (let
                      [lines (persistent! acc)
                       returned (count lines)
                       eof? (= stop :eof)
                       next-offset (when-not eof? (+ (long offset) returned))]

                      {:path (rel-path f)
                       :lines lines
                       :anchors (patch/lines->anchors lines)
                       :next-offset next-offset
                       :eof? eof?
                       :truncated? (= stop :bytes)
                       :mtime mtime
                       :size size})
               (>= read-count limit) (recur acc bytes-used read-count :limit)
               :else
               (let [raw (.readLine rdr)]
                 (if (nil? raw)
                   (recur acc bytes-used read-count :eof)
                   (let
                     [line-bytes (+ 1 (alength (.getBytes raw "UTF-8")))
                      new-bytes (+ bytes-used line-bytes)
                      line-no (+ (long offset) read-count)]

                     (if (and (pos? read-count) (> new-bytes byte-cap))
                       (recur acc bytes-used read-count :bytes)
                       (recur (conj! acc [line-no raw]) new-bytes (inc read-count) nil)))))))))))

(defn- anchor-read-error-message
  "Human message for a `patch/resolve-anchor-range` `:error` on the cat READ
   path - mirrors the patch hash-error copy and always points back to a
   fresh read for current `:anchors`."
  [{:keys [reason which hash from-line to-line stated-line found-lines anchor]}]
  (case reason
    :hashline-malformed
    (str "cat hash failed: " (name which)
         "_anchor " (pr-str anchor)
         " is not a `lineno:hash` anchor - hashline needs BOTH coordinates."
         " Re-read with cat(path) for fresh `lineno:hash` anchors.")

    :hashline-not-found
    (str "cat hash failed: " (name which)
         "_anchor hash " (pr-str hash)
         " matches no line (the line changed or the file moved)."
         " Re-read with cat(path) or cat(path, {\"tail\": N}) for fresh `lineno:hash` anchors.")

    :hashline-misplaced
    (str "cat hash failed: "
         (name which)
         "_anchor "
         (pr-str hash)
         " says line "
         stated-line
         " but that content is at line(s) "
         (pr-str found-lines)
         " - stale/misattributed anchor. Re-read with cat(path) for fresh `lineno:hash` anchors.")

    :hashline-range-inverted
    (str "cat hash failed: to_anchor line " to-line " precedes from_anchor line " from-line ".")

    (str "cat :anchor failed: " (pr-str reason))))

(defn- read-file-by-anchor
  "Read the inclusive window between the lines hashed `from_anchor`..`to_anchor`
   (`to_anchor` defaults to `from_anchor` — a single line). Resolves the hashes
   against LIVE file content via `patch/resolve-anchor-range-read`, so the read
   addresses lines BY CONTENT (following small drift) — the symmetric counterpart
   of `patch :from_anchor`, but READ-TOLERANT: unlike a write, a stale/missing hash
   does NOT throw. When a hash matches no live line the anchor's LINE NUMBER is used
   as a fallback and the result carries `:stale? true` (surfaced to the model as
   `anchors_stale`) alongside FRESH `:anchors` for the lines actually read. Returns
   the same shape as `read-file` plus `:range [from-line to-line]`. Throws ex-info
   ONLY when an anchor is genuinely unlocatable — malformed (no line number) or a
   line outside the file — the message points back to a fresh read."
  [path from_anchor to_anchor]
  (let
    [f
     (ensure-existing-file! (safe-path path))

     content
     (slurp f)

     res
     (patch/resolve-anchor-range-read content (str from_anchor) (when to_anchor (str to_anchor)))]

    (if-let [err (:error res)]
      (throw (ex-info (anchor-read-error-message err)
                      (merge {:type :ext.foundation.editing/invalid-cat-args} err)))
      (let
        [{:keys [from-line to-line stale?]} res
         n (inc (- (long to-line) (long from-line)))]

        (as-> (read-file path from-line n) out
          (assoc out
            :range [from-line to-line]
            :stale? (boolean stale?)
            :anchors (patch/lines->anchors (:lines out))))))))

(defn- read-file-ranges
  "Read several inclusive 1-based line ranges from one file. Result keeps both
   a flat `:lines` view for simple model filtering and per-range windows for
   channel display / diagnostics."
  [path ranges]
  (let
    [pairs
     (normalize-cat-ranges ranges)

     windows
     (mapv (fn [[start end]]
             (let
               [n
                (inc (- (long end) (long start)))

                out
                (read-file path start n)]

               (assoc (select-keys out [:lines :next-offset :eof? :truncated?])
                 :range [start end])))
           pairs)

     f
     (ensure-existing-file! (safe-path path))]

    {:path (rel-path f)
     :lines (vec (mapcat :lines windows))
     :anchors (patch/lines->anchors (vec (mapcat :lines windows)))
     :ranges windows
     :next-offset nil
     :eof? (every? :eof? windows)
     :truncated? (boolean (some :truncated? windows))
     :mtime (.lastModified f)
     :size (.length f)}))

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
  (let
    [n
     (long n)

     f
     (ensure-existing-file! (safe-path path))

     byte-cap
     (long max-cat-window-bytes)

     buf
     (java.util.ArrayDeque.)

     mtime
     (.lastModified f)

     size
     (.length f)]

    (with-open [^java.io.BufferedReader rdr (io/reader f)]
      (loop [total 0]
        (let [raw (.readLine rdr)]
          (if (nil? raw)
            (let
              [kept (vec (.toArray buf))
               kept-cnt (count kept)
               start (inc (- (long total) kept-cnt))
               ;; Walk kept from the END backwards, accumulating
               ;; until the byte cap. Anything dropped off the front
               ;; bumps `:truncated?`. Per-line text is verbatim —
               ;; there is no per-line cap (see the
               ;; `default-cat-limit` / `max-cat-window-bytes`
               ;; header note up-file); a single pathological long
               ;; line is included whole and the byte cap stops
               ;; further accumulation.
               [final-lines bytes-truncated?] (loop
                                                [i (dec kept-cnt)
                                                 bytes-used 0
                                                 acc ()]

                                                (if (neg? i)
                                                  [(vec acc) false]
                                                  (let
                                                    [^String s (nth kept i)
                                                     lb (+ 1 (alength (.getBytes s "UTF-8")))
                                                     nb (+ bytes-used lb)]

                                                    (if (and (seq acc) (> nb byte-cap))
                                                      [(vec acc) true]
                                                      (recur (dec i) nb (cons s acc))))))
               start-line (+ (long start) (- kept-cnt (count final-lines)))
               numbered (mapv vector (iterate inc start-line) final-lines)]

              {:path (rel-path f)
               :lines numbered
               :anchors (patch/lines->anchors numbered)
               :next-offset nil
               :eof? true
               :truncated? bytes-truncated?
               :mtime mtime
               :size size})
            (do (when (>= (.size buf) n) (.removeFirst buf))
                (.addLast buf raw)
                (recur (inc total)))))))))

;; =============================================================================
;; directory-walk cancellation
;; =============================================================================

(defn- check-interrupt!
  "Throw `InterruptedException` when the worker thread has been interrupted
   (e.g. by `cancel!` cancelling the turn's worker future). Long recursive
   directory walks poll this so Esc aborts them promptly instead of running
   the whole tree to completion - the symptom when `/fs add ..` widens the
   session onto a huge parent tree and the spinner hangs on 'cancelling'."
  []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "directory walk cancelled"))))

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
  (-> (or s "")
      str/lower-case
      (str/replace #"[^a-z0-9]" "")))

(defn- find-subseq-window
  "Length of the SMALLEST span in `hay` that contains `needle` as an ordered
   subsequence, or nil when `needle` is not a subsequence of `hay`. A tight span
   (few gaps) means a strong match; a sprawling one means scattered noise."
  [^String needle ^String hay]
  (let
    [n
     (count needle)

     h
     (count hay)]

    (when (pos? n)
      (loop
        [s
         (long 0)

         best
         (long Long/MAX_VALUE)]

        (if (< s h)
          (if (= (.charAt hay s) (.charAt needle 0))
            (let
              [end
               (loop
                 [i
                  (inc s)

                  k
                  (long 1)]

                 (cond (= k n) (dec i)
                       (>= i h) -1
                       (= (.charAt hay i) (.charAt needle k)) (recur (inc i) (inc k))
                       :else (recur (inc i) k)))

               span
               (- (long end) s)]

              (recur (inc s) (if (and (>= (long end) 0) (< span best)) span best)))
            (recur (inc s) best))
          (when (< best Long/MAX_VALUE) (inc best)))))))

(defn- find-token-score
  "Best subsequence-window density of `token` against the file NAME (full weight)
   or the whole PATH (0.6 weight — a directory hit is weaker than a name hit).
   0.0 when the token is absent entirely."
  [^String token ^String path-norm ^String name-norm]
  (let
    [wp
     (find-subseq-window token path-norm)

     wf
     (find-subseq-window token name-norm)]

    (if (nil? wp)
      0.0
      (max (if wf (/ (double (count token)) (long wf)) 0.0)
           (* 0.6 (/ (double (count token)) (long wp)))))))

(defn- find-relevance
  "Order-INSENSITIVE relevance of `query` to `path`, in [0.0, 1.0]. Splits the
   query into alnum tokens, scores each by its tightest subsequence window
   (name-weighted), and takes the MIN so EVERY token must land somewhere — this
   is what separates the handful of genuine hits from fff's page of loose
   subsequence noise."
  [query path]
  (let
    [toks
     (->> (str/split (str/lower-case (or query "")) #"[^a-z0-9]+")
          (remove str/blank?))

     pnorm
     (find-norm path)

     nnorm
     (find-norm (last (str/split (str path) #"/")))]

    (if (empty? toks) 0.0 (transduce (map #(find-token-score % pnorm nnorm)) min 1.0 toks))))

(defn- coerce-find-spec
  [args]
  (let
    [[a b]
     args

     spec
     (cond
       (and (= 1 (count args)) (string? a)) {"query" a}
       (and (= 2 (count args)) (string? a) (map? b)) (assoc b "query" a)
       (and (= 1 (count args)) (map? a)) a
       :else
       (throw
         (ex-info
           "find_files takes find_files(query), find_files(query, opts), or find_files({\"query\": q, ...})."
           {:type :ext.foundation.editing/invalid-find-args
            :expected '([query] [query opts] [spec-map])
            :got args})))

     allowed-keys
     #{"query" "paths" "path" "limit" "is_hidden" "is_respect_gitignore"}

     unknown-keys
     (seq (remove allowed-keys (keys spec)))]

    (when unknown-keys
      (throw (ex-info (str "find spec has unknown keys: "
                           (str/join ", " (map str unknown-keys))
                           ". Allowed: query, paths, limit, is_hidden, is_respect_gitignore.")
                      {:type :ext.foundation.editing/invalid-find-args
                       :unknown (vec unknown-keys)
                       :allowed (vec (sort allowed-keys))})))
    (let
      [query
       (get spec "query")

       _
       (when-not (and (string? query) (not (str/blank? query)))
         (throw (ex-info "find \"query\" must be a non-blank string"
                         {:type :ext.foundation.editing/invalid-find-args :query query})))

       _
       (when (and (contains? spec "paths") (contains? spec "path"))
         (throw (ex-info "find spec must use only one of canonical \"paths\" or alias \"path\"."
                         {:type :ext.foundation.editing/invalid-find-args :spec spec})))

       raw-paths
       (cond (contains? spec "paths") (get spec "paths")
             (contains? spec "path") (get spec "path")
             :else ["."])

       paths
       (cond (or (nil? raw-paths) (and (sequential? raw-paths) (empty? raw-paths))) ["."]
             (string? raw-paths) [raw-paths]
             (sequential? raw-paths) (vec raw-paths)
             :else raw-paths)

       _
       (when-not (and (vector? paths) (seq paths) (every? string? paths))
         (throw
           (ex-info
             "find \"paths\" must be a string or vector of strings (empty defaults to current directory)"
             {:type :ext.foundation.editing/invalid-find-args :paths raw-paths})))

       limit
       (or (get spec "limit") default-find-limit)

       _
       (when-not (and (integer? limit) (pos? (long limit)))
         (throw (ex-info "find \"limit\" must be a positive integer"
                         {:type :ext.foundation.editing/invalid-find-args :limit limit})))]

      {:query query
       :paths paths
       :limit limit
       :is_hidden (boolean (get spec "is_hidden"))
       :is_respect_gitignore (get spec "is_respect_gitignore" true)
       :is_gitignore_explicit (contains? spec "is_respect_gitignore")})))

(defn- find-walk-files
  "Walk directory `base` ONCE, returning a vector of `{:path :file-name :size}`
   for every file (honoring `is_hidden`; dotfiles — incl. every `.git` dir —
   skipped unless set). Exclusion is `search-excluded?`: `ignore-node` prunes
   gitignored paths (nil for a full opt-out that walks everything), while a
   non-nil `overlay` re-includes the configured `:include-gitignored-paths`
   subtrees and prunes `:always-exclude` matches — so a gitignore-RESPECTING
   walk still hides `.gitignore`'d files while surfacing the ones a `!` rule or
   the `:search` overlay re-included.
   Deliberately does NO scoring: `find-search` scores the strict query AND each
   fallback token, so scoring here would re-walk the whole tree up to 6× per call.
   Walk once, score the cached vector per query instead."
  [^File base is_hidden ignore-node overlay]
  (let [out (java.util.ArrayList.)]
    (letfn [(walk [^File f]
              (check-interrupt!)
              (when (and (or is_hidden (not (.isHidden f)))
                         (not (search-excluded? ignore-node overlay f base)))
                (cond (.isDirectory f) (doseq [^File c (or (.listFiles f) (into-array File []))]
                                         (walk c))
                      (.isFile f)
                      (.add out {:path (rel-path f) :file-name (.getName f) :size (.length f)}))))]
      (walk base))
    (vec out)))

(defn- score-walked-candidates
  "Score pre-walked `find-walk-files` entries against `query`, keeping those
   whose `find-relevance` clears `find-min-score`. Pure in-memory — no I/O — so
   it is cheap to run once per query token over a single cached walk."
  [entries query]
  (into []
        (keep (fn [{:keys [path] :as e}]
                (let [score (find-relevance query path)]
                  (when (>= (double score)
                            #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                            (double find-min-score))
                    (assoc e
                      :binary? false
                      :score score)))))
        entries))

(defn- find-scan
  "Scan `roots` for ONE `query` string and keep candidates whose
   `find-relevance` (name-weighted, order-insensitive) clears `find-min-score`.
   Returns raw item maps carrying `:score`. A direct FILE root contributes
   itself at score 1.0. The single-query building block `find-search` runs
   once for the strict whole-query pass and once per token for the fallback.

   Directory roots go through fff (fast, frecency-ranked) — but fff's index
   HONORS `.gitignore`, so when the caller opted OUT (`is_respect_gitignore`
   false) we bypass fff and walk the tree directly, otherwise the ignored files
   would never surface no matter what the flag says. `walk-cache` (a root→delay
   atom) memoizes that direct walk so the strict + per-token passes share ONE
   filesystem traversal instead of re-walking the whole tree each time."
  [roots query is_hidden is_respect_gitignore candidate-page walk-cache overlay]
  (->>
    roots
    (mapcat
      (fn [^File root]
        (cond (.isFile root) [{:path (rel-path root)
                               :file-name (.getName root)
                               :size (.length root)
                               :binary? false
                               :source :direct-file
                               :score 1.0}]
              ;; Walk the tree directly (bypassing fff) when the caller opted
              ;; OUT of gitignore entirely, OR when tool-only `.ignore`/`.rgignore`
              ;; files are present: fff's index only knows `.gitignore`, so it
              ;; would re-drop what those files' `!` rules re-included. `ignore-node`
              ;; is the FULL layered matcher while still respecting gitignore (nil
              ;; on full opt-out), so `find-walk-files` keeps `.gitignore`'d files
              ;; hidden yet surfaces the `!`-negated ones.
              (or (not is_respect_gitignore)
                  ;; …OR when the `:search` config overlay is active: fff's
                  ;; gitignore-honoring index can't see the re-included subtrees.
                  (some? overlay)
                  (gitignore/tool-ignore-present? (.getCanonicalFile root)))
              (let
                [base
                 (.getCanonicalFile root)

                 ignore-node
                 (when is_respect_gitignore (load-ignore-node base))

                 entries
                 @(-> walk-cache
                      (swap! (fn [m]
                               (if (contains? m base)
                                 m
                                 (assoc m
                                   base (delay
                                          (find-walk-files base is_hidden ignore-node overlay))))))
                      (get base))]

                (score-walked-candidates entries query))
              :else (with-open [idx (rg-fff-open root)]
                      (let [base (.getCanonicalFile root)]
                        ;; doall: realize hits INSIDE with-open, before the
                        ;; fresh instance is closed.
                        (doall
                          (->> (:items (fff/search idx {:query query :page-size candidate-page}))
                               (keep
                                 (fn
                                   [{:keys [relative-path file-name git-status size modified
                                            frecency-score binary?]}]
                                   (let
                                     [f (io/file base relative-path)
                                      rel (rel-path f)
                                      score (find-relevance query rel)]

                                     (when (and (>= (double score)
                                                    #_{:clj-kondo/ignore
                                                       [:redundant-primitive-coercion]}
                                                    (double find-min-score))
                                                (or is_hidden (not (.isHidden f)))
                                                (not (ignored? (load-ignore-node base) f base)))
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
  (let
    [glue #{"the" "and" "for" "with" "that" "this" "how" "what" "where" "into" "from" "was" "are"
            "any" "all" "not" "our" "you" "your"}]
    (->> (str/split (str/lower-case (or query "")) #"[^a-z0-9]+")
         (remove str/blank?)
         distinct
         (filter #(and (>= (count %) 3) (not (contains? glue %))))
         (sort-by (comp - count))
         (take 5)
         vec)))

(defn- find-search
  [args]
  (let
    [{:keys [query paths limit is_hidden is_respect_gitignore is_gitignore_explicit]}
     (coerce-find-spec args)

     {roots :roots find-resolutions :resolutions}
     (resolve-search-roots paths)

     ;; fff ranks genuine hits first but pads the page with loose subsequence
     ;; noise, so pull a WIDER candidate set than `limit` and let the relevance
     ;; filter below do the real cutting (a fresh fff scan is ~11ms).
     candidate-page
     (max (long limit) 300)

     ;; ONE walk-cache shared across the strict + per-token scans so an
     ;; is_respect_gitignore=false run walks each root's tree exactly once.
     walk-cache
     (atom {})

     ;; `:search` config overlay (issue #23): active only on the DEFAULT
     ;; gitignore-respecting path — an EXPLICIT per-call is_respect_gitignore
     ;; (either value) wins over the overlay for that call.
     search-overlay
     (when (and is_respect_gitignore (not is_gitignore_explicit)) (search-overlay-matchers))

     scan
     (fn [q]
       (find-scan roots q is_hidden is_respect_gitignore candidate-page walk-cache search-overlay))

     strict
     (scan query)

     tokens
     (find-fallback-tokens query)

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
     fuzzy-limit
     (min (long limit) 20)

     [ranked fuzzy?]
     (if (or (seq strict) (< (count tokens) 2))
       [strict false]
       (let
         [stem
          (fn [it]
            (find-norm (str/replace (str (:file-name it)) #"\.[^.]*$" "")))

          by-path
          (reduce (fn [m t]
                    (reduce (fn [m it]
                              (update m
                                      (:path it)
                                      (fn [cur]
                                        (-> (or cur
                                                (assoc it
                                                  :score 0.0
                                                  :terms #{}))
                                            (update :score max (:score it))
                                            (update :terms conj t)))))
                            m
                            (scan t)))
                  {}
                  tokens)

          ;; A term that IS the filename stem (`render` → `render.clj`) is a
          ;; bullseye — it must beat a 2-common-word loose match
          ;; (`native`+`tool` → `native-tool-handlers.md`), so it gets a
          ;; score bonus that ranks above raw term coverage.
          scored
          (map (fn [it]
                 (let
                   [s
                    (stem it)

                    bull?
                    (contains? (:terms it) s)]

                   (assoc it :rank-score (+ (double (:score it 0.0)) (if bull? 0.6 0.0)))))
               (vals by-path))]

         [(->> scored
               (sort-by (fn [it]
                          [(- (double (:rank-score it))) (- (count (:terms it))) ;; then coverage
                           (- (long (or (:frecency-score it) 0))) (:path it)]))
               vec) true]))

     items
     (if fuzzy?
       (vec (take fuzzy-limit (map #(dissoc % :rank-score) ranked)))
       (->> ranked
            ;; strongest match first; frecency then path break ties.
            (sort-by (fn [it]
                       [(- (double (:score it 0.0))) (- (long (or (:frecency-score it) 0)))
                        (:path it)]))
            (take limit)
            vec))

     ;; The query terms that actually landed a file (fuzzy pass only) — so
     ;; the card/model can see WHICH words matched, e.g. "render, native".
     matched-terms
     (when fuzzy?
       (->> items
            (mapcat :terms)
            distinct
            (sort-by (comp - count))
            vec))

     ;; Project each internal (keyword-keyed) item onto the model-facing
     ;; string-keyed row — snake_case keys, keyword values stringified.
     ->item
     (fn [it]
       (cond-> {"path" (:path it) "score" (:score it)}
         (contains? it :file-name)
         (assoc "file_name" (:file-name it))

         (contains? it :size)
         (assoc "size" (:size it))

         (contains? it :modified)
         (assoc "modified" (:modified it))

         (contains? it :frecency-score)
         (assoc "frecency_score" (:frecency-score it))

         (contains? it :git-status)
         (assoc "git_status"
           (let [g (:git-status it)]
             (if (keyword? g) (name g) g)))

         (contains? it :binary?)
         (assoc "binary" (boolean (:binary? it)))

         (:source it)
         (assoc "source" (name (:source it)))))]

    ;; Model-facing find_files result — string keys, no keyword values.
    (cond->
      {"items" (mapv ->item items)
       "item_count" (count items)
       "paths" (mapv :path items)
       "query" query
       "searched_paths" paths
       "limit" limit
       "truncated_by" (if (>= (count items) (long limit)) "limit" "end_of_results")}
      fuzzy?
      (assoc "fuzzy" true)

      (seq matched-terms)
      (assoc "matched_terms" matched-terms)

      (seq (missing-search-paths find-resolutions))
      (assoc "missing_paths" (missing-search-paths find-resolutions)))))

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
   \"item_count\", \"query\", \"searched_paths\", \"missing_paths\", \"limit\"} — plus a \"hint\" when
   nothing matched."
  [& args]
  (let
    [{:strs [query searched_paths limit item_count truncated_by] :as out}
     (find-search args)

     ;; A 0-result find is almost always a MISUSE: the model passed a
     ;; content/concept phrase to a FILENAME matcher. Steer it — a bare
     ;; "0 matches" just makes it retry the same wrong query (the exact loop
     ;; the user hit). Only fires on empty so a normal hit set stays lean.
     multiword?
     (> (count (str/split (str/trim (str query)) #"\s+")) 2)

     out
     (cond-> out
       (zero? (long (or item_count 0)))
       (assoc "hint"
         (str
           "No FILENAME/PATH matched \""
           query
           "\". find_files matches file "
           "NAMES, not contents. "
           (if multiword?
             (str "That query looks like CONTENT (a phrase describing code) — "
                  "search inside files with rg(\"a distinctive term\") instead, "
                  "or shorten this to a single filename fragment.")
             "Try a shorter/different filename fragment, or rg(...) to search file contents."))))]

    (tool-success {:op :find_files
                   :path (first searched_paths)
                   :kind :dir
                   :result out
                   :metadata {:query query
                              :paths searched_paths
                              :limit limit
                              :item-count item_count
                              :truncated-by truncated_by}})))

;; =============================================================================
;; rg
;; =============================================================================

(defn- parse-stringish-vector
  "Tolerate a value that arrives as a STRINGIFIED list literal — e.g.
   `\"[\\\"a\\\", \\\"b\\\"]\"` — a common JSON/LLM mistake where a real array
   gets quoted into one string. When `raw` is a string whose trimmed form is
   bracketed like a JSON/EDN array, parse it (commas are EDN whitespace); if
   every element reads as a string, return the parsed vector. A plain glob
   string, an already-real vector, or unparseable junk is returned unchanged
   for the normal scalar-tolerant path."
  [raw]
  (if (and (string? raw)
           (let [t (str/trim raw)]
             (and (str/starts-with? t "[") (str/ends-with? t "]"))))
    (let [parsed (try (edn/read-string (str/trim raw)) (catch Exception _ ::fail))]
      (if (and (vector? parsed) (seq parsed) (every? string? parsed)) parsed raw))
    raw))


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
    (throw (ex-info "rg takes one spec map: {\"query\": [...], \"paths\": [...]}."
                    {:type :ext.foundation.editing/invalid-rg-spec :got (type spec)})))
  (let
    [vector-of-strings
     (fn [k raw]
       (let
         [raw
          (parse-stringish-vector raw)

          ;; tolerate a stringified list literal
          v
          (if (string? raw) [raw] raw)]

         ;; scalar-tolerant
         (when-not (and (vector? v) (seq v) (every? string? v))
           (throw (ex-info "rg field must be a string or non-empty vector of strings."
                           {:type :ext.foundation.editing/invalid-rg-spec :field k :got v})))
         (when-not (every? #(not (str/blank? %)) v)
           (throw (ex-info "rg string values must be non-blank."
                           {:type :ext.foundation.editing/invalid-rg-spec :field k :got v})))
         v))

     ;; `query` canonical; `any`/`all` are accepted aliases (all mean OR now).
     query-key
     (some #(when (contains? spec %) %) ["query" "any" "all"])

     _
     (when-not query-key
       (throw (ex-info "rg needs `query`: a term or a list of terms."
                       {:type :ext.foundation.editing/invalid-rg-spec :spec spec})))

     ;; A query TERM is a substring; a LIST is OR. Models overwhelmingly write
     ;; the OR list as ONE comma-joined string (`\"model, cycle\"`) — which,
     ;; matched literally, hits nothing. So split every term on commas into
     ;; separate OR needles: `\"a, b\"` and `[\"a, b\", c]` both become
     ;; `[a b …]`. (A rare literal-comma search loses out; the model's intent
     ;; is virtually always \"these separate terms\".)
     needles
     (let
       [ns (->> (vector-of-strings query-key (get spec query-key))
                (mapcat #(str/split % #"\s*,\s*"))
                (map str/trim)
                (remove str/blank?)
                vec)]
       (when (empty? ns)
         (throw (ex-info "rg query has no non-blank terms."
                         {:type :ext.foundation.editing/invalid-rg-spec :field query-key})))
       ns)

     raw-paths
     (get spec "paths" ["."])

     paths
     (if (or (nil? raw-paths) (and (sequential? raw-paths) (empty? raw-paths)))
       ["."]
       (vector-of-strings :paths raw-paths))

     raw-include
     (get spec "include")

     include
     (if (or (nil? raw-include) (and (sequential? raw-include) (empty? raw-include)))
       []
       (vector-of-strings :include raw-include))

     nonneg-int!
     (fn [label v]
       (when (and (some? v) (not (and (integer? v) (not (neg? (long v))))))
         (throw (ex-info (str "rg " label " must be a non-negative integer")
                         {:type :ext.foundation.editing/invalid-rg-spec :field label :got v}))))

     _
     (nonneg-int! ":context" (get spec "context"))

     is_files_only
     (boolean (get spec "is_files_only"))

     ;; `context` is a CONTENT-mode concept — in files-only mode there are no
     ;; per-line hits to surround, so a stray `context` is simply IGNORED (never
     ;; a hard error: the model harmlessly set both, so honor `is_files_only`).
     context
     (if is_files_only 0 (or (get spec "context") 0))]

    {:needles needles
     :paths paths
     :include (or include [])
     :is_hidden (boolean (get spec "is_hidden"))
     :is_respect_gitignore (get spec "is_respect_gitignore" true)
     :is_gitignore_explicit (contains? spec "is_respect_gitignore")
     :limit (let [l (get spec "limit")]
              (if (and (integer? l) (pos? (long l))) (long l) default-grep-limit))
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
  (let
    [grouped
     (group-by has-upper? needles)

     cs
     (vec (get grouped true))

     ;; has uppercase → case-sensitive
     ci
     (mapv str/lower-case (get grouped false))]

    ;; no uppercase → case-insensitive
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
  (let
    [sum-lens (fn ^long [xs]
                (reduce (fn [^long acc x]
                          (+ acc (count (str (second x)))))
                        0
                        xs))]
    (+ (count (str (:text hit)))
       #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
       (long (sum-lens (:before hit)))
       #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
       (long (sum-lens (:after hit))))))

(defn- search-file-content
  "Walk one file once, emit hits with optional context. Content-mode helper.
   Returns a vec of hit maps; an empty vec means no match. `:text` and the
   `:before`/`:after` context are kept FULL — the value is the model's data,
   sliceable in Python via `r[...]`; only the wire VIEW is bounded downstream."
  [^File f matches? before-ctx after-ctx]
  (try
    (let
      [path
       (rel-path f)

       lines
       (with-open [r (io/reader f)]
         (vec (line-seq r)))

       n
       (count lines)

       before-ctx
       (long before-ctx)

       after-ctx
       (long after-ctx)

       want-before?
       (pos? before-ctx)

       want-after?
       (pos? after-ctx)]

      (loop
        [i
         0

         out
         (transient [])]

        (cond (>= i n) (persistent! out)
              (matches? (nth lines i)) (let
                                         [line-no
                                          (inc i)

                                          ;; FULL text here: the hit's :text feeds `patch/line-anchor`
                                          ;; (the patch hash must match the real file line). Display
                                          ;; clipping happens AFTER the anchor is computed (see rg-search).
                                          text
                                          (nth lines i)

                                          hit
                                          (cond-> {:path path :line line-no :text text}
                                            want-before?
                                            (assoc :before
                                              (mapv (fn [j]
                                                      [(inc (long j)) (nth lines j)])
                                                    (range (max 0 (- i before-ctx)) i)))

                                            want-after?
                                            (assoc :after
                                              (mapv (fn [j]
                                                      [(inc (long j)) (nth lines j)])
                                                    (range (inc i) (min n (+ i after-ctx 1))))))]

                                         (recur (inc i) (conj! out hit)))
              :else (recur (inc i) out))))
    (catch Throwable _ [])))

(defn- file-has-any-hit?
  "Short-circuit: true on first matching line. Used by :is_files_only mode
   so we exit each file as fast as possible."
  [^File f matches?]
  (try (with-open [r (io/reader f)]
         (boolean (some matches? (line-seq r))))
       (catch Throwable _ false)))

(defn- rg-search
  "The rg search ENGINE: takes the public rg spec map and does the
   actual file scanning. The public `rg-tool` (= `rg`) wraps this with
   arity/kwargs handling + the LLM-facing result envelope. Two output modes,
   picked by `:is_files_only` / (default content).

   Returns one of:
     {:hits   [{:path :line :text :anchor :before? :after?} ...] :truncated-by KW :total-file-count N :total-file-count-exact? BOOL}  ;; content
   `:anchor` is the `<lineno>:<hash>` anchor for that line — the same one `cat`
   emits in `:anchors` — so a hit is directly patchable via `{:from_anchor <anchor>}`
   without a follow-up `cat`. Absent on blank lines.
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW :total-file-count N :total-file-count-exact? BOOL}  ;; files-only

   `:truncated-by` is `:limit` (hit count), `:bytes` (total-bytes budget), or
   `:end-of-results`. Hit/context `:text` is kept FULL (sliceable in Python via
   `r[...]`); only the wire VIEW is bounded by the 64KB per-observation clip."
  [spec]
  (let
    [{:keys [needles paths include is_hidden is_respect_gitignore is_gitignore_explicit limit
             context is_files_only]}
     (coerce-rg-spec spec)

     before-ctx
     context

     after-ctx
     context

     glob-matcher
     (fn [pattern]
       ;; ripgrep/gitignore semantics: a leading `**/` matches at ANY depth
       ;; INCLUDING the root, but Java NIO glob requires `**/` to consume at
       ;; least one dir (so `**/deps.edn` misses a root-level `deps.edn`).
       ;; Rewrite the leading `**/` to `{**/,}` so zero leading dirs also match.
       (let [pattern (if (str/starts-with? pattern "**/") (str "{**/,}" (subs pattern 3)) pattern)]
         (.getPathMatcher (java.nio.file.FileSystems/getDefault) (str "glob:" pattern))))

     include-matchers
     (mapv glob-matcher include)

     match-globs?
     (fn [matchers ^File f]
       (let
         [rel
          (rel-path f)

          name
          (.getName f)

          rel-path
          (fs/path rel)

          name-path
          (fs/path name)]

         (boolean (some (fn [^java.nio.file.PathMatcher m]
                          (or (.matches m rel-path) (.matches m name-path)))
                        matchers))))

     include-file?
     (fn [^File f]
       (or (empty? include-matchers) (match-globs? include-matchers f)))

     search-roots
     (resolve-search-roots paths)

     rg-missing-paths
     (missing-search-paths (:resolutions search-roots))

     roots
     (->> (:roots search-roots)
          (sort-by (fn [^File f]
                     [(count (iterator-seq (.iterator (.toPath f)))) (.getPath f)]))
          (reduce (fn [acc ^File f]
                    (if (some (fn [^File parent]
                                (.startsWith (.toPath f) (.toPath parent)))
                              acc)
                      acc
                      (conj acc f)))
                  []))

     matches?
     (make-line-matcher needles)

     ;; `:search` config overlay (issue #23): active only on the DEFAULT
     ;; gitignore-respecting path — an EXPLICIT per-call is_respect_gitignore
     ;; (either value) wins over the overlay for that call.
     search-overlay
     (when (and is_respect_gitignore (not is_gitignore_explicit)) (search-overlay-matchers))

     walk
     (fn walk [ignore-node root ^File f]
       (check-interrupt!)
       ;; The hidden-guard filters hidden DESCENDANTS, but must never reject the
       ;; scan ROOT for its own dotname — an explicit/allowed root like `~/.vis`
       ;; is entered on purpose, so exempt `f == root` (else a dotfile root scans
       ;; to nothing, silently skipping a whole filesystem root on the default sweep).
       (cond (and (not is_hidden) (not= f root) (.isHidden f)) []
             (and is_respect_gitignore (search-excluded? ignore-node search-overlay f root)) []
             (.isDirectory f) (mapcat #(walk ignore-node root %)
                                      (or (.listFiles f) (into-array File [])))
             (and (.isFile f) (include-file? f)) [f]
             :else []))

     ;; fff-first: on the DEFAULT gitignore-respecting path fff OWNS discovery. It
     ;; enumerates the correct universe (nested `.gitignore`-aware — NEVER descends
     ;; node_modules/target/…) and needle-narrows via native grep, ~280× faster than a
     ;; raw walk. The result is IDENTICAL to the old `walk ∩ fff` (fff already dropped
     ;; the ignored files the walk kept), just without paying the walk. Skip fff — walk
     ;; instead — only when the caller opted OUT of gitignore, a tool-only
     ;; `.ignore`/`.rgignore` is present (its `!` rules re-include files fff would never
     ;; surface), or the `:search` overlay is active (its gitignore-honoring index would
     ;; re-drop the re-included files). fff surfaces dotfiles the walk hid by descent, so
     ;; re-apply the include globs + hidden-below-root guard on the fff path.
     fff-first?
     (and is_respect_gitignore
          (nil? search-overlay)
          (not (some gitignore/tool-ignore-present? roots)))

     candidates
     (if fff-first?
       (->> (rg-fff-candidate-files roots needles)
            (filter include-file?)
            (remove (fn [^File f]
                      (and (not is_hidden) (rg-hidden-below-root? roots f)))))
       (->> roots
            (mapcat (fn [root]
                      (let [ignore-node (when is_respect_gitignore (load-ignore-node root))]
                        (walk ignore-node root root))))))

     files
     ;; DECORATE-SORT-UNDECORATE: `rel-path` canonicalizes paths (syscalls). Handing it
     ;; to `sort-by` directly ran it INSIDE the comparator — O(n·log n) canonicalizations
     ;; that pinned a core for minutes on big trees, and with no interrupt checkpoint in
     ;; the sort the burn OUTLIVED cancellation (orphaned rg workers at 100% CPU until
     ;; exit). Compute the key ONCE per file, polling `check-interrupt!` so Esc/timeout
     ;; aborts, then sort the cheap precomputed string keys.
     (->> candidates
          (mapv (fn [^File f]
                  (check-interrupt!)
                  [(rel-path f) f]))
          (sort-by first)
          (mapv second))]

    (cond
      is_files_only (let
                      [out
                       (atom [])

                       capped?
                       (atom false)

                       total-files
                       (atom 0)

                       probed-extra
                       (atom 0)

                       breadth-capped?
                       (atom false)]

                      ;; the scan phase reads every candidate file — poll so Esc/timeout
                      ;; aborts mid-sweep. Past the display cap we keep short-circuit-probing
                      ;; candidate files to report TRUE breadth (`total_file_count`), bounded
                      ;; by `rg-breadth-probe-limit` so a hostile-needle full-tree scan (fff
                      ;; disabled) can't turn a truncated result into a whole-tree sweep.
                      (doseq
                        [^File f
                         files

                         :while (not @breadth-capped?)]

                        (check-interrupt!)
                        (if @capped?
                          (do (when (file-has-any-hit? f matches?) (swap! total-files inc))
                              (when (>= (long (swap! probed-extra inc))
                                        (long rg-breadth-probe-limit))
                                (reset! breadth-capped? true)))
                          (when (file-has-any-hit? f matches?)
                            (swap! total-files inc)
                            (swap! out conj (rel-path f))
                            (when (>= (count @out) (long limit)) (reset! capped? true)))))
                      {:files (vec @out)
                       :missing rg-missing-paths
                       :truncated-by (if @capped? :limit :end-of-results)
                       :total-file-count @total-files
                       :total-file-count-exact? (not @breadth-capped?)})
      :else (let
              [out
               (atom [])

               bytes-used
               (atom 0)

               cap-reason
               (atom nil)

               ;; nil | :limit | :bytes
               total-files
               (atom 0)

               probed-extra
               (atom 0)

               breadth-capped?
               (atom false)]

              (doseq
                [^File f
                 files

                 :while (not @breadth-capped?)]

                ;; the scan phase reads every candidate file — poll so Esc/timeout
                ;; aborts mid-sweep
                (check-interrupt!)
                (if @cap-reason
                  ;; DISPLAY is full — keep counting matching files for breadth via a
                  ;; short-circuit probe (no hit objects/context, so the tail stays
                  ;; cheap), bounded by `rg-breadth-probe-limit`.
                  (do (when (file-has-any-hit? f matches?) (swap! total-files inc))
                      (when (>= (long (swap! probed-extra inc)) (long rg-breadth-probe-limit))
                        (reset! breadth-capped? true)))
                  (let [hits (search-file-content f matches? before-ctx after-ctx)]
                    (when (seq hits)
                      (swap! total-files inc)
                      ;; Attach the `lineno:hash` anchor (patchable straight from the hit).
                      ;; :text is kept FULL — it's the model's data, sliceable in Python
                      ;; via r[...]; the wire VIEW is bounded by the 64KB observation clip.
                      ;; Stop on the hit limit OR the total-bytes budget (whichever first).
                      (doseq
                        [hit hits
                         :while (not @cap-reason)]

                        (let
                          [hit* (cond-> hit
                                  (not (str/blank? (:text hit)))
                                  (assoc :anchor (patch/line-anchor (:line hit) (:text hit))))]
                          (swap! out conj hit*)
                          (swap! bytes-used + (hit-bytes hit*))
                          (cond (>= (count @out) (long limit)) (reset! cap-reason :limit)
                                (>= (long @bytes-used) (long max-rg-result-bytes))
                                (reset! cap-reason :bytes))))))))
              {:hits (vec @out)
               :missing rg-missing-paths
               :truncated-by (or @cap-reason :end-of-results)
               :total-file-count @total-files
               :total-file-count-exact? (not @breadth-capped?)}))))

;; =============================================================================
;; Thin babashka.fs wrappers
;; =============================================================================

(def ^:private patch-required-keys #{"path" "from_anchor" "replace"})

(def ^:private patch-optional-keys
  "Optional keys recognised on an anchor edit map.
   - to_anchor  end of a hashline range; defaults to from_anchor (single line)
   - atomic     multi-file escape flag (read by `mutation-atomic?` from the raw
                args before this validation)."
  #{"to_anchor" "atomic"})

(def ^:private patch-allowed-keys (set/union patch-required-keys patch-optional-keys))

(defn- coerce-patch-edits
  "Validate the canonical vector of anchor-located edit maps.
   it must carry `:from_anchor` (and optionally `:to_anchor` for a range).
   A missing anchor, or an unknown key, throws."
  [edits]
  (when-not (sequential? edits)
    (throw (ex-info "patch expects a vector of edit maps"
                    {:type :ext.foundation.editing/invalid-patch-edits :got (type edits)})))
  (when (empty? edits)
    (throw (ex-info "patch expects at least one edit"
                    {:type :ext.foundation.editing/invalid-patch-edits :got edits})))
  (mapv (fn [edit]
          (when-not (map? edit)
            (throw (ex-info "patch edit must be a map"
                            {:type :ext.foundation.editing/invalid-patch-edit :edit edit})))
          (let
            [missing
             (seq (remove #(contains? edit %) patch-required-keys))

             unknown
             (seq (remove patch-allowed-keys (keys edit)))]

            (when missing
              (throw (ex-info (str "patch edit missing required keys: "
                                   (str/join ", " (map #(str "'" % "'") missing))
                                   ". Use a fresh lineno:hash from cat as from_anchor.")
                              {:type :ext.foundation.editing/invalid-patch-edit
                               :missing (vec missing)
                               :edit edit})))
            (when unknown
              (throw (ex-info (str "patch edit has unknown keys: "
                                   (str/join ", " (map #(str "'" % "'") unknown))
                                   ". Allowed: "
                                   (str/join ", " (sort patch-allowed-keys))
                                   ".")
                              {:type :ext.foundation.editing/invalid-patch-edit
                               :unknown (vec unknown)
                               :allowed (vec patch-allowed-keys)
                               :edit edit}))))
          (update edit "path" str))
        edits))

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
  (when (>= n (long patch-fail-loop-threshold))
    (str "Patch failed " n
         " times on " path
         ". Stop retrying: re-read the target, verify the intended structure, then use"
         " fresh anchors or the correct structural operation.")))

;; -----------------------------------------------------------------------------
;; Anchor-based patch analysis
;;
;; Each edit resolves against one live per-file snapshot. The content hash in
;; every anchor is the concurrency guard: unrelated changes survive, while a
;; changed target fails. Valid spans are applied end-to-start in one atomic plan.
;; -----------------------------------------------------------------------------

(defn- resolve-edit-target
  "Resolve the edit's path to an existing file. Returns either
   `{:file F :rel R}` or `{:error {:reason RK :message MSG}}` so the
   caller folds path-level problems (escape, missing file, target is a
   dir) into the same structured failure stream as match-level
   problems. Keeps `patch-analysis` exception-free."
  [path]
  (try (let [file (safe-path path)]
         (cond (not (.exists file)) {:error {:reason :file-not-found
                                             :path (.getPath file)
                                             :message (str "File not found: " (.getPath file))}}
               (.isDirectory file) {:error {:reason :path-is-dir
                                            :path (.getPath file)
                                            :message (str "Path is a directory, not a file: "
                                                          (.getPath file))}}
               :else {:file file :rel (rel-path file)}))
       (catch clojure.lang.ExceptionInfo e
         (let [{:keys [type] :as data} (ex-data e)]
           {:error {:reason (case type
                              :ext.foundation.editing/path-escape
                              :path-escape

                              :path-error)
                    :message (ex-message e)
                    :data data}}))))

;; Hashline locator resolution lives in the reusable `patch` layer
;; (`patch/resolve-anchor-edit-span`, `patch/indices-matching-hash`). The
;; `:from_anchor`/`:to_anchor` branch of `patch-analysis` calls straight into
;; it — no bespoke hash math in this channel/IO namespace.

(defn- patch-analysis
  "Resolve every edit to a char span against the original per-file snapshot,
   then splice all spans bottom-up. Hashline anchors are the concurrency guard:
   unrelated file changes are preserved when the target anchors still resolve;
   changed targets fail. Overlapping spans are rejected, and any failure means
   `patch-safe` writes nothing."
  [edits]
  (let
    [edits
     (coerce-patch-edits edits)

     {:keys [origs spans checks failures]}
     (loop
       [idx
        0

        remaining
        edits

        origs
        {}

        spans
        {}

        checks
        []

        failures
        []]

       (if-let [{:strs [path replace from_anchor to_anchor]} (first remaining)]
         (let [resolved (resolve-edit-target path)]
           (if-let [path-error (:error resolved)]
             (let
               [check
                {:edit-index idx :path path :reason (:reason path-error) :path-error path-error}]
               (recur (inc idx)
                      (next remaining)
                      origs
                      spans
                      (conj checks check)
                      (conj failures check)))
             (let
               [file (:file resolved)
                rel (:rel resolved)
                current (or (get origs path) (slurp file))
                origs (assoc origs path current)
                replace (str replace)
                base-check {:edit-index idx
                            :path rel
                            :from_anchor from_anchor
                            :to_anchor (or to_anchor from_anchor)}
                res (patch/resolve-anchor-edit-span current from_anchor to_anchor replace)]

               (if-let [err (:error res)]
                 (let
                   [check (assoc base-check
                            :reason (:reason err)
                            :hash-error err)]
                   (recur (inc idx)
                          (next remaining)
                          origs
                          spans
                          (conj checks check)
                          (conj failures check)))
                 (let
                   [span {:start (:start res)
                          :end (:end res)
                          :replacement (:replacement res)
                          :file file
                          :path rel
                          :edit-index idx}
                    check (assoc base-check :applied-positions [(:applied-line res)])]

                   (recur (inc idx)
                          (next remaining)
                          origs
                          (update spans path (fnil conj []) span)
                          (conj checks check)
                          failures))))))
         {:origs origs :spans spans :checks checks :failures failures}))

     results
     (for [[path file-spans] spans]
       (let
         [before (get origs path)
          sorted (sort-by :start file-spans)
          bad (first (filter (fn [[a b]]
                               (> (long (:end a)) (long (:start b))))
                             (partition 2 1 sorted)))]

         (if bad
           {:failure {:edit-index (:edit-index (second bad))
                      :path path
                      :reason :overlapping-edits
                      :overlap (mapv :edit-index bad)}}
           {:plan {:file (:file (first file-spans))
                   :path (:path (first file-spans))
                   :before before
                   :after (reduce (fn [content {:keys [start end replacement]}]
                                    (str (subs content 0 start) replacement (subs content end)))
                                  before
                                  (reverse sorted))}})))

     overlap-failures
     (vec (keep :failure results))

     plans
     (vec (keep :plan results))

     all-failures
     (into failures overlap-failures)]

    {:plans plans :checks checks :failures all-failures :valid? (empty? all-failures)}))

(defn- explain-failure
  [{:keys [edit-index path reason hash-error message path-error]}]
  (or message
      (:message path-error)
      (let [head (str "edit " edit-index " in " path)]
        (case reason
          :hashline-malformed
          (str head
               ": malformed "
               (name (:which hash-error))
               "_anchor "
               (pr-str (:anchor hash-error))
               "; use a fresh `lineno:hash` from `cat`.")

          :hashline-line-out-of-range
          (str head
               ": anchor line "
               (:line hash-error)
               " is outside the "
               (:lines hash-error)
               "-line file; refresh it with `cat`.")

          :hashline-not-found
          (str head
               ": stale " (name (:which hash-error))
               "_anchor" (if-let [ca (:current-anchor hash-error)]
                           (str "; line " (:stated-line hash-error) " is now `" ca "`.")
                           "; refresh the target with `cat`."))

          :hashline-misplaced
          (str head
               ": anchor says line "
               (:stated-line hash-error)
               " but matches line(s) "
               (pr-str (:found-lines hash-error))
               "; refresh the target before editing.")

          :overlapping-edits
          (str head ": overlapping targets; merge them or use separate patch calls.")

          :hashline-range-inverted
          (str head
               ": to_anchor line "
               (:to-line hash-error)
               " precedes from_anchor line "
               (:from-line hash-error)
               ".")

          (str head " failed.")))))

(defn- failure-family
  "Group anchor-resolution failures that share one stale-target cause."
  [{:keys [reason]}]
  (case reason
    (:hashline-not-found :hashline-misplaced :hashline-line-out-of-range :hashline-range-inverted)
    :stale-anchors

    reason))

(defn- family-headline
  "One shared, actionable sentence for a group of failures in the same
   `failure-family`. For reasons whose message is edit-specific (path errors,
   syntax refusals, malformed anchors) it falls back to the first member's full
   `explain-failure` — the affected-edit list rendered beside it names the rest."
  [family failures]
  (case family
    :stale-anchors
    "anchors no longer match the file; re-`cat` once, then resend the batch."

    :overlapping-edits
    "targets overlap; merge them or use separate patch calls."

    ;; Edit-specific reasons (path errors, :hashline-malformed, :syntax-error):
    ;; keep the first member's full, precomputed explanation.
    (explain-failure (first failures))))

(defn- failure-edit-ref
  "Compact `edit N` reference — with its anchor and/or path when present — for the
   per-group affected-edit list."
  [{:keys [edit-index from_anchor path]}]
  (str "edit " edit-index (when from_anchor (str " @" from_anchor)) (when path (str " in " path))))

(defn- patch-failure-message
  [failures]
  ;; patch is ATOMIC — a single failed edit rejects the WHOLE batch and writes
  ;; NOTHING, so the file is byte-for-byte unchanged. Say so up front: the model
  ;; must not assume a partial application and must not re-read to "repair" it.
  (let [atomic "No changes: patch is atomic. "]
    (if (= 1 (count failures))
      (str atomic (explain-failure (first failures)))
      ;; Group related failures into one cause plus compact affected-edit refs.
      (let
        [ordered-families (distinct (map failure-family failures))
         groups (group-by failure-family failures)]

        (str atomic
             (count failures)
             " edits failed" (when (> (count ordered-families) 1)
                               (str " (" (count ordered-families) " distinct causes)"))
             ":\n" (str/join "\n"
                             (for
                               [family ordered-families
                                :let [fs (get groups family)]]

                               (str "  • " (count fs)
                                    " × " (family-headline family fs)
                                    "\n      " (str/join "; " (map failure-edit-ref fs))))))))))

(defn- tracked-patch-failure-result
  "Attach the per-path retry count and loop hint to one atomic failure result."
  [failures checks]
  (let
    [counts
     (into {}
           (keep (fn [path]
                   (when-let [file (try (safe-path path) (catch Throwable _ nil))]
                     [path (bump-patch-fail-count! file)])))
           (distinct (map :path failures)))

     failures
     (mapv (fn [failure]
             (cond-> failure
               (get counts (:path failure))
               (assoc :consecutive-failures (get counts (:path failure)))))
           failures)

     hint
     (some (fn [[path n]]
             (patch-loop-hint n path))
           counts)]

    {:success? false
     :failures failures
     :checks checks
     :loop-hint hint
     :message (cond-> (patch-failure-message failures)
                hint
                (str "\n" hint))}))

(defn- patch-syntax-failures
  "Return plans that turn clean supported code into syntactically broken code."
  [plans]
  (vec (keep (fn [{:keys [path before after]}]
               (when-let [lang (index/code-language path)]
                 (when (and (not (zipper/syntax-broken? lang (str before)))
                            (zipper/syntax-broken? lang (str after)))
                   {:edit-index 0
                    :path path
                    :reason :syntax-error
                    :message (str "patch refused: this edit would leave "
                                  path
                                  " with a SYNTAX ERROR (unbalanced delimiters / a broken form) — "
                                  "it parsed cleanly before. NOTHING was written. Re-cat for fresh "
                                  "anchors and fix the replacement, or use struct_patch.")})))
             plans)))

(defn- commit-patch-plans!
  "Write validated plans and clear their retry counters."
  [plans]
  (doseq [{:keys [file after]} plans]
    (spit file after)
    (capture-temp-write! file))
  (doseq [{:keys [file]} plans]
    (clear-patch-fail-count! file)))

(defn patch-safe
  "Apply anchored patch edits to the filesystem.

   Returns a structured map; **never throws on normal failure paths**
   (stale anchor, file not found, path escape). Reserves exceptions for genuinely
   unexpected errors (thread interrupt, disk full, etc.).

   Success shape:
     {:success? true
      :plans    [{:path :before :after} ...]
      :checks   [<per-edit-check> ...]}

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
      (tracked-patch-failure-result failures checks)
      (let
        [plans (vec plans)
         syntax-failures (patch-syntax-failures plans)]

        (if (seq syntax-failures)
          (let [result (tracked-patch-failure-result syntax-failures checks)]
            (assoc result
              :checks (into (vec checks) (:failures result))
              ;; Language packs may repair the full unwritten candidates in an
              ;; :around hook. Never place their source inside the model-facing error.
              :candidate-plans (mapv #(select-keys % [:path :before :after]) plans)
              :broken-paths (mapv :path (:failures result))))
          (do (commit-patch-plans! plans)
              {:success? true
               :plans (mapv #(select-keys % [:path :before :after]) plans)
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
;; `:expected_size` pair with (:mtime / :size) from a prior cat for atomic
;; read-modify-write on existing files. Patch uses content-addressed anchors
;; instead of a file-wide metadata guard.
;; =============================================================================

(def ^:private write-required-keys #{"path" "content"})

(def ^:private write-optional-keys
  ;; "atomic" = the documented multi-file escape flag (read from raw args by
  ;; `mutation-atomic?`); allowed here so it isn't refused as unknown.
  #{"expected_mtime" "expected_size" "is_overwrite" "atomic" "allow_dirty"})

(def ^:private write-allowed-keys (set/union write-required-keys write-optional-keys))

(defn- coerce-write-args
  [args]
  (when-not (map? args)
    (throw (ex-info "write expects a single map argument"
                    {:type :ext.foundation.editing/invalid-write-args :got (type args)})))
  (let
    [missing
     (seq (remove #(contains? args %) write-required-keys))

     unknown
     (seq (remove write-allowed-keys (keys args)))]

    (when missing
      (throw (ex-info (str "write missing required keys: "
                           (str/join ", " (map #(str "'" % "'") missing))
                           " (write needs 'path' and 'content').")
                      {:type :ext.foundation.editing/invalid-write-args
                       :missing (vec missing)
                       :args args})))
    (when unknown
      (throw (ex-info (str "write has unknown keys: "
                           (str/join ", " (map #(str "'" % "'") unknown))
                           ". Allowed: "
                           (str/join ", " (sort write-allowed-keys))
                           ".")
                      {:type :ext.foundation.editing/invalid-write-args
                       :unknown (vec unknown)
                       :allowed (vec write-allowed-keys)
                       :args args})))
    (when-not (string? (get args "content"))
      (throw (ex-info "write \"content\" must be a string"
                      {:type :ext.foundation.editing/invalid-write-args
                       :got (type (get args "content"))}))))
  (update args "path" str))

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
  (let
    [args
     (coerce-write-args args)

     path
     (get args "path")

     content
     (str (get args "content"))

     is_overwrite
     (if (contains? args "is_overwrite") (get args "is_overwrite") true)

     allow_dirty
     (boolean (get args "allow_dirty"))

     expected_mtime
     (get args "expected_mtime")

     expected_size
     (get args "expected_size")

     resolved
     (try {:file (safe-path path) :rel (rel-path (safe-path path))}
          (catch clojure.lang.ExceptionInfo e
            {:error {:reason (case (:type (ex-data e))
                               :ext.foundation.editing/path-escape
                               :path-escape

                               :path-error)
                     :message (ex-message e)
                     :data (ex-data e)}}))]

    (if-let [perr (:error resolved)]
      (let
        [check {:edit-index 0 :path path :reason (:reason perr) :path-error perr}
         file-for-counter (try (safe-path path) (catch Throwable _ nil))
         n (when file-for-counter (bump-patch-fail-count! file-for-counter))]

        {:success? false
         :failures [(cond-> check
                      n
                      (assoc :consecutive-failures n))]
         :checks [check]
         :loop-hint (when (and file-for-counter n) (patch-loop-hint n path))
         :message (str "write failed: " (:message perr))})
      (let
        [^java.io.File file (:file resolved)
         rel (:rel resolved)
         exists? (.exists file)
         is-dir? (and exists? (.isDirectory file))
         before (when (and exists? (not is-dir?)) (slurp file))
         actual-mtime (when exists? (.lastModified file))
         actual-size (when exists? (.length file))
         fail (cond
                is-dir? {:reason :path-is-dir :message (str "write target is a directory: " rel)}
                (and (not is_overwrite) exists?)
                {:reason :exists
                 :path rel
                 :message (str "write refused: " rel " already exists and :is_overwrite is false")}
                ;; A whole-file write over a file with UNCOMMITTED changes is
                ;; how a truncated reconstruction silently wipes work. Refuse
                ;; it: surgical edits belong in patch()/struct_patch().
                (and exists? (not is-dir?) (not allow_dirty) (git/file-dirty? file))
                {:reason :dirty
                 :path rel
                 :message (str "write refused: "
                               rel
                               " has UNCOMMITTED changes — a "
                               "whole-file write would clobber edits already in flight "
                               "(this is exactly how a truncated reconstruction wipes a "
                               "file). Make surgical changes with patch(...) or "
                               "struct_patch(...) instead, or commit/checkout "
                               rel
                               " first. Pass allow_dirty=True to overwrite on purpose.")}
                (and exists?
                     (some? expected_mtime)
                     (pos? (long expected_mtime))
                     (not= (long expected_mtime) (long actual-mtime)))
                {:reason :stale
                 :stale {:reason :stale-mtime
                         :expected_mtime expected_mtime
                         :actual-mtime actual-mtime
                         :actual-size actual-size}
                 :message (str "write refused: " rel " mtime changed since :expected_mtime")}
                (and exists? (some? expected_size) (not= (long expected_size) (long actual-size)))
                {:reason :stale
                 :stale {:reason :stale-size
                         :expected_size expected_size
                         :actual-size actual-size
                         :actual-mtime actual-mtime}
                 :message (str "write refused: " rel " size changed since :expected_size")})]

        (if fail
          (let [n (bump-patch-fail-count! file)]
            {:success? false
             :failures [(cond->
                          (assoc fail
                            :edit-index 0
                            :path rel)
                          n
                          (assoc :consecutive-failures n))]
             :checks [(assoc fail
                        :edit-index 0
                        :path rel)]
             :loop-hint (patch-loop-hint n rel)
             :message (cond-> (:message fail)
                        (>= n (long patch-fail-loop-threshold))
                        (str "\n" (patch-loop-hint n rel)))})
          (do (ensure-parent-dirs! file)
              (spit file content)
              (capture-temp-write! file)
              (clear-patch-fail-count! file)
              {:success? true
               :plan {:path rel :before before :after content :op (if exists? :update :add)}
               :checks
               [{:edit-index 0 :path rel :op (if exists? :update :add) :existed? exists?}]}))))))

(defn- create-dirs-safe
  [path]
  (let [f (safe-path path)]
    (fs/create-dirs f)
    (rel-path f)))

(defn- copy-safe
  ([src dest] (copy-safe src dest nil))
  ([src dest opts]
   (let
     [src-file
      (safe-path src)

      dest-file
      (safe-path dest)]

     (ensure-parent-dirs! dest-file)
     (fs/copy src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- move-safe
  ([src dest] (move-safe src dest nil))
  ([src dest opts]
   (let
     [src-file
      (safe-path src)

      dest-file
      (safe-path dest)]

     (ensure-parent-dirs! dest-file)
     (fs/move src-file dest-file (or opts {}))
     (rel-path dest-file))))

(defn- delete-path!
  "Delete a file or directory tree after `safe-path` has confined it to the workspace."
  [path]
  (let [f (safe-path path)]
    (if (fs/directory? f) (fs/delete-tree f) (fs/delete f))
    true))

(defn- delete-safe [path] (delete-path! path))

(defn- delete-if-exists-safe
  [path]
  (let [f (safe-path path)]
    (if (fs/exists? f) (do (if (fs/directory? f) (fs/delete-tree f) (fs/delete f)) true) false)))

(defn- exists-safe? [path] (fs/exists? (safe-path path)))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-result->model
  "Shape an internal read result into the MODEL-facing form: the internal
   `:lines` (a vec of `[ln text]` tuples) becomes the model's `:anchors` — an
   ordered `{anchor {\"text\" text}}` map (`patch/lines->anchor-map`, a line-ordered
   LinkedHashMap, the key IS the `patch :from_anchor`). The internal `:lines`
   tuple vector and the read-file `{ln anchor}` `:anchors` are both dropped;
   every `:ranges` window converts the same way. The internal read pipeline
   keeps working on tuples — this is the single boundary where the model
   payload is built."
  [out]
  ;; The internal read pipeline works on keyword-keyed maps (`:lines` tuples);
  ;; this is the single boundary where the string-keyed MODEL payload is built.
  (letfn
    [(->win [m]
       (cond-> {"anchors" (patch/lines->anchor-map (:lines m))}
         (contains? m :path)
         (assoc "path" (:path m))

         (contains? m :next-offset)
         (assoc "next_offset" (:next-offset m))

         (contains? m :eof?)
         (assoc "eof" (:eof? m))

         (contains? m :truncated?)
         (assoc "truncated" (:truncated? m))

         (contains? m :mtime)
         (assoc "mtime" (:mtime m))

         (contains? m :size)
         (assoc "size" (:size m))

         (:stale? m)
         (assoc "anchors_stale" true)

         (contains? m :range)
         (assoc "range" (:range m))))]
    (cond-> (->win out)
      (seq (:ranges out))
      (assoc "ranges" (mapv ->win (:ranges out))))))

(defn- normalize-cat-anchor-option
  "Accept the documented anchor shapes plus the common model mistake of passing
  a JSON/EDN-looking anchor range as one quoted string."
  [anchor]
  (if (and (string? anchor) (str/starts-with? (str/trim anchor) "["))
    (try (let [v (edn/read-string anchor)]
           (if (vector? v) v anchor))
         (catch Throwable _ anchor))
    anchor))

(defn- cat-tool
  "Read a text-file window. `await cat(path)` reads the whole file (≤2000 lines)
   — slice only for bigger files or a middle/tail section. Options = a dict,
   snake_case keys:
     await cat(path, {\"range\": [start, end]})   # inclusive 1-based line range
     await cat(path, {\"ranges\": [[s, e], ...]})  # several windows in one call
     await cat(path, {\"anchor\": \"325:0e3\"})      # one line by its lineno:hash anchor
     await cat(path, {\"anchor\": [\"H1\", \"H2\"]})   # inclusive anchor range H1..H2
     await cat(path, {\"tail\": 200})              # last N lines (omit N → 2000)
   Returns {\"anchors\": {\"lineno:hash\": {\"text\": line}, ...}, \"next_offset\", \"eof\",
   \"truncated\", \"mtime\", \"size\"}. \"anchors\" is the ONLY content key — an ORDERED
   {anchor: {\"text\": line}} map — MIRRORS rg's hit value, so read v[\"text\"]
   uniformly; there is NO top-level \"lines\"/\"content\" key (c[\"lines\"] KeyErrors).
   Each key IS the `patch` from_anchor — copy it straight into an edit.
   Not \"eof\"/\"truncated\" → paginate from \"next_offset\"."
  ([path]
   (if (map? path)
     ;; All-kwargs form: `cat(path="p", ranges=rs)` collapses at the Python
     ;; boundary to ONE spec map `{"path" "p", ...opts}` (see __vis_exec_call__).
     ;; Pull the path out and delegate to the opts-map arity so range/ranges/
     ;; anchor/tail keep working — mirrors rg's lone-spec-map contract.
     (cat-tool (get path "path") (dissoc path "path"))
     (let [out (read-file path 1 default-cat-limit)]
       (tool-success {:op :cat
                      :path path
                      :kind :file
                      :result (cat-result->model out)
                      :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out)}}))))
  ([path arg]
   (cond
     ;; Python-native form: a single options dict, e.g.
     ;;   cat("p", {"range": [5, 10]})       cat("p", {"ranges": [[1,5],[20,25]]})
     ;;   cat("p", {"anchor": A})            cat("p", {"anchor": [A1, A2]})
     ;;   cat("p", {"tail": 100})            cat("p", {})  -> whole file
     ;; Delegated to the keyword arities below so internal Clojure callers
     ;; (which pass bare keyword args) keep working unchanged.
     (map? arg) (let
                  [rng
                   (get arg "range")

                   ranges
                   (get arg "ranges")

                   anc
                   (normalize-cat-anchor-option (get arg "anchor"))

                   tail
                   (get arg "tail")]

                  (cond rng (cat-tool path :range (first rng) (second rng))
                        ranges (cat-tool path :ranges ranges)
                        (vector? anc) (cat-tool path :anchor (first anc) (second anc))
                        (some? anc) (cat-tool path :anchor anc)
                        (integer? tail) (cat-tool path :tail tail)
                        (some? tail) (cat-tool path :tail)
                        :else (cat-tool path)))
     (= arg :tail) (let [out (tail-file path default-cat-limit)]
                     (tool-success {:op :cat
                                    :path path
                                    :kind :file
                                    :result (cat-result->model out)
                                    :metadata {:next-offset (:next-offset out)
                                               :truncated? (:truncated? out)
                                               :tail? true}}))
     :else (throw (ex-info "cat options must be a dict, e.g. cat(path, {\"range\": [start, end]})"
                           {:type :ext.foundation.editing/invalid-cat-args :got arg}))))
  ([path arg n]
   (case arg
     :tail
     (let [out (tail-file path n)]
       (tool-success {:op :cat
                      :path path
                      :kind :file
                      :result (cat-result->model out)
                      :metadata
                      {:next-offset (:next-offset out) :truncated? (:truncated? out) :tail? true}}))

     :ranges
     (let [out (read-file-ranges path n)]
       (tool-success {:op :cat
                      :path path
                      :kind :file
                      :result (cat-result->model out)
                      :metadata {:truncated? (:truncated? out)
                                 :ranges (mapv :range (:ranges out))}}))

     :anchor
     ;; (cat path :anchor A) — the single line carrying the `lineno:hash`
     ;; anchor A (the symmetric read for patch :from_anchor).
     (let [out (read-file-by-anchor path n nil)]
       (tool-success {:op :cat
                      :path path
                      :kind :file
                      :result (cat-result->model out)
                      :metadata {:next-offset (:next-offset out)
                                 :truncated? (:truncated? out)
                                 :range (:range out)}}))

     (throw
       (ex-info
         "cat options must use {\"tail\": N}, {\"ranges\": [[s, e], ...]}, or {\"anchor\": A}; for one range use {\"range\": [start, end]}"
         {:type :ext.foundation.editing/invalid-cat-args :got arg}))))
  ([path mode start end]
   (case mode
     ;; (cat path :range start end) — INCLUSIVE start..end (both 1-based).
     :range
     (do (validate-cat-range! start end)
         (let
           [n
            (inc (- (long end) (long start)))

            out
            (read-file path start n)]

           (tool-success {:op :cat
                          :path path
                          :kind :file
                          :result (cat-result->model out)
                          :metadata {:next-offset (:next-offset out)
                                     :truncated? (:truncated? out)
                                     :range [start end]}})))

     ;; (cat path :anchor from_anchor to_anchor) — INCLUSIVE window between the
     ;; lines anchored from_anchor..to_anchor, addressed by content.
     :anchor
     (let [out (read-file-by-anchor path start end)]
       (tool-success {:op :cat
                      :path path
                      :kind :file
                      :result (cat-result->model out)
                      :metadata {:next-offset (:next-offset out)
                                 :truncated? (:truncated? out)
                                 :range (:range out)}}))

     (throw (ex-info "cat window must use {\"range\": [start, end]} or {\"anchor\": [from, to]}"
                     {:type :ext.foundation.editing/invalid-cat-args :got mode})))))

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
   is_files_only. A `paths` entry is a FILE (searched as that ONE file) or a
   DIRECTORY (walked as a tree) — like ripgrep. A path that does NOT exist is
   never a hard error: it CLIMBS to its nearest existing ancestor dir (parent,
   then parent-of-parent), and each missing path is REPORTED in `missing_paths`
   (same field on find_files). No regex / no AND — filter in Python.

   Result:
     content:       {\"matches\": {path: {\"lineno:hash\": {\"text\": line}}}, \"hit_count\", \"file_count\", \"first_hit\"}
     is_files_only: {\"files\": [...], \"file_count\"}
   The content value is ALWAYS a `{\"text\": line}` map (ONE uniform shape); WITH
   context it ALSO carries \"before\"/\"after\" {anchor: {\"text\": line}} maps. `hit_count` and
   `file_count` count what's SHOWN; a multi-file result adds `file_counts`
   {path: n} — the shown files ranked hottest-first (skip the Counter). When MORE
   files match than shown, `total_file_count` reports the true breadth (a LOWER
   bound, flagged `total_file_count_is_exact` false, if the breadth probe capped).
   Gotcha: each \"matches\" key is a \"lineno:hash\" anchor — pass it AS patch from_anchor."
  [& args]
  ;; Accept either a single spec map OR inline kwargs. Manual dispatch
  ;; (instead of `& {:as spec}`) so that malformed input — a stray
  ;; positional string, an odd-length rest seq — routes through one
  ;; clean `:invalid-rg-spec` error instead of Clojure's raw
  ;; "No value supplied for key" destructure exception.
  (let
    [[a & more]
     args

     ;; A positional query is a string ("x") or a list of strings (["a" "b"]).
     query-arg?
     (fn [x]
       (or (string? x) (and (sequential? x) (seq x) (every? string? x))))

     ->query
     (fn [x]
       (if (string? x) [x] (vec x)))

     spec
     (cond
       ;; rg({...}) — a full spec map (string-keyed from the boundary).
       (and (= 1 (count args)) (map? a)) a
       ;; rg("x") / rg(["a" "b"]) — bare query.
       (and (= 1 (count args)) (query-arg? a)) {"query" (->query a)}
       ;; rg("x", {opts}) — query + an options MAP.
       (and (= 2 (count args)) (query-arg? a) (map? (first more))) (assoc (first more)
                                                                     "query" (->query a))
       :else (throw (ex-info
                      "rg takes a query, e.g. rg(\"x\") or rg([\"x\", \"y\"], paths=[\"src\"])."
                      {:type :ext.foundation.editing/invalid-rg-arity
                       :expected '([query] [query opts] [spec-map])
                       :got args})))

     {:keys [needles paths include is_files_only context limit]}
     (coerce-rg-spec spec)

     out
     (rg-search spec)

     mode
     (if is_files_only :files-only :content)

     ;; NO `:spec` echo in the model-facing payload: echoing the input
     ;; map back taught models a phantom "spec" INPUT key (`rg({...,
     ;; "spec": {}})`). The spec stays host-side on `:metadata` below
     ;; for channel labels.
     ;; `:needles` = the parsed OR search terms, carried on the result so the
     ;; op-card HEADLINE can name WHAT was searched (a bare "N hits in M
     ;; files" is useless without it). Model-facing but harmless — it is the
     ;; query the model itself sent, echoed back as data it can re-read.
     ;; `shared` and everything assoc'd onto it below is the model-facing rg
     ;; result — string keys, no keyword values (mode/truncated_by stringified).
     shared
     (cond->
       {"mode" (if is_files_only "files_only" "content")
        "needles" needles
        "truncated_by" (str/replace (name (:truncated-by out)) "-" "_")
        "paths" paths
        "limit" limit}
       (seq (:missing out))
       (assoc "missing_paths" (:missing out)))

     result
     (case mode
       :content
       (let
         [hits
          (vec (:hits out))

          ordered-paths
          (distinct (map :path hits))

          by-path
          (group-by :path hits)

          total-files
          (:total-file-count out)

          more-files?
          (> (long total-files) (count ordered-paths))

          ;; Grouped by file → each file is an ORDERED
          ;; `{match-anchor → value}` map (a LinkedHashMap, so it
          ;; serializes in line order). The path is stated ONCE (the
          ;; key). The value is ALWAYS a `{"text" <match>}` map — a
          ;; SINGLE uniform shape whether or not a context window was
          ;; asked for. WITH context it ALSO carries `:before`/`:after`
          ;; `{anchor→{"text" text}}` maps too, so every match keeps its own
          ;; before/after context and ALL lines (match + context) stay
          ;; patchable by anchor key.
          matches
          (let [^java.util.LinkedHashMap mm (java.util.LinkedHashMap.)]
            (doseq [p ordered-paths]
              (let [^java.util.LinkedHashMap fm (java.util.LinkedHashMap.)]
                (doseq [{:keys [line text before after]} (get by-path p)]
                  (.put fm
                        (patch/line-anchor line text)
                        (cond-> {"text" text}
                          (seq before)
                          (assoc "before" (patch/lines->anchor-map before))

                          (seq after)
                          (assoc "after" (patch/lines->anchor-map after)))))
                (.put mm p fm)))
            mm)

          ;; Per-file hit counts for the SHOWN files, ranked hottest-first
          ;; — saves the model a `Counter` over `matches`, and doubles as a
          ;; top-files view. Keys mirror `matches` (shown files only); the
          ;; hottest file's count can be partial when a cap cut it mid-file.
          file-counts
          (let [^java.util.LinkedHashMap fc (java.util.LinkedHashMap.)]
            (doseq
              [p (sort-by (fn [p]
                            [(- (count (get by-path p))) p])
                          ordered-paths)]
              (.put fc p (count (get by-path p))))
            fc)]

         (cond->
           (assoc shared
             "matches" matches
             "hit_count" (count hits)
             "file_count" (count ordered-paths)
             "first_hit" (when (pos? (count hits))
                           (let [{:keys [path line]} (nth hits 0)]
                             (str path ":" line)))
             "context" (when (pos? (long context)) {"before" context "after" context}))
           (> (count ordered-paths) 1)
           (assoc "file_counts" file-counts)

           more-files?
           (assoc "total_file_count" total-files)

           (and more-files? (not (:total-file-count-exact? out)))
           (assoc "total_file_count_is_exact" false)))

       :files-only
       (let
         [files
          (vec (:files out))

          total-files
          (:total-file-count out)

          more-files?
          (> (long total-files) (count files))]

         (cond->
           (assoc shared
             "files" files
             "file_count" (count files))
           more-files?
           (assoc "total_file_count" total-files)

           (and more-files? (not (:total-file-count-exact? out)))
           (assoc "total_file_count_is_exact" false))))]

    (tool-success
      {:op :rg
       :path (if (= 1 (count paths)) (first paths) ".")
       :kind :dir
       :result result
       :metadata
       (cond->
         {:spec spec :paths paths :include include :mode mode :truncated-by (:truncated-by out)}
         (= mode :content)
         (assoc :hit-count (get result "hit_count"))

         (= mode :files-only)
         (assoc :file-count (get result "file_count")))})))

(def ^:private ^:const patch-diff-context-lines 3)

(def ^:private ^:const patch-diff-max-render-lines 240)

(def ^:private ^:const patch-java-diff-max-lines 5000)

(defn- cap-diff-lines
  "Bound a rendered diff to `patch-diff-max-render-lines`, keeping a HEAD and a
   TAIL window rather than a plain head-cut. A pure head-cut let a
   deletion-heavy hunk fill the whole visible budget with `-` lines and bury
   the `+` replacement / trailing context below the cut, so a correct edit read
   as a catastrophic deletion. Head+tail keeps the additions and closing
   context visible."
  [lines]
  (let
    [lines
     (vec lines)

     n
     (long (count lines))]

    (if (<= n patch-diff-max-render-lines)
      lines
      (let
        [tail-n
         (quot patch-diff-max-render-lines 4)

         head-n
         (- patch-diff-max-render-lines tail-n)

         omitted
         (- n head-n tail-n)]

        (vec (concat (subvec lines 0 head-n)
                     [(str "... diff truncated; " omitted " line(s) omitted")]
                     (subvec lines (- n tail-n))))))))

(defn- common-prefix-count
  [a b]
  (let [limit (long (min (count a) (count b)))]
    (loop [i 0]
      (if (and (< i limit) (= (a i) (b i))) (recur (inc i)) i))))

(defn- common-suffix-count
  [a b ^long prefix-count]
  (let
    [a-count
     (long (count a))

     b-count
     (long (count b))

     limit
     (- (min a-count b-count) prefix-count)]

    (loop [i 0]
      (if (and (< i limit) (= (a (- a-count i 1)) (b (- b-count i 1)))) (recur (inc i)) i))))

(defn- prefixed-diff-lines
  [prefix lines]
  (let
    [lines
     (vec lines)

     n
     (long (count lines))

     shown-n
     (min n patch-diff-max-render-lines)

     shown
     (subvec lines 0 shown-n)

     omitted
     (- n shown-n)]

    (cond-> (mapv #(str prefix %) shown)
      (pos? omitted)
      (conj (str prefix "... (" omitted " line(s) omitted)")))))

(defn- compact-diff-lines
  "Linear fallback for very large files. It is a bounded preview, not a
   minimal diff: for normal-sized files `java-diff-utils` renders real
   unified hunks."
  [a b]
  (let
    [prefix-count
     (long (common-prefix-count a b))

     suffix-count
     (long (common-suffix-count a b prefix-count))

     a-count
     (long (count a))

     b-count
     (long (count b))

     a-change-end
     (- a-count suffix-count)

     b-change-end
     (- b-count suffix-count)

     pre-start
     (max 0 (- prefix-count patch-diff-context-lines))

     post-end
     (min a-count (+ a-change-end patch-diff-context-lines))

     pre-lines
     (subvec a pre-start prefix-count)

     del-lines
     (subvec a prefix-count a-change-end)

     add-lines
     (subvec b prefix-count b-change-end)

     post-lines
     (subvec a a-change-end post-end)

     before-skip
     pre-start

     after-skip
     (- a-count post-end)]

    (vec (concat ["--- before" "+++ after"]
                 (when (pos? before-skip) [(str "... " before-skip " unchanged line(s) before")])
                 (map #(str " " %) pre-lines)
                 (prefixed-diff-lines "-" del-lines)
                 (prefixed-diff-lines "+" add-lines)
                 (map #(str " " %) post-lines)
                 (when (pos? after-skip) [(str "... " after-skip " unchanged line(s) after")])))))

(defn- java-unified-diff-lines
  [a b]
  (let [patch (DiffUtils/diff a b)]
    (vec (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a patch patch-diff-context-lines))))

(defn- unified-diff-text
  "Unified diff preview for two file blobs. Normal-sized files use
   `java-diff-utils` for real hunks. Very large files use a linear bounded
   fallback to keep `patch` result rendering from becoming the slow path."
  [before after]
  (cond (= before after) nil
        (nil? before) (str/join "\n" (prefixed-diff-lines "+" (str/split-lines (or after ""))))
        (nil? after) (str "--- (deleted, " (count (str/split-lines (or before ""))) " lines)")
        :else (let
                [a
                 (vec (str/split-lines before))

                 b
                 (vec (str/split-lines after))

                 diff-lines
                 (if (and (<= (count a) patch-java-diff-max-lines)
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
                                    when both before+after are nil}

   Line counts (`:lines-before` / `:lines-after` / `:delta-lines`) were
   intentionally dropped: the `:diff` carries the exact change and the
   scalars duplicated that information at the cost of trailer bloat."
  [{:keys [op path before after]}]
  ;; Model-facing per-file summary (patch/write/struct_patch result) — string
  ;; keys, enum values stringified to snake_case.
  (let [diff-text (unified-diff-text before after)]
    (cond-> {"path" path "op" (name (or op :update)) "changed" (not= before after)}
      diff-text
      (assoc "diff" diff-text))))

(defn refresh-file-summary
  "Recompute a per-file summary's \"diff\"/\"changed\" from the ORIGINAL `before`
   and the FINAL on-disk `after`. A language pack that rewrites a just-edited
   file in an :after op-hook (parinfer paren-repair + cljfmt) calls this so the
   MODEL-FACING diff shows the bytes actually written, not the pre-hook
   intermediate the raw edit produced. All other summary keys are preserved."
  [summary before after]
  (let [diff-text (unified-diff-text before after)]
    (cond-> (assoc summary "changed" (not= before after))
      diff-text
      (assoc "diff" diff-text)

      (nil? diff-text)
      (dissoc "diff"))))

(defn- patch-tool
  "Edit files by anchor (no text search/replace).

   Each `lineno:hash` comes from a fresh `cat`, `rg`, or `struct_index` read.
   Anchors re-resolve against live content: unrelated changes are preserved when
   the targets still match; changed targets abort the entire atomic batch. Omit
   `to_anchor` for one line, use an inclusive range otherwise, and use an empty
   replacement to delete. Re-read after every successful write."
  [edits]
  (let [result (patch-safe edits)]
    (if (:success? result)
      (let
        [plans (:plans result)
         summaries (mapv patch-result-file-summary plans)]

        (tool-success {:op :patch
                       :path (or (:path (first plans)) ".")
                       :kind :file
                       :result summaries
                       :metadata {:file-count (count summaries)
                                  :changed-count (count (filter #(get % "changed") summaries))
                                  ;; Pre-edit content per file (relativized path == summary
                                  ;; "path") so an :after op-hook that rewrites the file
                                  ;; (paren-repair/format) can re-diff against final bytes.
                                  :file-befores (mapv #(select-keys % [:path :before]) plans)}}))
      ;; Failure: full structured `:error` map with `:reason`, per-edit
      ;; `:failures`, `:checks`, and the optional `:loop-hint` so the
      ;; model can read them as plain map keys (no try/catch needed).
      (let [first-failure (first (:failures result))]
        (extension/failure
          {:result nil
           :op :patch
           :metadata (cond->
                       {:target {:requested (str (or (:path first-failure) "."))
                                 :resolved nil
                                 :absolute nil
                                 :kind :file}
                        :started-at-ms (now-ms)
                        :finished-at-ms (now-ms)
                        :duration-ms 0}
                       ;; Whole-batch candidates on a SYNTAX refusal — metadata
                       ;; only (the model-facing throw carries `:error` alone),
                       ;; so a language pack's :around op-hook can whole-source-
                       ;; repair the broken files and commit the batch itself.
                       (:candidate-plans result)
                       (assoc :candidate-plans
                         (:candidate-plans result) :broken-paths
                         (:broken-paths result)))
           :error {:message (:message result)
                   :reason (:reason first-failure)
                   :failures (:failures result)
                   :checks (:checks result)
                   :loop-hint (:loop-hint result)}})))))

(defn- normalize-write-args
  "Accept write args EITHER as a single options map
   (`await write({\"path\": P, \"content\": S})`) OR positionally
   (`await write(P, S)` / `await write(P, S, {opts})`). Returns the
   canonical options map for `write-safe`."
  [args]
  (cond
    ;; single map → already canonical (also covers Clojure trailing-kwargs)
    (and (= 1 (count args)) (map? (first args))) (first args)
    ;; positional: path, content, optional trailing opts map
    (and (>= (count args) 2) (string? (first args)) (string? (second args)))
    (merge {"path" (first args) "content" (second args)}
           (let [extra (nth args 2 nil)]
             (when (map? extra) extra)))
    ;; legacy Clojure-style trailing kwargs (even k/v count)
    (and (pos? (count args)) (even? (count args))) (apply hash-map args)
    :else (throw (ex-info "write expects (path, content) or a single options map"
                          {:type :ext.foundation.editing/invalid-write-args
                           :got (mapv type args)}))))

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
      (let
        [plan (:plan result)
         summary (patch-result-file-summary plan)]

        (tool-success {:op :write
                       :path (:path plan)
                       :kind :file
                       :result [summary]
                       :metadata {:mode :write
                                  :file-count 1
                                  :changed-count (if (get summary "changed") 1 0)
                                  :op (:op plan)
                                  :file-befores [(select-keys plan [:path :before])]}}))
      (let [first-failure (first (:failures result))]
        (extension/failure
          {:result nil
           :op :write
           :metadata {:target {:requested (str (or (:path first-failure) (:path args) "."))
                               :resolved nil
                               :absolute nil
                               :kind :file}
                      :mode :write
                      :started-at-ms (now-ms)
                      :finished-at-ms (now-ms)
                      :duration-ms 0}
           :error {:message (:message result)
                   :reason (:reason first-failure)
                   :failures (:failures result)
                   :checks (:checks result)
                   :loop-hint (:loop-hint result)
                   :mode :write}})))))

(defn- create-dirs-tool
  "Ensure dir exists. Returns the canonical foundation map shape so the
   model destructures `(:path r)` / `(:created? r)` directly off the
   bound result."
  [path]
  (let
    [before
     (fs/exists? (safe-path path))

     out
     (create-dirs-safe path)]

    (tool-success {:op :create-dirs
                   :path path
                   :kind :dir
                   :result {"path" out "created" (not before) "already_existed" before}
                   :metadata {:created? (not before) :already-existed? before}})))

(defn- copy-tool
  "Copy a path.
     await copy(src, dest)
     await copy(src, dest, {\"is_overwrite\": True})

   Returns {\"src\": src, \"dest\": dest, \"path\": dest}.
   Gotcha: without is_overwrite an existing dest fails."
  ([src dest & {:as opts}]
   (let [out (copy-safe src dest opts)]
     (tool-success {:op :copy
                    :path dest
                    :kind :path
                    :result {"src" src "dest" dest "path" out}
                    :metadata
                    {:src (path->target src :path) :dest (path->target dest :path) :opts opts}}))))

(defn- move-tool
  "Move / rename a path.
     await move(src, dest)
     await move(src, dest, {\"is_overwrite\": True})

   Returns {\"src\": src, \"dest\": dest, \"path\": dest}.
   Gotcha: without is_overwrite an existing dest fails."
  ([src dest & {:as opts}]
   (let [out (move-safe src dest opts)]
     (tool-success {:op :move
                    :path dest
                    :kind :path
                    :result {"src" src "dest" dest "path" out}
                    :metadata
                    {:src (path->target src :path) :dest (path->target dest :path) :opts opts}}))))

(defn- delete-tool
  "Delete a path (file or directory).
     await delete(path)
     await delete(path, {\"is_missing_ok\": True})   # no-op instead of raising when absent

   Returns {\"path\": path, \"deleted\": bool}. Without is_missing_ok a missing path
   raises; with it, \"deleted\" is False when nothing was there (folds in the old
   delete_if_exists)."
  [path & {:as opts}]
  (let
    [deleted?
     (if (get opts "is_missing_ok") (delete-if-exists-safe path) (do (delete-safe path) true))]
    (tool-success {:op :delete
                   :path path
                   :kind :path
                   :result {"path" path "deleted" deleted?}
                   :metadata {:deleted? deleted?}})))

(defn- delete-if-exists-tool
  "Delete a path if it exists (no-op otherwise).
     await delete_if_exists(path)

   Returns {\"path\": path, \"deleted\": bool}.
   Gotcha: \"deleted\" is False when nothing was there — never raises on a missing path."
  [path]
  (let [deleted? (delete-if-exists-safe path)]
    (tool-success {:op :delete-if-exists
                   :path path
                   :kind :path
                   :result {"path" path "deleted" deleted?}
                   :metadata {:deleted? deleted?}})))

(defn- exists-tool
  "Check whether a path exists.
     await exists(path)

   Returns {\"path\": path, \"exists\": bool}.
   Gotcha: returns a dict, not a bare bool — read r[\"exists\"]."
  [path]
  (let [exists? (exists-safe? path)]
    (tool-success {:op :file-exists
                   :path path
                   :kind :path
                   :result {"path" (str path) "exists" exists?}
                   :metadata {:exists? exists?}})))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Symbol declarations.
;;
;; Underlying `xxx-tool` defs retain developer docs + arglists. Each native
;; symbol supplies compact routing/semantics in `:description`; exact inputs
;; live only in its schema and are appended once by `doc(name)`.
;; `:symbol` overrides the var name (`cat-tool` -> `cat`) for the model-facing
;; surface; everything else (examples, error hook, result spec)
;; lives in opts because it has nothing to do with the function's signature.
;; -----------------------------------------------------------------------------

(defn- def->wire
  "One `index/definitions` entry → snake_case wire map. Deliberately the SAME
   shape as a DEFINITION row from `struct_occurrences` (`kind`/`visibility`/`signature`/
   `doc`/`anchor`/`end_anchor`) so the two structural lenses read alike — plus the
   def's `name` (`struct_index` lists many) and nesting `depth` (0 = top-level). Nil
   fields are dropped to keep the row lean."
  [d]
  (cond->
    {"name" (:name d)
     "kind" (:kind d)
     "anchor" (:anchor d)
     "end_anchor" (:end-anchor d)
     "depth" (:depth d 0)}
    (:visibility d)
    (assoc "visibility" (:visibility d))

    (:signature d)
    (assoc "signature" (:signature d))

    (:doc d)
    (assoc "doc" (:doc d))))

(defn- import->wire
  "One `index/file-index` import row → snake_case wire map. `source` (the raw
   import statement / module) and its `anchor` are always present; `alias` /
   `items` / `wildcard` ride along only when the grammar parsed that detail."
  [imp]
  (cond-> {"source" (:source imp) "anchor" (:anchor imp)}
    (:alias imp)
    (assoc "alias" (:alias imp))

    (seq (:items imp))
    (assoc "items" (vec (:items imp)))

    (:wildcard imp)
    (assoc "wildcard" true)))

(defn- index-tool
  "Structural INDEX of a source file — a Maki-style, line-ranged skeleton via
   tree-sitter. Read this BEFORE cat: a `<file> · <language> · <N> lines` header,
   an anchored `imports:` section (the file's require/import deps), then every
   definition, nested by structure, one per line:
     <kind> [private] <name>  <signature>  @<start-anchor>..<end-anchor>
   with the first line of its doc/comment gist on an indented line below (when
   present) — for JS/TS/TSX that's the leading `//`/JSDoc comment above the def.
   So you get kind (function/constant/…), a `private` marker only when the def is
   private (public is the default and stays implicit), the exact name, the
   arglist, the doc/comment gist, and the full start..end span — WITHOUT reading
   the body. To read ONE definition's SOURCE, `cat` exactly its span — pass the
   row's anchors: cat(path, {\"anchor\": [<start-anchor>, <end-anchor>]}) — instead
   of the whole file. The <name> is the VERBATIM `struct_patch` target: copy it
   as-is (it is already clean — no `^:private`/type-hint noise), and pair it with
   <kind> when two defs
   share a name.
     await struct_index(path)
   Returns {\"skeleton\": \"...\", \"definitions\": [{\"name\",\"kind\",\"visibility\",
   \"signature\",\"doc\",\"anchor\",\"end_anchor\",\"depth\"} …], \"imports\":
   [{\"source\",\"alias\",\"items\",\"wildcard\",\"anchor\"} …], \"language\": \"...\",
   \"line_count\": N}. `definitions` is the machine-addressable data — each row the
   SAME shape as an `struct_occurrences` definition (read `anchor`/`end_anchor` as fields,
   no parsing the skeleton blob). When a language has no structural index yet,
   returns a note — fall back to cat(path)."
  [& args]
  ;; Accept struct_index("x") (positional) AND struct_index({"path":"x"}) — the native
  ;; tool-call path synthesizes the dict form.
  (let
    [a
     (first args)

     path
     (cond (string? a) a
           (map? a) (get a "path")
           :else a)

     ;; Resolve through safe-path (workspace-cwd confinement) like every other file
     ;; tool — file-index's internal (slurp path) must NOT see a raw relative
     ;; path (that resolves against the JVM user.dir, not the workspace root, so a
     ;; nested `src/foo.clj` 404s while cat finds it).
     f
     (ensure-existing-file! (safe-path path))

     abs
     (.getPath f)

     idx
     (index/file-index abs (slurp f))

     language
     (index/detect-language abs)]

    (tool-success
      {:op :struct_index
       :path path
       :kind :file
       :result (cond idx {"skeleton" (:skeleton idx)
                          "definitions" (mapv def->wire (:definitions idx))
                          "imports" (mapv import->wire (:imports idx))
                          "language" (:language idx)
                          "line_count" (:line-count idx)
                          "path" path}
                     language {"language" language
                               "path" path
                               "note" "No structural index for this language yet — use cat(path)."}
                     :else {"path" path "note" "Unknown language — use cat(path)."})})))

;; -----------------------------------------------------------------------------
;; Native-tool result renderers — `(result → markdown)`. The loop applies these
;; so a native tool's result shows as a clean card in BOTH the TUI and the web
;; (unified), surfacing only what matters — never the raw args+result dump. Tools
;; without a renderer fall back to a pretty-printed result (see the loop).
;;
;; CONTRACT: a renderer receives the tool's UNWRAPPED result value (the inner
;; `:result` the tool returns, e.g. cat's `{"path" ... "anchors" ...}`). The
;; strings-only boundary means keys are VERBATIM STRINGS in snake_case — both the
;; top-level result keys (`"exists"`, `"next_offset"`) AND nested DATA keys (cat's
;; anchor keys are `"1:5ad"`, never `:1:5ad`). Read string keys throughout; the
;; renderer still RETURNS the internal keyword-keyed `{:summary :body}` IR.
;; -----------------------------------------------------------------------------

(defn- anchor-line-spans
  "Sorted contiguous line-number runs from cat anchor keys —
   `[[1 60] [370 405] …]`. nil when any key fails to parse, so the
   caller can fall back to a count-only summary instead of lying."
  [anchors]
  (let
    [nums (mapv (fn [k]
                  (parse-long (first (str/split (str k) #":"))))
                (keys anchors))]
    (when (and (seq nums) (every? some? nums))
      (reduce (fn [acc ^long x]
                (let [[a ^long b] (peek acc)]
                  (if (and b (= x (inc b))) (conj (pop acc) [a x]) (conj acc [x x]))))
              []
              (sort nums)))))

(defn- disp-path
  "A file path shown on an op-card headline: workspace-relative paths pass
   through unchanged, but an out-of-workspace ABSOLUTE path under $HOME is
   abbreviated to `~/…` (matching the footer/navigator) so a cat/patch/… on a
   sibling repo reads `~/other-repo/x.clj` instead of a long absolute path.
   Nil-safe."
  [p]
  (paths/abbreviate-home p))

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
  (let
    [line-no
     (fn [k]
       (first (str/split (str k) #":")))

     anchors
     (get r "anchors")

     ;; Gutter width = widest line number in THIS slice, not a fixed 5.
     ;; A hardcoded `%5s` padded 3-digit reads (`380`) with two spurious
     ;; leading spaces that read as a broken left margin on the cat card.
     gutter-w
     (reduce (fn [^long w [k _]]
               (max w (count (line-no k))))
             1
             anchors)

     ;; Gap marker between NON-CONTIGUOUS slices (multi-range / multi-anchor
     ;; reads) so disjoint areas read as separate regions instead of one run.
     ;; `⋯` is the project's canonical "content omitted here" glyph (see the
     ;; `# ⋯ folded`/`# ⋯ clipped` breadcrumbs in loop.clj); right-align it in
     ;; the line-number gutter so it sits exactly where the skipped lines were.
     divider
     (format (str "%" gutter-w "s") "⋯")

     rows
     (:rows
       (reduce
         (fn [{:keys [rows prev]} [k v]]
           (let
             [ln
              (parse-long (line-no k))

              row
              (str (format (str "%" gutter-w "s") (line-no k)) "  " (patch/anchor-value-text v))]

             {:prev ln
              :rows (cond-> rows
                      (and prev ln (> (long ln) (inc (long prev))))
                      (conj divider)

                      :always
                      (conj row))}))
         {:rows [] :prev nil}
         (sort-by (comp parse-long line-no key) anchors)))

     n
     (count anchors)

     spans
     (anchor-line-spans (get r "anchors"))

     span-str
     (fn [[a b]]
       (if (= a b) (str "L" a) (str "L" a "-" b)))

     loc
     (cond (nil? spans) nil
           (= 1 (count spans)) (span-str (first spans))
           :else (str "L" (ffirst spans) "-" (second (peek spans)) " (" (count spans) " ranges)"))

     counted
     (str n " line" (when (not= 1 n) "s"))]

    {:summary (str "`" (disp-path (get r "path"))
                   "` · " (cond (nil? loc) counted
                                (= 1 (count spans)) loc
                                :else (str loc " · " counted)))
     :body (when (seq rows) (str "\n```\n" (str/join "\n" rows) "\n```"))}))

(defn- render-exists-result
  "file_exists → `{:summary}` only (no body): the path + presence mark. `r` is
   `{\"path\" \"exists\"}`."
  [r]
  {:summary (str "`" (disp-path (get r "path")) "` " (if (get r "exists") "exists ✓" "missing ✗"))})

(defn- kw->str
  "Coerce a result map KEY/VALUE to its string form. The strings-only boundary
   already hands renderers plain strings (paths, anchors), so this is now just
   `str`; kept as a named helper so the render call sites stay readable."
  [k]
  (str k))

(defn- md-inline-code
  "CommonMark-safe inline code span for `s`. A naive `` `s` `` breaks when `s`
   itself contains backticks (e.g. a regex rg needle like `` \\`` ``): the inner
   backtick closes the span early, corrupting every following span on the line
   (the `parse-inlineORinline` chip-glue bug). Pick a fence one longer than the
   longest backtick run in `s`, and pad with a space when `s` starts/ends with a
   backtick (CommonMark strips a single symmetric leading+trailing space), so the
   term renders as ONE clean chip."
  [s]
  (let
    [s
     (str s)

     longest
     (transduce (map count) max 0 (re-seq #"`+" s))

     fence
     (apply str (repeat (inc (long longest)) \`))

     pad
     (if (or (str/starts-with? s "`") (str/ends-with? s "`")) " " "")]

    (str fence pad s pad fence)))

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

(defn- rg-needle-re
  "Literal regex fragment for ONE OR `needle`, honoring rg smart-case: an
   all-lowercase needle matches any case (scoped inside `(?i:…)`) so its
   case-insensitivity can't leak into a sibling alternative; one carrying an
   uppercase letter stays exact. `Pattern/quote` makes metacharacters literal."
  [needle]
  (let [quoted (java.util.regex.Pattern/quote needle)]
    (if (re-find #"[A-Z]" needle) quoted (str "(?i:" quoted ")"))))

(defn- highlight-needles
  "Wrap every occurrence of any OR `needle` in `text` with reverse-video SGR
   (\u001B[7m … \u001B[0m) so BOTH channels paint the matched search term: the
   TUI's `paint-ansi-line!` maps the code to a fg, the web's `ansi->hiccup`
   turns it into a `.rg-hit` span (client `colorizeAnsi` re-applies it after the
   `marked` re-render). One non-overlapping pass, longest needle first, so a
   short needle can't re-wrap a longer one's match."
  [needles ^String text]
  (if (or (not (seq needles)) (str/blank? text))
    text
    (let
      [frags (into []
                   (keep (fn [n]
                           (when (seq n) (try (rg-needle-re n) (catch Exception _ nil)))))
                   (sort-by #(- (count %)) needles))]
      (if (seq frags)
        (str/replace text
                     (re-pattern (str/join "|" frags))
                     (fn [m]
                       (str "\u001B[7m" m "\u001B[0m")))
        text))))

(defn- rg-gutter-width
  "Widest line-number column across a file's hits AND their context lines, so
   every gutter row in the block right-aligns. Mixed 1- and 4-digit line
   numbers otherwise stagger the text column (the same cat-card margin bug)."
  [hits]
  (reduce (fn [^long w [k v]]
            (let [ctx (when (map? v) (concat (keys (get v "before")) (keys (get v "after"))))]
              (reduce (fn [^long w2 a]
                        (max w2 (count (rg-anchor-lineno a))))
                      (max w (count (rg-anchor-lineno k)))
                      ctx)))
          1
          hits))

(defn- rg-row
  "One `  <lineno>  <text>` gutter row for a match or a context line, the line
   number right-aligned to `width` (the file's widest). Any OR `needle`
   occurrence in the text is wrapped for highlight (see `highlight-needles`)."
  [needles width k txt]
  (str "  " (format (str "%" width "s") (rg-anchor-lineno k))
       "  " (str/trimr (highlight-needles needles (str txt)))))

(defn- rg-hit-rows
  "Rows for ONE match anchor `k` → value `v`. `v` is a `{:text <match> :before
   {anchor→{\"text\"}} :after {anchor→{\"text\"}}}` map (before/after only with a context
   window) — render the before-context, then the matched line, then the
   after-context, each as a line-numbered gutter row (sorted by line number),
   the gutter right-aligned to `width`. A bare string `v` is tolerated and
   rendered as the lone matched line."
  [needles width k v]
  (if (map? v)
    (let
      [ctx-rows (fn [m]
                  (map (fn [[ck cv]]
                         (rg-row needles width ck (patch/anchor-value-text cv)))
                       (sort-by (comp rg-anchor-lineno-long key) m)))]
      (concat (ctx-rows (get v "before"))
              [(rg-row needles width k (get v "text"))]
              (ctx-rows (get v "after"))))
    [(rg-row needles width k v)]))

(defn- render-rg-result
  "rg → a `{:summary :body}` card for whichever MODE ran — each carries a DIFFERENT
   result shape, so a single content-only renderer (the old bug) showed
   `0 hits in N files` with no body for files-only:

     - content    `:matches` {path {anchor VALUE}} + `:hit_count`/`:file_count`
                  → `N hits in M files`, per-file matching lines.
     - files-only `:files [path…]` + `:file_count` (no per-line hits — that IS the
                  mode) → `M files`, the matching paths listed.

   Paths/anchors are keywords; a content VALUE is a `{:text :before :after}` map
   — before/after only when a context window ran (see `rg-hit-rows`)."
  [r]
  (let
    [fc
     (or (get r "file_count") 0)

     files-word
     (str fc " file" (when (not= 1 fc) "s"))

     ;; NAME what was searched on the headline — a bare "N hits in M files"
     ;; is useless without the term(s). Each OR needle is backtick-quoted
     ;; (the same chip style paths use); multiple terms join with " OR ".
     needles
     (seq (get r "needles"))

     query-chip
     (when needles (str/join " OR " (map md-inline-code needles)))

     ;; NAME the search SCOPE too — WHERE rg looked. The implicit `["."]`
     ;; default (the whole workspace) stays off the headline; an explicit
     ;; `paths` scope is chipped so the card says which dir(s) were swept.
     scope
     (seq (remove #(= "." (kw->str %)) (get r "paths")))

     scope-chip
     (when scope (str "in " (str/join ", " (map #(md-inline-code (disp-path (kw->str %))) scope))))

     with-query
     (fn [tail]
       (str (when query-chip (str query-chip " · "))
            tail
            (when scope-chip (str " · " scope-chip))))]

    (cond
      ;; files-only — the matching FILES are the result; there are no per-line hits.
      ;; "N matching files" NAMES the mode: content-grep collapsed to paths, so it
      ;; never reads as a name-search (`find_files`) or an empty `0 hits` content run.
      (contains? r "files")
      {:summary (with-query (str fc " matching file" (when (not= 1 fc) "s")))
       :body (when-let [files (seq (get r "files"))]
               (str "\n```\n"
                    (str/join "\n" (map #(highlight-needles needles (kw->str %)) files))
                    "\n```"))}
      ;; content (default) — per-line hits grouped by file.
      :else (let
              [hc
               (or (get r "hit_count") 0)

               files
               (for [[path hits] (get r "matches")]
                 (let [width (rg-gutter-width hits)]
                   (str (md-inline-code (disp-path (kw->str path)))
                        "\n\n```\n"
                        (str/join "\n"
                                  (mapcat (fn [[k v]]
                                            (rg-hit-rows needles width k v))
                                          (sort-by (comp rg-anchor-lineno-long key) hits)))
                        "\n```")))]

              {:summary (with-query (str hc " hit" (when (not= 1 hc) "s") " in " files-word))
               :body (when (seq files) (str "\n" (str/join "\n\n" files)))}))))

(defn- render-patch-result
  "patch → `{:summary :body}`: the summary NAMES each file with its op
   (`update `path` · add `path`` …) so a single-file patch and a multi-file
   patch read the SAME way; only a large fan-out collapses to `first two +N
   more`. The body is the unified diff(s); for a SINGLE file it omits the
   per-file `op `path`` header (the summary already states it) and for
   MULTI-file it prefixes each diff with `op `path`` to disambiguate. `r` is a
   vector of per-file summaries `[{:path :op :changed :diff}]`."
  [r]
  (let
    [summaries
     (if (sequential? r) r [r])

     changed
     (filterv #(get % "changed") summaries)

     n
     (count summaries)

     file-label
     (fn [{:strs [path op changed]}]
       (str (if changed (str (or op "update") " ") "(no change) ") "`" (disp-path path) "`"))

     labels
     (mapv file-label summaries)]

    {:summary (if (<= n 3)
                (str/join " · " labels)
                (str (str/join " · " (take 2 labels))
                     " · +"
                     (- n 2)
                     " more ("
                     (count changed)
                     "/"
                     n
                     " changed)"))
     :body (some->> (str/join "\n\n"
                              (for [{:strs [path op changed diff]} summaries]
                                (let
                                  [diff-block (when (and changed (seq (str diff)))
                                                (str "```diff\n" diff "\n```"))]
                                  (if (= n 1)
                                    ;; single file: summary already names it — show just the diff
                                    (or diff-block "")
                                    (str (if changed (str (or op "update") " ") "(no change) ")
                                         "`" (disp-path path)
                                         "`" (when diff-block (str "\n" diff-block)))))))
                    not-empty
                    ;; leading blank = op-card BREATHE spacer (see tool-card-entries head-gap?)
                    (str "\n"))}))

(defn- render-find-result
  "find → `{:summary :body}`: match-count summary + the ranked paths body. `r` is
   `{:paths [path…] :item_count :query}`.

   Even a SINGLE match keeps the path in the body instead of riding the headline:
   a lone path can be long (`resources/META-INF/.../resource-config.json`) and the
   shared `result-card` only renders a collapsible disclosure when a body exists.
   Keeping all ranked paths in `:body` lets the TUI/Web show a compact collapsed
   FIND_FILES row by default, with the path(s) available on expansion. 0 results
   keep the steer/hint body so the user sees WHY nothing matched."
  [r]
  (let
    [n
     (or (get r "item_count") (count (get r "paths")) 0)

     q
     (some-> (get r "query")
             str
             not-empty)

     hint
     (some-> (get r "hint")
             kw->str
             not-empty)

     paths
     (get r "paths")

     ;; When the strict whole-query pass found nothing, find_files fell back
     ;; to per-TERM matching (see `find-search`). Name the terms that landed
     ;; so a fuzzy result reads honestly — `3 matches for "…" · terms: render,
     ;; native` — instead of implying an exact whole-query hit.
     terms
     (when (get r "fuzzy")
       (seq (keep #(some-> %
                           kw->str
                           not-empty)
                  (get r "matched_terms"))))

     ;; NAME the search SCOPE too — WHERE find_files looked. The implicit
     ;; `["."]` default (the whole workspace) stays off the headline; an
     ;; explicit `paths` scope is chipped so the card says which dir(s).
     scope
     (seq (remove #(= "." (kw->str %)) (get r "searched_paths")))

     scope-chip
     (when scope (str "in " (str/join ", " (map #(str "`" (kw->str %) "`") scope))))

     head
     (str n
          " match"
          (when (not= 1 n) "es")
          (when q (str " for \"" q "\""))
          (when terms (str " · terms: " (str/join ", " terms)))
          (when scope-chip (str " · " scope-chip)))]

    {:summary head
     :body (cond (seq paths)
                 (str "\n```\n" (str/join "\n" (map (comp disp-path kw->str) paths)) "\n```")
                 ;; 0 results: show the steer (filename-vs-content) instead of a
                 ;; blank card — the same hint the model reads, so the user sees
                 ;; WHY it found nothing.
                 hint (str "\n" hint))}))

(defn- idx-cell
  "One-line, pipe-escaped, length-capped text for a GFM table cell (the TUI
   table painter draws cells as plain text, so no inline markdown here)."
  [s max-len]
  (let
    [s (-> (str s)
           (str/replace #"\s+" " ")
           str/trim
           (str/replace "|" "\\|"))]
    (if (> (count s) (long max-len)) (str (subs s 0 (max 0 (dec (long max-len)))) "…") s)))

(defn- render-index-result
  "struct_index → `{:summary :body}`: a path headline (defs · language · lines)
   over a GFM TABLE of every definition — nesting shown by a `·` indent, the row
   columns Def (name) · Arity (signature) · Kind (visibility+kind) · Anchor (span
   anchors) · Doc (gist). `r` is the wire result `{:skeleton :definitions :imports
   :language :line_count :path}`. With no definitions it falls back to the raw
   anchored skeleton fence (imports-only files), and to a bare no-structure
   summary when nothing was indexed."
  [r]
  (let
    [loc
     (some-> (get r "path")
             disp-path
             not-empty
             (#(str "`" % "`")))

     defs
     (get r "definitions")

     lang
     (some-> (get r "language")
             kw->str
             not-empty)

     lc
     (get r "line_count")

     n
     (count defs)]

    (if (seq defs)
      {:summary (str (or loc "struct_index")
                     " · "
                     n
                     " def"
                     (when (not= 1 n) "s")
                     (when lang (str " · " lang))
                     (when lc (str " · " lc " line" (when (not= 1 (long lc)) "s"))))
       :body (let
               [header
                ["| Def | Arity | Kind | Anchor | Doc |" "|-----|-------|------|--------|-----|"]

                rows
                (for [d defs]
                  (let
                    [depth (long (or (get d "depth") 0))
                     nm (str (apply str (repeat depth "· ")) (kw->str (get d "name")))
                     sig (some-> (get d "signature")
                                 kw->str
                                 not-empty)
                     vis (some-> (get d "visibility")
                                 kw->str
                                 not-empty)
                     kind (kw->str (get d "kind"))
                     kindc (if (and vis (not= vis "public")) (str vis " " kind) kind)
                     span (str (kw->str (get d "anchor")) ".." (kw->str (get d "end_anchor")))]

                    (str "| "
                         (idx-cell nm 40)
                         " | "
                         (idx-cell (or sig "—") 22)
                         " | "
                         (idx-cell kindc 16)
                         " | "
                         (idx-cell span 20)
                         " | "
                         (idx-cell (or (get d "doc") "—") 60)
                         " |")))]

               (str "\n" (str/join "\n" (concat header rows))))}
      (if-let
        [sk (some-> (get r "skeleton")
                    kw->str
                    not-empty)]
        {:summary (or loc "struct_index") :body (str "\n```\n" sk "\n```")}
        {:summary (str (or loc "struct_index") " · no structural index")}))))

(defn- render-occurrences-result
  "struct_occurrences → `{:summary :body}`: a `N · K defs in M files of `name` · <scope>`
   headline (no leading 'struct_occurrences' word — the op-card badge already names it);
   <scope> is `project-wide` for the default/whole-project scan, else `in <paths>`,
   then per file the DEFINITION(s) (kind/visibility/signature + span anchors) on their
   own lines and the use lines (derived from each use's anchor) compacted. `r` is
   wire-shaped: `{:name :files [{:path :occurrences [{:anchor :is_definition :kind
   :visibility :signature :end_anchor}]}] :count :definition_count}`."
  [r]
  (let
    [files
     (get r "files")

     total
     (or (get r "count") 0)

     defs
     (or (get r "definition_count") 0)

     fc
     (count files)

     nm
     (some-> (get r "name")
             kw->str)

     paths
     (mapv kw->str (get r "paths"))

     scope
     (cond (or (empty? paths) (= paths ["."])) "project-wide"
           :else (str "in " (str/join ", " paths)))]

    {:summary (str total
                   (when (pos? (long defs)) (str " · " defs " def" (when (not= 1 defs) "s")))
                   " in "
                   fc
                   " file"
                   (when (not= 1 fc) "s")
                   (when nm (str " of `" nm "`"))
                   (when scope (str " · " scope)))
     :body (when (seq files)
             (str "\n"
                  (str/join "\n\n"
                            (for [f files]
                              (let
                                [occ (get f "occurrences")
                                 ds (filter #(get % "is_definition") occ)
                                 us (remove #(get % "is_definition") occ)]

                                (str "`"
                                     (disp-path (kw->str (get f "path")))
                                     "`\n```\n"
                                     (str/join "\n"
                                               (concat (for [d ds]
                                                         (str "  def "
                                                              (some-> (get d "kind")
                                                                      kw->str)
                                                              (when-let [v (get d "visibility")]
                                                                (str " " (kw->str v)))
                                                              (when-let [s (get d "signature")]
                                                                (str "  " (kw->str s)))
                                                              "  @" (kw->str (get d "anchor"))
                                                              ".." (kw->str (get d "end_anchor"))))
                                                       (when (seq us)
                                                         [(str "  used: "
                                                               (str/join ", "
                                                                         (map #(rg-anchor-lineno
                                                                                 (get % "anchor"))
                                                                              us)))])))
                                     "\n```"))))))}))

(defn- render-symbol-rename-result
  "struct_rename → `{:summary :body}`: `renamed in N files` (+ any failures), then
   the changed paths. `r` is `{:files [{:path :changed}] :file_count :failed}`."
  [r]
  (let
    [files
     (get r "files")

     fc
     (or (get r "file_count") (count files))

     failed
     (get r "failed")]

    {:summary (str "renamed in "
                   fc
                   " file"
                   (when (not= 1 fc) "s")
                   (when (seq failed) (str " · " (count failed) " failed")))
     :body (when (seq files)
             (str "\n```\n"
                  (str/join "\n" (map #(str "  " (disp-path (kw->str (get % "path")))) files))
                  "\n```"))}))

(defn- render-move-result
  "move → `{:summary}` only: `moved `src` → `dest``. `r` is `{:src :dest}`."
  [r]
  {:summary (str "moved `"
                 (disp-path (kw->str (get r "src")))
                 "` → `"
                 (disp-path (kw->str (get r "dest")))
                 "`")})

(defn- render-delete-result
  "delete → `{:summary}` only: `deleted `path`` (or a no-op note). `r` is
   `{:path :deleted}`."
  [r]
  {:summary (str (if (false? (get r "deleted")) "nothing to delete at `" "deleted `")
                 (disp-path (kw->str (get r "path")))
                 "`")})

(defn- render-copy-result
  "copy → `{:summary}` only: `copied `src` → `dest``. `r` is `{:src :dest :path}`."
  [r]
  {:summary (str "copied `"
                 (disp-path (kw->str (get r "src")))
                 "` → `"
                 (disp-path (kw->str (get r "dest")))
                 "`")})

(defn- render-create-dirs-result
  "create_dirs → `{:summary}` only: created / already-existed note. `r` is
   `{:path :created :already_existed}`."
  [r]
  {:summary (str (if (get r "created") "created dir `" "dir already exists `")
                 (disp-path (kw->str (get r "path")))
                 "`")})

(defn- render-sexpr-result
  "struct_node → `{:summary :body}`: a `<kind> @line..end_line` headline + the node's
   text as a code block. `r` is the zip shape `{:path :kind :line :end_line :text
   :children :can}`."
  [r]
  (let
    [kind
     (some-> (get r "kind")
             kw->str)

     line
     (get r "line")

     eol
     (get r "end_line")

     txt
     (some-> (get r "text")
             kw->str)]

    {:summary (str (or kind "node") (when line (str " @" line (when eol (str ".." eol)))))
     :body (when (seq txt) (str "\n```\n" txt "\n```"))}))

;; -----------------------------------------------------------------------------
;; Conditional advertising — the tree-sitter structural editors are only useful
;; when the project actually contains code in a supported language. Gate them on
;; the (cached) project language scan so a docs/config/unsupported-language repo
;; isn't handed tools it can't use.
;; -----------------------------------------------------------------------------

(def ^:private structural-scan-languages
  "The `environment/languages` SCAN vocabulary names whose files tree-sitter can
   structurally edit. Mostly == `index/code-languages`, but the SCAN names a few
   things differently — notably it rolls `sh`/`bash`/`zsh`/`fish` into `shell`,
   while tree-sitter calls it `bash` — so this is the reconciled set, NOT just
   `code-languages`. (Languages the scan doesn't recognize at all — e.g. `.elm`,
   `.jl` — simply don't appear, and `structural-supported?` fails OPEN on them.)"
  (conj index/code-languages "shell"))

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
  (try (let [langs (get-in (environment/snapshot) [:languages :languages])]
         (if (seq langs)
           (boolean (some (fn [l]
                            (contains? structural-scan-languages
                                       (some-> (:language l)
                                               str
                                               str/lower-case)))
                          langs))
           true)) ;; nothing recognized → fail OPEN
       (catch Throwable _ true)))    ;; any failure → fail OPEN

(def index-symbol
  (vis/symbol
    #'index-tool
    {:symbol 'struct_index
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Inspect supported source structurally before reading bodies. Returns imports plus a "
          "nested definition skeleton with signatures, doc gists, and fresh start/end anchors; "
          "use those anchors to read one definition or its name/kind with `struct_patch`.")
     :render render-index-result
     :color-role :tool-color/read
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Path to a source file."}}
              :required ["path"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :struct_index :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :struct_index :file nil)}))

(def cat-symbol
  (vis/symbol
    #'cat-tool
    {:symbol 'cat
     :native-tool? true
     ;; cat(path, {opts}) — path positional, the rest a Python options dict.
     :call {:pos ["path"] :rest :opt}
     :description
     (str "Read one sufficient file region as patch-ready `lineno:hash` anchored lines. "
          "For supported code, use `struct_index` first; every write invalidates returned anchors.")
     :render render-cat-result
     :color-role :tool-color/read
     :schema
     {:type "object"
      :properties
      {"path" {:type "string"
               :description "File path (relative to a filesystem root or absolute under one)."}
       "range" {:type "array"
                :items {:type "integer" :minimum 1}
                :minItems 2
                :maxItems 2
                :description "Optional [start,end] line range (1-based, inclusive)."}
       "ranges" {:type "array"
                 :items {:type "array" :items {:type "integer" :minimum 1} :minItems 2 :maxItems 2}
                 :minItems 1
                 :description "Optional several [[s,e],…] windows in one call."}
       "anchor"
       {:oneOf [{:type "string"} {:type "array" :items {:type "string"} :minItems 2 :maxItems 2}]
        :description
        "Optional lineno:hash anchor — a string for ONE line, or [from,to] for an inclusive span."}
       "tail"
       {:type "integer" :minimum 1 :description "Optional: read the last N lines (omit N → 2000)."}}
      :required ["path"]
      :additionalProperties false
      :maxProperties 2}
     :before-fn (path-protected-before-fn :cat :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :cat :file nil)}))

(def find-symbol
  (vis/symbol
    #'find-tool
    {:symbol 'find_files
     :native-tool? true
     :description
     (str
       "Fuzzy file/path-name discovery for vague names, concepts, or unfamiliar modules. "
       "Searches paths, not content; use `rg` for exact text, then inspect the best-ranked file.")
     :render render-find-result
     :color-role :tool-color/search
     :schema
     {:type "object"
      :properties
      {"query" {:type "string"
                :description
                "Fuzzy query — a name, concept, or partial path (matches the whole relative path)."}
       "paths"
       {:type "array" :items {:type "string"} :description "Restrict the search to these paths."}
       "is_hidden" {:type "boolean"
                    :description "Also match dotfiles / hidden dirs (default false)."}
       "is_respect_gitignore"
       {:type "boolean"
        :description
        "Honor .gitignore (default true). Set FALSE to discover files inside gitignored dirs (e.g. vendored / corporate repos the project ignores)."}}
      :required ["query"]
      :additionalProperties false}
     :before-fn (path-protected-before-fn :find_files :dir :read find-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :find_files :dir nil)}))

(def rg-symbol
  (vis/symbol
    #'rg-tool
    {:symbol 'rg
     :native-tool? true
     :description
     (str "Smart-case literal content search; use `find_files` for path names. Multiple terms are "
          "OR, not AND; filter complex matches in `python_execution`. After one no-hit, widen once "
          "to a real stem or inspect a likely file instead of guessing synonyms.")
     :render render-rg-result
     :color-role :tool-color/search
     :schema
     {:type "object"
      :properties
      {"query"
       {:oneOf [{:type "string" :minLength 1}
                {:type "array" :items {:type "string" :minLength 1} :minItems 1}]
        :description
        "Term or terms to find as smart-case literal OR matches; comma-separated strings are split into terms."}
       "paths" {:type "array"
                :items {:type "string"}
                :description "Restrict to these paths (default the whole tree)."}
       "include" {:oneOf [{:type "array" :items {:type "string"}} {:type "string"}]
                  :description
                  "Only files matching these globs, e.g. [\"**/*.clj\"] or \"**/*.clj\"."}
       "context" {:type "integer" :description "Lines of context around each match."}
       "is_files_only" {:type "boolean"
                        :description
                        "Return just the distinct files that contain a match, no per-line hits."}
       "is_hidden" {:type "boolean"
                    :description "Also search dotfiles / hidden dirs (default false)."}
       "is_respect_gitignore"
       {:type "boolean"
        :description
        "Honor .gitignore (default true). Set FALSE to search inside gitignored dirs (e.g. vendored / corporate repos the project ignores)."}}
      :required ["query"]
      :additionalProperties false}
     :before-fn (path-protected-before-fn :rg :dir :read rg-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :rg :dir nil)}))

(def patch-symbol
  (vis/symbol
    #'patch-tool
    {:symbol 'patch
     :native-tool? true
     :call (fn [input]
             {:args [(get input "edits")]})
     :description
     (str "Surgically edit text or unsupported code using fresh anchors from `cat`, `rg`, or "
          "`struct_index`. The batch is atomic and every write stales all anchors. On failure, "
          "follow its cause, refresh the target and anchor, retry once, then reassess; prefer "
          "`struct_patch` for supported code.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"edits"
       {:type "array"
        :minItems 1
        :description "Atomic anchor edits. Every item names its file, so one batch may span files."
        :items
        {:type "object"
         :properties
         {"path" {:type "string" :minLength 1 :description "File path."}
          "from_anchor" {:type "string" :minLength 1 :description "lineno:hash from a fresh read."}
          "to_anchor"
          {:type "string" :minLength 1 :description "Optional inclusive end anchor for a span."}
          "replace" {:type "string" :description "Replacement text; empty deletes."}}
         :required ["path" "from_anchor" "replace"]
         :additionalProperties false}}}
      :required ["edits"]
      :additionalProperties false}
     :before-fn (plan-gated-before-fn :patch :file :write patch-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :patch :file nil)}))

(def write-symbol
  ;; write reuses the patch channel renderer because its `:result`
  ;; shape is the same single-file summary (just always 1-file long).
  (vis/symbol
    #'write-tool
    {:symbol 'write
     :native-tool? true
     :description (str
                    "Create a new file or intentionally replace an entire clean file. Refuses "
                    "uncommitted targets unless explicitly allowed; use `patch` or `struct_patch` "
                    "for surgical changes.")
     :replay
     {:elide-args {"content" 8192} :retry-on #{:dirty} :retry-overrides {"allow_dirty" true}}
     :render render-patch-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties
              {"path" {:type "string" :description "File path to create or overwrite."}
               "content" {:type "string" :description "Full file content."}
               "is_overwrite"
               {:type "boolean"
                :description
                "Overwrite an existing file (default true); false = fail if it exists."}
               "allow_dirty" {:type "boolean"
                              :description "Allow writing a file with uncommitted git changes."}
               "expected_mtime" {:type "integer"
                                 :description
                                 "Staleness guard: only write if the file's mtime matches this."}}
              :required ["path" "content"]
              :additionalProperties false}
     :before-fn (plan-gated-before-fn :write :file :write write-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :write :file nil)}))

(def ^:private struct-op->kw
  "Bounded snake_case op string (as the model writes it) → the internal kebab
   keyword the `structural`/`zipper` edit fns dispatch on. A fixed allowlist so
   no keyword is ever MINTED from model-supplied data (strings-only boundary)."
  {"replace" :replace
   "delete" :delete
   "insert_before" :insert-before
   "insert_after" :insert-after
   "append" :append
   "add_doc" :add-doc
   "replace_doc" :replace-doc
   "replace_node" :replace-node
   "rename" :rename
   "move_before" :move-before
   "move_after" :move-after
   "append_child" :append-child
   "prepend_child" :prepend-child})

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
   ops (by PATH/`at`/node anchor): replace | replace_node (alias) | insert_before |
     insert_after | append_child | prepend_child (append/prepend = inside the node,
     after last / before first child; delete = replace with \"\"). `at` is the
     struct_node(path); `nav` adds relative moves — the full clojure.zip vocabulary:
     down|d|b up|u|t left|l right|r first last next|n prev|p {child:i}
     {find:\"text\"} {find_kind:\"if_statement\"}. Navigate with struct_node(...) first,
     then edit the same path here.
   Locate targets with struct_index(path) / struct_node(path) / struct_occurrences(name).
   Returns the [{\"path\", \"op\", \"changed\", \"diff\"}] shape as write."
  [& {:as args}]
  (let
    [path
     (get args "path")

     raw-op
     (or (struct-op->kw (or (get args "op") "replace"))
         (throw (ex-info (str "struct_patch: unknown op " (pr-str (get args "op")))
                         {:type :ext.foundation.editing/struct-unknown-op :op (get args "op")})))

     path-locator?
     (or (contains? args "at")
         ;; For moves, `anchor` is a definition NAME rather than a node handle.
         (and (contains? args "anchor") (not (#{:move-before :move-after} raw-op))))

     ;; STEERING — `append_child`/`prepend_child` are PATH-only (they add a form
     ;; INSIDE a located container node). Paired with a NAME `target` and no
     ;; `at`/node `anchor` they would fall through to the name-based op map and
     ;; throw a cryptic "Unknown structural op". Catch it up front with a fix.
     _
     (when (and (#{:append-child :prepend-child} raw-op) (not path-locator?))
       (throw (ex-info
                (str "struct_patch: "
                     (str/replace (name raw-op) "-" "_")
                     " is a PATH-based op — pass `at` or a node `anchor` to enter"
                     " the container node. To add a form into a def located by NAME,"
                     " use op `append` (name-based) instead.")
                {:type :ext.foundation.editing/struct-op-needs-path
                 :op (str/replace (name raw-op) "-" "_")
                 :hint
                 "append_child/prepend_child need `at`/`anchor`; use `append` for name-based"})))

     ;; LENIENCY — do the obvious thing instead of erroring:
     ;;  • `delete` (by name OR path) = replace the located node with "" (there was
     ;;    no name-based delete op, so a model wanting to drop a dead def was stuck).
     ;;  • `replace_node` with a PATH/anchor reuses the zipper's node-addressed
     ;;    `replace`; with a target but no match it is the name-based `replace`.
     delete?
     (= raw-op :delete)

     op
     (cond delete? :replace
           (and path-locator? (= raw-op :replace-node)) :replace
           (and (= raw-op :replace-node)
                (str/blank? (str (get args "match")))
                (not (str/blank? (str (get args "target")))))
           :replace
           :else raw-op)

     code
     (if delete? "" (get args "code"))

     new-content
     (if path-locator?
       ;; PATH-based (the zipper): locate by named-child index path + moves.
       (let
         [lang
          (or (zipper/detect-language path)
              (throw (ex-info (str "Unknown language for " path " — use patch(...).")
                              {:type :ext.foundation.editing/struct-unknown-language :path path})))

          source
          (slurp (safe-path path))

          base
          (if (contains? args "at")
            (get args "at")
            ;; `lineno:hash` anchor → the path of the node starting there
            ;; (staleness-guarded); `nav` then composes on top.
            (let [ra (zipper/path-at-anchor lang source (get args "anchor"))]
              (if (:ok? ra)
                (:path ra)
                (throw (ex-info (get-in ra [:error :message] "anchor did not resolve")
                                {:type :ext.foundation.editing/struct-anchor-error
                                 :reason (get-in ra [:error :reason])})))))

          nav
          (zipper/navigate lang source base (get args "nav"))

          at
          (if (:ok? nav)
            (:path nav)
            (throw (ex-info (get-in nav [:error :message] "navigation failed")
                            {:type :ext.foundation.editing/struct-nav-error
                             :reason (get-in nav [:error :reason])})))

          r
          (zipper/edit lang source at op code)]

         (if (:ok? r)
           (:new-source r)
           (throw (ex-info (get-in r [:error :message] "structural edit failed")
                           {:type :ext.foundation.editing/struct-zip-error
                            :reason (get-in r [:error :reason])
                            :at at}))))
       ;; NAME/MATCH-based (the original StructuralApi surface).
       (structural/edit-source path
                               (slurp (safe-path path))
                               {:op op
                                :target (get args "target")
                                :kind (get args "kind")
                                :code code
                                :match (get args "match")
                                :anchor (get args "anchor")}))

     ;; allow_dirty: a re-parsed structural edit is SAFE on a file with
     ;; uncommitted changes — the dirty-guard only blocks the raw `write`.
     result
     (write-safe {"path" path "content" new-content "allow_dirty" true})]

    (if (:success? result)
      (let
        [plan
         (:plan result)

         summary
         (patch-result-file-summary plan)]

        (tool-success {:op :struct_patch
                       :path (:path plan)
                       :kind :file
                       :result [summary]
                       :metadata {:mode :struct_patch
                                  :file-count 1
                                  :changed-count (if (get summary "changed") 1 0)
                                  :op (:op plan)
                                  :file-befores [(select-keys plan [:path :before])]}}))
      (extension/failure
        {:result nil
         :op :struct_patch
         :metadata {:target {:requested (str path) :resolved nil :absolute nil :kind :file}
                    :mode :struct_patch}
         :error {:message (:message result) :failures (:failures result) :mode :struct_patch}}))))

(def struct-patch-symbol
  (vis/symbol
    #'struct-patch-tool
    {:symbol 'struct_patch
     :native-tool? true
     :active-fn structural-supported?
     :description
     (str "Preferred syntax-safe editor for supported code. Locate a named definition from "
          "`struct_index` or a nested path from `struct_node`; the file is re-parsed and any "
          "syntax-breaking write is refused.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"path" {:type "string" :description "File to edit."}
       "op"
       {:type "string"
        :enum ["replace" "delete" "insert_before" "insert_after" "append" "add_doc" "replace_doc"
               "replace_node" "rename" "move_before" "move_after" "append_child" "prepend_child"]
        :description
        "Edit operation; name-based and path-based operations use only their applicable subset."}
       "target" {:type "string" :description "Definition NAME to locate (name-based ops)."}
       "code" {:type "string"
               :description "Replacement/insertion source (or the new name for rename)."}
       "kind" {:type "string"
               :description "function/class/method/… — disambiguates same-named defs."}
       "match" {:type "string"
                :description "For match-based replace_node: the unique sub-expr text to swap."}
       "anchor"
       {:type "string"
        :description
        "Dual use: for move_before/move_after the def NAME to relocate next to; otherwise a `lineno:hash` anchor (from struct_index/struct_occurrences/cat) that enters the zipper at the node starting on that line (compose with `nav`)."}
       "at"
       {:type "array"
        :items {:type "integer" :minimum 0}
        :description
        "Named-child index path from struct_node(path) (path-based ops). Or use `anchor` to enter by a lineno:hash row instead."}
       "nav" {:type "array"
              :description "Relative zipper moves applied after `at` (strings or maps)."}}
      :required ["path" "op"]
      :additionalProperties false}
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

(defn- zip-clip
  [s n]
  (if (and (string? s) (> (count s) (long n))) (str (subs s 0 (long n)) " …[clipped]") s))

(defn- zip-shape
  ;; `r` is zipper/inspect's internal (keyword) node data; this projects it onto
  ;; the model-facing struct_node result — string keys, no keyword values.
  [r]
  {"path" (:path r)
   "kind" (:kind r)
   "line" (:start-line r)
   "end_line" (:end-line r)
   "named_child_count" (:named-child-count r)
   "has_error" (:has-error? r)
   "text" (zip-clip (:text r) 2000)
   "sexp" (zip-clip (:sexp r) 1200)
   "children" (mapv (fn [c]
                      {"idx" (:idx c) "kind" (:kind c) "head" (zip-clip (:head c) 120)})
                    (:children r))})

(defn- sexpr-tool
  "The tree-sitter ZIPPER cursor (clojure.zip / rewrite-clj vocabulary, any
   language). A node's location is a PATH = list of NAMED-child indices from the
   file root, so the cursor round-trips through async tool calls.
     await struct_node(path)                                    # root + its named children
     await struct_node(path, {\"at\": [2, 0]})                    # jump to an absolute path
     await struct_node(path, {\"at\": [2], \"nav\": [\"down\", \"right\"]})  # cursor moves
     await struct_node(path, {\"nav\": [{\"find\": \"my_fn\"}]})            # jump to a node by text
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
  (let
    [lang
     (zipper/detect-language path)

     source
     (slurp (safe-path path))

     ;; anchor entry: a `lineno:hash` from a struct_index/struct_occurrences/cat row
     ;; resolves straight to the node's path, then `nav` composes on top.
     base
     (when-let [a (get opts "anchor")]
       (zipper/path-at-anchor lang source a))

     nav
     (if (and base (:error base))
       base
       (zipper/navigate lang source (if base (:path base) (get opts "at")) (get opts "nav")))]

    (if (:error nav)
      (extension/failure {:result nil
                          :op :struct_node
                          :metadata {:target {:requested (str path) :kind :file} :mode :struct_node}
                          :error {:message (get-in nav [:error :message])
                                  :reason (get-in nav [:error :reason])
                                  :mode :struct_node}})
      (let
        [at
         (:path nav)

         r
         (zipper/inspect lang source at)]

        (if (:error r)
          (extension/failure {:result nil
                              :op :struct_node
                              :metadata {:target {:requested (str path) :kind :file}
                                         :mode :struct_node}
                              :error {:message (get-in r [:error :message])
                                      :reason (get-in r [:error :reason])
                                      :mode :struct_node}})
          (tool-success {:op :struct_node
                         :path path
                         :kind :file
                         :result (assoc (zip-shape r)
                                   "can" (zipper/moves-available lang source at))}))))))

(def sexpr-symbol
  (vis/symbol
    #'sexpr-tool
    {:symbol 'struct_node
     :native-tool? true
     ;; struct_node(path, {opts}) — path positional, the rest a Python options dict.
     :call {:pos ["path"] :rest :opt}
     :active-fn structural-supported?
     :description
     (str "Inspect or navigate a nested tree-sitter node when a named definition is too coarse. "
          "Returns the node, its children, available moves, and a path accepted by `struct_patch`.")
     :render render-sexpr-result
     :color-role :tool-color/read
     :schema
     {:type "object"
      :properties
      {"path" {:type "string" :description "Source file to navigate."}
       "at" {:type "array"
             :items {:type "integer" :minimum 0}
             :description "Absolute named-child index path to jump to."}
       "nav" {:type "array"
              :description "Relative cursor moves (strings or {find/child/find_kind} maps)."}
       "anchor"
       {:type "string"
        :description
        "A `lineno:hash` anchor (from struct_index/struct_occurrences/cat) to enter the zipper at the node starting on that line — one hop from a listed row to its cursor; `nav` composes on top. Alternative to `at`."}}
      :required ["path"]
      :additionalProperties false}
     :before-fn (path-protected-before-fn :struct_node :file :read first-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :struct_node :file nil)}))

;; sexpr_edit was FOLDED INTO struct_patch — which now takes a zipper `at`/`nav`
;; path as an alternative to a `target` name. ONE structural editor (locate by
;; name OR by path), so the model isn't choosing between two near-identical
;; mutation verbs. `struct_node` stays as the read-only navigator that produces paths.

(defn- occurrence->wire
  "One `structural/occurrences` entry → snake_case wire map. Plain USE rows stay
   anchors-only (the `lineno:hash` is the sole position). DEFINITION rows mirror
   `struct_index` `definitions` rows where possible: `name`/`kind`/`visibility`/`signature`/
   `doc`/`anchor`/`end_anchor`, with nil metadata dropped."
  [name o]
  ;; Model-facing occurrence row — string keys, no keyword values.
  (let [base {"anchor" (:anchor o)}]
    (if-not (:is-definition o)
      base
      (cond->
        (assoc base
          "is_definition" true
          "name" name)
        (:kind o)
        (assoc "kind" (:kind o))

        (:visibility o)
        (assoc "visibility" (:visibility o))

        (:signature o)
        (assoc "signature" (:signature o))

        (:doc o)
        (assoc "doc" (:doc o))

        (:end-anchor o)
        (assoc "end_anchor" (:end-anchor o))))))

(defn- occurrences-tool
  "Every OCCURRENCE of an identifier across the project (or within `paths`), via
   tree-sitter — real identifier boundaries, never inside a bigger token / string
   / comment. The DEFINITION occurrences are MARKED with their kind, visibility,
   signature, doc, and full span; plain uses carry just location + a patch anchor:
     await struct_occurrences(\"handle_click\")            # whole project
     await struct_occurrences(\"foo\", paths=[\"src/api\"]) # scoped
   Result: {\"name\", \"files\": [{\"path\", \"occurrences\": [{\"anchor\"  # a use:
   the sole position, a `lineno:hash` patch handle
   , \"is_definition\": true, \"name\", \"kind\", \"visibility\", \"signature\", \"doc\",
   \"end_anchor\"  # a DEFINITION: named span = anchor..end_anchor, patch it directly
   }]}], \"count\", \"definition_count\", \"scanned\", \"failed\"}.
   ONE call answers both 'where is it defined' (filter is_definition in Python)
   AND 'where is it used'. Not truncated — loop/filter the structure in Python and
   print only what you need. Syntactic (no scope resolution): every same-named
   definition across the project is marked, so `definition_count` > 1 means the
   name is ambiguous — each carries its own path + signature to disambiguate."
  [& args]
  (let
    [[a & more]
     args

     spec
     (cond
       (and (= 1 (count args)) (string? a)) {"name" a}
       (and (= 1 (count args)) (map? a)) a
       (and (= 2 (count args)) (string? a) (map? (first more))) (assoc (first more) "name" a)
       :else
       (throw
         (ex-info
           "struct_occurrences takes struct_occurrences(name) or struct_occurrences(name, paths=[...])."
           {:type :ext.foundation.editing/invalid-occurrences-args :got args})))

     name
     (get spec "name")

     _
     (when-not (and (string? name) (not (str/blank? name)))
       (throw (ex-info "struct_occurrences needs a non-blank `name`."
                       {:type :ext.foundation.editing/invalid-occurrences-args :name name})))

     paths
     (let [p (or (get spec "paths") ["."])]
       (if (string? p) [p] (vec p)))

     ;; rg prefilters the files that mention the name (smart-case would over-
     ;; match casing, but a definition keeps the name's case, so a case-
     ;; sensitive identifier still lands among these files); each is parsed.
     files
     ;; struct_occurrences PARSES every matching file, so the rg prefilter must not
     ;; cap the candidate set at `default-grep-limit` (250) — a large dir /
     ;; project would silently drop every file past the 250th, contradicting
     ;; the "Not truncated" contract. Pass an unbounded limit.
     (vec (or (:files
                (rg-search
                  {"query" [name] "is_files_only" true "paths" paths "limit" Integer/MAX_VALUE}))
              []))

     ;; `per`/`failed` entries are the model-facing result payload — string keys.
     {:keys [per failed]}
     (reduce
       (fn [acc path]
         (try
           (let [occ (structural/occurrences path (slurp (safe-path path)) name)]
             (cond-> acc
               (seq occ)
               (update :per
                       conj
                       {"path" path "occurrences" (mapv #(occurrence->wire name %) occ)})))
           (catch Exception e
             (update acc :failed conj {"path" path "error" (or (ex-message e) (str (class e)))}))))
       {:per [] :failed []}
       files)

     total
     (reduce + 0 (map #(count (get % "occurrences")) per))

     defs
     (reduce +
             0
             (map (fn [f]
                    (count (filter #(get % "is_definition") (get f "occurrences"))))
                  per))]

    (tool-success {:op :struct_occurrences
                   :kind :dir
                   :result {"name" name
                            "files" per
                            "count" total
                            "definition_count" defs
                            "scanned" (count files)
                            "paths" paths
                            "failed" failed}
                   :metadata {:name name :paths paths :count total :definition_count defs}})))

(def occurrences-symbol
  (vis/symbol #'occurrences-tool
              {:symbol 'struct_occurrences
               :native-tool? true
               :active-fn structural-supported?
               :description
               (str "Trace a real identifier across supported project code before renaming or "
                    "assessing blast radius. Returns every syntactic use plus marked definitions "
                    "and patch-ready anchors; filter the complete result in `python_execution`.")
               :render render-occurrences-result
               :color-role :tool-color/search
               :schema {:type "object"
                        :properties {"name" {:type "string" :description "Identifier to trace."}
                                     "paths" {:type "array"
                                              :items {:type "string"}
                                              :description
                                              "Restrict to these paths (default whole project)."}}
                        :required ["name"]
                        :additionalProperties false}
               :tag :observation
               :on-error-fn (tool-failure-on-error :struct_occurrences :dir nil)}))

(defn- symbol-rename-tool
  "Rename identifier `name` → `new_name` across the WHOLE project via tree-sitter
   — at real identifier boundaries (never a string / comment / larger token),
   RE-PARSED per file so a syntax-breaking rename is refused. For a Clojure
   NAMESPACE this is the cross-file ns rename: it rewrites the `(ns …)` form, every
   `:require`/`:use` target, and qualified `old.ns/sym` usages, while leaving local
   `:as` aliases intact (then move the defining file with move(old, new)). Returns
   {\"files\": [{\"path\", \"changed\"}], \"file_count\", \"failed\": [{\"path\",
   \"error\"}]}.
     await struct_rename(\"foo.bar\", \"foo.baz\")     # ns or any symbol"
  [& args]
  (let
    [spec
     (cond (and (= 2 (count args)) (string? (first args)) (string? (second args)))
           {"name" (first args) "new_name" (second args)}
           (and (= 1 (count args)) (map? (first args))) (first args)
           :else (throw (ex-info "struct_rename takes struct_rename(name, new_name)."
                                 {:type :ext.foundation.editing/invalid-symbol-rename-args
                                  :got args})))

     name
     (get spec "name")

     new_name
     (get spec "new_name")

     _
     (when-not
       (and (string? name) (not (str/blank? name)) (string? new_name) (not (str/blank? new_name)))
       (throw (ex-info "struct_rename needs non-blank `name` and `new_name`."
                       {:type :ext.foundation.editing/invalid-symbol-rename-args :spec spec})))

     files
     (vec (or (:files (rg-search {"query" [name] "is_files_only" true})) []))

     out
     (reduce
       (fn [acc path]
         (try
           (let
             [src
              (slurp (safe-path path))

              hits
              (structural/references path src name)]

             (if (seq hits)
               (let
                 [renamed (structural/edit-source
                            path
                            src
                            {:op :rename :target name :kind nil :code new_name :match nil})]
                 (write-safe {"path" path "content" renamed "allow_dirty" true})
                 (update acc :changed conj path))
               acc))
           (catch Exception e
             (update acc :failed conj {"path" path "error" (or (ex-message e) (str (class e)))}))))
       {:changed [] :failed []}
       files)]

    (tool-success {:op :struct_rename
                   :kind :dir
                   ;; Model-facing result — string keys, no keyword values.
                   :result {"files" (mapv (fn [p]
                                            {"path" p "changed" true})
                                          (:changed out))
                            "file_count" (count (:changed out))
                            "failed" (:failed out)}})))

(def symbol-rename-symbol
  (vis/symbol
    #'symbol-rename-tool
    {:symbol 'struct_rename
     :native-tool? true
     :name "struct_rename"
     :call {:pos ["name" "new_name"]}
     :active-fn structural-supported?
     :description
     (str
       "Rename one identifier across supported project code at syntactic boundaries, with "
       "each changed file re-parsed. Run `struct_occurrences` first to confirm the blast radius; "
       "Clojure namespace renames still require moving the defining file.")
     :render render-symbol-rename-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"name" {:type "string" :description "Current identifier / namespace."}
                           "new_name" {:type "string" :description "New identifier / namespace."}}
              :required ["name" "new_name"]
              :additionalProperties false}
     :tag :mutation
     :on-error-fn (tool-failure-on-error :struct_rename :dir nil)}))

(def create-dirs-symbol
  (vis/symbol
    #'create-dirs-tool
    {:symbol 'create-dirs
     :native-tool? true
     :call {:pos ["path"]}
     :name "create_dirs"
     :description
     "Create a confined directory and any missing parents; reports whether anything changed."
     :render render-create-dirs-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Directory path to create."}}
              :required ["path"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :create-dirs :dir :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :create-dirs :dir nil)}))

(def copy-symbol
  (vis/symbol
    #'copy-tool
    {:symbol 'copy
     :native-tool? true
     ;; copy(src, dest, {opts}) — two positionals, the rest an options dict.
     :call {:pos ["src" "dest"] :rest :opt}
     :description
     "Copy a confined file or directory. Existing destinations fail unless overwrite is explicitly enabled."
     :render render-copy-result
     :color-role :tool-color/move
     :schema {:type "object"
              :properties {"src" {:type "string" :description "Source path."}
                           "dest" {:type "string" :description "Destination path."}
                           "is_overwrite" {:type "boolean"
                                           :description
                                           "Overwrite an existing dest (default false)."}}
              :required ["src" "dest"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :copy :path :write first-two-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
              {:symbol 'move
               :native-tool? true
               :call {:pos ["src" "dest"]}
               :description
               "Move or rename a confined file or directory without reconstructing its contents."
               :render render-move-result
               :color-role :tool-color/move
               :schema {:type "object"
                        :properties {"src" {:type "string" :description "Source path."}
                                     "dest" {:type "string" :description "Destination path."}}
                        :required ["src" "dest"]
                        :additionalProperties false}
               :before-fn (path-protected-before-fn :move :path :write first-two-arg-paths)
               :tag :mutation
               :on-error-fn (tool-failure-on-error :move :path nil)}))

(def delete-symbol
  (vis/symbol
    #'delete-tool
    {:symbol 'delete
     :native-tool? true
     :call {:pos ["path"] :rest :opt}
     :description
     "Destructively delete one confined file or directory only with explicit user intent; can safely no-op when the target is already absent."
     :render render-delete-result
     :color-role :tool-color/delete
     :schema {:type "object"
              :properties
              {"path" {:type "string" :description "Path to delete."}
               "is_missing_ok"
               {:type "boolean"
                :description
                "No-op instead of raising when the path does not exist (default false)."}}
              :required ["path"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :delete :path :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :delete :path nil)}))

(def delete-if-exists-symbol
  (vis/symbol
    #'delete-if-exists-tool
    {:symbol 'delete-if-exists
     :native-tool? false
     :call {:pos ["path"]}
     :name "delete_if_exists"
     :description
     "Delete a path if it exists, else no-op (never raises on a missing path). Returns {`path`, `deleted`: bool}."
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
               :description "Check whether a confined file or directory exists without reading it."
               :render render-exists-result
               :color-role :tool-color/read
               :schema {:type "object"
                        :properties {"path" {:type "string" :description "Path to check."}}
                        :required ["path"]
                        :additionalProperties false}
               :before-fn (path-protected-before-fn :file-exists :path :read first-arg-paths)
               :tag :observation
               :on-error-fn (tool-failure-on-error :file-exists :path nil)}))

(defn available-editing-symbols
  []
  [index-symbol cat-symbol find-symbol rg-symbol patch-symbol write-symbol struct-patch-symbol
   sexpr-symbol occurrences-symbol symbol-rename-symbol create-dirs-symbol copy-symbol move-symbol
   delete-symbol delete-if-exists-symbol file-exists-symbol])

(defn available-editing-prompt
  "No separate editing prompt: active native descriptions own routing and their
   JSON Schemas own inputs. Structural tools are already omitted by their
   activation gate when unsupported, so repeating that matrix would waste tokens."
  []
  "")

(def editing-symbols
  "Default editing symbol set for docs/tests. A `delay` so the language/env
   scan it triggers runs on first deref (tests, docs) and NEVER at namespace
   load — a load-time call reaches `git/run-git`'s `future`, which starts the
   agent thread-pool and native-image refuses that started thread in the image
   heap. Deref with `@editing-symbols`."
  (delay (available-editing-symbols)))

(def editing-prompt
  "Compatibility view of the now-empty editing prompt. Native tool contracts
   replaced this duplicated prompt fragment."
  (delay (available-editing-prompt)))
