(ns com.blockether.vis.internal.foundation.editing.core
  "Filesystem tools exposed as bare symbols in the Python sandbox.

   Two layers:

   1. Structured helpers for read / tree / search:

        (cat path)            ; -> {:path :anchors {<N:hash> text…} :next-offset N? :truncated? B}
        (cat path opts)       ; opts DICT only: ranges [[s e]…] / anchor A|[A1 A2] / tail N
        (cat path :tail)      ; last 400 lines (tail)
        (cat path :tail n)    ; last n lines
        (ls dir)              ; a DIRECTORY -> shallow listing {path, entries [{name path type size}], depth}
        (ls dir opts)         ; opts keys: depth (recurse) / is_hidden
                              ; Batch arg keys are NOT interchangeable: cat takes `files`,
                              ; ls and struct_index take `paths`. `cat` on a directory
                              ; refuses and points at `ls`, and vice versa; a nil/blank
                              ; path throws before any I/O.
        (grep query)         ; -> content hits (anchored) + ranked file-NAME matches;
                              ; query = a term or list of terms (OR), smart-case
                              ; substring. Opts: paths/include/limit/is_hidden

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
            [charred.api :as json]
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
            [com.blockether.vis.internal.fff-index :as fff-index]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.strutil :as strutil]
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




(defn- rg-needle-hostile-to-fff?
  "True when a needle is meaningless to fff's FUZZY PATH search — it honors
   quantifier/bracket chars as regex/glob, so `*workspace-root*`, `(defn foo` or
   `arr[0]` match nothing there. Only the path side is affected: fff's CONTENT
   grep runs in `:mode :plain`, which is a pure literal substring scan with the
   same smart-case rule as `make-line-matcher` (measured: `?`, `(`, `[`, `{`, `+`
   all match literally), so content discovery needs no bypass for any needle."
  [^String needle]
  (boolean (re-find #"[*+?(){}\[\]]" needle)))

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

(def ^:private rg-fff-grep-max-file-size
  "Largest file fff's native content grep will READ, in bytes.

   fff's own default is 10 MB (`MAX_FFFILE_SIZE`) and it skips bigger files
   SILENTLY, so a needle living in a 20 MB log/dump made grep answer \"No file
   NAME or CONTENT matched\" — a false negative, strictly worse than the slow
   scan this discovery path replaced. The index carries the SAME budget
   (`fff-index/max-content-file-size`); both have to move, raising only one
   still reads nothing."
  fff-index/max-content-file-size)

(def ^:private rg-search-budget-ms
  "Wall-clock budget for one grep's SCAN phase, in ms.

   Everything else about grep is bounded by COUNTS (`limit`, page limits,
   `rg-breadth-probe-limit`), which says nothing about time: a pathological tree
   or needle could ride all the way to the native-tool hard kill at 120 s and
   return NOTHING. Past this budget the sweep stops and reports what it has with
   `truncated_by`/`hits_truncated_by` = `time` plus a narrowing hint, so partial
   results never read as a whole answer."
  20000)

(def ^:private search-file-poll-lines
  "Lines between `check-interrupt!` polls while streaming ONE file. A 200 MB file
   is millions of lines; polling every line costs, polling never means Esc and
   the tool timeout cannot land mid-file."
  4096)

(def ^:private rg-fff-grep-page-limit
  "Matches per native-grep page. With `:max-matches-per-file 1` this is also the
   number of FILES a page can report, so a page is a file page."
  2000)

(def ^:private rg-fff-grep-max-pages
  "Hard ceiling on native-grep pages per needle (see `rg-fff-grep-files`). At
   `rg-fff-grep-page-limit` files a page that is 10k candidate files — far past
   any result cap — so a pathological needle can't page the whole tree."
  5)

(defn- rg-fff-grep-files
  "Every file fff's native grep sees `query` in, PAGED to exhaustion (bounded by
   `rg-fff-grep-max-pages`). A single page stops at `page-limit` matches and hands
   back `:next-file-offset`; dropping that resume cursor silently truncated the
   candidate set to one page, which the old fuzzy-path union then papered over."
  [idx ^String query]
  (loop
    [offset
     0

     page
     0

     acc
     (transient [])]

    (let
      [{:keys [matches next-file-offset]}
       (fff/grep idx
                 {:query query
                  :mode :plain
                  :file-offset offset
                  :page-limit rg-fff-grep-page-limit
                  :max-matches-per-file 1
                  :max-file-size rg-fff-grep-max-file-size
                  :time-budget-ms 1500})

       acc
       (reduce conj! acc matches)

       next-offset
       (long (or next-file-offset 0))]

      (if (or (zero? next-offset) (>= (inc page) (long rg-fff-grep-max-pages)))
        (persistent! acc)
        (recur next-offset (inc page) acc)))))

(defn- rg-fff-rel-files
  "fff items → File objects resolved under `base`, dropping items with no path."
  [^File base items]
  (keep (fn [{:keys [relative-path]}]
          (some->> relative-path
                   (io/file base)))
        items))

(defn- rg-fff-path-hit?
  "True when a FUZZY fff path item's path LITERALLY contains `query`, case-insensitively.
   fff's path search is fuzzy and returns a full page of unrelated paths; this is the
   filter that keeps rg from opening and reading all of them."
  [^String query {:keys [relative-path]}]
  (boolean (and relative-path
                (str/includes? (str/lower-case ^String relative-path) (str/lower-case query)))))

(defn- rg-fff-query-files
  "Candidate files ONE `query` contributes from an open fff index `idx`: native-GREP
   content hits (paged to exhaustion) plus the literal-filtered fuzzy-PATH hits.
   A needle hostile to fff's fuzzy path search skips the path side entirely — native
   grep is literal/smart-case, so it already sees every content candidate."
  [idx ^File base query]
  (let
    [path-items
     (when-not (rg-needle-hostile-to-fff? query)
       (->> (:items (fff/search idx {:query query :page-size 1000}))
            (filter #(rg-fff-path-hit? query %))))

     grep-items
     (rg-fff-grep-files idx query)]

    (concat (rg-fff-rel-files base path-items) (rg-fff-rel-files base grep-items))))

(defn- rg-fff-root-files
  "`rg-fff-candidate-files` for ONE root: a FILE root is its own only candidate, a
   directory root leases a single fff index and realizes every needle's hits in it."
  [^File root needles overlay]
  (if (.isFile root)
    [root]
    (fff-index/with-index [idx (fff-index/lease root true overlay)]
                          (let [base (.getCanonicalFile root)]
                            ;; doall: realize the lazy hits INSIDE with-open, before the fresh
                            ;; instance is closed.
                            (doall (mapcat #(rg-fff-query-files idx base %) needles))))))

(defn- rg-fff-candidate-files
  "Files under `roots` that MIGHT contain a needle, via fff — the fast, nested-
   `.gitignore`-aware universe rg then RE-VALIDATES with the literal `make-line-matcher`.
   fff-first: NO raw filesystem walk, so a `.gitignored` subtree is never descended.

   Normal needles → fff's native-GREP content hits (paged to exhaustion) plus the
   fff fuzzy-PATH hits whose path LITERALLY contains the needle. The path side is
   deliberately literal-filtered: fff's path search is FUZZY and happily returns a
   full page of unrelated paths (1000 files for a 1-word needle on this repo), and
   every one of those became a file rg then OPENED AND READ to find nothing —
   ~95% of the read budget spent on noise. Native grep already owns content
   discovery; the path side only carries the `foo.clj`-names-itself case.

   A needle HOSTILE to fff's FUZZY PATH search (a quantifier/bracket char it reads
   as regex/glob) simply skips the path side for that needle: native grep is
   `:mode :plain` — literal, smart-case, the same rule as `make-line-matcher` — so
   it already sees every content candidate. This used to fall back to the FULL fff
   enumeration, which then had rg OPEN AND READ every file in the tree (measured
   170ms vs 8ms on this repo) for no extra recall.
   Returns a File vec, deduped by canonical path."
  [roots needles overlay]
  (->> roots
       (mapcat #(rg-fff-root-files % needles overlay))
       ;; dedup by canonical path, keep File objects
       (reduce (fn [acc ^File f]
                 (assoc acc (.getCanonicalPath f) f))
               {})
       vals
       vec))


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
  "The `~/.vis` directory tree that file tools may ALWAYS reach, independent of
   workspace roots. Canonical (symlinks resolved), computed once on first use;
   dropped when `user.home` is unset. Kept SEPARATE from `temp-roots`: a write
   here is NOT captured as a session attachment (only temp writes are)."
  (delay (->> [".vis"]
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
        "Path is nil or blank - cat/ls take a concrete path string; note grep returns a MAP, so use (:paths r) or the keys under (get r \"matches\"), not the result itself"
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
     (.toPath (.getCanonicalFile
                (.toFile (.normalize (.toAbsolutePath (fs/path cwd (paths/expand-home (str p))))))))

     ;; A root this draft may not touch is refused OUTRIGHT — before any root
     ;; acceptance. With the jail disabled every host root is granted, so this
     ;; is the only thing standing between a drafted session and a `not-allowed`
     ;; root (or a copy-policy root with no clone minted for this draft).
     _denied
     (when (some (fn [denied]
                   (let [^java.nio.file.Path dp (canon denied)]
                     (.startsWith canonical dp)))
                 (workspace/denied-roots))
       (throw
         (ex-info
           (str
             "Path '"
             p
             "' lies in a filesystem root this draft may not touch (workspace.filesystem `draft` policy)")
           {:type :ext.foundation.editing/path-denied :path (str p)})))

     mappings
     (workspace/filesystem-root-mappings)

     ;; Roots the session works on through a PRIVATE copy: the model addresses a
     ;; context file by its REAL (trunk) path, and the edit must land in the
     ;; clone. Kept separate — and checked FIRST — because a broad allowed root
     ;; (`/` when the jail is disabled) would otherwise accept the trunk path
     ;; verbatim and let a drafted session write straight into the real tree.
     isolated
     (filterv (fn [{:keys [trunk clone]}]
                (and trunk clone (not= (str (canon trunk)) (str (canon clone)))))
       mappings)

     ^java.nio.file.Path target
     (or (some (fn [{:keys [trunk clone]}]
                 (let
                   [^java.nio.file.Path cp
                    (canon clone)

                    ^java.nio.file.Path tp
                    (canon trunk)]

                   ;; Already inside the clone → keep it (a clone nested under
                   ;; its own trunk must never remap twice).
                   (cond (.startsWith canonical cp) canonical
                         (.startsWith canonical tp) (.resolve cp (.relativize tp canonical)))))
               isolated)
         (when (.startsWith canonical cwd-canon) canonical)
         (some (fn [{:keys [clone]}]
                 (let [^java.nio.file.Path cp (canon clone)]
                   (when (.startsWith canonical cp) canonical)))
               mappings)
         ;; system temp dirs (/tmp, $TMPDIR) + Vis's own ~/.vis tree are ALWAYS
         ;; reachable, independent of workspace roots — config and diagnostics work.
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
    (throw
      (ex-info
        (str
          "File not found: "
          (paths/abbreviate-home (.getPath f))
          ". Do not guess or reconstruct paths; use grep to locate the file, then copy its returned path exactly.")
        {:type :ext.foundation.editing/file-not-found :path (paths/abbreviate-home (.getPath f))})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (paths/abbreviate-home (.getPath f)))
                    {:type :ext.foundation.editing/path-is-dir
                     :path (paths/abbreviate-home (.getPath f))})))
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

(defn- normalize-find-dir-path
  "Normalize one grep scope to a directory. Existing files become their
   parent directory; missing paths climb to the nearest existing confined
   directory. The model-facing value uses the same round-trippable address as
   every search result."
  [path]
  (let
    [p
     (str/trim (str path))

     p
     (if (str/blank? p) "." p)

     ^File f
     (safe-path p)

     ^File dir
     (cond (.isDirectory f) f
           (.isFile f) (.getParentFile f)
           :else (nearest-existing-dir f))]

    (if dir (rel-path dir) p)))

(defn- find-scope-misses
  "The requested grep scopes that DO NOT exist, each
   `{\"requested\" p \"searched\" nearest-existing-dir}`.

   `normalize-find-dir-path` climbs a stale path to the nearest existing
   directory so the search still runs — which would otherwise SWALLOW the typo.
   Computing the misses from the RAW scopes (before normalization) keeps the
   climb productive while `grep` still reports `missing_paths`, exactly
   like the content search does."
  [paths]
  (into []
        (keep (fn [p]
                (let [s (str/trim (str p))]
                  (when-not (str/blank? s)
                    (let [^File f (safe-path s)]
                      (when-not (.exists f)
                        (let [anc (nearest-existing-dir f)]
                          (cond-> {"requested" s}
                            anc
                            (assoc "searched" (rel-path anc))))))))))
        paths))


(defn- resolve-search-roots
  "Resolve grep `paths` into
   `{:roots [File …] :searched-paths [model-path …] :resolutions [{…} …]}`.

   `:roots` are the canonical Files actually searched — a FILE root is searched as
   that ONE file, a DIRECTORY root is walked as a tree (ripgrep / Claude-Code /
   Codex semantics: `rg PATTERN a.clj src/` greps `a.clj` as a file and `src/` as
   a tree in one pass; the whole downstream already special-cases `.isFile`).
   `:searched-paths` names those physical roots using the same round-trippable
   addresses returned for search hits; it never leaves a default multi-root sweep
   disguised as `[\".\"]`.

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
   primary cwd PLUS every bound filesystem-root clone — EXCEPT roots flagged
   `search: false` in the `workspace.filesystem` catalog (e.g. `~/.vis`, the
   language/dependency caches), which are pruned from the default sweep as search
   noise. The primary cwd is ALWAYS kept (so a session sitting in a draft or in
   `~/.vis` still searches its own tree), and explicit paths still reach a pruned
   root. NO `:resolutions` (a default sweep names nothing, so nothing is reportable).

   Explicit paths resolve through `safe-path` (confinement + trunk↔clone remap); a
   confinement violation still propagates — that is not a miss."
  [paths]
  (let
    [paths (mapv #(let [s (str/trim (str %))]

                    (if (str/blank? s) "." s))
                 paths)]
    (if (some #{"."} paths)
      (let
        [allowed (workspace/allowed-roots)
         primary (first allowed)
         no-search (workspace/no-search-roots)
         roots (into []
                     (comp (remove (fn [r]
                                     (and (not= r primary) (contains? no-search r))))
                           (map io/file))
                     allowed)]

        {:roots roots :searched-paths (mapv rel-path roots) :resolutions []})
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

        {:roots roots :searched-paths (mapv rel-path roots) :resolutions resolutions}))))

(defn- missing-search-paths
  "From `resolve-search-roots` `:resolutions`, the requested paths that did NOT
   exist — each `{\"requested\" p \"searched\" ancestor-dir}` (`searched` = the
   nearest existing directory the search climbed to, omitted when nothing in the
   chain existed). Empty when every named path was real (and always empty for the
   default `.` sweep). Surfaced identically on the `grep` name and content sides as
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

(defn- read-arg-paths
  "Extract one `path`, every `files` batch entry, or every `paths` batch entry
   from a native read call for protection. A batch entry is either a plain path
   string or a per-file options object carrying its own `path`."
  [args]
  (let
    [a
     (first args)

     entry-path
     (fn [e]
       (cond (string? e) e
             (map? e) (get e "path")
             :else nil))

     batch-paths
     (fn [entries]
       (when (sequential? entries) (vec (keep entry-path entries))))]

    (cond (map? a) (or (some-> (get a "files")
                               batch-paths)
                       (some-> (get a "paths")
                               batch-paths)
                       (when-let [path (get a "path")]
                         [path]))
          (some? a) [a]
          :else [])))

(defn- first-arg-paths [args] (when (seq args) [(first args)]))

(defn- nodes-arg-paths
  "Every file a `struct_nodes` call reads: the shared `path` plus the `path` of each
   `nodes` BATCH entry (a plain string entry IS its path)."
  [args]
  (let
    [a
     (first args)

     entry-path
     (fn [e]
       (cond (string? e) e
             (map? e) (get e "path")
             :else nil))]

    (cond (map? a) (let
                     [ns
                      (get a "nodes")

                      froms
                      (when (sequential? ns) (keep entry-path ns))]

                     (vec (distinct (remove nil? (cons (get a "path") froms)))))
          (some? a) [a]
          :else [])))

(defn- first-two-arg-paths [args] (take 2 args))

(defn- balanced-json-prefix
  "The substring of `s` from its first `[`/`{` through the bracket that closes it,
   nil when nothing balances. Lets a stringified argument with trailing junk
   (`\"[{…}]}\"` — one brace too many) still parse as the list it meant."
  [^String s]
  (let
    [array
     (str/index-of s "[")

     object
     (str/index-of s "{")

     start
     (if (and array object) (min (long array) (long object)) (or array object))]

    (when start
      (loop
        [i
         (long start)

         depth
         0

         in-string?
         false

         escaped?
         false]

        (when (< i (count s))
          (let [c (.charAt s i)]
            (cond escaped? (recur (inc i) depth in-string? false)
                  (= \\ c) (recur (inc i) depth in-string? true)
                  (= \" c) (recur (inc i) depth (not in-string?) false)
                  in-string? (recur (inc i) depth true false)
                  (or (= \[ c) (= \{ c)) (recur (inc i) (inc depth) false false)
                  (or (= \] c) (= \} c)) (if (<= depth 1)
                                           (subs s (long start) (inc i))
                                           (recur (inc i) (dec depth) false false))
                  :else (recur (inc i) depth false false))))))))

(defn- edit-maps-from-string
  "Edit maps recovered from an `edits` argument that arrived STRINGIFIED instead of
   as a real list — `patch(\"[{\\\"path\\\": …}]\")` is a recurring serializer slip.
   Parses the first BALANCED JSON array/object (trailing junk cannot lose the batch)
   and returns its maps as a vector; nil when nothing map-shaped parses."
  [s]
  (when-let [fragment (balanced-json-prefix s)]
    (let [parsed (try (json/read-json fragment) (catch Throwable _ nil))]
      (cond (map? parsed) [parsed]
            (and (sequential? parsed) (seq parsed) (every? map? parsed)) (vec parsed)
            :else nil))))

(defn- normalize-edits-arg
  "The `edits` batch as a REAL vector of edit maps, whatever shape it arrived in: the
   whole batch stringified, the kwargs spec map (`{\"edits\": [...]}`), ONE bare edit
   map, or a vector holding stringified entries. Pure coercion — every map is the
   caller's own and nothing is invented — so path guards, `coerce-patch-edits`, and
   `struct_patch`'s batch path all read the same shape. An unrecognisable value is
   returned unchanged so the caller's own validation still speaks."
  [edits]
  (let
    [unwrapped
     (if (and (map? edits) (contains? edits "edits")) (get edits "edits") edits)

     batch
     (cond (string? unwrapped) (or (edit-maps-from-string unwrapped) unwrapped)
           (map? unwrapped) [unwrapped]
           :else unwrapped)]

    (if (and (sequential? batch) (some string? batch))
      (mapv (fn [entry]
              (if (string? entry) (or (first (edit-maps-from-string entry)) entry) entry))
            batch)
      batch)))

(defn- patch-arg-paths
  [args]
  (let
    [edits
     (normalize-edits-arg (first args))

     edits
     (cond (map? edits) [edits]
           (sequential? edits) edits
           :else [])]

    (keep #(when (map? %) (get % "path")) edits)))

(defn- write-arg-paths
  "Every path a write-side call touches: the lone `path`, plus every path an
   `edits` BATCH entry carries (struct_patch batches one file or many)."
  [args]
  (let [a (first args)]
    (cond (map? a) (let
                     [own (when-let [path (get a "path")]
                            [path])
                      ;; struct_patch's PROJECT-wide rename scopes with `paths`.
                      scoped (when (sequential? (get a "paths")) (get a "paths"))
                      batch (let [batch-edits (normalize-edits-arg (get a "edits"))]
                              (when (sequential? batch-edits)
                                (keep #(when (map? %) (get % "path")) batch-edits)))]

                     (seq (distinct (concat own scoped batch))))
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
           :else nil)

     paths
     (cond (or (nil? paths) (and (sequential? paths) (empty? paths))) ["."]
           (sequential? paths) paths
           :else [paths])]

    (mapv normalize-find-dir-path paths)))

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

(defn- hidden-descendant-prefix?
  "Whether the static protected prefix is strictly below `ancestor` and crosses
   a dot-prefixed path segment. A default grep walk prunes at that segment."
  [ancestor glob]
  (let
    [prefix
     (glob-static-prefix glob)

     suffix
     (when (and (not= ancestor prefix) (path-prefix? ancestor prefix))
       (if (= "." ancestor) prefix (subs prefix (inc (count ancestor)))))]

    (boolean (some #(str/starts-with? % ".") (when suffix (str/split suffix #"/+"))))))

(defn- hidden-search? [args] (boolean (some #(and (map? %) (get % "is_hidden")) args)))

(defn- safe-read-ancestor-match?
  "True when a protected rule matched only because the read target is an
   ancestor of the protected path and the operation can safely start there.

   Reads at `.` retain the existing workspace-root behavior. A nested grep
   scope is also safe when reaching the protected prefix crosses a hidden
   segment (for example `bridge/.bridge/**`) and the caller did not request
   hidden files. Direct reads of that hidden directory, visible protected
   descendants, hidden-enabled searches, and every write remain blocked."
  [op access-intent target rule args]
  (let [rel (:resolved target)]
    (and (= :read access-intent)
         (composite-path-target? target)
         (not (protected-glob-matches? (:glob rule) rel))
         (or (= "." rel)
             (and (= :grep op)
                  (not (hidden-search? args))
                  (hidden-descendant-prefix? rel (:glob rule)))))))

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
                                   (safe-read-ancestor-match? op access-intent target rule args)))
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

(def ^:private fff-ls-page-size
  "Native page width for `ls`. Pages are exhausted so the listing keeps its complete,
   uncapped contract while each FFM result stays bounded. Wide on purpose: every page
   is a native crossing, and rows are filtered as they arrive (`fff-ls-scan`), so a
   wide page costs no extra retained memory."
  4096)


(defn- fff-ignore-overlay
  "The ignore OVERLAY handed to fff for a search, or nil when there is nothing
   to add. fff applies it natively in both the scan walk and the live watcher,
   so vis never walks a tree itself to honor these:

   - `:custom-ignore-filenames [\".rgignore\"]` — the ONE ignore filename
     ripgrep's `ignore` crate does not register on its own (`.gitignore`,
     `.ignore`, `.git/info/exclude` and the global gitignore are native).
   - `:unignore-globs` — the `:grep :include-gitignored-paths` config
     (issue #23): subtrees re-included although `.gitignore` excludes them.
     A gitignored DIRECTORY is never descended by any gitignore-honoring
     walker, which is why a `!` negation cannot do this and fff reopens those
     static prefixes in a second, ignore-free pass.
   - `:exclude-globs` — the `:grep :always-exclude` config guarding what
     those re-includes would otherwise drag in.

   The config half ALWAYS applies: `.gitignore` is honored unconditionally and
   `vis.yml` is the only way to widen or narrow what search sees."
  []
  (let
    [{:keys [include-gitignored-paths always-exclude]}
     (config/search-overlay)

     overlay
     {:custom-ignore-filenames [".rgignore"]
      :exclude-globs (vec always-exclude)
      :unignore-globs (vec include-gitignored-paths)}]

    (when (some seq (vals overlay)) overlay)))

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

(defn- coerce-cat-range
  "Normalize ONE requested `[start end]` window instead of refusing it: a reversed
   pair is swapped and a non-positive endpoint clamps to line 1. A caller that
   mistypes ONE end line (`[5288, 3400]`) among several good windows keeps the
   whole atomic read instead of losing all of them — and the read is never
   silent: the coerced pair ships back as the window's `range` beside a `note`
   saying what was corrected.

   Only a pair carrying no usable line number still throws: non-integer
   components, or the whole-file sentinel (both ends non-positive), which every
   caller resolves before a REAL window is built."
  [start end]
  (letfn [(fail! [defect]
            (throw (ex-info
                     (str "cat \"range\"/\"ranges\" window ["
                          (pr-str start)
                          ", "
                          (pr-str end)
                          "] is invalid: "
                          defect
                          " — pass [-1, -1] (any non-positive pair) to read the WHOLE file")
                     {:type :ext.foundation.editing/invalid-cat-args :start start :end end})))]
    (when-not (and (integer? start) (integer? end))
      (fail! "start and end must be integer line numbers"))
    (when (and (not (pos? (long start))) (not (pos? (long end))))
      (fail! "both line numbers are non-positive, which is the WHOLE-FILE sentinel, not a window"))
    (let
      [s
       (max 1 (long start))

       e
       (max 1 (long end))]

      (if (> s e) [e s] [s e]))))

(defn- cat-range-note
  "One-line report of a window `cat` corrected, so a coerced read is never silent:
   the caller sees the pair it asked for, the pair actually read, and can re-read
   when the correction is not the region it meant."
  [[requested-start requested-end] [start end]]
  (str "requested window [" requested-start
       ", " requested-end
       "] was normalized to [" start
       ", " end
       "] (windows are 1-based and must ascend)" " — re-read if that is not the region you meant"))

(defn- cat-range-scalar
  "Coerce a range component to a long. Accepts an int or a numeric string like
   \"1096\" (models routinely pass line numbers as strings), NEGATIVE included
   (\"-1\") so the whole-file sentinel survives stringification; nil otherwise."
  [x]
  (cond (integer? x) (long x)
        (and (string? x) (re-matches #"\s*-?\d+\s*" x)) (parse-long (str/trim x))
        :else nil))

(defn- cat-pair-items
  "Split a range entry into its raw components, or nil when it is not pair-shaped.
   Accepts a `[s e]` sequential or a comma-joined string `\"s, e\"`."
  [pair]
  (cond (and (string? pair) (str/includes? pair ",")) (mapv str/trim (str/split pair #","))
        (sequential? pair) (vec pair)
        :else nil))

(defn- normalize-cat-pair
  "Coerce one range entry to a `[start end]` long pair, or nil when it is not a
   pair. Accepts `[s e]` with int OR numeric-string components, or a comma-joined
   string `\"s, e\"` — the shapes a model produces when it forgets to nest/parse."
  [pair]
  (let [items (cat-pair-items pair)]
    (when (= 2 (count items))
      (let [nums (map cat-range-scalar items)]
        (when (every? some? nums) [(long (first nums)) (long (second nums))])))))

(defn- cat-pair-error!
  "Throw a specific error explaining exactly why `pair` is not a valid
   `[start end]`, naming the offending non-numeric component(s)."
  [pair]
  (let [items (cat-pair-items pair)]
    (cond
      (nil? items)
      (throw
        (ex-info
          (str
            "cat \"range\"/\"ranges\" entries must be [start, end] pairs, e.g. [10, 40] or \"10, 40\"; got "
            (pr-str pair))
          {:type :ext.foundation.editing/invalid-cat-args :range pair}))
      (not= 2 (count items))
      (throw (ex-info
               (str "cat range " (pr-str pair)
                    " must have exactly 2 components (start, end), got " (count items))
               {:type :ext.foundation.editing/invalid-cat-args :range pair :count (count items)}))
      :else
      (let [bad (filterv #(nil? (cat-range-scalar %)) items)]
        (throw (ex-info
                 (str
                   "cat range "
                   (pr-str pair)
                   " has non-numeric component(s) "
                   (str/join ", " (map pr-str bad))
                   " — start/end must be line numbers like 10 or \"10\", not variables/expressions")
                 {:type :ext.foundation.editing/invalid-cat-args :range pair :invalid bad}))))))

(defn- cat-flat-ranges-error!
  "Throw a specific error when `ranges` is a flat list of line numbers
   (`[108 120 130]`) that should have been nested `[[108 120] [130 140]]` —
   the per-item error would misleadingly blame the first scalar (`got 108`)."
  [ranges items]
  (let
    [nums
     (mapv cat-range-scalar items)

     suggestion
     (if (and (even? (count nums)) (every? some? nums))
       (pr-str (mapv vec (partition 2 nums)))
       "[[10, 40], [80, 120]]")]

    (throw (ex-info (str "cat \"ranges\" looks like a flat list of line numbers " (pr-str ranges)
                         "; nest them as [start, end] pairs, e.g. " suggestion)
                    {:type :ext.foundation.editing/invalid-cat-args :ranges ranges}))))

(defn- cat-ranges-from-string
  "Lenient parse of a whole `ranges` STRING a model stringified instead of
   passing a nested list — e.g. \"[[985, 1030]], [[236, 322]]\" or \"[10, 40]\".
   Pulls every run of digits in order; when there is an even count (>= 2)
   returns them partitioned into `[start end]` long pairs, else nil."
  [s]
  (let [nums (mapv parse-long (re-seq #"-?\d+" s))]
    (when (and (seq nums) (even? (count nums))) (mapv vec (partition 2 nums)))))

(defn- cat-entry->pair
  "Coerce ONE `ranges` entry to a `[start end]` long pair, or nil. Extends
   `normalize-cat-pair` with a lenient parse of a single stringified/bracketed
   pair like \"[985, 1030]\" or \"985,1030\", so a VECTOR whose elements are each
   a stringified pair (`[\"[985, 1030]\" \"[236, 322]\"]`) still normalizes."
  [entry]
  (or (normalize-cat-pair entry)
      (when (string? entry)
        (let [pairs (cat-ranges-from-string entry)]
          (when (= 1 (count pairs)) (first pairs))))))

(defn- cat-whole-file-pair?
  "True when ONE `ranges` entry is the WHOLE-FILE sentinel: a pair whose start AND
   end are both non-positive (`[-1, -1]`, `[0, 0]`, `\"-1, -1\"`). No file has a
   line 0 or -1, so the shape is unambiguous — it is how a batched read opts ONE
   file out of the call's shared `ranges` and takes all of it."
  [entry]
  (boolean (when-let [pair (cat-entry->pair entry)]
             (and (not (pos? (long (first pair)))) (not (pos? (long (second pair))))))))

(defn- cat-whole-file-ranges?
  "True when `ranges` EXPLICITLY asks for the whole file through a sentinel entry
   (`[[-1, -1]]`, the flat `[-1, -1]`, or their stringified forms). A sentinel is a
   superset of every sibling window, so mixing it with real ranges still reads
   everything. Absent/empty `ranges` are NOT handled here: they stay the caller's
   own default-vs-reject decision."
  [ranges]
  (boolean (or (cat-whole-file-pair? ranges)
               (and (sequential? ranges) (seq ranges) (some cat-whole-file-pair? ranges))
               (and (string? ranges) (some cat-whole-file-pair? (cat-ranges-from-string ranges))))))

(defn- cat-range-pairs
  "Every `ranges` entry as the raw `[start end]` long pair the caller REQUESTED —
   no swap, no clamp — so a reader can coerce each window and still report what it
   corrected. Throws only on shapes that carry no pair at all."
  [ranges]
  (let
    [flat
     (normalize-cat-pair ranges)

     items
     (cat-pair-items ranges)

     ;; a pair-shaped scalar (`\"1, x\"`) or flat vector of scalars (`[\"1\" \"x\"]`)
     ;; that failed coercion — explain the bad component instead of the generic
     ;; \"expects [[start, end], ...]\".
     flat-attempt?
     (and items (= 2 (count items)) (not-any? sequential? items))

     ;; a flat list of 3+ line numbers (`[108 120 130]`) instead of nested
     ;; pairs — the per-item error would misleadingly blame the first scalar.
     flat-list?
     (and (sequential? ranges) (> (count items) 2) (not-any? sequential? items))

     ;; a whole `ranges` passed as a STRING (`"[[985, 1030]], [[236, 322]]"`) —
     ;; parse the digit runs into pairs so a stringified nested list still works.
     str-pairs
     (when (string? ranges) (cat-ranges-from-string ranges))

     ;; a sequential `ranges` whose entries each coerce to a pair — including a
     ;; VECTOR of stringified pairs (`["[985, 1030]" "[236, 322]"]` or
     ;; `["985,1030" "236,322"]`) that `flat` cannot read as one pair.
     entry-pairs
     (when (sequential? ranges)
       (let [ps (mapv cat-entry->pair items)]
         (when (and (seq ps) (every? some? ps)) ps)))

     pairs
     (cond flat [flat]
           str-pairs str-pairs
           entry-pairs entry-pairs
           flat-attempt? (cat-pair-error! ranges)
           flat-list? (cat-flat-ranges-error! ranges items)
           (sequential? ranges) (vec ranges)
           :else (throw (ex-info "cat \"ranges\" expects [[start, end], ...]"
                                 {:type :ext.foundation.editing/invalid-cat-args :ranges ranges})))]

    (when (empty? pairs)
      (throw (ex-info "cat \"ranges\" expects at least one range"
                      {:type :ext.foundation.editing/invalid-cat-args :ranges ranges})))
    (mapv (fn [pair]
            (or (normalize-cat-pair pair) (cat-pair-error! pair)))
          pairs)))

(defn- normalize-cat-ranges
  "Requested windows as REAL, ascending, 1-based `[start end]` pairs — every entry
   coerced by `coerce-cat-range`."
  [ranges]
  (mapv (fn [[s e]]
          (coerce-cat-range s e))
        (cat-range-pairs ranges)))

(defn- hidden-relative-path?
  "True when `relative-path` has a hidden filesystem segment below `root`. This
   preserves `ls`'s `is_hidden` contract while fff owns every ignore decision."
  [^File root ^String relative-path]
  (loop
    [^File parent
     root

     [part & more]
     (str/split relative-path #"/")]

    (if part
      (let [^File child (io/file parent part)]
        (or (.isHidden child) (recur child more)))
      false)))

(defn- path-segments
  "Segment count of a `/`-joined relative path. Counting separators keeps the depth
   test allocation-free — it runs on EVERY indexed record."
  ^long [^String p]
  (loop
    [i
     (.indexOf p "/")

     n
     1]

    (if (neg? i) n (recur (.indexOf p "/" (unchecked-inc i)) (unchecked-inc n)))))

(def ^:private glob-syntax-chars
  "Characters fff's glob parser reads as PATTERN syntax. A directory whose path
   contains one has no literal prefix pattern, so `ls` scans the file index and
   compares prefixes itself instead of handing fff something it would reinterpret."
  #{\* \? \[ \] \{ \} \\})

(defn- fff-ls-file-pages
  "Page fetcher for fff's FILE index, prefix-filtered NATIVELY when it can be.

   `glob` costs the MATCHES rather than the tree: listing `src` asks fff for
   `src/**` instead of paging every indexed file and dropping most of them here.
   The workspace root (empty prefix) and a prefix carrying glob syntax have no
   literal pattern, so those page the whole file index and lean on the keeper."
  [idx ^String prefix]
  (if (and (seq prefix) (not-any? glob-syntax-chars prefix))
    (fn [offset]
      (fff/glob idx {:pattern (str prefix "**") :page-index offset :page-size fff-ls-page-size}))
    (fn [offset]
      (fff/search idx {:query "" :page-index offset :page-size fff-ls-page-size}))))

(defn- fff-ls-dir-pages
  "Page fetcher for fff's DIRECTORY index. It has no glob surface, so the prefix
   test stays in `fff-ls-keeper` — cheap, since directories are the small side of
   the index."
  [idx]
  (fn [offset]
    (fff/search-directories idx {:query "" :page-index offset :page-size fff-ls-page-size})))

(defn- fff-ls-scan
  "Stream one native index through `keep-fn`, page by page.

   fff's native `:page-index` is a record offset despite its name, so advance it by
   the records actually returned. Each page is FILTERED AS IT ARRIVES and only
   survivors are retained, so a 200k-record tree costs a few native pages and a
   handful of rows instead of one 200k-element vector. `directory?` is the index's
   own kind (the file and directory indexes each answer for one). `keep-fn` answers
   the row to keep, or nil to drop the record."
  [fetch directory? keep-fn]
  (loop
    [offset
     0

     acc
     (transient [])]

    (let
      [{page :items total :total-matched}
       (fetch offset)

       page
       (or page [])

       acc
       (reduce (fn [a item]
                 (if-let [row (keep-fn item directory?)]
                   (conj! a row)
                   a))
               acc
               page)

       seen
       (unchecked-add (long offset) (long (count page)))]

      (if (or (empty? page) (>= seen (long total))) (persistent! acc) (recur seen acc)))))

(defn- fff-ls-records
  "Every indexed record under one listed directory, taken from BOTH native indexes.

   fff keeps files and directories apart, and `search-mixed` returns exactly this
   union only after merging and re-ranking them. Measured on this repo (1215
   records): `search-mixed` 3.3 ms, the two direct calls 1.8 ms, and 0.8 ms once
   the file side is a native `glob` prefix — same rows, no merge tax."
  [idx prefix keep-fn]
  (into (fff-ls-scan (fff-ls-file-pages idx prefix) false keep-fn)
        (fff-ls-scan (fff-ls-dir-pages idx) true keep-fn)))

(defn- fff-ls-keeper
  "The per-record filter handed to `fff-ls-scan`, rebasing each kept record's
   `:relative-path` to be LOCAL to the listed directory and stamping the kind its
   index answers for.

   Ordered by cost: `prefix` (a plain string compare) drops records outside the
   listed subtree, the depth bound is a separator count, and only survivors reach
   `hidden-relative-path?` — the single filesystem touch. Cost therefore tracks the
   rows RETURNED, not the size of the indexed tree."
  [^File root ^String prefix ^long levels is-hidden?]
  (let [plen (count prefix)]
    (fn [{:keys [relative-path] :as item} directory?]
      (let [^String p (or relative-path "")]
        (when (and (> (count p) plen) (or (zero? plen) (str/starts-with? p prefix)))
          (let [local (subs p plen)]
            (when (and (<= (path-segments local) levels)
                       (or is-hidden? (not (hidden-relative-path? root local))))
              (assoc item
                :relative-path local
                :directory? directory?))))))))

(defn- fff-ls-overlay
  "Rebase the global `vis.yml` overlay for an explicitly listed directory.

   `ls` may name a directory already ignored by an ancestor (such as `target/`).
   Opening the pooled native index at that directory preserves explicit-read
   semantics, while paths configured beneath it must become local to fff's base."
  [target-rel]
  (let
    [prefix
     (when (seq target-rel) (str target-rel "/"))

     rebase
     (fn [globs]
       (->> globs
            (keep (fn [glob]
                    (let [glob (str glob)]
                      (cond (= glob target-rel) nil
                            (and prefix (str/starts-with? glob prefix)) (subs glob (count prefix))
                            :else glob))))
            vec))]

    (some-> (fff-ignore-overlay)
            (update :exclude-globs rebase)
            (update :unignore-globs rebase))))

(defn- warm-ls-lease
  "The pooled WORKSPACE-root index — the one `grep` and `find` already keep hot — but
   ONLY when it is ALREADY built. Reusing it turns `ls` of a subdirectory into a page
   scan over an existing index instead of a fresh per-directory tree scan plus its
   own watcher. A cold pool is left alone on purpose: indexing just the subtree is
   then the cheaper of the two. The root is CANONICAL so the pool key is the very one
   `grep`/`find`/`ls .` warm."
  []
  (let
    [lease
     (fff-index/lease (.getCanonicalFile (io/file (workspace/cwd))) true (fff-ignore-overlay))]
    (when (fff-index/warm? lease) lease)))

(def ^:private ls-workspace-reuse-limit
  "Directory ceiling for answering a listing out of the warm workspace index. With the
   file side natively prefix-filtered (`fff-ls-file-pages`), the directory index is the
   only part of a reuse still scanned whole — about 1.1 µs a directory — so its size is
   both the honest cost model and the cheapest gate to read (a 1-record directory probe
   is ~0.01 ms against ~0.13 ms for a mixed one). Above this ceiling, indexing just the
   listed subtree is the better trade and `ls` leases at the directory."
  20000)

(defn- fff-ls-workspace-items
  "Listing rows for `target-rel` served from the ALREADY-WARM workspace index, or nil
   when that index cannot or should not answer: nothing warm, an index too big to
   scan (`ls-workspace-reuse-limit`), a directory outside the workspace, or one fff
   ignores (`target/`, `node_modules/` …). The directory's OWN record proves
   coverage, so an empty-but-indexed directory answers here instead of paying for a
   fallback index."
  [^File root target-rel ^long levels is-hidden?]
  (when (and (seq target-rel) (not (str/starts-with? (str target-rel) "/")))
    (when-let [lease (warm-ls-lease)]
      (let
        [prefix (str target-rel "/")
         keep-fn (fff-ls-keeper root prefix levels is-hidden?)
         covered? (volatile! false)
         rows (fff-index/with-index
                [idx lease]
                (when (<= (long (or (:total-matched
                                      (fff/search-directories idx {:query "" :page-size 1}))
                                    0))
                          (long ls-workspace-reuse-limit))
                  (fff-ls-records idx
                                  prefix
                                  (fn [{:keys [relative-path] :as item} directory?]
                                    (if (= target-rel relative-path)
                                      (do (vreset! covered? true) nil)
                                      (keep-fn item directory?))))))]

        (when (and (some? rows) (or @covered? (seq rows))) rows)))))

(defn- fff-ls-target-items
  "Listing rows from an index rooted AT the listed directory: the fallback that keeps
   an explicitly named ignored directory readable, and the only source for a
   directory outside the workspace."
  [^File root target-rel ^long levels is-hidden?]
  (fff-index/with-index [idx (fff-index/lease root true (fff-ls-overlay target-rel))]
                        (fff-ls-records idx "" (fff-ls-keeper root "" levels is-hidden?))))

(defn- list-dir
  "Directory listing as MODEL data, powered by fff — never a filesystem walk.

   fff owns `.gitignore`, `.ignore`, `.rgignore`, and the live `vis.yml` grep overlay
   in one native index; this code only rebuilds the documented tree shape. Two
   sources, cheapest first:

   1. the WARM workspace index `grep`/`find` already maintain, prefix-filtered — no
      new index and no new watcher;
   2. otherwise an index rooted at the directory itself, which is also what keeps a
      directly listed ignored directory readable, as it was before.

   Records are depth-filtered inside the paging loop (`fff-ls-scan`), so cost tracks
   the rows RETURNED rather than the size of the tree, and rendered paths are joined
   from the directory's own rendered path instead of canonicalizing per row. Results
   remain directories-first then alphabetical, and are exhaustive."
  [^File d {:keys [depth is_hidden] :or {depth 1 is_hidden false}}]
  (let
    [^File root
     (.getCanonicalFile d)

     base
     (rel-path root)

     target-rel
     (if (= "." base) "" base)

     levels
     (long depth)

     is-hidden?
     (boolean is_hidden)

     items
     (or (fff-ls-workspace-items root target-rel levels is-hidden?)
         (fff-ls-target-items root target-rel levels is-hidden?))

     render-prefix
     (if (= "." base) "" (str base "/"))

     rows
     (->> items
          (map (fn [{:keys [relative-path directory? size]}]
                 (let
                   [^String path
                    (or relative-path "")

                    slash
                    (.lastIndexOf path "/")]

                   {:local path
                    :parent (if (neg? slash) "" (subs path 0 slash))
                    :entry {"name" (if (neg? slash) path (subs path (inc slash)))
                            "path" (str render-prefix path)
                            "type" (if directory? "dir" "file")
                            "size"
                            (if directory? (.length (io/file root path)) (long (or size 0)))}})))
          (group-by :parent))]

    (letfn [(children [parent ^long level]
              (->> (get rows parent)
                   (sort-by (fn [{:keys [entry]}]
                              [(if (= "dir" (get entry "type")) 0 1) (get entry "name")]))
                   (mapv (fn [{:keys [local entry]}]
                           (if (and (= "dir" (get entry "type")) (< level levels))
                             (assoc entry "children" (children local (unchecked-inc level)))
                             entry)))))]
      {"path" base "type" "dir" "entries" (children "" 1) "depth" levels})))

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
    [requested
     (cat-range-pairs ranges)

     windows
     (mapv (fn [[requested-start requested-end]]
             (let
               [[start end]
                (coerce-cat-range requested-start requested-end)

                out
                (read-file path start (inc (- (long end) (long start))))]

               (cond->
                 (assoc (select-keys out [:lines :next-offset :eof? :truncated?])
                   :range [start end])
                 (not= [requested-start requested-end] [start end])
                 (assoc :note (cat-range-note [requested-start requested-end] [start end])))))
           requested)

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
   directory walks poll this so Esc aborts them promptly instead of traversing a
   huge configured catalog path and leaving the spinner stuck on 'cancelling'."
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
     (cond (and (= 1 (count args)) (or (string? a) (sequential? a))) {"query" a}
           (and (= 2 (count args)) (or (string? a) (sequential? a)) (map? b)) (assoc b "query" a)
           (and (= 1 (count args)) (map? a)) a
           :else (throw (ex-info
                          "grep takes grep(query), grep(query, opts), or grep({\"query\": q, ...})."
                          {:type :ext.foundation.editing/invalid-find-args
                           :expected '([query] [query opts] [spec-map])
                           :got args})))

     allowed-keys
     #{"query" "paths" "path" "limit" "include" "context" "is_hidden"}

     unknown-keys
     (seq (remove allowed-keys (keys spec)))]

    (when unknown-keys
      (throw (ex-info (str "find spec has unknown keys: "
                           (str/join ", " (map str unknown-keys))
                           ". Allowed: query, paths, limit, include, context, is_hidden.")
                      {:type :ext.foundation.editing/invalid-find-args
                       :unknown (vec unknown-keys)
                       :allowed (vec (sort allowed-keys))})))
    (let
      [raw-query
       (get spec "query")

       ls?
       (or (nil? raw-query)
           (and (string? raw-query) (str/blank? raw-query))
           (and (sequential? raw-query) (empty? raw-query)))

       _
       (when-not (or ls?
                     (and (string? raw-query) (not (str/blank? raw-query)))
                     (and (sequential? raw-query)
                          (seq raw-query)
                          (every? #(and (string? %) (not (str/blank? %))) raw-query)))
         (throw
           (ex-info
             "find \"query\" must be a non-blank string or a non-empty vector of non-blank strings"
             {:type :ext.foundation.editing/invalid-find-args :query raw-query})))

       query
       (cond ls? ""
             (sequential? raw-query) (str/join " " raw-query)
             :else raw-query)

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
             "find \"paths\" must be a string or vector of directory strings (empty defaults to current directory)"
             {:type :ext.foundation.editing/invalid-find-args :paths raw-paths})))

       precise-paths
       (into [] (distinct) paths)

       missing
       (find-scope-misses paths)

       context
       (let [c (get spec "context" 0)]
         (when-not (and (integer? c) (not (neg? (long c))))
           (throw (ex-info "find \"context\" must be a non-negative integer"
                           {:type :ext.foundation.editing/invalid-find-args :context c})))
         (long c))

       paths
       (into [] (comp (map normalize-find-dir-path) (distinct)) paths)

       limit
       (or (get spec "limit") default-find-limit)

       _
       (when-not (and (integer? limit) (pos? (long limit)))
         (throw (ex-info "find \"limit\" must be a positive integer"
                         {:type :ext.foundation.editing/invalid-find-args :limit limit})))]

      {:query query
       :paths paths
       :precise-paths precise-paths
       :missing missing
       :context context
       :limit limit
       :is_hidden (boolean (get spec "is_hidden"))
       :is_ls ls?})))

(defn- find-direct-file-item
  "The single candidate a DIRECT FILE root contributes to a find scan: itself, at score 1.0."
  [^File root]
  {:path (rel-path root)
   :file-name (.getName root)
   :size (.length root)
   :binary? false
   :source :direct-file
   :score 1.0})

(defn- find-scan-item
  "One fff hit → a scored candidate map, or nil when it misses `find-min-score` or is
   hidden while `is_hidden` is false.

   No gitignore re-check here: the index was opened with this exact ignore policy AND
   overlay, so fff already decided. Re-running a Clojure matcher would drop the very
   files `:include-gitignored-paths` re-includes."
  [^File base query is_hidden
   {:keys [relative-path file-name git-status size modified frecency-score binary?]}]
  (let
    [f
     (io/file base relative-path)

     rel
     (rel-path f)

     score
     (find-relevance query rel)]

    (when (and (>= (double score) (double find-min-score)) (or is_hidden (not (.isHidden f))))
      {:path rel
       :file-name (or file-name (.getName f))
       :size size
       :modified modified
       :frecency-score frecency-score
       :git-status git-status
       :binary? (boolean binary?)
       :score score})))

(defn- find-scan-root
  "`find-scan` for ONE root: a FILE root contributes itself, a directory root leases one
   fff index and scores its page of hits inside the lease."
  [^File root query is_hidden candidate-page overlay]
  (if (.isFile root)
    [(find-direct-file-item root)]
    (fff-index/with-index [idx (fff-index/lease root true overlay)]
                          (let [base (.getCanonicalFile root)]
                            ;; doall: realize hits INSIDE with-open, before the fresh instance is closed.
                            (doall (->> (:items
                                          (fff/search idx {:query query :page-size candidate-page}))
                                        (keep #(find-scan-item base query is_hidden %))))))))

(defn- find-scan
  "Scan `roots` for ONE `query` string and keep candidates whose
   `find-relevance` (name-weighted, order-insensitive) clears `find-min-score`.
   Returns raw item maps carrying `:score`. A direct FILE root contributes
   itself at score 1.0. The single-query building block `find-search` runs
   once for the strict whole-query pass and once per token for the fallback.

   Directory roots ALWAYS go through fff (fast, frecency-ranked), including
   `.rgignore` / `:grep` overlay projects, which fff honors natively via
   `overlay`. `.gitignore` is always respected; vis walks no tree here."
  [roots query is_hidden candidate-page overlay]
  (->> roots
       (mapcat #(find-scan-root % query is_hidden candidate-page overlay))
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

(defn- find-ls-item
  "One fff ls hit → an unscored listing map, or nil when hidden while `is_hidden` is false.
   See `find-scan-item`: fff owns the ignore verdict."
  [^File base is_hidden
   {:keys [relative-path file-name git-status size modified frecency-score binary?]}]
  (let
    [f
     (io/file base relative-path)

     rel
     (rel-path f)]

    (when (or is_hidden (not (.isHidden f)))
      {:path rel
       :file-name (or file-name (.getName f))
       :size size
       :modified modified
       :frecency-score frecency-score
       :git-status git-status
       :binary? (boolean binary?)
       :score 1.0})))

(defn- find-ls-root
  "`find-ls` for ONE root: a FILE root lists itself, a directory root enumerates its fff
   index with a blank query inside the lease."
  [^File root limit is_hidden overlay]
  (if (.isFile root)
    [(find-direct-file-item root)]
    (fff-index/with-index
      [idx (fff-index/lease root true overlay)]
      (let [base (.getCanonicalFile root)]
        (doall (->> (:items (fff/search idx {:query "" :page-size (max (long limit) 300)}))
                    (keep #(find-ls-item base is_hidden %))))))))

(defn- find-ls-rank
  "ls ordering key: frecency desc, then recency desc, then path."
  [it]
  [(- (long (or (:frecency-score it) 0))) (- (long (or (:modified it) 0))) (:path it)])

(defn- find-ls
  "ls-mode listing for a BLANK grep query: enumerate every file under
   `roots` (a FILE root lists itself) ranked by frecency then recency, capped at
   `limit`. There is no pattern to match, so this is `ls`, not a fuzzy search —
   it skips the whole query-scoring path. Honors `is_hidden` and the same fff
   ignore `overlay` as the scored path."
  [roots limit is_hidden overlay]
  (->> roots
       (mapcat #(find-ls-root % limit is_hidden overlay))
       (distinct)
       (sort-by find-ls-rank)
       (take limit)
       vec))

(defn- find-search
  [args]
  (let
    [{:keys [query paths limit is_hidden is_ls] scope-misses :missing}
     (coerce-find-spec args)

     {roots :roots find-resolutions :resolutions searched-paths :searched-paths}
     (resolve-search-roots paths)

     ;; fff ranks genuine hits first but pads the page with loose subsequence
     ;; noise, so pull a WIDER candidate set than `limit` and let the relevance
     ;; filter below do the real cutting (a fresh fff scan is ~11ms).
     candidate-page
     (max (long limit) 300)

     ;; `.rgignore` + the `:grep` config overlay (issue #23), handed to fff
     ;; itself — see `fff-ignore-overlay`.
     search-overlay
     (fff-ignore-overlay)

     scan
     (fn [q]
       (find-scan roots q is_hidden candidate-page search-overlay))

     strict
     (if is_ls [] (scan query))

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
     (if is_ls
       [(find-ls roots limit is_hidden search-overlay) false]
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
                 vec) true])))

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

    ;; Model-facing grep result — string keys, no keyword values, and TOTAL:
    ;; every key ships on every call (empty vector / false, never absent) so
    ;; caller code can index a field without a `contains?` dance first.
    {"items" (mapv ->item items)
     "item_count" (count items)
     "paths" (mapv :path items)
     "query" query
     "searched_paths" searched-paths
     "limit" limit
     "truncated_by" (if (>= (count items) (long limit)) "limit" "end_of_results")
     "fuzzy" (boolean fuzzy?)
     "matched_terms" (vec matched-terms)
     "missing_paths" (into (missing-search-paths find-resolutions) scope-misses)}))

(declare ^:private rg-search ^:private coerce-rg-spec)

(defn- find-args->content-spec
  "Build grep's CONTENT-search spec from its public args. Scope paths are the
   PRECISE ones the caller named (a file greps as that one file, rg-style); the
   caller widens to the normalized directory scopes only when the precise pass
   finds nothing. `context` N (0 = off) rides along, so one call can ask for the
   surrounding lines of every hit."
  [args]
  (let
    [[a b]
     args

     spec
     (cond (and (= 1 (count args)) (map? a)) a
           (and (= 2 (count args)) (map? b)) (assoc b "query" a)
           (= 1 (count args)) {"query" a}
           :else {})

     {paths :precise-paths :keys [context]}
     (coerce-find-spec args)]

    (cond-> {"query" (get spec "query") "paths" paths}
      (pos? (long (or context 0)))
      (assoc "context" context)

      (contains? spec "include")
      (assoc "include" (get spec "include"))

      (contains? spec "is_hidden")
      (assoc "is_hidden" (get spec "is_hidden")))))

(defn- content-result
  "Build grep's CONTENT hits from an `rg-search` result: an ordered
   `{path {\"lineno:hash\" {\"text\" line}}}` matches map (each anchor key is a
   patch `from_anchor`), plus hit/file counts, first hit, echoed needles, and
   breadth flags when more files match than are shown. With `context` N each hit
   also carries `before`/`after` — vectors of `{\"line\" n \"text\" line}` — so the
   surrounding lines arrive in the same call. The compatibility arity takes the
   context count positionally; the hits already carry it."
  ([out needles]
   (let
     [hits
      (vec (:hits out))

      ordered-paths
      (distinct (map :path hits))

      by-path
      (group-by :path hits)

      total-files
      (:total-file-count out)

      matches
      (let [^java.util.LinkedHashMap mm (java.util.LinkedHashMap.)]
        (doseq [p ordered-paths]
          (let [^java.util.LinkedHashMap fm (java.util.LinkedHashMap.)]
            (doseq [{:keys [line text before after]} (get by-path p)]
              (.put fm
                    (patch/line-anchor line text)
                    (cond-> {"text" text}
                      (seq before)
                      (assoc "before"
                        (mapv (fn [[ln txt]]
                                {"line" ln "text" txt})
                              before))

                      (seq after)
                      (assoc "after"
                        (mapv (fn [[ln txt]]
                                {"line" ln "text" txt})
                              after)))))
            (.put mm p fm)))
        mm)

      file-counts
      (let [^java.util.LinkedHashMap fc (java.util.LinkedHashMap.)]
        (doseq
          [p (sort-by (fn [p]
                        [(- (count (get by-path p))) p])
                      ordered-paths)]
          (.put fc p (count (get by-path p))))
        fc)]

     ;; TOTAL result: breadth and truncation keys ALWAYS ship (nil / false when
     ;; there is nothing to report), so `r["hits_truncated_by"]` is a value test
     ;; rather than a key test. CONTENT truncation stays its OWN signal: the
     ;; top-level `truncated_by` is the NAME search's, so a capped content sweep
     ;; would otherwise read as `end_of_results` and a slice would pass as whole.
     {"needles" needles
      "matches" matches
      "hit_count" (count hits)
      "file_count" (count ordered-paths)
      "file_counts" file-counts
      "total_file_count" total-files
      "total_file_count_is_exact" (boolean (get out :total-file-count-exact? true))
      "hits_truncated_by" (when (contains? #{:limit :bytes :time} (:truncated-by out))
                            (name (:truncated-by out)))
      "first_hit" (when (pos? (count hits))
                    (let [{:keys [path line]} (nth hits 0)]
                      (str path ":" line)))}))
  ([out needles _former-context] (content-result out needles)))

(defn- name-relevant-paths
  "Prune the fuzzy NAME list down to the paths a human would call a match.

   Fuzzy subsequence matching is deliberately generous, so a distinctive symbol
   query (`strip-schema-constraints`) also drags in a pile of unrelated files
   whose path merely contains the letters in order. When the SAME query already
   produced CONTENT hits, those hits are the real answer and the generous name
   list is pure noise — keep only paths whose lowercased path contains one whole
   needle contiguously. Returns `paths` unchanged when there is nothing to
   filter on."
  [paths needles]
  (let
    [terms (->> needles
                (keep #(some-> %
                               str
                               str/lower-case
                               not-empty))
                seq)]
    (if-not terms
      paths
      (vec (filter (fn [p]
                     (let [lp (str/lower-case (str p))]
                       (some #(str/includes? lp %) terms)))
                   paths)))))

(defn- regex-looking-query?
  "True when `query` carries rg-style regex syntax that CONTENT search will match
   LITERALLY. `grep` is a literal smart-case substring search, so a pattern like
   `defn-? +grep`, `foo.*bar` or `^ns\\b` silently returns zero content hits and
   the caller cannot tell a missing symbol from a wrong dialect — exactly the
   dead end that invites a pointless re-run with cosmetic edits.

   Deliberately conservative: `?`, `*`, `(` and `)` alone are ordinary code
   characters (`stale?`, `*warn-on-reflection*`, `(defn foo`) and never trip it."
  [query]
  (let [qs (if (coll? query) (map str query) [(str query)])]
    (boolean (some (fn [q]
                     (or (re-find #"\.[*+]" q)
                         (re-find #"\\[bdwsBDWS]" q)
                         (re-find #"\[\^" q)
                         (re-find #"\S[*+?]\s" q)
                         (str/includes? q "|")
                         (str/starts-with? q "^")
                         (str/ends-with? q "$")))
                   qs))))

(defn- grep-tool
  "Search file CONTENT and match file NAMES/PATHS in one call (bound as `grep`;
   `find_files`/`find` stay as compatibility aliases).

     await grep(\"render-grep-result\")
     await grep(\"channel_tui render\", {\"paths\": [\"src\"], \"limit\": 20})
     await grep([\"TODO\", \"FIXME\"], {\"include\": [\"**/*.clj\"], \"context\": 2})

   CONTENT matching is smart-case literal substring; a query list is OR. Every
   hit lands in the CANONICAL flat result under `matches` —
   `{path {\"lineno:hash\" {\"text\" … \"before\" [{\"line\" \"text\"}] \"after\" […]}}}`
   — each key a patch-ready anchor, `context` N adding the surrounding lines.
   Alongside it: `hit_count`, `file_count`, `file_counts`, `first_hit`, the
   ranked NAME matches in `paths`, `searched_paths` naming every physical root
   actually scanned (including the default `.` expansion), and `missing_paths`
   for a scope that does not exist (never silently absorbed) — every one of them
   TOTAL: present on every result, `[]`/`null` when there is nothing to report.
   A blank query is ls mode: it lists scoped files by frecency/recency without
   running CONTENT search or fuzzy query scoring.

   Truncation is explicit on BOTH axes: `truncated_by` covers the NAME list,
   `hits_truncated_by` (`limit`/`bytes`) names a capped CONTENT sweep and is
   `null` when the content result is complete. A query that reads
   like a regex but matches no content gets a `hint` saying so, since CONTENT
   matching never interprets regex syntax.

   For NAME matching, existing files normalize to their parent directory and
   missing paths to the nearest existing confined directory. CONTENT matching
   searches an existing file exactly as scoped and never widens it on zero hits;
   missing scopes are searched at their nearest existing directory and reported.
   NAME matching is fuzzy subsequence over the fff file index."
  [& args]
  (let
    [{:strs [query searched_paths limit item_count truncated_by] :as name-out}
     (find-search args)

     content-spec
     (find-args->content-spec args)

     ls?
     (str/blank? (str query))

     {:keys [needles]}
     (if ls? {:needles []} (coerce-rg-spec content-spec))

     ;; CONTENT search preserves the caller's exact scope. In particular, an
     ;; existing file that has zero hits must not silently widen to its parent.
     content-out
     (if ls? {:hits [] :total-file-count 0 :total-file-count-exact? true} (rg-search content-spec))

     content
     (content-result content-out needles)

     content-hits
     (long (or (get content "hit_count") 0))

     multiword?
     (> (count (str/split (str/trim (str query)) #"\s+")) 2)

     ;; When CONTENT matched, the fuzzy NAME list is noise (see
     ;; `name-relevant-paths`) — keep only the paths that really carry a needle.
     kept-paths
     (if (pos? content-hits)
       (name-relevant-paths (get name-out "paths") needles)
       (get name-out "paths"))

     ;; ONE FLAT canonical result: the content block is merged UP (no nested
     ;; `content` envelope, no `items` duplicate of `paths`, no `fuzzy`/
     ;; `matched_terms`/`item_count` internals) so `matches`/`hit_count`/`paths`
     ;; sit at the top level for the model, the renderer and the tests alike.
     out
     (cond->
       (-> name-out
           (dissoc "items" "fuzzy" "matched_terms" "item_count")
           ;; `hint` is TOTAL too: present on every grep result, nil when the
           ;; search has nothing to explain.
           (assoc "paths" kept-paths
                  "hint" nil)
           (merge content))
       (and (not ls?) (zero? (long (or item_count 0))) (zero? content-hits))
       (assoc "hint"
         (str
           "No file NAME or CONTENT matched \""
           query
           "\". "
           (if multiword?
             "Shorten to a single distinctive filename fragment or a real symbol/string that exists."
             "Try a different term, a real symbol/string, or widen the scope.")
           (when (regex-looking-query? query)
             " CONTENT matching is LITERAL smart-case substring — regex syntax is not interpreted; search a plain distinctive fragment.")))

       ;; Names matched but content did not, and the query reads like a regex:
       ;; say so, or the caller re-runs the same pattern with cosmetic edits.
       (and (not ls?)
            (pos? (long (or item_count 0)))
            (zero? content-hits)
            (regex-looking-query? query))
       (assoc "hint"
         (str
           "No CONTENT matched \"" query
           "\" — CONTENT matching is LITERAL smart-case substring, regex syntax is not interpreted. "
           "Search a plain distinctive fragment; only the file NAME matches in `paths` are real."))

       ;; The scan stopped at its wall-clock budget, so these results are
       ;; PARTIAL. Said LAST because it outranks every hint above: "nothing
       ;; matched" is a lie when the sweep never finished.
       (= "time" (get content "hits_truncated_by"))
       (assoc "hint"
         (str
           "Search stopped at its " (quot (long rg-search-budget-ms) 1000)
           "s scan budget — these results are PARTIAL, not the whole tree. "
           "Narrow `paths` to a subdirectory, add `include` globs, or search a more distinctive term.")))]

    (tool-success {:op :grep
                   :path (first searched_paths)
                   :kind :dir
                   :result out
                   :metadata {:query query
                              :paths searched_paths
                              :limit limit
                              :item-count (count kept-paths)
                              :hit-count content-hits
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
    (+ (count (str (:text hit))) (long (sum-lens (:before hit))) (long (sum-lens (:after hit))))))

(defn- search-file-content
  "Walk one file once, emit hits with optional context. Content-mode helper.
   Returns a vec of hit maps; an empty vec means no match. `:text` and the
   `:before`/`:after` context are kept FULL — the value is the model's data,
   sliceable in Python via `r[...]`; only the wire VIEW is bounded downstream.

   STREAMING, one line at a time. This used to slurp the file with
   `(vec (line-seq r))`, which pinned a whole multi-hundred-MB candidate in heap
   and offered NO cancellation point, so one huge file outlived both Esc and the
   tool timeout. `:before` now comes from a bounded ring of the last
   `before-ctx` lines and `:after` is filled in by hits still awaiting it, so
   memory is O(context), not O(file), and `check-interrupt!` lands mid-file."
  [^File f matches? before-ctx after-ctx]
  (try
    (let
      [path
       (rel-path f)

       before-ctx
       (long before-ctx)

       after-ctx
       (long after-ctx)

       want-before?
       (pos? before-ctx)

       want-after?
       (pos? after-ctx)]

      (with-open [r (io/reader f)]
        (loop
          [ls (line-seq r)
           i 0
           ;; ring of the last `before-ctx` [line-no text] pairs
           ring clojure.lang.PersistentQueue/EMPTY
           ;; hits whose :after window is not full yet, oldest first — hits
           ;; therefore still complete in line order
           pending []
           out (transient [])]

          (when (zero? (rem (long i) (long search-file-poll-lines))) (check-interrupt!))
          (if-some [line (first ls)]
            (let
              [line-no (inc (long i))
               ;; feed the open :after windows BEFORE this line can open its own
               fed (mapv (fn [h]
                           (cond-> h
                             (< (count (:after h)) after-ctx)
                             (update :after conj [line-no line])))
                         pending)
               done (filterv (fn [h]
                               (>= (count (:after h)) after-ctx))
                      fed)
               waiting (filterv (fn [h]
                                  (< (count (:after h)) after-ctx))
                         fed)
               out (reduce conj! out done)
               ;; FULL text here: the hit's :text feeds `patch/line-anchor` (the
               ;; patch hash must match the real file line). Display clipping
               ;; happens AFTER the anchor is computed (see rg-search).
               hit (when (matches? line)
                     (cond-> {:path path :line line-no :text line}
                       want-before?
                       (assoc :before (vec ring))

                       want-after?
                       (assoc :after [])))
               out (if (and hit (not want-after?)) (conj! out hit) out)
               waiting (if (and hit want-after?) (conj waiting hit) waiting)
               ring (if want-before?
                      (let [q (conj ring [line-no line])]
                        (if (> (count q) before-ctx) (pop q) q))
                      ring)]

              (recur (rest ls) (inc (long i)) ring waiting out))
            ;; EOF: hits whose :after window never filled ship short
            (persistent! (reduce conj! out pending))))))
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
   actual file scanning. `grep-tool` (= `grep`) wraps this with
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
    [{:keys [needles paths include is_hidden limit context is_files_only]}
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

     ;; `.rgignore` + the `:grep` config overlay (issue #23) — handed to fff
     ;; itself, see `fff-ignore-overlay`.
     search-overlay
     (fff-ignore-overlay)

     ;; fff-first, ALWAYS: fff OWNS discovery. It enumerates the correct universe (nested
     ;; `.gitignore`-aware — NEVER descends node_modules/target/…) and needle-narrows via
     ;; native grep, ~280× faster than a raw walk. `.gitignore` is ALWAYS respected by
     ;; fff's walker, and `.rgignore` / the `:grep` overlay ride along as fff's own
     ;; ignore overlay — so there is no raw-walk fallback left (that bypass cost 120s on
     ;; this workspace). fff surfaces dotfiles a walk hid by descent, so re-apply the
     ;; include globs + hidden-below-root guard here.
     candidates
     (->> (rg-fff-candidate-files roots needles search-overlay)
          (filter include-file?)
          (remove (fn [^File f]
                    (and (not is_hidden) (rg-hidden-below-root? roots f)))))

     ;; WALL-CLOCK budget for the scan below. Every other bound here is a COUNT,
     ;; which says nothing about time: before this, a pathological tree could run
     ;; to the 120 s native-tool kill and return nothing at all.
     deadline
     (+ (System/currentTimeMillis) (long rg-search-budget-ms))

     out-of-time?
     (fn []
       (>= (System/currentTimeMillis) (long deadline)))

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
                       (atom false)

                       time-capped?
                       (atom false)]

                      ;; the scan phase reads every candidate file — poll so Esc/timeout
                      ;; aborts mid-sweep. Past the display cap we keep short-circuit-probing
                      ;; candidate files to report TRUE breadth (`total_file_count`), bounded
                      ;; by `rg-breadth-probe-limit` so a hostile-needle full-tree scan (fff
                      ;; disabled) can't turn a truncated result into a whole-tree sweep.
                      (doseq
                        [^File f
                         files

                         :while (and (not @breadth-capped?) (not @time-capped?))]

                        (check-interrupt!)
                        (when (out-of-time?) (reset! time-capped? true))
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
                       :truncated-by (cond @capped? :limit
                                           @time-capped? :time
                                           :else :end-of-results)
                       :total-file-count @total-files
                       :total-file-count-exact? (and (not @breadth-capped?) (not @time-capped?))})
      :else
      (let
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
         (atom false)

         time-capped?
         (atom false)]

        (doseq
          [^File f
           files

           :while (and (not @breadth-capped?) (not @time-capped?))]

          ;; the scan phase reads every candidate file — poll so Esc/timeout
          ;; aborts mid-sweep
          (check-interrupt!)
          (when (out-of-time?) (reset! time-capped? true))
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
                          (>= (long @bytes-used) (long max-rg-result-bytes)) (reset! cap-reason
                                                                               :bytes))))))))
        {:hits (vec @out)
         :missing rg-missing-paths
         :truncated-by (or @cap-reason (when @time-capped? :time) :end-of-results)
         :total-file-count @total-files
         :total-file-count-exact? (and (not @breadth-capped?) (not @time-capped?))}))))

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
   A missing anchor, or an unknown key, throws.

   `normalize-edits-arg` runs first, so a batch that was stringified by a
   serializer (`patch(\"[{…}]\")`) or a lone edit map passed bare still arrives
   here as a vector instead of failing the call."
  [edits]
  (let [edits (normalize-edits-arg edits)]
    (when-not (sequential? edits)
      (throw (ex-info
               (if (string? edits)
                 (str "patch \"edits\" arrived as a STRING that is not a JSON list of edit maps: "
                      (subs edits 0 (min 120 (count edits)))
                      " — pass the real list: "
                      "[{\"path\": …, \"from_anchor\": \"12:ab3\", \"replace\": …}]")
                 "patch expects a vector of edit maps")
               {:type :ext.foundation.editing/invalid-patch-edits :got (type edits)})))
    (when (empty? edits)
      (throw (ex-info "patch expects at least one edit"
                      {:type :ext.foundation.editing/invalid-patch-edits :got edits})))
    (mapv (fn [edit]
            (when-not (map? edit)
              (throw (ex-info "patch edit must be a map"
                              {:type :ext.foundation.editing/invalid-patch-edit :edit edit})))
            (let
              [missing (seq (remove #(contains? edit %) patch-required-keys))
               unknown (seq (remove patch-allowed-keys (keys edit)))]

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
            ;; Generated callers can serialize an omitted optional anchor as "".
            ;; That is equivalent to omitting the single-line range endpoint.
            (cond-> (update edit "path" str)
              (= "" (get edit "to_anchor"))
              (dissoc "to_anchor")))
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
  (when (>= n (long patch-fail-loop-threshold))
    (str "Patch failed " n
         " times on " path
         ". Stop retrying: re-read the target once, then switch to struct_patch — it locates the"
         " definition by NAME (`target`), so no anchor can go stale.")))

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
                ;; Key the per-file snapshot by the RESOLVED relative path, never by the
                ;; caller's spelling: "a/b.clj", "./a/b.clj" and an absolute path all name
                ;; ONE file. Keying by the raw string split them into independent plans,
                ;; each spliced from the same original snapshot — so the last write won and
                ;; the other edits vanished while `patch` still reported success (and the
                ;; overlap + syntax guards ran on a partial view of the batch).
                current (or (get origs rel) (slurp file))
                origs (assoc origs rel current)
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
                          :edit-index idx
                          :from_anchor from_anchor}
                    check (assoc base-check :applied-positions [(:applied-line res)])]

                   (recur (inc idx)
                          (next remaining)
                          origs
                          (update spans rel (fnil conj []) span)
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
                   :spans sorted
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
  (or
    message
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
                         (let
                           [text (str (:current-text hash-error))
                            preview (if (> (count text) 80) (str (subs text 0 80) "…") text)]

                           (str "; line "
                                (:stated-line hash-error)
                                " changed — now `"
                                ca
                                "`: "
                                (pr-str preview)
                                ". Confirm this is your target before reusing the anchor."))
                         "; refresh the target with `cat`."))

        :hashline-misplaced
        (str head
             ": anchor says line "
             (:stated-line hash-error)
             " but matches line(s) "
             (pr-str (:found-lines hash-error))
             (if-let [ca (:current-anchor hash-error)]
               (str "; content moved — resend this edit as `" ca "` if that is your target.")
               "; refresh the target before editing."))

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
  (let [atomic "No changes (atomic): "]
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

(defn- non-failure-checks
  "Model-facing `:checks` minus the edits already reported in `:failures`.
   A failing edit's check IS its failure map (both are `conj`ed from the same
   value in `patch-analysis`), so shipping both repeats every stale-anchor
   diagnostic verbatim twice. What survives is the useful part: the edits that
   resolved cleanly and were still discarded by the atomic refusal."
  [checks failures]
  (let
    [key-of
     (juxt :path :edit-index)

     failed
     (into #{} (map key-of) failures)]

    (vec (remove (comp failed key-of) checks))))

(defn- patch-syntax-failures
  "Return plans that turn clean supported code into syntactically broken code.
   When the combined batch breaks a file, bisect it: apply the file's edits
   cumulatively bottom-up (offset-safe) and blame the FIRST edit that flips a
   clean parse to a broken one, instead of a hardcoded edit-index 0."
  [plans]
  (vec
    (keep
      (fn [{:keys [path before after spans]}]
        (when-let [lang (index/code-language path)]
          (when (and (not (zipper/syntax-broken? lang (str before)))
                     (zipper/syntax-broken? lang (str after)))
            (let
              [[culprit broken-content]
               (loop
                 [content (str before)
                  remaining (reverse spans)]

                 (when-let [{:keys [start end replacement] :as span} (first remaining)]
                   (let
                     [next-content
                      (str (subs content 0 (long start)) replacement (subs content (long end)))]
                     (if (zipper/syntax-broken? lang next-content)
                       [span next-content]
                       (recur next-content (rest remaining))))))
               culprit (or culprit (first spans))
               ;; LOCATE the fault, never just assert it: tree-sitter knows where the
               ;; parse broke and often which delimiter it expected. A bare "would break
               ;; syntax" is unactionable and indistinguishable from a parser artefact.
               detail (zipper/describe-syntax-errors lang (or broken-content (str after)))]

              {:edit-index (:edit-index culprit)
               :from_anchor (:from_anchor culprit)
               :path path
               :reason :syntax-error
               :message (str "edit "
                             (:edit-index culprit)
                             " would break syntax in "
                             path
                             ;; The report is multi-line on purpose: a run-on sentence of
                             ;; line/col numbers into a file that was never written is
                             ;; unreadable, on a phone most of all.
                             (when detail (str "\n" detail))
                             "\n  fix       the replacement's delimiters, or use struct_patch"
                             " (edits by structure, never by text).")}))))
      plans)))

(defn- commit-patch-plans!
  "Write validated plans and clear their retry counters."
  [plans]
  (doseq [{:keys [file after]} plans]
    (spit file after)
    (fff-index/note-fs-write!)
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
  ;; "allow_dirty" = the retired spelling of "is_dirty_ok"; still accepted so
  ;; older call sites keep working, but only `is_dirty_ok` is advertised.
  #{"expected_mtime" "expected_size" "is_overwrite" "atomic" "is_dirty_ok" "allow_dirty"})

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

     is_dirty_ok
     (boolean (if (contains? args "is_dirty_ok") (get args "is_dirty_ok") (get args "allow_dirty")))

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
                (and exists? (not is-dir?) (not is_dirty_ok) (git/file-dirty? file))
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
                               " first. Pass is_dirty_ok=True to overwrite on purpose.")}
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
             :failures [(assoc fail
                          :edit-index 0
                          :path rel
                          :consecutive-failures n)]
             :checks [(assoc fail
                        :edit-index 0
                        :path rel)]
             :loop-hint (patch-loop-hint n rel)
             :message (cond-> (:message fail)
                        (>= n (long patch-fail-loop-threshold))
                        (str "\n" (patch-loop-hint n rel)))})
          (do (ensure-parent-dirs! file)
              (spit file content)
              (fff-index/note-fs-write!)
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
     (fff-index/note-fs-write!)
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
     (fff-index/note-fs-write!)
     (rel-path dest-file))))

(defn- delete-if-exists-safe
  [path]
  (let [f (safe-path path)]
    (if (fs/exists? f)
      (do (if (fs/directory? f) (fs/delete-tree f) (fs/delete f)) (fff-index/note-fs-write!) true)
      false)))

(defn- exists-safe? [path] (fs/exists? (safe-path path)))

;; =============================================================================
;; Tool-result facades
;; =============================================================================

(defn- cat-result->model
  "Shape an internal read result into the MODEL-facing form: the internal
   `:lines` (a vec of `[ln text]` tuples) becomes the model's `:anchors` — an
   ordered `{anchor {\"text\" text}}` map (`patch/lines->anchor-map`, a line-ordered
   LinkedHashMap, the key IS the `patch :from_anchor`). The internal `:lines`
   tuple vector and the read-file `{ln anchor}` `:anchors` are both dropped.
   Top-level `anchors` is the ONE content field and already holds the union of
   every window's lines, so `ranges` window maps carry METADATA only — never a
   second copy of the text. The internal read pipeline keeps working on tuples;
   this is the single boundary where the model payload is built."
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

         (contains? m :note)
         (assoc "note" (:note m))

         (contains? m :range)
         (assoc "range" (:range m))))]
    ;; TOTAL: `ranges` ships on every read — nil for a single window, a vector of
    ;; window maps for a multi-window read — so the caller never key-probes. Their
    ;; `anchors` are DROPPED: they duplicated the top-level union verbatim and thus
    ;; doubled the payload of every ranged read. Window membership stays derivable
    ;; from `range` plus the line number in each anchor key.
    (assoc (->win out)
      "ranges" (when (seq (:ranges out)) (mapv #(dissoc (->win %) "anchors") (:ranges out))))))

(defn- normalize-cat-anchor-option
  "Accept the documented anchor shapes plus the common model mistakes: a
  JSON/EDN-looking anchor range passed as one quoted string (`\"[H1, H2]\"`), or
  TWO anchors comma-joined into one string (`\"H1, H2\"` / `\"9357, 9412\"`).
  Both mistakes become the real `[from to]` vector the caller expects."
  [anchor]
  (cond (and (string? anchor) (str/starts-with? (str/trim anchor) "["))
        (try (let [v (edn/read-string anchor)]
               (if (vector? v) v anchor))
             (catch Throwable _ anchor))
        (and (string? anchor) (str/includes? anchor ",")) (mapv str/trim (str/split anchor #","))
        :else anchor))

(defn- cat-anchor-line-number
  "A bare LINE NUMBER behind a mis-passed `anchor` → its 1-based long, else nil.
  Models routinely send a line number (the int `9357` or the string `\"9357\"`)
  where a `lineno:hash` anchor belongs. A real anchor carries a `:` separator
  (`\"9357:1a2\"`) and returns nil, so the caller falls through to the tolerant
  hash-addressed read instead of choking on `:hashline-malformed`."
  [x]
  (cond (integer? x) (long x)
        (and (string? x) (re-matches #"\s*\d+\s*" x)) (parse-long (str/trim x))
        :else nil))

(defn- cat-anchor->line-range
  "Coerce a mis-passed line-number `anchor` into an inclusive 1-based `[start end]`
  line range, or nil when ANY component is a real `lineno:hash` anchor. Accepts a
  lone number/`\"N\"` (→ that single line) or a `[from to]` vector — the shape
  `normalize-cat-anchor-option` yields for `[9357, 9412]` or `\"9357, 9412\"`."
  [anc]
  (let
    [items
     (if (vector? anc) anc [anc])

     nums
     (map cat-anchor-line-number items)]

    (when (and (seq items) (<= (count items) 2) (every? some? nums))
      [(long (first nums)) (long (last nums))])))

(defn- batch-path-specs
  "Normalize a BATCH argument — `cat`'s `files`, `ls`/`struct_index`'s `paths` —
   into ONE option map per read, in request order. An entry is either a plain path
   string — the call's shared options apply to it —
   or an object `{\"path\" \"…\", …}` whose OWN selectors (`ranges`, `anchor`,
   `tail`, …) override the shared ones, so a single call can read a DIFFERENT
   region of every file. `arg-key` is the CALLER's own array key, so a rejection
   quotes the key that tool actually accepts instead of another tool's. A
   malformed entry fails the whole call instead of silently dropping a path."
  [tool arg-key err-type shared entries]
  (when-not (and (sequential? entries) (seq entries))
    (throw (ex-info (str tool " `" arg-key "` must be a non-empty array of paths")
                    {:type err-type :got entries})))
  (mapv (fn [e]
          (let
            [p (cond (string? e) e
                     (map? e) (get e "path")
                     :else nil)]
            (when-not (and (string? p) (seq (str/trim p)))
              (throw (ex-info (str tool
                                   " `"
                                   arg-key
                                   "` entries must be a path string or a {\"path\": …} object")
                              {:type err-type :got e})))
            (assoc (merge shared (when (map? e) (dissoc e "path"))) "path" p)))
        entries))

(declare cat-tool)

(defn- cat-directory-error!
  "`cat` reads FILES. A directory path is a routing mistake, not a read: `ls`
   owns listings, so fail loudly with the exact replacement call instead of
   quietly doing another tool's job under this tool's name."
  [path]
  (throw (ex-info (str "`cat` reads files — `"
                       path
                       "` is a directory. Use `ls`: "
                       "ls({\"paths\": [\""
                       path
                       "\"]})")
                  {:type :ext.foundation.editing/cat-on-directory :path path})))

(defn- cat-one
  "Read a text-file window. `await cat(path)` reads the whole file (≤2000 lines)
   — slice only for bigger files or a middle/tail section. Options = a dict,
   snake_case keys:
     await cat(path, {\"ranges\": [[start, end], ...]})  # inclusive 1-based line window(s)
     await cat(path, {\"ranges\": [[-1, -1]]})           # the WHOLE file (opts out of shared ranges)
     await cat(path, {\"anchor\": \"325:0e3\"})      # one line by its lineno:hash anchor
     await cat(path, {\"anchor\": [\"H1\", \"H2\"]})   # inclusive anchor range H1..H2
     await cat(path, {\"tail\": 200})              # last N lines (omit N → 2000)
   Returns {\"anchors\": {\"lineno:hash\": {\"text\": line}, ...}, \"next_offset\", \"eof\",
   \"truncated\", \"mtime\", \"size\"}. \"anchors\" is the ONLY content key — an ORDERED
   {anchor: {\"text\": line}} map — MIRRORS rg's hit value, so read v[\"text\"]
   uniformly; there is NO top-level \"lines\"/\"content\" key (c[\"lines\"] KeyErrors).
   Each key IS the `patch` from_anchor — copy it straight into an edit.
   Not \"eof\"/\"truncated\" → paginate from \"next_offset\".
   A DIRECTORY path is a routing mistake, not a read: it throws and names the
   replacement call, because `ls` owns listings — `ls({\"paths\": [dir]})`, with
   `depth` / `is_hidden` there. A nil/blank path throws too; the batch form
   `cat({\"files\": [...]})` rejects blank entries before any read."
  ([path]
   (if (map? path)
     ;; All-kwargs form: `cat(path="p", ranges=rs)` collapses at the Python
     ;; boundary to ONE spec map `{ "path" "p", ...opts}`. A grep result is
     ;; also a map; when it identifies exactly one matched file, accept it as
     ;; the path directly rather than turning the missing `"path"` into a
     ;; blank-path failure.
     (let
       [spec
        path

        direct-path
        (get spec "path")

        match-paths
        (keys (get spec "matches"))

        inferred-path
        (when (= 1 (count match-paths)) (first match-paths))]

       (cat-tool (or direct-path inferred-path) (dissoc spec "path")))
     (let [f (safe-path path)]
       (if (.isDirectory f)
         (cat-directory-error! path)
         (let [out (read-file path 1 default-cat-limit)]
           (tool-success {:op :cat
                          :path path
                          :kind :file
                          :result (cat-result->model out)
                          :metadata {:next-offset (:next-offset out)
                                     :truncated? (:truncated? out)}}))))))
  ([path arg]
   (cond
     ;; Python-native form: a single options dict, e.g.
     ;;   cat(\"p\", {\"ranges\": [[1,5],[20,25]]})  # one or several windows
     ;;   cat(\"p\", {\"anchor\": A})                 cat(\"p\", {\"anchor\": [A1, A2]})
     ;;   cat(\"p\", {\"tail\": 100})                cat(\"p\", {})  -> whole file
     ;; Delegated to the keyword arities below so internal Clojure callers
     ;; (which pass bare keyword args) keep working unchanged.
     (map? arg)
     (let [f (safe-path path)]
       (if (.isDirectory f)
         (cat-directory-error! path)
         (let
           [raw-ranges (get arg "ranges")
            ;; Empty JSON arrays are a common optional-argument serialization
            ;; artifact. Treat them as absent so `cat(path, {\"ranges\": []})`
            ;; retains the safe default whole-file read rather than failing.
            ranges (when-not (and (sequential? raw-ranges) (empty? raw-ranges)) raw-ranges)
            anc (normalize-cat-anchor-option (get arg "anchor"))
            tail (get arg "tail")]

           (when (contains? arg "range")
             (throw (ex-info "cat accepts `ranges` only; use {\"ranges\": [[start, end]]}"
                             {:type :ext.foundation.editing/invalid-cat-args :got arg})))
           (cond ranges (cat-tool path :ranges ranges)
                 ;; A mis-passed line-number `anchor` (`9357`, `"9357"`,
                 ;; `[9357, 9412]`, or `"9357, 9412"`) reads as a line
                 ;; RANGE; real `lineno:hash` anchors fall through below.
                 (cat-anchor->line-range anc) (let [[s e] (cat-anchor->line-range anc)]
                                                (cat-tool path :range s e))
                 (vector? anc) (cat-tool path :anchor (first anc) (second anc))
                 (some? anc) (cat-tool path :anchor anc)
                 (integer? tail) (cat-tool path :tail tail)
                 (some? tail) (cat-tool path :tail)
                 :else (cat-tool path)))))
     (= arg :tail) (let [out (tail-file path default-cat-limit)]
                     (tool-success {:op :cat
                                    :path path
                                    :kind :file
                                    :result (cat-result->model out)
                                    :metadata {:next-offset (:next-offset out)
                                               :truncated? (:truncated? out)
                                               :tail? true}}))
     :else (throw (ex-info
                    "cat options must be a dict, e.g. cat(path, {\"ranges\": [[start, end]]})"
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
     ;; `[-1, -1]` is the explicit WHOLE-FILE sentinel — the way a batched read
     ;; keeps one file unsliced while its siblings share narrow windows.
     (if (cat-whole-file-ranges? n)
       (cat-one path)
       (let [out (read-file-ranges path n)]
         (tool-success {:op :cat
                        :path path
                        :kind :file
                        :result (cat-result->model out)
                        :metadata {:truncated? (:truncated? out)
                                   :ranges (mapv :range (:ranges out))}})))

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

     (throw (ex-info
              "cat options must use {\"tail\": N}, {\"ranges\": [[s, e], ...]}, or {\"anchor\": A}"
              {:type :ext.foundation.editing/invalid-cat-args :got arg}))))
  ([path mode start end]
   (case mode
     ;; (cat path :range start end) — INCLUSIVE start..end (both 1-based).
     :range
     (let
       [[s e]
        (coerce-cat-range start end)

        raw
        (read-file path s (inc (- (long e) (long s))))

        out
        (cond-> raw
          (not= [start end] [s e])
          (assoc :note (cat-range-note [start end] [s e])))]

       (tool-success
         {:op :cat
          :path path
          :kind :file
          :result (cat-result->model out)
          :metadata {:next-offset (:next-offset out) :truncated? (:truncated? out) :range [s e]}}))

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

(defn- cat-tool
  "Read independent files with `{\"files\": [...]}`, answering `{\"results\": [...]}`
   in request order. Shared `ranges` apply to every entry; a `{\"path\": \"…\",
   \"ranges\": [[start, end]]}` entry scopes one file differently."
  [& args]
  (let [a (first args)]
    (if (and (= 1 (count args)) (map? a) (contains? a "files"))
      (let
        [specs (batch-path-specs "cat"
                                 "files"
                                 :ext.foundation.editing/invalid-cat-args
                                 (dissoc a "files")
                                 (get a "files"))]
        (tool-success {:op :cat
                       :kind :file
                       :result {"results" (mapv (fn [spec]
                                                  (:result (cat-one (get spec "path")
                                                                    (dissoc spec "path"))))
                                                specs)}}))
      (apply cat-one args))))

(defn- ls-one
  "List ONE normalized `ls` spec. `ls` is the DIRECTORY tool, so a file path is a
   routing mistake and says so instead of returning a degenerate one-row tree.
   A path that does NOT exist reports the nearest EXISTING directory above it
   (`nearest-existing-dir`, the same climb `grep` uses for `missing_paths`), so an
   invented address — typically a filesystem path assembled from a language
   namespace, which is wrong in a workspace with many source roots — is recovered
   by listing that real directory instead of being guessed a second time."
  [spec]
  (let
    [path
     (get spec "path")

     ^File f
     (safe-path path)]

    (when-not (.exists f)
      (let
        [near (some-> (nearest-existing-dir f)
                      rel-path)]
        (throw (ex-info (str "`ls`: no such path `" path
                             "`" (when near
                                   (str
                                     " \u2014 nearest existing directory is `" near
                                     "`. List that, or `grep` the name: a language namespace is"
                                     " not a path \u2014 this workspace has many source roots.")))
                        (cond-> {:type :ext.foundation.editing/invalid-ls-args :path path}
                          near
                          (assoc :nearest near))))))
    (when-not (.isDirectory f)
      (throw (ex-info (str "`ls` lists directories \u2014 `"
                           path
                           "` is a file. Use `cat`: "
                           "cat({\"files\": [\""
                           path
                           "\"]})")
                      {:type :ext.foundation.editing/ls-on-file :path path})))
    (list-dir f {:depth (or (get spec "depth") 1) :is_hidden (boolean (get spec "is_hidden"))})))

(defn- ls-tool
  "List directories: `{\"paths\": [dir, ...]}` answers `{\"results\": [...]}` in request
   order. Shared `depth`/`is_hidden` apply to every entry; a
   `{\"path\": \"...\", \"depth\": 2}` entry overrides them for that one directory."
  [& args]
  (let
    [a
     (first args)

     m
     (cond (map? a) a
           (string? a) (assoc (if (map? (second args)) (second args) {}) "paths" [a])
           :else nil)

     _
     (when-not (map? m)
       (throw (ex-info "`ls` takes {\"paths\": [dir, ...]} or a single path string"
                       {:type :ext.foundation.editing/invalid-ls-args :got a})))

     entries
     (or (get m "paths")
         (when-let [p (get m "path")]
           [p]))

     specs
     (batch-path-specs "ls"
                       "paths"
                       :ext.foundation.editing/invalid-ls-args
                       (dissoc m "paths" "path")
                       entries)

     rows
     (mapv ls-one specs)]

    (tool-success {:op :ls
                   :path (get (first specs) "path")
                   :kind :dir
                   :result {"results" rows}
                   :metadata {:dir? true
                              :entry-count (reduce + 0 (map #(count (get % "entries")) rows))}})))


(def ^:private ^:const patch-diff-context-lines 3)

(def ^:private ^:const patch-diff-max-render-lines 240)

(def ^:private ^:const patch-java-diff-max-work 20000000)

(defn- estimated-diff-size
  "Cheap O(n) LOWER BOUND on the Myers edit-script length: the size of the
   symmetric multiset difference of the two line bags. Sparse edits in a huge
   file score tiny; a full rewrite scores about n+m."
  [a b]
  (let
    [counts (reduce (fn [m line]
                      (update m line (fnil dec 0)))
                    (frequencies a)
                    b)]
    (long (reduce + 0 (map #(abs (long %)) (vals counts))))))

(defn- java-diff-affordable?
  "Whether `java-diff-utils` may diff this pair. Myers costs about O(n*d), so a
   flat line-count cap punished a huge file with a one-line edit (the cheapest
   case) while waving through an expensive full rewrite just under the cap.
   Budget the actual work instead."
  [a b]
  (let
    [size
     (long (max (count a) (count b)))

     edits
     (max 1 (long (estimated-diff-size a b)))]

    (<= (* size edits) (long patch-java-diff-max-work))))

(def ^:private ^:const patch-diff-min-hunk-lines 14)

(defn- head-tail-cap
  "Bound a line vector to `limit`, keeping a HEAD and a TAIL window rather than a
   plain head-cut. A pure head-cut let a deletion-heavy preview fill the whole
   visible budget with `-` lines and bury the `+` replacement below the cut, so a
   correct edit read as a catastrophic deletion."
  [lines ^long limit]
  (let
    [lines
     (vec lines)

     n
     (long (count lines))]

    (if (<= n limit)
      lines
      (let
        [tail-n
         (quot limit 4)

         head-n
         (- limit tail-n)

         omitted
         (- n head-n tail-n)]

        (vec (concat (subvec lines 0 head-n)
                     [(str "... diff truncated; " omitted " line(s) omitted")]
                     (subvec lines (- n tail-n))))))))

(defn- hunk-header? [line] (str/starts-with? (str line) "@@"))

(defn- split-diff-hunks
  "Split a unified diff into `[preamble hunks]`, each hunk a vector whose first
   element is its own `@@` header. A diff with no `@@` (pure add/delete preview)
   yields no hunks."
  [lines]
  (let
    [lines
     (vec lines)

     start
     (or (first (keep-indexed (fn [idx line]
                                (when (hunk-header? line) idx))
                              lines))
         (count lines))]

    [(subvec lines 0 start)
     (reduce (fn [hunks line]
               (if (hunk-header? line)
                 (conj hunks [line])
                 (cond-> hunks
                   (seq hunks)
                   (update (dec (count hunks)) conj line))))
             []
             (subvec lines start))]))

(defn- cap-hunk-lines
  "Bound ONE hunk to `budget` lines, ALWAYS keeping its `@@` header plus a head
   and a tail of its own body, so the hunk still reads as one connected region
   instead of two unrelated fragments."
  [hunk ^long budget]
  (let [n (long (count hunk))]
    (if (<= n budget)
      (vec hunk)
      (let
        [body (max 2 (dec budget))
         tail-n (max 1 (quot body 3))
         head-n (max 1 (- body tail-n))
         omitted (- n 1 head-n tail-n)]

        (if (pos? omitted)
          (vec (concat (subvec hunk 0 (inc head-n))
                       [(str "... " omitted " line(s) omitted in this hunk")]
                       (subvec hunk (- n tail-n))))
          (vec hunk))))))

(defn- cap-diff-lines
  "Bound a rendered diff to `patch-diff-max-render-lines` HUNK-WISE. Cutting the
   diff as one flat line list sliced through the middle of a hunk, so on a narrow
   screen the surviving head and tail looked like edits to two unrelated places.
   Each hunk keeps its own header, head and tail; whole hunks past the budget are
   dropped with an explicit count rather than half-shown."
  [lines]
  (let
    [lines
     (vec lines)

     n
     (long (count lines))]

    (if (<= n patch-diff-max-render-lines)
      lines
      (let [[preamble hunks] (split-diff-hunks lines)]
        (if (empty? hunks)
          (head-tail-cap lines patch-diff-max-render-lines)
          (let
            [budget (max patch-diff-min-hunk-lines
                         (- patch-diff-max-render-lines (count preamble) 1))
             ;; Fill the budget hunk by hunk: a hunk is shown whole when it
             ;; fits, capped in place when a usable remainder is left, and the
             ;; rest are reported as a count instead of being half-rendered.
             [kept dropped]
             (loop
               [pending hunks
                used 0
                kept []]

               (if-let [hunk (first pending)]
                 (let [remaining (- budget used)]
                   (cond (<= (count hunk) remaining)
                         (recur (next pending) (+ used (count hunk)) (into kept hunk))
                         (>= remaining patch-diff-min-hunk-lines)
                         (let [capped (cap-hunk-lines hunk remaining)]
                           (recur (next pending) (+ used (count capped)) (into kept capped)))
                         :else [kept (count pending)]))
                 [kept 0]))]

            (vec (concat preamble
                         kept
                         (when (pos? (long dropped))
                           [(str "... diff truncated; " dropped " more hunk(s) omitted")])))))))))

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

    (vec (concat (when (pos? before-skip) [(str "... " before-skip " unchanged line(s) before")])
                 (map #(str " " %) pre-lines)
                 (prefixed-diff-lines "-" del-lines)
                 (prefixed-diff-lines "+" add-lines)
                 (map #(str " " %) post-lines)
                 (when (pos? after-skip) [(str "... " after-skip " unchanged line(s) after")])))))

(defn- java-unified-diff-lines
  "Unified hunks WITHOUT the `--- before` / `+++ after` file header pair: every
   renderer (TUI, companion app) already shows the path and colours each line by
   its `-`/`+` prefix, so the two header lines only ate screen space."
  [a b]
  (let
    [patch
     (DiffUtils/diff a b)

     lines
     (vec (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a patch patch-diff-context-lines))]

    (if (and (>= (count lines) 2)
             (str/starts-with? (str (nth lines 0)) "---")
             (str/starts-with? (str (nth lines 1)) "+++"))
      (subvec lines 2)
      lines)))

(def ^:private diff-hunk-header-re #"^@@ -(\d+)(,\d+)? \+(\d+)(,\d+)? @@(.*)$")

(defn- shift-hunk-headers
  "Renumber `@@` headers produced from a WINDOW of the file back to real file
   line numbers."
  [lines ^long offset]
  (mapv (fn [line]
          (if-let [[_ a a-len b b-len trailing] (re-matches diff-hunk-header-re (str line))]
            (str "@@ -"
                 (+ offset (long (parse-long a)))
                 a-len
                 " +"
                 (+ offset (long (parse-long b)))
                 b-len
                 " @@"
                 trailing)
            line))
        lines))

(defn- windowed-unified-diff-lines
  "REAL unified hunks, at real file line numbers, for ANY file size. A huge file
   with a few small edits used to fall back to one flat delete-block plus one
   add-block spanning everything between the first and the last change, so a
   two-line edit rendered as hundreds of `-` lines of untouched code —
   disconnected nonsense on a narrow screen. Trim the shared prefix/suffix (minus
   context), diff only the changed window, and renumber the `@@` headers back to
   file lines. nil when even that window is too expensive to diff."
  [a b]
  (let
    [prefix-count
     (long (common-prefix-count a b))

     suffix-count
     (long (common-suffix-count a b prefix-count))

     start
     (max 0 (- prefix-count patch-diff-context-lines))

     a-end
     (min (count a) (+ (- (count a) suffix-count) patch-diff-context-lines))

     b-end
     (min (count b) (+ (- (count b) suffix-count) patch-diff-context-lines))]

    (when (and (<= start a-end) (<= start b-end))
      (let
        [a-win
         (subvec a start a-end)

         b-win
         (subvec b start b-end)]

        (when (java-diff-affordable? a-win b-win)
          (shift-hunk-headers (java-unified-diff-lines a-win b-win) start))))))

(defn- whole-file-rewrite?
  "True when NOTHING of the old content survives: no shared leading line and no
   shared trailing line. A real unified diff then degenerates into every old
   line as `-` immediately followed by every new line as `+` — the same file
   twice, with zero signal about what changed, because everything changed."
  [a b]
  (and (seq a)
       (seq b)
       (zero? (long (common-prefix-count a b)))
       (zero? (long (common-suffix-count a b 0)))))

(defn- unified-diff-text
  "Unified diff preview for two file blobs: real `@@` hunks at real file line
   numbers, bounded hunk-wise. Only a change too expensive to diff (a full
   rewrite of a very large file) drops to the linear bounded preview.

   A WHOLE-FILE REWRITE (`write` replacing a file, a full-body `struct_patch` —
   nothing of the old content survives) renders ONE side only: the new content
   as `+` lines under a `--- (replaced, N line(s))` marker. Both sides there
   printed the file twice on every renderer (TUI and companion app read this
   same `\"diff\"` string), so the fix belongs here, not in the renderers."
  [before after]
  (cond (= before after) nil
        (nil? before) (str/join "\n" (prefixed-diff-lines "+" (str/split-lines (or after ""))))
        (nil? after) (str "--- (deleted, " (count (str/split-lines (or before ""))) " lines)")
        :else (let
                [a
                 (vec (str/split-lines before))

                 b
                 (vec (str/split-lines after))]

                (if (whole-file-rewrite? a b)
                  (str/join "\n"
                            (into [(str "--- (replaced, " (count a) " line(s))")]
                                  (prefixed-diff-lines "+" b)))
                  (str/join "\n"
                            (cap-diff-lines (or (windowed-unified-diff-lines a b)
                                                (compact-diff-lines a b))))))))

(def ^:private fresh-anchor-max-lines
  "Cap on how many post-edit lines a patch/write result hands back as fresh
   `lineno:hash` anchors. A surgical edit rewrites a handful of lines, and
   mirroring them back lets the SAME file be edited again with no second read.
   A whole-file rewrite is not worth mirroring (the caller already holds what it
   sent), so past the cap the key is simply omitted and the caller re-reads."
  60)

(defn- changed-region-anchors
  "Fresh `{anchor {\"text\" line}}` for the region `after` carries where it differs
   from `before` — the SAME payload shape `cat` returns, so the result of one
   patch is directly reusable as the next edit's `:from_anchor` without a
   re-read. The window is the span between the common prefix and common suffix of
   the two versions (a superset of the touched hunks, exact for a single hunk).
   nil when nothing changed or the window is wider than `fresh-anchor-max-lines`."
  [before after]
  (when (and (string? before) (string? after) (not= before after))
    (let
      [a
       (patch/split-content-lines before)

       b
       (patch/split-content-lines after)

       na
       (long (count a))

       nb
       (long (count b))

       pre
       (long (loop [i 0]
               (if (and (< i na) (< i nb) (= (nth a i) (nth b i))) (recur (inc i)) i)))

       suf
       (long (loop [i 0]
               (if (and (< i (- na pre)) (< i (- nb pre)) (= (nth a (- na 1 i)) (nth b (- nb 1 i))))
                 (recur (inc i))
                 i)))

       end
       (- nb suf)]

      (when (and (< pre end) (<= (- end pre) (long fresh-anchor-max-lines)))
        (patch/lines->anchor-map (mapv (fn [i]
                                         [(inc (long i)) (nth b i)])
                                       (range pre end)))))))

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
  (let
    [diff-text
     (unified-diff-text before after)

     anchors
     (changed-region-anchors before after)]

    (cond-> {"path" path "op" (name (or op :update)) "changed" (not= before after)}
      diff-text
      (assoc "diff" diff-text)

      anchors
      (assoc "anchors" anchors))))

(defn refresh-file-summary
  "Recompute a per-file summary's \"diff\"/\"changed\" from the ORIGINAL `before`
   and the FINAL on-disk `after`. A language pack that rewrites a just-edited
   file in an :after op-hook (parinfer paren-repair + cljfmt) calls this so the
   MODEL-FACING diff shows the bytes actually written, not the pre-hook
   intermediate the raw edit produced. All other summary keys are preserved."
  [summary before after]
  (let
    [diff-text
     (unified-diff-text before after)

     anchors
     (changed-region-anchors before after)]

    (cond-> (assoc summary "changed" (not= before after))
      diff-text
      (assoc "diff" diff-text)

      (nil? diff-text)
      (dissoc "diff")

      anchors
      (assoc "anchors" anchors)

      (nil? anchors)
      (dissoc "anchors"))))

(defn- patch-tool
  "Edit files by anchor (no text search/replace).

   Each `lineno:hash` comes from a fresh `cat`, `grep`, or `struct_index` read.
   Anchors re-resolve against live content: unrelated changes are preserved when
   the targets still match; changed targets abort the entire atomic batch. Omit
   `to_anchor` for one line, use an inclusive range otherwise, and use an empty
   replacement to delete. On success each summary carries fresh `anchors` for the
   region it just rewrote — reuse them to edit that file again without re-reading."
  [edits]
  ;; All-kwargs form: `patch(edits=[...])` collapses at the Python boundary to ONE
  ;; spec map `{"edits" [...]}` (see __vis_exec_call__). Unwrap it — mirrors cat/rg.
  (let
    [edits
     (if (and (map? edits) (contains? edits "edits")) (get edits "edits") edits)

     result
     (patch-safe edits)]

    (if (:success? result)
      (let
        [plans
         (:plans result)

         summaries
         (mapv patch-result-file-summary plans)]

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
      (let
        [first-failure
         (first (:failures result))

         other-checks
         (non-failure-checks (:checks result) (:failures result))]

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
           ;; Only facts the message does NOT already carry: nil `:loop-hint`
           ;; and failure-duplicating `:checks` are pure payload noise.
           :error (cond->
                    {:message (:message result)
                     :reason (:reason first-failure)
                     :failures (:failures result)}
                    (seq other-checks)
                    (assoc :checks other-checks)

                    (some? (:loop-hint result))
                    (assoc :loop-hint (:loop-hint result)))})))))

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
   for NEW files and clean overwrites. Override with is_dirty_ok=True."
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
      (let
        [first-failure (first (:failures result))
         other-checks (non-failure-checks (:checks result) (:failures result))]

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
           :error (cond->
                    {:message (:message result)
                     :reason (:reason first-failure)
                     :failures (:failures result)
                     :mode :write}
                    (seq other-checks)
                    (assoc :checks other-checks)

                    (some? (:loop-hint result))
                    (assoc :loop-hint (:loop-hint result)))})))))

(defn- create-dirs-tool
  "Ensure dir exists. Returns the canonical foundation map shape so the
   model destructures `(:path r)` / `(:created? r)` directly off the
   bound result."
  [path]
  (let
    ;; All-kwargs `create_dirs(path="p")` collapses to ONE map `{"path" "p"}`; unwrap it.
    [path
     (if (map? path) (get path "path") path)

     before
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
  ;; All-kwargs `copy(src="a", dest="b")` collapses to ONE map — unwrap to the 3-arity.
  ([m] (copy-tool (get m "src") (get m "dest") (dissoc m "src" "dest")))
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
  ;; All-kwargs `move(src="a", dest="b")` collapses to ONE map — unwrap to the 3-arity.
  ([m] (move-tool (get m "src") (get m "dest") (dissoc m "src" "dest")))
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

   Returns {\"path\": path, \"deleted\": bool}. A missing path is NEVER an error:
   \"deleted\" is False when nothing was there (folds in the old delete_if_exists
   and the old is_missing_ok flag)."
  [path]
  (let
    [path
     (if (map? path) (get path "path") path)

     deleted?
     (delete-if-exists-safe path)]

    (tool-success {:op :delete
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
  (let
    [path
     (if (map? path) (get path "path") path)

     exists?
     (exists-safe? path)]

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
  "One `index/definitions` entry → snake_case wire map. It is the definition row in
   `struct_index`'s per-file `results` and mirrors the corresponding declaration in
   its `occurrences` group (`kind`/`visibility`/`signature`/`doc`/`anchor`/
   `end_anchor`), plus `name` and nesting `depth` (0 = top-level). Nil fields are
   dropped to keep the row lean."
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

(defn- index-one
  "Index one normalized path specification for the paths-only `struct_index` tool.
   Its result becomes one row in the tool's `results` vector."
  [spec]
  (let
    [path
     (get spec "path")

     ranges
     (get spec "ranges")

     ;; cat's whole-file sentinel means "index the WHOLE file" here too, so one
     ;; batched path can opt out of the call's shared `ranges`.
     whole-file?
     (cat-whole-file-ranges? ranges)

     ;; Coerced by cat's own normalizer so `struct_index` accepts every `ranges`
     ;; shape `cat` does and CORRECTS the same sloppy ones — a reversed pair or a
     ;; HALF sentinel like `[[-1, 60]]` normalizes to `[[1, 60]]` rather than
     ;; indexing a nonsense window. Only nil/empty means "no windows"; a
     ;; non-collection scalar (`3`) is FORWARDED so cat's guidance is thrown
     ;; instead of a raw `Don't know how to create ISeq from Long`.
     windows
     (when (and (some? ranges) (not whole-file?) (not (and (coll? ranges) (empty? ranges))))
       (normalize-cat-ranges ranges))

     ;; Resolve through safe-path (workspace-cwd confinement) like every other
     ;; file tool — file-index's internal slurp must not receive a raw relative
     ;; path, whose base would be the JVM user.dir rather than the workspace.
     f
     (ensure-existing-file! (safe-path path))

     abs
     (.getPath f)

     idx
     (index/file-index abs (slurp f) windows)

     language
     (index/detect-language abs)]

    (tool-success
      {:op :struct_index
       :path path
       :kind :file
       ;; TOTAL: an unindexed language is a note next to nil structure, never a
       ;; differently shaped response.
       :result (let
                 [base {"skeleton" nil
                        "definitions" []
                        "imports" []
                        "language" nil
                        "line_count" nil
                        "path" path
                        "ranges" windows
                        "note" nil}]
                 (cond idx (assoc base
                             "skeleton" (:skeleton idx)
                             "definitions" (mapv def->wire (:definitions idx))
                             "imports" (mapv import->wire (:imports idx))
                             "language" (:language idx)
                             "line_count" (:line-count idx))
                       ;; A detected language with a nil index is AMBIGUOUS: the pack
                       ;; may have no structure intel for it, OR the file may simply
                       ;; hold no top-level definition and no import (a Go file that is
                       ;; only a `package` clause plus directives indexes to nil). Say
                       ;; both — blaming the language sends the caller away from
                       ;; struct_index for a language it fully supports.
                       language
                       (assoc base
                         "language" language
                         "note"
                         (str "No top-level definitions or imports here — the file may hold "
                              "none, or the language has no structural index yet. Use cat(path)."))
                       :else (assoc base "note" "Unknown language — use cat(path).")))})))

(declare occurrences-data occurrence->wire symbol-rename-tool)

(defn- index-tool
  "Index source files with one paths-only contract. Results preserve request order.
   Set `include_occurrences` to true to additionally trace each declared identifier
   across the supplied files. Shared ranges apply to every entry, while entry-level
   ranges scope one file differently."
  [args]
  (let
    [include-occurrences?
     (true? (get args "include_occurrences"))

     invalid-args?
     (or (not (map? args))
         (not (contains? args "paths"))
         (not-every? #{"paths" "ranges" "include_occurrences"} (keys args))
         (and (contains? args "include_occurrences")
              (not (boolean? (get args "include_occurrences"))))
         (some (fn [entry]
                 (and (map? entry) (not-every? #{"path" "ranges"} (keys entry))))
               (get args "paths")))]

    (when invalid-args?
      (throw
        (ex-info
          "struct_index accepts only `paths`, optional `ranges`, and optional boolean `include_occurrences`."
          {:type :ext.foundation.editing/invalid-index-args :got args})))
    (let
      [entries
       (get args "paths")

       specs
       (batch-path-specs "struct_index"
                         "paths"
                         :ext.foundation.editing/invalid-index-args
                         (dissoc args "paths" "include_occurrences")
                         entries)

       results
       (structural/scan-mapv #(:result (index-one %)) specs)

       result
       (cond-> {"results" results}
         include-occurrences?
         (assoc "occurrences"
           (let
             [paths
              (->> specs
                   (map #(get % "path"))
                   distinct
                   vec)

              names
              (->> results
                   (mapcat #(get % "definitions"))
                   (keep #(get % "name"))
                   distinct
                   vec)

              ;; The tree-sitter layer owns the read + parse fan-out: one read
              ;; and ONE parse per file for the WHOLE name set, per-file
              ;; failures preserved. This side only confines the path and
              ;; TRANSPOSES the non-empty name/file pairs — a dense name × path
              ;; regrouping did 678k lookups to consume 8k pairs on Vis.
              scans
              (structural/occurrences-in-files paths names #(slurp (safe-path %)))

              failed
              (into []
                    (keep (fn [{:keys [path error]}]
                            (when error {"path" path "error" error})))
                    scans)

              by-name
              (reduce (fn [acc {:keys [path occurrences]}]
                        (reduce-kv (fn [m name occ]
                                     (update m
                                             name
                                             (fnil conj [])
                                             {"path" path
                                              "occurrences" (mapv #(occurrence->wire name %) occ)}))
                                   acc
                                   occurrences))
                      {}
                      scans)]

             (mapv #(occurrences-data % paths (get by-name % []) failed) names))))]

      (tool-success {:op :struct_index :kind :file :result result}))))


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

(defn- batch-paths-summary
  "Headline for a BATCH of paths: `dir/{a.clj, b.clj}` when the paths share a
   directory, else a comma-joined list — so ONE card says exactly which files the
   batched call touched without repeating the common prefix. Long batches show
   the first three plus `+N more`."
  [paths]
  (let
    [ps
     (->> paths
          (keep identity)
          (mapv (comp str disp-path)))

     segs
     (mapv #(vec (str/split % #"/")) ps)

     ;; Longest shared DIRECTORY prefix (the file name never joins it).
     common
     (if (< (count ps) 2)
       []
       (let [heads (mapv (comp vec butlast) segs)]
         (loop
           [i 0
            acc []]

           (let [col (mapv #(get % i) heads)]
             (if (and (every? some? col) (apply = col))
               (recur (inc i) (conj acc (first col)))
               acc)))))

     rel
     (mapv #(str/join "/" (drop (count common) %)) segs)

     shown
     (vec (take 3 rel))

     more
     (- (count rel) (count shown))

     listed
     (str (str/join ", " shown) (when (pos? more) (str ", +" more " more")))]

    (cond (empty? ps) ""
          (= 1 (count ps)) (str "`" (first ps) "`")
          (seq common) (str "`" (str/join "/" common) "/{" listed "}`")
          :else (str "`" listed "`"))))

(defn- render-ls-result
  "ONE `ls` listing → `{:summary :body}`: the summary is the dir path + entry
   count; the body lists entries one per row, `name/` for subdirs, two-space
   indent per nested level. `r` is `{\"path\" \"entries\" \"depth\"}`."
  [r]
  (let
    [entries
     (get r "entries")

     rows
     (fn rows [indent es]
       (mapcat (fn [e]
                 (let
                   [dir?
                    (= "dir" (get e "type"))

                    nm
                    (str indent (get e "name") (when dir? "/"))]

                   (cons nm (rows (str indent "  ") (get e "children")))))
               es))

     body
     (str/join "\n" (rows "" entries))

     n
     (count entries)]

    {:summary (str "`" (disp-path (get r "path")) "/` · " n " " (if (= 1 n) "entry" "entries"))
     :body (when (seq entries) (str "\n" (strutil/fenced body)))}))

(defn- render-cat-one
  "cat, ONE path → `{:summary :body}`: the summary is the path + the LINE SPANS read +
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
     (str n " line" (when (not= 1 n) "s"))

     ;; Fence the numbered slice with the file's code language. The TUI strips
     ;; the line-number gutter before parsing, then restores it uncolored.
     lang
     (index/code-language (get r "path"))]

    {:summary (str "`" (disp-path (get r "path"))
                   "` · " (cond (nil? loc) counted
                                (= 1 (count spans)) loc
                                :else (str loc " · " counted)))
     :body (when (seq rows)
             (let
               [joined
                (str/join "\n" rows)

                fenced
                (strutil/fenced joined lang)]

               (str "\n" fenced)))}))

(defn- render-ls-results
  "ls → `{:summary :body}`. One listed directory renders its own card; a BATCH
   renders a shared headline over each directory's own section, in request order."
  [r]
  (let [results (vec (get r "results"))]
    (if (= 1 (count results))
      (render-ls-result (first results))
      {:summary
       (str (batch-paths-summary (map #(get % "path") results)) " · " (count results) " dirs")
       :body (str/join "\n\n"
                       (map (fn [x]
                              (let [{:keys [summary body]} (render-ls-result x)]
                                (str "### " summary (or body ""))))
                            results))})))

(defn- render-cat-result
  "cat → `{:summary :body}`. A single path renders its own card; a BATCH renders a
   `dir/{a, b}` headline over each file's own section, preserving request order."
  [r]
  (if-let [results (seq (get r "results"))]
    (let
      [rendered (mapv render-cat-one results)
       files (count results)
       lines (reduce + (map #(count (get % "anchors")) results))]

      {:summary (str (batch-paths-summary (map #(get % "path") results))
                     " · "
                     files
                     " file"
                     (when (not= 1 files) "s")
                     (when (pos? (long lines)) (str " · " lines " line" (when (not= 1 lines) "s"))))
       :body (str/join "\n\n"
                       (map (fn [{:keys [summary body]}]
                              (str "### " summary (or body "")))
                            rendered))})
    (render-cat-one r)))

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

(defn- md-inline-code
  "CommonMark-safe inline code span for `s`. A naive `` `s` `` breaks when `s`
   itself contains backticks: the inner backtick closes the span early and
   corrupts every following span on the line. Pick a fence one longer than the
   longest backtick run in `s`, and pad when `s` starts/ends with a backtick
   (CommonMark strips a single symmetric leading+trailing space), so the term
   renders as ONE clean chip."
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

(defn- needle-re
  "Literal regex fragment for ONE OR `needle`, honoring the smart-case rule of
   `make-line-matcher`: an all-lowercase needle matches any case (scoped inside
   `(?i:…)` so its case-insensitivity can't leak into a sibling alternative);
   one carrying an uppercase letter stays exact. `Pattern/quote` makes
   metacharacters literal."
  [needle]
  (let [quoted (java.util.regex.Pattern/quote needle)]
    (if (re-find #"[A-Z]" needle) quoted (str "(?i:" quoted ")"))))

(defn- highlight-needles
  "Wrap every occurrence of any OR `needle` in `text` with reverse-video SGR
   (\u001B[7m … \u001B[0m) so the TUI's `paint-ansi-line!` maps the code to a
   highlight fg when it paints the row. One non-overlapping pass, longest needle
   first, so a
   short needle can't re-wrap a longer one's match."
  [needles ^String text]
  (if (or (not (seq needles)) (str/blank? text))
    text
    (let
      [frags (into []
                   (keep (fn [n]
                           (when (seq n) (try (needle-re n) (catch Exception _ nil)))))
                   (sort-by #(- (count %)) needles))]
      (if (seq frags)
        (str/replace text
                     (re-pattern (str/join "|" frags))
                     (fn [m]
                       (str "\u001B[7m" m "\u001B[0m")))
        text))))

(defn- hit-context-rows
  "The `before`/`after` context entries of one hit as `[lineno text]` pairs.
   Canonical shape is a vector of `{\"line\" n \"text\" s}` maps; a legacy
   `{anchor {\"text\" s}}` map is tolerated so an older result still renders."
  [ctx]
  (cond (sequential? ctx) (mapv (fn [c]
                                  [(str (get c "line")) (str (get c "text"))])
                                ctx)
        (map? ctx) (mapv (fn [[k v]]
                           [(rg-anchor-lineno k) (str (patch/anchor-value-text v))])
                         (sort-by (comp rg-anchor-lineno-long key) ctx))
        :else []))

(defn- hit-gutter-width
  "Widest line-number column across a file's hits AND their context lines, so
   every gutter row in the block right-aligns. Mixed 1- and 4-digit line numbers
   otherwise stagger the text column."
  ^long [hits]
  (reduce (fn [^long w [k v]]
            (let
              [ctx (when (map? v)
                     (concat (hit-context-rows (get v "before"))
                             (hit-context-rows (get v "after"))))]
              (reduce (fn [^long w2 [ln _]]
                        (max w2 (count (str ln))))
                      (max w (count (rg-anchor-lineno k)))
                      ctx)))
          1
          hits))

(defn- gutter-row
  "One `  <lineno>  <text>` gutter row, the line number right-aligned to `width`
   (the file's widest). Any OR `needle` occurrence in the text is wrapped for
   highlight (see `highlight-needles`)."
  [needles ^long width lineno txt]
  (str "  " (format (str "%" width "s") (str lineno))
       "  " (str/trimr (highlight-needles needles (str txt)))))

(defn- hit-line-entries
  "One match anchor `k` → value `v` (`{\"text\" … \"before\" […] \"after\" […]}`,
   before/after only with a context window) as `[[lineno text match?] …]` with
   NUMERIC line numbers: the before-context, the matched line, then the
   after-context. A bare string `v` is tolerated as the lone matched line.
   Numeric lines are what lets a file's hits merge into one ordered block."
  [k v]
  (let
    [ctx (fn [side]
           (map (fn [[ln txt]]
                  [(rg-anchor-lineno-long ln) txt false])
                (hit-context-rows (get v side))))]
    (if (map? v)
      (concat (ctx "before") [[(rg-anchor-lineno-long k) (get v "text") true]] (ctx "after"))
      [[(rg-anchor-lineno-long k) v true]])))

(defn- file-hit-rows
  "Gutter rows for ONE file's `hits` (anchor→value pairs), MERGED: every hit's
   context window and matched line collapse into a single line-ordered,
   DEDUPLICATED block. Rendering each hit independently re-printed the same
   source lines once per hit whenever hits sat closer together than the context
   window — a cluster of matches with `context 14` painted the same paragraph
   four times over, which is what a reader sees as a broken card. A `⋯` gutter
   row marks every discontinuity, so a merged block never implies that
   non-adjacent lines are adjacent. Matched lines win over a context copy of the
   same line, so needle highlighting is never lost to a neighbour's context."
  [needles ^long width hits]
  (let
    [by-line
     (reduce (fn [acc [ln txt match?]]
               (if (and (contains? acc ln) (not match?)) acc (assoc acc ln txt)))
             (sorted-map)
             (mapcat (fn [[k v]]
                       (hit-line-entries k v))
                     hits))

     gap-row
     (str "  " (format (str "%" width "s") "⋯"))]

    (loop
      [rows
       []

       prev
       nil

       entries
       (seq by-line)]

      (if-let [[[ln txt] & more] entries]
        (recur (cond-> rows
                 (and prev (> (long ln) (inc (long prev))))
                 (conj gap-row)

                 true
                 (conj (gutter-row needles width ln txt)))
               ln
               more)
        rows))))

(defn- render-patch-result
  "patch/write/struct_patch → `{:summary :body}`. The badge already states the
   operation, so the compact headline contains only affected paths
   (`` `a.clj`, `b.clj` ``), never redundant `update`/`add` verbs. Large fan-out
   collapses to the first two paths plus a count. The body is the unified diff(s);
   a single-file body omits a repeated path heading, while multi-file bodies use
   path-only headings to disambiguate each diff. `r` is a vector of per-file
   summaries `[{:path :op :changed :diff}]`."
  [r]
  (let
    [summaries
     (if (sequential? r) r [r])

     changed
     (filterv #(get % "changed") summaries)

     n
     (count summaries)

     file-label
     (fn [{:strs [path]}]
       (str "`" (disp-path path) "`"))

     labels
     (mapv file-label summaries)]

    {:summary (if (<= n 3)
                (str/join ", " labels)
                (str (str/join ", " (take 2 labels))
                     ", +"
                     (- n 2)
                     " more ("
                     (count changed)
                     "/"
                     n
                     " changed)"))
     :body (some->> (str/join "\n\n"
                              (for [{:strs [path changed diff]} summaries]
                                (let
                                  [diff-block (when (and changed (seq (str diff)))
                                                (strutil/fenced diff "diff"))]
                                  (if (= n 1)
                                    (or diff-block "")
                                    (str "`" (disp-path path)
                                         "`" (when diff-block (str "\n" diff-block)))))))
                    not-empty
                    (str "\n"))}))

(def ^:private grep-card-max-files
  "Files shown in a grep card body before the tail collapses to a `+N more`."
  12)

(def ^:private grep-card-max-hits-per-file
  "Hits shown per file in a grep card body before that file collapses."
  8)

(defn- render-grep-result
  "grep → `{:summary :body}`. Content HITS are the point of the tool, so they are
   what the card shows: a headline naming the term(s), the hit/file counts and
   the scope, over per-file blocks of line-numbered, needle-highlighted matches
   (`anchor` line numbers — the same `lineno:hash` the model patches with).

   File-NAME matches ride along as a plain path fence (they are the whole answer
   when a query matched no content). Long results collapse — at most
   `grep-card-max-files` files and `grep-card-max-hits-per-file` hits each, the
   remainder summarised as `+N more`. With nothing matched the steer/hint stays
   in the body so the user sees WHY."
  [r]
  (let
    [hit-count
     (long (or (get r "hit_count") 0))

     file-count
     (long (or (get r "file_count") 0))

     paths
     (seq (get r "paths"))

     matches
     (get r "matches")

     needles
     (seq (keep #(some-> %
                         kw->str
                         not-empty)
                (get r "needles")))

     q
     (some-> (get r "query")
             str
             not-empty)

     hint
     (some-> (get r "hint")
             kw->str
             not-empty)

     query-chip
     (if needles
       (str/join " OR " (map md-inline-code needles))
       (some-> q
               md-inline-code))

     scope
     (seq (remove #(= "." (kw->str %)) (get r "searched_paths")))

     scope-chip
     (when scope (str "in " (str/join ", " (map #(md-inline-code (disp-path (kw->str %))) scope))))

     counts-chip
     (cond (pos? hit-count) (str hit-count
                                 " hit" (when (not= 1 hit-count) "s")
                                 " in " file-count
                                 " file" (when (not= 1 file-count) "s"))
           paths (str (count paths) " file name" (when (not= 1 (count paths)) "s"))
           :else "no matches")

     shown
     (take grep-card-max-files matches)

     more-files
     (max 0 (- (count matches) (count shown)))

     blocks
     (for [[path hits] shown]
       (let
         [ordered (sort-by (comp rg-anchor-lineno-long key) hits)
          width (hit-gutter-width hits)
          kept (take grep-card-max-hits-per-file ordered)
          extra (max 0 (- (count ordered) (count kept)))
          hits (str (str/join "\n" (file-hit-rows needles width kept))
                    (when (pos? extra)
                      (str "\n  … +" extra " more hit" (when (not= 1 extra) "s"))))]

         (str (md-inline-code (disp-path (kw->str path))) "\n\n" (strutil/fenced hits))))

     body
     (str (when (seq blocks) (str "\n" (str/join "\n\n" blocks)))
          (when (pos? more-files)
            (str "\n\n… +" more-files " more file" (when (not= 1 more-files) "s")))
          (when paths (str "\n```\n" (str/join "\n" (map (comp disp-path kw->str) paths)) "\n```"))
          (when (and (not (seq blocks)) (not paths) hint) (str "\n" hint)))]

    {:summary (str (when query-chip (str query-chip " · "))
                   counts-chip
                   (when scope-chip (str " · " scope-chip)))
     :body (not-empty body)}))

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

(defn- idx-path-cell
  "Length-capped table cell for a PATH. Unlike `idx-cell` it elides the FRONT
   (`…/foundation/editing/core.clj`): a deep path's tail — file name, then its
   anchor — is what a reader needs, while a right-truncated one is all shared
   directory prefix and identifies nothing."
  [s max-len]
  (let
    [s
     (-> (str s)
         (str/replace #"\s+" " ")
         str/trim
         (str/replace "|" "\\|"))

     n
     (long max-len)]

    (if (> (count s) n) (str "…" (subs s (- (count s) (max 0 (dec n))))) s)))

(defn- render-index-one
  "Render one structural index result as its own headline and definition table."
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
     (count defs)

     ;; When the caller narrowed with `ranges`, surface the window count in the
     ;; headline so the card shows the index was SCOPED, not whole-file.
     win
     (let [rngs (get r "ranges")]
       (when (seq rngs) (str " · " (count rngs) " window" (when (not= 1 (count rngs)) "s"))))]

    (if (seq defs)
      {:summary (str (or loc "struct_index")
                     " · "
                     n
                     " def"
                     (when (not= 1 n) "s")
                     (when lang (str " · " lang))
                     (when lc (str " · " lc " line" (when (not= 1 (long lc)) "s")))
                     win)
       :body (let
               [header
                ["| Def | Arity | Kind | Anchor | Doc |" "|-----|-------|------|--------|-----|"]

                rows
                (for [d defs]
                  (let
                    [depth (long (or (get d "depth") 0))
                     nm (str (apply str (repeat depth "\u00a0\u00a0"))
                             (when (pos? depth) "· ")
                             (kw->str (get d "name")))
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
        {:summary (str (or loc "struct_index") win) :body (str "\n" (strutil/fenced sk))}
        {:summary (str (or loc "struct_index") " · no structural index" win)}))))

(declare render-occurrences-result)

(defn- render-index-result
  "struct_index → `{:summary :body}`. File tables preserve request order. When
   requested, an occurrences section groups every indexed definition with its
   syntactic uses."
  [r]
  (if-let [results (seq (get r "results"))]
    (let
      [rendered (mapv render-index-one results)
       occurrences? (contains? r "occurrences")
       occurrences (when occurrences? (get r "occurrences"))
       rendered-occurrences (when occurrences? (mapv render-occurrences-result occurrences))
       files (count rendered)
       defs (reduce + (map #(count (get % "definitions")) results))]

      {:summary (str (batch-paths-summary (map #(get % "path") results))
                     " · "
                     files
                     " file"
                     (when (not= 1 files) "s")
                     " · "
                     defs
                     " def"
                     (when (not= 1 defs) "s")
                     (when occurrences?
                       (str " · " (count occurrences)
                            " occurrence group" (when (not= 1 (count occurrences)) "s"))))
       :body (str/join "\n\n"
                       (cond->
                         (mapv (fn [{:keys [summary body]}]
                                 (str "### " summary (or body "")))
                               rendered)
                         (seq rendered-occurrences)
                         (conj (str "### Occurrences\n\n"
                                    (str/join "\n\n"
                                              (map (fn [{:keys [summary body]}]
                                                     (str "#### " summary (or body "")))
                                                   rendered-occurrences))))))})
    (render-index-one r)))

(defn- render-occurrences-result
  "One `struct_index` occurrence group → `{:summary :body}`, a definition TABLE
   grouped per declared identifier. Its headline is
   `` `name` · K defs · N uses · M files · <scope> ``; <scope> is the exact indexed
   file set. Each DEFINITION is a table row (name, signature, kind — visibility only
   when not public — definition site, use count), followed by use-site rows. Uses no
   single definition owns come last as `unowned use` rows. Cells are plain text (the
   table painter does not render inline markdown) and anchors are re-sorted by line
   number because wire maps do not preserve source order. `r` is wire-shaped:
   `{:name :symbols [{:kind :visibility :signature :path :anchor :end_anchor
   :use_count :uses [{:path :anchors}]}] :other_uses :count :definition_count}`."
  [r]
  (let
    [symbols
     (get r "symbols")

     other
     (get r "other_uses")

     total
     (or (get r "count") 0)

     defs
     (or (get r "definition_count") 0)

     uses
     (max 0 (- (long total) (long defs)))

     nm
     (some-> (get r "name")
             kw->str)

     paths
     (mapv kw->str (get r "paths"))

     scope
     (cond (or (empty? paths) (= paths ["."])) "project-wide"
           :else (str "in " (str/join ", " paths)))

     ;; Every file the answer touches: definition sites plus every use site.
     fc
     (count (distinct (concat (map #(kw->str (get % "path")) symbols)
                              (mapcat (fn [s]
                                        (map #(kw->str (get % "path")) (get s "uses")))
                                      symbols)
                              (map #(kw->str (get % "path")) other))))

     use-row
     (fn [u kind]
       (let
         [anchors
          (get u "anchors")

          lines
          (->> anchors
               (sort-by rg-anchor-lineno-long)
               (map (comp rg-anchor-lineno kw->str))
               (str/join ", "))]

         (str "| "
              "\u00a0\u00a0· "
              (idx-path-cell (disp-path (kw->str (get u "path"))) 42)
              " | "
              (idx-cell "—" 22)
              " | "
              (idx-cell kind 16)
              " | "
              (idx-cell lines 44)
              " | "
              (idx-cell (count anchors) 6)
              " |")))

     def-block
     (fn [s]
       (let
         [sig
          (some-> (get s "signature")
                  kw->str
                  not-empty)

          vis
          (some-> (get s "visibility")
                  kw->str
                  not-empty)

          kind
          (some-> (get s "kind")
                  kw->str
                  not-empty)

          kindc
          (if (and vis (not= vis "public")) (str vis " " kind) (or kind "—"))]

         (cons (str "| "
                    (idx-cell (or nm (kw->str (get s "name"))) 44)
                    " | "
                    (idx-cell (or sig "—") 22)
                    " | "
                    (idx-cell kindc 16)
                    " | "
                    (idx-path-cell (disp-path (kw->str (get s "path"))) 34)
                    " @"
                    (kw->str (get s "anchor"))
                    " | "
                    (idx-cell (or (get s "use_count") 0) 6)
                    " |")
               (map #(use-row % "use") (get s "uses")))))

     header
     ["| Def | Arity | Kind | Where | Uses |" "|-----|-------|------|-------|------|"]]

    {:summary (str (when nm (str "`" nm "` · "))
                   defs
                   " def" (when (not= 1 defs) "s")
                   " · " uses
                   " use" (when (not= 1 uses) "s")
                   " · " fc
                   " file" (when (not= 1 fc) "s")
                   " · " scope)
     :body (when (or (seq symbols) (seq other))
             (str "\n"
                  (str/join "\n"
                            (concat header
                                    (mapcat def-block symbols)
                                    (map #(use-row % "unowned use") other)))))}))

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

(defn- render-node-one
  "ONE `struct_nodes` entry → a `path:line..end_line · kind · at […]` headline, the
   node's SOURCE as a code block, and a compact line of the moves still available.
   A failed entry renders as its error instead."
  [r]
  (let
    [loc
     (some-> (get r "path")
             kw->str
             disp-path)

     kind
     (some-> (get r "kind")
             kw->str)

     line
     (get r "line")

     eol
     (get r "end_line")

     at
     (get r "at")

     src
     (some-> (get r "source")
             kw->str)

     err
     (some-> (get r "error")
             kw->str)

     head
     (str "**"
          (or loc "node")
          (when line (str ":" line (when eol (str ".." eol))))
          "**"
          (when kind (str " · " kind))
          (when (seq at) (str " · at " (pr-str (vec at)))))

     moves
     (let [can (get r "can")]
       (when (map? can)
         (->> ["down" "up" "left" "right" "next" "prev"]
              (filter #(true? (get can %)))
              (str/join " "))))]

    (if err
      (str head " · ⚠ " err)
      (str head
           (when (seq moves) (str "\n" "moves: " moves))
           (when (seq src) (str "\n" (strutil/fenced src (index/code-language (get r "path")))))))))

(defn- render-nodes-result
  "struct_nodes → `{:summary :body}`: `<paths> · N nodes`, then ONE source block per
   requested node, in request order."
  [r]
  (let
    [results
     (vec (get r "results"))

     n
     (count results)]

    {:summary (str (batch-paths-summary (keep #(get % "path") results))
                   " · " n
                   " node" (when (not= 1 n) "s"))
     :body (when (seq results) (str "\n" (str/join "\n\n" (map render-node-one results))))}))

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
     :result
     (str
       "String-keyed `{results, occurrences?}`. Row/file: `path,language,line_count,imports,definitions,skeleton,note,ranges`. "
       "`include_occurrences` adds a group per name: `symbols` (`path,anchor,end_anchor,kind,visibility,signature,use_count,uses{path,anchors}`), `other_uses`, `count`, `definition_count`, `scanned`, `failed`. "
       "No source — pass a row `anchor` to `struct_nodes`/`cat`.")
     :active-fn structural-supported?
     :description
     (str
       "Skeleton of supported source before bodies: imports, definitions, signatures, doc gists, and "
       "fresh anchors for `cat`/`struct_patch`. `include_occurrences` traces each definition's uses.")
     :render render-index-result
     :color-role :tool-color/read
     :schema
     {:type "object"
      :properties
      {"paths"
       {:type "array"
        :items {:oneOf [{:type "string" :minLength 1}
                        {:type "object"
                         :properties
                         {"path" {:type "string" :minLength 1}
                          "ranges"
                          {:type "array"
                           :items {:type "array" :items {:type "integer"} :minItems 2 :maxItems 2}
                           :minItems 1
                           :description (str "THIS path's windows; override shared `ranges`. "
                                             "`[[-1, -1]]` = the whole file.")}}
                         :required ["path"]
                         :additionalProperties false}]}
        :minItems 1
        :description
        (str
          "Exact physical paths from grep/cat/struct_index; batch them. Shared `ranges` apply to "
          "all; an object entry overrides one file.")}
       "include_occurrences" {:type "boolean"
                              :description "Adds each definition's occurrence group; default off."}
       "ranges"
       {:type "array"
        :items {:type "array" :items {:type "integer"} :minItems 2 :maxItems 2}
        :minItems 1
        :description
        "1-based inclusive `[[start,end],…]`; keeps definitions intersecting any window. `line_count` stays whole-file."}}
      :required ["paths"]
      :additionalProperties false}
     :before-fn (path-protected-before-fn :struct_index :file :read read-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :struct_index :file nil)}))

(def ^:private cat-ranges-schema
  "Shape of a `ranges` value: a non-empty list of inclusive 1-based [start end]
   pairs, or the whole-file sentinel [-1, -1]."
  {:type "array"
   :items {:type "array" :items {:type "integer"} :minItems 2 :maxItems 2}
   :minItems 1})


(def ^:private cat-file-entry-schema
  "A per-file object entry in `files`: the path plus its own `ranges`."
  {:type "object"
   :properties {"path" {:type "string" :minLength 1}
                "ranges" (assoc cat-ranges-schema
                           :description (str "THIS file; overrides shared `ranges`. "
                                             "`[[-1, -1]]` = the whole file."))}
   :required ["path"]
   :additionalProperties false})

(def ^:private cat-files-schema
  "The `files` argument: bare file paths and/or per-file range objects."
  {:type "array"
   :items {:oneOf [{:type "string" :minLength 1} cat-file-entry-schema]}
   :minItems 1
   :description
   "Exact physical files. Batch regions; strings use shared `ranges`, objects override."})

(def ^:private cat-schema
  "`cat`'s JSON Schema: plural `files` plus optional shared line `ranges`."
  {:type "object"
   :properties {"files" cat-files-schema
                "ranges" (assoc cat-ranges-schema
                           :description
                           (str
                             "Shared inclusive 1-based `[[start,end],…]` windows for bare entries; "
                             "omit for whole files, `[[-1, -1]]` to unslice ONE entry."))}
   :required ["files"]
   :additionalProperties false
   :maxProperties 2})

(def cat-symbol
  (vis/symbol
    #'cat-tool
    {:symbol 'cat
     :native-tool? true
     :result
     (str
       "String-keyed `{results}` rows: `{op,path,size,mtime,eof,truncated,next_offset,ranges,anchors}`. "
       "`anchors[\"line:hash\"]={\"text\":line}` is the ONLY content field (no `content`/`lines`) and holds every window; "
       "`ranges` carry metadata only (`{range,eof,next_offset,truncated}`) and never repeat the text.")
     :description
     (str
       "Read every needed region of every path as patch-ready `lineno:hash` lines; `ls` lists directories, "
       "`struct_index` maps code first. A write invalidates only that file's earlier anchors.")
     :render render-cat-result
     :color-role :tool-color/read
     :schema cat-schema
     :before-fn (path-protected-before-fn :cat :file :read read-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :cat :file nil)}))

(def ^:private ls-entry-schema
  "A per-directory object entry in `ls` `paths`: the path plus its own options."
  {:type "object"
   :properties {"path" {:type "string" :minLength 1}
                "depth" {:type "integer" :minimum 1}
                "is_hidden" {:type "boolean"}}
   :required ["path"]
   :additionalProperties false})

(def ^:private ls-schema
  "`ls`'s JSON Schema: plural `paths` plus the shared listing options."
  {:type "object"
   :properties
   {"paths" {:type "array"
             :items {:oneOf [{:type "string" :minLength 1} ls-entry-schema]}
             :minItems 1
             :description (str "Directories; an object entry overrides shared options. "
                               "Exact physical paths a tool returned; never assembled from a "
                               "language namespace \u2014 a workspace has many source roots.")}
    "depth" {:type "integer"
             :minimum 1
             :description "Levels to descend (default 1); nested rows sit in `children`."}
    "is_hidden" {:type "boolean"
                 :description "Add dotfiles; gitignored entries stay hidden either way."}}
   :required ["paths"]
   :additionalProperties false})

(def ls-symbol
  (vis/symbol
    #'ls-tool
    {:symbol 'ls
     :native-tool? true
     :result
     (str
       "String-keyed `{results}`, one row per requested path in order: `{path,type,depth,entries}`; "
       "entries are `{name,path,type,size}` plus `children` when `depth` nests.")
     :description
     (str "Directory contents batched over `paths`: directories first, then alphabetical. "
          "Map an unfamiliar tree's SHAPE here first — `depth` descends — instead of guessing "
          "paths for `cat`/`grep`. "
          "Dotfiles need `is_hidden`; gitignored entries are never listed. `cat` reads files.")
     :render render-ls-results
     :color-role :tool-color/read
     :schema ls-schema
     :before-fn (path-protected-before-fn :ls :dir :read read-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :ls :dir nil)}))

(def grep-symbol
  (vis/symbol
    #'grep-tool
    {:symbol 'grep
     :native-tool? true
     :result
     (str
       "Fields `op,query,needles,searched_paths,missing_paths,paths,matches,file_counts,first_hit,hint,hit_count,"
       "file_count,total_file_count,total_file_count_is_exact,limit,truncated_by,hits_truncated_by`. "
       "`matches={path:{\"line:hash\":{\"text\":string,\"before\"?:[{\"line\",\"text\"}],\"after\"?:[…]}}}` "
       "never a list; empty `before`/`after` omitted.")
     :description
     (str "Literal smart-case content plus fuzzy filenames; use first when location is unknown. "
          "`query: \"\"` lists files; null `hits_truncated_by` means complete content.")
     :render render-grep-result
     :color-role :tool-color/search
     :schema
     {:type "object"
      :properties
      {"query"
       {:oneOf [{:type "string"} {:type "array" :items {:type "string" :minLength 1} :minItems 1}]
        :description
        "Content/filename; an empty string lists by frecency/recency. Arrays are OR for content search; filenames use joined terms."}
       "paths"
       {:type "array"
        :items {:type "string" :minLength 1}
        :description
        "Default: whole tree. Existing files are searched exactly, never widened. Missing scopes use the nearest existing directory and enter `missing_paths`. Reuse exact physical paths; never rebuilt from a language namespace."}
       "include" {:oneOf [{:type "array" :items {:type "string"}} {:type "string"}]
                  :description "Content globs, e.g. [\"**/*.clj\"]."}
       "context" {:type "integer"
                  :minimum 0
                  :description "Context lines per hit in before/after (default 0)."}
       "limit" {:type "integer" :minimum 1 :description "Filename-match cap (default 50)."}
       "is_hidden" {:type "boolean" :description "Include hidden paths (default false)."}}
      :required ["query"]
      :additionalProperties false}
     :before-fn (path-protected-before-fn :grep :dir :read find-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :grep :dir nil)}))

(def patch-symbol
  (vis/symbol
    #'patch-tool
    {:symbol 'patch
     :native-tool? true
     :result
     "One row/edit: `path`, `op`, `changed`, `diff`; small regions add `anchors` (`{\"lineno:hash\":{\"text\":line}}`) reusable as the next `from_anchor`; auto-balanced files add `repaired` true and `note`."
     :call (fn [input]
             {:args [(get input "edits")]})
     :description
     (str
       "Anchor-based TEXT editor for prose, config, unsupported languages, or definition spans. "
       "ATOMIC: one bad edit writes NOTHING. Code that will not parse is refused; unbalanced Clojure "
       "delimiters are auto-repaired (`repaired`). A write invalidates only that file's earlier anchors.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties
              {"edits"
               {:type "array"
                :minItems 1
                :description "Atomic anchor edits; each names a file, so a batch may span files."
                :items
                {:type "object"
                 :properties
                 {"path" {:type "string" :minLength 1 :description "File path."}
                  "from_anchor" {:type "string" :minLength 1 :description "Fresh-read lineno:hash."}
                  "to_anchor"
                  {:type "string" :minLength 1 :description "Optional inclusive span-end anchor."}
                  "replace" {:type "string" :description "Replacement; empty deletes."}}
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
     :result
     "One-row array: `path`, `op`, `changed`, `diff`, plus optional small-region `anchors` (`{\"lineno:hash\":{\"text\":line}}`) reusable as the next `from_anchor`."
     :description "Create or wholly replace a clean file; dirty targets require `is_dirty_ok`."
     :replay
     {:elide-args {"content" 8192} :retry-on #{:dirty} :retry-overrides {"is_dirty_ok" true}}
     :render render-patch-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"path" {:type "string" :description "Target file path."}
       "content" {:type "string" :description "Complete file content."}
       "is_overwrite" {:type "boolean"
                       :description "Replace an existing target (default true); false refuses it."}
       "is_dirty_ok" {:type "boolean" :description "Permit overwrite with uncommitted changes."}
       "expected_mtime" {:type "integer"
                         :description "Write only when the target mtime equals this."}}
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

(defn- definition-path
  "Resolve one structural definition NAME (+ optional canonical kind) to the
   tree-sitter zipper path of its outer node. A unique name tolerates a stale or
   over-specific kind, matching the name-based StructuralApi behavior."
  [lang source target kind]
  (let
    [defs
     (index/definitions source lang target)

     requested-kind
     (some-> kind
             name
             str/lower-case
             keyword)

     same-kind
     (when requested-kind (filterv #(= requested-kind (:kind %)) defs))

     candidates
     (cond (seq same-kind) same-kind
           (= 1 (count defs)) defs
           requested-kind []
           :else defs)]

    (cond
      (empty? defs)
      (throw (ex-info
               (str "No definition named '" target "' found. Use struct_index to see valid names.")
               {:type :ext.foundation.editing/struct-target-not-found :target target}))
      (empty? candidates) (throw (ex-info (str "No definition named '"
                                               target
                                               "' of kind "
                                               kind
                                               " found. Use struct_index to see valid kinds.")
                                          {:type :ext.foundation.editing/struct-target-not-found
                                           :target target
                                           :kind kind}))
      (> (count candidates) 1)
      (throw
        (ex-info
          (str (count candidates) " definitions named '" target "' — pass kind to disambiguate.")
          {:type :ext.foundation.editing/struct-target-ambiguous :target target :kind kind}))
      :else (let [resolved (zipper/path-at-anchor lang source (:anchor (first candidates)))]
              (if (:ok? resolved)
                (:path resolved)
                (throw (ex-info
                         (get-in resolved [:error :message] "definition anchor did not resolve")
                         {:type :ext.foundation.editing/struct-anchor-error
                          :reason (get-in resolved [:error :reason])})))))))

(defn- subexpression-count
  "How many non-overlapping times the literal `needle` occurs in `s`."
  ^long [^String s ^String needle]
  (loop
    [from
     0

     n
     0]

    (let [i (.indexOf s needle (int from))]
      (if (neg? i) n (recur (+ i (count needle)) (inc n))))))

(defn- struct-patch-one
  "Structural edit via tree-sitter (every language). Locate the node EITHER by
   NAME or by a zipper PATH, then edit — the file is re-parsed and the write is
   REFUSED if it introduces a syntax error. This is the PREFERRED way to edit
   code; reach for patch(...) only for non-code text or unsupported languages.
     by name:  await struct_patch({\"path\": P, \"op\": \"rename\", \"target\": \"old\", \"code\": \"new\"})
     by path:  await struct_patch({\"path\": P, \"op\": \"replace\", \"at\": [2, 1], \"code\": S})
   ops (by NAME/`target`): replace | delete | insert_before | insert_after |
     append_child | prepend_child | add_doc | replace_doc | replace_node | rename |
     move_before | move_after. `append_child`/`prepend_child` insert inside the named
     definition; `append` alone appends at end-of-file and ignores `target`.
     `delete` drops the named def entirely (= replace it with \"\"); it also works
     by PATH (`at`).
     `rename` rewrites identifier `target` to `code` EVERYWHERE it occurs — a
     syntax-safe global rename, far safer than a blind text replace_all.
     `move_before`/`move_after` RELOCATE the def `target` next to the def `anchor`
     (e.g. move a fn below a dependency it forward-references) — one step, no manual
     cut-and-paste:
       await struct_patch({\"path\": P, \"op\": \"move_after\", \"target\": \"helper\", \"anchor\": \"dep\"})
     `kind` (function/class/method/…) disambiguates same-named defs; `replace_node`
     swaps the UNIQUE sub-expr equal to `match` (scope it with `target`, `anchor`, or `at`).
   ops (by PATH/`at`/node anchor): replace | replace_node (alias) | insert_before |
     insert_after | append_child | prepend_child (child ops insert inside the node,
     after last / before first child; delete = replace with \"\"). `at` is the
     struct_nodes entry's `at`; `nav` adds relative moves — the full clojure.zip vocabulary:
     down|d|b up|u|t left|l right|r first last next|n prev|p {child:i}
     {find:\"text\"} {find_kind:\"if_statement\"}. Navigate with struct_nodes(...) first,
     then edit the same path here.
   Locate targets with struct_index(paths) / struct_nodes(nodes).
   Returns the [{\"path\", \"op\", \"changed\", \"diff\"}] shape as write."
  [args]
  (let
    [path
     (get args "path")

     raw-op
     (or (struct-op->kw (or (get args "op") "replace"))
         (throw (ex-info (str "struct_patch: unknown op " (pr-str (get args "op")))
                         {:type :ext.foundation.editing/struct-unknown-op :op (get args "op")})))

     at-locator?
     ;; Some tool-call serializers materialize an omitted optional vector as [].
     ;; When a real anchor is also present, that empty path is not an intentional
     ;; request to edit the parse root.
     (and (contains? args "at") (or (seq (get args "at")) (not (contains? args "anchor"))))

     explicit-path-locator?
     (or at-locator?
         ;; For moves, `anchor` is a definition NAME rather than a node handle.
         (and (contains? args "anchor") (not (#{:move-before :move-after} raw-op))))

     ;; Child insertion can enter a container by its structural definition name;
     ;; resolve that name to a zipper path below. This keeps `append` distinct:
     ;; StructuralApi APPEND means end-of-file and ignores `target`.
     name-child-locator?
     (and (#{:append-child :prepend-child} raw-op)
          (not explicit-path-locator?)
          (not (str/blank? (str (get args "target")))))

     path-locator?
     (or explicit-path-locator? name-child-locator?)

     _
     (when (and (#{:append-child :prepend-child} raw-op) (not path-locator?))
       (throw (ex-info (str "struct_patch: " (str/replace (name raw-op) "-" "_")
                            " needs a container locator — pass definition `target`, `at`,"
                            " or a node `anchor`.")
                       {:type :ext.foundation.editing/struct-op-needs-container
                        :op (str/replace (name raw-op) "-" "_")})))

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
          (cond at-locator? (get args "at")
                name-child-locator?
                (definition-path lang source (get args "target") (get args "kind"))
                :else
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

          match
          (str (get args "match"))

          code
          ;; `match` means ONE thing under BOTH locators: the unique sub-expression
          ;; of the located node to swap. Naming the whole node is the degenerate
          ;; case, so it still reads as an optimistic concurrency guard.
          (if (and (= raw-op :replace-node) (not (str/blank? match)))
            (let
              [node
               (zipper/inspect lang source at)

               actual
               (str (:text node))

               hits
               (subexpression-count actual match)]

              (cond (= match actual) code
                    (= 1 hits) (str/replace actual match code)
                    :else
                    (throw (ex-info
                             (str "struct_patch: `match` "
                                  (if (zero? hits) "does not occur in" "is not unique in")
                                  " the `"
                                  (:kind node)
                                  "` node selected by `anchor`/`at` — inspect it with struct_nodes,"
                                  " or omit `match` to replace the node whole.\n  node: "
                                  (if (> (count actual) 300) (str (subs actual 0 300) " …") actual))
                             {:type :ext.foundation.editing/struct-locator-match-mismatch
                              :at at
                              :kind (:kind node)
                              :occurrences hits}))))
            code)

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

     ;; is_dirty_ok: a re-parsed structural edit is SAFE on a file with
     ;; uncommitted changes — the dirty-guard only blocks the raw `write`.
     result
     (write-safe {"path" path "content" new-content "is_dirty_ok" true})]

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

(defn- struct-patch-project-rename
  "struct_patch with `paths` instead of `path` — the PROJECT-wide rename. Same op
   as the single-file `rename` (tree-sitter identifier boundaries, each changed
   file re-parsed), widened to every supported file under `paths`, and reported
   in struct_patch's own per-file row shape."
  [args]
  (let
    [op
     (struct-op->kw (or (get args "op") "rename"))

     target
     (str (get args "target"))

     new-name
     (str (get args "code"))

     paths
     (let [p (get args "paths")]
       (if (string? p) [p] (vec p)))

     ;; `paths` is the ONLY way to reach the project-wide rewrite: a missing
     ;; `path` can never silently widen a single-file edit into one.
     _
     (when-not (= op :rename)
       (throw (ex-info
                (str
                  "struct_patch: `paths` is the project-wide rename and takes op \"rename\", not "
                  (pr-str (get args "op"))
                  " — use `path` to edit ONE file.")
                {:type :ext.foundation.editing/struct-paths-needs-rename :op (get args "op")})))

     _
     (when (or (str/blank? target) (str/blank? new-name))
       (throw (ex-info "struct_patch rename needs `target` (current name) and `code` (new name)."
                       {:type :ext.foundation.editing/invalid-symbol-rename-args})))

     r
     (:result (symbol-rename-tool {"name" target "new_name" new-name "paths" paths}))

     ;; Model-facing rows — string keys, no keyword values.
     rows
     (into (mapv (fn [f]
                   {"path" (get f "path") "op" "rename" "changed" true})
                 (get r "files"))
           (mapv (fn [f]
                   {"path" (get f "path") "op" "rename" "changed" false "error" (get f "error")})
                 (get r "failed")))]

    (tool-success {:op :struct_patch
                   :kind :dir
                   :path (or (first paths) ".")
                   :result rows
                   :metadata {:mode :struct_patch
                              :file-count (count rows)
                              :changed-count (count (get r "files"))
                              :edit-count 1}})))

(defn- struct-patch-tool
  "struct_patch — ONE syntax-safe structural edit, or an ORDERED `edits` BATCH.

   Batch form: `{\"edits\": [{...}, {...}]}`. Every entry takes the same keys as a
   single call (`path`/`op`/`target`/`at`/`anchor`/`code`/…), and TOP-LEVEL keys
   are shared defaults for every entry — so one `path` plus many ops needs no
   repetition, and entries may also span several files. Entries apply in request
   order, each against the file as the previous entry left it, and the results
   come back as ONE ordered array. There is no rollback: a failing entry stops
   the batch and the earlier writes stand — the error says how many applied.

   `paths` (in place of `path`) with op \"rename\" is the PROJECT-wide rename."
  [& {:as args}]
  ;; Same `edits` coercion as patch: a batch a serializer stringified, or a lone edit
  ;; map, becomes a real vector instead of being silently ignored as a single call.
  (let [edits (normalize-edits-arg (get args "edits"))]
    (cond (contains? args "paths") (struct-patch-project-rename args)
          (not (and (sequential? edits) (seq edits))) (struct-patch-one args)
          :else
          (let
            [shared (dissoc args "edits")
             specs (mapv #(merge shared %) edits)
             total (count specs)]

            (loop
              [i 0
               summaries []
               befores []]

              (if (>= i total)
                (tool-success
                  {:op :struct_patch
                   :path (or (get (first summaries) "path") (get (first specs) "path") ".")
                   :kind :file
                   :result summaries
                   :metadata {:mode :struct_patch
                              :file-count (count summaries)
                              :changed-count (count (filter #(get % "changed") summaries))
                              :edit-count total
                              :file-befores befores}})
                (let
                  [env
                   ;; A throwing entry keeps its `:type` (so :on-error-fn still routes
                   ;; it) but gains the batch position — earlier writes already stand.
                   (try (struct-patch-one (nth specs i))
                        (catch Throwable e
                          (throw (ex-info (str (ex-message e)
                                               " — struct_patch batch stopped at edit "
                                               (inc i)
                                               " of "
                                               total
                                               "; "
                                               i
                                               " earlier edit(s) already written.")
                                          (assoc (or (ex-data e) {})
                                            :edit-index i
                                            :applied-count i)
                                          e))))]
                  (if (:success? env)
                    (recur (inc i)
                           (into summaries (:result env))
                           (into befores (get-in env [:metadata :file-befores])))
                    (extension/failure {:result nil
                                        :op :struct_patch
                                        :metadata (assoc (:metadata env)
                                                    :mode :struct_patch
                                                    :edit-index i
                                                    :applied-count i)
                                        :error (assoc (:error env)
                                                 :edit-index i
                                                 :applied-count i)})))))))))

(def struct-patch-symbol
  (vis/symbol
    #'struct-patch-tool
    {:symbol 'struct_patch
     :native-tool? true
     :result
     (str
       "One row/edit: `path`, `op`, `changed`, `diff`; small regions add reusable `anchors` for the "
       "next `from_anchor`. A `paths` rename returns one row/file; failures set `changed` false plus `error`.")
     :active-fn structural-supported?
     :description
     (str
       "Structurally edit supported code: definition by NAME (`target`)—no stale anchors—or node by "
       "`at`/`anchor`. Renames, docs, moves, `append_child`. Writes re-parse: code that will not parse "
       "is REFUSED; unbalanced Clojure delimiters auto-repaired.")
     :render render-patch-result
     :color-role :tool-color/edit
     :schema
     {:type "object"
      :properties
      {"path" {:type "string" :description "Edit file, or shared default for `edits`."}
       "paths" {:type "array"
                :items {:type "string" :minLength 1}
                :minItems 1
                :description
                "`rename` only: rewrite `target` to `code` in scopes (`[\".\"]` = project)."}
       "edits"
       {:type "array"
        :minItems 1
        :items {:type "object"}
        :description
        "ORDERED batch; top-level keys are defaults, applied in order, never rolled back. Omit for a lone edit."}
       "op" {:type "string"
             :enum ["replace" "delete" "insert_before" "insert_after" "append" "add_doc"
                    "replace_doc" "replace_node" "rename" "move_before" "move_after" "append_child"
                    "prepend_child"]
             :description
             "`append`=EOF; `append_child`/`prepend_child` take a definition or node locator."}
       "target" {:type "string" :description "Definition NAME; also container for child appends."}
       "code" {:type "string" :description "Source to replace/insert, or rename's new name."}
       "kind" {:type "string" :description "Disambiguates same-named defs."}
       "match" {:type "string"
                :description
                "`replace_node`: unique subexpression text to swap inside the located node."}
       "anchor"
       {:type "string"
        :description
        "`move_before`/`move_after`: adjacent def NAME; else `lineno:hash` entering its node. Composes with `nav`."}
       "at" {:type "array"
             :items {:type "integer" :minimum 0}
             :description "Named-child path from a `struct_nodes` row."}
       "nav" {:type "array" :description "Relative zipper moves after `at` (strings/maps)."}}
      ;; Either a lone `path`+`op` edit or an `edits` batch — validated in the tool.
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
  ;; ONE model-facing struct_nodes entry — string keys, no keyword values. The
  ;; node's SOURCE CODE is `source`; the zipper API is `at` + `children` + `can`.
  [r]
  {"at" (vec (:path r))
   "kind" (:kind r)
   "line" (:start-line r)
   "end_line" (:end-line r)
   "named_child_count" (:named-child-count r)
   "has_error" (:has-error? r)
   "source" (zip-clip (:text r) 4000)
   "sexp" (zip-clip (:sexp r) 1200)
   "children" (mapv (fn [c]
                      {"idx" (:idx c) "kind" (:kind c) "head" (zip-clip (:head c) 120)})
                    (:children r))})

(defn- node-miss
  ;; A navigation miss on ONE entry is DATA, not a dead batch: that entry carries
  ;; `error`/`reason` with the same key set nil-filled, and its siblings answer.
  [path e]
  {"path" path
   "at" nil
   "kind" nil
   "line" nil
   "end_line" nil
   "source" nil
   "sexp" nil
   "named_child_count" nil
   "has_error" nil
   "children" []
   "can" nil
   "error" (get-in e [:error :message])
   "reason" (some-> (get-in e [:error :reason])
                    name)})

(defn- node-one
  ;; ONE cursor: resolve `path` + (`at` | `nav` | `anchor`) and answer with the
  ;; node's SOURCE plus its zipper API.
  [spec]
  (let
    [path
     (get spec "path")

     lang
     (zipper/detect-language path)

     source
     (slurp (safe-path path))

     ;; anchor entry: a `lineno:hash` from a struct_index / cat row
     ;; resolves straight to the node's path, then `nav` composes on top.
     base
     (when-let [a (get spec "anchor")]
       (zipper/path-at-anchor lang source a))

     nav
     (if (and base (:error base))
       base
       (zipper/navigate lang source (if base (:path base) (get spec "at")) (get spec "nav")))]

    (if (:error nav)
      (node-miss path nav)
      (let
        [at
         (:path nav)

         r
         (zipper/inspect lang source at)]

        (if (:error r)
          (node-miss path r)
          (assoc (zip-shape r)
            "path" path
            "can" (zipper/moves-available lang source at)))))))

(defn- nodes-tool
  "The tree-sitter ZIPPER cursor (clojure.zip / rewrite-clj vocabulary, any
   language) — MANY nodes in ONE call. `nodes` is ALWAYS a list; each entry is a
   path string or `{\"path\", \"at\"|\"nav\"|\"anchor\"}`, and TOP-LEVEL keys are shared
   defaults, so one `path` plus many cursors needs no repetition.
     await struct_nodes({\"nodes\": [\"a.clj\", {\"path\": \"b.clj\", \"at\": [2, 0]}]})
     await struct_nodes({\"path\": \"a.clj\"
                         \"nodes\": [{\"nav\": [{\"find\": \"my_fn\"}]}, {\"anchor\": \"120:9f6\"}]})
   EVERY entry answers with BOTH the node's SOURCE CODE (`source`, verbatim text)
   AND the zipper API: `at` — the named-child index path `struct_patch` takes —
   plus `children` [{idx,kind,head}] and `can` {down,up,left,right,next,prev,index,
   siblings}, so you navigate and edit without probing or re-reading the file.
   nav moves — the full clojure.zip / rewrite-clj vocabulary (single-letter
   aliases): SIBLING/PARENT/CHILD down|d|b up|u|t left|l right|r leftmost|first
   rightmost|last root|home {\"child\": i}; DEPTH-FIRST next|n prev|p; SEARCH
   {\"find\": \"text\"} {\"find_kind\": \"if_statement\"}. Boundary / not-found moves
   FAIL CLOSED: that ENTRY carries `error`/`reason` and its siblings still answer.
   EDIT the node under a cursor with struct_patch({\"path\": P, \"op\": ..., \"at\":
   <that entry's `at`>})."
  [& args]
  (let
    [a
     (first args)

     ;; struct_nodes("p") / struct_nodes("p", {opts}) stay usable from Clojure; the
     ;; native schema advertises only the plural `nodes` contract.
     a
     (cond (string? a) (merge {"path" a} (when (map? (second args)) (second args)))
           (map? a) a
           :else {"path" a})

     shared
     (dissoc a "nodes")

     entries
     (get a "nodes")

     specs
     (if (some? entries)
       (do (when-not (and (sequential? entries) (seq entries))
             (throw (ex-info "struct_nodes: `nodes` must be a NON-EMPTY list of node selectors"
                             {:type :ext.foundation.editing/invalid-nodes-args :got a})))
           (mapv
             (fn [e]
               (merge
                 shared
                 (cond
                   (string? e) {"path" e}
                   (map? e) e
                   :else
                   (throw
                     (ex-info
                       "struct_nodes: every `nodes` entry is a path string or an object with `path`"
                       {:type :ext.foundation.editing/invalid-nodes-args :got e})))))
             entries))
       [a])]

    (doseq [s specs]
      (when-not (string? (get s "path"))
        (throw (ex-info "struct_nodes: every node needs a `path`"
                        {:type :ext.foundation.editing/invalid-nodes-args :got s}))))
    (let [results (mapv node-one specs)]
      ;; FAIL CLOSED when NOTHING resolved: a call whose every cursor missed is an
      ;; error, not an empty success. A PARTIAL miss stays data, so the entries that
      ;; did resolve still answer.
      (if (every? #(get % "error") results)
        (let [e (first results)]
          (extension/failure {:result nil
                              :op :struct_nodes
                              :metadata {:target {:requested (str (get (first specs) "path"))
                                                  :kind :file}
                                         :mode :struct_nodes}
                              :error {:message (get e "error")
                                      :reason (some-> (get e "reason")
                                                      keyword)
                                      :mode :struct_nodes}}))
        (tool-success {:op :struct_nodes :kind :file :result {"results" results}})))))

(def nodes-symbol
  (vis/symbol
    #'nodes-tool
    {:symbol 'struct_nodes
     :native-tool? true
     :result
     (str
       "String-keyed `{results}`; one ordered row/node: `path`, `at` (named-child path for `struct_patch`), "
       "`kind,line,end_line,source` (verbatim), `sexp,named_child_count,children,can,has_error`. "
       "Misses add `error`/`reason`; other fields nil.")
     :active-fn structural-supported?
     :description
     "Read nested tree-sitter node SOURCE and navigate when a named definition is too coarse."
     :render render-nodes-result
     :color-role :tool-color/read
     :schema
     {:type "object"
      :properties
      {"nodes"
       {:type "array"
        :items {:oneOf [{:type "string" :minLength 1}
                        {:type "object"
                         :properties
                         {"path" {:type "string" :minLength 1}
                          "at" {:type "array"
                                :items {:type "integer" :minimum 0}
                                :description "THIS node's absolute named-child index path."}
                          "nav" {:type "array" :description "THIS node's relative cursor moves."}
                          "anchor" {:type "string"
                                    :description "`lineno:hash` entry for THIS node."}}
                         :additionalProperties false}]}
        :minItems 1
        :description
        "ONE call's nodes: file-root path or object overriding shared `path`/`at`/`nav`/`anchor`."}
       "path" {:type "string" :description "Shared source file when an entry omits `path`."}
       "at" {:type "array"
             :items {:type "integer" :minimum 0}
             :description "Shared absolute named-child index path."}
       "nav" {:type "array"
              :description "Shared relative moves: strings or {find/child/find_kind} maps."}
       "anchor" {:type "string"
                 :description
                 "`lineno:hash` from struct_index/cat; enters that line's node instead of `at`."}}
      :additionalProperties false}
     :before-fn (path-protected-before-fn :struct_nodes :file :read nodes-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :struct_nodes :file nil)}))

;; sexpr_edit was FOLDED INTO struct_patch — which now takes a zipper `at`/`nav`
;; path as an alternative to a `target` name. ONE structural editor (locate by
;; name OR by path), so the model isn't choosing between two near-identical
;; mutation verbs. `struct_nodes` stays as the read-only navigator that produces paths.

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

(defn- occurrences-data
  "Every syntactic occurrence of one declared identifier across the exact indexed
   `paths`, grouped per definition. Attribution remains conservative: a sole
   definition owns every use; otherwise only a file-local unique definition owns
   its uses, and ambiguous uses remain in `other_uses`.

   The 3-arity accepts a tracer for compatibility and error isolation. The 4-arity
   consumes already-transposed, non-empty path rows; repo-wide indexing uses that
   sparse path to avoid scanning every path again for every declared name."
  ([name paths]
   (occurrences-data name
                     paths
                     (fn [path nm]
                       (structural/occurrences path (slurp (safe-path path)) nm))))
  ([name paths trace]
   (let
     [{:keys [per failed]}
      (reduce
        (fn [acc path]
          (try
            (let [occ (trace path name)]
              (cond-> acc
                (seq occ)
                (update :per
                        conj
                        {"path" path "occurrences" (mapv #(occurrence->wire name %) occ)})))
            (catch Exception e
              (update acc :failed conj {"path" path "error" (or (ex-message e) (str (class e)))}))))
        {:per [] :failed []}
        paths)]
     (occurrences-data name paths per failed)))
  ([name paths per failed]
   (let
     [files
      (vec paths)

      total
      (reduce + 0 (map #(count (get % "occurrences")) per))

      def-rows
      (vec (for
             [f
              per

              o
              (get f "occurrences")

              :when (get o "is_definition")]

             (assoc (dissoc o "is_definition") "path" (get f "path"))))

      use-rows
      (vec (for
             [f
              per

              :let [us
                    (remove #(get % "is_definition") (get f "occurrences"))]
              :when (seq us)]

             {"path" (get f "path") "anchors" (mapv #(get % "anchor") us)}))

      defs
      (count def-rows)

      defs-per-file
      (frequencies (map #(get % "path") def-rows))

      owner
      (fn [use-path]
        (cond (= 1 defs) (first def-rows)
              (= 1 (get defs-per-file use-path)) (first (filter #(= use-path (get % "path"))
                                                                def-rows))
              :else nil))

      grouped
      (group-by #(owner (get % "path")) use-rows)

      symbols
      (mapv (fn [d]
              (let [us (vec (get grouped d))]
                (assoc d
                  "uses" us
                  "use_count" (reduce + 0 (map #(count (get % "anchors")) us)))))
            def-rows)

      other-uses
      (vec (get grouped nil))]

     (cond->
       {"name" name
        "symbols" symbols
        "count" total
        "definition_count" defs
        "scanned" (count files)
        "paths" files
        "failed" failed}
       (seq other-uses)
       (assoc "other_uses" other-uses)))))

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

     paths
     (let [p (or (get spec "paths") ["."])]
       (if (string? p) [p] (vec p)))

     files
     ;; Unbounded: the default rg limit would silently skip every file past it —
     ;; renaming HALF a project is worse than renaming none of it.
     (vec (or (:files
                (rg-search
                  {"query" [name] "is_files_only" true "paths" paths "limit" Integer/MAX_VALUE}))
              []))

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
                 (write-safe {"path" path "content" renamed "is_dirty_ok" true})
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
     ;; Reachable as struct_patch(paths=…, op="rename") on the native surface; kept
     ;; here as a Python-callable function (apropos/doc), off the tool list.
     :native-tool? false
     :result "String-keyed `{files:[{path,changed:true}], file_count, failed:[{path,error}], op}`."
     :name "struct_rename"
     :call {:pos ["name" "new_name"]}
     :active-fn structural-supported?
     :description
     (str
       "Rename one identifier at syntactic boundaries across supported project code, re-parsing changed files. "
       "First grep candidates, then struct_index those paths for declarations/occurrences. Clojure namespace "
       "renames still require moving the defining file.")
     :render render-symbol-rename-result
     :color-role :tool-color/edit
     :schema {:type "object"
              :properties {"name" {:type "string" :description "Current identifier or namespace."}
                           "new_name" {:type "string"
                                       :description "Replacement identifier or namespace."}}
              :required ["name" "new_name"]
              :additionalProperties false}
     :tag :mutation
     :on-error-fn (tool-failure-on-error :struct_rename :dir nil)}))

(def create-dirs-symbol
  (vis/symbol
    #'create-dirs-tool
    {:symbol 'create-dirs
     :native-tool? false
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
     :native-tool? false
     ;; copy(src, dest, {opts}) — two positionals, the rest an options dict.
     :call {:pos ["src" "dest"] :rest :opt}
     :description "Copy a confined file or directory; existing destinations require `is_overwrite`."
     :render render-copy-result
     :color-role :tool-color/move
     :schema {:type "object"
              :properties {"src" {:type "string" :description "Source."}
                           "dest" {:type "string" :description "Destination."}
                           "is_overwrite" {:type "boolean"
                                           :description
                                           "Replace an existing destination (default false)."}}
              :required ["src" "dest"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :copy :path :write first-two-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :copy :path nil)}))

(def move-symbol
  (vis/symbol #'move-tool
              {:symbol 'move
               :native-tool? false
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
     :native-tool? false
     :call {:pos ["path"]}
     :description
     "Destructively delete a confined file/directory only with explicit intent; an absent path is a non-error no-op."
     :render render-delete-result
     :color-role :tool-color/delete
     :schema {:type "object"
              :properties {"path" {:type "string" :description "Target path."}}
              :required ["path"]
              :additionalProperties false}
     :before-fn (path-protected-before-fn :delete :path :write first-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :delete :path nil)}))

(def file-exists-symbol
  (vis/symbol #'exists-tool
              {:symbol 'file-exists
               :native-tool? false
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

(defn- fs-targets
  "Every target path of a single-path fs op (delete / create_dirs / exists), in
   request order: the scalar `path` first, then the batch `paths`. ONE call may
   carry N paths — that is the whole point of the batch form: deleting nine temp
   files is one tool call and one card, not nine."
  [m]
  (into (if-some [p (get m "path")]
          [p]
          [])
        (remove nil?)
        (get m "paths")))

(defn- fs-batch?
  "True when the caller used the batch key `paths`. Presence — never the NUMBER
   of entries — selects the batch result shape, so the answer follows the
   request: a one-element `paths` still answers `{\"paths\" [entry]}` and a
   scalar `path` still answers the flat historical shape."
  [m]
  (contains? m "paths"))

(defn- fs-arg-paths
  "Paths of ONE fs input map: `path` plus the batch `paths`
   (delete/create_dirs/exists), or `src`+`dest`. Path protection must see the
   WHOLE batch — one unchecked entry would be a hole in the gate."
  [args]
  (let [m (first args)]
    (when (map? m)
      (let [targets (fs-targets m)]
        (if (seq targets) targets (vec (keep #(get m %) ["src" "dest"])))))))

(defn- fs-before-fn
  "Route the fs op through path protection with the RIGHT intent: `exists`
   reads, every other op writes."
  [env f args]
  (let
    [m
     (first args)

     intent
     (if (= "exists" (get m "op")) :read :write)]

    ((path-protected-before-fn :fs :path intent fs-arg-paths) env f args)))

(defn- fs-plural [n one many] (if (= 1 n) one many))

(defn- fs-batch-body
  "One scan-friendly markdown row per batch target. The headline already states the
   verdict whenever every target agrees, so a uniform batch lists bare paths and
   only a MIXED batch spends a per-row verdict — no decorative glyphs either way."
  [entries status]
  (let
    [verdicts
     (mapv status entries)

     mixed?
     (< 1 (count (distinct verdicts)))]

    (str "\n"
         (str/join "\n"
                   (map (fn [e v]
                          (str "- `" (disp-path (get e "path")) "`" (when mixed? (str " — " v))))
                        entries
                        verdicts)))))

(defn- render-fs-batch-result
  "Compact card for `{\"action\" … \"paths\" [entry …]}`.

   The summary answers what happened and how many; the expandable body lists one
   verdict and path per row. Keeping paths out of the headline prevents long
   batches from turning the result band into an ambiguous wrapped paragraph."
  [action entries]
  (let
    [n
     (count entries)

     changed
     (fn ^long [k]
       (long (count (filter #(get % k) entries))))

     status
     (case action
       "delete"
       (fn [e]
         (if (get e "is_deleted") "deleted" "already absent"))

       "create_dirs"
       (fn [e]
         (if (get e "is_created") "created" "already existed"))

       "exists"
       (fn [e]
         (if (get e "is_existing") "exists" "missing"))

       (fn [_]
         (or (not-empty (str action)) "target")))]

    {:summary (case action
                "delete"
                (let
                  [^long d
                   (changed "is_deleted")

                   absent
                   (- (long n) d)]

                  (if (zero? absent)
                    (str "deleted " n " " (fs-plural n "path" "paths"))
                    (str "deleted " d " of " n " paths · " absent " already absent")))

                "create_dirs"
                (let
                  [^long c
                   (changed "is_created")

                   existing
                   (- (long n) c)]

                  (if (zero? existing)
                    (str "created " n " " (fs-plural n "dir" "dirs"))
                    (str "created " c " of " n " dirs · " existing " already existed")))

                "exists"
                (let
                  [^long existing
                   (changed "is_existing")

                   missing
                   (- (long n) existing)]

                  (cond (zero? missing) (str n " " (fs-plural n "path exists" "paths exist"))
                        (zero? existing) (str n " " (fs-plural n "path missing" "paths missing"))
                        :else (str existing " of " n " paths exist · " missing " missing")))

                (str/join " " (remove nil? [(not-empty (str action)) (str n " paths")])))
     :body (fs-batch-body entries status)}))

(defn- render-fs-result
  "fs → `{:summary :body?}`, read off the CANONICAL fs result
   `{\"op\" \"fs\" \"action\" <verb> \"path\"|\"src\"+\"dest\"|\"paths\"
   \"is_created\"|\"is_deleted\"|\"is_existing\"}`.

   A single-target result stays on one verb-led headline. A batch uses a compact
   aggregate headline plus an expandable, one-target-per-row body. The badge is
   the generic `FS`, so the headline still says WHICH filesystem op ran.

   The action rides `\"action\"`, NOT `\"op\"`: the engine stamps `\"op\"` with the
   canonical TOOL op (`\"fs\"`) on every result. `\"op\"` remains a fallback for
   results persisted before the split."
  [r]
  (let
    [p
     (disp-path (get r "path"))

     entries
     (get r "paths")

     action
     (if-some [a (get r "action")]
       a
       (let [o (get r "op")]
         (when-not (= "fs" o) o)))]

    (if (seq entries)
      (render-fs-batch-result action entries)
      {:summary
       (case action
         "copy"
         (str "copied `" (disp-path (get r "src")) "` → `" (disp-path (get r "dest")) "`")

         "move"
         (str "moved `" (disp-path (get r "src")) "` → `" (disp-path (get r "dest")) "`")

         "delete"
         (if (get r "is_deleted") (str "deleted `" p "`") (str "nothing to delete at `" p "`"))

         "create_dirs"
         (if (get r "is_created") (str "created dir `" p "`") (str "dir `" p "` already exists"))

         "exists"
         (str "`" p "` " (if (get r "is_existing") "exists ✓" "missing ✗"))

         ;; Unknown/absent action: never echo the tool name twice — say what is
         ;; actually known (the verb and/or the path), else just the tool.
         (str/join " " (remove nil? ["fs" (not-empty (str action)) (when p (str "`" p "`"))])))})))

(defn- fs-paths-success
  "ONE envelope for a single-path fs op run over N targets. `entries` are the
   canonical per-path maps (`{\"path\" … \"is_<flag>\" …}`) in request order.
   The batch form answers `{\"action\" … \"paths\" [entry …]}`; the scalar form
   FLATTENS the lone entry, so a one-path call keeps exactly the shape it always
   had and only the batch pays for the extra nesting."
  [{:keys [batch? action flag kind entries]}]
  (let [changed (count (filter #(get % flag) entries))]
    (tool-success
      {:op :fs
       :path (get (first entries) "path")
       :kind kind
       :result (if batch? {"action" action "paths" entries} (assoc (first entries) "action" action))
       :metadata {:path-count (count entries) :changed-count changed}})))

(defn- fs-tool
  "ONE filesystem tool dispatching on `op` — folds the old copy/move/delete/
   create_dirs/file_exists native tools into a single wire surface. Takes the
   whole input dict and answers ONE canonical flat shape: always an `action`
   discriminator plus workspace-relative paths (`path`, or `src`+`dest` for the
   two-path ops) and, where the action has a verdict, exactly one `is_<foo>`
   flag (`is_created` / `is_deleted` / `is_existing`) — never an absolute path
   and never a derivable duplicate. The discriminator is `action`, NOT `op`:
   the engine stamps `op` with the canonical TOOL op (`fs`) on every result,
   so a sub-op under `op` would be silently clobbered. The bare copy/move/
   delete/… sandbox functions keep their own legacy shapes.

   delete / create_dirs / exists also take a BATCH `paths` — N targets in ONE
   call, answered as `{\"action\" … \"paths\" [{\"path\" … \"is_<foo>\" …} …]}` in
   request order, so removing nine files is one tool call and one card instead
   of nine. Targets are processed IN ORDER, one result entry per path. Deleting
   a path that is already absent is a no-op reported as `is_deleted` false."
  [m]
  (let
    [op
     (get m "op")

     src
     (get m "src")

     dest
     (get m "dest")

     batch?
     (fs-batch? m)

     targets
     (fs-targets m)]

    (when (and batch? (#{"copy" "move"} op))
      (throw (ex-info "fs: `paths` is for delete | create_dirs | exists — copy/move take src+dest"
                      {:type :ext.foundation.editing/bad-fs-paths :op op})))
    (when (and (#{"delete" "create_dirs" "exists"} op) (empty? targets))
      (throw (ex-info (str "fs: " op " needs `path` or a non-empty `paths`")
                      {:type :ext.foundation.editing/missing-fs-path :op op})))
    (case op
      "copy"
      (let
        [out
         (copy-safe src dest (select-keys m ["is_overwrite"]))

         from
         (rel-path (safe-path src))]

        (tool-success {:op :fs
                       :path out
                       :kind :path
                       :result {"action" "copy" "src" from "dest" out}
                       :metadata {:src (path->target src :path) :dest (path->target dest :path)}}))

      "move"
      (let
        [out
         (move-safe src dest (select-keys m ["is_overwrite"]))

         from
         (rel-path (safe-path src))]

        (tool-success {:op :fs
                       :path out
                       :kind :path
                       :result {"action" "move" "src" from "dest" out}
                       :metadata {:src (path->target src :path) :dest (path->target dest :path)}}))

      "delete"
      (fs-paths-success {:batch? batch?
                         :action "delete"
                         :flag "is_deleted"
                         :kind :path
                         :entries (mapv (fn [p]
                                          (let
                                            [rel
                                             (rel-path (safe-path p))

                                             deleted?
                                             (delete-if-exists-safe p)]

                                            {"path" rel "is_deleted" deleted?}))
                                        targets)})

      "create_dirs"
      (fs-paths-success {:batch? batch?
                         :action "create_dirs"
                         :flag "is_created"
                         :kind :dir
                         :entries (mapv (fn [p]
                                          (let
                                            [before
                                             (fs/exists? (safe-path p))

                                             out
                                             (create-dirs-safe p)]

                                            {"path" out "is_created" (not before)}))
                                        targets)})

      "exists"
      (fs-paths-success {:batch? batch?
                         :action "exists"
                         :flag "is_existing"
                         :kind :path
                         :entries (mapv (fn [p]
                                          (let
                                            [rel
                                             (rel-path (safe-path p))

                                             exists?
                                             (exists-safe? p)]

                                            {"path" rel "is_existing" exists?}))
                                        targets)})

      (throw (ex-info (str "fs: unknown op "
                           (pr-str op)
                           " — expected copy | move | delete | create_dirs | exists")
                      {:type :ext.foundation.editing/bad-fs-op :op op})))))

(def fs-symbol
  (vis/symbol
    #'fs-tool
    {:symbol 'fs
     :native-tool? true
     :result
     (str
       "String-keyed, `action`-discriminated: copy/move `{action,src,dest}`, delete `{action,path,is_deleted}`, "
       "create_dirs `{action,path,is_created}`, exists `{action,path,is_existing}`; batch `paths` gives "
       "`{action,paths}`, one ordered row/target. Top level adds `op`.")
     :description
     "Confined filesystem ops. delete is destructive and needs explicit intent; a missing target is a no-op (`is_deleted` false); create_dirs makes parents; exists never reads."
     :render render-fs-result
     :color-role :tool-color/move
     :schema {:type "object"
              :properties
              {"op" {:type "string"
                     :enum ["copy" "move" "delete" "create_dirs" "exists"]
                     :description "Operation."}
               "paths"
               {:type "array"
                :items {:type "string"}
                :minItems 1
                :description
                "delete/create_dirs/exists targets; ALWAYS a list, ordered, one result/path."}
               "src" {:type "string" :description "copy/move source."}
               "dest" {:type "string" :description "copy/move destination."}
               "is_overwrite" {:type "boolean" :description "copy/move overwrite (default false)."}}
              :required ["op"]
              :additionalProperties false}
     :before-fn fs-before-fn
     :tag :mutation
     :on-error-fn (tool-failure-on-error :fs :path nil)}))

(defn available-editing-symbols
  []
  [index-symbol cat-symbol ls-symbol grep-symbol patch-symbol write-symbol struct-patch-symbol
   nodes-symbol symbol-rename-symbol fs-symbol create-dirs-symbol copy-symbol move-symbol
   delete-symbol file-exists-symbol])

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
