(ns com.blockether.vis.internal.foundation.editing.core
  "Filesystem tools exposed as bare symbols in the Python sandbox.

   Two layers:

   1. Structured helpers for tree / search:

        (ls dir)              ; a DIRECTORY -> a compact TREE STRING, ready to print:
        (ls dir, depth=2)     ; a `path  Nd Nf` header, then `name/` per directory and
                              ; `name  size` per file, directories first. `depth`
                              ; descends. The walk runs inside the block, so mapping a
                              ; tree costs no wire round trip.
                              ; A nil or blank path throws before any I/O.
        (grep query)          ; -> ONE anchored TEXT block, never a map: a summary line,
                              ; then `  <line>:<hash>| <text>` rows under each path;
                              ; query = a term or list of terms (OR), smart-case
                              ; substring — or a REGEX with `is_regex`.
                              ; Opts: paths/include/limit/is_hidden/is_regex

   2. Cwd-safe wrappers over the babashka.fs file API. Code is edited by ADDRESS
      with `cat`/`patch` — ONE `patch`
      call carries every edit for one file and writes once; plain Python owns
      whole-file creation and deletion:

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
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.fff :as fff]
            [com.blockether.parinferish.balance :as balance]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.editing.escapes :as escapes]
            [com.blockether.vis.internal.foundation.editing.hashline :as hashline]
            [com.blockether.vis.internal.foundation.editing.parse :as parse]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.fff-index :as fff-index]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import (com.github.difflib DiffUtils UnifiedDiffUtils)
           (com.github.difflib.patch AbstractDelta Chunk Patch)
           (java.io File)
           (java.nio.file AtomicMoveNotSupportedException
                          CopyOption
                          Files
                          LinkOption
                          Path
                          StandardCopyOption)))

;; Tools in this namespace (grep/cat/patch/move/…) can execute
;; DEFERRED on a virtual thread that has entered the GraalPy polyglot Context —
;; e.g. inside `await gather(grep(a), cat(b))`. While on a context-entered thread, GraalVM's
;; HostAccess DENIES reflective Java calls (clojure.lang.Reflector → "Cannot
;; reflectively invoke …"). So every Java interop call here MUST compile to a
;; direct invokevirtual (type-hinted), never a reflective one. Keep this on.
(set! *warn-on-reflection* true)

;; Tunables

(def ^:private default-grep-limit 50)

(def ^:private default-cat-limit
  "Lines one `cat` call renders before it clips and names the call that
   continues it. A whole-file read is capped rather than refused: the model
   almost always wants the file, and a cap that SAYS what it dropped costs one
   line where a refusal costs a whole round trip."
  2000)

(def ^:private max-cat-window-bytes
  "Byte ceiling for one `cat` window, whichever cap bites first. One enormous
   minified line must not blow a block's printed-output budget on its own."
  (* 50 1024))

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
   or needle could ride all the way to the outer Python eval wall and
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
   candidate set to one page, which the old fuzzy-path union then papered over.

   `is-regex?` runs fff's NATIVE regex grep (`:mode :regex`) instead of the
   literal one, so a pattern narrows the candidate set as tightly as a literal
   needle does. fff's dialect is Rust's `regex` crate: on a pattern IT cannot
   compile it silently FALLS BACK to literal matching and reports
   `:regex-fallback-error` — a candidate set that misses every real hit, i.e.
   exactly the false negative this whole path exists to avoid. So that fallback
   is REFUSED here, naming the engine's own reason."
  [idx ^String query is-regex?]
  (loop [offset
         0

         page
         0

         acc
         (transient [])]

    (let [{:keys [matches next-file-offset regex-fallback-error]}
          (fff/grep idx
                    {:query query
                     :mode (if is-regex? :regex :plain)
                     :file-offset offset
                     :page-limit rg-fff-grep-page-limit
                     :max-matches-per-file 1
                     :max-file-size rg-fff-grep-max-file-size
                     :time-budget-ms 1500})

          _
          (when (and is-regex? (not (str/blank? (str regex-fallback-error))))
            (throw (ex-info
                     (str
                       "grep is_regex pattern is not supported by the native scanner: " query
                       " — " regex-fallback-error
                       ". Rust regex syntax has no lookaround and no backreferences; rewrite the"
                       " pattern without that construct, or drop is_regex to search it literally.")
                     {:type :ext.foundation.editing/invalid-rg-spec
                      :field :query
                      :pattern query
                      :engine-error (str regex-fallback-error)})))

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
   grep is literal/smart-case, so it already sees every content candidate.

   `is-regex?` skips the path side for the same reason: fff's path search is a
   FUZZY SUBSEQUENCE over path text, which reads a PATTERN as literal characters
   and drags in a page of files that carry no match at all. Native regex grep
   already owns content discovery in that mode."
  [idx ^File base query is-regex?]
  (let [path-items
        (when (and (not is-regex?) (not (rg-needle-hostile-to-fff? query)))
          (->> (:items (fff/search idx {:query query :page-size 1000}))
               (filter #(rg-fff-path-hit? query %))))

        grep-items
        (rg-fff-grep-files idx query is-regex?)]

    (concat (rg-fff-rel-files base path-items) (rg-fff-rel-files base grep-items))))

(defn- rg-fff-root-files
  "`rg-fff-candidate-files` for ONE root: a FILE root is its own only candidate, a
   directory root leases a single fff index and realizes every needle's hits in it."
  [^File root needles is-regex? overlay]
  (if (.isFile root)
    [root]
    (fff-index/with-index [idx (fff-index/lease root true overlay)]
                          (let [base (.getCanonicalFile root)]
                            ;; doall: realize the lazy hits INSIDE with-open, before the fresh
                            ;; instance is closed.
                            (doall (mapcat #(rg-fff-query-files idx base % is-regex?) needles))))))

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
   `is-regex?` swaps fff's literal content grep for its NATIVE regex grep, so a
   pattern narrows the universe instead of widening it to the whole tree.
   Returns a File vec, deduped by canonical path."
  [roots needles is-regex? overlay]
  (->> roots
       (mapcat #(rg-fff-root-files % needles is-regex? overlay))
       ;; dedup by canonical path, keep File objects
       (reduce (fn [acc ^File f]
                 (assoc acc (.getCanonicalPath f) f))
               {})
       vals
       vec))


(def ^:private default-find-limit 50)

;; Path safety

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
   here was never captured as a session attachment even when the temp capture
   was live (see `capture-temp-write!`)."
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
  "DORMANT — a no-op while `mpl-capture/incidental-capture-enabled?` is false, and
   that is the whole point: a file tool writing scratch into temp is not an
   artifact anyone asked for. What a session should SHOW is what a tool `attach`es
   deliberately.

   It streamed a just-written TEMP file (under `/tmp` or `$TMPDIR`) to the DB as a
   `session_iteration_attachment` — the host-side twin of the sandbox OUTBOX
   tap. A no-op for a non-temp path, or when no capture sink is bound (the file
   tool ran outside a driven block). NEVER throws — a capture must not break an
   edit."
  [^File f]
  (try (when (and mpl-capture/incidental-capture-enabled? (under-temp-root? f))
         (mpl-capture/record-file! (.toPath f)))
       (catch Throwable _ nil))
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
        "Path is nil or blank - ls/grep/cat take a concrete path string; grep answers anchored TEXT, not a map, so take a path from its per-path header or an anchor from a `line:hash` row, never the result itself"
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
                 (let [^java.nio.file.Path cp
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

(defn- canonical-path
  "Canonical `java.nio.file.Path` for `x` — absolute, normalized, symlinks resolved.
   The address a listing RENDERS (`rel-path`) and the ownership question it ASKS
   (`workspace-dir?`) have to agree on every byte, so both read a path through this
   one function."
  ^java.nio.file.Path [x]
  (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))

(defn- rel-path
  [^File f]
  ;; Reverse of safe-path's remap so the address the model SEES round-trips:
  ;; a file under the primary cwd renders RELATIVE; a file under a context
  ;; CLONE renders as its REAL (trunk) absolute path — never the ~/.vis/drafts
  ;; clone path. Anything else falls back to the absolute path.
  (let [^java.nio.file.Path cwd-canon
        (canonical-path (workspace/cwd))

        ^java.nio.file.Path p
        (.toPath (.getCanonicalFile f))]

    (cond (.startsWith p cwd-canon) (let [rel (paths/unixify (.relativize cwd-canon p))]
                                      (if (str/blank? rel) "." rel))
          :else (or (some (fn [{:keys [trunk clone]}]
                            (let [^java.nio.file.Path cp
                                  (canonical-path clone)

                                  ^java.nio.file.Path tp
                                  (canonical-path trunk)]

                              (when (.startsWith p cp)
                                (paths/unixify (.resolve tp (.relativize cp p))))))
                          (workspace/filesystem-root-mappings))
                    (str p)))))

(defn- workspace-dir?
  "True when `f` lies INSIDE this session's workspace: under the primary cwd, or under
   either side of a bound context-clone mapping — a draft clone IS the workspace,
   mounted elsewhere.

   The RENDERED address cannot answer this, which is why the question is asked of the
   FILE. `rel-path` deliberately renders a clone as its TRUNK absolute path, so the
   workspace itself reads exactly like `/etc` does. Only a directory that is ours has a
   warm index to ride and a `vis.yml` overlay to rebase."
  [^File f]
  (let [^java.nio.file.Path p
        (canonical-path f)

        under?
        (fn [x]
          (.startsWith p ^java.nio.file.Path (canonical-path x)))]

    (boolean (or (under? (workspace/cwd))
                 (some (fn [{:keys [trunk clone]}]
                         (or (under? clone) (under? trunk)))
                       (workspace/filesystem-root-mappings))))))

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
  (let [p
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
  (let [paths (mapv #(let [s (str/trim (str %))] (if (str/blank? s) "." s)) paths)]
    (if (some #{"."} paths)
      (let [allowed (workspace/allowed-roots)
            primary (first allowed)
            no-search (workspace/no-search-roots)
            roots (into []
                        (comp (remove (fn [r]
                                        (and (not= r primary) (contains? no-search r))))
                              (map io/file))
                        allowed)]

        {:roots roots :searched-paths (mapv rel-path roots) :resolutions []})
      (let [resolutions
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

;; The :fs/access gate

;; The vocabulary of what is protected belongs to an EXTENSION, never to the
;; engine: the engine only ASKS, through the `:fs/access` gate that the Python
;; sandbox filesystem asks under the interpreter. This layer extracts the op's
;; paths and turns a refusal into a tool failure.

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
  (let [a
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

(defn- balanced-json-prefix
  "The substring of `s` from its first `[`/`{` through the bracket that closes it,
   nil when nothing balances. Lets a stringified argument with trailing junk
   (`\"[{…}]}\"` — one brace too many) still parse as the list it meant."
  [^String s]
  (let [array
        (str/index-of s "[")

        object
        (str/index-of s "{")

        start
        (if (and array object) (min (long array) (long object)) (or array object))]

    (when start
      (loop [i
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
   `patch`'s batch path all read the same shape. An unrecognisable value is
   returned unchanged so the caller's own validation still speaks."
  [edits]
  (let [unwrapped
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

(defn- find-arg-paths
  [args]
  (let [a
        (first args)

        spec
        (when (map? a) a)

        paths
        (cond (contains? spec "paths") (get spec "paths")
              (contains? spec "path") (get spec "path")
              :else nil)

        paths
        (cond (or (nil? paths) (and (sequential? paths) (empty? paths))) ["."]
              (sequential? paths) paths
              :else [paths])]

    (mapv normalize-find-dir-path paths)))

(defn- gate-refusal-failure
  "Turn a `:fs/access` refusal into the tool failure the model reads. The gate's
   own sentence IS the remedy — the extension that declared the boundary is the
   only party that knows what to do instead — so it is the message, the `:hint`
   and the `:loop-hint` alike."
  [op kind operation {:keys [reason owner target]}]
  (let [t (now-ms)]
    (extension/failure
      {:result nil
       :op op
       :metadata {:target target
                  :started-at-ms t
                  :finished-at-ms t
                  :duration-ms 0
                  :operation operation
                  :gate :fs/access
                  :owner owner}
       :error {:message (str op " blocked: " (:resolved target) " — " reason)
               :type :ext.foundation.editing/path-protected
               :reason :path-protected
               :operation operation
               :hint reason
               :loop-hint reason
               :owner owner
               :kind kind}})))

(defn- fs-access-refusal
  "First `:fs/access` refusal among `paths`, or nil when every one is allowed.

   ONE place asks the gate, so a native reader and the sandbox `ls` helper cannot
   drift into two vocabularies. `operation` is the verb the hook sees
   (`\"file-read\"`, `\"file-write\"`) — the same vocabulary `internal/sandbox-fs`
   passes for guest IO, so one rule covers the tool and `open(p, \"w\")` alike."
  [env kind operation paths]
  (when (extension/gate-hooked? :fs/access)
    (some (fn [path]
            (let [target (path->target path kind)]
              (some-> (extension/run-gate-hooks :fs/access
                                                env
                                                {:operation operation
                                                 :path (or (:absolute target) (str path))})
                      (assoc :target target))))
          paths)))

(defn- fs-access-before-fn
  "Ask the `:fs/access` gate about every path this op touches, BEFORE it runs.
   The first refusal wins and no path of the batch is touched: a batch that would
   be half-refused is refused whole."
  [op kind operation path-extractor]
  (fn [env f args]
    (if-let [refusal (fs-access-refusal env kind operation (extracted-paths path-extractor args))]
      {:result (gate-refusal-failure op kind operation refusal)}
      {:env env :fn f :args args})))

(defn- mutation-atomic?
  "True when a patch args vector carries the documented `atomic`
   escape flag - on the lone edit map, or on ANY edit in the batch."
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
  "Write-intent gate for patch. The `:fs/access` gate runs
   FIRST (an extension's boundary always wins); only AFTER it clears does this
   consult the env's OPTIONAL `:mutation-gate`. The gate receives
   `{:op :paths :atomic?}` and returns a refusal string to short-circuit with a
   `:plan-required` failure, or nil to pass through. No `:mutation-gate` on the
   env = pass through unchanged (the gate is opt-in)."
  [op kind path-extractor]
  (let [protect (fs-access-before-fn op kind "file-write" path-extractor)]
    (fn [env f args]
      (let [out (protect env f args)]
        (if (contains? out :result)
          out
          (if-let [gate (:mutation-gate env)]
            (let [paths (extracted-paths path-extractor args)
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
  [op kind]
  (fn [err _env _f args]
    (let [path
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
                  :metadata
                  (cond-> {:target target :started-at-ms t :finished-at-ms t :duration-ms 0}
                    interrupted?
                    (assoc :interrupted?
                      true :status
                      :interrupted))
                  :error error
                  :throwable (when-not error err)})})))

;; .gitignore (cheap, lazy)

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
  (let [{:keys [include-gitignored-paths always-exclude]}
        (config/search-overlay)

        overlay
        {:custom-ignore-filenames [".rgignore"]
         :exclude-globs (vec always-exclude)
         :unignore-globs (vec include-gitignored-paths)}]

    (when (some seq (vals overlay)) overlay)))

(defn- hidden-relative-path?
  "True when `relative-path` has a hidden filesystem segment below `root`. This
   preserves `ls`'s `is_hidden` contract while fff owns every ignore decision."
  [^File root ^String relative-path]
  (loop [^File parent
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
  (loop [i
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
  (loop [offset
         0

         acc
         (transient [])]

    (let [{page :items total :total-matched}
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
  (let [prefix
        (when (seq target-rel) (str target-rel "/"))

        rebase
        (fn [globs]
          (->> globs
               (keep (fn [glob]
                       (let [glob (str glob)]
                         (cond (= glob target-rel) nil
                               (and prefix (str/starts-with? glob prefix)) (subs glob
                                                                                 (count prefix))
                               :else glob))))
               vec))]

    (some-> (fff-ignore-overlay)
            (update :exclude-globs rebase)
            (update :unignore-globs rebase))))

(defn- workspace-relative-address?
  "True when `rel-path` rendered a RELATIVE address for the listed directory, so
   `target-rel` is a usable PREFIX into the workspace index's own records.

   This is a question about the ADDRESS, never about ownership: a context clone renders
   as its trunk ABSOLUTE path and so answers false while still being the workspace —
   `workspace-dir?` is what decides whether a directory may be indexed at all."
  [target-rel]
  (not (str/starts-with? (str target-rel) "/")))

(defn- warm-ls-lease
  "The pooled WORKSPACE-root index — the one `grep` and `find` already keep hot — but
   ONLY when it is ALREADY built. Reusing it turns `ls` of a subdirectory into a page
   scan over an existing index instead of a fresh per-directory tree scan plus its
   own watcher. A cold pool is left alone on purpose: indexing just the subtree is
   then the cheaper of the two. The root is CANONICAL so the pool key is the very one
   `grep`/`find`/`ls .` warm."
  []
  (let [lease
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
  (when (and (seq target-rel) (workspace-relative-address? target-rel))
    (when-let [lease (warm-ls-lease)]
      (let [prefix (str target-rel "/")
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
   an explicitly named ignored directory (`target/`, `node_modules/` …) readable
   INSIDE the workspace."
  [^File root target-rel ^long levels is-hidden?]
  (fff-index/with-index [idx (fff-index/lease root true (fff-ls-overlay target-rel))]
                        (fff-ls-records idx "" (fff-ls-keeper root "" levels is-hidden?))))

(defn- fff-ls-listing-items
  "Listing rows from fff's STATELESS directory listing — one bounded, ignore-aware walk
   that needs no index and no watcher at all.

   fff refuses to INDEX a filesystem root or any home directory (\"Can not run certain
   FFF features in a file system root or home directories\"), so `ls /` and `ls ~` used
   to have no answer whatsoever, only the `fff-unavailable` throw. `fff/list-directory`
   builds nothing, so that refusal cannot apply and the listing every machine has is
   served by fff like every other one: fff still owns `.gitignore`/`.ignore` and its own
   cache-directory rules, a symlinked directory is listed but never descended, and
   dotfiles still need `is_hidden`."
  [^File root ^long levels is-hidden?]
  (mapv (fn [{:keys [relative-path dir? size]}]
          {:relative-path relative-path :directory? dir? :size (when-not dir? size)})
        (fff/list-directory (.getPath root) {:max-depth levels :include-hidden? is-hidden?})))

(defn- list-dir
  "Directory listing as MODEL data, powered by fff — never a filesystem walk.

   fff owns `.gitignore`, `.ignore`, `.rgignore`, and the live `vis.yml` grep overlay;
   this code only rebuilds the documented tree shape. Four fff sources, and which
   one answers is decided FIRST by whether the directory is even ours:

   1. a directory that is not OURS — under neither the primary cwd nor any bound
      context-clone root, which only the FILE can say (`workspace-dir?`) — is
      LISTED, never indexed. Naming a few entries must not cost a recursive walk,
      a live filesystem watcher and a bigram CONTENT index over whatever lives
      there: `~/.vis/models` is ~790 MB of model weights, and indexing it to
      answer `ls` is what turned that listing into minutes and then a
      `fff-scan-timeout`;
   2. inside the workspace, the WARM workspace index `grep`/`find` already
      maintain, prefix-filtered — no new index and no new watcher;
   3. otherwise an index rooted at the directory itself, which is also what keeps
      a directly listed ignored directory readable, as it was before;
   4. and, when fff refuses to INDEX that root (`fff-unavailable`: a filesystem
      root or a home directory), the stateless listing again —
      `fff-ls-listing-items` builds no index and so is never refused.

   Records are depth-filtered inside the paging loop (`fff-ls-scan`), so cost tracks
   the rows RETURNED rather than the size of the tree, and rendered paths are joined
   from the directory's own rendered path instead of canonicalizing per row. Results
   remain directories-first then alphabetical, and are exhaustive."
  [^File d {:keys [depth is_hidden] :or {depth 1 is_hidden false}}]
  (let [^File root
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
        (if-not (workspace-dir? root)
          (fff-ls-listing-items root levels is-hidden?)
          (or (fff-ls-workspace-items root target-rel levels is-hidden?)
              (try (fff-ls-target-items root target-rel levels is-hidden?)
                   (catch clojure.lang.ExceptionInfo e
                     (if (= :ext.foundation.editing/fff-unavailable (:type (ex-data e)))
                       (fff-ls-listing-items root levels is-hidden?)
                       (throw e))))))

        render-prefix
        (if (= "." base) "" (str base "/"))

        rows
        (->> items
             (map (fn [{:keys [relative-path directory? size]}]
                    (let [^String path
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

;; directory-walk cancellation

(defn- check-interrupt!
  "Throw `InterruptedException` when the worker thread has been interrupted
   (e.g. by `cancel!` cancelling the turn's worker future). Long recursive
   directory walks poll this so Esc aborts them promptly instead of traversing a
   huge configured catalog path and leaving the spinner stuck on 'cancelling'."
  []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "directory walk cancelled"))))

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
  (let [n
        (count needle)

        h
        (count hay)]

    (when (pos? n)
      (loop [s
             (long 0)

             best
             (long Long/MAX_VALUE)]

        (if (< s h)
          (if (= (.charAt hay s) (.charAt needle 0))
            (let [end
                  (loop [i
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
  (let [wp
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
  (let [toks
        (->> (str/split (str/lower-case (or query "")) #"[^a-z0-9]+")
             (remove str/blank?))

        pnorm
        (find-norm path)

        nnorm
        (find-norm (last (str/split (str path) #"/")))]

    (if (empty? toks) 0.0 (transduce (map #(find-token-score % pnorm nnorm)) min 1.0 toks))))

(def ^:private find-spec-canonical-keys
  "grep's WHOLE key vocabulary — one canonical name per idea, the names
   `doc(\"grep\")` declares and the only ones the search itself reads."
  #{"query" "paths" "limit" "offset" "include" "exclude" "context" "is_hidden" "is_regex"})

(def ^:private find-spec-aliases
  "Near-miss spellings that FOLD onto the canonical key they meant. A model
   types `glob=`, `globs=`, `context_lines=`, `max_results=`, `path=` — and each
   one used to cost the whole search (\"find spec has unknown keys: glob\") over
   a word nobody could have guessed differently. Naming ONE idea twice — alias
   and canonical in the same call — is still refused: that is two answers to one
   question, not a slip."
  {"path" "paths"
   "dir" "paths"
   "dirs" "paths"
   "max_results" "limit"
   "glob" "include"
   "globs" "include"
   "includes" "include"
   "excludes" "exclude"
   "context_lines" "context"
   "regex" "is_regex"
   "hidden" "is_hidden"})

(defn- fold-find-aliases
  "`spec` with every accepted alias renamed to the canonical key it means, or a
   throw naming the idea that was answered twice. An UNKNOWN key is left alone —
   [[coerce-find-spec]] refuses it by name, with the vocabulary."
  [spec]
  (reduce-kv (fn [m alias canonical]
               (if-not (contains? m alias)
                 m
                 (if (contains? m canonical)
                   (throw (ex-info (str "find spec must use only one of canonical \""
                                        canonical
                                        "\" or alias \""
                                        alias
                                        "\".")
                                   {:type :ext.foundation.editing/invalid-find-args :spec spec}))
                   (-> m
                       (assoc canonical (get m alias))
                       (dissoc alias)))))
             spec
             find-spec-aliases))

(defn- coerce-find-spec
  "ONE canonical call shape: grep takes a SINGLE options map. Python kwargs fold
   into that very map, so `grep(query=q, paths=[\"src\"])` and
   `grep({\"query\": q, \"paths\": [\"src\"]})` are the same call. A positional query is
   REFUSED because the second positional used to mean OPTIONS: the obvious
   reading of `grep([\"a\" \"b\"], [\"src\" \"tools\"])` — needles, then scopes — died
   on argument shape instead of searching. ONE canonical name per idea, and the
   near-miss spellings fold onto it ([[find-spec-aliases]]) — never both in one
   call."
  [args]
  (let [spec
        (first args)

        _
        (when-not (and (= 1 (count args)) (map? spec))
          (throw
            (ex-info
              (str "grep takes ONE options map: grep({\"query\": q, \"paths\": [\"src\"]}) "
                   "— kwargs are that same map, grep(query=q, paths=[\"src\"]). "
                   "A positional query or paths argument is not accepted.")
              {:type :ext.foundation.editing/invalid-find-args :expected '([spec-map]) :got args})))

        spec
        (fold-find-aliases spec)

        unknown-keys
        (seq (remove find-spec-canonical-keys (keys spec)))]

    (when unknown-keys
      (throw (ex-info
               (str
                 "find spec has unknown keys: "
                 (str/join ", " (map str unknown-keys))
                 ". Allowed: query, paths, limit, offset, include, exclude, context, is_hidden, "
                 "is_regex — a near-miss spelling (path, max_results, glob, globs, context_lines) "
                 "folds onto the one it means.")
               {:type :ext.foundation.editing/invalid-find-args
                :unknown (vec unknown-keys)
                :allowed (vec (sort find-spec-canonical-keys))})))
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

       raw-paths
       (get spec "paths" ["."])

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
       (get spec "limit" default-find-limit)

       _
       (when-not (and (integer? limit) (pos? (long limit)))
         (throw (ex-info "find \"limit\" (alias \"max_results\") must be a positive integer"
                         {:type :ext.foundation.editing/invalid-find-args :limit limit})))

       ;; PAGING. `offset` is where this page STARTS on both grep axes — the
       ;; ranked NAME list and the CONTENT hits. A caller never guesses it: the
       ;; previous result's `next_offset` IS the value to pass back.
       offset
       (let [o (get spec "offset" 0)]
         (when-not (and (integer? o) (not (neg? (long o))))
           (throw (ex-info "find \"offset\" must be a non-negative integer"
                           {:type :ext.foundation.editing/invalid-find-args :offset o})))
         (long o))]

      {:query query
       :paths paths
       :precise-paths precise-paths
       :missing missing
       :context context
       :limit limit
       :offset offset
       :is_hidden (boolean (get spec "is_hidden"))
       ;; REGEX mode is a CONTENT dialect: it changes what a hit IS, so it rides
       ;; the same spec as `context`/`include` rather than a second call shape.
       :is_regex (boolean (get spec "is_regex"))
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
  (let [f
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
  (let [glue #{"the" "and" "for" "with" "that" "this" "how" "what" "where" "into" "from" "was" "are"
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
  (let [f
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
  (let [{:keys [query paths limit offset is_hidden is_ls is_regex] scope-misses :missing}
        (coerce-find-spec args)

        {roots :roots find-resolutions :resolutions searched-paths :searched-paths}
        (resolve-search-roots paths)

        ;; fff ranks genuine hits first but pads the page with loose subsequence
        ;; noise, so pull a WIDER candidate set than `limit` and let the relevance
        ;; filter below do the real cutting (a fresh fff scan is ~11ms).
        ;; The page the caller asked for starts at `offset`, so the candidate set
        ;; has to cover everything up to its END, not just one page's worth.
        candidate-page
        (max (+ (long limit) (long offset)) 300)

        ;; `.rgignore` + the `:grep` config overlay (issue #23), handed to fff
        ;; itself — see `fff-ignore-overlay`.
        search-overlay
        (fff-ignore-overlay)

        scan
        (fn [q]
          (find-scan roots q is_hidden candidate-page search-overlay))

        strict
        (if (or is_ls is_regex) [] (scan query))

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
        [ranked fuzzy?]
        (cond
          ;; `is_regex` is a CONTENT dialect. The NAME axis is a FUZZY SUBSEQUENCE
          ;; score over filenames, which reads `^ns\b|foo.*bar` as literal
          ;; characters and ranks unrelated paths above nothing at all — so in
          ;; regex mode there is no name axis and `paths` stays empty.
          is_regex [[] false]
          is_ls [(find-ls roots (+ (long limit) (long offset)) is_hidden search-overlay) false]
          (or (seq strict) (< (count tokens) 2)) [strict false]
          :else (let [stem
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
                             (let [s
                                   (stem it)

                                   bull?
                                   (contains? (:terms it) s)]

                               (assoc it
                                 :rank-score (+ (double (:score it 0.0)) (if bull? 0.6 0.0)))))
                           (vals by-path))]

                  [(->> scored
                        (sort-by (fn [it]
                                   [(- (double (:rank-score it))) (- (count (:terms it))) ;; then coverage
                                    (- (long (or (:frecency-score it) 0))) (:path it)]))
                        vec) true]))

        items
        (if fuzzy?
          (vec (take limit (drop offset (map #(dissoc % :rank-score) ranked))))
          (->> ranked
               ;; strongest match first; frecency then path break ties.
               (sort-by (fn [it]
                          [(- (double (:score it 0.0))) (- (long (or (:frecency-score it) 0)))
                           (:path it)]))
               (drop offset)
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

    ;; grep's canonical DATA result — `grep-tool` RENDERS this as anchored text;
    ;; every key ships on every call (empty vector / false, never absent) so
    ;; caller code can index a field without a `contains?` dance first.
    {"items" (mapv ->item items)
     "item_count" (count items)
     "paths" (mapv :path items)
     "query" query
     "searched_paths" searched-paths
     "limit" limit
     "offset" offset
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
  (let [[a b]
        args

        spec
        ;; FOLDED, not raw: `glob=`/`excludes=` mean `include`/`exclude`, and a
        ;; content search that read the raw map would silently drop the very
        ;; filter the caller asked for.
        (fold-find-aliases (cond (and (= 1 (count args)) (map? a)) a
                                 (and (= 2 (count args)) (map? b)) (assoc b "query" a)
                                 (= 1 (count args)) {"query" a}
                                 :else {}))

        {paths :precise-paths :keys [context offset]}
        (coerce-find-spec args)]

    (cond-> {"query" (get spec "query") "paths" paths "offset" offset}
      (pos? (long (or context 0)))
      (assoc "context" context)

      (contains? spec "include")
      (assoc "include" (get spec "include"))

      (contains? spec "exclude")
      (assoc "exclude" (get spec "exclude"))

      (contains? spec "is_hidden")
      (assoc "is_hidden" (get spec "is_hidden"))

      (contains? spec "is_regex")
      (assoc "is_regex" (get spec "is_regex")))))

(defn- content-result
  "Build grep's CONTENT hits from an `rg-search` result: an ordered
   `{path {\"<lineno>\" {\"text\" line}}}` matches map, plus hit/file counts,
   first hit, echoed needles, and
   breadth flags when more files match than are shown. With `context` N each hit
   also carries `before`/`after` — vectors of `{\"line\" n \"text\" line}` — so the
   surrounding lines arrive in the same call. The compatibility arity takes the
   context count positionally; the hits already carry it."
  ([out needles]
   (let [hits
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
                       (str line)
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
           (doseq [p (sort-by (fn [p]
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

;; grep's TEXT projection
;;
;; `content-result` above stays the tested pure core and keeps returning the
;; ordered data. This is its RENDERER: one anchored text block, because the
;; model pays for what it PRINTS and `print(grep(...))` used to print a
;; four-level dict repr whose keys were bare line numbers. Every rendered line —
;; context lines included — carries its `<line>:<hash>` anchor, so a grep hit is
;; not "a place to go read" but a `patch` argument.

(defn- count-phrase
  "`3 files` / `1 file` — the unit singularized on 1, so a count never reads as a
   template with a number dropped into it."
  [n unit]
  (str n " " (if (= 1 (long n)) unit (str unit "s"))))

(defn- grep-hit-tuples
  "Every `[line text]` tuple ONE path contributes — its hits and their `context`
   lines alike — de-duplicated and in line order, so a rendered block is a
   window on the file rather than a list of overlapping fragments."
  [file-map]
  (->> file-map
       (mapcat (fn [e]
                 (let [v (val e)]
                   (concat (map (fn [b]
                                  [(long (get b "line")) (get b "text")])
                                (get v "before"))
                           [[(long (parse-long (str (key e)))) (get v "text")]]
                           (map (fn [a]
                                  [(long (get a "line")) (get a "text")])
                                (get v "after"))))))
       (reduce (fn [m [ln t]]
                 (assoc m ln t))
               (sorted-map))
       (mapv (fn [e]
               [(key e) (val e)]))))

(defn- contiguous-runs
  "Split line-ordered tuples into contiguous runs, so a `context` window's gaps
   render as a `⋮` marker instead of a misleading unbroken block."
  [tuples]
  (reduce (fn [runs t]
            (let [r (peek runs)]
              (if (and r (= (long (first t)) (inc (long (first (peek r))))))
                (conj (pop runs) (conj r t))
                (conj runs [t]))))
          []
          tuples))

(defn- grep-summary-line
  "Line 1, ALWAYS: the query, the counts, and — when the sweep was capped — how
   to CONTINUE: `next(r)` first, because the caller is Python and that is the
   whole step, then the literal call, which carries the offset the next page
   starts at and is what the sandbox reads back out of this line. It goes FIRST
   because a block's printed output is HEAD-clipped, so a trailing summary is the
   first casualty of exactly the wide grep whose truncation you must know about."
  ^String [result]
  (let [ls?
        (str/blank? (str (get result "query")))

        query
        (let [q (get result "query")]
          (if (coll? q) (str/join " " (map str q)) (str q)))

        hits
        (long (or (get result "hit_count") 0))

        files
        (long (or (get result "file_count") 0))

        total
        (get result "total_file_count")

        exact?
        (boolean (get result "total_file_count_is_exact"))

        breadth
        (if (and total (> (long total) files) (pos? files))
          (str files " of " (when-not exact? "~") total " files")
          (count-phrase files "file"))

        capped
        (let [content-cap
              (get result "hits_truncated_by")

              name-cap
              (get result "truncated_by")

              next-offset
              (get result "next_offset")]

          (cond (= "time" content-cap) "  stopped at the scan budget — PARTIAL, narrow the scope"
                (and next-offset
                     (or (contains? #{"limit" "bytes"} content-cap) (= "limit" name-cap)))
                (str "  capped by "
                     (or content-cap name-cap)
                     " → next(r) or grep({…, \"offset\": "
                     next-offset
                     "})")
                :else ""))]

    (if ls?
      (str "grep '' — file listing  " (count-phrase (count (get result "paths")) "file"))
      (str "grep '" query "'  " (count-phrase hits "hit") " · " breadth capped))))

(defn- render-grep-text
  "grep's whole model-facing answer as ONE string: the summary line, then per
   path a header and its anchored rows, then fuzzy NAME matches, then the hint.

   The counts stay PROSE on line 1 and no structured field survives — control
   flow reads the summary or counts the lines it just split. One structured
   field would put a map back around a string and take the saving with it."
  ^String [result]
  (let [matches
        (get result "matches")

        counts
        (get result "file_counts")

        content-paths
        (set (map str (keys matches)))

        blocks
        (mapv (fn [e]
                (let [path
                      (str (key e))

                      runs
                      (contiguous-runs (grep-hit-tuples (val e)))]

                  (str path
                       "  (" (get counts path (count runs))
                       ")\n" (str/join "\n  ⋮\n"
                                       (map #(hashline/render-hashline-block % "  ") runs)))))
              matches)

        name-rows
        (->> (get result "paths")
             (map str)
             (remove content-paths)
             (mapv #(str "~ " %)))

        missing
        (seq (get result "missing_paths"))

        scope-row
        (when missing
          (str "missing: "
               (str/join ", " (map str missing))
               "  (searched: "
               (str/join ", " (map str (get result "searched_paths")))
               ")"))

        hint-row
        (when-let [h (some-> (get result "hint")
                             str
                             not-empty)]
          (str "hint: " h))]

    (->> (concat [(grep-summary-line result)]
                 (when scope-row [scope-row])
                 blocks
                 name-rows
                 (when hint-row [hint-row]))
         (remove nil?)
         (str/join "\n"))))

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
  (let [terms (->> needles
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
   LITERALLY. `grep` is a literal smart-case substring search UNLESS the call
   passes `is_regex`, so a pattern like `defn-? +grep`, `foo.*bar` or `^ns\\b`
   silently returns zero content hits and the caller cannot tell a missing symbol
   from a wrong dialect — exactly the dead end that invites a pointless re-run
   with cosmetic edits. The hint this drives names the switch, so the recovery is
   one flag rather than a guess.

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

(defn- grep-data
  "grep's ORDERED DATA — the tested pure core. `grep-tool` is now only its
   RENDERER (`render-grep-text`), so this is the map every shape, count, paging
   and hint assertion reads, and the rendering is pinned separately.

   Search file CONTENT and match file NAMES/PATHS in one call (bound as `grep`;
   `find_files`/`find` stay as compatibility aliases).

     await grep({\"query\": \"grep-tool\"})
     await grep({\"query\": \"channel_tui render\", \"paths\": [\"src\"], \"limit\": 20})
     await grep({\"query\": [\"TODO\", \"FIXME\"], \"include\": [\"**/*.clj\"], \"context\": 2})
     await grep({\"query\": \"defn grep\", \"exclude\": [\"**/*_test.clj\"]})
     await grep({\"query\": \"defn-? +grep-data\", \"is_regex\": True})

   ONE options map is the WHOLE call surface — kwargs
   (`grep(query=…, paths=[…])`) fold into that same map. There is no positional
   query and no second positional argument.

   CONTENT matching is smart-case literal substring; a query list is OR.
   `include` and `exclude` bound WHICH FILES the CONTENT sweep reads — globs
   matched against each file's repo-relative path AND its bare name, with
   `**/` matching at any depth including the root. `exclude` WINS: a file
   matching both is not searched, so `include: [\"**/*.clj\"]` with
   `exclude: [\"**/*_test.clj\"]` reads every Clojure source but its tests.
   Both are CONTENT-axis filters; the fuzzy NAME list is unaffected.
   `is_regex` switches CONTENT matching to REGULAR EXPRESSIONS — every term a
   pattern, a list still OR, the SAME smart-case rule (no uppercase in the
   pattern → case-insensitive) — and turns the fuzzy NAME axis OFF, because a
   pattern scored as a filename subsequence is noise. A pattern that does not
   compile is REFUSED with its syntax error, never answered as zero hits. Every
   hit lands in the CANONICAL flat result under `matches` —
   `{path {\"<lineno>\" {\"text\" … \"before\" [{\"line\" \"text\"}] \"after\" […]}}}`
   — each key the hit's 1-based line, `context` N adding the surrounding lines.
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

   PAGING is one knob on each side: `offset` says where this page starts on
   both axes, and `next_offset` is the value to pass back as the NEXT page's
   `offset` — `null` exactly when the page already is the whole answer. When
   both axes are still capped it advances by the SMALLER delivered count, so a
   page can repeat a row of the wider axis but can never skip one. A `time`
   cap is NOT paged: a re-scan stops at the same wall, so narrow the search.

   For NAME matching, existing files normalize to their parent directory and
   missing paths to the nearest existing confined directory. CONTENT matching
   searches an existing file exactly as scoped and never widens it on zero hits;
   missing scopes are searched at their nearest existing directory and reported.
   NAME matching is fuzzy subsequence over the fff file index."
  [& args]
  (let
    [{:strs [query offset item_count truncated_by] :as name-out}
     (find-search args)

     content-spec
     (find-args->content-spec args)

     is_regex
     (boolean (get content-spec "is_regex"))

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

     ;; PAGING. `next_offset` is the ONE value a caller passes back as `offset`;
     ;; nil means this page IS the whole answer. Each capped axis proposes the
     ;; offset just past what IT delivered and the smaller wins, so the next
     ;; page may repeat a row of the wider axis but never skips one. A `time`
     ;; cap proposes nothing: re-scanning stops at the same wall, and the hint
     ;; below asks for a narrower search instead.
     next-offset
     (let [advances (cond-> []
                      (= "limit" truncated_by)
                      (conj (long (or item_count 0)))

                      (contains? #{"limit" "bytes"} (get content "hits_truncated_by"))
                      (conj content-hits))]
       (when-some [advance (some->> (seq advances)
                                    (apply min))]
         (when (pos? (long advance)) (+ (long (or offset 0)) (long advance)))))

     ;; ONE FLAT canonical result: the content block is merged UP (no nested
     ;; `content` envelope, no `items` duplicate of `paths`, no `fuzzy`/
     ;; `matched_terms`/`item_count` internals) so `matches`/`hit_count`/`paths`
     ;; sit at the top level for the model, the renderer and the tests alike.
     out
     (cond-> (-> name-out
                 (dissoc "items" "fuzzy" "matched_terms" "item_count")
                 ;; `hint` and `next_offset` are TOTAL too: present on every grep
                 ;; result, nil when the search has nothing to explain / no next page.
                 (assoc "paths" kept-paths
                        "hint" nil
                        "next_offset" next-offset)
                 (merge content))
       ;; REGEX mode: the NAME axis is off, so "nothing matched" is only ever a
       ;; CONTENT story — and the pattern demonstrably RAN (it compiled on both
       ;; sides), so the recovery is a looser pattern, never a different dialect.
       (and (not ls?) is_regex (zero? content-hits))
       (assoc "hint"
         (str "No CONTENT matched the regex \"" query
              "\" — the pattern compiled and ran. Loosen it or widen `paths`. "
              "`is_regex` searches CONTENT only: file NAMES are not matched in this mode."))

       (and (not ls?) (not is_regex) (zero? (long (or item_count 0))) (zero? content-hits))
       (assoc "hint"
         (str
           "No file NAME or CONTENT matched \""
           query
           "\". "
           (if multiword?
             "Shorten to a single distinctive filename fragment or a real symbol/string that exists."
             "Try a different term, a real symbol/string, or widen the scope.")
           (when (regex-looking-query? query)
             " CONTENT matching is LITERAL smart-case substring by DEFAULT — pass `is_regex: True` to run this query as a regular expression.")))

       ;; Names matched but content did not, and the query reads like a regex:
       ;; say so AND name the switch, or the caller re-runs the same pattern with
       ;; cosmetic edits.
       (and (not ls?)
            (not is_regex)
            (pos? (long (or item_count 0)))
            (zero? content-hits)
            (regex-looking-query? query))
       (assoc "hint"
         (str
           "No CONTENT matched \"" query
           "\" — CONTENT matching is LITERAL smart-case substring by DEFAULT. Pass `is_regex: True` "
           "to run it as a regular expression; only the file NAME matches in `paths` are real."))

       ;; The scan stopped at its wall-clock budget, so these results are
       ;; PARTIAL. Said LAST because it outranks every hint above: "nothing
       ;; matched" is a lie when the sweep never finished.
       (= "time" (get content "hits_truncated_by"))
       (assoc "hint"
         (str
           "Search stopped at its " (quot (long rg-search-budget-ms) 1000)
           "s scan budget — these results are PARTIAL, not the whole tree. "
           "Narrow `paths` to a subdirectory, add `include`/`exclude` globs, or search a more distinctive term.")))]

    out))

(defn- grep-tool
  "grep — literal smart-case CONTENT search plus fuzzy file-NAME matching, in ONE
   options map, answered as ONE anchored TEXT block.

     await grep({\"query\": \"grep-tool\"})
     await grep({\"query\": \"channel_tui render\", \"paths\": [\"src\"], \"limit\": 20})
     await grep({\"query\": [\"TODO\", \"FIXME\"], \"include\": [\"**/*.clj\"], \"context\": 2})
     await grep({\"query\": \"defn grep\", \"exclude\": [\"**/*_test.clj\"]})
     await grep({\"query\": \"defn-? +grep-tool\", \"is_regex\": True})

   The whole call surface is `grep-data`'s; this is its projection. Line 1 always
   summarizes — hits, files, truncation and the literal next call — then each
   path heads a block of `  <line>:<hash>│ <text>` rows, context lines anchored
   too. A hit is therefore already a `patch` argument: split it on the gutter and
   spend the anchor, with no `cat` in between."
  [& args]
  (let [out
        (apply grep-data args)

        searched_paths
        (get out "searched_paths")]

    (tool-success {:op :grep
                   :path (first searched_paths)
                   :kind :dir
                   :result (render-grep-text out)
                   :metadata {:query (get out "query")
                              :paths searched_paths
                              :limit (get out "limit")
                              :offset (get out "offset")
                              :item-count (count (get out "paths"))
                              :hit-count (get out "hit_count")
                              :truncated-by (get out "truncated_by")}})))

;; rg

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


(defn- has-upper?
  "True when `s` contains an uppercase letter — the smart-case trigger."
  [^String s]
  (boolean (some #(Character/isUpperCase ^char %) s)))

(defn- needle-pattern
  "One `is_regex` needle compiled to a `java.util.regex.Pattern` under the SAME
   smart-case rule the literal side uses: no uppercase in the pattern →
   CASE-INSENSITIVE, an uppercase anywhere → case-sensitive. That is verbatim the
   rule fff's native regex grep applies to its own candidate pass (`build_regex`),
   so discovery and JVM re-validation can never disagree about a hit.

   A pattern that does not COMPILE is refused right here, at coercion time,
   carrying the syntax error: a broken pattern must never read as `0 hits`."
  ^java.util.regex.Pattern [^String needle]
  (try (java.util.regex.Pattern/compile
         needle
         (int (if (has-upper? needle) 0 java.util.regex.Pattern/CASE_INSENSITIVE)))
       (catch java.util.regex.PatternSyntaxException e
         (throw (ex-info
                  (str "grep is_regex pattern does not compile: "
                       needle
                       " — "
                       (.getDescription e)
                       " at index "
                       (.getIndex e)
                       ". Fix the pattern, or drop is_regex to search it literally.")
                  {:type :ext.foundation.editing/invalid-rg-spec :field :query :pattern needle})))))

(defn- coerce-rg-spec
  "Coerce the public rg spec map into the search engine's shape.

   `query` IS the search — a string, or a LIST of terms matched as OR (a line
   containing ANY term). Matching is smart-case literal substring (see
   `make-line-matcher`) — unless `is_regex` is set, which makes every term a
   REGULAR EXPRESSION instead (still OR, still smart-case) and turns the
   comma-splitting and trimming below OFF: `a{1,3}` is ONE pattern, not two
   terms, and a trailing space is part of the pattern.
   `any` is accepted as a back-compat alias for `query`
   (and a stray `all` is treated as OR too — the same-line AND mode was dropped;
   filter the hits in Python for the rare \"both terms\" case).

   Optional: `paths` (scope, default \".\"), `include` (globs — only files whose
   path/name matches), `exclude` (globs — files whose path/name matches are NOT
   searched; exclude WINS over include), `context` N (lines of context around
   each hit), `is_files_only` (return the distinct matching file paths, no
   per-line hits).
   Unknown keys are ignored so a stray annotation never hard-fails the call."
  [spec]
  (when-not (map? spec)
    (throw (ex-info "rg takes one spec map: {\"query\": [...], \"paths\": [...]}."
                    {:type :ext.foundation.editing/invalid-rg-spec :got (type spec)})))
  (let [vector-of-strings
        (fn [k raw]
          (let [raw
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

        ;; REGEX mode is decided BEFORE the needles: it turns the comma-splitting
        ;; and trimming below OFF (they would cut `a{1,3}` in half and silently
        ;; change what a trailing-space pattern means).
        is_regex
        (boolean (get spec "is_regex"))

        ;; A query TERM is a substring; a LIST is OR. Models overwhelmingly write
        ;; the OR list as ONE comma-joined string (`\"model, cycle\"`) — which,
        ;; matched literally, hits nothing. So split every term on commas into
        ;; separate OR needles: `\"a, b\"` and `[\"a, b\", c]` both become
        ;; `[a b …]`. (A rare literal-comma search loses out; the model's intent
        ;; is virtually always \"these separate terms\".)
        needles
        (let [raw
              (vector-of-strings query-key (get spec query-key))

              ns
              (if is_regex
                (into [] (remove str/blank?) raw)
                (->> raw
                     (mapcat #(str/split % #"\s*,\s*"))
                     (map str/trim)
                     (remove str/blank?)
                     vec))]

          (when (empty? ns)
            (throw (ex-info "rg query has no non-blank terms."
                            {:type :ext.foundation.editing/invalid-rg-spec :field query-key})))
          ;; REFUSE a broken pattern here, where the caller still gets a reason —
          ;; not deep in the scan where it would surface as an empty result.
          (when is_regex (run! needle-pattern ns))
          ns)

        raw-paths
        (get spec "paths" ["."])

        paths
        (if (or (nil? raw-paths) (and (sequential? raw-paths) (empty? raw-paths)))
          ["."]
          (vector-of-strings :paths raw-paths))

        ;; A blank glob filters NOTHING, so `""`, `[""]` and a stray empty entry in
        ;; a real list all mean exactly what nil/[] mean: no filter at all. Refusing
        ;; the whole search over an empty OPTIONAL filter threw away a caller's call
        ;; for a field that was asking for no restriction in the first place.
        globs
        (fn [field raw-globs]
          (let [raw
                (parse-stringish-vector raw-globs)

                items
                (cond (nil? raw) []
                      (string? raw) [raw]
                      (sequential? raw) (vec raw)
                      :else [raw])

                items
                (into [] (remove #(and (string? %) (str/blank? %))) items)]

            (if (empty? items) [] (vector-of-strings field items))))

        include
        (globs :include (get spec "include"))

        ;; `exclude` is `include`'s complement and OUTRANKS it: a file whose path or
        ;; name matches an exclude glob is never scanned, even when an include glob
        ;; matches it too. One call can therefore say "every .clj but the tests".
        exclude
        (globs :exclude (get spec "exclude"))

        nonneg-int!
        (fn [label v]
          (when (and (some? v) (not (and (integer? v) (not (neg? (long v))))))
            (throw (ex-info (str "rg " label " must be a non-negative integer")
                            {:type :ext.foundation.editing/invalid-rg-spec :field label :got v}))))

        _
        (do (nonneg-int! ":context" (get spec "context"))
            (nonneg-int! ":offset" (get spec "offset")))

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
     :exclude (or exclude [])
     :is_hidden (boolean (get spec "is_hidden"))
     :is_regex is_regex
     :limit (let [l (get spec "limit")]
              (if (and (integer? l) (pos? (long l))) (long l) default-grep-limit))
     :offset (let [o (get spec "offset")]
               (if (integer? o) (long o) 0))
     :context context
     :is_files_only is_files_only}))

(defn- make-line-matcher
  "A `(fn [line] boolean)` — true when the line matches ANY needle (OR). SMART-
   CASE either way, the SAME rule the fff candidate pre-filter (`fff/grep
   :smart-case?`) uses, so the two never disagree: a needle with NO uppercase
   matches case-INSENSITIVELY (`rg(\"key\")` finds `Key`/`KEY`/`keymap`); a needle
   WITH an uppercase letter matches case-sensitively (you typed a capital on
   purpose).

   `is-regex?` false — plain literal substring, no regex, no per-line AND. \"Both
   terms\" is a Python filter on the hits.

   `is-regex?` true — every needle is a `java.util.regex.Pattern` (`needle-pattern`)
   and a line matches when any pattern is FOUND in it. Each line is matched on its
   own, so `^`/`$` anchor the line exactly as they do in the native scanner."
  [needles is-regex?]
  (if is-regex?
    (let [patterns (mapv needle-pattern needles)]
      (fn [^String line]
        (boolean (some (fn [^java.util.regex.Pattern p]
                         (.find (.matcher p line)))
                       patterns))))
    (let [grouped
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
                   (boolean (some #(str/includes? low %) ci)))))))))

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
  (let [sum-lens (fn ^long [xs]
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
    (let [path
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
        (loop [ls (line-seq r)
               i 0
               ;; ring of the last `before-ctx` [line-no text] pairs
               ring clojure.lang.PersistentQueue/EMPTY
               ;; hits whose :after window is not full yet, oldest first — hits
               ;; therefore still complete in line order
               pending []
               out (transient [])]

          (when (zero? (rem (long i) (long search-file-poll-lines))) (check-interrupt!))
          (if-some [line (first ls)]
            (let [line-no (inc (long i))
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
                  ;; FULL text here — the model's data. Display clipping happens
                  ;; downstream (see rg-search).
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
     {:hits   [{:path :line :text :before? :after?} ...] :truncated-by KW :total-file-count N :total-file-count-exact? BOOL}  ;; content
   `:line` is the hit's 1-based line number.
     {:files  [\"path/a\" \"path/b\" ...]               :truncated-by KW :total-file-count N :total-file-count-exact? BOOL}  ;; files-only

   `:truncated-by` is `:limit` (hit count), `:bytes` (total-bytes budget), or
   `:end-of-results`. Hit/context `:text` is kept FULL (sliceable in Python via
   `r[...]`); only the wire VIEW is bounded by the 64KB per-observation clip."
  [spec]
  (let [{:keys [needles paths include exclude is_hidden is_regex limit offset context
                is_files_only]}
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
          (let [pattern
                (if (str/starts-with? pattern "**/") (str "{**/,}" (subs pattern 3)) pattern)]
            (.getPathMatcher (java.nio.file.FileSystems/getDefault) (str "glob:" pattern))))

        include-matchers
        (mapv glob-matcher include)

        exclude-matchers
        (mapv glob-matcher exclude)

        match-globs?
        (fn [matchers ^File f]
          (let [rel
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

        ;; WHICH FILES the scan may read: `include` narrows, `exclude` subtracts,
        ;; and exclude WINS — a file matching both globs is not searched.
        scan-file?
        (fn [^File f]
          (and (or (empty? include-matchers) (match-globs? include-matchers f))
               (not (and (seq exclude-matchers) (match-globs? exclude-matchers f)))))

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
        (make-line-matcher needles is_regex)

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
        ;; include/exclude globs + hidden-below-root guard here.
        candidates
        (->> (rg-fff-candidate-files roots needles is_regex search-overlay)
             (filter scan-file?)
             (remove (fn [^File f]
                       (and (not is_hidden) (rg-hidden-below-root? roots f)))))

        ;; WALL-CLOCK budget for the scan below. Every other bound here is a COUNT,
        ;; which says nothing about time: before this, a pathological tree could run
        ;; to the outer Python eval wall and return nothing at all.
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
      is_files_only (let [out
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
                      (doseq [^File f
                              files

                              :while (and (not @breadth-capped?) (not @time-capped?))]

                        (check-interrupt!)
                        (when (out-of-time?) (reset! time-capped? true))
                        (if @capped?
                          (do (when (file-has-any-hit? f matches?) (swap! total-files inc))
                              (when (>= (long (swap! probed-extra inc))
                                        (long rg-breadth-probe-limit))
                                (reset! breadth-capped? true)))
                          ;; PAGING: `total-files` is this page's position in the
                          ;; matching-file stream, so the first `offset` matches
                          ;; are counted for breadth and left out of the page.
                          (when (file-has-any-hit? f matches?)
                            (swap! total-files inc)
                            (when (> (long @total-files) (long offset))
                              (swap! out conj (rel-path f))
                              (when (>= (count @out) (long limit)) (reset! capped? true))))))
                      {:files (vec @out)
                       :missing rg-missing-paths
                       :truncated-by (cond @capped? :limit
                                           @time-capped? :time
                                           :else :end-of-results)
                       :total-file-count @total-files
                       :total-file-count-exact? (and (not @breadth-capped?) (not @time-capped?))})
      :else
      (let [out
            (atom [])

            bytes-used
            (atom 0)

            ;; PAGING: hits before `offset` are walked but not kept, so the byte
            ;; budget and the hit limit both bound the page the caller ASKED for
            ;; rather than everything up to it.
            skipped
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

        (doseq [^File f
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
                ;; :text is kept FULL — it's the model's data, sliceable in Python
                ;; via r[...]; the wire VIEW is bounded by the 64KB observation clip.
                ;; Stop on the hit limit OR the total-bytes budget (whichever first).
                (doseq [hit hits
                        :while (not @cap-reason)]

                  (if (< (long @skipped) (long offset))
                    (swap! skipped inc)
                    (do (swap! out conj hit)
                        (swap! bytes-used + (hit-bytes hit))
                        (cond (>= (count @out) (long limit)) (reset! cap-reason :limit)
                              (>= (long @bytes-used) (long max-rg-result-bytes)) (reset! cap-reason
                                                                                   :bytes)))))))))
        {:hits (vec @out)
         :missing rg-missing-paths
         :truncated-by (or @cap-reason (when @time-capped? :time) :end-of-results)
         :total-file-count @total-files
         :total-file-count-exact? (and (not @breadth-capped?) (not @time-capped?))}))))

;; Per-path consecutive-failure tracker (Roo-style loop detector)
;;
;; A process-wide atom of `{absolute-path consecutive-fail-count}`. We bump
;; on every failed WRITE that touched the path and reset to zero when the
;; same path's write applies cleanly. Once the count crosses
;; `write-fail-loop-threshold`, the error message escalates with a hard
;; "stop blind retry" hint that nudges the model out of the loop.

(def ^:private write-fail-counts (atom {}))

(def ^:private write-fail-loop-threshold 3)

(defn- bump-write-fail-count!
  ^long [^java.io.File file]
  (let [abs (.getAbsolutePath file)]
    (long (get (swap! write-fail-counts update abs (fnil inc 0)) abs))))

(defn- clear-write-fail-count!
  [^java.io.File file]
  (let [abs (.getAbsolutePath file)]
    (swap! write-fail-counts dissoc abs)))

(defn- write-loop-hint
  [^long n path]
  (when (>= n (long write-fail-loop-threshold))
    (str "Write failed " n
         " times on " path
         ". Stop retrying: re-read the target once with cat, then spend the fresh"
         " anchors it hands back — an anchor from that read cannot be stale.")))

;; write-safe — whole-file write primitive (create or overwrite)
;;
;; INTERNAL: no native tool maps onto it. Python owns whole-file writes
;; on the model-facing side (`Path.write_text`, `open(p, "w")`), which pass the
;; same `:fs/access` gate.
;;
;; Shape:
;;   {:success? true
;;    :plan   {:path :before :after :op}}
;;   {:success? false
;;    :failures [<failure-with-:reason>]
;;    :loop-hint <string-or-nil>
;;    :message  <human-readable>}
;;
;; The `:is_overwrite` knob defaults to true. `:expected_mtime` /
;; `:expected_size` guard an atomic read-modify-write on an existing file
;; against the `:mtime` / `:size` a caller read earlier.
;;
;; The write itself is ATOMIC (`atomic-replace!`): the bytes land in a sibling
;; temp file that carries the target's own mode and a rename publishes them, so
;; a failure ANYWHERE — including one mid-write — leaves the previous source
;; exactly as the caller last read it and answers `:reason :io-error`.

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
  (let [missing
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

(defn- atomic-replace!
  "Put `content` in `file` as ONE atomic replacement — the WRITE half of every
   editor's all-or-nothing promise.

   `spit` opened the target and truncated it IN PLACE, so a failure mid-write
   destroyed the only copy of the previous source and escaped `write-safe`'s
   never-throw contract as a raw java exception. Here the bytes go to a sibling
   temp file that inherits the target's own mode (a patched script keeps its +x
   bit), and only a rename publishes them: a reader sees the old file or the new
   one, never a torn one, and a failure ANYWHERE leaves the previous source
   exactly as the caller last read it.

   Answers nil when the bytes landed, or a `{:reason :io-error :message …}`
   failure the caller reports like any other refusal — nothing was written."
  [^File file rel ^String content]
  (let [^Path target
        (.toPath file)

        ^File tmp
        (io/file (.getParentFile file)
                 (str "." (.getName file) ".vis-" (java.util.UUID/randomUUID) ".tmp"))

        ^Path tmp-path
        (.toPath tmp)

        existed?
        (.exists file)]

    (try (spit tmp content)
         ;; A fresh temp file would otherwise hand back a file whose permission bits
         ;; the caller never asked to change. Where the mode is not ours to read
         ;; (a non-POSIX filesystem) the new file simply keeps the default.
         (when existed?
           (try (Files/setPosixFilePermissions
                  tmp-path
                  (Files/getPosixFilePermissions target (into-array LinkOption [])))
                (catch Throwable _ nil)))
         (try (Files/move tmp-path target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
              (catch AtomicMoveNotSupportedException _
                (Files/move tmp-path
                            target
                            (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
         nil
         (catch Throwable t
           {:reason :io-error
            :message (str "write failed: "
                          rel
                          " could not be written — "
                          (or (ex-message t) (str t))
                          ". The file is unchanged.")})
         (finally (try (Files/deleteIfExists tmp-path) (catch Throwable _ nil))))))

(defn write-safe
  "Whole-file write primitive: create a new file OR overwrite an
   existing one with `:content`, as ONE atomic replacement. Returns a
   structured result; **never throws on normal failure paths** (file exists
   with is_overwrite false, stale mtime/size, path escape, or bytes that
   could not land — the previous source stands).

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
  (let [args
        (coerce-write-args args)

        path
        (get args "path")

        content
        (str (get args "content"))

        is_overwrite
        (if (contains? args "is_overwrite") (get args "is_overwrite") true)

        is_dirty_ok
        (boolean
          (if (contains? args "is_dirty_ok") (get args "is_dirty_ok") (get args "allow_dirty")))

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
      (let [check {:edit-index 0 :path path :reason (:reason perr) :path-error perr}
            file-for-counter (try (safe-path path) (catch Throwable _ nil))
            n (when file-for-counter (bump-write-fail-count! file-for-counter))]

        {:success? false
         :failures [(cond-> check
                      n
                      (assoc :consecutive-failures n))]
         :checks [check]
         :loop-hint (when (and file-for-counter n) (write-loop-hint n path))
         :message (str "write failed: " (:message perr))})
      (let [^java.io.File file (:file resolved)
            rel (:rel resolved)
            exists? (.exists file)
            is-dir? (and exists? (.isDirectory file))
            before (when (and exists? (not is-dir?)) (slurp file))
            actual-mtime (when exists? (.lastModified file))
            actual-size (when exists? (.length file))
            fail
            (cond is-dir? {:reason :path-is-dir :message (str "write target is a directory: " rel)}
                  (and (not is_overwrite) exists?)
                  {:reason :exists
                   :path rel
                   :message
                   (str "write refused: " rel " already exists and :is_overwrite is false")}
                  ;; A whole-file write over a file with UNCOMMITTED changes is
                  ;; how a truncated reconstruction silently wipes work. Refuse
                  ;; it: surgical edits belong in patch().
                  (and exists? (not is-dir?) (not is_dirty_ok) (git/file-dirty? file))
                  {:reason :dirty
                   :path rel
                   :message (str "write refused: "
                                 rel
                                 " has UNCOMMITTED changes — a "
                                 "whole-file write would clobber edits already in flight "
                                 "(this is exactly how a truncated reconstruction wipes a "
                                 "file). Make surgical changes with patch(...) "
                                 "instead, or commit/checkout "
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
          (let [n (bump-write-fail-count! file)]
            {:success? false
             :failures [(assoc fail
                          :edit-index 0
                          :path rel
                          :consecutive-failures n)]
             :checks [(assoc fail
                        :edit-index 0
                        :path rel)]
             :loop-hint (write-loop-hint n rel)
             :message (cond-> (:message fail)
                        (>= n (long write-fail-loop-threshold))
                        (str "\n" (write-loop-hint n rel)))})
          (do (ensure-parent-dirs! file)
              (if-let [io-fail (atomic-replace! file rel content)]
                ;; The bytes never reached the target, so this reads like every other
                ;; refusal instead of surfacing a raw java IO exception: the previous
                ;; source stands and the caller reports that nothing was written.
                (let [n (bump-write-fail-count! file)]
                  {:success? false
                   :failures [(assoc io-fail
                                :edit-index 0
                                :path rel
                                :consecutive-failures n)]
                   :checks [(assoc io-fail
                              :edit-index 0
                              :path rel)]
                   :loop-hint (write-loop-hint n rel)
                   :message (:message io-fail)})
                (do (fff-index/note-fs-write!)
                    (capture-temp-write! file)
                    (clear-write-fail-count! file)
                    {:success? true
                     :plan {:path rel :before before :after content :op (if exists? :update :add)}
                     :checks [{:edit-index 0
                               :path rel
                               :op (if exists? :update :add)
                               :existed? exists?}]}))))))))

;; Batch path specs + directory listing

(defn- batch-path-specs
  "Normalize a BATCH argument — `ls`'s `paths` — into ONE option
   map per read, in request order. An entry is either a plain path string — the
   call's shared options apply to it — or an object `{\"path\" \"…\", …}` whose OWN
   selectors (`ranges`, …) override the shared ones, so a single call can index a
   DIFFERENT region of every file. `arg-key` is the CALLER's own array key, so a rejection
   quotes the key that tool actually accepts instead of another tool's. A
   malformed entry fails the whole call instead of silently dropping a path."
  [tool arg-key err-type shared entries]
  (when-not (and (sequential? entries) (seq entries))
    (throw (ex-info (str tool " `" arg-key "` must be a non-empty array of paths")
                    {:type err-type :got entries})))
  (mapv (fn [e]
          (let [p (cond (string? e) e
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

(defn- ls-one
  "List ONE normalized `ls` spec. `ls` is the DIRECTORY helper, so a file path is a
   routing mistake and says so instead of returning a degenerate one-row tree.
   A path that does NOT exist reports the nearest EXISTING directory above it
   (`nearest-existing-dir`, the same climb `grep` uses for `missing_paths`), so an
   invented address — typically a filesystem path assembled from a language
   namespace, which is wrong in a workspace with many source roots — is recovered
   by listing that real directory instead of being guessed a second time."
  [spec]
  (let [path
        (get spec "path")

        ^File f
        (safe-path path)]

    (when-not (.exists f)
      (let [near (some-> (nearest-existing-dir f)
                         rel-path)]
        (throw (ex-info (str "`ls`: no such path `" path
                             "`" (when near
                                   (str
                                     " \u2014 nearest existing directory is `" near
                                     "`. List that, or `grep` the name: a language namespace is"
                                     " not a path \u2014 this workspace has many source roots.")))
                        (cond-> {:type :ext.foundation.editing/ls-missing-path :path path}
                          near
                          (assoc :nearest near))))))
    (when-not (.isDirectory f)
      (throw (ex-info (str "`ls` lists directories \u2014 `"
                           path
                           "` is a file. Read it in python_execution: "
                           "Path(\""
                           path
                           "\").read_text()")
                      {:type :ext.foundation.editing/ls-on-file :path path})))
    (list-dir f {:depth (or (get spec "depth") 1) :is_hidden (boolean (get spec "is_hidden"))})))

(defn list-directories
  "List directories for the sandbox's `ls` helper. `args` is the string-keyed
   request — `{\"paths\" [dir | {\"path\" dir, …}, …], \"depth\" n, \"is_hidden\" b}`,
   an entry's own options overriding the shared ones — and the answer is one row
   per requested directory IN REQUEST ORDER.

   Listing a directory is not a wire round trip: it is a call inside the Python
   block the model was already running, so it costs no tool result and no native
   tool slot. What it may never lose by leaving the tool layer is the boundary,
   so the `:fs/access` gate is asked here exactly as the native readers ask it —
   an extension that hides a tree hides it from the listing too.

   Everything that can go wrong throws `ex-info` and the shim maps its `:type`
   onto the Python exception a caller can actually catch: a gate refusal is a
   `PermissionError`, a path that does not exist a `FileNotFoundError` (naming
   the nearest existing directory), a FILE path a `NotADirectoryError`."
  [env args]
  (let [entries
        (or (get args "paths")
            (when-let [p (get args "path")]
              [p]))

        specs
        (batch-path-specs "ls"
                          "paths"
                          :ext.foundation.editing/invalid-ls-args
                          (dissoc args "paths" "path")
                          entries)]

    (when-let [refusal (fs-access-refusal env :dir "file-read" (map #(get % "path") specs))]
      (throw (ex-info (str "ls blocked: " (:resolved (:target refusal)) " — " (:reason refusal))
                      {:type :ext.foundation.editing/path-protected :owner (:owner refusal)})))
    (mapv ls-one specs)))


(def ^:private ^:const patch-diff-context-lines 3)

(def ^:private ^:const patch-diff-max-render-lines 240)

;; One row per edit, and a batch is one write: a 300-edit refactor still answers in
;; a block a reader can hold, with the middle counted rather than shown.
(def ^:private ^:const patch-edit-rows-max 60)

(def ^:private ^:const patch-java-diff-max-work 20000000)

(defn- estimated-diff-size
  "Cheap O(n) LOWER BOUND on the Myers edit-script length: the size of the
   symmetric multiset difference of the two line bags. Sparse edits in a huge
   file score tiny; a full rewrite scores about n+m."
  [a b]
  (let [counts (reduce (fn [m line]
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
  (let [size
        (long (max (count a) (count b)))

        edits
        (max 1 (long (estimated-diff-size a b)))]

    (<= (* size edits) (long patch-java-diff-max-work))))

(def ^:private ^:const patch-diff-min-hunk-lines 14)

(defn- head-tail-cap
  "Bound a line vector to `limit`, keeping a HEAD and a TAIL window rather than a
   plain head-cut. A pure head-cut let a deletion-heavy preview fill the whole
   visible budget with `-` lines and bury the `+` replacement below the cut, so a
   correct edit read as a catastrophic deletion. `what`/`unit` name the truncation
   in the caller's own words: a diff loses lines, a patch report loses rows."
  ([lines ^long limit] (head-tail-cap lines limit "diff" "line"))
  ([lines ^long limit ^String what ^String unit]
   (let [lines
         (vec lines)

         n
         (long (count lines))]

     (if (<= n limit)
       lines
       (let [tail-n
             (quot limit 4)

             head-n
             (- limit tail-n)

             omitted
             (- n head-n tail-n)]

         (vec (concat (subvec lines 0 head-n)
                      [(str "... " what " truncated; " omitted " " unit "(s) omitted")]
                      (subvec lines (- n tail-n)))))))))

(defn- hunk-header? [line] (str/starts-with? (str line) "@@"))

(defn- split-diff-hunks
  "Split a unified diff into `[preamble hunks]`, each hunk a vector whose first
   element is its own `@@` header. A diff with no `@@` (pure add/delete preview)
   yields no hunks."
  [lines]
  (let [lines
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
      (let [body (max 2 (dec budget))
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
  (let [lines
        (vec lines)

        n
        (long (count lines))]

    (if (<= n (long patch-diff-max-render-lines))
      lines
      (let [[preamble hunks] (split-diff-hunks lines)]
        (if (empty? hunks)
          (head-tail-cap lines patch-diff-max-render-lines)
          (let [budget (max (long patch-diff-min-hunk-lines)
                            (- (long patch-diff-max-render-lines) (count preamble) 1))
                ;; Fill the budget hunk by hunk: a hunk is shown whole when it
                ;; fits, capped in place when a usable remainder is left, and the
                ;; rest are reported as a count instead of being half-rendered.
                [kept dropped]
                (loop [pending hunks
                       used 0
                       kept []]

                  (if-let [hunk (first pending)]
                    (let [remaining (- budget used)]
                      (cond (<= (count hunk) remaining)
                            (recur (next pending) (+ used (count hunk)) (into kept hunk))
                            (>= remaining (long patch-diff-min-hunk-lines))
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
  (let [a-count
        (long (count a))

        b-count
        (long (count b))

        limit
        (- (min a-count b-count) prefix-count)]

    (loop [i 0]
      (if (and (< i limit) (= (a (- a-count i 1)) (b (- b-count i 1)))) (recur (inc i)) i))))

(defn- prefixed-diff-lines
  [prefix lines]
  (let [lines
        (vec lines)

        n
        (long (count lines))

        shown-n
        (min n (long patch-diff-max-render-lines))

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
  (let [prefix-count
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
        (max 0 (- prefix-count (long patch-diff-context-lines)))

        post-end
        (min a-count (+ a-change-end (long patch-diff-context-lines)))

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
  (let [patch
        (DiffUtils/diff a b)

        lines
        (vec
          (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a patch patch-diff-context-lines))]

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
  (let [prefix-count
        (long (common-prefix-count a b))

        suffix-count
        (long (common-suffix-count a b prefix-count))

        start
        (max 0 (- prefix-count (long patch-diff-context-lines)))

        a-end
        (min (count a) (+ (- (count a) suffix-count) (long patch-diff-context-lines)))

        b-end
        (min (count b) (+ (- (count b) suffix-count) (long patch-diff-context-lines)))]

    (when (and (<= start a-end) (<= start b-end))
      (let [a-win
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

   A WHOLE-FILE REWRITE (a Python whole-file write —
   nothing of the old content survives) renders ONE side only: the new content
   as `+` lines under a `--- (replaced, N line(s))` marker. Both sides there
   printed the file twice on every renderer (TUI and companion app read this
   same `\"diff\"` string), so the fix belongs here, not in the renderers."
  [before after]
  (cond (= before after) nil
        (nil? before) (str/join "\n" (prefixed-diff-lines "+" (str/split-lines (or after ""))))
        (nil? after) (str "--- (deleted, " (count (str/split-lines (or before ""))) " lines)")
        :else (let [a
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

(defn- window-line-counts
  "Line counts from the common prefix/suffix WINDOW alone — the fallback for a
   pair too expensive for a real Myers diff. The overlapping part of the window
   counts as modified, the surplus on either side as added or removed."
  [a b]
  (let [pre
        (long (common-prefix-count a b))

        suf
        (long (common-suffix-count a b pre))

        removed
        (- (long (count a)) pre suf)

        added
        (- (long (count b)) pre suf)

        both
        (min removed added)]

    {"added" (- added both) "removed" (- removed both) "modified" both}))

(defn- delta-line-counts
  "Line counts from the real edit script: every delta is a source chunk replaced
   by a target chunk, so the overlapping lines are MODIFIED and only the surplus
   on one side is a pure addition or deletion."
  [a b]
  (let [^Patch patch (DiffUtils/diff a b)]
    (reduce (fn [acc ^AbstractDelta delta]
              (let [src (long (count (.getLines ^Chunk (.getSource delta))))
                    tgt (long (count (.getLines ^Chunk (.getTarget delta))))
                    both (min src tgt)]

                (-> acc
                    (update "modified" + both)
                    (update "removed" + (- src both))
                    (update "added" + (- tgt both)))))
            {"added" 0 "removed" 0 "modified" 0}
            (.getDeltas patch))))

(defn- line-change-counts
  "`{\"added\" a \"removed\" r \"modified\" m}` for one file's before→after, or nil
   when nothing changed. The `:diff` is capped hunk-wise for rendering, so these
   counts are computed from the content itself and stay exact for the whole file
   even when the rendered diff is truncated. A new file is all additions, a
   deleted one all removals."
  [before after]
  (cond (= before after) nil
        (nil? before)
        {"added" (long (count (str/split-lines (or after "")))) "removed" 0 "modified" 0}
        (nil? after) {"added" 0 "removed" (long (count (str/split-lines before))) "modified" 0}
        :else (let [a
                    (vec (str/split-lines before))

                    b
                    (vec (str/split-lines after))]

                (if (java-diff-affordable? a b) (delta-line-counts a b) (window-line-counts a b)))))

;; Symbol declarations
;;
;; Underlying `xxx-tool` defs retain developer docs + arglists. Each symbol
;; supplies compact routing/semantics in `:description`, which is what
;; `doc(name)` answers. `:symbol` overrides the var name for the
;; model-facing surface; everything else (examples,
;; error hook, result spec) lives in opts because it has nothing to do with
;; the function's signature.

;; cat / patch — the ANCHORED read/write pair
;;
;; An edit needs a COORDINATE. Prose, config, a comment, a docstring line and
;; every language have none by themselves, so the only address left is the old
;; text quoted back — O(region) tokens, verified by nothing, thrown away after
;; one use. These two verbs are that coordinate:
;;
;;   `cat`   READS a region and its output IS the address — every line arrives
;;           as `<line>:<hash>│ <text>`.
;;   `patch` SPENDS that address: it names a span and the new text, and NEVER
;;           restates the text being replaced.
;;
;; The read is forgiving (a stale anchor falls back to its line number); the
;; WRITE is verified and refuses — a stale or misplaced anchor, or an edit that
;; would not parse, writes nothing and hands back the anchor that fixes it.

;; The refusal payload crosses the boundary as ex-data -> tool failure -> wire.
(s/def :ext.editing.patch/reason
  #{:anchor-malformed :anchor-line-out-of-range :anchor-not-found :anchor-misplaced
    :anchor-range-inverted :replacement-missing :replacement-is-anchor :parse-broken :file-not-found
    :path-is-dir :path-escape})
(s/def :ext.editing.patch/current-anchor :ext.editing.hashline/anchor)
(s/def :ext.editing.patch/stated-line pos-int?)
(s/def :ext.editing.patch/found-lines (s/coll-of pos-int? :kind vector?))
(s/def :ext.editing.patch/error-line pos-int?)
(s/def :ext.editing.patch/refusal
  (s/keys :req-un [:ext.editing.patch/reason]
          :opt-un [:ext.editing.patch/current-anchor :ext.editing.patch/stated-line
                   :ext.editing.patch/found-lines :ext.editing.patch/error-line]))

(defn- positional-only!
  "Refuse an options MAP in a slot that takes a positional argument. `cat` and
   `patch` are positional ON PURPOSE — a path and its addresses, no key vocabulary
   to learn — so a folded kwargs map arriving here is a call the model has to
   rewrite, not a shape to guess at. `patch`'s edits slot is NOT guarded: a map
   there is one edit of the batch."
  [op v]
  (when (map? v)
    (throw (ex-info (str (name op)
                         " takes POSITIONAL arguments, not an options map — "
                         (if (= op :cat)
                           "cat(path), cat(path, start) or cat(path, start, end)."
                           (str "patch(path, edits), edits "
                                "[{\"from\": anchor, \"to\": anchor, \"replace\": text}].")))
                    {:type :ext.foundation.editing/positional-only :op op})))
  v)

(defn- anchored-file-content
  "The text of an existing, readable file at `path`, resolved through
   `safe-path` so cat/patch stay inside the workspace like every other verb."
  ^String [path]
  (slurp (ensure-existing-file! (safe-path (positional-only! :cat path)))))

(defn- cat-endpoint-line
  "Resolve ONE `cat` endpoint to a 1-based line in `content`. An endpoint is
   EITHER a line number or a `<line>:<hash>` anchor, and the two mix freely
   across `start`/`end` — the read is the forgiving side, so a stale anchor
   resolves through `resolve-anchor-range-read` (which falls back to the
   anchor's line number) and only a genuinely unlocatable one refuses. A NEGATIVE
   line counts from the END — -1 is the last line — so `cat(path, -50)` is the
   tail 50 lines. A numeric `end` past the last line CLAMPS to it — asking for
   the tail is not an error — while a `start` past the end still refuses."
  ^long [^String content endpoint which ^long line-count]
  (if (hashline/anchor-string? endpoint)
    (let [r (hashline/resolve-anchor-range-read content endpoint endpoint)]
      (if-let [err (:error r)]
        (throw (ex-info
                 (str
                   "cat: the " which
                   " anchor " (pr-str endpoint)
                   " cannot be located — "
                   (case (:reason err)
                     :anchor-malformed
                     "an anchor is `<line>:<hash>` exactly as cat/grep print it, e.g. \"120:7f2\"."

                     :anchor-line-out-of-range
                     (str "line " (:line err) " is past this file's " (:lines err) " lines.")

                     (str "reason " (name (:reason err)) ".")))
                 {:type :ext.foundation.editing/anchor-unresolved
                  :reason (:reason err)
                  :which which
                  :anchor (str endpoint)}))
        (long (:from-line r))))
    (let [n
          (long (cond (integer? endpoint) (long endpoint)
                      (number? endpoint) (long endpoint)
                      (and (string? endpoint) (parse-long (str/trim (str endpoint))))
                      (long (parse-long (str/trim (str endpoint))))
                      :else (throw (ex-info (str "cat: the "
                                                 which
                                                 " endpoint must be a line number or a "
                                                 "`<line>:<hash>` anchor, got "
                                                 (pr-str endpoint)
                                                 ".")
                                            {:type :ext.foundation.editing/invalid-range
                                             :which which
                                             :endpoint endpoint}))))

          ;; A NEGATIVE endpoint counts from the END, Python-style: -1 IS the last
          ;; line, so `cat(path, -50)` reads the tail 50 lines and
          ;; `cat(path, -50, -30)` the window between them. It is resolved BEFORE
          ;; the range check, so every message names a real 1-based line; a
          ;; magnitude past the top clamps to line 1, because asking for more tail
          ;; than the file has is a reader asking for the whole file.
          n
          (long (if (neg? n) (max 1 (+ line-count n 1)) n))]

      (when (or (< n 1) (and (> n line-count) (not= which "end")))
        (throw
          (ex-info
            (str "cat: " which " line " n " is outside this file's 1.." line-count " lines.")
            {:type :ext.foundation.editing/invalid-range :which which :line n :lines line-count})))
      ;; An END past EOF CLAMPS instead of refusing: `cat(path, 2172, 2212)` on a
      ;; 2210-line file is a reader asking for the tail, and a refusal throws away
      ;; everything the block printed before the call. A START past EOF still
      ;; refuses — nothing is there to show and the address is genuinely wrong.
      (if (> n line-count) line-count n))))

(defn- cat-one
  "`cat`'s whole implementation: ONE file, a closed line window, ONE string.
   Every rendered line — blanks included, so the read is gap-free and every line
   is addressable — carries its `<line>:<hash>` anchor. The window is capped at
   `default-cat-limit` lines and `max-cat-window-bytes`; a clipped read SAYS so
   on its last line and names the call that continues it, so nothing is ever
   silently dropped."
  ^String [path from to]
  (let [content
        (anchored-file-content path)

        rel
        (rel-path (safe-path path))

        lines
        (hashline/split-content-lines content)

        line-count
        (long (count lines))]

    (if (zero? line-count)
      ""
      (let [from-line
            (if (some? from)
              (cat-endpoint-line content (positional-only! :cat from) "start" line-count)
              1)

            to-line
            (if (some? to)
              (cat-endpoint-line content (positional-only! :cat to) "end" line-count)
              line-count)

            _
            (when (> from-line to-line)
              (throw (ex-info
                       (str "cat: start line "
                            from-line
                            " is after end line "
                            to-line
                            " — order the window.")
                       {:type :ext.foundation.editing/invalid-range :from from-line :to to-line})))

            wanted
            (subvec lines (dec from-line) to-line)

            ;; Two caps, whichever bites first. `byte-capped` walks the window once
            ;; and stops at the byte ceiling, so one enormous line cannot blow the
            ;; block's printed-output budget on its own.
            line-capped
            (vec (take (long default-cat-limit) wanted))

            byte-capped
            (loop [i
                   0

                   total
                   0

                   acc
                   []]

              (if (>= i (count line-capped))
                acc
                (let [next-total (+ total (count (nth line-capped i)) 8)]
                  (if (and (pos? i) (> next-total (long max-cat-window-bytes)))
                    acc
                    (recur (inc i) next-total (conj acc (nth line-capped i)))))))

            shown
            (long (count byte-capped))

            last-line
            (+ from-line (dec shown))

            body
            (hashline/render-hashline-block (map-indexed (fn [i s]
                                                           [(+ from-line (long i)) s])
                                                         byte-capped))]

        (if (>= last-line to-line)
          body
          (str body
               "\n… clipped at "
               shown
               (if (= shown (count line-capped)) " lines" " lines (50 KiB window)")
               " — file has "
               line-count
               "; continue with cat(\""
               rel
               "\", "
               (inc last-line)
               ", "
               (min line-count (+ last-line (long default-cat-limit)))
               ")"))))))

(defn- cat-envelope
  "`cat-one`'s string in the canonical envelope every symbol must answer. The
   MODEL still sees the bare string — the host unwraps `:result` on the way into
   the sandbox — but a symbol that RETURNS a raw String is refused at the
   extension boundary before its value ever gets there."
  [path start end]
  (let [text (cat-one path start end)]
    (tool-success {:op :cat
                   :path path
                   :kind :file
                   :result text
                   :metadata {:mode :cat :line-count (count (str/split-lines text))}})))

(defn- cat-tool
  "cat — read one file's region as patch-ready `<line>:<hash>` text.

     cat(path)                whole file, capped
     cat(path, start)         start -> EOF, capped
     cat(path, start, end)    closed range, inclusive
     cat(path, -50)           the tail 50 lines
     cat(path, -50, -30)      the window between them

   `start`/`end` are line numbers or anchors and mix freely; a NEGATIVE line
   counts from the END, where -1 is the last line. They are NOT named
   `from`/`to`: `from` is a Python KEYWORD, so a declared parameter carrying
   that name cannot be compiled into the signature stub the sandbox reports and
   `inspect.signature(cat)` would fall back to `(*a, **k)`.

   What the model receives is a plain STRING, never a map: `print(cat(...))`
   shows it and `cat(...).splitlines()` slices it. Its every line is a `patch`
   argument, so the read that finds the region is also the read that addresses
   it."
  ([path] (cat-envelope path nil nil))
  ([path start] (cat-envelope path start nil))
  ([path start end] (cat-envelope path start end)))

;; patch

(defn- anchor-window-hint
  "The `cat(...)` call that re-reads the neighbourhood of `line` — appended to a
   refusal so recovery is ONE call and never a hunt for the region again."
  [rel ^long line]
  (str "cat(\""
       rel
       "\", "
       (max 1 (- line (long patch-diff-context-lines)))
       ", "
       (+ line (long patch-diff-context-lines))
       ")"))

(defn- patch-refusal!
  "Raise ONE refusal. A refusal RAISES rather than returning a row: a returned
   failure row is skimmable, and this one carries the anchor that fixes the call.
   `lines` are the message's body lines, already indented by the caller."
  ([rel refusal message-lines]
   (patch-refusal! rel refusal message-lines "patch refused — nothing was written."))
  ([rel refusal message-lines head]
   (throw (ex-info (str/join "\n" (cons head message-lines))
                   (assoc refusal
                     :type :ext.foundation.editing/patch-refused
                     :path rel)))))

(defn- anchor-refusal!
  "Turn a `hashline` resolution error into the patch refusal the model reads.
   Each shape names WHAT disagreed and hands back the one-step recovery: the
   anchor that is really there, or the window to re-read — and, since the batch is
   atomic, WHICH edit of the call disagreed."
  [rel
   {:keys [reason which hash stated-line found-lines current-anchor current-text line lines
           from-line to-line anchor edit-index edit-count]}]
  (let [slot
        (if (= :to which) "to" "from")

        refusal
        (cond-> {:reason reason}
          stated-line
          (assoc :stated-line stated-line)

          (seq found-lines)
          (assoc :found-lines (vec found-lines))

          current-anchor
          (assoc :current-anchor current-anchor)

          edit-index
          (assoc :edit-index edit-index))]

    (patch-refusal!
      rel
      refusal
      (case reason
        :anchor-malformed
        [(str "  " rel "  " slot " " (pr-str anchor))
         "  an anchor is `<line>:<hash>`, exactly as cat and grep print it (e.g. 4439:a80)."
         "  a bare line number is not accepted: patch verifies the content it overwrites."]

        :anchor-line-out-of-range
        [(str "  " rel "  " slot " names line " line) (str "  the file has " lines " lines.")]

        :anchor-not-found
        [(str "  " rel "  " slot " " stated-line ":" hash)
         (str "  line "
              stated-line
              " now hashes "
              (hashline/line-hash current-text)
              ", and no line within "
              hashline/hash-line-drift-tolerance
              " lines carries "
              hash
              ".")
         (str "  current anchor at "
              stated-line
              " →  "
              current-anchor
              hashline/hashline-gutter
              current-text)
         (str "  retry with that anchor, or re-read: " (anchor-window-hint rel stated-line))]

        :anchor-misplaced
        [(str "  " rel "  " slot " " stated-line ":" hash)
         (str "  "
              hash
              " is not at line "
              stated-line
              "; it is at "
              (if (= 1 (count found-lines))
                (str "line " (first found-lines))
                (str "lines " (str/join ", " found-lines)))
              ", beyond the "
              hashline/hash-line-drift-tolerance
              "-line drift window.")
         (if current-anchor
           (str "  current anchor →  " current-anchor)
           (str "  re-read before retrying: " (anchor-window-hint rel (first found-lines))))
         "  the anchor is stale or belongs to another region; confirm with cat before retrying."]

        :anchor-range-inverted
        [(str "  " rel "  `from` resolves to line " from-line ", after `to`'s line " to-line ".")
         "  order the span: `from` first, `to` last."]

        [(str "  " rel "  " slot " could not be resolved (" (name reason) ").")])
      ;; The batch is atomic, so ONE stale anchor refuses every edit in the call:
      ;; the coordinate is what turns "an anchor is stale" into "edit 2 of 5 is".
      (str "patch refused"
           (when edit-count (str " at edit " (inc (long edit-index)) " of " edit-count))
           " — nothing was written."))))

(defn- language-balancer
  "The `:balance-fn` an active language pack registered for `lang` — the delimiter
   repair the editors may try on a splice that would otherwise be refused — or nil
   when the language has no pack, in which case a broken splice is refused exactly
   as it always was.

   The lookup lives HERE, at the tool boundary, and not inside `balance`:
   the repair is a POLICY of the model-facing editors, so the balancer under
   them stays deterministic — an internal caller or a test gets precisely the splice
   it asked for unless a tool hands the balancer down."
  [lang]
  (when-let [want (some-> lang
                          name
                          str/lower-case)]
    (some (fn [entry]
            (let [f (:balance-fn entry)]
              (when (and (ifn? f)
                         (= want
                            (some-> (:language entry)
                                    str
                                    str/lower-case)))
                f)))
          (try (mapcat :ext/language-tools (extension/registered-extensions))
               (catch Throwable _ nil)))))

(defn- patch-parse-gate
  "The consistency check, run AFTER splicing and BEFORE writing. Answers
   `{:content <what to write> :clause <status clause>}`, or raises when the edit
   would break a file that parsed clean.

     1. supported language    -> re-parse the new content with tree-sitter;
     2. new errors, old clean -> ask `balance/rebalance` for a delimiter repair of
                                 the WHOLE spliced file, confined to `spans` — the
                                 lines THIS call wrote — with `original`, the text
                                 this edit replaced, as the evidence for WHERE a
                                 delimiter it dropped belongs. A confined, delimiters-only
                                 repair is written and NAMED on the status line;
                                 anything else REFUSES and says why the repair was
                                 rejected, because a repair that reaches outside the
                                 edit is guessing at code nobody here wrote;
     3. already broken        -> WRITE, and say it is still broken (you must be
                                 able to repair a broken file);
     4. no grammar            -> no gate, no clause."
  [rel lang ^String original ^String updated span-label spans]
  (if-not lang
    {:content updated :clause ""}
    (let [clean?
          (fn [^String s]
            (empty? (parse/error-nodes lang s)))

          after
          (parse/error-nodes lang updated)]

      (if (empty? after)
        {:content updated :clause "  parse: clean"}
        (let [before
              (parse/error-nodes lang original)

              e
              (first after)]

          (if (seq before)
            {:content updated :clause (str "  parse: still broken at line " (:line e))}
            (let [repair (balance/rebalance {:balancer (language-balancer lang)
                                             :parses-clean? clean?
                                             :source updated
                                             :spans spans
                                             :original original})]
              (if (:ok? repair)
                {:content (:content repair)
                 :clause
                 (str "  parse: clean (delimiters repaired: " (str/join ", " (:notes repair)) ")")}
                (patch-refusal!
                  rel
                  {:reason :parse-broken :error-line (:line e)}
                  (into
                    [(str "  " rel "  " span-label)
                     (str "  "
                          (name lang)
                          ": "
                          (if (:missing? e) "MISSING" "ERROR")
                          " node at line "
                          (:line e)
                          ", col "
                          (:col e)
                          (when-let [t (some-> (:text e)
                                               str
                                               str/trim
                                               not-empty)]
                            (str " — near `" (subs t 0 (min 60 (count t))) "`")))
                     "  the file parsed clean before this edit, so the replacement introduced it."]
                    (when-let [why (:why repair)]
                      [(str "  " why " — re-read the region and fix the replacement.")]))
                  "patch refused — the edit would not parse; nothing was written.")))))))))

(defn- patch-status-line
  "The one status line every successful patch answers with: what was written, how
   many edits landed in that ONE write, how the file's line count moved, and the
   trailing `clauses` the write earned — the parse verdict, and any note about a
   replacement. The batch is atomic, so this line describes the FILE; the per-edit
   detail belongs in the rows under it."
  [rel edit-count old-count new-count clauses]
  (let [delta (- (long new-count) (long old-count))]
    (str "patched "
         rel
         "  "
         edit-count
         (if (= 1 (long edit-count)) " edit" " edits")
         "  "
         old-count
         " → "
         new-count
         (if (= 1 (long new-count)) " line" " lines")
         (when-not (zero? delta) (str " (" (if (pos? delta) "+" "") delta ")"))
         clauses)))

(defn- pad-cell
  "One report cell padded to `width`, `:right` for the numbers that read as a
   column. An unaligned report is read row by row; an aligned one is read column
   by column, which is how a caller finds the edit it cares about."
  [s ^long width side]
  (let [pad (apply str (repeat (max 0 (- width (count (str s)))) \space))]
    (if (= :right side) (str pad s) (str s pad))))

(defn- patch-edit-rows
  "One row per applied edit, IN THE ORDER THE CALLER LISTED THEM, each carrying the
   anchors that are live AFTER the write:

     1  41..41    → 2 lines   41:9c2 .. 42:7ab
     2  88..90    → 1 line    89:0af
     3  120..120  → deleted

   These rows ARE the answer. A batch cannot be described by a window around one
   edit, and the fresh anchor is the only part of an echo the next call spends —
   so the text is dropped and the addresses are kept. `head-tail-cap` bounds them,
   so a 300-edit batch still answers in a block a reader can hold."
  [applied new-lines]
  (let [anchor-at
        (fn [^long ln]
          (hashline/line-anchor ln (nth new-lines (dec ln) "")))

        cells
        (mapv (fn [{:keys [index from-line to-line new-from written unchanged?]}]
                (let [n
                      (long written)

                      outcome
                      (cond (zero? n) "deleted"
                            unchanged? "unchanged"
                            :else (str n (if (= 1 n) " line" " lines")))

                      anchors
                      (when (pos? n)
                        (let [from-anchor
                              (anchor-at new-from)

                              to-anchor
                              (anchor-at (+ (long new-from) (dec n)))]

                          (if (= from-anchor to-anchor)
                            from-anchor
                            (str from-anchor " .. " to-anchor))))]

                  [(str (inc (long index))) (str from-line ".." to-line) (str "→ " outcome)
                   (str anchors)]))
              (sort-by :index applied))

        width
        (fn [col]
          (reduce max 0 (map #(count (nth % col)) cells)))

        rows
        (mapv (fn [[idx span outcome anchors]]
                (str/trimr (str "  " (pad-cell idx (width 0) :right)
                                "  " (pad-cell span (width 1) :left)
                                "  " (pad-cell outcome (width 2) :left)
                                "  " anchors)))
              cells)]

    (head-tail-cap rows patch-edit-rows-max "edit rows" "row")))

(defn- edit-field
  "One `edits` entry's value for `k`, whichever key form it arrived in: string keys
   off the Python call, keyword keys when Clojure code builds the batch itself."
  [entry ^String k]
  (if (contains? entry k) (get entry k) (get entry (keyword k))))

(defn- patch-edits-shape!
  "Refuse — before the file is even read — every BATCH SHAPE that would damage it
   silently, and answer the batch as `[{:index :from :to :replace}]`.

   An entry with NO `replace` DELETED its span: `(str nil)` is the empty string, so
   a truncated entry erased lines and reported success. A key that is not
   `from`/`to`/`replace` is NAMED rather than ignored, because a `replacement` typo
   otherwise reads as exactly that missing replacement. An empty batch is a call
   that meant to edit and never said what, and a non-map entry is the positional
   call this verb no longer has."
  [rel edits]
  (let
    [batch
     (normalize-edits-arg edits)

     shape-lines
     ["  patch(path, edits) takes a LIST of edit maps:"
      "    patch(path, [{\"from\": \"41:9c2\", \"replace\": \"…\"},"
      "                 {\"from\": \"88:0af\", \"to\": \"90:7ab\", \"replace\": \"…\"}])"
      "  `to` defaults to `from`, `replace: \"\"` deletes the span, and the edits may be listed in any order."]]

    (when-not (and (sequential? batch) (seq batch))
      (patch-refusal! rel
                      {:reason :edits-missing}
                      (cons (str "  " rel "  edits " (pr-str edits)) shape-lines)))
    (into
      []
      (map-indexed
        (fn [i entry]
          (let [at (str "  " rel "  edit " (inc (long i)) " of " (count batch) " ")]
            (when-not (map? entry)
              (patch-refusal! rel
                              {:reason :edit-not-a-map :edit-index i}
                              (cons (str at "is " (pr-str entry) ", not an edit map.")
                                    shape-lines)))
            (let [unknown (remove #{"from" "to" "replace"}
                            (map #(if (keyword? %) (name %) (str %)) (keys entry)))
                  from (edit-field entry "from")
                  replacement (edit-field entry "replace")]

              (when (seq unknown)
                (patch-refusal!
                  rel
                  {:reason :edit-unknown-key :edit-index i :unknown-keys (vec unknown)}
                  (cons (str at
                             "carries "
                             (str/join ", " (map pr-str unknown))
                             " — the only keys are from, to and replace.")
                        shape-lines)))
              (when (nil? from)
                (patch-refusal! rel
                                {:reason :anchor-missing :edit-index i}
                                (cons (str at "names no `from` anchor.") shape-lines)))
              (when (nil? replacement)
                (patch-refusal!
                  rel
                  {:reason :replacement-missing :edit-index i}
                  [(str at "is from " (pr-str (str from)) ", but carries no `replace`.")
                   "  an absent replacement is NOT a deletion — patch will not guess at erasing lines."
                   "  to DELETE the span, say so: {\"from\": anchor, \"replace\": \"\"}."]))
              {:index i :from from :to (or (edit-field entry "to") from) :replace replacement})))
        batch))))

(defn- patch-overlap!
  "Refuse a batch whose spans touch the same line. Two edits over one line have no
   defined result — whichever the splice applied last would silently win — so the
   refusal IS the feature: it names both edits with their resolved ranges, and
   nothing is written."
  [rel resolved]
  (doseq [[a b] (partition 2 1 (sort-by :from-line resolved))]
    (when (>= (long (:to-line a)) (long (:from-line b)))
      (let [i (inc (long (:index a)))
            j (inc (long (:index b)))]

        (patch-refusal!
          rel
          {:reason :edits-overlap :edit-index (:index a) :other-edit-index (:index b)}
          [(str "  " rel "  edit " i " covers lines " (:from-line a) ".." (:to-line a))
           (str "  " rel "  edit " j " covers lines " (:from-line b) ".." (:to-line b))
           "  two edits over the same line have no defined result; make them ONE edit over the whole span."]
          (str "patch refused — edits " i " and " j " overlap; nothing was written."))))))

(defn- patch-file!
  "Every anchored edit for ONE file, resolved against ONE read and applied in ONE
   write. Atomic for the FILE: every span resolves, every shape is checked and the
   spliced result is parse-gated BEFORE anything reaches disk, so a refusal — a
   stale anchor, an overlap, a syntax break — leaves the file exactly as the caller
   last read it. The splice runs from the END of the file backwards, so the order
   the edits arrive in is irrelevant and no anchor from the caller's own read can
   go stale mid-batch. The answer is the status line and one row per edit, carrying
   the anchors that are live AFTER the write."
  [path edits]
  (let [^File f
        (ensure-existing-file! (safe-path (positional-only! :patch path)))

        rel
        (rel-path f)

        batch
        (patch-edits-shape! rel edits)

        total
        (count batch)

        original
        (slurp f)

        ;; Resolve EVERY span against the ONE read before a character moves: a span is
        ;; a char range, not new content, so the whole batch is verified while the file
        ;; is still exactly what the caller read.
        resolved
        (mapv (fn [{:keys [index from to replace]}]
                (let [;; A drifted `\uXXXX` otherwise reaches disk as six literal characters.
                      ^String new-text
                      (escapes/decode-unicode-escapes (str replace))

                      span
                      (hashline/resolve-anchor-edit-span original from to new-text)]

                  (when-let [err (:error span)]
                    (anchor-refusal! rel
                                     (assoc err
                                       :anchor (str (if (= :to (:which err)) to from))
                                       :edit-index index
                                       :edit-count total)))
                  (assoc span
                    :index index
                    :new-text new-text)))
              batch)

        _
        (patch-overlap! rel resolved)

        ;; Descending by start offset: every earlier span keeps the offsets it resolved
        ;; with, so one pass applies the whole batch without re-resolving anything.
        updated
        (reduce (fn [^String acc {:keys [start end replacement]}]
                  (str (subs acc 0 (long start)) replacement (subs acc (long end))))
                original
                (sort-by :start #(compare %2 %1) resolved))

        ;; Only a CODE grammar may gate a write. `detect-language` answers for every
        ;; extension it knows — `.txt` is `vimdoc`, whose grammar reports an ERROR node
        ;; on ordinary prose — so gating on it alone told EVERY prose edit that its file
        ;; was broken. `parse/code-languages` is the set where a parse error means
        ;; something; vimdoc, markdown and csv are not in it.
        lang
        (let [l (parse/detect-language rel)]
          (when (contains? parse/code-languages l) l))

        ;; Where each edit ENDED UP: walk the spans in file order carrying the line
        ;; delta every earlier edit already applied, so every anchor reported below is
        ;; one a next call can spend without a `cat` — and so the parse gate knows
        ;; exactly which lines of the NEW content this call may have its delimiters
        ;; repaired in. Pure: it reads the resolved spans, never the write.
        applied
        (:rows
          (reduce
            (fn [{:keys [^long delta rows]} {:keys [index start end from-line to-line replacement]}]
              (let [;; What the splice ACTUALLY put in the file. The matched
                    ;; region's terminator stays OUTSIDE the span, so a mid-file
                    ;; replacement always closes its last line and only a span
                    ;; that reaches EOF carries a terminator of its own — counting
                    ;; the replacement's own lines instead reported one line too
                    ;; few for every replacement that ended in a newline, and every
                    ;; later row's anchor inherited the drift.
                    written
                    (long (let [text (str replacement)]
                            ;; The SPLICED text decides, not the caller's `replace`:
                            ;; a replacement that reduces to nothing — `"\n"` over a
                            ;; span that ends a file with no final newline — writes
                            ;; no line at all.
                            (if (= "" text)
                              0
                              (let [breaks (count (filter #(= \newline %) text))]
                                (if (and (= (long end) (count original)) (str/ends-with? text "\n"))
                                  breaks
                                  (inc breaks))))))

                    replaced
                    (inc (- (long to-line) (long from-line)))]

                {:delta (+ delta (- written replaced))
                 :rows (conj rows
                             {:index index
                              :from-line from-line
                              :to-line to-line
                              :new-from (+ (long from-line) delta)
                              :written written
                              :unchanged? (= (subs original (long start) (long end))
                                             (str replacement))})}))
            {:delta 0 :rows []}
            (sort-by :start resolved)))

        gate
        (patch-parse-gate rel
                          lang
                          original
                          updated
                          (str total
                               (if (= 1 (long total)) " edit" " edits")
                               ", lines " (reduce min (map :from-line resolved))
                               ".." (reduce max (map :to-line resolved)))
                          (mapv (fn [{:keys [new-from written]}]
                                  (let [from (long new-from)]
                                    (if (zero? (long written))
                                      ;; A deletion writes NO line: what it left behind is a
                                      ;; SEAM between the line above it and the line that moved
                                      ;; up into its place, and the delimiters it took with it
                                      ;; belong to one of those two. Naming the line the deleted
                                      ;; text used to occupy names a line that no longer exists,
                                      ;; so every repair of a deletion fell "outside the edit".
                                      [(max 1 (dec from)) from]
                                      [from (+ from (dec (long written)))])))
                                applied))

        ;; What actually reaches disk: the splice, or the delimiter repair the gate
        ;; accepted for it. Every anchor, count and diff below is taken from THIS.
        ^String written-content
        (:content gate)

        parse-clause
        (:clause gate)

        ;; `cat`'s gutter is an ADDRESS, not text. A replacement that carries one is a
        ;; copied read, and it lands in the file verbatim — say so on the status line
        ;; rather than refuse, because our own docs quote sample `cat` output.
        gutter-clause
        (when (some (fn [{:keys [new-text]}]
                      (some (fn [l]
                              (re-find #"^\s*\d+:[0-9a-f]{3}│ " (str l)))
                            (hashline/split-content-lines new-text)))
                    resolved)
          "  note: a replacement carries a `line:hash│ ` gutter, written verbatim")

        ;; is_dirty_ok: an anchored span replace is SURGICAL and content-verified —
        ;; the dirty guard exists to stop a blind whole-file rewrite, not this.
        result
        (write-safe {"path" path "content" written-content "is_dirty_ok" true})]

    (if-not (:success? result)
      (throw (ex-info (str "patch refused — nothing was written.\n  " (:message result))
                      {:type :ext.foundation.editing/patch-refused
                       :reason (or (:reason (first (:failures result))) :write-refused)
                       :path rel}))
      (let [new-lines (hashline/split-content-lines written-content)]
        (tool-success
          {:op :patch
           :path (get-in result [:plan :path])
           :kind :file
           :result (str (patch-status-line rel
                                           total
                                           (count (hashline/split-content-lines original))
                                           (count new-lines)
                                           (str parse-clause gutter-clause))
                        "\n"
                        (str/join "\n" (patch-edit-rows applied new-lines)))
           ;; The unified diff is METADATA, not payload: the human channel can
           ;; render the write in full while the model pays only for the fresh
           ;; anchors it will actually spend.
           :metadata {:mode :patch
                      :file-count 1
                      :changed-count (if (= original written-content) 0 1)
                      :edit-count total
                      :from-line (reduce min (map :from-line resolved))
                      :to-line (reduce max (map :to-line resolved))
                      :diff (unified-diff-text original written-content)
                      :lines (line-change-counts original written-content)
                      :file-befores [{:path rel :before original}]}})))))

(defn- patch-tool
  "patch — every anchored edit for ONE file, applied atomically in ONE write.

     patch(path, [{\"from\": anchor, \"replace\": new}])              ONE line
     patch(path, [{\"from\": a, \"to\": b, \"replace\": new}])        a span
     patch(path, [{\"from\": anchor, \"replace\": \"\"}])               delete

   `to` defaults to `from`; `replace` is required — an absent one is refused, never
   read as a deletion. Every anchor resolves against ONE read, so the edits may be
   listed in ANY order and no anchor from the caller's own read goes stale
   mid-batch; two edits over the same line are refused. The anchors are the ones
   `cat` and `grep` print; a bare line number is refused on purpose — the hash is
   what makes a wrong-line write impossible. One file per call: a refusal writes
   NOTHING."
  [path edits]
  (patch-file! path edits))

(def cat-symbol
  (vis/symbol
    #'cat-tool
    {:symbol 'cat
     :result
     (str
       "A plain string: one `<line>:<hash>│ <text>` line per source line, blanks included. "
       "No map, no keys. Clipped windows say so on the last line and name the call that continues them.")
     :description
     (str
       "SHOW the lines of one file — read, view or display any window of any text file, and the "
       "region comes back as patch-ready `line:hash` text: the read that produces the address "
       "`patch` spends. `cat(path)`, `cat(path, start)`, `cat(path, start, end)`; `start`/`end` "
       "are line numbers or anchors, and a NEGATIVE line counts from the end (-1 is the last line), "
       "so `cat(path, -50)` is the tail 50 lines and `cat(path, -50, -30)` the window between them. "
       "`ls(dir)` lists directories; `grep` locates the file first.")
     :call {:pos ["path"] :opt-pos ["start" "end"]}
     :before-fn (fs-access-before-fn :cat :file "file-read" read-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :cat :file)}))

(def patch-symbol
  (vis/symbol
    #'patch-tool
    {:symbol 'patch
     :result
     (str
       "A plain string: one status line — path, edit count, lines before → after, parse verdict — "
       "then one row per edit with the anchors that are LIVE AFTER the write, so the next patch needs no cat.")
     :description
     (str
       "Apply EVERY anchored edit for one file in one atomic write — replace lines, rewrite a docstring in "
       "place, swap a function body, rename an identifier through a file, edit a config file without retyping "
       "it, in prose, code or any language: "
       "`patch(path, [{\"from\": a, \"replace\": new}, {\"from\": b, \"to\": c, \"replace\": \"\"}])`. `to` "
       "defaults to `from`, `replace: \"\"` deletes, and the edits may be listed in ANY order because every "
       "anchor resolves against ONE read. NEVER restate the text you are replacing. Atomic: a stale anchor, "
       "an overlap or a syntax-breaking write refuses the WHOLE batch and writes NOTHING, naming the edit "
       "and carrying the correct anchor. A delimiter you OMITTED is put back where the text you replaced "
       "had it — mid-line, or a lost opening `(` — and the line it produced is named; one you WROTE is never "
       "deleted or retyped, so a closer too many, or `(` typed where `]` belongs, is refused instead of guessed at.")
     :call {:pos ["path" "edits"]}
     :before-fn (plan-gated-before-fn :patch :file read-arg-paths)
     :tag :mutation
     :on-error-fn (tool-failure-on-error :patch :file)}))

(def grep-symbol
  (vis/symbol
    #'grep-tool
    {:symbol 'grep
     :result
     (str
       "Text, not a map: line 1 summarizes (hits, files, truncation and the exact next call); then "
       "per path a header and one `  <line>:<hash>│ <text>` row per hit, context lines anchored "
       "too, `⋮` marking a gap. Fuzzy NAME matches follow as `~ path`, then `hint:`. Feed an "
       "anchor straight to `patch`. The page CONTINUES ITSELF when capped: `next(r)` is the next "
       "page (StopIteration when complete, so `next(r, None)` is the sentinel form), `r.pages()` "
       "walks them bounded, `r.all()` is every page as one text, `r.next_offset` is where the "
       "next page starts.")
     :description
     (str
       "FIND WHERE something is — the codebase-wide search that answers `where is X`, `who calls this "
       "function`, `which file defines this class`, `every usage of this symbol`. Scans the whole repo, "
       "or only the paths you name. "
       "ONE options map is the whole call — `grep({\"query\": q, \"paths\": [\"src\"]})`, or that "
       "same map as kwargs; never a positional query. "
       "Literal smart-case content plus fuzzy filenames; use first when location is unknown. "
       "`is_regex: True` runs the query as a REGEX over CONTENT instead (names are not matched). "
       "Hits come back ANCHORED, so a hit is already a `patch` argument. "
       "`include`/`exclude` globs bound which files the content sweep reads (exclude wins). "
       "`query: \"\"` lists files. Capped is never silent: line 1 names the next call, and the "
       "result pages itself — `next(r)` / `r.pages()` / `r.all()`, or pass `offset` by hand. "
       "A near-miss key folds onto the one it means — `glob`/`globs`→`include`, `context_lines`→"
       "`context`, `max_results`→`limit`, `path`→`paths` — so no search dies over a word.")
     :params [{:name "query"} {:name "paths" :note "or `path`"} {:name "include" :note "or `glob`"}
              {:name "exclude"} {:name "is_regex"} {:name "context" :note "or `context_lines`"}
              {:name "limit" :note "or `max_results`"} {:name "offset"} {:name "is_hidden"}]
     :call {:pos ["options"] :rest :always}
     :before-fn (fs-access-before-fn :grep :dir "file-read" find-arg-paths)
     :tag :observation
     :on-error-fn (tool-failure-on-error :grep :dir)}))

(defn available-editing-symbols [] [cat-symbol patch-symbol grep-symbol])

(def editing-symbols
  "Default editing symbol set for docs/tests. A `delay` so the language/env
   scan it triggers runs on first deref (tests, docs) and NEVER at namespace
   load — a load-time call reaches `git/run-git`'s `future`, which starts the
   agent thread-pool and native-image refuses that started thread in the image
   heap. Deref with `@editing-symbols`."
  (delay (available-editing-symbols)))
