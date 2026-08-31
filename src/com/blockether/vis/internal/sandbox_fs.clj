(ns com.blockether.vis.internal.sandbox-fs
  "A GraalPy `FileSystem` that gives the Python sandbox REAL filesystem access
   CONFINED to the session's filesystem roots.

   Security model — every path-accessing operation canonicalizes its arguments
   and refuses anything that does not resolve UNDER a current filesystem root:

     - `..` traversal is defeated by `normalize`.
     - symlink escapes are defeated by resolving the path through the REAL path
       of its nearest existing ancestor (so a symlink inside a root that points
       outside is rejected, and a symlink whose target is inside is allowed).
     - the root set is supplied by the current environment, rebuilt after `/reload`.

   GraalPy's own stdlib / internal resources live OUTSIDE the roots, so the
   confined FS is wrapped with `allowLanguageHomeAccess` +
   `allowInternalResourceAccess` (read-only access to the language home and
   bundled resources) before it reaches the Context.

   OUTBOX tap (DORMANT — the engine passes no `outbox` today, see
   `mpl-capture/incidental-capture-enabled?`) — an optional engine-managed capture
   directory (`$VIS_OUTBOX`, distinct from configured filesystem roots): the sandbox
   may WRITE there and every file it closes is handed to `on-close` so the engine
   can persist it as a `session_iteration_attachment` (the implicit twin of
   `attach`). Reads, and writes anywhere else, are untouched. The machinery is kept
   and tested for a future capture feature; today only `attach` records artifacts.

   Empty/zero roots ⇒ DENY everything (fail closed)."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.diff :as diff]
            [com.blockether.vis.internal.foundation.editing.parse :as parse])
  (:import [org.graalvm.polyglot.io FileSystem]
           [java.io IOException]
           [java.nio.channels SeekableByteChannel]
           [java.nio.file Path Paths Files LinkOption StandardOpenOption StandardCopyOption]
           [java.nio.charset StandardCharsets]))

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-link-opts (make-array LinkOption 0))

(def ^:private ^"[Ljava.nio.file.LinkOption;" nofollow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private temp-roots
  "System temp directories the sandbox may ALWAYS read/write, independent of
   configured filesystem roots — `/tmp` (and the JVM `java.io.tmpdir`, e.g. `$TMPDIR`).
   Canonicalized ONCE via `toRealPath` (symlinks resolved, so macOS `/tmp` ->
   `/private/tmp` matches). Held in a delay so the syscall happens on first use,
   not at class-load. Non-existent/unresolvable entries are dropped."
  (delay (->> [(System/getProperty "java.io.tmpdir") "/tmp"]
              (keep (fn [s]
                      (when-not (str/blank? (str s))
                        (try (.toRealPath (Paths/get (str s) (make-array String 0)) no-link-opts)
                             (catch Throwable _ nil)))))
              distinct
              vec)))

(defn- real-path
  "Canonicalize `p` to an absolute, symlink-resolved, `..`-free Path WITHOUT
   requiring `p` itself to exist: resolve the REAL path of its nearest existing
   ancestor (following symlinks there), then re-append the non-existent tail and
   normalize. This is the path used for the under-root check."
  ^Path [^Path p]
  (let [abs (.normalize (.toAbsolutePath p))]
    (loop [^Path anc abs
           tail ()]

      (cond
        ;; nearest existing ancestor (NOFOLLOW so a dangling symlink still counts
        ;; as 'exists' and gets resolved by toRealPath below)
        (Files/exists anc nofollow) (let [real-anc (try (.toRealPath anc no-link-opts)
                                                        (catch Throwable _ anc))]
                                      (.normalize ^Path
                                                  (reduce (fn [^Path acc ^String seg]
                                                            (.resolve acc seg))
                                                          real-anc
                                                          tail)))
        (nil? (.getParent anc)) abs
        :else (recur (.getParent anc) (cons (str (.getFileName anc)) tail))))))

(def ^:private vis-always-roots
  "The `~/.vis` directory tree that the sandbox may ALWAYS read/write,
   independent of configured filesystem roots. Canonicalized ONCE via `real-path`
   (which resolves the real path of `~/.vis`, even before a child exists). Held
   in a delay so the syscall happens on first use. Kept SEPARATE from
   `temp-roots`: a write here was never tapped to the OUTBOX (only temp writes
   were, back when the tap was armed)."
  (delay (let [home (System/getProperty "user.home")]
           (if (str/blank? (str home))
             []
             (try [(real-path (Paths/get (str home) (into-array String [".vis"])))]
                  (catch Throwable _ []))))))

(defn- current-real-roots
  "Canonical (real) Paths of the CURRENT filesystem roots. Reads the root STRINGS
   fresh each call, but MEMOIZES the expensive
   `toRealPath` syscall per root string in `cache` (a string→Path atom). A root
   dir's canonical path is stable, so this turns what was a stat-per-root on EVERY
   file op — an os.walk/glob over a big tree was a syscall storm — into one stat
   per distinct root for the life of the context. Only SUCCESSFUL resolutions are
   cached; a missing/unreadable root is dropped and retried next call (cheap, rare).
   Trade-off: a root whose real path changes mid-session (dir replaced / symlink
   retargeted) keeps the cached path — acceptable for the perf win."
  [roots-fn cache]
  (->> (roots-fn)
       (keep (fn [r]
               (let [s (str r)]
                 (when-not (str/blank? s)
                   (or (get @cache s)
                       (when-let [rp (try (.toRealPath (Paths/get s (make-array String 0))
                                                       no-link-opts)
                                          (catch Throwable _ nil))]
                         (swap! cache assoc s rp)
                         rp))))))
       vec))

(defn- confine!
  "Throw a clear IOException unless `p` resolves under a current root OR one
   of the always-allowed `extra-roots` (the engine outbox dir, the system temp
   dirs `/tmp`/`$TMPDIR`, and Vis's own `~/.vis` tree) AND the `:fs/access` gate
   allows it. Returns `p` (a Path) on success. `cache` memoizes root
   canonicalization.

   `gate-fn` is `(fn [operation abs-path] -> refusal | nil)` — the `:fs/access`
   gate an extension's hook answers, built by `extension/fs-access-gate`; nil
   leaves only root confinement. A THROW from it denies the operation (fail
   closed) rather than opening the path.

   A path under `extra-roots` is NOT gated. Those are engine surfaces LENT to the
   guest — the attachment outbox, temp scratch, `~/.vis` — not the user's tree,
   and a guard able to refuse them is a guard able to brick a live session."
  ^Path [roots-fn cache extra-roots gate-fn operation p]
  (let [^Path pp
        (if (instance? Path p) p (Paths/get (str p) (make-array String 0)))

        real
        (real-path pp)

        extra
        (vec extra-roots)

        roots
        (into extra (current-real-roots roots-fn cache))

        engine-owned?
        (some (fn [^Path root]
                (.startsWith real root))
              extra)]

    (when-not (some (fn [^Path root]
                      (.startsWith real root))
                    roots)
      ;; This is a policy decision made by Vis, not an ambiguous OS EACCES/EPERM.
      ;; IOException is intentional: GraalPy preserves its message in the guest
      ;; error, whereas it replaces SecurityException with generic PermissionError.
      ;; Keep paths and roots out of the guest-visible message: they may be secret.
      (throw (IOException. (str "[vis:sandbox_denied] operation="
                                operation
                                " reason=outside_approved_filesystem_roots"))))
    (when (and gate-fn (not engine-owned?))
      (when-let [refusal (try (gate-fn operation (str real))
                              (catch Throwable _
                                {:reason
                                 "the :fs/access gate itself failed; file IO fails closed."}))]
        ;; The gate's own sentence IS the remedy — it names what to do instead, so
        ;; the guest error is actionable without naming the path.
        (throw (IOException. (str "[vis:sandbox_denied] operation=" operation
                                  " reason=path_protected" (when-let [h (:reason refusal)]
                                                             (str " hint=" h)))))))
    pp))

(defn- write-opts?
  "True when the open options request a WRITE/APPEND (⇒ the channel is write-CAPABLE,
   so the outbox tap is wrapped around it; it only FIRES if bytes are written)."
  [opts]
  (boolean (and opts
                (or (.contains ^java.util.Set opts StandardOpenOption/WRITE)
                    (.contains ^java.util.Set opts StandardOpenOption/APPEND)))))

(defn- tap-write-channel
  "Wrap a write-capable `SeekableByteChannel` so that, once the sandbox CLOSES it,
   `on-close` is invoked with `path` (the just-written file, now flushed) — but ONLY
   when the sandbox actually PRODUCED bytes through it. A write-capable mode is not
   a write: `open(p, \"r+\")`, `sqlite3.connect`, and any library that opens a file
   read-write to read it hand us WRITE in the open options while touching nothing,
   and capturing those re-attached a file the sandbox merely READ. So the tap arms
   itself on the first `write`/`truncate` and stays disarmed otherwise. Every other
   channel method delegates straight to `inner`; `on-close` is best-effort — a
   failure there never propagates to the sandbox."
  ^SeekableByteChannel [^SeekableByteChannel inner ^Path path on-close]
  (let [wrote? (java.util.concurrent.atomic.AtomicBoolean. false)]
    (proxy [SeekableByteChannel] []
      (read [dst] (.read inner dst))
      (write [src] (.set wrote? true) (.write inner src))
      (position ([] (.position inner)) ([n] (.position inner (long n)) this))
      (truncate [n] (.set wrote? true) (.truncate inner (long n)) this)
      (size [] (.size inner))
      (isOpen [] (.isOpen inner))
      (close []
        (.close inner)
        (try (when (and on-close (.get wrote?)) (on-close path)) (catch Throwable _ nil))))))

(defn- atomic-replace!
  "Commit a staged file beside its target without exposing a partial candidate."
  [^Path stage ^Path target]
  (Files/move stage
              target
              (into-array java.nio.file.CopyOption
                          [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING])))

(def ^:private ^:const max-capture-bytes
  "Per-file ceiling on the text a change is diffed from. Past it the row still names
   the file and still counts, it just carries no hunks."
  (* 256 1024))

(defn- capture-text
  "One side of a change, as text, for the moment it can still be had.

   Answers `[known? text]`, and the difference matters: `[true nil]` is a file that
   is simply NOT THERE (so the other side is a creation or a deletion), while
   `[false nil]` is a file that IS there but is too large or is not text. Only two
   KNOWN sides may be compared — otherwise a binary rewrite would read as the whole
   file being deleted."
  [^Path p]
  (try (if (Files/exists p no-link-opts)
         (if (and (Files/isRegularFile p no-link-opts) (<= (Files/size p) (long max-capture-bytes)))
           (let [text (Files/readString p StandardCharsets/UTF_8)]
             (if (str/includes? text "\u0000") [false nil] [true text]))
           [false nil])
         [true nil])
       (catch Throwable _ [false nil])))

(defn- content-change
  "What a change did to ONE file's CONTENTS, in the vocabulary `patch` already
   reports: `:diff` for rendered hunks and `:lines` for exact counts. Nil when there
   is nothing to say — nothing changed, or a side could not be captured — and the row
   then names the file and stops.

   Only a write and a delete reach here. A move, a copy or a link relocates bytes that
   already existed, so no file's contents changed and a line delta there would be an
   invention."
  [[before-known? before] [after-known? after]]
  (when (and before-known? after-known?)
    (let [counts
          (diff/line-change-counts before after)

          text
          (diff/unified-diff-text before after)]

      (when counts
        (cond-> {:lines counts}
          text
          (assoc :diff text))))))
(def ^:private max-reported-syntax-errors
  "Parse locations a refusal names before the rest become a bare count. The
   first error is usually the cause and the rest its wreckage, so five say what
   twenty would.

   Must equal `editing.parse/detail-budget`, which decides how many rows arrive
   carrying the text and delimiter this renders."
  5)

(defn- syntax-diagnostic
  "Keep one bounded, model-facing tree-sitter location in source coordinates."
  [error]
  (let [raw-text
        (some-> (:text error)
                str
                str/trim
                not-empty)

        text
        (when raw-text (subs raw-text 0 (min 80 (count raw-text))))]

    (cond-> {:line (:line error)
             :column (:col error)
             :end-line (:end-line error)
             :end-column (:end-col error)
             :node-type (:kind error)
             :missing? (boolean (:missing? error))}
      text
      (assoc :text text))))

(defn- syntax-rejection
  "Describe every bounded parse location and the two valid ways to retry a refused write."
  [^Path requested lang errors]
  (let [total
        (count errors)

        diagnostics
        (->> errors
             (sort-by (juxt :line :col :end-line :end-col))
             (take max-reported-syntax-errors)
             (mapv syntax-diagnostic))

        location-lines
        (map (fn [{:keys [line column node-type missing? text]}]
               (str "  - line "
                    line
                    ", column "
                    column
                    ": "
                    (if missing? "MISSING " "")
                    node-type
                    (when text (str " near " (pr-str text)))))
             diagnostics)

        omitted
        (- total (count diagnostics))

        message
        (str "Python write was refused: "
             (pr-str (str requested))
             " would introduce invalid "
             lang
             " syntax.
Detected syntax locations:
"
             (str/join "
" location-lines)
             (when (pos? omitted) (str "
  - … " omitted " more parser locations"))
             "
No candidate bytes were committed; the previous file state was left unchanged. "
             "Fix the syntax and retry the write, or use patch(...) for a guarded edit.")]

    {:path (str requested)
     :language lang
     :reason "introduced_parse_error"
     :diagnostic-count total
     :diagnostics diagnostics
     :message message}))

(defn- guarded-write-channel
  "Stage a write-capable channel for a guarded code file. The original target remains
   untouched until close, when the shared tree-sitter transition verdict either permits
   one atomic replacement or raises IOException. Raw writes are never repaired."
  ^SeekableByteChannel [^FileSystem delegate ^Path requested opts lang on-close on-rejection]
  (let [exists?
        (Files/exists requested no-link-opts)

        create?
        (or (.contains ^java.util.Set opts StandardOpenOption/CREATE)
            (.contains ^java.util.Set opts StandardOpenOption/CREATE_NEW))

        create-new?
        (.contains ^java.util.Set opts StandardOpenOption/CREATE_NEW)]

    (when (and exists? create-new?)
      (throw (java.nio.file.FileAlreadyExistsException. (str requested))))
    (when (and (not exists?) (not create?))
      (throw (java.nio.file.NoSuchFileException. (str requested))))
    (let [^Path target
          (if exists? (.toRealPath requested no-link-opts) (real-path requested))

          ^Path stage
          (Files/createTempFile (.getParent target)
                                ".vis-write-"
                                ".tmp"
                                (make-array java.nio.file.attribute.FileAttribute 0))

          original
          (if exists? (Files/readString target StandardCharsets/UTF_8) "")

          changed?
          (java.util.concurrent.atomic.AtomicBoolean.
            (or (not exists?) (.contains ^java.util.Set opts StandardOpenOption/TRUNCATE_EXISTING)))

          closed?
          (java.util.concurrent.atomic.AtomicBoolean. false)]

      (try (when exists?
             (Files/copy target
                         stage
                         ^"[Ljava.nio.file.CopyOption;"
                         (into-array java.nio.file.CopyOption
                                     [StandardCopyOption/REPLACE_EXISTING
                                      StandardCopyOption/COPY_ATTRIBUTES])))
           (let [stage-opts
                 (-> (set opts)
                     (disj StandardOpenOption/CREATE
                           StandardOpenOption/CREATE_NEW
                           StandardOpenOption/DELETE_ON_CLOSE)
                     (conj StandardOpenOption/WRITE))

                 ^SeekableByteChannel inner
                 (.newByteChannel delegate
                                  stage
                                  stage-opts
                                  (make-array java.nio.file.attribute.FileAttribute 0))]

             (proxy [SeekableByteChannel] []
               (read [dst] (.read inner dst))
               (write [src] (.set changed? true) (.write inner src))
               (position ([] (.position inner)) ([n] (.position inner (long n)) this))
               (truncate [n] (.set changed? true) (.truncate inner (long n)) this)
               (size [] (.size inner))
               (isOpen [] (and (not (.get closed?)) (.isOpen inner)))
               (close []
                 (when (.compareAndSet closed? false true)
                   (try (.close inner)
                        (when (.get changed?)
                          (let [candidate
                                (Files/readString stage StandardCharsets/UTF_8)

                                {:keys [status after]}
                                (parse/transition-verdict lang original candidate)]

                            (when (= :introduced-error status)
                              (let [rejection (syntax-rejection requested lang after)]
                                (try (when on-rejection (on-rejection rejection))
                                     (catch Throwable _ nil))
                                (throw (IOException. (str "[vis:syntax_guard] "
                                                          (:message rejection))))))
                            (atomic-replace! stage target)
                            (try (when on-close (on-close requested)) (catch Throwable _ nil))))
                        (finally (Files/deleteIfExists stage)))))))
           (catch Throwable t (Files/deleteIfExists stage) (throw t))))))

(defn- guard-transfer!
  "Apply the guarded-write verdict to a move/copy DESTINATION. `newByteChannel` stages
   and re-parses every write that lands in a code file, but a real `move`/`copy` never
   opens one — without this the guard would be bypassed by `os.replace(tmp, \"core.clj\")`.
   A source that is not readable as text (a directory, a binary) carries no verdict and
   passes untouched."
  [^Path src ^Path dst on-rejection]
  (when-let [lang (parse/guarded-language (str dst))]
    (when-let [candidate (try (Files/readString src StandardCharsets/UTF_8)
                              (catch Throwable _ nil))]
      (let [original (if (Files/exists dst no-link-opts)
                       (try (Files/readString dst StandardCharsets/UTF_8) (catch Throwable _ ""))
                       "")
            {:keys [status after]} (parse/transition-verdict lang original candidate)]

        (when (= :introduced-error status)
          (let [rejection (syntax-rejection dst lang after)]
            (try (when on-rejection (on-rejection rejection)) (catch Throwable _ nil))
            (throw (IOException. (str "[vis:syntax_guard] " (:message rejection))))))))))
(defn confined-filesystem
  "A GraalPy `FileSystem` confined to the filesystem roots returned by `roots-fn`
   (a 0-arg fn → seq of root path strings). Delegates real I/O to the default FS
   after confining every path argument. Wrapped so GraalPy's own stdlib / bundled
   resources stay readable. Uses `proxy` (runtime dispatch) so the interface's
   overloaded `parsePath` + varargs + void methods bind cleanly.

   TWO proxy layers, and the outer one is not optional. `allowInternalResourceAccess` +
   `allowLanguageHomeAccess` answer a `CompositeFileSystem` that implements NEITHER
   `move` NOR `copy`, so both fall through to the interface DEFAULTS — and the default
   `move` REFUSES `ATOMIC_MOVE` outright, which is why `os.replace`, `os.rename` and
   `Path.rename` all died with `[Errno 5] Atomic move not supported`, while the default
   `copy` quietly degrades a directory rename into copy-then-delete. So the composite is
   wrapped once more: every method delegates to it EXCEPT `move`/`copy`, which confine
   both paths and hand the real operation to the default FS, where a rename is a rename.

   `root-cache` lives for the FS's lifetime and memoizes the per-root `toRealPath`
   so confinement doesn't re-stat every root on every path operation.

   Options:

   `:outbox` (optional, DORMANT — the engine passes nil, see
   `mpl-capture/incidental-capture-enabled?`) —
   `{:dir <existing dir path string> :on-close (fn [^Path])}`.
   Its real path is treated as an always-allowed root (so the sandbox can write
   there even though it is outside configured filesystem roots); a WRITE channel closed under it
   fires `on-close` with the file path. The SAME `on-close` also fires for a
   write closed under any system temp root (`/tmp`, `$TMPDIR`), so plain /tmp
   scratch streams to the DB too, not just `$VIS_OUTBOX`. Nil ⇒ no tap.

   `:gate-fn` (optional) — the per-path prefix-rule verdict consulted by `confine!`.

   `:on-rejection` (optional) receives a structured map whenever a guarded write is
   rejected. The agent context uses it to surface close-time failures that GraalPy
   suppresses as a model-facing python_execution error.

   `:on-mutation` (optional) receives `{:kind :write|:move|:copy|:delete|:mkdir|:link
   :path <string> :to <string>?}` AFTER the sandbox changed the tree, so what a block did
   to disk can become Activity rows. A write and a delete also carry `:diff` (rendered
   hunks) and `:lines` (`added`/`removed`/`modified`), the SAME pair `patch` reports —
   see `content-change`. Scratch is not the tree: anything under the outbox or
   a system temp root is excluded, and so are metadata-only edits (`setAttribute`, i.e.
   chmod/utime) — this reports what moved bytes. Best-effort, and its own failure never
   reaches the sandbox."
  (^FileSystem [roots-fn] (confined-filesystem roots-fn nil))
  (^FileSystem [roots-fn {:keys [outbox gate-fn on-rejection on-mutation]}]
   (let [^FileSystem d
         (FileSystem/newDefaultFileSystem)

         root-cache
         (atom {})

         ^Path outbox-real
         (when-let [dir (:dir outbox)]
           (try (.toRealPath (Paths/get (str dir) (make-array String 0)) no-link-opts)
                (catch Throwable _ (real-path (Paths/get (str dir) (make-array String 0))))))

         on-close
         (:on-close outbox)

         extra-roots
         (into (into (if outbox-real [outbox-real] []) @temp-roots) @vis-always-roots)

         c
         (fn [operation p]
           (confine! roots-fn root-cache extra-roots gate-fn operation p))

         ;; The outbox and the system temp dirs are the sandbox's scratch space, not the
         ;; user's tree: a NamedTemporaryFile is not a file the session changed.
         scratch?
         (fn [^Path p]
           (try (let [^Path rp (real-path p)]
                  (boolean (or (and outbox-real (.startsWith rp outbox-real))
                               (some (fn [^Path tr]
                                       (.startsWith rp tr))
                                     @temp-roots))))
                (catch Throwable _ false)))

         mutated!
         (fn mutated! ([kind ^Path p] (mutated! kind p nil nil)) ([kind ^Path p ^Path to] (mutated!
                                                                                            kind p
                                                                                            to nil))
           ([kind ^Path p ^Path to detail-fn]
            ;; `detail-fn` is a THUNK so the cost of reading a file back only lands on
            ;; a change that is actually reported — never on scratch, never with no
            ;; listener attached.
            (when (and on-mutation (not (scratch? (or to p)))) (try (on-mutation (cond-> {:kind kind
                                                                                          :path
                                                                                          (str p)}
                                                                                   to
                                                                                   (assoc :to
                                                                                     (str to))

                                                                                   detail-fn
                                                                                   (merge
                                                                                     (detail-fn))))
                                                                 (catch Throwable _ nil)))))

         confined
         (proxy [FileSystem] []
           ;; path math — no file access, no confinement
           (parsePath [arg]
             (if (instance? java.net.URI arg)
               (.parsePath d ^java.net.URI arg)
               (.parsePath d ^String arg)))
           (toAbsolutePath [p] (.toAbsolutePath d ^Path p))
           (getSeparator [] (.getSeparator d))
           (getPathSeparator [] (.getPathSeparator d))
           ;; confine the path, then delegate the real op
           (toRealPath [p opts] (.toRealPath d (c "file-read" p) opts))
           (checkAccess [p modes opts] (.checkAccess d (c "file-read" p) modes opts))
           (readAttributes [p attrs opts] (.readAttributes d (c "file-read" p) attrs opts))
           (newByteChannel [p opts attrs]
             (let [write?
                   (boolean (write-opts? opts))

                   ^Path cp
                   (c (if write? "file-write" "file-read") p)

                   ;; The BEFORE side, taken while it still exists: a plain
                   ;; `open(path, "w")` is an edit with two texts exactly as an anchored
                   ;; patch is, and this is the only moment the old one can be had.
                   before
                   (when (and write? on-mutation) (capture-text cp))

                   close-fn
                   (when write?
                     (let [tap? (boolean (and on-close (scratch? cp)))]
                       (when (or tap? on-mutation)
                         (fn [^Path written]
                           (when tap? (on-close written))
                           (mutated! :write
                                     written
                                     nil
                                     #(content-change before (capture-text written)))))))

                   lang
                   (when write? (parse/guarded-language (str cp)))]

               (if lang
                 (guarded-write-channel d cp opts lang close-fn on-rejection)
                 (let [ch (.newByteChannel d cp opts attrs)]
                   (if close-fn (tap-write-channel ch cp close-fn) ch)))))
           (newDirectoryStream [dir filt] (.newDirectoryStream d (c "file-read" dir) filt))
           (createDirectory [dir attrs]
             (let [^Path cd (c "file-write" dir)]
               (.createDirectory d cd attrs)
               (mutated! :mkdir cd)))
           (delete [p]
             (let [^Path cp
                   (c "file-write" p)

                   before
                   (when on-mutation (capture-text cp))]

               (.delete d cp)
               (mutated! :delete cp nil #(content-change before [true nil]))))
           ;; move/copy live in the OUTER proxy — the composite below never routes them here.
           ;;
           ;; GraalPy 25.1.3 never delivers `os.link`'s DESTINATION to a filesystem: its
           ;; emulated `linkat` converts the new path and then resolves the OLD one a second
           ;; time (`EmulatedPosixSupport.linkat`), so the guest call arrives here as
           ;; `(src, src)` and every `os.link` died with EEXIST naming the source. Nothing can
           ;; be repaired at this layer — the destination is already lost — so the guest call
           ;; is re-routed around the broken backend by `vis-python/hard_link.py`, which hands
           ;; BOTH ends back to this method. A hard link is a new name for existing bytes, so
           ;; it is confined, syntax-gated and reported exactly like a write.
           (createLink [link existing]
             (let [^Path cl
                   (c "file-write" link)

                   ^Path ce
                   (c "file-read" existing)]

               (guard-transfer! ce cl on-rejection)
               (.createLink d cl ce)
               (mutated! :link ce cl)))
           (createSymbolicLink [link target attrs]
             (let [^Path cl
                   (c "file-write" link)

                   ^Path ct
                   (c "file-read" target)]

               (.createSymbolicLink d cl ct attrs)
               (mutated! :link ct cl)))
           (readSymbolicLink [link] (.readSymbolicLink d (c "file-read" link)))
           (setAttribute [p attr value opts] (.setAttribute d (c "file-write" p) attr value opts))
           ;; default interface methods — proxy does NOT inherit them, so delegate
           ;; explicitly. Pure metadata delegates raw; file-touching ones confine.
           (getMimeType [p] (.getMimeType d ^Path p))
           (getEncoding [p] (.getEncoding d ^Path p))
           (getTempDirectory [] (.getTempDirectory d))
           (isSameFile [p1 p2 opts] (.isSameFile d (c "file-read" p1) (c "file-read" p2) opts))
           (setCurrentWorkingDirectory [p] (.setCurrentWorkingDirectory d (c "file-read" p)))
           (getFileStoreBlockSize [p] (.getFileStoreBlockSize d (c "file-read" p)))
           (getFileStoreTotalSpace [p] (.getFileStoreTotalSpace d (c "file-read" p)))
           (getFileStoreUnallocatedSpace [p] (.getFileStoreUnallocatedSpace d (c "file-read" p)))
           (getFileStoreUsableSpace [p] (.getFileStoreUsableSpace d (c "file-read" p)))
           (isFileStoreReadOnly [p] (.isFileStoreReadOnly d (c "file-read" p))))

         ;; Layer GraalPy's language-home + internal-resource read access ON TOP so
         ;; importing the stdlib still works while user paths stay confined.
         ^FileSystem composite
         (-> ^FileSystem confined
             (FileSystem/allowInternalResourceAccess)
             (FileSystem/allowLanguageHomeAccess))]

     ;; …and re-take `move`/`copy`, which the composite does not implement and the
     ;; interface default would answer with "Atomic move not supported".
     (proxy [FileSystem] []
       (parsePath [arg]
         (if (instance? java.net.URI arg)
           (.parsePath composite ^java.net.URI arg)
           (.parsePath composite ^String arg)))
       (toAbsolutePath [p] (.toAbsolutePath composite ^Path p))
       (getSeparator [] (.getSeparator composite))
       (getPathSeparator [] (.getPathSeparator composite))
       (toRealPath [p opts] (.toRealPath composite ^Path p opts))
       (checkAccess [p modes opts] (.checkAccess composite ^Path p modes opts))
       (readAttributes [p attrs opts] (.readAttributes composite ^Path p ^String attrs opts))
       (newByteChannel [p opts attrs] (.newByteChannel composite ^Path p opts attrs))
       (newDirectoryStream [dir filt] (.newDirectoryStream composite ^Path dir filt))
       (createDirectory [dir attrs] (.createDirectory composite ^Path dir attrs))
       (delete [p] (.delete composite ^Path p))
       (copy [src dst opts]
         (let [^Path s
               (c "file-read" src)

               ^Path t
               (c "file-write" dst)]

           (guard-transfer! s t on-rejection)
           (.copy d s t opts)
           (mutated! :copy s t)))
       (move [src dst opts]
         (let [^Path s
               (c "file-write" src)

               ^Path t
               (c "file-write" dst)]

           (guard-transfer! s t on-rejection)
           (.move d s t opts)
           (mutated! :move s t)))
       (createLink [link existing] (.createLink composite ^Path link ^Path existing))
       (createSymbolicLink [link target attrs]
         (.createSymbolicLink composite ^Path link ^Path target attrs))
       (readSymbolicLink [link] (.readSymbolicLink composite ^Path link))
       (setAttribute [p attr value opts] (.setAttribute composite ^Path p ^String attr value opts))
       (getMimeType [p] (.getMimeType composite ^Path p))
       (getEncoding [p] (.getEncoding composite ^Path p))
       (getTempDirectory [] (.getTempDirectory composite))
       (isSameFile [p1 p2 opts] (.isSameFile composite ^Path p1 ^Path p2 opts))
       (setCurrentWorkingDirectory [p] (.setCurrentWorkingDirectory composite ^Path p))
       (getFileStoreBlockSize [p] (.getFileStoreBlockSize composite ^Path p))
       (getFileStoreTotalSpace [p] (.getFileStoreTotalSpace composite ^Path p))
       (getFileStoreUnallocatedSpace [p] (.getFileStoreUnallocatedSpace composite ^Path p))
       (getFileStoreUsableSpace [p] (.getFileStoreUsableSpace composite ^Path p))
       (isFileStoreReadOnly [p] (.isFileStoreReadOnly composite ^Path p))))))
