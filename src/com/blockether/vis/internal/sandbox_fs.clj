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

   OUTBOX tap — an optional engine-managed capture directory (`$VIS_OUTBOX`,
   distinct from configured filesystem roots): the sandbox may WRITE there and every
   file it closes is handed to `on-close` so the engine can persist it as a
   `session_iteration_attachment` (the implicit twin of `vis_attach`). Reads,
   and writes anywhere else, are untouched.

   Empty/zero roots ⇒ DENY everything (fail closed)."
  (:require [clojure.string :as str])
  (:import [org.graalvm.polyglot.io FileSystem]
           [java.io IOException]
           [java.nio.channels SeekableByteChannel]
           [java.nio.file Path Paths Files LinkOption StandardOpenOption]))

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
    (loop
      [^Path anc abs
       tail ()]

      (cond
        ;; nearest existing ancestor (NOFOLLOW so a dangling symlink still counts
        ;; as 'exists' and gets resolved by toRealPath below)
        (Files/exists anc nofollow)
        (let [real-anc (try (.toRealPath anc no-link-opts) (catch Throwable _ anc))]
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
   `temp-roots`: a write here is NOT tapped to the OUTBOX (only temp writes are)."
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
                       (when-let
                         [rp (try (.toRealPath (Paths/get s (make-array String 0)) no-link-opts)
                                  (catch Throwable _ nil))]
                         (swap! cache assoc s rp)
                         rp))))))
       vec))

(defn- confine!
  "Throw a clear IOException unless `p` resolves under a current root OR one
   of the always-allowed `extra-roots` (the engine outbox dir, the system temp
   dirs `/tmp`/`$TMPDIR`, and Vis's own `~/.vis` tree) AND is not refused by
   `protected-fn` — the extension-declared `:ext/protected-paths` boundary, the
   very same registry the native file verbs consult. Returns `p` (a Path) on
   success. `cache` memoizes root canonicalization.

   `protected-fn` is `(fn [operation abs-path] -> rule | nil)`; nil disables the
   check. A THROW from it denies the operation (fail closed) rather than opening
   the path."
  ^Path [roots-fn cache extra-roots protected-fn operation p]
  (let
    [^Path pp
     (if (instance? Path p) p (Paths/get (str p) (make-array String 0)))

     real
     (real-path pp)

     roots
     (into (vec extra-roots) (current-real-roots roots-fn cache))]

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
    (when protected-fn
      (when-let
        [rule (try
                (protected-fn operation (str real))
                (catch Throwable _
                  {:hint
                   "Fix the extension's :ext/protected-paths callback before retrying file IO."}))]
        ;; The owning extension's own hint IS the remedy — it names the API to use
        ;; instead, so the guest error is actionable without naming the path.
        (throw (IOException. (str "[vis:sandbox_denied] operation=" operation
                                  " reason=path_protected" (when-let [h (:hint rule)]
                                                             (str " hint=" h)))))))
    pp))

(defn- write-opts?
  "True when the open options request a WRITE/APPEND (⇒ the sandbox is producing
   the file, so an outbox tap fires on close)."
  [opts]
  (boolean (and opts
                (or (.contains ^java.util.Set opts StandardOpenOption/WRITE)
                    (.contains ^java.util.Set opts StandardOpenOption/APPEND)))))

(defn- tap-write-channel
  "Wrap a write `SeekableByteChannel` so that, once the sandbox CLOSES it, `on-close`
   is invoked with `path` (the just-written file, now flushed). Every other channel
   method delegates straight to `inner`; `on-close` is best-effort — a failure there
   never propagates to the sandbox."
  ^SeekableByteChannel [^SeekableByteChannel inner ^Path path on-close]
  (proxy [SeekableByteChannel] []
    (read [dst] (.read inner dst))
    (write [src] (.write inner src))
    (position ([] (.position inner)) ([n] (.position inner (long n)) this))
    (truncate [n] (.truncate inner (long n)) this)
    (size [] (.size inner))
    (isOpen [] (.isOpen inner))
    (close [] (.close inner) (try (when on-close (on-close path)) (catch Throwable _ nil)))))

(defn confined-filesystem
  "A GraalPy `FileSystem` confined to the filesystem roots returned by `roots-fn`
   (a 0-arg fn → seq of root path strings). Delegates real I/O to the default FS
   after confining every path argument. Wrapped so GraalPy's own stdlib / bundled
   resources stay readable. Uses `proxy` (runtime dispatch) so the interface's
   overloaded `parsePath` + varargs + void methods bind cleanly.

   `root-cache` lives for the FS's lifetime and memoizes the per-root `toRealPath`
   so confinement doesn't re-stat every root on every path operation.

   `outbox` (optional) — `{:dir <existing dir path string> :on-close (fn [^Path])}`.
   Its real path is treated as an always-allowed root (so the sandbox can write
   there even though it is outside configured filesystem roots); a WRITE channel closed under it
   fires `on-close` with the file path. The SAME `on-close` also fires for a
   write closed under any system temp root (`/tmp`, `$TMPDIR`), so plain /tmp
   scratch streams to the DB too, not just `$VIS_OUTBOX`. Nil ⇒ no tap."
  (^FileSystem [roots-fn] (confined-filesystem roots-fn nil nil))
  (^FileSystem [roots-fn outbox] (confined-filesystem roots-fn outbox nil))
  (^FileSystem [roots-fn outbox protected-fn]
   (let
     [^FileSystem d
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
        (confine! roots-fn root-cache extra-roots protected-fn operation p))

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
          (let
            [^Path cp
             (c (if (write-opts? opts) "file-write" "file-read") p)

             ch
             (.newByteChannel d cp opts attrs)

             ;; Tap a WRITE opened under the OUTBOX *or* any system temp
             ;; root (/tmp, $TMPDIR): once the sandbox CLOSES the file it
             ;; streams to the DB as a `session_iteration_attachment` —
             ;; the $VIS_OUTBOX capture, widened to plain /tmp scratch so
             ;; anything the sandbox drops in /tmp is persisted too.
             tap?
             (and on-close
                  (write-opts? opts)
                  (let [^Path rp (real-path cp)]
                    (or (and outbox-real (.startsWith rp outbox-real))
                        (some (fn [^Path tr]
                                (.startsWith rp tr))
                              @temp-roots))))]

            (if tap? (tap-write-channel ch cp on-close) ch)))
        (newDirectoryStream [dir filt] (.newDirectoryStream d (c "file-read" dir) filt))
        (createDirectory [dir attrs] (.createDirectory d (c "file-write" dir) attrs))
        (delete [p] (.delete d (c "file-write" p)))
        (copy [src dst opts] (.copy d (c "file-read" src) (c "file-write" dst) opts))
        (move [src dst opts] (.move d (c "file-write" src) (c "file-write" dst) opts))
        (createLink [link existing] (.createLink d (c "file-write" link) (c "file-read" existing)))
        (createSymbolicLink [link target attrs]
          (.createSymbolicLink d (c "file-write" link) (c "file-read" target) attrs))
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
        (isFileStoreReadOnly [p] (.isFileStoreReadOnly d (c "file-read" p))))]

     ;; Layer GraalPy's language-home + internal-resource read access ON TOP so
     ;; importing the stdlib still works while user paths stay confined.
     (-> ^FileSystem confined
         (FileSystem/allowInternalResourceAccess)
         (FileSystem/allowLanguageHomeAccess)))))
