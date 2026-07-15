(ns com.blockether.vis.internal.sandbox-fs
  "A GraalPy `FileSystem` that gives the Python sandbox REAL filesystem access
   CONFINED to the session's filesystem roots.

   Security model — every path-accessing operation canonicalizes its arguments
   and refuses anything that does not resolve UNDER a current filesystem root:

     - `..` traversal is defeated by `normalize`.
     - symlink escapes are defeated by resolving the path through the REAL path
       of its nearest existing ancestor (so a symlink inside a root that points
       outside is rejected, and a symlink whose target is inside is allowed).
     - the root set is read LIVE via `roots-fn` on every check, so
       `/fs add|remove` takes effect immediately.

   GraalPy's own stdlib / internal resources live OUTSIDE the roots, so the
   confined FS is wrapped with `allowLanguageHomeAccess` +
   `allowInternalResourceAccess` (read-only access to the language home and
   bundled resources) before it reaches the Context.

   OUTBOX tap — an optional engine-managed capture directory (`$VIS_OUTBOX`,
   distinct from the user `/fs` roots): the sandbox may WRITE there and every
   file it closes is handed to `on-close` so the engine can persist it as a
   `session_iteration_attachment` (the implicit twin of `vis_attach`). Reads,
   and writes anywhere else, are untouched.

   Empty/zero roots ⇒ DENY everything (fail closed)."
  (:require [clojure.string :as str])
  (:import [org.graalvm.polyglot.io FileSystem]
           [java.nio.channels SeekableByteChannel]
           [java.nio.file Path Paths Files LinkOption StandardOpenOption]))

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-link-opts (make-array LinkOption 0))
(def ^:private ^"[Ljava.nio.file.LinkOption;" nofollow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private temp-roots
  "System temp directories the sandbox may ALWAYS read/write, independent of the
   user's `/fs` roots — `/tmp` (and the JVM `java.io.tmpdir`, e.g. `$TMPDIR`).
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
  "Dirs under `~/.vis` the sandbox may ALWAYS read/write, independent of the
   user's `/fs` roots: the Python-extension dir `~/.vis/extensions` (author or
   debug an extension in any project) and the log dir `~/.vis/logs` (grep vis's
   own diagnostics — `vis.log`, `telegram.log`, JFR dumps). Each canonicalized
   ONCE via `real-path` (resolves the real path of `~/.vis`, then re-appends the
   leaf, so it matches even before the dir exists). Held in a delay so the
   syscall happens on first use. Kept SEPARATE from `temp-roots`: a write here is
   NOT tapped to the OUTBOX (only temp writes are), and the secret-bearing rest
   of `~/.vis` (config.edn, the session DB, gateway tokens) stays OUT of reach."
  (delay (let [home (System/getProperty "user.home")]
           (->> [["extensions"] ["logs"]]
                (keep (fn [tail]
                        (when-not (str/blank? (str home))
                          (try (real-path (Paths/get (str home)
                                                     (into-array String (cons ".vis" tail))))
                               (catch Throwable _ nil)))))
                distinct
                vec))))

(defn- current-real-roots
  "Canonical (real) Paths of the CURRENT filesystem roots. Reads the root STRINGS
   fresh each call (so `/fs add|remove` applies live), but MEMOIZES the expensive
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
  "Throw a clear SecurityException unless `p` resolves under a current root OR one
   of the always-allowed `extra-roots` (the engine outbox dir, the system temp
   dirs `/tmp`/`$TMPDIR`, and the always-on vis dirs `~/.vis/extensions` + `~/.vis/logs`). Returns `p` (a Path) on
   success. `cache` memoizes root canonicalization."
  ^Path [roots-fn cache extra-roots p]
  (let [^Path pp
        (if (instance? Path p) p (Paths/get (str p) (make-array String 0)))

        real
        (real-path pp)

        roots
        (into (vec extra-roots) (current-real-roots roots-fn cache))]

    (when-not (some (fn [^Path root]
                      (.startsWith real root))
                    roots)
      (throw (SecurityException.
               (str "vis sandbox: '" pp
                    "' is outside the filesystem roots — read/write "
                    "files via the file tools (cat/rg/patch), or add the dir with `/fs add`."))))
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
   there even though it is not a user `/fs` root); a WRITE channel closed under it
   fires `on-close` with the file path. The SAME `on-close` also fires for a
   write closed under any system temp root (`/tmp`, `$TMPDIR`), so plain /tmp
   scratch streams to the DB too, not just `$VIS_OUTBOX`. Nil ⇒ no tap."
  (^FileSystem [roots-fn] (confined-filesystem roots-fn nil))
  (^FileSystem [roots-fn outbox]
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
         (fn [p]
           (confine! roots-fn root-cache extra-roots p))

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
           (toRealPath [p opts] (.toRealPath d (c p) opts))
           (checkAccess [p modes opts] (.checkAccess d (c p) modes opts))
           (readAttributes [p attrs opts] (.readAttributes d (c p) attrs opts))
           (newByteChannel [p opts attrs]
             (let [^Path cp
                   (c p)

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
           (newDirectoryStream [dir filt] (.newDirectoryStream d (c dir) filt))
           (createDirectory [dir attrs] (.createDirectory d (c dir) attrs))
           (delete [p] (.delete d (c p)))
           (copy [src dst opts] (.copy d (c src) (c dst) opts))
           (move [src dst opts] (.move d (c src) (c dst) opts))
           (createLink [link existing] (.createLink d (c link) (c existing)))
           (createSymbolicLink [link target attrs]
             (.createSymbolicLink d (c link) (c target) attrs))
           (readSymbolicLink [link] (.readSymbolicLink d (c link)))
           (setAttribute [p attr value opts] (.setAttribute d (c p) attr value opts))
           ;; default interface methods — proxy does NOT inherit them, so delegate
           ;; explicitly. Pure metadata delegates raw; file-touching ones confine.
           (getMimeType [p] (.getMimeType d ^Path p))
           (getEncoding [p] (.getEncoding d ^Path p))
           (getTempDirectory [] (.getTempDirectory d))
           (isSameFile [p1 p2 opts] (.isSameFile d (c p1) (c p2) opts))
           (setCurrentWorkingDirectory [p] (.setCurrentWorkingDirectory d (c p)))
           (getFileStoreBlockSize [p] (.getFileStoreBlockSize d (c p)))
           (getFileStoreTotalSpace [p] (.getFileStoreTotalSpace d (c p)))
           (getFileStoreUnallocatedSpace [p] (.getFileStoreUnallocatedSpace d (c p)))
           (getFileStoreUsableSpace [p] (.getFileStoreUsableSpace d (c p)))
           (isFileStoreReadOnly [p] (.isFileStoreReadOnly d (c p))))]

     ;; Layer GraalPy's language-home + internal-resource read access ON TOP so
     ;; importing the stdlib still works while user paths stay confined.
     (-> ^FileSystem confined
         (FileSystem/allowInternalResourceAccess)
         (FileSystem/allowLanguageHomeAccess)))))
