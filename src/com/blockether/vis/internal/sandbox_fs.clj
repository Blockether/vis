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

   Empty/zero roots ⇒ DENY everything (fail closed)."
  (:require [clojure.string :as str])
  (:import
   [org.graalvm.polyglot.io FileSystem]
   [java.nio.file Path Paths Files LinkOption]))

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-link-opts (make-array LinkOption 0))
(def ^:private ^"[Ljava.nio.file.LinkOption;" nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- real-path
  "Canonicalize `p` to an absolute, symlink-resolved, `..`-free Path WITHOUT
   requiring `p` itself to exist: resolve the REAL path of its nearest existing
   ancestor (following symlinks there), then re-append the non-existent tail and
   normalize. This is the path used for the under-root check."
  ^Path [^Path p]
  (let [abs (.normalize (.toAbsolutePath p))]
    (loop [^Path anc abs, tail ()]
      (cond
        ;; nearest existing ancestor (NOFOLLOW so a dangling symlink still counts
        ;; as 'exists' and gets resolved by toRealPath below)
        (Files/exists anc nofollow)
        (let [real-anc (try (.toRealPath anc no-link-opts) (catch Throwable _ anc))]
          (.normalize ^Path (reduce (fn [^Path acc ^String seg] (.resolve acc seg)) real-anc tail)))

        (nil? (.getParent anc)) abs
        :else (recur (.getParent anc) (cons (str (.getFileName anc)) tail))))))

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
                  (when-let [rp (try (.toRealPath (Paths/get s (make-array String 0)) no-link-opts)
                                  (catch Throwable _ nil))]
                    (swap! cache assoc s rp)
                    rp))))))
    vec))

(defn- confine!
  "Throw a clear SecurityException unless `p` resolves under a current root.
   Returns `p` (a Path) on success. `cache` memoizes root canonicalization."
  ^Path [roots-fn cache p]
  (let [^Path pp (if (instance? Path p) p (Paths/get (str p) (make-array String 0)))
        real     (real-path pp)
        roots    (current-real-roots roots-fn cache)]
    (when-not (some (fn [^Path root] (.startsWith real root)) roots)
      (throw (SecurityException.
               (str "vis sandbox: '" pp "' is outside the filesystem roots — read/write "
                 "files via the file tools (cat/rg/patch), or add the dir with `/fs add`."))))
    pp))

(defn confined-filesystem
  "A GraalPy `FileSystem` confined to the filesystem roots returned by `roots-fn`
   (a 0-arg fn → seq of root path strings). Delegates real I/O to the default FS
   after confining every path argument. Wrapped so GraalPy's own stdlib / bundled
   resources stay readable. Uses `proxy` (runtime dispatch) so the interface's
   overloaded `parsePath` + varargs + void methods bind cleanly.

   `root-cache` lives for the FS's lifetime and memoizes the per-root `toRealPath`
   so confinement doesn't re-stat every root on every path operation."
  ^FileSystem [roots-fn]
  (let [^FileSystem d (FileSystem/newDefaultFileSystem)
        root-cache (atom {})
        c (fn [p] (confine! roots-fn root-cache p))
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
          (newByteChannel [p opts attrs] (.newByteChannel d (c p) opts attrs))
          (newDirectoryStream [dir filt] (.newDirectoryStream d (c dir) filt))
          (createDirectory [dir attrs] (.createDirectory d (c dir) attrs))
          (delete [p] (.delete d (c p)))
          (copy [src dst opts] (.copy d (c src) (c dst) opts))
          (move [src dst opts] (.move d (c src) (c dst) opts))
          (createLink [link existing] (.createLink d (c link) (c existing)))
          (createSymbolicLink [link target attrs] (.createSymbolicLink d (c link) (c target) attrs))
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
      (FileSystem/allowLanguageHomeAccess))))
