(ns com.blockether.vis.internal.python.runtime
  "Getting the embedded CPython onto THIS machine.

   The interpreter is a DIRECTORY — a cdylib plus the vendored standard library
   beside it, tens of megabytes — so it is neither a maven dependency nor a
   resource inside a jar. A native distribution stages it beside the binary and
   the wrapper points `VIS_PYTHON_NATIVE_PATH` at it; everywhere else (a source
   checkout, a gateway running from `clojure -M:vis`) it is fetched ONCE from the
   runtime's own GitHub release into `~/.vis/python/runtime/<version>/<platform>/`
   and named through `runtime/use-library!`, because a JVM cannot set its own
   environment.

   Resolution that already answers is never disturbed: an environment variable,
   a staged distribution and a built checkout all win over the network. The
   archive is unpacked into a sibling directory and MOVED into place, so a second
   process sees a complete installation or none — never a half-written standard
   library — and `tar` does the unpacking because the tree carries symlinks and
   execute bits that no jar or zip round-trips."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.contract.config :as contract-config]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [com.blockether.vispython Interpreter Locations]
           [java.io File RandomAccessFile]
           [java.lang ProcessBuilder$Redirect ProcessHandle]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileLockInterruptionException]
           [java.nio.file CopyOption FileVisitResult Files LinkOption OpenOption Path
            SimpleFileVisitor StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.security MessageDigest]
           [java.util.concurrent TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

(set! *warn-on-reflection* true)

(defn version-globals
  "Host-owned build metadata shared by Python namespaces and the model prompt.
   The build names the bundled SDK version, never an editable or pip package.
   Source builds report dev; an unavailable commit is nil, not a guessed release."
  []
  {"VIS_PYTHON_RUNTIME_VERSION" runtime/version
   "VIS_SHA_RELEASE" (gateway-runtime/release-sha)
   "VIS_VERSION" (gateway-runtime/release-version)
   "VIS_PYTHON_SDK_VERSION" (gateway-runtime/sdk-version)})

(defn version-globals-python
  "Python bootstrap statement for the host's build metadata, encoded as JSON."
  []
  (str "globals().update(__import__('json').loads("
       (json/write-json-str (json/write-json-str (version-globals)) :escape-slash false)
       "))"))

(def ^:private release-base
  "Where the platform archives live. Vis pins the runtime by immutable Git commit;
   its JVM jar and platform interpreters are published as GitHub release assets."
  "https://github.com/Blockether/vis-python-runtime/releases/download")

(defn archive-url
  "The release asset for one version and platform tag."
  [version platform]
  (str release-base "/v" version "/vis-python-runtime-" platform "-" version ".tar.gz"))

(defn- resolved-library
  "The library the runtime resolves on its own, or nil when it resolves none.
   A refusal here is the ordinary case on a machine that has not fetched one."
  []
  (try (:path (runtime/resolve-library)) (catch Throwable _ nil)))

(defn- delete-tree!
  [^File root]
  (when (.exists root)
    (doseq [^File f (reverse (file-seq root))]
      (.delete f))))

(defn- untar!
  "Unpack `archive` into `dir` with the system `tar` — the one tool on every
   platform we ship to that restores symlinks and execute bits."
  [^File archive ^File dir]
  (let [^java.util.List command
        ["tar" "xzf" (.getAbsolutePath archive) "-C" (.getAbsolutePath dir)]

        process
        (.start (doto (ProcessBuilder. command) (.redirectErrorStream true)))

        output
        (slurp (.getInputStream process))

        exit
        (.waitFor process)]

    (when-not (zero? exit)
      (throw (ex-info "Could not unpack the embedded CPython archive."
                      {:archive (.getAbsolutePath archive)
                       :exit exit
                       :output (util/truncate output 400)})))))

(defn- install-archive!
  "Unpack `archive` into `home`, atomically. Answers `home`."
  [^File archive ^File home]
  (let [staging (io/file (str (.getAbsolutePath home) ".tmp." (.pid (ProcessHandle/current))))]
    (try (delete-tree! staging)
         (.mkdirs staging)
         (untar! archive staging)
         (io/make-parents home)
         (try (Files/move (.toPath staging)
                          (.toPath home)
                          (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
              (catch java.io.IOException _
                ;; Another process finished first; its installation is as good as
                ;; ours, and the loser only has a directory to remove.
                nil))
         home
         (finally (delete-tree! staging)))))

(defn- download!
  "Stream `url` to `dest`. A status that is not 200 is a refusal naming it —
   there is no partial installation to fall back to."
  [url ^File dest]
  (let [{:keys [status body]} (http/get url {:as :stream :throw false :timeout 600000})]
    (when-not (= 200 (long status))
      (throw (ex-info (str "Could not download the embedded CPython: HTTP " status)
                      {:url url :status status})))
    (io/make-parents dest)
    (with-open [in ^java.io.InputStream body]
      (io/copy in dest))))

(defonce ^:private library-provisioning-lock (Object.))

(defn- store-version-dir
  "The `<vis-home>/python/<kind>/<version>` tree `path` belongs to, or nil when it
   sits outside this machine's store — a staged native distribution or a built
   checkout, neither of which anything here installs or reclaims."
  ^File [^String kind ^String path]
  (let [root (.getAbsolutePath (io/file (Locations/stateRoot) kind))]
    (paths/nearest-ancestor (fn [^File dir]
                              (= root
                                 (some-> (.getParentFile dir)
                                         .getAbsolutePath)))
                            (some-> path
                                    io/file))))

(defn- claim-store-trees!
  "Claim the interpreter and source trees this process boots from, so cleanup in
   another Vis process can tell an older install that still serves somebody from
   one nothing runs against any more. Best effort, and the claim is the ONLY
   liveness signal there is: an install's age says nothing, because a daemon
   that has served one for a month never touches its files. Answers `library`."
  [library]
  (doseq [[kind path] [["runtime" library] ["sources" (Locations/sourcesDir)]]]
    (when-let [dir (store-version-dir kind path)]
      (when (.isDirectory dir) (paths/claim-dir! dir))))
  library)

(defn ensure-library!
  "Make the interpreter for this platform resolvable, answering the library path.

   A no-op when the runtime already resolves one. Otherwise the cached
   installation is used, or the platform archive is fetched into it first. Concurrent
   cold callers share one installation and recheck resolution after acquiring the lock."
  []
  (claim-store-trees!
    (or (resolved-library)
        (locking library-provisioning-lock
          (or (resolved-library)
              (let [version
                    runtime/version

                    platform
                    (runtime/platform)

                    home
                    (io/file (Locations/runtimeDir version platform))

                    library
                    (io/file home (runtime/library-name platform))]

                (when-not (.isFile library)
                  (let [url
                        (archive-url version platform)

                        archive
                        (io/file
                          (str (.getAbsolutePath home) ".tar.gz." (.pid (ProcessHandle/current))))]

                    (tel/log! {:level :info :id ::fetching-runtime :url url :home (str home)})
                    (try (download! url archive)
                         (install-archive! archive home)
                         (finally (.delete archive)))))
                (when-not (.isFile library)
                  (throw (ex-info "The embedded CPython installation holds no runtime library."
                                  {:home (str home) :platform platform :version version})))
                (runtime/use-library! (str home))
                (.getAbsolutePath library)))))))

(defn- configured-index-url
  []
  (let [python (get (config/load-config-raw) "python")]
    (when (contains? python "index_url")
      (let [index (get python "index_url")]
        (when-not (contract-config/definition-valid? "python" {"index_url" index})
          (throw (ex-info
                   "python.index_url must be an HTTP(S) URL without credentials, query or fragment"
                   {:type ::invalid-index-url})))
        index))))

(defn- uv-index!
  "Supply Vis's index as uv's default without replacing explicit uv environment settings."
  [^ProcessBuilder builder]
  (when-let [index (configured-index-url)]
    (let [environment (.environment builder)]
      (when-not (or (.containsKey environment "UV_DEFAULT_INDEX")
                    (.containsKey environment "UV_INDEX_URL"))
        (.put environment "UV_DEFAULT_INDEX" index))))
  builder)

(defn- pycache-prefix
  "The bytecode cache of the embedded Python: `VIS_PYTHON_PYCACHE_PREFIX` or the Vis state."
  []
  (Locations/pycachePrefix))

(defn- bytecode-cache!
  "Send the bytecode of uv's Python children to the runtime cache, not the shipped tree.

   uv runs the interpreter for its query and for build backends. Without a prefix,
   CPython writes `__pycache__` beside the standard library it imports. An explicit
   `PYTHONPYCACHEPREFIX` stays."
  [^ProcessBuilder builder]
  (let [environment (.environment builder)]
    (when-not (.containsKey environment "PYTHONPYCACHEPREFIX")
      (.put environment "PYTHONPYCACHEPREFIX" (pycache-prefix))))
  builder)

(def ^:private diagnostic-limit 16384)

(defn- redact-installer-text
  [text]
  (-> (str text)
      (str/replace #"\u001b\[[0-?]*[ -/]*[@-~]" "")
      (str/replace #"[\p{Cntrl}&&[^\n\t]]" "")
      (util/redact-secret-text (keep (fn [[k v]]
                                       (when (util/secret-key? k) v))
                                     (System/getenv)))
      (str/replace #"(?i)\b[a-z][a-z0-9+.-]*://[^\s<>\"']+"
                   (fn [url]
                     (-> url
                         (str/replace #"(://)[^/?#]*@" "$1[REDACTED]@")
                         (str/replace #"[?#].*" "?[REDACTED]"))))))

(defn- capture-diagnostics!
  "Keep a bounded, redacted tail. Discard oversized lines whole, never expose a
   credential fragment created by truncation. Also flush a final partial line.
   After each read, put the redacted partial line in `pending`: a reader that a
   detached child blocks on Linux never sees the end of its input."
  ([reader tail] (capture-diagnostics! reader tail (atom "")))
  ([^java.io.Reader reader tail pending]
   (let [line
         (StringBuilder.)

         private-key?
         (volatile! false)

         render
         (fn [overflow?]
           (let [raw
                 (str line)

                 hidden?
                 (or @private-key? (re-find #"-----BEGIN (?:[A-Z0-9]+ )?PRIVATE KEY-----" raw))]

             [raw hidden?
              (cond hidden? "[REDACTED]"
                    overflow? "[installer output line exceeded limit; omitted]"
                    :else (redact-installer-text raw))]))

         emit!
         (fn [overflow?]
           (let [[raw hidden? text] (render overflow?)]
             (vreset! private-key?
                      (and (boolean hidden?)
                           (not (re-find #"-----END (?:[A-Z0-9]+ )?PRIVATE KEY-----" raw))))
             (swap! tail (fn [previous]
                           (let [text (str previous text "\n")]
                             (subs text (max 0 (- (count text) (long diagnostic-limit)))))))
             (reset! pending "")
             (.setLength line 0)))

         buffer
         (char-array 8192)]

     (with-open [reader (java.io.BufferedReader. reader)]
       (try (loop [overflow? false]
              (let [n (.read reader buffer)]
                (if (= -1 n)
                  (when (or overflow? (pos? (.length line))) (emit! overflow?))
                  (let [overflow? (loop [i 0
                                         overflow? overflow?]

                                    (if (= i n)
                                      overflow?
                                      (let [c (aget buffer i)]
                                        (cond (= \newline c) (do (emit! overflow?)
                                                                 (recur (inc i) false))
                                              (or overflow? (>= (.length line) diagnostic-limit))
                                              (recur (inc i) true)
                                              :else (do (.append line c) (recur (inc i) false))))))]
                    (when (or overflow? (pos? (.length line)))
                      (reset! pending (nth (render overflow?) 2)))
                    (recur overflow?)))))
            (catch java.io.IOException e
              ;; A closed pipe must not drop the partial line already read.
              (when (pos? (.length line)) (emit! false))
              (throw e))))
     @tail)))

(defn installer-error
  "A safe installation failure for CLI/UI boundaries. Keep only the verdict and
   a bounded, redacted diagnostic tail, never a raw command or exception cause."
  ^clojure.lang.ExceptionInfo [installer phase {:keys [exit timeout? diagnostics out]}]
  (let [diagnostics
        (capture-diagnostics! (java.io.StringReader. (str (or diagnostics out))) (atom ""))

        exit
        (when (integer? exit) exit)

        data
        {:type ::installer-failed
         :installer installer
         :phase phase
         :exit exit
         :timeout? (boolean timeout?)
         :diagnostics (str/trim diagnostics)}]

    (ex-info (str installer
                  " "
                  (name phase)
                  (if timeout? " timed out" " failed")
                  (when exit (str " (exit " exit ")"))
                  (when-not (str/blank? diagnostics) (str ":\n" (str/trim diagnostics))))
             data)))

(defn- bundled-uv!
  []
  (ensure-library!)
  (or (runtime/uv-executable)
      (throw (ex-info "Bundled uv is missing or not executable; reinstall the Vis Python runtime"
                      {:type ::bundled-uv-missing}))))

(defn- signal!
  "Send `signal` to `handles` with the system `kill` command. Without the command, they keep running."
  [signal handles]
  (when (seq handles)
    (try (-> (ProcessBuilder.
               ^java.util.List
               (into ["kill" (str "-" signal)] (map #(str (.pid ^ProcessHandle %))) handles))
             (.redirectErrorStream true)
             (.redirectOutput ProcessBuilder$Redirect/DISCARD)
             (.start)
             (.waitFor 5 TimeUnit/SECONDS))
         (catch java.io.IOException _ nil))))

(defn- kill-installer!
  "Kill the installer and all its descendants. First stop the tree from the root down:
   a stopped process cannot fork, so no child starts after its parent's children are read."
  [^Process process]
  (let [tree (loop [level [(.toHandle process)]
                    tree []]

               (if (seq level)
                 (do (signal! "STOP" level)
                     (recur (mapcat #(.toList ^java.util.stream.Stream (.children ^ProcessHandle %))
                                    level)
                            (into tree level)))
                 tree))]
    ;; Process/destroyForcibly also closes stdout on Linux, so the reader loses unread
    ;; output. Kill through the handle and let the reader drain the pipe to EOF.
    (doseq [^ProcessHandle handle (rseq tree)]
      (.destroyForcibly handle)))
  (.waitFor process 5 TimeUnit/SECONDS))

(def ^:dynamic ^:private *extension-preparation?* false)

(def ^:dynamic ^:private *workspace-dirs*
  "Nil, or an atom of uv workspace directories by project path for one preparation."
  nil)

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-follow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- delete-environment!
  "Delete `root` depth-first. A link is removed as a link, never followed."
  [^File root]
  (Files/walkFileTree
    (.toPath root)
    (proxy [SimpleFileVisitor] []
      (visitFile [^Path p _attrs] (Files/deleteIfExists p) FileVisitResult/CONTINUE)
      (postVisitDirectory [^Path p _e] (Files/deleteIfExists p) FileVisitResult/CONTINUE))))

(defn- embedded-executable
  "The executable of this process's embedded Python, inside the install that loaded it."
  ^String []
  (Interpreter/pythonExecutable))

(defn- build-length
  "The count of the leading bytes of the executable `file` that belong to its build: all
   of them, or for a thin 64-bit Mach-O the bytes before the code signature at its end.
   Each macOS release signs the same build again and each signature holds its own
   timestamp. With the signature in the identity, each release prepared every extension
   again."
  ^long [^File file]
  (let [length (.length file)]
    (or (when (<= 32 length)
          (with-open [in (RandomAccessFile. file "r")]
            (let [head (byte-array 32)
                  _ (.readFully in head)
                  header (.order (ByteBuffer/wrap head) ByteOrder/LITTLE_ENDIAN)
                  size (.getInt header 20)]

              (when (and (= (unchecked-int 0xfeedfacf) (.getInt header 0))
                         (pos? size)
                         (<= (+ 32 size) length))
                (let [block (byte-array size)
                      _ (.readFully in block)
                      commands (.order (ByteBuffer/wrap block) ByteOrder/LITTLE_ENDIAN)]

                  (loop [at 0]
                    (when (<= (+ at 16) size)
                      (let [command-size (.getInt commands (+ at 4))]
                        ;; 0x1d is LC_CODE_SIGNATURE: the offset and the size of the signature.
                        (if (= 0x1d (.getInt commands at))
                          (let [offset (Integer/toUnsignedLong (.getInt commands (+ at 8)))
                                signature (Integer/toUnsignedLong (.getInt commands (+ at 12)))]

                            (when (and (pos? offset) (= length (+ offset signature))) offset))
                          (when (<= 8 command-size) (recur (+ at command-size))))))))))))
        length)))

(defn- build-digest
  "Lowercase-hex SHA-256 of the build of the executable `file` and of the uv beside it:
   the content of each file without its code signature."
  ^String [^File file]
  (let [digest (util/sha256-digest)]
    (doseq [^File part [file (io/file (.getParentFile file) "uv")]
            :when (.isFile part)
            :let [length (build-length part)]]

      (.update digest (util/utf8 (str (.getName part) ":" length ":")))
      (with-open [in (io/input-stream part)]
        (let [buffer (byte-array 65536)]
          (loop [left length]
            (let [n (.read in buffer 0 (int (min left 65536)))]
              (when (pos? n) (.update digest buffer 0 n) (recur (- left n))))))))
    (util/bytes->hex (.digest digest))))

(defn- interpreter-store
  "The directory of the stable interpreter copies, or nil without a Vis state directory."
  ^File []
  (some-> (Locations/stateRoot)
          (io/file "interpreters")))

(defn- read-state
  "Read an optional state file. An unreadable or incomplete entry is a miss."
  [^File file]
  (when (and file (.isFile file))
    (try (edn/read-string (slurp file)) (catch Exception _ nil))))

(defn- write-state!
  "Publish optional state atomically. Read-only state does not prevent preparation."
  [^File file value]
  (when file
    (try (paths/spit-private! file (pr-str value) :fsync? false)
         (catch java.io.IOException _ nil))))

(defn- python-state-file
  "A file in the runtime state, separated by use and by canonical path."
  ^File [kind ^String path]
  (when-let [store (interpreter-store)]
    (io/file (.getParentFile store) kind (str (util/sha256-hex path) ".edn"))))

(defn- executable-input
  "The path and file metadata of an executable. Include Unix change time, so an
   in-place write that restores the old modification time is still a change."
  [^File file]
  (let [file (.getCanonicalFile file)
        path (.toPath file)]
    (try (let [^BasicFileAttributes attrs (Files/readAttributes path BasicFileAttributes
                                                                  ^"[Ljava.nio.file.LinkOption;" no-follow)]
           (when (.isRegularFile attrs)
             [(.getPath file) (.size attrs) (str (.lastModifiedTime attrs))
              (some-> (.fileKey attrs) str)
              (try (str (Files/getAttribute path "unix:ctime" (make-array LinkOption 0)))
                   (catch UnsupportedOperationException _ nil))]))
         (catch java.nio.file.NoSuchFileException _ nil))))

(defn- build-inputs
  "The inputs of the interpreter build digest, including the bundled uv if present."
  [^File file]
  (when-let [python (executable-input file)]
    [python (executable-input (io/file (.getParentFile file) "uv"))]))

(defonce ^:private interpreter-identities
  ;; Canonical executable path -> [file inputs, build digest].
  (atom {}))

(defn- interpreter-identity
  "Lowercase-hex SHA-256 of the interpreter build of `python`: the executable and uv
   beside it, without their code signatures. Reuse the hash while both files are unchanged."
  ^String [python]
  (let [file (.getCanonicalFile (io/file (str python)))
        path (.getPath file)]
    (if-let [inputs (build-inputs file)]
      (or (let [[saved identity] (get @interpreter-identities path)]
            (when (= saved inputs) identity))
          (let [stamp (python-state-file "build-identities" path)
                entry (read-state stamp)
                [saved identity] (when (vector? entry) entry)
                cached? (and (= saved inputs) (string? identity) (re-matches #"[0-9a-f]{64}" identity))
                identity (if cached? identity (build-digest file))]
            ;; A concurrent install must not stamp the digest of files that it was replacing.
            (if (= inputs (build-inputs file))
              (do (swap! interpreter-identities assoc path [inputs identity])
                  (when-not cached? (write-state! stamp [inputs identity]))
                  identity)
              (recur python))))
      (util/sha256-hex path))))

(defn- environment-name
  "The directory of the environment that automatic preparation owns for `python`.
   The name follows the interpreter build, not the install path: each native release, each
   pip install of `vis-agent` and each checkout has its own copy of the interpreter, and a
   name per path prepared every extension again for each copy (#486). A different build,
   such as the JVM's and an older native release's, gets its own environment."
  ^String [python]
  (str ".venv-" (subs (interpreter-identity python) 0 12)))

(defn- interpreter-home
  "The tree of the interpreter that `python` names: the parent of `bin/`, else its directory."
  ^File [^File python]
  (let [dir (.getParentFile python)]
    (if (= "bin" (.getName dir)) (.getParentFile dir) dir)))

(defonce ^:private copy-lock
  ;; One monitor for the interpreter copies of this process. See `with-copy-lock`.
  (Object.))

(defn- with-copy-lock
  "Call `f` while no other thread or process copies an interpreter into `store`.
   The monitor keeps the threads of this process apart; the OS lock keeps processes apart."
  [^File store f]
  (locking copy-lock
    (.mkdirs store)
    (with-open [channel
                (FileChannel/open (.toPath (io/file store ".vis-copy.lock"))
                                  (into-array OpenOption
                                              [StandardOpenOption/CREATE StandardOpenOption/WRITE]))

                _lock
                (.lock channel)]

      (f))))

(defn- interpreter-starts?
  "True when the interpreter `python` starts and imports its standard library. A copy with
   an intact executable but a damaged library tree cannot serve an environment."
  [^File python]
  (try (let [^java.util.List command
             [(.getPath python) "-I" "-c" "import encodings, os, site"]

             process
             (.start (doto (ProcessBuilder. command)
                       (.redirectErrorStream true)
                       (.redirectOutput ProcessBuilder$Redirect/DISCARD)))]

         (try (and (.waitFor process 30 TimeUnit/SECONDS) (zero? (.exitValue process)))
              (finally (when (.isAlive process) (.destroyForcibly process)))))
       (catch java.io.IOException _ false)))

(defn- copy-valid?
  "True when `copy` is an executable of the interpreter build `identity` that starts."
  [^File copy ^String identity]
  (and (.isFile copy)
       (= identity (interpreter-identity copy))
       (interpreter-starts? copy)))

(defn- copy-interpreter!
  "Copy the interpreter tree `home` to `target` for the build `identity`, unless `target`
   already holds it. The SDK packages of a native release stay out: they belong to one
   Vis version, not to the build. Each attempt copies into its own staging directory,
   checks the copied executable at `relative` and renames the tree into place. The copy
   lock holds while a staging directory exists, so a staging directory that the lock finds
   is from a stopped process. Answer true when `target` holds the build after the call."
  [^File store ^File home ^File target ^String relative ^String identity]
  (with-copy-lock
    store
    (fn []
      (or
        (copy-valid? (io/file target relative) identity)
        (do
          (doseq [^File stale
                  (or (.listFiles store) (make-array File 0))

                  :when (str/includes? (.getName stale) ".tmp.")]

            (delete-environment! stale))
          (let [staging
                (.toFile (Files/createTempDirectory (.toPath store)
                                                    (str (.getName target) ".tmp.")
                                                    (make-array FileAttribute 0)))

                tree
                (io/file staging "python")]

            (try (let [^java.util.List command
                       ["cp" "-Rp" (.getPath home) (.getPath tree)]

                       process
                       (.start (doto (ProcessBuilder. command)
                                 (.redirectErrorStream true)
                                 (.redirectOutput ProcessBuilder$Redirect/DISCARD)))]

                   (when (and (zero? (try (.waitFor process)
                                         (finally (when (.isAlive process) (kill-installer! process)))))
                              (copy-valid? (io/file tree relative) identity))
                     (let [packages (io/file tree "lib" "vis-sdk-packages")]
                       (when (.isDirectory packages) (delete-environment! packages)))
                     ;; A broken copy cannot serve an environment, so it is replaced.
                     (when (Files/exists (.toPath target) no-follow) (delete-environment! target))
                     (Files/move (.toPath tree)
                                 (.toPath target)
                                 (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
                     (copy-valid? (io/file target relative) identity)))
                 (catch InterruptedException e (throw e))
                 (catch Exception e
                   (tel/log! {:level :warn
                              :id ::interpreter-copy-failed
                              :data {:target (.getPath target)}
                              :error e})
                   false)
                 (finally (when (.exists staging) (delete-environment! staging))))))))))

(def ^:private installs-file
  "The file in a stable interpreter copy that lists the install executables of its build."
  ".vis-installs")

(defn- record-install!
  "Add the install executable `python` to the install list of the copy `target`."
  [^File store ^File target ^String python]
  (paths/with-claim-lock store
                         (fn []
                           (let [file
                                 (io/file target installs-file)

                                 known
                                 (if (.isFile file) (set (str/split-lines (slurp file))) #{})]

                             (when-not (contains? known python)
                               (spit file (str python "\n") :append true))))))

(defn- copy-in-use?
  "True when the stable copy `home` still serves an install: an install in its list still
   holds the build of the copy, or a process holds its claim. An update replaces an install
   at the same path, so an existing path is not enough. A copy without a list is not one of
   ours, so it stays."
  [^File store ^File home]
  (let [target
        (paths/nearest-ancestor #(= store (.getParentFile ^File %)) home)

        file
        (some-> target
                (io/file installs-file))]

    (or (nil? file)
        (not (.isFile file))
        (= :held (paths/claim-state target))
        (boolean (some #(and (seq %)
                             (.isFile (io/file %))
                             (str/starts-with? (interpreter-identity %) (.getName ^File target)))
                       (str/split-lines (slurp file)))))))

(defn- base-in-use?
  "True when the base interpreter directory `home` of an environment can still serve it:
   it exists and, for a stable copy, an install of its build still exists."
  [^File home]
  (let [store (some-> (interpreter-store)
                      .getCanonicalFile)]
    (and (.exists home)
         (or (nil? store)
             (not (.startsWith (.toPath (.getCanonicalFile home)) (.toPath store)))
             (copy-in-use? store (.getCanonicalFile home))))))

(defn- remove-unused-copies!
  "Delete the stable interpreter copies, other than `own`, that no install uses any more.
   Their environments are then orphans, and preparation removes them."
  [^File store ^File own]
  (doseq [^File copy
          (or (.listFiles store) (make-array File 0))

          :when (and (re-matches #"[0-9a-f]{12}" (.getName copy))
                     (not= (.getName own) (.getName copy))
                     (Files/isDirectory (.toPath copy) no-follow))]

    (try (paths/with-claim-lock store
                                #(when-not (copy-in-use? store copy) (delete-environment! copy)))
         (catch Exception e
           (tel/log! {:level :warn
                      :id ::interpreter-copy-not-removed
                      :data {:copy (.getPath copy)}
                      :error e})))))

(defonce ^:private base-pythons
  ;; Install executable path and build -> base executable for automatic environments.
  (atom {}))

(defn- base-python
  "The interpreter that automatic environments use as base: a copy of this build in the
   Vis state, so an environment outlives the install that made it and every install of the
   build shares it. uv keeps the base path in each environment and replaces an environment
   of another path. The JVM uses the copy too: with its own path, the JVM and a native release
   of one build replaced each other's environment. Without a state directory or when the copy
   fails, the install's own."
  ^String []
  (let [python (embedded-executable)
        identity (when python (interpreter-identity python))
        key [python identity]]
    (or (get @base-pythons key)
        (locking base-pythons
          (or (get @base-pythons key)
              (let [base (or (when-let [store (when python (interpreter-store))]
                               (let [file (.getCanonicalFile (io/file python))
                                     home (interpreter-home file)
                                     target (io/file store (subs identity 0 12))
                                     relative (str (.relativize (.toPath home) (.toPath file)))
                                     copy (io/file target relative)]

                                 (when (and (.isFile file)
                                            (or (copy-valid? copy identity)
                                                (copy-interpreter! store home target relative identity)))
                                   (record-install! store target (.getPath file))
                                   (paths/claim-dir! target)
                                   (remove-unused-copies! store target)
                                   (.getPath copy))))
                             python)]
                (swap! base-pythons assoc key base)
                base))))))

(def ^:dynamic ^:private *environment*
  "Nil, or the project's own `.venv` while this interpreter made it and prepares the project."
  nil)

(defn- automatic-environment
  "The environment that automatic preparation uses: the bound `.venv` of this interpreter,
   else the automatic environment of this process's embedded Python."
  ^String []
  (or *environment* (environment-name (embedded-executable))))

(defn- automatic-environment!
  "Point uv at the automatic environment during preparation, never at the caller's
   UV_PROJECT_ENVIRONMENT. Elsewhere keep the caller's choice."
  [^ProcessBuilder builder]
  (when *extension-preparation?*
    (.put (.environment builder) "UV_PROJECT_ENVIRONMENT" (automatic-environment))))

(defn- run-uv!
  "Run bundled uv. Failure and timeout retain phase, exit and safe diagnostics."
  ([project command] (run-uv! project command {}))
  ([^File project command {:keys [timeout-ms] :or {timeout-ms 180000}}]
   (let [phase
         (if (= "pip" (second command)) :install (keyword (second command)))

         process
         (try (let [builder (doto (ProcessBuilder. ^java.util.List command)
                              (.directory project)
                              uv-index!
                              bytecode-cache!
                              automatic-environment!
                              (.redirectErrorStream true))]
                (.start builder))
              (catch java.io.IOException e
                (throw (installer-error "uv" phase {:out (.getMessage e)}))))

         tail
         (atom "")

         pending
         (atom "")

         output
         (future
           (capture-diagnostics! (io/reader (.getInputStream ^Process process)) tail pending))]

     (try (.close (.getOutputStream ^Process process))
          (let [finished? (.waitFor ^Process process (long timeout-ms) TimeUnit/MILLISECONDS)]
            (when-not finished? (kill-installer! process))
            (let [capture-error (try (when (= ::unfinished (deref output 2000 ::unfinished))
                                       ;; A detached child holds the pipe. On macOS, the close
                                       ;; ends the read; on Linux, the read stays blocked, so
                                       ;; take the partial line that the reader put in `pending`.
                                       (.close (.getInputStream ^Process process))
                                       (try (deref output 2000 nil) (catch Exception _ nil))
                                       (let [partial @pending]
                                         (str (when (seq partial) (str partial "\n"))
                                              "Installer output did not close")))
                                     (catch Exception e (redact-installer-text (.getMessage e))))
                  exit (when finished? (.exitValue ^Process process))]

              (when (or (not finished?) (not= 0 exit) capture-error)
                (throw (installer-error "uv"
                                        phase
                                        {:exit exit
                                         :timeout? (not finished?)
                                         :diagnostics (str @tail capture-error)})))
              @tail))
          (finally (when (.isAlive ^Process process) (kill-installer! process))
                   (future-cancel output))))))

(defn- project-workspace
  "Find the canonical uv workspace without changing it."
  ^File [^File project]
  (let [lookup (fn []
                 (let [dir (io/file (str/trim (run-uv! project [(bundled-uv!) "workspace" "dir" "--offline"])))]
                   (when-not (and (.isAbsolute dir) (.isDirectory dir))
                     (throw (ex-info "uv did not report an existing workspace directory" {})))
                   (.getCanonicalFile dir)))
        path (.getCanonicalPath project)]
    (if-let [cache *workspace-dirs*]
      (or (get @cache path)
          (let [dir (lookup)]
            (swap! cache assoc path dir)
            dir))
      (lookup))))

(defn- project-environment
  "Locate uv's environment without creating it or resolving dependencies.
   Workspace members share the workspace environment. An override replaces .venv,
   and a relative override resolves against the workspace root."
  ^File [^File project override]
  (let [workspace (project-workspace project)
        environment (io/file (or (not-empty override) ".venv"))]
    (if (.isAbsolute environment) environment (io/file workspace (.getPath environment)))))

(defn project-environment-exists?
  "Check for uv's environment without creating it or resolving dependencies.
   UV_PROJECT_ENVIRONMENT overrides .venv. The automatic environment of this interpreter counts too.
   An existing but invalid environment still needs preparation, not shared fallback."
  [^File project]
  (binding [*workspace-dirs* (or *workspace-dirs* (atom {}))]
    (boolean (or (.exists (project-environment project (System/getenv "UV_PROJECT_ENVIRONMENT")))
                 (.exists (project-environment project (automatic-environment)))))))

(defn- environment-home
  "The base interpreter directory that the PEP 405 pyvenv.cfg of `environment` names, or nil."
  [^File environment]
  (let [config (io/file environment "pyvenv.cfg")]
    (when (.isFile config)
      (some #(second (re-matches #"\s*home\s*=\s*(.+?)\s*" %)) (str/split-lines (slurp config))))))

(defn- selected-python
  "The base interpreter of uv commands: the stable copy during automatic preparation into
   an automatic environment. A project `.venv` of this install and manual preparation
   keep the install's own embedded Python."
  ^String []
  (if (and *extension-preparation?* (nil? *environment*)) (base-python) (embedded-executable)))

(defn- own-environment?
  "True when the PEP 405 pyvenv.cfg of `environment` names the directory of the selected
   embedded Python as home. Reads one file and starts no uv."
  [^File environment]
  (let [home (environment-home environment)]
    (boolean (and home
                  (= (.getCanonicalFile (io/file home))
                     (-> (io/file (selected-python))
                         .getCanonicalFile
                         .getParentFile))))))

(defn- own-venv
  "`.venv` when this interpreter made the project's `.venv`, else nil.
   That `.venv` stays the project environment, as before. A `.venv` of the other
   embedded Python stays untouched: each Python then prepares its own environment."
  [^File project]
  (when (own-environment? (io/file project ".venv")) ".venv"))

(defn- runtime-environment?
  "True when the selected extension environment comes from the embedded Python.
   Automatic preparation uses its own environment. Manual validation honors UV_PROJECT_ENVIRONMENT.
   Its PEP 405 pyvenv.cfg names the directory of the base interpreter as home."
  [^File project]
  (own-environment? (project-environment project
                                         (if *extension-preparation?*
                                           (automatic-environment)
                                           (System/getenv "UV_PROJECT_ENVIRONMENT")))))

(defn- remove-environment-shell!
  "Remove an automatic environment that holds only empty directories.
   uv rejects such a shell and never replaces it. A file or link can be user data, so keep it."
  [^File project]
  (let [environment (project-environment project (automatic-environment))]
    (when (and (.isDirectory environment)
               (not-any? #(or (.isFile ^File %) (Files/isSymbolicLink (.toPath ^File %)))
                         (file-seq environment)))
      (delete-tree! environment))))

(defn- environment-build?
  "True when the executable of `environment` is still the interpreter build that its name
   records. An update replaces an install at the same path, and the environment of the old
   build cannot serve a process again. An environment without an executable stays."
  [^File environment]
  (let [python (io/file environment "bin" "python")]
    (or (not (.isFile python))
        (str/starts-with? (interpreter-identity python) (subs (.getName environment) 6)))))

(defonce ^:private swept-projects
  ;; Canonical project paths whose orphan environments this process removed.
  (atom #{}))

(defn- remove-orphan-environments!
  "Remove the automatic environments in `project` whose base interpreter is gone or
   holds another build. A native release keeps its interpreter in its own directory, so
   each release prepares a new environment. When the release is removed, no process can
   use that environment again. The bytecode cache of a removed tree is swept at startup.
   An environment on a stable copy stays while an install of that build exists.
   A link, a project `.venv` and an environment of an existing interpreter stay.
   Runs one time for each project in a process, also when the preparation is cached."
  [^File project]
  (swap! swept-projects conj (.getCanonicalPath project))
  (let [own (automatic-environment)]
    (doseq [^File environment (or (.listFiles project) (make-array File 0))
            :let [name (.getName environment)]
            :when (and (re-matches #"\.venv-[0-9a-f]{12}" name)
                       (not= own name)
                       (Files/isDirectory (.toPath environment) no-follow))
            :let [home (environment-home environment)]
            :when (and home (.isAbsolute (io/file home)))]

      (try (when-not (and (base-in-use? (io/file home)) (environment-build? environment))
             (delete-environment! environment)
             (tel/log! {:level :info
                        :id ::orphan-environment-removed
                        :data {:project (.getPath project) :environment name}}))
           (catch InterruptedException e (throw e))
           (catch Exception e
             (tel/log! {:level :warn
                        :id ::orphan-environment-not-removed
                        :data {:project (.getPath project) :environment name}
                        :error e}))))))

(defn- embedded-python
  "uv options that select an embedded Python executable and never download another."
  ([] (embedded-python (selected-python)))
  ([python] ["--python" python "--no-python-downloads"]))

(defn- export-project
  "Read uv's locked dependency selection without the `pruned` packages and the dependencies
   that only they select. Automatic preparation can update a stale lock."
  [^File project update? pruned]
  (let [requirements (Files/createTempFile (.toPath (paths/temp-dir)) "vis-project-" ".txt"
                                           (make-array FileAttribute 0))]
    (try (let [command (into (cond-> [(bundled-uv!) "export" "--format" "requirements-txt" "--no-header"
                                     "--no-annotate" "--no-hashes" "--output-file" (str requirements)]
                              (seq pruned) (into (mapcat #(vector "--prune" %) pruned)))
                            (embedded-python (embedded-executable)))
               locked (into command ["--locked" "--offline"])]
           (if update?
             (try (run-uv! project locked)
                  (catch clojure.lang.ExceptionInfo _ (run-uv! project command)))
             (run-uv! project locked))
           (slurp (.toFile requirements)))
         (finally (Files/deleteIfExists requirements)))))

(defn- shared-project?
  "Use shared packages when uv selects no project installation or dependencies beyond the SDK."
  [^File project automatic?]
  (str/blank? (export-project project automatic? ["vis-agent"])))

(defn- sdk-lock?
  "True when a project lock includes the SDK."
  [^File project]
  (let [lock (io/file project "uv.lock")]
    (and (.isFile lock) (boolean (re-find #"(?m)^name = \"vis-agent\"$" (slurp lock))))))

(defn- install-package-name
  "The normalized name in a pinned or direct-URL requirement from uv."
  [line]
  (when-let [[_ name] (re-find #"(?i)^([a-z0-9][a-z0-9._-]*)\s*(?:==|@)" line)]
    (str/replace (str/lower-case name) #"[-_.]+" "-")))

(def ^:private sdk-evaluation-packages
  "The SDK dependencies that only `blockether.vis.evals` imports. A managed environment omits
   their tree. The bundled SDK brings no dependencies, and `vis.session` and the contracts
   import the other ones, so the environment keeps them."
  ["harbor" "litellm"])

(def ^:private sdk-selection
  "The SDK selection that a stamp records. A stamp of another selection needs uv's check again."
  "sdk-runtime-kept")

(defn- sdk-exclusions
  "Omit the SDK and the packages that only its evaluation tree selects. `pruned` is the
   selection without the SDK tree, `kept` the selection without the evaluation tree.
   Keep names selected on any other branch. If uv emits a changed line without a package
   name, keep the full selection."
  [full pruned kept]
  (let [lines #(set (remove str/blank? (str/split-lines %)))
        retained (lines pruned)
        removed (remove retained (lines full))
        removed-names (map install-package-name removed)
        retained-names (set (map install-package-name retained))
        kept-names (set (map install-package-name (lines kept)))]
    (when (and (some #{"vis-agent"} removed-names) (every? some? removed-names)
               (not (contains? retained-names nil)))
      (sort (filter #(or (= "vis-agent" %) (not (kept-names %)))
                    (remove retained-names (set removed-names)))))))

(defn- project-packages
  "Ask uv's project interpreter for its site-packages; do not guess workspace paths.
   A missing environment comes from the embedded Python; an existing one stays as it is."
  ^File [^File project]
  (let
    [output
     (run-uv!
       project
       ;; -I ignores PYTHONPYCACHEPREFIX, so -B keeps bytecode out of the shipped runtime.
       (vec
         (concat
           [(bundled-uv!) "run" "--no-sync"]
           (embedded-python)
           ["python" "-I" "-B" "-c"
            "import json, sysconfig; print('VIS_PROJECT_SITE=' + json.dumps(sysconfig.get_path('purelib')))"])))

     path
     (some #(when (str/starts-with? % "VIS_PROJECT_SITE=")
              (json/read-json (subs % (count "VIS_PROJECT_SITE="))))
           (str/split-lines output))]

    (when-not (and (string? path) (.isDirectory (io/file path)))
      (throw (ex-info "uv project interpreter did not report an existing site-packages directory"
                      {})))
    (.getCanonicalFile (io/file path))))

(defn- project-argument
  "Name `project` for a sync command: `.` for the current directory, else its shell path."
  ^String [^File project]
  (if (= (.getCanonicalFile project) (.getCanonicalFile (io/file (System/getProperty "user.dir"))))
    "."
    (paths/shell-path (.getCanonicalPath project))))

(defn prepared-project
  "Validate a manually prepared environment. Never install or replace it.
   Source-only projects without dependencies beyond Vis use shared packages."
  ^File [^File project]
  (try (if (shared-project? project false)
         (.getCanonicalFile (io/file (runtime/packages-dir)))
         (do (when-not (runtime-environment? project)
               (throw (ex-info "Extension environment must use the embedded Python." {})))
             (run-uv! project (into [(bundled-uv!) "sync" "--check"] (embedded-python)))
             (project-packages project)))
       (catch clojure.lang.ExceptionInfo e
         (throw (ex-info (str (.getMessage e)
                              "\nRun vis-agent python uv sync --project "
                              (project-argument project)
                              " --python "
                              (paths/shell-path (Interpreter/pythonExecutable))
                              " --no-python-downloads, then /reload.")
                         (assoc (ex-data e) :type ::project-sync-required)
                         e)))))

(defonce ^:private preparation (atom {}))

(defonce ^:private preparation-gates (java.util.concurrent.ConcurrentHashMap.))

(defn- with-preparation-lock
  "Serialize writers to one workspace or package directory. Never remove the lock file:
   a waiting process may already have opened it."
  [^File file f]
  (let [key (.getCanonicalPath file)
        ^ReentrantLock gate (.computeIfAbsent ^java.util.concurrent.ConcurrentHashMap preparation-gates
                                             key
                                             (reify java.util.function.Function
                                               (apply [_ _] (ReentrantLock.))))]
    (.lockInterruptibly gate)
    (try (paths/create-private-dirs! (.getParentFile file))
         (with-open [channel (FileChannel/open (.toPath file)
                                               (into-array OpenOption
                                                           [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                     _lock (.lock channel)]
           (f))
         (finally (.unlock gate)))))

(defn- with-workspace-lock
  "Serialize Vis operations that can write to one uv workspace, even with different venvs."
  [^File workspace f]
  (with-preparation-lock
    (or (python-state-file "workspace-locks" (.getCanonicalPath workspace))
        (io/file (paths/vis-home) "python" "workspace-locks"
                 (str (util/sha256-hex (.getCanonicalPath workspace)) ".lock")))
    f))

(defn- with-shared-packages-lock
  "Serialize writes to the shared packages, without blocking unrelated uv workspaces."
  [packages f]
  (let [dir (.getCanonicalFile (io/file packages))]
    (with-preparation-lock (io/file (.getParentFile dir) (str ".vis-" (.getName dir) "-write.lock")) f)))

(defn preparation-status
  "Public, credential-free preparation stages keyed by package display name."
  []
  (vals @preparation))

(defn- project-display-name
  "How a prepared extension names ITSELF to a human. An installed extension lives in
   `<extensions>/<package>/<version>/`, so its project directory alone is a bare version
   number - `[vis extensions] 1.5.1: cached` named nothing the reader could act on, and
   two extensions sharing a version collided on one status entry. A version directory is
   therefore qualified with the package directory above it."
  ^String [^File project]
  (let [dir
        (.getName project)

        ^File parent
        (.getParentFile project)]

    (if (and parent (re-matches #"\d+\.[\w.+-]*" dir)) (str (.getName parent) " " dir) dir)))

(defn- preparation-stage!
  "Record a package's preparation stage for status. Only a failure reaches the terminal:
   a cached, installing or ready package is not news."
  [^File project stage]
  (let [name (project-display-name project)]
    (swap! preparation assoc name {:name name :stage stage})
    ;; An installation can take minutes. Its start in the log names the package that runs (#493).
    (when (= "installing" stage)
      (tel/log! {:level :info :id ::project-installing :data {:name name}}
                "installing extension dependencies"))
    (when (= "failed" stage)
      (.println config/original-stderr (str "[vis extensions] " name ": " stage))
      (.flush config/original-stderr))))

(def ^:private prepared-stamp
  "The file in a project's own automatic environment that records its last verified preparation."
  ".vis-prepared.edn")

(defn- path-sources
  "Local path sources that `lock` installs, the project included, with their build metadata.
   uv's check reads that metadata, so a stamp must change with it."
  [^File project lock]
  (vec (for [[_ path] (re-seq #"source = \{ (?:editable|directory|path|virtual) = \"([^\"]+)\" \}"
                              (or lock ""))]
         (let [source (let [file (io/file path)]
                        (if (.isAbsolute file) file (io/file project path)))]
           (if (.isDirectory source)
             [path
              (mapv #(let [file (io/file source %)] (when (.isFile file) (slurp file)))
                    ["pyproject.toml" "setup.cfg" "setup.py"])]
             [path (.lastModified source)])))))

(defn- update-digest!
  "Add `part` to `digest` with a kind and length prefix, so two values never share input.
   A sequential part adds its items. A string adds its UTF-8 bytes, other values `pr-str`."
  [^MessageDigest digest part]
  (if (sequential? part)
    (do (.update digest (util/utf8 (str "[" (count part) ":")))
        (doseq [item part]
          (update-digest! digest item)))
    (let [^bytes data (util/utf8 (if (string? part) part (pr-str part)))]
      (.update digest (util/utf8 (str (if (string? part) "s" "v") (alength data) ":")))
      (.update digest data))))

(defn- digest-parts
  "Lowercase-hex SHA-256 of `parts`. A string is hashed as it is: `pr-str` escaped a
   500 KB `uv.lock` one character at a time on every extension load."
  ^String [parts]
  (let [digest (util/sha256-digest)]
    (update-digest! digest parts)
    (util/bytes->hex (.digest digest))))

(defn- file-text
  "The contents of `file`, if it exists."
  [^File file]
  (when (.isFile file) (slurp file)))

(defn- shared-decision-inputs
  "Digest inputs that can change uv's dependency selection. Without a lock, keep the uv check."
  [^File project ^File packages]
  (let [files (vec (for [^File dir (paths/ancestor-chain project)
                         :let [manifest (file-text (io/file dir "pyproject.toml"))
                               lock (file-text (io/file dir "uv.lock"))
                               config (file-text (io/file dir "uv.toml"))]
                         :when (or manifest lock config)]
                     [(.getPath dir) manifest lock config (when lock (path-sources dir lock))]))
        config-home (or (not-empty (System/getenv "XDG_CONFIG_HOME"))
                        (str (System/getProperty "user.home") "/.config"))
        explicit-config (not-empty (System/getenv "UV_CONFIG_FILE"))]
    (when (some #(nth % 2) files)
      (digest-parts ["shared-sdk-v1" (.getCanonicalPath project) files
                     (->> (System/getenv)
                          (filter #(str/starts-with? (key %) "UV_"))
                          (sort-by key)
                          (mapv (fn [entry] [(key entry) (val entry)])))
                     (file-text (io/file config-home "uv" "uv.toml"))
                     (file-text (io/file "/etc/uv/uv.toml"))
                     (when explicit-config
                       (let [file (io/file explicit-config)]
                         (file-text (if (.isAbsolute file) file (io/file project explicit-config)))))
                     (interpreter-identity (embedded-executable)) (.getCanonicalPath packages)
                     (.isDirectory packages) (.lastModified packages)
                     (vec (sort (.list packages)))]))))

(defn- shared-decision-file
  ^File [^File project]
  (python-state-file "shared-decisions" (.getCanonicalPath project)))

(defn- stamped-shared-packages
  "Answer the shared package directory while the last classification is still valid."
  ^File [^File project]
  (when-let [saved (read-state (shared-decision-file project))]
    (when-let [packages (runtime/packages-dir)]
      (let [dir (.getCanonicalFile (io/file packages))]
        (when (= saved (shared-decision-inputs project dir)) dir)))))

(defn- prepared-inputs
  "Digest what uv's offline check reads, without uv: project and lock files, local path
   sources, the environment and the embedded tools. The bytecode cache stays an input, so
   the stamps of earlier releases stay valid. Nil without an own automatic environment.
   An mtime counts in whole seconds: tar, cp and image layers drop the milliseconds."
  [^File project ^File packages]
  (let [config
        (io/file project (automatic-environment) "pyvenv.cfg")

        text
        #(let [file (io/file project %)] (when (.isFile file) (slurp file)))

        lock
        (text "uv.lock")]

    (when (and (.isFile config) (.isDirectory packages))
      (digest-parts [(.getCanonicalPath project) (text "pyproject.toml") lock
                     (path-sources project lock) (slurp config) (quot (.lastModified packages) 1000)
                     (interpreter-identity (embedded-executable)) (pycache-prefix)]))))

(defn- stamped-packages
  "The site-packages of the project's last verified preparation, while its inputs are unchanged."
  ^File [^File project]
  (let [stamp (io/file project (automatic-environment) prepared-stamp)]
    (when (.isFile stamp)
      (try (let [{:keys [inputs packages selection]} (edn/read-string (slurp stamp))
                 packages (io/file (str packages))]

             (when (and (string? inputs)
                        (or (= sdk-selection selection) *environment* (not (sdk-lock? project)))
                        (= inputs (prepared-inputs project packages)))
               (.getCanonicalFile packages)))
           (catch Exception _ nil)))))

(defn- cached-project
  ^File [^File project]
  (when-let [packages (or (stamped-packages project) (stamped-shared-packages project))]
    (preparation-stage! project "cached")
    packages))

(defn- stamp-prepared!
  "Record a verified preparation in the project's own automatic environment and return `packages`.
   An environment elsewhere, such as a workspace member's, keeps the uv check.
   A read-only environment, such as one in a container image, keeps the uv check too."
  ^File [^File project ^File packages selection]
  (let [environment
        (.getCanonicalFile (io/file project (automatic-environment)))

        inputs
        (prepared-inputs project packages)]

    (when (and inputs
               (str/starts-with? (.getCanonicalPath packages)
                                 (str (.getPath environment) File/separator)))
      (try (let [staging (Files/createTempFile (.toPath environment)
                                               prepared-stamp
                                               ".tmp"
                                               (make-array java.nio.file.attribute.FileAttribute
                                                           0))]
             (try (spit (.toFile staging)
                        (pr-str {:inputs inputs :packages (.getCanonicalPath packages)
                                 :selection selection}))
                  (Files/move staging
                              (.toPath (io/file environment prepared-stamp))
                              (into-array CopyOption
                                          [StandardCopyOption/REPLACE_EXISTING
                                           StandardCopyOption/ATOMIC_MOVE]))
                  (finally (Files/deleteIfExists staging))))
           (catch java.io.IOException _ nil))))
  packages)

(defn- compile-bytecode!
  "Compile a project that an explicit `uv sync` installed, and its packages, into the
   bytecode cache. A worker then imports them without a compile (#407).
   A failure costs only speed, because a worker compiles what it imports.
   Environments and directory links of the project stay out: each other interpreter build
   keeps its own `.venv-<hash>` there, and compiling them all held the load lock 30 s (#498)."
  [^File project ^File packages]
  (let [prefix
        (pycache-prefix)

        python
        (Interpreter/pythonExecutable)

        root
        (.getCanonicalPath project)

        roots
        (-> (into []
                  (comp (remove #(or (str/starts-with? (.getName ^File %) ".venv")
                                     (and (.isDirectory ^File %)
                                          (Files/isSymbolicLink (.toPath ^File %)))))
                        (map #(.getPath ^File %)))
                  (sort (or (.listFiles (io/file root)) (make-array File 0))))
            (conj (.getCanonicalPath packages)))]

    (when (and prefix python)
      (try (let [process (.start (doto (ProcessBuilder. ^java.util.List
                                                        (into [python "-I" "-X"
                                                               (str "pycache_prefix=" prefix) "-m"
                                                               "compileall" "-q" "-j" "0"]
                                                              roots))
                                   (.directory project)
                                   (.redirectErrorStream true)
                                   (.redirectOutput ProcessBuilder$Redirect/DISCARD)))]
             (when-not (.waitFor process 300 TimeUnit/SECONDS) (kill-installer! process)))
           (catch java.io.IOException e
             (tel/log! {:level :warn
                        :id ::bytecode-compile-failed
                        :data {:project root :error (.getMessage e)}}))))))

(defn- preparation-failure
  "Record a failed preparation stage and wrap `t` as a credential-free preparation error."
  [^File project ^Throwable t]
  (preparation-stage! project "failed")
  (let [data
        (ex-data t)

        cause
        (installer-error "uv"
                         (or (:phase data) :prepare)
                         (assoc data :diagnostics (or (:diagnostics data) (.getMessage t))))]

    (ex-info (str "Extension preparation failed: " (.getMessage cause))
             (assoc (ex-data cause) :type ::project-preparation-failed)
             cause)))

(defn- prepare-project!
  "Classify and update `project` while its uv workspace is locked."
  ^File [^File project]
  (let [workspace (get @*workspace-dirs* (.getCanonicalPath project) project)
        manifest (file-text (io/file project "pyproject.toml"))
        ;; A workspace root and its members can use the same environment.
        prune? (and (nil? *environment*) (= (.getCanonicalFile project) workspace)
                    (not (Files/isSymbolicLink (.toPath (io/file workspace (automatic-environment)))))
                    (not (str/includes? (or manifest "") "workspace"))
                    (or (sdk-lock? project) (str/includes? (or manifest "") "vis-agent")))
        pruned (when prune? (export-project project true ["vis-agent"]))]
    (if (if prune? (str/blank? pruned) (shared-project? project true))
      (let [packages (.getCanonicalFile (io/file (runtime/packages-dir)))]
        (when-let [inputs (shared-decision-inputs project packages)]
          (write-state! (shared-decision-file project) inputs))
        (preparation-stage! project "cached")
        packages)
      (let [exclusions (when prune?
                         (sdk-exclusions (export-project project false nil)
                                         pruned
                                         (export-project project false sdk-evaluation-packages)))
            uv (bundled-uv!)
            options (cond-> (embedded-python)
                      (seq exclusions) (into (cons "--locked" (mapcat #(vector "--no-install-package" %) exclusions))))
            ready? (try (and (runtime-environment? project)
                             (do (run-uv! project (into [uv "sync" "--check" "--offline"] options))
                                 true))
                        (catch clojure.lang.ExceptionInfo _ false))]
        (when-not ready?
          (preparation-stage! project "installing")
          (let [stamp (io/file workspace (automatic-environment) prepared-stamp)]
            (Files/deleteIfExists (.toPath stamp)))
          (remove-environment-shell! project)
          (run-uv! project (into [uv "sync"] options)))
        (let [packages (project-packages project)]
          (stamp-prepared! project packages sdk-selection)
          (preparation-stage! project (if ready? "cached" "ready"))
          packages)))))

(defn ensure-project!
  "Prepare an extension with bundled uv and the embedded Python.
   Source-only projects without dependencies beyond Vis use shared packages.
   Otherwise uv owns lock updates, dependency groups and its package cache.
   A ready private environment must pass uv's offline check and use the embedded Python.
   A project `.venv` of this Python is that environment. Else each embedded Python owns its own.
   Stamps skip uv while the checked inputs stay unchanged (#407, #498).
   Preparation compiles no bytecode: a worker compiles what it imports into the cache."
  [^File project]
  (binding [*workspace-dirs* (atom {})
            *environment* (own-venv project)
            *extension-preparation?* true]
    (try (when-not (contains? @swept-projects (.getCanonicalPath project))
           (remove-orphan-environments! project))
         (or (cached-project project)
             (with-workspace-lock (if (some #(.isFile (io/file % "pyproject.toml"))
                                           (paths/ancestor-chain project))
                                    (project-workspace project)
                                    (.getCanonicalFile project))
               #(or (cached-project project) (prepare-project! project))))
         (catch InterruptedException e (throw e))
         (catch FileLockInterruptionException e (throw e))
         (catch Throwable t (throw (preparation-failure project t))))))

(def ^:private shared-sync-selection-options
  {"--locked" 0
   "--frozen" 0
   "--extra" 1
   "--all-extras" 0
   "--no-extra" 1
   "--group" 1
   "--all-groups" 0
   "--no-group" 1
   "--only-group" 1
   "--no-dev" 0
   "--only-dev" 0
   "--no-default-groups" 0
   "--package" 1
   "--all-packages" 0
   "--no-editable" 0
   "--no-editable-package" 1
   "--no-install-project" 0
   "--no-install-workspace" 0
   "--no-install-local" 0
   "--no-install-package" 1})

(def ^:private shared-sync-common-options
  {"--offline" 0
   "--no-cache" 0
   "--cache-dir" 1
   "--refresh" 0
   "--refresh-package" 1
   "--config-file" 1
   "--no-config" 0
   "--system-certs" 0
   "--allow-insecure-host" 1
   "--no-progress" 0
   "--quiet" 0
   "--index-strategy" 1
   "--keyring-provider" 1
   "--no-build-isolation" 0
   "--no-build-isolation-package" 1
   "--no-build" 0
   "--no-build-package" 1
   "--no-binary" 0
   "--no-binary-package" 1
   "--link-mode" 1})

(def ^:private shared-sync-location-options {"--project" 1 "--directory" 1})

;; `uv sync --check` compares a whole environment exactly. Shared packages always hold
;; packages of other projects, so these modes ask uv for the install plan instead.
(def ^:private shared-sync-mode-options {"--check" 0 "--dry-run" 0})

(defn- shared-sync-args
  [args]
  (loop [args
         (seq args)

         options
         {:selection [] :common [] :location [] :mode []}]

    (if-let [arg (first args)]
      (let [[flag inline] (str/split arg #"=" 2)
            [kind arity]
            (some (fn [[kind supported]]
                    (when-let [arity (get supported flag)]
                      [kind arity]))
                  [[:selection shared-sync-selection-options] [:common shared-sync-common-options]
                   [:location shared-sync-location-options] [:mode shared-sync-mode-options]])]

        (when-not kind
          (throw
            (ex-info
              "Unsupported shared sync option; run vis-agent python --shared uv sync --help. Use ordinary uv sync for other environments."
              {})))
        (when (or (and (zero? arity) inline)
                  (and (= 1 arity)
                       (or (str/blank? (or inline (second args)))
                           (str/starts-with? (or inline (second args)) "-"))))
          (throw (ex-info (str flag " requires " (if (zero? arity) "no value" "a value")) {})))
        (let [value (when (= 1 arity) (or inline (second args)))
              flag (str/replace flag "--no-install-" "--no-emit-")]

          (recur (if (and (= 1 arity) (nil? inline)) (nnext args) (next args))
                 (update options
                         kind
                         conj
                         (cond-> [flag]
                           value
                           (conj value))))))
      options)))

(defn- shared-sync-help!
  []
  (.println
    config/original-stdout
    (str
      "Usage: vis-agent python --shared uv sync [OPTIONS]\n\n"
      "Install a uv project's locked dependencies and editable project into Vis shared packages.\n"
      "Uses embedded Python; leaves .venv and unrelated shared packages alone.\n"
      "Existing versions can change. Run /reload after installing.\n"
      "--dry-run lists the planned installs. --check exits 1 when a locked package is missing\n"
      "or has another version; it ignores unrelated shared packages.\n\n"
      "Supported options (VALUE marks an argument):\n"
      (str/join "\n"
                (for [[flag arity] (sort (merge shared-sync-selection-options
                                                shared-sync-common-options
                                                shared-sync-location-options
                                                shared-sync-mode-options))]
                  (str "  " flag (when (= 1 arity) " VALUE"))))
      "\n  --help\n\n"
      "Configure indices with python.index_url, uv project configuration, or UV_* environment variables.\n"
      "Shared sync always retains unrelated packages; --active, --exact and interpreter overrides are not supported."))
  0)

(defn- shared-sync-process!
  [^File cwd args discard-stdout?]
  (let [builder (doto (ProcessBuilder. ^java.util.List args)
                  (.directory cwd)
                  uv-index!
                  bytecode-cache!
                  (.inheritIO))]
    (when discard-stdout? (.redirectOutput builder ProcessBuilder$Redirect/DISCARD))
    (.waitFor (.start builder))))

(defn- shared-sync-check!
  "Run the `--dry-run` install `args`; answer 0 when uv plans no change, else 1.
   uv plans only the locked packages, so unrelated shared packages never fail the check.
   An unknown plan text fails the check, which costs one extra sync at most."
  [^File cwd args]
  (let [process
        ;; The check reads the plan text, which --quiet removes.
        (.start (doto (ProcessBuilder. ^java.util.List (vec (remove #{"--quiet"} args)))
                  (.directory cwd)
                  uv-index!
                  bytecode-cache!
                  (.redirectErrorStream true)))

        output
        (slurp (.getInputStream process))

        exit
        (.waitFor process)]

    (cond (not (zero? exit)) (do (.print config/original-stderr output) exit)
          (str/includes? output "Would make no changes") 0
          :else (do
                  (.print config/original-stderr output)
                  (.println
                    config/original-stderr
                    "Shared packages differ from the lock; run vis-agent python --shared uv sync.")
                  1))))

(defn- editable-installs
  "Map each editable distribution in shared `packages` to its canonical source directory.
   uv records the source in PEP 610 `direct_url.json`; other installs are skipped."
  [packages]
  (into {}
        (keep (fn [^File info]
                (let [record (io/file info "direct_url.json")]
                  (when (and (str/ends-with? (.getName info) ".dist-info") (.isFile record))
                    (try (let [{:strs [url dir_info]} (json/read-json (slurp record))]
                           (when (and (map? dir_info)
                                      (true? (get dir_info "editable"))
                                      (str/starts-with? (str url) "file:"))
                             [(str/lower-case (first (str/split (.getName info) #"-" 2)))
                              (.getCanonicalPath (File. (java.net.URI. ^String url)))]))
                         (catch Exception _ nil))))))
        (.listFiles (io/file packages))))

(defn- warn-repointed-editables!
  "Name each shared editable package that this sync moved from an earlier directory (#353).
   Shared packages serve every session, so the earlier checkout now imports this code."
  [before after]
  (doseq [[package previous]
          (sort before)

          :let [current
                (get after package)]
          :when (and current (not= previous current))]

    (.println config/original-stderr
              (str "Warning: shared editable package "
                   package
                   " now uses "
                   current
                   "; it used "
                   previous
                   " before.\n"
                   "Vis sessions of "
                   previous
                   " now import the code of "
                   current
                   ".\n"
                   "To restore it, run: vis-agent python --shared uv sync --project "
                   previous))))

(defn- shared-change!
  "Run uv `command`, which changes shared `packages`; return its exit code. After a success,
   name each editable package that moved and the reload that Vis workers need."
  [^File cwd packages command]
  (with-shared-packages-lock packages
    (fn []
      (let [before
            (editable-installs packages)

            ;; uv keeps its `.lock` file there; only other entries are packages.
            entries
            #(disj (set (.list (io/file packages))) ".lock")

            present
            (entries)

            exit
            (long (shared-sync-process! cwd command false))]

        (when (zero? exit)
          (warn-repointed-editables! before (editable-installs packages))
          ;; A quiet command, such as the sync before `uv run`, stays quiet when nothing changed.
          (when-not (and (some #{"--quiet"} command) (= present (entries)))
            (.println config/original-stderr
                      (str "Shared packages: " packages "\nRun /reload to refresh Vis workers."))))
        exit))))

(defn- shared-uv-sync!
  [args]
  (if (some #{"--help" "-h"} (rest args))
    (shared-sync-help!)
    (let [{:keys [selection common location mode]}
          (shared-sync-args (rest args))

          uv
          (bundled-uv!)

          python
          (Interpreter/pythonExecutable)

          cwd
          (io/file (System/getProperty "user.dir"))

          directory
          (or (some #(when (= "--directory" (first %)) (second %)) (reverse location))
              (System/getenv "UV_WORKING_DIR"))

          effective-cwd
          (if directory
            (let [path (io/file directory)]
              (if (.isAbsolute path) path (io/file cwd directory)))
            cwd)

          ;; pip discovers configuration at the workspace root. Resolve explicit
          ;; path options before changing directories so their meaning stays intact.
          common
          (mapv (fn [[flag value :as option]]
                  (if (and (#{"--config-file" "--cache-dir"} flag)
                           (not (.isAbsolute (io/file value))))
                    [flag (.getCanonicalPath (io/file effective-cwd value))]
                    option))
                common)

          discovery
          (concat (mapcat identity location)
                  (mapcat identity (filter #(#{"--config-file" "--no-config"} (first %)) common)))

          project
          (io/file (str/trim (run-uv! cwd (into [uv "workspace" "dir"] discovery))))]

      (when-not (and (.isAbsolute project) (.isDirectory project))
        (throw (ex-info "uv did not report an existing workspace directory" {})))
      ;; uv writes local sources relative to the workspace root, and reads them
      ;; relative to the pylock's parent. Never put this file in the system temp dir.
      (let [lock-file
            (Files/createTempFile (.toPath project)
                                  "pylock.vis-"
                                  ".toml"
                                  (make-array java.nio.file.attribute.FileAttribute 0))

            interpreter
            ["--python" python "--no-python-downloads"]

            common
            (vec (mapcat identity common))]

        (try (let [exit (shared-sync-process! cwd
                                              (into [uv "export" "--format" "pylock.toml"
                                                     "--output-file" (str lock-file)]
                                                    (concat (mapcat identity location)
                                                            (mapcat identity selection)
                                                            common
                                                            interpreter))
                                              true)]
               (if-not (zero? exit)
                 exit
                 (let [packages (runtime/packages-dir)
                       modes (set (map first mode))
                       install (into [uv "pip" "install" "--target" packages "--requirements"
                                      (str lock-file) "--no-deps" "--directory"
                                      (.getCanonicalPath project)]
                                     (concat common interpreter (when (seq modes) ["--dry-run"])))
                       exit (cond (modes "--check") (shared-sync-check! cwd install)
                                  (seq modes) (shared-sync-process! cwd install false)
                                  :else (shared-change! cwd packages install))]

                   exit)))
             (finally (Files/deleteIfExists lock-file)))))))

(def ^:private shared-edit-install-options
  "Options of `uv add` and `uv remove` that select what the sync after the edit installs.
   uv refuses them together with --no-sync, so only the shared sync gets them."
  {"--no-install-project" 0
   "--no-install-workspace" 0
   "--no-install-local" 0
   "--no-install-package" 1})

(def ^:private shared-edit-options
  "Options of `uv add` and `uv remove` that the shared sync after the edit needs too: the
   project, the uv settings, the place of the dependency and the install selection."
  (merge shared-sync-location-options
         shared-sync-common-options
         shared-edit-install-options
         {"--package" 1 "--group" 1 "--optional" 1 "--dev" 0}))

(defn- without-options
  "`options` without the flags of `dropped`, a map of each flag to its count of values."
  [dropped options]
  (loop [options
         (seq options)

         kept
         []]

    (if-let [option (first options)]
      (let [[flag inline] (str/split option #"=" 2)
            arity (get dropped flag)]

        (cond (nil? arity) (recur (next options) (conj kept option))
              (and (= 1 arity) (nil? inline)) (recur (nnext options) kept)
              :else (recur (next options) kept)))
      kept)))

(defn- shared-edit-sync-args
  "The shared sync options that the `options` of `uv add` or `uv remove` imply. Upstream uv
   syncs the group or the extra that it changed, so the shared sync selects it too."
  [options]
  (loop [options
         (seq options)

         sync
         []]

    (if-let [option (first options)]
      (let [[flag inline] (str/split option #"=" 2)
            arity (get shared-edit-options flag)
            value (when (= 1 arity) (or inline (second options)))]

        (recur (if (and value (nil? inline)) (nnext options) (next options))
               (cond (nil? arity) sync
                     (= "--dev" flag) (conj sync "--group" "dev")
                     (= "--optional" flag) (conj sync "--extra" value)
                     value (conj sync flag value)
                     :else (conj sync flag))))
      sync)))

(defn- refuse-interpreter-option!
  "Shared packages serve one interpreter, so a shared uv command takes no other."
  [options]
  (when (some #(re-matches #"--python(=.*)?|-p.*" %) options)
    (throw (ex-info "Shared packages use the embedded Python of Vis; remove --python." {}))))

(defn- shared-uv-project!
  "Run `uv lock`, `uv export` or `uv tree`, which use only project files, with the embedded
   Python of Vis. Without it uv resolves for another interpreter that it finds or downloads."
  [[command & options]]
  (refuse-interpreter-option! options)
  (shared-sync-process! (io/file (System/getProperty "user.dir"))
                        (-> [(bundled-uv!) command]
                            (into (embedded-python))
                            (into options))
                        false))

(defn- shared-uv-edit!
  "Run `uv add` or `uv remove` for a project that installs into shared packages.
   uv changes `pyproject.toml` and `uv.lock` and never a project environment; a shared sync
   then installs the lock. As in upstream uv, --frozen and --no-sync stop after the edit, and
   --script changes only the script. uv refuses --no-sync together with --frozen and with the
   install selection, so an edit that stops gets the options unchanged. A removed package
   stays in shared packages, because other projects can use it."
  [[command & options :as args]]
  (let [uv
        (bundled-uv!)

        cwd
        (io/file (System/getProperty "user.dir"))]

    (if (some #{"--help" "-h"} options)
      (shared-sync-process! cwd (into [uv] args) false)
      (do (refuse-interpreter-option! options)
          (let [edit-only?
                (some #(re-matches #"--frozen|--no-sync|--script(=.*)?" %) options)

                exit
                (shared-sync-process! cwd
                                      (-> [uv command]
                                          (into (when-not edit-only? ["--no-sync"]))
                                          (into (embedded-python))
                                          (into (if edit-only?
                                                  options
                                                  (without-options shared-edit-install-options
                                                                   options))))
                                      false)]

            (if (or (not (zero? exit)) edit-only?)
              exit
              (let [exit (shared-uv-sync! (into ["sync"] (shared-edit-sync-args options)))]
                (when (and (zero? exit) (= "remove" command))
                  (.println
                    config/original-stderr
                    (str
                      "Shared packages keep a removed package, because other projects can use it.\n"
                      "To delete it, run: vis-agent python --shared uv pip uninstall NAME")))
                exit)))))))

(def ^:private shared-pip-commands
  "The `uv pip` commands that read or change a --target directory, each with true when it
   changes the packages. `sync` takes --target too, but an exact sync deletes the packages of
   every other project. `tree` and `check` cannot read a --target directory."
  {"install" true "uninstall" true "list" false "freeze" false "show" false})

(defn- shared-uv-pip!
  "Run a `uv pip` command on shared packages with the embedded Python of Vis."
  [[command & options]]
  (cond (= "sync" command)
        (throw (ex-info
                 (str
                   "--shared does not support uv pip sync: it deletes the shared packages of other "
                   "projects. Use vis-agent python --shared uv pip install -r FILE.")
                 {}))
        (= "check" command) (throw (ex-info (str "uv pip check cannot read shared packages. "
                                                 "Use vis-agent python --shared -m pip check.")
                                            {}))
        (= "tree" command) (throw (ex-info (str "uv pip tree cannot read shared packages. "
                                                "Use vis-agent python --shared uv pip list.")
                                           {}))
        (not (contains? shared-pip-commands command))
        (throw (ex-info (str "--shared supports uv pip "
                             (str/join ", " (sort (keys shared-pip-commands)))
                             ". Use vis-agent python uv pip for other commands.")
                        {}))
        :else (let [packages
                    (runtime/packages-dir)

                    cwd
                    (io/file (System/getProperty "user.dir"))

                    argv
                    (-> [(bundled-uv!) "pip" command "--target" packages]
                        (into (embedded-python))
                        (into options))]

                (if (and (shared-pip-commands command)
                         (not-any? #{"--dry-run" "--help" "-h"} options))
                  (shared-change! cwd packages argv)
                  (shared-sync-process! cwd argv false)))))

(def ^:private shared-run-sync-options
  "Options of `uv run` that the shared sync before the run takes."
  (merge shared-sync-selection-options shared-sync-common-options {"--project" 1}))

(def ^:private shared-run-console-script
  "Guest program for `uv run NAME`. `sys.argv[1]` is the shared packages directory and
   `sys.argv[2]` is NAME. Call the console script NAME of an installed package in this
   interpreter, as its launcher file does. Else run the file NAME that a package installed
   in `bin`: a Python script in this interpreter, another program as a child process."
  "import os, sys
from importlib.metadata import entry_points
packages, name = sys.argv[1], sys.argv[2]
sys.argv = sys.argv[2:]
found = [e for g in ('console_scripts', 'gui_scripts') for e in entry_points(group=g, name=name)]
if found:
    sys.exit(found[0].load()())
program = os.path.join(packages, 'bin', name)
if os.path.basename(name) != name or not os.path.isfile(program) or not os.access(program, os.X_OK):
    sys.exit(f'{name} is not a console script or a program of a package in Vis shared packages. '
             'Install its package: vis-agent python --shared uv pip install PACKAGE')
with open(program, 'rb') as file:
    lines = file.read(1024).split(b'\\n')
# uv starts a Python script with a long interpreter path through /bin/sh and an exec line.
if b'python' in lines[0] or (len(lines) > 1 and lines[1].startswith(b\"'''exec'\")):
    import runpy
    sys.argv[0] = program
    runpy.run_path(program, run_name='__main__')
else:
    import subprocess
    sys.exit(subprocess.call([program, *sys.argv[1:]]))
")

(defn- shared-run-help!
  []
  (.println
    config/original-stdout
    (str
      "Usage: vis-agent python --shared uv run [OPTIONS] COMMAND [ARGS]\n\n"
      "Run COMMAND in the embedded Python of Vis with Vis shared packages. No other Python starts.\n"
      "In a uv project, first install its locked dependencies into shared packages, as sync does.\n"
      "Uses no .venv.\n\n"
      "Commands:\n"
      "  python [ARGS]   The embedded Python: python -c CODE, python -m MODULE, python FILE.\n"
      "  FILE.py | -     A Python file, or a program from standard input.\n"
      "                  Inline script dependencies are not installed.\n"
      "  pip [ARGS]      pip on shared packages.\n"
      "  NAME [ARGS]     The console script or program NAME of a package in shared packages.\n"
      "                  A program that is not Python runs as a child process.\n\n"
      "Options:\n"
      "  -m, --module    COMMAND is a module, as in python -m COMMAND.\n"
      "  -s, --script    COMMAND is a Python file.\n"
      "  --no-sync, --no-project\n"
      "                  Do not sync the project first.\n"
      "  --help\n"
      "Options of the sync before the run (VALUE marks an argument):\n"
      (str/join "\n"
                (for [[flag arity] (sort shared-run-sync-options)]
                  (str "  " flag (when (= 1 arity) " VALUE"))))
      "\n\n"
      "Other programs, --with, --python and --directory are not supported.\n"
      "Use vis-agent python uv run to run a command in a project environment."))
  0)

(defn- shared-run-plan
  "Split the arguments of `uv run` into the `:sync` options, the `:project` directory, the
   `:sync?` and `:kind` switches and the `:command`. The options of uv end at the first word
   that is no option, so each option must be known with its count of values."
  [args]
  (loop [args
         (seq args)

         plan
         {:sync [] :sync? true :kind :command}]

    (let [arg
          (first args)

          [flag inline]
          (some-> arg
                  (str/split #"=" 2))

          arity
          (get shared-run-sync-options flag)]

      (cond
        (or (nil? arg) (= ["--"] args))
        (throw (ex-info "uv run requires a command; run vis-agent python --shared uv run --help."
                        {}))
        (#{"--help" "-h"} arg) (assoc plan :help? true)
        (= "--" arg) (assoc plan :command (vec (next args)))
        (or (= "-" arg) (not (str/starts-with? arg "-"))) (assoc plan :command (vec args))
        (#{"--no-sync" "--no-project"} arg) (recur (next args) (assoc plan :sync? false))
        (#{"-m" "--module"} arg) (recur (next args) (assoc plan :kind :module))
        (#{"-s" "--script"} arg) (recur (next args) (assoc plan :kind :script))
        (= 0 arity) (recur (next args) (update plan :sync conj arg))
        (= 1 arity) (let [value (or inline (second args))]
                      (when (str/blank? value) (throw (ex-info (str flag " requires a value") {})))
                      (recur (if inline (next args) (nnext args))
                             (cond-> (update plan :sync conj flag value)
                               (= "--project" flag)
                               (assoc :project value))))
        (re-matches #"--with(-editable|-requirements)?(=.*)?|-w.*" arg)
        (throw (ex-info
                 (str "--shared uv run does not support --with: the embedded Python of Vis uses no "
                      "temporary environment. Install the package first: "
                      "vis-agent python --shared uv pip install PACKAGE")
                 {}))
        :else (do (refuse-interpreter-option! [arg])
                  (throw (ex-info (str "--shared uv run does not support "
                                       flag
                                       "; run vis-agent python --shared uv run --help.")
                                  {})))))))

(defn- shared-run-project?
  "True when `uv run` finds a project to sync: a pyproject.toml in the `project` directory,
   by default the working directory, or in a directory above it."
  [project]
  (let [cwd
        (io/file (System/getProperty "user.dir"))

        start
        (cond (nil? project) cwd
              (.isAbsolute (io/file project)) (io/file project)
              :else (io/file cwd project))]

    (boolean (some #(.isFile (io/file % "pyproject.toml"))
                   (take-while some?
                               (iterate #(.getParentFile ^File %) (.getCanonicalFile start)))))))

(defn- shared-run-python-args
  "The `vis-agent python` arguments that run the `command` of `uv run` in the embedded Python."
  [kind [program & more :as command]]
  (cond (= :module kind) (into ["-m"] command)
        ;; After `--`, a script with the name `uv` is a file and not bundled uv.
        (or (= :script kind) (= "-" program) (str/ends-with? program ".py")) (into ["--"] command)
        (re-matches #"python(3(\.\d+)?)?" program) (vec more)
        (re-matches #"pip3?" program) (into ["-m" "pip"] more)
        :else (into ["-c" shared-run-console-script (str (runtime/packages-dir))] command)))

(defn- shared-uv-run!
  "Run the command of `uv run ARGS` in the embedded Python of this process; return its exit
   code. `python!` takes `vis-agent python --shared` arguments and returns the exit code.
   Like upstream uv, sync the project first. Unlike it, sync into shared packages and start
   no other interpreter, which could not import them."
  [args python!]
  (let [{:keys [help? sync project sync? kind command]} (shared-run-plan args)]
    (cond help? (shared-run-help!)
          (nil? python!) (throw (ex-info "Run it as: vis-agent python --shared uv run COMMAND" {}))
          :else (let [exit (if (and sync? (shared-run-project? project))
                             (try (long (shared-uv-sync! (into ["sync" "--quiet"] sync)))
                                  (catch clojure.lang.ExceptionInfo e
                                    ;; Upstream uv runs the command when the nearest pyproject.toml
                                    ;; has no project: there is nothing to install.
                                    (if (str/includes? (ex-message e) "No `project` table found")
                                      0
                                      (do (.println config/original-stderr (ex-message e)) 1))))
                             0)]
                  (if (zero? exit)
                    (python! (shared-run-python-args kind command))
                    (do (.println config/original-stderr
                                  (str "The project sync failed, so the command did not run. "
                                       "Add --no-sync to run it without the sync."))
                        exit))))))

(defn- shared-uv-help!
  []
  (.println
    config/original-stdout
    (str
      "Usage: vis-agent python --shared uv COMMAND [OPTIONS]\n\n"
      "Run uv on Vis shared packages with the embedded Python of Vis. No command makes or reads .venv.\n\n"
      "Commands:\n"
      "  sync            Install a project's locked dependencies. See sync --help.\n"
      "  add             Add dependencies to pyproject.toml and uv.lock, then sync.\n"
      "  remove          Remove dependencies from pyproject.toml and uv.lock, then sync.\n"
      "                  The removed packages stay in shared packages.\n"
      "  pip install     Install packages.\n"
      "  pip uninstall   Delete packages. Other projects can still need them.\n"
      "  run             Run a command in the embedded Python. See run --help.\n"
      "  pip list | pip freeze | pip show\n"
      "                  Read the installed packages.\n" "  lock | export | tree\n"
      "                  Use only project files, as in upstream uv.\n\n"
      "All but sync and run take the options of upstream uv, without --python.\n"
      "Use vis-agent python uv for all other uv commands.\n"
      "Run /reload after a change to refresh Vis workers."))
  0)

(defn- shared-uv!
  "Run `uv ARGS` on Vis shared packages, never on a project environment; return the exit code.
   Only commands that install into, delete from, read or run in an environment have a shared
   form. `python!` runs `vis-agent python --shared` arguments for `uv run`."
  [args python!]
  (case (first args)
    (nil "--help" "-h" "help")
    (shared-uv-help!)

    ("--version" "-V")
    (shared-sync-process! (io/file (System/getProperty "user.dir"))
                          [(bundled-uv!) (first args)]
                          false)

    "sync"
    (shared-uv-sync! args)

    ("add" "remove")
    (shared-uv-edit! args)

    ("lock" "export" "tree")
    (shared-uv-project! args)

    "pip"
    (shared-uv-pip! (rest args))

    "run"
    (shared-uv-run! (rest args) python!)

    (throw (ex-info (str "--shared supports uv sync, add, remove, run, lock, export, tree and pip; "
                         "run vis-agent python --shared uv --help. "
                         "Use vis-agent python uv for other uv commands.")
                    {}))))

(defn locations
  "Where this Vis keeps its Python, for a trusted extension that starts project Python (#479).
   An extension must not search `PATH` or guess the Vis home: a native binary in a container
   or a second installation has neither. `launcher` runs this Vis again from any directory."
  []
  (ensure-library!)
  {"launcher" (discovery/launcher-argv)
   "python_executable" (Interpreter/pythonExecutable)
   "uv_executable" (some-> (runtime/uv-executable)
                           str)
   "shared_packages" (str (runtime/packages-dir))})

(defn shared-environment?
  "True when merged config selects shared packages for project extensions.
   `python.environment` is `project` (the default) or `shared`."
  []
  (let [environment (get-in (config/load-config-raw) ["python" "environment"] "project")]
    (when-not (contract-config/definition-valid? "python" {"environment" environment})
      (throw (ex-info "python.environment must be project or shared"
                      {:type ::invalid-environment})))
    (= "shared" environment)))

(defn dependency-sync
  "Read merged `python.dependency_sync`: `auto` (the default) or `never`.
   With `never`, a tool that starts project Python installs nothing."
  []
  (let [sync (get-in (config/load-config-raw) ["python" "dependency_sync"] "auto")]
    (when-not (contract-config/definition-valid? "python" {"dependency_sync" sync})
      (throw (ex-info "python.dependency_sync must be auto or never"
                      {:type ::invalid-dependency-sync})))
    sync))

(defn tls-strict?
  "Read merged `python.tls_strict` for every Vis Python process. Invalid YAML must not weaken TLS."
  []
  (let [strict (get-in (config/load-config-raw) ["python" "tls_strict"] true)]
    (when-not (boolean? strict)
      (throw (ex-info "python.tls_strict must be a boolean" {:type ::invalid-tls-strict})))
    strict))

(def ^:private bundled-sdk
  "Vis runs its bundled SDK. Extension preparation never installs or checks it in shared packages,
   because each extension lock pins another version of it (#391)."
  "vis-agent")

(def ^:private shared-check-source
  "Python source that writes range requirements from a pylock and `uv.lock` (#391)."
  (delay (slurp (or (io/resource "vis-host-python/shared_check.py")
                    (throw (ex-info "Missing Vis resource vis-host-python/shared_check.py" {}))))))

(defn- workspace-temp-file
  "A new temporary file in `workspace`. uv writes local sources relative to the workspace root,
   and reads them relative to the file's parent. Never put this file in the system temp dir."
  ^java.nio.file.Path [^File workspace ^String suffix]
  (Files/createTempFile (.toPath workspace)
                        "pylock.vis-"
                        suffix
                        (make-array java.nio.file.attribute.FileAttribute 0)))

(defn- shared-project-install!
  "Install `project`'s locked dependencies into shared packages, as `--shared uv sync` does.
   Skip the bundled SDK."
  [^File project]
  (let [uv
        (bundled-uv!)

        workspace
        (project-workspace project)

        lock-file
        (workspace-temp-file workspace ".toml")

        packages
        (runtime/packages-dir)]

    (try (run-uv! project
                  (into [uv "export" "--format" "pylock.toml" "--output-file" (str lock-file)
                         "--no-emit-package" bundled-sdk]
                        (embedded-python)))
         (let [before (editable-installs packages)]
           (run-uv! project
                    (into [uv "pip" "install" "--target" packages "--requirements" (str lock-file)
                           "--no-deps" "--directory" (.getCanonicalPath workspace)]
                          (embedded-python)))
           (warn-repointed-editables! before (editable-installs packages)))
         (finally (Files/deleteIfExists lock-file)))))

(defn- shared-project-plan
  "Check offline, without an installation, that shared packages satisfy `project`. Answer nil when
   they do, else uv's planned changes. Every extension lock installs into the same packages, so the
   check uses the declared ranges, not the exact lock pins (#391)."
  [^File project]
  (let [uv
        (bundled-uv!)

        workspace
        (project-workspace project)

        [lock-file requirements overrides excludes :as files]
        (mapv #(workspace-temp-file workspace %) [".toml" ".txt" "-overrides.txt" "-excludes.txt"])]

    (try (run-uv! project
                  (into [uv "export" "--format" "pylock.toml" "--output-file" (str lock-file)
                         "--no-emit-package" bundled-sdk "--locked" "--offline"]
                        (embedded-python)))
         (spit (.toFile ^java.nio.file.Path excludes) (str bundled-sdk "\n"))
         (run-uv! project
                  (into [uv "run" "--no-project" "--offline"]
                        (concat (embedded-python)
                                ["python" "-I" "-B" "-c" @shared-check-source (str lock-file)
                                 (str (io/file workspace "uv.lock")) (str requirements)
                                 (str overrides) bundled-sdk])))
         (let [output (run-uv! project
                               (into [uv "pip" "install" "--target" (runtime/packages-dir)
                                      "--requirements" (str requirements) "--overrides"
                                      (str overrides) "--excludes" (str excludes) "--directory"
                                      (.getCanonicalPath workspace) "--offline" "--dry-run"]
                                     (embedded-python)))]
           (when-not (str/includes? output "Would make no changes") output))
         (finally (run! #(Files/deleteIfExists ^java.nio.file.Path %) files)))))

(defonce ^:private shared-prepared
  ;; Canonical project path -> the inputs of its last passed shared check or installation (#422).
  (atom {}))

(defn- shared-inputs
  "Digest what the shared check reads, without uv: the `pyproject.toml` and `uv.lock` files of the
   project and its ancestors, their local path sources, the shared packages and the embedded tools.
   Nil without a `uv.lock`, so that a project without a lock always keeps the uv check."
  [^File project]
  (let [project
        (.getCanonicalFile project)

        packages
        (io/file (runtime/packages-dir))

        text
        #(let [file (io/file %1 %2)] (when (.isFile file) (slurp file)))

        files
        (vec (for [^File dir
                   (paths/ancestor-chain project)

                   :let [pyproject
                         (text dir "pyproject.toml")

                         lock
                         (text dir "uv.lock")]
                   :when (or pyproject lock)]

               [(.getPath dir) pyproject lock (when lock (path-sources dir lock))]))]

    (when (and (some #(nth % 2) files) (.isDirectory packages))
      (digest-parts [(.getPath project) files (quot (.lastModified packages) 1000)
                     (vec (sort (.list packages))) (Interpreter/pythonExecutable)
                     (runtime/uv-executable) @shared-check-source]))))

(defn- shared-prepared?
  "True when `inputs` are those of the project's last passed shared check or installation."
  [^File project inputs]
  (boolean (and inputs (= inputs (get @shared-prepared (.getCanonicalPath project))))))

(defn- remember-shared!
  "Record that shared packages satisfy `project` with `inputs`."
  [^File project inputs]
  (when inputs (swap! shared-prepared assoc (.getCanonicalPath project) inputs)))

(defn ensure-shared-project!
  "Prepare an extension project in shared packages, as `vis-agent python --shared uv sync` does.
   Install only when shared packages do not satisfy the project. Never create or read `.venv`.
   While the inputs of the last passed check or installation stay the same, skip uv (#422)."
  ^File [^File project]
  (binding [*extension-preparation?* true
            *workspace-dirs* (atom {})]
    (try (let [packages (.getCanonicalFile (io/file (runtime/packages-dir)))
               inputs (shared-inputs project)]
           (if (shared-prepared? project inputs)
             (preparation-stage! project "cached")
             (with-workspace-lock (project-workspace project)
               #(with-shared-packages-lock packages
                  (fn []
                    (let [inputs (shared-inputs project)
                          ready? (or (shared-prepared? project inputs)
                                     (try (nil? (shared-project-plan project))
                                          (catch clojure.lang.ExceptionInfo _ false)))]
                      (when-not ready?
                        (preparation-stage! project "installing")
                        (shared-project-install! project))
                      ;; An installation changes shared packages: record their new state.
                      (remember-shared! project (if ready? inputs (shared-inputs project)))
                      (preparation-stage! project (if ready? "cached" "ready")))))))
           packages)
         (catch InterruptedException e (throw e))
         (catch FileLockInterruptionException e (throw e))
         (catch Throwable t (throw (preparation-failure project t))))))

(defn prepared-shared-project
  "Check that shared packages satisfy an extension project's lock. Never install.
   While the inputs of the last passed check or installation stay the same, skip uv (#422)."
  ^File [^File project]
  (let [hint
        (str "Run vis-agent python --shared uv sync --project "
             (project-argument project)
             ", then /reload.")

        inputs
        (shared-inputs project)]

    (when-let [plan (when-not (shared-prepared? project inputs)
                      (try (shared-project-plan project)
                           (catch clojure.lang.ExceptionInfo e
                             (throw (ex-info (str (.getMessage e) "\n" hint)
                                             (assoc (ex-data e) :type ::project-sync-required)
                                             e)))))]
      ;; Name the packages, so a real conflict between two extension locks is visible.
      (throw (ex-info (str/join "\n"
                                ["Shared packages differ from the extension project's lock."
                                 (str/trim plan) hint])
                      {:type ::project-sync-required})))
    (remember-shared! project inputs)
    (.getCanonicalFile (io/file (runtime/packages-dir)))))

(defn- option-value
  "The value of the long uv option `name` in `args`, from `--name value` or `--name=value`."
  [args name]
  (some (fn [[flag value]]
          (cond (= name flag) value
                (str/starts-with? (str flag) (str name "=")) (subs flag (inc (count name)))))
        (partition 2 1 [nil] args)))

(defn- sync-project
  "The project that `uv sync` `args` select: --project, else --directory, else the current directory."
  ^File [args]
  (let [resolve
        (fn [^File base path]
          (let [file (io/file path)]
            (if (.isAbsolute file) file (io/file base path))))

        directory
        (cond-> (io/file (System/getProperty "user.dir"))
          (option-value args "--directory")
          (resolve (option-value args "--directory")))

        ^File project
        (if-let [project (option-value args "--project")]
          (resolve directory project)
          directory)]

    (.getCanonicalFile project)))

(defn- stamp-synced-project!
  "After a successful `uv sync`, run the check of automatic preparation, compile the bytecode
   and stamp a ready project, so the next gateway start skips uv (#407). Another Python's environment gets no stamp."
  [args]
  (let [project (sync-project args)]
    (try (binding [*extension-preparation?* true
                   *workspace-dirs* (atom {})
                   *environment* (own-venv project)]

           (when (and (.isFile (io/file project "pyproject.toml"))
                      (not (shared-project? project false))
                      (runtime-environment? project))
             (run-uv! project (into [(bundled-uv!) "sync" "--check" "--offline"] (embedded-python)))
             (let [packages (project-packages project)]
               (compile-bytecode! project packages)
               (stamp-prepared! project packages nil))))
         (catch Exception _ nil))))

(defn uv-command!
  "Run bundled upstream uv with unchanged arguments and stdio; return its exit code.
   Explicit :shared? opts select Vis's shared sync workflow, never a project venv.
   `:python!` runs the command of a shared `uv run` in the embedded Python.
   Vis's python.index_url supplies UV_DEFAULT_INDEX unless uv's index environment
   is already set. Explicit uv CLI options retain upstream precedence.
   A successful `sync` stamps a ready extension project for the next preparation."
  ([args]
   (let [exit (.waitFor (.start (doto (ProcessBuilder. ^java.util.List (into [(bundled-uv!)] args))
                                  (.directory (io/file (System/getProperty "user.dir")))
                                  uv-index!
                                  bytecode-cache!
                                  (.inheritIO))))]
     (when (and (zero? exit) (= "sync" (first args))) (stamp-synced-project! args))
     exit))
  ([args {:keys [shared? python!]}] (if shared? (shared-uv! args python!) (uv-command! args))))

(defn pip-install!
  "Install `specs` with pip and make what landed importable in THIS process,
   answering pip's own `{:exit … :out … :command …}`.
   `python.index_url` from the merged vis.yml overrides pip's primary index;
   when absent, pip's inherited environment and configuration remain unchanged.

   pip runs as a host process writing into a directory this interpreter already
   has on `sys.path`, and a path entry remembers the listing it saw when it was
   first read. Without the invalidation the install succeeds and the very next
   import still raises `ModuleNotFoundError` — for the life of the process.
   Measured on a machine that had never installed pytest."
  ([specs] (pip-install! {} specs))
  ([opts specs]
   (let [index
         (configured-index-url)

         install
         #(runtime/pip-install! opts (into (if index ["--index-url" index] []) specs))

         result
         (let [packages (runtime/packages-dir)
               target (:target opts)]
           (if (and packages (or (nil? target)
                                 (= (.getCanonicalFile (io/file target))
                                    (.getCanonicalFile (io/file packages)))))
             (with-shared-packages-lock packages install)
             (install)))]

     (when (zero? (long (or (:exit result) 1)))
       (try
         (runtime/exec! runtime/default-session "import importlib; importlib.invalidate_caches()")
         (catch Throwable t (tel/log! {:level :warn :id ::import-caches-not-refreshed :error t}))))
     result)))
