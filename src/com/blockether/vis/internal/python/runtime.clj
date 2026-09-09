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
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.contract.config :as contract-config]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [com.blockether.vispython Interpreter Locations]
           [java.io File]
           [java.lang ProcessHandle]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

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

(defn ensure-library!
  "Make the interpreter for this platform resolvable, answering the library path.

   A no-op when the runtime already resolves one. Otherwise the cached
   installation is used, or the platform archive is fetched into it first. Every
   caller of the interpreter goes through `env-python/ensure-interpreter!`, which
   calls this before starting it."
  []
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
                (io/file (str (.getAbsolutePath home) ".tar.gz." (.pid (ProcessHandle/current))))]

            (tel/log! {:level :info :id ::fetching-runtime :url url :home (str home)})
            (try (download! url archive)
                 (install-archive! archive home)
                 (finally (.delete archive)))))
        (when-not (.isFile library)
          (throw (ex-info "The embedded CPython installation holds no runtime library."
                          {:home (str home) :platform platform :version version})))
        (runtime/use-library! (str home))
        (.getAbsolutePath library))))

(defn- index-args
  [flag]
  (let [python (get (config/load-config-raw) "python")]
    (if (contains? python "index_url")
      (let [index (get python "index_url")]
        (when-not (contract-config/definition-valid? "python" {"index_url" index})
          (throw (ex-info
                   "python.index_url must be an HTTP(S) URL without credentials, query or fragment"
                   {:type ::invalid-index-url})))
        [flag index])
      [])))

(defn- run-uv!
  "Run uv without exposing registry diagnostics, which can contain credentials."
  [^File project command]
  (let [process (.start (doto (ProcessBuilder. ^java.util.List command)
                          (.directory project)
                          (.redirectErrorStream true)
                          (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)))]
    (try
      (when-not (.waitFor process 180 TimeUnit/SECONDS)
        (throw (ex-info "uv dependency preparation timed out" {})))
      (when-not (zero? (.exitValue process))
        (throw
          (ex-info
            "uv dependency preparation failed; check uv.lock, project sources, python.index_url and Python compatibility"
            {:exit (.exitValue process)})))
      (finally (when (.isAlive process)
                 (with-open [children (.descendants process)]
                   (.forEach children
                             (reify
                               java.util.function.Consumer
                                 (accept [_ child] (.destroyForcibly ^ProcessHandle child)))))
                 (.destroyForcibly process))))))

(defn uv-sync!
  "Install a locked uv project into the shared packages directory.
   uv exports its resolved sources and artifact hashes to pylock.toml, then installs
   that lock with its target-directory installer, preserving editable local sources.
   Unrelated packages are retained.
   Builds are allowed for explicitly selected trusted projects. No project .venv
   or private dependency copy is created. Requires uv on PATH."
  ([project packages] (uv-sync! project packages []))
  ([^File project ^File packages options]
   (let [python (Interpreter/pythonExecutable)]
     (when-not python (throw (ex-info "uv requires the embedded Python executable" {})))
     (.mkdirs packages)
     ;; uv exports local source paths relative to the project, not --output-file.
     (let [lock-file (.toFile (Files/createTempFile
                                (.toPath project)
                                "pylock.vis-"
                                ".toml"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
           common (concat ["--python" python "--no-python-downloads"]
                          (index-args "--default-index")
                          options)]

       (try (run-uv! project
                     (into ["uv" "export" "--locked" "--no-default-groups" "--format" "pylock.toml"
                            "--project" (str project) "--output-file" (str lock-file)]
                           common))
            (run-uv! project
                     (into ["uv" "pip" "install" "--target" (str packages) "--requirements"
                            (str lock-file) "--no-deps"]
                           common))
            {:exit 0}
            (finally (Files/deleteIfExists (.toPath lock-file))))))))

(defn- project-key
  [^File project]
  (util/sha256-hex (pr-str [(.getCanonicalPath project) runtime/version
                            (Interpreter/pythonExecutable) (runtime/packages-dir)
                            (slurp (io/file project "pyproject.toml"))
                            (slurp (io/file project "uv.lock")) (index-args "--default-index")])))

(defn- project-home
  ^File [^File project]
  (io/file (System/getProperty "user.home")
           ".vis" "python"
           "projects" (util/sha256-hex (.getCanonicalPath project))))

(defn- package-metadata
  "Record installed distribution metadata so a later conflicting install invalidates readiness."
  [^File packages]
  (reduce (fn [installed ^File dir]
            (let [name
                  (.getName dir)

                  metadata
                  (io/file dir "METADATA")

                  record
                  (io/file dir "RECORD")]

              (if (and (.endsWith name ".dist-info") (.isFile metadata))
                (let [distribution (-> (first (str/split name #"-" 2))
                                       str/lower-case
                                       (str/replace #"[_.]+" "-"))]
                  (assoc-in installed
                    [distribution name]
                    (util/sha256-hex
                      (str (slurp metadata) "\n" (when (.isFile record) (slurp record))))))
                installed)))
          (sorted-map)
          (.listFiles packages)))

(defn prepared-project
  "Validate a manually prepared project and return the shared packages directory.
   Never installs or copies dependencies. A changed lock, runtime, index or installed
   distribution requires another explicit sync. Project state stores metadata only."
  ^File [^File project]
  (let [pointer
        (io/file (project-home project) (str (project-key project) ".ready"))

        packages
        (.getCanonicalFile (io/file (runtime/packages-dir)))

        ready
        (when (.isFile pointer) (try (edn/read-string (slurp pointer)) (catch Exception _ nil)))

        installed
        (package-metadata packages)]

    (when-not (and (.isDirectory packages)
                   (= (str packages) (:packages ready))
                   (map? (:metadata ready))
                   (every? (fn [[name digest]]
                             (= digest (get installed name)))
                           (:metadata ready)))
      (throw (ex-info (str
                        "Missing or stale Vis environment; run: vis-agent python uv sync --project "
                        (pr-str (.getCanonicalPath project))
                        " --locked")
                      {:type ::project-sync-required})))
    packages))

(defn sync-project!
  "Explicitly install a trusted uv project into the one shared packages directory.
   Publish readiness only after success; never remove unrelated installed packages."
  [^File project options]
  (when-not (every? #{"--offline" "--no-cache"} options)
    (throw (ex-info "Supported uv sync options: --project PATH, --locked, --offline, --no-cache"
                    {})))
  (let [project
        (.getCanonicalFile project)

        key
        (project-key project)

        home
        (project-home project)

        packages
        (.getCanonicalFile (io/file (runtime/packages-dir)))

        pointer
        (io/file home (str (java.util.UUID/randomUUID) ".ready"))]

    (.mkdirs home)
    (try (uv-sync! project packages options)
         (when-not (= key (project-key project))
           (throw (ex-info "Project changed during sync; run sync again" {})))
         (spit pointer (pr-str {:packages (str packages) :metadata (package-metadata packages)}))
         (Files/move (.toPath pointer)
                     (.toPath (io/file home (str key ".ready")))
                     (into-array CopyOption
                                 [StandardCopyOption/ATOMIC_MOVE
                                  StandardCopyOption/REPLACE_EXISTING]))
         {:exit 0 :packages (str packages)}
         (finally (.delete pointer)))))

(defonce ^:private preparation (atom {}))

(defonce ^:private preparation-lock (Object.))

(defn preparation-status
  "Public, credential-free preparation stages keyed by package display name."
  []
  (vals @preparation))

(defn- preparation-stage!
  [^File project stage]
  (let [name (.getName project)]
    (swap! preparation assoc name {:name name :stage stage})
    (.println config/original-stderr (str "[vis extensions] " name ": " stage))
    (.flush config/original-stderr)))

(defn ensure-project!
  "Automatically prepare a trusted package, reusing readiness when unchanged.
   Resolve a missing lock, never rewrite a supplied lock. Installer diagnostics
   stay private because index credentials may be present in them."
  [^File project]
  (locking preparation-lock
    (try
      (when-not (.isFile (io/file project "uv.lock"))
        (preparation-stage! project "resolving")
        (run-uv! project
                 (into ["uv" "lock" "--project" (str project) "--python"
                        (Interpreter/pythonExecutable) "--no-python-downloads"]
                       (index-args "--default-index"))))
      (let [ready? (try (prepared-project project)
                        true
                        (catch clojure.lang.ExceptionInfo e
                          (if (= ::project-sync-required (:type (ex-data e))) false (throw e))))]
        (when-not ready? (preparation-stage! project "installing") (sync-project! project []))
        (preparation-stage! project "ready")
        (prepared-project project))
      (catch Throwable _
        (preparation-stage! project "failed")
        (throw
          (ex-info
            "Extension preparation failed; check uv availability, Python compatibility and indexes. If pyproject.toml changed, update uv.lock and /reload."
            {:type ::project-preparation-failed}))))))

(defn uv-command!
  "Handle the explicit `python uv sync` command; refuse environment/interpreter overrides."
  [args]
  (when-not (= "sync" (first args))
    (throw (ex-info
             "Usage: vis-agent python uv sync --project PATH --locked [--offline] [--no-cache]"
             {})))
  (loop [args
         (next args)

         project
         (io/file (System/getProperty "user.dir"))

         options
         []]

    (case (first args)
      nil
      (sync-project! project options)

      "--project"
      (if-let [path (second args)]
        (recur (nnext args) (io/file path) options)
        (throw (ex-info "--project requires a directory" {})))

      "--locked"
      (recur (next args) project options)

      (if (#{"--offline" "--no-cache"} (first args))
        (recur (next args) project (conj options (first args)))
        (throw (ex-info "Unsupported uv sync option; Vis owns the interpreter and environment"
                        {}))))))

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
   (let [result (runtime/pip-install! opts (into (index-args "--index-url") specs))]
     (when (zero? (long (or (:exit result) 1)))
       (try
         (runtime/exec! runtime/default-session "import importlib; importlib.invalidate_caches()")
         (catch Throwable t (tel/log! {:level :warn :id ::import-caches-not-refreshed :error t}))))
     result)))
