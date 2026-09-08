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
            [clojure.java.io :as io]
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

(defn uv-sync!
  "Sync a locked uv project into a private snapshot using the embedded Python.
   Uses uv's project sources and noneditable installs, never the project's .venv.
   Builds are allowed for explicitly selected trusted projects. Output is discarded
   because registry diagnostics may contain credentials. Requires uv on PATH."
  [^File project ^File snapshot]
  (let [python
        (Interpreter/pythonExecutable)

        environment
        (io/file snapshot ".vis-uv")

        command
        (into ["uv" "sync" "--locked" "--no-editable" "--no-default-groups" "--no-python-downloads"
               "--python" python "--project" (str project)]
              (index-args "--default-index"))

        builder
        (doto (ProcessBuilder. ^java.util.List command)
          (.redirectErrorStream true)
          (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD))]

    (when-not python (throw (ex-info "uv requires the embedded Python executable" {})))
    (.put (.environment builder) "UV_PROJECT_ENVIRONMENT" (str environment))
    (let [process (.start builder)]
      (try
        (when-not (.waitFor process 180 TimeUnit/SECONDS)
          (throw (ex-info "Extension uv sync timed out" {})))
        (when-not (zero? (.exitValue process))
          (throw
            (ex-info
              "Extension uv sync failed; check uv.lock, project sources, python.index_url and Python compatibility"
              {:exit (.exitValue process)})))
        (finally (when (.isAlive process)
                   (with-open [children (.descendants process)]
                     (.forEach children
                               (reify
                                 java.util.function.Consumer
                                   (accept [_ child] (.destroyForcibly ^ProcessHandle child)))))
                   (.destroyForcibly process)))))
    (let [candidates (distinct (map #(.getCanonicalFile ^File %)
                                    (filter #(and (.isDirectory ^File %)
                                                  (= "site-packages" (.getName ^File %)))
                                            (file-seq environment))))]
      (when-not (= 1 (count candidates))
        (throw (ex-info "uv environment must have one site-packages directory" {})))
      (Files/move (.toPath ^File (first candidates))
                  (.toPath (io/file snapshot ".vis-packages"))
                  (make-array CopyOption 0)))
    {:exit 0}))

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
