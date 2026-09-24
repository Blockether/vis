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
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.contract.config :as contract-config]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [com.blockether.vispython Interpreter Locations]
           [java.io File]
           [java.lang ProcessBuilder$Redirect ProcessHandle]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

(defn version-globals
  "Host-owned build metadata shared by Python namespaces and the model prompt.
   The bundled SDK is versioned with Vis, never with an editable or pip package.
   Source builds report dev; an unavailable commit is nil, not a guessed release."
  []
  (let [version (gateway-runtime/release-version)]
    {"VIS_PYTHON_RUNTIME_VERSION" runtime/version
     "VIS_SHA_RELEASE" (gateway-runtime/release-sha)
     "VIS_VERSION" version
     "VIS_PYTHON_SDK_VERSION" version}))

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
  "The `~/.vis/python/<kind>/<version>` tree `path` belongs to, or nil when it
   sits outside this machine's store — a staged native distribution or a built
   checkout, neither of which anything here installs or reclaims."
  ^File [^String kind ^String path]
  (let [root (.getAbsolutePath (io/file (System/getProperty "user.home") ".vis" "python" kind))]
    (loop [^File dir (some-> path
                             io/file)]
      (when dir
        (if (= root
               (some-> (.getParentFile dir)
                       .getAbsolutePath))
          dir
          (recur (.getParentFile dir)))))))

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
   credential fragment created by truncation. Also flush a final partial line."
  [^java.io.Reader reader tail]
  (let [line
        (StringBuilder.)

        private-key?
        (volatile! false)

        emit!
        (fn [overflow?]
          (let [raw
                (str line)

                hidden?
                (or @private-key? (re-find #"-----BEGIN (?:[A-Z0-9]+ )?PRIVATE KEY-----" raw))

                text
                (cond hidden? "[REDACTED]"
                      overflow? "[installer output line exceeded limit; omitted]"
                      :else (redact-installer-text raw))]

            (vreset! private-key?
                     (and (boolean hidden?)
                          (not (re-find #"-----END (?:[A-Z0-9]+ )?PRIVATE KEY-----" raw))))
            (swap! tail (fn [previous]
                          (let [text (str previous text "\n")]
                            (subs text (max 0 (- (count text) (long diagnostic-limit)))))))
            (.setLength line 0)))]

    (with-open [reader (java.io.BufferedReader. reader)]
      (loop [overflow? false]
        (let [c (.read reader)]
          (cond (= -1 c) (when (or overflow? (pos? (.length line))) (emit! overflow?))
                (= 10 c) (do (emit! overflow?) (recur false))
                :else (if (or overflow? (>= (.length line) diagnostic-limit))
                        (recur true)
                        (do (.append line (char c)) (recur false)))))))
    @tail))

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

(defn- kill-installer!
  [^Process process]
  (with-open [children (.descendants process)]
    (.forEach children
              (reify
                java.util.function.Consumer
                  (accept [_ child] (.destroyForcibly ^ProcessHandle child)))))
  (.destroyForcibly process)
  (.waitFor process 5 TimeUnit/SECONDS))

(defn- run-uv!
  "Run bundled uv. Failure and timeout retain phase, exit and safe diagnostics."
  ([project command] (run-uv! project command {}))
  ([^File project command {:keys [timeout-ms] :or {timeout-ms 180000}}]
   (let [phase
         (if (= "pip" (second command)) :install (keyword (second command)))

         process
         (try (.start (doto (ProcessBuilder. ^java.util.List command)
                        (.directory project)
                        uv-index!
                        (.redirectErrorStream true)))
              (catch java.io.IOException e
                (throw (installer-error "uv" phase {:out (.getMessage e)}))))

         tail
         (atom "")

         output
         (future (capture-diagnostics! (io/reader (.getInputStream ^Process process)) tail))]

     (try (.close (.getOutputStream ^Process process))
          (let [finished? (.waitFor ^Process process (long timeout-ms) TimeUnit/MILLISECONDS)]
            (when-not finished? (kill-installer! process))
            (let [capture-error (try (when (= ::unfinished (deref output 2000 ::unfinished))
                                       "Installer output did not close")
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

(defn project-environment-exists?
  "Check for uv's environment without creating it or resolving dependencies.
   Workspace members share the workspace environment; UV_PROJECT_ENVIRONMENT
   overrides .venv and relative overrides resolve against the workspace root.
   An existing but invalid environment still needs preparation, not shared fallback."
  [^File project]
  (let [workspace
        (io/file (str/trim (run-uv! project [(bundled-uv!) "workspace" "dir" "--offline"])))

        path
        (or (not-empty (System/getenv "UV_PROJECT_ENVIRONMENT")) ".venv")

        environment
        (io/file path)]

    (when-not (and (.isAbsolute workspace) (.isDirectory workspace))
      (throw (ex-info "uv did not report an existing workspace directory" {})))
    (.exists (if (.isAbsolute environment) environment (io/file workspace path)))))

(defn- project-packages
  "Ask uv's project interpreter for its site-packages; do not guess workspace paths."
  ^File [^File project]
  (let
    [output
     (run-uv!
       project
       [(bundled-uv!) "run" "--no-sync" "python" "-I" "-c"
        "import json, sysconfig; print('VIS_PROJECT_SITE=' + json.dumps(sysconfig.get_path('purelib')))"])

     path
     (some #(when (str/starts-with? % "VIS_PROJECT_SITE=")
              (json/read-json (subs % (count "VIS_PROJECT_SITE="))))
           (str/split-lines output))]

    (when-not (and (string? path) (.isDirectory (io/file path)))
      (throw (ex-info "uv project interpreter did not report an existing site-packages directory"
                      {})))
    (.getCanonicalFile (io/file path))))

(defn prepared-project
  "Validate a manually prepared environment with uv sync --check; never install."
  ^File [^File project]
  (try (run-uv! project [(bundled-uv!) "sync" "--check"])
       (project-packages project)
       (catch clojure.lang.ExceptionInfo e
         (throw (ex-info (str (.getMessage e)
                              "\nRun vis-agent python uv sync --project "
                              (if (= (.getCanonicalFile project)
                                     (.getCanonicalFile (io/file (System/getProperty "user.dir"))))
                                "."
                                (pr-str (.getCanonicalPath project)))
                              ", then /reload; or use /reload --sync.")
                         (assoc (ex-data e) :type ::project-sync-required)
                         e)))))

(defonce ^:private preparation (atom {}))

(defonce ^:private preparation-lock (Object.))

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
  [^File project stage]
  (let [name (project-display-name project)]
    (swap! preparation assoc name {:name name :stage stage})
    (.println config/original-stderr (str "[vis extensions] " name ": " stage))
    (.flush config/original-stderr)))

(defn ensure-project!
  "Prepare an extension with bundled uv and the worker's embedded Python.
   An offline check reuses a ready environment without resolution or installation;
   otherwise uv owns lock updates, dependency groups and its package cache."
  [^File project]
  (locking preparation-lock
    (try (let [ready? (try (run-uv! project
                                    [(bundled-uv!) "sync" "--check" "--offline" "--python"
                                     (Interpreter/pythonExecutable)])
                           true
                           (catch clojure.lang.ExceptionInfo _ false))]
           (when-not ready?
             (preparation-stage! project "installing")
             (run-uv! project [(bundled-uv!) "sync" "--python" (Interpreter/pythonExecutable)]))
           (let [packages (project-packages project)]
             (preparation-stage! project (if ready? "cached" "ready"))
             packages))
         (catch Throwable t
           (preparation-stage! project "failed")
           (let [data
                 (ex-data t)

                 cause
                 (installer-error "uv"
                                  (or (:phase data) :prepare)
                                  (assoc data
                                    :diagnostics (or (:diagnostics data) (.getMessage t))))]

             (throw (ex-info (str "Extension preparation failed: " (.getMessage cause))
                             (assoc (ex-data cause) :type ::project-preparation-failed)
                             cause)))))))

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

(defn- shared-sync-args
  [args]
  (loop [args
         (seq args)

         options
         {:selection [] :common [] :location []}]

    (if-let [arg (first args)]
      (let [[flag inline] (str/split arg #"=" 2)
            [kind arity] (some (fn [[kind supported]]
                                 (when-let [arity (get supported flag)]
                                   [kind arity]))
                               [[:selection shared-sync-selection-options]
                                [:common shared-sync-common-options]
                                [:location shared-sync-location-options]])]

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
      "Existing versions can change. Run /reload after installing.\n\n"
      "Supported options (VALUE marks an argument):\n"
      (str/join "\n"
                (for [[flag arity] (sort (merge shared-sync-selection-options
                                                shared-sync-common-options
                                                shared-sync-location-options))]
                  (str "  " flag (when (= 1 arity) " VALUE"))))
      "\n  --help\n\n"
      "Configure indices with python.index_url, uv project configuration, or UV_* environment variables.\n"
      "Shared sync always retains unrelated packages; --check, --dry-run, --active and interpreter overrides are not supported."))
  0)

(defn- shared-sync-process!
  [^File cwd args discard-stdout?]
  (let [builder
        (doto (ProcessBuilder. ^java.util.List args) (.directory cwd) uv-index! (.inheritIO))]
    (when discard-stdout? (.redirectOutput builder ProcessBuilder$Redirect/DISCARD))
    (.waitFor (.start builder))))

(defn- shared-uv-sync!
  [args]
  (when-not (= "sync" (first args))
    (throw (ex-info "--shared supports uv sync only; use vis-agent python uv for other uv commands."
                    {})))
  (if (some #{"--help" "-h"} (rest args))
    (shared-sync-help!)
    (let [{:keys [selection common location]}
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
                       exit (shared-sync-process! cwd
                                                  (into [uv "pip" "install" "--target" packages
                                                         "--requirements" (str lock-file)
                                                         "--no-deps" "--directory"
                                                         (.getCanonicalPath project)]
                                                        (concat common interpreter))
                                                  false)]

                   (when (zero? exit)
                     (.println
                       config/original-stderr
                       (str "Shared packages: " packages "\nRun /reload to refresh Vis workers.")))
                   exit)))
             (finally (Files/deleteIfExists lock-file)))))))

(defn uv-command!
  "Run bundled upstream uv with unchanged arguments and stdio; return its exit code.
   Explicit :shared? opts select Vis's shared sync workflow, never a project venv.
   Vis's python.index_url supplies UV_DEFAULT_INDEX unless uv's index environment
   is already set. Explicit uv CLI options retain upstream precedence."
  ([args]
   (.waitFor (.start (doto (ProcessBuilder. ^java.util.List (into [(bundled-uv!)] args))
                       (.directory (io/file (System/getProperty "user.dir")))
                       uv-index!
                       (.inheritIO)))))
  ([args {:keys [shared?]}] (if shared? (shared-uv-sync! args) (uv-command! args))))

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

         result
         (runtime/pip-install! opts (into (if index ["--index-url" index] []) specs))]

     (when (zero? (long (or (:exit result) 1)))
       (try
         (runtime/exec! runtime/default-session "import importlib; importlib.invalidate_caches()")
         (catch Throwable t (tel/log! {:level :warn :id ::import-caches-not-refreshed :error t}))))
     result)))
