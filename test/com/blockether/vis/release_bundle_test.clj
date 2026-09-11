(ns com.blockether.vis.release-bundle-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.runtime :as protocol]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.net URL URLClassLoader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(defn- write-executable! [file body] (spit file body) (.setExecutable ^java.io.File file true) file)

(defn- run-bash
  "Runs `args` from the repository root with `env-extra` applied; merges stderr."
  [args env-extra]
  (let [pb
        (ProcessBuilder. ^java.util.List (vec args))

        env
        (.environment pb)]

    (.redirectErrorStream pb true)
    (doseq [[k v] env-extra]
      (.put env (str k) (str v)))
    (let [process
          (.start pb)

          output
          (slurp (.getInputStream process))]

      {:exit (.waitFor process) :output output})))

(defn- git!
  "Run git in `dir`, returning its trimmed output or throwing with the command output."
  [dir & args]
  (let [{:keys [exit output]}
        (run-bash (into ["git" "-C" (.getAbsolutePath ^java.io.File dir)] args) {})]
    (when-not (zero? (long exit))
      (throw (ex-info "Git fixture command failed" {:dir dir :args args :output output})))
    (str/trim output)))

(defn- with-source-update-fixture
  "Run one source update between two fixture commits with fetch failures or damaged packs."
  [{:keys [fetch-failures keep-gateway? pack-index corrupt-pack? installer? dirty?]} f]
  (let [root
        (.toFile (Files/createTempDirectory "vis-source-update-test-" (make-array FileAttribute 0)))

        home
        (doto (io/file root "home") .mkdirs)

        vis-home
        (io/file home ".vis")

        install-dir
        (doto (io/file vis-home "install") .mkdirs)

        managed-src
        (io/file install-dir "src")

        remote
        (doto (io/file root "remote") .mkdirs)

        remote-bin
        (doto (io/file remote "bin") .mkdirs)

        launcher-dir
        (doto (io/file root "installed") .mkdirs)

        launcher
        (io/file launcher-dir "vis-agent")

        path-dir
        (doto (io/file root "path") .mkdirs)

        clojure-calls
        (io/file root "clojure-calls")

        fetch-calls
        (io/file root "fetch-calls")

        failed-fetch
        (io/file root "failed-fetch")

        real-git
        (str/trim (:output (run-bash ["bash" "-lc" "command -v git"] {})))]

    (try
      (spit (io/file remote "deps.edn") "{}\n")
      (spit (io/file remote "update-marker") "old\n")
      (io/copy (io/file "bin/vis-agent") (io/file remote-bin "vis-agent"))
      (.setExecutable ^java.io.File (io/file remote-bin "vis-agent") true)
      (git! remote "init" "--quiet" "--initial-branch=main")
      (git! remote "add" ".")
      (git! remote
            "-c" "user.name=Vis Test"
            "-c" "user.email=vis@example.com"
            "commit" "--quiet"
            "-m" "old source")
      (let [old-commit (git! remote "rev-parse" "HEAD")]
        (spit (io/file remote "update-marker") "new\n")
        (git! remote "add" "update-marker")
        (git! remote
              "-c" "user.name=Vis Test"
              "-c" "user.email=vis@example.com"
              "commit" "--quiet"
              "-m" "new source")
        (let [new-commit (git! remote "rev-parse" "HEAD")]
          (git! root "clone" "--quiet" (.getAbsolutePath remote) (.getAbsolutePath managed-src))
          (git! managed-src "checkout" "--quiet" "--force" "--detach" old-commit)
          (git! managed-src "branch" "-D" "main")
          (git! managed-src "symbolic-ref" "--delete" "refs/remotes/origin/HEAD")
          (git! managed-src "update-ref" "-d" "refs/remotes/origin/main")
          (when pack-index
            (git! managed-src "repack" "-ad")
            (let [indexes (filter #(str/ends-with? (.getName ^java.io.File %) ".idx")
                                  (file-seq (io/file managed-src ".git" "objects" "pack")))]
              (expect (seq indexes))
              (doseq [index indexes]
                (case pack-index
                  :healthy
                  nil

                  :missing
                  (io/delete-file index)

                  :corrupt
                  (do (io/delete-file index) (spit index "invalid pack index\n"))

                  :truncated
                  (do (.setWritable ^java.io.File index true)
                      (with-open [file (java.io.RandomAccessFile. ^java.io.File index "rw")]
                        (.setLength file (- (.length file) 20)))))
                (when corrupt-pack?
                  (let [pack (io/file
                               (str/replace (.getPath ^java.io.File index) #"\.idx$" ".pack"))]
                    (io/delete-file pack)
                    (spit pack "invalid pack\n"))))))
          (when dirty? (spit (io/file managed-src "update-marker") "local work\n"))
          (spit (io/file install-dir "track") "dev\n")
          (spit (io/file install-dir "ref") (str old-commit "\n"))
          ;; Dev must ignore a native binary retained from an earlier installation.
          (write-executable! (io/file launcher-dir "vis-agent-native")
                             "#!/usr/bin/env bash\necho unexpected-native >&2\nexit 77\n")
          (write-executable!
            launcher
            (if installer?
              (str "#!/usr/bin/env bash\n"
                   "echo \"vis-agent: update: 'dev' is not a distribution track\" >&2\n"
                   "exit 1\n")
              (slurp "bin/vis-agent")))
          (when installer?
            (let [{:keys [exit output]}
                  (run-bash ["bash" (.getAbsolutePath launcher) "update" "--track" "dev"] {})]
              (expect (= 1 exit) output)
              (expect (str/includes? output "'dev' is not a distribution track") output))
            (io/copy (io/file "bin/install-vis-agent") (io/file root "install-vis-agent"))
            (write-executable!
              (io/file path-dir "curl")
              (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                   "[[ \"$*\" == *'/releases/download/installer/vis-agent'* ]] || exit 77\n"
                   "while (( $# )); do\n" "  if [[ \"$1\" == -o ]]; then cp -- "
                   "'" (.getAbsolutePath (io/file remote-bin "vis-agent"))
                   "' \"$2\"; exit 0; fi\n" "  shift\ndone\nexit 77\n")))
          (write-executable!
            (io/file path-dir "clojure")
            (str
              "#!/usr/bin/env bash\n" "set -euo pipefail\n"
              "head=\"$(\"$VIS_TEST_REAL_GIT\" -C \"$VIS_HOME/install/src\" rev-parse HEAD)\"\n"
              "printf '%s\\n' \"$head\" >> \"$VIS_TEST_CLOJURE_CALLS\"\n"
              "if [[ \"$head\" != \"$VIS_TEST_OLD\" ]]; then\n"
              "  printf '%s\\n' 'vis-agent: fatal error - Update the gateway - protocol mismatch' >&2\n"
              "  exit 1\n" "fi\n"))
          (when (pos? (long (or fetch-failures 0)))
            (write-executable!
              (io/file path-dir "git")
              (str
                "#!/usr/bin/env bash\n"
                "set -euo pipefail\n" "is_fetch=0\n"
                "for arg in \"$@\"; do [[ \"$arg\" != fetch ]] || is_fetch=1; done\n"
                "if (( is_fetch )); then\n"
                "  printf '%q ' \"$@\" >> \"$VIS_TEST_FETCH_CALLS\"; printf '\\n' >> \"$VIS_TEST_FETCH_CALLS\"\n"
                "  failures=0\n"
                "  [[ ! -e \"$VIS_TEST_FAILED_FETCH\" ]] || read -r failures < \"$VIS_TEST_FAILED_FETCH\"\n"
                "  if (( failures < VIS_TEST_FETCH_FAILURES )); then\n"
                "    printf '%s\\n' \"$((failures + 1))\" > \"$VIS_TEST_FAILED_FETCH\"\n"
                "    printf '%s\\n' 'fatal: unable to access remote: connection reset by peer' >&2\n"
                "    exit 128\n" "  fi\n"
                "fi\n" "exec \"$VIS_TEST_REAL_GIT\" \"$@\"\n")))
          (let [{:keys [exit output]}
                (run-bash (if installer?
                            ["bash" (.getAbsolutePath (io/file root "install-vis-agent"))
                             "--install-dir" (.getAbsolutePath launcher-dir) "--track" "dev"]
                            (cond-> ["bash" (.getAbsolutePath launcher) "update" "--track" "dev"]
                              keep-gateway?
                              (conj "--keep-gateway")))
                          {"HOME" (.getAbsolutePath home)
                           "VIS_HOME" (.getAbsolutePath vis-home)
                           "VIS_REPO_SLUG" "local/vis"
                           "VIS_REPO_URL" (.getAbsolutePath remote)
                           "VIS_NO_AUTO_INSTALL" "1"
                           "VIS_TEST_REAL_GIT" real-git
                           "VIS_TEST_OLD" old-commit
                           "VIS_TEST_CLOJURE_CALLS" (.getAbsolutePath clojure-calls)
                           "VIS_TEST_FETCH_CALLS" (.getAbsolutePath fetch-calls)
                           "VIS_TEST_FAILED_FETCH" (.getAbsolutePath failed-fetch)
                           "VIS_TEST_FETCH_FAILURES" (or fetch-failures 0)
                           "PATH" (str (.getAbsolutePath path-dir) ":" (System/getenv "PATH"))})]
            (expect (= "dev\n" (slurp (io/file install-dir "track"))))
            (f {:exit exit
                :output output
                :old-commit old-commit
                :new-commit new-commit
                :managed-src managed-src
                :launcher launcher
                :clojure-calls clojure-calls
                :fetch-calls fetch-calls}))))
      (finally (delete-tree! root)))))

(defn- with-native-install-fixture
  "Exercise installed commands with local release archives; Git/JVM are denied by default."
  [{:keys [installer? installed? missing-worker? missing-tui? track previous-track prepare!
           build-commit extra-env target]} f]
  (let [root
        (.toFile (Files/createTempDirectory "vis-native-install-" (make-array FileAttribute 0)))

        home
        (doto (io/file root "home") .mkdirs)

        bin
        (doto (io/file home "bin") .mkdirs)

        tools
        (doto (io/file root "tools") .mkdirs)

        payload
        (doto (io/file root "payload") .mkdirs)

        tui
        (doto (io/file root "tui") .mkdirs)

        launcher
        (io/file bin "vis-agent")

        native
        (io/file bin "vis-agent-native")

        urls
        (io/file root "urls")

        archive
        (io/file root "engine.tar.gz")

        tui-archive
        (io/file root "tui.tar.gz")

        env
        {"HOME" (.getAbsolutePath home)
         "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))
         "VIS_INSTALL_DIR" (.getAbsolutePath bin)
         "PATH" (str (.getAbsolutePath tools) ":" (.getAbsolutePath bin) ":" (System/getenv "PATH"))
         "VIS_TEST_URLS" (.getAbsolutePath urls)
         "VIS_TEST_ARCHIVE" (.getAbsolutePath archive)
         "VIS_TEST_TUI_ARCHIVE" (.getAbsolutePath tui-archive)}

        env
        (merge env extra-env)]

    (try
      (when previous-track
        (let [track-file (io/file (get env "VIS_HOME") "install" "track")]
          (io/make-parents track-file)
          (spit track-file (str previous-track "\n"))))
      (doseq [file [launcher (io/file payload "vis-agent")]]
        (io/copy (io/file "bin/vis-agent") file)
        (.setExecutable ^java.io.File file true))
      (when installed? (write-executable! native "#!/usr/bin/env bash\necho old-runtime\n"))
      (write-executable! (io/file payload "vis-agent-native")
                         "#!/usr/bin/env bash\necho new-runtime\n")
      (spit (io/file payload "vis-agent-native.build")
            (str "9.9.9 " (or build-commit "abc123") " " (or track "release") " now\n"))
      (when-not missing-worker?
        (.mkdirs (io/file payload "vis-agent-python/python"))
        (spit (io/file payload "vis-agent-python/libvispython.so") "runtime"))
      (when-not missing-tui?
        (write-executable! (io/file tui "vis-tui") "#!/usr/bin/env bash\necho native-tui\n"))
      (doseq [[dir dest] [[payload archive] [tui tui-archive]]]
        (let [{:keys [exit output]} (run-bash ["tar" "-czf" (.getAbsolutePath ^java.io.File dest)
                                               "-C" (.getAbsolutePath ^java.io.File dir) "."]
                                              {})]
          (expect (zero? exit) output)))
      (doseq [tool ["git" "java" "clojure"]]
        (write-executable! (io/file tools tool)
                           (str "#!/usr/bin/env bash\necho 'unexpected " tool "' >&2\nexit 77\n")))
      (when prepare! (prepare! env tools))
      (write-executable!
        (io/file tools "uname")
        "#!/usr/bin/env bash\ncase $1 in -s) echo Linux;; -m) echo x86_64;; esac\n")
      (write-executable!
        (io/file tools "curl")
        (str
          "#!/usr/bin/env bash\nset -euo pipefail\nurl=''; dest=''\n"
          "while (( $# )); do case $1 in -o) dest=$2; shift;; https:*) url=$1;; esac; shift; done\n"
          "printf '%s\\n' \"$url\" >> \"$VIS_TEST_URLS\"\n"
          "case $url in\n"
          (when (= track "beta")
            (str
              "  */releases/download/installer/native-beta) printf '%s\n' 'beta-"
              (apply str (repeat 40 "a"))
              "' ;;\n"
              ;; GitHub returned an older published beta first on the real installer path.
              "  *'releases?per_page=100&page=1') printf '%s' '[{\"assets\":[{\"browser_download_url\":\"https://github.com/example/vis/releases/download/beta-"
              (apply str (repeat 40 "b"))
              "/vis-agent-linux-x64.tar.gz\"}]}]' ;;\n"
              "  *'releases?per_page=100&page=2'|*/releases/tags/beta-*) printf '%s' '"
              "{\"assets\":[{\"browser_download_url\":\"https://github.com/example/vis/releases/download/beta-"
              (apply str (repeat 40 "a"))
              "/vis-agent-linux-x64.tar.gz\"},"
              "{\"browser_download_url\":\"https://github.com/example/vis/releases/download/beta-"
              (apply str (repeat 40 "a"))
              "/vis-tui-linux-x64.tar.gz\"}]}' ;;\n"))
          "  */releases/latest|*/releases/tags/v9.9.9) printf '%s' '"
          "{\"assets\":[{\"browser_download_url\":\"https://github.com/example/vis/releases/download/v9.9.9/vis-agent-linux-x64.tar.gz\"},"
          "{\"browser_download_url\":\"https://github.com/example/vis/releases/download/v9.9.9/vis-tui-linux-x64.tar.gz\"}]}' ;;\n"
          "  */vis-agent-linux-x64.tar.gz) cp \"$VIS_TEST_ARCHIVE\" \"$dest\" ;;\n"
          "  */vis-tui-linux-x64.tar.gz) cp \"$VIS_TEST_TUI_ARCHIVE\" \"$dest\" ;;\n"
          "  *) echo 'unexpected release URL' >&2; exit 22 ;;\nesac\n"))
      (let [args
            (if installer?
              ["bash" "bin/install-vis-agent"]
              ["bash" (.getAbsolutePath launcher) "update" "--keep-gateway"])

            result
            (run-bash (cond-> args
                        track
                        (into ["--track" track])

                        target
                        (conj target))
                      env)]

        (f (assoc result
             :bin bin
             :native native
             :launcher launcher
             :env env
             :urls (if (.exists urls) (slurp urls) ""))))
      (finally (delete-tree! root)))))

;; #195: native updates must refresh existing source to the artifact's commit, not main.
(defn- with-native-source-fixture
  [options f]
  (let [remote
        (.toFile (Files/createTempDirectory "vis-native-source-" (make-array FileAttribute 0)))

        commit!
        (fn [message]
          (git! remote "add" ".")
          (git! remote
                "-c" "user.name=Vis Test"
                "-c" "user.email=vis@example.com"
                "commit" "--quiet"
                "-m" message)
          (git! remote "rev-parse" "HEAD"))]

    (try (git! remote "init" "--quiet" "--initial-branch=main")
         (spit (io/file remote "deps.edn") "{}\n")
         (spit (io/file remote "VIS_VERSION") "9.9.8\n")
         (spit (io/file remote ".gitignore") "ignored-cache\n")
         (let [old
               (commit! "old source")

               _
               (spit (io/file remote "VIS_VERSION") "9.9.9\n")

               selected
               (commit! "native source")

               _
               (spit (io/file remote "VIS_VERSION") "10.0.0\n")

               _
               (commit! "newer main")]

           (with-native-install-fixture
             (merge
               {:installed? true
                :previous-track "beta"
                :build-commit selected
                :extra-env {"VIS_REPO_URL" (.getAbsolutePath remote)}
                :prepare!
                (fn [env tools]
                  (io/delete-file (io/file tools "git"))
                  (let [src (io/file (get env "VIS_HOME") "install" "src")]
                    (git! remote "clone" "--quiet" (.getAbsolutePath remote) (.getAbsolutePath src))
                    (git! src "checkout" "--quiet" "--detach" old)
                    (spit (io/file src ".." "ref") (str old "\n"))
                    (when-let [dirty (:dirty options)]
                      (spit (io/file src "ignored-cache") "cached data\n")
                      (spit (io/file src (if (= dirty :untracked) "local-work" "VIS_VERSION"))
                            "local work\n")
                      (when (= dirty :staged) (git! src "add" "VIS_VERSION")))
                    (when (:fetch-failure? options) (delete-tree! remote))))}
               (dissoc options :dirty :fetch-failure?))
             (fn [result]
               (f (assoc result
                    :old old
                    :selected selected
                    :src (io/file (get-in result [:env "VIS_HOME"]) "install" "src"))))))
         (finally (delete-tree! remote)))))

(defdescribe
  native-source-sync-test
  (it "pins release, explicit older release and beta source without selecting dev"
      (doseq [options [{:track "release"} {:target "v9.9.9"} {:track "beta"}]]
        (with-native-source-fixture
          options
          (fn [{:keys [exit output selected src launcher env]}]
            (expect (zero? exit) output)
            (expect (= selected (git! src "rev-parse" "HEAD")) output)
            (expect (= selected (str/trim (slurp (io/file src ".." "ref")))))
            (expect (= (str (or (:track options) "release") "\n")
                       (slurp (io/file src ".." "track"))))
            (expect (str/includes? output selected) output)
            (expect (str/includes? output "9.9.9") output)
            (expect (str/includes?
                      (:output (run-bash ["bash" (.getAbsolutePath launcher) "--version"] env))
                      "new-runtime"))))))
  ;; #195: local changes require manual attention, not automatic source replacement.
  (it "stops release and beta updates on dirty source with actionable instructions"
      (doseq [track
              ["release" "beta"]

              dirty
              [:tracked :staged :untracked]]

        (with-native-source-fixture
          {:track track :dirty dirty}
          (fn [{:keys [exit output old src native]}]
            (expect (not (zero? exit)) output)
            (expect (= old (git! src "rev-parse" "HEAD")))
            (expect (= (str old "\n") (slurp (io/file src ".." "ref"))))
            (expect (= "beta\n" (slurp (io/file src ".." "track"))))
            (expect (= "local work\n"
                       (slurp (io/file src (if (= dirty :untracked) "local-work" "VIS_VERSION")))))
            (expect (= (if (= dirty :staged) "local work" "9.9.8")
                       (git! src "show" ":VIS_VERSION")))
            (expect (= "cached data\n" (slurp (io/file src "ignored-cache"))))
            (expect (str/includes? (slurp native) "old-runtime"))
            (doseq [message ["local changes" (.getAbsolutePath src) "git -C" "status"
                             "commit or stash" "untracked files" "manually" "detached pin"
                             "same 'vis-agent update' command" "--track"
                             "Native installation and selected track unchanged"]]
              (expect (str/includes? output message) output))
            (expect (not (str/includes? output "installed the")) output)
            (expect (empty? (filter #(str/starts-with? (.getName ^java.io.File %) "src-recovery.")
                                    (.listFiles (.getParentFile ^java.io.File src)))))))))
  (it "reports local changes before trying to fetch source even when the remote is unavailable"
      (with-native-source-fixture
        {:dirty :staged :fetch-failure? true}
        (fn [{:keys [exit output old src native]}]
          (expect (not (zero? exit)) output)
          (expect (= old (git! src "rev-parse" "HEAD")))
          (expect (= (str old "\n") (slurp (io/file src ".." "ref"))))
          (expect (= "beta\n" (slurp (io/file src ".." "track"))))
          (expect (= "local work\n" (slurp (io/file src "VIS_VERSION"))))
          (expect (= "local work" (git! src "show" ":VIS_VERSION")))
          (expect (= "cached data\n" (slurp (io/file src "ignored-cache"))))
          (expect (str/includes? (slurp native) "old-runtime"))
          (expect (str/includes? output "local changes") output)
          (expect (not (str/includes? output "fetching")) output)
          (expect (not (str/includes? output "source recovery")) output)
          (expect (not (str/includes? output "installed the release track")) output))))
  (it "reports source fetch failure without claiming synchronization or replacing native"
      (with-native-source-fixture
        {:fetch-failure? true}
        (fn [{:keys [exit output old src native]}]
          (expect (not (zero? exit)) output)
          (expect (= old (git! src "rev-parse" "HEAD")))
          (expect (= (str old "\n") (slurp (io/file src ".." "ref"))))
          (expect (= "beta\n" (slurp (io/file src ".." "track"))))
          (expect (str/includes? (slurp native) "old-runtime"))
          (expect (str/includes? output "source refresh failed") output)
          (expect (not (str/includes? output "installed the release track")) output))))
  (it "never substitutes main when the stamped commit is unavailable"
      (with-native-source-fixture {:build-commit (apply str (repeat 40 "f"))}
                                  (fn [{:keys [exit output old src native]}]
                                    (expect (not (zero? exit)) output)
                                    (expect (= old (git! src "rev-parse" "HEAD")))
                                    (expect (= (str old "\n") (slurp (io/file src ".." "ref"))))
                                    (expect (str/includes? (slurp native) "old-runtime"))
                                    (expect (str/includes? output "source refresh failed")
                                            output))))
  (it "rejects an unresolvable build identity before changing either runtime"
      (with-native-source-fixture {:build-commit "unknown"}
                                  (fn [{:keys [exit output old src native]}]
                                    (expect (not (zero? exit)) output)
                                    (expect (= old (git! src "rev-parse" "HEAD")))
                                    (expect (str/includes? (slurp native) "old-runtime"))
                                    (expect (str/includes? output "build stamp") output)))))

(defdescribe
  production-install-test
  (it "installs a stable native engine, Python runtime and matching TUI without Git or Java"
      (with-native-install-fixture
        {:installer? true}
        (fn [{:keys [exit output bin launcher env urls]}]
          (expect (zero? exit) output)
          (expect (not (str/includes? output "unexpected")) output)
          (expect (str/includes? urls "/releases/latest") urls)
          (expect (not (.exists (io/file (get env "VIS_HOME") "install" "src"))))
          (expect (.isDirectory (io/file bin "vis-agent-python/python")))
          (expect (.canExecute (io/file bin "vis-tui")))
          (let [runtime (run-bash ["bash" (.getAbsolutePath launcher) "--version"] env)]
            (expect (str/includes? (:output runtime) "new-runtime") (:output runtime))))))
  (it "acquires native releases when a standalone wrapper has no runtime yet"
      (with-native-install-fixture {}
                                   (fn [{:keys [exit output urls]}]
                                     (expect (zero? exit) output)
                                     (expect (str/includes? urls "/releases/latest") urls))))
  (it "rejects incomplete bundles before replacing any installed component"
      (doseq [missing [:missing-worker? :missing-tui?]]
        (with-native-install-fixture {:installed? true missing true}
                                     (fn [{:keys [exit output native]}]
                                       (expect (not (zero? exit)) output)
                                       (expect (str/includes? (slurp native) "old-runtime")
                                               output))))))

;; Regression, session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: source update
;; replaced the runtime before asking its protocol-2 gateway to stop, then labelled a
;; transient fetch reset as an unadvertised main branch and downloaded all history.
(defdescribe
  source-update-transaction-test
  (it "releases the old gateway with the old runtime and keeps Git's detached pin private"
      (with-source-update-fixture
        {}
        (fn [{:keys [exit output old-commit new-commit managed-src clojure-calls]}]
          (expect (zero? exit) output)
          (expect (= old-commit (str/trim (slurp clojure-calls))) output)
          (expect (= new-commit (git! managed-src "rev-parse" "HEAD")) output)
          (expect (not (str/includes? output "Update the gateway")) output)
          (expect (not (str/includes? output "leaving 1 commit behind")) output))))
  ;; #195: dev must stop for the same local changes as release and beta.
  (it "leaves dirty managed source in place and requires manual attention before updating dev"
      (with-source-update-fixture
        {:dirty? true :keep-gateway? true}
        (fn [{:keys [exit output old-commit managed-src]}]
          (expect (not (zero? exit)) output)
          (expect (= old-commit (git! managed-src "rev-parse" "HEAD")))
          (expect (= old-commit (str/trim (slurp (io/file managed-src ".." "ref")))))
          (expect (= "local work\n" (slurp (io/file managed-src "update-marker"))))
          (doseq [message ["local changes" (.getAbsolutePath managed-src) "git -C" "status"
                           "commit or stash" "manually" "detached pin"
                           "same 'vis-agent update' command" "--track"]]
            (expect (str/includes? output message) output))
          (expect (not (str/includes? output "fetching")) output)
          (expect (not (str/includes? output "installed the dev track")) output)
          (expect (empty? (filter #(str/starts-with? (.getName ^java.io.File %) "src-recovery.")
                                  (.listFiles (.getParentFile ^java.io.File managed-src))))))))
  (it "retries the pinned fetch without pretending main is unadvertised"
      (with-source-update-fixture
        {:fetch-failures 1 :keep-gateway? true}
        (fn [{:keys [exit output new-commit managed-src fetch-calls]}]
          (let [calls (str/split-lines (slurp fetch-calls))]
            (expect (zero? exit) output)
            (expect (= new-commit (git! managed-src "rev-parse" "HEAD")) output)
            (expect (= 2 (count calls)) (pr-str calls))
            (expect (every? #(str/includes? % "--depth 1") calls) (pr-str calls))
            (expect (str/includes? output "retrying") output)
            (expect (not (str/includes? output "connection reset by peer")) output)
            (expect (not (str/includes? output "not an advertised branch")) output)))))
  (it "keeps a repeated transport failure bounded and reports the real error"
      (with-source-update-fixture
        {:fetch-failures 3 :keep-gateway? true}
        (fn [{:keys [exit output old-commit managed-src fetch-calls]}]
          (let [calls (str/split-lines (slurp fetch-calls))]
            (expect (not (zero? exit)) output)
            (expect (= old-commit (git! managed-src "rev-parse" "HEAD")) output)
            (expect (= 3 (count calls)) (pr-str calls))
            (expect (every? #(str/includes? % "--depth 1") calls) (pr-str calls))
            (expect (str/includes? output "connection reset by peer") output)
            (expect (not (str/includes? output "not an advertised branch")) output))))))

;; Regression #177: an unreadable pack index made source updates require a second run.
(defdescribe
  source-update-pack-index-test
  (it "updates once with healthy, missing or corrupt pack indexes"
      (doseq [pack-index [:healthy :missing :corrupt :truncated]]
        (with-source-update-fixture
          {:pack-index pack-index}
          (fn [{:keys [exit output old-commit new-commit managed-src clojure-calls]}]
            (expect (zero? exit) output)
            (expect (= old-commit (str/trim (slurp clojure-calls))) output)
            (expect (= new-commit (git! managed-src "rev-parse" "HEAD")) output)
            (expect (not (str/includes? output "error:")) output)
            (expect (= (not= :healthy pack-index) (str/includes? output "rebuilding pack index"))
                    output)
            (expect (= new-commit (str/trim (slurp (io/file managed-src ".." "ref")))))
            (expect (= "new\n" (slurp (io/file managed-src "update-marker"))))
            (let [{:keys [exit output]} (run-bash ["git" "-C"
                                                   (.getAbsolutePath ^java.io.File managed-src)
                                                   "fsck" "--no-dangling"]
                                                  {})]
              (expect (zero? exit) output))))))
  ;; Regression #177: damaged pack data needs a fresh fetch, not another index rebuild.
  (it "recovers corrupt packs and preserves the original checkout"
      (doseq [pack-index [:missing :healthy]]
        (with-source-update-fixture
          {:pack-index pack-index :corrupt-pack? true :keep-gateway? true}
          (fn [{:keys [exit output new-commit managed-src]}]
            (expect (zero? exit) output)
            (expect (= new-commit (git! managed-src "rev-parse" "HEAD")))
            (expect (= new-commit (str/trim (slurp (io/file managed-src ".." "ref")))))
            (expect (= "new\n" (slurp (io/file managed-src "update-marker"))))
            (expect (string? (git! managed-src "fsck" "--no-dangling")))
            (let [backups (filter #(.isDirectory ^java.io.File %)
                                  (filter #(str/starts-with? (.getName ^java.io.File %)
                                                             "src-recovery.")
                                          (.listFiles (.getParentFile ^java.io.File managed-src))))]
              (expect (= 1 (count backups)))
              (when-let [backup (first backups)]
                (expect (= "old\n" (slurp (io/file backup "previous" "update-marker"))))))))))
  (it "keeps the original source and pin if recovery cannot fetch"
      (with-source-update-fixture
        {:pack-index :missing :corrupt-pack? true :keep-gateway? true :fetch-failures 3}
        (fn [{:keys [exit output old-commit managed-src]}]
          (expect (not (zero? exit)) output)
          (expect (str/includes? output "vis-agent update") output)
          (expect (not (str/includes? output "source pinned at")) output)
          (expect (= old-commit (git! managed-src "rev-parse" "HEAD")))
          (expect (= old-commit (str/trim (slurp (io/file managed-src ".." "ref")))))
          (expect (= "old\n" (slurp (io/file managed-src "update-marker"))))))))

(defdescribe native-image-python-sidecar-test
             (it "stages the embedded interpreter beside the image instead of inside it"
                 (let [build (slurp "build.clj")]
                   ;; Embedding a whole interpreter tree pushed the builder's live set past what
                   ;; a 16 GB runner survives; the sidecar is what makes the release build finish.
                   (expect (str/includes? build "target/vis-agent-python") build)
                   (expect (str/includes? build "(stage-python-sidecar! basis)") build))))

(defdescribe
  stage-release-bundle-test
  (it
    "packs the sidecar and refuses a bundle that lost it"
    (let [root
          (.toFile (Files/createTempDirectory "vis-release-bundle-test-"
                                              (make-array FileAttribute 0)))

          from-dir
          (doto (io/file root "from") .mkdirs)

          bundle-dir
          (io/file root "bundle")

          asset
          (io/file root "vis-agent-linux-arm64.tar.gz")

          stamp
          "0.1.28 4c1f2a9dabcdef0123456789abcdef01234567 beta 2026-08-17T10:22:31.123Z\n"

          stage!
          (fn []
            (run-bash ["bash" "bin/stage-release-bundle" "--from-dir" (.getAbsolutePath from-dir)
                       (.getAbsolutePath asset)]
                      {"VIS_BUNDLE_DIR" (.getAbsolutePath bundle-dir)}))]

      (try (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent"]]
             (write-executable! (io/file from-dir entry) "#!/usr/bin/env bash\nexit 0\n"))
           ;; A binary without its interpreter is a runtime whose first Python call
           ;; dies with a library-not-found: hard error, never a warning.
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "vis-agent-python") output)
             (expect (not (.isFile asset)) "no asset may survive a rejected bundle"))
           (.mkdirs (io/file from-dir "vis-agent-python/python"))
           (spit (io/file from-dir "vis-agent-python/python/marker") "stdlib\n")
           ;; Regression, issue #148: a bundle carried no record of the commit its
           ;; runtime was built from, so a months-old binary beside fresh source
           ;; passed for current and its long-fixed crash was reported again.
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "vis-agent-native.build") output)
             (expect (not (.isFile asset)) "no asset may survive a rejected bundle"))
           (spit (io/file from-dir "vis-agent-native.build") stamp)
           ;; Vis #183: a Python directory alone does not prove uv was packaged.
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "python/bin/uv") output)
             (expect (not (.isFile asset)) "no asset may survive a rejected bundle"))
           (.mkdirs (io/file from-dir "vis-agent-python/python/bin"))
           (write-executable! (io/file from-dir "vis-agent-python/python/bin/uv")
                              "#!/bin/sh\nexit 0\n")
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "uv-LICENSE") output))
           (doseq [license ["uv-LICENSE-APACHE" "uv-LICENSE-MIT"]]
             (let [file (io/file from-dir "vis-agent-python/licenses" license)]
               (io/make-parents file)
               (spit file "license\n")))
           (let [{:keys [exit output]} (stage!)]
             (expect (= 0 exit) output)
             (expect (.isFile asset) output)
             (expect (= "stdlib\n" (slurp (io/file bundle-dir "vis-agent-python/python/marker"))))
             (expect (= stamp (slurp (io/file bundle-dir "vis-agent-native.build"))))
             (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent"]]
               (expect (.canExecute (io/file bundle-dir entry)) entry))
             (expect (.canExecute (io/file bundle-dir "vis-agent-python/python/bin/uv"))))
           (finally (delete-tree! root))))))

(defdescribe
  stage-tui-release-test
  (it
    "packs only the standalone executable and rejects a missing binary"
    (let [root
          (.toFile (Files/createTempDirectory "vis-tui-release-test-" (make-array FileAttribute 0)))

          binary
          (io/file root "vis-tui")

          bundle-dir
          (io/file root "bundle")

          asset
          (io/file root "vis-tui-linux-arm64.tar.gz")

          stage!
          (fn []
            (run-bash ["bash" "bin/stage-tui-release" (.getAbsolutePath binary)
                       (.getAbsolutePath asset)]
                      {"VIS_TUI_BUNDLE_DIR" (.getAbsolutePath bundle-dir)}))]

      (try (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "missing vis-tui binary") output))
           (write-executable! binary "#!/usr/bin/env bash\nexit 0\n")
           (let [{:keys [exit output]} (stage!)]
             (expect (= 0 exit) output)
             (expect (.isFile asset) output)
             (expect (.canExecute (io/file bundle-dir "vis-tui")) output)
             (expect (= #{"vis-tui"}
                        (->> (.listFiles bundle-dir)
                             (map #(.getName ^java.io.File %))
                             set))))
           (finally (delete-tree! root))))))

;; Regression #148: keep build provenance in the bundle, not a second CLI surface.
(defdescribe native-build-stamp-test
             (it "rejects the removed runtime command without starting an engine"
                 (let [source
                       (slurp "bin/vis-agent")

                       {:keys [exit output]}
                       (run-bash ["bash" "bin/vis-agent" "runtime"] {})]

                   (expect (not (zero? exit)) output)
                   (expect (str/includes? output "unknown command") output)
                   (expect (not (str/includes? source "vis_runtime()")))
                   (expect (not (str/includes? source "vis_runtime_usage()")))))
             (it "writes that stamp from the build — into the image and beside the binary"
                 (let [build-clj
                       (slurp "build.clj")

                       stage
                       (slurp "bin/stage-release-bundle")]

                   (expect (str/includes? build-clj "(spit (str native-bin \".build\")") build-clj)
                   (expect (str/includes? build-clj "\"-H:IncludeResources=vis/BUILD\"") build-clj)
                   ;; Provenance, never a version: `--version` still reports VIS_VERSION alone.
                   (expect (str/includes? build-clj "(spit vfile version)") build-clj)
                   (expect (str/includes? stage "vis-agent-native.build") stage))))

(defn- fake-tools!
  "A PATH directory whose `uname` claims `os`/`arch`, plus a container engine that
  answers `--version`/`info`/`run` without a VM. Lets the host-target and
  emulation rules be exercised for hosts this machine is not."
  [os arch]
  (let [dir (.toFile (Files/createTempDirectory "vis-release-native-test-"
                                                (make-array FileAttribute 0)))]
    (write-executable! (io/file dir "uname")
                       (str "#!/usr/bin/env bash\n"
                            "case \"${1:-}\" in\n"
                            "  -s) printf '"
                            os
                            "\\n' ;;\n"
                            "  -m) printf '"
                            arch
                            "\\n' ;;\n"
                            "  *)  printf '" os
                            "\\n' ;;\n" "esac\n"))
    (write-executable! (io/file dir "engine")
                       (str "#!/usr/bin/env bash\n" "case \"${1:-}\" in\n"
                            ;; `info` doubles as the RAM probe: 32 GiB, so the
                            ;; memory guard passes and the emulation one decides.
                            "  --version) printf 'podman version 6.0.0\\n' ;;\n"
                            "  info)      printf '34359738368\\n' ;;\n"
                            "  *)         exit 0 ;;\n" "esac\n"))
    dir))

(defdescribe
  platform-asset-name-test
  (it "selects the same unqualified asset name the release workflows publish"
      (let [wrapper
            (slurp "bin/vis-agent")

            platform-function
            (re-find #"(?ms)^vis_platform_asset\(\) \{\n.*?^\}" wrapper)

            stable
            (slurp ".github/workflows/native-release.yml")

            beta
            (slurp ".github/workflows/beta-native.yml")]

        (expect (some? platform-function))
        (doseq [[os arch asset] [["Linux" "x86_64" "vis-agent-linux-x64.tar.gz"]
                                 ["Linux" "aarch64" "vis-agent-linux-arm64.tar.gz"]
                                 ["Darwin" "arm64" "vis-agent-macos-arm64.tar.gz"]]]
          (let [dir (fake-tools! os arch)]
            (try (let [{:keys [exit output]}
                       (run-bash ["bash" "-c" (str platform-function "\nvis_platform_asset")]
                                 {"PATH" (str (.getAbsolutePath dir) ":" (System/getenv "PATH"))})]
                   (expect (= 0 exit) output)
                   (expect (= asset output))
                   (expect (str/includes? stable asset) asset)
                   (expect (str/includes? beta "uses: ./.github/workflows/native-release.yml")))
                 (finally (delete-tree! dir)))))))
  (it "has no distribution-profile selector in the build"
      (let [build (slurp "build.clj")]
        (expect (not (str/includes? build "resolve-profile")))
        (expect (not (str/includes? build ":profile"))))))

(defn- run-release-native
  [^java.io.File dir args env-extra]
  (run-bash (into ["bash" "bin/release-native"] args)
            (merge {"PATH" (str (.getAbsolutePath dir) ":" (System/getenv "PATH"))} env-extra)))

(defdescribe release-native-targets-test
             (it "builds every asset an Apple-silicon host can reach, and refuses the rest"
                 (let [mac
                       (fake-tools! "Darwin" "arm64")

                       linux-arm
                       (fake-tools! "Linux" "aarch64")]

                   (try
                     ;; linux-x64 belongs here now: native-image under Rosetta measured 4.8x
                     ;; native (15.8 s vs 1 m 16 s for a hello-world image), still inside the
                     ;; 86-130 min the free x64 runner takes when it does not OOM.
                     (let [{:keys [exit output]} (run-release-native mac ["--list"] {})]
                       (expect (= 0 exit) output)
                       (doseq [target ["macos-arm64" "linux-arm64" "linux-x64"]]
                         (expect (str/includes? output target) output)))
                     ;; A Linux host cannot produce the macOS asset, whatever is installed.
                     (let [{:keys [exit output]}
                           (run-release-native linux-arm ["--targets" "macos-arm64"] {})]
                       (expect (not= 0 exit) output)
                       (expect (str/includes? output "cannot build") output))
                     (finally (delete-tree! mac) (delete-tree! linux-arm))))))

(defdescribe release-native-emulation-guard-test
             (it "refuses a foreign platform that is not Rosetta-fast, before building anything"
                 (let [mac (fake-tools! "Darwin" "arm64")]
                   (try (let [{:keys [exit output]}
                              ;; A budget no measurement can meet stands in for qemu-user: the guard
                              ;; must fire on the probe, never after an hour of analysis.
                              (run-release-native mac
                                                  ["--targets" "linux-x64"]
                                                  {"VIS_CONTAINER_CLI" (.getAbsolutePath
                                                                         (io/file mac "engine"))
                                                   "VIS_EMULATION_MAX_SECONDS" "-1"})]
                          (expect (not= 0 exit) output)
                          (expect (str/includes? output "qemu-user") output)
                          (expect (str/includes? output "native-release.yml") output)
                          (expect (not (str/includes? output "building linux-x64")) output))
                        (finally (delete-tree! mac))))))

(defdescribe
  release-native-builder-machine-test
  (it "prefers a podman machine big enough for the builder over a small default"
      (let [mac
            (fake-tools! "Darwin" "arm64")

            podman
            (io/file mac "podman")]

        ;; Two machines, as on a real workstation: the everyday default with 2
        ;; GiB (native-image OOMs there) and a dedicated 24 GiB builder.
        (write-executable!
          podman
          (str "#!/usr/bin/env bash\n" "conn=\"\"\n"
               "if [ \"${1:-}\" = \"--connection\" ]; then conn=\"$2\"; shift 2; fi\n"
               "case \"${1:-}\" in\n"
               "  --version) printf 'podman version 6.0.0\\n' ;;\n"
               "  info)      if [ \"$conn\" = vis-builder ]; then printf '25769803776\\n';"
               " else printf '2147483648\\n'; fi ;;\n"
               "  system)    printf 'podman-machine-default\\nvis-builder\\n' ;;\n"
               "  *)         exit 0 ;;\n" "esac\n"))
        (try (let [{:keys [exit output]} (run-release-native mac
                                                             ["--targets" "linux-x64"]
                                                             {"VIS_CONTAINER_CLI" (.getAbsolutePath
                                                                                    podman)
                                                              "VIS_EMULATION_MAX_SECONDS" "-1"})]
               ;; No VIS_CONTAINER_CONNECTION: the builder machine is found and used
               ;; on its own, and the 2 GiB default never decides the run.
               (expect (str/includes? output "vis-builder") output)
               (expect (not (str/includes? output "only has 2 GB")) output)
               ;; Emulation speed still gates the build itself.
               (expect (not= 0 exit) output))
             (finally (delete-tree! mac))))))

(defdescribe
  release-native-podman-export-test
  (it
    "copies the bundle out of a container, because podman-remote rejects --output"
    (let [mac
          (fake-tools! "Darwin" "arm64")

          log
          (io/file mac "engine.log")

          podman
          (io/file mac "podman")]

      ;; `podman build -o type=local,dest=…` is a LOCAL-only flag: a Mac client
      ;; drives its Linux VM in REMOTE mode and refuses it AFTER the whole build
      ;; has run. The export therefore goes through create + cp, and the build
      ;; args this host overrides (heap) must reach the builder.
      (write-executable!
        podman
        (str "#!/usr/bin/env bash\n"
             "printf '%s\\n' \"$*\" >> \"" (.getAbsolutePath log)
             "\"\n" "if [ \"${1:-}\" = \"--connection\" ]; then shift 2; fi\n"
             "case \"${1:-}\" in\n" "  --version) printf 'podman version 6.0.0\\n' ;;\n"
             "  info)      printf '34359738368\\n' ;;\n" "  create)    printf 'ctr123\\n' ;;\n"
             "  *)         exit 0 ;;\n" "esac\n"))
      (try (let [{:keys [exit output]}
                 (run-release-native mac
                                     ["--targets" "linux-arm64"]
                                     {"VIS_CONTAINER_CLI" (.getAbsolutePath podman)
                                      "VIS_NATIVE_EXTRA_ARGS" "-J-Xmx7g"})

                 logged
                 (slurp log)]

             ;; The fake engine copies nothing, so staging rejects the bundle —
             ;; the run must still have driven the export the documented way.
             (expect (not= 0 exit) output)
             (expect (str/includes? logged "--target native-export") logged)
             (expect (not (str/includes? logged "type=local")) logged)
             (expect (str/includes? logged "--build-arg VIS_NATIVE_EXTRA_ARGS=-J-Xmx7g") logged)
             (expect (str/includes? logged "create --platform linux/arm64") logged)
             (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent" "vis-agent-python"]]
               (expect (str/includes? logged (str "cp ctr123:/" entry " ")) logged)))
           (finally (delete-tree! mac))))))

;; Regression: container-built assets reported `vis-agent <git-sha>`, then
;; `vis-agent <VIS_VERSION>+<git-sha>`, so a deployed gateway never simply said
;; which VIS_VERSION it was running. VIS_VERSION is the only version there is.
(defdescribe
  version-stamp-test
  (it
    "stamps the repo-root VIS_VERSION into `vis/VERSION`, verbatim"
    (let [dockerfile
          (slurp "Dockerfile")

          build-clj
          (slurp "build.clj")

          compose
          (slurp "docker-compose.yml")

          release-native
          (slurp "bin/release-native")

          declared
          (str/trim (slurp "VIS_VERSION"))]

      (expect (re-matches #"\d+\.\d+\.\d+" declared) declared)
      ;; VIS_VERSION is the ONLY version source: build.clj's `version` IS that
      ;; file and the native build spits it unchanged.
      (expect (str/includes? build-clj "(str/trim (slurp \"VIS_VERSION\"))") build-clj)
      (expect (str/includes? build-clj "(spit vfile version)") build-clj)
      ;; no second version source may creep back in through a build arg, an env
      ;; override or a snapshot suffix
      (doseq [[what source] {"build.clj" build-clj
                             "Dockerfile" dockerfile
                             "docker-compose.yml" compose
                             "bin/release-native" release-native}]
        (expect (not (str/includes? source "VIS_BUILD_SHA")) what)
        (expect (not (str/includes? source "-SNAPSHOT")) what)
        (expect (not (str/includes? source "rev-parse --short HEAD")) what))
      ;; the native stage refuses to ship an image whose --version is anything
      ;; other than the declared VIS_VERSION
      (expect (str/includes? dockerfile "= \"vis-agent$(tr -d '[:space:]' < VIS_VERSION)\"")
              dockerfile)
      ;; the runtime image installs that very binary, so it needs no second
      ;; stamping step of its own
      (expect (str/includes? dockerfile "COPY --from=native-export") dockerfile)
      (expect (not (str/includes? dockerfile "/opt/vis/src/resources/vis/VERSION")) dockerfile)))
  (it "reports the stamped string verbatim from the classpath resource"
      (let [dir
            (.toFile (Files/createTempDirectory "vis-version" (make-array FileAttribute 0)))

            stamped
            (str/trim (slurp "VIS_VERSION"))

            thread
            (Thread/currentThread)

            prior
            (.getContextClassLoader thread)]

        (spit (doto (io/file dir "vis" "VERSION") io/make-parents) (str stamped "\n"))
        (try (.setContextClassLoader
               thread
               (URLClassLoader. (into-array URL [(.toURL (.toURI ^java.io.File dir))]) prior))
             ;; what `/healthz` and `/v1/capabilities` advertise
             (expect (= stamped (protocol/release-version)))
             (finally (.setContextClassLoader thread prior) (delete-tree! dir))))))

(defdescribe
  release-native-engine-fallback-test
  (it "steps over an installed engine whose VM is down instead of ending the run"
      (let [mac (fake-tools! "Darwin" "arm64")]
        ;; Exactly this workstation: Docker Desktop installed but NOT running,
        ;; while the podman builder machine is up. `docker` resolves first on
        ;; PATH, so a run that trusts PATH alone dies with nothing built.
        (write-executable!
          (io/file mac "docker")
          (str "#!/usr/bin/env bash\n"
               "case \"${1:-}\" in\n" "  --version) printf 'Docker version 28.0.0\\n' ;;\n"
               "  *)         printf 'Cannot connect to the Docker daemon\\n' >&2; exit 1 ;;\n"
               "esac\n"))
        (write-executable! (io/file mac "podman")
                           (str "#!/usr/bin/env bash\n" "case \"${1:-}\" in\n"
                                "  --version) printf 'podman version 6.0.0\\n' ;;\n"
                                "  info)      printf '25769803776\\n' ;;\n"
                                "  *)         exit 0 ;;\n" "esac\n"))
        (try (let [{:keys [exit output]} (run-release-native mac
                                                             ["--targets" "linux-x64"]
                                                             {"VIS_EMULATION_MAX_SECONDS" "-1"})]
               (expect (str/includes? output "using podman instead") output)
               ;; It got PAST engine resolution: the run now fails on the
               ;; emulation guard, not on a dead docker daemon.
               (expect (str/includes? output "qemu-user") output)
               (expect (not= 0 exit) output))
             (finally (delete-tree! mac))))))

;; Update defaults are independent of the last installed track.
(defdescribe
  distribution-track-test
  (it "replaces an old dev-rejecting launcher through the published bootstrap"
      (with-source-update-fixture {:installer? true}
                                  (fn [{:keys [exit output new-commit managed-src launcher]}]
                                    (expect (zero? exit) output)
                                    (expect (= new-commit (git! managed-src "rev-parse" "HEAD")))
                                    (expect (= (slurp (io/file managed-src "bin/vis-agent"))
                                               (slurp launcher))))))
  (it "installs a complete immutable beta through either entry point without Git or JVM"
      (doseq [installer? [false true]]
        (with-native-install-fixture
          {:track "beta" :previous-track "dev" :installer? installer?}
          (fn [{:keys [exit output launcher env urls bin]}]
            (expect (zero? exit) output)
            (expect (str/includes? urls "/releases/download/installer/native-beta") urls)
            (expect (not (str/includes? urls "releases?per_page=")) urls)
            (expect (str/includes? urls (str "/releases/tags/beta-" (apply str (repeat 40 "a"))))
                    urls)
            (expect (= "beta\n" (slurp (io/file (get env "VIS_HOME") "install" "track"))))
            (let [engine (run-bash ["bash" (.getAbsolutePath launcher) "--version"] env)
                  tui (run-bash ["bash" (.getAbsolutePath launcher) "tui" "--version"] env)]

              (expect (zero? (:exit engine)) (:output engine))
              (expect (str/includes? (:output engine) "new-runtime") (:output engine))
              (expect (str/includes? (:output tui) "native-tui") (:output tui)))
            (expect (.isDirectory (io/file bin "vis-agent-python/python")))))))
  (it "defaults every plain update to release, including after dev or beta"
      (doseq [previous ["beta" "dev"]]
        (with-native-install-fixture
          {}
          (fn [{:keys [launcher env]}]
            (let [track-file (io/file (get env "VIS_HOME") "install" "track")]
              (io/make-parents track-file)
              (spit track-file (str previous "\n"))
              (let [{:keys [exit output]}
                    (run-bash ["bash" (.getAbsolutePath launcher) "update" "--keep-gateway"] env)]
                (expect (zero? exit) output)
                (expect (str/includes? output "/releases/latest") output)
                (expect (= "release\n" (slurp track-file)))))))))
  (it "rejects retired selectors and version pins outside release without changing selection"
      (with-native-install-fixture
        {}
        (fn [{:keys [launcher env]}]
          (let [track-file (io/file (get env "VIS_HOME") "install" "track")]
            (doseq [args [["--track" "stable"] ["--track" "nightly"] ["--track" "dry-run"]
                          ["--track="] ["--track"] ["--rebuild"] ["--jvm"]
                          ["--track" "beta" "v1.2.3"] ["--track" "dev" "v1.2.3"]]]
              (let [{:keys [exit output]} (run-bash (into ["bash" (.getAbsolutePath launcher)
                                                           "update" "--keep-gateway"]
                                                          args)
                                                    env)]
                (expect (not (zero? exit)) output)
                (expect (= "release\n" (slurp track-file)) output)))))))
  (it "does not change the installed selection after an unavailable beta"
      (with-native-install-fixture {}
                                   (fn [{:keys [launcher env]}]
                                     (let [track-file
                                           (io/file (get env "VIS_HOME") "install" "track")

                                           {:keys [exit output]}
                                           (run-bash ["bash" (.getAbsolutePath launcher) "update"
                                                      "--track" "beta" "--keep-gateway"]
                                                     env)]

                                       (expect (not (zero? exit)) output)
                                       (expect (= "release\n" (slurp track-file)) output)))))
  (it "always launches dev on JVM and never falls back from a native track to source"
      (let [body (re-find #"(?ms)^runtime_effective\(\) \{.*?^\}\n" (slurp "bin/vis-agent"))]
        (doseq [[track expected] [["dev" "jvm"] ["beta" "native"] ["release" "native"]]
                native-status [0 1]]

          (let [{:keys [exit output]} (run-bash
                                        ["bash" "-c"
                                         (str "read_state() { printf '%s' \"$TEST_TRACK\"; }; "
                                              "find_native() { return "
                                              native-status
                                              "; }; find_jvm_source() { return 0; }; "
                                              "launcher_is_git_owned=0; vis_track_file=unused; "
                                              body
                                              "runtime_effective")]
                                        {"TEST_TRACK" track})]
            (expect (zero? exit) output)
            (expect (= expected (str/trim output)) output)))))
  ;; The stamp is ONE space-separated line, so its track field is a closed
  ;; vocabulary or it is a parsing hazard: VIS_RELEASE_TRACK took any string at
  ;; all, and a typo — or a value with a space in it — shipped a build whose
  ;; every later field read one place to the right.
  (it
    "stamps only a track build.clj declares, in the build and in every workflow"
    (let [build-clj
          (slurp "build.clj")

          tracks
          (->> (re-find #"(?s)\(def release-tracks.*?#\{([^}]*)\}" build-clj)
               second
               (re-seq #"\"([^\"]+)\"")
               (map second)
               set)

          stamped
          (->> (file-seq (io/file ".github/workflows"))
               (filter #(str/ends-with? (.getName ^java.io.File %) ".yml"))
               (mapcat (fn [f]
                         (map (fn [v]
                                [(.getName ^java.io.File f) v])
                              (map second (re-seq #"VIS_RELEASE_TRACK:\s*(.+)" (slurp f))))))
               vec)]

      ;; Both ends of the axis, and both marks for a build nobody publishes.
      (expect (= #{"release" "beta" "dev" "dry-run"} tracks) tracks)
      ;; The stamp reads the environment THROUGH that vocabulary, and an
      ;; unlabelled build is a build of your own.
      (expect (str/includes? build-clj "(release-track)") build-clj)
      (expect (str/includes? build-clj "(contains? release-tracks track)") build-clj)
      (expect (seq stamped) "no workflow stamps a track at all")
      (doseq [[wf value] stamped]
        (doseq [t (if (str/includes? value "${{")
                    (map second (re-seq #"(?:&&|\|\|)\s*'([^']+)'" value))
                    [(str/trim value)])]
          (expect (contains? tracks t) (str wf ": " value))))))
  ;; Hosted macOS needs a heap below physical RAM; dry runs may use quick build.
  (it "sizes the low-memory hosted macOS runner without swapping"
      (let [stable
            (slurp ".github/workflows/native-release.yml")

            fallback
            (->> (str/split-lines stable)
                 (drop-while #(not (str/includes? % "-lt 16")))
                 (take 8)
                 (str/join "\n"))]

        (expect (str/includes? fallback "--parallelism=2") fallback)
        (expect (str/includes? fallback "-J-Xmx5g") fallback)
        (expect (nil? (re-find #"-J-Xmx(?:[89]|1[0-9])g" fallback)) fallback)
        ;; Quick build produces a slower binary, so it is for dry runs only.
        (expect (str/includes? fallback "-Ob") fallback)
        (expect (str/includes? fallback "steps.target.outputs.publish") fallback)
        (expect (str/includes? stable "runs-on: [self-hosted, macOS, ARM64, vis-macos-arm64]")
                stable)))
  (it
    "builds the macOS asset on self-hosted ARM64 and bounds queue waiting"
    (let [stable
          (slurp ".github/workflows/native-release.yml")

          macos-job
          (->> (str/split-lines stable)
               (drop-while #(not (str/starts-with? % "  macos:")))
               (take-while #(not (str/starts-with? % "  macos-pickup:")))
               (str/join "\n"))

          pickup
          (->> (str/split-lines stable)
               (drop-while #(not (str/includes? % "macos-pickup:")))
               (take 25)
               (str/join "\n"))]

      (expect (str/includes? macos-job "runs-on: [self-hosted, macOS, ARM64, vis-macos-arm64]")
              macos-job)
      ;; A clean macOS image does not include espeak-ng's phoneme tables;
      ;; without this dependency the built binary reaches test-native and fails.
      (expect (str/includes? macos-job "brew install espeak-ng") macos-job)
      ;; Six hours was the undersized hosted Mac's price for not finishing;
      ;; a stalled cloud build should still return capacity the same morning.
      ;; (The Linux matrix keeps its 6 h cap: a swapping analysis there is slow.)
      (expect (not (str/includes? macos-job "timeout-minutes: 350")) macos-job)
      (expect (seq pickup) "no job watches the macOS queue")
      ;; The watchdog must never wait on the runner class it is watching.
      (expect (str/includes? pickup "runs-on: ubuntu-latest") pickup)
      ;; `contents: write` at the top of that file REPLACES the default token
      ;; scopes, so reading this run's job list has to be granted explicitly.
      (expect (str/includes? pickup "actions: read") pickup)
      (expect (str/includes? pickup "DEADLINE_MINUTES") pickup)
      (expect (str/includes? stable "::error::No runner labelled") stable)))
  (it "keeps runner routing fixed rather than accepting overrides"
      (doseq [wf
              (->> (file-seq (io/file ".github/workflows"))
                   (filter #(str/ends-with? (.getName ^java.io.File %) ".yml")))

              :let [body
                    (slurp wf)

                    directives
                    (->> (str/split-lines body)
                         (remove #(str/starts-with? (str/trim %) "#"))
                         (str/join "\n"))]]

        (doseq [forbidden ["inputs.runner" "VIS_MACOS_ARM64_RUNNER" "VIS_IOS_RUNNER"]]
          (expect (not (str/includes? directives forbidden)) (str wf ": " forbidden)))))
  (it "routes trusted macOS jobs locally and all pull requests to hosted runners"
      (let [ci (slurp ".github/workflows/ci.yml")]
        (expect (str/includes? ci "github.event_name == 'push'"))
        (expect (str/includes?
                  ci
                  "fromJSON('[\"self-hosted\",\"macOS\",\"ARM64\",\"vis-macos-arm64\"]')"))
        (expect (str/includes? ci "|| 'macos-26'")))
      (doseq [wf ["mobile-release.yml" "native-release.yml" "desktop-companion.yml"]]
        (expect (str/includes? (slurp (str ".github/workflows/" wf))
                               "[self-hosted, macOS, ARM64, vis-macos-arm64]")
                wf)))
  (it "isolates Vis state and cleans up partial iOS signing setup on persistent runners"
      (let [ci
            (slurp ".github/workflows/ci.yml")

            native
            (slurp ".github/workflows/native-release.yml")

            ios
            (slurp ".github/workflows/mobile-release.yml")]

        (expect (str/includes? ci "HOME=$RUNNER_TEMP/vis-ci-home"))
        ;; CI 34206989503 prepared the runtime under a different home than classpath resolution.
        (expect (str/includes? ci "JAVA_TOOL_OPTIONS=-Duser.home=$RUNNER_TEMP/vis-ci-home"))
        (expect (str/includes? ci "JAVA_TOOL_OPTIONS: ${{ env.JAVA_TOOL_OPTIONS }}"))
        (expect (not (str/includes? ci "-J-Duser.home")))
        (expect (str/includes? native "HOME=$RUNNER_TEMP/vis-native-home"))
        (expect (str/includes? native "JAVA_TOOL_OPTIONS=-Duser.home=$RUNNER_TEMP/vis-native-home"))
        (expect (str/includes? ios
                               "- name: Restore the runner keychain state\n        if: always()\n"))
        (expect (not (str/includes? ios "steps.keychain.outcome == 'success'")))))
  ;; The tuning history in native-release.yml records runs labelled "no extra
  ;; args" that still carried build.clj's computed `-J-Xmx`/`-J-Xms` pair, so
  ;; native-image's OWN sizing has never actually been measured for this image.
  ;; One dispatch must be able to try it, on one platform, without spending two
  ;; hours of Linux builds to watch a macOS experiment.
  (it "can measure native-image's own configuration from one dispatch"
      (let [stable
            (slurp ".github/workflows/native-release.yml")

            build
            (slurp "build.clj")]

        (doseq [input ["only:" "native_args:" "builder_heap:"]]
          (expect (str/includes? stable input) input))
        ;; Either job runs alone.
        (expect (str/includes? stable "if: inputs.only != 'macos'") stable)
        (expect (str/includes? stable "if: inputs.only != 'linux'") stable)
        ;; A dispatched argument set wins over the automatic hosted fallback,
        ;; which would otherwise silently put the measured heap back.
        (expect (str/includes? stable "[ -z \"$args\" ] && [ -z \"$heap\" ] && [ \"$gib\" -lt 16 ]")
                stable)
        ;; And two experiments on one branch do not serialize behind each other.
        (expect (str/includes?
                  stable
                  "group: native-release-${{ inputs.tag || github.ref }}-${{ inputs.only }}")
                stable)
        ;; `natural` means NEITHER -J flag: an explicit ceiling is exactly what
        ;; the measurement is trying to remove.
        (expect (str/includes? build "(System/getenv \"VIS_NATIVE_BUILDER_HEAP\")") build)
        (expect (re-find #"\(not natural-heap\?\)\s+\(conj \(str \"-J-Xmx\"" build) build)))
  ;; Releases #39/#40 failed native builds; green source CI alone must not publish a beta.
  (it "automatically builds immutable native betas only after successful main CI"
      (let [beta
            (slurp ".github/workflows/beta-native.yml")

            native
            (slurp ".github/workflows/native-release.yml")]

        (doseq [contract ["workflow_run:" "workflows: [CI]" "branches: [main]" "types: [completed]"
                          "workflow_run.conclusion == 'success'" "workflow_run.event == 'push'"
                          "workflow_run.head_branch == 'main'"
                          "workflow_run.head_repository.full_name == github.repository"
                          "actions/workflows/ci.yml/runs?head_sha="
                          "uses: ./.github/workflows/native-release.yml" "needs: [pick, native]"
                          "require-draft-release" "--draft --prerelease --latest=false"
                          "--draft=false --prerelease --latest=false"]]
          (expect (str/includes? beta contract) contract))
        (expect (str/includes? beta "tag=\"beta-$sha\""))
        (expect (not (str/includes? beta "git push -f")))
        (expect (not (str/includes? beta "softprops/action-gh-release")))
        (doseq [contract ["track=release" "track=beta" "track=dry-run"
                          "VIS_RELEASE_TRACK: ${{ steps.target.outputs.track }}"
                          "clojure -M:test-native" "test-native-python-sdk"
                          "Verify draft before attaching" "gh release upload"]]
          (expect (str/includes? native contract) contract))
        (expect (not (str/includes? native "tags: ['v[0-9]*']"))))))

(defdescribe
  native-asset-upload-test
  (it
    "uploads native assets to a verified draft without rewriting release metadata"
    ;; Native beta jobs passed their checks but GitHub rejected the release metadata PATCH.
    (let [workflow
          (slurp ".github/workflows/native-release.yml")

          uploads
          (re-seq #"(?ms)^      - name: Verify draft before attaching\n(.*?)(?=^  \S|\z)" workflow)]

      (expect (= 2 (count uploads)))
      (expect (not (str/includes? workflow "softprops/action-gh-release")))
      (doseq [[_ steps] uploads]
        (expect (= 2 (count (re-seq #"if: steps.target.outputs.publish == 'true'" steps))))
        (doseq [contract ["uses: ./.github/actions/require-draft-release"
                          "tag: ${{ inputs.tag || github.ref_name }}"
                          "GH_TOKEN: ${{ github.token }}"
                          "RELEASE_TAG: ${{ inputs.tag || github.ref_name }}" "set -euo pipefail"
                          "gh release upload \"$RELEASE_TAG\" \"$VIS_ASSET\" \"$VIS_TUI_ASSET\""
                          "--repo \"$GITHUB_REPOSITORY\" --clobber"]]
          (expect (str/includes? steps contract) contract))
        (expect (< (.indexOf ^String steps "require-draft-release")
                   (.indexOf ^String steps "gh release upload")))
        (let [script (-> steps
                         (str/split #"        run: \|\n" 2)
                         second
                         (str/replace #"(?m)^          " ""))
              dir (.toFile (Files/createTempDirectory "vis-native-upload-"
                                                      (make-array FileAttribute 0)))
              calls (io/file dir "calls")]

          (try
            (write-executable!
              (io/file dir "gh")
              "#!/usr/bin/env bash\nprintf '%s\\n' \"$@\" > \"$TEST_CALLS\"\nexit \"$TEST_UPLOAD_EXIT\"\n")
            (doseq [status [0 23]]
              (let [{:keys [exit output]}
                    (run-bash ["bash" "-c" script]
                              {"PATH" (str (.getAbsolutePath dir) ":" (System/getenv "PATH"))
                               "RELEASE_TAG" "beta-fixture"
                               "VIS_ASSET" "engine archive.tar.gz"
                               "VIS_TUI_ASSET" "tui archive.tar.gz"
                               "GITHUB_REPOSITORY" "example/vis"
                               "TEST_CALLS" (.getAbsolutePath calls)
                               "TEST_UPLOAD_EXIT" (str status)})]
                (expect (= status exit) output)
                (expect (= ["release" "upload" "beta-fixture" "engine archive.tar.gz"
                            "tui archive.tar.gz" "--repo" "example/vis" "--clobber"]
                           (str/split-lines (slurp calls))))))
            (finally (delete-tree! dir))))))))

(defdescribe
  native-build-logs-test
  (it "retains engine and TUI build logs even when a build fails"
      (let [workflow (slurp ".github/workflows/native-release.yml")]
        (expect (= 2 (count (re-seq #"name: Upload native build logs" workflow))))
        (expect (= 2
                   (count (re-seq #"if: always\(\)\n        uses: actions/upload-artifact@v4"
                                  workflow))))
        (doseq [name ["engine" "tui"]]
          (expect (= 2
                     (count (filter #(str/includes? % (str "tee \"$log_dir/" name ".log\""))
                                    (str/split-lines workflow))))))))
  (it
    "streams both channels and preserves a failing compiler exit through tee"
    (let
      [workflow
       (slurp ".github/workflows/native-release.yml")

       scripts
       (re-seq
         #"(?s)        run: \|\n(          set -euo pipefail\n          log_dir=.*?tee \"\$log_dir/(?:engine|tui)\.log\")"
         workflow)]

      (expect (= 4 (count scripts)))
      (doseq [[_ body]
              scripts

              status
              [0 23]]

        (let [dir
              (.toFile (Files/createTempDirectory "vis-build-logs-" (make-array FileAttribute 0)))

              script
              (str "clojure() { echo compiler-out; echo compiler-err >&2; "
                   "if [ \"$1\" = -X:deps ]; then return 0; fi; return " status
                   "; };\n" (str/replace body #"(?m)^          " ""))]

          (try (let [{:keys [exit output]}
                     (run-bash ["bash" "-c" script]
                               {"RUNNER_TEMP" (.getAbsolutePath dir)
                                "GITHUB_RUN_ID" "123"
                                "GITHUB_RUN_ATTEMPT" "1"})

                     log
                     (io/file dir
                              "vis-native-123-1"
                              (if (str/includes? body "/engine.log") "engine.log" "tui.log"))]

                 (expect (= status exit) output)
                 (doseq [channel ["compiler-out" "compiler-err"]]
                   (expect (str/includes? output channel) output)
                   (expect (and (.isFile log) (str/includes? (slurp log) channel)))))
               (finally (delete-tree! dir))))))))

(defdescribe
  native-pipeline-order-test
  (it "builds and stages both binaries before native tests and release attachment"
      (let [workflow
            (slurp ".github/workflows/native-release.yml")

            jobs
            (re-seq #"(?s)- name: Build and stage standalone TUI(.*?)- name: Attach to release"
                    workflow)]

        (expect (= 2 (count jobs)))
        (doseq [[_ steps] jobs]
          (expect (str/includes? steps "run: clojure -M:test-native") steps)
          (expect (str/includes? steps "VIS_TUI_NATIVE_BIN: target/tui-release-bundle/vis-tui")
                  steps)
          (expect (str/includes? steps "uses: ./.github/actions/test-native-python-sdk") steps)))))

(defn- workflow-job-script
  "Read the first production inline shell of one workflow job for isolated execution."
  [workflow job]
  (->> (str/split-lines (slurp workflow))
       (drop-while #(not= % (str "  " job ":")))
       (drop-while #(not= % "        run: |"))
       rest
       (take-while #(or (str/blank? %) (str/starts-with? % "          ")))
       (map #(if (str/blank? %) "" (subs % 10)))
       (str/join "\n")))

(defdescribe
  installer-bootstrap-gate-test
  ;; The public bootstrap still rejected dev after main already supported it.
  (it
    "selects only the latest green main commit without waiting for a stable release"
    (let [script
          (workflow-job-script ".github/workflows/installer-assets.yml" "publish")

          sha
          (apply str (repeat 40 "a"))]

      (expect (not (str/blank? script)))
      (doseq [[overrides expected-exit publish?]
              [[{} 0 true] [{"EVENT_SHA" ""} 0 true] [{"TEST_GREEN_MAIN" ""} 0 false]
               [{"TEST_GREEN_MAIN" (apply str (repeat 40 "b"))} 0 false]
               [{"EVENT_SHA" "main"} 1 false] [{"TEST_GREEN_MAIN" "main" "EVENT_SHA" ""} 1 false]
               [{"TEST_API_EXIT" "22"} 22 false]]]
        (let [dir (.toFile (Files/createTempDirectory "vis-installer-gate-"
                                                      (make-array FileAttribute 0)))
              outputs (io/file dir "outputs")]

          (try
            (spit outputs "")
            (let
              [{:keys [exit output]}
               (run-bash
                 ["bash" "-c"
                  (str
                    "gh() {\ncase \"$*\" in\n"
                    " *'/actions/workflows/ci.yml/runs?branch=main&event=push&status=success&per_page=1'*)\n"
                    "  [[ \"$TEST_API_EXIT\" = 0 ]] || return \"$TEST_API_EXIT\"\n"
                    "  printf '%s' \"$TEST_GREEN_MAIN\" ;;\n"
                    " *) echo 'unexpected GitHub request' >&2; return 77 ;;\nesac\n}\n" script)]
                 (merge {"GITHUB_REPOSITORY" "example/vis"
                         "EVENT_SHA" sha
                         "TEST_GREEN_MAIN" sha
                         "TEST_API_EXIT" "0"
                         "GITHUB_OUTPUT" (.getAbsolutePath outputs)}
                        overrides))]
              (expect (= expected-exit exit) output)
              (expect (= (if publish? (str "sha=" sha "\n") "") (slurp outputs)) output))
            (finally (delete-tree! dir))))))))

(defn- run-beta-job
  "Execute a workflow job against a fake GitHub CLI; no live requests or publication."
  [job overrides metadata]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-beta-gate-" (make-array FileAttribute 0)))

        outputs
        (io/file dir "outputs")

        calls
        (io/file dir "calls")

        data
        (io/file dir "release.json")

        sha
        (apply str (repeat 40 "a"))

        script
        (workflow-job-script ".github/workflows/beta-native.yml" job)]

    (try
      (expect (not (str/blank? script)))
      (spit outputs "")
      (spit calls "")
      (spit data (wire/json-str metadata))
      (let
        [result
         (run-bash
           ["bash" "-c"
            (str
              "gh() {\ncase \"$*\" in\n"
              " *'/commits/main'*) printf '%s' \"$TEST_MAIN\" ;;\n"
              " *'/actions/workflows/ci.yml/runs?branch=main&event=push&status=success&per_page=1'*) printf '%s' \"$TEST_GREEN_MAIN\" ;;\n"
              " *'/actions/workflows/ci.yml/runs?head_sha='*) printf '%s' \"$TEST_GREEN\" ;;\n"
              " *'/releases?per_page=100'*) printf '%s' \"$TEST_DRAFT\" ;;\n"
              " *'/releases/tags/beta-'*) printf '%s' \"$TEST_PUBLISHED\" ;;\n"
              " *'/git/ref/tags/beta-'*) [ \"$TEST_TAG_EXISTS\" = 1 ] || [ -f \"$TEST_TAG_CREATED\" ] ;;\n"
              " 'api --method POST '*'/git/refs '*) printf '%s\\n' \"$*\" >> \"$TEST_CALLS\"; : > \"$TEST_TAG_CREATED\" ;;\n"
              " *'/commits/beta-'*) [ \"$TEST_TAG_EXISTS\" = 1 ] || [ -f \"$TEST_TAG_CREATED\" ] || return 1; printf '%s' \"$TEST_TAG_SHA\" ;;\n"
              " 'release create '*|'release edit '*) printf '%s\\n' \"$*\" >> \"$TEST_CALLS\" ;;\n"
              " 'release upload installer '*) printf '%s\\n' \"$*\" >> \"$TEST_CALLS\"; cat \"$RUNNER_TEMP/native-beta\" >> \"$TEST_CALLS\" ;;\n"
              " *) echo 'unexpected GitHub request' >&2; return 77 ;;\nesac\n}\n" script)]
           (merge {"REPO" "example/vis"
                   "EVENT_SHA" sha
                   "SHA" sha
                   "TAG" (str "beta-" sha)
                   "TEST_MAIN" sha
                   "TEST_GREEN_MAIN" sha
                   "TEST_TAG_SHA" sha
                   "TEST_TAG_EXISTS" "1"
                   "TEST_TAG_CREATED" (.getAbsolutePath (io/file dir "created-tag"))
                   "TEST_GREEN" "1"
                   "TEST_DRAFT" ""
                   "TEST_PUBLISHED" "true"
                   "GITHUB_OUTPUT" (.getAbsolutePath outputs)
                   "TEST_CALLS" (.getAbsolutePath calls)
                   "METADATA" (.getAbsolutePath data)
                   "RUNNER_TEMP" (.getAbsolutePath dir)}
                  overrides))]
        (assoc result
          :outputs (slurp outputs)
          :calls (slurp calls)))
      (finally (delete-tree! dir)))))

(defdescribe
  automatic-beta-gate-test
  (it "uses the latest green main commit even when main advances before its CI passes"
      (doseq [[overrides build? create?]
              [[{} true true] [{"EVENT_SHA" ""} true true] [{"TEST_GREEN" "0"} false false]
               [{"TEST_MAIN" (apply str (repeat 40 "b"))} true true]
               [{"TEST_GREEN_MAIN" (apply str (repeat 40 "b"))} false false]
               [{"TEST_GREEN_MAIN" ""} false false] [{"TEST_DRAFT" "false"} false false]
               [{"TEST_DRAFT" "true"} true false]]]
        (let [{:keys [exit output outputs calls]} (run-beta-job "pick" overrides {})]
          (expect (zero? exit) output)
          (expect (str/includes? outputs (str "build=" build?)) outputs)
          (expect (= create? (str/includes? calls "release create")) calls)
          (expect (not (str/includes? calls "release edit")) calls))))
  (it "creates a missing git tag before a draft is used for native checkout"
      ;; A draft release alone does not provide a checkoutable Git ref.
      (doseq [draft ["" "true"]]
        (let [{:keys [exit output outputs calls]}
              (run-beta-job "pick" {"TEST_TAG_EXISTS" "0" "TEST_DRAFT" draft} {})]
          (expect (zero? exit) output)
          (expect (str/includes? outputs "build=true") outputs)
          (expect (str/includes? calls "/git/refs -f ref=refs/tags/beta-") calls))))
  (it "refuses invalid commit identities and a tag pointing at different source"
      (doseq [overrides [{"EVENT_SHA" "main"}
                         {"TEST_DRAFT" "true" "TEST_TAG_SHA" (apply str (repeat 40 "b"))}]]
        (let [{:keys [exit output calls]} (run-beta-job "pick" overrides {})]
          (expect (not (zero? exit)) output)
          (expect (empty? calls) calls))))
  (it "publishes a complete immutable draft until a newer green main commit supersedes it"
      (let [sha
            (apply str (repeat 40 "a"))

            assets
            (vec (for [name
                       ["vis-agent" "vis-tui"]

                       platform
                       ["linux-x64" "linux-arm64" "macos-arm64"]]

                   {:name (str name "-" platform ".tar.gz") :size 123 :state "uploaded"}))

            metadata
            {:tag_name (str "beta-" sha) :draft true :prerelease true :assets assets}]

        (doseq [[overrides changes exit-ok? publish?]
                [[{} {} true true] [{"TEST_MAIN" (apply str (repeat 40 "b"))} {} true true]
                 [{"TEST_GREEN_MAIN" (apply str (repeat 40 "b"))} {} true false]
                 [{} {:assets (pop assets)} false false]
                 [{} {:assets (assoc-in assets [0 :size] 0)} false false]
                 [{} {:assets (assoc-in assets [0 :state] "new")} false false]
                 [{} {:assets (assoc assets 0 (last assets))} false false]
                 [{} {:draft false} false false] [{} {:prerelease false} false false]
                 [{} {:tag_name "v9.9.9"} false false]]]
          (let [{:keys [exit output calls]}
                (run-beta-job "publish" overrides (merge metadata changes))]
            (expect (= exit-ok? (zero? exit)) output)
            (expect (= publish? (str/includes? calls "--draft=false --prerelease --latest=false"))
                    calls))))))

(defdescribe
  beta-index-test
  (it "updates the installer selection only for the latest green published beta"
      (doseq [[overrides pass? update?]
              [[{} true true] [{"TEST_GREEN_MAIN" (apply str (repeat 40 "b"))} true false]
               [{"TEST_PUBLISHED" "false"} false false] [{"SHA" "main"} false false]
               [{"TAG" "v9.9.9"} false false]]]
        (let [{:keys [exit output calls]} (run-beta-job "index" overrides {})]
          (expect (= pass? (zero? exit)) output)
          (expect (= update? (str/includes? calls "release upload installer")) calls)
          (when update?
            (expect (str/includes? calls (str "beta-" (apply str (repeat 40 "a")))) calls)))))
  (it
    "rejects missing or malformed installer beta selections"
    (let [body (re-find #"(?ms)^vis_beta_tag\(\) \{.*?^\}\n" (slurp "bin/vis-agent"))]
      (doseq [[value curl-exit valid?] [[(str "beta-" (apply str (repeat 40 "a"))) "0" true]
                                        ["" "0" false] ["v1.2.3" "0" false]
                                        [(str "beta-" (apply str (repeat 39 "a"))) "0" false]
                                        [(str "beta-" (apply str (repeat 40 "a")) "\nbeta-other")
                                         "0" false] ["" "22" false]]]
        (let
          [{:keys [exit output]}
           (run-bash
             ["bash" "-c"
              (str
                "die() { echo \"$*\" >&2; exit 1; }\n"
                "curl() { case $* in */releases/download/installer/native-beta) printf '%s' \"$TEST_BETA_TAG\"; return \"$TEST_CURL_EXIT\" ;; *) echo '[]' ;; esac; }\n"
                body
                "vis_beta_tag")]
             {"TEST_BETA_TAG" value "TEST_CURL_EXIT" curl-exit})]
          (expect (= valid? (zero? exit)) output)
          (when valid? (expect (= value (str/trim output)))))))))

;; Regression: a source run from any directory other than the checkout
;; died with "Could not locate com/blockether/vis/core". tools.deps caches the
;; project roots RELATIVELY and the JVM resolves them against `user.dir`, which
;; the launcher points at the invocation directory on purpose.
(defdescribe
  source-runtime-classpath-test
  (it "anchors every relative classpath root to the source checkout before -Scp"
      (let [launcher
            (slurp "bin/vis-agent")

            fn-body
            (re-find #"(?s)absolute_project_classpath\(\) \{.*?\n\}\n" launcher)

            dir
            (.toFile (Files/createTempDirectory "vis-classpath" (make-array FileAttribute 0)))]

        (expect (some? fn-body) "launcher defines absolute_project_classpath")
        (try (write-executable!
               (io/file dir "clojure")
               "#!/usr/bin/env bash\nprintf '%s' 'src:resources:/opt/m2/lib.jar'\n")
             (let [{:keys [exit output]}
                   (run-bash ["bash" "-c"
                              (str fn-body "absolute_project_classpath /checkout -Spath -M:vis")]
                             {"PATH" (str (.getPath dir) ":" (System/getenv "PATH"))})]
               (expect (zero? exit) output)
               (expect (= "/checkout/src:/checkout/resources:/opt/m2/lib.jar" output) output))
             (expect (str/includes? launcher "-Scp \"$vis_classpath\" -M:vis")
                     "launch line hands -Scp over")
             (finally (delete-tree! dir))))))

(defdescribe
  source-runtime-python-extensions-only-test
  (it
    "never injects old user JVM libraries into the engine classpath"
    (let [dir
          (fake-tools! "Linux" "x86_64")

          home
          (doto (io/file dir "home") .mkdirs)

          vis-home
          (io/file home ".vis")

          retired-deps
          (io/file vis-home "vis-extensions/retired/deps.edn")]

      (try
        (io/make-parents retired-deps)
        (spit retired-deps "{:deps {example/retired {:mvn/version \"0.0.0\"}}}")
        (write-executable!
          (io/file dir "clojure")
          (str
            "#!/usr/bin/env bash\n"
            "for arg in \"$@\"; do\n"
            "  [[ \"$arg\" != -Sdeps ]] || { echo 'unexpected user JVM dependencies'; exit 7; }\n"
            "  if [[ \"$arg\" == -Spath ]]; then printf src:resources; exit 0; fi\n"
            "done\nprintf '<%s>' \"$@\"\n"))
        (let [{:keys [exit output]}
              (run-bash ["bash" "bin/vis-agent" "python" "-c" "pass"]
                        {"HOME" (.getAbsolutePath home)
                         "VIS_HOME" (.getAbsolutePath vis-home)
                         "VIS_NO_AUTO_INSTALL" "1"
                         "PATH" (str (.getAbsolutePath dir) ":" (System/getenv "PATH"))})]
          (expect (zero? exit) output)
          (expect (str/includes? output "<-M:vis><python><-c><pass>") output))
        (finally (delete-tree! dir))))))

;; Regression: the public `vis-agent tui` command fell through to the one-shot
;; prompt shortcut, so asking for the terminal client sent "tui" to a model.
(defdescribe
  tui-launcher-dispatch-test
  (it "routes tui to the terminal app before the agent prompt dispatcher"
      (let [root
            (.toFile (Files/createTempDirectory "vis-tui-launcher-test-"
                                                (make-array FileAttribute 0)))

            home
            (doto (io/file root "home") .mkdirs)

            path-dir
            (doto (io/file root "path") .mkdirs)

            clojure
            (io/file path-dir "clojure")]

        (try (write-executable! clojure
                                (str "#!/usr/bin/env bash\n" "for arg in \"$@\"; do\n"
                                     "  if [[ \"$arg\" == -Spath ]]; then\n"
                                     "    printf '%s' 'src:resources:/opt/lib.jar'\n"
                                     "    exit 0\n" "  fi\n"
                                     "done\n" "printf '<%s>' \"$@\"\n"))
             (let [{:keys [exit output]}
                   (run-bash ["bash" "bin/vis-agent" "tui" "--help"]
                             {"HOME" (.getAbsolutePath home)
                              "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))
                              "VIS_NO_AUTO_INSTALL" "1"
                              "PATH" (str (.getAbsolutePath path-dir) ":" (System/getenv "PATH"))})]
               (expect (zero? exit) output)
               (expect (str/includes? output "<-J--enable-native-access=ALL-UNNAMED>") output)
               (expect (str/includes? output "<-M:run>") output)
               (expect (not (str/includes? output "<-M:vis>")) output)
               (expect (str/includes? output "<--help>") output))
             (finally (delete-tree! root)))))
  (it
    "launches the JVM terminal through the JVM engine, preserving the caller directory"
    (let [root
          (.toFile (Files/createTempDirectory "vis-jvm-tui-lease-" (make-array FileAttribute 0)))

          home
          (doto (io/file root "home") .mkdirs)

          path-dir
          (doto (io/file root "path") .mkdirs)]

      (try
        (write-executable!
          (io/file path-dir "clojure")
          (str
            "#!/usr/bin/env bash\n"
            "for arg in \"$@\"; do if [[ \"$arg\" == -Spath ]]; then printf 'src:resources'; exit 0; fi; done\n"
            "printf '<%s>' \"$@\"\n"))
        (let [{:keys [exit output]}
              (run-bash ["bash" "bin/vis-agent" "tui" "--continue"]
                        {"HOME" (.getAbsolutePath home)
                         "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))
                         "VIS_NO_AUTO_INSTALL" "1"
                         "PATH" (str (.getAbsolutePath path-dir) ":" (System/getenv "PATH"))})]
          (expect (zero? exit) output)
          (expect (str/includes? output "<-M:vis><gateway><tui><-->") output)
          (expect (str/includes? output
                                 "<clojure.main><-m><com.blockether.vis.tui.main><--continue>")
                  output)
          (expect (str/includes? output "<-Duser.dir=") output)
          (expect (str/includes? output "/apps/vis-tui/src:") output))
        (finally (delete-tree! root)))))
  (it
    "launches the native terminal through the matching engine gateway lease"
    (let [root
          (.toFile (Files/createTempDirectory "vis-native-tui-launcher-test-"
                                              (make-array FileAttribute 0)))

          bin
          (doto (io/file root "bin") .mkdirs)

          home
          (doto (io/file root "home") .mkdirs)

          launcher
          (io/file bin "vis-agent")]

      (try (io/copy (io/file "bin/vis-agent") launcher)
           (.setExecutable ^java.io.File launcher true)
           (write-executable! (io/file bin "vis-agent-native")
                              "#!/usr/bin/env bash\nprintf 'engine<%s>' \"$@\"\n")
           (write-executable! (io/file bin "vis-tui") "#!/usr/bin/env bash\nprintf '<%s>' \"$@\"\n")
           (let [{:keys [exit output]} (run-bash
                                         ["bash" (.getAbsolutePath launcher) "tui" "--continue"]
                                         {"HOME" (.getAbsolutePath home)
                                          "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))})]
             (expect (zero? exit) output)
             (expect (str/includes? output "engine<gateway>engine<tui>") output)
             (expect (str/includes? output
                                    (str "engine<" (.getCanonicalPath (io/file bin "vis-tui")) ">"))
                     output)
             (expect (str/includes? output "engine<--continue>") output))
           (doseq [[args extra-env] [[["--help"] {}] [["--version"] {}]
                                     [["--gateway" "gateway.example.com" "--continue"] {}]
                                     [["--continue"]
                                      {"VIS_GATEWAY_URL" "http://gateway.example.com:7890"}]]]
             (let [{:keys [exit output]}
                   (run-bash (into ["bash" (.getAbsolutePath launcher) "tui"] args)
                             (merge {"HOME" (.getAbsolutePath home)
                                     "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))}
                                    extra-env))]
               (expect (zero? exit) output)
               (expect (not (str/includes? output "engine<")) output)
               (expect (str/includes? output (str "<" (last args) ">")) output)))
           (finally (delete-tree! root))))))

(defdescribe
  jvm-launcher-override-test
  ;; Regression: `vis-agent tui --jvm` reached the terminal as an unknown flag.
  (it
    "selects JVM for one launch without changing the installed track or child arguments"
    (let [root
          (.toFile (Files/createTempDirectory "vis-jvm-override-" (make-array FileAttribute 0)))

          home
          (doto (io/file root "home") .mkdirs)

          bin
          (doto (io/file root "bin") .mkdirs)

          install
          (doto (io/file home ".vis/install") .mkdirs)

          source
          (doto (io/file install "src/apps/vis-tui") .mkdirs)

          track-file
          (io/file install "track")

          launcher
          (io/file bin "vis-agent")

          env
          {"HOME" (.getAbsolutePath home)
           "VIS_HOME" (.getAbsolutePath (io/file home ".vis"))
           "VIS_GATEWAY_URL" ""
           "VIS_NO_AUTO_INSTALL" "1"
           "PATH" (str (.getAbsolutePath bin) ":" (System/getenv "PATH"))}]

      (try
        (io/copy (io/file "bin/vis-agent") launcher)
        (spit (io/file install "src/deps.edn") "{}")
        (spit (io/file source "deps.edn") "{}")
        (write-executable! (io/file bin "vis-agent-native")
                           "#!/usr/bin/env bash\nprintf 'native<%s>' \"$@\"\n")
        (write-executable! (io/file bin "vis-tui")
                           "#!/usr/bin/env bash\nprintf 'native-tui<%s>' \"$@\"\n")
        (write-executable!
          (io/file bin "clojure")
          (str
            "#!/usr/bin/env bash\n"
            "for arg in \"$@\"; do if [[ \"$arg\" == -Spath ]]; then printf src:resources; exit 0; fi; done\n"
            "printf '<%s>' \"$@\"\n"))
        (doseq [track
                ["release" "beta" "dev"]

                [args expected]
                [[["tui" "--jvm" "--continue"] "<-M:vis><gateway><tui><-->"]
                 [["--jvm" "tui" "--continue"] "<-M:vis><gateway><tui><-->"]
                 [["tui" "--jvm" "--gateway" "gateway.example.com" "--continue"]
                  "<-M:run><--gateway><gateway.example.com><--continue>"]
                 [["tui" "--jvm" "--help"] "<-M:run><--help>"]
                 [["--jvm" "--version"] "<-M:vis><--version>"]
                 [["gateway" "start" "--jvm" "--port" "8080"]
                  "<-M:vis><gateway><start><--port><8080>"]
                 [["gateway" "--jvm" "start" "--host" "127.0.0.1"]
                  "<-M:vis><gateway><start><--host><127.0.0.1>"]
                 [["--jvm" "gateway" "start"] "<-M:vis><gateway><start>"]
                 [["gateway" "--jvm" "--help"] "<-M:vis><gateway><--help>"]
                 [["gateway" "start" "--jvm" "--help"] "<-M:vis><gateway><start><--help>"]
                 [["gateway" "status" "--jvm" "--db" "custom db"]
                  "<-M:vis><gateway><status><--db><custom db>"]]]

          (spit track-file (str track "\n"))
          (let [{:keys [exit output]} (run-bash (into ["bash" (.getAbsolutePath launcher)] args)
                                                env)]
            (expect (zero? exit) output)
            (expect (str/includes? output expected) output)
            (expect (not (str/includes? output "<--jvm>")) output)
            (expect (not (str/includes? output "native<")) output)
            (expect (= (str track "\n") (slurp track-file)))))
        (spit track-file "release\n")
        (doseq [args [["--" "--jvm"] ["python" "uv" "run" "python" "--jvm"]]]
          (let [{:keys [exit output]} (run-bash (into ["bash" (.getAbsolutePath launcher)] args)
                                                env)]
            (expect (zero? exit) output)
            (expect (str/includes? output "native<--jvm>") output)))
        (delete-tree! (io/file install "src"))
        (doseq [args [["tui" "--jvm"] ["gateway" "start" "--jvm"]]]
          (let [{:keys [exit output]} (run-bash (into ["bash" (.getAbsolutePath launcher)] args)
                                                env)]
            (expect (not (zero? exit)) output)
            (expect (str/includes? output "vis-agent update --track dev") output)
            (expect (= "release\n" (slurp track-file)))))
        (finally (delete-tree! root))))))

;; Regression: source launches accepted old Java and coupled users to the native-build pin.
(defdescribe
  java-runtime-selection-test
  (it
    "validates the selected Java version without a vendor restriction or installer"
    (let [dir
          (.toFile (Files/createTempDirectory "vis-java-test-" (make-array FileAttribute 0)))

          java
          (io/file dir "java")

          body
          (re-find #"(?ms)^ensure_java_runtime\(\) \{.*?^\}\n" (slurp "bin/vis-agent"))]

      (try
        (doseq [[version status]
                [["24" 1] ["25" 0] ["26" 0] ["unknown" 1]]

                vendor
                ["Temurin" "GraalVM CE 25.3.4.1"]

                selector
                [:command :home :path :missing-command :missing-home]]

          (write-executable! java
                             (str "#!/bin/sh\nprintf '    java.specification.version = "
                                  version
                                  "\n    java.vendor.version = "
                                  vendor
                                  "\n' >&2\n"))
          (let [selected
                (case selector
                  :command
                  {"JAVA_CMD" (.getPath java) "JAVA_HOME" "/missing-jdk"}

                  :home
                  {"JAVA_HOME" (.getPath dir)}

                  :missing-command
                  {"JAVA_CMD" "/missing-java"}

                  :missing-home
                  {"JAVA_HOME" "/missing-jdk"}

                  {})

                ;; JAVA_HOME points to a JDK root, not its bin directory.
                _
                (do (.mkdirs (io/file dir "bin"))
                    (io/copy java (io/file dir "bin/java"))
                    (.setExecutable (io/file dir "bin/java") true))

                expected
                (if (#{:missing-command :missing-home} selector) 1 status)

                {:keys [exit output]}
                (run-bash
                  ["bash" "-c"
                   (str "warn() { :; }; "
                        body
                        "ensure_java_runtime; status=$?; printf '%s' \"$JAVA_CMD\"; exit $status")]
                  (merge {"JAVA_CMD" ""
                          "JAVA_HOME" ""
                          "PATH" (str (.getPath dir) ":" (System/getenv "PATH"))}
                         selected))]

            (expect (= expected exit) (str selector " " version " " vendor " " output))
            (when (zero? expected)
              (expect (= (.getPath (if (= :home selector) (io/file dir "bin/java") java))
                         output)))))
        (finally (delete-tree! dir))))))

;; Regression: launcher overwrote the operator's pip certificate configuration.
(defdescribe pip-certificate-selection-test
             (it "leaves pip's explicit CA selection to pip and the runtime"
                 (let [body
                       (re-find #"(?ms)^export_system_ca_bundle\(\) \{.*?^\}\n"
                                (slurp "bin/vis-agent"))

                       {:keys [exit output]}
                       (run-bash ["bash" "-c"
                                  (str body
                                       "\nexport PIP_CERT=/custom.pem\n"
                                       "export_system_ca_bundle /host.pem\n"
                                       "printf '%s' \"$PIP_CERT\"\n")]
                                 {})]

                   (expect (= 0 exit))
                   (expect (= "/custom.pem" output)))))

(defdescribe native-sdk-isolation-test
             (it "installs SDK dependencies without assuming the hosted runner Python directory"
                 (let [action (slurp ".github/actions/test-native-python-sdk/action.yml")]
                   (expect (not (str/includes? action "actions/setup-python")))
                   (expect (str/includes? action "uv venv --python 3.11"))
                   (expect (str/includes? action "uv pip install --python"))
                   (expect (str/includes? action "RUNNER_TEMP/vis-native-sdk-venv/bin/python"))
                   (expect (str/includes? action "subprocess.run([sys.executable")))))

(defdescribe
  native-linux-abi-baseline-test
  ;; The engine and TUI must not require a newer glibc than the Python worker.
  (it "builds both Linux release architectures on Ubuntu 22.04"
      (let [workflow (slurp ".github/workflows/native-release.yml")]
        (expect (str/includes? workflow "os: ubuntu-22.04, bin: target/vis"))
        (expect (str/includes? workflow "os: ubuntu-22.04-arm, bin: target/vis"))
        (expect (str/includes? workflow "bin/verify-linux-abi target/release-bundle"))
        (expect (str/includes? workflow "bin/verify-linux-abi target/tui-release-bundle"))))
  (it "uses the same ABI baseline for container-exported binaries"
      (let [dockerfile (slurp "Dockerfile")]
        (expect (str/includes? dockerfile "ARG BUILD_IMAGE=ubuntu:22.04"))
        (expect (str/includes? dockerfile "FROM ${BUILD_IMAGE} AS jdk"))
        (expect (str/includes? dockerfile
                               "bin/verify-linux-abi target/vis target/vis-agent-python")))))

(defdescribe
  native-test-tools-test
  ;; Release 34630933135 failed on both Linux runners because the test command uses bb.
  (it
    "installs Babashka explicitly before native tests on every build host"
    (let
      [workflow
       (slurp ".github/workflows/native-release.yml")

       setups
       (re-seq
         #"(?m)^      - uses: DeLaGuardo/setup-clojure@[^\n]+\n        with:\n((?:          [^\n]*\n)+)"
         workflow)]

      (expect (= 2 (count setups)))
      (doseq [[_ inputs] setups]
        (expect (str/includes? inputs "bb: latest") inputs)))))

(defdescribe
  native-linux-isolation-test
  (it "provisions and exercises user namespaces before native tests"
      (let [workflow (slurp ".github/workflows/native-release.yml")]
        (expect (str/includes? workflow "sudo apt-get install -y bubblewrap binutils"))
        (expect (str/includes? workflow
                               "sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0"))
        (expect (str/includes? workflow "bwrap --unshare-all --ro-bind / / /bin/true")))))

(defdescribe
  ci-supersession-and-npm-cache-test
  ;; A per-SHA main queue delayed releases, and a persistent npm cache uploaded 8.5 GB.
  (it
    "supersedes branch CI without cancelling tagged release verification"
    (let [ci (slurp ".github/workflows/ci.yml")]
      (expect (str/includes? ci "group: ci-${{ github.workflow }}-${{ github.ref }}"))
      (expect
        (str/includes?
          ci
          "cancel-in-progress: ${{ github.event_name == 'pull_request' || (github.event_name == 'push' && startsWith(github.ref, 'refs/heads/')) }}"))
      (expect (not (str/includes? ci "|| github.sha"))))
    (doseq [workflow ["release.yml" "mobile-release.yml" "native-release.yml"
                      "desktop-companion.yml"]]
      (expect (str/includes? (slurp (str ".github/workflows/" workflow))
                             "cancel-in-progress: false"))))
  (it "keeps npm downloads job-local and never uploads a persistent release-runner cache"
      (doseq [[workflow jobs]
              [["mobile-release.yml" 2] ["desktop-companion.yml" 1]]

              :let [body
                    (slurp (str ".github/workflows/" workflow))]]

        (expect (= jobs (count (re-seq #"npm_config_cache=\$RUNNER_TEMP/vis-npm-cache" body))))
        (expect (not (str/includes? body "cache: npm")))
        (expect (not (str/includes? body "cache-dependency-path:"))))))

(defdescribe
  npm-cache-cleanup-test
  (it
    "deletes only oversized or week-old npm caches and tolerates concurrent eviction"
    (let
      [workflow
       (slurp ".github/workflows/npm-cache-cleanup.yml")

       script
       (-> workflow
           (str/split #"          script: \|\n" 2)
           second
           (str/replace #"(?m)^            " ""))

       fixture
       "const assert = require('node:assert/strict');\nconst now = Date.now();\nconst recent = new Date(now).toISOString();\nconst old = new Date(now - 8 * 86400000).toISOString();\nconst cache = (id, key, created_at, size_in_bytes) => ({id, key, created_at, size_in_bytes});\nconst npm = 'node-cache-macOS-arm64-npm-lock';\nconst rows = [cache(1, npm, recent, 2 ** 31), cache(2, npm, old, 100),\n  cache(3, npm, recent, 100), cache(4, 'deps-macOS-lock', old, 2 ** 31),\n  cache(5, 'node-cache-Linux-x64-yarn-lock', old, 2 ** 31),\n  cache(6, npm, recent, 2 ** 30), cache(7, npm, 'unknown', 100),\n  cache(8, npm, old, 100)];\nconst deleted = [];\nconst context = {repo: {owner: 'example', repo: 'project'}};\nconst core = {info: () => {}};\nconst github = {\n  paginate: async (method, params) => {\n    assert.equal(method, 'list-caches');\n    assert.deepEqual(params, {...context.repo, per_page: 100});\n    return rows;\n  },\n  rest: {actions: {getActionsCacheList: 'list-caches', deleteActionsCacheById: async params => {\n    assert.deepEqual(params, {...context.repo, cache_id: params.cache_id});\n    deleted.push(params.cache_id);\n    if (params.cache_id === 8) throw Object.assign(new Error('removed concurrently'), {status: 404});\n  }}}\n};\n"

       {:keys [exit output]}
       (run-bash ["node" "-e"
                  (str fixture
                       "(async () => {\n"
                       script
                       "\n})().then(() => assert.deepEqual(deleted, [1, 2, 8]));")]
                 {})]

      (expect (str/includes? workflow "- cron: '0 5 * * 1'"))
      (expect (str/includes? workflow "workflow_dispatch:"))
      (expect (str/includes? workflow "actions: write"))
      (expect (zero? exit) output))))

(defdescribe
  ci-ubuntu-package-sources-test
  (it
    "uses only Ubuntu package sources and stops before installation if their refresh fails"
    ;; Main CI was blocked by a hash mismatch in an unrelated Chrome repository.
    (let
      [workflow
       (slurp ".github/workflows/ci.yml")

       script
       (-> (second (re-find
                     #"(?s)- name: Install bubblewrap.*?        run: \|\n((?:          [^\n]*\n)+)"
                     workflow))
           (str/replace #"(?m)^          " ""))

       options
       "-o Dir::Etc::sourcelist=/etc/apt/sources.list.d/ubuntu.sources -o Dir::Etc::sourceparts=-"

       mocks
       (str
         "sudo() {\n"
         "  printf 'sudo %s\\n' \"$*\"\n" "  if [[ \"$1\" == apt-get ]]; then\n"
         "    [[ \" $* \" == *' Dir::Etc::sourcelist=/etc/apt/sources.list.d/ubuntu.sources '* && \" $* \" == *' Dir::Etc::sourceparts=- '* ]] || return 101\n"
         "    if [[ \"${FAIL_UBUNTU_UPDATE}\" == 1 && \"$*\" == *' update' ]]; then return 42; fi\n"
         "  fi\n  return 0\n}\n" "bwrap() { printf 'bwrap %s\\n' \"$*\"; }\n")]

      (doseq [fail-update ["0" "1"]]
        (let [{:keys [exit output]} (run-bash ["bash" "-e" "-c" (str mocks script)]
                                              {"FAIL_UBUNTU_UPDATE" fail-update})
              expected (cond-> [(str "sudo apt-get " options " update")]
                         (= "0" fail-update)
                         (into [(str "sudo apt-get " options " install -y bubblewrap passt")
                                "sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0"
                                "sudo sysctl -w kernel.unprivileged_userns_clone=1"
                                "bwrap --unshare-all --ro-bind / / /bin/true"]))]

          (expect (= (if (= "0" fail-update) 0 42) exit) output)
          (expect (= expected (str/split-lines output)) output))))))

(defdescribe ci-native-runtime-provisioning-test
             (it "provisions the Python library before the suite disables outbound downloads"
                 ;; A warm developer runtime hid the cold Linux worker failure.
                 (let [workflow
                       (slurp ".github/workflows/ci.yml")

                       preparation
                       (subs workflow 0 (str/index-of workflow "- name: Run test suite"))]

                   (expect (str/includes? preparation "com.blockether.vis.internal.python.runtime"))
                   (expect (str/includes? preparation "(python-runtime/ensure-library!)")))))

(defn- run-draft-release-action
  [release-json view-exit]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-draft-release-" (make-array FileAttribute 0)))

        output-file
        (io/file dir "outputs")

        script
        (-> (slurp ".github/actions/require-draft-release/action.yml")
            (str/split #"      run: \|\n" 2)
            second
            (str/replace #"(?m)^        " ""))]

    (try
      (write-executable!
        (io/file dir "gh")
        (str
          "#!/usr/bin/env python3\n"
          "import os, sys\n" "args = sys.argv[1:]\n"
          "url = 'https://api.github.com/repos/example/project/releases/42'\n"
          "if args == ['release', 'view', 'v9.8.7', '--repo', 'example/project', '--json', 'apiUrl', '--template', '{{.apiUrl}}']:\n"
          "    if os.environ['FIXTURE_VIEW_EXIT'] != '0': sys.exit(1)\n" "    print(url)\n"
          "elif args == ['api', url]:\n" "    print(os.environ['FIXTURE_RELEASE'])\n"
          "else:\n"
          "    sys.exit('Draft releases require lookup by ID, not the published-tag endpoint')\n"))
      (let [result
            (run-bash ["bash" "-c" script]
                      {"PATH" (str (.getPath dir) ":" (System/getenv "PATH"))
                       "GITHUB_REPOSITORY" "example/project"
                       "RELEASE_TAG" "v9.8.7"
                       "RUNNER_TEMP" (.getPath dir)
                       "GITHUB_OUTPUT" (.getPath output-file)
                       "FIXTURE_VIEW_EXIT" view-exit
                       "FIXTURE_RELEASE" release-json})

            metadata-path
            (when (.exists output-file)
              (second (re-find #"(?m)^metadata=(.+)$" (slurp output-file))))]

        (assoc result :metadata (when metadata-path (str/trim (slurp metadata-path)))))
      (finally (delete-tree! dir)))))

(defdescribe
  draft-release-lookup-test
  ;; The complete release stopped because GitHub's published-tag endpoint hides drafts.
  (it "resolves draft metadata by release ID and exposes it to the complete-asset gate"
      (let [metadata
            "{\"tag_name\":\"v9.8.7\",\"draft\":true,\"assets\":[]}"

            result
            (run-draft-release-action metadata 0)]

        (expect (zero? (:exit result)) (:output result))
        (expect (= metadata (:metadata result)))))
  (it "refuses published, mismatched, malformed or unavailable releases without output metadata"
      (doseq [[metadata view-exit] [["{\"tag_name\":\"v9.8.7\",\"draft\":false}" 0]
                                    ["{\"tag_name\":\"v9.8.6\",\"draft\":true}" 0]
                                    ["{\"tag_name\":\"v9.8.7\",\"draft\":\"true\"}" 0]
                                    ["invalid JSON" 0]
                                    ["{\"tag_name\":\"v9.8.7\",\"draft\":true}" 1]]]
        (let [result (run-draft-release-action metadata view-exit)]
          (expect (pos? (:exit result)) metadata)
          (expect (nil? (:metadata result)))))))

(defdescribe
  python-release-publication-test
  (it "publishes after a Vis release or a verified SDK-only main commit (#203)"
      (let [release-name
            (second (re-find #"(?m)^name: (.+)$" (slurp ".github/workflows/release.yml")))

            publisher
            (slurp ".github/workflows/python-publish.yml")]

        (doseq
          [needle
           [(str "workflow_run:\n    workflows: ['" release-name "']\n    types: [completed]")
            "github.event_name == 'workflow_run'"
            "github.event.workflow_run.conclusion == 'success'"
            "github.event.workflow_run.event == 'push'"
            "github.event.workflow_run.head_repository.full_name == github.repository"
            "github.event_name == 'workflow_dispatch'" "github.ref == 'refs/heads/main'"
            "uses: ./.github/workflows/python-packages.yml"
            "ref: ${{ inputs.release_tag || github.event.workflow_run.head_sha || github.sha }}"
            "version: ${{ inputs.version }}" "needs: verify" "environment: pypi" "id-token: write"
            "uses: pypa/gh-action-pypi-publish@release/v1"]]
          (expect (str/includes? publisher needle) needle))
        ;; PyPI trusted publishing does not support reusable workflows.
        (expect (not (str/includes? publisher "workflow_call:")))
        (expect (not (str/includes? publisher "continue-on-error:")))))
  (it
    "builds and tests the released commit and uses its VIS_VERSION for the distribution"
    (let [packages (slurp ".github/workflows/python-packages.yml")]
      (expect (str/includes? packages "      ref:\n"))
      (expect
        (=
          2
          (count
            (re-seq
              #"uses: actions/checkout@v7\n        with:\n          ref: \$\{\{ inputs.ref \|\| github.sha \}\}"
              packages))))
      (doseq
        [needle
         ["version = (Path(os.environ['GITHUB_WORKSPACE']) / 'VIS_VERSION').read_text().strip()"
          "assert metadata.version('vis-agent') == version"
          "os.environ['EXPECTED_VERSION'] == version"
          "python -m build packages/vis-agent --outdir dist" "name: python-sdk-distributions"
          "needs: distribution"]]
        (expect (str/includes? packages needle) needle)))))

(defdescribe
  python-existing-publication-test
  (it
    "only accepts an identical complete PyPI release or a confirmed missing version"
    (let [publisher
          (slurp ".github/workflows/python-publish.yml")

          source
          (some->> (re-find #"(?s)python - <<'PY'\n(.*?)          PY" publisher)
                   second
                   str/split-lines
                   (map #(str/replace-first % #"^          " ""))
                   (str/join "\n"))]

      (expect (some? source) "the workflow must check existing distribution hashes")
      (when source
        (doseq [scenario ["missing" "matching" "different" "partial" "extra" "duplicate"
                          "wrong-version" "bad-json" "wrong-shape" "unavailable" "offline"
                          "extra-local"]]
          (let
            [{:keys [exit output]}
             (run-bash
               ["python3" "-c"
                (str/join
                  "\n"
                  ["import hashlib, io, json, os, tempfile" "from pathlib import Path"
                   "from unittest.mock import patch" "from urllib.error import HTTPError, URLError"
                   "" "case = os.environ['FIXTURE_CASE']" "check = os.environ['PUBLICATION_CHECK']"
                   "with tempfile.TemporaryDirectory() as directory:" "    os.chdir(directory)"
                   "    Path('dist').mkdir()"
                   "    for name in ('vis_agent-9.8.7-py3-none-any.whl', 'vis_agent-9.8.7.tar.gz'):"
                   "        Path('dist', name).write_bytes(name.encode())"
                   "    files = [{'filename': path.name, 'digests': {'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}}"
                   "             for path in sorted(Path('dist').iterdir())]"
                   "    metadata = {'info': {'version': '9.8.7'}, 'urls': files}"
                   "    if case == 'different':" "        files[0]['digests']['sha256'] = '0' * 64"
                   "    elif case == 'partial':" "        files.pop()" "    elif case == 'extra':"
                   "        files.append({'filename': 'unexpected.whl', 'digests': {'sha256': '0' * 64}})"
                   "    elif case == 'duplicate':" "        files.append(files[0])"
                   "    elif case == 'wrong-version':"
                   "        metadata['info']['version'] = '9.8.6'" "    elif case == 'wrong-shape':"
                   "        metadata = []" "    elif case == 'extra-local':"
                   "        Path('dist/unexpected.whl').write_bytes(b'extra')"
                   "    payload = 'invalid JSON' if case == 'bad-json' else json.dumps(metadata)"
                   "    error = None" "    if case in ('missing', 'unavailable'):"
                   "        error = HTTPError('https://pypi.org', 404 if case == 'missing' else 500, 'fixture', {}, None)"
                   "    elif case == 'offline':"
                   "        error = URLError('fixture network unavailable')"
                   "    output = Path('outputs')" "    os.environ['GITHUB_OUTPUT'] = str(output)"
                   "    failure = None"
                   "    with patch('urllib.request.urlopen', return_value=io.StringIO(payload), side_effect=error) as request:"
                   "        try:"
                   "            exec(compile(check, 'python-publish.yml', 'exec'), {})"
                   "        except (Exception, SystemExit) as caught:"
                   "            failure = caught" "        if case == 'extra-local':"
                   "            request.assert_not_called()" "        else:"
                   "            request.assert_called_once_with('https://pypi.org/pypi/vis-agent/9.8.7/json', timeout=30)"
                   "    if case in ('missing', 'matching'):"
                   "        assert failure is None, repr(failure)"
                   "        expected = 'true' if case == 'matching' else 'false'"
                   "        assert output.read_text() == f'published={expected}\\n', output.read_text()"
                   "    else:"
                   "        assert failure is not None, f'{case}: unsafe publication was accepted'"
                   "        assert not output.exists(), f'{case}: failure produced a publication decision'"])]
               {"PUBLICATION_CHECK" source "FIXTURE_CASE" scenario})]
            (expect (zero? exit) (str scenario ": " output)))))))
  (it "skips uploading only after checking hashes without bypassing SDK verification"
      (let [publisher (slurp ".github/workflows/python-publish.yml")]
        (expect (str/includes? publisher "id: existing"))
        (expect (str/includes? publisher "if: steps.existing.outputs.published != 'true'"))
        (expect (not (str/includes? publisher "skip-existing:"))))))

(defdescribe
  complete-release-gate-test
  (it
    "keeps stable publication behind native, mobile, desktop and full CI verification"
    (let [release
          (slurp ".github/workflows/release.yml")

          native
          (slurp ".github/workflows/native-release.yml")

          mobile
          (slurp ".github/workflows/mobile-release.yml")]

      (doseq [needle
              ["uses: ./.github/workflows/ci.yml" "uses: ./.github/workflows/native-release.yml"
               "needs: [prepare, native, mobile, desktop, recover]" "bin/verify-release-assets.py"
               "--draft" "--draft=false --latest" "require_complete: true"]]
        (expect (str/includes? release needle) needle))
      (expect (= 3 (count (re-seq #"uses: \./\.github/actions/require-draft-release" release))))
      (expect (not (str/includes? release "/releases/tags/")))
      (expect (str/includes? release "RELEASE_METADATA: ${{ steps.release.outputs.metadata }}"))
      (expect (str/includes? native "workflow_call:"))
      (expect (not (str/includes? release "git commit")))
      (expect (str/includes? mobile "require_complete:"))
      (expect (str/includes? mobile "IFS= read -r keychain"))
      (expect (str/includes? mobile "security default-keychain -d user -s \"$keychain\""))
      (expect (str/includes? mobile "security list-keychain -d user -s \"${keychains[@]}\""))
      (expect (str/includes? mobile "printf 'path=%s\\n' \"$KEYCHAIN_PATH\" >> \"$GITHUB_OUTPUT\""))
      (expect (str/includes? mobile "VIS_IOS_SIGNING_KEYCHAIN: ${{ steps.keychain.outputs.path }}"))
      (doseq [needle ["-ios.ipa" "-android.aab"]]
        (expect (str/includes? mobile needle) needle))))
  (it
    "retains the signed iOS package after failed public distribution without passing the job"
    ;; Release 34396848655 lost its attachment after Apple's review submission limit.
    (let [mobile
          (slurp ".github/workflows/mobile-release.yml")

          attachment
          (second (re-find
                    #"(?s)- name: Attach signed iOS package to the draft\n(.*?)\n      - name:"
                    mobile))]

      (expect (=
                "${{ !cancelled() && inputs.require_complete && steps.creds.outputs.ok == 'true' }}"
                (second (re-find #"(?m)^        if: (.+)$" attachment))))
      (expect (str/includes? attachment
                             "packages=(build/ios/export-\"$VERSION\"-\"$BUILD\"/*.ipa)"))
      (expect (str/includes? attachment "test \"${#packages[@]}\" = 1"))
      (expect (str/includes? attachment "gh release upload \"$RELEASE_TAG\" \"$asset\" --clobber"))
      (expect (not (str/includes? mobile "continue-on-error:")))
      (expect
        (re-find
          #"(?m)^          npm run release:ios:store -- --audience \$\{\{ inputs.audience \|\| 'all' \}\}$"
          mobile))))
  (it "checks main alignment before slow CI without allowing unverified artifact jobs"
      (let [jobs (into {}
                       (map (fn [[_ name body]]
                              [name body])
                            (re-seq #"(?ms)^  ([\w-]+):\n(.*?)(?=^  [\w-]+:|\z)"
                                    (slurp ".github/workflows/release.yml"))))]
        ;; Concurrent main commits must not invalidate a candidate after its full CI run.
        (expect (not (str/includes? (get jobs "prepare") "needs:")))
        (expect (str/includes? (get jobs "prepare")
                               "test \"$(git rev-parse origin/main)\" = \"$(git rev-parse HEAD)\""))
        (expect (str/includes? (get jobs "prepare") "--verify-tag --draft"))
        (expect (str/includes? (get jobs "verify") "needs: prepare"))
        (doseq [job ["native" "mobile" "desktop"]]
          (expect (str/includes? (get jobs job) "needs: [prepare, verify]") job))))
  (it "delegates job-list read access to the native workflow's runner pickup check"
      (let [native-call (second (re-find #"(?s)  native:\n(.*?)\n  mobile:"
                                         (slurp ".github/workflows/release.yml")))]
        (expect (str/includes? native-call "actions: read"))
        (expect (str/includes? native-call "contents: write"))))
  (it "refreshes the bootstrap from trusted green main CI without moving tags"
      (let [workflow (slurp ".github/workflows/installer-assets.yml")]
        (doseq [contract ["workflow_run:" "workflows: [CI]" "branches: [main]" "types: [completed]"
                          "workflow_run.conclusion == 'success'" "workflow_run.event == 'push'"
                          "workflow_run.head_branch == 'main'"
                          "workflow_run.head_repository.full_name == github.repository"
                          "github.ref == 'refs/heads/main'" "actions: read"
                          "ref: ${{ steps.pick.outputs.sha }}" "persist-credentials: false"
                          ".head_repository.full_name ==" "group: installer-assets"
                          "cancel-in-progress: false" "--prerelease --latest=false"]]
          (expect (str/includes? workflow contract) contract))
        (expect (= 2 (count (re-seq #"if: steps.pick.outputs.sha != ''" workflow))))
        (doseq [obsolete ["workflow_call:" "inputs.tag" "/releases/latest" "git tag -f"]]
          (expect (not (str/includes? workflow obsolete)) obsolete))
        (expect (not (str/includes? (slurp ".github/workflows/release.yml")
                                    "uses: ./.github/workflows/installer-assets.yml")))))
  (it
    "requires every uploaded platform artifact and rejects published or mismatched releases"
    (let
      [{:keys [exit output]}
       (run-bash
         ["python3" "-c"
          (str
            "import runpy\n"
            "m = runpy.run_path('bin/verify-release-assets.py')\n" "tag = 'v9.8.7'\n"
            "names = m['required_assets'](tag)\n" "assert len(names) == 15, names\n"
            "release = {'tag_name': tag, 'draft': True, 'prerelease': False, 'assets': "
            "[{'name': n, 'size': 42, 'state': 'uploaded'} for n in names]}\n"
            "m['verify_release'](release, tag)\n"
            "m['verify_release'](dict(release, draft=False), tag, draft=False)\n"
            "bad = [dict(release, assets=release['assets'][:i] + release['assets'][i+1:]) for i in range(len(names))]\n"
            "bad += [dict(release, draft=False), dict(release, prerelease=True), dict(release, tag_name='v9.8.6')]\n"
            "bad += [dict(release, assets=[dict(a, size=0) for a in release['assets']]), "
            "dict(release, assets=[dict(a, state='starter') for a in release['assets']])]\n"
            "for candidate in bad:\n" "    try: m['verify_release'](candidate, tag)\n"
            "    except ValueError: pass\n"
            "    else: raise AssertionError('accepted incomplete or immutable release')\n")]
         {})]
      (expect (zero? exit) output))))

(defdescribe
  release-recovery-test
  (it "resumes only an existing draft through verified original and repaired jobs"
      (let [workflow (slurp ".github/workflows/release.yml")]
        (doseq [needle ["workflow_dispatch:" "source_run:" "native_run:"
                        "github.ref == 'refs/heads/main'" "--source-run" "--native-run"
                        "git merge-base --is-ancestor" "needs.recover.outputs.sha"
                        "needs.recover.result == 'success'" "needs.prepare.result == 'success'"
                        "needs.native.result == 'success'" "needs.mobile.result == 'success'"
                        "needs.desktop.result == 'success'"]]
          (expect (str/includes? workflow needle) needle))
        (expect (not (str/includes? workflow "continue-on-error:")))
        (expect (not (str/includes? workflow "git tag -f")))))
  (it
    "refuses failed, partial, foreign and different-source recovery evidence"
    (let
      [{:keys [exit output]}
       (run-bash
         ["python3" "-c"
          (str/join
            "\n"
            ["import copy, json, runpy" "from unittest.mock import patch"
             "m = runpy.run_path('bin/verify-release-assets.py')"
             "repo, tag, sha = 'example/project', 'v9.8.7', 'a' * 40"
             "names = m['required_recovery_checks']()" "assert len(names) == 30, names"
             "replaced = {f'native / native / vis-agent-linux-{arch}.tar.gz' for arch in ('x64', 'arm64')}"
             "publisher = 'Verify complete assets, deploy libraries and publish stable'"
             "base = {'repository': {'full_name': repo}, 'head_repository': {'full_name': repo}, 'status': 'completed'}"
             "source = dict(base, path='.github/workflows/release.yml', event='push', head_branch=tag, head_sha=sha, conclusion='failure', jobs=[])"
             "for name in names:"
             "    source['jobs'].append({'name': name, 'status': 'completed', 'conclusion': 'failure' if name in replaced else 'skipped' if name == publisher else 'success'})"
             "steps = [{'name': name, 'conclusion': 'success'} for name in ('Test the native binaries', 'Run ./.github/actions/test-native-python-sdk')]"
             "native = dict(base, path='.github/workflows/native-release.yml', event='workflow_dispatch', conclusion='success', jobs=[{'id': i, 'name': f'native / vis-agent-linux-{arch}.tar.gz', 'status': 'completed', 'conclusion': 'success', 'steps': steps, 'checkout_shas': [sha]} for i, arch in enumerate(('x64', 'arm64'), 1)])"
             "def check(s=source, n=native, t=tag, commit=sha):"
             "    m['verify_recovery'](repo, t, commit, s, n)" "check()" "cases = []"
             "for name in names:" "    changed = copy.deepcopy(source)"
             "    changed['jobs'] = [job for job in changed['jobs'] if job['name'] != name]"
             "    cases.append((changed, native))" "for name in names - replaced - {publisher}:"
             "    changed = copy.deepcopy(source)"
             "    next(job for job in changed['jobs'] if job['name'] == name)['conclusion'] = 'failure'"
             "    cases.append((changed, native))"
             "for target, key, value in [('source', 'head_sha', 'b' * 40), ('source', 'head_branch', 'main'), ('source', 'event', 'pull_request'), ('source', 'conclusion', 'cancelled'), ('native', 'conclusion', 'failure'), ('native', 'event', 'push')]:"
             "    s, n = copy.deepcopy(source), copy.deepcopy(native)"
             "    (s if target == 'source' else n)[key] = value" "    cases.append((s, n))"
             "for target in ('source', 'native'):"
             "    for key, value in [('repository', {'full_name': 'other/project'}), ('head_repository', {'full_name': 'other/project'}), ('path', '.github/workflows/other.yml'), ('status', 'in_progress')]:"
             "        s, n = copy.deepcopy(source), copy.deepcopy(native)"
             "        (s if target == 'source' else n)[key] = value" "        cases.append((s, n))"
             "    s, n = copy.deepcopy(source), copy.deepcopy(native)"
             "    changed = s if target == 'source' else n"
             "    changed['jobs'].append(copy.deepcopy(changed['jobs'][0]))"
             "    cases.append((s, n))" "for index in range(2):"
             "    for key, value in [('checkout_shas', []), ('checkout_shas', ['b' * 40]), ('checkout_shas', [sha, 'b' * 40]), ('conclusion', 'skipped'), ('steps', []), ('steps', [dict(step, conclusion='skipped') for step in steps])]:"
             "        changed = copy.deepcopy(native)" "        changed['jobs'][index][key] = value"
             "        cases.append((source, changed))" "    changed = copy.deepcopy(native)"
             "    del changed['jobs'][index]" "    cases.append((source, changed))"
             "for s, n in cases:" "    try: check(s, n)" "    except ValueError: pass"
             "    else: raise AssertionError('accepted unverified or mismatched recovery')"
             "for t, commit in [('main', sha), (tag, 'bad')]:" "    try: check(t=t, commit=commit)"
             "    except ValueError: pass"
             "    else: raise AssertionError('accepted invalid release identity')"
             "log = '2026-01-01T00:00:00Z [command]/usr/bin/git log -1 --format=%H\\n2026-01-01T00:00:00Z ' + sha + '\\n'"
             "run = {key: value for key, value in native.items() if key != 'jobs'}"
             "pages = [{'jobs': [dict(job)]} for job in native['jobs']]" "for page in pages:"
             "    page['jobs'][0].pop('checkout_shas')"
             "with patch('subprocess.check_output', side_effect=[json.dumps(run), json.dumps(pages), log, log]) as request:"
             "    loaded = m['load_recovery_run'](repo, 42, native=True)" "    check(n=loaded)"
             "    assert request.call_count == 4"
             "    assert '--paginate' in request.call_args_list[1].args[0]"
             "    assert '--slurp' in request.call_args_list[1].args[0]"
             "print(f'{len(cases) + 2} unsafe recovery cases refused; matching source and repaired native checks accepted')"])]
         {})]
      (expect (zero? exit) output))))

(defdescribe
  release-recovery-options-test
  (it
    "rejects zero, empty and partial recovery options before reading remote evidence"
    (let
      [{:keys [exit output]}
       (run-bash
         ["python3" "-c"
          (str/join
            "\n"
            ["import contextlib, io, json, runpy" "from unittest.mock import patch"
             "m = runpy.run_path('bin/verify-release-assets.py')" "tag = 'v9.8.7'"
             "release = {'tag_name': tag, 'draft': True, 'prerelease': False, 'assets': [{'name': name, 'size': 42, 'state': 'uploaded'} for name in m['required_assets'](tag)]}"
             "cases = [['--source-run', '0'], ['--native-run', '0'], ['--sha', ''], ['--source-run', '-1', '--native-run', '2', '--sha', 'a' * 40], ['--source-run', '1', '--native-run', '2', '--sha', 'a' * 40, '--published']]"
             "for flags in cases:"
             "    with patch('sys.argv', ['verify-release-assets.py', 'fixture.json', tag, *flags]), patch('pathlib.Path.open', return_value=io.StringIO(json.dumps(release))), patch('subprocess.check_output') as request, contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):"
             "        try: m['main']()" "        except SystemExit as error: assert error.code != 0"
             "        else: raise AssertionError(f'ignored explicit recovery options: {flags}')"
             "        request.assert_not_called()"])]
         {})]
      (expect (zero? exit) output))))
