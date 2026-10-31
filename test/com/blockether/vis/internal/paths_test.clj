(ns com.blockether.vis.internal.paths-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import (java.io File)
           (java.nio.file Files FileVisitOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant)
           (java.util TimeZone)))

(defdescribe unixify-test
             (it "normalizes Windows separators and leaves POSIX paths alone"
                 (expect (= "a/b/c" (paths/unixify "a\\b\\c")))
                 (expect (= "a/b" (paths/unixify "a/b")))
                 (expect (= "C:/x/y" (paths/unixify "C:\\x\\y")))))

(defdescribe
  expand-home-test
  (it "expands a bare `~` and a `~/…` / `~\\…` prefix"
      (expect (= "/home/u" (paths/expand-home "~" "/home/u")))
      (expect (= (.getPath (java.io.File. "/home/u" "x/y")) (paths/expand-home "~/x/y" "/home/u")))
      (expect (= (.getPath (java.io.File. "/home/u" "x")) (paths/expand-home "~\\x" "/home/u"))))
  (it "leaves `~user`, mid-path tildes and ordinary paths untouched"
      (expect (= "~other/x" (paths/expand-home "~other/x" "/home/u")))
      (expect (= "/a/~/b" (paths/expand-home "/a/~/b" "/home/u")))
      (expect (= "/abs" (paths/expand-home "/abs" "/home/u")))
      (expect (= "rel/x" (paths/expand-home "rel/x" "/home/u"))))
  (it "is nil-safe and a no-op when home is unavailable"
      (expect (nil? (paths/expand-home nil "/home/u")))
      (expect (= "~/x" (paths/expand-home "~/x" nil)))
      (expect (= "~/x" (paths/expand-home "~/x" "")))))

(defdescribe ancestor-chain-test
             (it "lists a File and each parent, nearest first, up to the root"
                 (expect (= ["/a/b/c" "/a/b" "/a" "/"]
                            (map #(.getPath ^File %) (paths/ancestor-chain (File. "/a/b/c"))))))
             (it "keeps the Path type"
                 (let [chain (paths/ancestor-chain (.toPath (File. "/a/b")))]
                   (expect (every? #(instance? Path %) chain))
                   (expect (= ["/a/b" "/a" "/"] (map str chain)))))
             (it "gives an empty chain for nil" (expect (empty? (paths/ancestor-chain nil)))))

(defdescribe nearest-ancestor-test
             (it "answers the nearest match, or nil when nothing matches"
                 (let [named (fn [name]
                               #(= name (.getName ^File %)))]
                   (expect (= "/b/x/b"
                              (.getPath ^File
                                        (paths/nearest-ancestor (named "b") (File. "/b/x/b/y")))))
                   (expect (nil? (paths/nearest-ancestor (named "z") (File. "/b/x")))))))

(defdescribe abbreviate-home-test
             (it "renders home itself as `~/` and descendants with `/` separators"
                 (expect (= "~/" (paths/abbreviate-home "/home/u" "/home/u")))
                 (expect (= "~/a/b" (paths/abbreviate-home "/home/u/a/b" "/home/u")))
                 ;; normalized first, so a `..` detour still abbreviates
                 (expect (= "~/a" (paths/abbreviate-home "/home/u/../u/a" "/home/u"))))
             (it "only rewrites paths at or under home"
                 (expect (= "/etc/x" (paths/abbreviate-home "/etc/x" "/home/u")))
                 ;; a sibling home whose name merely STARTS with ours is not under it
                 (expect (= "/home/user2/a" (paths/abbreviate-home "/home/user2/a" "/home/u"))))
             (it "round-trips with expand-home for a path under home"
                 (let [home
                       (System/getProperty "user.home")

                       abbreviated
                       (paths/abbreviate-home (str home "/projects/vis"))]

                   (expect (str/starts-with? abbreviated "~/"))
                   (expect (= (paths/unixify (str home "/projects/vis"))
                              (paths/unixify (paths/expand-home abbreviated)))))))

(defdescribe
  compact-path-text-test
  ;; #306: extension diagnostics use this so shared terminal output never names the user.
  (it "shows home as `~` and the root as relative inside text"
      (expect (= "File \"~/.vis/x.py\", line 3"
                 (paths/compact-path-text nil "File \"/home/u/.vis/x.py\", line 3" "/home/u")))
      (expect
        (= "edit src/a.clj in ~"
           (paths/compact-path-text "/home/u/p" "edit /home/u/p/src/a.clj in /home/u" "/home/u"))))
  (it "leaves sibling names, URL paths and non-strings unchanged"
      (expect
        (= "/home/u2/x and https://example.test/home/u/x"
           (paths/compact-path-text nil "/home/u2/x and https://example.test/home/u/x" "/home/u")))
      (expect (nil? (paths/compact-path-text nil nil "/home/u")))
      (expect (= "/home/u/x" (paths/compact-path-text nil "/home/u/x" nil)))))

(defdescribe shell-path-test
             ;; #306: a command printed in an error must still work when it is pasted into a shell.
             (it "keeps `~/` unquoted so the shell expands it"
                 (expect (= "~/vis-repro" (paths/shell-path "/home/u/vis-repro" "/home/u")))
                 (expect (= "~/" (paths/shell-path "/home/u" "/home/u")))
                 (expect (= "/opt/work/p" (paths/shell-path "/opt/work/p" "/home/u"))))
             (it "single-quotes only a remainder that needs quotes"
                 (expect (= "~/'My Projects/it'\\''s'"
                            (paths/shell-path "/home/u/My Projects/it's" "/home/u")))
                 (expect (= "'/opt/my work'" (paths/shell-path "/opt/my work" "/home/u")))
                 (expect (nil? (paths/shell-path nil "/home/u")))))

(defdescribe logs-dir-test
             (it "defaults to a DEDICATED subdir, never ~/.vis itself"
                 ;; The file tools and the Python sandbox get always-on access to logs; that
                 ;; must not expose config.edn, the session DB, or gateway tokens.
                 (let [previous (System/clearProperty "vis.logs.dir")]
                   (try (let [d (paths/logs-dir)]
                          (expect (str/ends-with? (paths/unixify d) "/.vis/logs"))
                          (expect (str/starts-with? d (System/getProperty "user.home")))
                          (expect (not= d (str (System/getProperty "user.home") "/.vis"))))
                        (finally (when previous (System/setProperty "vis.logs.dir" previous))))))
             (it "follows the vis.logs.dir property so a test JVM keeps its logs in target/"
                 (let [previous (System/getProperty "vis.logs.dir")]
                   (try (System/setProperty "vis.logs.dir" "target/vis-logs-override")
                        (expect (= "target/vis-logs-override" (paths/logs-dir)))
                        (finally (if previous
                                   (System/setProperty "vis.logs.dir" previous)
                                   (System/clearProperty "vis.logs.dir"))))))
             (it "creates the directory and returns the same path"
                 (let [d (paths/ensure-logs-dir!)]
                   (expect (= (paths/logs-dir) d))
                   (expect (.isDirectory (java.io.File. ^String d)))
                   ;; idempotent: a second call on an existing dir still returns it
                   (expect (= d (paths/ensure-logs-dir!))))))

(defn- with-test-logs
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-dated-logs-test-"
                                                (make-array FileAttribute 0)))]
    (try (with-redefs [paths/logs-dir #(.getPath dir)]
           (f dir))
         (finally (with-open [stream (Files/walk (.toPath dir) (make-array FileVisitOption 0))]
                    (doseq [^Path path (reverse (iterator-seq (.iterator stream)))]
                      (Files/deleteIfExists path)))))))

(defdescribe
  dated-logs-test
  (it "uses UTC at midnight regardless of the machine's timezone"
      (with-test-logs (fn [dir]
                        (let [previous (TimeZone/getDefault)]
                          (try (TimeZone/setDefault (TimeZone/getTimeZone "Pacific/Auckland"))
                               (doseq [[instant date] [["2026-09-01T23:59:59.999Z" "2026-09-01"]
                                                       ["2026-09-02T00:00:00Z" "2026-09-02"]]]
                                 (let [at (Instant/parse instant)
                                       expected (.getPath (io/file dir date))]

                                   (expect (= expected (paths/log-date-dir at)))
                                   (expect (not (.exists (io/file expected))))
                                   (expect (= expected (paths/ensure-log-date-dir! at)))
                                   (expect (.isDirectory (io/file expected)))
                                   (expect (= expected (paths/ensure-log-date-dir! at)))))
                               (finally (TimeZone/setDefault previous)))))))
  (it "lists only real date directories and does not follow links"
      (with-test-logs (fn [dir]
                        (doseq [name ["2026-09-02" "2026-08-31" "2026-02-30" "notes"]]
                          (.mkdirs (io/file dir name)))
                        (spit (io/file dir "2026-09-03") "not a directory")
                        (Files/createSymbolicLink (.toPath (io/file dir "2026-09-04"))
                                                  (.toPath (io/file dir "notes"))
                                                  (make-array FileAttribute 0))
                        (expect (= ["2026-09-02" "2026-08-31"]
                                   (mapv #(.getName ^File %) (paths/log-date-dirs)))))))
  (it "returns no dates before the log root exists"
      (with-test-logs (fn [dir]
                        (with-redefs [paths/logs-dir #(.getPath (io/file dir "absent"))]
                          (expect (empty? (paths/log-date-dirs)))))))
  (it "keeps a process log under its startup date after midnight"
      (with-test-logs
        (fn [dir]
          (with-redefs [paths/process-start-time (delay (Instant/parse "2026-09-01T23:59:59Z"))]
            (let [file (io/file (paths/log-file "gateway"))]
              (expect (= (io/file dir "2026-09-01") (.getParentFile file)))
              (expect (str/starts-with? (.getName file) "gateway-20260901T235959Z-pid"))
              (expect (= (.getPath file) (paths/log-file "gateway")))))))))

;; Regression: two vis processes (TUI + gateway daemon) shared `~/.vis/vis.log`;
;; Telemere's rolling handler rotates by RENAMING the file, so the process that
;; did not rotate went on appending into a deleted inode and everything it
;; logged after that point — including the SSE stream trace — was unreadable.
(defdescribe log-file-test
             (it "stamps the process role, UTC start time, and pid into the name"
                 (let [f
                       (paths/log-file "gateway")

                       filename
                       (.getName (java.io.File. ^String f))

                       pid
                       (paths/process-id)]

                   (expect (some? (re-matches #"\d{4}-\d{2}-\d{2}"
                                              (.getName (.getParentFile (java.io.File. ^String
                                                                                       f))))))
                   (expect (some? (re-matches #"gateway-\d{8}T\d{6}Z-pid\d+\.log" filename)))
                   (expect (str/ends-with? filename (str "pid" pid ".log")))
                   (expect (= f (paths/log-file "gateway")))
                   (expect (pos? pid))
                   (expect (.isDirectory (java.io.File. ^String (paths/logs-dir))))))
             (it "uses the process role for the default log path"
                 (let [previous (System/getProperty "vis.log.role")]
                   (try (paths/set-log-role! "tui")
                        (expect (str/includes? (paths/unixify (paths/log-file)) "/tui-"))
                        (finally (if previous
                                   (System/setProperty "vis.log.role" previous)
                                   (System/clearProperty "vis.log.role")))))))

(defdescribe
  claim-dir-test
  (it "tells a directory this process serves from one a dead process left behind"
      (let [held
            (.toFile (Files/createTempDirectory "vis-claim-held" (make-array FileAttribute 0)))

            free
            (.toFile (Files/createTempDirectory "vis-claim-free" (make-array FileAttribute 0)))

            bare
            (.toFile (Files/createTempDirectory "vis-claim-bare" (make-array FileAttribute 0)))]

        (try (expect (= :none (paths/claim-state bare)))
             (expect (= held (paths/claim-dir! held)))
             (expect (= :held (paths/claim-state held)))
             (expect (.isFile (io/file held ".vis-live")))
             ;; Nothing else in this process may open the token: that would drop the lock.
             (expect (paths/held-file? (io/file held ".vis-live")))
             ;; Claiming twice is the same claim, not a second descriptor.
             (expect (= :held (paths/claim-state (paths/claim-dir! held))))
             ;; What a killed process leaves behind: the token, held by nobody.
             (spit (io/file free ".vis-live") "")
             (expect (= :free (paths/claim-state free)))
             (expect (= :none (paths/claim-state (io/file bare "gone"))))
             (finally (doseq [^File dir
                              [held free bare]

                              ^File entry
                              (reverse (file-seq dir))]

                        (.delete entry)))))))

;; POSIX locks belong to the process: closing ANY descriptor of a locked file drops
;; them all, so the registry is what keeps file tools away from sqlite's live files.
(defdescribe held-files-test
             (it "tracks files by owner under their canonical path until every owner lets go"
                 (let [dir
                       (.getCanonicalFile (.toFile (Files/createTempDirectory
                                                     "vis-held"
                                                     (make-array FileAttribute 0))))

                       db
                       (io/file dir "vis.db")

                       shm
                       (io/file dir "vis.db-shm")]

                   (.mkdir (io/file dir "sub"))
                   (try (expect (not (paths/held-file? db)))
                        ;; Files need not exist yet: sqlite creates `-shm` after the first open.
                        (paths/hold-files! ::sqlite [db shm])
                        (paths/hold-files! ::other [(.getPath db)])
                        (expect (paths/held-file? (io/file dir "sub" ".." "vis.db")))
                        (expect (paths/held-file? (.getPath shm)))
                        (expect (contains? (paths/held-files) (.getCanonicalPath shm)))
                        (paths/release-held-files! ::sqlite)
                        (expect (paths/held-file? db))
                        (expect (not (paths/held-file? shm)))
                        (paths/release-held-files! ::other)
                        (expect (not (paths/held-file? db)))
                        (finally (paths/release-held-files! ::sqlite)
                                 (paths/release-held-files! ::other)
                                 (doseq [^File entry (reverse (file-seq dir))]
                                   (.delete entry)))))))

;; #423: an outer sandbox can deny /tmp, so VIS_RUNTIME_DIR also holds temporary files.
(defn- with-runtime-setting
  "Run `f` with `vis.runtime.dir` set to `value`, then restore it and `java.io.tmpdir`."
  [value f]
  (let [previous-setting
        (System/getProperty "vis.runtime.dir")

        previous-tmpdir
        (System/getProperty "java.io.tmpdir")]

    (try (System/setProperty "vis.runtime.dir" (str value))
         (f)
         (finally (if previous-setting
                    (System/setProperty "vis.runtime.dir" previous-setting)
                    (System/clearProperty "vis.runtime.dir"))
                  (System/setProperty "java.io.tmpdir" previous-tmpdir)))))

(defn- private-task-dir
  "A new private directory with its real path, so it has no symbolic link."
  ^File []
  (.toFile (.toRealPath (Files/createTempDirectory "vis-423-" (make-array FileAttribute 0))
                        (make-array java.nio.file.LinkOption 0))))

(defn- delete-tree!
  [^File dir]
  (doseq [^File f (reverse (file-seq dir))]
    (.delete f)))

(defn- refusal
  "The ex-data and message of the refusal of `setting`, or nil when nothing refused."
  [setting]
  (with-runtime-setting setting
                        (fn []
                          (try (paths/install-temp-dir!)
                               nil
                               (catch clojure.lang.ExceptionInfo e
                                 (assoc (ex-data e)
                                   :message (ex-message e)
                                   :tmpdir (System/getProperty "java.io.tmpdir")))))))

(defdescribe
  runtime-temp-dir-test
  (it "changes nothing when the task runtime directory is unset"
      (let [previous (System/getProperty "java.io.tmpdir")]
        (with-runtime-setting ""
                              (fn []
                                (expect (nil? (paths/install-temp-dir!)))
                                (expect (= previous (System/getProperty "java.io.tmpdir")))
                                (expect (= {} (paths/temp-env)))))))
  (it "moves temporary files into a private directory in the task directory"
      (let [task (private-task-dir)]
        (try (with-runtime-setting
               task
               (fn []
                 (let [dir (paths/install-temp-dir!)
                       expected (.toPath (File. task "tmp"))]

                   (expect (= expected dir))
                   (expect (= "rwx------"
                              (java.nio.file.attribute.PosixFilePermissions/toString
                                (Files/getPosixFilePermissions dir
                                                               (make-array java.nio.file.LinkOption
                                                                           0)))))
                   (expect (= (str expected) (System/getProperty "java.io.tmpdir")))
                   (expect (= (.toFile expected) (paths/temp-dir)))
                   (expect (= {"TMPDIR" (str expected)} (paths/temp-env)))
                   (expect (= expected (paths/install-temp-dir!)) "a second run keeps it")
                   (let [file (File/createTempFile "vis-423-" ".tmp" (paths/temp-dir))]
                     (expect (= (.toFile expected) (.getParentFile file)))))))
             (finally (delete-tree! task)))))
  (it
    "fails closed and keeps the temporary directory on a refused setting"
    (let [task
          (private-task-dir)

          link
          (File. (.getParentFile task) (str (.getName task) "-link"))

          shared
          (doto (File. task "shared") (.mkdirs))

          linked-tmp
          (doto (File. task "linked") (.mkdirs))

          previous
          (System/getProperty "java.io.tmpdir")]

      (try (Files/createSymbolicLink (.toPath link) (.toPath task) (make-array FileAttribute 0))
           (Files/setPosixFilePermissions (.toPath shared)
                                          (java.nio.file.attribute.PosixFilePermissions/fromString
                                            "rwxrwxrwx"))
           (Files/createSymbolicLink (.toPath (File. linked-tmp "tmp"))
                                     (.toPath task)
                                     (make-array FileAttribute 0))
           (doseq [[setting reason] [[(File. task "missing") "does not exist"]
                                     ["relative/task" "is not an absolute path"]
                                     [link "contains a symbolic link"]
                                     [shared "is writable by other users"]
                                     [linked-tmp "that is not a private directory"]]]
             (let [{:keys [type message tmpdir]} (refusal setting)]
               (expect (= :vis/runtime-dir type) (str setting))
               (expect (str/includes? (str message) reason) (str message))
               (expect (= previous tmpdir))))
           (finally (.delete link) (delete-tree! task))))))

(defdescribe
  spit-private-test
  (it "throws and keeps the previous file when the replace cannot happen"
      ;; Regression: the old fallback rewrote the file in place with plain spit,
      ;; so an interrupted write could leave a credential file half written.
      (let [dir
            (private-task-dir)

            file
            (io/file dir "auth.json")]

        (try (paths/spit-private! file "previous")
             (.setWritable dir false)
             (expect (throws? java.io.IOException #(paths/spit-private! file "next")))
             (expect (= "previous" (slurp file)))
             (expect (= ["auth.json"] (vec (.list dir))))
             (finally (.setWritable dir true) (delete-tree! dir)))))
  (it "writes through a symbolic link and keeps the link"
      (let [dir
            (private-task-dir)

            target
            (io/file dir "real.json")

            link
            (io/file dir "link.json")]

        (try (spit target "old")
             (Files/createSymbolicLink (.toPath link) (.toPath target) (make-array FileAttribute 0))
             (paths/spit-private! link "new")
             (expect (Files/isSymbolicLink (.toPath link)))
             (expect (= "new" (slurp target)))
             (expect (= #{"real.json" "link.json"} (set (.list dir))))
             (finally (delete-tree! dir))))))
