(ns com.blockether.vis.internal.paths-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files FileVisitOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant)
           (java.util TimeZone)))

(defdescribe unixify-test
             (it "normalizes Windows separators and leaves POSIX paths alone"
                 (expect (= "a/b/c" (paths/unixify "a\\b\\c")))
                 (expect (= "a/b" (paths/unixify "a/b")))
                 (expect (= "C:/x/y" (paths/unixify "C:\\x\\y"))))
             (it "is nil-safe and stringifies non-strings"
                 (expect (nil? (paths/unixify nil)))
                 (expect (= "" (paths/unixify "")))
                 (expect (= "a/b" (paths/unixify (java.io.File. "a/b"))))))

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
      (expect (= "~/x" (paths/expand-home "~/x" ""))))
  (it "uses the JVM's user.home in the 1-arity"
      (expect (= (System/getProperty "user.home") (paths/expand-home "~")))))

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
             (it "leaves relative paths and nil alone, and never throws"
                 (expect (= "rel/x" (paths/abbreviate-home "rel/x" "/home/u")))
                 (expect (nil? (paths/abbreviate-home nil "/home/u")))
                 (expect (= "/home/u/a" (paths/abbreviate-home "/home/u/a" nil))))
             (it "round-trips with expand-home for a path under home"
                 (let [home
                       (System/getProperty "user.home")

                       abbreviated
                       (paths/abbreviate-home (str home "/projects/vis"))]

                   (expect (str/starts-with? abbreviated "~/"))
                   (expect (= (paths/unixify (str home "/projects/vis"))
                              (paths/unixify (paths/expand-home abbreviated)))))))

(defdescribe logs-dir-test
             (it "is a DEDICATED subdir, never ~/.vis itself"
                 ;; The file tools and the Python sandbox get always-on access to logs; that
                 ;; must not expose config.edn, the session DB, or gateway tokens.
                 (let [d (paths/logs-dir)]
                   (expect (str/ends-with? (paths/unixify d) "/.vis/logs"))
                   (expect (str/starts-with? d (System/getProperty "user.home")))
                   (expect (not= d (str (System/getProperty "user.home") "/.vis")))))
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
