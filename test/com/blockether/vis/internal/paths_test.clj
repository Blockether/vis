(ns com.blockether.vis.internal.paths-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [lazytest.core :refer [defdescribe expect it]]))

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

;; Regression: two vis processes (TUI + gateway daemon) shared `~/.vis/vis.log`;
;; Telemere's rolling handler rotates by RENAMING the file, so the process that
;; did not rotate went on appending into a deleted inode and everything it
;; logged after that point — including the SSE stream trace — was unreadable.
(defdescribe log-file-test
             (it "stamps THIS process's pid into the name, inside ~/.vis/logs"
                 (let [f
                       (paths/unixify (paths/log-file))

                       pid
                       (paths/process-id)]

                   (expect (str/ends-with? f (str "/.vis/logs/vis-" pid ".log")))
                   (expect (= f (paths/unixify (paths/log-file))))
                   (expect (pos? pid))
                   (expect (.isDirectory (java.io.File. ^String (paths/logs-dir))))))
             (it "gives a second sink in the same process its OWN file"
                 ;; A raw FileOutputStream (GraalPy's polyglot log) keeps a stale fd
                 ;; across the handler's rotation rename, so it must not share a name.
                 (expect (not= (paths/log-file) (paths/log-file "graalpy")))
                 (expect (str/ends-with? (paths/unixify (paths/log-file "graalpy"))
                                         (str "/.vis/logs/graalpy-" (paths/process-id) ".log")))))
