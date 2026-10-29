(ns com.blockether.vis.native-image-env-capture-test
  "The native binary must read the MACHINE IT RUNS ON, not the machine it was built on.

   `native-image` (via the `graal-build-time` feature) initializes every Clojure
   namespace at BUILD time and snapshots the resulting class state into the image
   heap. A top-level `(def x (System/getProperty \"user.home\"))` is therefore
   evaluated on the BUILDER and frozen: the shipped binary looks for the builder's
   `~/.vis`, and `(def *color-enabled?* (boolean (System/console)))` freezes `false`
   because a build has no controlling terminal.

   BEHAVIORAL checks that the paths and the color decision actually follow the
   process' own `user.home` / `user.dir` when they are asked."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-home
  "Run `f` with `home` as `user.home` and without the test alias' `vis.logs.dir`."
  [home f]
  (let [original
        (System/getProperty "user.home")

        logs-dir
        (System/getProperty "vis.logs.dir")]

    (try (System/setProperty "user.home" home)
         (System/clearProperty "vis.logs.dir")
         (f)
         (finally (System/setProperty "user.home" original)
                  (when logs-dir (System/setProperty "vis.logs.dir" logs-dir))))))

(defdescribe
  runtime-paths-follow-this-process-test
  (it "config paths resolve against the RUNNING process' home"
      (with-home "/tmp/vis-home-probe"
                 (fn []
                   (expect (= "/tmp/vis-home-probe/.vis" (config/config-dir)))
                   (expect (= "/tmp/vis-home-probe/.vis/state.yml" (config/state-path)))
                   (expect (= "/tmp/vis-home-probe/.vis/vis.mdb" (config/db-path)))
                   (expect (= {:backend :sqlite :path "/tmp/vis-home-probe/.vis/vis.mdb"}
                              (config/default-db-spec)))
                   ;; Per-process by construction (`paths/log-file`): the
                   ;; DIRECTORY follows this process' home, and the NAME carries
                   ;; the role, this process' UTC start stamp and its pid, so two
                   ;; vis processes never rotate one file.
                   (expect (re-matches (re-pattern (str "/tmp/vis-home-probe/\\.vis/logs/"
                                                        "\\d{4}-\\d{2}-\\d{2}/"
                                                        "(?:gateway|tui|vis)-\\d{8}T\\d{6}Z-pid"
                                                        (.pid (java.lang.ProcessHandle/current))
                                                        "\\.log"))
                                       (config/log-path))))))
  (it "dotenv defaults resolve against the working directory when consulted"
      (expect (= (str (System/getProperty "user.dir") "/.env") (#'config/dotenv-path :cwd "/.env")))
      (expect (= (str (System/getProperty "user.dir") "/.env.local")
                 (#'config/dotenv-path :cwd "/.env.local")))
      ;; An explicit binding still wins, and nil still means "no file".
      (expect (= "/tmp/other.env" (#'config/dotenv-path "/tmp/other.env" "/.env")))
      (expect (nil? (#'config/dotenv-path nil "/.env")))))

(defdescribe
  color-is-decided-at-runtime-test
  (it "the color toggle carries no build-time snapshot"
      (expect
        (nil? (var-get #'commandline/*color-enabled?*))
        "*color-enabled?* must default to nil (auto-detect per call), never a captured boolean")
      (expect (= (boolean (and (System/console)
                               (str/blank? (System/getenv "NO_COLOR"))
                               (not= "dumb" (System/getenv "TERM"))))
                 (commandline/color-enabled?))))
  (it "an explicit binding still forces the answer"
      (binding [commandline/*color-enabled?* true]
        (expect (true? (commandline/color-enabled?))))
      (binding [commandline/*color-enabled?* false]
        (expect (false? (commandline/color-enabled?))))))
