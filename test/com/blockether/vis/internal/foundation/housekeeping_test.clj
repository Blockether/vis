(ns com.blockether.vis.internal.foundation.housekeeping-test
  "Stale-state accounting for `~/.vis/gateway/events`, `~/.vis/sandbox` and the
   directories `sweep-stale!` deletes on its own.

   Everything here runs against throwaway directories bound through — for every
   sweep test, ALL FIVE at once via `with-homes` — `housekeeping/*logs-home*`,
   `*cache-home*`, `*python-home*`, `*events-home*` and `*sandbox-home*`; no test
   may read, let alone delete, anything under the real `~/.vis`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]])
  (:import [java.io File]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute FileAttribute]))

;; Fixtures

(defn- tmp-dir ^File [prefix] (test-temp/temp-dir prefix))

;; `scan` and `purge!` also read the sandbox directory. Each test gets its own,
;; so no test reads or deletes the real `~/.vis/sandbox`.
(set-ns-context! [(around-each [f]
                               (binding [housekeeping/*sandbox-home* (.getPath (tmp-dir
                                                                                 "vis-hk-sbx"))]
                                 (f)))])

(def ^:private day-ms 86400000)

(defn- age-ms
  "The mtime of something `age-days` old."
  ^long [age-days]
  (- (System/currentTimeMillis) (* (long age-days) (long day-ms))))

(defn- touch!
  "Write `content` to `path` and stamp it `age-days` old. The mtime is what
   staleness is judged on, so tests state the age directly instead of sleeping."
  ^File [^File dir path age-days content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)
    (.setLastModified f (age-ms age-days))
    f))

;; scan — gateway journals

(defdescribe scan-journals-test
             (it "reports only `.ndjson` journals older than the cutoff"
                 (let [events (tmp-dir "vis-hk-journals")]
                   (touch! events "old.ndjson" 30 "{}\n")
                   (touch! events "live.ndjson" 0 "{}\n")
                   (touch! events "notes.txt" 30 "not a journal")
                   (binding [housekeeping/*events-home* (.getPath events)]

                     (let [report (housekeeping/scan {:days 14})
                           journals (:journals report)]

                       (expect (= 2 (:file-count journals)))
                       (expect (= ["old.ndjson"] (mapv :label (:reclaimable journals))))
                       (expect (= 1 (:count report))))))))

;; scan and purge! — sandbox snapshots
;; The sandbox directory kept 11,388 snapshot files (254 MiB) of sessions with no
;; row, written by test runs, and nothing could report or remove them.

(defn- sandbox-fixture!
  "A sandbox directory with snapshots of the sessions `gone` (no row, old), `kept`
   (row, old) and `young` (no row, new), and an old and a new temp file."
  [^File dir gone kept young]
  (touch! dir (str gone ".py") 30 "x = 1\n")
  (touch! dir (str gone ".spill" File/separator "index.json") 30 "{}")
  (touch! dir (str kept ".py") 30 "x = 2\n")
  (touch! dir (str young ".py") 1 "x = 3\n")
  (touch! dir (str "." gone ".py.tmp-" (random-uuid)) 30 "partial")
  (touch! dir (str "." young ".py.tmp-" (random-uuid)) 0 "in flight"))

(defn- sandbox-items [report] (sort (map (juxt :kind :label) (:reclaimable (:sandbox report)))))

(defdescribe
  scan-sandbox-test
  (it "reports old snapshots with no session row and old temp files, and keeps the rest"
      (let [dir
            (tmp-dir "vis-hk-sandbox")

            [gone kept young]
            (repeatedly 3 #(str (random-uuid)))]

        (sandbox-fixture! dir gone kept young)
        (binding [housekeeping/*events-home*
                  (.getPath (tmp-dir "vis-hk-ev"))

                  housekeeping/*sandbox-home*
                  (.getPath dir)]

          (with-redefs [p/db-get-session (fn [_ id]
                                           (when (= kept (str id)) {:id id}))]
            (let [report (housekeeping/scan {:db-info :fixture :days 14})
                  gone-item (first (filter #(= gone (:label %)) (:reclaimable (:sandbox report))))]

              (expect (= 2 (:count report)))
              (expect (= [[:sandbox gone] [:sandbox-temp (str "." gone)]]
                         (mapv (fn [[k l]]
                                 [k (if (= k :sandbox-temp) (subs l 0 37) l)])
                               (sandbox-items report))))
              ;; The snapshot and its spill directory go together.
              (expect (= 2 (count (:paths gone-item)))))))))
  (it "reports no snapshot without a database, and keeps one whose row lookup fails"
      (let [dir
            (tmp-dir "vis-hk-sandbox-nodb")

            [gone kept young]
            (repeatedly 3 #(str (random-uuid)))]

        (sandbox-fixture! dir gone kept young)
        (binding [housekeeping/*events-home*
                  (.getPath (tmp-dir "vis-hk-ev"))

                  housekeeping/*sandbox-home*
                  (.getPath dir)]

          (expect (= [:sandbox-temp] (mapv first (sandbox-items (housekeeping/scan {:days 14})))))
          (with-redefs [p/db-get-session (fn [_ _]
                                           (throw (ex-info "db closed" {})))]
            (expect (= [:sandbox-temp]
                       (mapv first
                             (sandbox-items (housekeeping/scan {:db-info :fixture :days 14})))))))))
  (it "keeps the snapshot of a session that has a row in a real store"
      ;; `vis-agent doctor` passed a bare spec, so every row read answered nil and
      ;; the snapshots of live sessions were reported as orphans.
      (let [dir
            (tmp-dir "vis-hk-sandbox-store")

            db
            (p/db-create-connection! :memory)]

        (try (let [live
                   (str (h/store-session! db {:channel :api}))

                   gone
                   (str (random-uuid))]

               (touch! dir (str live ".py") 30 "x = 1\n")
               (touch! dir (str gone ".py") 30 "x = 2\n")
               (binding [housekeeping/*events-home*
                         (.getPath (tmp-dir "vis-hk-ev"))

                         housekeeping/*sandbox-home*
                         (.getPath dir)]

                 (expect (= [[:sandbox gone]]
                            (sandbox-items (housekeeping/scan {:db-info db :days 14}))))))
             (finally (p/db-dispose-connection! db)))))
  (it "purges the reported files only, and a second pass finds nothing"
      (let [dir
            (tmp-dir "vis-hk-sandbox-purge")

            [gone kept young]
            (repeatedly 3 #(str (random-uuid)))]

        (sandbox-fixture! dir gone kept young)
        (binding [housekeeping/*events-home*
                  (.getPath (tmp-dir "vis-hk-ev"))

                  housekeeping/*sandbox-home*
                  (.getPath dir)]

          (with-redefs [p/db-get-session (fn [_ id]
                                           (when (= kept (str id)) {:id id}))]
            (let [report (housekeeping/purge! {:db-info :fixture :days 14})]
              (expect (every? :is-purged (:purged report)))
              (expect (= #{(str kept ".py") (str young ".py")}
                         (set (keep #(let [n (.getName ^File %)] (when-not (.startsWith n ".") n))
                                    (.listFiles dir)))))
              (expect (= 1 (count (filter #(.startsWith (.getName ^File %) ".") (.listFiles dir)))))
              (expect (zero? (:count (housekeeping/purge! {:db-info :fixture :days 14})))))))))
  ;; Regression, user report in this Vis session: temp files of killed writes inside
  ;; spill and turn state folders were never found, and a hard-linked turn state was
  ;; counted once for each link.
  (it
    "reports old temp files in nested folders and counts a hard-linked file once"
    (let [dir
          (tmp-dir "vis-hk-sandbox-nested")

          [gone kept]
          (repeatedly 2 #(str (random-uuid)))

          snapshot
          (touch! dir (str gone ".py") 30 "x = 1\n")

          linked
          (io/file dir (str gone ".turns") "turn.py")]

      (io/make-parents linked)
      (Files/createLink (.toPath linked) (.toPath snapshot))
      (touch! dir (str gone ".turns" File/separator ".turn.py.tmp-" (random-uuid)) 30 "partial")
      (touch! dir (str kept ".py") 30 "x = 2\n")
      (touch! dir (str kept ".turns" File/separator ".turn.py.tmp-" (random-uuid)) 30 "partial")
      (touch! dir (str kept ".spill" File/separator ".young.bin.tmp-" (random-uuid)) 0 "in flight")
      (binding [housekeeping/*events-home*
                (.getPath (tmp-dir "vis-hk-ev"))

                housekeeping/*sandbox-home*
                (.getPath dir)]

        (with-redefs [p/db-get-session (fn [_ id]
                                         (when (= kept (str id)) {:id id}))]
          (let [items (:reclaimable (:sandbox (housekeeping/scan {:db-info :fixture :days 14})))]
            (expect (= {:sandbox 1 :sandbox-temp 1} (frequencies (map :kind items))))
            ;; The orphan item holds the temp file in its folder, and the linked file once.
            (expect (= (+ (count "x = 1\n") (count "partial"))
                       (:bytes (first (filter #(= :sandbox (:kind %)) items)))))
            (expect (= 1 (count (:paths (first (filter #(= :sandbox-temp (:kind %)) items))))))
            (expect (every? :is-purged
                            (:purged (housekeeping/purge! {:db-info :fixture :days 14}))))
            (expect (not (.exists linked)))
            (expect (zero? (:count (housekeeping/scan {:db-info :fixture :days 14}))))))))))

;; purge!

(defdescribe purge-test
             (it "dry-run deletes nothing and still returns the full plan"
                 (let [events (tmp-dir "vis-hk-dry-ev")]
                   (touch! events "old.ndjson" 40 "{}\n")
                   (binding [housekeeping/*events-home* (.getPath events)]
                     (let [report (housekeeping/purge! {:days 14 :is-dry-run true})]
                       (expect (true? (:is-dry-run report)))
                       (expect (= 1 (count (:purged report))))
                       (expect (every? #(false? (:is-purged %)) (:purged report)))
                       (expect (zero? (long (:reclaimed-bytes report))))
                       (expect (.exists (io/file events "old.ndjson")))))))
             (it "reclaims stale journals and leaves fresh ones alone"
                 (let [events (tmp-dir "vis-hk-purge-ev")]
                   (touch! events "old.ndjson" 40 "{}\n")
                   (touch! events "live.ndjson" 0 "{}\n")
                   (binding [housekeeping/*events-home* (.getPath events)]
                     (let [report (housekeeping/purge! {:days 14})]
                       (expect (false? (:is-dry-run report)))
                       (expect (every? :is-purged (:purged report)))
                       (expect (pos? (long (:reclaimed-bytes report))))
                       (expect (not (.exists (io/file events "old.ndjson"))))
                       (expect (.exists (io/file events "live.ndjson")))
                       ;; A second pass finds nothing left to do.
                       (expect (zero? (:count (housekeeping/purge! {:days 14}))))))))
             (it "never deletes outside the roots it owns"
                 ;; The `under?` guard is the only thing between a bad `:root` and an
                 ;; arbitrary `rm -rf`, so it is asserted directly.
                 (let [outside
                       (tmp-dir "vis-hk-outside")

                       victim
                       (touch! outside "precious.txt" 99 "do not delete")

                       events
                       (tmp-dir "vis-hk-ev")]

                   (binding [housekeeping/*events-home* (.getPath events)]
                     (with-redefs [housekeeping/scan
                                   (constantly {:days 14
                                                :journals {:root (.getPath events)
                                                           :reclaimable [{:kind :journal
                                                                          :root (.getPath victim)
                                                                          :bytes 1}]}
                                                :sandbox {:reclaimable []}})]
                       (housekeeping/purge! {:days 14})))
                   (expect (.exists victim)))))

;; sweep-stale! — the self-deleting surface

(defn- target
  "The one target report named `id` inside a `sweep-stale!` result."
  [report id]
  (first (filter #(= id (:id %)) (:targets report))))

(defn- seam-path
  "`dir` as a path, or a fresh throwaway directory named `prefix`."
  ^String [^File dir ^String prefix]
  (let [^File d (or dir (tmp-dir prefix))]
    (.getPath d)))

(defn- with-homes
  "Call `f` with ALL FIVE sweep seams pointed at throwaway directories. A test
   that bound only the seam it cares about would leave the other targets
   resolving to the operator's real `~/.vis` — and this sweep deletes."
  [{:keys [logs cache events python sandbox]} f]
  (binding [housekeeping/*logs-home*
            (seam-path logs "vis-hk-idle-logs")

            housekeeping/*cache-home*
            (seam-path cache "vis-hk-idle-cache")

            housekeeping/*python-home*
            (seam-path python "vis-hk-idle-python")

            housekeeping/*events-home*
            (seam-path events "vis-hk-idle-events")

            housekeeping/*sandbox-home*
            (seam-path sandbox "vis-hk-idle-sandbox")]

    (f)))

(defdescribe
  sweep-stale-test
  (it "deletes only log files older than the retention window"
      (let [logs (tmp-dir "vis-hk-logs")]
        (touch! logs "vis-nrepl-fresh.log" 1 "fresh")
        (touch! logs "vis-nrepl-edge.log" 13 "edge")
        (touch! logs "vis-nrepl-old.log" 15 "old-content")
        (touch! logs "vis-nrepl-ancient.log" 400 "ancient")
        (let [report (target (with-homes {:logs logs} #(housekeeping/sweep-stale! nil)) :logs)]
          (expect (= 4 (:file-count report)))
          (expect (= 2 (:deleted report)))
          (expect (= (+ (count "old-content") (count "ancient")) (:bytes report)))
          (expect (= #{"vis-nrepl-fresh.log" "vis-nrepl-edge.log"}
                     (set (map #(.getName ^File %) (.listFiles logs))))))))
  ;; Regression: the sweep looked at the TOP LEVEL of `~/.vis/logs` only and
  ;; explicitly left subdirectories alone, so every log `shell` ever wrote --
  ;; `logs/shell/<run>/<id>.log`, one directory per command -- was immortal. A
  ;; single week of them outweighed everything the sweep could see.
  (it "deletes stale logs inside the per-command shell directories and prunes the ones it empties"
      (let [logs
            (tmp-dir "vis-hk-logs-nested")

            stale
            (touch! logs (str "shell" File/separator "run-1" File/separator "npm-test.log") 90 "x")

            fresh
            (touch! logs (str "shell" File/separator "run-2" File/separator "npm-build.log") 1 "y")

            report
            (target (with-homes {:logs logs} #(housekeeping/sweep-stale! nil)) :logs)]

        (expect (= 2 (:file-count report)))
        (expect (= 1 (:deleted report)))
        (expect (= 1 (:dirs-removed report)))
        (expect (not (.exists stale)))
        (expect (not (.exists (.getParentFile stale))))
        (expect (.exists fresh))
        (expect (.isDirectory logs))))
  (it "sweeps every diagnostic format by file age and prunes only empty date directories"
      (doseq [sweep! [#(target (housekeeping/sweep-stale! nil) :logs)
                      #(#'housekeeping/sweep-logs! nil)]]
        (let [logs (tmp-dir "vis-hk-dated-logs")
              stale (mapv #(touch! logs (str "2026-08-01/" %) 40 "old")
                          ["gateway.log" "gateway.log.1.gz" "gateway-boot.log" "vis-nrepl.log"
                           "shell/session/build.log" "outside/shell-run.log"
                           "pyext-worker/worker.log" "pyext-worker/jvm-crash-1.log"
                           "pyext-worker/jvm-heap.hprof" "gateway-hang-1/report.json"
                           "gateway-hang-1/threads.json" "vis-gateway.jfr"])
              live-log (touch! logs "2026-08-02/gateway-live.log" 0 "still running")
              report (with-homes {:logs logs} sweep!)]

          (expect (= (inc (count stale)) (:file-count report)))
          (expect (= (count stale) (:deleted report)))
          (expect (every? #(not (.exists ^File %)) stale))
          (expect (not (.exists (io/file logs "2026-08-01"))))
          (expect (.exists live-log)))))
  (it "honours an explicit :days window"
      (let [logs (tmp-dir "vis-hk-logs-days")]
        (touch! logs "a.log" 5 "a")
        (touch! logs "b.log" 30 "b")
        (expect (= 2
                   (:deleted (target (with-homes {:logs logs}
                                                 #(housekeeping/sweep-stale! {:days 1}))
                                     :logs))))
        (expect (zero? (count (.listFiles logs))))))
  (it "preserves a new writer's empty directory but prunes abandoned empty directories"
      ;; A periodic sweep can run between a writer's mkdir and its first file open.
      (doseq [sweep! [#(housekeeping/sweep-stale! nil) #(#'housekeeping/sweep-logs! nil)]]
        (let [logs (tmp-dir "vis-hk-empty-logs")
              fresh (io/file logs "2026-09-14/pyext-new")
              stale (io/file logs "2026-08-01/pyext-abandoned")]

          (.mkdirs fresh)
          (.mkdirs stale)
          (.setLastModified stale (long (age-ms 40)))
          (with-homes {:logs logs} sweep!)
          (expect (.isDirectory fresh))
          (expect (not (.exists (.getParentFile stale)))))))
  (it "never follows or deletes a symlink, so a link out of the root costs nothing"
      (let [logs
            (tmp-dir "vis-hk-logs-link")

            outside
            (touch! (tmp-dir "vis-hk-outside") "keep.txt" 400 "keep")

            link
            (io/file logs "ancient-link.log")]

        (Files/createSymbolicLink (.toPath link) (.toPath outside) (make-array FileAttribute 0))
        (with-homes {:logs logs} #(housekeeping/sweep-stale! nil))
        (expect (.exists outside))
        (expect (Files/exists (.toPath link) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))))
  ;; Regression: gateway journals were swept only by `gateway.bus/sweep!`, from
  ;; inside a running daemon's tailer loop, so the journals of every crashed,
  ;; kill-9'd or never-restarted daemon were immortal — nothing bounded them at
  ;; startup, which is exactly when no daemon is running.
  (it "deletes the journals of daemons that never came back and keeps a live one"
      (let [events (tmp-dir "vis-hk-events")]
        (touch! events "live.ndjson" 1 "{}")
        (touch! events "orphan.ndjson" 30 "{}")
        (let [report (target (with-homes {:events events} #(housekeeping/sweep-stale! nil))
                             :gateway-events)]
          (expect (= 2 (:file-count report)))
          (expect (= 1 (:deleted report)))
          (expect (= ["live.ndjson"] (mapv #(.getName ^File %) (.listFiles events)))))))
  (it "deletes display-cache pictures past the window and keeps the recent ones"
      (let [cache (tmp-dir "vis-hk-cache")]
        (touch! cache (str "display" File/separator "fig-old.png") 40 "old")
        (touch! cache (str "display" File/separator "fig-new.png") 3 "new")
        (let [report (target (with-homes {:cache cache} #(housekeeping/sweep-stale! nil)) :display)]
          (expect (= 1 (:deleted report)))
          (expect (= ["fig-new.png"]
                     (mapv #(.getName ^File %) (.listFiles (io/file cache "display"))))))))
  (it "sweeps the terminal-image cache by the same rule as the figure cache"
      (let [cache (tmp-dir "vis-hk-cache-tui")]
        (touch! cache (str "tui-attachments" File/separator "old.png") 45 "old")
        (touch! cache (str "tui-attachments" File/separator "new.png") 2 "new")
        (let [report (target (with-homes {:cache cache} #(housekeeping/sweep-stale! nil))
                             :tui-attachments)]
          (expect (= 1 (:deleted report)))
          (expect (= ["new.png"]
                     (mapv #(.getName ^File %) (.listFiles (io/file cache "tui-attachments"))))))))
  (it "drops the oldest pictures first when a cache is over its byte budget"
      (let [cache (tmp-dir "vis-hk-cache-budget")]
        (touch! cache (str "display" File/separator "fig-1.png") 9 "aaaaa")
        (touch! cache (str "display" File/separator "fig-2.png") 6 "bbbbb")
        (touch! cache (str "display" File/separator "fig-3.png") 3 "ccccc")
        (let [report (target (with-homes {:cache cache}
                                         #(housekeeping/sweep-stale! {:budget-bytes 10}))
                             :display)]
          (expect (= 1 (:over-budget-deleted report)))
          (expect (= 5 (:bytes report)))
          (expect (= #{"fig-2.png" "fig-3.png"}
                     (set (map #(.getName ^File %) (.listFiles (io/file cache "display")))))))))
  (it "degrades to zero work when none of the directories exist"
      (let [report (with-homes {:logs (io/file (tmp-dir "vis-hk-none") "nope")}
                               #(housekeeping/sweep-stale! nil))]
        (expect (zero? (:deleted report)))
        (expect (zero? (:bytes report)))
        (expect (= [:logs :gateway-events :display :tui-attachments :python-archives :python-guest
                    :python-pycache :sandbox-temp :python-runtime :python-sources]
                   (mapv :id (:targets report))))))
  (it "sweeps at startup and then repeats only diagnostic cleanup with bindings conveyed"
      ;; A startup-only sweep leaves logs behind when the daemon runs for weeks.
      (let [logs
            (tmp-dir "vis-hk-logs-async")

            events
            (tmp-dir "vis-hk-events-async")

            cache
            (tmp-dir "vis-hk-cache-async")

            python
            (tmp-dir "vis-hk-python-async")

            started
            (promise)

            repeated
            (promise)

            startup!
            housekeeping/sweep-stale!

            files!
            @#'housekeeping/sweep-files!]

        (touch! logs "old.log" 60 "old")
        (touch! logs "new.log" 1 "new")
        (with-redefs-fn {#'housekeeping/sweep-stale! (fn [opts]
                                                       (deliver started (startup! opts)))
                         #'housekeeping/sweep-files!
                         (fn [dir canon ^long cutoff]
                           (let [report (files! dir canon cutoff)]
                             (when (and (= dir logs) (realized? started) (pos? (:deleted report)))
                               (deliver repeated report))
                             report))}
          (fn []
            ;; Without bound-fn*, these seams would resolve to the real ~/.vis.
            (let [^Thread thread (with-homes {:logs logs :events events :cache cache :python python}
                                             #(housekeeping/sweep-stale-async! {:interval-ms 20}))]
              (try (expect (map? (deref started 5000 nil)))
                   (expect (.isDaemon thread))
                   (expect (= Thread/MIN_PRIORITY (.getPriority thread)))
                   (expect (= ["new.log"] (mapv #(.getName ^File %) (.listFiles logs))))
                   (let [journal (touch! events "keep.ndjson" 60 "session replay")
                         picture (touch! cache "display/keep.png" 60 "picture")
                         runtime-file (touch! python "runtime/old/keep.py" 60 "runtime")
                         stale (touch! logs "2026-08-01/pyext-worker/worker.log" 60 "old worker")]

                     (.setLastModified (.getParentFile runtime-file) (age-ms 60))
                     (expect (map? (deref repeated 5000 nil)))
                     (expect (not (.exists stale)))
                     (expect (.exists (io/file logs "new.log")))
                     (expect (every? #(.exists ^File %) [journal picture runtime-file])))
                   (finally (.interrupt thread) (.join thread 5000)))
              (expect (not (.isAlive thread))))))))
  (it "retries failed startup and periodic passes without keeping the process alive"
      (let [startup-calls
            (atom 0)

            log-calls
            (atom 0)

            recovered
            (promise)]

        (with-redefs-fn {#'housekeeping/sweep-stale! (fn [_]
                                                       (swap! startup-calls inc)
                                                       (throw (ex-info "Initial sweep failed" {})))
                         #'housekeeping/sweep-logs! (fn [_]
                                                      (if (= 1 (swap! log-calls inc))
                                                        (throw (ex-info "Periodic sweep failed" {}))
                                                        (deliver recovered true)))}
          (fn []
            (let [^Thread thread (with-homes {}
                                             #(housekeeping/sweep-stale-async! {:interval-ms 20}))]
              (try (expect (true? (deref recovered 5000 nil)))
                   (expect (= 1 @startup-calls))
                   (expect (>= @log-calls 2))
                   (finally (.interrupt thread) (.join thread 5000)))
              (expect (not (.isAlive thread))))))))
  (it
    "reclaims a stale unclaimed version while keeping the pinned, newest, recent and unrecognized stores"
    (let [python
          (tmp-dir "vis-hk-python")

          versions
          [[runtime/version 400] ["0.0.0" 400] ["9.9.9" 400] ["0.0.1" 2] ["dev" 400]]

          archive
          (touch! python "archives/old-runtime.tar.gz" 40 "tar")]

      ;; The pin, the newest install and an unrecognized name are never candidates,
      ;; and a candidate no process claims still needs the retention window to pass.
      (doseq [kind
              ["runtime" "sources"]

              [version days]
              versions]

        (touch! python (str kind "/" version "/lib/x.py") days "runtime")
        (.setLastModified (io/file python kind version) (age-ms days)))
      (let [report (with-homes {:python python} #(housekeeping/sweep-stale! nil))]
        (expect (= 1 (:deleted (target report :python-archives))))
        (expect (not (.exists archive)))
        (expect (pos? (long (:deleted (target report :python-runtime)))))
        (expect (pos? (long (:deleted (target report :python-sources))))))
      (doseq [kind
              ["runtime" "sources"]

              [version]
              versions]

        (if (= "0.0.0" version)
          (expect (not (.exists (io/file python kind version))))
          (expect (.isFile (io/file python kind version "lib/x.py")))))))
  (it "does not sweep through linked runtime or source stores"
      (let [python
            (tmp-dir "vis-hk-python-links")

            outside
            (tmp-dir "vis-hk-python-outside")]

        (doseq [kind ["runtime" "sources"]]
          (touch! outside (str kind "/0.0.0/lib/x.py") 400 "runtime")
          (.setLastModified (io/file outside kind "0.0.0") (age-ms 400))
          (Files/createSymbolicLink (.toPath (io/file python kind))
                                    (.toPath (io/file outside kind))
                                    (make-array FileAttribute 0)))
        (with-homes {:python python} #(housekeeping/sweep-stale! nil))
        (doseq [kind ["runtime" "sources"]]
          (expect (Files/isSymbolicLink (.toPath (io/file python kind))))
          (expect (.isFile (io/file outside kind "0.0.0/lib/x.py")))))))

(defdescribe
  runtime-retention-plan-test
  (it "previews release churn without deleting runtime candidates"
      (let [python
            (tmp-dir "vis-hk-release-churn")

            versions
            (mapv #(str "0.5." %) (range 17))]

        (doseq [kind
                ["runtime" "sources"]

                version
                versions]

          (touch! python (str kind "/" version "/library") 0 "runtime"))
        (with-homes
          {:python python}
          (fn []
            ;; Fresh candidates survive: nothing claims them, but the sweep only
            ;; reclaims an unclaimed tree once it is past the retention window.
            (let [swept
                  (housekeeping/sweep-stale! nil)

                  plan
                  (housekeeping/runtime-retention-plan {:runtime-version "0.5.15"})]

              (expect (zero? (:deleted swept)))
              (expect (true? (:is-dry-run plan)))
              (doseq [target (:targets plan)]
                (expect (= "0.5.16" (:latest-version target)))
                (expect (= #{"0.5.15" "0.5.16"} (set (map :version (:retained target)))))
                (expect (= 15 (count (:candidates target))))
                (expect (every? #(= :liveness-unverified (:reason %)) (:candidates target)))
                (expect (every? #(.exists (io/file (:root %))) (:candidates target)))))))))
  (it "retains unknown and linked directories, and reports missing stores without mutation"
      (let [python
            (tmp-dir "vis-hk-plan-unknown")

            outside
            (tmp-dir "vis-hk-plan-outside")

            link
            (io/file python "runtime" "9.9.9")]

        (touch! python "runtime/0.5.9/library" 0 "runtime")
        (touch! python "runtime/0.5.10/library" 0 "runtime")
        (touch! python "runtime/dev/library" 0 "runtime")
        (Files/createSymbolicLink (.toPath link) (.toPath outside) (make-array FileAttribute 0))
        (with-homes {:python python}
                    (fn []
                      (let [[runtime sources] (:targets (housekeeping/runtime-retention-plan
                                                          {:runtime-version "0.5.9"}))]
                        (expect (= "0.5.10" (:latest-version runtime)))
                        (expect (empty? (:candidates runtime)))
                        (expect (= #{:pinned :latest :unrecognized-version :linked-directory}
                                   (set (map :reason (:retained runtime)))))
                        (expect (empty? (:candidates sources)))
                        (expect (empty? (:retained sources)))
                        (expect (nil? (:latest-version sources)))
                        (expect (false? (:unavailable? sources)))
                        (expect (not (.exists (io/file python "sources"))))))))))

(defdescribe
  sweep-python-stores-test
  (it
    "keeps the guest generation this process claims and reclaims what nothing holds"
    (let [python
          (tmp-dir "vis-hk-guest")

          guest
          (io/file python "vis-guest")

          served-name
          "0.2.19-aaaaaaaaaaaa"

          abandoned-name
          "0.2.18-bbbbbbbbbbbb"

          legacy-name
          "7cf47097c39cf1afcee8b1d0c2a5f6e4d3b2a1908f7e6d5c4b3a291807f6e5d40"

          recent-name
          "1a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f8090"]

      ;; Every tree here is OLD on purpose. Age was the only signal this sweep
      ;; could have used, and it is the wrong one: a generation a daemon has
      ;; served for a month is untouched by definition.
      (touch! guest (str served-name "/vis_sdk.py") 30 "SDK = 1\n")
      (touch! guest (str served-name "/vis-ext-code7/gh.py") 30 "orphaned snapshot")
      (touch! guest (str abandoned-name "/vis_sdk.py") 30 "SDK = 0\n")
      (touch! guest (str legacy-name "/vis_results.py") 30 "before claims existed")
      (touch! guest (str recent-name "/vis_results.py") 1 "before claims, still recent")
      (touch! guest "vis_autoinstall.py" 30 "a release before content identity")
      (paths/claim-dir! (io/file guest served-name))
      ;; What a killed process leaves: the token file, with nobody holding it.
      (spit (io/file guest abandoned-name ".vis-live") "")
      (spit (io/file guest served-name "vis-ext-code7" ".vis-live") "")
      (with-homes {:python python}
                  (fn []
                    (let [row (target (housekeeping/sweep-stale! nil) :python-guest)]
                      (expect (pos? (long (:deleted row))))
                      (expect (pos? (long (:bytes row))))
                      (expect (.isFile (io/file guest served-name "vis_sdk.py")))
                      (expect (not (.exists (io/file guest served-name "vis-ext-code7"))))
                      (expect (not (.exists (io/file guest abandoned-name))))
                      (expect (not (.exists (io/file guest legacy-name))))
                      (expect (.isFile (io/file guest recent-name "vis_results.py")))
                      (expect (not (.exists (io/file guest "vis_autoinstall.py")))))))))
  (it "reclaims interpreter versions nothing claims, keeping the pinned, the newest and the served"
      (let [python
            (tmp-dir "vis-hk-runtime-claims")

            version-dir
            (fn [version]
              (io/file python "runtime" version))]

        (doseq [version ["0.4.0" "0.4.1" "0.5.9" runtime/version "9.9.9"]]
          (touch! python
                  (str "runtime/" version "/darwin-arm64/libvispython.dylib")
                  30
                  "interpreter"))
        (touch! python (str "sources/" runtime/version "/vis-python") 30 "sources")
        (paths/claim-dir! (version-dir "0.4.1"))
        (spit (io/file (version-dir "0.4.0") ".vis-live") "")
        (with-homes {:python python}
                    (fn []
                      (let [report (housekeeping/sweep-stale! nil)]
                        (expect (not (.exists (version-dir "0.4.0"))))
                        (expect (.isDirectory (version-dir "0.4.1")))
                        (expect (not (.exists (version-dir "0.5.9"))))
                        (expect (.isDirectory (version-dir runtime/version)))
                        (expect (.isDirectory (version-dir "9.9.9")))
                        (expect (pos? (long (:bytes (target report :python-runtime)))))
                        (expect (zero? (long (:deleted (target report :python-sources))))))))))
  (it
    "reclaims compiled bytecode whose source is gone and keeps what a live source still needs"
    (let [python
          (tmp-dir "vis-hk-pycache")

          sources
          (tmp-dir "vis-hk-pycache-src")

          mirror
          (fn [^File dir]
            (str "pycache/" (subs (.getCanonicalPath dir) 1)))

          live
          (io/file sources "live")

          emptied
          (io/file sources "emptied")

          vanished
          (io/file sources "vanished")]

      (touch! live "kept.py" 0 "SOURCE = 1\n")
      (.mkdirs emptied)
      ;; The entry a live source still wants is deliberately the OLDEST file here:
      ;; bytecode is not re-stamped when it is imported, so age judges it backwards.
      (touch! python (str (mirror live) "/kept.cpython-314.pyc") 90 "bytecode")
      (touch! python (str (mirror live) "/removed.cpython-314.opt-1.pyc") 0 "bytecode")
      (touch! python (str (mirror emptied) "/only.cpython-314.pyc") 0 "bytecode")
      (touch! python (str (mirror vanished) "/gone.cpython-314.pyc") 0 "bytecode")
      (with-homes
        {:python python}
        (fn []
          (let [row (target (housekeeping/sweep-stale! nil) :python-pycache)]
            (expect (pos? (long (:deleted row))))
            (expect (pos? (long (:bytes row))))
            (expect (.isFile (io/file python (str (mirror live) "/kept.cpython-314.pyc"))))
            (expect (not (.exists (io/file python
                                           (str (mirror live) "/removed.cpython-314.opt-1.pyc")))))
            (expect (not (.exists (io/file python (mirror emptied)))))
            (expect (not (.exists (io/file python (mirror vanished)))))
            (expect (.isDirectory (io/file sources "live"))))))))
  ;; Every process start also walked ~/.vis/logs: 127,455 files, 0.7 to 2.2 s and
  ;; 60 MB of allocation per start of any `vis-agent` command.
  (it "walks the diagnostic logs once per sweep interval across process starts"
      (let [logs
            (tmp-dir "vis-hk-logs-stamp")

            cache
            (tmp-dir "vis-hk-logs-stamp-cache")

            now
            (System/currentTimeMillis)

            swept
            (fn [at]
              (with-homes {:logs logs :cache cache}
                          #(:deleted (target (housekeeping/sweep-stale! {:now-ms at}) :logs))))]

        (touch! logs "first.log" 60 "old")
        (expect (= 1 (swept now)))
        (let [kept (touch! logs "second.log" 60 "old")]
          (expect (zero? (long (swept (+ now 60000)))))
          (expect (.isFile kept)))
        (expect (= 1 (swept (+ now (long housekeeping/default-log-sweep-interval-ms)))))))
  ;; Every process start walked the whole store: on a live machine 304,215 entries,
  ;; about 1.7 GB of allocation and 20 s of CPU per start of any `vis-agent` command.
  (it
    "walks the bytecode store once a day across process starts, and again after a clock moves back"
    (let [python
          (tmp-dir "vis-hk-pycache-stamp")

          cache
          (tmp-dir "vis-hk-pycache-stamp-cache")

          now
          (System/currentTimeMillis)

          orphan
          (fn []
            (touch! python "pycache/no/such/source/gone.cpython-314.pyc" 0 "bytecode"))

          swept
          (fn [at]
            (with-homes {:python python :cache cache}
                        #(:deleted (target (housekeeping/sweep-stale! {:now-ms at})
                                           :python-pycache))))]

      (orphan)
      (expect (pos? (long (swept now))))
      (let [kept (orphan)]
        (expect (zero? (long (swept (+ now 3600000)))))
        (expect (.isFile kept)))
      (expect (pos? (long (swept (+ now (long housekeeping/day-ms))))))
      (orphan)
      (expect (pos? (long (swept (- now 3600000)))))))
  ;; Regression, user report in this Vis session: the store reached 42 GB. A walk takes
  ;; minutes, and a short-lived command that claimed it exited mid-walk, so no process
  ;; walked the store again for a day.
  (it "walks the bytecode store again soon when the process that claimed it stopped mid-walk"
      (let [python
            (tmp-dir "vis-hk-pycache-cut")

            cache
            (tmp-dir "vis-hk-pycache-cut-cache")

            now
            (System/currentTimeMillis)

            gone
            (touch! python "pycache/no/such/source/gone.cpython-314.pyc" 0 "bytecode")]

        (with-homes {:python python :cache cache}
                    (fn []
                      (with-redefs-fn {#'housekeeping/sweep-pycache!
                                       (fn [& _]
                                         (throw (ex-info "process exited mid-walk" {})))}
                        #(try (housekeeping/sweep-stale! {:now-ms now})
                              (catch clojure.lang.ExceptionInfo _ nil)))
                      (let [row (target (housekeeping/sweep-stale! {:now-ms (+ now 3600000)})
                                        :python-pycache)]
                        (expect (pos? (long (:deleted row))))
                        (expect (not (.exists gone)))))))))

;; Regression, user report in this Vis session: only `vis-agent doctor --purge` removed
;; the temp files of killed snapshot writes, and only at the top of the folder.
(defdescribe
  sweep-sandbox-temps-test
  (it
    "deletes old temp files in every sandbox folder and keeps young ones and snapshots"
    (let [dir
          (tmp-dir "vis-hk-sandbox-temps")

          sid
          (str (random-uuid))

          old
          (mapv #(touch! dir % 30 "partial")
                [(str "." sid ".py.tmp-" (random-uuid))
                 (str sid ".spill" File/separator ".a.bin.tmp-" (random-uuid))
                 (str sid
                      ".turns" File/separator
                      "t1.spill" File/separator
                      ".index.json.tmp-" (random-uuid))])

          kept
          [(touch! dir (str sid ".turns" File/separator ".t2.py.tmp-" (random-uuid)) 0 "in flight")
           (touch! dir (str sid ".py") 30 "x = 1\n")
           (touch! dir (str sid ".turns" File/separator "t1.py") 30 "x = 1\n")]

          ;; A link temp keeps the old mtime of its source; its name holds its start.
          link
          (doto (io/file dir
                         (str sid ".turns")
                         (str ".t3.py.tmp-" (System/currentTimeMillis) "-" (random-uuid)))
            (-> .toPath
                (Files/createLink (.toPath (io/file dir (str sid ".py"))))))

          report
          (target (with-homes {:sandbox dir} #(housekeeping/sweep-stale! nil)) :sandbox-temp)]

      (expect (= 3 (:deleted report)))
      (expect (= (* 3 (count "partial")) (:bytes report)))
      (expect (not-any? #(.exists ^File %) old))
      (expect (every? #(.exists ^File %) kept))
      (expect (.exists link)))))
