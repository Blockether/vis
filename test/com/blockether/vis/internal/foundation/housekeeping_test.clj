(ns com.blockether.vis.internal.foundation.housekeeping-test
  "Stale-state accounting for `~/.vis/drafts`, `~/.vis/gateway/events` and the
   directories `sweep-stale!` deletes on its own.

   Everything here runs against throwaway directories bound through
   `workspace/*drafts-home*` and — for every sweep test, ALL FOUR at once via
   `with-homes` — `housekeeping/*logs-home*`, `*cache-home*`, `*python-home*`
   and `*events-home*`; no test may read, let alone delete,
   anything under the real `~/.vis`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute FileAttribute]))

;; Fixtures

(defn- tmp-dir
  ^File [prefix]
  (let [d (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (.deleteOnExit d)
    d))

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

(defn- draft-dir!
  "Create `<drafts>/<repo>/<name>` holding one file of the given age."
  ^File [^File drafts repo name age-days]
  (let [f (touch! drafts (str repo File/separator name File/separator "file.txt") age-days "x")]
    (.getParentFile f)))

(defn- kinds [report] (frequencies (map :kind (:reclaimable (:drafts report)))))

;; scan — drafts

(defdescribe
  scan-drafts-test
  (it "reports nothing when every draft directory is younger than the cutoff"
      (let [drafts (tmp-dir "vis-hk-fresh")]
        (draft-dir! drafts "repo" "ws-a" 1)
        (draft-dir! drafts "repo" "ws-b" 3)
        (binding [workspace/*drafts-home* (.getPath drafts)
                  housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (let [report (housekeeping/scan {:days 14})]
            (expect (zero? (:count report)))
            (expect (zero? (:bytes report)))
            (expect (= 2 (:dir-count (:drafts report))))))))
  (it "reports a directory with no workspace row as a stale orphan once it ages out"
      (let [drafts (tmp-dir "vis-hk-orphan")]
        (draft-dir! drafts "repo" "ancient" 40)
        (draft-dir! drafts "repo" "recent" 2)
        (binding [workspace/*drafts-home* (.getPath drafts)
                  housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (let [report (housekeeping/scan {:days 14})
                item (first (:reclaimable (:drafts report)))]

            (expect (= 1 (:count report)))
            (expect (= {:orphan 1} (kinds report)))
            (expect (= "ancient" (:label item)))
            (expect (>= (long (:age-days item)) 40))
            (expect (pos? (long (:bytes item))))))))
  (it "judges an orphan by its newest FILE, not by the directory's own timestamp"
      ;; A clone that is worked in daily can keep an old directory mtime, because
      ;; a directory's timestamp only moves when entries are added or removed.
      ;; Deleting such a tree would destroy live work.
      (let [drafts
            (tmp-dir "vis-hk-mtime")

            dir
            (draft-dir! drafts "repo" "busy" 90)]

        (touch! dir "fresh.txt" 0 "still in use")
        (.setLastModified dir (age-ms 90))
        (binding [workspace/*drafts-home*
                  (.getPath drafts)

                  housekeeping/*events-home*
                  (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 14})))))))
  (it "honours an explicit :days cutoff"
      (let [drafts (tmp-dir "vis-hk-days")]
        (draft-dir! drafts "repo" "ws" 20)
        (binding [workspace/*drafts-home* (.getPath drafts)
                  housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 30}))))
          (expect (= 1 (:count (housekeeping/scan {:days 7})))))))
  (it "ignores dot-entries, so the store's own internals are never reclaimed"
      (let [drafts (tmp-dir "vis-hk-dot")]
        (touch! drafts (str ".trash" File/separator "old" File/separator "f.txt") 99 "x")
        (touch! drafts (str ".fresh-seed" File/separator "seed" File/separator "f.txt") 99 "x")
        (binding [workspace/*drafts-home* (.getPath drafts)
                  housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 14}))))))))

(defdescribe scan-private-root-ownership-test
             (it "keeps extra-root ownership after the primary clone has been removed"
                 (doseq [state [:active :discarded]]
                   (let [drafts (tmp-dir "vis-hk-extra-roots")
                         first-copy (draft-dir! drafts "repo-a" "extra" 40)
                         second-copy (draft-dir! drafts "repo-b" "extra" 40)
                         row {:id "ws"
                              :root (.getPath (io/file drafts "repo" "removed-primary"))
                              :fork-ms 1
                              :state state
                              :last-focused-at-ms (System/currentTimeMillis)
                              :filesystem-roots [{:trunk (.getPath (io/file drafts "trunk-a"))
                                                  :clone (.getPath first-copy)
                                                  :policy :copy-and-apply}
                                                 {:trunk (.getPath (io/file drafts "trunk-b"))
                                                  :clone (.getPath second-copy)
                                                  :policy :copy-only}]}]

                     (binding [workspace/*drafts-home* (.getPath drafts)
                               housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-extra-ev"))]

                       (with-redefs [p/db-workspace-list-drafts (constantly [row])]
                         (let [items (get-in (housekeeping/scan {:db-info :fixture})
                                             [:drafts :reclaimable])]
                           (expect (= (if (= :discarded state) 2 0) (count items)))
                           (expect (every? #(= :discarded (:kind %)) items))
                           (expect (every? #(= "ws" (:workspace-id %)) items)))))))))

;; scan — gateway journals

(defdescribe scan-journals-test
             (it "reports only `.ndjson` journals older than the cutoff"
                 (let [events (tmp-dir "vis-hk-journals")]
                   (touch! events "old.ndjson" 30 "{}\n")
                   (touch! events "live.ndjson" 0 "{}\n")
                   (touch! events "notes.txt" 30 "not a journal")
                   (binding [workspace/*drafts-home* (.getPath (tmp-dir "vis-hk-d"))
                             housekeeping/*events-home* (.getPath events)]

                     (let [report (housekeeping/scan {:days 14})
                           journals (:journals report)]

                       (expect (= 2 (:file-count journals)))
                       (expect (= ["old.ndjson"] (mapv :label (:reclaimable journals))))
                       (expect (= 1 (:count report))))))))

;; purge!

(defdescribe
  purge-test
  (it "dry-run deletes nothing and still returns the full plan"
      (let [drafts
            (tmp-dir "vis-hk-dry")

            events
            (tmp-dir "vis-hk-dry-ev")

            dir
            (draft-dir! drafts "repo" "old" 40)]

        (touch! events "old.ndjson" 40 "{}\n")
        (binding [workspace/*drafts-home*
                  (.getPath drafts)

                  housekeeping/*events-home*
                  (.getPath events)]

          (let [report (housekeeping/purge! {:days 14 :is-dry-run true})]
            (expect (true? (:is-dry-run report)))
            (expect (= 2 (count (:purged report))))
            (expect (every? #(false? (:is-purged %)) (:purged report)))
            (expect (zero? (long (:reclaimed-bytes report))))
            (expect (.isDirectory dir))
            (expect (.exists (io/file events "old.ndjson")))))))
  (it "reclaims stale orphan trees and stale journals, and leaves fresh ones alone"
      (let [drafts
            (tmp-dir "vis-hk-purge")

            events
            (tmp-dir "vis-hk-purge-ev")

            old
            (draft-dir! drafts "repo" "old" 40)

            fresh
            (draft-dir! drafts "repo" "fresh" 1)]

        (touch! events "old.ndjson" 40 "{}\n")
        (touch! events "live.ndjson" 0 "{}\n")
        (binding [workspace/*drafts-home*
                  (.getPath drafts)

                  housekeeping/*events-home*
                  (.getPath events)]

          (let [report (housekeeping/purge! {:days 14})]
            (expect (false? (:is-dry-run report)))
            (expect (every? :is-purged (:purged report)))
            (expect (pos? (long (:reclaimed-bytes report))))
            (expect (not (.exists old)))
            (expect (not (.exists (io/file events "old.ndjson"))))
            (expect (.isDirectory fresh))
            (expect (.exists (io/file events "live.ndjson")))
            ;; A second pass finds nothing left to do.
            (expect (zero? (:count (housekeeping/purge! {:days 14}))))))))
  (it "never deletes outside the two roots it owns"
      ;; The `under?` guard is the only thing between a bad `:root` and an
      ;; arbitrary `rm -rf`, so it is asserted directly.
      (let [outside
            (tmp-dir "vis-hk-outside")

            victim
            (touch! outside "precious.txt" 99 "do not delete")

            drafts
            (tmp-dir "vis-hk-guard")]

        (binding [workspace/*drafts-home*
                  (.getPath drafts)

                  housekeeping/*events-home*
                  (.getPath (tmp-dir "vis-hk-ev"))]

          (housekeeping/purge! {:days 14}))
        (expect (.exists victim)))))

(defdescribe
  purge-backend-retry-test
  (it "routes stale and discarded retries through the backend and waits for removal"
      (doseq [kind [:stale :discarded]]
        (let [drafts (tmp-dir "vis-hk-backend")
              dir (draft-dir! drafts "repo" "retry" 40)
              calls (atom [])
              item {:kind kind :root (.getPath dir) :workspace-id "ws" :bytes 1}
              report {:drafts {:root (.getPath drafts) :reclaimable [item]}}
              completion (reify
                           clojure.lang.IBlockingDeref
                             (deref [_ timeout-ms _]
                               (expect (= 30000 timeout-ms))
                               (.delete (io/file dir "file.txt"))
                               (.delete dir)
                               true))]

          (with-redefs [housekeeping/scan (constantly report)
                        workspace/abandon! (fn [db opts]
                                             (swap! calls conj [db opts])
                                             {:discard-future completion})]

            (let [result (housekeeping/purge! {:db-info :fixture})]
              (expect (= [[:fixture {:workspace-id "ws" :reason :housekeeping}]] @calls))
              (expect (true? (:is-purged (first (:purged result)))))
              (expect (= 1 (:reclaimed-bytes result)))
              (expect (not (.exists dir))))))))
  (it
    "never bypasses stalled, refused or failed backend cleanup with raw deletion"
    (doseq [kind
            [:stale :discarded]

            outcome
            [:timeout :refused :failed :incomplete]]

      (let [drafts
            (tmp-dir "vis-hk-retain")

            dir
            (draft-dir! drafts "repo" "retry" 40)

            item
            {:kind kind :root (.getPath dir) :workspace-id "ws" :bytes 1}

            report
            {:drafts {:root (.getPath drafts) :reclaimable [item]}}

            completion
            (reify
              clojure.lang.IBlockingDeref
                (deref [_ timeout-ms timeout-value]
                  (expect (= 30000 timeout-ms))
                  (case outcome
                    :timeout
                    timeout-value

                    :failed
                    (throw (ex-info "Backend failed" {}))

                    true)))]

        (with-redefs [housekeeping/scan
                      (constantly report)

                      workspace/abandon!
                      (fn [_ _]
                        (if (= :refused outcome) {:status :refused} {:discard-future completion}))]

          (let [result (housekeeping/purge! {})]
            (expect (.exists (io/file dir "file.txt")))
            (expect (false? (:is-purged (first (:purged result)))))
            (expect (zero? (:reclaimed-bytes result)))))))))

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
  "Call `f` with ALL FOUR sweep seams pointed at throwaway directories. A test
   that bound only the seam it cares about would leave the other targets
   resolving to the operator's real `~/.vis` — and this sweep deletes."
  [{:keys [logs cache events python]} f]
  (binding [housekeeping/*logs-home*
            (seam-path logs "vis-hk-idle-logs")

            housekeeping/*cache-home*
            (seam-path cache "vis-hk-idle-cache")

            housekeeping/*python-home*
            (seam-path python "vis-hk-idle-python")

            housekeeping/*events-home*
            (seam-path events "vis-hk-idle-events")]

    (f)))

(defdescribe
  sweep-stale-test
  (it "keeps two weeks of every derived kind, under one number"
      (expect (= 14 housekeeping/default-retention-days))
      (expect (= (* 512 1024 1024) housekeeping/default-cache-budget-bytes)))
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
        (expect (= [:logs :gateway-events :display :tui-attachments :python-archives]
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
  (it "retains installed runtimes and sources regardless of age while removing stale archives"
      (let [python
            (tmp-dir "vis-hk-python")

            versions
            [[runtime/version 400] ["0.0.0" 400] ["9.9.9" 400] ["0.0.1" 2] ["dev" 400]]

            archive
            (touch! python "archives/old-runtime.tar.gz" 40 "tar")]

        ;; An install timestamp and this process's pin cannot prove that another
        ;; process has finished using a version, even after the retention window.
        (doseq [kind
                ["runtime" "sources"]

                [version days]
                versions]

          (touch! python (str kind "/" version "/lib/x.py") days "runtime")
          (.setLastModified (io/file python kind version) (age-ms days)))
        (let [report (with-homes {:python python} #(housekeeping/sweep-stale! nil))]
          (expect (= 1 (:deleted report)))
          (expect (= 1 (:deleted (target report :python-archives))))
          (expect (not (.exists archive)))
          (expect (not-any? #{:python-runtimes :python-sources} (map :id (:targets report)))))
        (doseq [kind
                ["runtime" "sources"]

                [version]
                versions]

          (expect (.isFile (io/file python kind version "lib/x.py"))))))
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
            ;; The advisory plan does not authorize startup deletion.
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
