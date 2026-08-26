(ns com.blockether.vis.internal.foundation.housekeeping-test
  "Stale-state accounting for `~/.vis/drafts`, `~/.vis/gateway/events` and the
   directories `sweep-stale!` deletes on its own.

   Everything here runs against throwaway directories bound through
   `workspace/*drafts-home*` and — for every sweep test, ALL FOUR at once via
   `with-homes` — `housekeeping/*logs-home*`, `*cache-home*`, `*rewind-home*`
   and `*events-home*`; no test may read, let alone delete, anything under the
   real `~/.vis`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.workspace :as workspace]
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

(defn- touch!
  "Write `content` to `path` and stamp it `age-days` old. The mtime is what
   staleness is judged on, so tests state the age directly instead of sleeping."
  ^File [^File dir path age-days content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)
    (.setLastModified f (- (System/currentTimeMillis) (* (long age-days) day-ms)))
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
        (.setLastModified dir (- (System/currentTimeMillis) (* 90 day-ms)))
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

;; sweep-stale! — the self-deleting surface

(defn- target
  "The one target report named `id` inside a `sweep-stale!` result."
  [report id]
  (first (filter #(= id (:id %)) (:targets report))))

(defn- with-homes
  "Call `f` with ALL FOUR sweep seams pointed at throwaway directories. A test
   that bound only the seam it cares about would leave the other targets
   resolving to the operator's real `~/.vis` — and this sweep deletes."
  [{:keys [logs cache rewind events]} f]
  (binding [housekeeping/*logs-home*
            (.getPath ^File (or logs (tmp-dir "vis-hk-idle-logs")))

            housekeeping/*cache-home*
            (.getPath ^File (or cache (tmp-dir "vis-hk-idle-cache")))

            housekeeping/*rewind-home*
            (.getPath ^File (or rewind (tmp-dir "vis-hk-idle-rewind")))

            housekeeping/*events-home*
            (.getPath ^File (or events (tmp-dir "vis-hk-idle-events")))]

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
  (it "honours an explicit :days window"
      (let [logs (tmp-dir "vis-hk-logs-days")]
        (touch! logs "a.log" 5 "a")
        (touch! logs "b.log" 30 "b")
        (expect (= 2
                   (:deleted (target (with-homes {:logs logs}
                                                 #(housekeeping/sweep-stale! {:days 1}))
                                     :logs))))
        (expect (zero? (count (.listFiles logs))))))
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
  (it "deletes a whole rewind store once its newest file has aged out, and leaves a live one whole"
      (let [rewind
            (tmp-dir "vis-hk-rewind")

            dead
            (touch! rewind (str "dead-session" File/separator "journal.ndjson") 30 "{}")

            live
            (touch! rewind (str "live-session" File/separator "journal.ndjson") 30 "{}")

            blob
            (touch! rewind
                    (str "live-session" File/separator "objects" File/separator "blob")
                    1
                    "fresh")

            report
            (target (with-homes {:rewind rewind} #(housekeeping/sweep-stale! nil)) :rewind)]

        (expect (= 2 (:file-count report)))
        (expect (= 1 (:deleted report)))
        (expect (not (.exists (.getParentFile dead))))
        (expect (.exists live))
        (expect (.exists blob))))
  (it "degrades to zero work when none of the directories exist"
      (let [report (with-homes {:logs (io/file (tmp-dir "vis-hk-none") "nope")}
                               #(housekeeping/sweep-stale! nil))]
        (expect (zero? (:deleted report)))
        (expect (zero? (:bytes report)))
        (expect (= [:logs :gateway-events :display :tui-attachments :rewind]
                   (mapv :id (:targets report))))))
  (it "sweeps off-thread with the caller's bindings conveyed"
      (let [logs (tmp-dir "vis-hk-logs-async")]
        (touch! logs "old.log" 60 "old")
        (touch! logs "new.log" 1 "new")
        ;; `bound-fn*` in `sweep-stale-async!` is what keeps these temp-dir
        ;; bindings visible to the sweeper thread; without it the thread would
        ;; fall back to the root bindings and sweep the REAL `~/.vis`.
        (with-homes {:logs logs} #(.join ^Thread (housekeeping/sweep-stale-async! nil) 5000))
        (expect (= ["new.log"] (mapv #(.getName ^File %) (.listFiles logs)))))))
