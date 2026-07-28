(ns com.blockether.vis.internal.foundation.housekeeping-test
  "Stale-state accounting for `~/.vis/drafts` and `~/.vis/gateway/events`.

   Everything here runs against throwaway directories bound through
   `workspace/*drafts-home*` and `housekeeping/*events-home*`; no test may
   read — let alone delete — anything under the real `~/.vis`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; scan — drafts
;; ---------------------------------------------------------------------------

(defdescribe
  scan-drafts-test
  (it "reports nothing when every draft directory is younger than the cutoff"
      (let [drafts (tmp-dir "vis-hk-fresh")]
        (draft-dir! drafts "repo" "ws-a" 1)
        (draft-dir! drafts "repo" "ws-b" 3)
        (binding
          [workspace/*drafts-home* (.getPath drafts)
           housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (let [report (housekeeping/scan {:days 14})]
            (expect (zero? (:count report)))
            (expect (zero? (:bytes report)))
            (expect (= 2 (:dir-count (:drafts report))))))))
  (it "reports a directory with no workspace row as a stale orphan once it ages out"
      (let [drafts (tmp-dir "vis-hk-orphan")]
        (draft-dir! drafts "repo" "ancient" 40)
        (draft-dir! drafts "repo" "recent" 2)
        (binding
          [workspace/*drafts-home* (.getPath drafts)
           housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (let
            [report (housekeeping/scan {:days 14})
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
      (let
        [drafts
         (tmp-dir "vis-hk-mtime")

         dir
         (draft-dir! drafts "repo" "busy" 90)]

        (touch! dir "fresh.txt" 0 "still in use")
        (.setLastModified dir (- (System/currentTimeMillis) (* 90 day-ms)))
        (binding
          [workspace/*drafts-home*
           (.getPath drafts)

           housekeeping/*events-home*
           (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 14})))))))
  (it "honours an explicit :days cutoff"
      (let [drafts (tmp-dir "vis-hk-days")]
        (draft-dir! drafts "repo" "ws" 20)
        (binding
          [workspace/*drafts-home* (.getPath drafts)
           housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 30}))))
          (expect (= 1 (:count (housekeeping/scan {:days 7})))))))
  (it "ignores dot-entries, so the store's own internals are never reclaimed"
      (let [drafts (tmp-dir "vis-hk-dot")]
        (touch! drafts (str ".trash" File/separator "old" File/separator "f.txt") 99 "x")
        (touch! drafts (str ".fresh-seed" File/separator "seed" File/separator "f.txt") 99 "x")
        (binding
          [workspace/*drafts-home* (.getPath drafts)
           housekeeping/*events-home* (.getPath (tmp-dir "vis-hk-ev"))]

          (expect (zero? (:count (housekeeping/scan {:days 14}))))))))

;; ---------------------------------------------------------------------------
;; scan — gateway journals
;; ---------------------------------------------------------------------------

(defdescribe scan-journals-test
             (it "reports only `.ndjson` journals older than the cutoff"
                 (let [events (tmp-dir "vis-hk-journals")]
                   (touch! events "old.ndjson" 30 "{}\n")
                   (touch! events "live.ndjson" 0 "{}\n")
                   (touch! events "notes.txt" 30 "not a journal")
                   (binding
                     [workspace/*drafts-home* (.getPath (tmp-dir "vis-hk-d"))
                      housekeeping/*events-home* (.getPath events)]

                     (let
                       [report (housekeeping/scan {:days 14})
                        journals (:journals report)]

                       (expect (= 2 (:file-count journals)))
                       (expect (= ["old.ndjson"] (mapv :label (:reclaimable journals))))
                       (expect (= 1 (:count report))))))))

;; ---------------------------------------------------------------------------
;; purge!
;; ---------------------------------------------------------------------------

(defdescribe
  purge-test
  (it "dry-run deletes nothing and still returns the full plan"
      (let
        [drafts
         (tmp-dir "vis-hk-dry")

         events
         (tmp-dir "vis-hk-dry-ev")

         dir
         (draft-dir! drafts "repo" "old" 40)]

        (touch! events "old.ndjson" 40 "{}\n")
        (binding
          [workspace/*drafts-home*
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
      (let
        [drafts
         (tmp-dir "vis-hk-purge")

         events
         (tmp-dir "vis-hk-purge-ev")

         old
         (draft-dir! drafts "repo" "old" 40)

         fresh
         (draft-dir! drafts "repo" "fresh" 1)]

        (touch! events "old.ndjson" 40 "{}\n")
        (touch! events "live.ndjson" 0 "{}\n")
        (binding
          [workspace/*drafts-home*
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
      (let
        [outside
         (tmp-dir "vis-hk-outside")

         victim
         (touch! outside "precious.txt" 99 "do not delete")

         drafts
         (tmp-dir "vis-hk-guard")]

        (binding
          [workspace/*drafts-home*
           (.getPath drafts)

           housekeeping/*events-home*
           (.getPath (tmp-dir "vis-hk-ev"))]

          (housekeeping/purge! {:days 14}))
        (expect (.exists victim)))))

;; ---------------------------------------------------------------------------
;; format-bytes
;; ---------------------------------------------------------------------------

(defdescribe format-bytes-test
             (it "renders locale-stable, unit-scaled sizes"
                 (expect (= "512 B" (housekeeping/format-bytes 512)))
                 (expect (= "1.0 KB" (housekeeping/format-bytes 1024)))
                 (expect (= "1.0 MB" (housekeeping/format-bytes (* 1024 1024))))
                 (expect (= "2.5 GB" (housekeeping/format-bytes (long (* 2.5 1024 1024 1024)))))))

;; ---------------------------------------------------------------------------
;; sweep-logs! — the one self-deleting surface
;; ---------------------------------------------------------------------------

(defdescribe sweep-logs-test
             (it "keeps the retention window at three weeks"
                 (expect (= 21 housekeeping/default-log-retention-days)))
             (it "deletes only files older than the retention window"
                 (let [logs (tmp-dir "vis-hk-logs")]
                   (touch! logs "vis-nrepl-fresh.log" 1 "fresh")
                   (touch! logs "vis-nrepl-edge.log" 20 "edge")
                   (touch! logs "vis-nrepl-old.log" 22 "old-content")
                   (touch! logs "vis-nrepl-ancient.log" 400 "ancient")
                   (binding [housekeeping/*logs-home* (.getPath logs)]
                     (let [report (housekeeping/sweep-logs! nil)]
                       (expect (= 4 (:file-count report)))
                       (expect (= 2 (:deleted report)))
                       (expect (= (+ (count "old-content") (count "ancient")) (:bytes report)))
                       (expect (= #{"vis-nrepl-fresh.log" "vis-nrepl-edge.log"}
                                  (set (map #(.getName ^File %) (.listFiles logs)))))))))
             (it "honours an explicit :days window"
                 (let [logs (tmp-dir "vis-hk-logs-days")]
                   (touch! logs "a.log" 5 "a")
                   (touch! logs "b.log" 30 "b")
                   (binding [housekeeping/*logs-home* (.getPath logs)]
                     (expect (= 2 (:deleted (housekeeping/sweep-logs! {:days 1}))))
                     (expect (zero? (count (.listFiles logs)))))))
             (it "leaves subdirectories alone however stale they are"
                 (let
                   [logs
                    (tmp-dir "vis-hk-logs-dir")

                    nested
                    (touch! logs (str "keep-me" File/separator "inner.log") 90 "x")

                    dir
                    (.getParentFile nested)]

                   (.setLastModified dir (- (System/currentTimeMillis) (* 90 day-ms)))
                   (binding [housekeeping/*logs-home* (.getPath logs)]
                     (expect (zero? (:deleted (housekeeping/sweep-logs! nil))))
                     (expect (.isDirectory dir))
                     (expect (.exists nested)))))
             (it "degrades to zero work when the logs directory does not exist"
                 (let [missing (io/file (tmp-dir "vis-hk-logs-none") "nope")]
                   (binding [housekeeping/*logs-home* (.getPath missing)]
                     (let [report (housekeeping/sweep-logs! nil)]
                       (expect (zero? (:file-count report)))
                       (expect (zero? (:deleted report)))
                       (expect (zero? (:bytes report)))))))
             (it "sweeps off-thread with the caller's bindings conveyed"
                 (let [logs (tmp-dir "vis-hk-logs-async")]
                   (touch! logs "old.log" 60 "old")
                   (touch! logs "new.log" 1 "new")
                   (binding [housekeeping/*logs-home* (.getPath logs)]
                     ;; `bound-fn*` in `sweep-logs-async!` is what keeps this temp-dir
                     ;; binding visible to the sweeper thread; without it the thread would
                     ;; fall back to the root binding and sweep the REAL `~/.vis/logs`.
                     (.join ^Thread (housekeeping/sweep-logs-async! nil) 5000))
                   (expect (= ["new.log"] (mapv #(.getName ^File %) (.listFiles logs)))))))
