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
