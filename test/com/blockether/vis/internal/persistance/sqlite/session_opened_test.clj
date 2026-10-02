(ns com.blockether.vis.internal.persistance.sqlite.session-opened-test
  (:require [babashka.fs :as fs]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.persistance.sqlite.migration :as migration]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe describe it expect]]
            [next.jdbc :as jdbc]))

(h/use-mem-store!)

(defdescribe
  session-opening-storage-test
  (describe
    "durable explicit openings"
    (it "carries an opening through storage, loop records, wire rows and recent paging"
        (let [store
              (h/store)

              first-id
              (h/store-session! store {:channel :tui :title "First"})

              second-id
              (h/store-session! store {:channel :web :title "Second"})]

          (expect (nil? (:last-opened-at (p/db-get-session store first-id))))
          (with-redefs [lp/db-info (constantly store)]
            (let [opened (state/mark-session-opened! first-id)
                  stamp (get opened "last_opened_at")
                  date-stamp (java.util.Date. (long stamp))]

              (expect (some? stamp))
              (expect (= date-stamp (:last-opened-at (lp/by-id first-id))))
              (expect (= date-stamp
                         (:last-opened-at (first (filter #(= first-id (:id %))
                                                         (lp/by-channel :all))))))
              (let [head (state/list-sessions-page :all {:limit 1 :order :recent})
                    tail (state/list-sessions-page
                           :all
                           {:limit 1 :order :recent :after (:next-cursor head)})]

                (expect (= [(str first-id)] (mapv #(get % "id") (:sessions head))))
                (expect (= [(str second-id)] (mapv #(get % "id") (:sessions tail)))))
              ;; A soul read and a list refresh do not count as another selection.
              (state/soul first-id)
              (state/list-sessions-page :all {:order :recent})
              (expect (= date-stamp (:last-opened-at (p/db-get-session store first-id))))
              (expect (nil? (:last-opened-at (p/db-get-session store second-id))))))))
    (it "orders rapid selections monotonically even within one millisecond"
        (let [store
              (h/store)

              first-id
              (h/store-session! store {:channel :tui})

              second-id
              (h/store-session! store {:channel :web})]

          (with-redefs [util/now-ms (fn ^long []
                                      1000)]
            (let [first-stamp (p/db-mark-session-opened! store first-id)
                  second-stamp (p/db-mark-session-opened! store second-id)
                  reopened-stamp (p/db-mark-session-opened! store first-id)]

              (expect (= [1001 1002 1003]
                         (mapv #(.getTime ^java.util.Date %)
                               [first-stamp second-stamp reopened-stamp])))))))
    (it "does not create an opening for a missing session"
        (expect (nil? (p/db-mark-session-opened! (h/store) (random-uuid)))))
    (it "keeps the opening after a store is closed and reopened"
        (let [dir
              (fs/create-temp-dir {:prefix "vis-opened-recency-"})

              path
              (str dir "/vis.db")]

          (try (let [[sid stamp]
                     (let [store (p/db-create-connection! path)]
                       (try (let [sid (h/store-session! store {:channel :tui :title "Persisted"})]
                              [sid (p/db-mark-session-opened! store sid)])
                            (finally (p/db-dispose-connection! store))))

                     reopened
                     (p/db-create-connection! path)]

                 (try (expect (= stamp (:last-opened-at (p/db-get-session reopened sid))))
                      (expect (= stamp
                                 (:last-opened-at (first (p/db-list-sessions reopened :all)))))
                      (finally (p/db-dispose-connection! reopened))))
               (finally (fs/delete-tree dir)))))
    (it "adds the nullable opening clock to an existing canonical schema"
        (let [store
              (h/store)

              sid
              (h/store-session! store {:channel :tui :title "Existing"})

              datasource
              (:datasource store)]

          (jdbc/execute! datasource ["ALTER TABLE session_soul DROP COLUMN last_opened_at"])
          (migration/migrate! datasource ["db/sqlite/migration"])
          (expect (= "Existing" (:title (p/db-get-session store sid))))
          (expect (nil? (:last-opened-at (p/db-get-session store sid))))
          (expect (some? (p/db-mark-session-opened! store sid)))))))
