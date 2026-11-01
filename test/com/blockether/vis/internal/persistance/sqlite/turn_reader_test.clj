(ns com.blockether.vis.internal.persistance.sqlite.turn-reader-test
  (:require [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe it expect]]))

(h/use-mem-store!)

(defdescribe
  selective-turn-reader-test
  (it "reads exactly one canonical latest turn, with session ownership and missing-row checks"
      (let [db
            (h/store)

            sid
            (h/store-session! db {:channel :api})

            other
            (h/store-session! db {:channel :api})

            tid
            (persistence/db-store-session-turn! db {:parent-session-id sid :user-request "request"})

            answer
            [{"type" "prose" "markdown" "full answer"}]

            f
            (ns-resolve 'com.blockether.vis.internal.persistance.core 'db-read-session-turn)]

        (persistence/db-update-session-turn! db tid {:status :done :content answer})
        (expect (some? f))
        (when f
          (let [expected (first (persistence/db-list-session-turns db sid))]
            (with-redefs [persistence/db-list-session-turns (fn [& _]
                                                              (throw (ex-info "full scan" {})))]
              (expect (= expected (f db sid tid)))
              (expect (= answer (:content (f db sid tid))))
              (expect (nil? (f db other tid)))
              (expect (nil? (f db sid "missing")))
              (expect (nil? (f nil sid tid))))))))
  ;; The model context of each turn reads all prior turns at once. A stale state row or a
  ;; turn of another session there puts a wrong request or answer into the prompt.
  (it
    "reads many latest turns in one call, the same as one read for each turn"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          other
          (h/store-session! db {:channel :api})

          tids
          (mapv #(persistence/db-store-session-turn! db
                                                     {:parent-session-id sid
                                                      :user-request (str "request " %)})
                (range 3))

          foreign
          (persistence/db-store-session-turn! db
                                              {:parent-session-id other :user-request "foreign"})]

      (persistence/db-update-session-turn! db (first tids) {:status :running})
      (persistence/db-update-session-turn! db
                                           (first tids)
                                           {:status :done
                                            :content [{"type" "prose" "markdown" "done"}]})
      (let [rows (persistence/db-read-session-turns db sid (conj tids foreign "missing"))]
        (expect (= (set (map str tids)) (set (keys rows))))
        (expect (= (into {}
                         (map (fn [t]
                                [(str t) (persistence/db-read-session-turn db sid t)]))
                         tids)
                   rows))
        (expect (= :done (:status (get rows (str (first tids)))))))
      (expect (= {} (persistence/db-read-session-turns db sid [])))
      (expect (= {} (persistence/db-read-session-turns nil sid tids))))))
