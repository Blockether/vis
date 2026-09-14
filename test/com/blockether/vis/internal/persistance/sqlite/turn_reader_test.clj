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
              (expect (nil? (f nil sid tid)))))))))
