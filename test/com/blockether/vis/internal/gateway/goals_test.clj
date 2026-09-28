(ns com.blockether.vis.internal.gateway.goals-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.goals :as goals]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io ByteArrayOutputStream)))

(h/use-mem-store!)

(defdescribe
  goal-event-and-reconnect-snapshot-test
  (it
    "goal event and reconnect snapshot"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          emitted
          (atom [])]

      (with-redefs [lp/db-info
                    (constantly db)

                    state/append-event!
                    (fn [& args]
                      (swap! emitted conj args))]

        (let [goal (goals/set-goal! db sid "Visible on reconnect" nil)]
          (expect (= [sid "session.goal_updated" {:goal goal} {:store? false}] (first @emitted)))
          (expect (= goal (get (state/soul sid) "goal")))
          (expect (= goal (:goal (lp/by-id sid))))
          (expect (= goal (:goal (first (lp/by-channel :api)))))
          ;; Session introspection must expose the same durable goal, not a parallel API.
          (let [env {:db-info db :session-id sid}]
            (expect (= goal (:goal (#'introspection/foundation-session-descriptor env sid))))
            (expect (= goal (:goal (first (#'introspection/foundation-sessions-data env)))))
            (expect
              (= goal (get-in (#'introspection/foundation-inspect-data env sid) [:session :goal]))))
          (with-open [out (ByteArrayOutputStream.)]
            (#'server/sse-ready! out (str sid) 0 [] (state/soul sid))
            (let [frame (->> (str/split-lines (.toString out "UTF-8"))
                             (some #(when (str/starts-with? % "data:") (subs % 5)))
                             json/read-json)]
              (expect (str/includes? (.toString out "UTF-8") "event: subscription.ready"))
              (expect (= goal (get frame "goal")))))
          (expect (= goal
                     (get (first (:sessions (state/list-sessions-page :all
                                                                      {:ids [(str sid)]
                                                                       :dirty [(str sid)]})))
                          "goal"))))))))
