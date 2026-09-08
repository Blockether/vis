(ns com.blockether.vis.internal.council.host-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store!)

(deftest publication-survives-presentation-failure-test
  ;; C24/C25: provenance is trusted, canonical and independent of stdout or Activity IO.
  (foundation/register!)
  (let [db
        (h/store)

        sid
        (str (h/store-session! db {:channel :api}))

        gid
        (str (:id (ps/db-create-project! db {:name "Council host"})))

        activation
        (str (random-uuid))

        env
        {:session-id sid
         :db-info db
         :ctx-atom (atom {:council-actor {:activation-id activation}})
         :turn-state-atom (atom {:turn-position 1 :iteration 1 :form-idx 0})}]

    (ps/db-set-session-project! db sid gid)
    (with-redefs [toggles/enabled?
                  (constantly true)

                  council/runtime
                  (constantly
                    {sid {:activation-id activation :group-id gid :state "running" :title "Host"}})

                  extension/publish-activity!
                  (fn [_]
                    (throw (ex-info "fixture presentation unavailable" {})))]

      (binding [extension/*current-invocation-id* "fixture-op"]
        (let [result (host/publish env "Published without stdout" {"idempotency_key" "retry"})
              entry (:result result)
              replay (:result
                       (host/publish env "Published without stdout" {"idempotency_key" "retry"}))]

          (is (= entry replay))
          (is (document/valid-json? "council" "entry" entry))
          (is (= "fixture-op" (get-in entry ["source_ref" "operation_id"])))
          (is (= 1 (count (:entries (council/read-entries db sid {})))))
          (is (seq (:council-publications @(:ctx-atom env))))
          (doseq [ref (get-in result [:metadata :activity/resources])]
            (is (document/valid? "council" "activity_resource" ref))))))))

(deftest disabled-host-metadata-test
  (with-redefs [toggles/enabled? (constantly false)]
    (is (nil? (host/context {})))
    (is (nil? (council/prompt {})))
    (is (every? #(false? ((:ext.symbol/active-fn %) {})) host/symbols))))
