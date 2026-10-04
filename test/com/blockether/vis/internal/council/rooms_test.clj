(ns com.blockether.vis.internal.council.rooms-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.rooms :as rooms]
            [com.blockether.vis.internal.council.transport :as transport]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(h/use-mem-store!)

(def ^:private relay "https://gateway.example.com")

(defn- identity-file
  "The private state file of one relay."
  ([home] (identity-file home relay))
  ([home origin]
   (.resolve (.toPath (io/file (str home) "rooms"))
             (str "relay-" (util/sha256-hex origin) ".json"))))

(defn- exists? [path] (Files/exists path (make-array java.nio.file.LinkOption 0)))

(defn- with-room
  [f]
  (let [home
        (Files/createTempDirectory "vis-rooms-test" (make-array FileAttribute 0))

        previous
        (System/getProperty "vis.rooms.home")

        room-id
        (str (random-uuid))

        machines
        (atom {})

        rooms
        (atom {})

        calls
        (atom [])

        db
        (h/store)

        pid
        (str (:id (store/db-create-project! db {:name "Rooms test"})))

        gid
        (str (:id (store/db-create-session-group! db pid {:name "Private group"})))

        sid
        (str (h/store-session! db {:channel :api}))]

    (System/setProperty "vis.rooms.home" (str home))
    (store/db-set-session-project! db sid pid)
    (store/db-set-session-group! db sid gid)
    (try
      (with-redefs [config/load-global-yaml-config-raw
                    (constantly {})

                    config/load-global-config-raw
                    (constantly {})

                    config/load-project-tiers-raw
                    (constantly {})

                    config/load-project-config-raw
                    (constantly {})

                    lp/db-info
                    (constantly db)

                    transport/call!
                    (fn [state method path params body query]
                      (swap! calls conj
                        {:method method
                         :path path
                         :params params
                         :body body
                         :query query
                         :relay (:relay_url state)})
                      (let [origin (:relay_url state)]
                        (case path
                          "/v1/rooms/machines"
                          (let [value {:machine_id (:machine_id state)
                                       :name (:name body)
                                       :can_create_rooms true
                                       :created_at 1}]
                            (swap! machines assoc origin value)
                            (swap! rooms assoc
                              origin
                              {:room_id (if (= relay origin) room-id (str (random-uuid)))
                               :name "Builds"
                               :owner_machine_id (:machine_id state)
                               :created_at 1})
                            value)

                          "/v1/rooms/join"
                          (let [value {:machine_id (:machine_id body)
                                       :name (:machine_name body)
                                       :can_create_rooms false
                                       :created_at 1}
                                joined {:room_id (str (random-uuid))
                                        :name "Guests"
                                        :owner_machine_id (str (random-uuid))
                                        :created_at 1}]

                            (swap! machines assoc origin value)
                            (swap! rooms assoc origin joined)
                            {:room joined :machine value})

                          "/v1/rooms/machine"
                          (if (= :patch method)
                            (get (swap! machines assoc-in [origin :name] (:name body)) origin)
                            (get @machines origin))

                          "/v1/rooms"
                          [(get @rooms origin)]

                          "/v1/rooms/machines/{machine_id}"
                          {:machine_id (:machine_id state) :deleted_rooms 1 :retained_history false}

                          "/v1/rooms/{room_id}/presence"
                          {:expires_at 1000}

                          "/v1/rooms/{room_id}/sessions"
                          []

                          nil)))]

        (store/db-council-set-machine-name! db "Laptop")
        (rooms/register! db {:relay_url relay :admin_token (apply str (repeat 43 "a"))})
        (f {:db db :sid sid :gid gid :pid pid :room-id room-id :home home :calls calls}))
      (finally (if previous
                 (System/setProperty "vis.rooms.home" previous)
                 (System/clearProperty "vis.rooms.home"))
               (rooms/register-settings! [])
               (doseq [file (reverse (file-seq (.toFile home)))]
                 (io/delete-file file true))))))

(defdescribe
  rooms-policy
  (describe "monotone restrictions"
            (it "retains the first explicit denial and its scope, but not the default denial"
                (let [spec
                      [{:id "wake"
                        :default false
                        :inheritance "restrict"
                        :scopes ["global" "project" "group" "session"]}]

                      layers
                      [{:scope "group" :values {"wake" false}}
                       {:scope "session" :values {"wake" true}}]

                      denied
                      (first (scoped/resolve-layers spec layers "session" {"wake" true}))

                      allowed
                      (first (scoped/resolve-layers spec [(last layers)] "session" {"wake" true}))]

                  (expect (= false (:value denied)))
                  (expect (= "group" (:source denied)))
                  (expect (true? (:is-override denied)))
                  (expect (= true (:value allowed)))))
            (it "requires explicit selection and prevents a session from widening its group"
                (with-room
                  (fn [{:keys [db sid gid room-id calls]}]
                    (expect (nil? (rooms/selected-room db sid)))
                    (store/db-set-scoped-setting! db "group" gid "council_room" room-id)
                    (expect (= room-id (rooms/selected-room db sid)))
                    (expect (not (rooms/wake-allowed? db sid)))
                    (store/db-set-scoped-setting! db "session" sid "council_room_wake" true)
                    (expect (rooms/wake-allowed? db sid))
                    (store/db-set-scoped-setting! db "group" gid "council_room_wake" false)
                    (expect (not (rooms/wake-allowed? db sid)))
                    (store/db-set-scoped-setting! db "group" gid (rooms/access-id room-id) false)
                    (store/db-set-scoped-setting! db "session" sid (rooms/access-id room-id) true)
                    (reset! calls [])
                    (expect (nil? (rooms/selected-room db sid)))
                    (expect (= 403
                               (try (rooms/operation! db sid room-id :members {})
                                    nil
                                    (catch clojure.lang.ExceptionInfo e (:status (ex-data e))))))
                    (expect (empty? @calls))
                    (expect (= gid (council/default-group db sid)))
                    (store/db-set-scoped-setting! db "group" gid (rooms/access-id room-id) nil)
                    (expect (= room-id (council/default-group db sid)))))))
  (describe "private durable state"
            (it "uses private permissions and preserves separate cursor keys across reloads"
                (with-room (fn [{:keys [db home sid room-id]}]
                             (rooms/advance! room-id sid :wake 7)
                             (rooms/advance! room-id sid :input 3)
                             (rooms/advance! room-id sid :wake 2)
                             (expect (= 7 (rooms/cursor room-id sid :wake)))
                             (expect (= 3 (rooms/cursor room-id sid :input)))
                             (expect (= 0 (rooms/cursor room-id (str (random-uuid)) :wake)))
                             (expect (= "rw-------"
                                        (PosixFilePermissions/toString
                                          (Files/getPosixFilePermissions
                                            (identity-file home)
                                            (make-array java.nio.file.LinkOption 0)))))
                             (let [status (rooms/status! db)]
                               (expect (document/valid? "rooms" "gateway_status" status))
                               (expect (not (contains? (first (:relays status)) :credential)))
                               (expect (not (contains? (first (:relays status)) :cursors)))))))
            (it "deletes the relay machine before it forgets the local credential"
                (with-room (fn [{:keys [db home calls]}]
                             (expect (= {:configured false :relays []}
                                        (rooms/delete-machine! db {:relay_url relay})))
                             (expect (= {:method :delete :path "/v1/rooms/machines/{machine_id}"}
                                        (select-keys (last @calls) [:method :path])))
                             (expect (not (exists? (identity-file home))))
                             (expect (= {:configured false :relays []} (rooms/status! db)))
                             (expect (= {:configured false :relays []}
                                        (rooms/delete-machine! db {:relay_url relay}))))))
            (it "keeps the credential when the relay cannot delete the machine"
                (with-room (fn [{:keys [db home]}]
                             (with-redefs [transport/call! (fn [& _]
                                                             (transport/fail! 503 "unavailable"))]
                               (expect (= 503
                                          (try (rooms/delete-machine! db {:relay_url relay})
                                               nil
                                               (catch clojure.lang.ExceptionInfo e
                                                 (:status (ex-data e)))))))
                             (expect (exists? (identity-file home))))))
            (it "forgets a credential that the relay already deleted"
                (with-room (fn [{:keys [db home]}]
                             (with-redefs [transport/call! (fn [& _]
                                                             (transport/fail! 401 "unauthorized"))]
                               (expect (= {:configured false :relays []}
                                          (rooms/delete-machine! db {:relay_url relay}))))
                             (expect (not (exists? (identity-file home)))))))
            (it "rejects secret-bearing malformed links without echoing them"
                (doseq [value ["https://user:secret@gateway.example.com/rooms/join#invite=bad"
                               "https://gateway.example.com/rooms/join#private-fragment"
                               "http://gateway.example.com/rooms/join#invite=secret"]]
                  (expect (= "Council Rooms request failed"
                             (try (transport/invite-parts value)
                                  nil
                                  (catch Exception e (ex-message e))))))))
  (describe "canonical boundaries"
            (it "resolves Council references and rejects unknown publication fields"
                (let [value {:session_id (str (random-uuid))
                             :publication {:kind "informational" :content "Ready"}}]
                  (expect (document/valid? "rooms" "publication" value))
                  (expect (not (document/valid?
                                 "rooms"
                                 "publication"
                                 (assoc-in value [:publication :credential] "private"))))))
            (it "uses the selected room without changing the persisted UI group"
                (with-room (fn [{:keys [db sid gid room-id calls]}]
                             (store/db-set-scoped-setting! db "session" sid "council_room" room-id)
                             (expect (= room-id (council/default-group db sid)))
                             (expect (= [] (council/members db (constantly {}) sid {})))
                             (expect (= gid (str (:group-id (store/db-get-session db sid)))))
                             (expect (= "/v1/rooms/{room_id}/sessions" (:path (last @calls)))))))))

(defdescribe
  rooms-delivery-lifecycle
  (it
    "claims a wake durably before dispatch and never consumes model input"
    (with-room
      (fn [{:keys [db sid room-id]}]
        (let [calls
              (atom [])

              wakes
              (atom 0)

              original
              transport/call!

              old-runtime
              @(var-get #'rooms/runtime)]

          (store/db-set-scoped-setting! db "session" sid "council_room" room-id)
          (store/db-set-scoped-setting! db "session" sid "council_room_wake" true)
          (try (rooms/install-runtime! (fn [_ _]
                                         {})
                                       (constantly true)
                                       (fn [_ _ entry]
                                         (expect (= (:entry_id entry)
                                                    (rooms/cursor room-id sid :wake)))
                                         (swap! wakes inc)
                                         (throw (ex-info "dispatch failed" {}))))
               (with-redefs [transport/call! (fn [state method path params body query]
                                               (if (= path "/v1/rooms/{room_id}/inbox")
                                                 (do (swap! calls conj (:after query))
                                                     {:entries
                                                      (if (< (:after query) 7) [{:entry_id 7}] [])})
                                                 (original state method path params body query)))]
                 (rooms/poll! db)
                 (rooms/poll! db)
                 (expect (= [0 7] @calls))
                 (expect (= 1 @wakes))
                 (expect (= 0 (rooms/cursor room-id sid :input))))
               (finally (reset! (var-get #'rooms/runtime) old-runtime)))))))
  (it "withdraws presence after a group disables its last shared session"
      (with-room (fn [{:keys [db sid gid room-id calls]}]
                   (store/db-set-scoped-setting! db "session" sid "council_room" room-id)
                   (rooms/poll! db)
                   (store/db-set-scoped-setting! db "group" gid (rooms/access-id room-id) false)
                   (reset! calls [])
                   (rooms/poll! db)
                   (expect (= [[]]
                              (mapv #(get-in % [:body :sessions])
                                    (filter #(= "/v1/rooms/{room_id}/presence" (:path %))
                                            @calls)))))))
  (it "rejects restrictive enum contributions at the canonical boundary"
      (expect (not (document/valid? "toggle"
                                    "contribution"
                                    {:id "room_policy"
                                     :label "Room policy"
                                     :type "enum"
                                     :choices ["local"]
                                     :default "local"
                                     :inheritance "restrict"})))))

(defdescribe
  room-input-receipts
  (it "keeps remote receipts remote after policy changes and defers relay failures"
      (doseq [selected [nil "room"]]
        (let [calls (atom [])
              input (atom {:key ["room" [1 0]]
                           :batch {:entries [{:entry_id 7}]}
                           :delivery {:room-id "room"}})]

          (with-redefs [rooms/selected-room (constantly selected)
                        rooms/operation! (fn [& _]
                                           (swap! calls conj :remote)
                                           (transport/fail! (if selected 503 403) "unavailable"))
                        rooms/advance! (fn [& _]
                                         (swap! calls conj :cursor))
                        store/db-council-delivered! (fn [& _]
                                                      (swap! calls conj :local))]

            (expect (nil? (try (council/acknowledge-input! nil
                                                           "session"
                                                           {:group-id "room" :input-state input}
                                                           [1 0])
                               (catch Exception _ :threw))))
            (expect (= [:remote] @calls))))))
  (it "retains a batch's room identity when access changes during lookup"
      (with-room
        (fn [{:keys [db sid gid room-id]}]
          (store/db-set-scoped-setting! db "session" sid "council_room" room-id)
          (let [input (atom {})]
            (with-redefs [rooms/operation!
                          (fn [_ _ _ operation _]
                            (case operation
                              :pending
                              []

                              :inbox
                              (do (store/db-set-scoped-setting! db
                                                                "group"
                                                                gid
                                                                (rooms/access-id room-id)
                                                                false)
                                  {:entries
                                   [{:entry_id 7 :thread_id 7 :content "Shared finding"}]})))]
              (expect (seq (:entries
                             (council/prepare-input! db sid "active" room-id input [1 0] 8192))))
              (expect (= room-id (get-in @input [:delivery :room-id])))))))))

(defdescribe
  rooms-settings-group
  (it "lists Council and its room settings in one Council group"
      (with-room
        (fn [{:keys [room-id]}]
          (let [groups
                (get (json/read-json (:body (#'settings-api/list-settings-handler {}))) "groups")

                council
                (first (filter #(= "council" (get % "id")) groups))]

            (expect (= "Council" (get council "title")))
            (expect
              (= {"council" "next_turn"
                  "council_room" "next_call"
                  "council_room_wake" "next_call"
                  (rooms/access-id room-id) "next_call"
                  rooms/machine-name-id "immediate"}
                 (into {} (map (juxt #(get % "id") #(get % "applies"))) (get council "toggles"))))
            (expect (= [rooms/machine-name-id "Laptop"]
                       ((juxt #(get % "id") #(get % "value")) (last (get council "toggles")))))
            (expect (= [council]
                       (filter #(some #{"council"}
                                      (map (fn [row]
                                             (get row "id"))
                                           (get % "toggles")))
                               groups)))
            (expect (not-any? #(= "council_rooms" (get % "id")) groups))))))
  (it "removes the access setting of a room that leaves the membership"
      (with-room (fn [{:keys [room-id]}]
                   (expect (some? (toggles/toggle-spec (rooms/access-id room-id))))
                   (rooms/register-settings! [])
                   (expect (nil? (toggles/toggle-spec (rooms/access-id room-id))))
                   (expect (some? (toggles/toggle-spec "council_room_wake")))))))

(defdescribe
  rooms-machine-name
  (describe
    "machine name"
    (it "saves the host name on first use and keeps it after the host changes"
        (with-room (fn [{:keys [db]}]
                     (rooms/delete-machine! db {:relay_url relay})
                     (h/raw-query db {:delete-from :council_machine})
                     (with-redefs [rooms/default-machine-name (constantly "Workstation")]
                       (expect (= "Workstation" (rooms/machine-name db)))
                       (expect (= "Workstation" (store/db-council-machine-name db))))
                     (with-redefs [rooms/default-machine-name (constantly "Other-host")]
                       (expect (= "Workstation" (rooms/machine-name db)))))))
    (it "saves a trimmed name and rejects a name that the relay refuses"
        (with-room (fn [{:keys [db]}]
                     (rooms/delete-machine! db {:relay_url relay})
                     (expect (= "Desk" (rooms/set-machine-name! db "  Desk  ")))
                     (expect (= "Desk" (rooms/machine-name db)))
                     (doseq [value [nil 7 "" "   " "Bad\u0007name"
                                    (apply str (repeat (inc rooms/machine-name-length) "a"))]]
                       (expect (= 400
                                  (try (rooms/set-machine-name! db value)
                                       nil
                                       (catch clojure.lang.ExceptionInfo e
                                         (:status (ex-data e)))))))
                     (expect (= "Desk" (store/db-council-machine-name db))))))
    (it "renames the machine on each connected relay and keeps its identity"
        (with-room (fn [{:keys [db calls]}]
                     (let [before (get-in (rooms/status! db) [:relays 0 :machine :machine_id])]
                       (expect (= "Desk" (rooms/set-machine-name! db "Desk")))
                       (expect (= {:method :patch :path "/v1/rooms/machine" :body {:name "Desk"}}
                                  (select-keys (last @calls) [:method :path :body])))
                       (expect (= "Desk" (store/db-council-machine-name db)))
                       (expect (= {:machine_id before :name "Desk"}
                                  (select-keys (get-in (rooms/status! db) [:relays 0 :machine])
                                               [:machine_id :name])))))))
    (it "saves the name when a relay does not answer and sends it at the next status check"
        (with-room (fn [{:keys [db calls]}]
                     (with-redefs [transport/call! (fn [& _]
                                                     (transport/fail! 503 "unavailable"))]
                       (expect (= "Desk" (rooms/set-machine-name! db "Desk"))))
                     (expect (= "Desk" (store/db-council-machine-name db)))
                     (reset! calls [])
                     (expect (= "Desk" (get-in (rooms/status! db) [:relays 0 :machine :name])))
                     (expect (some #(= [:patch "/v1/rooms/machine"] [(:method %) (:path %)])
                                   @calls)))))
    (it "registers with the saved name and the right to create rooms"
        (with-room (fn [{:keys [calls]}]
                     (expect (= {:name "Laptop" :can_create_rooms true}
                                (select-keys (:body (first @calls)) [:name :can_create_rooms])))
                     (expect (not-any? #(= "/v1/rooms/machines/{machine_id}" (:path %)) @calls)))))
    (it
      "keeps one machine identity for each relay"
      (with-room
        (fn [{:keys [db home room-id calls]}]
          (let [other
                "https://relay.example.com"

                joined
                (rooms/join! db
                             {:invite_url
                              (str other "/rooms/join#invite=" (apply str (repeat 43 "c")))})

                status
                (rooms/status! db)]

            (expect (= "Laptop"
                       (->> @calls
                            (filter #(= "/v1/rooms/join" (:path %)))
                            last
                            :body
                            :machine_name)))
            (expect (= [relay other] (mapv :relay_url (:relays status))))
            (expect (apply not= (map (comp :machine_id :machine) (:relays status))))
            (expect (every? exists? [(identity-file home) (identity-file home other)]))
            (expect (= #{room-id (get-in joined [:room :room_id])}
                       (set (map :room_id (rooms/known-rooms)))))
            (reset! calls [])
            (rooms/set-machine-name! db "Desk")
            (expect (= #{relay other}
                       (set (keep #(when (= [:patch "/v1/rooms/machine"] [(:method %) (:path %)])
                                     (:relay %))
                                  @calls))))
            (let [call transport/call!]
              (with-redefs [transport/call! (fn [state & args]
                                              (if (= other (:relay_url state))
                                                (transport/fail! 503 "unavailable")
                                                (apply call state args)))]
                (let [status (rooms/status! db)]
                  (expect (= [nil "unavailable"] (mapv :error (:relays status))))
                  (expect (= 2 (count (mapcat :rooms (:relays status))))))))
            (let [status (rooms/delete-machine! db {:relay_url other})]
              (expect (= [relay] (mapv :relay_url (:relays status))))
              (expect (= [room-id] (map :room_id (rooms/known-rooms))))
              (expect (not (exists? (identity-file home other))))
              (expect (exists? (identity-file home))))))))
    (it "reads and changes the machine name through the global settings"
        (with-room
          (fn [{:keys [db pid]}]
            (let [put!
                  (fn [params]
                    (#'settings-api/set-setting-handler
                     {:query-params (merge {"id" rooms/machine-name-id "action" "value"} params)}))

                  body
                  (fn [response]
                    (json/read-json (:body response)))]

              (expect (= "Laptop"
                         (get (body (#'settings-api/get-setting-handler
                                     {:path-params {:id rooms/machine-name-id}}))
                              "value")))
              (expect (= "Desk" (get (body (put! {"value" "Desk"})) "value")))
              (expect (= 400 (:status (put! {"action" "toggle"}))))
              (expect (= 400 (:status (put! {"value" "Desk" "scope" "project" "target_id" pid}))))
              (rooms/delete-machine! db {:relay_url relay})
              (with-redefs [rooms/default-machine-name (constantly "Workstation")]
                (expect (= "Desk" (get (body (put! {"value" "Desk"})) "value")))
                (let [reset (body (put! {"action" "inherit"}))]
                  (expect (= "Workstation" (get reset "value")))
                  (expect (false? (get reset "is_override")))))))))))
