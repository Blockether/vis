(ns com.blockether.vis.internal.council.rooms
  "Machine membership, scoped sharing and durable Council delivery through the relay."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.toggle :as toggle-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.transport :as transport]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.util :as util])
  (:import (java.nio.file Files Path LinkOption CopyOption StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(set! *warn-on-reflection* true)

(defonce ^:private state-lock (Object.))

(defonce ^:private runtime (atom nil))

(defonce ^:private presence-cache (atom {}))

(defn install-runtime!
  [snapshot eligible? wake!]
  (reset! runtime {:snapshot snapshot :eligible? eligible? :wake! wake!}))

(defn- state-path
  ^Path []
  (.toPath (io/file (or (System/getProperty "vis.rooms.home")
                        (System/getenv "VIS_HOME")
                        (str (System/getProperty "user.home") "/.vis"))
                    "rooms"
                    "identity.json")))

(defn read-state
  "Read private machine state. A missing file disables all background network work."
  []
  (let [path (state-path)]
    (when (or (Files/isSymbolicLink path) (Files/isSymbolicLink (.getParent path)))
      (transport/fail! 503 "unsafe_state"))
    (when (Files/exists path (make-array LinkOption 0))
      (let [value (wire/parse-json (slurp (.toFile path)))]
        (when-not (document/valid-json? "rooms" "local_state" value)
          (transport/fail! 503 "invalid_state"))
        (assoc (walk/keywordize-keys (dissoc value "cursors" "redemptions"))
          :cursors (get value "cursors")
          :redemptions (get value "redemptions"))))))

(defn- save-state!
  [value]
  (transport/validate! "local_state" value)
  (let [path
        (state-path)

        parent
        (.getParent path)

        permissions
        (PosixFilePermissions/fromString "rw-------")]

    (when (or (Files/isSymbolicLink parent) (Files/isSymbolicLink path))
      (transport/fail! 503 "unsafe_state"))
    (Files/createDirectories parent
                             (into-array FileAttribute
                                         [(PosixFilePermissions/asFileAttribute
                                            (PosixFilePermissions/fromString "rwx------"))]))
    (let [temp (Files/createTempFile
                 parent
                 "identity-"
                 ".json"
                 (into-array FileAttribute [(PosixFilePermissions/asFileAttribute permissions)]))]
      (try (spit (.toFile temp) (wire/json-str value))
           (Files/move temp
                       path
                       (into-array CopyOption
                                   [StandardCopyOption/ATOMIC_MOVE
                                    StandardCopyOption/REPLACE_EXISTING]))
           value
           (finally (Files/deleteIfExists temp))))))

(defn- update-state! [f] (locking state-lock (save-state! (f (read-state)))))

(defn- secret [] (util/base64url (util/random-bytes 32)))

(defn access-id [room-id] (str "council_room_" (str/replace room-id "-" "") "_access"))

(defn register-settings!
  "Membership permits selection. It never selects or shares a session."
  [rooms]
  (toggles/register-toggle! {:id "council_room"
                             :label "Council room"
                             :default "local"
                             :type :enum
                             :choices (into ["local"] (map :room_id rooms))
                             :description "Select a shared room, or keep Council on this machine."
                             :scopes toggle-contract/scopes
                             :persist? true
                             :group :council_rooms})
  (toggles/register-toggle!
    {:id "council_room_wake"
     :label "Allow room wake"
     :default false
     :inheritance "restrict"
     :scopes toggle-contract/scopes
     :description
     "Allow room messages to start idle sessions. An ancestor denial cannot be overridden."
     :persist? true
     :group :council_rooms})
  (doseq [room rooms]
    (toggles/register-toggle!
      {:id (access-id (:room_id room))
       :label (str "Allow " (:name room))
       :description
       "Permit this room. Selection is separate. An ancestor denial cannot be overridden."
       :default true
       :inheritance "restrict"
       :scopes toggle-contract/scopes
       :persist? true
       :group :council_rooms})))

(register-settings! [])

(defn selected-room
  "Resolve fresh restrictions. A denied or revoked room cannot receive session data."
  [db sid]
  (when-let [state (read-state)]
    (when (and (:machine state) (store/db-get-session db sid))
      (let [values (scoped/values db sid)
            id (get values "council_room")]

        (when (and (not= false (get values "council"))
                   (some #(= id (:room_id %)) (:rooms state))
                   (true? (get values (access-id id))))
          id)))))

(defn wake-allowed?
  [db sid]
  (and (selected-room db sid) (true? (get (scoped/values db sid) "council_room_wake"))))

(defn- authorize!
  [db sid room-id]
  (when-not (= room-id (selected-room db sid)) (transport/fail! 403 "room_denied")))

(defn- ensure-identity!
  [relay-url name]
  (let [base (transport/origin relay-url)]
    (update-state! (fn [old]
                     (when (and old (not= base (:relay_url old)))
                       (transport/fail! 409 "relay_conflict"))
                     (or old
                         {:relay_url base
                          :machine_id (str (random-uuid))
                          :credential (secret)
                          :name name
                          :rooms []
                          :cursors {}
                          :redemptions {}})))))

(defn- call!
  [method template params body query]
  (let [state (or (read-state) (transport/fail! 409 "rooms_unconfigured"))]
    (transport/call! state method template params body query)))

(defn status!
  "Return safe membership metadata. No key, invite, cursor or transcript is included."
  []
  (if-let [state (read-state)]
    (if (:machine state)
      (let [rooms (call! :get "/v1/rooms" {} nil {})
            machine (call! :get "/v1/rooms/machine" {} nil {})]

        (update-state! #(assoc %
                          :rooms rooms
                          :machine machine))
        (register-settings! rooms)
        {:configured true :relay_url (:relay_url state) :machine machine :rooms rooms})
      {:configured false :rooms []})
    {:configured false :rooms []}))

(defn register!
  [{:keys [relay_url name admin_token] :as body}]
  (transport/validate! "gateway_register" body)
  (let [state
        (ensure-identity! relay_url name)

        machine
        (transport/call! (assoc state :credential admin_token)
                         :post
                         "/v1/rooms/machines"
                         {}
                         {:machine_id (:machine_id state)
                          :name name
                          :credential (:credential state)
                          :can_create_rooms true}
                         {})]

    (update-state! #(assoc % :machine machine))
    (status!)))

(defn join!
  [{:keys [invite_url machine_name] :as body}]
  (transport/validate! "gateway_join" body)
  (let [{:keys [relay_url token]}
        (transport/invite-parts invite_url)

        _
        (ensure-identity! relay_url machine_name)

        key
        (util/sha256-hex token)

        state
        (update-state! #(if (get-in % [:redemptions key])
                          %
                          (update %
                                  :redemptions
                                  (fn [prior]
                                    (let [limit (get-in transport/schema
                                                        ["$defs" "local_state" "properties"
                                                         "redemptions" "maxProperties"])]
                                      (assoc (into {} (take (dec limit) prior))
                                        key (str (random-uuid))))))))

        result
        (call! :post
               "/v1/rooms/join"
               {}
               {:request_id (get-in state [:redemptions key])
                :invite_token token
                :machine_id (:machine_id state)
                :machine_name machine_name}
               {})]

    (update-state! #(assoc % :machine (:machine result)))
    (status!)
    result))

(defn create!
  [body]
  (transport/validate! "gateway_create" body)
  (let [result (call! :post
                      "/v1/rooms"
                      {}
                      {:room_id (str (random-uuid))
                       :name (:name body)
                       :owner_machine_id (:machine_id (read-state))}
                      {})]
    (status!)
    result))

(defn manage!
  "Apply a declared membership operation through the authenticated machine."
  [operation room-id id body]
  (transport/validate! "id" room-id)
  (let [params
        {:room_id room-id}

        result
        (case operation
          :delete
          (call! :delete "/v1/rooms/{room_id}" params nil {})

          :members
          (call! :get "/v1/rooms/{room_id}/members" params nil {})

          :remove
          (call! :delete
                 "/v1/rooms/{room_id}/members/{machine_id}"
                 (assoc params :machine_id id)
                 nil
                 {})

          :revoke
          (call! :delete
                 "/v1/rooms/{room_id}/invites/{invite_id}"
                 (assoc params :invite_id id)
                 nil
                 {})

          :invite
          (do (transport/validate! "gateway_invite" body)
              (call! :post
                     "/v1/rooms/{room_id}/invites"
                     params
                     {:invite_id (str (random-uuid))
                      :token (secret)
                      :expires_at (+ (util/now-ms) (* 1000 (long (:expires_in_seconds body 86400))))
                      :max_uses (:max_uses body 1)}
                     {})))]

    (when (#{:delete :remove} operation) (status!))
    result))

(defn delete-machine!
  "Delete this machine from the relay, then forget its credential. A relay failure keeps the credential."
  []
  (locking state-lock
    (when-let [state (read-state)]
      (try (transport/call! state
                            :delete
                            "/v1/rooms/machines/{machine_id}"
                            {:machine_id (:machine_id state)}
                            nil
                            {})
           (catch clojure.lang.ExceptionInfo e (when-not (= 401 (:status (ex-data e))) (throw e))))
      (Files/deleteIfExists (state-path))))
  (reset! presence-cache {})
  (register-settings! [])
  {:configured false :rooms []})

(defn- fleet
  [db]
  (let [active
        (if-let [snapshot (:snapshot @runtime)]
          (snapshot db nil)
          {})

        sessions
        (merge (into {}
                     (map (fn [row]
                            [(str (:id row)) row]))
                     (store/db-list-sessions db :all))
               (into {}
                     (map (fn [[sid _]]
                            [sid (store/db-get-session db sid)]))
                     active))]

    (for [[sid row]
          sessions

          :let [room
                (selected-room db sid)]
          :when room]

      {:room_id room
       :session_id sid
       :title (or (:title row) "")
       :state (or (get-in active [sid :state]) "idle")
       :wake_allowed (boolean (wake-allowed? db sid))})))

(defn sync-room!
  [db room-id]
  (let [sessions
        (mapv #(dissoc % :room_id) (filter #(= room-id (:room_id %)) (fleet db)))

        key
        [(str (state-path)) room-id]]

    (locking presence-cache
      (let [old
            (get @presence-cache key)

            now
            (util/now-ms)]

        (when (or (not= sessions (:sessions old)) (>= (- now (:at old 0)) 15000))
          (call! :post "/v1/rooms/{room_id}/presence" {:room_id room-id} {:sessions sessions} {})
          (swap! presence-cache assoc key {:sessions sessions :at now}))))))

(defn operation!
  "Recheck session policy at the transport boundary, including reads and receipts."
  [db sid room-id operation opts]
  (authorize! db sid room-id)
  (sync-room! db room-id)
  (authorize! db sid room-id)
  (let [params {:room_id room-id}]
    (case operation
      :members
      (call! :get "/v1/rooms/{room_id}/sessions" params nil {})

      :read
      (call! :get "/v1/rooms/{room_id}/entries" params nil (dissoc opts :group_id))

      :threads
      (call! :get "/v1/rooms/{room_id}/threads" params nil (dissoc opts :group_id))

      :get
      (call! :get
             "/v1/rooms/{room_id}/entries/{entry_id}"
             (assoc params :entry_id (:entry_id opts))
             nil
             {})

      :publish
      (call! :post
             "/v1/rooms/{room_id}/entries"
             params
             {:session_id sid
              :publication (assoc opts
                             :idempotency_key (or (:idempotency_key opts) (str (random-uuid))))}
             {})

      :wake
      (call! :post
             "/v1/rooms/{room_id}/wake"
             params
             {:session_id sid :event (dissoc opts :ping :group_id)}
             {})

      :pending
      (call! :get "/v1/rooms/{room_id}/pending" params nil {:session_id sid})

      :inbox
      (call! :get "/v1/rooms/{room_id}/inbox" params nil (assoc opts :session_id sid))

      :receipts
      (call! :post "/v1/rooms/{room_id}/receipts" params opts {}))))

(defn cursor
  [room-id sid kind]
  (get-in (read-state) [:cursors (str room-id "/" sid "/" (name kind))] 0))

(defn advance!
  [room-id sid kind entry-id]
  (update-state! #(update-in %
                             [:cursors (str room-id "/" sid "/" (name kind))]
                             (fn [old]
                               (max (long (or old 0)) (long entry-id))))))

(defn poll!
  "Claim each idle wake before dispatch. A crash cannot repeat a paid activation."
  [db]
  (when (:machine (read-state))
    (let [status
          (status!)

          by-room
          (group-by :room_id (fleet db))]

      (doseq [{room-id :room_id}
              (:rooms status)

              :let [sessions
                    (get by-room room-id)]]

        (sync-room! db room-id)
        (doseq [{:keys [session_id state wake_allowed]}
                sessions

                :when (and (= "idle" state) wake_allowed)]

          (let [page (operation! db
                                 session_id
                                 room-id
                                 :inbox
                                 {:after (cursor room-id session_id :wake)})]
            (doseq [entry (:entries page)]
              (when (and (wake-allowed? db session_id)
                         (if-let [eligible? (:eligible? @runtime)]
                           (eligible? db session_id)
                           false))
                (advance! room-id session_id :wake (:entry_id entry))
                (try (when-let [wake! (:wake! @runtime)]
                       (wake! db session_id entry))
                     (catch Exception _
                       (operation! db
                                   session_id
                                   room-id
                                   :receipts
                                   {:receipts [{:session_id session_id
                                                :entry_id (:entry_id entry)
                                                :state "unavailable"}]})))))))))))

(defn start!
  "Own one bounded poller for this gateway. Return its stop function."
  [db]
  (register-settings! (:rooms (read-state)))
  (let [running
        (atom true)

        job
        (future (while @running
                  (try (poll! db) (catch Exception _ nil))
                  (when @running (Thread/sleep 15000))))]

    (fn []
      (reset! running false)
      (future-cancel job))))
