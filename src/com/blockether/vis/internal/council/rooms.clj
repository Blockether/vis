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
  (:import (java.net InetAddress)
           (java.nio.file Files Path LinkOption CopyOption StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(set! *warn-on-reflection* true)

(defonce ^:private state-lock (Object.))

(defonce ^:private runtime (atom nil))

(defonce ^:private presence-cache (atom {}))

(defn install-runtime!
  [snapshot eligible? wake!]
  (reset! runtime {:snapshot snapshot :eligible? eligible? :wake! wake!}))

(defn- rooms-dir
  ^Path []
  (.toPath (io/file (or (System/getProperty "vis.rooms.home")
                        (System/getenv "VIS_HOME")
                        (str (System/getProperty "user.home") "/.vis"))
                    "rooms")))

(defn- state-path
  "Each relay has one private file. The file name is a hash of the relay origin."
  ^Path [relay-url]
  (.resolve (rooms-dir) (str "relay-" (util/sha256-hex relay-url) ".json")))

(defn- load-state
  [^Path path]
  (when (or (Files/isSymbolicLink path) (Files/isSymbolicLink (.getParent path)))
    (transport/fail! 503 "unsafe_state"))
  (when (Files/exists path (make-array LinkOption 0))
    (let [value (wire/parse-json (slurp (.toFile path)))]
      (when-not (document/valid-json? "rooms" "local_state" value)
        (transport/fail! 503 "invalid_state"))
      (assoc (walk/keywordize-keys (dissoc value "cursors" "redemptions"))
        :cursors (get value "cursors")
        :redemptions (get value "redemptions")))))

(defn read-state
  "Read the private machine state of one relay origin. A relay without a file gets no network work."
  [relay-url]
  (load-state (state-path relay-url)))

(defn- identities
  "Read the machine state of each relay, ordered by relay origin. A file must match its origin."
  []
  (let [dir (rooms-dir)]
    (if (Files/isDirectory dir (make-array LinkOption 0))
      (->> (with-open [paths (Files/newDirectoryStream dir "relay-*.json")]
             (vec paths))
           (keep (fn [^Path path]
                   (when-let [state (load-state path)]
                     (when (= path (state-path (:relay_url state))) state))))
           (sort-by :relay_url)
           vec)
      [])))

(defn- save-state!
  [value]
  (transport/validate! "local_state" value)
  (let [path
        (state-path (:relay_url value))

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
                 "pending-"
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

(defn- update-state!
  "Change the saved state of a known relay. A relay without a file stays unknown."
  [relay-url f]
  (locking state-lock
    (when-let [state (read-state relay-url)]
      (save-state! (f state)))))

(defn- secret [] (util/base64url (util/random-bytes 32)))

(defn access-id [room-id] (str "council_room_" (str/replace room-id "-" "") "_access"))

(defn setting?
  "True for the room selection, wake and access settings. They apply on the next Council call."
  [id]
  (str/starts-with? id "council_room"))

(defn register-settings!
  "Membership permits selection. It never selects or shares a session.
   A room that leaves the membership loses its access setting."
  [rooms]
  (toggles/register-toggle! {:id "council_room"
                             :label "Council room"
                             :default "local"
                             :type :enum
                             :choices (into ["local"] (map :room_id rooms))
                             :description "Select a shared room, or keep Council on this machine."
                             :scopes toggle-contract/scopes
                             :persist? true
                             :group :council
                             :parent "council"})
  (toggles/register-toggle!
    {:id "council_room_wake"
     :label "Allow room wake"
     :default false
     :inheritance "restrict"
     :scopes toggle-contract/scopes
     :description
     "Allow room messages to start idle sessions. An ancestor denial cannot be overridden."
     :persist? true
     :group :council
     :parent "council"})
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
       :group :council
       :parent "council_room"}))
  (let [current (set (map (comp access-id :room_id) rooms))]
    (doseq [{:keys [id]} (toggles/registered-toggles)
            :when (and (setting? id) (str/ends-with? id "_access") (not (current id)))]

      (toggles/unregister-toggle! id))))

(register-settings! [])

(defn- ensure-identity!
  "Return the machine state of a relay origin. The first use creates a machine ID and credential."
  [relay-url]
  (locking state-lock
    (or (read-state relay-url)
        (save-state! {:relay_url relay-url
                      :machine_id (str (random-uuid))
                      :credential (secret)
                      :rooms []
                      :cursors {}
                      :redemptions {}}))))

(defn- unique-rooms
  "Keep the first room of each room ID."
  [rooms]
  (:rooms (reduce (fn [found {:keys [room_id] :as room}]
                    (if (contains? (:ids found) room_id)
                      found
                      {:ids (conj (:ids found) room_id) :rooms (conj (:rooms found) room)}))
                  {:ids #{} :rooms []}
                  rooms)))

(defn known-rooms
  "The saved rooms of all connected relays. Room IDs are random, so the first relay wins a duplicate."
  []
  (unique-rooms (mapcat :rooms (filter :machine (identities)))))

(defn- room-state
  "The state of the connected relay that holds a room, or nil."
  [room-id]
  (some (fn [state]
          (when (and (:machine state) (some #(= room-id (:room_id %)) (:rooms state))) state))
        (identities)))

(defn selected-room
  "Resolve fresh restrictions. A denied or revoked room cannot receive session data."
  [db sid]
  (when-let [rooms (seq (known-rooms))]
    (when (store/db-get-session db sid)
      (let [values (scoped/values db sid)
            id (get values "council_room")]

        (when (and (not= false (get values "council"))
                   (some #(= id (:room_id %)) rooms)
                   (true? (get values (access-id id))))
          id)))))

(defn wake-allowed?
  [db sid]
  (and (selected-room db sid) (true? (get (scoped/values db sid) "council_room_wake"))))

(defn- authorize!
  [db sid room-id]
  (when-not (= room-id (selected-room db sid)) (transport/fail! 403 "room_denied")))

(defn- room-call!
  "Call the relay that holds a room. An unknown room fails before any network call."
  [room-id method template params body query]
  (transport/call! (or (room-state room-id) (transport/fail! 404 "room_unknown"))
                   method
                   template
                   params
                   body
                   query))

(def machine-name-id
  "The setting id of the name that rooms show for this machine."
  "council_machine_name")

(def machine-name-length
  "The longest machine name that the relay accepts."
  (get-in transport/schema ["$defs" "machine_registration" "properties" "name" "maxLength"]))

(defn- machine-name? [value] (document/valid? "rooms" "machine_registration/properties/name" value))

(defonce ^:private host-label
  (delay (let [label (some-> (or (try (.getHostName (InetAddress/getLocalHost))
                                      (catch Exception _ nil))
                                 (System/getenv "HOSTNAME")
                                 (System/getenv "COMPUTERNAME"))
                             str/trim
                             (str/split #"\.")
                             first)]
           (when (and (machine-name? label) (not (re-matches #"[0-9]+" label))) label))))

(defn default-machine-name
  "This computer's host name without its domain, or Vis when the host has no usable name."
  []
  (or @host-label "Vis"))

(defn machine-name
  "The name that rooms show for this machine on each relay. The first use saves the host name."
  [db]
  (or (store/db-council-machine-name db)
      (let [value (default-machine-name)]
        (store/db-council-set-machine-name! db value)
        value)))

(defn- rename!
  "Give the relay machine the saved name. The relay keeps the machine ID and credential."
  [state name]
  (let [machine (transport/call! state :patch "/v1/rooms/machine" {} {:name name} {})]
    (update-state! (:relay_url state) #(assoc % :machine machine))
    machine))

(defn set-machine-name!
  "Save the name that rooms show for this machine, send it to each connected relay and return it.
   A relay that does not answer gets the name at the next status check."
  [db value]
  (let [value (when (string? value) (str/trim value))]
    (when-not (machine-name? value)
      (throw (ex-info
               (str "Use 1 to " machine-name-length " characters, without control characters.")
               {:status 400 :type :invalid-setting-value :id machine-name-id})))
    (store/db-council-set-machine-name! db value)
    (doseq [state (identities)
            :when (and (:machine state) (not= value (get-in state [:machine :name])))]

      (try (rename! state value) (catch clojure.lang.ExceptionInfo _ nil)))
    value))

(defn- relay-status!
  "Read the rooms and the machine of one relay, and send the saved name when the relay has another.
   A relay that does not answer keeps its saved rooms and reports the error code."
  [state name]
  (try (let [rooms
             (transport/call! state :get "/v1/rooms" {} nil {})

             machine
             (transport/call! state :get "/v1/rooms/machine" {} nil {})

             machine
             (if (= name (:name machine)) machine (rename! state name))]

         (update-state! (:relay_url state)
                        #(assoc %
                           :rooms rooms
                           :machine machine))
         {:relay_url (:relay_url state) :machine machine :rooms rooms})
       (catch clojure.lang.ExceptionInfo e
         {:relay_url (:relay_url state)
          :machine (:machine state)
          :rooms (:rooms state)
          :error (str (:code (ex-data e) "unavailable"))})))

(defn status!
  "Return safe membership metadata for each connected relay. No key, invite, cursor or transcript
   is included."
  [db]
  (let [name
        (machine-name db)

        relays
        (mapv #(relay-status! % name) (filter :machine (identities)))]

    (register-settings! (unique-rooms (mapcat :rooms relays)))
    {:configured (boolean (seq relays)) :relays relays}))

(defn register!
  "Register this machine on a relay with the rooms administrator token. The machine gets the right to
   create rooms there, also when it joined that relay through an invitation."
  [db {:keys [relay_url admin_token] :as body}]
  (transport/validate! "gateway_register" body)
  (let [state
        (ensure-identity! (transport/origin relay_url))

        machine
        (transport/call! (assoc state :credential admin_token)
                         :post
                         "/v1/rooms/machines"
                         {}
                         {:machine_id (:machine_id state)
                          :name (machine-name db)
                          :credential (:credential state)
                          :can_create_rooms true}
                         {})]

    (update-state! (:relay_url state) #(assoc % :machine machine))
    (status! db)))

(defn join!
  "Join the room of an invitation with the saved machine name. One machine can be in many rooms on
   many relays."
  [db {:keys [invite_url] :as body}]
  (transport/validate! "gateway_join" body)
  (let [{:keys [relay_url token]}
        (transport/invite-parts invite_url)

        _
        (ensure-identity! relay_url)

        key
        (util/sha256-hex token)

        state
        (update-state! relay_url
                       #(if (get-in % [:redemptions key])
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
        (transport/call! state
                         :post
                         "/v1/rooms/join"
                         {}
                         {:request_id (get-in state [:redemptions key])
                          :invite_token token
                          :machine_id (:machine_id state)
                          :machine_name (machine-name db)}
                         {})]

    (update-state! relay_url #(assoc % :machine (:machine result)))
    (status! db)
    result))

(defn create!
  "Create a room on a connected relay where this machine can create rooms."
  [db {:keys [relay_url name] :as body}]
  (transport/validate! "gateway_create" body)
  (let [state
        (read-state (transport/origin relay_url))

        _
        (when-not (:machine state) (transport/fail! 409 "rooms_unconfigured"))

        result
        (transport/call!
          state
          :post
          "/v1/rooms"
          {}
          {:room_id (str (random-uuid)) :name name :owner_machine_id (:machine_id state)}
          {})]

    (status! db)
    result))

(defn manage!
  "Apply a declared membership operation through the authenticated machine."
  [db operation room-id id body]
  (transport/validate! "id" room-id)
  (let [params
        {:room_id room-id}

        result
        (case operation
          :delete
          (room-call! room-id :delete "/v1/rooms/{room_id}" params nil {})

          :members
          (room-call! room-id :get "/v1/rooms/{room_id}/members" params nil {})

          :remove
          (room-call! room-id
                      :delete
                      "/v1/rooms/{room_id}/members/{machine_id}"
                      (assoc params :machine_id id)
                      nil
                      {})

          :revoke
          (room-call! room-id
                      :delete
                      "/v1/rooms/{room_id}/invites/{invite_id}"
                      (assoc params :invite_id id)
                      nil
                      {})

          :invite
          (do (transport/validate! "gateway_invite" body)
              (room-call! room-id
                          :post
                          "/v1/rooms/{room_id}/invites"
                          params
                          {:invite_id (str (random-uuid))
                           :token (secret)
                           :expires_at (+ (util/now-ms)
                                          (* 1000 (long (:expires_in_seconds body 86400))))
                           :max_uses (:max_uses body 1)}
                          {})))]

    (when (#{:delete :remove} operation) (status! db))
    result))

(defn delete-machine!
  "Delete this machine from one relay, then forget its credential there. A relay failure keeps the
   credential. The machine stays on the other relays."
  [db {:keys [relay_url] :as body}]
  (transport/validate! "gateway_disconnect" body)
  (let [relay (transport/origin relay_url)]
    (locking state-lock
      (when-let [state (read-state relay)]
        (try (transport/call! state
                              :delete
                              "/v1/rooms/machines/{machine_id}"
                              {:machine_id (:machine_id state)}
                              nil
                              {})
             (catch clojure.lang.ExceptionInfo e
               (when-not (= 401 (:status (ex-data e))) (throw e))))
        (Files/deleteIfExists (state-path relay))))
    (swap! presence-cache #(into {}
                                 (remove (fn [[[cached] _]]
                                           (= relay cached)))
                                 %))
    (status! db)))

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

        state
        (or (room-state room-id) (transport/fail! 404 "room_unknown"))

        key
        [(:relay_url state) room-id]]

    (locking presence-cache
      (let [old
            (get @presence-cache key)

            now
            (util/now-ms)]

        (when (or (not= sessions (:sessions old)) (>= (- now (:at old 0)) 15000))
          (transport/call! state
                           :post
                           "/v1/rooms/{room_id}/presence"
                           {:room_id room-id}
                           {:sessions sessions}
                           {})
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
      (room-call! room-id :get "/v1/rooms/{room_id}/sessions" params nil {})

      :read
      (room-call! room-id :get "/v1/rooms/{room_id}/entries" params nil (dissoc opts :group_id))

      :threads
      (room-call! room-id :get "/v1/rooms/{room_id}/threads" params nil (dissoc opts :group_id))

      :get
      (room-call! room-id
                  :get
                  "/v1/rooms/{room_id}/entries/{entry_id}"
                  (assoc params :entry_id (:entry_id opts))
                  nil
                  {})

      :publish
      (room-call! room-id
                  :post
                  "/v1/rooms/{room_id}/entries"
                  params
                  {:session_id sid
                   :publication
                   (assoc opts :idempotency_key (or (:idempotency_key opts) (str (random-uuid))))}
                  {})

      :wake
      (room-call! room-id
                  :post
                  "/v1/rooms/{room_id}/wake"
                  params
                  {:session_id sid :event (dissoc opts :ping :group_id)}
                  {})

      :pending
      (room-call! room-id :get "/v1/rooms/{room_id}/pending" params nil {:session_id sid})

      :inbox
      (room-call! room-id :get "/v1/rooms/{room_id}/inbox" params nil (assoc opts :session_id sid))

      :receipts
      (room-call! room-id :post "/v1/rooms/{room_id}/receipts" params opts {}))))

(defn cursor
  [room-id sid kind]
  (get-in (room-state room-id) [:cursors (str room-id "/" sid "/" (name kind))] 0))

(defn advance!
  [room-id sid kind entry-id]
  (when-let [state (room-state room-id)]
    (update-state! (:relay_url state)
                   #(update-in %
                               [:cursors (str room-id "/" sid "/" (name kind))]
                               (fn [old]
                                 (max (long (or old 0)) (long entry-id)))))))

(defn poll!
  "Claim each idle wake before dispatch. A crash cannot repeat a paid activation."
  [db]
  (when (some :machine (identities))
    (let [status
          (status! db)

          by-room
          (group-by :room_id (fleet db))]

      (doseq [{room-id :room_id}
              (unique-rooms (mapcat :rooms (remove :error (:relays status))))

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
  (register-settings! (known-rooms))
  (let [running
        (atom true)

        job
        (future (while @running
                  (try (poll! db) (catch Exception _ nil))
                  (when @running (Thread/sleep 15000))))]

    (fn []
      (reset! running false)
      (future-cancel job))))
