(ns com.blockether.vis.internal.gateway.session-snapshot-test
  "Session snapshots (#405): export a session tree to `.vissnap` bytes and restore it."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.session.snapshot :as snapshot]
            [lazytest.core :refer [defdescribe describe expect it]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- rows
  [store sql-params]
  (jdbc/execute! (:datasource store) sql-params {:builder-fn rs/as-unqualified-maps}))

(defn- session!
  "A new session on a trunk workspace with `title`. Returns its id."
  [store title]
  (let [root
        (temp-dir "vis-snapshot")

        trunk
        (ps/db-workspace-insert! store
                                 {:repo-id (str "repo-" title)
                                  :repo-root root
                                  :root root
                                  :workspace-kind :trunk
                                  :workspace-backend :live
                                  :state :active})

        sid
        (str (random-uuid))

        ds
        (:datasource store)]

    (spit (io/file root "seed.txt") "seed")
    (jdbc/execute! ds
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" sid "api"
                    1])
    (jdbc/execute!
      ds
      ["INSERT INTO session_state (id, session_soul_id, workspace_id, version, title, created_at) VALUES (?,?,?,?,?,?)"
       (str (random-uuid)) sid (:id trunk) 0 title 1])
    sid))

(defn- turn!
  [store sid request status]
  (str (ps/db-store-session-turn! store
                                  {:parent-session-id sid :user-request request :status status})))

(defn- attach!
  "Store `bytes` as a user attachment of turn `tid`."
  [store tid ^bytes data]
  (jdbc/execute!
    (:datasource store)
    ["INSERT INTO session_attachment (id, session_turn_soul_id, position, media_type, filename, size_bytes, bytes, created_at) VALUES (?,?,?,?,?,?,?,?)"
     (str (random-uuid)) tid 0 "image/png" "seed.png" (alength data) data 1]))

(defn- child!
  "A managed child of `parent` with agent `status`, made by turn `team-id`."
  [store parent team-id status]
  (let [cid (session! store "Child")]
    (jdbc/execute!
      (:datasource store)
      ["INSERT INTO session_agent (session_id, parent_id, leader_id, team_id, task, status, depth, inherited_turns, spawn_key, spawn_fingerprint, checkpoint, created_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
       cid parent parent team-id "Check the seed" status 1 0 "k1" "f1" (byte-array 0) 1])
    cid))

(defn- entry!
  "A Council entry by `author` that pings `recipient`. Returns its id."
  [store author recipient]
  (let
    [ds
     (:datasource store)

     id
     (->
       (jdbc/execute-one!
         ds
         ["INSERT INTO council_entry (group_id, author_sid, activation_id, title, content, created_at, idempotency_key, fingerprint) VALUES (?,?,?,?,?,?,?,?) RETURNING id"
          "g1" author "a1" "Status" "Seed is fine" 1 (str "key-" author) "fp"]
         {:builder-fn rs/as-unqualified-maps})
       :id)]

    (jdbc/execute!
      ds
      ["INSERT INTO council_ping (recipient_sid, activation_id, group_id, entry_id, state) VALUES (?,?,?,?,?)"
       recipient "a1" "g1" id "pending"])
    id))

(defn- error-type [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defn- with-store
  "Call `f` with an in-memory store that the gateway uses."
  [f]
  (let [store (ps/db-open! :memory)]
    (try (with-redefs [lp/db-info (constantly store)]
           (f store))
         (finally (ps/db-close! store)))))

(defn- on
  "Call `f` with `store` as the gateway store: one store stands for one computer."
  [store f]
  (with-redefs [lp/db-info (constantly store)]
    (f)))

(defdescribe
  session-snapshot-test
  (describe
    "export and restore"
    (it
      "restores turns and attachment bytes under new ids"
      (with-store
        (fn [store]
          (let [sid
                (session! store "Trunk talk")

                tid
                (turn! store sid "first ask" :success)

                image
                (.getBytes "png-bytes" "UTF-8")]

            (turn! store sid "second ask" :success)
            (attach! store tid image)
            (let [data
                  (state/export-session sid)

                  manifest
                  (snapshot/info data)

                  restored
                  (state/import-session! data {})

                  new-id
                  (get restored "id")]

              (expect (= sid (get manifest "root_session_id")))
              (expect (= 2 (get-in manifest ["counts" "turns"])))
              (expect (= 1 (get-in manifest ["counts" "attachments"])))
              (expect (not= sid new-id))
              (expect (= ["first ask" "second ask"]
                         (mapv :user-request (ps/db-list-session-turns store new-id))))
              (expect (= sid (get (:imported-from (ps/db-get-session store new-id)) "session_id")))
              (expect
                (=
                  [(seq image)]
                  (mapv
                    (comp seq :bytes)
                    (rows
                      store
                      ["SELECT a.bytes FROM session_attachment a JOIN session_turn_soul t ON t.id = a.session_turn_soul_id JOIN session_state s ON s.id = t.session_state_id WHERE s.session_soul_id = ?"
                       new-id])))))))))
    (it "answers nil for an unknown session"
        (with-store (fn [_store]
                      (expect (nil? (state/export-session (str (random-uuid)))))))))
  (describe
    "another machine"
    (it
      "restores every turn state into a separate store and lists the session first"
      (let [source
            (ps/db-open! :memory)

            target
            (ps/db-open! :memory)]

        (try (let [sid
                   (on source #(session! source "Durable talk"))

                   data
                   (on source
                       (fn []
                         (doseq [[request status] [["remember ORCA-4127" :success]
                                                   ["slow" :cancelled] ["broken" :error]
                                                   ["still running" :running]]]
                           (turn! source sid request status))
                         ;; A week-old conversation: its turns are older than any work on the target.
                         (jdbc/execute! (:datasource source)
                                        ["UPDATE session_turn_soul SET created_at = 1000"])
                         (state/export-session sid)))

                   fresh
                   (on target #(session! target "Fresh work"))]

               (on target #(turn! target fresh "today" :success))
               (let [new-id (on target #(get (state/import-session! data {}) "id"))]
                 (expect (= [:done :interrupted :error :interrupted]
                            (mapv :status (ps/db-list-session-turns target new-id))))
                 (expect (= [new-id fresh]
                            (on target
                                #(mapv (fn [row]
                                         (get row "id"))
                                       (:sessions (state/list-sessions-page {:limit 10}))))))))
             (finally (ps/db-close! source) (ps/db-close! target)))))
    (it
      "exports a restored session again and records the session that it came from"
      (let [stores
            (vec (repeatedly 3 #(ps/db-open! :memory)))

            [a b c]
            stores]

        (try (let [sid
                   (on a #(session! a "Durable talk"))

                   _
                   (on a #(turn! a sid "remember ORCA-4127" :success))

                   on-b
                   (on b
                       #(get (state/import-session! (on a
                                                        (fn []
                                                          (state/export-session sid)))
                                                    {})
                             "id"))

                   _
                   (on b #(turn! b on-b "continued on b" :success))

                   on-c
                   (on c
                       #(get (state/import-session! (on b
                                                        (fn []
                                                          (state/export-session on-b)))
                                                    {})
                             "id"))]

               (expect (= ["remember ORCA-4127" "continued on b"]
                          (mapv :user-request (ps/db-list-session-turns c on-c))))
               (expect (= on-b (get (:imported-from (ps/db-get-session c on-c)) "session_id"))))
             (finally (run! ps/db-close! stores))))))
  (describe
    "children and Council"
    (it
      "remaps the child, fails its unfinished work and interrupts open pings"
      (with-store
        (fn [store]
          (let [sid
                (session! store "Leader")

                team
                (turn! store sid "spawn" :success)

                cid
                (child! store sid team "running")]

            (turn! store cid "child work" :running)
            (entry! store sid cid)
            (let [data
                  (state/export-session sid)

                  manifest
                  (snapshot/info data)

                  new-root
                  (get (state/import-session! data {}) "id")

                  [agent]
                  (rows store
                        ["SELECT session_id, team_id, status FROM session_agent WHERE parent_id = ?"
                         new-root])

                  new-child
                  (:session_id agent)]

              (expect (= 2 (get-in manifest ["counts" "sessions"])))
              (expect (= 1 (get-in manifest ["counts" "council_entries"])))
              (expect (= "failed" (:status agent)))
              (expect (not= cid new-child))
              (expect (= (str (:id (first (ps/db-list-session-turns store new-root))))
                         (:team_id agent)))
              (expect (= [:interrupted] (mapv :status (ps/db-list-session-turns store new-child))))
              (expect
                (=
                  [{:author_sid new-root :recipient_sid new-child :state "interrupted"}]
                  (rows
                    store
                    ["SELECT e.author_sid, p.recipient_sid, p.state FROM council_entry e JOIN council_ping p ON p.entry_id = e.id WHERE e.author_sid = ?"
                     new-root]))))))))
    (it "keeps entries that ping a session outside the tree out of the snapshot"
        (with-store (fn [store]
                      (let [sid
                            (session! store "Leader")

                            other
                            (session! store "Other")]

                        (entry! store sid other)
                        (expect (= 0
                                   (get-in (snapshot/info (state/export-session sid))
                                           ["counts" "council_entries"]))))))))
  (describe
    "restore keys and refusals"
    (it "answers the first restore again for the same key"
        (with-store (fn [store]
                      (let [sid
                            (session! store "Trunk talk")

                            _
                            (turn! store sid "ask" :success)

                            data
                            (state/export-session sid)

                            first-id
                            (get (state/import-session! data {:key "k-405"}) "id")]

                        (expect (= first-id (get (state/import-session! data {:key "k-405"}) "id")))
                        (expect (not= first-id (get (state/import-session! data {}) "id")))))))
    (it "refuses bytes that are not a snapshot or that were changed"
        (with-store
          (fn [store]
            (let [sid
                  (session! store "Trunk talk")

                  _
                  (turn! store sid "ask" :success)

                  ^bytes data
                  (state/export-session sid)

                  changed
                  (aclone data)

                  last-index
                  (dec (alength changed))]

              (aset-byte changed last-index (unchecked-byte (bit-xor (aget changed last-index) 1)))
              (expect (= :session/invalid-snapshot
                         (error-type #(state/import-session! (.getBytes "not a snapshot") {}))))
              (expect (= :session/invalid-snapshot
                         (error-type #(state/import-session! changed {}))))))))
    (it "refuses a snapshot above the size limit"
        (with-store (fn [store]
                      (let [sid (session! store "Trunk talk")]
                        (turn! store sid "ask" :success)
                        (with-redefs [snapshot/max-bytes (constantly 64)]
                          (expect (= :session/snapshot-too-large
                                     (error-type #(state/export-session sid)))))))))))

(defrecord Foreign [a])

(defn- recoded
  "Snapshot `data` again with its tree changed by `f`."
  [data f]
  (snapshot/encode (f (:tree (snapshot/decode data))) {:vis-version "test" :now 1}))

(defn- session-count [store] (count (rows store ["SELECT id FROM session_soul"])))

(defdescribe
  session-snapshot-edge-test
  (describe
    "refusals before any write"
    (it "refuses a newer format version with unsupported-version"
        (with-store (fn [store]
                      (let [sid (session! store "Trunk talk")]
                        (turn! store sid "ask" :success)
                        (let [^bytes data (state/export-session sid)]
                          (aset-byte data 11 (byte 2))
                          (expect (= :session/unsupported-version
                                     (error-type #(state/import-session! data {}))))
                          (expect (= 1 (session-count store))))))))
    (it "refuses a record or a Java class that Nippy does not allow"
        (with-store
          (fn [store]
            (let [sid
                  (session! store "Trunk talk")

                  _
                  (turn! store sid "ask" :success)

                  data
                  (state/export-session sid)]

              (doseq [value [(->Foreign 1) (java.util.concurrent.atomic.AtomicLong. 5)]]
                (expect (= :session/invalid-snapshot
                           (error-type #(state/import-session! (recoded data
                                                                        (fn [tree]
                                                                          (assoc tree
                                                                            :extra value)))
                                                               {})))))
              (expect (= 1 (session-count store)))))))
    (it
      "refuses a repeated session, a cycle and a root with a parent"
      (with-store
        (fn [store]
          (let [sid
                (session! store "Leader")

                team
                (turn! store sid "spawn" :success)

                cid
                (child! store sid team "completed")

                data
                (state/export-session sid)

                broken
                [#(update %
                          :sessions
                          (fn [ss]
                            (conj (vec ss) (first ss))))
                 #(update %
                          :sessions
                          (fn [ss]
                            (mapv (fn [s]
                                    (if (= cid (:id (:soul s))) (assoc s :parent-id cid) s))
                                  ss))) #(update-in % [:sessions 0] assoc :parent-id cid)]]

            (doseq [f broken]
              (expect (= :session/invalid-snapshot
                         (error-type #(state/import-session! (recoded
                                                               data
                                                               (comp f
                                                                     (fn [t]
                                                                       (update t :sessions vec))))
                                                             {})))))
            (expect (= 2 (session-count store)))))))
    (it "writes nothing when the restore fails in the middle"
        (with-store (fn [store]
                      (let [sid
                            (session! store "Trunk talk")

                            tid
                            (turn! store sid "ask" :success)]

                        (attach! store tid (.getBytes "png-bytes" "UTF-8"))
                        (let [data (recoded (state/export-session sid) #(assoc % :blobs {}))]
                          (expect (= :session/invalid-snapshot
                                     (error-type #(state/import-session! data {}))))
                          (expect (= 1 (session-count store)))))))))
  (describe
    "session shapes"
    (it "restores a session without turns with its settings"
        (with-store
          (fn [store]
            (let [sid (session! store "Empty")]
              (jdbc/execute!
                (:datasource store)
                ["INSERT INTO session_setting (owner_id, setting_id, value) VALUES (?,?,?)" sid
                 "verbosity" "\"short\""])
              (let [new-id (get (state/import-session! (state/export-session sid) {}) "id")]
                (expect (= [] (vec (ps/db-list-session-turns store new-id))))
                (expect (= [{:setting_id "verbosity" :value "\"short\""}]
                           (rows store
                                 ["SELECT setting_id, value FROM session_setting WHERE owner_id = ?"
                                  new-id]))))))))
    (it "restores a child alone as an independent session"
        (with-store
          (fn [store]
            (let [sid
                  (session! store "Leader")

                  team
                  (turn! store sid "spawn" :success)

                  cid
                  (child! store sid team "completed")]

              (turn! store cid "child work" :success)
              (let [data
                    (state/export-session cid)

                    new-id
                    (get (state/import-session! data {}) "id")]

                (expect (= 1 (get-in (snapshot/info data) ["counts" "sessions"])))
                (expect (nil? (get-in (snapshot/info data) ["sessions" 0 "parent_id"])))
                (expect (= []
                           (rows store
                                 ["SELECT session_id FROM session_agent WHERE session_id = ?"
                                  new-id])))
                (expect (= [{:parent_state_id nil}]
                           (rows store
                                 ["SELECT parent_state_id FROM session_soul WHERE id = ?"
                                  new-id]))))))))
    (it
      "stores a shared attachment once and restores it to each turn"
      (with-store
        (fn [store]
          (let [sid
                (session! store "Trunk talk")

                image
                (.getBytes "same-png-bytes" "UTF-8")]

            (attach! store (turn! store sid "first" :success) image)
            (attach! store (turn! store sid "second" :success) image)
            (let [data
                  (state/export-session sid)

                  manifest
                  (snapshot/info data)

                  new-id
                  (get (state/import-session! data {}) "id")]

              (expect (= 2 (get-in manifest ["counts" "attachments"])))
              (expect (= (alength image) (get-in manifest ["sizes" "attachment_bytes"])))
              (expect
                (=
                  [(seq image) (seq image)]
                  (mapv
                    (comp seq :bytes)
                    (rows
                      store
                      ["SELECT a.bytes FROM session_attachment a JOIN session_turn_soul t ON t.id = a.session_turn_soul_id JOIN session_state s ON s.id = t.session_state_id WHERE s.session_soul_id = ?"
                       new-id])))))))))
    (it "keeps a model preference whose provider this machine does not configure"
        (with-store
          (fn [store]
            (let [sid (session! store "Pinned")]
              (jdbc/execute!
                (:datasource store)
                ["UPDATE session_soul SET llm_pref_provider = ?, llm_pref_model = ? WHERE id = ?"
                 "provider-not-here" "model-not-here" sid])
              (let [new-id (get (state/import-session! (state/export-session sid) {}) "id")]
                (expect
                  (= [{:llm_pref_provider "provider-not-here" :llm_pref_model "model-not-here"}]
                     (rows
                       store
                       ["SELECT llm_pref_provider, llm_pref_model FROM session_soul WHERE id = ?"
                        new-id]))))))))
    (it "keeps 4-byte Unicode in titles and content"
        (with-store
          (fn [store]
            (let [title
                  "Zażółć 🎉 𝄞"

                  sid
                  (session! store title)]

              (turn! store sid "Ask 😀 about 𝔘𝔫𝔦𝔠𝔬𝔡𝔢" :success)
              (let [data
                    (state/export-session sid)

                    new-id
                    (get (state/import-session! data {}) "id")]

                (expect (= title (get-in (snapshot/info data) ["sessions" 0 "title"])))
                (expect (= ["Ask 😀 about 𝔘𝔫𝔦𝔠𝔬𝔡𝔢"]
                           (mapv :user-request (ps/db-list-session-turns store new-id))))
                (expect (= [{:title title}]
                           (rows store
                                 ["SELECT title FROM session_state WHERE session_soul_id = ?"
                                  new-id]))))))))))
