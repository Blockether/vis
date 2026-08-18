(ns com.blockether.vis.internal.gateway.session-fork-test
  "Gateway-owned session forking: the wire twin of the TUI's fork / fork-at-turn.

   The companion's slide offers both, so both must be answerable from the daemon
   without a terminal: fork the whole session, or fork THROUGH one turn."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- seeded-session!
  "A session pinned to a trunk workspace, carrying `requests` as turns in order.
   Returns `{:sid :turn-ids}`."
  [store requests]
  (let [root
        (temp-dir "vis-gateway-fork")

        trunk
        (ps/db-workspace-insert! store
                                 {:repo-id "fork-repo"
                                  :repo-root root
                                  :root root
                                  :workspace-kind :trunk
                                  :workspace-backend :live
                                  :state :active})

        sid
        (str (random-uuid))

        state-id
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
       state-id sid (:id trunk) 0 "Trunk talk" 1])
    {:sid sid
     :turn-ids (mapv (fn [request]
                       (str (ps/db-store-session-turn!
                              store
                              {:parent-session-id sid :user-request request :status :success})))
                     requests)}))

(defdescribe
  gateway-session-fork-test
  (it "lists lean fork points and forks through the picked turn into a new session"
      (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
        (try (let [{:keys [sid turn-ids]} (seeded-session! store
                                                           ["first ask" "second ask" "third ask"])]
               (with-redefs [lp/db-info (constantly store)]
                 (let [points (state/fork-points sid)]
                   (expect (= turn-ids (mapv #(get % "turn_id") points)))
                   (expect (= ["first ask" "second ask" "third ask"]
                              (mapv #(get % "request") points))))
                 ;; Fork AT the second turn: the fork keeps two turns, the source keeps three.
                 (let [forked (state/fork-session! sid (second turn-ids))
                       fork-id (get forked "id")]

                   (expect (string? fork-id))
                   (expect (not= sid fork-id))
                   (expect (= ["first ask" "second ask"]
                              (mapv :user-request (ps/db-list-session-turns store fork-id))))
                   (expect (= 3 (count (ps/db-list-session-turns store sid)))))
                 ;; A plain fork (no turn named) copies the session THROUGH its last turn.
                 (let [whole (state/fork-session! sid nil)]
                   (expect (= 3 (count (ps/db-list-session-turns store (get whole "id"))))))
                 ;; A turn from another session is refused rather than silently forking everything.
                 (expect (= :session/unknown-turn
                            (try (state/fork-session! sid (str (random-uuid)))
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
             (finally (ps/db-close! store))))))
