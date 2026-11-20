(ns com.blockether.vis.internal.loop.atif-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.persistance.sqlite.core :as db]
            [com.blockether.vis.internal.loop.atif :as atif]
            [com.blockether.vis.test-temp :as temp]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

(defn- store-attachment!
  [store owner filename audience]
  (let [[turn-id iteration-id]
        owner

        data
        (byte-array (* 2 1024 1024) (byte 1))]

    (jdbc/execute!
      (:datasource store)
      ["INSERT INTO session_attachment (id, session_turn_soul_id, session_turn_iteration_id, position, media_type, filename, size_bytes, bytes, audience, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       (str (random-uuid)) (str turn-id)
       (some-> iteration-id
               str) 1 "image/png" filename (alength data) data audience 1])))

(defdescribe
  saved-export-test
  (it "exports the path of the assigned project, not the path of the workspace"
      (let [store (db/db-open! :memory)]
        (try
          (let [workspace-root (.getCanonicalPath (temp/temp-dir "vis-atif-workspace"))
                project-root (.getCanonicalPath (temp/temp-dir "vis-atif-project"))
                workspace (db/db-workspace-insert!
                            store {:repo-id "atif" :root workspace-root :state :active})
                sid (db/db-store-session! store {:channel :api :workspace-id (:id workspace)})
                project (db/db-create-project! store {:name "ATIF" :workspace-root project-root})]
            (expect (not (contains? (get (wire/parse-json (atif/export-json store sid)) "extra")
                                    "cwd")))
            (db/db-set-session-project! store sid (:id project))
            (expect (= project-root
                       (get-in (wire/parse-json (atif/export-json store sid)) ["extra" "cwd"]))))
          (finally (db/db-close! store)))))
  (it
    "exports attachment metadata and bounded tool output with token details"
    (let [store (db/db-open! :memory)]
      (try
        (let [root (.getCanonicalPath (temp/temp-dir "vis-atif"))
              workspace (db/db-workspace-insert! store {:repo-id "atif" :root root :state :active})
              sid (db/db-store-session! store
                                        {:channel :api :title "ATIF" :workspace-id (:id workspace)})
              tid (db/db-store-session-turn! store
                                             {:parent-session-id sid :user-request "Check it"})
              output (apply str (repeat 20001 "😀"))
              iid (db/db-store-iteration! store
                                          {:session-turn-id tid
                                           :code "print(value)"
                                           :forms [{:src "print(value)" :stdout output}]
                                           :tokens
                                           {"input" 1200 "output" 150 "cached" 600 "reasoning" 80}
                                           :cache-created-tokens 100})]

          (store-attachment! store [tid nil] "user.png" "both")
          (store-attachment! store [tid iid] "tool.png" "user")
          (store-attachment! store [tid iid] "model.png" "model")
          (let [[user agent] (get (wire/parse-json (atif/export-json store sid)) "steps")
                attachment {"media_type" "image/png" "size_bytes" 2097152}
                observation (get-in agent ["observation" "results" 0 "content"])]

            (expect (= [(assoc attachment "filename" "user.png")]
                       (get-in user ["extra" "attachments"])))
            (expect (= [(assoc attachment "filename" "tool.png")]
                       (get-in agent ["extra" "attachments"])))
            (expect (str/ends-with? observation "\n… [1 more characters cut]"))
            (expect (= (apply str (repeat 20000 "😀")) (first (str/split-lines observation))))
            (expect (= {"cache_creation_tokens" 100 "reasoning_tokens" 80}
                       (get-in agent ["metrics" "extra"])))))
        (finally (db/db-close! store))))))
