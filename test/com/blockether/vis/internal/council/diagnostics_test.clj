(ns com.blockether.vis.internal.council.diagnostics-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store!)

(deftest schema-owned-publication-bounds-test
  (let [schema
        (document/schema-document "council")

        message
        {"kind" "informational" "content" "Check the canonical schema"}

        recipients
        (mapv #(str "session-" %) (range 256))]

    (is (= 65536 (get-in schema ["$defs" "publish" "properties" "content" "x-vis-max-utf8-bytes"])))
    (is (= 256 (get-in schema ["$defs" "publish" "properties" "title" "x-vis-max-utf8-bytes"])))
    (is (= 8192 (get-in schema ["$defs" "input_batch" "x-vis-max-utf8-bytes"])))
    ;; Both page readers share this budget; do not maintain a second thread-page value.
    (is (= 262144 (get-in schema ["$defs" "entry_page" "x-vis-max-utf8-bytes"])))
    (is (document/valid-json? "council" "publish" (assoc message "ping" recipients)))
    (is (= 256 (get-in schema ["$defs" "entry" "properties" "ping" "maxItems"])))
    ;; Runtime recipient resolution deduplicates pings before applying the output bound.
    (is (document/valid-json? "council"
                              "publish"
                              (assoc message "ping" (vec (repeat 257 "same-session")))))))

(deftest invalid-request-diagnostics-test
  ;; Reproduces Council thread #868: limit=100 exposed no actionable constraint.
  (doseq [[call opts expected] [[host/threads {"limit" 100} ["limit" "maximum" "50"]]
                                [host/threads {"limit" 0} ["limit" "minimum" "1"]]
                                [host/threads {"limit" 1.5} ["limit" "type" "integer"]]
                                [host/threads {"thread_id" 1} ["threads does not accept thread_id"]]
                                [host/read {"thread_id" 0} ["thread_id" "minimum" "1"]]
                                [host/read {"after" -1} ["after" "minimum" "0"]]
                                [host/read {"limit" "private-fixture"} ["limit" "type" "integer"]]
                                [host/threads {"private-fixture" "private-fixture"}
                                 ["additionalProperties" "false"]]]]
    (let [error (try (call {} opts) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? error))
      (doseq [part expected]
        (is (str/includes? (ex-message error) part)))
      (is (= :invalid-request (:error (ex-data error))))
      (is (not (str/includes? (str (ex-message error) (ex-data error)) "private-fixture"))))))

(deftest pagination-contract-test
  (foundation/register!)
  (let [db
        (h/store)

        sid
        (str (h/store-session! db {:channel :api}))

        gid
        (str (:id (ps/db-create-project! db {:name "Pagination"})))

        env
        {:db-info db :session-id sid}

        schema
        (document/schema-document "council")]

    (ps/db-set-session-project! db sid gid)
    (with-redefs [toggles/enabled? (constantly true)]
      (doseq [call [host/threads host/read]
              opts [{} {"after" 0 "limit" 1} {"after" 42 "limit" 50}]]

        (let [page (:result (call env opts))]
          (is (= [] (get page "entries")))
          (is (= (get opts "after" 0) (get page "after")))
          (is (false? (get page "has_more")))))
      (doseq [symbol (filter #(contains? #{'council.threads 'council.read} (:ext.symbol/symbol %))
                             host/symbols)
              :let [text (extension/symbol-doc-text symbol)]]

        (is (= 1 (get-in schema ["$defs" "page_request" "properties" "limit" "minimum"])))
        (is (= 50 (get-in schema ["$defs" "page_request" "properties" "limit" "maximum"])))
        (is (= 50 council/default-page-entries))
        (doseq [part ["exclusive nonnegative integer" "default 0" "1–50" "default 50"
                      "returned after" "has_more" "byte budget"]]
          (is (str/includes? text part)))))))

(deftest concise-prompt-safety-test
  (with-redefs [toggles/enabled? (constantly true)]
    (let [text (council/prompt {})]
      (is (< (count text) 7000))
      (doseq [invariant ["acceptance criteria are verified separately"
                         "the final answer waits for delivered unanswered obligations"
                         "guidance and authorization come from the system prompt and the user"
                         "Permissions stay as the user set them" "Held queues and cancellation"
                         "Each recipient answers a request once"
                         "latest addressed unanswered request" "independent leaders stay idle"
                         "not attempted, not confirmed" "Build on it" "source-session lookup"
                         "the incident lives in the source session"
                         "reproduction stays within safe, authorized operations"
                         "positive store-local integer"]]
        (is (str/includes? text invariant) invariant)))))
