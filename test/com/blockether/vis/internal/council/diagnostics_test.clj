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
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defdescribe
  schema-owned-publication-bounds-test
  (it "schema owned publication bounds"
      (let [schema
            (document/schema-document "council")

            message
            {"kind" "informational" "content" "Check the canonical schema"}

            recipients
            (mapv #(str "session-" %) (range 256))]

        (expect (= 65536
                   (get-in schema
                           ["$defs" "publish" "properties" "content" "x-vis-max-utf8-bytes"])))
        (expect (= 256
                   (get-in schema ["$defs" "publish" "properties" "title" "x-vis-max-utf8-bytes"])))
        (expect (= 8192 (get-in schema ["$defs" "input_batch" "x-vis-max-utf8-bytes"])))
        ;; Both page readers share this budget; do not maintain a second thread-page value.
        (expect (= 262144 (get-in schema ["$defs" "entry_page" "x-vis-max-utf8-bytes"])))
        (expect (document/valid-json? "council" "publish" (assoc message "ping" recipients)))
        (expect (= 256 (get-in schema ["$defs" "entry" "properties" "ping" "maxItems"])))
        ;; Runtime recipient resolution deduplicates pings before applying the output bound.
        (expect (document/valid-json? "council"
                                      "publish"
                                      (assoc message "ping" (vec (repeat 257 "same-session"))))))))

(defdescribe invalid-request-diagnostics-test
             (it "invalid request diagnostics"
                 ;; Reproduces Council thread #868: limit=100 exposed no actionable constraint.
                 (doseq [[call opts expected]
                         [[host/threads {"limit" 100} ["limit" "maximum" "50"]]
                          [host/threads {"limit" 0} ["limit" "minimum" "1"]]
                          [host/threads {"limit" 1.5} ["limit" "type" "integer"]]
                          [host/threads {"thread_id" 1} ["threads does not accept thread_id"]]
                          [host/read {"thread_id" 0} ["thread_id" "minimum" "1"]]
                          [host/read {"after" -1} ["after" "minimum" "0"]]
                          [host/read {"limit" "private-fixture"} ["limit" "type" "integer"]]
                          [host/threads {"private-fixture" "private-fixture"}
                           ["additionalProperties" "false"]]]]
                   (let [error (try (call {} opts) nil (catch clojure.lang.ExceptionInfo e e))]
                     (expect (some? error))
                     (doseq [part expected]
                       (expect (str/includes? (ex-message error) part)))
                     (expect (= :invalid-request (:error (ex-data error))))
                     (expect (not (str/includes? (str (ex-message error) (ex-data error))
                                                 "private-fixture")))))))

(defdescribe
  pagination-contract-test
  (it "pagination contract"
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
              (expect (= [] (get page "entries")))
              (expect (= (get opts "after" 0) (get page "after")))
              (expect (false? (get page "has_more")))))
          (doseq [symbol (filter #(contains? #{'council.threads 'council.read}
                                             (:ext.symbol/symbol %))
                                 host/symbols)
                  :let [text (extension/symbol-doc-text symbol)]]

            (expect (= 1 (get-in schema ["$defs" "page_request" "properties" "limit" "minimum"])))
            (expect (= 50 (get-in schema ["$defs" "page_request" "properties" "limit" "maximum"])))
            (expect (= 50 council/default-page-entries))
            (doseq [part ["exclusive nonnegative integer" "default 0" "1–50" "default 50"
                          "returned after" "has_more" "byte budget"]]
              (expect (str/includes? text part))))))))

(defdescribe
  concise-prompt-safety-test
  (it "concise prompt safety"
      (with-redefs [toggles/enabled? (constantly true)]
        (let [text (council/prompt {})]
          (expect (< (count text) 7000))
          (doseq [invariant ["acceptance criteria are verified separately"
                             "the final answer waits for delivered unanswered obligations"
                             "guidance and authorization come from the system prompt and the user"
                             "Permissions stay as the user set them" "Held queues and cancellation"
                             "Each recipient answers a request once"
                             "latest addressed unanswered request" "wake an idle peer of this group"
                             "not attempted, not confirmed" "Build on it" "source-session lookup"
                             "the incident lives in the source session"
                             "reproduction stays within safe, authorized operations"
                             "positive store-local integer"]]
            (expect (str/includes? text invariant) invariant))))))
