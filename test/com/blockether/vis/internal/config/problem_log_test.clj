(ns com.blockether.vis.internal.config.problem-log-test
  "Provider warnings and errors reach a JSON-lines file with their correlation ids (issue #403)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [lazytest.core :refer [defdescribe describe expect it]]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- signal
  [level data]
  (tel/with-signal :force-msg
                   (tel/with-ctx+ {:db-info {:datasource (Object.)}
                                   :session-soul-id #uuid "11111111-2222-3333-4444-555555555555"
                                   :session-turn-id #uuid "66666666-7777-8888-9999-000000000000"
                                   :turn 3
                                   :iteration-id "iter-1"}
                                  (tel/log! {:level level
                                             :id :com.blockether.svar.internal.llm/retry-declined
                                             :data data}
                                            "not retrying provider failure"))))

(defdescribe problem-log-line-test
             ;; Issue #403: the cause of a provider failure was only in the SQLite `log` table.
             (describe
               "a provider warning"
               (it "becomes one JSON line with session, turn, iteration, query and request ids"
                   (let [line
                         (config/problem-log-line
                           (signal :warn
                                   {:query-id #uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                                    :request-id "req_42"
                                    :iteration 2
                                    :status 429
                                    :category :quota-exhausted
                                    :error "Exceptional status code: 429"}))

                         row
                         (wire/parse-json line)]

                     (expect (str/ends-with? line "\n"))
                     (expect (= 1 (count (str/split-lines line))))
                     (expect (= "warn" (get row "level")))
                     (expect (= "com.blockether.svar.internal.llm/retry-declined" (get row "id")))
                     (expect (= "not retrying provider failure" (get row "msg")))
                     (expect (= "11111111-2222-3333-4444-555555555555" (get row "session_id")))
                     (expect (= "66666666-7777-8888-9999-000000000000" (get row "turn_id")))
                     (expect (= 3 (get row "turn")))
                     (expect (= "iter-1" (get row "iteration_id")))
                     (expect (= 2 (get row "iteration")))
                     (expect (= "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" (get row "query_id")))
                     (expect (= "req_42" (get row "request_id")))
                     (expect (= 429 (get row "status")))
                     (expect (= "quota-exhausted" (get row "category")))
                     (expect (string? (get row "ts")))
                     (expect (not (str/includes? line "datasource")))))
               (it "does not write credentials"
                   (let [line (config/problem-log-line
                                (signal :error
                                        {:api-key "sk-live-123456"
                                         :headers {"Authorization" "Bearer abc.def"}
                                         :error "call failed: token=xyz123"}))]
                     (expect (not (str/includes? line "sk-live-123456")))
                     (expect (not (str/includes? line "abc.def")))
                     (expect (not (str/includes? line "xyz123")))
                     (expect (str/includes? line "[REDACTED]")))))
             (describe "a signal below warn"
                       (it "is not written"
                           (expect (nil? (config/problem-log-line (signal :info {:status 200})))))))

(defdescribe problem-log-path-test
             (it "sits next to the process log, so housekeeping sweeps it"
                 (with-redefs [config/log-path
                               (constantly
                                 "/tmp/x/logs/2026-01-01/gateway-20260101T000000Z-pid1.log")]
                   (expect (= "/tmp/x/logs/2026-01-01/gateway-20260101T000000Z-pid1.problems.jsonl"
                              (config/problem-log-path))))))

(defdescribe
  problem-log-handler-test
  (it "appends warnings to the file"
      (let [dir
            (.toFile (Files/createTempDirectory "vis-problem-log-" (make-array FileAttribute 0)))

            path
            (str dir "/p.problems.jsonl")]

        (try
          (tel/add-handler! ::test (config/problem-log-handler path) {:async nil :min-level :warn})
          (tel/with-ctx+ {:session-soul-id "s-1"}
                         (tel/log! {:level :warn :id ::provider-down :data {:status 503}}
                                   "provider down")
                         (tel/log! {:level :info :id ::quiet} "not a problem"))
          (let [rows (->> (slurp path)
                          str/split-lines
                          (mapv wire/parse-json))]
            (expect (= 1 (count rows)))
            (expect (= "s-1" (get-in rows [0 "session_id"])))
            (expect (= 503 (get-in rows [0 "status"]))))
          (finally (tel/remove-handler! ::test)
                   (doseq [f (reverse (file-seq dir))]
                     (io/delete-file f true)))))))
