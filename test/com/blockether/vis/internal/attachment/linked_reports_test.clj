(ns com.blockether.vis.internal.attachment.linked-reports-test
  (:refer-clojure :exclude [deliver])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.attachment.linked-reports :as reports]
            [com.blockether.vis.internal.gateway.server]
            [com.blockether.vis.internal.gateway.server.turns]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  local-report-delivery-test
  (it
    "local report delivery"
    ;; #193: a TUI answer must persist a snapshot, not a phone-inaccessible host link.
    (let [dir
          (.toFile (Files/createTempDirectory "vis-report-" (make-array FileAttribute 0)))

          report
          (io/file dir "report.md")

          router
          (svar/make-router [{:id :lmstudio
                              :api-key "test"
                              :base-url "http://127.0.0.1:1234/v1"
                              :models [{:name "model"}]}])

          environment
          (loop-env/create-environment router {:db :memory})

          db
          (:db-info environment)

          chunks
          (atom [])]

      (try
        (workspace/change-root! db (:session/state-id environment) (.getCanonicalPath dir))
        (spit report "# Durable report\n")
        (let [result
              (with-redefs [svar/ask-code!
                            (fn [_ _]
                              {:stop-reason :end :tool-calls [] :content "[Report](report.md)"})]
                (turn/run-turn! environment
                                "Write a report"
                                {:hooks {:on-chunk #(swap! chunks conj %)}}))

              tid
              (:session-turn-id result)

              iteration
              (last (persistence/db-list-session-turn-iterations db tid))

              attachments
              (persistence/db-list-iteration-attachments db (:id iteration))

              attachment
              (first attachments)

              link
              (str "attachment://" (:id attachment))]

          (expect (= 1 (count attachments))
                  (pr-str {:workspace (:workspace/root environment)
                           :answer (:answer result)
                           :tid tid
                           :iteration-keys (keys iteration)}))
          (expect (str/includes? (str (transcript/answer-markdown (:answer result))) link))
          (expect (str/includes? (str (:content (last (persistence/db-list-session-turns
                                                        db
                                                        (:session-id environment)))))
                                 link))
          (expect (str/includes? (str (:final (last (filter #(= :iteration-final (:phase %))
                                                            @chunks))))
                                 link))
          (expect (= "report.md" (:filename attachment)))
          (expect (= "text/markdown" (:media-type attachment)))
          (io/delete-file report)
          (expect (= "# Durable report\n"
                     (some-> (:base64 (first (persistence/db-list-iteration-attachments
                                               db
                                               (:id iteration))))
                             (#(.decode (java.util.Base64/getDecoder) ^String %))
                             (String. "UTF-8"))))
          (with-redefs [lp/db-info
                        (constantly db)

                        com.blockether.vis.internal.gateway.server/auth-required?
                        (constantly true)]

            (let [handler
                  ((ns-resolve 'com.blockether.vis.internal.gateway.server 'wrap-auth)
                    @(ns-resolve 'com.blockether.vis.internal.gateway.server.turns
                                 'attachment-bytes-handler)
                    "report-fixture-token"
                    [])

                  request
                  {:uri (str "/v1/sessions/"
                             (:session-id environment)
                             "/iterations/"
                             (:id iteration)
                             "/attachments/0")
                   :path-params
                   {:sid (str (:session-id environment)) :iid (str (:id iteration)) :idx "0"}}

                  response
                  (handler (assoc request
                             :headers {"authorization" "Bearer report-fixture-token"}))]

              (expect (= 401 (:status (handler (assoc request :headers {})))))
              (expect (= 200 (:status response)))
              (expect (= "text/markdown" (get-in response [:headers "Content-Type"])))
              (expect (str/starts-with? (get-in response [:headers "Cache-Control"]) "private"))
              (with-open [body (:body response)]
                (expect (= "# Durable report\n" (slurp body)))))))
        (finally (loop-env/dispose-environment! environment)
                 (io/delete-file report true)
                 (io/delete-file dir true))))))

(defn- report-fixture
  [f]
  (let [dir
        (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-links-"
                                                               (make-array FileAttribute 0))))

        environment
        {:db-info {}
         :workspace/root (.getCanonicalPath dir)
         :security-policy {:process-jail {:deny-read [] :no-search []}}}]

    (try (spit (io/file dir "report.md") "# Report\n")
         (f dir environment)
         (finally (with-open [walk (Files/walk (.toPath dir)
                                               (make-array java.nio.file.FileVisitOption 0))]
                    (doseq [path (reverse (vec (.toArray walk)))]
                      (Files/deleteIfExists path)))))))

(defn- deliver
  [environment markdown]
  (reports/deliver-iteration environment {:final-result {:answer {:answer markdown}}}))

(defdescribe
  markdown-delivery-test
  (it
    "markdown delivery"
    ;; #193: parse actual links, not examples, and share a snapshot across repeated links.
    (report-fixture
      (fn [_ environment]
        (let
          [markdown
           (str
             "[**Report**](./report.md) and [again][r]\n\n" "[r]: ./report.md\n\n"
             "`[example](report.md)`\n\n```md\n[example](report.md)\n```\n"
             "![image](report.md) [web](https://gateway.example.com/report.md) [section](#report)")

           result
           (deliver environment markdown)

           answer
           (get-in result [:final-result :answer :answer])]

          (expect (= 1 (count (:linked-report-attachments result))))
          (expect (= 2 (count (re-seq #"attachment://" answer))))
          (expect (str/includes? answer "`[example](report.md)`"))
          (expect (str/includes? answer "![image](report.md)"))
          (expect (str/includes? answer "https://gateway.example.com/report.md"))
          (expect (str/includes? answer "[section](#report)")))))))

(defdescribe
  denied-report-delivery-test
  (it "denied report delivery"
      ;; #193: a link does not authorize other roots, exclusions or symlink targets.
      (report-fixture
        (fn [dir environment]
          (spit (io/file dir ".hidden.md") "hidden")
          (spit (io/file dir "credentials.txt") "not a deliverable")
          (Files/createSymbolicLink (.toPath (io/file dir "link.md"))
                                    (.toPath (io/file dir "report.md"))
                                    (make-array FileAttribute 0))
          (.mkdir (io/file dir "directory.md"))
          (doseq [path ["missing.md" "../report.md" "%2e%2e/report.md" "sub/../report.md"
                        "/report.md" "link.md" ".hidden.md" "credentials.txt" "directory.md"]]
            (let [result (deliver environment (str "[Report](" path ")"))]
              (expect (empty? (:linked-report-attachments result)) path)
              (expect (str/includes? (get-in result [:final-result :answer :answer])
                                     "check the workspace path and access")
                      path)))
          (doseq [key [:deny-read :no-search]]
            (expect (empty? (:linked-report-attachments (deliver
                                                          (assoc-in environment
                                                            [:security-policy :process-jail key]
                                                            [(str (io/file dir "report.md"))])
                                                          "[Report](report.md)")))))
          (expect (empty? (:linked-report-attachments (deliver (dissoc environment :security-policy)
                                                               "[Report](report.md)"))))))))

(defdescribe
  report-delivery-limits-test
  (it "report delivery limits"
      (report-fixture
        (fn [dir environment]
          (spit (io/file dir "large.md") (apply str (repeat (inc (* 8 1024 1024)) "x")))
          (expect (empty? (:linked-report-attachments (deliver environment "[Large](large.md)"))))
          (let [links
                (for [i (range 9)]
                  (let [filename (str "report-" i ".md")]
                    (spit (io/file dir filename) "report")
                    (str "[Report](" filename ")")))

                result
                (deliver environment (str/join " " links))]

            (expect (= 8 (count (:linked-report-attachments result))))
            (expect (every? #(= "user" (:audience %)) (:linked-report-attachments result))))))))
