(ns com.blockether.vis.internal.attachment.linked-reports-test
  (:refer-clojure :exclude [deliver])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.attachment.linked-reports :as reports]
            [com.blockether.vis.internal.gateway.server]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest local-report-delivery-test
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
        (lp/create-environment router {:db :memory})

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
              (lp/run-turn! environment
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

        (is (= 1 (count attachments))
            (pr-str {:workspace (:workspace/root environment)
                     :answer (:answer result)
                     :tid tid
                     :iteration-keys (keys iteration)}))
        (is (str/includes? (str (lp/answer-markdown (:answer result))) link))
        (is (str/includes? (str (:content (last (persistence/db-list-session-turns
                                                  db
                                                  (:session-id environment)))))
                           link))
        (is (str/includes? (str (:final (last (filter #(= :iteration-final (:phase %)) @chunks))))
                           link))
        (is (= "report.md" (:filename attachment)))
        (is (= "text/markdown" (:media-type attachment)))
        (io/delete-file report)
        (is (= "# Durable report\n"
               (some-> (:base64 (first (persistence/db-list-iteration-attachments db
                                                                                  (:id iteration))))
                       (#(.decode (java.util.Base64/getDecoder) ^String %))
                       (String. "UTF-8"))))
        (with-redefs [lp/db-info
                      (constantly db)

                      com.blockether.vis.internal.gateway.server/auth-required?
                      (constantly true)]

          (let [handler
                ((ns-resolve 'com.blockether.vis.internal.gateway.server 'wrap-auth)
                  @(ns-resolve 'com.blockether.vis.internal.gateway.server
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
                (handler (assoc request :headers {"authorization" "Bearer report-fixture-token"}))]

            (is (= 401 (:status (handler (assoc request :headers {})))))
            (is (= 200 (:status response)))
            (is (= "text/markdown" (get-in response [:headers "Content-Type"])))
            (is (str/starts-with? (get-in response [:headers "Cache-Control"]) "private"))
            (with-open [body (:body response)]
              (is (= "# Durable report\n" (slurp body)))))))
      (finally (lp/dispose-environment! environment)
               (io/delete-file report true)
               (io/delete-file dir true)))))

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

(deftest markdown-delivery-test
  ;; #193: parse actual links, not examples, and share a snapshot across repeated links.
  (report-fixture
    (fn [_ environment]
      (let [markdown
            (str
              "[**Report**](./report.md) and [again][r]\n\n" "[r]: ./report.md\n\n"
              "`[example](report.md)`\n\n```md\n[example](report.md)\n```\n"
              "![image](report.md) [web](https://gateway.example.com/report.md) [section](#report)")

            result
            (deliver environment markdown)

            answer
            (get-in result [:final-result :answer :answer])]

        (is (= 1 (count (:linked-report-attachments result))))
        (is (= 2 (count (re-seq #"attachment://" answer))))
        (is (str/includes? answer "`[example](report.md)`"))
        (is (str/includes? answer "![image](report.md)"))
        (is (str/includes? answer "https://gateway.example.com/report.md"))
        (is (str/includes? answer "[section](#report)"))))))

(deftest denied-report-delivery-test
  ;; #193: a link does not authorize other roots, exclusions or symlink targets.
  (report-fixture
    (fn [dir environment]
      (spit (io/file dir ".hidden.md") "hidden")
      (spit (io/file dir "credentials.txt") "not a deliverable")
      (Files/createSymbolicLink (.toPath (io/file dir "link.md"))
                                (.toPath (io/file dir "report.md"))
                                (make-array FileAttribute 0))
      (.mkdir (io/file dir "directory.md"))
      (doseq [path ["missing.md" "../report.md" "%2e%2e/report.md" "sub/../report.md" "/report.md"
                    "link.md" ".hidden.md" "credentials.txt" "directory.md"]]
        (let [result (deliver environment (str "[Report](" path ")"))]
          (is (empty? (:linked-report-attachments result)) path)
          (is (str/includes? (get-in result [:final-result :answer :answer])
                             "check the workspace path and access")
              path)))
      (doseq [key [:deny-read :no-search]]
        (is (empty? (:linked-report-attachments (deliver (assoc-in environment
                                                           [:security-policy :process-jail key]
                                                           [(str (io/file dir "report.md"))])
                                                         "[Report](report.md)")))))
      (is (empty? (:linked-report-attachments (deliver (dissoc environment :security-policy)
                                                       "[Report](report.md)")))))))

(deftest report-delivery-limits-test
  (report-fixture
    (fn [dir environment]
      (spit (io/file dir "large.md") (apply str (repeat (inc (* 8 1024 1024)) "x")))
      (is (empty? (:linked-report-attachments (deliver environment "[Large](large.md)"))))
      (let [links
            (for [i (range 9)]
              (let [filename (str "report-" i ".md")]
                (spit (io/file dir filename) "report")
                (str "[Report](" filename ")")))

            result
            (deliver environment (str/join " " links))]

        (is (= 8 (count (:linked-report-attachments result))))
        (is (every? #(= "user" (:audience %)) (:linked-report-attachments result)))))))
