(ns com.blockether.vis.native-activity-history-test
  "Durable Activity across a real linked agent turn, not a JVM Python substitute."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]
           [java.util Base64]))

(def ^:private python-code
  "for i in range(160):\n    ls(f'record-{i:03d}')\nprint('native-history-done')")

(defn- tool-reply
  [stream?]
  (let [call {:id "native-history-call"
              :type "function"
              :function {:name "python_execution"
                         :arguments (json/write-json-str {:code python-code})}}]
    (if stream?
      (str (#'binary/json-chunk
            (json/write-json-str {:index 0
                                  :delta {:role "assistant" :tool_calls [(assoc call :index 0)]}
                                  :finish_reason nil}))
           (#'binary/json-chunk "{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}")
           "data: [DONE]\n\n")
      (json/write-json-str {:id "stub"
                            :object "chat.completion"
                            :model "stub-model"
                            :choices [{:index 0
                                       :message {:role "assistant" :content nil :tool_calls [call]}
                                       :finish_reason "tool_calls"}]
                            :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}}))))

(defn- history-pages
  [store sid aid]
  (loop [after
         0

         pages
         []]

    (let [page
          (db/db-activity-page store sid aid {:after after})

          next-after
          (get-in page [:history :next-after])]

      (expect (some? page) "The native process must persist a readable history")
      (if next-after
        (do (expect (> (long next-after) (long after)) "Keyset cursor must advance")
            (if (> (long next-after) (long after))
              (recur next-after (conj pages page))
              (conj pages page)))
        (conj pages page)))))

(defn- export-history
  [^File dir ^File bin ^File database sid aid]
  (let [request-file (io/file dir "export.ndjson")]
    (spit request-file
          (str (json/write-json-str {:method "GET"
                                     :route (str "/v1/sessions/" sid "/activity/" aid "/export")})
               "\n"))
    (let [{:keys [finished? exit output]}
          (#'binary/run-binary
           dir
           ["/usr/bin/env" (str "VIS_DB_PATH=" (.getAbsolutePath database)) "/bin/sh" "-c"
            "exec \"$1\" \"-Duser.home=$2\" sdk-stdio < \"$3\"" "native-export"
            (.getAbsolutePath bin) (.getAbsolutePath dir) (.getAbsolutePath request-file)]
           60)
          frames (try (mapv json/read-json (remove str/blank? (str/split-lines output)))
                      (catch Exception error
                        (throw (ex-info (str "Owned SDK export did not return NDJSON (exit " exit
                                             "): " (subs output 0 (min 1200 (count output))))
                                        {}
                                        error))))
          response (second frames)]

      (expect finished? "Owned SDK export must finish at stdin EOF")
      (expect (= 0 exit))
      (expect (= 2 (count frames)))
      (expect (number? (get (first frames) "protocol")))
      (expect (= 200 (get response "status")))
      (String. (.decode (Base64/getDecoder) ^String (get response "content" "")) "UTF-8"))))

(defdescribe
  native-activity-history-test
  ;; #212: old native collectors dropped rows after 128 and details after 64 KiB.
  (it
    "retains all quick Python activities after exit without inflating model tool output"
    (let [^File dir
          (#'binary/temp-dir "vis-native-activity-")

          original-stream
          @#'binary/stream-body

          original-whole
          @#'binary/whole-body

          calls
          (atom 0)]

      (try
        ;; Each listing contains distinct safe details, all larger than the old aggregate cap.
        (doseq [n (range 160)]
          (let [directory (io/file dir (format "record-%03d" n))]
            (.mkdirs directory)
            (doseq [part (range 8)]
              (spit (io/file directory
                             (str (format "activity-detail-%03d-%d-" n part)
                                  (apply str (repeat 80 "x"))
                                  ".txt"))
                    "fixture"))))
        ;; Reuse the hermetic provider's request recording, HTTP serving and cleanup contract.
        (with-redefs-fn {#'binary/stream-body
                         (fn [reply]
                           (if (= 1 (swap! calls inc)) (tool-reply true) (original-stream reply)))
                         #'binary/whole-body
                         (fn [reply]
                           (if (= 1 (swap! calls inc)) (tool-reply false) (original-whole reply)))}
          (fn []
            (let [{:keys [server asked port]} (#'binary/start-stub-provider!
                                               "Native history complete")]
              (try
                (#'binary/overlay! dir port)
                (let [^File bin (#'binary/require-binary)
                      database (io/file dir "sessions")
                      {:keys [finished? exit output]}
                      (#'binary/run-binary
                       dir
                       [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                        (.getAbsolutePath database) "--raw"
                        "Run the supplied Python fixture and finish."]
                       180)
                      messages (mapcat #(get (json/read-json (:body %)) "messages") @asked)
                      tool-messages (filter #(= "tool" (get % "role")) messages)]

                  (expect finished? "Native Activity dispatch must not deadlock")
                  (expect (= 0 exit) output)
                  (expect (str/includes? output "Native history complete") output)
                  (expect (seq tool-messages) "The model must receive the Python result")
                  (doseq [message tool-messages]
                    (let [content (get message "content")]
                      (expect (str/includes? (str content) "native-history-done"))
                      (expect (< (count (str content)) 8192))
                      (expect (not (str/includes? (str content) "activity-detail-")))))
                  ;; Opening here is a disk reopen: the producing native process has exited.
                  (let [store (vis/db-create-connection! (.getAbsolutePath database))]
                    (try
                      (let [sessions (db/db-list-sessions store :all)
                            sid (:id (first sessions))
                            forms (mapcat :forms
                                          (mapcat #(db/db-list-session-turn-iterations store
                                                                                       (:id %))
                                                  (db/db-list-session-turns store sid)))
                            form (first (filter #(= python-code (:src %)) forms))
                            saved (:activity form)
                            aid (get-in saved [:history :id])]

                        (expect (= 1 (count sessions)))
                        (expect (= "native-history-done\n" (:stdout form)))
                        (expect (nil? (:error form)))
                        (expect (string? aid) "The saved form must reference durable history")
                        (when aid
                          (let [pages (history-pages store sid aid)
                                rows (vec (mapcat :rows pages))
                                tail
                                (db/db-activity-page store sid aid {:q "activity-detail-159-"})]

                            (expect (= 160 (get-in saved [:history :total])))
                            (expect (<= (count (:rows saved)) 32))
                            (expect (> (count pages) 1))
                            (expect (= 160 (count rows)))
                            (expect (= 160 (count (distinct (map :id rows)))))
                            (expect (= (range 1 161) (map :sequence rows)))
                            (expect (every? #(= "succeeded" (:state %)) rows))
                            (expect (every? #(zero? (get-in % [:omitted :rows])) pages))
                            (expect (> (reduce + (map #(activity/byte-size (:presentation %)) rows))
                                       65536))
                            (doseq [[n row] (map-indexed vector rows)]
                              (expect (str/includes? (pr-str (:presentation row))
                                                     (format "activity-detail-%03d-7-" n))))
                            (expect (= [(:id (peek rows))] (mapv :id (:rows tail))))
                            ;; SDK-stdio owns this temporary DB, never the user's gateway.
                            ;; This calls Ring StreamableResponseBody inside the linked image.
                            (let [exported (export-history dir bin database sid aid)]
                              (expect (> (count exported) 65536))
                              (expect (not (str/includes? exported "INCOMPLETE EXPORT")))
                              (doseq [n (range 160)]
                                (expect (str/includes? exported
                                                       (format "activity-detail-%03d-7-" n))))))))
                      (finally (vis/db-dispose-connection! store)))))
                (finally (.stop ^HttpServer server 0))))))
        (finally (#'binary/delete-tree! dir))))))
