(ns com.blockether.vis.native-format-test
  "Formatter reachability through an actual native agent tool call, without a paid model."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

(defdescribe
  native-formatters-remain-callable-test
  ;; JVM startup defers these dependencies. Their first use must still work after
  ;; native-image has discarded everything the builder did not load.
  (it
    "formats with cljfmt and a project zprint config through the linked image"
    (doseq [model ["gpt-4o" "gpt-4"]]
      (let [^File dir (#'native/temp-dir "vis-native-format-")
            ^File bin (#'native/require-binary)
            source "(defn f [x]\n(+ x 1))"
            calls (atom 0)
            code (str "print(format_code('clojure', {'path': 'default.clj'}))\n"
                      "print(format_code('clojure', {'path': 'configured/example.clj'}))")
            tool {:id "format-native"
                  :type "function"
                  :function {:name "python_execution"
                             :arguments (json/write-json-str {:code code})}}
            whole @#'native/whole-body
            stream @#'native/stream-body
            reply (fn [stream? text]
                    (if (= 1 (swap! calls inc))
                      (if stream?
                        (str "data: "
                             (json/write-json-str {:id "stub"
                                                   :object "chat.completion.chunk"
                                                   :created 0
                                                   :model "stub-model"
                                                   :choices [{:index 0
                                                              :delta {:role "assistant"
                                                                      :tool_calls [(assoc tool
                                                                                     :index 0)]}
                                                              :finish_reason nil}]})
                             "\n\ndata: "
                             (json/write-json-str
                               {:id "stub"
                                :object "chat.completion.chunk"
                                :created 0
                                :model "stub-model"
                                :choices [{:index 0 :delta {} :finish_reason "tool_calls"}]})
                             "\n\ndata: [DONE]\n\n")
                        (json/write-json-str
                          {:id "stub"
                           :object "chat.completion"
                           :created 0
                           :model "stub-model"
                           :choices [{:index 0
                                      :message {:role "assistant" :content nil :tool_calls [tool]}
                                      :finish_reason "tool_calls"}]
                           :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}}))
                      ((if stream? stream whole) text)))]

        (try
          (io/make-parents (io/file dir "configured/example.clj"))
          (spit (io/file dir "default.clj") source)
          (spit (io/file dir "configured/example.clj") source)
          (spit (io/file dir "configured/.zprint.edn") "{:width 80}")
          (with-redefs-fn {#'native/whole-body #(reply false %)
                           #'native/stream-body #(reply true %)}
            (fn []
              (let [{:keys [server port asked]} (#'native/start-stub-provider! "FORMAT_COMPLETE")]
                (try
                  (#'native/overlay! dir port)
                  (let [config (io/file dir ".vis/config.yml")]
                    (spit config (str/replace (slurp config) "stub-model" model)))
                  (let [log (io/file dir "run.log")
                        builder (doto (ProcessBuilder. ^java.util.List
                                                       [(.getAbsolutePath bin)
                                                        (str "-Duser.home=" (.getAbsolutePath dir))
                                                        "--db" ":memory" "--raw"
                                                        "Format both Clojure files."])
                                  (.directory dir)
                                  (.redirectErrorStream true)
                                  (.redirectOutput log))
                        env (.environment builder)
                        kept (select-keys (into {} env)
                                          ["PATH" "JAVA_HOME" "SystemRoot" "TMPDIR" "TMP" "TEMP"])]

                    ;; Never inherit a gateway override or the operator's provider credentials.
                    (.clear env)
                    (.putAll env kept)
                    (.putAll env (#'native/native-environment))
                    (.put env "HOME" (.getAbsolutePath dir))
                    (let [process (.start builder)]
                      (try (expect (.waitFor process 180 TimeUnit/SECONDS)
                                   "native formatting timed out")
                           (let [tool-results
                                 (for [request @asked
                                       message (:messages
                                                 (json/read-json (:body request) :key-fn keyword))
                                       :when (= "tool" (:role message))]

                                   (:content message))
                                 output (str (slurp log) "\n" (pr-str tool-results))]

                             (expect (= 0 (.exitValue process)) output)
                             (expect (>= @calls 2) output)
                             (expect (.isDirectory (io/file dir ".vis/native/sqlite")) output)
                             (expect (every? #(= model
                                                 (:model
                                                   (json/read-json (:body %) :key-fn keyword)))
                                             @asked)
                                     output)
                             (expect (= "(defn f [x]\n  (+ x 1))\n"
                                        (slurp (io/file dir "default.clj")))
                                     output)
                             (expect (= "(defn f [x] (+ x 1))\n"
                                        (slurp (io/file dir "configured/example.clj")))
                                     output))
                           (finally (when (.isAlive process) (#'native/kill-tree! process))))))
                  (finally (.stop ^com.sun.net.httpserver.HttpServer server 0))))))
          (finally (#'native/delete-tree! dir)))))))
