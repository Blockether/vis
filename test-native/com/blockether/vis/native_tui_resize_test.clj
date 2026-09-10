(ns com.blockether.vis.native-tui-resize-test
  "Real PTY resize coverage for the standalone native TUI."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.contract.gateway :as gateway]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.lang ProcessBuilder$Redirect ProcessHandle)
           (java.util.concurrent TimeUnit)))

(defn- start-gateway-stub!
  []
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (with-open [^HttpExchange exchange exchange]
              (let [path (.getPath (.getRequestURI exchange))
                    body (case path
                           "/healthz"
                           {"status" "ok"
                            "protocol" (into {}
                                             (map (fn [[k v]]
                                                    [(get gateway/handshake-keys k) v])
                                                  (gateway/handshake {:version "resize-test"})))}

                           "/v1/clients"
                           {"client_id" "resize-test"}

                           "/v1/router"
                           {"providers" []}

                           "/v1/sessions"
                           {"id" "00000000-0000-0000-0000-000000000001"}

                           {})
                    data (.getBytes ^String (json/write-json-str body) "UTF-8")]

                (.sendResponseHeaders exchange 200 (alength data))
                (.write (.getResponseBody exchange) data))))))
    (.start server)
    server))

(defdescribe
  native-tui-resize-test
  ;; Regression: Lanterna silently discarded failure to register its reflective
  ;; WINCH handler in native-image, leaving the screen at its original size.
  (it "repaints after growing and shrinking the terminal without keyboard input"
      (let [binary (io/file (or (System/getenv "VIS_TUI_NATIVE_BIN")
                                "apps/vis-tui/target/vis-tui"))]
        (expect (.canExecute binary) "Build apps/vis-tui with clojure -T:build native first")
        (when (.canExecute binary)
          (let [server (start-gateway-stub!)]
            (try
              (let [process (.start (doto (ProcessBuilder.
                                            ^java.util.List
                                            ["python3"
                                             "test-native/com/blockether/vis/fixtures/tui_resize.py"
                                             (.getAbsolutePath binary)
                                             (str "127.0.0.1:" (.getPort (.getAddress server)))])
                                      (.redirectErrorStream true)
                                      (.redirectOutput ProcessBuilder$Redirect/PIPE)))]
                (try (let [finished? (.waitFor process 55 TimeUnit/SECONDS)]
                       (expect finished? "PTY resize fixture timed out")
                       (when finished?
                         (let [output (slurp (.getInputStream process))]
                           (expect (zero? (.exitValue process)) output)
                           (expect (str/includes? output "resized to 100x35") output))))
                     (finally (when (.isAlive process)
                                (with-open [children (.descendants process)]
                                  (doseq [^ProcessHandle child (iterator-seq (.iterator children))]
                                    (.destroyForcibly child)))
                                (.destroyForcibly process)
                                (.waitFor process 5 TimeUnit/SECONDS)))))
              (finally (.stop server 0))))))))
