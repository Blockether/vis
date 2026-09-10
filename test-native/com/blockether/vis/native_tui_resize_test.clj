(ns com.blockether.vis.native-tui-resize-test
  "Real PTY resize, input and clipboard coverage for the standalone native TUI."
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
  [& [slow-model? requests clipboard?]]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (with-open [^HttpExchange exchange exchange]
              (let [path (.getPath (.getRequestURI exchange))
                    _ (when requests (swap! requests conj [(.getRequestMethod exchange) path]))
                    _ (when (and slow-model? (str/ends-with? path "/model")) (Thread/sleep 5000))
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
                           {"providers" [{"id" "openai"
                                          "label" "OpenAI"
                                          "is_default" true
                                          "default_model" "model-a"
                                          "models" ["model-a" "model-b"]}]}

                           "/v1/sessions"
                           {"id" "00000000-0000-0000-0000-000000000001"}

                           "/v1/sessions/00000000-0000-0000-0000-000000000001"
                           {"id" "00000000-0000-0000-0000-000000000001" "status" "idle"}

                           "/v1/sessions/00000000-0000-0000-0000-000000000001/transcript"
                           {"offset" 0
                            "total" 1
                            "has_more" false
                            "turns"
                            [{"turn_id" "native-highlighting"
                              "status" "completed"
                              "request"
                              (if clipboard? "Copy: Zażółć gęślą jaźń 中文 😀" "Show Python syntax.")
                              "content" [{"id" "code"
                                          "type" "prose"
                                          "markdown"
                                          (str "```python\n"
                                               "vis_identifier_marker = \"vis_string_marker\"\n"
                                               "```")}]}]}

                           {})
                    data (.getBytes ^String (json/write-json-str body) "UTF-8")]

                (.sendResponseHeaders exchange 200 (alength data))
                (.write (.getResponseBody exchange) data))))))
    (.setExecutor server (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor))
    (.start server)
    server))

(defn- check-native-tui!
  [mode]
  (let [model-key
        (when (#{"c" "m"} mode) mode)

        clipboard?
        (contains? #{"osc52" "clip.exe"} mode)

        binary
        (io/file (or (System/getenv "VIS_TUI_NATIVE_BIN") "apps/vis-tui/target/vis-tui"))]

    (expect (.canExecute binary) "Build apps/vis-tui with clojure -T:build native first")
    (when (.canExecute binary)
      (let [requests
            (atom [])

            server
            (start-gateway-stub! (some? model-key) requests clipboard?)]

        (try
          (let [process (.start
                          (doto (ProcessBuilder.
                                  ^java.util.List
                                  ["python3" "test-native/com/blockether/vis/fixtures/tui_resize.py"
                                   (.getAbsolutePath binary)
                                   (str "127.0.0.1:" (.getPort (.getAddress server))) (or mode "")])
                            (.redirectErrorStream true)
                            (.redirectOutput ProcessBuilder$Redirect/PIPE)))]
            (try (let [finished? (.waitFor process 55 TimeUnit/SECONDS)]
                   (expect finished? "Native TUI PTY fixture timed out")
                   (when finished?
                     (let [output (slurp (.getInputStream process))]
                       (expect (zero? (.exitValue process)) output)
                       (expect (str/includes? output
                                              (cond clipboard? "native clipboard verified"
                                                    model-key
                                                    "input responsive during slow model HTTP"
                                                    :else "resized to 100x35"))
                               output)
                       (when model-key
                         (expect (some (fn [[method path]]
                                         (and (= "PATCH" method) (str/ends-with? path "/model")))
                                       @requests))))))
                 (finally (when (.isAlive process)
                            (with-open [children (.descendants process)]
                              (doseq [^ProcessHandle child (iterator-seq (.iterator children))]
                                (.destroyForcibly child)))
                            (.destroyForcibly process)
                            (.waitFor process 5 TimeUnit/SECONDS)))))
          (finally (.stop server 0)
                   (.shutdownNow ^java.util.concurrent.ExecutorService (.getExecutor server))))))))

(defdescribe native-tui-resize-test
             ;; Regression: Lanterna silently discarded native WINCH handler registration.
             (it "resizes and syntax-highlights persisted Python in the native terminal"
                 (check-native-tui! nil)))

(defdescribe
  native-tui-model-shortcuts-test
  ;; Model HTTP must never block the keyboard thread, including in native-image.
  (it "opens C-x c and accepts input while its model PATCH waits" (check-native-tui! "c"))
  (it "cycles C-x m and accepts input while its model HTTP waits" (check-native-tui! "m")))

(defdescribe native-tui-clipboard-test
             ;; Regression: lazy System/out resolved after log redirection, so the native
             ;; app showed Copied without ever sending OSC 52 to its controlling terminal.
             (it "copies bubbles and selections to the PTY, never the log, with no helpers"
                 (check-native-tui! "osc52"))
             (it "copies Unicode bubbles and selections through WSL clip.exe with UTF-16LE"
                 (check-native-tui! "clip.exe")))
