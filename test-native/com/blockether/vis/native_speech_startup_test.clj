(ns com.blockether.vis.native-speech-startup-test
  "Deferred speech engine discovery through an isolated native gateway and its CLI client."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File IOException]
           [java.net InetSocketAddress ServerSocket Socket]
           [java.util.concurrent TimeUnit]))

(defn- start-native!
  [^File home ^File binary label args]
  (let [log
        (io/file home (str label ".log"))

        builder
        (doto (ProcessBuilder. ^java.util.List
                               (into [(.getAbsolutePath binary) (str "-Duser.home=" home)] args))
          (.directory home)
          (.redirectErrorStream true)
          (.redirectOutput log))

        env
        (.environment builder)

        kept
        (select-keys (into {} env) ["PATH" "JAVA_HOME" "SystemRoot" "TMPDIR" "TMP" "TEMP"])]

    (.clear env)
    (.putAll env kept)
    (.putAll env (#'native/native-environment))
    (.put env "HOME" (.getAbsolutePath home))
    [(.start builder) log]))

(defn- listening?
  [port]
  (try (with-open [socket (Socket.)]
         (.connect socket (InetSocketAddress. "127.0.0.1" (int port)) 100)
         true)
       (catch IOException _ false)))

(defdescribe
  native-speech-discovery-test
  (it
    "keeps both deferred engine catalogs callable without downloading models"
    (let [^File binary
          (#'native/require-binary)

          ^File home
          (#'native/temp-dir "vis-native-speech-startup-")]

      (try
        (let [port
              (with-open [socket (ServerSocket. 0)]
                (.getLocalPort socket))

              [^Process gateway gateway-log]
              (start-native! home
                             binary
                             "gateway"
                             ["gateway" "start" "--host" "127.0.0.1" "--port" (str port)])]

          (try
            (let [started?
                  (loop [remaining 300]
                    (cond (not (.isAlive gateway)) false
                          (listening? port) true
                          (zero? remaining) false
                          :else (do (Thread/sleep 50) (recur (dec remaining)))))

                  errors
                  (->> (cons gateway-log
                             (filter #(and (.isFile ^File %)
                                           (str/ends-with? (.getName ^File %) ".log"))
                                     (file-seq (io/file home ".vis/logs"))))
                       (mapcat #(str/split-lines (slurp %)))
                       (filter #(re-find #"(?i)error|exception|failed|unknown|invalid|usage" %))
                       (take-last 20)
                       (str/join "\n"))]

              (expect started?
                      (str "isolated native gateway did not start listening; exit="
                           (when-not (.isAlive gateway) (.exitValue gateway))
                           "\n" errors)))
            ;; The CLI uses gateway.client/request!: no copied registry credentials or
            ;; alternate HTTP client. Both processes have the same private home and DB.
            (doseq [[label args expected] [["models" ["speech" "models" "status"]
                                            ["Parakeet" "piper" "pocket-tts" "absent"]]
                                           ["voices" ["speech" "voices"] ["piper" "pocket-tts"]]]]
              (let [[^Process client log] (start-native! home binary label args)]
                (try (expect (.waitFor client 60 TimeUnit/SECONDS) "native speech query timed out")
                     (let [output (slurp log)]
                       (expect (= 0 (.exitValue client)) output)
                       (doseq [word expected]
                         (expect (str/includes? output word) output)))
                     (finally (when (.isAlive client) (#'native/kill-tree! client))))))
            (finally (when (.isAlive gateway) (#'native/kill-tree! gateway)))))
        (finally (#'native/delete-tree! home))))))
