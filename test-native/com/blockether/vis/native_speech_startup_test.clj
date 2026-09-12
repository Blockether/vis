(ns com.blockether.vis.native-speech-startup-test
  "Speech discovery and round trips through an isolated native gateway and its CLI client."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [com.blockether.vis.internal.speech.assets :as assets]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File IOException]
           [java.net InetSocketAddress ServerSocket Socket]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

(defn- start-native!
  ([home binary label args] (start-native! home binary label args {}))
  ([^File home ^File binary label args environment]
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
     (.putAll env environment)
     (.put env "HOME" (.getAbsolutePath home))
     [(.start builder) log])))

(defn- listening?
  [port]
  (try (with-open [socket (Socket.)]
         (.connect socket (InetSocketAddress. "127.0.0.1" (int port)) 100)
         true)
       (catch IOException _ false)))

(defn- run-native!
  [home binary args timeout-secs]
  (let [[^Process client log] (start-native! home binary "client" args)]
    (try (expect (.waitFor client timeout-secs TimeUnit/SECONDS) "native speech query timed out")
         {:exit (.exitValue client) :output (slurp log)}
         (finally (when (.isAlive client) (#'native/kill-tree! client))))))

(defn- with-native-gateway
  "Own the gateway and its private home; only downloaded model assets may be shared."
  [prefix cached-models? f]
  (let [binary
        (#'native/require-binary)

        home
        (#'native/temp-dir prefix)]

    (try
      (let [port
            (with-open [socket (ServerSocket. 0)]
              (.getLocalPort socket))

            environment
            (if cached-models? {assets/models-dir-env (assets/models-root)} {})

            [^Process gateway gateway-log]
            (start-native! home
                           binary
                           "gateway"
                           ["gateway" "start" "--host" "127.0.0.1" "--port" (str port)]
                           environment)]

        (try (let [started?
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
             (f home #(run-native! home binary %1 %2))
             (finally (when (.isAlive gateway) (#'native/kill-tree! gateway)))))
      (finally (#'native/delete-tree! home)))))

(defdescribe native-speech-discovery-test
             (it "keeps both deferred engine catalogs callable without downloading models"
                 (with-native-gateway "vis-native-speech-startup-"
                                      false
                                      (fn [_ run!]
                                        (doseq [[args expected]
                                                [[["speech" "models" "status"]
                                                  ["Parakeet" "piper" "pocket-tts" "absent"]]
                                                 [["speech" "voices"] ["piper" "pocket-tts"]]]]
                                          (let [{:keys [exit output]} (run! args 60)]
                                            (expect (= 0 exit) output)
                                            (doseq [word expected]
                                              (expect (str/includes? output word) output))))))))

(defn- plain-words
  "Lowercase words only. A transcript differs from what was spoken by punctuation
   and casing long before it differs by a word."
  [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z0-9]+")
       (remove str/blank?)
       (str/join " ")))

(defn- heard-most-words?
  "ASR round-trip proof tolerant of one acoustic substitution."
  [output sentence]
  (let [words
        #(set (str/split (plain-words %) #" "))

        expected
        (words sentence)

        heard
        (words output)]

    (<= (count (set/difference expected heard)) 1)))

(defn- wav-facts
  "What a WAV header claims, read by hand: `nil` when the file is not one."
  [^File f]
  (when (and (.isFile f) (> (.length f) 44))
    (let [header (byte-array 44)]
      (with-open [in (io/input-stream f)]
        (.read in header))
      (let [tag (fn [from]
                  (String. header from 4 StandardCharsets/US_ASCII))
            u32 (fn [from]
                  (reduce (fn [acc i]
                            (+ acc
                               (bit-shift-left (bit-and (long (aget header (+ from i))) 0xff)
                                               (* 8 i))))
                          0
                          (range 4)))]

        (when (and (= "RIFF" (tag 0)) (= "WAVE" (tag 8)))
          {:sample-rate (u32 24) :bytes (.length f)})))))

(def ^:private spoken-sentence
  "Plain words on purpose: this sentence is spoken by one engine and read back by
   another, and a rare word would test the vocabulary rather than the image."
  "Local speech now runs entirely on this machine, with no account and no network.")

(defdescribe
  native-binary-speaks-and-listens-test
  ;; #212: a protocol bump exposed that these tests used the developer's running
  ;; gateway, not the linked image. Own both processes without touching that service.
  ;; The native gateway must exercise JNI/model discovery, synthesis and recognition;
  ;; a successful image build or JVM-only round trip cannot establish this.
  ;; Missing models download here. Share only the canonical model cache, never
  ;; the developer's configuration, database, credentials or gateway registry.
  (it
    "speaks a sentence and reads its own recording back"
    (with-native-gateway
      "vis-native-voice-"
      true
      (fn [home run!]
        (let [wav
              (io/file home "spoken.wav")

              said
              (run! ["speech" "say" spoken-sentence "--out" (.getAbsolutePath wav)] 900)]

          (expect (= 0 (:exit said)) (:output said))
          (expect
            (nil?
              (re-find
                #"ClassNotFoundException|NoClassDefFoundError|UnsatisfiedLinkError|NoSuchMethodError"
                (:output said)))
            (:output said))
          (let [facts (wav-facts wav)]
            (expect facts (str "no WAV at " (.getAbsolutePath wav) ":\n" (:output said)))
            (expect (<= 8000 (long (:sample-rate facts)) 48000)
                    (str "implausible sample rate: " facts))
            (expect (> (long (:bytes facts)) 20000)
                    (str "the binary wrote a WAV with nothing in it: " facts)))
          (let [heard (run! ["speech" "transcribe" (.getAbsolutePath wav)] 900)]
            (expect (= 0 (:exit heard)) (:output heard))
            (expect (heard-most-words? (:output heard) spoken-sentence)
                    (str "the binary did not hear what it had just said:\n" (:output heard))))))))
  (it "speaks with the pocket-tts export Vis publishes itself"
      ;; Piper uses VITS; pocket-tts uses a separate ONNX config and reference clip.
      (with-native-gateway
        "vis-native-pocket-"
        true
        (fn [home run!]
          (let [wav
                (io/file home "pocket.wav")

                said
                (run! ["speech" "say" "The bundle we ship is the ONNX export itself." "--pocket-tts"
                       "--out" (.getAbsolutePath wav)]
                      900)]

            (expect (= 0 (:exit said)) (:output said))
            (let [facts (wav-facts wav)]
              (expect facts (str "no WAV at " (.getAbsolutePath wav) ":\n" (:output said)))
              (expect (= 24000 (long (:sample-rate facts)))
                      (str "pocket-tts speaks at 24 kHz; got " facts)))))))
  (it "lists the voices this machine can speak in without loading a model"
      (with-native-gateway "vis-native-voices-"
                           false
                           (fn [_ run!]
                             (let [{:keys [exit output]} (run! ["speech" "voices"] 120)]
                               (expect (= 0 exit) output)
                               (expect (str/includes? output "piper") output)
                               (expect (str/includes? output "pocket-tts") output))))))
