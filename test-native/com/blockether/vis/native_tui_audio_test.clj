(ns com.blockether.vis.native-tui-audio-test
  "Compare native Java Sound with the JVM on the same host, without opening a microphone."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util ServiceLoader]
           [java.util.concurrent TimeUnit]
           [javax.sound.sampled AudioFormat AudioSystem DataLine$Info SourceDataLine TargetDataLine]
           [javax.sound.sampled.spi MixerProvider]))

(defn- line-available?
  [line-type format]
  (try (let [line (AudioSystem/getLine (DataLine$Info. line-type format))]
         (.close line)
         true)
       (catch Throwable _ false)))

(defn- jvm-audio
  []
  (let [format (AudioFormat. 16000.0 16 1 true false)]
    {"providers" (count (iterator-seq (.iterator (ServiceLoader/load MixerProvider))))
     "mixers" (alength (AudioSystem/getMixerInfo))
     "target-line" (if (line-available? TargetDataLine format) "available" "unavailable")
     "source-line" (if (line-available? SourceDataLine format) "available" "unavailable")}))

(defn- run-native-audio
  [^java.io.File binary]
  (let [process (.start (doto (ProcessBuilder.
                                ^"[Ljava.lang.String;"
                                (into-array String [(.getAbsolutePath binary) "--check-audio"]))
                          (.redirectErrorStream true)))]
    (try (if (.waitFor process 30 TimeUnit/SECONDS)
           {:exit (.exitValue process) :output (slurp (.getInputStream process))}
           {:exit :timeout :output "native audio check timed out"})
         (finally (when (.isAlive process) (.destroyForcibly process))))))

;; Regression, issue #293: the shipped macOS image omitted libjsound.dylib and
;; Java Sound tried to read a nonexistent native-image java.home. Compare with the
;; JVM on this host so headless CI need not have a microphone or speakers.
(defdescribe native-tui-audio-test
             (it "discovers the same providers, devices and capture/playback lines as the JVM"
                 (let [binary (io/file (or (System/getenv "VIS_TUI_NATIVE_BIN")
                                           "apps/vis-tui/target/vis-tui"))]
                   (expect (.canExecute binary)
                           "Build apps/vis-tui with clojure -T:build native first")
                   (when (.canExecute binary)
                     (let [{:keys [exit output]} (run-native-audio binary)
                           expected (jvm-audio)]

                       (expect (= 0 exit) output)
                       (doseq [[label value] expected]
                         (expect (str/includes? output (str label "=" value))
                                 (str label " differs from JVM: " output)))
                       (expect (not (str/includes? output "Can't find java.home")) output))))))
