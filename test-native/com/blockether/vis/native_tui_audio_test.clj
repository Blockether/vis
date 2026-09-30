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
  ([binary] (run-native-audio binary nil))
  ([^java.io.File binary path]
   (let [builder (doto (ProcessBuilder. ^"[Ljava.lang.String;"
                                        (into-array String
                                                    [(.getAbsolutePath binary) "--check-audio"]))
                   (.redirectErrorStream true))]
     (when path (.put (.environment builder) "PATH" path))
     (let [process (.start builder)]
       (try (if (.waitFor process 30 TimeUnit/SECONDS)
              {:exit (.exitValue process) :output (slurp (.getInputStream process))}
              {:exit :timeout :output "native audio check timed out"})
            (finally (when (.isAlive process) (.destroyForcibly process))))))))

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

;; Regression, issue #298: standard Homebrew tools must remain visible with a minimal PATH.
(defdescribe
  native-tui-recorder-discovery-test
  (it "reports recorder paths and missing programs without opening a microphone"
      (let [binary
            (io/file (or (System/getenv "VIS_TUI_NATIVE_BIN") "apps/vis-tui/target/vis-tui"))

            macos?
            (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")

            directories
            (cond-> ["/usr/bin" "/bin"]
              macos?
              (into ["/opt/homebrew/bin" "/usr/local/bin"]))

            programs
            (if macos?
              [["sox" "sox"] ["ffmpeg" "ffmpeg"]]
              [["pipewire" "pw-record"] ["pulse" "parec"]])]

        (expect (.canExecute binary) "Build apps/vis-tui with clojure -T:build native first")
        (when (.canExecute binary)
          (let [{:keys [exit output]} (run-native-audio binary "/usr/bin:/bin")]
            (expect (= 0 exit) output)
            (doseq [[backend command] programs]
              (let [path (some (fn [dir]
                                 (let [file (io/file dir command)]
                                   (when (and (.isFile file) (.canExecute file))
                                     (.getAbsolutePath file))))
                               directories)]
                (expect (str/includes? output (str backend "=" (or path "unavailable")))
                        output))))))))
