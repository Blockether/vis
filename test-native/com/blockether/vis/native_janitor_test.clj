;; Native coverage for the maintenance worker in the linked gateway image.
(ns com.blockether.vis.native-janitor-test
  "The maintenance worker in the linked gateway image. The start of a process gives
   each stale-state sweep to the worker. Only a real start shows that it did: the
   unit suite calls the sweeps directly and never runs `-main`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.net ServerSocket]))

(def ^:private ^:const day-ms 86400000)

(defn- seed!
  "Write `file`, with `age-days` as the age of its last change."
  ^File [^File file age-days]
  (io/make-parents file)
  (spit file "line\n")
  (.setLastModified file (- (System/currentTimeMillis) (* (long age-days) (long day-ms))))
  file)

(defn- eventually?
  "Whether `pred` becomes true in `secs` seconds while `process` runs."
  [^Process process secs pred]
  (let [deadline (+ (System/nanoTime) (* (long secs) 1000000000))]
    (loop []

      (cond (pred) true
            (and (.isAlive process) (< (System/nanoTime) deadline)) (do (Thread/sleep 200) (recur))
            :else false))))

(defn- gateway-log
  "The text of each process log that a gateway wrote below `logs`."
  [^File logs]
  (->> (file-seq logs)
       (filter (fn [^File file]
                 (and (.isFile file) (str/starts-with? (.getName file) "gateway-"))))
       (map slurp)
       (str/join "\n")))

(defdescribe
  native-janitor-test
  ;; The sweeps wait 30 seconds after the start, so this test runs for that time. The
  ;; fresh file is in a folder with an old date: the age of a file decides, not its folder.
  (it
    "removes a stale log file in a running gateway and writes the run to the gateway log"
    (let [^File dir
          (#'binary/temp-dir "vis-native-janitor-")

          ^File bin
          (#'binary/require-binary)

          logs
          (io/file dir ".vis" "logs")

          stale
          (seed! (io/file logs "2020-01-01" "stale.log") 30)

          fresh
          (seed! (io/file logs "2020-01-02" "fresh.log") 0)

          port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)]

      (try (let [pb
                 (doto (ProcessBuilder. ^java.util.List
                                        [(.getAbsolutePath bin)
                                         (str "-Duser.home=" (.getAbsolutePath dir))
                                         (str "-Djava.io.tmpdir=" (.getAbsolutePath dir)) "gateway"
                                         "start" "--host" "127.0.0.1" "--port" (str port) "--db"
                                         (.getAbsolutePath (io/file dir "sessions"))])
                   (.directory dir)
                   (.redirectErrorStream true)
                   (.redirectOutput (io/file dir "gateway.out")))

                 environment
                 (.environment pb)]

             (.putAll environment (#'binary/native-environment))
             (doseq [key ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED" "VIS_HOME"]]
               (.remove environment key))
             (reset! process (.start pb)))
           (expect (eventually? @process 90 #(not (.exists stale))))
           (expect (.exists fresh))
           ;; The entry of a run that did work has a result. The warning of a failed run
           ;; has none.
           (expect (eventually? @process
                                10
                                #(re-find
                                   #":task :housekeeping/logs, :ms \d+, :result \{:deleted 1,"
                                   (gateway-log logs))))
           (finally (when-let [owned @process]
                      (#'binary/kill-tree! owned))
                    (#'binary/delete-tree! dir))))))
