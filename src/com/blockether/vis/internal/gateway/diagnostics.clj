(ns com.blockether.vis.internal.gateway.diagnostics
  "Best-effort local hang evidence, independent of Python worker retirement.

   Gateway watchdogs capture platform-thread stacks before cancellation/cleanup.
   An authenticated local client can additionally ask a previously observed JVM
   for platform AND virtual stacks through jcmd when HTTP stops responding.
   Neither path stops/restarts the gateway. A frozen VM may also refuse attach;
   that failure is evidence, not proof of a particular cause. No HTTP bodies,
   credentials, prompts, tool arguments or process command-line arguments are saved."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.io File)
           (java.lang ProcessBuilder$Redirect ProcessHandle)
           (java.nio.file CopyOption Files Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(def ^:private CAPTURE_MS 500)

(def ^:private ATTACH_MS 5000)

(def ^:private PROBE_COOLDOWN_MS 60000)

(def ^:private MAX_REPORTS 10)

(defonce ^:private capturing? (atom false))

(defonce ^:private attaching? (atom false))

(defonce ^:private observed-target (atom nil))

(defn- bounded!
  "One platform helper at a time, even if an interrupted collector never unwinds."
  [busy? timeout-ms f]
  (if-not (compare-and-set! busy? false true)
    {:status :busy}
    (let [interrupted?
          (Thread/interrupted)

          result
          (promise)

          helper
          (Thread. ^Runnable
                   (fn []
                     (try (deliver result (f))
                          (catch Throwable error
                            (deliver result {:status :failed :error-type (.getName (class error))}))
                          (finally (reset! busy? false))))
                   "vis-gateway-hang-diagnostic")]

      (try (.setDaemon helper true)
           (.start helper)
           (let [value (deref result timeout-ms {:status :timed-out})]
             (when (= :timed-out (:status value)) (.interrupt helper))
             value)
           (catch InterruptedException _
             (.interrupt helper)
             (.interrupt (Thread/currentThread))
             {:status :interrupted})
           (catch Throwable _ (when-not (.isAlive helper) (reset! busy? false)) {:status :failed})
           (finally (when interrupted? (.interrupt (Thread/currentThread))))))))

(defn- private-attributes
  [^Path parent permissions]
  (if (.supportsFileAttributeView (Files/getFileStore parent) "posix")
    (into-array FileAttribute
                [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString
                                                         permissions))])
    (make-array FileAttribute 0)))

(defn- report-directory!
  ^File []
  (let [parent (Path/of (paths/ensure-logs-dir!) (make-array String 0))]
    (.toFile
      (Files/createTempDirectory parent "gateway-hang-" (private-attributes parent "rwx------")))))

(defn- private-file!
  ^File [^File dir name]
  (.toFile (Files/createFile (.toPath (io/file dir name))
                             (private-attributes (.toPath dir) "rw-------"))))

(defn- prune-reports!
  []
  (let [reports (->> (.listFiles (io/file (paths/logs-dir)))
                     (filter (fn [^File dir]
                               (and (re-matches #"gateway-hang-[0-9]+" (.getName dir))
                                    (not (Files/isSymbolicLink (.toPath dir)))
                                    (.isFile (io/file dir "report.json")))))
                     (sort-by (fn [^File dir]
                                (- (.lastModified dir)))))]
    (doseq [^File dir (drop MAX_REPORTS reports)]
      ;; Only our fixed report files; never recurse into an unexpected directory.
      (doseq [name ["report.json" "threads.json" "attach.log"]]
        (Files/deleteIfExists (.toPath (io/file dir name))))
      (Files/deleteIfExists (.toPath dir)))))

(defn- finish-report!
  [^File dir report]
  (let [^File file
        (private-file! dir ".report.json")

        result
        {:status (:status report) :path (str (io/file dir "report.json"))}]

    (spit file
          (str (wire/json-str (assoc report
                                :schema-version 1
                                :recorded-ms (System/currentTimeMillis)))
               "\n")
          :encoding
          "UTF-8")
    (Files/move (.toPath file)
                (.toPath (io/file dir "report.json"))
                (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (try (prune-reports!) (catch Throwable _ nil))
    (tel/log! {:level :warn :id ::hang-evidence :data result}
              "Gateway hang diagnostic saved locally")
    result))

(defn- write-snapshot!
  [context]
  (let [stacks
        (Thread/getAllStackTraces)

        target-name
        (str "gateway-turn-" (:turn-id context))

        threads
        (sort-by (fn [[^Thread thread _]]
                   [(not= target-name (.getName thread)) (.threadId thread)])
                 stacks)]

    (finish-report! (report-directory!)
                    (merge (select-keys context
                                        [:session-id :turn-id :reason :phase :idle-ms :grace-ms])
                           {:status :written
                            :pid (.pid (ProcessHandle/current))
                            :coverage :platform-threads
                            :thread-count (count stacks)
                            :threads (mapv (fn [[^Thread thread stack]]
                                             {:id (.threadId thread)
                                              :name (util/redact-secret-text (.getName thread))
                                              :state (str (.getState thread))
                                              :stack (mapv str (take 64 stack))})
                                           (take 256 threads))}))))

(defn capture!
  "Save private, bounded platform stacks before a gateway watchdog tears down a
   stalled turn. Adds at most CAPTURE_MS of waiting; failures never prevent recovery.
   Reports are UTF-8 JSON with snake_case field names under
   ~/.vis/logs/gateway-hang-*/report.json (newest ten retained).
   JVM virtual threads are not enumerated by Thread/getAllStackTraces."
  [context]
  (try (bounded! capturing? CAPTURE_MS #(write-snapshot! context))
       (catch Throwable _ {:status :failed})))

(defn- local-jvm-target
  [{:keys [pid remote?]}]
  (when (and (not remote?) (integer? pid) (pos? (long pid)))
    (when-let [^ProcessHandle handle (.orElse (ProcessHandle/of (long pid)) nil)]
      (let [info (.info handle)
            started (.orElse (.startInstant info) nil)
            command (.orElse (.command info) nil)]

        (when (and (.isAlive handle)
                   started
                   command
                   (#{"java" "java.exe"} (.getName (io/file command))))
          (let [jcmd (io/file (.getParentFile (io/file command))
                              (if (= "java.exe" (.getName (io/file command))) "jcmd.exe" "jcmd"))]
            (when (.canExecute jcmd)
              {:pid pid :handle handle :started started :jcmd (.getAbsolutePath jcmd)})))))))

(defn- same-process?
  [{:keys [^ProcessHandle handle started]}]
  (and (.isAlive handle) (= started (.orElse (.startInstant (.info handle)) nil))))

(defn- start-attach!
  ^Process [{:keys [pid jcmd]} ^File stacks ^File log]
  (let [builder (doto (ProcessBuilder. ^java.util.List
                                       (vec [jcmd (str pid) "Thread.dump_to_file" "-overwrite"
                                             "-format=json" (.getAbsolutePath stacks)]))
                  (.redirectErrorStream true)
                  (.redirectOutput (ProcessBuilder$Redirect/to log)))]
    ;; Diagnostic tooling needs no injected Java agents/options, whose startup
    ;; banners could otherwise copy environment values to attach.log.
    (doseq [name ["JAVA_TOOL_OPTIONS" "JDK_JAVA_OPTIONS" "_JAVA_OPTIONS"]]
      (.remove (.environment builder) name))
    (.start builder)))

(defn- attach-snapshot!
  [{:keys [pid] :as target}]
  (if-not (same-process? target)
    {:status :gone}
    (let [dir
          (report-directory!)

          stacks
          (private-file! dir "threads.json")

          log
          (private-file! dir "attach.log")

          report
          {:pid pid
           :process-started (str (:started target))
           :reason :gateway-health-probe-failed
           :coverage :platform-and-virtual-threads}]

      (try (if-not (same-process? target)
             (finish-report! dir (assoc report :status :gone))
             (let [process (start-attach! target stacks log)]
               (try (let [finished? (.waitFor process (long ATTACH_MS) TimeUnit/MILLISECONDS)]
                      (finish-report! dir
                                      (cond-> (assoc report
                                                :status (cond (not finished?) :timed-out
                                                              (and (zero? (.exitValue process))
                                                                   (pos? (.length stacks)))
                                                              :written
                                                              :else :failed))
                                        finished?
                                        (assoc :exit (.exitValue process)))))
                    ;; Kill only the diagnostic command, NEVER the target JVM. Closing an
                    ;; attach client cannot guarantee the VM stops a dump already accepted.
                    (finally (when (.isAlive process)
                               (.destroyForcibly process)
                               (.waitFor process 100 TimeUnit/MILLISECONDS))))))
           (catch Throwable error
             (finish-report! dir
                             (assoc report
                               :status :failed
                               :error-type (.getName (class error)))))))))

(defn observe-probe!
  "Observe the canonical client's authenticated health result. A failed probe may
   collect external JVM stacks ONLY for a previously successful local PID/start
   identity with a sibling jcmd. At most once per minute, one helper at a time.
   Returns immediately; remote/native/unknown processes are never attached/signalled.
   This needs a running local client, and attach can itself fail on a frozen JVM."
  [entry healthy?]
  (try (when-not (:remote? entry)
         (if healthy?
           (let [target (local-jvm-target entry)]
             (swap! observed-target (fn [previous]
                                      (if (= (select-keys previous [:pid :started])
                                             (select-keys target [:pid :started]))
                                        previous
                                        target))))
           (let [target
                 @observed-target

                 now
                 (System/nanoTime)]

             (when (and target
                        (= (:pid entry) (:pid target))
                        (same-process? target)
                        (or (nil? (:attempt-ns target))
                            (>= (- now (long (:attempt-ns target)))
                                (* (long PROBE_COOLDOWN_MS) 1000000)))
                        (compare-and-set! observed-target target (assoc target :attempt-ns now)))
               (doto (Thread. ^Runnable
                              #(bounded! attaching?
                                         (+ (long ATTACH_MS) 500)
                                         (fn []
                                           (attach-snapshot! target)))
                              "vis-gateway-external-diagnostic")
                 (.setDaemon true)
                 (.start))))))
       (catch Throwable _ nil))
  nil)
