(ns com.blockether.vis.internal.gateway.diagnostics-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.diagnostics :as diagnostics]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.lang ProcessBuilder$Redirect)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent CountDownLatch TimeUnit)))

(defn- with-logs
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-gateway-diagnostics-test-"
                                                (make-array FileAttribute 0)))]
    (try (with-redefs [paths/logs-dir #(.getAbsolutePath dir)]
           (f dir))
         (finally (doseq [^File file (reverse (file-seq dir))]
                    (io/delete-file file true))))))

(defn- await-value
  [f]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []

      (let [value (f)]
        (cond value value
              (>= (System/nanoTime) deadline) nil
              :else (do (Thread/sleep 10) (recur)))))))

(defdescribe
  gateway-diagnostic-triggers-test
  (it
    "captures a provider-only stall before cancellation without Python retirement"
    (let [token
          (cancellation/cancellation-token)

          evidence
          (atom [])

          cancelled
          (promise)

          stall
          (atom {:phase :provider-call :started? true :produced? true :last-ms 0})]

      (cancellation/on-cancel! token #(deliver cancelled @evidence))
      (with-redefs [state/TURN_STALL_TIMEOUT_MS
                    25

                    state/cancel-terminal-grace-ms
                    (constantly 1)

                    state/turn-watchdog-live?
                    (fn [& _]
                      (not (cancellation/cancelled? token)))

                    diagnostics/capture!
                    #(swap! evidence conj [% (cancellation/cancelled? token)])]

        (#'state/start-turn-stall-watchdog! "diagnostic-session" "diagnostic-turn" token stall)
        (let [result (deref cancelled 2000 ::timeout)]
          (expect (not= ::timeout result))
          (expect (= [[{:session-id "diagnostic-session"
                        :turn-id "diagnostic-turn"
                        :reason :turn-stall
                        :phase :provider-call
                        :idle-ms (:idle-ms (ffirst result))} false]]
                     result))))))
  (it "captures a stuck cancellation before terminal persistence and engine disposal"
      (let [events
            (atom [])

            landed
            (promise)]

        (with-redefs [state/session-entry
                      (constantly {:current-turn "diagnostic-turn"})

                      loop-env/condemn-env!
                      (fn [& _]
                        nil)

                      diagnostics/capture!
                      #(swap! events conj (:reason %))]

          (#'state/start-cancel-terminal-backstop!
           "diagnostic-session"
           "diagnostic-turn"
           (cancellation/cancellation-token)
           1
           (fn [& _]
             (deliver landed @events)))
          (expect (= [:cancel-backstop] (deref landed 2000 ::timeout))))))
  (it "feeds both successful and failed canonical health probes to the independent collector"
      (let [events
            (atom [])

            entry
            {:pid 123 :secret "fixture-secret"}]

        (with-redefs [diagnostics/observe-probe!
                      #(swap! events conj [%1 %2])

                      client/gw-send!
                      (fn [& _]
                        {:status 200 :body "{\"status\":\"ok\",\"secret_match\":true}"})]

          (expect (true? (#'client/probe-entry? entry))))
        (with-redefs [diagnostics/observe-probe!
                      #(swap! events conj [%1 %2])

                      client/gw-send!
                      (fn [& _]
                        (throw (ex-info "fixture timeout" {})))]

          (expect (false? (#'client/probe-entry? entry))))
        (expect (= [[entry true] [entry false]] @events)))))

(defdescribe
  gateway-diagnostic-capture-test
  (it
    "saves a blocked turn's stacks as JSON with only allowlisted metadata in private files"
    (with-logs
      (fn [_]
        (let [gate
              (CountDownLatch. 1)

              ready
              (promise)

              thread
              (doto (Thread. ^Runnable #(do (deliver ready true) (.await gate))
                             "gateway-turn-fixture")
                (.setDaemon true)
                (.start))]

          (try
            (expect (true? (deref ready 2000 false)))
            (let [result (diagnostics/capture! {:session-id "session"
                                                :turn-id "fixture"
                                                :phase :provider-call
                                                :reason :turn-stall
                                                :prompt "must-not-be-in-report"})]
              (expect (= :written (:status result)))
              (when-let [path (:path result)]
                (let [text (slurp path :encoding "UTF-8")
                      report (wire/parse-json text)
                      target (first (get report "threads"))
                      file (.toPath (io/file path))]

                  (expect (= "report.json" (.getName (io/file path))))
                  (expect (re-matches #"\d{4}-\d{2}-\d{2}"
                                      (.getName (.getParentFile (.getParentFile (io/file path))))))
                  (expect (= #{"report.json"} (set (.list (.getParentFile (io/file path))))))
                  (expect (map? report))
                  (expect (= 1 (get report "schema_version")))
                  (expect (pos-int? (get report "recorded_ms")))
                  (expect (= {"status" "written"
                              "session_id" "session"
                              "turn_id" "fixture"
                              "phase" "provider-call"
                              "reason" "turn-stall"
                              "coverage" "platform-threads"}
                             (select-keys report
                                          ["status" "session_id" "turn_id" "phase" "reason"
                                           "coverage"])))
                  (expect (= "gateway-turn-fixture" (get target "name")))
                  (expect (seq (get target "stack")))
                  (expect (<= (count (get report "threads")) 256))
                  (expect (str/ends-with? text "\n"))
                  (expect (not (str/includes? text "must-not-be-in-report")))
                  (when (.supportsFileAttributeView (Files/getFileStore file) "posix")
                    (expect (= "rw-------"
                               (PosixFilePermissions/toString
                                 (Files/getPosixFilePermissions file (make-array LinkOption 0)))))
                    (expect (= "rwx------"
                               (PosixFilePermissions/toString (Files/getPosixFilePermissions
                                                                (.getParent file)
                                                                (make-array LinkOption 0)))))))))
            (finally (.countDown gate) (.join thread 2000)))))))
  (it "retains the newest completed reports across dates without touching unrelated files"
      (with-logs
        (fn [dir]
          (let [old
                (doto (io/file dir "2026-09-01" "gateway-hang-1") .mkdirs)

                recent
                (doto (io/file dir "2026-09-02" "gateway-hang-2") .mkdirs)

                incomplete
                (doto (io/file dir "2026-09-01" "gateway-hang-3") .mkdirs)]

            (spit (io/file dir "unrelated.log") "keep")
            (spit (io/file incomplete ".report.json") "unfinished")
            (spit (io/file old "report.json") "{}")
            (spit (io/file recent "report.json") "{}")
            (.setLastModified old 1000)
            (.setLastModified recent 2000)
            (with-redefs [diagnostics/MAX_REPORTS 2]
              (let [first-result (#'diagnostics/write-snapshot! {:reason :turn-stall})
                    second-result (#'diagnostics/write-snapshot! {:reason :turn-stall})]

                (expect (= :written (:status first-result)))
                (expect (= :written (:status second-result)))
                (expect (not= (:path first-result) (:path second-result)))
                (expect (.isFile (io/file (:path first-result))))
                (expect (.isFile (io/file (:path second-result))))))
            (expect (not (.exists old)))
            (expect (not (.exists recent)))
            (expect (= "unfinished" (slurp (io/file incomplete ".report.json"))))
            (expect (= "keep" (slurp (io/file dir "unrelated.log"))))))))
  (it "bounds a stuck helper and refuses to accumulate more helpers"
      (let [busy?
            (atom false)

            gate
            (CountDownLatch. 1)

            started
            (promise)]

        (try (let [result (#'diagnostics/bounded!
                           busy?
                           50
                           #(do (deliver started true)
                                (loop []

                                  (when-not (try (.await gate 10 TimeUnit/MILLISECONDS)
                                                 (catch InterruptedException _ false))
                                    (recur)))
                                {:status :written}))]
               (expect (true? (deref started 1000 false)))
               (expect (= :timed-out (:status result)))
               (expect (= :busy (:status (#'diagnostics/bounded! busy? 50 (constantly {}))))))
             (finally (.countDown gate) (expect (await-value #(not @busy?)))))))
  (it "contains capture failures and preserves an existing interrupt flag"
      (let [busy? (atom false)]
        (.interrupt (Thread/currentThread))
        (try (expect (= :failed
                        (:status
                          (#'diagnostics/bounded! busy? 500 #(throw (ex-info "not copied" {}))))))
             (expect (.isInterrupted (Thread/currentThread)))
             (finally (Thread/interrupted))))))

(defdescribe
  gateway-external-diagnostic-test
  (it
    "requires prior local authentication, pins the process identity and rate-limits retries"
    (let [observed
          @#'diagnostics/observed-target

          previous
          @observed

          calls
          (atom [])

          target
          (atom {:pid 123 :started "first"})

          alive?
          (atom true)]

      (try (reset! observed nil)
           (with-redefs [diagnostics/local-jvm-target
                         (fn [_]
                           @target)

                         diagnostics/same-process?
                         (fn [_]
                           @alive?)

                         diagnostics/attach-snapshot!
                         #(do (swap! calls conj %) {:status :written})]

             (diagnostics/observe-probe! {:pid 123} false)
             (expect (empty? @calls))
             (diagnostics/observe-probe! {:pid 123} true)
             (diagnostics/observe-probe! {:pid 123 :remote? true} false)
             (expect (empty? @calls))
             (diagnostics/observe-probe! {:pid 456} false)
             (expect (empty? @calls))
             (diagnostics/observe-probe! {:pid 123} false)
             (expect (await-value #(= 1 (count @calls))))
             (diagnostics/observe-probe! {:pid 123} true)
             (diagnostics/observe-probe! {:pid 123} false)
             (expect (= 1 (count @calls)))
             (reset! target {:pid 123 :started "reused-pid"})
             (diagnostics/observe-probe! {:pid 123} true)
             (reset! alive? false)
             (diagnostics/observe-probe! {:pid 123} false)
             (expect (= 1 (count @calls))))
           (finally (reset! observed previous)))))
  (it "bounds a hung attach command, records failure and kills only the helper"
      (with-logs
        (fn [_]
          (let [destroyed?
                (atom false)

                waits
                (atom [])

                helper
                (proxy [Process] []
                  (isAlive [] (not @destroyed?))
                  (waitFor [ms _unit] (swap! waits conj ms) @destroyed?)
                  (destroyForcibly [] (reset! destroyed? true) this))]

            (with-redefs [diagnostics/same-process?
                          (constantly true)

                          diagnostics/ATTACH_MS
                          20

                          diagnostics/start-attach!
                          (fn [& _]
                            helper)]

              (let [result (#'diagnostics/attach-snapshot! {:pid 123 :started "fixture"})]
                (expect (= :timed-out (:status result)))
                (expect (= "timed-out" (get (wire/parse-json (slurp (:path result))) "status"))))
              (expect @destroyed?)
              (expect (= [20 100] @waits)))))))
  (it "records attach startup errors without copying exception messages"
      (with-logs (fn [_]
                   (with-redefs [diagnostics/same-process?
                                 (constantly true)

                                 diagnostics/start-attach!
                                 (fn [& _]
                                   (throw (ex-info "private-fixture" {})))]

                     (let [result (#'diagnostics/attach-snapshot! {:pid 123 :started "fixture"})]
                       (expect (= :failed (:status result)))
                       (let [text (slurp (:path result))
                             report (wire/parse-json text)]

                         (expect (= "failed" (get report "status")))
                         (expect (= "clojure.lang.ExceptionInfo" (get report "error_type")))
                         (expect (not (str/includes? text "private-fixture")))))))))
  (it
    "collects platform and virtual stacks from an isolated JVM without stopping it"
    (with-logs
      (fn [dir]
        (let
          [java
           (io/file
             (System/getProperty "java.home")
             "bin"
             (if (str/starts-with? (System/getProperty "os.name") "Windows") "java.exe" "java"))

           output
           (io/file dir "fixture.log")

           process
           (.start
             (doto
               (ProcessBuilder.
                 ^java.util.List
                 [(.getAbsolutePath java) "-cp" (System/getProperty "java.class.path")
                  "clojure.main" "-e"
                  "(do (.setName (Thread/currentThread) \"hang-diagnostic-fixture\") (Thread/startVirtualThread (fn [] (.setName (Thread/currentThread) \"hang-virtual-fixture\") @(promise))) (println :ready) (flush) @(promise))"])
               (.redirectErrorStream true)
               (.redirectOutput (ProcessBuilder$Redirect/to output))))]

          (try
            (expect (await-value #(when (.isFile output) (str/includes? (slurp output) ":ready"))))
            (let [target (#'diagnostics/local-jvm-target {:pid (.pid process)})]
              (expect (some? target) "A JDK with jcmd is required")
              (when target
                (let [result (#'diagnostics/attach-snapshot! target)]
                  (expect (= :written (:status result)))
                  (when-let [path (:path result)]
                    (let [report (wire/parse-json (slurp path))
                          stacks (slurp (io/file (.getParentFile (io/file path)) "threads.json"))]

                      (expect (= "report.json" (.getName (io/file path))))
                      (expect (= "written" (get report "status")))
                      (expect (= "platform-and-virtual-threads" (get report "coverage")))
                      (expect (= "gateway-health-probe-failed" (get report "reason")))
                      (expect (= (.pid process) (get report "pid")))
                      (expect (string? (get report "process_started")))
                      (expect (map? (wire/parse-json stacks)))
                      (expect (str/includes? stacks "hang-diagnostic-fixture"))
                      (expect (str/includes? stacks "hang-virtual-fixture")))))))
            (expect (.isAlive process))
            (finally (.destroyForcibly process) (.waitFor process 5 TimeUnit/SECONDS))))))))
