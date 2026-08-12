(ns com.blockether.vis.internal.python-process-handler-test
  "Containment for processes an extension spawns. Every case here is a shape
   really produced by GraalPy (recorded off a live context): uncaptured,
   `capture_output`, `stderr=STDOUT`, `os.popen`. The point of the namespace is
   that a stream nobody reads goes to the log instead of the JVM's fd 1/2 —
   which under a foreground gateway is the operator's terminal — WITHOUT ever
   deadlocking on an undrained pipe."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.python-process-handler :as process-handler]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]
           [org.graalvm.polyglot Context]
           [org.graalvm.polyglot.io ProcessHandler ProcessHandler$ProcessCommand
            ProcessHandler$Redirect]))

;; =============================================================================
;; Harness
;; =============================================================================

(defn- recorder
  "An `emit` plus the atom it appends `[stream line]` pairs to."
  []
  (let [seen (atom [])]
    [seen
     (fn [stream line]
       (swap! seen conj [stream line]))]))

(defn- wait-until
  "Poll `pred` for at most `ms`. Drain threads are asynchronous by design, so
   every assertion about a drained stream waits instead of sleeping once."
  [^long ms pred]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []

      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 20) (recur))))))

(defn- start!
  "Start one process through the contained handler, exactly as a guest would."
  ^Process [emit {:keys [command directory environment merged? in out err]}]
  (.start ^ProcessHandler (process-handler/contained-handler emit)
          (ProcessHandler$ProcessCommand/create (vec command)
                                                directory
                                                (or environment (into {} (System/getenv)))
                                                (boolean merged?)
                                                (or in ProcessHandler$Redirect/INHERIT)
                                                (or out ProcessHandler$Redirect/INHERIT)
                                                (or err ProcessHandler$Redirect/INHERIT))))

(defn- slurp-stream
  ^String [stream]
  (let [buffer (ByteArrayOutputStream.)]
    (io/copy stream buffer)
    (String. (.toByteArray buffer) StandardCharsets/UTF_8)))

(defn- lines-of
  [seen stream]
  (->> @seen
       (filter (fn [[s _]]
                 (= stream s)))
       (mapv second)))

;; =============================================================================
;; A stream the guest does not read is drained, never inherited
;; =============================================================================

(defdescribe
  uncaptured-streams-test
  (it "drains both uncaptured streams into the sink instead of a descriptor"
      (let
        [[seen emit]
         (recorder)

         process
         (start! emit {:command ["/bin/sh" "-c" "echo OUT; echo ERR >&2"]})]

        (expect (zero? (.waitFor process)))
        (expect (wait-until 5000 #(= 2 (count @seen))))
        (expect (= ["OUT"] (lines-of seen "stdout")))
        (expect (= ["ERR"] (lines-of seen "stderr")))))
  (it "hands the guest an empty stream for anything a drain thread owns"
      (let
        [[_ emit]
         (recorder)

         process
         (start! emit {:command ["/bin/sh" "-c" "echo HIDDEN"]})]

        (expect (zero? (.waitFor process)))
        (expect (= "" (slurp-stream (.getInputStream process))))
        (expect (= "" (slurp-stream (.getErrorStream process))))))
  (it "drains a stream the guest redirected to its own sink"
      (let
        [[seen emit]
         (recorder)

         sink
         (ByteArrayOutputStream.)

         process
         (start! emit
                 {:command ["/bin/sh" "-c" "echo TO-SINK"]
                  :out (ProcessHandler$Redirect/createRedirectToStream sink)})]

        (expect (zero? (.waitFor process)))
        (expect (wait-until 5000 #(str/includes? (str sink) "TO-SINK")))
        (expect (= [] (lines-of seen "stdout")))))
  (it
    "never blocks on a flood too large for the pipe buffer"
    ;; The one way containment can actually break: an undrained pipe
    ;; fills the OS buffer (about 64 KiB) and the child hangs forever.
    (let
      [[seen emit]
       (recorder)

       process
       (start!
         emit
         {:command
          ["/bin/sh" "-c"
           "i=0; while [ $i -lt 20000 ]; do echo LINE-OF-FIFTY-CHARACTERS-0123456789-0123456789; i=$((i+1)); done"]})]

      (expect (.waitFor process 60 TimeUnit/SECONDS))
      (expect (zero? (.exitValue process)))
      (expect (wait-until 60000 #(= 20000 (count (lines-of seen "stdout"))))))))

;; =============================================================================
;; A stream the guest DOES read is passed through untouched
;; =============================================================================

(defdescribe captured-streams-test
             (it "passes a piped stream to the guest and logs nothing"
                 (let
                   [[seen emit]
                    (recorder)

                    process
                    (start! emit
                            {:command ["/bin/sh" "-c" "echo CAPTURED; echo CAPTURED-ERR >&2"]
                             :out ProcessHandler$Redirect/PIPE
                             :err ProcessHandler$Redirect/PIPE})]

                   (expect (= "CAPTURED\n" (slurp-stream (.getInputStream process))))
                   (expect (= "CAPTURED-ERR\n" (slurp-stream (.getErrorStream process))))
                   (expect (zero? (.waitFor process)))
                   (expect (= [] @seen))))
             (it "honours a merge the guest asked for instead of splitting it"
                 ;; `stderr=STDOUT`: both streams arrive on stdout, and the guest
                 ;; must not find stderr separately readable.
                 (let
                   [[seen emit]
                    (recorder)

                    process
                    (start! emit
                            {:command ["/bin/sh" "-c" "echo M-OUT; echo M-ERR >&2"]
                             :merged? true
                             :out ProcessHandler$Redirect/PIPE})

                    merged
                    (slurp-stream (.getInputStream process))]

                   (expect (zero? (.waitFor process)))
                   (expect (str/includes? merged "M-OUT"))
                   (expect (str/includes? merged "M-ERR"))
                   (expect (= "" (slurp-stream (.getErrorStream process))))
                   (expect (= [] @seen))))
             (it "drains a merged stream the guest left uncaptured"
                 (let
                   [[seen emit]
                    (recorder)

                    process
                    (start! emit
                            {:command ["/bin/sh" "-c" "echo MERGED-LEAK; echo MERGED-LEAK-ERR >&2"]
                             :merged? true})]

                   (expect (zero? (.waitFor process)))
                   (expect (wait-until 5000 #(= 2 (count (lines-of seen "stdout")))))
                   (expect (= #{"MERGED-LEAK" "MERGED-LEAK-ERR"} (set (lines-of seen "stdout"))))))
             (it "leaves stdin to the guest, so an interactive child keeps its prompt"
                 ;; Rewriting stdin would hang `sudo`, `ssh` and credential prompts
                 ;; on a prompt nobody can see; the input redirect is passed through.
                 (let
                   [[_ emit]
                    (recorder)

                    process
                    (start! emit
                            {:command ["/bin/sh" "-c" "cat"]
                             :in ProcessHandler$Redirect/PIPE
                             :out ProcessHandler$Redirect/PIPE})]

                   (with-open [stdin (.getOutputStream process)]
                     (.write stdin (.getBytes "STDIN-OK" StandardCharsets/UTF_8)))
                   (expect (= "STDIN-OK" (slurp-stream (.getInputStream process))))
                   (expect (zero? (.waitFor process))))))

;; =============================================================================
;; Everything else about the process is the guest's, unchanged
;; =============================================================================

(defdescribe
  process-contract-test
  (it "preserves a non-zero exit status"
      (let [[_ emit] (recorder)]
        (expect (= 7 (.waitFor (start! emit {:command ["/bin/sh" "-c" "exit 7"]}))))))
  (it "passes the environment the guest asked for"
      (let
        [[_ emit]
         (recorder)

         process
         (start! emit
                 {:command ["/bin/sh" "-c" "echo $VIS_HANDLER_TEST_VALUE"]
                  :environment {"VIS_HANDLER_TEST_VALUE" "from-guest"}
                  :out ProcessHandler$Redirect/PIPE})]

        (expect (= "from-guest\n" (slurp-stream (.getInputStream process))))))
  (it "passes the working directory the guest asked for"
      (let
        [[_ emit]
         (recorder)

         dir
         (str (Files/createTempDirectory "vis-handler-test" (make-array FileAttribute 0)))

         process
         (start!
           emit
           {:command ["/bin/sh" "-c" "pwd"] :directory dir :out ProcessHandler$Redirect/PIPE})]

        (expect (str/includes? (slurp-stream (.getInputStream process)) "vis-handler-test"))))
  (it "still terminates a child on request"
      (let
        [[_ emit]
         (recorder)

         process
         (start! emit {:command ["/bin/sh" "-c" "sleep 30"]})]

        (expect (.isAlive process))
        (.destroyForcibly process)
        (expect (.waitFor process 20 TimeUnit/SECONDS))
        (expect (not (.isAlive process))))))

;; =============================================================================
;; The extension context is wired to it — the hygiene gate
;; =============================================================================

(defdescribe
  extension-context-containment-test
  (it
    "sends guest output and an uncaptured child's output to the extension's log sink"
    ;; Regression guard for the leak this namespace exists for: a
    ;; context left at Truffle defaults hands the child the JVM's own
    ;; fd 1/2, so extension output appeared on the operator's terminal
    ;; and in no log at all.
    (let [[seen emit] (recorder)]
      (with-redefs
        [process-handler/log-emit (fn [label]
                                    (fn [stream line]
                                      (emit stream (str label "|" line))))]
        (let [^Context ctx (pyx/build-context "containment-test.py")]
          (try
            (.eval ctx "python" "import sys\nprint('GUEST-PRINT')\nsys.stdout.flush()")
            (.eval
              ctx
              "python"
              (str
                "import subprocess\n"
                "uncaptured = subprocess.run(['/bin/sh','-c','echo CHILD-OUT; echo CHILD-ERR >&2']).returncode\n"
                "captured = subprocess.run(['/bin/sh','-c','echo CAPTURED-BY-GUEST'], capture_output=True).stdout.decode()"))
            (let [bindings (.getBindings ctx "python")]
              (expect (zero? (.asInt (.getMember bindings "uncaptured"))))
              (expect (= "CAPTURED-BY-GUEST\n" (.asString (.getMember bindings "captured")))))
            (expect (wait-until 10000 #(= 3 (count @seen))))
            ;; GraalPy asks for `isRedirectErrorStream` on a fully
            ;; uncaptured run, so the child's stderr arrives merged
            ;; onto stdout — honoured rather than split back apart.
            (expect
              (= ["containment-test.py|GUEST-PRINT" "containment-test.py|CHILD-OUT"
                  "containment-test.py|CHILD-ERR"]
                 (lines-of seen "stdout"))
              "guest print and both child streams are attributed, not written to a descriptor")
            (expect (= [] (lines-of seen "stderr")))
            (finally (.close ctx true))))))))
