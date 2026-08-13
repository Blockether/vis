(ns com.blockether.vis.internal.python-process-handler-test
  "Containment for processes an extension spawns. Every case here is a shape
   really produced by GraalPy (recorded off a live context): uncaptured,
   `capture_output`, `stderr=STDOUT`, `os.popen`. The point of the namespace is
   that a stream nobody reads goes to the log instead of the JVM's fd 1/2 —
   which under a foreground gateway is the operator's terminal — WITHOUT ever
   deadlocking on an undrained pipe.

   The last two namespaces cover the GUEST half
   (`vis-python/process_redirect.py`): GraalPy discards a `stdout=`/`stderr=`/
   `stdin=` file or descriptor before the handler can see it, so the file an
   extension named stayed empty and a `stdin=` file hung the child on the
   JVM's own stdin.

   The last group covers the pid the guest ends up holding: emulated posix
   answers `Popen.pid` with a per-context child-slot index it recycles after a
   reap, so the handle named no OS process and later named a stranger."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.python-process-handler :as process-handler]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream]
           [java.lang ProcessHandle]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]
           [org.graalvm.polyglot Context Value]
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

(defn- start-on!
  "Start one process through `handler`, exactly as a guest would."
  ^Process [^ProcessHandler handler {:keys [command directory environment merged? in out err]}]
  (.start handler
          (ProcessHandler$ProcessCommand/create (vec command)
                                                directory
                                                (or environment (into {} (System/getenv)))
                                                (boolean merged?)
                                                (or in ProcessHandler$Redirect/INHERIT)
                                                (or out ProcessHandler$Redirect/INHERIT)
                                                (or err ProcessHandler$Redirect/INHERIT))))

(defn- start!
  "Start one process through a contained handler of its own."
  ^Process [emit opts]
  (start-on! (process-handler/contained-handler emit (process-handler/pid-handoff)) opts))

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
;; A pipe the guest asked for is drained too — the CPython deadlock
;; =============================================================================

(defdescribe
  guest-backlog-test
  (it
    "never blocks a child whose output the guest asked for and never reads"
    ;; `Popen(stdout=PIPE)` then `wait()`: the child fills the OS pipe
    ;; buffer and waits for a reader, the guest waits for an exit that
    ;; therefore never comes. Handing the guest a backlog instead of the
    ;; raw pipe means the pipe always has a reader.
    (let
      [[_ emit]
       (recorder)

       process
       (start!
         emit
         {:command
          ["/bin/sh" "-c"
           "i=0; while [ $i -lt 20000 ]; do echo LINE-OF-FIFTY-CHARACTERS-0123456789-0123456789; i=$((i+1)); done"]
          :out ProcessHandler$Redirect/PIPE})]

      (expect (.waitFor process 60 TimeUnit/SECONDS)
              "the child exits without the guest reading a byte")
      (expect (zero? (.exitValue process)))))
  (it
    "hands the guest every byte of a flood larger than the backlog"
    ;; Past its capacity the backlog throttles the child, and throttling
    ;; must never become dropping.
    (let
      [[_ emit]
       (recorder)

       process
       (start!
         emit
         {:command
          ["/bin/sh" "-c"
           "i=0; while [ $i -lt 200000 ]; do echo LINE-OF-FIFTY-CHARACTERS-0123456789-0123456789; i=$((i+1)); done"]
          :out ProcessHandler$Redirect/PIPE})

       text
       (slurp-stream (.getInputStream process))]

      (expect (zero? (.waitFor process)))
      (expect (= 200000 (count (str/split-lines text))))
      (expect (= #{"LINE-OF-FIFTY-CHARACTERS-0123456789-0123456789"}
                 (set (str/split-lines text))))))
  (it "gives the guest a line while the child is still running"
      ;; A chunk is handed over as it arrives, so a guest streaming a
      ;; long-running child's output does not wait for it to exit.
      (let
        [[_ emit]
         (recorder)

         process
         (start! emit
                 {:command ["/bin/sh" "-c" "echo FIRST-LINE; sleep 20; echo LAST-LINE"]
                  :out ProcessHandler$Redirect/PIPE})

         reader
         (io/reader (.getInputStream process))]

        (expect (= "FIRST-LINE" (.readLine ^java.io.BufferedReader reader)))
        (expect (.isAlive process) "the line arrived before the child exited")
        (.destroyForcibly process)
        (expect (.waitFor process 20 TimeUnit/SECONDS))))
  (it "breaks the child's pipe when the guest stops reading early"
      ;; Closing the stream must reach the real pipe, or an endless child
      ;; would keep running against a backlog nobody drains.
      (let
        [[_ emit]
         (recorder)

         process
         (start! emit
                 {:command ["/bin/sh" "-c" "while true; do echo SPAM; done"]
                  :out ProcessHandler$Redirect/PIPE})

         stream
         (.getInputStream process)]

        (expect (pos? (.read stream (byte-array 16))))
        (.close stream)
        (expect (.waitFor process 30 TimeUnit/SECONDS) "the child died on a broken pipe"))))

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


;; =============================================================================
;; The guest half — a redirect GraalPy discards
;; =============================================================================

(defn- guest-value
  "Eval `code` in a real extension context; answer its `__vis_res__` string.
   `@TMP@` in `code` is replaced by a fresh temp directory."
  [label code]
  (let
    [dir
     (str (Files/createTempDirectory "vis-redirect" (into-array FileAttribute [])))

     ^Context ctx
     (pyx/build-context label)]

    (try (.eval ctx "python" ^String (str/replace code "@TMP@" dir))
         (.asString (.getMember (.getBindings ctx "python") "__vis_res__"))
         (finally (.close ctx true)))))

(defdescribe
  guest-redirect-repair-test
  ;; Regression guard: GraalPy hands the host a plain INHERIT for every file
  ;; or descriptor redirect, so an extension that redirected a CLI's output to
  ;; a file got an EMPTY file and the bytes went to the JVM's fd 1 — the
  ;; operator's terminal under a foreground gateway.
  (it "writes a file-object redirect to the file the extension named"
      (expect (= "'FILE-OBJ-OUT\\n'"
                 (guest-value
                   "redirect-file.py"
                   (str "import os, subprocess\n"
                        "p = os.path.join('@TMP@', 'out.txt')\n" "with open(p, 'w') as f:\n"
                        "    subprocess.run(['/bin/sh', '-c', 'echo FILE-OBJ-OUT'], stdout=f)\n"
                        "__vis_res__ = repr(open(p).read())\n")))))
  (it "writes a raw descriptor redirect to that descriptor"
      (expect (= "'RAW-FD-OUT\\n'"
                 (guest-value "redirect-fd.py"
                              (str
                                "import os, subprocess\n" "p = os.path.join('@TMP@', 'out.txt')\n"
                                "fd = os.open(p, os.O_WRONLY | os.O_CREAT | os.O_TRUNC)\n"
                                "subprocess.run(['/bin/sh', '-c', 'echo RAW-FD-OUT'], stdout=fd)\n"
                                "os.close(fd)\n" "__vis_res__ = repr(open(p).read())\n")))))
  (it "appends where the extension opened for append"
      (expect (= "'PRE\\nAPPENDED\\n'"
                 (guest-value "redirect-append.py"
                              (str
                                "import os, subprocess\n"
                                "p = os.path.join('@TMP@', 'out.txt')\n" "with open(p, 'w') as f:\n"
                                "    f.write('PRE\\n')\n" "with open(p, 'a') as f:\n"
                                "    subprocess.run(['/bin/sh', '-c', 'echo APPENDED'], stdout=f)\n"
                                "__vis_res__ = repr(open(p).read())\n")))))
  (it "sends a stderr redirect to the file the extension named"
      (expect (= "'FILE-ERR\\n'"
                 (guest-value
                   "redirect-stderr.py"
                   (str "import os, subprocess\n"
                        "p = os.path.join('@TMP@', 'err.txt')\n" "with open(p, 'w') as f:\n"
                        "    subprocess.run(['/bin/sh', '-c', 'echo FILE-ERR 1>&2'], stderr=f)\n"
                        "__vis_res__ = repr(open(p).read())\n")))))
  ;; Regression guard: a discarded `stdin=` file is worse than a lost
  ;; redirect — the child inherited the JVM's own stdin and blocked forever
  ;; on input the extension had already supplied.
  (it "feeds a stdin file to the child instead of leaving it on the terminal"
      (expect (= "b'STDIN-FROM-FILE'"
                 (guest-value "redirect-stdin.py"
                              (str
                                "import os, subprocess\n"
                                "p = os.path.join('@TMP@', 'in.txt')\n" "with open(p, 'w') as f:\n"
                                "    f.write('STDIN-FROM-FILE')\n"
                                "done = subprocess.run(['/bin/sh', '-c', 'cat'], stdin=open(p),\n"
                                "                      stdout=subprocess.PIPE, timeout=30)\n"
                                "__vis_res__ = repr(done.stdout)\n")))))
  (it "never blocks on a flood too large for the pipe buffer"
      ;; The pump is what keeps this from deadlocking: 1 MB through a pipe
      ;; whose buffer is about 64 KiB, with the file complete before `run`
      ;; returns.
      (expect (= "(0, 20000, 1000000)"
                 (guest-value
                   "redirect-flood.py"
                   (str "import os, subprocess\n" "p = os.path.join('@TMP@', 'big.txt')\n"
                        "with open(p, 'w') as f:\n" "    done = subprocess.run(\n"
                        "        ['/bin/sh', '-c',\n" "         'for i in $(seq 1 20000); do echo "
                        "0123456789012345678901234567890123456789012345678; done'],\n"
                        "        stdout=f)\n"
                        "lines = sum(1 for _ in open(p))\n"
                        "__vis_res__ = repr((done.returncode, lines, os.path.getsize(p)))\n")))))
  (it "leaves capture_output and DEVNULL exactly as subprocess handles them"
      (expect
        (= "(b'CAP\\n', b'', 0)"
           (guest-value
             "redirect-untouched.py"
             (str "import subprocess\n"
                  "captured = subprocess.run(['/bin/sh', '-c', 'echo CAP'], capture_output=True)\n"
                  "quiet = subprocess.run(['/bin/sh', '-c', 'echo QUIET'],\n"
                  "                       stdout=subprocess.DEVNULL)\n"
                  "__vis_res__ = repr((captured.stdout, captured.stderr, quiet.returncode))\n")))))
  (it "refuses a sink that has no descriptor instead of losing its bytes"
      ;; CPython raises the same `UnsupportedOperation`; the alternative here
      ;; was silence plus output on a descriptor nobody named.
      (expect (= "'UnsupportedOperation'"
                 (guest-value
                   "redirect-no-fileno.py"
                   (str "import io, subprocess\n" "try:\n"
                        "    subprocess.run(['/bin/sh', '-c', 'echo X'], stdout=io.BytesIO())\n"
                        "    __vis_res__ = 'NO-ERROR'\n"
                        "except Exception as e:\n"
                        "    __vis_res__ = repr(type(e).__name__)\n"))))))

(defdescribe
  standard-stream-redirect-test
  (it "sends a redirect to sys.stdout into the extension's log, not descriptor 1"
      ;; Descriptor 1 belongs to the JVM, not to this context: writing the
      ;; child's bytes there would put them back on the operator's terminal,
      ;; which is the leak the whole namespace exists to close.
      (let [[seen emit] (recorder)]
        (with-redefs
          [process-handler/log-emit (fn [label]
                                      (fn [stream line]
                                        (emit stream (str label "|" line))))]
          (let [^Context ctx (pyx/build-context "redirect-sys-stdout.py")]
            (try (.eval
                   ctx
                   "python"
                   (str
                     "import subprocess, sys\n"
                     "subprocess.run(['/bin/sh', '-c', 'echo TO-SYS-STDOUT'], stdout=sys.stdout)\n"
                     "sys.stdout.flush()\n"))
                 (expect (wait-until 10000
                                     #(some (fn [line]
                                              (str/includes? line "TO-SYS-STDOUT"))
                                            (lines-of seen "stdout"))))
                 (finally (.close ctx true))))))))


;; =============================================================================
;; The pid on the handle
;; =============================================================================

(defdescribe pid-handoff-test
             ;; Regression, issue #142: the real pid never left the host handler, so the
             ;; guest was left holding GraalPy's per-context child-slot index.
             (it "hands the started child's OS pid over exactly once"
                 (let
                   [[_ emit]
                    (recorder)

                    handoff
                    (process-handler/pid-handoff)

                    ^Process started
                    (start-on! (process-handler/contained-handler emit handoff)
                               {:command ["/bin/sh" "-c" "exit 0"]})]

                   (expect (= (.pid started) (process-handler/claim-pid! handoff))
                           "the pid of the process this handler really started")
                   (expect (nil? (process-handler/claim-pid! handoff))
                           "an emptied slot answers nothing rather than the previous child")
                   (.waitFor started)))
             ;; Regression, issue #142: the handoff was one slot for the whole context,
             ;; and `Popen` is not a context's only spawn - `os.system` reaches the
             ;; handler without constructing one. A spawn on another thread overwrote
             ;; the pid a constructor was about to claim, so the handle adopted a
             ;; stranger's - by then exited - pid.
             (it
               "confines the slot to the thread that started the child"
               (let
                 [[_ emit]
                  (recorder)

                  handoff
                  (process-handler/pid-handoff)

                  handler
                  (process-handler/contained-handler emit handoff)

                  started-elsewhere
                  (promise)

                  claimed-elsewhere
                  (promise)

                  release
                  (promise)

                  ^Thread other
                  (Thread. ^Runnable
                           (fn []
                             (let
                               [^Process p (start-on! handler {:command ["/bin/sh" "-c" "exit 0"]})]
                               (deliver started-elsewhere (.pid p))
                               (deref release 5000 :timeout)
                               (deliver claimed-elsewhere (process-handler/claim-pid! handoff))
                               (.waitFor p))))]

                 (.start other)
                 (expect (number? (deref started-elsewhere 5000 nil))
                         "the other thread started its child first")
                 (let [^Process mine (start-on! handler {:command ["/bin/sh" "-c" "exit 0"]})]
                   (expect (= (.pid mine) (process-handler/claim-pid! handoff))
                           "this thread claims the child it started, not the other thread's")
                   (.waitFor mine))
                 (deliver release :go)
                 (expect (= @started-elsewhere (deref claimed-elsewhere 5000 nil))
                         "and the other thread's own pid survived this thread's claim")
                 (.join other 5000))))

(def ^:private reap-guest-children
  "Terminate every child still running in the context. A `Context.close(true)`
   throws while a sub-process is alive, and that exception would otherwise
   replace the assertion failure a test is reporting."
  (str "import subprocess\n" "__values = list(globals().values())\n"
       ;; A test that spawns many children keeps them in a list, so look one deep.
       "for __group in [__v for __v in __values if isinstance(__v, list)]:\n"
       "    __values.extend(__group)\n"
       "for __handle in __values:\n"
       "    if isinstance(__handle, subprocess.Popen) and __handle.poll() is None:\n"
       "        __handle.terminate()\n" "        __handle.wait()\n"))

(defn- in-guest
  "Eval `code` in a real extension context and hand `check` the context, so an
   assertion can run while the child is still alive."
  [label ^String code check]
  (let [^Context ctx (pyx/build-context label)]
    (try (.eval ctx "python" code)
         (check ctx)
         (finally (try (.eval ctx "python" reap-guest-children) (catch Exception _ nil))
                  (.close ctx true)))))

(defn- member [^Context ctx name] (.getMember (.getBindings ctx "python") ^String name))

(defdescribe
  guest-real-pid-test
  ;; Regression, issue #142: `Popen.pid` was GraalPy's per-context child-slot
  ;; index — `1` for the first child of every context, and 1 is init — so the
  ;; handle named no OS process, nothing could `ps`, `lsof` or supervise the
  ;; child, and a pid kept past `wait()` named whichever child later took the
  ;; recycled slot.
  (it "puts the child's real OS pid on the handle and keeps the slot index"
      (in-guest "real-pid.py"
                (str "import subprocess\n" "child = subprocess.Popen(['/bin/sleep', '30'])\n"
                     "pid = child.pid\n" "slot = getattr(child, '__vis_virtual_pid__', pid)\n")
                (fn [ctx]
                  (let
                    [pid
                     (.asLong (member ctx "pid"))

                     slot
                     (.asLong (member ctx "slot"))

                     found
                     (ProcessHandle/of pid)]

                    (expect (= 1 slot) "the first child of a context still lands in the first slot")
                    (expect (not= slot pid) "the handle carries a pid, not the slot index")
                    (expect (.isPresent found) "the pid names a process the host can see")
                    (expect (str/includes?
                              (str (.orElse (.command (.info ^ProcessHandle (.get found))) ""))
                              "sleep")
                            "and it is the child the extension started")))))
  (it "still polls, terminates and waits through the slot index"
      (in-guest "real-pid-signals.py"
                (str "import subprocess\n" "child = subprocess.Popen(['/bin/sleep', '30'])\n"
                     "alive = child.poll() is None\n" "child.terminate()\n"
                     "code = child.wait()\n" "reaped = child.poll()\n")
                (fn [ctx]
                  (expect (.asBoolean (member ctx "alive")))
                  (expect (= -15 (.asInt (member ctx "code"))) "SIGTERM reached the child")
                  (expect (= -15 (.asInt (member ctx "reaped")))))))
  (it "signals a live child by its OS pid and never signals a recycled slot"
      (in-guest "real-pid-recycled.py"
                (str "import os, signal, subprocess\n"
                     "first = subprocess.Popen(['/bin/sleep', '30'])\n"
                     "os.kill(first.pid, signal.SIGTERM)\n" "first_code = first.wait()\n"
                     ;; Reaping `first` frees the slot it held, so this child takes it.
                     "second = subprocess.Popen(['/bin/sleep', '31'])\n"
                     "distinct = first.pid != second.pid\n"
                     "try:\n" "    os.kill(first.pid, signal.SIGTERM)\n"
                     "    stale = 'accepted'\n" "except Exception as error:\n"
                     "    stale = type(error).__name__\n" "second_alive = second.poll() is None\n")
                (fn [ctx]
                  (expect (= -15 (.asInt (member ctx "first_code")))
                          "os.kill with the real pid reached the child")
                  (expect (.asBoolean (member ctx "distinct")) "two children never share a pid")
                  (expect (.asBoolean (member ctx "second_alive"))
                          (str "the reaped child's pid signalled nobody, os.kill answered "
                               (.asString (member ctx "stale")))))))
  (it "refuses a pid whose slot a wildcard wait handed to another child"
      ;; `os.waitpid(-1, ...)` reaps a child of this context and answers -1, so the
      ;; layer that swaps pids never learns which handle died; the freed slot goes
      ;; to the next child, and the first child's pid used to signal THAT one.
      (in-guest "real-pid-wildcard.py"
                (str "import os, signal, subprocess, time\n"
                     "first = subprocess.Popen(['/bin/sleep', '0.2'])\n" "time.sleep(0.8)\n"
                     "wildcard = os.waitpid(-1, 0)\n"
                     "second = subprocess.Popen(['/bin/sleep', '32'])\n"
                     "recycled = getattr(second, '__vis_virtual_pid__', 0) == 1\n" "try:\n"
                     "    os.kill(first.pid, signal.SIGTERM)\n" "    stale = 'accepted'\n"
                     "except Exception as error:\n" "    stale = type(error).__name__\n"
                     "time.sleep(0.4)\n" "second_alive = second.poll() is None\n")
                (fn [ctx]
                  (expect (.asBoolean (member ctx "recycled"))
                          "the wildcard wait freed the first child's slot for the second")
                  (expect (= "ProcessLookupError" (.asString (member ctx "stale")))
                          "a pid whose slot moved on names nobody")
                  (expect (.asBoolean (member ctx "second_alive"))
                          "and the child that took the slot was left alone")))))

(defdescribe
  guest-pid-under-concurrent-spawn-test
  ;; Regression, issue #142: the host handed the pid over through ONE slot per
  ;; context, and `os.system` reaches that handler without constructing a
  ;; `Popen`. Called from another thread it overwrote the slot between a
  ;; constructor starting its child and claiming it, so roughly a fifth of the
  ;; handles ended up carrying the pid of an `os.system` child that had already
  ;; exited - a real pid, naming the wrong process, or none at all.
  (it
    "gives every child its own pid while other threads spawn"
    (in-guest
      "real-pid-concurrent.py"
      (str "import os, subprocess, threading\n"
           "stop = False\n"
           ;; `os.system` starts its child through the same host handler,
           ;; with no `Popen` constructor to claim the pid it leaves.
           "def hammer():\n"
           "    while not stop:\n" "        os.system('/bin/sleep 0.001')\n"
           "hammers = [threading.Thread(target=hammer) for _ in range(3)]\n" "for __t in hammers:\n"
           "    __t.start()\n"
           "children = [subprocess.Popen(['/bin/sleep', '3600.%d' % i]) for i in range(12)]\n"
           "stop = True\n" "for __t in hammers:\n"
           "    __t.join()\n" "pids = [child.pid for child in children]\n")
      (fn [ctx]
        (let [^Value pids (member ctx "pids")]
          (doseq
            [i (range (.getArraySize pids))
             :let [pid (.asLong (.getArrayElement pids i))
                   found (ProcessHandle/of pid)]]

            (expect (.isPresent found) (str "child " i " carries the pid of a process that exists"))
            (expect (str/includes? (str (.orElse (.commandLine (.info ^ProcessHandle (.get found)))
                                                 ""))
                                   (str "3600." i))
                    (str "child " i " carries its OWN pid, not another spawn's"))))))))
