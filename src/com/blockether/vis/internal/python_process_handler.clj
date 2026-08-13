(ns com.blockether.vis.internal.python-process-handler
  "Containment for processes spawned inside an extension's GraalPy context.

   Truffle's default process handler starts a child with whatever the guest
   did not capture left at `Redirect.INHERIT` — the JVM's OWN fd 1/2. Under a
   foreground `vis-agent gateway start` that descriptor is the operator's
   terminal, so an extension shelling out to a CLI sprays its output (and any
   secret in it) onto that terminal and into no log at all. The JVM stream
   swap in `internal.config/init-cli!` cannot reach it: `System/setOut` and
   `System/setErr` replace PrintStreams, while the child writes to the file
   DESCRIPTOR. This handler is therefore the only containment point, and every
   extension context is built with it (`python-extensions/build-context`).

   Nothing here is a policy guess — the guest's intent arrives as data on the
   `ProcessCommand`:

   | incoming redirect | what it means                | what happens here                     |
   | ----------------- | ---------------------------- | ------------------------------------- |
   | `PIPE`            | the guest reads the stream   | passed through untouched              |
   | a stream redirect | the guest named a sink       | piped, drained into that sink         |
   | `INHERIT`         | nobody reads it — THE LEAK   | piped, drained into the log by `emit` |

   Three invariants, each one a way this breaks when dropped:

   - **Every pipe this creates is drained** by a daemon thread. An undrained
     pipe fills the OS buffer (about 64 KiB) and the child blocks forever;
     naive `PIPE` without a drainer is worse than the leak it replaces.
   - **stdin is never rewritten.** `getInputRedirect` is a separate value, so
     `sudo`, `ssh`, `gh auth login` and git credential prompts keep the
     terminal they need instead of hanging on an invisible prompt.
   - **`isRedirectErrorStream` is honoured**, or a stream the guest
     deliberately merged comes back split.
   - **A pipe the guest asked for is drained too**, into a bounded backlog the
     guest then reads from. `Popen(stdout=PIPE)` followed by `wait()` is the
     classic CPython deadlock — the child fills the OS buffer while the guest
     waits for an exit that cannot come — and inside an extension it wedges the
     agent, not a script. The backlog decouples the two; past its capacity the
     child blocks again, which is exactly the unbuffered behaviour, so a child
     streaming without end can never exhaust the heap.

   The `Process` handed to the guest answers `nullInputStream()` for the
   streams a drain thread owns, so nothing downstream races the drainer for
   bytes.

   This handler is only the HOST half. GraalPy discards a `stdout=`,
   `stderr=` or `stdin=` file or descriptor before a `ProcessCommand` is ever
   built, so that choice cannot be honoured from here; the guest half that
   turns it back into a pumped pipe is `vis-python/process_redirect.py`,
   evaluated into every extension context by `python-extensions/build-context`
   (see `python-extensions/redirect-repair-python`).

   The cost is inherent, not a defect: an uncaptured stream is now a pipe, so
   `isatty` is false for it — progress bars render plain and a genuinely
   interactive child cannot work. Capturing output and staying a terminal are
   the same choice made two ways."
  (:require [clojure.java.io :as io]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayOutputStream InputStream OutputStream]
           [java.lang ProcessBuilder$Redirect]
           [java.nio.charset StandardCharsets]
           [java.util Arrays List Map]
           [java.util.concurrent LinkedBlockingQueue Semaphore TimeUnit]
           [java.util.concurrent.atomic AtomicBoolean]
           [java.util.function Function]
           [org.graalvm.polyglot.io ProcessHandler ProcessHandler$ProcessCommand
            ProcessHandler$Redirect]))

(set! *warn-on-reflection* true)

(defn line-sink-stream
  "An `OutputStream` that calls `(emit line)` once per completed line, plus
   once for a trailing partial line on `flush`/`close`. Line-oriented because
   the destination is a log: a signal per line keeps the extension's name on
   every line instead of on an arbitrary buffer boundary. Newlines are not
   part of `line`; a trailing carriage return is dropped."
  ^OutputStream [emit]
  (let
    [buf
     (ByteArrayOutputStream.)

     emit-buffered!
     (fn []
       (let [bytes (.toByteArray buf)]
         (.reset buf)
         (when (pos? (alength bytes))
           (let [line (String. bytes StandardCharsets/UTF_8)]
             (emit (if (.endsWith line "\r") (subs line 0 (dec (count line))) line))))))

     write-byte!
     (fn [b]
       (if (= 10 b) (emit-buffered!) (.write buf (int b))))]

    (proxy [OutputStream] []
      (write
        ([b]
         (locking buf
           (if (bytes? b)
             (doseq [x ^bytes b]
               (write-byte! (bit-and (int x) 0xff)))
             (write-byte! (bit-and (int b) 0xff)))))
        ([b off len]
         (locking buf
           (dotimes [i (int len)]
             (write-byte! (bit-and (int (aget ^bytes b (+ (int off) i))) 0xff))))))
      (flush [] (locking buf (emit-buffered!)))
      (close [] (locking buf (emit-buffered!))))))

(defn log-emit
  "The default sink: one telemere signal per line, tagged with the extension
   `label` and the stream it came from, so output that used to land unattributed
   on a terminal lands attributed in the log."
  [label]
  (fn [stream line]
    (tel/log! {:level :info
               :id ::extension-process-output
               :data {:extension label :stream stream}
               :msg (str line)})))

(defn- drain!
  "Daemon thread copying `in` into `out` until EOF. `out` is flushed but never
   closed — it may be a sink the guest owns. Closing `in` at EOF releases the
   pipe's descriptor."
  [^InputStream in ^OutputStream out ^String thread-name]
  (doto (Thread. ^Runnable
                 (fn []
                   (try (io/copy in out)
                        (catch Throwable _ nil)
                        (finally (try (.flush out) (catch Throwable _ nil))
                                 (try (.close in) (catch Throwable _ nil)))))
                 thread-name)
    (.setDaemon true)
    (.start)))

(defn- hidden-stream-sink
  "Where a stream the guest is NOT reading goes: the sink the guest named on
   the redirect when there is one, otherwise `emit` under `stream-name`."
  ^OutputStream [^ProcessHandler$Redirect redirect emit stream-name]
  (or (.getOutputStream redirect)
      (line-sink-stream (fn [line]
                          (emit stream-name line)))))

(def ^:private ^:const pump-chunk-bytes
  "Bytes read from a pipe per hand-off. A chunk is handed over as soon as it
   arrives and is never filled first, so a guest reading line by line sees no
   added latency."
  65536)

(def ^:private ^:const guest-backlog-bytes
  "How many bytes may sit unread in one guest stream's backlog before the child
   blocks again — the ceiling this handler can hold on the heap per stream for
   a guest that stopped reading. Counted in BYTES, never in chunks: a pipe
   hands over whatever has arrived, so a chunk count is a ceiling only in the
   best case and lets a child of small writes block far below it."
  (* 8 1024 1024))

(defn- buffered-guest-stream
  "Pump `source` into a bounded backlog on a daemon thread and answer the
   `InputStream` the guest reads instead. The child therefore writes to a
   consumer that is always running, so it reaches exit even when the guest
   never reads a byte, while the guest still receives every byte in order.
   Past `guest-backlog-bytes` unread the pump waits and the child blocks on a
   full pipe again, exactly as it would unbuffered. Closing the answered stream
   abandons the backlog and closes `source`, so a guest that stops reading
   early still breaks the child's pipe."
  ^InputStream [^InputStream source ^String thread-name]
  (let
    [queue
     (LinkedBlockingQueue.)

     budget
     (Semaphore. guest-backlog-bytes)

     abandoned
     (AtomicBoolean. false)

     pump
     (doto (Thread. ^Runnable
                    (fn []
                      (try (let [buffer (byte-array pump-chunk-bytes)]
                             (loop []

                               (let [n (.read source buffer)]
                                 (when (pos? n)
                                   (.acquire budget n)
                                   (when-not (.get abandoned)
                                     (.put queue (Arrays/copyOf buffer n))
                                     (recur))))))
                           (catch Throwable _ nil)
                           (finally (try (.close source) (catch Throwable _ nil)))))
                    thread-name)
       (.setDaemon true)
       (.start))

     lock
     (Object.)

     current
     (volatile! nil)

     next-chunk!
     (fn []
       (loop []

         (let [chunk (.poll queue 50 TimeUnit/MILLISECONDS)]
           (cond (some? chunk) (do (.release budget (alength ^bytes chunk)) chunk)
                 (.get abandoned) nil
                 (.isAlive pump) (recur)
                 ;; The pump exits after its last `put`, so one non-blocking
                 ;; re-poll settles the race between that put and this check.
                 :else (.poll queue)))))

     ready
     (fn []
       (or @current
           (when-let [chunk (next-chunk!)]
             (vreset! current [chunk 0]))))

     take-bytes!
     (fn [^bytes destination ^long offset ^long length]
       (locking lock
         (if-let [[^bytes chunk position] (ready)]
           (let [taken (min length (- (alength chunk) (long position)))]
             (System/arraycopy chunk (int position) destination (int offset) (int taken))
             (vreset! current
                      (when (< (+ (long position) taken) (alength chunk))
                        [chunk (+ (long position) taken)]))
             taken)
           -1)))]

    (proxy [InputStream] []
      (read
        ([]
         (let [one (byte-array 1)]
           (if (neg? (long (take-bytes! one 0 1))) -1 (bit-and (int (aget one 0)) 0xff))))
        ([b] (take-bytes! b 0 (alength ^bytes b)))
        ([b off len] (if (zero? (long len)) 0 (take-bytes! b off len))))
      (available []
        (locking lock
          (if-let [[^bytes chunk position] @current]
            (- (alength chunk) (long position))
            0)))
      (close []
        (.set abandoned true)
        (.clear queue)
        (locking lock (vreset! current nil))
        (try (.close source) (catch Throwable _ nil))
        (.interrupt pump)))))

(defn- guest-facing-process
  "Wrap `p` so the guest reads exactly the streams this handler decided it may:
   `out-stream` and `err-stream` are the empty stream for anything a drain
   thread owns and the backlog for anything the guest asked to read, never the
   raw pipe. Every other method delegates, including `destroyForcibly` and
   `onExit`, which answer the wrapper rather than leaking the raw process (and
   with it those pipes) back to the guest."
  ^Process [^Process p ^InputStream out-stream ^InputStream err-stream]
  (proxy [Process] []
    (getOutputStream [] (.getOutputStream p))
    (getInputStream [] out-stream)
    (getErrorStream [] err-stream)
    (waitFor ([] (.waitFor p)) ([timeout unit] (.waitFor p (long timeout) ^TimeUnit unit)))
    (exitValue [] (.exitValue p))
    (destroy [] (.destroy p))
    (destroyForcibly [] (.destroyForcibly p) this)
    (isAlive [] (.isAlive p))
    (pid [] (.pid p))
    (supportsNormalTermination [] (.supportsNormalTermination p))
    (toHandle [] (.toHandle p))
    (info [] (.info p))
    (children [] (.children p))
    (descendants [] (.descendants p))
    (onExit []
      (let [self this]
        (.thenApply (.onExit p)
                    (reify
                      Function
                        (apply [_ _] self)))))))

(defn pid-handoff
  "A one-slot handoff, CONFINED TO THE STARTING THREAD, carrying the OS pid of
   the child a `contained-handler` most recently started on that thread, from
   this handler to the guest that started it.

   An extension context runs with `allowNativeAccess false`, so GraalPy serves
   `subprocess` from its EMULATED posix and never shows the guest an OS pid:
   `Popen.pid` is the per-context CHILD SLOT INDEX `PosixResources` registered
   the child under (1, 2, 3 ...), and that slot is REUSED once the child is
   reaped. So the number names no process - 1 is init - and a pid held past
   `wait()` names whichever child later took the slot.
   `vis-python/process_redirect.py` claims this slot inside `Popen.__init__`
   and puts the real pid on the handle.

   Per thread, because `Popen` is not a context's only spawn: GraalPy's
   `os.system` reaches this handler WITHOUT constructing a `Popen`, and an
   extension may call it from another thread (`allowCreateThread true`). One
   shared slot let such a spawn overwrite the pid in the window between the
   constructor starting its child and claiming it, so the handle adopted a
   stranger's - by then already exited - pid. GraalPy calls
   `ProcessHandler.start` on the very thread the guest spawned from, so a
   thread-confined slot pairs start with claim exactly, whatever else the
   context spawns meanwhile."
  ^ThreadLocal []
  (ThreadLocal.))

(defn claim-pid!
  "Take the OS pid `handoff` holds FOR THIS THREAD, emptying the slot. `nil`
   when nothing has started on this thread through this handler since the last
   claim - the guest then keeps the slot index it already has rather than being
   handed a guess."
  [^ThreadLocal handoff]
  (let [pid (.get handoff)]
    (.remove handoff)
    pid))

(defn- start-contained
  "Start `command`'s process with every output stream piped and consumed by a
   thread of its own — into the log or the guest's sink when nobody reads it,
   into a bounded backlog when the guest does — so the child never blocks on a
   full pipe. Input redirect, working directory, environment and
   `isRedirectErrorStream` are passed through exactly as the guest asked. The
   child's real OS pid is left in this thread's `handoff` slot for the guest
   half to claim."
  ^Process [emit handoff ^ProcessHandler$ProcessCommand command]
  (let
    [out-redirect
     (.getOutputRedirect command)

     err-redirect
     (.getErrorRedirect command)

     merged?
     (.isRedirectErrorStream command)

     guest-reads-out?
     (= ProcessHandler$Redirect/PIPE out-redirect)

     guest-reads-err?
     (= ProcessHandler$Redirect/PIPE err-redirect)

     builder
     (ProcessBuilder. ^List (.getCommand command))]

    (when-let [dir (.getDirectory command)]
      (.directory builder (io/file dir)))
    (when-let [env ^Map (.getEnvironment command)]
      (doto (.environment builder) (.clear) (.putAll env)))
    (.redirectInput builder
                    (if (= ProcessHandler$Redirect/PIPE (.getInputRedirect command))
                      ProcessBuilder$Redirect/PIPE
                      ProcessBuilder$Redirect/INHERIT))
    (.redirectOutput builder ProcessBuilder$Redirect/PIPE)
    (.redirectError builder ProcessBuilder$Redirect/PIPE)
    (.redirectErrorStream builder merged?)
    (let
      [process
       (.start builder)

       out-stream
       (if guest-reads-out?
         (buffered-guest-stream (.getInputStream process) "vis-extension-process-stdout-backlog")
         (do (drain! (.getInputStream process)
                     (hidden-stream-sink out-redirect emit "stdout")
                     "vis-extension-process-stdout")
             (InputStream/nullInputStream)))

       err-stream
       (cond
         ;; A merged stderr has no stream of its own to read or drain.
         merged? (InputStream/nullInputStream)
         guest-reads-err? (buffered-guest-stream (.getErrorStream process)
                                                 "vis-extension-process-stderr-backlog")
         :else (do (drain! (.getErrorStream process)
                           (hidden-stream-sink err-redirect emit "stderr")
                           "vis-extension-process-stderr")
                   (InputStream/nullInputStream)))]

      ;; The guest claims this the moment its `Popen` constructor returns
      ;; (`vis-python/process_redirect.py`), which is the only place the real
      ;; pid can still be paired with the handle GraalPy is building. This runs
      ;; on the thread the guest spawned from, so the slot is that thread's and
      ;; a spawn on another thread cannot overwrite it.
      (.set ^ThreadLocal handoff (.pid process))
      (guest-facing-process process out-stream err-stream))))

(defn contained-handler
  "The `ProcessHandler` every extension context is built with. `emit` is
   `(fn [stream-name line])` and receives each line of every stream the guest
   left uncaptured; `log-emit` builds the production one. `handoff` is the
   per-thread slot `pid-handoff` makes, through which the guest half learns the
   real OS pid of the child it just started."
  ^ProcessHandler [emit handoff]
  (reify
    ProcessHandler
      (start [_ command] (start-contained emit handoff command))))
