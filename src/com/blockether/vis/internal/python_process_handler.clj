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

   The `Process` handed to the guest answers `nullInputStream()` for the
   streams a drain thread owns, so nothing downstream races the drainer for
   bytes.

   The cost is inherent, not a defect: an uncaptured stream is now a pipe, so
   `isatty` is false for it — progress bars render plain and a genuinely
   interactive child cannot work. Capturing output and staying a terminal are
   the same choice made two ways."
  (:require [clojure.java.io :as io]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayOutputStream InputStream OutputStream]
           [java.lang ProcessBuilder$Redirect]
           [java.nio.charset StandardCharsets]
           [java.util List Map]
           [java.util.concurrent TimeUnit]
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

(defn- guest-facing-process
  "Wrap `p` so the streams a drain thread owns read as empty. Every other
   method delegates, including `destroyForcibly` and `onExit`, which answer the
   wrapper rather than leaking the raw process (and with it the drained
   streams) back to the guest."
  ^Process [^Process p hide-out? hide-err?]
  (proxy [Process] []
    (getOutputStream [] (.getOutputStream p))
    (getInputStream [] (if hide-out? (InputStream/nullInputStream) (.getInputStream p)))
    (getErrorStream [] (if hide-err? (InputStream/nullInputStream) (.getErrorStream p)))
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

(defn- start-contained
  "Start `command`'s process with every stream the guest does not read piped
   and drained. Input redirect, working directory, environment and
   `isRedirectErrorStream` are passed through exactly as the guest asked."
  ^Process [emit ^ProcessHandler$ProcessCommand command]
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
    (let [process (.start builder)]
      (when-not guest-reads-out?
        (drain! (.getInputStream process)
                (hidden-stream-sink out-redirect emit "stdout")
                "vis-extension-process-stdout"))
      (when-not (or merged? guest-reads-err?)
        (drain! (.getErrorStream process)
                (hidden-stream-sink err-redirect emit "stderr")
                "vis-extension-process-stderr"))
      (guest-facing-process process (not guest-reads-out?) (or merged? (not guest-reads-err?))))))

(defn contained-handler
  "The `ProcessHandler` every extension context is built with. `emit` is
   `(fn [stream-name line])` and receives each line of every stream the guest
   left uncaptured; `log-emit` builds the production one."
  ^ProcessHandler [emit]
  (reify
    ProcessHandler
      (start [_ command] (start-contained emit command))))
