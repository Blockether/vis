(ns com.blockether.vis.internal.foundation.pty
  "Pure-Java pseudo-terminal for background `shell` children — NO JNA, NO extracted native helper,
   NO external `tmux`. Everything is a `java.lang.foreign` (Panama FFM) downcall
   into the platform libc, so it survives GraalVM native-image the same way the
   rest of vis's FFM surface (fff / rift / ruff / tree-sitter) does.

   Why FFM and not pty4j:
   - pty4j drags in JNA *and* ships its own compiled `libpty` that it extracts to
     a temp dir at runtime — two native-image headaches (reflection metadata for
     JNA + a resource-extraction dance for the .dylib/.so).
   - The obvious pure-FFM shortcut — call libc `forkpty` then `execve` in the
     child — does NOT work from a JVM: invoking an FFM downcall MethodHandle after
     `fork()` is not async-signal-safe and SIGBUSes the child. (Verified.)
   - `posix_spawn` sidesteps that entirely: libc does the fork+exec ATOMICALLY in
     native code, so vis only ever issues ONE parent-side downcall and never runs
     any JVM code in the child. Paired with `openpty` (master/slave fds) + a
     `dup2` of the slave onto the child's 0/1/2, the child gets a real TTY:
     `isatty()` is true, `$TERM` is honoured, stdin is writable.

   Public surface — `spawn!` returns a PLAIN MAP (not a `java.lang.Process`; a
   runtime `proxy`/`gen-class` would break native-image), shaped for the
   `internal.foundation.shell` background pump:

     {:pid      <long>              OS pid (a genuine child — `ProcessHandle/of`
                                    works, unlike a pty4j process)
      :in       <java.io.InputStream>   master-fd reader (a real piped stream)
      :send     (fn [^bytes b])     write bytes to the master (the stdin channel)
      :wait     (fn [] <int>)       block until exit, reap, return the exit code
      :alive?   (fn [] <bool>)
      :destroy  (fn [force?])       SIGTERM (false) / SIGKILL (true) the child}"
  (:require [clojure.string :as str])
  (:import (java.io File PipedInputStream PipedOutputStream)
           (java.lang ProcessHandle)
           (java.lang.foreign AddressLayout
                              Arena
                              FunctionDescriptor
                              Linker
                              Linker$Option
                              MemoryLayout
                              MemorySegment
                              SymbolLookup
                              ValueLayout
                              ValueLayout$OfInt
                              ValueLayout$OfShort)))

;; FFM plumbing

(def ^:private ^AddressLayout ADDR ValueLayout/ADDRESS)

(def ^:private ^ValueLayout$OfInt I ValueLayout/JAVA_INT)

(def ^:private ^ValueLayout$OfShort S ValueLayout/JAVA_SHORT)

(def ^:private L ValueLayout/JAVA_LONG)

(def ^:private mac? (str/includes? (str/lower-case (System/getProperty "os.name" "")) "mac"))

(def ^:private ^Linker linker (Linker/nativeLinker))

(def ^:private ^SymbolLookup lookup
  ;; libc symbols (posix_spawn*, read/write/kill/waitpid/close) live in the
  ;; default lookup on every platform. openpty/forkpty live there on macOS
  ;; (libSystem) but in libutil on Linux, so fall back to a `util` library
  ;; lookup when the default misses.
  (let
    [^SymbolLookup std
     (.defaultLookup linker)

     ^SymbolLookup util
     (try (SymbolLookup/libraryLookup "util" (Arena/global)) (catch Throwable _ nil))]

    (reify
      SymbolLookup
        (find [_ name]
          (let [f (.find std name)]
            (if (.isPresent f) f (if util (.find util name) f)))))))

(defn- dh
  "Bind a libc function to a downcall MethodHandle."
  [nm ret arg-layouts]
  (.downcallHandle linker
                   (.orElseThrow (.find lookup nm))
                   (FunctionDescriptor/of ret (into-array MemoryLayout arg-layouts))
                   (make-array Linker$Option 0)))

(def ^:private h-openpty (delay (dh "openpty" I [ADDR ADDR ADDR ADDR ADDR])))

(def ^:private h-fa-init (delay (dh "posix_spawn_file_actions_init" I [ADDR])))

(def ^:private h-fa-dup2 (delay (dh "posix_spawn_file_actions_adddup2" I [ADDR I I])))

(def ^:private h-fa-close (delay (dh "posix_spawn_file_actions_addclose" I [ADDR I])))

(def ^:private h-fa-destr (delay (dh "posix_spawn_file_actions_destroy" I [ADDR])))

(def ^:private h-at-init (delay (dh "posix_spawnattr_init" I [ADDR])))

(def ^:private h-at-flags (delay (dh "posix_spawnattr_setflags" I [ADDR S])))

(def ^:private h-at-destr (delay (dh "posix_spawnattr_destroy" I [ADDR])))

;; posix_spawnP (not posix_spawn): searches $PATH for a bare program name like
;; "bash", exactly as execvp would — plain posix_spawn uses execv (no PATH search)
;; and ENOENTs on anything that isn't an absolute/relative path.
(def ^:private h-spawn (delay (dh "posix_spawnp" I [ADDR ADDR ADDR ADDR ADDR ADDR])))

(def ^:private h-read (delay (dh "read" L [I ADDR L])))

(def ^:private h-write (delay (dh "write" L [I ADDR L])))

(def ^:private h-close (delay (dh "close" I [I])))

(def ^:private h-kill (delay (dh "kill" I [I I])))

(def ^:private h-waitpid (delay (dh "waitpid" I [I ADDR I])))

;; poll(2). The reader loop ASKS whether the terminal has anything left instead of
;; blocking in `read`, because the parent holds a slave descriptor open for the
;; child's lifetime and a blocking read would never see EOF. `nfds_t` is 32-bit on
;; macOS and 64-bit on Linux, so the count crosses as a long: correct on both.
(def ^:private h-poll (delay (dh "poll" I [ADDR L I])))

;; addchdir_np: glibc >= 2.29 and macOS >= 10.15 (best-effort; ignored if absent).
(def ^:private h-fa-chdir
  (delay (try (dh "posix_spawn_file_actions_addchdir_np" I [ADDR ADDR]) (catch Throwable _ nil))))

;; addclosefrom_np: close every descriptor >= n in the child, in ONE action and
;; without enumerating anything (glibc >= 2.34, FreeBSD). Absent on musl and on
;; older glibc, where `close-inherited!` sweeps the kernel's fd listing instead.
(def ^:private h-fa-closefrom
  (delay (try (dh "posix_spawn_file_actions_addclosefrom_np" I [ADDR I]) (catch Throwable _ nil))))

(defn- invoke
  ^Object [h & args]
  (.invokeWithArguments ^java.lang.invoke.MethodHandle h (object-array args)))

;; POSIX_SPAWN_SETSID makes the child a session leader (detached from vis's own
;; controlling terminal) — the value differs by platform.
(def ^:private POSIX_SPAWN_SETSID (short (if mac? 0x0400 0x80)))

;; POSIX_SPAWN_CLOEXEC_DEFAULT (Darwin only, <spawn.h>): the kernel closes EVERY
;; descriptor in the child except the ones the file actions name. See
;; `close-inherited!` for why a pty child must inherit nothing above stdio.
(def ^:private POSIX_SPAWN_CLOEXEC_DEFAULT (short 0x4000))

(def ^:private spawn-flags
  (short (bit-or (int POSIX_SPAWN_SETSID) (int (if mac? POSIX_SPAWN_CLOEXEC_DEFAULT 0)))))

(def ^:private SIGTERM (int 15))

(def ^:private SIGKILL (int 9))

;; spawn!

(defn- build-strv
  "Allocate a NULL-terminated C `char*[]` from a seq of strings in `arena`."
  ^MemorySegment [^Arena arena strs]
  (let
    [strs
     (vec strs)

     n
     (count strs)

     ^MemorySegment seg
     (.allocate arena (long (* 8 (inc n))))]

    (dotimes [i n]
      (.setAtIndex seg ADDR i (.allocateFrom arena ^String (nth strs i))))
    (.setAtIndex seg ADDR n MemorySegment/NULL)
    seg))

(defn- open-fd-numbers
  "Every descriptor this process holds RIGHT NOW, from the kernel's own listing.
   nil when that directory is unreadable — then there is nothing to sweep."
  []
  (when-let [names (.list (File. ^String (if mac? "/dev/fd" "/proc/self/fd")))]
    (keep (fn [^String n]
            (try (Integer/parseInt n) (catch NumberFormatException _ nil)))
          names)))

(defn- close-inherited!
  "Declare that the child inherits NOTHING above stdio, keeping only `keep` for
   the file actions already queued ahead of this one.

   `posix_spawn` is not `ProcessBuilder`. The JDK's own spawn path runs through
   `jspawnhelper`, which closes every descriptor above 2 in the child before it
   execs; `posix_spawn` performs ONLY the file actions it is handed. The JVM does
   not set FD_CLOEXEC on the sockets it opens, so without this a pty child
   inherits the entire descriptor table of the process that spawned it — the
   gateway's LISTENING socket included. A child that outlives the gateway and
   never exits on its own (`adb`'s daemon, a stray `python -m http.server`) then
   holds that listen socket open forever, and the next `gateway start` fails to
   bind a port that nothing is serving.

   macOS does this in the kernel via POSIX_SPAWN_CLOEXEC_DEFAULT (`spawn-flags`),
   which is atomic: it cannot race a descriptor another thread opens while these
   actions are being built. Elsewhere prefer glibc's `addclosefrom_np` for the
   same atomicity, and fall back to sweeping the fd listing. The sweep is the only
   racy variant — a descriptor opened after the listing still leaks, and the
   listing's OWN directory fd is already closed by the time the child replays the
   action — which is why it is the last resort and not the default. A close action
   on a stale but in-range fd is tolerated rather than fatal."
  [fa keep]
  (when-not mac?
    (if-let [h @h-fa-closefrom]
      (invoke h fa (int 3))
      (doseq
        [fd (open-fd-numbers)
         :when (and (<= 3 (long fd)) (not (contains? keep (int fd))))]

        (try (invoke @h-fa-close fa (int fd)) (catch Throwable _ nil))))))

(defn- winsize
  ^MemorySegment [^Arena arena rows cols]
  (let [^MemorySegment ws (.allocate arena (long 8))]
    (.setAtIndex ws S 0 (short rows))
    (.setAtIndex ws S 1 (short cols))
    (.setAtIndex ws S 2 (short 0))
    (.setAtIndex ws S 3 (short 0))
    ws))

(defn- decode-status
  "waitpid status int -> conventional exit code (128+signal when killed)."
  [^long status]
  (let [st (bit-and status 0xffff)]
    (if (zero? (bit-and st 0x7f))
      (bit-and (bit-shift-right st 8) 0xff) ;; WIFEXITED -> WEXITSTATUS
      (+ 128 (bit-and st 0x7f)))))          ;; WIFSIGNALED -> 128+signo

;; struct pollfd { int fd; short events; short revents; } — 8 bytes, and POLLIN is
;; 0x0001 on both platforms.
(def ^:private POLLIN (short 0x0001))

(def ^:private reader-poll-ms
  "How long one poll in [[reader-loop!]] waits before re-asking whether the child
   has been reaped. It is pure EXIT latency on an idle terminal — output itself is
   never delayed, because a poll returns the moment bytes are queued — and a live
   shell costs one syscall a hundredth of a second, which is nothing beside the
   process it is watching."
  (int 10))

(defn- close-fd-once!
  "Close a descriptor through `open?` so it is closed EXACTLY once. A second close
   is not a harmless no-op: the number is free the moment the first one returns and
   another thread's `open` may already be holding it."
  [open? ^long fd]
  (when (compare-and-set! open? true false)
    (try (invoke @h-close (int fd)) (catch Throwable _ nil))))

(defn- reader-loop!
  "Drain the PTY master fd into `pout` until the terminal has no writer left, then
   close it. Runs on its own daemon thread; a real terminal has no separate stderr,
   so this is the single merged stream. Every chunk is ALSO fanned out to each fn in
   `@listeners` (deref'd fresh each chunk) — the passthrough bridge subscribes there
   to tee live output to attached human terminals without stealing from the pump.

   The parent keeps a slave descriptor (`slave`) so the CHILD's exit is never the
   LAST close of this terminal: on macOS that last close revokes the tty and
   DISCARDS whatever is still queued in it, so a command that printed and exited
   before this thread copied the bytes out reported that it had printed nothing at
   all. Holding a slave costs the EOF that used to end this loop, so the loop polls:
   it takes whatever is queued, and once the child is reaped (`exit`) and one poll
   finds the terminal empty, everything the child wrote has been handed over. Only
   then is the parent's slave dropped — and reading continues, because a grandchild
   that inherited the terminal (`cmd &`) may still write to it, and EOF again means
   what it always meant: the last writer is gone."
  [master slave ^PipedOutputStream pout listeners exit master-open? slave-open?]
  (try (with-open [arena (Arena/ofConfined)]
         (let
           [buf (.allocate arena (long 8192))
            ^MemorySegment pfd (.allocate arena (long 8))
            ;; ONE read of whatever the terminal holds, into the pipe and the
            ;; listeners. Answers whether the stream is still open: a zero or
            ;; negative `read` is its end.
            drain-once! (fn []
                          (let [n (long (invoke @h-read (int master) buf (long 8192)))]
                            (when (pos? n)
                              (let [ba (.toArray (.asSlice buf 0 n) ValueLayout/JAVA_BYTE)]
                                (.write pout ba)
                                (.flush pout)
                                (doseq [l @listeners]
                                  (try (l ba) (catch Throwable _ nil)))))
                            (pos? n)))]

           (.setAtIndex pfd I 0 (int master))
           (.setAtIndex pfd S 2 (short POLLIN))
           (when (loop []

                   (let [ready (int (invoke @h-poll pfd (long 1) reader-poll-ms))]
                     (cond
                       ;; Bytes are queued: take them, whoever wrote them.
                       (pos? ready) (when (drain-once!) (recur))
                       ;; Nothing queued and the child is reaped: the terminal holds
                       ;; nothing more of the child's, so the parent's slave can go.
                       (some? @exit) true
                       ;; Still running, or an interrupted poll: keep watching.
                       :else (do (when (neg? ready) (Thread/sleep 1)) (recur)))))
             (close-fd-once! slave-open? slave)
             (loop []

               (when (drain-once!) (recur))))))
       (catch Throwable _ nil)
       (finally (try (.close pout) (catch Throwable _ nil))
                (close-fd-once! slave-open? slave)
                (close-fd-once! master-open? master))))

(defn spawn!
  "Spawn `command` (a vector of program + args) under a real pseudo-terminal.
   Options: :dir (working dir string), :env (Map string->string), :cols, :rows.
   Returns the handle map documented on the namespace."
  [{:keys [command dir env cols rows] :or {cols 120 rows 40}}]
  (let
    [spawned
     (with-open [arena (Arena/ofConfined)]
       (let
         [^MemorySegment amaster (.allocate arena (long 4))
          ^MemorySegment aslave (.allocate arena (long 4))
          ^MemorySegment pidp (.allocate arena (long 4))
          fa (.allocate arena (long 256)) ;; opaque; big enough for glibc's struct
          at (.allocate arena (long 512))
          ws (winsize arena rows cols)
          argv (build-strv arena command)
          envp (build-strv arena
                           (map (fn [[k v]]
                                  (str k "=" v))
                                env))
          path (.allocateFrom arena ^String (first command))]

         (when-not (zero?
                     (int
                       (invoke @h-openpty amaster aslave MemorySegment/NULL MemorySegment/NULL ws)))
           (throw (ex-info "openpty failed" {:type ::openpty-failed})))
         (let
           [master (int (.getAtIndex amaster I 0))
            slave (int (.getAtIndex aslave I 0))]

           (invoke @h-fa-init fa)
           (invoke @h-fa-dup2 fa slave (int 0))
           (invoke @h-fa-dup2 fa slave (int 1))
           (invoke @h-fa-dup2 fa slave (int 2))
           (invoke @h-fa-close fa master)
           (invoke @h-fa-close fa slave)
           ;; AFTER the dup2s (which still need `slave`) and after the two closes
           ;; above, which is why both are in the keep set.
           (close-inherited! fa #{master slave})
           (invoke @h-at-init at)
           (invoke @h-at-flags at spawn-flags)
           (when dir
             (when-let [h @h-fa-chdir]
               (try (invoke h fa (.allocateFrom arena ^String dir)) (catch Throwable _ nil))))
           (let [rc (int (invoke @h-spawn pidp path fa at argv envp))]
             (invoke @h-fa-destr fa)
             (invoke @h-at-destr at)
             ;; The parent KEEPS its slave descriptor: the child's exit must not be
             ;; the last close of this terminal ([[reader-loop!]] owns dropping it,
             ;; once nothing of the child's is queued in the tty any more).
             (when-not (zero? rc)
               (invoke @h-close slave)
               (invoke @h-close master)
               (throw (ex-info (str "posix_spawn failed (errno " rc ")")
                               {:type ::spawn-failed :errno rc})))
             [master slave (long (.getAtIndex pidp I 0))]))))

     [master-fd slave-fd pid]
     spawned

     master-fd
     (int master-fd)

     slave-fd
     (int slave-fd)

     pid
     (long pid)

     pin
     (PipedInputStream. (* 64 1024))

     pout
     (PipedOutputStream. pin)

     listeners
     (atom [])

     exit
     (atom nil)

     ;; Each descriptor is closed exactly once, by whichever side finishes last.
     master-open?
     (atom true)

     slave-open?
     (atom true)

     _rthread
     (doto (Thread.
             ^Runnable
             (fn []
               (reader-loop! master-fd slave-fd pout listeners exit master-open? slave-open?))
             (str "vis-pty-read-" pid))
       (.setDaemon true)
       (.start))

     wait-fn
     (fn []
       (or @exit
           (locking exit
             (or @exit
                 (with-open [arena (Arena/ofConfined)]
                   (let [^MemorySegment status (.allocate arena (long 4))]
                     (invoke @h-waitpid (int pid) status (int 0))
                     (let [code (decode-status (long (.getAtIndex status I 0)))]
                       (reset! exit code)
                       code)))))))

     ;; The reader can no longer learn that the child is gone from an EOF the
     ;; terminal will not send while the parent holds a slave, so ONE thread reaps
     ;; the child the moment it exits; every other caller of `:wait` reads the code
     ;; it published straight out of `exit`.
     _reaper
     (doto (Thread. ^Runnable wait-fn (str "vis-pty-reap-" pid)) (.setDaemon true) (.start))]

    {:pid pid
     :in pin
     :send (fn [^bytes b]
             (with-open [arena (Arena/ofConfined)]
               (let
                 [n (alength b)
                  buf (.allocate arena (long (max 1 n)))]

                 (MemorySegment/copy b 0 buf ValueLayout/JAVA_BYTE (long 0) n)
                 (invoke @h-write master-fd buf (long n)))))
     :wait wait-fn
     :alive? (fn []
               (if-let [^ProcessHandle ph (.orElse (ProcessHandle/of pid) nil)]
                 (.isAlive ph)
                 (nil? @exit)))
     :destroy (fn [force?]
                (try (invoke @h-kill (int pid) (if force? SIGKILL SIGTERM))
                     (catch Throwable _ nil)))
     ;; Subscribe to live master output: `f` is called with each byte[] chunk
     ;; as it arrives. Returns a 0-arg unsubscribe fn. The passthrough bridge
     ;; (internal.foundation.pty-bridge) tees these chunks to attached sockets.
     :add-listener (fn [f]
                     (swap! listeners conj f)
                     (fn []
                       (swap! listeners (fn [ls]
                                          (vec (remove #(identical? % f) ls))))))}))
