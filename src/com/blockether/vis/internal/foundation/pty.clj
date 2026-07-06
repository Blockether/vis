(ns com.blockether.vis.internal.foundation.pty
  "Pure-Java pseudo-terminal for `shell_bg` — NO JNA, NO extracted native helper,
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
  (:import (java.io PipedInputStream PipedOutputStream)
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

;; =============================================================================
;; FFM plumbing
;; =============================================================================

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
  (let [^SymbolLookup std
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
;; addchdir_np: glibc >= 2.29 and macOS >= 10.15 (best-effort; ignored if absent).
(def ^:private h-fa-chdir
  (delay (try (dh "posix_spawn_file_actions_addchdir_np" I [ADDR ADDR]) (catch Throwable _ nil))))

(defn- invoke
  ^Object [h & args]
  (.invokeWithArguments ^java.lang.invoke.MethodHandle h (object-array args)))

;; POSIX_SPAWN_SETSID makes the child a session leader (detached from vis's own
;; controlling terminal) — the value differs by platform.
(def ^:private POSIX_SPAWN_SETSID (short (if mac? 0x0400 0x80)))
(def ^:private SIGTERM (int 15))
(def ^:private SIGKILL (int 9))

;; =============================================================================
;; spawn!
;; =============================================================================

(defn- build-strv
  "Allocate a NULL-terminated C `char*[]` from a seq of strings in `arena`."
  ^MemorySegment [^Arena arena strs]
  (let [strs
        (vec strs)

        n
        (count strs)

        ^MemorySegment seg
        (.allocate arena (long (* 8 (inc n))))]

    (dotimes [i n]
      (.setAtIndex seg ADDR i (.allocateFrom arena ^String (nth strs i))))
    (.setAtIndex seg ADDR n MemorySegment/NULL)
    seg))

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

(defn- reader-loop!
  "Drain the PTY master fd into `pout` until EOF, then close it. Runs on its own
   daemon thread; a real terminal has no separate stderr, so this is the single
   merged stream. Every chunk is ALSO fanned out to each fn in `@listeners`
   (deref'd fresh each chunk) — the passthrough bridge subscribes there to tee
   live output to attached human terminals without stealing from the pump."
  [^long master ^PipedOutputStream pout listeners]
  (try (with-open [arena (Arena/ofConfined)]
         (let [buf (.allocate arena (long 8192))]
           (loop []

             (let [n (long (invoke @h-read (int master) buf (long 8192)))]
               (when (pos? n)
                 (let [ba (.toArray (.asSlice buf 0 n) ValueLayout/JAVA_BYTE)]
                   (.write pout ba)
                   (.flush pout)
                   (doseq [l @listeners]
                     (try (l ba) (catch Throwable _ nil))))
                 (recur))))))
       (catch Throwable _ nil)
       (finally (try (.close pout) (catch Throwable _ nil)))))

(defn spawn!
  "Spawn `command` (a vector of program + args) under a real pseudo-terminal.
   Options: :dir (working dir string), :env (Map string->string), :cols, :rows.
   Returns the handle map documented on the namespace."
  [{:keys [command dir env cols rows] :or {cols 120 rows 40}}]
  (let [master
        (with-open [arena (Arena/ofConfined)]
          (let [^MemorySegment amaster (.allocate arena (long 4))
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

            (when-not
              (zero? (int
                       (invoke @h-openpty amaster aslave MemorySegment/NULL MemorySegment/NULL ws)))
              (throw (ex-info "openpty failed" {:type ::openpty-failed})))
            (let [master (int (.getAtIndex amaster I 0))
                  slave (int (.getAtIndex aslave I 0))]

              (invoke @h-fa-init fa)
              (invoke @h-fa-dup2 fa slave (int 0))
              (invoke @h-fa-dup2 fa slave (int 1))
              (invoke @h-fa-dup2 fa slave (int 2))
              (invoke @h-fa-close fa master)
              (invoke @h-fa-close fa slave)
              (invoke @h-at-init at)
              (invoke @h-at-flags at POSIX_SPAWN_SETSID)
              (when dir
                (when-let [h @h-fa-chdir]
                  (try (invoke h fa (.allocateFrom arena ^String dir)) (catch Throwable _ nil))))
              (let [rc (int (invoke @h-spawn pidp path fa at argv envp))]
                (invoke @h-fa-destr fa)
                (invoke @h-at-destr at)
                (invoke @h-close slave) ;; parent drops slave -> master sees EOF on exit
                (when-not (zero? rc)
                  (invoke @h-close master)
                  (throw (ex-info (str "posix_spawn failed (errno " rc ")")
                                  {:type ::spawn-failed :errno rc})))
                [master (long (.getAtIndex pidp I 0))]))))

        [master-fd pid]
        master

        master-fd
        (int master-fd)

        pid
        (long pid)

        pin
        (PipedInputStream. (* 64 1024))

        pout
        (PipedOutputStream. pin)

        listeners
        (atom [])

        _rthread
        (doto (Thread. ^Runnable
                       (fn []
                         (reader-loop! master-fd pout listeners))
                       (str "vis-pty-read-" pid))
          (.setDaemon true)
          (.start))

        exit
        (atom nil)

        wait-fn
        (fn []
          (or @exit
              (locking exit
                (or @exit
                    (with-open [arena (Arena/ofConfined)]
                      (let [^MemorySegment status (.allocate arena (long 4))]
                        (invoke @h-waitpid (int pid) status (int 0))
                        (let [code (decode-status (long (.getAtIndex status I 0)))]
                          (try (invoke @h-close master-fd) (catch Throwable _ nil))
                          (reset! exit code)
                          code)))))))]

    {:pid pid
     :in pin
     :send (fn [^bytes b]
             (with-open [arena (Arena/ofConfined)]
               (let [n (alength b)
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
