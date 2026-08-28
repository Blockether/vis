(ns com.blockether.vis.internal.gateway.bus
  "Cross-process gateway event bus.

   The gateway's live event log + SSE fan-out (`gateway.state`) is a
   PROCESS-LOCAL in-memory registry: `append-event!` only reaches
   subscribers inside the SAME JVM. That is why a turn streaming in the
   TUI process is invisible to another process watching the SAME
   conversation — each process has its own registry, and the only thing
   they share is the persisted DB (which lands whole turns, not the live
   token stream). So two watchers never stream together.

   This bus closes that gap with the simplest transport that needs no
   schema change and no always-on daemon: a shared append-only journal
   under `~/.vis/gateway/events/<sid>.ndjson`. Every LOCALLY-produced
   gateway event is `publish!`ed there (one JSON line, tagged with this
   process's `producer` id). A background tailer in each process follows
   those files and re-delivers FOREIGN events (producer != self) into the
   local registry via a delivery fn wired by `gateway.state` — so every
   process's subscribers see the same stream, live.

   Ordering/seq: exactly ONE turn runs per session at a time, so at any
   moment a single producer owns the stream and its monotonic `:seq` is
   authoritative for every watcher. The producer truncates the journal at
   each `turn.started`, bounding a file to one turn's worth of deltas;
   consumers detect the truncation (offset past EOF) and rewind.

   Degrades safely: any IO failure is swallowed and the process falls
   back to today's in-process-only behavior."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [java.io File RandomAccessFile]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(def ^:private POLL_MS
  "Sleep between tail polls while a sibling process is actively streaming."
  100)

(def ^:private IDLE_POLL_MS
  "Sleep once the tail has gone quiet — the steady state, since the cross-process
   tailer only has work when a SIBLING vis process shares a session. Every poll
   still stats every journal, so backing off to 2×/s here (vs 17×/s before) is
   what keeps an otherwise-idle daemon off the CPU."
  500)

(def ^:private ^:const IDLE_AFTER
  "Consecutive quiet polls before backing off from `POLL_MS` to `IDLE_POLL_MS`."
  20)

(def ^:private ^:const MAX_FILE_BYTES (* 16 1024 1024))

(def ^:private ^:const RETAIN_MS
  "Age past which an untouched journal is presumed orphaned and swept: a live
   session rewrites (truncates + appends) its journal every turn, so a file whose
   mtime is a day stale cannot belong to a running turn."
  (* 24 60 60 1000))

(def ^:private ^:const SWEEP_MS
  "Run the orphan-journal sweep about this often (wall-clock ms)."
  (* 60 1000))

(defonce ^{:doc "Stable per-process id: foreign events carry a different one."} producer-id
  (str (java.util.UUID/randomUUID)))

(def ^:private producer-pid
  "This process's OS pid, tagged onto every published event as `:_pid`. Lets a
   consumer tell a turn genuinely streaming in a live SIBLING process apart from
   one orphaned by a crashed/restarted daemon — the difference between hydrating
   a live turn and reaping a dead one."
  (try (.pid (java.lang.ProcessHandle/current)) (catch Throwable _ -1)))

(defn- producer-alive?
  "True when the OS process that produced a journal event is still running. A
   missing pid (a pre-`:_pid` journal) or our own pid is treated as alive, so we
   never reap a turn we cannot PROVE is orphaned."
  [pid]
  (or (nil? pid)
      (= (long pid) (long producer-pid))
      (try (.isPresent (java.lang.ProcessHandle/of (long pid))) (catch Throwable _ true))))

(defn- events-dir
  ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway" "events"])))

(defn- session-file ^File [sid] (.toFile (.resolve (events-dir) (str sid ".ndjson"))))

(defn- ensure-dir!
  []
  (let [dir (events-dir)]
    (when-not (Files/exists dir (make-array LinkOption 0))
      (Files/createDirectories dir (make-array FileAttribute 0)))
    dir))

;; Fleet liveness index

(def ^:private ^:const LIVE_CACHE_MS
  "How long ONE scan of the liveness directory is reused. A fleet listing asks for
   every session's liveness inside a single build, so the scan must not run per
   row. A local `mark-live!`/`clear-live!` invalidates immediately, so only a
   SIBLING process's transition can be this stale — invisible next to the seconds
   between a client's polls."
  200)

(defn- live-dir
  "Directory holding one tiny marker per turn in flight ANYWHERE on this machine.
   A journal answers \"what happened in session X\" and costs a file read per
   session; this answers \"which sessions are running\" in one listing whose size
   is the number of LIVE turns, which is what a fleet listing can afford."
  ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway" "live"])))

(defn- live-file ^File [sid] (.toFile (.resolve (live-dir) (str sid ".json"))))

(defonce ^:private live-cache (atom {:at 0 :turns {}}))

(defn- invalidate-live-cache! [] (swap! live-cache assoc :at 0) nil)

(defn- scan-live-turns
  "Read every marker into `{sid turn-id}`, keeping only those whose producer
   process is still running. A marker whose pid is gone (kill -9, a crash, a
   daemon restart mid-turn) is DELETED here: the producer that owed it a retract
   no longer exists, and without this one dead process leaves a session lit
   forever on every client of this machine."
  []
  (try (let [dir (.toFile (live-dir))]
         (if-not (.isDirectory dir)
           {}
           (reduce (fn [acc ^File f]
                     (let [marker (when (str/ends-with? (.getName f) ".json")
                                    (try (wire/parse-json (slurp f)) (catch Throwable _ nil)))
                           sid (get marker "session_id")
                           tid (get marker "turn_id")
                           pid (get marker "pid")]

                       (cond (nil? marker) acc
                             (and sid tid pid (producer-alive? pid)) (assoc acc (str sid) (str tid))
                             :else (do (.delete f) acc))))
                   {}
                   (or (.listFiles dir) (make-array File 0)))))
       (catch Throwable t (tel/log! :debug ["gateway-bus: live scan failed" (ex-message t)]) {})))

(defn live-turns
  "`{session-id-string turn-id-string}` for every turn in flight on this machine —
   this process's and every sibling's alike.

   THE cross-process answer to \"is this session live\". A process-local registry
   only knows the turns IT runs, and a sibling's turn is mirrored into it only
   once somebody SUBSCRIBES to that session, so a gateway answering from the
   registry lit up exactly the sessions the asking client happened to watch: two
   apps talking to the SAME gateway reported two different fleets."
  []
  (let [{:keys [at turns]}
        @live-cache

        now
        (util/now-ms)]

    (if (< (- now (long at)) (long LIVE_CACHE_MS))
      turns
      (let [fresh (scan-live-turns)]
        (reset! live-cache {:at now :turns fresh})
        fresh))))

(defn live-turn-id
  "The turn `sid` is running in ANY vis process on this machine, or nil."
  [sid]
  (get (live-turns) (str sid)))

(defn- mark-live!
  "Announce that `turn-id` is in flight for `sid` in THIS process. Never throws."
  [sid turn-id]
  (try (let [dir (live-dir)]
         (when-not (Files/exists dir (make-array LinkOption 0))
           (Files/createDirectories dir (make-array FileAttribute 0)))
         (spit (live-file sid)
               (wire/json-str {"schema" 1
                               "session_id" (str sid)
                               "turn_id" (str turn-id)
                               "pid" producer-pid
                               "started_at" (util/now-ms)}))
         (invalidate-live-cache!))
       (catch Throwable t (tel/log! :debug ["gateway-bus: live mark failed" (ex-message t)])))
  nil)

(defn- clear-live!
  "Retract `sid`'s liveness marker: its turn reached a terminal. Never throws."
  [sid]
  (try (.delete (live-file sid))
       (invalidate-live-cache!)
       (catch Throwable t (tel/log! :debug ["gateway-bus: live clear failed" (ex-message t)])))
  nil)

;; Fleet human-input index

(def ^:private ^:const WAITING_CACHE_MS
  "How long ONE scan of the waiting directory is reused. Same reason as
   `LIVE_CACHE_MS`: a fleet listing asks every session whether it is parked
   inside a single build, so the scan must not run per row."
  200)

(defn- waiting-dir
  "Directory holding one marker per session PARKED on an unanswered human-input
   request ANYWHERE on this machine. The journal says a request was RAISED and,
   later, that it was closed; this says which sessions are waiting on a human
   RIGHT NOW, in one listing whose size is the number of open requests — which
   is what a fleet listing can afford (same shape as [[live-dir]])."
  ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway" "waiting"])))

(defn- waiting-file ^File [sid] (.toFile (.resolve (waiting-dir) (str sid ".json"))))

(defonce ^:private waiting-cache (atom {:at 0 :sessions {}}))

(defn- invalidate-waiting-cache! [] (swap! waiting-cache assoc :at 0) nil)

(defn- waiting-marker
  "This machine's waiting marker for `sid`, or nil."
  [sid]
  (try (let [f (waiting-file sid)]
         (when (.isFile f) (wire/parse-json (slurp f))))
       (catch Throwable _ nil)))

(defn- scan-waiting
  "Read every marker into `{sid [request …]}`, keeping only those whose producer
   process is still running. A marker whose pid is gone (a crash or a restart
   while a dialog stood open) is DELETED here: the thread that was blocked on
   that answer no longer exists, and without this one dead process leaves a
   session demanding input forever on every client of this machine."
  []
  (try
    (let [dir (.toFile (waiting-dir))]
      (if-not (.isDirectory dir)
        {}
        (reduce (fn [acc ^File f]
                  (let [marker (when (str/ends-with? (.getName f) ".json")
                                 (try (wire/parse-json (slurp f)) (catch Throwable _ nil)))
                        sid (get marker "session_id")
                        pid (get marker "pid")
                        requests (get marker "requests")]

                    (cond (nil? marker) acc
                          (and sid (seq requests) (producer-alive? pid)) (assoc acc
                                                                           (str sid) (vec requests))
                          :else (do (.delete f) acc))))
                {}
                (or (.listFiles dir) (make-array File 0)))))
    (catch Throwable t (tel/log! :debug ["gateway-bus: waiting scan failed" (ex-message t)]) {})))

(defn waiting-requests
  "`{session-id-string [{\"id\" … \"since\" …} …]}` for every human-input request
   still unanswered on this machine — this process's and every sibling's alike.

   THE cross-process answer to \"is this session waiting on a HUMAN\". The
   engine's pending registry is process-local (only the process that raised a
   request can settle it), so a gateway answering from it lit up exactly the
   requests it happened to own: the phone listing a fleet was never told that a
   run is blocked on the operator. Answering the request still goes through its
   own process; this index only says WHO is waiting, so every surface — session
   list, TUI picker, push — paints the same demand."
  []
  (let [{:keys [at sessions]}
        @waiting-cache

        now
        (util/now-ms)]

    (if (< (- now (long at)) (long WAITING_CACHE_MS))
      sessions
      (let [fresh (scan-waiting)]
        (reset! waiting-cache {:at now :sessions fresh})
        fresh))))

(defn session-waiting?
  "True when `sid` is parked on an unanswered human-input request in ANY vis
   process on this machine."
  [sid]
  (boolean (seq (get (waiting-requests) (str sid)))))

(defn- mark-waiting!
  "Announce that `sid` is blocked on `request` until somebody answers it. A
   marker left by a DEAD process is replaced rather than extended, so an
   abandoned request cannot ride along with a live one. Never throws."
  [sid request]
  (try (let [dir
             (waiting-dir)

             marker
             (waiting-marker sid)

             rid
             (str (get request "id"))

             kept
             (if (= (long producer-pid) (long (or (get marker "pid") -1)))
               (vec (remove #(= rid (str (get % "id"))) (get marker "requests")))
               [])]

         (when-not (Files/exists dir (make-array LinkOption 0))
           (Files/createDirectories dir (make-array FileAttribute 0)))
         (spit (waiting-file sid)
               (wire/json-str {"schema" 1
                               "session_id" (str sid)
                               "pid" producer-pid
                               "requests" (conj kept {"id" rid "since" (util/now-ms)})}))
         (invalidate-waiting-cache!))
       (catch Throwable t (tel/log! :debug ["gateway-bus: waiting mark failed" (ex-message t)])))
  nil)

(defn- clear-waiting!
  "Retract ONE request from `sid`'s marker: it was answered, cancelled or timed
   out. The marker survives while other requests are still open — a run can park
   on more than one — and is deleted with the last of them. Never throws."
  [sid request-id]
  (try (let [marker
             (waiting-marker sid)

             rid
             (str request-id)

             kept
             (vec (remove #(= rid (str (get % "id"))) (get marker "requests")))]

         (when marker
           (if (seq kept)
             (spit (waiting-file sid) (wire/json-str (assoc marker "requests" kept)))
             (.delete (waiting-file sid)))
           (invalidate-waiting-cache!)))
       (catch Throwable t (tel/log! :debug ["gateway-bus: waiting clear failed" (ex-message t)])))
  nil)

(defn- clear-all-waiting!
  "Drop `sid`'s marker outright: its turn reached a terminal, so no thread of it
   can still be blocked on an answer. Never throws."
  [sid]
  (try (.delete (waiting-file sid))
       (invalidate-waiting-cache!)
       (catch Throwable t (tel/log! :debug ["gateway-bus: waiting clear failed" (ex-message t)])))
  nil)
;; sid-str -> this process's tail of that session's journal:
;;   {:lock Object   ; hydrate! (HTTP threads) vs drain-file! (tailer thread)
;;    :off  long     ; bytes already consumed
;;    :head String}  ; fingerprint of the file's FIRST bytes = its GENERATION
;; ONE entry per session, so a tail is created, read and dropped as a unit —
;; parallel sid-keyed atoms drift the moment one call site forgets one of them.
(defonce ^:private tails (atom {}))

(defn- tail
  "`sid`'s tail entry, lock included, created on first use."
  [sid]
  (let [k (str sid)]
    (or (get @tails k)
        (get (swap! tails (fn [m]
                            (if (get m k) m (assoc m k {:lock (Object.) :off 0}))))
             k))))

(defn- tail-lock
  "The ONE lock owning this process's tail of `sid`'s journal. `hydrate!` runs on
   HTTP threads while `drain-file!` runs on the tailer thread; both read the same
   file and move the same cursor, so unsynchronized they hand the SAME lines to
   the deliver-fn twice. Everything else about a tail is plain state under this
   lock — deliberately NO second forward-only/CAS discipline layered on top."
  ^Object [sid]
  (:lock (tail sid)))

(defn- set-tail!
  "Record that this process consumed `off` bytes of the journal generation whose
   first bytes are `head`. Callers hold [[tail-lock]]."
  [sid off head]
  (swap! tails update
    (str sid)
    (fn [t]
      (assoc (or t {:lock (Object.)})
        :off (long off)
        :head head)))
  nil)

(def ^:private ^:const HEAD_MAX_BYTES
  "Cap on the generation-identifying read — a `turn.started` line, not a budget
   anyone should tune."
  512)

(defn- head-of
  "The FIRST LINE of `b`, byte-faithful (Latin-1, never lossy) and capped at
   `HEAD_MAX_BYTES`. That line is the generation's `turn.started`, whose `seq` +
   millisecond `ts` cannot repeat, so comparing whole lines beats fingerprinting
   a fixed byte span: no field can drift out of range."
  ^String [^bytes b]
  (let [n
        (int (min (alength b) (long HEAD_MAX_BYTES)))

        nl
        (loop [i 0]
          (cond (== i n) n
                (== (aget b i) 10) i
                :else (recur (inc i))))]

    (String. b 0 (int nl) StandardCharsets/ISO_8859_1)))

(defn- whole-bytes
  "How many bytes of `b` form COMPLETE lines: everything up to and including its
   LAST newline, 0 when none. A producer caught mid-write leaves a partial
   trailing line, and claiming it would make the next read resume INSIDE that
   line — losing the event outright once it lands."
  ^long [^bytes b]
  (loop [i (dec (alength b))]
    (cond (neg? i) 0
          (== (aget b i) 10) (inc i)
          :else (recur (dec i)))))

(defn- read-at!
  "Read up to `n` bytes of `raf` from `pos` into `buf`; returns how many were
   ACTUALLY read. A sibling truncating the journal between our `.length` and our
   read is ordinary, so a shorter file is a SHORT READ - not an `EOFException`
   that aborts the whole drain, precisely on the truncations we most need to see."
  ^long [^RandomAccessFile raf ^long pos ^bytes buf ^long n]
  (.seek raf pos)
  (loop [off 0]
    (if (>= off n)
      off
      (let [got (.read raf buf (int off) (int (- n off)))]
        (if (neg? got) off (recur (+ off (long got))))))))

(defn- read-head!
  "[[head-of]] through an ALREADY-OPEN `raf`. Leaves the raf cursor at the head -
   every caller seeks before reading on."
  ^String [^RandomAccessFile raf ^long len]
  (let [n
        (int (min len (long HEAD_MAX_BYTES)))

        buf
        (byte-array n)

        got
        (int (read-at! raf 0 buf n))]

    (head-of (if (== got n) buf (java.util.Arrays/copyOf buf got)))))

(defn- journal-head
  "[[read-head!]] for a file the caller has not opened. nil when unreadable."
  ^String [^File f]
  (try (with-open [raf (RandomAccessFile. f "r")]
         (read-head! raf (.length f)))
       (catch Throwable _ nil)))

;; Producer

(declare start!)

(def ^:private WRITE_QUEUE_SIZE
  "Bounded async journal backlog. Transient live deltas may be dropped when disk
   is behind; durable/store? events block until there is room and are flushed
   before `publish!` returns."
  4096)

(defonce ^:private ^ArrayBlockingQueue writer-queue (ArrayBlockingQueue. WRITE_QUEUE_SIZE))

(defonce ^:private writer (atom nil))

(defn- write-event!
  "Write one already-shaped (canonical string-keyed) event to the shared\n   journal. Runs ONLY on the single FIFO writer thread — that, not a lock, is\n   what serializes writes (`session-file` hands back a FRESH File per call, so\n   `locking` it would take a brand-new monitor every time and guard nothing).\n   Never throws."
  [sid event {:keys [store? truncate?]}]
  (try (ensure-dir!)
       (let [f
             (session-file sid)

             line
             (str (wire/json-str (assoc event
                                   "_producer" producer-id
                                   "_pid" producer-pid
                                   "_store" (boolean store?)))
                  "\n")]

         (with-open [raf (RandomAccessFile. f "rw")]
           (let [len (.length raf)]
             (cond truncate? (.setLength raf 0)
                   (> len (long MAX_FILE_BYTES))
                   ;; Size cap MID-turn. Keep the turn's first line: hydrate reads
                   ;; it to learn which turn is in flight (`:current-turn`, and the
                   ;; orphan reap's `turn_id`) and the tailer reads it as the
                   ;; generation marker. Dropping it left a >16MB turn impossible to
                   ;; mirror without duplicating, and impossible to reap at all.
                   (let [head (read-head! raf len)]
                     (.setLength raf 0)
                     (when (< (count head) (long HEAD_MAX_BYTES))
                       (.seek raf 0)
                       (.write raf (.getBytes (str head "\n") StandardCharsets/UTF_8))))))
           (.seek raf (.length raf))
           (.write raf (.getBytes ^String line StandardCharsets/UTF_8))))
       ;; The fleet's liveness marker rides the SAME durable write as the event
       ;; that changes it: a turn whose start reached disk is announced to every
       ;; process on this machine, and a write that failed announces nothing.
       ;; A run PARKED on a human is the same kind of machine-wide fact, and the
       ;; only one no terminal event will ever announce (see `waiting-dir`).
       (case (str (get event "type"))
         "turn.started"
         (mark-live! sid (get event "turn_id"))

         ("turn.completed" "turn.failed" "turn.cancelled")
         (do (clear-live! sid) (clear-all-waiting! sid))

         "human_input.request"
         (mark-waiting! sid (get event "request"))

         "human_input.close"
         (clear-waiting! sid (get event "request_id"))

         nil)
       (catch Throwable t (tel/log! :debug ["gateway-bus: publish failed" (ex-message t)]) nil))
  nil)

(defn- spawn-writer-thread!
  "Create, mark daemon and START the journal writer thread, returning it. Seam:
   tests simulate a JVM that cannot create a thread by redefining this to throw."
  ^Thread []
  (let [t (Thread. ^Runnable
                   (fn []
                     (while (not (Thread/interrupted))
                       (try (let [{:keys [sid event opts done]} (.take writer-queue)]
                              (write-event! sid event opts)
                              (when done (deliver done true)))
                            (catch InterruptedException _ (.interrupt (Thread/currentThread)))
                            (catch Throwable t
                              (tel/log! :debug ["gateway-bus: writer failed" (ex-message t)])))))
                   "gateway-bus-writer")]
    (.setDaemon t true)
    (.start t)
    t))

(defn- start-writer!
  "Start the single async journal writer. Idempotent. The gateway hot path can now
   fan out to local subscribers without waiting for ndjson I/O; cross-process
   ordering is preserved by this one FIFO writer thread."
  []
  (let [cur @writer]
    ;; Resurrect a DEAD writer, not just a missing one: the loop exits on
    ;; interrupt, and a `writer` atom still holding that corpse made every later
    ;; durable `publish!` block the full timeout and then vanish — the process
    ;; silently stops journalling (no cross-process mirror, no orphan-reap
    ;; terminal) for the rest of its life.
    (when (and (not= ::starting cur)
               (or (nil? cur) (not (.isAlive ^Thread cur)))
               (compare-and-set! writer cur ::starting))
      ;; ::starting is the "another thread is already spawning" gate, so it must
      ;; never outlive this block: a failed spawn (thread limit / OOME) that left
      ;; it behind wedged the gate forever and killed journalling exactly as
      ;; permanently as the dead-writer corpse above.
      (try (reset! writer (spawn-writer-thread!))
           (catch Throwable t
             (compare-and-set! writer ::starting nil)
             (tel/log! :debug ["gateway-bus: writer start failed" (ex-message t)])))))
  nil)

(def ^:private durable-write-timeout-ms
  "Ceiling a producer thread will spend handing ONE durable event to the writer:
   the enqueue wait plus the acknowledgement wait. Named so a wedged-writer test
   can lower the ceiling it is proving, instead of paying it twice per case."
  5000)

(defn- enqueue-write!
  [sid event {:keys [store?] :as opts}]
  (start-writer!)
  (if store?
    ;; Durable events are correctness boundaries (turn.started/completed/failed,
    ;; queue mutations, titles). Put them behind any already-enqueued transient
    ;; deltas and wait, so the file is ordered and tests/hydration see them.
    (let [done (promise)]
      ;; Bounded: never park a turn/provider thread forever on a wedged writer.
      (if (.offer writer-queue
                  {:sid sid :event event :opts opts :done done}
                  durable-write-timeout-ms
                  TimeUnit/MILLISECONDS)
        (deref done durable-write-timeout-ms false)
        (tel/log! :debug
                  ["gateway-bus: dropped durable event; writer queue wedged" (get event "type")]))
      nil)
    ;; Transient deltas are live hints. Never block provider/input threads on disk;
    ;; if the queue is saturated, sibling processes will catch the final canonical
    ;; text from the durable completion event instead of making the active TUI stutter.
    (when-not (.offer writer-queue {:sid sid :event event :opts opts})
      (tel/log! :debug ["gateway-bus: dropped transient event; writer queue full" (:type event)])))
  nil)

(defn publish!
  "Append one locally-produced `event` to the shared journal for `sid`.
   `truncate?` (true on `turn.started`) resets the file first, bounding it
   to the current turn. Never throws. Transient (`store? false`) live deltas are
   queued to a writer thread so ndjson I/O cannot stall the gateway hot path;
   durable events are flushed before this fn returns."
  ([sid event] (publish! sid event {:store? true}))
  ([sid event {:keys [store?] :as opts}]
   (try
     ;; Lazily start the tailer/writer on first publish so the native binary
     ;; (whose ns-load ran at BUILD time, where started threads can't be baked
     ;; into the image heap) still gets live runtime workers. Idempotent.
     (start!)
     (enqueue-write! sid event (assoc opts :store? store?))
     (catch Throwable t
       (tel/log! :debug ["gateway-bus: publish enqueue failed" (ex-message t)])
       nil))
   nil))

(defonce ^:private reaped-turns
  ;; sid -> turn-id this process has already orphan-reaped. The journal terminal
  ;; is the CROSS-process guard, but `publish!` lands it asynchronously, so two
  ;; hydrates racing inside that write window would both read no-terminal. This
  ;; marker is the in-process compare-and-set that closes the window.
  (atom {}))

(defn- claim-reap!
  "Compare-and-set the orphan-reap marker for `tid` in `sid`. True EXACTLY once
   per turn — only that caller may publish the synthetic terminal."
  [sid tid]
  (let [k
        (str sid)

        [old _]
        (swap-vals! reaped-turns assoc k tid)]

    (not= tid (get old k))))

(defn forget!
  "Drop a session's journal (on session close). Never throws."
  [sid]
  (try (.delete (session-file sid)) (catch Throwable _ nil))
  (clear-live! sid)
  (clear-all-waiting! sid)
  (let [k (str sid)]
    (swap! reaped-turns dissoc k)
    (swap! tails dissoc k))
  nil)

;; Consumer (tailer)

(defonce ^:private deliver-fn (atom nil))

(defn set-deliver-fn!
  "Register the fn the tailer calls for every FOREIGN event:
   `(f sid event store?)`. Wired by `gateway.state`."
  [f]
  (reset! deliver-fn f))

(defonce ^:private relevant-sid-fn (atom nil))

(defn set-relevant-sid-fn!
  "Register a predicate `(f sid) -> truthy` naming the sessions this process has a
   LOCAL consumer for. The tailer skips draining journals for any other sid: a
   foreign event for a session with no local registry entry is dropped by the
   delivery fn anyway (gateway.state/ingest-mirrored-event! no-ops on an unknown
   sid), so opening + reading + JSON-parsing — and even stat'ing — those journals
   on every poll is pure waste, the CPU an otherwise-idle daemon burns re-scanning
   every sibling's journal forever. Absent a wired predicate (tests, early boot)
   every sid is relevant, preserving the drain-everything behavior. Wired by
   `gateway.state`."
  [f]
  (reset! relevant-sid-fn f))

(defonce ^:private relevant-sids-fn (atom nil))

(defn set-relevant-sids-fn!
  "Register a 0-arg fn returning the COLLECTION of sids this process has a local
   consumer for. When wired, the tailer drains ONLY those sessions' journal files
   directly — it never lists/stats the whole events dir on a poll, so an idle
   daemon (empty set) does zero per-poll directory work. Falls back to the
   `relevant-sid?` directory scan when this is not wired (tests)."
  [f]
  (reset! relevant-sids-fn f))

(defn- relevant-sid?
  "Whether the tailer should drain `sid`'s journal — true unless a wired predicate
   says this process has no local consumer for it. Never throws: a predicate error
   fails open (drain) so a wiring bug can't silently stall cross-process mirroring."
  [sid]
  (if-let [f @relevant-sid-fn]
    (boolean (try (f sid) (catch Throwable _ true)))
    true))

(defn- journal-sid
  "The sid a `.ndjson` journal file name encodes, or nil for a non-journal file."
  ^String [^String n]
  (when (str/ends-with? n ".ndjson") (subs n 0 (- (count n) (count ".ndjson")))))

(defonce ^:private tailer (atom nil))

(def ^:private ^String self-marker
  "The exact `\"_producer\":\"<id>\"` JSON fragment this process writes. A raw
   substring test against it short-circuits the (comparatively expensive) JSON
   parse for our OWN journal lines — and the streaming producer tails its own
   file, so without this it would parse-then-discard nearly every line it wrote."
  (str "\"_producer\":\"" producer-id "\""))

;; One growable read buffer, reused across polls. drain-file! runs ONLY on the
;; single tailer thread (poll-once! drains files sequentially), so steady-state
;; tailing needs no per-drain byte-array allocation — zero GC churn on the hot
;; loop that runs in every vis process.
(defonce ^:private drain-buf (atom (byte-array 0)))

(defn- deliver-line!
  [sid ^String line]
  ;; Skip our OWN lines with a cheap substring test BEFORE parsing — the
  ;; streaming producer tails its own journal, so this avoids parse-then-discard
  ;; on nearly every line it just wrote. The `"_producer"` equality below stays as
  ;; a correctness backstop for the (foreign) lines that do get parsed.
  (when-not (.contains line self-marker)
    (when-let [event (wire/parse-json line)]
      (when-not (= (get event "_producer") producer-id)
        (when-let [f @deliver-fn]
          (let [store? (boolean (get event "_store"))
                clean (dissoc event "_producer" "_pid" "_store")]

            (try (f sid store? clean)
                 (catch Throwable t
                   (tel/log! :debug ["gateway-bus: deliver failed" (ex-message t)])))))))))

(defn- drain-file!
  "Drain the journal `f` of the session the CALLER named `sid`.

   `sid` is passed in and never re-derived from the file name: this process
   registers, looks up and mirrors a session under ONE key (the gateway registry
   is UUID-keyed, `server/path-sid`), and a String lookalike parsed out of
   `<sid>.ndjson` made `state/ingest-mirrored-event!`'s registry lookup miss —
   silently discarding every live event of a turn running in a sibling process."
  [sid ^File f]
  ;; Missing (session closed) or empty journal: nothing to deliver, and — the
  ;; point of doing this BEFORE `tail-lock` — nothing to remember either. A
  ;; drain that still had a closed sid in flight used to re-create its tail
  ;; entry after `forget!` dropped it, and `sweep!` only forgets sids whose
  ;; FILE it deletes, so that entry would sit in the map for the daemon's life.
  (when (pos? (.length f))
    ;; ONE tail owner per sid: `hydrate!` reads the same file and claims the same
    ;; cursor from an HTTP thread, so without this both deliver the same lines.
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (tail-lock sid)
      (let [{:keys [head] prev :off}
            (tail sid)

            off0
            (long (or prev 0))

            len
            (.length f)]

        ;; Unchanged since the last drain: no open, no read, no parse.
        (when-not (== len off0)
          (with-open [raf (RandomAccessFile. f "r")]
            (let [head' (read-head! raf len)
                  ;; The producer TRUNCATES the journal at every `turn.started` (and
                  ;; at the size cap), so a file that regrows PAST our cursor inside
                  ;; ONE poll interval is indistinguishable from appended data by
                  ;; length alone: the new turn's head — `turn.started` included —
                  ;; would be skipped forever and the tab would sit frozen-idle
                  ;; through a live sibling turn. That first line is the new turn's
                  ;; `turn.started` (unrepeatable seq + ms ts), so a CHANGED head
                  ;; PROVES a rewrite and we replay from byte 0.
                  off (if (or (and head (not= head head')) (> off0 len)) 0 off0)
                  ;; Bytes of COMPLETE lines this drain consumed (0 = nothing whole).
                  consumed
                  (if (<= len off)
                    0
                    (let [want (int (- len off))
                          ;; Reuse one growable buffer across polls (single tailer thread) so
                          ;; steady-state tailing allocates NOTHING — no per-drain byte-array.
                          ^bytes buf
                          (let [^bytes b @drain-buf]
                            (if (>= (alength b) want) b (reset! drain-buf (byte-array want))))
                          ;; Bytes ACTUALLY read: a sibling truncating mid-drain just
                          ;; shortens this, and the head check above already caught it.
                          remaining (int (read-at! raf off buf want))]

                      ;; ONE forward pass over the tail: decode each COMPLETE line in
                      ;; isolation and deliver it inline — no whole-tail String, no regex
                      ;; `split-lines`, no backward pre-scan, no re-encode to count bytes.
                      ;; `last-nl` tracks the last newline seen, so a trailing partial
                      ;; line simply stays unconsumed for the next drain.
                      (loop [start 0
                             i 0
                             last-nl -1]

                        (if (< i remaining)
                          (if (== (aget buf i) 10)
                            (let [end (if (and (> i start) (== (aget buf (dec i)) 13))
                                        (dec i) ; strip a CR from a CRLF line ending
                                        i)]
                              (when (> end start)
                                (let [line (String. buf start (- end start) StandardCharsets/UTF_8)]
                                  (when-not (str/blank? line) (deliver-line! sid line))))
                              (recur (inc i) (inc i) i))
                            (recur start (inc i) last-nl))
                          (inc last-nl)))))]

              (set-tail! sid (+ off (long consumed)) head')
              (pos? (long consumed)))))))))

(defn journal-high-water-seq
  "Highest `\"seq\"` persisted in `sid`'s journal file, or 0 when there is none.

   A daemon restart resets its in-memory `:seq` counter to 0, but a client
   (TUI) keeps its replay cursor as a monotonic MAX across reconnects — so
   events from the fresh daemon (seq 1, 2, …) fall UNDER the client's stale
   cursor and its `seq > cursor` replay filter silently drops the whole new
   turn (the orphan-reap terminal included). Seeding a fresh registry entry
   from this high-water keeps the restarted daemon numbering ABOVE what the
   client already saw. Never throws."
  [sid]
  (try
    (let [f (session-file sid)]
      (if (.exists f)
        (->> (str/split-lines (slurp f))
             (remove str/blank?)
             (keep wire/parse-json)
             (map #(long (or (get % "seq") 0)))
             (reduce max 0))
        0))
    (catch Throwable t (tel/log! :debug ["gateway-bus: high-water read failed" (ex-message t)]) 0)))

(defn hydrate!
  "Replay a session's CURRENT journal into this process's registry NOW, so a
   watcher subscribing mid-turn sees the turn running in a SIBLING process from
   its `turn.started` — not just the deltas that happen to arrive after it
   connects.

   The producer truncates the journal at each `turn.started`, so a file holds
   exactly one turn. We replay it ONLY while that turn is still running (no
   terminal event yet): a finished turn is already covered by the durable DB +
   normal history, and re-streaming it would double-render a completed answer.

   A non-terminal journal has TWO causes, told apart by `\"_pid\"` liveness: the
   producer is still alive (a real in-flight sibling turn — mirror it) OR the
   producer PROCESS is gone (a daemon crash/restart mid-turn). An orphaned turn
   will never emit a terminal, so resurrecting it pins this process's
   `:current-turn` to a dead turn — wedging the session queue (new sends pile up
   `queued`, nothing drains) and spinning every watcher forever. For an orphan we
   instead land a synthetic `turn.failed` so the queue drains, clients get
   closure, and no later hydrate replays it again.

   Runs under the sid's [[tail-lock]] and moves this process's tail cursor past
   the COMPLETE lines it read, so neither the background tailer nor a concurrent
   hydrate re-delivers what we hand over here. Never throws."
  [sid]
  (try
    (when-let [f (session-file sid)]
      (when (.exists f)
        #_{:clj-kondo/ignore [:locking-suspicious-lock]}
        (locking (tail-lock sid)
          (let [raw (Files/readAllBytes (.toPath f))
                ;; ONE read serves parsing, the cursor claim AND the generation
                ;; head: re-opening the file for the head would fingerprint a
                ;; LATER generation than the bytes we actually handed over.
                whole (whole-bytes raw)
                events (->> (str/split-lines
                              (String. ^bytes raw 0 (int whole) StandardCharsets/UTF_8))
                            (remove str/blank?)
                            (keep wire/parse-json))
                foreign (remove #(= (get % "_producer") producer-id) events)
                ;; A terminal from ANYONE (a sibling, or a prior orphan-reap by
                ;; THIS process) means the turn is done — don't re-stream it.
                terminal? (some #(contains? #{"turn.completed" "turn.failed" "turn.cancelled"}
                                            (get % "type"))
                                events)]

            (when (and (seq foreign) (not terminal?))
              ;; Claim the COMPLETE lines we just read, pinned to their
              ;; generation, so neither the tailer nor a later hydrate
              ;; re-delivers them.
              (set-tail! sid whole (head-of raw))
              ;; The turn this journal is about. `turn.started` is its first line,
              ;; but read the LAST foreign event when it isn't there, so ONE anchor
              ;; answers both "whose pid?" and "which turn_id?".
              (let [anchor (or (some #(when (= "turn.started" (get % "type")) %) foreign)
                               (last foreign))]
                (if (producer-alive? (get anchor "_pid"))
                  ;; Live sibling: mirror its in-flight turn into the registry.
                  (when-let [f' @deliver-fn]
                    (doseq [ev foreign]
                      (try
                        (f' sid (boolean (get ev "_store")) (dissoc ev "_producer" "_pid" "_store"))
                        (catch Throwable t
                          (tel/log! :debug
                                    ["gateway-bus: hydrate deliver failed" (ex-message t)])))))
                  ;; Orphan: producer process is gone. Reap it terminally.
                  (when-let [tid (get anchor "turn_id")]
                    ;; CAS-claim the reap BEFORE publishing: `publish!` lands the
                    ;; terminal ASYNCHRONOUSLY, so two hydrates inside that write
                    ;; window would each read `terminal? = false` and each emit
                    ;; its own `turn.failed` for the same turn.
                    (when (claim-reap! sid tid)
                      (let [term {"schema" 1
                                  "type" "turn.failed"
                                  "session_id" (str sid)
                                  "turn_id" tid
                                  "status" "interrupted"
                                  "error" "gateway producer exited before the turn finished"}]
                        ;; Durable + cross-process: appended (no truncate), so any
                        ;; process hydrating later sees `terminal?` and skips.
                        (publish! sid term {:store? true})
                        (when-let [f' @deliver-fn]
                          (try (f' sid true term)
                               (catch Throwable t
                                 (tel/log! :debug
                                           ["gateway-bus: orphan-reap deliver failed"
                                            (ex-message t)]))))))))))))))
    (catch Throwable t (tel/log! :debug ["gateway-bus: hydrate failed" (ex-message t)])))
  nil)

(defn- sweep!
  "Delete journals untouched for `RETAIN_MS` — the crashed / kill-9'd / restarted
   sessions `forget!` never got to clean, which otherwise pile up forever and get
   re-scanned by every `poll-once!`. A live session rewrites its journal each
   turn, so a stale mtime proves the producer is gone. Drops the swept file's
   tail offset (and lock) too. Never throws."
  []
  (try (let [dir
             (.toFile (events-dir))

             cutoff
             (- (util/now-ms) (long RETAIN_MS))]

         (when (.isDirectory dir)
           (doseq [^File f (.listFiles dir)]
             (let [n (.getName f)]
               (when (and (str/ends-with? n ".ndjson") (< (.lastModified f) cutoff) (.delete f))
                 (let [swept (subs n 0 (- (count n) (count ".ndjson")))]
                   (swap! tails dissoc swept)))))))
       (catch Throwable t (tel/log! :debug ["gateway-bus: sweep failed" (ex-message t)]))))

(defn- poll-once!
  "Drain the journals with a LOCAL consumer once. Returns true when any line was
   delivered, so the tailer can poll fast under load and back off when quiet.

   Fast path (production): when `set-relevant-sids-fn!` is wired, drain ONLY the
   sessions this process tracks by resolving each sid's journal file directly —
   no `.listFiles`, no stat of every sibling's journal on every poll. An idle
   daemon (no local consumers) therefore does zero directory work per poll, which
   was the dominant CPU/allocation cost of the tailer (a File per journal, twice
   a second, forever).

   Fallback: no sids-fn wired (tests) — scan the events dir and drain every
   journal whose sid passes `relevant-sid?`, exactly as before."
  []
  (try (if-let [sids-fn @relevant-sids-fn]
         (reduce (fn [busy sid]
                   (or (try (boolean (drain-file! sid (session-file sid)))
                            (catch Throwable t
                              (tel/log! :debug
                                        ["gateway-bus: drain failed" (str sid) (ex-message t)])
                              false))
                       busy))
                 false
                 (try (seq (sids-fn)) (catch Throwable _ nil)))
         (let [dir (.toFile (events-dir))]
           (if (.isDirectory dir)
             (reduce (fn [busy ^File f]
                       (let [sid (journal-sid (.getName f))]
                         (if (and sid (relevant-sid? sid))
                           (or (try (boolean (drain-file! sid f))
                                    (catch Throwable t
                                      (tel/log! :debug
                                                ["gateway-bus: drain failed" (.getName f)
                                                 (ex-message t)])
                                      false))
                               busy)
                           busy)))
                     false
                     (.listFiles dir))
             false)))
       (catch Throwable t (tel/log! :debug ["gateway-bus: poll failed" (ex-message t)]) false)))

(defn start!
  "Start the background tailer once. Idempotent."
  []
  (when (compare-and-set! tailer nil ::starting)
    (let [t (Thread.
              ^Runnable
              (fn []
                ;; On boot, skip whatever already sits in each journal so a
                ;; late-starting process doesn't replay a finished turn's
                ;; deltas; we only want the live tail from now on.
                (try (let [dir (.toFile (events-dir))]
                       (when (.isDirectory dir)
                         (doseq [^File f (.listFiles dir)]
                           (when (str/ends-with? (.getName f) ".ndjson")
                             (let [sid
                                   (subs (.getName f) 0 (- (count (.getName f)) (count ".ndjson")))]
                               (set-tail! sid (.length f) (journal-head f)))))))
                     (catch Throwable _ nil))
                ;; Poll fast while a sibling is streaming, then back off to
                ;; IDLE_POLL_MS once quiet so an idle daemon stays off the CPU.
                ;; Sweep orphaned journals ~once a minute (wall-clock) so the
                ;; poll set — and the disk — never grow without bound.
                (loop [quiet 0
                       last-sweep 0]

                  (when-not (Thread/interrupted)
                    (let [busy? (poll-once!)
                          now (util/now-ms)
                          last-sweep
                          (if (>= (- now last-sweep) (long SWEEP_MS)) (do (sweep!) now) last-sweep)
                          quiet (if busy? 0 (inc quiet))]

                      (try (Thread/sleep (long
                                           (if (>= quiet (long IDLE_AFTER)) IDLE_POLL_MS POLL_MS)))
                           (catch InterruptedException _ (.interrupt (Thread/currentThread))))
                      (recur quiet last-sweep)))))
              "gateway-bus-tailer")]
      (.setDaemon t true)
      (.start t)
      (reset! tailer t)))
  nil)
