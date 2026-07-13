(ns com.blockether.vis.internal.gateway.bus
  "Cross-process gateway event bus.

   The gateway's live event log + SSE fan-out (`gateway.state`) is a
   PROCESS-LOCAL in-memory registry: `append-event!` only reaches
   subscribers inside the SAME JVM. That is why a turn streaming in the
   TUI process is invisible to a web/Telegram process watching the SAME
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
            [taoensso.telemere :as tel])
  (:import [java.io File RandomAccessFile]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute FileAttribute]))

(def ^:private POLL_MS
  "Sleep between tail polls while a sibling process is actively streaming."
  100)
(def ^:private IDLE_POLL_MS
  "Sleep once the tail has gone quiet — the steady state, since the cross-process
   tailer only has work when a SIBLING vis process shares a session. Every poll
   still stats every journal, so backing off to 2×/s here (vs 17×/s before) is
   what keeps an otherwise-idle daemon off the CPU."
  500)
(def ^:private IDLE_AFTER
  "Consecutive quiet polls before backing off from `POLL_MS` to `IDLE_POLL_MS`."
  20)
(def ^:private MAX_FILE_BYTES (* 16 1024 1024))

(def ^:private RETAIN_MS
  "Age past which an untouched journal is presumed orphaned and swept: a live
   session rewrites (truncates + appends) its journal every turn, so a file whose
   mtime is a day stale cannot belong to a running turn."
  (* 24 60 60 1000))

(def ^:private SWEEP_MS
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

;; ---------------------------------------------------------------------------
;; Producer
;; ---------------------------------------------------------------------------

(declare start!)

(defn publish!
  "Append one locally-produced `event` to the shared journal for `sid`.
   `truncate?` (true on `turn.started`) resets the file first, bounding it
   to the current turn. Never throws."
  ([sid event] (publish! sid event {:store? true}))
  ([sid event {:keys [store? truncate?]}]
   (try
     ;; Lazily start the tailer on first publish so the native binary (whose
     ;; ns-load ran at BUILD time, where a started thread can't be baked into
     ;; the image heap) still gets a live consumer at RUNTIME. Idempotent.
     (start!)
     (ensure-dir!)
     (let [f
           (session-file sid)

           line
           (str (wire/json-str (assoc event
                                 :_producer producer-id
                                 :_pid producer-pid
                                 :_store (boolean store?)))
                "\n")]

       (locking f
         (with-open [raf (RandomAccessFile. f "rw")]
           (let [len (.length raf)]
             (cond truncate? (.setLength raf 0)
                   (> len MAX_FILE_BYTES) (.setLength raf 0)))
           (.seek raf (.length raf))
           (.write raf (.getBytes ^String line StandardCharsets/UTF_8)))))
     nil
     (catch Throwable t (tel/log! :debug ["gateway-bus: publish failed" (ex-message t)]) nil))))

(defn forget!
  "Drop a session's journal (on session close). Never throws."
  [sid]
  (try (.delete (session-file sid)) (catch Throwable _ nil))
  nil)

;; ---------------------------------------------------------------------------
;; Consumer (tailer)
;; ---------------------------------------------------------------------------

(defonce ^:private deliver-fn (atom nil))

(defn set-deliver-fn!
  "Register the fn the tailer calls for every FOREIGN event:
   `(f sid event store?)`. Wired by `gateway.state`."
  [f]
  (reset! deliver-fn f))

;; sid-str -> byte offset already consumed
(defonce ^:private offsets (atom {}))
(defonce ^:private tailer (atom nil))

(defn- deliver-line!
  [sid ^String line]
  (when-let [event (wire/parse-json line)]
    (when-not (= (:_producer event) producer-id)
      (when-let [f @deliver-fn]
        (let [store? (boolean (:_store event))
              clean (dissoc event :_producer :_pid :_store)]

          (try (f sid store? clean)
               (catch Throwable t
                 (tel/log! :debug ["gateway-bus: deliver failed" (ex-message t)]))))))))

(defn- drain-file!
  [^File f]
  (let [name
        (.getName f)

        sid
        (subs name 0 (- (count name) (count ".ndjson")))

        len
        (.length f)

        off0
        (get @offsets sid 0)

        ;; truncation/rotation: file shrank under our cursor -> rewind
        off
        (if (> off0 len) 0 off0)]

    (when (> len off)
      (with-open [raf (RandomAccessFile. f "r")]
        (.seek raf (long off))
        (let [remaining (- len off)
              buf (byte-array remaining)
              _ (.readFully raf buf)
              text (String. buf StandardCharsets/UTF_8)
              nl (.lastIndexOf text "\n")]

          (when (>= nl 0)
            (let [complete (subs text 0 nl)
                  consumed (count (.getBytes ^String (subs text 0 (inc nl))
                                             StandardCharsets/UTF_8))]

              (doseq [line (str/split-lines complete)]
                (when-not (str/blank? line) (deliver-line! sid line)))
              (swap! offsets assoc sid (+ off consumed))
              true)))))))

(defn journal-high-water-seq
  "Highest `:seq` persisted in `sid`'s journal file, or 0 when there is none.

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
             (map #(long (or (:seq %) 0)))
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

   A non-terminal journal has TWO causes, told apart by `:_pid` liveness: the
   producer is still alive (a real in-flight sibling turn — mirror it) OR the
   producer PROCESS is gone (a daemon crash/restart mid-turn). An orphaned turn
   will never emit a terminal, so resurrecting it pins this process's
   `:current-turn` to a dead turn — wedging the session queue (new sends pile up
   `queued`, nothing drains) and spinning every watcher forever. For an orphan we
   instead land a synthetic `turn.failed` so the queue drains, clients get
   closure, and no later hydrate replays it again.

   Rewinds this process's tail cursor to the file's current end first, so the
   background tailer won't re-deliver what we hand over here. Never throws."
  [sid]
  (try
    (when-let [f (session-file sid)]
      (when (.exists f)
        (let [len (.length f)
              events (->> (str/split-lines (slurp f))
                          (remove str/blank?)
                          (keep wire/parse-json))
              foreign (remove #(= (:_producer %) producer-id) events)
              ;; A terminal from ANYONE (a sibling, or a prior orphan-reap by
              ;; THIS process) means the turn is done — don't re-stream it.
              terminal? (some #(contains? #{"turn.completed" "turn.failed"} (:type %)) events)]

          (when (and (seq foreign) (not terminal?))
            ;; claim everything up to EOF so poll-once! won't re-deliver it
            (swap! offsets assoc (str sid) len)
            (let [started (some #(when (= "turn.started" (:type %)) %) foreign)]
              (if (producer-alive? (:_pid (or started (last foreign))))
                ;; Live sibling: mirror its in-flight turn into the registry.
                (when-let [f' @deliver-fn]
                  (doseq [ev foreign]
                    (try (f' sid (boolean (:_store ev)) (dissoc ev :_producer :_pid :_store))
                         (catch Throwable t
                           (tel/log! :debug
                                     ["gateway-bus: hydrate deliver failed" (ex-message t)])))))
                ;; Orphan: producer process is gone. Reap it terminally.
                (when-let [tid (:turn_id started)]
                  (let [term {:schema 1
                              :type "turn.failed"
                              :session_id (str sid)
                              :turn_id tid
                              :status "interrupted"
                              :error "gateway producer exited before the turn finished"}]
                    ;; Durable + cross-process: appended (no truncate), so any
                    ;; process hydrating later sees `terminal?` and skips.
                    (publish! sid term {:store? true})
                    (when-let [f' @deliver-fn]
                      (try (f' sid true term)
                           (catch Throwable t
                             (tel/log! :debug
                                       ["gateway-bus: orphan-reap deliver failed"
                                        (ex-message t)]))))))))))))
    (catch Throwable t (tel/log! :debug ["gateway-bus: hydrate failed" (ex-message t)])))
  nil)

(defn- sweep!
  "Delete journals untouched for `RETAIN_MS` — the crashed / kill-9'd / restarted
   sessions `forget!` never got to clean, which otherwise pile up forever and get
   re-scanned by every `poll-once!`. A live session rewrites its journal each
   turn, so a stale mtime proves the producer is gone. Drops the swept file's
   tail offset too. Never throws."
  []
  (try (let [dir
             (.toFile (events-dir))

             cutoff
             (- (System/currentTimeMillis) RETAIN_MS)]

         (when (.isDirectory dir)
           (doseq [^File f (.listFiles dir)]
             (let [n (.getName f)]
               (when (and (str/ends-with? n ".ndjson") (< (.lastModified f) cutoff) (.delete f))
                 (swap! offsets dissoc (subs n 0 (- (count n) (count ".ndjson")))))))))
       (catch Throwable t (tel/log! :debug ["gateway-bus: sweep failed" (ex-message t)]))))

(defn- poll-once!
  "Drain every journal once. Returns true when any line was delivered, so the
   tailer can poll fast under load and back off when the tail is quiet."
  []
  (try (let [dir (.toFile (events-dir))]
         (if (.isDirectory dir)
           (reduce (fn [busy ^File f]
                     (if (str/ends-with? (.getName f) ".ndjson")
                       (or (try (boolean (drain-file! f))
                                (catch Throwable t
                                  (tel/log! :debug
                                            ["gateway-bus: drain failed" (.getName f)
                                             (ex-message t)])
                                  false))
                           busy)
                       busy))
                   false
                   (.listFiles dir))
           false))
       (catch Throwable t (tel/log! :debug ["gateway-bus: poll failed" (ex-message t)]) false)))

(defn start!
  "Start the background tailer once. Idempotent."
  []
  (when (compare-and-set! tailer nil ::starting)
    (let [t (Thread. ^Runnable
                     (fn []
                       ;; On boot, skip whatever already sits in each journal so a
                       ;; late-starting process doesn't replay a finished turn's
                       ;; deltas; we only want the live tail from now on.
                       (try (let [dir (.toFile (events-dir))]
                              (when (.isDirectory dir)
                                (doseq [^File f (.listFiles dir)]
                                  (when (str/ends-with? (.getName f) ".ndjson")
                                    (let [n (.getName f)]
                                      (swap! offsets assoc
                                        (subs n 0 (- (count n) (count ".ndjson")))
                                        (.length f)))))))
                            (catch Throwable _ nil))
                       ;; Poll fast while a sibling is streaming, then back off to
                       ;; IDLE_POLL_MS once quiet so an idle daemon stays off the CPU.
                       ;; Sweep orphaned journals ~once a minute (wall-clock) so the
                       ;; poll set — and the disk — never grow without bound.
                       (loop [quiet 0
                              last-sweep 0]

                         (when-not (Thread/interrupted)
                           (let [busy? (poll-once!)
                                 now (System/currentTimeMillis)
                                 last-sweep
                                 (if (>= (- now last-sweep) SWEEP_MS) (do (sweep!) now) last-sweep)
                                 quiet (if busy? 0 (inc quiet))]

                             (try
                               (Thread/sleep (long (if (>= quiet IDLE_AFTER) IDLE_POLL_MS POLL_MS)))
                               (catch InterruptedException _ (.interrupt (Thread/currentThread))))
                             (recur quiet last-sweep)))))
                     "gateway-bus-tailer")]
      (.setDaemon t true)
      (.start t)
      (reset! tailer t)))
  nil)
