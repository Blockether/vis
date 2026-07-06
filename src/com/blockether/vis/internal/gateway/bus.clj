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
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.gateway.wire :as wire]
   [taoensso.telemere :as tel])
  (:import
   [java.io File RandomAccessFile]
   [java.nio.charset StandardCharsets]
   [java.nio.file Files LinkOption Path]
   [java.nio.file.attribute FileAttribute]))

(def ^:private POLL_MS 60)
(def ^:private MAX_FILE_BYTES (* 16 1024 1024))

(defonce ^{:doc "Stable per-process id: foreign events carry a different one."}
  producer-id (str (java.util.UUID/randomUUID)))

(defn- events-dir ^Path []
  (Path/of (System/getProperty "user.home")
    (into-array String [".vis" "gateway" "events"])))

(defn- session-file ^File [sid]
  (.toFile (.resolve (events-dir) (str sid ".ndjson"))))

(defn- ensure-dir! []
  (let [dir (events-dir)]
    (when-not (Files/exists dir (make-array LinkOption 0))
      (Files/createDirectories dir (make-array FileAttribute 0)))
    dir))

;; ---------------------------------------------------------------------------
;; Producer
;; ---------------------------------------------------------------------------

(defn publish!
  "Append one locally-produced `event` to the shared journal for `sid`.
   `truncate?` (true on `turn.started`) resets the file first, bounding it
   to the current turn. Never throws."
  ([sid event] (publish! sid event {:store? true}))
  ([sid event {:keys [store? truncate?]}]
   (try
     (ensure-dir!)
     (let [f    (session-file sid)
           line (str (wire/json-str (assoc event
                                      :_producer producer-id
                                      :_store (boolean store?)))
                  "\n")]
       (locking f
         (with-open [raf (RandomAccessFile. f "rw")]
           (let [len (.length raf)]
             (cond
               truncate?               (.setLength raf 0)
               (> len MAX_FILE_BYTES)  (.setLength raf 0)))
           (.seek raf (.length raf))
           (.write raf (.getBytes ^String line StandardCharsets/UTF_8)))))
     nil
     (catch Throwable t
       (tel/log! :debug ["gateway-bus: publish failed" (ex-message t)])
       nil))))

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

(defn- deliver-line! [sid ^String line]
  (when-let [event (wire/parse-json line)]
    (when-not (= (:_producer event) producer-id)
      (when-let [f @deliver-fn]
        (let [store? (boolean (:_store event))
              clean  (dissoc event :_producer :_store)]
          (try (f sid store? clean)
            (catch Throwable t
              (tel/log! :debug ["gateway-bus: deliver failed" (ex-message t)]))))))))

(defn- drain-file! [^File f]
  (let [name  (.getName f)
        sid   (subs name 0 (- (count name) (count ".ndjson")))
        len   (.length f)
        off0  (get @offsets sid 0)
        ;; truncation/rotation: file shrank under our cursor -> rewind
        off   (if (> off0 len) 0 off0)]
    (when (> len off)
      (with-open [raf (RandomAccessFile. f "r")]
        (.seek raf (long off))
        (let [remaining (- len off)
              buf       (byte-array remaining)
              _         (.readFully raf buf)
              text      (String. buf StandardCharsets/UTF_8)
              nl        (.lastIndexOf text "\n")]
          (when (>= nl 0)
            (let [complete (subs text 0 nl)
                  consumed (count (.getBytes ^String (subs text 0 (inc nl))
                                    StandardCharsets/UTF_8))]
              (doseq [line (str/split-lines complete)]
                (when-not (str/blank? line)
                  (deliver-line! sid line)))
              (swap! offsets assoc sid (+ off consumed)))))))))

(defn- poll-once! []
  (try
    (let [dir (.toFile (events-dir))]
      (when (.isDirectory dir)
        (doseq [^File f (.listFiles dir)]
          (when (str/ends-with? (.getName f) ".ndjson")
            (try (drain-file! f)
              (catch Throwable t
                (tel/log! :debug ["gateway-bus: drain failed" (.getName f) (ex-message t)])))))))
    (catch Throwable t
      (tel/log! :debug ["gateway-bus: poll failed" (ex-message t)]))))

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
                (try
                  (let [dir (.toFile (events-dir))]
                    (when (.isDirectory dir)
                      (doseq [^File f (.listFiles dir)]
                        (when (str/ends-with? (.getName f) ".ndjson")
                          (let [n (.getName f)]
                            (swap! offsets assoc
                              (subs n 0 (- (count n) (count ".ndjson")))
                              (.length f)))))))
                  (catch Throwable _ nil))
                (while (not (Thread/interrupted))
                  (poll-once!)
                  (try (Thread/sleep (long POLL_MS))
                    (catch InterruptedException _
                      (.interrupt (Thread/currentThread))))))
              "gateway-bus-tailer")]
      (.setDaemon t true)
      (.start t)
      (reset! tailer t)))
  nil)
