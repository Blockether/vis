(ns com.blockether.vis.internal.human-input.live-sink
  "The STORE OF RECORD of one live view: an append-only NDJSON file per view.

   A live view is a STREAM, and nothing else in this process keeps one. The
   channel bus is a ring of 2000 events and the session journal is truncated per
   turn and past 16 MB, so a log that ran for twenty minutes would already be
   gone by the time the human scrolls back to its beginning. Here every line the
   engine ACCEPTED is on disk, in the order it accepted them.

   One file, three kinds of line: the OPEN line is the view as it was declared,
   one PATCH line per accepted patch, and the CLOSE line is the verdict. A patch
   is appended BEFORE it is published, so a crash keeps everything the engine
   accepted rather than everything a surface managed to paint, and the file is
   opened in APPEND mode, so a resumed process never truncates a view it did not
   open.

   Lines are wire JSON, which is what lets the file BE the artifact instead of a
   re-encoded copy of one, and what lets a reader hand a patch line straight back
   to `human-input/normalize-patch`: every live vocabulary is a closed table read
   in either spelling."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachment-storage :as attachment-storage]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import (java.io File)
           (java.nio.file Files)))

(set! *warn-on-reflection* true)

(defn- views-dir
  "Where the records live. `~/.vis/gateway/live` is TAKEN — the bus keeps one
   turn-liveness marker per session there and LISTS that directory — so a view's
   stream gets a directory of its own rather than a subdirectory somebody else's
   listing has to learn to skip."
  ^File []
  (io/file (System/getProperty "user.home") ".vis" "gateway" "views"))

(defn view-file
  "The record of view `view-id`, kept under the session that opened it."
  ^File [session-id view-id]
  (io/file (views-dir) (str session-id) (str view-id ".ndjson")))

(def ^:private record-scheme
  "URI scheme of a live record. Owned by [[record-backend]], which is the only thing
   that reads one back."
  "vis-live")

(def ^:private record-segment
  "What a session or view id may look like inside a [[record-uri]]. A segment with a
   separator or a `..` in it never reaches [[view-file]]."
  #"[A-Za-z0-9._-]+")

(defn record-uri
  "Where the artifact of a settled view POINTS: `vis-live://<session-id>/<view-id>`.

   Addressed by IDENTITY, not by path. A `file://` URI would put an absolute path of
   the machine that wrote it into a row that outlives it, and would only ever resolve
   for an operator who happened to register a file backend — so the artifact of a view
   would read back as a 404 on the very runs it exists for. This scheme is owned here,
   so the bytes come back through [[view-file]] wherever the records now live."
  [session-id view-id]
  (str record-scheme "://" session-id "/" view-id))

(defn- uri-file
  "The record ONE [[record-uri]] names, or nil for anything that is not one: another
   scheme, a missing half, or a segment that is not a plain id."
  ^File [uri]
  (let [prefix
        (str record-scheme "://")

        s
        (str uri)]

    (when (str/starts-with? s prefix)
      (let [[session-id view-id extra] (str/split (subs s (count prefix)) #"/")]
        (when (and (nil? extra)
                   (re-matches record-segment (str session-id))
                   (re-matches record-segment (str view-id)))
          (view-file session-id view-id))))))

(def ^:private record-backend
  "The storage rail that reads a live record back. The artifact a settled view files
   carries no bytes past the inline floor — it carries a [[record-uri]] — and this is
   what turns that address into the bytes `GET /v1/sessions/:sid/iterations/:iid/
   attachments/:idx` serves.

   READ-ONLY BY DESIGN: `:storage/offload?` is false and the PUT declines, so having
   this rail registered never moves anybody else's artifact off the database. The sink
   is the only writer this file has ever had."
  {:storage/id :vis-live-record
   :storage/scheme record-scheme
   :storage/priority -1
   :storage/offload? (constantly false)
   :storage/put-fn (fn [_]
                     nil)
   :storage/get-fn (fn [uri]
                     (when-let [^File file (uri-file uri)]
                       (when (.isFile file) (Files/readAllBytes (.toPath file)))))})

(defn ^:private record-rail!
  "Register [[record-backend]]. Idempotent by id, and called from BOTH ends: on LOAD,
   because the process that serves the byte endpoint for a record written weeks ago
   never opened a view of its own, and from [[open!]], so a rail something else
   deregistered is back before the record that needs it exists."
  []
  (attachment-storage/register-backend! record-backend))

(record-rail!)
(defn- append-line!
  "One line, flushed and closed. A live view writes rarely (a patch is a human's
   heartbeat, not a byte stream), and a handle held open for the length of a CI
   run is a handle leaked when the run is killed."
  ^File [^File file line]
  (io/make-parents file)
  (with-open [writer (io/writer file :append true)]
    (.write writer ^String (wire/json-str line))
    (.write writer "\n"))
  file)

(defn open!
  "Start the record of `view` and return the file it is kept in. The whole
   declared view is the first line, so a reader that has the file needs nothing
   else to rebuild what the patches were applied to."
  ^File [view]
  (record-rail!)
  (append-line! (view-file (:session-id view) (:id view))
                {:kind :open :at (System/currentTimeMillis) :view view}))

(defn append!
  "Record one ACCEPTED patch. Called after the materializer took it and before
   any surface is told, so the file is never behind a screen."
  ^File [^File file patch]
  (append-line! file {:kind :patch :at (System/currentTimeMillis) :patch patch}))

(defn close!
  "Seal the record with the verdict the model reads."
  ^File [^File file result]
  (append-line! file {:kind :close :at (System/currentTimeMillis) :result result}))

(defn stats
  "How much of the run the record in `file` holds: `:size` bytes and `:line-count`
   NDJSON lines. `{:size 0 :line-count 0}` for a record nothing ever wrote.

   Read at CLOSE, BEFORE the trailer, so what it counts is the run — the declared
   view and every accepted patch — and not the verdict that seals it. One streamed
   pass; the log itself never comes into memory, which is the whole reason the
   artifact addresses this file instead of carrying it."
  [^File file]
  (if-not (and file (.isFile file))
    {:size 0 :line-count 0}
    (with-open [reader (io/reader file)]
      {:size (.length file)
       :line-count (reduce (fn [n _]
                             (inc (long n)))
                           0
                           (line-seq reader))})))

(defn read-range
  "`limit` lines of the record from 0-based `from`, each decoded into the
   engine's keyword-keyed shape.

   A range past the end is an empty vector rather than a refusal: a surface
   asking for scrollback a crash never wrote is asking an honest question, and a
   view that is still open has no last line to bound the ask with."
  [^File file from limit]
  (if-not (and file (.isFile file))
    []
    (with-open [reader (io/reader file)]
      (into []
            (comp (drop (max 0 (long from)))
                  (take (max 0 (long limit)))
                  (map (fn [line]
                         (wire/->engine (json/read-json line)))))
            (line-seq reader)))))

(defn verdict
  "The verdict the record in `file` ENDS with, or nil while the view is still
   open.

   The registry drops a view the moment it closes, so this file is the only place
   left that knows HOW it ended. An extension that pushes into a view the human
   interrupted reads its reason here rather than being told merely that the view
   is gone — and it costs one streamed pass, never the whole log in memory."
  [^File file]
  (when (and file (.isFile file))
    (with-open [reader (io/reader file)]
      (let [last-line (reduce (fn [_ line]
                                line)
                              nil
                              (line-seq reader))
            record (some-> last-line
                           json/read-json
                           wire/->engine)]

        (when (= "close" (:kind record)) (:result record))))))

(defn- log-node?
  "True when `node` is the declaration of log node `node-id`."
  [node node-id]
  (and (= node-id (str (:id node))) (= "log" (str (:type node)))))

(def ^:private empty-log "A log node with nothing recorded against it yet." {:total 0 :lines []})

(defn- take-window
  "Count `lines` into `state` and keep only the ones inside `[from end)`. The
   window is the only thing held in memory, so a 100 000-line record costs the
   asked-for page and a counter."
  [state lines from end]
  (reduce (fn [acc text]
            (let [at (long (:total acc))]
              (cond-> (update acc :total inc)
                (and (>= at (long from)) (< at (long end)))
                (update :lines conj text))))
          state
          (or lines [])))

(defn- fold-record
  "Fold one record line into what log node `node-id` holds. Only three things
   ever touch a log: it is declared (in the view, or by `add-node`), lines are
   appended to it, or it is emptied — `clear` and `remove-node` both start the
   count again, exactly as the materializer does."
  [state entry node-id from end]
  (case (str (:kind entry))
    "open"
    (if-let [node (first (filter #(log-node? % node-id) (:nodes (:view entry))))]
      (take-window state (:lines node) from end)
      state)

    "patch"
    (reduce (fn [acc op]
              (let [op-name (str (:op op))]
                (cond (= "add-node" op-name)
                      (if (log-node? (:node-spec op) node-id)
                        (take-window empty-log (:lines (:node-spec op)) from end)
                        acc)
                      (not= node-id (str (:node-id op))) acc
                      (= "append" op-name) (take-window acc (:lines op) from end)
                      (contains? #{"clear" "remove-node"} op-name) empty-log
                      :else acc)))
            state
            (:ops (:patch entry)))

    state))

(defn log-range
  "`limit` lines of log node `node-id`, from 0-based `from`, as the RECORD holds
   them — plus `:total`, every line that node ever accepted.

   The picture a surface paints carries only the node's WINDOW (`:window-lines`,
   2000 by default), so this is the one way back to output that scrolled out of
   it: a phone that joined an hour into a build, or one whose patches were
   evicted from the gateway's reconnect ring, pages the earlier lines from here
   instead of being told the run has no history. One streamed pass, and only the
   asked-for page is held."
  [^File file node-id from limit]
  (let [node-id
        (str node-id)

        from
        (max 0 (long from))

        end
        (+ from (max 0 (long limit)))]

    (if-not (and file (.isFile file))
      (assoc empty-log
        :node-id node-id
        :from from)
      (with-open [reader (io/reader file)]
        (-> (reduce (fn [state line]
                      (fold-record state (wire/->engine (json/read-json line)) node-id from end))
                    empty-log
                    (line-seq reader))
            (assoc :node-id node-id
                   :from from))))))
