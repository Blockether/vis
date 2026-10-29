(ns com.blockether.vis.internal.view.sink
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
   to `view/normalize-patch`: every live vocabulary is a closed table read in
   either spelling."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.view.materializer :as materializer]
            [taoensso.telemere :as tel])
  (:import (java.io File)
           (java.nio.file Files)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(defn- views-dir
  "Where the records live. `~/.vis/gateway/live` is TAKEN — the bus keeps one
   turn-liveness marker per session there and LISTS that directory — so a view's
   stream gets a directory of its own rather than a subdirectory somebody else's
   listing has to learn to skip."
  ^File []
  (io/file (System/getProperty "user.home") ".vis" "gateway" "views"))

(defn view-file
  "The record of view `view-id`, kept under the session that opened it. A header
   view has no session, so its record is kept under `machine`."
  ^File [session-id view-id]
  (io/file (views-dir) (or (not-empty (str session-id)) "machine") (str view-id ".ndjson")))

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
    (.write writer ^String (wire/json-str (materializer/redact-presentation line)))
    (.write writer "\n"))
  file)

(defn open!
  "Start the record of `view` and return the file it is kept in. The whole
   declared view is the first line, so a reader that has the file needs nothing
   else to rebuild what the patches were applied to."
  ^File [view]
  (record-rail!)
  (append-line! (view-file (:session-id view) (:id view))
                {:kind :open :at (util/now-ms) :view view}))

(defn append!
  "Record one ACCEPTED patch. Called after the materializer took it and before
   any surface is told, so the file is never behind a screen."
  ^File [^File file patch]
  (append-line! file {:kind :patch :at (util/now-ms) :patch patch}))

(defn close!
  "Seal the record with the verdict the model reads."
  ^File [^File file result]
  (append-line! file {:kind :close :at (util/now-ms) :result result}))

(def ^:private archive-line
  "The line that marks an app archived. It follows the close line, and a verdict
   reader skips it."
  "{\"kind\":\"archive\"}")

(defn archive!
  "Mark the app recorded in `file` archived. Call it after the close line."
  ^File [^File file]
  (io/make-parents file)
  (with-open [writer (io/writer file :append true)]
    (.write writer ^String archive-line)
    (.write writer "\n"))
  file)

;; --- Compaction of an app record ---

(def compact-floor-bytes
  "The smallest app record that is compacted. A record below it keeps every line."
  262144)

(declare log-range)

(defn- full-logs
  "`nodes` with each log node carrying every line of its record in `file`, not only
   its live window."
  [^File file nodes]
  (mapv (fn [node]
          (let [log?
                (= "log"
                   (some-> (:type node)
                           name))

                full
                (when log? (log-range file (:id node) 0 Long/MAX_VALUE))]

            (cond-> node
              log?
              (-> (assoc :lines (:lines full)
                         :total-lines (count (:lines full)))
                  (cond->
                    (:line-tones full)
                    (assoc :line-tones (:line-tones full))

                    (nil? (:line-tones full))
                    (dissoc :line-tones)))

              (seq (:children node))
              (update :children #(full-logs file %)))))
        (or nodes [])))

(defn compact!
  "Replace the record of an open app in `file` with one open line, and answer the
   new size in bytes.

   An app records its picture, not its data: a game patches on every tick, and
   only the current picture is worth a restart. The open line holds `view`, the
   current picture, with every line of each log, so a log page reads the same
   lines. `node-seqs` maps each node id to the seq of its last write; it is kept
   as pairs, so a node id is never read as a key. The new file replaces the old
   one atomically, so a crash keeps one of them whole, and a second call keeps the
   same picture and lines. Call it under the lock of the view."
  [^File file view node-seqs]
  (let [line
        {:kind :open
         :at (util/now-ms)
         :is-compacted true
         :view (update view :nodes #(full-logs file %))
         :node-seqs (mapv (fn [[node-id node-seq]]
                            [node-id node-seq])
                          node-seqs)}

        tmp
        (io/file (.getParentFile file) (str "." (.getName file) ".compact"))]

    (spit tmp (str (wire/json-str (materializer/redact-presentation line)) "\n"))
    (Files/move (.toPath tmp)
                (.toPath file)
                (into-array java.nio.file.CopyOption
                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                             java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
    (.length file)))

(defn fold-lines
  "Fold `f` over each line of the record in `file`, decoded like [[read-range]].
   One streamed pass; answers `init` for a record that nothing wrote. A line that
   does not parse, such as a line that a crash cut, is logged and skipped."
  [^File file f init]
  (if-not (and file (.isFile file))
    init
    (with-open [reader (io/reader file)]
      (reduce (fn [acc line]
                (if-let [decoded (try (wire/->engine (json/read-json line))
                                      (catch Exception e
                                        (tel/log!
                                          {:level :warn
                                           :id ::record-line-unreadable
                                           :data {:file (str file) :error (ex-message e)}
                                           :msg "Vis skipped an unreadable line of a view record"})
                                        nil))]
                  (f acc decoded)
                  acc))
              init
              (line-seq reader)))))

(defn find-view-file
  "The record of view `view-id` in any session directory, or nil."
  ^File [view-id]
  (when (re-matches record-segment (str view-id))
    (some (fn [^File dir]
            (let [file (io/file dir (str view-id ".ndjson"))]
              (when (.isFile file) file)))
          (.listFiles (views-dir)))))

(defn- app-index-file
  "The list of the apps that session `session-id` opened, one NDJSON line each."
  ^File [session-id]
  (io/file (views-dir) (str session-id) "apps.ndjson"))

(defn index-app!
  "Add one app to the list of session `session-id`. `entry` holds `:view-id` and
   `:created-at`, and `:key` and `:extension` when the app has them."
  [session-id entry]
  (append-line! (app-index-file session-id) entry))

(defn app-index
  "The apps that session `session-id` opened, oldest first. A line that does not
   parse is logged and skipped."
  [session-id]
  (let [file (app-index-file session-id)]
    (if-not (.isFile file)
      []
      (with-open [reader (io/reader file)]
        (into []
              (keep (fn [line]
                      (try (wire/->engine (json/read-json line))
                           (catch Exception e
                             (tel/log! {:level :warn
                                        :id ::app-index-line-unreadable
                                        :data {:session-id (str session-id) :error (ex-message e)}
                                        :msg "Vis skipped an unreadable line of an app list"})
                             nil))))
              (line-seq reader))))))

(def page-note-limit
  "How many HTML pages of one session keep a note for the model. A newer note
   drops the oldest page."
  8)

(defonce ^:private page-notes-lock (Object.))

(defn- page-notes-file
  "The notes that the HTML pages of session `session-id` keep for the model."
  ^File [session-id]
  (io/file (views-dir) (str session-id) "page-notes.json"))

(defn page-notes
  "The notes of the HTML pages of session `session-id`, oldest first. Each entry
   holds `:id`, `:title` and `:agent-note`. A file that does not parse is logged
   and reads as no notes."
  [session-id]
  (let [file (page-notes-file session-id)]
    (if-not (.isFile file)
      []
      (try (let [notes (wire/->engine (json/read-json (slurp file)))]
             (if (sequential? notes) (filterv map? notes) []))
           (catch Exception e
             (tel/log! {:level :warn
                        :id ::page-notes-unreadable
                        :data {:session-id (str session-id) :file (str file) :error (ex-message e)}
                        :msg "Vis skipped the unreadable page notes of a session"})
             [])))))

(defn note-page!
  "Keep `note`, a map of `:id`, `:title` and `:agent-note`, as the note of one
   HTML page of session `session-id` for the model. A blank `:agent-note` removes
   the note of the page. Answers the notes of the session, oldest first.

   The page with the newest note goes last, and only the newest
   [[page-note-limit]] pages keep a note. The file is replaced atomically, so a
   crash keeps the older file whole, and the same call twice gives the same file."
  [session-id {:keys [id title agent-note]}]
  (locking page-notes-lock
    (let [file
          (page-notes-file session-id)

          others
          (filterv #(not= id (:id %)) (page-notes session-id))

          notes
          (if (str/blank? agent-note)
            others
            (vec (take-last page-note-limit
                            (conj others {:id id :title title :agent-note (str/trim agent-note)}))))

          tmp
          (io/file (.getParentFile file) (str "page-notes." (util/now-ms) ".tmp"))]

      (io/make-parents file)
      (spit tmp (wire/json-str notes))
      (Files/move (.toPath tmp)
                  (.toPath file)
                  (into-array java.nio.file.CopyOption
                              [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                               java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      notes)))

(defn- owned-file
  "The last pictures of every extension-owned view, one JSON array."
  ^File []
  (io/file (views-dir) "owned.json"))

(defn save-owned!
  "Replace the saved pictures of owned views with `views`, atomically. A crash
   between two saves keeps the older file whole."
  [views]
  (let [file
        (owned-file)

        tmp
        (io/file (.getParentFile file) (str "owned." (util/now-ms) ".tmp"))]

    (io/make-parents file)
    (spit tmp (wire/json-str (mapv materializer/redact-presentation views)))
    (Files/move (.toPath tmp)
                (.toPath file)
                (into-array java.nio.file.CopyOption
                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                             java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
    file))

(defn load-owned
  "The saved pictures of owned views as wire maps, or [] when there are none or
   the file does not parse."
  []
  (let [file (owned-file)]
    (if-not (.isFile file)
      []
      (try (let [views (json/read-json (slurp file) :key-fn identity)]
             (if (sequential? views) (filterv map? views) []))
           (catch Exception _ [])))))

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
      (let [last-line (reduce (fn [kept line]
                                (if (= archive-line line) kept line))
                              nil
                              (line-seq reader))
            record (some-> last-line
                           json/read-json
                           wire/->engine)]

        (when (= "close" (:kind record)) (:result record))))))

(defn- find-log-node
  "Find log node `node-id` at any depth of a declared layout."
  [nodes node-id]
  (first (filter #(and (= node-id (str (:id %))) (= "log" (str (:type %))))
                 (mapcat #(tree-seq :children :children %) nodes))))

(def ^:private empty-log
  "Counters and one retained result page. Line numbers are one-based."
  {:total 0 :matched 0 :lines [] :line-numbers []})

(defn- take-window
  "Count every line and match, retaining only matches inside `[from end)`."
  [state lines tones from end ^String query]
  (reduce (fn [acc [text tone]]
            (let [line-number
                  (inc (long (:total acc)))

                  at
                  (long (:matched acc))

                  matches?
                  (.contains (.toLowerCase ^String text Locale/ROOT) query)]

              (cond-> (assoc acc :total line-number)
                matches?
                (update :matched inc)

                (and matches? (<= (long from) at) (< at (long end)))
                (-> (cond->
                      (or tone (contains? acc :line-tones))
                      (assoc :line-tones
                        (conj (or (:line-tones acc) (vec (repeat (count (:lines acc)) nil))) tone)))
                    (update :lines conj text)
                    (update :line-numbers conj line-number)))))
          state
          (map vector (or lines []) (concat tones (repeat nil)))))

(defn- fold-record
  "Fold declarations, appends and resets into one bounded log result page."
  [state entry node-id from end query]
  (case (str (:kind entry))
    "open"
    (if-let [node (find-log-node (:nodes (:view entry)) node-id)]
      (take-window state (:lines node) (:line-tones node) from end query)
      state)

    "patch"
    (reduce
      (fn [acc op]
        (let [op-name (str (:op op))]
          (cond (= "add-node" op-name)
                (if-let [node (find-log-node [(:node-spec op)] node-id)]
                  (take-window empty-log (:lines node) (:line-tones node) from end query)
                  acc)
                (not= node-id (str (:node-id op))) acc
                (= "append" op-name)
                (take-window acc (:lines op) (repeat (count (:lines op)) (:tone op)) from end query)
                (contains? #{"clear" "remove-node"} op-name) empty-log
                :else acc)))
      state
      (:ops (:patch entry)))

    state))

(defn log-range
  "Read a bounded page from a log's record, including output outside its live window.
   Optional `query` is a literal, case-insensitive substring (empty matches all).
   `from` is a zero-based MATCH offset, `:line-numbers` are original one-based
   positions, `:matched` counts matches and `:total` counts all retained lines.
   Clear/remove resets both counts. Reads open and closed records in one streamed
   pass, retaining only the requested page."
  [^File file node-id from limit & [query]]
  (let [node-id
        (str node-id)

        from
        (max 0 (long from))

        end
        (+ from (max 0 (long limit)))

        query
        (.toLowerCase (str query) Locale/ROOT)]

    (assoc (if-not (and file (.isFile file))
             empty-log
             (with-open [reader (io/reader file)]
               (reduce
                 (fn [state line]
                   (fold-record state (wire/->engine (json/read-json line)) node-id from end query))
                 empty-log
                 (line-seq reader))))
      :node-id node-id
      :from from)))
