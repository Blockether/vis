(ns com.blockether.vis.internal.shell-log
  "The output of ONE background shell, stored as a FILE and read by BYTE OFFSET.

   The file is the STORAGE and the in-memory ring buffer is only a VIEW. A ring
   is a display convenience: it answers \"what is on screen now\", and the moment a
   command prints more than the ring holds, the head is gone before the first
   poll and no sequence of reads can recover it. That is the whole reported bug,
   and it is a storage bug, so the fix is storage: every byte the pump reads is
   appended to `~/.vis/logs/shell/<session>/<id>.log`, and a read names the byte
   it starts at.

   A chunk is the paging contract for a growing file, key for key: give an
   `offset`, get the bytes and the `next-offset` to continue from. Feeding
   `next-offset` back in a loop yields the WHOLE stream with no overlap and no
   gap, which is why there is no `dropped` count anywhere in this namespace —
   nothing is dropped, so nothing has to be reported as lost.

   `is-eof` means \"you have read everything WRITTEN so far\", never \"the command
   finished\": the process's own status belongs to its handle, not to a read of
   its log. `is-truncated` is a cap on THIS read alone and never on the file.

   The log is PERSISTENT and belongs to the SESSION: it outlives the process's
   exit, a daemon restart, and the turn that started the command. It dies with
   the session — [[delete-session-logs!]] runs where the session record is
   deleted, and the DB index row is scoped to the session soul, so the database
   cascade retires it in the same breath.

   Bytes on disk, index in the DB. The log never becomes a row: it is an
   append-only stream read by offset, and sqlite would turn every pump flush into
   a blob rewrite and every cursor read into a substring over that blob. What
   the DB carries is the ROW that makes a log FINDABLE without holding a
   handle — the command, the path, the start/end and the exit — on the
   `extension_aggregate` sidecar rail under [[index-extension-id]]."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.persistance :as persistance])
  (:import (java.io File InputStream OutputStream RandomAccessFile)
           (java.nio.charset StandardCharsets)))

;; =============================================================================
;; Data
;; =============================================================================

(s/def ::id (s/and string? seq))
(s/def ::offset (s/int-in 0 Long/MAX_VALUE))
(s/def ::next-offset ::offset)
(s/def ::text string?)
(s/def ::is-eof boolean?)
(s/def ::is-truncated boolean?)

(s/def ::log-chunk
  (s/and (s/keys :req-un [::id ::offset ::next-offset ::text ::is-eof] :opt-un [::is-truncated])
         #(>= (long (:next-offset %)) (long (:offset %)))))

(def default-chunk-bytes
  "Bytes ONE read returns when the caller named no limit. A window, not the
   file: the rest is one more read away at `next-offset`."
  16384)

(def max-chunk-bytes
  "Ceiling on ONE read, so a caller cannot bill a 40 MiB build log to a single
   context window by asking for it."
  262144)

;; =============================================================================
;; Where the bytes live
;; =============================================================================

(defn- vis-home ^File [] (io/file (System/getProperty "user.home") ".vis"))

(defn- id-digest
  "Eight hex chars of SHA-1 over the RAW id — enough to separate two ids that
   sanitize or truncate to the same segment."
  [s]
  (let
    [d (.digest (java.security.MessageDigest/getInstance "SHA-1")
                (.getBytes (str s) java.nio.charset.StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" %) (take 4 d)))))

(defn- safe-name
  "One path segment from an arbitrary session or shell id: everything outside
   `[A-Za-z0-9._-]` becomes `_`, so a handle named `../../etc` names a file
   inside the log directory and nowhere else.

   The mapping is INJECTIVE. Sanitizing alone is not: `a/b` and `a_b` both became
   `a_b`, so two live shells shared one log file and the second `open!` truncated
   the first one's output — each handle then reported the other's bytes. Whenever
   the segment is not the id verbatim, a digest of the RAW id is appended, so a
   distinct id always names a distinct file."
  [s]
  (let
    [raw
     (str s)

     cleaned
     (str/replace raw #"[^A-Za-z0-9._-]" "_")

     cleaned
     (if (str/blank? cleaned) "_" cleaned)

     cleaned
     (subs cleaned 0 (min 100 (count cleaned)))]

    (if (= raw cleaned) cleaned (str cleaned "-" (id-digest raw)))))

(defn session-dir
  "Directory holding every shell log of ONE session."
  ^File [session]
  (io/file (vis-home) "logs" "shell" (safe-name session)))

(defn log-file
  "The log file of shell `id` in `session`. Deterministic, so the bytes are
   reachable with `cat` and `grep` like any other file."
  ^File [session id]
  (io/file (session-dir session) (str (safe-name id) ".log")))

(defn open!
  "Create (truncating) the log file for `session`/`id` and return the sink the
   pump writes through: `{:path :out}`. Truncating is the point — a new spawn
   under an id IS a new shell, and its log starts at offset 0."
  [session id]
  (let [f (log-file session id)]
    (io/make-parents f)
    {:path (.getPath f) :out (java.io.BufferedOutputStream. (java.io.FileOutputStream. f false))}))

(defn tee
  "Wrap `in` so every byte READ is also written to `sink`. The pump keeps its
   character-level line splitting for the ring view while the file receives the
   stream verbatim, so the two can never disagree about what the shell printed."
  ^InputStream [^InputStream in sink]
  (let [^OutputStream out (:out sink)]
    (proxy [java.io.FilterInputStream] [in]
      (read
        ([]
         (let [c (.read in)]
           (when-not (neg? c) (try (.write out c) (.flush out) (catch Throwable _ nil)))
           c))
        ([b]
         (let
           [^bytes b b
            n (.read in b 0 (alength b))]

           (when (pos? n) (try (.write out b 0 n) (.flush out) (catch Throwable _ nil)))
           n))
        ([b off len]
         (let
           [^bytes b b
            n (.read in b (int off) (int len))]

           (when (pos? n) (try (.write out b (int off) n) (.flush out) (catch Throwable _ nil)))
           n))))))

(defn close!
  "Flush and close a sink. Idempotent and never throws — a log that cannot be
   closed must not take the shell's teardown down with it."
  [sink]
  (when-let [^OutputStream out (:out sink)]
    (try (.flush out) (catch Throwable _ nil))
    (try (.close out) (catch Throwable _ nil)))
  nil)

(defn delete-session-logs!
  "Delete every shell log of `session`. The retention rule: a log dies with the
   session that produced it, and with nothing else — a build log is large and
   boring the day after, but only the session knows when that day came."
  [session]
  (let [dir (session-dir session)]
    (when (.isDirectory dir)
      (doseq [^File f (.listFiles dir)]
        (try (.delete f) (catch Throwable _ nil)))
      (try (.delete dir) (catch Throwable _ nil)))
    nil))

;; =============================================================================
;; Reading a chunk
;; =============================================================================

(defn- continuation?
  "True when byte `b` continues a multi-byte UTF-8 sequence (`10xxxxxx`)."
  [^long b]
  (= 0x80 (bit-and b 0xC0)))

(defn- sequence-length
  "Bytes the UTF-8 sequence starting with `b` occupies."
  ^long [^long b]
  (cond (< b 0x80) 1
        (= 0xC0 (bit-and b 0xE0)) 2
        (= 0xE0 (bit-and b 0xF0)) 3
        (= 0xF0 (bit-and b 0xF8)) 4
        :else 1))

(defn- partial-head
  "Bytes to skip at the START of `buf` because the offset landed inside a
   character. Only a defaulted (tail) read can do that; a caller feeding back
   `next-offset` always lands on a boundary."
  ^long [^bytes buf ^long n]
  (loop [i 0]
    (if (and (< i n) (< i 3) (continuation? (bit-and (aget buf i) 0xFF))) (recur (inc i)) i)))

(defn- partial-tail
  "Bytes to drop from the END of `buf` because the cap cut a character in half.
   They are not lost: `next-offset` stops before them and the next read begins
   with the whole character."
  ^long [^bytes buf ^long n]
  (loop [i (dec n)]
    (cond (neg? i) 0
          (< (- n i) 1) 0
          :else (let [b (bit-and (aget buf i) 0xFF)]
                  (cond (continuation? b) (if (< (- n i) 4) (recur (dec i)) 0)
                        (< (- n i) (sequence-length b)) (- n i)
                        :else 0)))))

(defn- read-bytes
  ^bytes [^File f ^long off ^long n]
  (let [buf (byte-array n)]
    (with-open [raf (RandomAccessFile. f "r")]
      (.seek raf off)
      (loop [read 0]
        (if (>= read n)
          buf
          (let [got (.read raf buf (int read) (int (- n read)))]
            (if (neg? got) (java.util.Arrays/copyOf buf (int read)) (recur (+ read got)))))))))

(defn read-chunk
  "Read shell `id`'s log `file` from `:offset` and answer a [[::log-chunk]].

   With no `:offset` the read is the TAIL — the last [[default-chunk-bytes]] —
   because that is what someone watching a live command wants, and the head is
   one `{:offset 0}` away rather than gone. With an offset it reads FORWARD from
   exactly that byte, which is what a loop feeding `next-offset` does.

   A missing file is an empty chunk at offset 0, not an error: a shell that has
   printed nothing yet and a shell whose log was deleted read alike, and neither
   is worth an exception."
  ([id file] (read-chunk id file nil))
  ([id ^File file {:keys [offset limit]}]
   (let
     [len
      (long (if (.isFile file) (.length file) 0))

      lim
      (-> (long (or limit default-chunk-bytes))
          (max 1)
          (min (long max-chunk-bytes)))

      off
      (long (if (nil? offset)
              (max 0 (- len lim))
              (-> (long offset)
                  (max 0)
                  (min len))))

      want
      (long (min (- len off) lim))

      ^bytes buf
      (if (pos? want) (read-bytes file off want) (byte-array 0))

      got
      (long (alength buf))

      head
      (long (partial-head buf got))

      tail
      (long (if (< (+ off got) len) (partial-tail buf got) 0))

      keep-n
      (long (max 0 (- got head tail)))]

     {:id (str id)
      :offset (+ off head)
      :next-offset (+ off head keep-n)
      :text (String. buf (int head) (int keep-n) StandardCharsets/UTF_8)
      :is-eof (>= (+ off head keep-n) len)
      :is-truncated (< (+ off got) len)})))

;; =============================================================================
;; The index row: which logs does this session have?
;; =============================================================================

(def index-extension-id
  "Owner of the sidecar rows. The shell extension owns its own index."
  "foundation-shell")

(def index-kind "`kind` of a shell-log sidecar row." :shell-log)

(defn index!
  "Upsert the row that makes ONE log findable by session: the command, the
   log path, the start, and — once the pump has seen the child die — the end and
   the exit code. Best effort by construction: the bytes are on disk either way,
   and an index that throws must never take a running shell with it.

   `data` is engine-shaped (kebab keywords) and reaches the database through
   [[wire/->wire]], because a row that outlives the process is JSON on disk and
   wears the wire's snake_case string keys — which is exactly what
   [[session-logs]] reads back."
  [db-info session id data]
  (when (and db-info (seq (str session)) (seq (str id)))
    (try (persistance/db-put-extension-aggregate! db-info
                                                  {:extension-id index-extension-id
                                                   :aggregate-key (str id)
                                                   :kind index-kind
                                                   :session-soul-id (str session)
                                                   :index-data (wire/->wire (assoc data
                                                                              :id (str id)
                                                                              :session-id
                                                                              (str session)))})
         (catch Throwable _ nil))))

(defn session-logs
  "Every indexed shell log of `session`, newest start first. The answer to
   \"what did that build print\" a turn later, with no handle in hand.

   Rows are wire-shaped: snake_case string keys, exactly as [[index!]] wrote
   them."
  [db-info session]
  (if (and db-info (seq (str session)))
    (->> (try (persistance/db-list-extension-aggregates
                db-info
                {:extension-id index-extension-id :kind index-kind :session-soul-id (str session)})
              (catch Throwable _ nil))
         (keep :index-data)
         (sort-by #(or (get % "started_at") 0) >)
         vec)
    []))
