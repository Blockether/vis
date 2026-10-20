(ns com.blockether.vis.internal.session.snapshot
  "The `.vissnap` session snapshot file (#405). A program reads its JSON manifest without
   a JVM; only the engine reads the payload.

   Layout, format version 1:

     magic        8 bytes, `VISSNAP` and a zero byte
     version      4 bytes, big-endian
     length       4 bytes, big-endian, the manifest length
     manifest     UTF-8 JSON
     payload      the Nippy-frozen rows of one session tree, Zstandard-compressed

   The manifest holds the format and Vis versions, the creation time, the root session,
   the session tree, counts, sizes and the payload SHA-256. A decode checks the digest
   before it thaws the payload."
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.util :as util]
            [taoensso.nippy :as nippy])
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:private magic (.getBytes "VISSNAP\u0000" StandardCharsets/US_ASCII))

(def ^:const format-version 1)

(def ^:private ^:const header-bytes "Magic, version and manifest length." 16)

(def ^:private ^:const max-manifest-bytes
  "A manifest lists the tree, never the content, so a larger one is not a snapshot."
  16777216)

(def ^:private limit-bytes
  {"256_mib" (* 256 1024 1024) "512_mib" (* 512 1024 1024) "1_gib" (* 1024 1024 1024)})

(defn max-bytes
  "The configured snapshot size limit in bytes, from the `session_snapshot_limit` setting."
  []
  (get limit-bytes (toggles/value-of "session_snapshot_limit") (get limit-bytes "1_gib")))

(defn- invalid! [message] (throw (ex-info message {:type :session/invalid-snapshot})))

(defn- manifest
  "The JSON manifest of `tree` with its frozen `payload`."
  [tree ^bytes payload vis-version now]
  (let [sessions
        (:sessions tree)

        turns
        (mapcat :turns sessions)]

    {"format_version" format-version
     "vis_version" (str vis-version)
     "created_at" (long now)
     "root_session_id" (str (:root-id tree))
     "sessions" (mapv (fn [{:keys [soul state parent-id]}]
                        {"id" (str (:id soul))
                         "parent_id" (some-> parent-id
                                             str)
                         "title" (:title state)})
                      sessions)
     "counts" {"sessions" (count sessions)
               "turns" (count turns)
               "iterations" (count (mapcat :iterations turns))
               "attachments" (count (mapcat :attachments turns))
               "council_entries" (count (get-in tree [:council :entries]))}
     "sizes" {"payload_bytes" (alength payload)
              "attachment_bytes" (reduce + 0 (map #(alength ^bytes %) (vals (:blobs tree))))}
     "payload_sha256" (util/sha256-hex payload)}))

(defn encode
  "The snapshot bytes of one exported session `tree`. Throws `:session/snapshot-too-large`
   past [[max-bytes]]."
  ^bytes [tree {:keys [vis-version now]}]
  (let [^bytes payload
        (nippy/freeze tree {:compressor nippy/zstd-compressor})

        ^bytes head
        (.getBytes ^String (wire/json-str (manifest tree payload vis-version now))
                   StandardCharsets/UTF_8)

        size
        (+ header-bytes (alength head) (alength payload))]

    (when (> size (long (max-bytes)))
      (throw (ex-info "The session snapshot exceeds the size limit"
                      {:type :session/snapshot-too-large :bytes size :limit (max-bytes)})))
    (-> (ByteBuffer/allocate (int size))
        (.put ^bytes magic)
        (.putInt (int format-version))
        (.putInt (alength head))
        (.put head)
        (.put payload)
        (.array))))

(defn- parts
  "The manifest map and the payload offset of snapshot `data`."
  [^bytes data]
  (when (or (< (alength data) (long header-bytes))
            (not (Arrays/equals ^bytes magic (Arrays/copyOfRange data 0 (alength ^bytes magic)))))
    (invalid! "This file is not a Vis session snapshot"))
  (let [buffer
        (ByteBuffer/wrap data)

        version
        (.getInt buffer 8)

        length
        (.getInt buffer 12)]

    (when-not (= format-version version)
      (invalid! (str "Unsupported session snapshot format version " version)))
    (when (or (neg? length)
              (> length (long max-manifest-bytes))
              (> (+ (long header-bytes) length) (alength data)))
      (invalid! "The session snapshot manifest is damaged"))
    {:manifest (try (wire/parse-json (String. data header-bytes length StandardCharsets/UTF_8))
                    (catch Exception _ (invalid! "The session snapshot manifest is damaged")))
     :offset (+ header-bytes length)}))

(defn info
  "The manifest of snapshot `data`, without the payload."
  [^bytes data]
  (:manifest (parts data)))

(defn decode
  "The manifest and the session tree of snapshot `data`. Throws `:session/invalid-snapshot`
   for a damaged file or a digest that does not match, and `:session/snapshot-too-large`
   past [[max-bytes]]."
  [^bytes data]
  (when (> (alength data) (long (max-bytes)))
    (throw (ex-info "The session snapshot exceeds the size limit"
                    {:type :session/snapshot-too-large :bytes (alength data) :limit (max-bytes)})))
  (let [{:keys [manifest offset]}
        (parts data)

        payload
        (Arrays/copyOfRange data (int offset) (alength data))]

    (when-not (= (get manifest "payload_sha256") (util/sha256-hex payload))
      (invalid! "The session snapshot payload does not match its digest"))
    (let [tree (try (nippy/thaw payload)
                    (catch Exception _ (invalid! "The session snapshot payload is damaged")))]
      (when-not (and (map? tree) (seq (:sessions tree)))
        (invalid! "The session snapshot has no sessions"))
      {:manifest manifest :tree tree})))
