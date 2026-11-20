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
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
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

(def ^:private nippy-head (.getBytes "NPY" StandardCharsets/US_ASCII))

(defn- nippy-blob?
  "True for the bytes of a BLOB column, which Nippy froze with its header."
  [x]
  (and (bytes? x)
       (>= (alength ^bytes x) 4)
       (Arrays/equals ^bytes nippy-head (Arrays/copyOfRange ^bytes x 0 3))))

(defn- binary-media?
  "True for a media type whose bytes are never text. SVG is text."
  [media-type]
  (let [media (str/lower-case (str media-type))]
    (and (boolean (re-find #"^(image|audio|video|font)/" media))
         (not (str/includes? media "svg")))))

(defn- utf-8-text
  "The text of `data`, or nil when it is not valid UTF-8 or holds a zero byte."
  [^bytes data]
  (when-not (some zero? data)
    (try (str (.decode (.newDecoder StandardCharsets/UTF_8) (ByteBuffer/wrap data)))
         (catch java.nio.charset.CharacterCodingException _ nil))))

(defn- redact-rows
  "Apply `redact-text` to each string of `rows`, also in each BLOB column. A BLOB that
   does not change keeps its bytes."
  [rows redact-text]
  ;; `=` ignores metadata, so `changed` records each string that changes, also in metadata.
  (letfn
    [(clean [changed x]
       (cond (string? x) (let [y (redact-text x)]
                           (when (not= x y) (vreset! changed true))
                           y)
             (nippy-blob? x) (let [value
                                   (try (nippy/thaw x)
                                        (catch Exception e
                                          (throw (ex-info "A saved record is not readable"
                                                          {:type :session/snapshot-incomplete}
                                                          e))))

                                   blob-changed
                                   (volatile! false)

                                   cleaned
                                   (clean blob-changed value)]

                               (if @blob-changed
                                 (do (vreset! changed true)
                                     (nippy/freeze cleaned {:compressor nippy/zstd-compressor}))
                                 x))
             (coll? x) (let [y (walk/walk (partial clean changed) identity x)]
                         (if-let [m (meta x)]
                           (with-meta y (clean changed m))
                           y))
             (and (symbol? x) (meta x)) (with-meta x (clean changed (meta x)))
             :else x))]
    (clean (volatile! false) rows)))

(defn redact
  "The session `tree` with `redact-text` applied to each string: in rows, in BLOB columns
   and in text attachments. A text attachment is valid UTF-8 with a media type that is not
   image, audio, video or font. A changed text attachment gets a new SHA-256 and size; a
   binary attachment with the same bytes keeps them. The manifest comes from the result,
   so its titles are clean too."
  [tree redact-text]
  (let [text?
        (fn [att]
          (and (:bytes_sha256 att) (not (binary-media? (:media_type att)))))

        cleaned
        (into
          {}
          (for [sha
                (into
                  #{}
                  (comp (mapcat :turns) (mapcat :attachments) (filter text?) (map :bytes_sha256))
                  (:sessions tree))

                :let [text
                      (some-> (get-in tree [:blobs sha])
                              utf-8-text)

                      clean
                      (some-> text
                              redact-text)]
                :when (and text (not= text clean))
                :let [data
                      (.getBytes ^String clean StandardCharsets/UTF_8)]]

            [sha {:sha (util/sha256-hex data) :data data}]))

        rename
        (fn [att]
          (if-let [{:keys [sha ^bytes data]} (when (text? att) (get cleaned (:bytes_sha256 att)))]
            (assoc att
              :bytes_sha256 sha
              :size_bytes (alength data))
            att))

        sessions
        (mapv (fn [session]
                (update session
                        :turns
                        (fn [turns]
                          (mapv #(update % :attachments (partial mapv rename)) turns))))
              (:sessions tree))

        used
        (into #{} (comp (mapcat :turns) (mapcat :attachments) (keep :bytes_sha256)) sessions)]

    (assoc tree
      :sessions (redact-rows sessions redact-text)
      :council (redact-rows (:council tree) redact-text)
      :blobs (into (select-keys (:blobs tree) used)
                   (map (fn [{:keys [sha data]}]
                          [sha data]))
                   (vals cleaned)))))

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

    (when (> version (long format-version))
      (throw (ex-info (str "This session snapshot needs a newer Vis: format version " version)
                      {:type :session/unsupported-version :format-version version})))
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

(defn- foreign?
  "True for a value that a snapshot never holds: a record, or a class that Nippy quarantined."
  [x]
  (or (record? x) (and (map? x) (contains? x :nippy/unthawable))))

(defn- check-tree!
  "Throw unless `tree` holds sessions with distinct ids, the first one as the only root, and
   a chain of fork or child links from each other session to that root."
  [tree]
  (let [sessions
        (when (map? tree) (:sessions tree))

        ids
        (mapv (comp :id :soul) sessions)

        parent
        (zipmap ids (map #(or (:parent-id %) (get-in % [:agent :parent_id])) sessions))

        root
        (first ids)

        reaches-root?
        (fn [id]
          (loop [id
                 id

                 steps
                 (count ids)]

            (cond (= id root) true
                  (or (nil? id) (zero? steps)) false
                  :else (recur (get parent id) (dec steps)))))]

    (when (empty? sessions) (invalid! "The session snapshot has no sessions"))
    (when-not (apply distinct? ids) (invalid! "The session snapshot repeats a session"))
    (when-not (and (every? string? ids) (nil? (get parent root)) (every? reaches-root? ids))
      (invalid! "The session snapshot tree is damaged"))))

(defn decode
  "The manifest and the session tree of snapshot `data`. Throws `:session/invalid-snapshot`
   for a damaged file, a digest that does not match, a value of an unknown type or a broken
   session tree. Throws `:session/unsupported-version` for a newer format and
   `:session/snapshot-too-large` past [[max-bytes]]."
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
    (let [tree (binding [nippy/*thaw-serializable-allowlist* #{}]
                 (try (nippy/thaw payload)
                      (catch Exception _ (invalid! "The session snapshot payload is damaged"))))]
      (when (some foreign? (tree-seq coll? seq tree))
        (invalid! "The session snapshot holds a value of an unknown type"))
      (check-tree! tree)
      {:manifest manifest :tree tree})))
