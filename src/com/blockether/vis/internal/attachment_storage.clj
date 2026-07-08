(ns com.blockether.vis.internal.attachment-storage
  "Attachment storage-offload rail: a registry of storage BACKENDS plus the
   pure OFFLOAD DECISION that routes one attachment's payload either INLINE
   (bytes in the `session_attachment.bytes` BLOB) or EXTERNAL (bytes handed to
   a backend, which returns a `storage_uri` -- `scheme://...` -- kept in the row
   instead).

   Zero SQL, zero schema change: the V4 `session_attachment` table already
   carries a nullable `storage_uri` with an exactly-one(bytes, storage_uri)
   CHECK. This namespace only decides WHICH of the two a given attachment takes,
   PUTs/GETs the external bytes through the scheme-dispatched backend, and
   hydrates a read-back envelope's `:base64` from its `:storage-uri` on demand.

   The decision is a PURE predicate `hot? AND size` (see `default-offload?`):
   an image replays to a vision model every turn its iteration stays live, so it
   is HOT -- kept inline even when large; a non-image artifact (PDF/CSV/wav/
   download) is fetched at most once by a human, so it is COLD -- a good offload
   candidate past a size floor. A backend may override the predicate wholesale
   via `:storage/offload?`.

   Precedence, engine-owned so the loop never learns a storage dialect:
     1. active backend's `:storage/offload?`  (the backend knows its own cost)
     2. else `default-offload?`               (engine default policy)
     3. no active backend                     -> always inline (zero regression)"
  (:require [clojure.string :as str])
  (:import (java.nio.file Files LinkOption OpenOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.util Base64 UUID)))

;; =============================================================================
;; Backend registry (mirrors internal.workspace's backend registry)
;; =============================================================================

(defonce ^:private backends (atom {}))

(defn attachment-backend
  "Validate + normalize a storage backend descriptor. Returns the descriptor
   with `:storage/priority` coerced to a long (default 0).

   Required keys:
     :storage/id       keyword identity, e.g. `:file`
     :storage/scheme   the URI scheme string it owns, e.g. \"file\" -- the GET
                       dispatch key (a `storage_uri` is `\"<scheme>://...\"`)
     :storage/put-fn   ({:bytes ^bytes :media-type :kind :filename :id}
                          -> uri-string) writes the bytes, returns the handle
     :storage/get-fn   (uri-string -> ^bytes | nil) reads the bytes back
   Optional:
     :storage/offload? ({:size :media-type :kind :tool-call-id} -> boolean)
                       overrides `default-offload?` when present
     :storage/priority long; higher wins when several are registered (default 0)"
  [backend]
  (let [id
        (:storage/id backend)

        scheme
        (:storage/scheme backend)]

    (when-not (keyword? id)
      (throw (ex-info "Storage backend id must be a keyword"
                      {:type :storage/invalid-backend :backend backend})))
    (when-not (and (string? scheme) (not (str/blank? scheme)))
      (throw (ex-info "Storage backend scheme must be a non-blank string"
                      {:type :storage/invalid-backend :backend-id id :scheme scheme})))
    (doseq [k [:storage/put-fn :storage/get-fn]]
      (when-not (ifn? (get backend k))
        (throw (ex-info (str "Storage backend requires " k)
                        {:type :storage/invalid-backend :backend-id id :key k}))))
    (when-let [off (:storage/offload? backend)]
      (when-not (ifn? off)
        (throw (ex-info "Storage backend :storage/offload? must be a fn"
                        {:type :storage/invalid-backend :backend-id id}))))
    (update backend :storage/priority #(long (or % 0)))))

(defn register-backend!
  "Register a storage backend. Idempotent by `:storage/id`. Returns the
   normalized descriptor."
  [backend]
  (let [backend (attachment-backend backend)]
    (swap! backends assoc (:storage/id backend) backend)
    backend))

(defn deregister-backend! [id] (swap! backends dissoc id) nil)

(defn registered-backends
  "Registered storage backends ordered by descending priority (id breaks ties)."
  []
  (->> @backends
       vals
       (sort-by (juxt (comp - :storage/priority) (comp str :storage/id)))
       vec))

(defn active-backend
  "Highest-priority registered backend, or nil when none. Nil means every
   attachment stays inline -- the zero-backend behaviour, unchanged."
  []
  (first (registered-backends)))

;; =============================================================================
;; Offload decision (pure)
;; =============================================================================

(def ^:const default-offload-floor-bytes
  "Size floor below which the engine default keeps an artifact inline (256 KiB).
   Small payloads never earn an external round-trip."
  (* 256 1024))

(defn default-offload?
  "Engine default offload predicate: offload a COLD artifact past the size
   floor. Images are HOT (replayed to a vision model every turn their iteration
   is live) so they NEVER offload by default, regardless of size -- otherwise
   each turn would re-fetch the bytes. Pure; safe to call on any attachment
   map."
  [{:keys [size media-type]}]
  (boolean (and size
                (> (long size) default-offload-floor-bytes)
                (not (and media-type (str/starts-with? (str media-type) "image/"))))))

(defn offload?
  "Decide inline (false) vs external (true) for ONE attachment, given `backend`
   (typically `active-backend`). Precedence: the backend's `:storage/offload?`
   when present, else `default-offload?`. Nil backend -> false. Never throws --
   a predicate that blows up falls back to inline."
  [backend att]
  (boolean (when backend
             (let [pred (or (:storage/offload? backend) default-offload?)]
               (try (pred (select-keys att [:size :media-type :kind :tool-call-id]))
                    (catch Throwable _ false))))))

;; =============================================================================
;; Write side: offload store-bound attachments
;; =============================================================================

(defn- decode-b64
  ^bytes [att]
  (try (.decode (Base64/getDecoder) (str (:base64 att))) (catch Throwable _ nil)))

(defn offload-attachment
  "Route ONE store-bound attachment map. An inline candidate carries `:base64`.
   When a backend is active AND `offload?` says so, decode + PUT the bytes and
   return the map with `:storage-uri` set (and `:size` fixed to the real byte
   count) and `:base64` DROPPED; otherwise the map is returned unchanged. NEVER
   throws -- a decode/PUT failure or a non-string URI falls back to inline."
  [att]
  (let [backend (active-backend)]
    (if (and backend (:base64 att) (offload? backend att))
      (or (try (when-let [^bytes data (decode-b64 att)]
                 (let [uri ((:storage/put-fn backend)
                             {:bytes data
                              :media-type (:media-type att)
                              :kind (:kind att)
                              :filename (:filename att)
                              :id (:id att)})]
                   (when (string? uri)
                     (-> att
                         (assoc :storage-uri uri
                                :size (alength data))
                         (dissoc :base64)))))
               (catch Throwable _ nil))
          att)
      att)))

(defn offload-attachments
  "Map `offload-attachment` over a store-bound attachment seq. The value handed
   to `db-store-iteration!` / `db-store-session-turn!` `:attachments`; the
   in-memory replay copy stays inline (offload only the persisted path)."
  [atts]
  (mapv offload-attachment (or atts [])))

;; =============================================================================
;; Read side: resolve + hydrate
;; =============================================================================

(defn scheme-of
  "URI scheme of a `storage-uri` (`\"<scheme>://...\"`), or nil."
  [uri]
  (when (string? uri)
    (let [i (.indexOf ^String uri "://")]
      (when (pos? i) (subs uri 0 i)))))

(defn- backend-for-scheme
  [scheme]
  (some #(when (= scheme (:storage/scheme %)) %) (registered-backends)))

(defn resolve-bytes
  "Fetch the raw bytes for an external `storage-uri` by dispatching on its
   scheme to the owning backend's `:storage/get-fn`. nil when no backend owns
   the scheme, or the GET fails / returns nil."
  ^bytes [storage-uri]
  (when-let [b (backend-for-scheme (scheme-of storage-uri))]
    (try ((:storage/get-fn b) storage-uri) (catch Throwable _ nil))))

(defn hydrate
  "Read-back envelope -> same envelope with `:base64` populated. A row stored
   INLINE already carries `:base64` (returned untouched). A row stored EXTERNAL
   carries only `:storage-uri`, so its bytes are fetched via `resolve-bytes` and
   base64-encoded into `:base64`. Left alone when it already has `:base64`, has
   no `:storage-uri`, or the fetch fails (the caller still sees `:storage-uri`)."
  [att]
  (if (or (:base64 att) (not (:storage-uri att)))
    att
    (if-let [^bytes data (resolve-bytes (:storage-uri att))]
      (assoc att :base64 (.encodeToString (Base64/getEncoder) data))
      att)))

(defn hydrate-all
  "Map `hydrate` over a read-back attachment seq (nil-safe)."
  [atts]
  (mapv hydrate (or atts [])))

;; =============================================================================
;; Reference backend: local-disk `file://`
;; =============================================================================

(defn file-backend
  "Reference storage backend that offloads bytes to a local DIRECTORY, one file
   per artifact, addressed by a `file://<absolute-path>` URI. A genuinely useful
   default -- offload big COLD artifacts to disk instead of bloating the DB --
   AND the reference implementation the registry contract is exercised against.

   NOT auto-registered: an operator or extension opts in with
   `(register-backend! (file-backend {:dir \"/var/vis/attachments\"}))`.

   Opts: `:dir` (required) target directory (created if absent); `:id` backend
   id (default `:file`); `:priority` (default 0); `:offload?` an optional
   predicate override (nil -> the engine `default-offload?`). Each PUT mints its
   own opaque filename, so the storage key is independent of the DB row id."
  [{:keys [dir id priority offload?] :or {id :file priority 0}}]
  (when (str/blank? (str dir)) (throw (ex-info "file-backend requires :dir" {:opts {:dir dir}})))
  (let [^Path root (Path/of (str dir) (make-array String 0))]
    (Files/createDirectories root (make-array FileAttribute 0))
    (cond-> {:storage/id id
             :storage/scheme "file"
             :storage/priority (long priority)
             :storage/put-fn (fn [{:keys [^bytes bytes]}]
                               (let [^Path p (.resolve root (str (UUID/randomUUID)))]
                                 (Files/write p bytes (make-array OpenOption 0))
                                 (str "file://" (.toAbsolutePath p))))
             :storage/get-fn (fn [uri]
                               (let [path (subs (str uri) (count "file://"))
                                     ^Path p (Path/of path (make-array String 0))]

                                 (when (Files/isRegularFile p (make-array LinkOption 0))
                                   (Files/readAllBytes p))))}
      offload?
      (assoc :storage/offload? offload?))))
