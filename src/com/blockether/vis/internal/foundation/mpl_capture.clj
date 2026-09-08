(ns com.blockether.vis.internal.foundation.mpl-capture
  "Per-block collection of explicit attachments and rendered matplotlib figures.

   Producers call `record-attachment!` with bytes they already hold.
   `run-python-block` binds `*attachment-sink*` and drains it into the block's
   `:attachments`; the loop passes those records to iteration persistence.
   Stdout is used for display, not attachment persistence. Ordinary filesystem
   writes are not collected.

   This namespace does not depend on the renderer or tool namespaces."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.util :as util]))

(def ^:dynamic *attachment-sink*
  "Per-block artifact collector: an atom holding a vector of attachment maps, bound
   by `run-python-block` around ONE block's eval (else nil). Producers append into
   it via `record-attachment!`; the block drains `@*attachment-sink*` into its
   `:attachments`."
  nil)

(def ^:dynamic *attachment-reader*
  "Per-block READ-BACK accessor for artifacts already persisted in THIS session,
   bound by `run-python-code` around one block's eval (else nil). A map
   `{:list (fn [] [{:id :filename :media-type :kind :size :position :tool-call-id
   :iteration-id} …]) :read (fn [attachment-id] {:id :base64 :media-type …}|nil)}`
   closing over the session's db-info + id. Lets the `list_attachments` /
   `read_attachment` sandbox shims re-fetch an artifact a tool (or an earlier
   turn) produced. Nil outside a driven block ⇒ the shim surfaces a clear
   `RuntimeError` instead of silently returning nothing."
  nil)

(def ^:dynamic *attachment-reinspection-sink*
  "Per-block queue of persisted image attachments deliberately reintroduced to the
   NEXT provider request. Bound by `run-python-code`; `show_attachment`
   appends hydrated session-owned images here. Unlike `*attachment-sink*`, these
   are ephemeral: the loop consumes them once and never stores duplicate bytes."
  nil)

(defn- attachment-versions-for
  "Every version already handed out for `filename` in this session: the stored
   cuts `*attachment-reader*` can see, plus the ones THIS block has already
   recorded into `*attachment-sink*`. Empty for an anonymous artifact, or
   outside a driven block."
  [filename]
  (let [name-s (str filename)]
    (if (str/blank? name-s)
      []
      (->> (concat (try (when-let [r *attachment-reader*]
                          ((:list r)))
                        (catch Throwable _ nil))
                   (some-> *attachment-sink*
                           deref))
           (filter #(= name-s (str (:filename %))))
           (mapv #(long (or (:version %) 1)))))))

(defn next-attachment-version
  "The version the persistence layer will store `filename` under: 1 + the highest
   cut of that name already in this session, and 1 for an anonymous artifact.

   THE SAME RULE the insert allocator applies (`store-iteration-attachments!`),
   evaluated here so the descriptor a producer gets back at `attach` time names
   the cut its row will actually carry. Never throws."
  ^long [filename]
  (try (inc (long (reduce max 0 (attachment-versions-for filename)))) (catch Throwable _ 1)))

(defn record-attachment!
  "Append ONE produced-artifact attachment map to the active per-block
   `*attachment-sink*` (a silent no-op when unbound — e.g. a call outside a driven
   block). Shape mirrors ONE element of `db-store-iteration!`'s `:attachments`,
   minus `:tool-call-id` which the loop stamps from the block that produced it:
   `{:kind <\"image\"|\"file\"|…> :media-type <mime> :base64 <b64> :size <bytes>
     :filename <name> :dims <\"WxH\", images only>}`.

   IDENTITY IS MINTED HERE, at the source: an artifact gets its durable `:id` and
   its `:version` the moment it is recorded, so the producer can address what it
   just made (`get_attachment`/`read_attachment`/`show_attachment`) inside the
   very block that made it, and the row the loop inserts later carries the same
   id. Returns the recorded map (nil with no sink). NEVER throws — capture must
   not break a turn.

   An artifact whose bytes are only final long after its block handed control back
   — a live view a human stops from a gateway thread — is filed by REBINDING this
   var to the collector that block captured. The sink is the whole contract; there
   is deliberately no second way to hand one in."
  [m]
  (when-let [sink *attachment-sink*]
    (try (let [rec (cond-> m
                     (str/blank? (str (:id m)))
                     (assoc :id (str (java.util.UUID/randomUUID)))

                     (nil? (:version m))
                     (assoc :version (next-attachment-version (:filename m))))]
           (swap! sink conj rec)
           rec)
         (catch Throwable _ nil))))

(defn pending-attachments
  "What THIS block has recorded into `*attachment-sink*` so far — artifacts the
   loop has not persisted yet, each already carrying the `:id` and `:version`
   [[record-attachment!]] minted. `[]` outside a driven block."
  []
  (vec (some-> *attachment-sink*
               deref)))

(defn queue-reinspection!
  "Queue one hydrated, session-owned image for exactly one provider request. A
   silent no-op outside a driven block; callers validate ownership and media type."
  [attachment]
  (when-let [sink *attachment-reinspection-sink*]
    ;; Reinspection means "show this attachment", not "charge vision once per
    ;; repeated tool call". Keep first-seen order while coalescing by durable id.
    (try (swap! sink (fn [queued]
                       (if (some #(= (:id attachment) (:id %)) queued)
                         queued
                         (conj queued attachment))))
         (catch Throwable _ nil)))
  nil)

(defn drain-reinspections
  "Queued one-request image re-inspections for `sink`, or nil when none."
  [sink]
  (not-empty (vec (some-> sink
                          deref))))

(defn drain
  "The attachments collected in `sink` (an atom vector) as a plain vector, or nil
   when empty — the value `run-python-block` folds into a block outcome's
   `:attachments`."
  [sink]
  (not-empty (vec (some-> sink
                          deref))))

(def
  ^:dynamic
  ^{:doc
    "Test seam for the display cache directory. `nil` (production) resolves to
                 `~/.vis/cache/display`, the location `housekeeping/sweep-stale!` bounds by age
                 — a fixed contract, not a configurable."}
  *display-home*
  nil)

(defn display-cache-file
  "Durable, content-addressed host file backing ONE inline `vis-image` display
   fence: `~/.vis/cache/display/<prefix><sha256-16>.<ext>`.

   The fence a shim prints carries a HOST PATH, and that path is persisted with
   the iteration output — a TUI re-rendering history repaints the picture from
   it. An OS temp file (the old home) is swept by the system days later, so a
   restored bubble then pointed at a dead path while the same artifact still
   rendered fine in the companion app (which fetches DB bytes). This cache is
   the TUI-side equivalent of that durability. DISPLAY ONLY — the bytes stay
   DB-owned via `record-attachment!`.

   Content-addressed: the same figure written twice reuses one file, the name is
   stable across restarts, and an existing file is never rewritten — only
   re-stamped, because `housekeeping/sweep-stale!` ages this directory out and a
   picture rendered again today is not a month-old one."
  ^java.io.File [^String prefix ^String ext ^bytes bs]
  (let [dir
        (doto (if *display-home*
                (java.io.File. ^String *display-home*)
                (java.io.File.
                  (java.io.File. (java.io.File. (System/getProperty "user.home") ".vis") "cache")
                  "display"))
          (.mkdirs))

        digest
        (subs (util/sha256-hex bs) 0 16)

        f
        (java.io.File. dir (str prefix digest "." ext))]

    (if (.isFile f)
      ;; Reuse. `housekeeping/sweep-stale!` judges a cache file by its mtime, so
      ;; a picture rendered AGAIN today must not age out on the day its content
      ;; was first written.
      (.setLastModified f (util/now-ms))
      (java.nio.file.Files/write (.toPath f)
                                 bs
                                 ^"[Ljava.nio.file.OpenOption;"
                                 (make-array java.nio.file.OpenOption 0)))
    f))

(def ^:const max-capture-bytes
  "Per-attachment byte limit (32 MiB). Explicit attach rejects larger payloads
   before reading them into memory or storing them in the session database."
  (* 32 1024 1024))
