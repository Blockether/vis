(ns com.blockether.vis.internal.foundation.mpl-capture
  "SINK for artifacts the sandbox PRODUCES — matplotlib figures (`plt.show()` /
   `plt.savefig()`) and anything a tool hands to `attach` — so the engine OWNS the
   bytes (a `session_iteration_attachment` row) captured AT THE SOURCE, with NO
   re-parsing of the model-facing stdout fence.

   The INCIDENTAL capture — every file the sandbox happened to write into
   `$VIS_OUTBOX` or system temp — is DORMANT: see [[incidental-capture-enabled?]].
   Only what a producer DELIBERATELY hands over is recorded now.

   The old flow rendered each figure to a `$TMPDIR/vis-mpl` temp file, printed a
   `vis-image` fence carrying just that PATH, then at persist time re-parsed the
   fence out of stdout and re-read the (possibly already-gone) file. We control the
   whole boundary, so that round-trip is gone: a producer renders/reads the bytes
   HOST-side (matplotlib's `__vis_mpl_render_file__` imaging backend; `attach`'s
   sandbox-confined `open`; the DORMANT outbox filesystem tap in `sandbox-fs`) and,
   right where it already holds the bytes, calls `record-attachment!` (or
   `record-file!`).
   `run-python-block` binds `*attachment-sink*` to a fresh collector around each
   block's eval and drains it into the block outcome's `:attachments`; the loop
   stamps each with the block's tool-call-id and hands them to `db-store-iteration!`'s
   `:attachments`. The stdout fence now serves ONLY the inline TUI/web display +
   ASCII fallback — never persistence.

   Deliberately dependency-free (no AWT, no `vis.core`): safe to require from BOTH
   a render shim and the hot engine loop without dragging the imaging renderer or a require
   cycle."
  (:require [clojure.string :as str])
  (:import [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute BasicFileAttributes]
           [java.nio.charset StandardCharsets]
           [java.util Base64]))

(def ^:dynamic *attachment-sink*
  "Per-block artifact collector: an atom holding a vector of attachment maps, bound
   by `run-python-block` around ONE block's eval (else nil). Producers append into
   it via `record-attachment!`; the block drains `@*attachment-sink*` into its
   `:attachments`."
  nil)

(def ^:dynamic *outbox-seen*
  "Per-block set of canonical outbox paths ALREADY captured by the filesystem tap,
   bound (an atom `#{}`) alongside `*attachment-sink*` so a file re-closed in the
   same block is not recorded twice. Nil outside a driven block. Dormant with the
   tap itself — see [[incidental-capture-enabled?]]."
  nil)

(def ^:const incidental-capture-enabled?
  "THE switch for INCIDENTAL file capture — the old outbox pattern — and it is OFF.

   What it did: every file the sandbox CLOSED under `$VIS_OUTBOX` or a system temp
   root (`sandbox-fs`'s write tap) and every native `write_file` that landed in
   temp (`editing.core/capture-temp-write!`) was read host-side by
   [[record-file!]] and persisted as a `session_iteration_attachment` — an
   implicit twin of `attach` for a library that only knows how to write a file.

   Why it is off: `attach` is the whole need, deliberately. A producer that wants
   a human to SEE something names it and hands over the bytes; everything else the
   sandbox writes is scratch, and harvesting it filled the session DB and the
   companion's recorded-files row with build chips nobody asked for. Nothing else
   changes — the sandbox still writes wherever it may write, the writes are simply
   not collected.

   Kept whole and still covered by tests (this ns, `sandbox-fs`, `env-python`'s
   outbox dir) because a future feature may want an engine-owned capture directory
   again: flip this to `true` and both wirings re-arm."
  false)

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
    (try (let
           [rec (cond-> m
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
  (let
    [dir
     (doto (if *display-home*
             (java.io.File. ^String *display-home*)
             (java.io.File. (java.io.File. (java.io.File. (System/getProperty "user.home") ".vis")
                                           "cache")
                            "display"))
       (.mkdirs))

     digest
     (->> (.digest (java.security.MessageDigest/getInstance "SHA-256") bs)
          (take 8)
          (map #(format "%02x" (bit-and (long %) 0xff)))
          (apply str))

     f
     (java.io.File. dir (str prefix digest "." ext))]

    (if (.isFile f)
      ;; Reuse. `housekeeping/sweep-stale!` judges a cache file by its mtime, so
      ;; a picture rendered AGAIN today must not age out on the day its content
      ;; was first written.
      (.setLastModified f (System/currentTimeMillis))
      (java.nio.file.Files/write (.toPath f)
                                 bs
                                 ^"[Ljava.nio.file.OpenOption;"
                                 (make-array java.nio.file.OpenOption 0)))
    f))

;; Host-side media-type sniffing — mirrors the Python `__vis_guess_media_type`
;; in `shim-attach`, used by the filesystem outbox tap which only has path+bytes
;; (no Python `mimetypes`). Magic bytes → extension → utf-8 probe.

(def ^:const max-capture-bytes
  "THE single per-artifact byte cap (32 MiB) — the one source of truth every
   producer path shares. The filesystem outbox/temp tap (`record-file!`) SKIPS a
   larger file silently (an incidental write must not throw); the explicit
   `attach` shim REJECTS one with a clear error (a
   deliberate attach deserves a signal, not a silent drop). Either way a huge
   write can't OOM the engine or bloat the DB."
  (* 32 1024 1024))

(def ^:private noisy-capture-exts
  "Extensions the filesystem tap SKIPS — machine output, never a document. Dormant
   with the tap ([[incidental-capture-enabled?]]); this is what it filtered.

   Every entry is something a TOOLCHAIN writes for itself: a compiler, a linker,
   a packager, a VM, an editor or a downloader. None of them is an artifact a
   person came for, and once every /tmp write streams through here one `clojure
   -T:build`, `npm run build` or GraalVM run would otherwise bury the session in
   chips and the DB in megabytes.

   This blocklist governs the INCIDENTAL tap only. `attach` is deliberate and
   bypasses it entirely, so a task whose product genuinely IS a jar still ships
   one — by naming it, which is the whole difference between the two paths."
  #{;; bytecode + compiled objects
    "pyc" "pyo" "pyd" "class" "o" "obj" "bc" "rlib" "rmeta"
    ;; linked binaries and native libraries (`dylib`/`jnilib` are macOS `so`)
    "so" "dll" "dylib" "jnilib" "node" "a" "lib" "exp" "pdb" "ilk"
    ;; JVM packaging and runtime dumps
    "jar" "war" "ear" "jmod" "jsa" "ser" "hprof" "jfr"
    ;; build sidecars a reader never opens: source maps, incremental state, digests
    "map" "tsbuildinfo" "cache" "sha1" "sha256" "sha512" "md5"
    ;; process/edit/download scratch
    "lock" "pid" "swp" "swo" "swn" "tmp" "temp" "bak" "orig" "rej" "part" "crdownload" "log"})

(def ^:private ext->media-type
  "Extension → media-type fallback when magic bytes don't decide it."
  {"csv" "text/csv"
   "tsv" "text/tab-separated-values"
   "json" "application/json"
   "txt" "text/plain"
   "md" "text/markdown"
   "html" "text/html"
   "xml" "application/xml"
   "yaml" "text/yaml"
   "yml" "text/yaml"
   "svg" "image/svg+xml"
   "pdf" "application/pdf"
   "png" "image/png"
   "jpg" "image/jpeg"
   "jpeg" "image/jpeg"
   "gif" "image/gif"
   "webp" "image/webp"
   "bmp" "image/bmp"
   "wav" "audio/wav"
   "mp3" "audio/mpeg"
   "ogg" "audio/ogg"
   "zip" "application/zip"
   "gz" "application/gzip"})

(defn- starts-with-sig?
  "True when byte-array `data` begins with the byte sequence `sig` (a seq of ints)."
  [^bytes data sig]
  (let [n (count sig)]
    (and (>= (alength data) n)
         (loop [i 0]
           (cond (= i n) true
                 (= (aget data i) (unchecked-byte (nth sig i))) (recur (inc i))
                 :else false)))))

(defn- utf8-text?
  "True when `data` decodes cleanly as UTF-8 (⇒ treat as text/plain)."
  [^bytes data]
  (try (let [dec (.newDecoder StandardCharsets/UTF_8)]
         (.decode dec (java.nio.ByteBuffer/wrap data))
         true)
       (catch Throwable _ false)))

(defn- ext-of
  "Lower-case file extension of `filename` (sans dot), or nil when there is none."
  [^String filename]
  (let [i (.lastIndexOf (str filename) ".")]
    (when (pos? i) (str/lower-case (subs filename (inc i))))))

(def ^:private anonymous-scratch-name
  "Shape of the filename CPython's `tempfile` invents when NOBODY named the file:
   an 8-character draw from its own `abcdefghijklmnopqrstuvwxyz0123456789_`
   alphabet, optionally behind the default `tmp` prefix (`mkstemp` /
   `NamedTemporaryFile`). `tempfile.gettempdir()` PROBES each candidate directory
   by creating one of these and writing four bytes into it, so merely ASKING for
   the temp dir used to mint a session attachment."
  #"(?:tmp)?[a-z0-9_]{8}")

(defn- anonymous-scratch?
  "True when `filename` has NO extension at all, wears `anonymous-scratch-name`, and
   carries a digit or an underscore — the one signal a suffix blocklist can never
   hold, because the files it must catch have no suffix.

   The digit/underscore clause is the deliberate side of the trade: `manifest`,
   `metadata`, `settings`, `makefile` are all eight lower-case letters, and
   silently dropping a real artifact is worse than keeping the ~8% of random draws
   that happen to come out all-letters. Dropping is safe at all only because this
   tap sees WRITES UNDER A TEMP ROOT alone — a deliberate artifact either names its
   format or goes through the explicit `attach` surface."
  [^String filename]
  (boolean (and (nil? (ext-of filename))
                (re-matches anonymous-scratch-name filename)
                (re-find #"[0-9_]" filename))))

(defn sniff-media-type
  "Best-effort media-type for produced bytes: magic-byte signatures first, then a
   filename-extension fallback, then a utf-8 probe (`text/plain`), else
   `application/octet-stream`. NEVER throws."
  [^bytes data ^String filename]
  (try (cond (starts-with-sig? data [0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A]) "image/png"
             (starts-with-sig? data [0xFF 0xD8 0xFF]) "image/jpeg"
             (starts-with-sig? data [0x47 0x49 0x46 0x38]) "image/gif"
             (starts-with-sig? data [0x42 0x4D]) "image/bmp"
             (starts-with-sig? data [0x25 0x50 0x44 0x46]) "application/pdf"
             (starts-with-sig? data [0x1F 0x8B]) "application/gzip"
             (and (starts-with-sig? data [0x52 0x49 0x46 0x46])
                  (>= (alength data) 12)
                  (= (String. data 8 4 StandardCharsets/US_ASCII) "WEBP"))
             "image/webp"
             (and (starts-with-sig? data [0x52 0x49 0x46 0x46])
                  (>= (alength data) 12)
                  (= (String. data 8 4 StandardCharsets/US_ASCII) "WAVE"))
             "audio/wav"
             (starts-with-sig? data [0x4F 0x67 0x67 0x53]) "audio/ogg"
             (or (starts-with-sig? data [0x49 0x44 0x33]) (starts-with-sig? data [0xFF 0xFB]))
             "audio/mpeg"
             ;; ZIP: could be a bare zip or an OOXML/odf container — leave generic.
             (or (starts-with-sig? data [0x50 0x4B 0x03 0x04])
                 (starts-with-sig? data [0x50 0x4B 0x05 0x06]))
             "application/zip"
             :else (or (get ext->media-type (ext-of filename))
                       (when (utf8-text? data) "text/plain")
                       "application/octet-stream"))
       (catch Throwable _ "application/octet-stream")))

(defn record-file!
  "DORMANT — nothing calls this while [[incidental-capture-enabled?]] is false;
   kept whole (and tested) for a future capture feature.

   Read the bytes of a just-written file at `path` (a `java.nio.file.Path`), sniff
   its media-type, base64-encode, and `record-attachment!` it — the host side of
   the filesystem OUTBOX tap. De-dups per block via `*outbox-seen*` (a file
   re-closed in the same block records once). Skips a file larger than
   `max-capture-bytes`, an EMPTY (0-byte) file, one whose extension is in
   `noisy-capture-exts`, one `anonymous-scratch?` judges nameless scratch, a
   directory, or one already seen. NEVER throws — an outbox write must not break a
   turn."
  [^Path path]
  (try
    (let
      [k
       (str (.toAbsolutePath path))

       seen
       *outbox-seen*]

      (when (and *attachment-sink*
                 (or (nil? seen) (not (contains? @seen k)))
                 (not (contains? noisy-capture-exts (ext-of (str (.getFileName path)))))
                 (not (anonymous-scratch? (str (.getFileName path))))
                 (Files/isRegularFile path (make-array LinkOption 0)))
        (let
          [^BasicFileAttributes attrs
           (Files/readAttributes path
                                 BasicFileAttributes
                                 ^"[Ljava.nio.file.LinkOption;" (make-array LinkOption 0))

           size
           (.size attrs)]

          (when (<= 1 size max-capture-bytes)
            (let
              [data
               (Files/readAllBytes path)

               fname
               (str (.getFileName path))

               mt
               (sniff-media-type data fname)

               b64
               (.encodeToString (Base64/getEncoder) data)]

              (when seen (swap! seen conj k))
              (record-attachment! {:kind (if (str/starts-with? mt "image/") "image" "file")
                                   :media-type mt
                                   :filename fname
                                   :size (alength data)
                                   :base64 b64}))))))
    (catch Throwable _ nil))
  nil)
