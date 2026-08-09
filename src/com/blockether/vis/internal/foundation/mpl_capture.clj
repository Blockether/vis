(ns com.blockether.vis.internal.foundation.mpl-capture
  "SINK for artifacts the sandbox PRODUCES — matplotlib figures (`plt.show()` /
   `plt.savefig()`), anything a tool hands to `attach`, AND anything written
   into the per-context OUTBOX directory (`$VIS_OUTBOX`) — so the engine OWNS the
   bytes (a `session_iteration_attachment` row) captured AT THE SOURCE, with NO
   re-parsing of the model-facing stdout fence.

   The old flow rendered each figure to a `$TMPDIR/vis-mpl` temp file, printed a
   `vis-image` fence carrying just that PATH, then at persist time re-parsed the
   fence out of stdout and re-read the (possibly already-gone) file. We control the
   whole boundary, so that round-trip is gone: a producer renders/reads the bytes
   HOST-side (matplotlib's `__vis_mpl_render_file__` imaging backend; `attach`'s
   sandbox-confined `open`; the outbox filesystem tap in `sandbox-fs`) and, right
   where it already holds the bytes, calls `record-attachment!` (or `record-file!`).
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
   same block is not recorded twice. Nil outside a driven block."
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

(defn record-attachment!
  "Append ONE produced-artifact attachment map to the active per-block
   `*attachment-sink*` (a silent no-op when unbound — e.g. a call outside a driven
   block). Shape mirrors ONE element of `db-store-iteration!`'s `:attachments`,
   minus `:tool-call-id` which the loop stamps from the block that produced it:
   `{:kind <\"image\"|\"file\"|…> :media-type <mime> :base64 <b64> :size <bytes>
     :filename <name> :dims <\"WxH\", images only>}`. NEVER throws — capture must not
   break a turn."
  [m]
  (when-let [sink *attachment-sink*]
    (try (swap! sink conj m) (catch Throwable _ nil)))
  nil)

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
   stable across restarts, and an existing file is never rewritten."
  ^java.io.File [^String prefix ^String ext ^bytes bs]
  (let
    [dir
     (doto (java.io.File. (java.io.File. (java.io.File. (System/getProperty "user.home") ".vis")
                                         "cache")
                          "display")
       (.mkdirs))

     digest
     (->> (.digest (java.security.MessageDigest/getInstance "SHA-256") bs)
          (take 8)
          (map #(format "%02x" (bit-and (long %) 0xff)))
          (apply str))

     f
     (java.io.File. dir (str prefix digest "." ext))]

    (when-not (.isFile f)
      (java.nio.file.Files/write (.toPath f)
                                 bs
                                 ^"[Ljava.nio.file.OpenOption;"
                                 (make-array java.nio.file.OpenOption 0)))
    f))

;; ---------------------------------------------------------------------------
;; Host-side media-type sniffing — mirrors the Python `__vis_guess_media_type`
;; in `shim-attach`, used by the filesystem outbox tap which only has path+bytes
;; (no Python `mimetypes`). Magic bytes → extension → utf-8 probe.
;; ---------------------------------------------------------------------------

(def ^:const max-capture-bytes
  "THE single per-artifact byte cap (32 MiB) — the one source of truth every
   producer path shares. The filesystem outbox/temp tap (`record-file!`) SKIPS a
   larger file silently (an incidental write must not throw); the explicit
   `attach` shim REJECTS one with a clear error (a
   deliberate attach deserves a signal, not a silent drop). Either way a huge
   write can't OOM the engine or bloat the DB."
  (* 32 1024 1024))

(def ^:private noisy-capture-exts
  "Extensions the filesystem tap SKIPS — machine output, never a document.

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
  "Read the bytes of a just-written file at `path` (a `java.nio.file.Path`), sniff
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
