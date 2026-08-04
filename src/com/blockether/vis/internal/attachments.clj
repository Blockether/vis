(ns com.blockether.vis.internal.attachments
  "User-message image and video attachments.

   Dropping a file onto the terminal pastes its PATH into the input (the
   terminal's drop behavior — same mechanism pi relies on). At turn start
   the engine scans the user message for path-shaped tokens that resolve
   to real image files, reads them, and attaches them to the initial user
   message as multimodal content blocks. Channel-neutral: every channel
   gets the same behavior because the scan runs in the engine,
   not the channel.

   Only files the model can genuinely consume are attached: the MIME type
   is sniffed from magic bytes (pi-parity: jpeg / non-animated png / gif /
   webp / bmp, plus MP4/QuickTime clips) or, for SVG, from the markup head --
   never trusted from the extension alone.

   Storing and SENDING are deliberately separate concerns:

     * STORE — the original bytes under their sniffed container, nothing
       converted, nothing downscaled, nothing re-compressed. What the user
       supplied is what the session keeps, and a file over `max-image-bytes`
       is skipped with a reason the prompt assembler surfaces.
     * SEND — [[wire-image]] is the one gate every image crosses on its way to
       a provider: it decodes the payload to prove it is pixels, re-containers
       what no wire accepts (BMP/SVG -> PNG via `image-convert`, on
       `com.blockether/imaging`, so it behaves identically in the native image)
       and REFUSES what it cannot turn into a picture.

   The split is what makes a bad attachment survivable. Attachments replay on
   every later turn, so a row blessed once on the way IN is shipped forever —
   one corrupt PNG that way is a permanent provider 400. Judged on the way OUT,
   the same row is simply dropped and the session keeps working."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.image-convert :as image-convert]
            [com.blockether.vis.internal.paths :as paths])
  (:import [java.io File RandomAccessFile]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.security MessageDigest]
           [java.util Base64]))

;; =============================================================================
;; Limits
;; =============================================================================

(def max-image-bytes
  "Per-image cap on the WIRE PAYLOAD, in bytes of BASE64 -- which is what a
   provider measures, not the picture that decodes out of it (see
   `wire-byte-budget`, so the picture itself gets 3/4 of this).

   Anthropic's API limit is 5MB/image and it is enforced on the base64 string
   (`image exceeds 5 MB maximum: 5994492 bytes > 5242880 bytes`); OpenAI and
   Gemini bound the whole request far more generously. The smallest common
   bound keeps one attachment valid on every wire."
  (* 5 1024 1024))

(def max-video-bytes
  "Per-clip byte cap. A clip is never sent in its own container -- it leaves as
   a small animated GIF ([[wire-verdict]]) -- so this bounds the ORIGINAL the
   session keeps and the memory one drop can cost, not any provider limit. A
   32MB ceiling covers a normal screen recording or a phone clip."
  (* 32 1024 1024))

(def max-image-count
  "Attachment count cap per user message. Guards against a pathological
   message (e.g. a pasted directory listing) ballooning the request."
  8)

(def ^:const oversize-rescue-factor
  "How far past `max-image-bytes` a file may sit and still be worth reading for
   a shrink attempt. Bounds the memory one pathological drop can cost."
  4)

(def ^:const upload-rescue-factor
  "How far past `max-image-bytes` a STILL may sit and still be accepted for
   storage. A phone photo or a retina screenshot is routinely tens of megabytes
   while the provider ceiling is 5MB, and [[wire-image]] shrinks whatever it is
   given on the way OUT — so refusing the upload only loses the picture."
  5)

(def max-upload-image-bytes
  "Per-still INTAKE cap (25MB): the largest image the gateway accepts and the
   session stores. Distinct from [[max-image-bytes]], which is the PROVIDER cap
   applied to the bytes that actually go on the wire."
  (* (long upload-rescue-factor) (long max-image-bytes)))

(def ^:private sniff-bytes
  "Bytes read from the file head for MIME sniffing (pi parity: enough for
   the PNG chunk walk that rejects animated PNGs)."
  4100)

;; =============================================================================
;; Magic-byte MIME sniffing (pi utils/mime.ts parity)
;; =============================================================================

(defn- u8 ^long [^bytes b ^long i] (bit-and (long (aget b i)) 0xff))

(defn- u16-le ^long [^bytes b ^long i] (+ (u8 b i) (bit-shift-left (u8 b (inc i)) 8)))

(defn- u32-le
  ^long [^bytes b ^long i]
  (+ (u8 b i)
     (bit-shift-left (u8 b (+ i 1)) 8)
     (bit-shift-left (u8 b (+ i 2)) 16)
     (bit-shift-left (u8 b (+ i 3)) 24)))

(defn- u32-be
  ^long [^bytes b ^long i]
  (+ (bit-shift-left (u8 b i) 24)
     (bit-shift-left (u8 b (+ i 1)) 16)
     (bit-shift-left (u8 b (+ i 2)) 8)
     (u8 b (+ i 3))))

(defn- bytes-at?
  [^bytes b ^long offset sig]
  (let [n (count sig)]
    (and (>= (alength b) (+ offset n))
         (every? (fn [^long i]
                   (= (long (nth sig i)) (u8 b (+ offset i))))
                 (range n)))))

(defn- ascii-at?
  [^bytes b ^long offset ^String s]
  (bytes-at? b offset (map long (.getBytes s "US-ASCII"))))

(def ^:private png-signature [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a])

(defn- png?
  [^bytes b]
  (and (>= (alength b) 16) (= 13 (u32-be b (count png-signature))) (ascii-at? b 12 "IHDR")))

(defn- animated-png?
  "True when an `acTL` chunk appears before the first `IDAT` — the APNG
   marker. Providers reject animated inputs; pi skips them too."
  [^bytes b]
  (loop [offset (long (count png-signature))]
    (if (> (+ offset 8) (alength b))
      false
      (let
        [chunk-length (u32-be b offset)
         type-offset (+ offset 4)]

        (cond (ascii-at? b type-offset "acTL") true
              (ascii-at? b type-offset "IDAT") false
              :else (let [next-offset (+ offset 8 chunk-length 4)]
                      (if (or (<= next-offset offset) (> next-offset (alength b)))
                        false
                        (recur next-offset))))))))

(defn- bmp?
  [^bytes b]
  (and (>= (alength b) 30)
       (let
         [declared-size
          (u32-le b 2)

          pixel-data-offset
          (u32-le b 10)

          dib-header-size
          (u32-le b 14)]

         (and (or (zero? declared-size) (>= declared-size 26))
              (>= pixel-data-offset (+ 14 dib-header-size))
              (or (zero? declared-size) (< pixel-data-offset declared-size))
              (let
                [[planes bpp] (cond (= dib-header-size 12) [(u16-le b 22) (u16-le b 24)]
                                    (<= 40 dib-header-size 124) [(u16-le b 26) (u16-le b 28)]
                                    :else [nil nil])]
                (and (= 1 planes) (contains? #{1 4 8 16 24 32} bpp)))))))

(defn- svg-markup?
  "True when the head reads as an SVG document: an XML/comment/doctype prolog or
   the root tag itself, followed by an `<svg` element. Text sniffing, not magic
   bytes -- SVG has none."
  [^bytes b]
  (let
    [head
     (str/lower-case (String. b 0 (int (min (alength b) 4096)) StandardCharsets/UTF_8))

     start
     (str/triml (str/replace head "\ufeff" ""))]

    (and (str/starts-with? start "<") (str/includes? head "<svg"))))

(defn detect-image-mime
  "Sniff a supported image MIME type from the leading bytes of a file.
   Returns \"image/png\" | \"image/jpeg\" | \"image/gif\" | \"image/webp\" |
   \"image/bmp\" | \"image/svg+xml\", or nil when the bytes are not a supported
   still image. BMP and SVG are not wire-legal themselves — they are readable,
   and `image-convert` turns them into PNG before they are sent.
   Animated PNGs and JPEG-LS return nil (provider-rejected shapes)."
  [^bytes b]
  (cond (bytes-at? b 0 [0xff 0xd8 0xff]) (when-not (and (>= (alength b) 4) (= 0xf7 (u8 b 3)))
                                           "image/jpeg")
        (bytes-at? b 0 png-signature) (when (and (png? b) (not (animated-png? b))) "image/png")
        (ascii-at? b 0 "GIF8") "image/gif"
        (and (ascii-at? b 0 "RIFF") (ascii-at? b 8 "WEBP")) "image/webp"
        (and (ascii-at? b 0 "BM") (bmp? b)) "image/bmp"
        (svg-markup? b) "image/svg+xml"
        :else nil))

(def ^:private still-image-brands
  "ISO-BMFF major brands that are a STILL PICTURE, not a clip. HEIF/AVIF share
   MP4's `ftyp` container header, so an iPhone photo looks exactly like a video
   to a naive `ftyp` check and would be pushed down the decode path as one.
   Mirrors `TerminalImage/STILL_IMAGE_BRANDS` in the Lanterna fork so the
   terminal and the engine classify the same bytes identically."
  #{"heic" "heix" "heim" "heis" "hevc" "hevx" "mif1" "msf1" "avif" "avis"})

(defn- brand-at
  "The 4-char ASCII box brand at `off`, lower-cased, or nil when the head is
   too short."
  [^bytes b ^long off]
  (when (>= (alength b) (+ off 4))
    (str/lower-case (String. b (int off) 4 StandardCharsets/US_ASCII))))

(defn detect-video-mime
  "Sniff a supported video MIME type from the leading bytes of a file.
   Returns \"video/mp4\" | \"video/quicktime\", or nil.

   ISO-BMFF only (`ftyp` at offset 4); the major brand at offset 8 decides.
   `qt  ` is QuickTime, a [[still-image-brands]] brand is a PHOTO (nil, never a
   clip), anything else is MP4."
  [^bytes b]
  (when (and (>= (alength b) 12) (ascii-at? b 4 "ftyp"))
    (let [brand (brand-at b 8)]
      (when-not (contains? still-image-brands brand)
        (if (= "qt  " brand) "video/quicktime" "video/mp4")))))

(def video-media-types
  "Video containers vis stores. No wire takes any of them verbatim: a clip is
   transcoded to an animated GIF at SEND time (see [[wire-verdict]]), which is
   why storing one is safe even though sending the container never is."
  #{"video/mp4" "video/quicktime"})

(defn video-media-type?
  "True when `media-type` is one of [[video-media-types]]."
  [media-type]
  (contains? video-media-types (str/lower-case (str/trim (str media-type)))))

(defn detect-media-mime
  "The sniffed type of anything vis can attach: [[detect-image-mime]] first,
   then [[detect-video-mime]]. Stills win the tie deliberately -- HEIF/AVIF
   photos share the MP4 container header."
  [^bytes b]
  (or (detect-image-mime b) (detect-video-mime b)))

(def provider-image-media-types
  "The ONLY image media types a vision wire accepts VERBATIM. Anthropic names
   exactly these four in its rejection (`the image data you provided does not
   represent a valid image … supported image formats: ['image/jpeg',
   'image/png', 'image/gif', 'image/webp']`); OpenAI and Gemini are supersets.
   Anything else — an `image/svg+xml` figure from `vis_attach`/matplotlib, a
   BMP screenshot — is a hard 400, and since attachments
   REPLAY on every later turn ONE such row kills the whole session."
  #{"image/jpeg" "image/png" "image/gif" "image/webp"})

(defn provider-image-media-type?
  "True when `media-type` is one of [[provider-image-media-types]]."
  [media-type]
  (contains? provider-image-media-types (str/lower-case (str/trim (str media-type)))))

(defn unsupported-media-reason
  "Why an otherwise readable image was not attached, in the user's words."
  [media-type]
  (str media-type " is not a provider-supported image format (JPEG, PNG, GIF, WebP)"))


(defn- sniff-file-mime
  "Read the file head and sniff its MIME type. nil on any read failure."
  [^File f]
  (try (with-open [raf (RandomAccessFile. f "r")]
         (let
           [n (int (min (.length raf) (long sniff-bytes)))
            buf (byte-array n)]

           (.readFully raf buf)
           (detect-media-mime buf)))
       (catch Throwable _ nil)))

;; =============================================================================
;; Path-candidate extraction
;; =============================================================================

(def ^:private media-extension-pattern
  "Cheap pre-filter before any filesystem access: only tokens that END in
   an image or video extension are stat'd. The magic-byte sniff still owns
   the final verdict."
  #"(?i)\.(png|jpe?g|gif|webp|bmp|svg|mp4|m4v|mov)$")

(def ^:private media-extension-present-pattern
  "Whole-text fast path: a single unanchored scan that answers \"could this
   message mention ANY image or video file at all?\". When it misses we skip
   tokenization + per-token filtering entirely -- turning a large paste (a
   90KB log, thousands of tokens) from a ~10ms multi-regex walk into one
   ~0.3ms linear scan. Deliberately looser than [[media-extension-pattern]]
   (no end anchor): a hit only means \"keep looking\", the anchored per-token
   pattern and the magic-byte sniff still own the real verdict, so the
   loose match can never let a non-media file through."
  #"(?i)\.(?:png|jpe?g|gif|webp|bmp|svg|mp4|m4v|mov)")

(def ^:private quoted-span-pattern
  "Single- or double-quoted spans — several terminals quote dropped paths
   that contain spaces."
  #"'([^']+)'|\"([^\"]+)\"")

(def ^:private escaped-token-pattern
  "Whitespace-delimited token honoring backslash escapes — the macOS
   Terminal/iTerm drop shape (`/Users/x/My\\ Shot.png`)."
  #"(?:[^\s\\]|\\.)+")

(defn- unescape-token [token] (str/replace token #"\\(.)" "$1"))

(defn- strip-file-url
  "`file://` URLs (some apps drop those) → percent-decoded plain path."
  [s]
  (if (str/starts-with? s "file://")
    (try (.getPath (java.net.URI. s)) (catch Throwable _ (subs s (count "file://"))))
    s))

(defn- strip-edge-punct
  "Drop sentence / enclosure punctuation clinging to an UNQUOTED path token:
   trailing `.,;:!?` and closing `)]}>'\"`, plus a matching LEADING opener
   `([{<'\"`. Prose routinely wraps or trails a pasted path (`(foo.png)`,
   `foo.png.`); without this the end-anchored extension match in
   `resolve-candidate` misses and the image never attaches / never reaches the
   vision model. Quoted spans are verbatim and skip this."
  [^String s]
  (-> s
      (str/replace #"^[(\[{<'\"]+" "")
      (str/replace #"[.,;:!?)\]}>'\"]+$" "")))

(def ^:private media-path-token-pattern
  "One media-shaped PATH token as it appears IN prose: a quoted span or a
   whitespace-delimited token ending in an image or video extension.
   Non-capturing throughout so `str/replace` hands the matcher fn a plain
   string."
  #"(?i)(?:\"[^\"]*\.(?:png|jpe?g|gif|webp|bmp|mp4|m4v|mov)\"|'[^']*\.(?:png|jpe?g|gif|webp|bmp|mp4|m4v|mov)'|\S+\.(?:png|jpe?g|gif|webp|bmp|mp4|m4v|mov))")

(defn- path-candidates
  "Raw path-shaped candidates from user text, drop-pattern aware:
   quoted spans first (verbatim content), then escape-honoring tokens —
   each also yielding an edge-punctuation-trimmed variant so a path
   followed by sentence punctuation still resolves. Order preserved;
   duplicates collapse later on the canonical path."
  [text]
  (concat (keep (fn [[_ single double*]]
                  (or single double*))
                (re-seq quoted-span-pattern text))
          (mapcat (fn [tok]
                    (let
                      [tok
                       (unescape-token tok)

                       trimmed
                       (strip-edge-punct tok)]

                      (if (= tok trimmed) [tok] [tok trimmed])))
                  (re-seq escaped-token-pattern text))))

(defn- resolve-candidate
  "Candidate string → existing readable regular `File` with an image
   extension, or nil. Relative candidates resolve against
   `workspace-root` (falling back to cwd)."
  ^File [candidate workspace-root]
  (let
    [^String s (-> candidate
                   str/trim
                   strip-file-url
                   paths/expand-home)]
    (when (and (seq s) (re-find media-extension-pattern s))
      (let
        [f (File. s)
         f
         (if (.isAbsolute f) f (File. (str (or workspace-root (System/getProperty "user.dir"))) s))]

        (when (and (.isFile f) (.canRead f)) f)))))

;; =============================================================================
;; Collection
;; =============================================================================

(defn size-label
  [^long n]
  (cond (>= n (* 1024 1024)) (format "%.1fMB" (/ (double n) (* 1024.0 1024.0)))
        (>= n 1024) (format "%.0fKB" (/ (double n) 1024.0))
        :else (str n "B")))

(defn- oversize-skip
  [^File f ^long limit]
  {:path (.getAbsolutePath f)
   :reason (str (size-label (.length f)) " exceeds the " (size-label limit) " attachment limit")})

(defn- storable-limit
  "Byte ceiling for STORING one attachment of `media-type`. A still is allowed
   `upload-rescue-factor` x the per-image cap ([[max-upload-image-bytes]] by
   default): the per-image cap is a PROVIDER limit, and an oversize still is
   squeezed under it at SEND time rather than refused at intake. A container
   that is rasterized on the way OUT (BMP, SVG) is allowed
   `oversize-rescue-factor` x that cap, because rendering it routinely lands far
   under it -- and the cap that decides is the one applied to the bytes that
   actually go on the wire (see [[wire-image]]).

   A CLIP answers to [[max-video-bytes]] instead: no clip ever reaches a
   provider in its own container -- it leaves as a small GIF, which is the
   payload the cap then judges."
  ^long [media-type ^long max-bytes]
  (cond (provider-image-media-type? media-type) (* (long upload-rescue-factor) max-bytes)
        (video-media-type? media-type) max-video-bytes
        :else (* (long oversize-rescue-factor) max-bytes)))

(defn- attach-file
  "Read one image file and keep it EXACTLY as it sits on disk: original bytes,
   sniffed media type, no conversion, no resample, no re-compression.

   Container adaptation (BMP/SVG -> PNG) and decode verification happen at SEND
   time (see [[wire-image]]), never here: a stored row must not be frozen into
   one provider's shape, and a row that turns out to be unsendable has to be
   droppable on the way out — a bad row baked into the DB replays forever.

   Returns the attachment map, or `{:reason <why>}` when the file is too big to
   store (see [[storable-limit]])."
  [^File f mime ^long max-bytes]
  (let
    [raw
     (Files/readAllBytes (.toPath f))

     size
     (alength raw)

     limit
     (storable-limit mime max-bytes)]

    (if (> size limit)
      {:reason (str (size-label size) " exceeds the " (size-label limit) " attachment limit")}
      {:path (.getAbsolutePath f)
       :filename (.getName f)
       :media-type mime
       :base64 (.encodeToString (Base64/getEncoder) raw)
       :size size
       :size-label (size-label size)})))

(defn- resolved-media-files
  "Ordered, de-duped `[canonical-path File]` pairs for every path-shaped
   token in `text` that resolves to a readable image- or video-extension file.
   The magic-byte sniff still owns the final verdict downstream.

   Fast path: if the whole message contains no media-extension substring
   at all we return `[]` after a single linear scan, skipping the
   tokenization + per-candidate filesystem work that dominates the cost on
   large non-media pastes (the common case on every turn)."
  [text workspace-root]
  (let [s (str text)]
    (if-not (re-find media-extension-present-pattern s)
      []
      (into []
            (comp (keep #(resolve-candidate % workspace-root))
                  (map (fn [^File f]
                         [(.getCanonicalPath f) f]))
                  (distinct))
            (path-candidates s)))))

(defn scan-image-descriptors
  "Resolve every image the user text points at, WITHOUT loading pixel bytes.
   Returns `[{:path :media-type :size :size-label :filename}]` for files whose
   magic bytes sniff to a supported still image — ordered, de-duped. Cheap
   enough to run on every paste (only a small file-head read per candidate).
   Never throws."
  ([text] (scan-image-descriptors text {}))
  ([text {:keys [workspace-root]}]
   (if (str/blank? (str text))
     []
     (into []
           (keep (fn [[_canonical ^File f]]
                   (try (when-let [mime (sniff-file-mime f)]
                          {:path (.getAbsolutePath f)
                           :media-type mime
                           :size (.length f)
                           :size-label (size-label (.length f))
                           :filename (.getName f)})
                        (catch Throwable _ nil))))
           (resolved-media-files text workspace-root)))))

(defn collect-user-images
  "Scan `text` (one user message) for paths of readable image files and
   load them as attachments.

   Options:
     :workspace-root - base for relative candidates (default: cwd).
     :max-bytes      - per-image cap (default [[max-image-bytes]]).
     :max-images     - attachment count cap (default [[max-image-count]]).

   Returns `{:attached [{:path :media-type :base64 :size :size-label}]
             :skipped  [{:path :reason}]}` — `:skipped` names sniffed
   image files that were rejected (size cap / count cap) so the prompt
   assembler can tell the model WHY an image it can see referenced is
   absent. Non-image or non-existent candidates are silently ignored.
   Never throws — a failure to read one file skips that file."
  ([text] (collect-user-images text {}))
  ([text
    {:keys [workspace-root max-bytes max-images]
     :or {max-bytes max-image-bytes max-images max-image-count}}]
   (if (str/blank? (str text))
     {:attached [] :skipped []}
     (let [files (resolved-media-files text workspace-root)]
       (reduce
         (fn [acc [_canonical ^File f]]
           (try
             (if-let [mime (sniff-file-mime f)]
               (cond
                 (> (.length f) (storable-limit mime (long max-bytes)))
                 (update acc :skipped conj (oversize-skip f (storable-limit mime (long max-bytes))))
                 (>= (count (:attached acc)) (long max-images))
                 (update acc
                         :skipped
                         conj
                         {:path (.getAbsolutePath f)
                          :reason
                          (str "attachment limit of " max-images " images per message reached")})
                 :else
                 (let [res (attach-file f mime (long max-bytes))]
                   (if (:reason res)
                     (update acc :skipped conj {:path (.getAbsolutePath f) :reason (:reason res)})
                     (update acc :attached conj res))))
               acc)
             (catch Throwable _ acc)))
         {:attached [] :skipped []}
         files)))))

(defn- strip-data-url-prefix
  "Drop a `data:<mime>;base64,` prefix if the payload arrived as a data URL."
  [^String s]
  (if (str/starts-with? s "data:")
    (let [i (.indexOf s ",")]
      (if (neg? i) s (subs s (inc i))))
    s))

(defn prepare-inline-attachments
  "Validate already-encoded image attachments delivered INLINE (web/API upload)
   rather than as filesystem paths. Each entry is `{:base64 :filename :media-type?}`;
   the base64 may be a bare payload or a `data:...;base64,` URL. Decodes each,
   sniffs the MIME from magic bytes (the declared `:media-type` is NEVER trusted),
   enforces the same caps as [[collect-user-images]], and returns the same
   `{:attached [...] :skipped [...]}` shape so the assemble seam treats
   disk-scanned and inline images uniformly. Never throws.

   Like [[collect-user-images]] it stores the ORIGINAL payload under its SNIFFED
   container and converts nothing: what a provider will accept is only knowable
   at SEND time, against that turn's model (see [[wire-image]])."
  ([attachments] (prepare-inline-attachments attachments {}))
  ([attachments
    {:keys [max-bytes max-images] :or {max-bytes max-image-bytes max-images max-image-count}}]
   (reduce
     (fn [acc att]
       (try
         (let
           [^String payload
            (strip-data-url-prefix (str (or (:base64 att) (get att "base64"))))

            label
            (or (not-empty (str (or (:filename att) (get att "filename")))) "image")

            ^bytes raw
            (.decode (Base64/getDecoder) payload)

            mime
            (detect-media-mime raw)

            size
            (alength raw)]

           (cond (nil? mime)
                 (update acc :skipped conj {:path label :reason "not a supported image or video"})
                 (> size (storable-limit mime (long max-bytes)))
                 (update acc
                         :skipped
                         conj
                         {:path label
                          ;; The EFFECTIVE ceiling, not the per-image cap: a clip
                          ;; answers to `max-video-bytes`, so quoting `max-bytes`
                          ;; here would tell the user 5 MB when 32 MB was allowed.
                          :reason (str (size-label size)
                                       " exceeds the "
                                       (size-label (storable-limit mime (long max-bytes)))
                                       " attachment limit")})
                 (>= (count (:attached acc)) (long max-images))
                 (update acc
                         :skipped
                         conj
                         {:path label
                          :reason
                          (str "attachment limit of " max-images " images per message reached")})
                 :else (update acc
                               :attached
                               conj
                               {:path label
                                :filename label
                                :media-type mime
                                :base64 payload
                                :size size
                                :size-label (size-label size)})))
         (catch Throwable _ acc)))
     {:attached [] :skipped []}
     (or attachments []))))

(defn text->inline-chips
  "`text` with every image-shaped PATH token replaced by a short `name.png`
   chip, and NOTHING else touched — line breaks, indentation and the prose around
   each path all survive.

   Pure string work: no filesystem access, no pixel bytes, so it is safe on a
   paint path and on text whose files have since been deleted. Use it wherever a
   user message is DISPLAYED (transcript bubble, notification body); the original
   text is never mutated, so re-send/edit still ships the path that re-attaches
   the picture."
  [text]
  (let [s (str (or text ""))]
    (if-not (re-find media-extension-present-pattern s)
      s
      (str/replace s
                   media-path-token-pattern
                   (fn [m]
                     (let
                       [clean (-> (str m)
                                  (str/replace #"^[\"'(\[{<]+" "")
                                  (str/replace #"[\"')\]}>.,;:!?]+$" "")
                                  unescape-token
                                  strip-file-url)
                        base (last (str/split clean #"[/\\\\]"))]

                       (if (str/blank? (str base)) clean base)))))))

(defn text->chip-preview
  "One-line, path-free rendering of ONE user message for compact previews
   (queue rows, tab titles, notifications).

   Every image-shaped path token collapses to a short `name.png` chip (see
   `text->inline-chips`), the prose around it survives, and whitespace collapses
   to single spaces. Pure string work — no filesystem access, no pixel bytes — so
   it is safe on a paint path and on text whose files have since been deleted.
   That is the whole point: a queued message authored by dropping a screenshot
   used to render as a raw `/var/folders/…/clipboard-….png`, which tells the user
   nothing about what they queued.

   Returns nil when the message is nothing but images — the caller should paint
   attachment chips alone rather than an empty row. The ORIGINAL text is never
   mutated; callers keep it for re-send/edit so the paths still re-attach."
  [text]
  (not-empty (str/trim (str/replace (text->inline-chips text) #"\s+" " "))))

;; =============================================================================
;; Send-time wire gate
;; =============================================================================

(def ^:private no-vision-reason
  "Why an image the user really attached is nevertheless not on the wire."
  "the active model has no vision — image not attached")

(def ^:private user-only-reason
  "Why an image that decoded perfectly is nevertheless not on the wire."
  "attached for the human only — image kept on disk, not sent to the model")

(def audiences
  "The CLOSED vocabulary of an attachment's AUDIENCE — who the artifact is for:

     * `\"both\"` (the default) paints it for the human AND sends the image to
       the model.
     * `\"user\"`  paints it for the human and keeps the bytes off the wire.
     * `\"model\"` sends it to the model and paints nothing for the human.

   One word, three routes; there is no fourth and no second copy of this table."
  #{"both" "user" "model"})

(defn normalize-audience
  "Coerce whatever the shim bridge, a wire payload or a DB row carries into a
   member of [[audiences]], defaulting to `\"both\"`. An unknown word falls back
   to the PERMISSIVE default: a typo must not silently hide someone's figure."
  [v]
  (let [s (str/lower-case (str/trim (str v)))]
    (if (contains? audiences s) s "both")))

(defn attachment-audience
  "This attachment's audience word, normalized — `\"both\"` when unstamped."
  [attachment]
  (normalize-audience (:audience attachment)))

(defn hidden-from-model?
  "True when this attachment is for the HUMAN only (`audience=\"user\"`): shown in
   the UI and stored in the session DB, but deliberately never rendered as an
   image block.

   The escape hatch for the one thing multimodal replay cannot undo — an image
   replays IN FULL on every later request, so a screenshot the model does not
   actually need is re-billed forever. `vis_attach(..., audience='user')` stamps
   `:audience \"user\"`, and the send-time gate routes the row to `:skipped` +
   `:readable-blind?` instead: the model is TOLD the file exists and can open the
   bytes on demand (`vis_read_attachment`) or ask for it back with
   `vis_reinspect_attachment`."
  [attachment]
  (= "user" (attachment-audience attachment)))

(defn hidden-from-user?
  "True when this attachment is for the MODEL only (`audience=\"model\"`): the
   bytes ride the request and the human's transcript stays clean. The twin of
   [[hidden-from-model?]], and the reason a run can gather evidence for itself
   without burying the human in thumbnails it never asked to review."
  [attachment]
  (= "model" (attachment-audience attachment)))

(defn- media-candidate?
  "True when an attachment CLAIMS a still image or a clip and is therefore worth
   decoding at send time. A generic `vis_attach` artifact (csv/json/pdf/wav/...)
   is DB- and display-only and must never cost a base64 decode on every turn,
   and a BLANK media type never counts: unverifiable bytes an image block would
   have to label `image/png` are exactly the thing that bricks a session."
  [media-type]
  (let [mt (str/lower-case (str/trim (str media-type)))]
    (and (not (str/blank? mt))
         (or (str/starts-with? mt "image/")
             (str/starts-with? mt "video/")
             (image-convert/svg-media-type? mt)))))

(def ^:private wire-cache-max-entries "Distinct payloads whose send verdict is remembered." 64)

(def ^:private wire-cache-max-bytes
  "Total base64 the verdict cache may hold. Only CONVERTED payloads cost
   anything: a payload that went out untouched is remembered as a verdict alone
   and re-uses the caller's own string."
  (* 48 1024 1024))

(defonce ^:private wire-cache
  ;; {:order <insertion queue of keys> :entries {k verdict} :bytes <cached b64>}
  ;; Attachments REPLAY on every later turn, so an uncached gate would re-decode
  ;; (and re-rasterize) every figure in the session on every single request.
  ;; Content-keyed, so eviction only ever costs a recompute, never correctness.
  (atom {:order clojure.lang.PersistentQueue/EMPTY :entries {} :bytes 0}))

(defn- content-key
  "Cache key: the payload's own content, plus everything the verdict depends on."
  [^String payload media-type ^long max-bytes ^long max-dimension]
  (let
    [md
     (MessageDigest/getInstance "SHA-256")

     digest
     (.digest md (.getBytes payload StandardCharsets/UTF_8))]

    (str (.encodeToString (Base64/getUrlEncoder) digest)
         "|" media-type
         "|" max-bytes
         "|" max-dimension)))

(defn- cache-put!
  [k verdict]
  (swap! wire-cache (fn [cache]
                      (loop
                        [order
                         (conj (:order cache) k)

                         entries
                         (assoc (:entries cache) k verdict)

                         total
                         (+ (long (:bytes cache)) (long (count (str (:base64 verdict)))))]

                        (if (and (seq order)
                                 (or (> (count entries) (long wire-cache-max-entries))
                                     (> total (long wire-cache-max-bytes))))
                          (let [oldest (peek order)]
                            (recur (pop order)
                                   (dissoc entries oldest)
                                   (- total
                                      (long (count (str (get-in entries [oldest :base64])))))))
                          {:order order :entries entries :bytes total}))))
  verdict)

(defn- base64-length
  "How many CHARACTERS the base64 encoding of `n` raw bytes occupies -- 4 per 3,
   padded to a multiple of 4. This, not the decoded length, is the number a
   provider measures the payload by."
  ^long [^long n]
  (* 4 (quot (+ n 2) 3)))

(defn- wire-byte-budget
  "The largest RAW byte count whose base64 encoding still fits `max-bytes`.

   Every wire carries an inline image as base64 and enforces its size cap on
   THAT string, not on the picture it decodes to:

     messages.0.content.1.image.source.base64: image exceeds 5 MB maximum:
     5994492 bytes > 5242880 bytes

   Base64 costs 4 characters per 3 bytes, so a 4.5MB PNG -- comfortably under a
   5MB cap by every local measure -- leaves as a 6MB payload and is refused.
   And since attachments REPLAY, that 400 comes back on every later turn,
   including plain-text ones, until the row leaves the session. So the byte
   ladder is aimed at 3/4 of the cap and the picture really fits."
  ^long [^long max-bytes]
  (max 0 (* 3 (quot max-bytes 4))))

(defn- video-verdict
  "The send verdict for a CLIP, which no vision wire accepts in any container.
   The clip is sampled across its whole length into a small animated GIF
   ([[com.blockether.vis.internal.image-convert/video->wire-gif]]) -- a format
   every provider reads as an image -- so a dropped screen recording is
   something the model can actually WATCH instead of a filename it is told
   about. Refused only when the track cannot be decoded (HEVC/AV1/VP9, a
   corrupt container) or the GIF still does not fit."
  [^bytes raw media-type ^long max-bytes]
  (let
    [gif
     (image-convert/video->wire-gif raw nil)

     ;; The cap is on the BASE64 payload, not on the GIF ([[wire-byte-budget]]).
     budget
     (wire-byte-budget max-bytes)]

    (cond (nil? gif) {:reason (str media-type " cannot be turned into a picture on this build")}
          (:reason gif) {:reason (:reason gif)}
          :else (let [fitted (image-convert/fit-within ^bytes (:bytes gif) budget)]
                  (if (> (alength ^bytes fitted) budget)
                    {:reason (str (size-label (base64-length (alength ^bytes fitted)))
                                  " exceeds the "
                                  (size-label max-bytes)
                                  " attachment limit")}
                    {:media-type "image/gif"
                     :size (alength ^bytes fitted)
                     :base64 (.encodeToString (Base64/getEncoder) ^bytes fitted)})))))

(defn- wire-verdict
  "The send verdict for ONE payload, uncached.

   `{:media-type <mt> :size <n>}` — send the caller's own base64, untouched.
   `{:media-type <mt> :size <n> :base64 <b64>}` — send THIS instead (converted).
   `{:reason <why>}` — do not send it, and tell whoever asks why."
  [^String payload declared ^long max-bytes ^long max-dimension]
  (let [raw (try (.decode (Base64/getDecoder) payload) (catch Throwable _ nil))]
    (if (or (nil? raw) (zero? (alength ^bytes raw)))
      {:reason "the attachment carries no readable image bytes"}
      (let
        [;; The BYTES decide the container; a declared type is a claim, and the
         ;; sniff covers everything vis stores. A type the sniff does not know
         ;; (tiff, heic, an inline upload's own label) still reaches the decoder
         ;; below, which sniffs for itself.
         mt (or (detect-media-mime raw) declared)
         ;; A clip takes its own path: no wire reads a video container, and
         ;; handing these bytes to the STILL decoder is a wasted full read of
         ;; every byte the user dropped.
         video? (video-media-type? mt)
         safe (when-not video? (image-convert/to-provider-safe raw mt))
         ;; PIXELS are a provider limit of their own, and the one a byte cap
         ;; never catches: a well-compressed 4K screenshot sits happily under
         ;; 5MB and is still refused outright once the request carries many
         ;; images. Clamped BEFORE the byte ladder — fewer pixels is also fewer
         ;; bytes — and a picture already within the ceiling comes back ITSELF.
         scaled (some-> safe
                        :bytes
                        (image-convert/fit-dimensions max-dimension))
         ;; What a provider weighs is the BASE64 string, which is 4/3 of the
         ;; picture ([[wire-byte-budget]]), so the ladder aims at 3/4 of the cap.
         budget (wire-byte-budget max-bytes)
         ;; Over the cap is NOT the same as unsendable. The real optimisers get
         ;; one shot at it (lossless first, then the imaging library's own
         ;; ladder) instead of the picture being dropped; a payload that already
         ;; fits comes back IDENTICAL, so nothing under the cap is re-encoded.
         fitted (some-> ^bytes (:bytes scaled)
                        (image-convert/fit-within budget))]

        (cond video? (video-verdict raw mt max-bytes)
              ;; Conversion unavailable (a build with no imaging cdylib): a container
              ;; the wire already accepts still goes out unverified — best effort
              ;; beats a blind turn — and nothing else can be made sendable at all.
              (nil? safe) (if (provider-image-media-type? mt)
                            {:media-type mt :size (alength ^bytes raw)}
                            {:reason (unsupported-media-reason mt)})
              (not (provider-image-media-type? (:media-type safe)))
              {:reason (or (:reason safe) (unsupported-media-reason mt))}
              (:reason scaled) {:reason (:reason scaled)}
              (> (alength ^bytes fitted) budget)
              {:reason (str (size-label (base64-length (alength ^bytes fitted)))
                            " exceeds the "
                            (size-label max-bytes)
                            " attachment limit")}
              ;; Byte-identical: the payload was already wire-safe AND decoded, so
              ;; the caller's base64 is reused rather than re-encoded.
              (identical? fitted raw) {:media-type (:media-type safe) :size (alength ^bytes raw)}
              :else {:media-type (or (detect-image-mime fitted) (:media-type safe))
                     :size (alength ^bytes fitted)
                     :base64 (.encodeToString (Base64/getEncoder) ^bytes fitted)})))))

(defn wire-image
  "ONE stored attachment as the wire will actually carry it — the single gate
   every image crosses on its way to a provider, applied at SEND time.

   Send time is the only correct layer. Storage is permanent and providers are
   not: the model that will read a row is unknown when the row is written, an
   attachment REPLAYS on every later turn, and a payload converted (or blessed)
   on the way IN can never be reconsidered — which is exactly how one corrupt
   83-byte PNG, wire-legal by media type and garbage in its `IDAT`, bricked a
   whole session with a permanent `Could not process image` 400. Here the same
   row is re-judged every turn, so a bad one is DROPPED instead of fatal and the
   session heals itself.

   Every container is handled in one place:
     * JPEG / PNG / GIF / WebP — decoded to prove they are pixels, then sent
       BYTE-IDENTICAL (a header sniff is not a picture)
     * SVG / SVGZ            — rasterized to PNG (no wire reads markup)
     * MP4 / QuickTime       — sampled into an animated GIF (no wire takes a clip)
     * BMP, TIFF, anything else the decoder reads — re-containered to PNG
     * HEIC/AVIF, a corrupt raster, an image past the decoder's limits —
       refused, with the decoder's own words
     * any side over `max-dimension` — DOWNSCALED to fit, because a provider
       refuses an oversized picture outright once a request carries many images
       (`image-convert/max-wire-dimension`), whatever it weighs
     * anything whose BASE64 payload passes `max-bytes` — OPTIMIZED first (the
       imaging library's oxipng / jpegtran / gifsicle pass, then its bounded
       ladder) and refused only when even that does not fit. The cap is on the
       encoded string every wire actually weighs, so the picture is held to 3/4
       of it
     * a non-image artifact (csv/pdf/wav/…) — nil: not an image, not a failure

   Returns the attachment with `:media-type`/`:base64`/`:size`/`:size-label`
   replaced by the WIRE payload, `{:path :filename :reason}` when it cannot be
   sent, or nil when it is not an image at all. Never throws."
  ([attachment] (wire-image attachment {}))
  ([{:keys [media-type base64] :as attachment}
    {:keys [max-bytes max-dimension]
     :or {max-bytes max-image-bytes max-dimension image-convert/max-wire-dimension}}]
   (let
     [declared
      (str/lower-case (str/trim (str media-type)))

      payload
      (strip-data-url-prefix (str base64))]

     (when (and (media-candidate? declared) (not (str/blank? payload)))
       (let
         [k
          (content-key payload declared (long max-bytes) (long max-dimension))

          verdict
          (or (get-in @wire-cache [:entries k])
              (cache-put! k (wire-verdict payload declared (long max-bytes) (long max-dimension))))]

         (if (:reason verdict)
           (assoc (select-keys attachment [:path :filename]) :reason (:reason verdict))
           (let [size (long (:size verdict))]
             (assoc attachment
               :media-type (:media-type verdict)
               :base64 (or (:base64 verdict) payload)
               :size size
               :size-label (size-label size)))))))))

(defn wire-images
  "[[wire-image]] over a whole user message's attachments, keeping the
   `{:attached [...] :skipped [{:path :reason}]}` shape the prompt manifest
   speaks — so an image that could not be sent is NAMED to the model instead of
   vanishing.

   `:vision?` false (a text-only target: Copilot without vision, glm-5-turbo,
   deepseek …) attaches nothing and marks every image `:readable-blind?`: the
   files are real and on disk, so the manifest can tell the model to open them
   with an imaging library instead of hunting for blocks that are not there.

   A HUMAN-ONLY row ([[hidden-from-model?]]) takes that same blind path even on a
   vision model, and is checked FIRST: the caller asked for the bytes to stay
   off the wire, and that beats every capability question."
  ([images] (wire-images images {}))
  ([images {:keys [vision?] :or {vision? true} :as opts}]
   (reduce
     (fn [acc {:keys [path filename media-type] :as att}]
       (let [label (or (not-empty (str path)) (not-empty (str filename)) "image")]
         (cond
           (hidden-from-model? att)
           (update acc :skipped conj {:path label :reason user-only-reason :readable-blind? true})
           (not vision?)
           (update acc :skipped conj {:path label :reason no-vision-reason :readable-blind? true})
           :else
           (let [wired (wire-image att opts)]
             (cond (nil? wired) (update acc
                                        :skipped
                                        conj
                                        {:path label :reason (unsupported-media-reason media-type)})
                   (:reason wired) (update acc :skipped conj {:path label :reason (:reason wired)})
                   :else (update acc :attached conj wired))))))
     {:attached [] :skipped []}
     (or images []))))
