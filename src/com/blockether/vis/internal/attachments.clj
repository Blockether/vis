(ns com.blockether.vis.internal.attachments
  "User-message image attachments.

   Dropping a file onto the terminal pastes its PATH into the input (the
   terminal's drop behavior — same mechanism pi relies on). At turn start
   the engine scans the user message for path-shaped tokens that resolve
   to real image files, reads them, and attaches them to the initial user
   message as multimodal content blocks. Channel-neutral: TUI, web, and
   Telegram all get the same behavior because the scan runs in the engine,
   not the channel.

   Only files the model can genuinely consume are attached: the MIME type
   is sniffed from magic bytes (pi-parity: jpeg / non-animated png / gif /
   webp / bmp), never trusted from the extension alone. Files over
   `max-image-bytes` are skipped with a reason the prompt assembler
   surfaces to the model (providers downscale server-side; vis does no
   client-side resizing — AWT/ImageIO is unavailable under GraalVM
   native-image on macOS)."
  (:require [clojure.string :as str])
  (:import [java.io File RandomAccessFile]
           [java.nio.file Files]
           [java.util Base64]))

;; =============================================================================
;; Limits
;; =============================================================================

(def max-image-bytes
  "Per-image byte cap. Anthropic's API limit is 5MB/image; OpenAI allows
   more, but the smallest common bound keeps one attachment valid on
   every wire."
  (* 5 1024 1024))

(def max-image-count
  "Attachment count cap per user message. Guards against a pathological
   message (e.g. a pasted directory listing) ballooning the request."
  8)

(def ^:private sniff-bytes
  "Bytes read from the file head for MIME sniffing (pi parity: enough for
   the PNG chunk walk that rejects animated PNGs)."
  4100)

;; =============================================================================
;; Magic-byte MIME sniffing (pi utils/mime.ts parity)
;; =============================================================================

(defn- u8 [^bytes b ^long i] (bit-and (long (aget b i)) 0xff))

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
      (let [chunk-length (u32-be b offset)
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
       (let [declared-size
             (u32-le b 2)

             pixel-data-offset
             (u32-le b 10)

             dib-header-size
             (u32-le b 14)]

         (and (or (zero? declared-size) (>= declared-size 26))
              (>= pixel-data-offset (+ 14 dib-header-size))
              (or (zero? declared-size) (< pixel-data-offset declared-size))
              (let [[planes bpp] (cond (= dib-header-size 12) [(u16-le b 22) (u16-le b 24)]
                                       (<= 40 dib-header-size 124) [(u16-le b 26) (u16-le b 28)]
                                       :else [nil nil])]
                (and (= 1 planes) (contains? #{1 4 8 16 24 32} bpp)))))))

(defn detect-image-mime
  "Sniff a supported image MIME type from the leading bytes of a file.
   Returns \"image/png\" | \"image/jpeg\" | \"image/gif\" | \"image/webp\" |
   \"image/bmp\", or nil when the bytes are not a supported still image.
   Animated PNGs and JPEG-LS return nil (provider-rejected shapes)."
  [^bytes b]
  (cond (bytes-at? b 0 [0xff 0xd8 0xff]) (when-not (and (>= (alength b) 4) (= 0xf7 (u8 b 3)))
                                           "image/jpeg")
        (bytes-at? b 0 png-signature) (when (and (png? b) (not (animated-png? b))) "image/png")
        (ascii-at? b 0 "GIF8") "image/gif"
        (and (ascii-at? b 0 "RIFF") (ascii-at? b 8 "WEBP")) "image/webp"
        (and (ascii-at? b 0 "BM") (bmp? b)) "image/bmp"
        :else nil))

(defn- sniff-file-mime
  "Read the file head and sniff its MIME type. nil on any read failure."
  [^File f]
  (try (with-open [raf (RandomAccessFile. f "r")]
         (let [n (int (min (.length raf) (long sniff-bytes)))
               buf (byte-array n)]

           (.readFully raf buf)
           (detect-image-mime buf)))
       (catch Throwable _ nil)))

;; =============================================================================
;; Path-candidate extraction
;; =============================================================================

(def ^:private image-extension-pattern
  "Cheap pre-filter before any filesystem access: only tokens that END in
   an image extension are stat'd. The magic-byte sniff still owns the
   final verdict."
  #"(?i)\.(png|jpe?g|gif|webp|bmp)$")

(def ^:private image-extension-present-pattern
  "Whole-text fast path: a single unanchored scan that answers \"could this
   message mention ANY image file at all?\". When it misses we skip
   tokenization + per-token filtering entirely — turning a large paste (a
   90KB log, thousands of tokens) from a ~10ms multi-regex walk into one
   ~0.3ms linear scan. Deliberately looser than [[image-extension-pattern]]
   (no end anchor): a hit only means \"keep looking\", the anchored per-token
   pattern and the magic-byte sniff still own the real verdict, so the
   loose match can never let a non-image through."
  #"(?i)\.(?:png|jpe?g|gif|webp|bmp)")

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

(defn- path-candidates
  "Raw path-shaped candidates from user text, drop-pattern aware:
   quoted spans first (verbatim content), then escape-honoring tokens.
   Order preserved; duplicates collapse later on the canonical path."
  [text]
  (concat (keep (fn [[_ single double*]]
                  (or single double*))
                (re-seq quoted-span-pattern text))
          (map unescape-token (re-seq escaped-token-pattern text))))

(defn- expand-home
  [s]
  (cond (= s "~") (System/getProperty "user.home")
        (str/starts-with? s "~/") (str (System/getProperty "user.home") (subs s 1))
        :else s))

(defn- resolve-candidate
  "Candidate string → existing readable regular `File` with an image
   extension, or nil. Relative candidates resolve against
   `workspace-root` (falling back to cwd)."
  ^File [candidate workspace-root]
  (let [^String s (-> candidate
                      str/trim
                      strip-file-url
                      expand-home)]
    (when (and (seq s) (re-find image-extension-pattern s))
      (let [f (File. s)
            f (if (.isAbsolute f)
                f
                (File. (str (or workspace-root (System/getProperty "user.dir"))) s))]

        (when (and (.isFile f) (.canRead f)) f)))))

;; =============================================================================
;; Collection
;; =============================================================================

(defn size-label
  [^long n]
  (cond (>= n (* 1024 1024)) (format "%.1fMB" (/ (double n) (* 1024.0 1024.0)))
        (>= n 1024) (format "%.0fKB" (/ (double n) 1024.0))
        :else (str n "B")))

(defn- attach-file
  [^File f mime]
  (let [data (Files/readAllBytes (.toPath f))]
    {:path (.getAbsolutePath f)
     :filename (.getName f)
     :media-type mime
     :base64 (.encodeToString (Base64/getEncoder) data)
     :size (alength data)
     :size-label (size-label (alength data))}))

(defn- resolved-image-files
  "Ordered, de-duped `[canonical-path File]` pairs for every path-shaped
   token in `text` that resolves to a readable image-extension file.
   The magic-byte sniff still owns the final image verdict downstream.

   Fast path: if the whole message contains no image-extension substring
   at all we return `[]` after a single linear scan, skipping the
   tokenization + per-candidate filesystem work that dominates the cost on
   large non-image pastes (the common case on every turn)."
  [text workspace-root]
  (let [s (str text)]
    (if-not (re-find image-extension-present-pattern s)
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
           (resolved-image-files text workspace-root)))))

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
     (let [files (resolved-image-files text workspace-root)]
       (reduce (fn [acc [_canonical ^File f]]
                 (try (if-let [mime (sniff-file-mime f)]
                        (cond (> (.length f) (long max-bytes))
                              (update acc
                                      :skipped
                                      conj
                                      {:path (.getAbsolutePath f)
                                       :reason (str (size-label (.length f))
                                                    " exceeds the "
                                                    (size-label max-bytes)
                                                    " attachment limit")})
                              (>= (count (:attached acc)) (long max-images))
                              (update acc
                                      :skipped
                                      conj
                                      {:path (.getAbsolutePath f)
                                       :reason (str "attachment limit of "
                                                    max-images
                                                    " images per message reached")})
                              :else (update acc :attached conj (attach-file f mime)))
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
   enforces the same per-image and count caps as [[collect-user-images]], and
   returns the same `{:attached [...] :skipped [...]}` shape so the assemble seam
   treats disk-scanned and inline images uniformly. Never throws."
  ([attachments] (prepare-inline-attachments attachments {}))
  ([attachments
    {:keys [max-bytes max-images] :or {max-bytes max-image-bytes max-images max-image-count}}]
   (reduce (fn [acc {:keys [base64 filename]}]
             (try
               (let [^String payload
                     (strip-data-url-prefix (str base64))

                     data
                     (.decode (Base64/getDecoder) payload)

                     size
                     (alength data)

                     label
                     (or (not-empty (str filename)) "image")

                     mime
                     (detect-image-mime data)]

                 (cond
                   (nil? mime)
                   (update acc :skipped conj {:path label :reason "not a supported still image"})
                   (> size (long max-bytes)) (update acc
                                                     :skipped
                                                     conj
                                                     {:path label
                                                      :reason (str (size-label size)
                                                                   " exceeds the "
                                                                   (size-label max-bytes)
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
                                  :base64 (.encodeToString (Base64/getEncoder) data)
                                  :size size
                                  :size-label (size-label size)})))
               (catch Throwable _ acc)))
           {:attached [] :skipped []}
           (or attachments []))))
