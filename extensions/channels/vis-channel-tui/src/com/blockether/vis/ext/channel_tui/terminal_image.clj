(ns com.blockether.vis.ext.channel-tui.terminal-image
  "Inline terminal image rendering — Kitty graphics protocol / iTerm2 inline
   images.

   The pixel-free logic (capability detection, intrinsic dimension sniffing,
   cell-box sizing, escape encoding, base64/PNG transcoding) now lives in the
   lanterna fork's Java class
   `com.googlecode.lanterna.terminal.image.TerminalImage`. This namespace is the
   thin Clojure adapter that keeps vis's map-shaped API (`{:images …}`,
   `{:w :h}`, `{:cols :rows}`) plus the attachment-aware paste probe, which
   reaches back into vis internals and so stays here.

   The escape strings are emitted DIRECTLY to the tty AFTER Lanterna's delta
   refresh (the screen loop owns that), placed over rows the renderer
   reserved as blanks. Lanterna never sees the graphics bytes, so its cell
   diff stays intact and the image survives subsequent delta frames."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as img]
            [com.blockether.vis.internal.attachments :as attach]
            [com.blockether.vis.internal.format :as fmt])
  (:import [com.googlecode.lanterna.terminal.image TerminalImage TerminalImage$Protocol]
           [java.util Base64]))

;; Capability detection

(defn- proto->kw
  [^TerminalImage$Protocol p]
  (condp = p TerminalImage$Protocol/KITTY :kitty TerminalImage$Protocol/ITERM2 :iterm2 nil))

(defn detect-capabilities
  "Sniff which inline-image protocol the host terminal speaks from `env`
   (defaults to the process environment). Returns `{:images :kitty|:iterm2|nil}`.
   tmux and screen report `nil` — they don't reliably forward graphics."
  ([] {:images (proto->kw (TerminalImage/detectCapabilities))})
  ([env] {:images (proto->kw (TerminalImage/detectCapabilities ^java.util.Map env))}))

(def ^:private caps (delay (detect-capabilities)))

(defn images-protocol "`:kitty`, `:iterm2`, or nil for the current terminal." [] (:images @caps))

(defn graphical-terminal?
  "Whether we're in a GRAPHICAL terminal — one that speaks an inline-image
   protocol (kitty/Ghostty/WezTerm/Warp → Kitty, iTerm2 → iTerm2) — as opposed
   to a plain/non-graphical terminal (or tmux/screen, which mangle graphics
   pass-through). Callers branch on this to draw an inline image vs. a text-card
   fallback. The 0-arity reuses the memoized capability detection; pass `env` to
   probe an explicit environment (delegates to `TerminalImage/isGraphicalTerminal`)."
  ([] (some? (images-protocol)))
  ([env] (TerminalImage/isGraphicalTerminal ^java.util.Map env)))

(defn set-cell-dimensions! [w h] (TerminalImage/setCellDimensions (int w) (int h)))

(defn parse-cell-size-report
  "Parse a terminal window-report reply into `{:w :h}` CELL pixel dimensions, or
   nil. The fork recognises the `CSI 16 t` cell-size reply `ESC[6;<h>;<w>t`
   directly, and otherwise derives the cell from a `CSI 14 t` text-area-pixels
   reply `ESC[4;<hpx>;<wpx>t` paired with a `CSI 18 t` text-area-cells reply
   `ESC[8;<rows>;<cols>t` (cell = px / cells) — tolerant of the replies arriving
   concatenated in any order and interleaved with whatever else the tty had
   queued."
  [^String s]
  (when-let [wh (TerminalImage/parseCellSizeReport s)]
    {:w (aget ^ints wh 0) :h (aget ^ints wh 1)}))

;; Intrinsic pixel-dimension sniffing

;; Video (poster frames) — resolved lazily to break a namespace cycle

(def ^:private video-ns 'com.blockether.vis.ext.channel-tui.video)

(defn- video-var
  "One var from `channel-tui.video`, resolved on FIRST use and never at load
   time: that namespace requires THIS one for the escape encoders, so a static
   `:require` back would be a compile-time cycle. nil when video support is not
   on the classpath, which turns every video path here into a graceful no-op
   instead of a load error."
  [sym]
  (try (requiring-resolve (symbol (str video-ns) (str sym))) (catch Throwable _ nil)))

(def ^:private v-poster (delay (video-var 'poster)))
(def ^:private v-probe (delay (video-var 'probe)))

(defn video-mime?
  "True for a media type the fork sizes and posters as a clip."
  [mime]
  (TerminalImage/isVideoMime (when mime (str mime))))

(defn video-source?
  "True when `path` is a clip this namespace can draw — by media type when the
   caller knows one, else by sniffing the file's head (a dropped path carries no
   mime). Both halves live in the fork; 12 bytes are read, never a decode, so it
   is cheap enough to sit on the still-image transcode path."
  [path mime]
  (TerminalImage/isVideoSource (str path) (when mime (str mime))))

(defn image-dimensions
  "Intrinsic `{:w :h}` pixel size from the leading bytes of an image, or nil."
  [^bytes b mime]
  (when-let [wh (TerminalImage/imageDimensions b mime)]
    {:w (aget ^ints wh 0) :h (aget ^ints wh 1)}))

(defn probe-dimensions
  "Read `path`'s head and sniff its `{:w :h}` pixel dimensions. nil on failure.
   A VIDEO is answered by the fork from the MP4 track header (no decode, no
   native imaging library), and only then by the imaging index — so a clip can
   size its cell box without touching a single picture."
  [path mime]
  (or (when-let [wh (TerminalImage/probeDimensions (str path) mime)]
        {:w (aget ^ints wh 0) :h (aget ^ints wh 1)})
      (when (video-source? path mime)
        (when-let [probe @v-probe]
          (let [{:keys [width height]} (probe path)]
            (when (and width height) {:w width :h height}))))))

;; Cell-box sizing

(defn cell-size
  "Fit an image of `{:w :h}` px into `max-cols` × `max-rows` cells,
   aspect-preserving. Returns `{:cols :rows}` (>= 1)."
  [{:keys [w h]} max-cols max-rows]
  (let [r (TerminalImage/cellSize (int (or w 1))
                                  (int (or h 1))
                                  (int max-cols)
                                  (when max-rows (Integer/valueOf (int max-rows))))]
    {:cols (aget ^ints r 0) :rows (aget ^ints r 1)}))

(defn box-pixels
  "Long-edge PIXEL ceiling of a `cols`×`rows` cell box — what to ask a decoder
   for when the result will be drawn into that box.

   Decoding 1080p for an 80-column window produces about six times the pixels
   the cells can show: seconds of extra decode and tens of megabytes of extra
   escape bytes, all of it thrown away by the terminal's own downscale. The fork
   reads its LIVE cell metrics, so a HiDPI cell asks for a proportionally
   sharper picture."
  [cols rows]
  (TerminalImage/boxPixels (int (or cols 0)) (int (or rows 0))))

;; Escape encoding

(defn encode-kitty
  "Kitty graphics `\\x1b_G` transmit+display sequence for `data`, sized to
   `cols`×`rows` cells. `C=1` keeps the cursor put after placement. When
   `crop-top`/`crop-bottom` (cell rows scrolled past the band's top/bottom edge)
   are positive, only the visible vertical slice is shown at native scale via the
   Kitty source rectangle, sized from the transmitted image's `img-w`×`img-h`
   pixel dimensions.

   `data` is a base64 String OR the RAW PNG bytes. Raw bytes are base64'd one
   chunk at a time straight into the escape by the fork, so the 1.33x base64
   String never exists — the per-frame path for video playback, where the
   payload changes every frame and nothing can be cached."
  [data {:keys [cols rows crop-top crop-bottom img-w img-h]}]
  (let [c
        (int (or cols 0))

        r
        (int (or rows 0))

        ct
        (int (or crop-top 0))

        cb
        (int (or crop-bottom 0))

        iw
        (int (or img-w 0))

        ih
        (int (or img-h 0))]

    (if (bytes? data)
      (TerminalImage/encodeKitty ^bytes data c r ct cb iw ih)
      (TerminalImage/encodeKitty ^String data c r ct cb iw ih))))

(defn encode-iterm2
  "iTerm2 `\\x1b]1337;File=` inline-image sequence for `data` (base64 String or
   RAW image bytes, base64'd into the escape without an intermediate String)."
  [data {:keys [cols]}]
  (let [c (int (or cols 0))]
    (if (bytes? data)
      (TerminalImage/encodeIterm2 ^bytes data c)
      (TerminalImage/encodeIterm2 ^String data c))))

(defn read-base64
  "Read `path` and base64-encode its bytes, or nil on failure. Cached by
   path + mtime + size so an unchanged file is encoded at most once."
  [path]
  (TerminalImage/readBase64 (str path)))

(def ^:private png-transcode-cache
  "[abs-path mtime size cols rows] -> `{:data :w :h}` for an already box-fitted PNG,
   `:data` being the raw PNG bytes."
  (atom {}))

(defn- still->png
  "Box-fitted PNG `{:data :w :h}` for a STILL image file, or nil. `:data` is the
   RAW PNG bytes: the escape encoders base64 them as they chunk, so encoding one
   here would only build a 1.33x String for someone else to re-slice."
  [^java.io.File f {:keys [cols rows]}]
  (try (with-open [src (img/decode f)]
         (let [target-w (* (long cols) (long (TerminalImage/cellWidth)))
               target-h (* (long rows) (long (TerminalImage/cellHeight)))
               iw (img/width src)
               ih (img/height src)
               scale (min 1.0 (/ (double target-w) iw) (/ (double target-h) ih))]

           (if (< scale 1.0)
             (let [sw (max 1 (Math/round (* iw scale)))
                   sh (max 1 (Math/round (* ih scale)))]

               (with-open [scaled (img/resize src sw sh :lanczos3)]
                 {:data (img/encode scaled :png) :w (int sw) :h (int sh)}))
             {:data (img/encode src :png) :w (int iw) :h (int ih)})))
       (catch Throwable _ nil)))

(defn- video->png
  "Box-fitted PNG `{:data :w :h}` of a clip's FIRST frame — the still a terminal
   draws for a video. The decoder stops after one picture AND scales while it
   converts, so the cost is one keyframe at cell-box size however long the clip
   runs; the cache above then makes every re-emit free."
  [^java.io.File f {:keys [cols rows]}]
  (when-let [poster @v-poster]
    (try (when-let [{:keys [png width height]} (poster f {:max-dimension (box-pixels cols rows)})]
           {:data png :w (int width) :h (int height)})
         (catch Throwable _ nil))))

(defn- transcode->png
  "Transcode `path` to a box-fitted PNG, returning `{:data :w :h}` — base64 plus
   the TRANSMITTED (scaled) PNG's pixel dims — or nil on failure. A VIDEO comes
   back as its poster frame, which is what makes a clip drawable by every
   still-image surface in the TUI.

   Decoding and re-encoding a multi-megapixel JPEG (or pulling a frame out of an
   MP4) is expensive, so the box-sized PNG is cached by path+mtime+size+box: a
   scroll that re-emits the image never re-decodes."
  [path {:keys [cols rows] :as box}]
  (let [f
        (io/file (str path))

        key
        [(.getAbsolutePath f) (.lastModified f) (.length f) cols rows]]

    (or (get @png-transcode-cache key)
        (when-let [r (if (video-source? f nil) (video->png f box) (still->png f box))]
          (swap! png-transcode-cache assoc key r)
          r))))

(defn transcode->png-base64
  "Decode `path` (any format `com.blockether.imaging` reads) and re-encode it as a
   PNG base64 string, downscaled so it fits the `cols`×`rows` cell box in pixels.
   The Kitty protocol's `f=100` only accepts PNG, so a JPEG/GIF/BMP drop must
   pass through here first. Pure FFM (no java.desktop, works in the native
   image); returns nil on any failure so callers fall back to a text card.

   Prefer [[transcode->png]] and hand its raw `:data` to the escape encoders:
   this arity exists for callers that genuinely need the base64 text."
  [path box]
  (when-let [{:keys [data]} (transcode->png path box)]
    (if (bytes? data) (.encodeToString (Base64/getEncoder) ^bytes data) data)))

(defn kitty-png
  "PNG payload + transmitted pixel dims `{:data :w :h}` for the Kitty wire. A PNG
   file rides through verbatim — transmitted at its intrinsic `width`×`height`
   (and works in the native image too); anything else is decoded and re-encoded
   as a box-fitted PNG via `com.blockether.imaging`, and a VIDEO becomes its
   poster frame.

   `:data` is base64 text for the verbatim path (the fork caches that read) and
   RAW PNG bytes for a transcode; every encoder here takes either."
  [path mime {:keys [width height] :as box}]
  (if (= mime "image/png")
    (when-let [data (read-base64 path)]
      {:data data :w width :h height})
    (transcode->png path box)))

(defn render-sequence
  "Full escape sequence that draws the image at `path` (`mime`) into a
   `cols`×`rows` cell box for the current terminal, or nil when the terminal
   can't render inline images / the file can't be decoded. `box` may carry
   `:crop-top`/`:crop-bottom` (cell rows scrolled past the band edge) plus the
   image's intrinsic `:width`/`:height`, so a partly-scrolled Kitty image renders
   its visible slice at native scale instead of vanishing."
  [path mime box]
  (when-let [proto (images-protocol)]
    (case proto
      ;; Kitty (Ghostty/kitty/WezTerm/Warp): f=100 is PNG-only — transcode, then
      ;; crop to the visible slice via the source rectangle when scrolled.
      :kitty
      (when-let [{:keys [data w h]} (kitty-png path mime box)]
        (encode-kitty data
                      (assoc box
                        :img-w w
                        :img-h h)))

      ;; iTerm2 accepts any container format as-is (no source-crop; the fitting
      ;; pass shrinks a bottom-overflowing box instead).
      :iterm2
      (when-let [data (if (video-source? path mime)
                        ;; A clip's own bytes are not an image: iTerm2 would draw a
                        ;; broken download card. Send the poster frame instead.
                        (:data (transcode->png path box))
                        (read-base64 path))]
        (encode-iterm2 data box))

      nil)))


(defn kitty-transmit
  "Kitty `a=t` transmit-ONLY sequence: upload PNG `data` (base64 String or RAW
   bytes) under client image id `id` WITHOUT displaying it, chunked into
   `m=1`…`m=0` pieces by the fork's own encoder. A later `kitty-place` draws it
   with NO re-upload — the key to flicker-free scrolling (transmit once, then
   re-place cheaply)."
  [data id]
  (let [i (int id)]
    (if (bytes? data)
      (TerminalImage/transmitKitty ^bytes data i)
      (TerminalImage/transmitKitty ^String data i))))

(defn kitty-place
  "Kitty `a=p` placement sequence for an ALREADY-transmitted image `id` at the
   cursor: draw it into a `cols`×`rows` cell box, optionally cropped to the visible
   vertical slice (`crop-top`/`crop-bottom` cell rows over an `img-w`×`img-h` px
   image) via the protocol's source rectangle — the SAME `x/y/w/h` math the fork's
   crop `encodeKitty` uses, because it is the same code. Reusing placement id `p=1`
   REPLACES the prior placement, so a scroll moves the picture atomically: no
   delete-all, no re-upload, no flash."
  [{:keys [id cols rows crop-top crop-bottom img-w img-h]}]
  (TerminalImage/placeKitty (int id)
                            (int (or cols 0))
                            (int (or rows 0))
                            (int (or crop-top 0))
                            (int (or crop-bottom 0))
                            (int (or img-w 0))
                            (int (or img-h 0))))

(defn kitty-delete-placement
  "Kitty sequence removing image `id`'s placement while KEEPING its uploaded data,
   so an image scrolled off screen leaves no ghost yet needs no re-upload if it
   scrolls back into view."
  [id]
  (TerminalImage/deleteKittyPlacement (int id)))

(defn kitty-free-image
  "Kitty sequence deleting image `id` AND freeing its uploaded data — used when the
   transmit cache evicts a long-off-screen image to bound terminal-side memory."
  [id]
  (TerminalImage/freeKittyImage (int id)))


(defn- video-paste-descriptor
  "Descriptor for a dropped VIDEO path, in exactly the shape a dropped picture
   returns, so a clip flows through the same paste → fence → inline-render path.
   The engine's media scanner attaches clips too (it samples them into a wire GIF),
   but the TUI resolves them here to paint the drop the instant it lands. The fork
   unquotes / un-escapes /
   de-`file://`s the dropped line and sniffs the mime; dimensions come from the MP4
   index, never a decode. nil unless the path really is a readable clip."
  [text workspace-root]
  (when-let [s (TerminalImage/pastedVideoPath (str text))]
    (let [f (io/file s)
          f (if (.isAbsolute f)
              f
              (io/file (str (or workspace-root (System/getProperty "user.dir"))) s))]

      (when (and (.isFile f) (.canRead f))
        (when-let [mime (TerminalImage/probeVideoMime (.getAbsolutePath f))]
          (let [{:keys [w h]} (or (probe-dimensions (.getAbsolutePath f) mime) {})]
            {:path (.getAbsolutePath f)
             :mime mime
             :filename (.getName f)
             :size (.length f)
             :size-label (fmt/format-bytes (.length f))
             :width w
             :height h}))))))

(defn probe-paste-image
  "Detect the FIRST image the pasted `text` points at (a dropped file path).
   Returns `{:path :mime :filename :size :size-label :width :height}` or nil.
   A dropped MP4/M4V/MOV resolves too — it renders as its poster frame.
   `workspace-root` anchors relative candidates. Never throws."
  [text {:keys [workspace-root]}]
  (try (or (when-let [{:keys [path media-type filename size size-label]}
                      (first (attach/scan-image-descriptors text {:workspace-root workspace-root}))]
             (let [{:keys [w h]} (or (probe-dimensions path media-type) {})]
               {:path path
                :mime media-type
                :filename filename
                :size size
                :size-label size-label
                :width w
                :height h}))
           (video-paste-descriptor text workspace-root))
       (catch Throwable _ nil)))

;; Persisted-attachment materialization (durable history re-render)

(defn attachment-cache-dir
  "Durable cache dir (`~/.vis/cache/tui-attachments`) for persisted image
   attachments, so a re-rendered history image survives its original (often
   OS-temp) source path vanishing. Created on demand."
  ^java.io.File []
  (let [dir (java.io.File. (java.io.File. (java.io.File. (System/getProperty "user.home") ".vis")
                                          "cache")
                           "tui-attachments")]
    (.mkdirs dir)
    dir))


(defn materialize-attachment
  "Decode ONE persisted user image attachment (canonical wire map, STRING keys
   `id`/`base64`/`media_type`/`filename`) into a STABLE cache file keyed by its
   row id and return the descriptor a `vis-image` fence needs -
   `{:path :mime :filename :size :size-label :width :height}` - or nil when it is
   neither a usable still nor a decodable clip. Idempotent: an already-written
   cache file is reused,
   never rewritten, so a resumed session re-renders the picture from DB-owned
   bytes even after the original source path is gone. Never throws."
  [att]
  (try
    (let [media
          (str (get att "media_type"))

          b64
          (str (get att "base64"))]

      (when (and (or (str/starts-with? media "image/") (video-mime? media)) (not (str/blank? b64)))
        (let [ext
              (or (TerminalImage/extensionForMime media) ".png")

              id
              (or (not-empty (str (get att "id"))) (str (java.util.UUID/randomUUID)))

              f
              (java.io.File. (attachment-cache-dir) (str id ext))]

          (when-not (.isFile f)
            (java.nio.file.Files/write (.toPath f)
                                       ^bytes (.decode (java.util.Base64/getDecoder) b64)
                                       ^"[Ljava.nio.file.OpenOption;"
                                       (make-array java.nio.file.OpenOption 0)))
          (let [path
                (.getAbsolutePath f)

                size
                (.length f)

                {:keys [w h]}
                (or (probe-dimensions path media) {})]

            {:path path
             :mime media
             :filename (or (not-empty (str (get att "filename"))) "image")
             :size size
             :size-label (fmt/format-bytes size)
             :width w
             :height h}))))
    (catch Throwable _ nil)))
