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
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attach])
  (:import [com.googlecode.lanterna.terminal.image TerminalImage TerminalImage$Protocol]))

;; =============================================================================
;; Capability detection
;; =============================================================================

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
   nil. Recognises the `CSI 16 t` cell-size reply `ESC[6;<h>;<w>t` directly, and
   falls back to deriving the cell size from a `CSI 14 t` text-area-pixels reply
   `ESC[4;<hpx>;<wpx>t` paired with a `CSI 18 t` text-area-cells reply
   `ESC[8;<rows>;<cols>t` (cell = px / cells). Tolerant of the replies arriving
   concatenated in any order / interleaved with other bytes."
  [^String s]
  (when (and (string? s) (seq s))
    (or (when-let [m (re-find #"\u001b\[6;(\d+);(\d+)t" s)]
          (let
            [h (long (parse-long (nth m 1)))
             w (long (parse-long (nth m 2)))]

            (when (and (pos? w) (pos? h)) {:w w :h h})))
        (let
          [px
           (re-find #"\u001b\[4;(\d+);(\d+)t" s)

           ch
           (re-find #"\u001b\[8;(\d+);(\d+)t" s)]

          (when (and px ch)
            (let
              [hpx
               (long (parse-long (nth px 1)))

               wpx
               (long (parse-long (nth px 2)))

               rows
               (long (parse-long (nth ch 1)))

               cols
               (long (parse-long (nth ch 2)))]

              (when (and (pos? wpx) (pos? hpx) (pos? rows) (pos? cols))
                {:w (quot wpx cols) :h (quot hpx rows)})))))))

;; =============================================================================
;; Intrinsic pixel-dimension sniffing
;; =============================================================================

(defn image-dimensions
  "Intrinsic `{:w :h}` pixel size from the leading bytes of an image, or nil."
  [^bytes b mime]
  (when-let [wh (TerminalImage/imageDimensions b mime)]
    {:w (aget ^ints wh 0) :h (aget ^ints wh 1)}))

(defn probe-dimensions
  "Read `path`'s head and sniff its `{:w :h}` pixel dimensions. nil on failure."
  [path mime]
  (when-let [wh (TerminalImage/probeDimensions (str path) mime)]
    {:w (aget ^ints wh 0) :h (aget ^ints wh 1)}))

;; =============================================================================
;; Cell-box sizing
;; =============================================================================

(defn cell-size
  "Fit an image of `{:w :h}` px into `max-cols` × `max-rows` cells,
   aspect-preserving. Returns `{:cols :rows}` (>= 1)."
  [{:keys [w h]} max-cols max-rows]
  (let
    [r (TerminalImage/cellSize (int (or w 1))
                               (int (or h 1))
                               (int max-cols)
                               (when max-rows (Integer/valueOf (int max-rows))))]
    {:cols (aget ^ints r 0) :rows (aget ^ints r 1)}))

;; =============================================================================
;; Escape encoding
;; =============================================================================

(defn encode-kitty
  "Kitty graphics `\\x1b_G` transmit+display sequence for base64 `data`, sized to
   `cols`×`rows` cells. `C=1` keeps the cursor put after placement. When
   `crop-top`/`crop-bottom` (cell rows scrolled past the band's top/bottom edge)
   are positive, only the visible vertical slice is shown at native scale via the
   Kitty source rectangle, sized from the transmitted image's `img-w`×`img-h`
   pixel dimensions."
  [^String data {:keys [cols rows crop-top crop-bottom img-w img-h]}]
  (TerminalImage/encodeKitty data
                             (int (or cols 0))
                             (int (or rows 0))
                             (int (or crop-top 0))
                             (int (or crop-bottom 0))
                             (int (or img-w 0))
                             (int (or img-h 0))))

(defn encode-iterm2
  "iTerm2 `\\x1b]1337;File=` inline-image sequence for base64 `data`."
  [^String data {:keys [cols]}]
  (TerminalImage/encodeIterm2 data (int (or cols 0))))

(defn read-base64
  "Read `path` and base64-encode its bytes, or nil on failure. Cached by
   path + mtime + size so an unchanged file is encoded at most once."
  [path]
  (TerminalImage/readBase64 (str path)))

(defn transcode->png-base64
  "Decode `path` (any ImageIO-readable format) and re-encode it as a PNG
   base64 string, downscaled so it fits the `cols`×`rows` cell box in pixels.
   The Kitty protocol's `f=100` only accepts PNG, so a JPEG/GIF/BMP drop must
   pass through here first. Uses AWT/ImageIO (JVM-only — unavailable in the
   macOS native image); returns nil on any failure so callers fall back to a
   text card."
  [path {:keys [cols rows]}]
  (TerminalImage/transcodePngBase64 (str path) (int cols) (int rows)))

(defn- transcode->png
  "Transcode `path` to a box-fitted PNG, returning `{:data :w :h}` — base64 plus
   the TRANSMITTED (scaled) PNG's pixel dims — or nil on failure."
  [path {:keys [cols rows]}]
  (when-let [r (TerminalImage/transcodePng (str path) (int cols) (int rows))]
    {:data (aget ^objects r 0) :w (aget ^objects r 1) :h (aget ^objects r 2)}))

(defn kitty-png
  "PNG base64 + transmitted pixel dims `{:data :w :h}` for the Kitty wire. A PNG
   file rides through verbatim — transmitted at its intrinsic `width`×`height`
   (and works in the native image too); anything else is transcoded via ImageIO."
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
      (when-let [data (read-base64 path)]
        (encode-iterm2 data box))

      nil)))

(def ^:private kitty-chunk
  "Kitty caps a single `\\x1b_G` escape's base64 payload at 4096 bytes; longer data
   rides `m=1`…`m=0` continuation chunks. Mirrors the fork encoder's KITTY_CHUNK."
  4096)

(defn kitty-transmit
  "Kitty `a=t` transmit-ONLY sequence: upload base64 PNG `data` under client image
   id `id` WITHOUT displaying it, chunked into `m=1`…`m=0` pieces like the fork's
   `encodeKitty`. A later `kitty-place` draws it with NO re-upload — the key to
   flicker-free scrolling (transmit once, then re-place cheaply)."
  [^String data id]
  (let
    [esc
     "\u001b"

     n
     (long (count data))

     chunk
     (long kitty-chunk)

     control
     (str "a=t,i=" id ",f=100,q=2")]

    (if (<= n chunk)
      (str esc "_G" control ";" data esc "\\")
      (let [sb (StringBuilder.)]
        (loop
          [off 0
           first? true]

          (when (< off n)
            (let
              [end (min n (+ off chunk))
               chunk (subs data off end)
               last? (>= end n)
               ctrl (cond first? (str control ",m=1")
                          last? "m=0"
                          :else "m=1")]

              (.append sb esc)
              (.append sb "_G")
              (.append sb ctrl)
              (.append sb ";")
              (.append sb chunk)
              (.append sb esc)
              (.append sb "\\")
              (recur (long end) false))))
        (.toString sb)))))

(defn kitty-place
  "Kitty `a=p` placement sequence for an ALREADY-transmitted image `id` at the
   cursor: draw it into a `cols`×`rows` cell box, optionally cropped to the visible
   vertical slice (`crop-top`/`crop-bottom` cell rows over an `img-w`×`img-h` px
   image) via the protocol's source rectangle — the SAME `x/y/w/h` math the fork's
   crop `encodeKitty` uses. Reusing placement id `p=1` REPLACES the prior placement,
   so a scroll moves the picture atomically: no delete-all, no re-upload, no flash."
  [{:keys [id cols rows crop-top crop-bottom img-w img-h]}]
  (let
    [ct
     (max 0 (long (or crop-top 0)))

     cb
     (max 0 (long (or crop-bottom 0)))

     rows
     (long (or rows 0))

     cols
     (long (or cols 0))

     ih
     (long (or img-h 0))

     vis
     (max 1 (- rows ct cb))

     base
     (str "\u001b_Ga=p,i=" id ",p=1,C=1,q=2" (when (pos? cols) (str ",c=" cols)))]

    (if (and (pos? rows) (pos? ih) (or (pos? ct) (pos? cb)))
      (let
        [src-y
         (Math/round (/ (* (double ih) ct) (double rows)))

         src-h0
         (max 1 (Math/round (/ (* (double ih) vis) (double rows))))

         ;; Clamp the source rectangle inside the image like the fork's
         ;; encodeKitty: an out-of-bounds y+h makes Kitty reject the placement,
         ;; which would blank the image mid-scroll and reintroduce the flicker.
         src-h
         (if (> (+ src-y src-h0) ih) (- ih src-y) src-h0)]

        (str base ",r=" vis ",x=0,y=" src-y ",w=" (long (or img-w 0)) ",h=" src-h "\u001b\\"))
      (str base ",r=" vis "\u001b\\"))))

(defn kitty-delete-placement
  "Kitty sequence removing image `id`'s placement while KEEPING its uploaded data,
   so an image scrolled off screen leaves no ghost yet needs no re-upload if it
   scrolls back into view."
  [id]
  (str "\u001b_Ga=d,d=i,i=" id ",q=2\u001b\\"))

(defn kitty-free-image
  "Kitty sequence deleting image `id` AND freeing its uploaded data — used when the
   transmit cache evicts a long-off-screen image to bound terminal-side memory."
  [id]
  (str "\u001b_Ga=d,d=I,i=" id ",q=2\u001b\\"))

(defn probe-paste-image
  "Detect the FIRST image the pasted `text` points at (a dropped file path).
   Returns `{:path :mime :filename :size :size-label :width :height}` or nil.
   `workspace-root` anchors relative candidates. Never throws."
  [text {:keys [workspace-root]}]
  (try (when-let
         [{:keys [path media-type filename size size-label]}
          (first (attach/scan-image-descriptors text {:workspace-root workspace-root}))]
         (let [{:keys [w h]} (or (probe-dimensions path media-type) {})]
           {:path path
            :mime media-type
            :filename filename
            :size size
            :size-label size-label
            :width w
            :height h}))
       (catch Throwable _ nil)))

;; =============================================================================
;; Persisted-attachment materialization (durable history re-render)
;; =============================================================================

(defn attachment-cache-dir
  "Durable cache dir (`~/.vis/cache/tui-attachments`) for persisted image
   attachments, so a re-rendered history image survives its original (often
   OS-temp) source path vanishing. Created on demand."
  ^java.io.File []
  (let
    [dir (java.io.File. (java.io.File. (java.io.File. (System/getProperty "user.home") ".vis")
                                       "cache")
                        "tui-attachments")]
    (.mkdirs dir)
    dir))

(def ^:private media-type->ext
  {"image/png" ".png"
   "image/jpeg" ".jpg"
   "image/gif" ".gif"
   "image/webp" ".webp"
   "image/bmp" ".bmp"})

(defn materialize-attachment
  "Decode ONE persisted user image attachment (canonical wire map, STRING keys
   `id`/`base64`/`media_type`/`filename`) into a STABLE cache file keyed by its
   row id and return the descriptor a `vis-image` fence needs -
   `{:path :mime :filename :size :size-label :width :height}` - or nil when it is
   not a usable still image. Idempotent: an already-written cache file is reused,
   never rewritten, so a resumed session re-renders the picture from DB-owned
   bytes even after the original source path is gone. Never throws."
  [att]
  (try
    (let
      [media
       (str (get att "media_type"))

       b64
       (str (get att "base64"))]

      (when (and (str/starts-with? media "image/") (not (str/blank? b64)))
        (let
          [ext
           (get media-type->ext media ".png")

           id
           (or (not-empty (str (get att "id"))) (str (java.util.UUID/randomUUID)))

           f
           (java.io.File. (attachment-cache-dir) (str id ext))]

          (when-not (.isFile f)
            (java.nio.file.Files/write (.toPath f)
                                       ^bytes (.decode (java.util.Base64/getDecoder) b64)
                                       ^"[Ljava.nio.file.OpenOption;" (make-array java.nio.file.OpenOption 0)))
          (let
            [path
             (.getAbsolutePath f)

             size
             (.length f)

             {:keys [w h]}
             (or (probe-dimensions path media) {})]

            {:path path
             :mime media
             :filename (or (not-empty (str (get att "filename"))) "image")
             :size size
             :size-label (attach/size-label size)
             :width w
             :height h}))))
    (catch Throwable _ nil)))
