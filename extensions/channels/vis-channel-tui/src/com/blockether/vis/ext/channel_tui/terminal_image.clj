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
  (:require [com.blockether.vis.internal.attachments :as attach])
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

(defn- kitty-png
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
