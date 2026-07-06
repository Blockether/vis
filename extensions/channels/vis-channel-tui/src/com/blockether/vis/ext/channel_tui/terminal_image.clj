(ns com.blockether.vis.ext.channel-tui.terminal-image
  "Inline terminal image rendering — Kitty graphics protocol / iTerm2 inline
   images, ported from pi's `packages/tui/src/terminal-image.ts`.

   Two independent concerns live here:

     1. Capability detection — which graphics protocol (if any) the host
        terminal speaks, sniffed from the environment (`TERM_PROGRAM`,
        `KITTY_WINDOW_ID`, `ITERM_SESSION_ID`, …). tmux/screen are treated
        as image-incapable because they mangle the pass-through.

     2. Pixel-free image probing + escape encoding — read a file head, sniff
        its intrinsic pixel dimensions (png/jpeg/gif/webp/bmp), and encode
        the base64 payload as a Kitty `\\x1b_G…` or iTerm2 `\\x1b]1337;File=…`
        sequence sized to a cell box.

   The escape strings are emitted DIRECTLY to the tty AFTER Lanterna's delta
   refresh (the screen loop owns that), placed over rows the renderer
   reserved as blanks. Lanterna never sees the graphics bytes, so its cell
   diff stays intact and the image survives subsequent delta frames."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attach])
  (:import [java.awt RenderingHints]
           [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream File RandomAccessFile]
           [java.util Base64]
           [javax.imageio ImageIO]))

;; A TUI JVM has no display; keep AWT/ImageIO off the windowing system so
;; decoding a JPEG never tries to open one.
(System/setProperty "java.awt.headless" "true")

;; =============================================================================
;; Capability detection (pi terminal-image.ts parity)
;; =============================================================================

(defn detect-capabilities
  "Sniff which inline-image protocol the host terminal speaks from `env`
   (defaults to the process environment). Returns `{:images :kitty|:iterm2|nil}`.
   tmux and screen report `nil` — they don't reliably forward graphics."
  ([] (detect-capabilities (System/getenv)))
  ([env]
   (let [g
         (fn [k]
           (some-> (get env k)
                   str/lower-case))

         term-prog
         (or (g "TERM_PROGRAM") "")

         term
         (or (g "TERM") "")]

     (cond (or (get env "TMUX") (str/starts-with? term "tmux")) {:images nil}
           (str/starts-with? term "screen") {:images nil}
           (or (get env "KITTY_WINDOW_ID") (= term-prog "kitty")) {:images :kitty}
           (or (= term-prog "ghostty")
               (str/includes? term "ghostty")
               (get env "GHOSTTY_RESOURCES_DIR"))
           {:images :kitty}
           (or (get env "WEZTERM_PANE") (= term-prog "wezterm")) {:images :kitty}
           (or (= term-prog "warpterminal")
               (get env "WARP_SESSION_ID")
               (get env "WARP_TERMINAL_SESSION_UUID"))
           {:images :kitty}
           (or (get env "ITERM_SESSION_ID") (= term-prog "iterm.app")) {:images :iterm2}
           :else {:images nil}))))

(def ^:private caps (delay (detect-capabilities)))

(defn images-protocol "`:kitty`, `:iterm2`, or nil for the current terminal." [] (:images @caps))

;; Terminal cell pixel size. The real values come from a terminal query at
;; startup; 9x18 matches pi's default and is close enough for box sizing.
(def ^:private cell-dims (atom {:w 9 :h 18}))

(defn set-cell-dimensions!
  [w h]
  (when (and (pos? (long w)) (pos? (long h))) (reset! cell-dims {:w (long w) :h (long h)})))

;; =============================================================================
;; Intrinsic pixel-dimension sniffing (pi getImageDimensions parity)
;; =============================================================================

(defn- u8 [^bytes b ^long i] (bit-and (long (aget b i)) 0xff))
(defn- u16-be ^long [^bytes b ^long i] (+ (bit-shift-left (u8 b i) 8) (u8 b (inc i))))
(defn- u16-le ^long [^bytes b ^long i] (+ (u8 b i) (bit-shift-left (u8 b (inc i)) 8)))
(defn- u32-be
  ^long [^bytes b ^long i]
  (+ (bit-shift-left (u8 b i) 24)
     (bit-shift-left (u8 b (+ i 1)) 16)
     (bit-shift-left (u8 b (+ i 2)) 8)
     (u8 b (+ i 3))))
(defn- u32-le
  ^long [^bytes b ^long i]
  (+ (u8 b i)
     (bit-shift-left (u8 b (+ i 1)) 8)
     (bit-shift-left (u8 b (+ i 2)) 16)
     (bit-shift-left (u8 b (+ i 3)) 24)))
(defn- ascii ^String [^bytes b ^long off ^long n] (String. b (int off) (int n) "US-ASCII"))

(defn- png-dims [^bytes b] (when (>= (alength b) 24) {:w (u32-be b 16) :h (u32-be b 20)}))

(defn- jpeg-dims
  [^bytes b]
  (when (>= (alength b) 4)
    (loop [off 2]
      (when (< off (- (alength b) 9))
        (if (not= 0xff (u8 b off))
          (recur (inc off))
          (let [marker (u8 b (inc off))]
            (if (and (>= marker 0xc0) (<= marker 0xc2))
              {:h (u16-be b (+ off 5)) :w (u16-be b (+ off 7))}
              (let [len (u16-be b (+ off 2))]
                (when (>= len 2) (recur (+ off 2 len)))))))))))

(defn- gif-dims [^bytes b] (when (>= (alength b) 10) {:w (u16-le b 6) :h (u16-le b 8)}))

(defn- webp-dims
  [^bytes b]
  (when (>= (alength b) 30)
    (let [chunk (ascii b 12 4)]
      (cond (= chunk "VP8 ") {:w (bit-and (u16-le b 26) 0x3fff) :h (bit-and (u16-le b 28) 0x3fff)}
            (= chunk "VP8L") (let [bits (u32-le b 21)]
                               {:w (inc (bit-and bits 0x3fff))
                                :h (inc (bit-and (bit-shift-right bits 14) 0x3fff))})
            (= chunk "VP8X")
            {:w (inc (+ (u8 b 24) (bit-shift-left (u8 b 25) 8) (bit-shift-left (u8 b 26) 16)))
             :h (inc (+ (u8 b 27) (bit-shift-left (u8 b 28) 8) (bit-shift-left (u8 b 29) 16)))}
            :else nil))))

(defn- bmp-dims
  [^bytes b]
  (when (>= (alength b) 26) {:w (u32-le b 18) :h (Math/abs (int (u32-le b 22)))}))

(defn image-dimensions
  "Intrinsic `{:w :h}` pixel size from the leading bytes of an image, or nil."
  [^bytes b mime]
  (try (case mime
         "image/png"
         (png-dims b)

         "image/jpeg"
         (jpeg-dims b)

         "image/gif"
         (gif-dims b)

         "image/webp"
         (webp-dims b)

         "image/bmp"
         (bmp-dims b)

         nil)
       (catch Throwable _ nil)))

(defn- read-head
  ^bytes [^File f ^long n]
  (with-open [raf (RandomAccessFile. f "r")]
    (let [len (int (min (.length raf) n))
          buf (byte-array len)]

      (.readFully raf buf)
      buf)))

(defn probe-dimensions
  "Read `path`'s head and sniff its `{:w :h}` pixel dimensions. nil on failure."
  [path mime]
  (try (image-dimensions (read-head (File. (str path)) 4100) mime) (catch Throwable _ nil)))

;; =============================================================================
;; Cell-box sizing (pi calculateImageCellSize parity)
;; =============================================================================

(defn cell-size
  "Fit an image of `{:w :h}` px into `max-cols` × `max-rows` cells,
   aspect-preserving. Returns `{:cols :rows}` (>= 1)."
  [{:keys [w h]} max-cols max-rows]
  (let [{cw :w ch :h}
        @cell-dims

        max-cols
        (max 1 (long max-cols))

        max-rows
        (when max-rows (max 1 (long max-rows)))

        iw
        (max 1 (long (or w 1)))

        ih
        (max 1 (long (or h 1)))

        w-scale
        (/ (double (* max-cols cw)) iw)

        h-scale
        (if max-rows (/ (double (* max-rows ch)) ih) w-scale)

        scale
        (min w-scale h-scale)

        cols
        (long (Math/ceil (/ (* iw scale) (double cw))))

        rows
        (long (Math/ceil (/ (* ih scale) (double ch))))]

    {:cols (max 1 (min max-cols cols)) :rows (max 1 (if max-rows (min max-rows rows) rows))}))

;; =============================================================================
;; Escape encoding (pi encodeKitty / encodeITerm2 parity)
;; =============================================================================

(def ^:private kitty-chunk 4096)

(defn encode-kitty
  "Kitty graphics `\\x1b_G` transmit+display sequence for base64 `data`,
   sized to `cols`×`rows` cells. `C=1` keeps the cursor put after placement."
  [^String data {:keys [cols rows]}]
  (let [params
        (cond-> ["a=T" "f=100" "q=2" "C=1"]
          cols
          (conj (str "c=" cols))

          rows
          (conj (str "r=" rows)))

        head
        (str/join "," params)]

    (if (<= (count data) kitty-chunk)
      (str "\u001b_G" head ";" data "\u001b\\")
      (let [n (count data)]
        (loop [off 0
               first? true
               acc (StringBuilder.)]

          (if (>= off n)
            (.toString acc)
            (let [end (min n (+ off kitty-chunk))
                  chunk (subs data off end)
                  last? (>= end n)]

              (.append acc
                       (cond first? (str "\u001b_G" head ",m=1;" chunk "\u001b\\")
                             last? (str "\u001b_Gm=0;" chunk "\u001b\\")
                             :else (str "\u001b_Gm=1;" chunk "\u001b\\")))
              (recur end false acc))))))))

(defn encode-iterm2
  "iTerm2 `\\x1b]1337;File=` inline-image sequence for base64 `data`."
  [^String data {:keys [cols]}]
  (str "\u001b]1337;File=inline=1;width=" cols ";height=auto;preserveAspectRatio=1:" data "\u0007"))

(def ^:private base64-cache
  ;; path -> {:mtime :size :data}. Images are re-emitted on every scroll that
  ;; moves them; caching keeps a 5MB file off the read+encode path each time.
  (atom {}))

(defn read-base64
  "Read `path` and base64-encode its bytes, or nil on failure. Cached by
   path + mtime + size so an unchanged file is encoded at most once."
  [path]
  (try (let [f
             (File. (str path))

             mtime
             (.lastModified f)

             size
             (.length f)

             cached
             (get @base64-cache (str path))]

         (if (and cached (= mtime (:mtime cached)) (= size (:size cached)))
           (:data cached)
           (let [data (.encodeToString (Base64/getEncoder) (read-head f size))]
             (swap! base64-cache assoc (str path) {:mtime mtime :size size :data data})
             data)))
       (catch Throwable _ nil)))

(def ^:private png-transcode-cache
  ;; [path mtime size cols rows] -> png base64. Decoding + re-encoding a
  ;; multi-megapixel JPEG is expensive; cache the box-sized PNG so a scroll
  ;; that re-emits the image doesn't re-run ImageIO each time.
  (atom {}))

(defn transcode->png-base64
  "Decode `path` (any ImageIO-readable format) and re-encode it as a PNG
   base64 string, downscaled so it fits the `cols`×`rows` cell box in pixels.
   The Kitty protocol's `f=100` only accepts PNG, so a JPEG/GIF/BMP drop must
   pass through here first. Uses AWT/ImageIO (JVM-only — unavailable in the
   macOS native image); returns nil on any failure so callers fall back to a
   text card."
  [path {:keys [cols rows]}]
  (try
    (let [f
          (File. (str path))

          key
          [(str path) (.lastModified f) (.length f) cols rows]

          hit
          (get @png-transcode-cache key)]

      (or hit
          (when-let [img (ImageIO/read f)]
            (let [{cw :w ch :h} @cell-dims
                  target-w (* (long cols) (long cw))
                  target-h (* (long rows) (long ch))
                  iw (.getWidth img)
                  ih (.getHeight img)
                  scale (min 1.0 (/ (double target-w) iw) (/ (double target-h) ih))
                  sw (max 1 (long (Math/round (* iw scale))))
                  sh (max 1 (long (Math/round (* ih scale))))
                  scaled (if (< scale 1.0)
                           (let [bi (BufferedImage. sw sh BufferedImage/TYPE_INT_ARGB)
                                 g (.createGraphics bi)]

                             (.setRenderingHint g
                                                RenderingHints/KEY_INTERPOLATION
                                                RenderingHints/VALUE_INTERPOLATION_BILINEAR)
                             (.drawImage g img 0 0 sw sh nil)
                             (.dispose g)
                             bi)
                           img)
                  baos (ByteArrayOutputStream.)]

              (ImageIO/write scaled "png" baos)
              (let [data (.encodeToString (Base64/getEncoder) (.toByteArray baos))]
                (swap! png-transcode-cache assoc key data)
                data)))))
    (catch Throwable _ nil)))

(defn- kitty-png-base64
  "PNG base64 for the Kitty wire. A PNG file rides through verbatim (works in
   the native image too); anything else is transcoded via ImageIO."
  [path mime box]
  (if (= mime "image/png") (read-base64 path) (transcode->png-base64 path box)))

(defn render-sequence
  "Full escape sequence that draws the image at `path` (`mime`) into a
   `cols`×`rows` cell box for the current terminal, or nil when the terminal
   can't render inline images / the file can't be decoded."
  [path mime box]
  (when-let [proto (images-protocol)]
    (case proto
      ;; Kitty (Ghostty/kitty/WezTerm/Warp): f=100 is PNG-only — transcode.
      :kitty
      (when-let [data (kitty-png-base64 path mime box)]
        (encode-kitty data box))

      ;; iTerm2 accepts any container format as-is.
      :iterm2
      (when-let [data (read-base64 path)]
        (encode-iterm2 data box))

      nil)))

(defn probe-paste-image
  "Detect the FIRST image the pasted `text` points at (a dropped file path).
   Returns `{:path :mime :filename :size :size-label :width :height}` or nil.
   `workspace-root` anchors relative candidates. Never throws."
  [text {:keys [workspace-root]}]
  (try (when-let [{:keys [path media-type filename size size-label]}
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
