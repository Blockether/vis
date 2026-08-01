(ns com.blockether.vis.ext.channel-tui.video
  "MP4 (H.264) → frames, so a clip can be WATCHED inline in a graphical terminal.

   The codec lives in `com.blockether/imaging`, NOT here: `imaging/probe-video`,
   `imaging/decode-video` and `imaging/video->gif` are the pure-Rust demuxer
   (re_mp4) + H.264 decoder (rust_h264) inside the same cdylib that already does
   every still image in vis. That means no `ffmpeg` on PATH, no `java.desktop`,
   nothing to install, and it survives the GraalVM native image — while jcodec
   stays on this jar's classpath for what it is genuinely good at, ENCODING a
   replayed session in `channel-tui.cinema`.

   What is left in this namespace is the TERMINAL half: brand sniffing, sizing a
   clip to a cell grid, and the Kitty/iTerm2 escape sequences that actually
   animate it. Decoding pixels is imaging's job.

   Reach for [[->gif]] first. An animated GIF is the format the WHOLE existing
   stack already understands — `foundation.gif`, the attachment sniffer, the
   provider wire (`image/gif` is one of the four verbatim-legal vision types) and
   iTerm2's native inline animation — so transcoding a clip is what makes it both
   viewable and describable-by-the-model without touching any of that plumbing.
   [[->gif]] never materialises frames on the JVM heap at all: the transcode
   happens inside the cdylib and only GIF bytes come back. [[decode-frames]] is
   the lower level for a frame-stepping Kitty player.

   Frames arrive in DISPLAY order (B-frames reordered), display-cropped (H.264
   pads to whole macroblocks, so 1080p codes as 1920×1088) and as straight RGBA8,
   because the decoder resolves all three before the bytes cross the FFI.

   H.264 is the supported codec. HEVC/H.265 — what a modern iPhone records by
   default — AV1 and VP9 are demuxed but NOT decoded, and that is reported as a
   clear `ex-info` rather than a corrupt picture."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as img]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg])
  (:import [com.googlecode.lanterna.terminal.image TerminalImage]
           [java.io File]))


;; ── Tunables ────────────────────────────────────────────────────────────────

(def default-max-frames
  "Frames [[decode-frames]] will hold in memory before it stops. Every frame is a
   full uncompressed RGBA canvas, so a 1280×720 clip costs ~3.7 MB per frame —
   240 of them is already ~880 MB unscaled. Callers that want a long clip pass
   `:max-dimension` (and usually `:stride`) rather than raising this."
  240)

(def default-gif-max-dimension
  "Long-edge ceiling [[->gif]] scales down to. A GIF is palette-quantized and
   LZW-packed per frame; full-resolution video turns into tens of megabytes that
   no vision wire would accept anyway."
  480)

;; ── Sniffing ────────────────────────────────────────────────────────────────
;;
;; The container sniff itself lives in the lanterna fork's `TerminalImage`: it is
;; the same brand table the terminal-side probe uses to size a clip without
;; decoding it, so a HEIF photo cannot be mistaken for a movie by one half of the
;; stack and not the other.

(defn mp4?
  "True when `data` begins with an ISO base-media (`ftyp`) box whose major brand
   is a VIDEO brand — `.mp4`, `.m4v` and `.mov` all land here. HEIF/AVIF still
   images share the container and are rejected by the fork's brand table."
  [^bytes data]
  (TerminalImage/isVideoHead data))

(defn- io-file ^File [x] (if (instance? File x) x (io/file (str x))))

(defn video-file?
  "True when `src` is an existing readable file whose FIRST BYTES are a video
   `ftyp` box — 12 bytes read, no decode and no index walk. This is the cheap
   gate every still-image surface uses to ask \"is this thing a clip?\" before
   spending a poster frame on it, so it must stay allocation-free and total; the
   fork's `isVideoFile` is exactly that."
  [src]
  (TerminalImage/isVideoFile (.getPath (io-file src))))

(defn media-type
  "The `video/…` media type for `src`, or nil when it is not a clip we can open.
   The NAME only picks the label (`.mov` → `video/quicktime`); the verdict is
   always the magic-byte sniff, so a mislabelled file cannot fool a caller."
  [src]
  (TerminalImage/probeVideoMime (.getPath (io-file src))))

;; ── Metadata ────────────────────────────────────────────────────────────────

(defn probe
  "Cheap `{:codec :codec-string :width :height :frames :duration-s :fps
   :has-audio :is-decodable}` for `src` (a File or path), read from the MP4 index
   WITHOUT decoding a single picture. nil when `src` is not a readable MP4 or
   carries no video track.

   `:codec` is a lowercase keyword (`:h264`, `:hevc`, …); only `:h264` can be
   decoded — see [[decodable?]]. `:codec-string` keeps the container's precise
   RFC 6381 name (`\"avc1.420028\"`)."
  [src]
  (let [file (io-file src)]
    (when (video-file? file)
      (try (let [p (img/probe-video file)]
             {:codec (some-> (:codec-kind p)
                             str/lower-case
                             keyword)
              :codec-string (:codec p)
              :width (:width p)
              :height (:height p)
              :frames (:frames p)
              :duration-s (:duration-s p)
              :fps (:fps p)
              :has-audio (:has-audio p)
              :is-decodable (boolean (:is-decodable p))})
           (catch Throwable _ nil)))))

(defn decodable?
  "True when `src` is an MP4 whose video codec this decoder actually supports."
  [src]
  (true? (:is-decodable (probe src))))

(defn- checked-meta
  "`probe` for a clip we are about to decode, with the two failure modes reported
   as `ex-info` (`:reason` `:not-mp4` / `:unsupported-codec`) instead of
   something subtly wrong."
  [file]
  (let [meta (probe file)]
    (when-not meta (throw (ex-info "Not a readable MP4 video" {:reason :not-mp4 :path (str file)})))
    (when-not (:is-decodable meta)
      (throw (ex-info (str "Unsupported video codec "
                           (name (or (:codec meta) :unknown))
                           " — this decoder handles H.264 only")
                      {:reason :unsupported-codec :codec (:codec meta) :path (str file)})))
    meta))

;; ── Frame decode ────────────────────────────────────────────────────────────

(defn decode-frames
  "Decode `src` (a File or path to an H.264 MP4) into

     `{:width :height :fps :frames [{:index :timestamp-s :width :height :rgba} …]}`

   where `:rgba` is one frame's straight RGBA8 canvas and `:index` its position
   in the ORIGINAL stream (so a strided decode still reports true timing).
   `:width`/`:height` describe the returned frames, after any scaling.

   opts:
     :max-frames     stop after this many KEPT frames (default [[default-max-frames]])
     :stride         keep every Nth frame — 2 halves the frame rate (default 1)
     :max-dimension  downscale each frame so its long edge fits this
     :encoding       `:rgba` (default) or `:png`

   `:encoding :png` makes the cdylib encode each frame itself and hands back
   `:png` bytes in place of `:rgba`. That is what both terminal protocols
   transmit anyway, and it is strictly cheaper: frames cross the FFI boundary
   base64'd, so a 120-frame 720p clip moves ~9 MB of PNG instead of ~140 MB of
   RGBA — faster than decoding raw and re-encoding here, and playback then holds
   megabytes instead of hundreds of them.

   Throws `ex-info` with `:reason` `:not-mp4` / `:unsupported-codec` rather than
   returning something subtly wrong."
  ([src] (decode-frames src nil))
  ([src
    {:keys [max-frames stride max-dimension encoding] :or {max-frames default-max-frames stride 1}}]
   (let
     [file
      (io-file src)

      _
      (checked-meta file)

      step
      (max 1 (long stride))

      png?
      (= :png encoding)

      decoded
      (img/decode-video file
                        (cond-> {:max-frames (max 0 (long max-frames)) :stride step}
                          max-dimension
                          (assoc :max-dimension (long max-dimension))

                          png?
                          (assoc :encoding "png")))

      {:keys [width height]}
      decoded]

     {:width width
      :height height
      ;; imaging reports the SOURCE rate; a strided decode plays back that much
      ;; slower, and every caller here times frames from `:fps`.
      :fps (when-let [f (:fps decoded)]
             (/ (double f) step))
      ;; Each frame carries its OWN dims: that keeps a frame self-describing for
      ;; `frame->png` / `playback-sequences`, which would otherwise have to
      ;; thread the clip's size through separately.
      :frames
      (mapv (fn [f]
              (assoc {:index (:index f) :timestamp-s (:timestamp-s f) :width width :height height}
                (if png? :png :rgba) (:data f)))
            (:frames decoded))})))

;; ── Transcode ───────────────────────────────────────────────────────────────

(defn ->gif
  "Transcode the MP4 at `src` into ONE animated GIF byte array, looping forever.

   This is the format that makes a clip work everywhere the rest of vis already
   works — iTerm2 animates it inline with no player loop, `foundation.gif`
   round-trips it, and `image/gif` is legal on the provider vision wire, so the
   model can be shown a clip instead of being told about one.

   The whole transcode runs inside imaging's cdylib, so the frames never touch
   the JVM heap. Takes [[decode-frames]]'s opts; `:max-dimension` defaults to
   [[default-gif-max-dimension]] and `:fps` overrides the playback rate (GIF
   stores delays in centiseconds, so the rate is quantised to 1/100 s)."
  ^bytes
  [src {:keys [max-frames stride max-dimension fps] :or {max-frames default-max-frames stride 1}}]
  (let [file (io-file src)]
    (checked-meta file)
    (img/video->gif file
                    (cond->
                      {:max-frames (max 0 (long max-frames))
                       :stride (max 1 (long stride))
                       :max-dimension (long (or max-dimension default-gif-max-dimension))
                       :loop-count -1}
                      fps
                      (assoc :fps (double fps))))))

;; ── Terminal playback ───────────────────────────────────────────────────────
;;
;; There is no "video protocol" in any terminal. Kitty and iTerm2 both draw
;; STILL images, so playing a clip is: draw a frame, wait, draw the next one
;; over it. The two differ in how the previous frame is retired —
;;
;;   Kitty   images float ABOVE the text cells and persist until deleted, so a
;;           frame must REPLACE its predecessor. Transmitting under the SAME
;;           client id overwrites the stored image and re-placing it replaces
;;           the placement, which is why every frame here reuses `image-id`
;;           instead of leaking one upload per frame into terminal memory.
;;   iTerm2  inline images are CELL-BOUND — drawing over the same cells erases
;;           what was there — so each frame is simply re-emitted, and there is
;;           no cleanup to do.
;;
;; [[playback-sequences]] is deliberately PURE: it turns decoded pixels into the
;; exact bytes plus the exact delays, and can therefore be asserted in a
;; headless test. [[play!]] only adds a clock and a stream.

(def ^:private image-id
  "The single Kitty client image id playback reuses for EVERY frame. A fresh id
   per frame would pin one upload per frame in the terminal's image memory —
   a few hundred megabytes for a modest clip."
  9901)

(defn frame->png
  "ONE decoded frame → PNG bytes, via imaging (no java.desktop).

   A frame decoded with `:encoding :png` already CARRIES its PNG, so this is
   free for the playback path; an `:rgba` frame is encoded here."
  ^bytes [{:keys [width height ^bytes rgba ^bytes png]}]
  (or png
      (with-open [i (img/from-pixels rgba (int width) (int height))]
        (img/encode i :png))))

(defn- lazy-escapes
  "`map`, but ONE element at a time.

   `clojure.core/map` over a vector hands back a CHUNKED seq: asking for frame 0
   encodes the first THIRTY-TWO frames. At full resolution that is tens of
   megabytes of base64 and a visible stall before a single picture appears,
   which defeats the entire point of building the escapes on demand."
  [f coll]
  (lazy-seq (when-let [s (seq coll)]
              (cons (f (first s)) (lazy-escapes f (rest s))))))

(defn poster
  "`{:png <bytes> :width :height}` for ONE frame of `src` — the STILL picture a
   terminal, a paste probe or a transcript fence shows to stand for a clip.

   `:max-frames 1` stops the decoder at the first picture, `:max-dimension`
   makes it scale during conversion and `:encoding :png` has the cdylib hand the
   PNG straight back, so this costs a single keyframe at the size you will
   actually draw (~0.2 s for 1080p, ~0.1 s for 720p) no matter how long the clip
   runs. Decoding the timeline just to keep frame 0 would cost seconds and
   hundreds of megabytes of RGBA.

   nil when `src` holds no decodable picture."
  ([src] (poster src nil))
  ([src {:keys [max-dimension]}]
   (let
     [{:keys [frames]} (decode-frames src
                                      {:max-frames 1 :max-dimension max-dimension :encoding :png})]
     (when-let [f (first frames)]
       {:png (frame->png f) :width (:width f) :height (:height f)}))))

(defn playback-sequences
  "Turn a [[decode-frames]] result into the terminal byte stream that PLAYS it.

   Returns `{:protocol :cols :rows :frame-count :frames [{:index :delay-ms
   :escape}]}`, where
   `:escape` is the full sequence drawing that one frame at the saved cursor
   position and `:delay-ms` is how long to hold it. Concatenating every
   `:escape` with the right pauses between them IS the playback — see [[play!]].

   Takes the DECODED map, not a file: `(playback-sequences (decode-frames src)
   opts)`. Handing it a File/path throws rather than quietly returning zero
   frames.

   `protocol` defaults to whatever the host terminal speaks
   (`terminal-image/images-protocol`); pass it explicitly to render for a
   specific terminal (what the tests do). Returns nil when neither the given nor
   the detected protocol can draw an image at all."
  [{:keys [width height fps frames] :as decoded} {:keys [protocol cols rows] :as _opts}]
  (when-not (and (map? decoded) (sequential? frames))
    (throw
      (ex-info
        "playback-sequences takes a decode-frames result, not a file — call (decode-frames src opts) first."
        {:got (some-> decoded
                      class
                      .getName)})))
  (when-let [proto (or protocol (timg/images-protocol))]
    (let
      [box (timg/cell-size {:w width :h height} (or cols 80) rows)
       cols' (:cols box)
       rows' (:rows box)
       delay-ms (max 1 (long (Math/round (/ 1000.0 (double (or fps 12))))))]

      {:protocol proto
       :cols cols'
       :rows rows'
       :frame-count (count frames)
       ;; LAZY on purpose. One frame's escape is its whole PNG in base64, so a
       ;; clip's escapes are tens of megabytes; building them eagerly makes the
       ;; caller wait for the LAST frame's encode before the FIRST one can be
       ;; painted. On demand, playback starts after one PNG and each frame is
       ;; encoded while its predecessor is still on screen.
       :frames (lazy-escapes (fn [f]
                               {:index (:index f)
                                :delay-ms delay-ms
                                :escape (let [data (frame->png f)]
                                          (str
                                            ;; Park the cursor back at the clip's top-left every frame so
                                            ;; frame N+1 lands exactly on frame N.
                                            "\u001b8"
                                            (case proto
                                              :kitty
                                              (str (timg/kitty-transmit data image-id)
                                                   (timg/kitty-place {:id image-id
                                                                      :cols cols'
                                                                      :rows rows'
                                                                      :img-w (:width f)
                                                                      :img-h (:height f)}))

                                              :iterm2
                                              (timg/encode-iterm2 data {:cols cols'})

                                              "")))})
                             frames)})))

(defn play!
  "Play the MP4 at `src` inline in a graphical terminal — the whole point of this
   namespace.

   Decodes, then paces the frames against a monotonic clock so playback keeps
   real time instead of drifting by however long each PNG encode took. Options
   are [[decode-frames]]'s plus:

     :out       OutputStream to write to (default `System/out`)
     :cols :rows  cell box to fit the picture into (default 80 cols, free rows)
     :protocol  force `:kitty` / `:iterm2` instead of sniffing the terminal
     :loops     how many times to play (default 1)

   Returns `{:frames :protocol :cols :rows}`, or nil when the terminal cannot
   draw inline images (a non-graphical TERM — nothing is written in that case)."
  [src {:keys [out loops cols rows max-dimension encoding] :or {loops 1} :as opts}]
  (let
    [decoded (decode-frames src
                            (cond-> opts
                              ;; Decode straight to the size the terminal will
                              ;; DRAW. A 1080p clip in an 80-column window is
                              ;; ~6x more pixels than the cells can show: it
                              ;; doubles decode time and turns 10 MB of escape
                              ;; bytes into 60 MB, all of it discarded by the
                              ;; terminal's own downscale. Callers wanting the
                              ;; native picture pass `:max-dimension` explicitly.
                              (nil? max-dimension)
                              (assoc :max-dimension (timg/box-pixels cols rows))

                              ;; Let the cdylib encode the frames. PNG is what
                              ;; both protocols transmit, so decoding to RGBA
                              ;; first only buys an extra copy: ~140 MB of
                              ;; base64'd pixels across the FFI boundary and
                              ;; onto the heap for a 120-frame 720p clip.
                              (nil? encoding)
                              (assoc :encoding :png)))]
    (when-let [{:keys [frames protocol cols rows]} (playback-sequences decoded opts)]
      (let
        [^java.io.OutputStream os (or out System/out)
         emit (fn [^String s]
                (.write os (.getBytes s "UTF-8"))
                (.flush os))]

        ;; Save the cursor ONCE: every frame's escape restores to it.
        (emit "\u001b7")
        (try (dotimes [_ (max 1 (long loops))]
               (let [t0 (System/nanoTime)]
                 (doseq [[i {:keys [escape delay-ms]}] (map-indexed vector frames)]
                   (emit escape)
                   ;; Sleep to the frame's SCHEDULED wall-clock slot, not for a flat
                   ;; delay, so encode time doesn't accumulate into a slow clip.
                   (let
                     [due (+ t0 (* (inc (long i)) (long delay-ms) 1000000))
                      left (quot (- due (System/nanoTime)) 1000000)]

                     (when (pos? left) (Thread/sleep left))))))
             (finally
               ;; Leave no floating image behind, and put the cursor BELOW the clip
               ;; so the next thing printed doesn't overwrite the last frame.
               (when (= protocol :kitty) (emit (timg/kitty-free-image image-id)))
               (emit (str "\u001b8" (apply str (repeat rows "\n"))))))
        {:frames (count frames) :protocol protocol :cols cols :rows rows}))))
