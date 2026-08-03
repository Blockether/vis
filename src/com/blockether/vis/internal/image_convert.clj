(ns com.blockether.vis.internal.image-convert
  "Container conversion for attachment payloads -- NOT optimization.

   vis stores and replays the attachment PICTURE verbatim: whatever the user
   dropped is what is stored and what the provider sees, pixel for pixel (the
   stored ENCODING may be shrunk losslessly -- [[compact]]). The one thing that
   cannot be passed through is a container no vision wire accepts (`attachments/
   provider-image-media-types`): a BMP is a perfectly good image and an SVG is
   a perfectly good figure, and both are a hard 400 on every provider. Since an
   attachment REPLAYS on every later turn, one such row kills the whole session.

   So this namespace does exactly one thing: turn bytes a provider REFUSES into
   the same picture in a container it ACCEPTS.

     * raster (BMP, TIFF, or anything else the decoder reads) -> PNG, 1:1
     * vector (`.svg` / gzipped `.svgz`)                      -> rendered PNG
     * video (`.mp4` / `.mov`)                                -> animated GIF

   Everything a renderer can answer is ASKED, not re-implemented.
   `com.blockether/imaging` (Rust `image` + resvg over FFM) decodes, sniffs,
   gunzips `.svgz`, resolves `width=\"100%\"`/`cm`/`viewBox` letterboxing, and
   rasterizes. It also owns the two BROWSER repairs resvg alone refuses, behind
   `imaging/svg-canvas`:

     * a zero or negative declared size -- resvg refuses the document outright
       (\"SVG has an invalid size\"); browsers fall back to the `viewBox`.
     * a document that declares NO size -- resvg's bounds run from the ORIGIN
       (a figure at x=50 gains a 50px margin) and collapse to a bare 100x100
       when content sits at negative coordinates, so the canvas is framed by the
       ink actually painted.

   Both used to be hand-written Clojure HERE. They are Rust in the library now --
   one implementation for every caller, and vis only asks for the answer.
   `renderer-delegation-test` still pins that answer document by document, so a
   change in resvg or in the repair shows up as a vis test failure.

   Conversion is 1:1 and never re-compresses what the wire already takes: no
   quality knob, no size heuristics on the way through. There are exactly two
   exceptions, both narrow and both named: [[fit-within]], which the send gate
   calls ONLY for a payload over the wire's byte cap that would otherwise be
   DROPPED, and [[compact]], which the storage rail calls on the way into the
   `session_attachment` BLOB and which is LOSSLESS -- same picture, fewer bytes,
   verified by a re-probe. Nothing here ever throws: a
   failure comes back as `{:reason <why>}` -- no `:bytes`, so the caller still
   SKIPS the attachment, but it can TELL the user why instead of dropping a
   perfectly valid picture in silence.

   Pixels come from `com.blockether/imaging`, never from AWT/Java2D/ImageIO --
   which is why this works identically in the native image, on every platform,
   with no headless or fontconfig bootstrap. Tests do the opposite on purpose:
   `independent-decoder-test` re-reads every payload this namespace emits with
   `javax.imageio`, so \"it decodes\" is never just the encoder agreeing with
   itself -- the provider's decoder is a third implementation again."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as imaging]))

;; =============================================================================
;; Media types
;; =============================================================================

(def svg-media-types
  "Vector containers vis RASTERIZES on the way in instead of refusing. No
   provider reads SVG -- it is markup, not pixels -- but unlike a HEIC the
   pixels are recoverable: the document is rendered here, so a matplotlib
   `savefig(format=\"svg\")` or a dropped icon still reaches the model."
  #{"image/svg+xml" "image/svg" "application/svg+xml" "text/svg+xml"})

(defn svg-media-type?
  [media-type]
  (contains? svg-media-types (str/lower-case (str/trim (str media-type)))))

(def ^:dynamic *enabled?*
  "Kill switch -- bind false to make conversion unavailable (what a build with
   no imaging cdylib looks like)."
  true)

;; =============================================================================
;; SVG rendering
;; =============================================================================

(def wire-safe-media-types
  "Containers no vision wire needs CONVERTED (mirrors
   `attachments/provider-image-media-types`). Kept here so [[to-provider-safe]]
   can hand these back byte-identical instead of re-encoding a payload that was
   already fine -- byte-identical, but never UNCHECKED: the bytes are still
   decoded once to prove they are pixels (see [[verified-raster]])."
  #{"image/jpeg" "image/png" "image/gif" "image/webp"})

(def ^:const svg-max-raster-dimension
  "Hard ceiling on the rendered long edge. A vector document can declare any
   size at all, and a 40000px canvas is an OOM, not a picture."
  4096)

(defn- failure-reason
  "One line a HUMAN can act on, taken from whatever the decoder or the renderer
   threw. The thrown message is kept verbatim (folded onto one line) because
   `imaging` already says exactly what is wrong -- \"image too large\", an
   unsupported container -- and swallowing it is how a perfectly valid
   attachment ends up dropped with no explanation at all."
  [^Throwable t]
  (let
    [msg (some-> t
                 (.getMessage)
                 str
                 str/trim
                 not-empty)]
    (if msg (str/replace msg #"\s+" " ") (.getSimpleName (class t)))))

(defn rasterize-svg
  "Render an SVG (`.svg` or gzipped `.svgz`) into a PNG every vision wire
   accepts. Returns `{:bytes :media-type :size :original-size :width :height
   :original-width :original-height}`, `{:reason <why>}` when the document does
   not parse or renders to nothing, or nil when conversion is disabled. Never
   throws.

   The ASPECT RATIO is the whole game: a squashed chart is a wrong chart, and
   the model cannot tell the difference. The raster size is the document's own
   resolved size, and the only scaling that ever happens is the
   [[svg-max-raster-dimension]] ceiling, one factor applied to both edges.

   The canvas is filled WHITE first: SVG figures routinely leave the background
   transparent, and a transparent PNG of black axes is what a viewer -- or a
   model -- sees as an empty image."
  ([^bytes data] (rasterize-svg data nil))
  ([^bytes data {:keys [max-dimension]}]
   (when (and *enabled?* data (pos? (alength data)))
     (try
       (let
         [max-dim
          (long (or max-dimension svg-max-raster-dimension))

          ;; The library sizes the canvas the way a BROWSER would -- its own
          ;; resolved size when the document declares a usable one, else the
          ;; `viewBox` or the painted ink -- and hands back repaired markup when
          ;; the original is one resvg would refuse.
          {:keys [width height svg]}
          (imaging/svg-canvas data)

          payload
          (or svg data)

          iw
          (double width)

          ih
          (double height)

          longest
          (max (double iw) (double ih))

          ;; 1:1 with the document's own declared size. Rasterizing needs SOME
          ;; pixel count, and the honest one is the size the author wrote; the
          ;; ceiling only exists because a 40000px canvas is an OOM -- and it
          ;; scales BOTH edges by the SAME factor, so the ratio survives it.
          scale
          (/ (double (min (double max-dim) longest)) longest)

          tw
          (max 1 (long (Math/round (* (double iw) scale))))

          th
          (max 1 (long (Math/round (* (double ih) scale))))]

         (with-open [im (imaging/render-svg payload {:width tw :height th :background "white"})]
           (let
             [out (imaging/encode im :png)
              {:keys [width height]} (imaging/info im)]

             (if (and out (pos? (alength ^bytes out)))
               {:bytes out
                :media-type "image/png"
                :size (alength ^bytes out)
                :original-size (alength data)
                :width (long width)
                :height (long height)
                :original-width (long (Math/round (double iw)))
                :original-height (long (Math/round (double ih)))}
               {:reason "the SVG rendered to no bytes"}))))
       (catch Throwable t
         ;; Unparseable markup or a document limit: no pixels either way, but
         ;; the caller gets the renderer's own words instead of a silent drop.
         {:reason (str "the SVG could not be rendered: " (failure-reason t))})))))

;; =============================================================================
;; Raster re-containering
;; =============================================================================

(defn- transcode-png
  "Decode `data` and write it back out as PNG, pixel for pixel -- same
   dimensions, no resample, no lossy step. `{:reason <why>}` -- the decoder's
   own words -- when the payload cannot be decoded (HEIC, AVIF, or an image
   past the decoder's own memory limit) or cannot be encoded."
  [^bytes data]
  (try (with-open [im (imaging/decode data)]
         (let
           [out (imaging/encode im :png)
            {:keys [width height]} (imaging/info im)]

           (if (and out (pos? (alength ^bytes out)))
             {:bytes out
              :media-type "image/png"
              :size (alength ^bytes out)
              :original-size (alength data)
              :width (long width)
              :height (long height)
              :original-width (long width)
              :original-height (long height)}
             {:reason "the PNG encoder produced no bytes"})))
       (catch Throwable t {:reason (str "the image could not be decoded: " (failure-reason t))})))

(defn- verified-raster
  "`data` handed back BYTE-IDENTICAL, once the decoder has actually turned it
   into pixels. A container sniff only reads a header, and a header is not a
   picture: a PNG with a perfect signature + `IHDR` and a garbage `IDAT` stream
   sniffs as `image/png`, passes every media-type gate vis has, and is a hard
   provider 400 (`Could not process image`) that -- because attachments REPLAY
   -- repeats on every later turn and bricks the session. So the one payload
   class that is never re-encoded is still DECODED here, and the pixels, not the
   magic bytes, decide whether it may be sent.

   `{:reason <why>}` -- the decoder's own words -- when the bytes are not a
   decodable image. Cost is one decode per distinct payload; the send seam
   memoizes on content hash so a replayed attachment pays it once."
  [^bytes data ^String mt]
  (try (with-open [im (imaging/decode data)]
         (let [{:keys [width height]} (imaging/info im)]
           (if (and width height (pos? (long width)) (pos? (long height)))
             {:bytes data
              :media-type mt
              :size (alength data)
              :original-size (alength data)
              :width (long width)
              :height (long height)
              :original-width (long width)
              :original-height (long height)}
             {:reason "the image decoded to an empty canvas"})))
       (catch Throwable t {:reason (str "the image could not be decoded: " (failure-reason t))})))

(defn to-provider-safe
  "Re-container `data` into something every vision wire accepts (PNG) -- and
   never hand back bytes the wire would refuse. Payloads whose container is
   already wire-safe come back UNTOUCHED, byte for byte, but only after
   [[verified-raster]] has decoded them: this is a VALIDITY fix, never a size
   one, and an undecodable payload is exactly the thing it must catch. Returns
   the same map as [[rasterize-svg]]; `{:reason <why>}` when the payload can be
   turned into pixels by neither the decoder nor the SVG renderer (a corrupt
   raster, HEIC, AVIF, an image the decoder refuses as too large); nil when
   conversion is DISABLED -- the caller skips it either way, but a reason is
   something it can SHOW."
  [^bytes data media-type]
  (when (and *enabled?* data (pos? (alength data)))
    (let [mt (str/lower-case (str/trim (str media-type)))]
      (cond (contains? wire-safe-media-types mt) (verified-raster data mt)
            (svg-media-type? mt) (rasterize-svg data nil)
            :else (transcode-png data)))))

(defn fit-within
  "Squeeze an already wire-safe payload under `max-bytes` with the imaging
   library's REAL optimisers: oxipng's filter/colour-type/palette search for
   PNG, jpegtran-style lossless metadata stripping for JPEG, gifsicle's
   differencing re-encoder for GIF -- and, only if the lossless pass still does
   not fit, that library's own bounded ladder (quality/palette, then downscale).

   This is the ONE place a byte SIZE is allowed to matter here, and it earns the
   exception: the alternative is not \"keep the original\", it is DROP -- the
   picture is refused and the model is told a file it can see on disk cannot be
   shown ([[com.blockether.vis.internal.attachments/wire-verdict]]). A payload
   that already fits is returned ITSELF, so the verbatim-bytes contract holds for
   every attachment the wire can take as it stands.

   Never bigger than `data`, never nil, never throws."
  ^bytes [^bytes data ^long max-bytes]
  (if (or (not *enabled?*) (nil? data) (<= (alength data) max-bytes))
    data
    (let [out (try (imaging/optimize data {:max-bytes max-bytes}) (catch Throwable _ nil))]
      (if (and out (pos? (alength ^bytes out)) (< (alength ^bytes out) (alength data))) out data))))

(def ^:const max-wire-dimension
  "Long-edge ceiling, in PIXELS, for a payload on a vision wire.

   Bytes are not the only cap a provider enforces. Anthropic refuses any image
   whose side exceeds 8000px -- and 2000px as soon as ONE request carries many
   images, which every long vis session becomes, because attachments REPLAY on
   every later turn:

     messages.0.content.52.image.source.base64.data: At least one of the image
     dimensions exceed max allowed size for many-image requests: 2000 pixels

   That is a hard 400 on a request whose predecessor was fine, and -- since the
   same rows replay -- it repeats until the offending attachment leaves the
   session. So the send gate clamps the PICTURE, not only its weight.

   1568 sits deliberately below the 2000 ceiling: it is the long edge Anthropic
   downscales to on its own side, so a retina screenshot reaches the model as
   the same picture it would have seen anyway, in fewer bytes and fewer tokens,
   on every provider."
  1568)

(defn fit-dimensions
  "Downscale an already wire-safe payload until NEITHER side exceeds
   `max-dimension`, preserving aspect ratio and container (`imaging/optimize`'s
   `:max-width`/`:max-height`, so an animated GIF stays animated).

   The third and last exception to \"never re-encode\", and it earns it exactly
   the way [[fit-within]] does: the alternative is not \"send the original\", it
   is a provider 400 that replays on every later turn ([[max-wire-dimension]]).

   Returns `{:bytes <b> :width <w> :height <h>}` -- `data` ITSELF when it already
   fits, so the verbatim-bytes contract holds for every picture within the
   ceiling -- or `{:reason <why>}` when an oversized picture cannot be brought
   under it. Never nil, never throws."
  ([^bytes data] (fit-dimensions data max-wire-dimension))
  ([^bytes data ^long max-dimension]
   (let
     [probe
      (when (and *enabled?* data (pos? (alength data)))
        (try (imaging/probe data) (catch Throwable _ nil)))

      w
      (long (or (:width probe) 0))

      h
      (long (or (:height probe) 0))]

     (cond
       ;; Nothing to measure (conversion off, or a container the prober cannot
       ;; read): best effort beats dropping a picture over a size never seen.
       (or (nil? probe) (not (pos? w)) (not (pos? h))) {:bytes data}
       (and (<= w max-dimension) (<= h max-dimension)) {:bytes data :width w :height h}
       :else (let
               [out
                (try (imaging/optimize
                       data
                       {:max-width max-dimension :max-height max-dimension :force true})
                     (catch Throwable _ nil))

                scaled
                (when (and out (pos? (alength ^bytes out)))
                  (try (imaging/probe out) (catch Throwable _ nil)))

                w2
                (long (or (:width scaled) 0))

                h2
                (long (or (:height scaled) 0))]

               (if (and (pos? w2) (pos? h2) (<= w2 max-dimension) (<= h2 max-dimension))
                 {:bytes out :width w2 :height h2}
                 {:reason (str w
                               "x"
                               h
                               " exceeds the "
                               max-dimension
                               "px per-side limit for images and could not be downscaled")}))))))

(def ^:const video-gif-max-dimension
  "Long-edge ceiling for the GIF a clip becomes. A model reads a contact sheet,
   not a film: 320px keeps a UI flow or a crash repro legible while leaving
   [[video-gif-max-frames]] frames comfortably inside the per-image cap."
  320)

(def ^:const video-gif-max-frames
  "Frames kept from a clip. Enough to read motion, few enough that a 30s screen
   recording stays a few hundred KB."
  24)

(def ^:const video-gif-fps
  "Playback rate of the produced GIF. Slow on purpose: the frames are strided
   samples of the WHOLE clip, not consecutive ones."
  6)

(defn video->wire-gif
  "Turn a video container (MP4 / QuickTime) into an animated GIF every vision
   wire accepts, sampled across the clip's whole length: at most
   [[video-gif-max-frames]] evenly strided frames, long edge
   [[video-gif-max-dimension]].

   A clip is the one attachment NO provider takes in any form, so the choice is
   not \"convert or pass through\", it is this or a blind turn -- the model gets
   the motion at a legible size instead of nothing at all. Sampling is strided
   rather than truncated on purpose: the first 24 frames of a screen recording
   are its first second, which is exactly the part that shows nothing.

   Returns the same map as [[rasterize-svg]] plus `:frames`; `{:reason <why>}`
   when the clip cannot be decoded (an HEVC/AV1/VP9 track this build does not
   decode, a corrupt container); nil when conversion is DISABLED."
  ([^bytes data] (video->wire-gif data nil))
  ([^bytes data opts]
   (when (and *enabled?* data (pos? (alength data)))
     (try
       (let
         [probe
          (imaging/probe-video data)

          frames
          (long (or (:frames probe) 0))

          keep-n
          (long (or (:max-frames opts) video-gif-max-frames))

          stride
          (max 1 (long (Math/ceil (/ (double (max frames 1)) (double keep-n)))))]

         (cond (nil? probe) {:reason "the clip could not be read"}
               (false? (:is-decodable probe))
               {:reason (str "the clip's " (or (:codec probe) "video") " track cannot be decoded")}
               :else (let
                       [^bytes gif (imaging/video->gif data
                                                       {:max-frames keep-n
                                                        :stride stride
                                                        :max-dimension
                                                        (long (or (:max-dimension opts)
                                                                  video-gif-max-dimension))
                                                        :fps (or (:fps opts) video-gif-fps)})]
                       (if (and gif (pos? (alength gif)))
                         {:bytes gif
                          :media-type "image/gif"
                          :size (alength gif)
                          :original-size (alength data)
                          :width (:width probe)
                          :height (:height probe)
                          :frames (min keep-n (quot (+ frames stride -1) stride))}
                         {:reason "the clip decoded to no frames"}))))
       (catch Throwable t {:reason (str "the clip could not be decoded: " (failure-reason t))})))))

(defn- same-picture?
  "True when `b` probes as the SAME container, frame count and pixel dimensions
   as `a` -- the cheap guard that an \"optimiser\" stayed lossless. Colour type,
   channel count and alpha may legitimately change (oxipng drops a fully opaque
   alpha channel and palettes what it can); WIDTH x HEIGHT x FRAMES may not."
  [^bytes a ^bytes b]
  (boolean (try (let
                  [pa
                   (imaging/probe a)

                   pb
                   (imaging/probe b)]

                  (and pa
                       pb
                       (= (:format pa) (:format pb))
                       (= (:width pa) (:width pb))
                       (= (:height pa) (:height pb))
                       (= (:frames pa) (:frames pb))))
                (catch Throwable _ false))))

(defn compact
  "Losslessly re-compress an image payload on its way INTO the store: oxipng's
   filter/colour-type/palette search for PNG, jpegtran-style marker stripping
   for JPEG, gifsicle's differencing re-encoder for GIF -- `imaging/optimize`
   with NO options, which that library documents as pixel-preserving (only
   `:lossy`/`:quality`/`:format`/`:max-*` license a re-encode).

   The second exception to \"never re-compress\", and a narrower one than
   [[fit-within]]: the PICTURE is untouched, only its encoding is. A matplotlib
   or screenshot PNG comes out of a speed-tuned encoder and routinely costs
   several times the bytes it needs -- bytes that are then paid in the
   `session_attachment` BLOB, in every `/poll` + SSE replay of that row, and in
   every later turn the attachment replays. What the model and the human see is
   bit-identical, which is what the verbatim contract protects.

   Belt and braces, because a silent quality drop here would be invisible: the
   result is taken ONLY when it is strictly smaller AND still probes as the same
   format at the same dimensions and frame count ([[same-picture?]]).

   Never bigger than `data`, never nil, never throws."
  ^bytes [^bytes data]
  (if (or (not *enabled?*) (nil? data) (zero? (alength data)))
    data
    (let [out (try (imaging/optimize data) (catch Throwable _ nil))]
      (if (and out
               (pos? (alength ^bytes out))
               (< (alength ^bytes out) (alength data))
               (same-picture? data out))
        out
        data))))
