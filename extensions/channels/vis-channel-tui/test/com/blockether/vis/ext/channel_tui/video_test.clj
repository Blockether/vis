(ns com.blockether.vis.ext.channel-tui.video-test
  "MP4 playback, END TO END: a real H.264 file on disk -> jcodec demux/decode ->
   RGBA -> an animated GIF and terminal escape sequences.

   Deliberately an INTEGRATION test built on a REAL encode. The clip is a moving
   white bar whose position is a pure function of the frame index, which is what
   makes decode fidelity ASSERTABLE rather than merely plausible: frame N must
   show the bar in block N, so a decoder that returns the right frame COUNT with
   the wrong pixels still fails.

   That matters because the failure this namespace exists to prevent is silent.
   `FrameGrab.getNativeFrame` hands back the SAME `Picture` buffer every call, so
   collecting frames and reading them later yields the LAST frame N times over —
   with no exception, correct frame count, correct dimensions and a perfectly
   valid GIF at the end of it. Only comparing PIXELS across frames catches it."
  (:require [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [com.blockether.vis.ext.channel-tui.video :as video]
            [com.blockether.vis.internal.attachments :as att]
            [com.blockether.vis.internal.foundation.gif :as gif]
            [lazytest.core :refer [defdescribe describe expect it throws?]])
  (:import [java.io File]
           [org.jcodec.api SequenceEncoder]
           [org.jcodec.common.model ColorSpace Picture]))

(def ^:private clip-w 64)
(def ^:private clip-h 48)
(def ^:private clip-frames 10)
(def ^:private clip-fps 8)

(def ^:private block-px "Width of the moving bar, and so the stride of its position per frame." 8)

(defn- bar-picture
  "Frame `i` of the reference clip: a white bar `block-px` wide sitting in block
   `i mod 8` of an otherwise black frame. jcodec samples are SIGNED and offset by
   -128, so -128 is black and 127 is white."
  ^Picture [^long i]
  (let [p
        (Picture/create clip-w clip-h ColorSpace/RGB)

        ^bytes d
        (aget (.getData p) 0)

        lit
        (long (mod i (quot clip-w block-px)))]

    (dotimes [y clip-h]
      (dotimes [x clip-w]
        (let [o (* 3 (+ x (* y clip-w)))
              v (byte (if (= (quot x block-px) lit) 127 -128))]

          (aset d o v)
          (aset d (inc o) v)
          (aset d (+ o 2) v))))
    p))

(defn- reference-clip
  "A real `.mp4` on disk holding [[bar-picture]] frames 0..n-1."
  ^File []
  (let [out (File/createTempFile "vis-video-test" ".mp4")]
    (.deleteOnExit out)
    (let [enc (SequenceEncoder/createSequenceEncoder out (int clip-fps))]
      (dotimes [i clip-frames]
        (.encodeNativeFrame enc (bar-picture i)))
      (.finish enc))
    out))

(defn- bar-block
  "Which block the bar occupies in one decoded RGBA frame, by scanning a middle
   row for the first bright pixel. The inverse of [[bar-picture]]."
  [{:keys [width ^bytes rgba]}]
  (let [w
        (long width)

        y
        20]

    (first (for [x
                 (range w)

                 :let [v
                       (bit-and (aget rgba (* 4 (+ (long x) (* y w)))) 0xff)]
                 :when (> v 127)]

             (quot (long x) (long block-px))))))

(defdescribe mp4-sniffing-test
             (it "accepts an ISO base-media video and rejects still-image containers"
                 (let [head (byte-array 32)]
                   (with-open [in (java.io.FileInputStream. (reference-clip))]
                     (.read in head))
                   (expect (true? (video/mp4? head))))
                 ;; `ftyp` alone is NOT enough: HEIC/AVIF are ISO-BMFF too, and treating one as
                 ;; a video would send a still photo down the decoder.
                 (let [heic (byte-array (map unchecked-byte
                                             [0 0 0 24 0x66 0x74 0x79 0x70 0x68 0x65 0x69 0x63 0 0 0
                                              0]))]
                   (expect (false? (video/mp4? heic))))
                 (expect (false? (video/mp4? (byte-array (map unchecked-byte
                                                              [0x89 0x50 0x4e 0x47 13 10 26 10])))))
                 (expect (false? (video/mp4? (byte-array (map unchecked-byte
                                                              [0x47 0x49 0x46 0x38 0x39 0x61])))))
                 (expect (false? (video/mp4? (byte-array 3))))
                 (expect (false? (video/mp4? nil)))))

(defdescribe probe-test
             (it "reads codec, geometry and timing without decoding pixels"
                 (let [m (video/probe (reference-clip))]
                   (expect (= :h264 (:codec m)))
                   (expect (= clip-w (:width m)))
                   (expect (= clip-h (:height m)))
                   (expect (= clip-frames (:frames m)))
                   (expect (= (double clip-fps) (double (:fps m))))
                   (expect (< 1.2 (double (:duration-s m)) 1.3))))
             (it "is nil for something that is not a video, and says so via decodable?"
                 (let [f (File/createTempFile "vis-video-test" ".png")]
                   (.deleteOnExit f)
                   (expect (nil? (video/probe f)))
                   (expect (false? (video/decodable? f))))))

(defdescribe decode-frames-test
             (it "returns every frame at the clip's own geometry"
                 (let [d (video/decode-frames (reference-clip))]
                   (expect (= clip-w (:width d)))
                   (expect (= clip-h (:height d)))
                   (expect (= clip-frames (count (:frames d))))
                   (expect (= (range clip-frames) (map :index (:frames d))))
                   ;; straight RGBA8, four bytes a pixel
                   (expect (= (* clip-w clip-h 4) (alength ^bytes (:rgba (first (:frames d))))))))
             (it "decodes each frame's OWN pixels (the shared-buffer regression)"
                 ;; The bar walks one block per frame and wraps after 8, so frames 0..9 must
                 ;; read back as blocks 0,1,2,3,4,5,6,7,0,1. A decoder handing back a reused
                 ;; buffer yields the last frame ten times: [1 1 1 1 1 1 1 1 1 1].
                 (let [d (video/decode-frames (reference-clip))]
                   (expect (= [0 1 2 3 4 5 6 7 0 1] (mapv bar-block (:frames d))))))
             (it "honours max-frames and stride"
                 (let [capped
                       (video/decode-frames (reference-clip) {:max-frames 4})

                       strided
                       (video/decode-frames (reference-clip) {:stride 3})]

                   (expect (= 4 (count (:frames capped))))
                   (expect (= [0 1 2 3] (mapv bar-block (:frames capped))))
                   ;; every 3rd frame: indices 0,3,6,9 -> blocks 0,3,6,1
                   (expect (= [0 3 6 9] (mapv :index (:frames strided))))
                   (expect (= [0 3 6 1] (mapv bar-block (:frames strided))))
                   ;; sampling slows the clip down, so the reported rate must follow
                   (expect (= (/ (double clip-fps) 3) (double (:fps strided))))))
             (it "downscales to max-dimension, preserving aspect"
                 (let [d (video/decode-frames (reference-clip) {:max-dimension 32})]
                   (expect (= 32 (:width d)))
                   (expect (= 24 (:height d)))
                   (expect (= (* 32 24 4) (alength ^bytes (:rgba (first (:frames d))))))))
             (it "refuses a non-video with a reason a caller can branch on"
                 (let [f (File/createTempFile "vis-video-test" ".png")]
                   (.deleteOnExit f)
                   (expect (throws? clojure.lang.ExceptionInfo #(video/decode-frames f)))
                   (expect (= :not-mp4
                              (try (video/decode-frames f)
                                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(defdescribe ->gif-test
             (it "transcodes a clip into an animated GIF the rest of vis already understands"
                 (let [g (video/->gif (reference-clip) {})]
                   ;; vis's OWN attachment sniffer must recognise it, and the result has to be
                   ;; a format the vision wire carries VERBATIM -- that is what lets a model
                   ;; actually see a video the user dropped in.
                   (expect (= "image/gif" (att/detect-image-mime g)))
                   (expect (true? (att/provider-image-media-type? (att/detect-image-mime g))))
                   (let [back (gif/decode g)]
                     (expect (= clip-w (:width back)))
                     (expect (= clip-h (:height back)))
                     (expect (= clip-frames (count (:frames back))))
                     ;; 8 fps is 125ms a frame, but GIF stores delays in CENTIseconds,
                     ;; so the wire value is quantized down to 12cs. Asserting the
                     ;; quantized number keeps this honest about what a GIF can carry.
                     (expect (= 120 (:delay-ms (first (:frames back)))))
                     ;; -1 is the cdylib's "loop forever"
                     (expect (= -1 (:loop-count back))))))
             (it "keeps the animation moving through the palette quantizer"
                 ;; A GIF whose frames all came out identical would still decode cleanly.
                 (let [back
                       (gif/decode (video/->gif (reference-clip) {}))

                       hashes
                       (map #(java.util.Arrays/hashCode ^ints (:argb %)) (:frames back))]

                   ;; the bar has 8 distinct positions across the 10 frames
                   (expect (= 8 (count (distinct hashes)))))))

(defdescribe playback-sequences-test
             (describe "kitty"
                       (it "emits a graphics escape per frame and reuses ONE image id"
                           (let [d
                                 (video/decode-frames (reference-clip))

                                 p
                                 (video/playback-sequences d {:protocol :kitty :cols 20})

                                 e
                                 (:escape (first (:frames p)))]

                             (expect (= :kitty (:protocol p)))
                             (expect (= clip-frames (count (:frames p))))
                             (expect (.startsWith ^String e "\u001b8"))
                             (expect (.contains ^String e "\u001b_G"))
                             ;; A fresh id per frame would pin one upload per frame in the terminal's
                             ;; image memory for the whole clip.
                             (expect (= ["i=9901"] (vec (distinct (re-seq #"i=\d+" e)))))
                             ;; and the frames really are different pictures
                             (expect (= 8 (count (distinct (map :escape (:frames p)))))))))
             (describe "iterm2"
                       (it "emits an inline-image sequence per frame"
                           (let [d
                                 (video/decode-frames (reference-clip))

                                 p
                                 (video/playback-sequences d {:protocol :iterm2 :cols 20})]

                             (expect (= :iterm2 (:protocol p)))
                             (expect (.contains ^String (:escape (first (:frames p)))
                                                "\u001b]1337;File="))
                             (expect (= 8 (count (distinct (map :escape (:frames p)))))))))
             (it "paces frames at the clip's own rate"
                 (let [d
                       (video/decode-frames (reference-clip))

                       p
                       (video/playback-sequences d {:protocol :kitty :cols 20})]

                   (expect (= 125 (:delay-ms (first (:frames p)))))))
             (it "fits the picture into the requested cell box, aspect-preserving"
                 (let [d
                       (video/decode-frames (reference-clip))

                       p
                       (video/playback-sequences d {:protocol :kitty :cols 20})]

                   (expect (= 20 (:cols p)))
                   (expect (pos? (long (:rows p))))))
             (it "is nil when the terminal cannot draw images at all"
                 ;; A plain TERM detects no protocol, and then there is nothing to draw:
                 ;; callers fall back to a text card rather than spraying escape bytes at
                 ;; a dumb terminal. Detection is redefined because the SUITE may itself be
                 ;; running inside a graphical terminal.
                 (let [d (video/decode-frames (reference-clip))]
                   (with-redefs [timg/images-protocol (constantly nil)]
                     (expect (nil? (video/playback-sequences d {:cols 20})))))))

(defdescribe play!-test
             (it "writes the clip and leaves no image or cursor behind"
                 (let [bos
                       (java.io.ByteArrayOutputStream.)

                       res
                       (video/play! (reference-clip)
                                    {:out bos :protocol :kitty :cols 20 :max-frames 3})

                       s
                       (.toString bos "UTF-8")]

                   (expect (= 3 (:frames res)))
                   (expect (= :kitty (:protocol res)))
                   ;; saves the cursor once up front...
                   (expect (.startsWith s "\u001b7"))
                   ;; ...and frees the uploaded image at the end, so a played clip does not
                   ;; keep floating above the text.
                   (expect (.contains s "a=d")))))

(defdescribe playback-sequences-arg-test
             (it "refuses a file instead of silently rendering zero frames"
                 (expect (throws? clojure.lang.ExceptionInfo
                                  #(video/playback-sequences (reference-clip)
                                                             {:protocol :kitty :cols 20})))))

;; ── A clip on a STILL-image surface ──────────────────────────────────────────
;;
;; Every inline-image surface in the TUI (the Kitty/iTerm2 renderer, the paste
;; probe, the transcript fence) draws ONE picture. These tests pin the contract
;; that makes an MP4 usable there — it shows its poster frame — and that getting
;; that poster costs one keyframe rather than the whole timeline.

(defdescribe video-file-sniffing-test
             (it "recognises a clip by MAGIC BYTES, not by its name"
                 (let [clip
                       (reference-clip)

                       fake
                       (File/createTempFile "vis-not-a-movie" ".mp4")]

                   (.deleteOnExit fake)
                   (spit fake "this is text, not a movie")
                   (expect (video/video-file? clip))
                   (expect (= "video/mp4" (video/media-type clip)))
                   (expect (not (video/video-file? fake)))
                   (expect (nil? (video/media-type fake)))
                   (expect (not (video/video-file? (File. "/nope/missing.mp4"))))))
             (it "labels a QuickTime container from its name once the BYTES agree"
                 (let [clip
                       (reference-clip)

                       mov
                       (File/createTempFile "vis-video-test" ".mov")]

                   (.deleteOnExit mov)
                   (java.nio.file.Files/copy (.toPath clip)
                                             (.toPath mov)
                                             ^"[Ljava.nio.file.CopyOption;"
                                             (into-array
                                               java.nio.file.CopyOption
                                               [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
                   (expect (= "video/quicktime" (video/media-type mov))))))

(defdescribe poster-test
             (it "is the first picture, decoded ONCE and at draw size"
                 (let [clip
                       (reference-clip)

                       seen
                       (atom nil)

                       real
                       video/decode-frames

                       p
                       (with-redefs [video/decode-frames (fn [src opts]
                                                           (reset! seen opts)
                                                           (real src opts))]
                         (video/poster clip {:max-dimension 32}))]

                   ;; The whole point: ONE keyframe at the size it will be drawn,
                   ;; never the timeline, and encoded by the cdylib rather than
                   ;; carried over as RGBA. A 240-frame clip must cost the same.
                   (expect (= {:max-frames 1 :max-dimension 32 :encoding :png} @seen))
                   ;; PNG magic 0x89 'P' 'N' 'G'
                   (expect (= [-119 80 78 71] (mapv #(aget ^bytes (:png p) %) (range 4))))
                   (expect (= {:w (:width p) :h (:height p)}
                              (timg/image-dimensions (:png p) "image/png")))
                   (expect (<= (long (:width p)) 32))))
             (it "keeps the clip's own size when nothing constrains it"
                 (let [p (video/poster (reference-clip))]
                   (expect (= clip-w (:width p)))
                   (expect (= clip-h (:height p))))))

(defdescribe
  video-in-terminal-image-test
  (it "Kitty gets a PNG poster for an MP4 — that wire is PNG-only"
      (let [clip
            (reference-clip)

            {:keys [data w h]}
            (timg/kitty-png (.getAbsolutePath clip) "video/mp4" {:cols 20 :rows 10})

            bs
            ;; RAW PNG bytes, not base64: the fork base64s them as it
            ;; chunks the escape, so nothing here builds the 1.33x String.
            data]

        (expect (= [-119 80 78 71] (mapv #(aget ^bytes bs %) (range 4))))
        (expect (= {:w w :h h} (timg/image-dimensions bs "image/png")))))
  (it "raw bytes and base64 produce the IDENTICAL escape, in every encoder"
      (let [bs
            (:data
              (timg/kitty-png (.getAbsolutePath (reference-clip)) "video/mp4" {:cols 20 :rows 10}))

            b64
            (.encodeToString (java.util.Base64/getEncoder) ^bytes bs)]

        ;; The saving is the intermediate String, never the picture:
        ;; whatever reaches the terminal must be byte-for-byte the same.
        (expect (= (timg/encode-kitty b64 {:cols 20 :rows 10})
                   (timg/encode-kitty bs {:cols 20 :rows 10})))
        (expect (= (timg/kitty-transmit b64 4242) (timg/kitty-transmit bs 4242)))
        (expect (= (timg/encode-iterm2 b64 {:cols 20}) (timg/encode-iterm2 bs {:cols 20})))))
  (it "sizes a clip's cell box from the CONTAINER, decoding nothing"
      (let [clip (reference-clip)]
        (expect (= {:w clip-w :h clip-h}
                   (timg/probe-dimensions (.getAbsolutePath clip) "video/mp4")))
        ;; and with no media type either — a dropped path carries none
        (expect (= {:w clip-w :h clip-h} (timg/probe-dimensions (.getAbsolutePath clip) nil)))))
  (it "a dropped clip path probes exactly like a dropped picture"
      (let [clip
            (reference-clip)

            d
            (timg/probe-paste-image (.getAbsolutePath clip) {})]

        (expect (= "video/mp4" (:mime d)))
        (expect (= [clip-w clip-h] [(:width d) (:height d)]))
        (expect (= (.getName clip) (:filename d)))
        (expect (pos? (long (:size d))))))
  (it "renders in both protocols — iTerm2 gets the poster, never raw MP4 bytes"
      (let [path
            (.getAbsolutePath (reference-clip))

            k
            (with-redefs [timg/images-protocol (constantly :kitty)]
              (timg/render-sequence path "video/mp4" {:cols 20 :rows 10}))

            i
            (with-redefs [timg/images-protocol (constantly :iterm2)]
              (timg/render-sequence path "video/mp4" {:cols 20 :rows 10}))

            payload
            (.decode (java.util.Base64/getDecoder)
                     ^String (second (re-find #":([A-Za-z0-9+/=]{16,})" i)))]

        (expect (.contains ^String k "\u001b_G"))
        (expect (.contains ^String i "\u001b]1337;File="))
        ;; iTerm2 would happily inline the MP4's own bytes as a broken
        ;; file card; what it must receive is the decoded poster PNG.
        (expect (= [-119 80 78 71] (mapv #(aget ^bytes payload %) (range 4)))))))

;; ── Playback cost ────────────────────────────────────────────────────────────

(defdescribe playback-laziness-test
             (it "encodes a frame only when playback reaches it"
                 (let [d
                       (video/decode-frames (reference-clip))

                       n
                       (atom 0)

                       real
                       video/frame->png]

                   (with-redefs [video/frame->png (fn [f]
                                                    (swap! n inc)
                                                    (real f))]
                     (let [p (video/playback-sequences d {:protocol :kitty :cols 20})]
                       ;; nothing is encoded up front...
                       (expect (zero? @n))
                       ;; ...the frame COUNT is known without encoding anything...
                       (expect (= clip-frames (:frame-count p)))
                       ;; ...and asking for ONE frame encodes exactly one. `map`
                       ;; would chunk 32 frames ahead, which is tens of megabytes
                       ;; of base64 and a stall before the first picture appears.
                       (expect (some? (:escape (first (:frames p)))))
                       (expect (= 1 @n))
                       (expect (= clip-frames (count (:frames p))))
                       (expect (= clip-frames @n)))))))

(defdescribe
  playback-decode-size-test
  (it "decodes to the CELL BOX by default, not the clip's full resolution"
      (let [seen
            (atom nil)

            real
            video/decode-frames

            bos
            (java.io.ByteArrayOutputStream.)]

        (with-redefs [video/decode-frames (fn [src opts]
                                            (reset! seen opts)
                                            (real src opts))]
          (video/play! (reference-clip) {:out bos :protocol :kitty :cols 20 :max-frames 2}))
        ;; 20 cols x 9px = 180px of terminal; decoding 1080p for that
        ;; box doubles decode time and sextuples the escape bytes.
        (expect (= (timg/box-pixels 20 nil) (:max-dimension @seen)))))
  (it "an explicit :max-dimension always wins"
      (let [seen
            (atom nil)

            real
            video/decode-frames

            bos
            (java.io.ByteArrayOutputStream.)]

        (with-redefs [video/decode-frames (fn [src opts]
                                            (reset! seen opts)
                                            (real src opts))]
          (video/play! (reference-clip)
                       {:out bos :protocol :kitty :cols 20 :max-frames 2 :max-dimension 24}))
        (expect (= 24 (:max-dimension @seen))))))

(defdescribe
  native-png-frames-test
  (it
    "asks the cdylib for PNG frames instead of hauling RGBA over the FFI boundary"
    (let [clip
          (reference-clip)

          raw
          (video/decode-frames clip)

          png
          (video/decode-frames clip {:encoding :png})

          f
          (first (:frames png))]

      (expect (= clip-frames (count (:frames png))))
      ;; The frame carries an ENCODED still, not a canvas...
      (expect (nil? (:rgba f)))
      (expect (= [-119 80 78 71] (mapv #(aget ^bytes (:png f) %) (range 4))))
      ;; ...which `frame->png` hands straight back — no second encode.
      (expect (identical? (:png f) (video/frame->png f)))
      ;; RGBA frames still work, and are encoded on demand.
      (expect (some? (:rgba (first (:frames raw)))))
      (expect (= [-119 80 78 71]
                 (mapv #(aget ^bytes (video/frame->png (first (:frames raw))) %) (range 4))))
      ;; Same picture either way: the escape stream is byte-identical,
      ;; so the cheap path is a pure saving, not a different image.
      (expect (= (mapv :escape (:frames (video/playback-sequences raw {:protocol :kitty :cols 20})))
                 (mapv :escape
                       (:frames (video/playback-sequences png {:protocol :kitty :cols 20})))))
      ;; RGBA for this clip is already 5x the PNG; at 720p it is 133 MB
      ;; against 9 MB, which is the difference between playback fitting
      ;; in the heap and not.
      (expect (< (reduce + (map #(count (:png %)) (:frames png)))
                 (reduce + (map #(count (:rgba %)) (:frames raw))))))))

(defdescribe playback-encoding-default-test
             (it "play! defaults to the encoded-frame path"
                 (let [seen
                       (atom nil)

                       real
                       video/decode-frames

                       bos
                       (java.io.ByteArrayOutputStream.)]

                   (with-redefs [video/decode-frames (fn [src opts]
                                                       (reset! seen opts)
                                                       (real src opts))]
                     (video/play! (reference-clip) {:out bos :protocol :kitty :cols 20 :rows 10})
                     (expect (= :png (:encoding @seen)))
                     ;; An explicit choice still wins.
                     (video/play! (reference-clip)
                                  {:out bos :protocol :kitty :cols 20 :rows 10 :encoding :rgba})
                     (expect (= :rgba (:encoding @seen)))))))

(defdescribe video-attachment-materialization-test
             (it "re-renders a persisted clip attachment after a resume"
                 (let [clip
                       (reference-clip)

                       att
                       {"id" (str "vid-" (System/currentTimeMillis))
                        "media_type" "video/mp4"
                        "filename" "clip.mp4"
                        "base64" (.encodeToString (java.util.Base64/getEncoder)
                                                  (java.nio.file.Files/readAllBytes (.toPath
                                                                                      clip)))}

                       d
                       (timg/materialize-attachment att)]

                   (expect (some? d))
                   ;; Cached under its OWN extension: a `.png` named MP4 could be
                   ;; neither probed nor decoded on the next paint.
                   (expect (.endsWith ^String (:path d) ".mp4"))
                   (expect (= "video/mp4" (:mime d)))
                   ;; and it is a real clip on disk, not an opaque blob: the
                   ;; descriptor carries the size the fence needs to lay out.
                   (expect (= [clip-w clip-h] [(:width d) (:height d)]))
                   (expect (= clip-frames (:frames (video/probe (:path d))))))))
