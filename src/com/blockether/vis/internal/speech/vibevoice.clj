(ns com.blockether.vis.internal.speech.vibevoice
  "Microsoft VibeVoice speech through ONNX Runtime, without sherpa-onnx.

   Two engines share one runtime here:

     :vibevoice-local      VibeVoice-Realtime-0.5B text to speech. A voice is a
                           cached prompt state in `voices.bin`, so the catalogue is
                           fixed by the bundle.
     :vibevoice-asr-local  VibeVoice-ASR-Streaming-1.5B transcription, in chunks of
                           22 frames with 4 frames of lookahead.

   The graphs, the step loops and the noise generator follow the reference runners
   of the vis-podcaster repository (`scripts/vibevoice_onnx.py` and
   `scripts/vibevoice_asr_onnx.py`). A change to a loop here changes the audio, so
   change the reference runner with it.

   The KV caches stay in native memory: each step feeds the tensors of the previous
   `OrtSession$Result` back in, and that result closes only after its replacement
   exists."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.inference.runtime :as runtime]
            [com.blockether.vis.internal.speech.asr :as asr]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.engine :as engine]
            [com.blockether.vis.internal.speech.transcode :as transcode]
            [com.blockether.vis.internal.speech.tts :as tts])
  (:import [ai.djl.huggingface.tokenizers HuggingFaceTokenizer]
           [ai.djl.huggingface.tokenizers.jni TokenizersLibrary]
           [ai.onnxruntime OnnxTensor OrtEnvironment OrtSession OrtSession$Result
            OrtSession$SessionOptions TensorInfo]
           [java.io File FileOutputStream RandomAccessFile]
           [java.nio ByteBuffer ByteOrder FloatBuffer LongBuffer]
           [java.nio.file Files]
           [java.util HashMap Map Optional]))

(set! *warn-on-reflection* true)

(def ^:const tts-asset-id "vibevoice-realtime-0.5b-int8")

(def ^:const asr-asset-id "vibevoice-asr-streaming-1.5b-int8")

(def ^:const cfg-scale "Classifier-free guidance of each diffusion step, as upstream ships it." 1.5)

(def ^:const max-positions
  "The longest TTS language-model context. Generation stops here even without an
   end-of-speech signal."
  8192)

(def ^:const max-chunk-tokens "The most text tokens one ASR chunk may emit." 256)

(def ^:const default-seed
  "Noise seed of a request without one, so the same text in the same voice gives
   the same audio."
  7)

;; Portable noise - SplitMix64 uniforms and Box-Muller pairs, as in the reference
;; runner. The constants are the unsigned SplitMix64 constants as signed longs.

(defn noise-state "A fresh noise generator for `seed`." ^longs [seed] (long-array [(long seed)]))

(defn- next-u64
  ^long [^longs state]
  (let [s
        (unchecked-add (aget state 0) -7046029254386353131)

        _
        (aset state 0 s)

        z
        (unchecked-multiply (bit-xor s (unsigned-bit-shift-right s 30)) -4658895280553007687)

        z
        (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) -7723592293110705685)]

    (bit-xor z (unsigned-bit-shift-right z 31))))

(defn uniform
  ^double [^longs state]
  (* (double (unsigned-bit-shift-right (next-u64 state) 11)) (/ 1.0 9007199254740992.0)))

(defn gaussian
  "`n` standard normal values. Each pair uses two uniforms; an odd `n` drops the
   last sine value, as the reference does."
  ^floats [^longs state ^long n]
  (let [out (float-array n)]
    (loop [i 0]
      (when (< i n)
        (let [r (Math/sqrt (* -2.0 (Math/log (- 1.0 (uniform state)))))
              a (* 2.0 Math/PI (uniform state))]

          (aset out i (float (* r (Math/cos a))))
          (when (< (inc i) n) (aset out (inc i) (float (* r (Math/sin a)))))
          (recur (+ i 2)))))
    out))

;; ONNX Runtime

(defn environment ^OrtEnvironment [] (runtime/ensure-ort!) (OrtEnvironment/getEnvironment))

(defn float-tensor
  ^OnnxTensor [^OrtEnvironment env ^floats data shape]
  (OnnxTensor/createTensor env (FloatBuffer/wrap data) (long-array shape)))

(defn long-tensor
  ^OnnxTensor [^OrtEnvironment env ^longs data shape]
  (OnnxTensor/createTensor env (LongBuffer/wrap data) (long-array shape)))

(defn output
  ^OnnxTensor [^OrtSession$Result result ^String output-name]
  (.get ^Optional (.get result output-name)))

(defn- tensor-shape ^longs [^OnnxTensor tensor] (.getShape ^TensorInfo (.getInfo tensor)))

(defn floats-of
  ^floats [^OnnxTensor tensor]
  (let [b
        (.getFloatBuffer tensor)

        a
        (float-array (.remaining b))]

    (.get b a)
    a))

(defn run-session ^OrtSession$Result [^OrtSession session ^Map inputs] (.run session inputs))

(defn- kv-names
  "`past_key_0 past_value_0 …` (or `present_…`) for `layers` layers, in feed order."
  [prefix layers]
  (vec (for [i
             (range layers)

             kind
             ["key" "value"]]

         (str prefix "_" kind "_" i))))

(defn- feed
  ^HashMap [named kv]
  (let [m (HashMap.)]
    (doseq [[k v] named]
      (.put m k v))
    (doseq [[k v] (map vector (kv-names "past" (quot (count kv) 2)) kv)]
      (.put m k v))
    m))

(defn- presents
  [^OrtSession$Result result layers]
  (mapv #(output result %) (kv-names "present" layers)))

(defn holder
  "Three result slots: storing a result closes the one it replaces, and
   `close-all!` closes what is left."
  []
  (object-array 3))

(defn hold!
  ^OrtSession$Result [^objects slots ^long i ^OrtSession$Result result]
  (when-let [old (aget slots i)]
    (.close ^OrtSession$Result old))
  (aset slots i result)
  result)

(defn close-all!
  [^objects slots]
  (dotimes [i (alength slots)]
    (when-let [r (aget slots i)]
      (.close ^OrtSession$Result r)
      (aset slots i nil))))

;; Loaded bundles - one per install dir, kept for the life of the process

(defonce ^:private loaded* (atom {}))

(defn read-json [^File f] (json/read-json (slurp f)))

(defn open-session
  ^OrtSession [^OrtEnvironment env ^File file]
  (with-open [options (OrtSession$SessionOptions.)]
    (.createSession env (.getAbsolutePath file) options)))

(defn open-tokenizer
  ^HuggingFaceTokenizer [^File file]
  (HuggingFaceTokenizer/newInstance (.toPath file) {"truncation" "false" "padding" "false"}))

(defn loaded
  "The bundle in `dir`, opened by `open` once and shared."
  [kind ^File dir open]
  (let [k [kind (.getAbsolutePath dir)]]
    (or (get @loaded* k)
        (locking loaded*
          (or (get @loaded* k)
              (let [bundle (open (environment) dir)]
                (swap! loaded* assoc k bundle)
                bundle))))))

(defn release!
  "Close every loaded session. The next request loads its bundle again."
  []
  (locking loaded*
    (doseq [bundle
            (vals @loaded*)

            v
            (vals bundle)

            :when (instance? OrtSession v)]

      (.close ^OrtSession v))
    (reset! loaded* {})))

(defn token-ids
  "Token ids of `text` without special tokens. DJL `Encoding` requests character
   spans that fail in native-image, so this reads the ids straight from the library."
  [^HuggingFaceTokenizer tokenizer ^String text]
  (let [^TokenizersLibrary library
        TokenizersLibrary/LIB

        encoding
        (.encode library (long (.getHandle tokenizer)) text false)]

    (try (vec (.getTokenIds library encoding)) (finally (.deleteEncoding library encoding)))))

;; WAV

(defn write-wav!
  "Write mono float `samples` in -1..1 as a 16-bit PCM WAV."
  [^File out ^floats samples ^long sample-rate]
  (let [n
        (alength samples)

        data-bytes
        (* 2 n)

        bb
        (doto (ByteBuffer/allocate (+ 44 data-bytes)) (.order ByteOrder/LITTLE_ENDIAN))

        ascii
        (fn [^String s]
          (.put bb (.getBytes s "US-ASCII")))]

    (ascii "RIFF")
    (.putInt bb (int (+ 36 data-bytes)))
    (ascii "WAVE")
    (ascii "fmt ")
    (.putInt bb 16)
    (.putShort bb (short 1))
    (.putShort bb (short 1))
    (.putInt bb (int sample-rate))
    (.putInt bb (int (* 2 sample-rate)))
    (.putShort bb (short 2))
    (.putShort bb (short 16))
    (ascii "data")
    (.putInt bb (int data-bytes))
    (dotimes [i n]
      (.putShort bb
                 (short (Math/round (* 32767.0 (max -1.0 (min 1.0 (double (aget samples i)))))))))
    (with-open [o (FileOutputStream. out)]
      (.write o (.array bb)))
    out))

(defn read-wav
  "A 16-bit PCM WAV as `{:samples <mono floats> :sample-rate n}`. Channels are
   averaged. Validate the file first: this reader trusts its chunk sizes."
  [^File file]
  (let [bb
        (doto (ByteBuffer/wrap (Files/readAllBytes (.toPath file)))
          (.order ByteOrder/LITTLE_ENDIAN))

        tag
        (fn [^long at]
          (String. (.array bb) (int at) 4 "US-ASCII"))]

    (loop [pos
           12

           fmt
           nil]

      (let [id
            (tag pos)

            size
            (bit-and (long (.getInt bb (int (+ pos 4)))) 0xffffffff)

            body
            (+ pos 8)]

        (cond (= id "fmt ") (recur (+ body size (rem size 2))
                                   {:channels (long (.getShort bb (int (+ body 2))))
                                    :sample-rate (long (.getInt bb (int (+ body 4))))})
              (= id "data")
              (let [channels
                    (max 1 (long (:channels fmt)))

                    frames
                    (quot size (* 2 channels))

                    out
                    (float-array frames)]

                (dotimes [f frames]
                  (loop [c 0
                         acc 0.0]

                    (if (< c channels)
                      (recur (inc c)
                             (+ acc
                                (/ (double (.getShort bb (int (+ body (* 2 (+ c (* f channels)))))))
                                   32768.0)))
                      (aset out f (float (/ acc channels))))))
                {:samples out :sample-rate (:sample-rate fmt)})
              :else (recur (+ body size (rem size 2)) fmt))))))

(defn resample
  "`samples` at `from` Hz as `to` Hz, with a Hann-windowed sinc of 16 zero
   crossings on each side. The cutoff follows the lower of the two rates."
  ^floats [^floats samples ^long from ^long to]
  (if (= from to)
    samples
    (let [ratio
          (/ (double to) (double from))

          cutoff
          (min 1.0 ratio)

          width
          (/ 16.0 cutoff)

          n
          (alength samples)

          out
          (float-array (long (Math/floor (* n ratio))))]

      (dotimes [i (alength out)]
        (let [t (/ (double i) ratio)
              hi (min (dec n) (long (Math/floor (+ t width))))]

          (loop [j (max 0 (long (Math/ceil (- t width))))
                 acc 0.0]

            (if (> j hi)
              (aset out i (float acc))
              (let [d (- (double j) t)
                    x (* cutoff d)
                    sinc (if (zero? x) 1.0 (/ (Math/sin (* Math/PI x)) (* Math/PI x)))
                    w (+ 0.5 (* 0.5 (Math/cos (/ (* Math/PI d) width))))]

                (recur (inc j) (+ acc (* (double (aget samples j)) cutoff sinc w))))))))
      out)))

;; Text to speech

(defn tts-asset [] (assets/entry tts-asset-id))

(defn voices
  "The voices the bundle speaks in, from the manifest, so a surface can list them
   before the download."
  []
  (mapv #(select-keys % [:id :label :language :gender]) (:voices (tts-asset))))

(defn- bf16-floats
  ^floats [^RandomAccessFile raf ^long offset ^long n-bytes]
  (let [raw
        (byte-array n-bytes)

        out
        (float-array (quot n-bytes 2))]

    (.seek raf offset)
    (.readFully raf raw)
    (dotimes [i (alength out)]
      (let [lo (bit-and (long (aget raw (* 2 i))) 0xff)
            hi (bit-and (long (aget raw (inc (* 2 i)))) 0xff)]

        (aset out
              i
              (Float/intBitsToFloat (unchecked-int (bit-shift-left (bit-or lo (bit-shift-left hi 8))
                                                                   16))))))
    out))

(defn- open-tts
  [^OrtEnvironment env ^File dir]
  (let [f #(io/file dir %)]
    {:env env
     :meta (read-json (f "vibevoice.json"))
     :index (read-json (f "voices.json"))
     :voices (atom {})
     :tokenizer (open-tokenizer (f "tokenizer.json"))
     :base (open-session env (f "base_lm.onnx"))
     :tts (open-session env (f "tts_lm.onnx"))
     :diffusion (open-session env (f "diffusion.onnx"))
     :decoder (open-session env (f "acoustic_decoder.onnx"))
     :dir dir}))

(defn- voice-tensors
  "The prompt state of `voice-id` as tensors, read once and kept: they are only
   ever inputs."
  [{:keys [^OrtEnvironment env index voices ^File dir]} voice-id]
  (or (get @voices voice-id)
      (let [voice
            (or (first (filter #(= voice-id (get % "id")) (get index "voices")))
                (throw (ex-info (str "No VibeVoice voice named " (pr-str voice-id))
                                {:type :speech-tts/unknown-voice
                                 :family :vibevoice
                                 :voice-id voice-id
                                 :known (mapv #(get % "id") (get index "voices"))})))

            tensors
            (with-open [raf (RandomAccessFile. (io/file dir "voices.bin") "r")]
              (into {}
                    (map (fn [t]
                           [(get t "name")
                            (float-tensor
                              env
                              (bf16-floats raf (long (get t "offset")) (long (get t "bytes")))
                              (get t "shape"))]))
                    (get voice "tensors")))]

        (swap! voices assoc voice-id tensors)
        tensors)))

(defn- voice-kv
  [tensors prefix layers]
  (mapv #(get tensors (str prefix "." %)) (kv-names "past" layers)))

(defn- tts-step
  "One TTS language-model step over `hidden` with every position of `type-id`."
  ^OrtSession$Result [{:keys [^OrtEnvironment env tts]} ^OnnxTensor hidden ^long type-id kv]
  (let [s (aget (tensor-shape hidden) 1)]
    (with-open [types (long-tensor env (long-array s type-id) [1 s])]
      (run-session tts (feed {"hidden_in" hidden "type_ids" types} kv)))))

(defn- latents
  "Every acoustic latent for `ids`, as a vector of 64-float arrays."
  [{:keys [^OrtEnvironment env meta base diffusion] :as bundle} ids tensors ^longs noise report]
  (let [tw
        (long (get meta "text_window"))

        sw
        (long (get meta "speech_window"))

        base-layers
        (long (get meta "base_layers"))

        tts-layers
        (long (get meta "tts_layers"))

        windows
        (max 1 (long (Math/ceil (/ (double (count ids)) tw))))

        slots
        (holder)]

    (with-open [cfg (float-tensor env (float-array [cfg-scale]) [1])]
      (try
        (loop [w 0
               base-kv (voice-kv tensors "base" base-layers)
               tts-kv (voice-kv tensors "tts" tts-layers)
               neg-kv (voice-kv tensors "neg_tts" tts-layers)
               pos nil
               neg (get tensors "neg_tts.hidden")
               out []]

          (let [win (subvec ids (min (count ids) (* w tw)) (min (count ids) (* (inc w) tw)))
                [base-kv tts-kv pos]
                (if (seq win)
                  (let [r (hold! slots
                                 0
                                 (with-open [input
                                             (long-tensor env (long-array win) [1 (count win)])]
                                   (run-session base (feed {"input_ids" input} base-kv))))
                        t (hold! slots 1 (tts-step bundle (output r "hidden") 1 tts-kv))]

                    [(presents r base-layers) (presents t tts-layers) (output t "hidden")])
                  [base-kv tts-kv pos])]

            (report {:phase :synthesizing :progress (min 99 (long (/ (* 100 (inc w)) windows)))})
            (let [[done? out tts-kv neg-kv pos neg]
                  (loop [k 0
                         out out
                         tts-kv tts-kv
                         neg-kv neg-kv
                         pos pos
                         neg neg]

                    (if (= k sw)
                      [false out tts-kv neg-kv pos neg]
                      (let [[eos out tts-kv neg-kv pos neg]
                            (with-open [eps (float-tensor env (gaussian noise 64) [1 64])
                                        d (run-session diffusion
                                                       (feed {"positive" pos
                                                              "negative" neg
                                                              "noise" eps
                                                              "cfg_scale" cfg}
                                                             []))]

                              (let [embed (output d "embed")
                                    t (hold! slots 1 (tts-step bundle embed 0 tts-kv))
                                    m (hold! slots 2 (tts-step bundle embed 0 neg-kv))]

                                [(aget (floats-of (output t "eos")) 0)
                                 (conj out (floats-of (output d "latent"))) (presents t tts-layers)
                                 (presents m tts-layers) (output t "hidden") (output m "hidden")]))]
                        (if (or (> (double eos) 0.5)
                                (>= (aget (tensor-shape (first tts-kv)) 2) max-positions))
                          [true out tts-kv neg-kv pos neg]
                          (recur (inc k) out tts-kv neg-kv pos neg)))))]
              (if done? out (recur (inc w) base-kv tts-kv neg-kv pos neg out)))))
        (finally (close-all! slots))))))

(defn- decode-audio
  ^floats [{:keys [^OrtEnvironment env decoder]} frames]
  (let [n
        (count frames)

        flat
        (float-array (* 64 n))]

    (dotimes [i n]
      (System/arraycopy ^floats (nth frames i) 0 flat (* 64 i) 64))
    (with-open [input
                (float-tensor env flat [1 n 64])

                r
                (run-session decoder (feed {"latents" input} []))]

      (floats-of (output r "audio")))))

(defn speak
  "Speak `text` in `voice-id` with the bundle in `dir`. Returns the mono samples
   and their rate."
  ([dir text voice-id] (speak dir text voice-id nil))
  ([dir text voice-id
    {:keys [seed report]
     :or {seed default-seed
          report (fn [_])}}]
   (let [bundle
         (loaded :tts (io/file dir) open-tts)

         ids
         (token-ids (:tokenizer bundle) (str (str/trim (str text)) "\n"))

         frames
         (latents bundle ids (voice-tensors bundle voice-id) (noise-state seed) report)]

     {:samples (decode-audio bundle frames)
      :sample-rate (long (get (:meta bundle) "sample_rate"))})))

(defn synthesize!
  "Speak `text` and return the WAV that was written, plus the facts a player
   needs. The model is installed first when it is missing, reported as `:preparing`."
  [{:keys [text voice-id on-progress seed]}]
  (let [report
        (fn [m]
          (when on-progress (try (on-progress m) (catch Throwable _ nil))))

        spoken
        (str/trim (str text))

        voice
        (or (some-> voice-id
                    name
                    not-empty)
            (some-> (first (voices))
                    :id
                    name))]

    (when (str/blank? spoken)
      (throw (ex-info "Nothing to speak" {:type :speech-tts/blank-text :family :vibevoice})))
    (when-not (some #(= voice (name (:id %))) (voices))
      (throw (ex-info (str "No VibeVoice voice named " (pr-str voice-id))
                      {:type :speech-tts/unknown-voice
                       :family :vibevoice
                       :voice-id voice-id
                       :known (mapv #(name (:id %)) (voices))})))
    (let [dir
          (assets/ensure! (tts-asset) #(report (assoc % :phase :preparing)))

          _
          (report {:phase :synthesizing :progress 0})

          {:keys [^floats samples sample-rate]}
          (speak dir spoken voice {:seed (or seed default-seed) :report report})

          out
          (File/createTempFile "vis-speech-" ".wav")]

      (write-wav! out samples sample-rate)
      {:audio-path (str out)
       :media-type "audio/wav"
       :sample-rate sample-rate
       :duration-ms (long (* 1000.0 (/ (alength samples) (double sample-rate))))})))

;; Samples - one sentence in each voice, so a voice is chosen by ear

;; The pack holds the sample sentence in all 25 voices, about 6 MB against 1.2 GB
;; for the model, so a surface can play a voice before the download. A machine that
;; has the model speaks a missing sample itself and keeps it. Each voice reads the
;; sentence in its own language: a Korean voice reading English gives noise, not a
;; preview. Voices of one language read the same words, so they compare.

(def sample-texts
  "The Piper sample sentence in each language of the catalogue, by base language."
  {"de" "So klinge ich, wenn ich in normalem Tempo lese, mit den Pausen dort, wo sie hingehören."
   "es" "Así sueno yo, leyendo a un ritmo normal, con las pausas donde caen."
   "fr" "Voici ma voix, quand je lis à un rythme normal, avec les pauses là où elles tombent."
   "it" "Ecco come suono, leggendo a un ritmo normale, con le pause dove capitano."
   "ja" "これが私の声です。普通の速さで、区切りは自然なところで読んでいます。"
   "ko" "이것이 제 목소리입니다. 보통 속도로, 쉼은 자연스러운 곳에 두고 읽고 있습니다."
   "nl" "Zo klink ik, als ik in een gewoon tempo lees, met de pauzes waar ze vallen."
   "pl" "Tak brzmię, czytając w zwykłym tempie, z pauzami tam, gdzie wypadają."
   "pt" "É assim que eu soo, lendo num ritmo normal, com as pausas onde elas caem."})

(defn sample-text
  "The sentence that `voice-id` reads for its sample, in the voice's language."
  [voice-id]
  (let [language (some #(when (= (name voice-id) (name (:id %))) (:language %)) (voices))]
    (get sample-texts (first (str/split (str language) #"-")) tts/sample-text)))

(def ^:const samples-asset-id "vibevoice-voice-samples")

(defn samples-asset [] (assets/entry samples-asset-id))

(defn- sample-file-name ^String [voice-id] (str (name voice-id) ".wav"))

(defn- known-voice?
  "Whether `voice-id` is in the bundle catalogue. A name from a URL that is not in
   the catalogue never becomes a path."
  [voice-id]
  (boolean (some #(= (name voice-id) (name (:id %))) (voices))))

(defn- is-packed-voice
  [voice-id]
  (boolean (some #{(sample-file-name voice-id)} (:requires (samples-asset)))))

(defn- spoken-sample
  "Where a sample that this machine spoke lives: beside the pack, never inside it."
  ^File [voice-id]
  (io/file (assets/models-root) "vibevoice-samples-spoken" (sample-file-name voice-id)))

(defn- sample-on-disk
  [voice-id]
  (let [^File f (first (filter #(.isFile ^File %)
                               [(io/file (assets/install-dir (samples-asset))
                                         (sample-file-name voice-id)) (spoken-sample voice-id)]))]
    (when f {:audio-path (str f) :media-type "audio/wav"})))

(defn level
  "Scale `samples` to a peak of 0.7 (-3 dBFS), as the Piper samples are, so no
   voice plays louder than the one before it. Silence stays silence."
  ^floats [^floats samples]
  (let [n
        (alength samples)

        peak
        (loop [i
               0

               peak
               0.0]

          (if (< i n) (recur (inc i) (max peak (Math/abs (double (aget samples i))))) peak))

        out
        (float-array n)]

    (if (< peak 1.0e-6)
      (aclone samples)
      (let [gain (/ 0.7 peak)]
        (dotimes [i n]
          (aset out i (float (* gain (double (aget samples i))))))
        out))))

(defn speak-sample!
  "Speak the shared sample sentence in `voice-id` into `out` with the installed
   model, levelled. The file appears whole or not at all.
   `bin/make-vibevoice-samples` builds the pack with it."
  [voice-id ^File out]
  (let [{:keys [^floats samples sample-rate]}
        (speak (assets/install-dir (tts-asset)) (sample-text voice-id) (name voice-id))

        part
        (io/file (str out ".part"))]

    (io/make-parents out)
    (write-wav! part (level samples) (long sample-rate))
    (when-not (.renameTo part out) (io/copy part out) (.delete part))
    out))

(defn voice-sample
  "The sample of `voice-id`, without making one: `{:audio-path …}` on disk,
   `{:is-preparable true}` when the pack or the installed model can make it, or nil."
  [voice-id]
  (when (known-voice? voice-id)
    (or (sample-on-disk voice-id)
        (when (or (is-packed-voice voice-id) (assets/installed? (tts-asset)))
          {:is-preparable true}))))

(defn prepare-voice-sample!
  "Make the sample that [[voice-sample]] called preparable and return it. Installs
   the small pack, or speaks the sentence with the installed model and keeps the
   file. Never downloads the model."
  [voice-id]
  (when-not (known-voice? voice-id)
    (throw (ex-info (str "No VibeVoice voice named " (pr-str voice-id))
                    {:type :speech-tts/unknown-voice :family :vibevoice :voice-id voice-id})))
  (or (sample-on-disk voice-id)
      (when (is-packed-voice voice-id) (assets/install! (samples-asset)) (sample-on-disk voice-id))
      (when (assets/installed? (tts-asset))
        (speak-sample! voice-id (spoken-sample voice-id))
        (sample-on-disk voice-id))
      (throw (ex-info
               (str "No sample for " (name voice-id) " without the VibeVoice model.")
               {:type :speech-tts/sample-unavailable :family :vibevoice :voice-id voice-id}))))

;; Transcription

(defn asr-asset [] (assets/entry asr-asset-id))

(defn- open-asr
  [^OrtEnvironment env ^File dir]
  (let [f #(io/file dir %)]
    {:env env
     :meta (read-json (f "vibevoice_asr.json"))
     :tokenizer (open-tokenizer (f "tokenizer.json"))
     :encoder (open-session env (f "speech_encoder.onnx"))
     :decoder (open-session env (f "decoder.onnx"))}))

(defn- asr-step
  ^OrtSession$Result [{:keys [^OrtEnvironment env decoder]} ids ^OnnxTensor speech kv]
  (with-open [input (long-tensor env (long-array ids) [1 (count ids)])]
    (run-session decoder (feed {"input_ids" input "speech" speech} kv))))

(defn- token-of ^long [^OrtSession$Result r] (.get (.getLongBuffer (output r "token")) 0))

(defn- chunk-text
  [{:keys [meta ^HuggingFaceTokenizer tokenizer]} tokens]
  (reduce #(str/replace %1 (str %2) "")
          (.decode tokenizer (long-array tokens) true)
          (get meta "strip_tokens")))

(defn transcribe-samples
  "The text of each audio chunk of 24 kHz `samples`, as `{:start :end :text}` in
   seconds, with the bundle in `dir`."
  ([dir samples] (transcribe-samples dir samples nil))
  ([dir ^floats samples
    {:keys [seed report context]
     :or {seed default-seed
          report (fn [_])}}]
   (let [{:keys [^OrtEnvironment env meta encoder tokenizer] :as bundle}
         (loaded :asr (io/file dir) open-asr)

         ids
         (get meta "ids")

         id
         #(long (get ids %))

         layers
         (long (get meta "layers"))

         heads
         (long (get meta "kv_heads"))

         head-dim
         (long (get meta "head_dim"))

         hidden
         (long (get meta "hidden"))

         frame
         (long (get meta "samples_per_frame"))

         frames
         (+ (long (get meta "chunk_frames")) (long (get meta "lookahead_frames")))

         chunk
         (* frame (long (get meta "chunk_frames")))

         window
         (* frame frames)

         rate
         (double (get meta "sample_rate"))

         std
         (double (get meta "acoustic_noise_std"))

         noise
         (noise-state seed)

         n
         (alength samples)

         chunks
         (max 1 (long (Math/ceil (/ (double n) chunk))))

         prompt
         (str/replace
           (str (get meta "prompt"))
           "{extra}"
           (if context (str/replace (str (get meta "prompt_extra")) "{context}" (str context)) ""))

         stop
         #{(id "text_chunk_end") (id "eos")}

         slots
         (holder)

         empty-kv
         (vec (repeatedly (* 2 layers) #(float-tensor env (float-array 0) [1 heads 0 head-dim])))]

     (with-open [no-speech (float-tensor env (float-array hidden) [1 1 hidden])]
       (try
         (loop [kv
                (presents
                  (hold! slots 0 (asr-step bundle (token-ids tokenizer prompt) no-speech empty-kv))
                  layers)
                start 0
                out []]

           (if (>= start n)
             out
             (let [seg (float-array window)
                   _ (System/arraycopy samples start seg 0 (min window (- n start)))
                   scale (* std (double (aget ^floats (gaussian noise 1) 0)))
                   eps (gaussian noise (* frames 64))
                   r (with-open [a (float-tensor env seg [1 window])
                                 e (float-tensor env eps [1 frames 64])
                                 s (float-tensor env (float-array [scale]) [1])
                                 enc (run-session encoder
                                                  (feed {"audio" a "noise" e "noise_scale" s} []))]

                       (hold! slots
                              0
                              (asr-step bundle
                                        (concat [(id "speech_start")]
                                                (repeat frames (id "speech_pad"))
                                                [(id "speech_end")])
                                        (output enc "features")
                                        kv)))
                   [tokens kv]
                   (loop [r r
                          tokens []]

                     (let [tok (token-of r)]
                       (if (or (stop tok) (>= (count tokens) max-chunk-tokens))
                         [tokens (presents r layers)]
                         (recur
                           (hold! slots 0 (asr-step bundle [tok] no-speech (presents r layers)))
                           (conj tokens tok)))))
                   kv (presents
                        (hold! slots 0 (asr-step bundle [(id "text_chunk_end")] no-speech kv))
                        layers)
                   done (inc (quot start chunk))]

               (report {:phase :transcribing :progress (min 99 (long (/ (* 100 done) chunks)))})
               (recur kv
                      (+ start chunk)
                      (conj out
                            {:start (/ start rate)
                             :end (/ (min n (+ start chunk)) rate)
                             :text (chunk-text bundle tokens)})))))
         (finally (close-all! slots)
                  (doseq [^OnnxTensor t empty-kv]
                    (.close t))))))))

(def ^:private markup
  "What the model writes besides speech: speaker labels and bracketed tags such as
   `[Silence]`."
  #"(?i)speaker\s*\d+\s*:|\[[^\]]*\]")

(defn chunk-words
  "Timed words from chunk texts. A chunk may end inside a word, so the chunks join
   first; each character keeps a time inside its chunk, and a word spans the times
   of its first and last character."
  [chunks]
  (let [sb
        (StringBuilder.)

        times
        (java.util.ArrayList.)]

    (doseq [{:keys [start end text]}
            chunks

            :let [t
                  (str text)

                  n
                  (count t)]
            k
            (range n)]

      (.append sb (.charAt t (int k)))
      (.add times
            (+ (double start)
               (* (- (double end) (double start)) (/ (+ (double k) 0.5) (double n))))))
    (let [masked
          (str/replace (str sb) markup #(apply str (repeat (count %) " ")))

          m
          (re-matcher #"\S+" masked)]

      (loop [words []]
        (if (.find m)
          (recur (conj words
                       {:text (.group m)
                        :start (double (.get times (.start m)))
                        :end (double (.get times (dec (.end m))))}))
          words)))))

(defn transcribe
  "Transcribe one recording: the words, and the timed lines they fall into. The
   times come from the chunk that heard each word, so a line is accurate to about
   three seconds."
  [{:keys [audio-path on-progress]}]
  (let [report
        (fn [m]
          (when on-progress (try (on-progress m) (catch Throwable _ nil))))

        dir
        (assets/ensure! (asr-asset) #(report (assoc % :phase :preparing)))]

    (transcode/with-wav
      (io/file (str audio-path))
      (fn [^File wav]
        (asr/validate-wav-file! (str wav))
        (let [{:keys [samples sample-rate]}
              (read-wav wav)

              chunks
              (transcribe-samples dir (resample samples (long sample-rate) 24000) {:report report})

              cleaned
              (engine/clean-words (chunk-words chunks))]

          {:text (engine/words->text cleaned) :segments (engine/words->segments cleaned)})))))

;; Engines

(defn- model-state [asset] (tts/model-state-of [asset]))

(defn- start-download! [asset] (tts/start-downloads! [asset]))

(defn warm
  "Load the transcription model when it is installed. True when it is loaded now."
  []
  (let [asset (asr-asset)]
    (boolean (when (assets/installed? asset)
               (loaded :asr (io/file (assets/install-dir asset)) open-asr)))))

(def tts-descriptor
  {:id :vibevoice-local
   :label "VibeVoice (local)"
   :synthesize synthesize!
   :voices voices
   :model-state #(model-state (tts-asset))
   :start-download #(start-download! (tts-asset))
   :voice-sample voice-sample
   :prepare-voice-sample prepare-voice-sample!})

(def asr-descriptor
  {:id :vibevoice-asr-local
   :label "VibeVoice ASR (local)"
   :transcribe transcribe
   :model-state #(model-state (asr-asset))
   :start-download #(start-download! (asr-asset))
   :warm warm})
