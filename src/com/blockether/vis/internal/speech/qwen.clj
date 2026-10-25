(ns com.blockether.vis.internal.speech.qwen
  "Qwen3-TTS-12Hz-1.7B-CustomVoice text to speech through ONNX Runtime.

     :qwen3-tts-local  Nine built-in speakers. Each speaker can read ten languages,
                       and the language of the text selects the language prompt.

   The bundle is the `cpu_int4` ONNX export of speed-brain-ai. Its graphs are:

     text_embed           text token ids to projected embeddings
     codec_embed          codec token ids to talker embeddings
     talker_cache         the talker with a KV cache: the first codebook of a frame
     code_predictor_loop  the other 15 codebooks of a frame, sampled in the graph
     residual_embed       the 16 codes of a frame to the next talker input
     tok_decoder          frames to 24 kHz audio, 1920 samples for each frame

   The prompt and the sampling follow the reference generation of Qwen3-TTS
   (`Qwen3TTSModel.generate` in mlx-audio): the whole text goes into the prefill,
   and each later step feeds the embedded frame plus the pad embedding. The KV
   caches stay in native memory, as in [[com.blockether.vis.internal.speech.vibevoice]]."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.vibevoice :as vv])
  (:import [ai.onnxruntime OnnxTensor OrtEnvironment OrtSession OrtSession$Result]
           [java.io File]
           [java.util Arrays HashMap]))

(set! *warn-on-reflection* true)

(def ^:const asset-id "qwen3-tts-12hz-1.7b-customvoice-int4")

(def ^:const hidden-size 2048)

(def ^:const talker-layers 28)

(def ^:const kv-heads 8)

(def ^:const head-dim 128)

(def ^:const codec-vocab
  "Codec tokens of the talker head, with the control tokens above 2047."
  3072)

(def ^:const residual-groups "Codebooks that the code predictor adds to each frame." 15)

(def ^:const residual-vocab 2048)

(def ^:const frame-samples "Audio samples of one 80 ms frame at 24 kHz." 1920)

(def ^:const sample-rate 24000)

(def ^:const max-frames
  "The most frames of one text chunk, about 160 seconds. Generation stops here even
   without an end-of-speech token."
  2048)

(def ^:const temperature "The sampling temperature of the reference generation." 0.9)

(def ^:const top-k 50)

(def ^:const repetition-penalty "Applied to the first codebook only, as the reference does." 1.05)

(def ^:const repetition-window 64)

(def ^:const decode-window "Frames that one decoder call returns as audio." 240)

(def ^:const decode-context
  "Frames before a decoder window that the decoder hears again: the attention window
   of the codec, so a window boundary does not change the audio."
  72)

(def ^:const chunk-chars
  "The longest text of one generation. A longer text is split at sentence ends, as
   the quality of one prefill drops on long text."
  300)

(def ^:const chunk-gap-seconds "Silence between two chunks of one text." 0.2)

(def ^:const default-seed
  "Noise seed of a request without one, so the same text in the same voice gives
   the same audio."
  7)

;; Catalogue

(defn asset [] (assets/entry asset-id))

(defn voices
  "The speakers of the bundle, from the manifest, so a surface can list them before
   the download."
  []
  (mapv #(select-keys % [:id :label :language :gender]) (:voices (asset))))

(defn- known-voice? [voice-id] (boolean (some #(= (name voice-id) (name (:id %))) (voices))))

;; Language

(def ^:private common-words
  "Frequent short words of each Latin-script language that Qwen3-TTS speaks."
  {"english" #{"the" "and" "is" "of" "to" "in" "that" "it" "was" "for" "with" "are" "this" "you"
               "not" "on" "they" "have" "but" "be"}
   "german" #{"der" "die" "das" "und" "ist" "nicht" "ich" "mit" "sie" "ein" "eine" "zu" "den" "auf"
              "es" "auch" "sich" "dem" "wir" "wie"}
   "french" #{"le" "la" "les" "et" "est" "une" "des" "que" "pas" "dans" "pour" "qui" "sur" "du" "au"
              "ce" "il" "elle" "nous" "avec"}
   "spanish" #{"el" "los" "las" "y" "es" "una" "del" "por" "con" "para" "se" "su" "como" "pero"
               "muy" "está" "hay" "sus" "fue" "al"}
   "italian" #{"il" "gli" "e" "è" "della" "per" "non" "con" "sono" "di" "che" "una" "nel" "anche"
               "ma" "questo" "alla" "dei" "delle" "più"}
   "portuguese" #{"o" "os" "e" "é" "uma" "do" "da" "não" "em" "para" "com" "um" "se" "mais" "como"
                  "dos" "das" "foi" "ao" "muito"}})

(defn text-language
  "The language of `text` as a Qwen3-TTS language name, or nil when the text is in
   none of its ten languages. The script decides first; for Latin script, the most
   frequent common words decide."
  [text]
  (let [s (str text)]
    (cond (re-find #"[\p{IsHiragana}\p{IsKatakana}]" s) "japanese"
          (re-find #"\p{IsHangul}" s) "korean"
          (re-find #"\p{IsHan}" s) "chinese"
          (re-find #"\p{IsCyrillic}" s) "russian"
          :else (let [words (re-seq #"\p{L}+" (str/lower-case s))
                      scores (for [[language common] common-words]
                               [language (count (filter common words))])
                      [[best top] [_ runner-up]] (sort-by (comp - second) scores)]

                  (when (and (>= (long top) 2) (> (long top) (long runner-up))) best)))))

(defn- language-id
  "The codec token of the language prompt, or nil for an automatic prompt. A Chinese
   text in a dialect speaker uses the dialect, as the reference does."
  [config voice text]
  (let [talker
        (get config "talker_config")

        language
        (text-language text)

        dialect
        (get-in talker ["spk_is_dialect" voice])]

    (if (and (= "chinese" language) (string? dialect))
      (get-in talker ["codec_language_id" dialect])
      (some->> language
               (get (get talker "codec_language_id"))))))

;; Text chunks

(defn chunks
  "`text` in pieces of at most [[chunk-chars]], split at sentence ends and line
   breaks. A sentence that is longer stays whole."
  [text]
  (let [sentences (->> (str/split (str/trim (str text)) #"(?<=[.!?…。！？])\s+|\s*\n+\s*")
                       (map str/trim)
                       (remove str/blank?))]
    (loop [remaining sentences
           current nil
           acc []]

      (if-let [s (first remaining)]
        (if (and current (> (+ (count current) 1 (count s)) (long chunk-chars)))
          (recur (rest remaining) s (conj acc current))
          (recur (rest remaining) (if current (str current " " s) s) acc))
        (cond-> acc
          current
          (conj current))))))

;; Bundle

(defn- open-bundle
  [^OrtEnvironment env ^File dir]
  (let [f #(io/file dir %)]
    {:env env
     :config (vv/read-json (f "config.json"))
     :tokenizer (vv/open-tokenizer (f "tokenizer.json"))
     :text-embed (vv/open-session env (f "text_embed.onnx"))
     :codec-embed (vv/open-session env (f "codec_embed.onnx"))
     :talker (vv/open-session env (f "talker_cache.onnx"))
     :code-predictor (vv/open-session env (f "code_predictor_loop.onnx"))
     :residual-embed (vv/open-session env (f "residual_embed.onnx"))
     :decoder (vv/open-session env (f "tok_decoder.onnx"))}))

(defn- longs-of
  ^longs [^OnnxTensor tensor]
  (let [b
        (.getLongBuffer tensor)

        a
        (long-array (.remaining b))]

    (.get b a)
    a))

(defn- embed
  "The embedding rows of `ids` from an embedding graph, as one flat array."
  ^floats [{:keys [^OrtEnvironment env]} ^OrtSession session ids]
  (with-open [input
              (vv/long-tensor env (long-array ids) [1 (count ids)])

              r
              (vv/run-session session (doto (HashMap.) (.put "input_ids" input)))]

    (vv/floats-of (vv/output r "embeds"))))

(defn- add-row!
  "Write row `i` of `a` plus row `j` of `b` into row `k` of `out`."
  [^floats out k ^floats a i ^floats b j]
  (let [h
        (long hidden-size)

        k
        (long k)

        i
        (long i)

        j
        (long j)]

    (dotimes [x h]
      (aset out (+ (* k h) x) (+ (aget a (+ (* i h) x)) (aget b (+ (* j h) x)))))))

(defn prefill-embeddings
  "The talker input of the prefill for the text token `ids`: the role, the codec
   prompt with the speaker, the whole text, then the codec start. Returns the flat
   embeddings and the pad embedding that each later step adds."
  [{:keys [config text-embed codec-embed] :as bundle} ids codec-ids]
  (let [text
        (embed bundle text-embed ids)

        special
        (embed bundle
               text-embed
               [(get config "tts_bos_token_id") (get config "tts_eos_token_id")
                (get config "tts_pad_token_id")])

        codec
        (embed bundle codec-embed codec-ids)

        t
        (count ids)

        n
        (count codec-ids)

        rows
        (+ 3 (dec n) (- t 8) 1 1)

        out
        (float-array (* rows (long hidden-size)))]

    (System/arraycopy text 0 out 0 (* 3 (long hidden-size)))
    ;; The codec prompt: pad text rows, then the text start, over all codec rows but the last.
    (dotimes [j (dec n)]
      (add-row! out (+ 3 j) special (if (< j (- n 2)) 2 0) codec j))
    ;; The text without its role and closing tokens, then the text end, over the codec pad.
    (dotimes [i (- t 8)]
      (add-row! out (+ 3 (dec n) i) text (+ 3 i) codec (- n 2)))
    (add-row! out (+ 3 (dec n) (- t 8)) special 1 codec (- n 2))
    (add-row! out (dec rows) special 2 codec (dec n))
    {:embeds out
     :rows rows
     :pad (Arrays/copyOfRange special (* 2 (long hidden-size)) (* 3 (long hidden-size)))}))

(defn- kv-feed-names
  []
  (vec (for [l
             (range talker-layers)

             kind
             ["key" "value"]]

         [(str "past_key_values." l "." kind) (str "present." l "." kind)])))

(defn- talker-step
  ^OrtSession$Result [{:keys [^OrtEnvironment env talker]} ^floats embeds rows past kv]
  (let [rows
        (long rows)

        past
        (long past)

        total
        (+ past rows)

        positions
        (long-array (* 3 rows))]

    (dotimes [i (* 3 rows)]
      (aset positions i (+ past (rem i rows))))
    (with-open [x
                (vv/float-tensor env embeds [1 rows hidden-size])

                pos
                (vv/long-tensor env positions [3 1 rows])

                mask
                (vv/long-tensor env (doto (long-array total) (Arrays/fill 1)) [1 total])]

      (let [m (doto (HashMap.)
                (.put "inputs_embeds" x)
                (.put "position_ids" pos)
                (.put "attention_mask" mask))]
        (doseq [[[past-name] tensor] (map vector (kv-feed-names) kv)]
          (.put m past-name tensor))
        (vv/run-session talker m)))))

(defn sample-first-code
  "The first codebook of the next frame from the last row of the talker `logits`
   at `offset`: control tokens except end of speech are blocked, recent tokens are
   penalised, then temperature, top-k and one draw from `noise`."
  [^floats logits offset history eos ^longs noise]
  (let [offset
        (long offset)

        eos
        (long eos)

        v
        (double-array codec-vocab)]

    (dotimes [i codec-vocab]
      (aset v
            i
            (if (and (>= i (long residual-vocab)) (not= i eos))
              Double/NEGATIVE_INFINITY
              (double (aget logits (+ offset i))))))
    (doseq [token (distinct (take-last repetition-window history))]
      (let [x (aget v (long token))]
        (aset v
              (long token)
              (if (neg? x) (* x (double repetition-penalty)) (/ x (double repetition-penalty))))))
    (let [sorted
          (doto (aclone v) Arrays/sort)

          threshold
          (aget sorted (- (long codec-vocab) (long top-k)))

          top
          (aget sorted (dec (long codec-vocab)))

          weights
          (double-array codec-vocab)]

      (dotimes [i codec-vocab]
        (let [x (aget v i)]
          (aset weights i (if (< x threshold) 0.0 (Math/exp (/ (- x top) (double temperature)))))))
      (let [total
            (areduce weights i acc 0.0 (+ acc (aget weights i)))

            target
            (* total (vv/uniform noise))]

        (loop [i
               0

               acc
               0.0

               last-kept
               -1]

          (if (= i codec-vocab)
            last-kept
            (let [w
                  (aget weights i)

                  acc
                  (+ acc w)

                  kept
                  (if (pos? w) i last-kept)]

              (if (and (pos? w) (>= acc target)) i (recur (inc i) acc kept)))))))))

(defn- gumbel
  "Gumbel noise for the in-graph sampling of the code predictor."
  ^floats [^longs noise ^long n]
  (let [out (float-array n)]
    (dotimes [i n]
      (let [u (min (- 1.0 1.0e-7) (max 1.0e-10 (vv/uniform noise)))]
        (aset out i (float (- (Math/log (- (Math/log u))))))))
    out))

(defn- frame-codes
  "All 16 codes of a frame: `code0` and the 15 codebooks that the code predictor
   samples from the talker `hidden` row."
  ^longs [{:keys [^OrtEnvironment env code-predictor]} ^floats hidden ^long code0 ^longs noise]
  (with-open [h
              (vv/float-tensor env hidden [1 hidden-size])

              c0
              (vv/long-tensor env (long-array [code0]) [1])

              nz
              (vv/float-tensor env
                               (gumbel noise (* residual-groups residual-vocab))
                               [residual-groups residual-vocab])

              temp
              (vv/float-tensor env (float-array [temperature]) [1])

              k
              (vv/long-tensor env (long-array [top-k]) [1])

              p
              (vv/float-tensor env (float-array [1.0]) [1])

              r
              (vv/run-session code-predictor
                              (doto (HashMap.)
                                (.put "talker_hidden" h)
                                (.put "code0" c0)
                                (.put "noise" nz)
                                (.put "temperature" temp)
                                (.put "top_k" k)
                                (.put "top_p" p)))]

    (longs-of (vv/output r "codes"))))

(defn- next-input
  ^floats [{:keys [^OrtEnvironment env residual-embed]} ^longs codes ^floats pad]
  (with-open [c
              (vv/long-tensor env codes [1 (inc residual-groups)])

              r
              (vv/run-session residual-embed (doto (HashMap.) (.put "codes" c)))]

    (let [e (vv/floats-of (vv/output r "embeds"))]
      (dotimes [i hidden-size]
        (aset e i (+ (aget e i) (aget pad i))))
      e)))

(defn generate-frames
  "Every frame of one text chunk as 16-code arrays, until the end-of-speech token
   or [[max-frames]]. `report` gets the frame count now and then."
  [{:keys [^OrtEnvironment env] :as bundle} {:keys [embeds rows pad]} ^longs noise report]
  (let [eos
        (long (get-in bundle [:config "talker_config" "codec_eos_token_id"]))

        empty-kv
        (vec (repeatedly (* 2 (long talker-layers))
                         #(vv/float-tensor env (float-array 0) [1 kv-heads 0 head-dim])))

        slots
        (vv/holder)]

    (try
      (loop [input
             embeds

             rows
             (long rows)

             past
             0

             kv
             empty-kv

             history
             []

             frames
             []]

        (let [r
              (vv/hold! slots 0 (talker-step bundle input rows past kv))

              logits
              (vv/floats-of (vv/output r "logits"))

              hidden
              (vv/floats-of (vv/output r "hidden"))

              code0
              (sample-first-code logits (* (dec rows) codec-vocab) history eos noise)]

          (if (or (= code0 eos) (>= (count frames) (long max-frames)))
            frames
            (let [codes (frame-codes bundle
                                     (Arrays/copyOfRange hidden
                                                         (int (* (dec rows) hidden-size))
                                                         (int (* rows hidden-size)))
                                     code0
                                     noise)]
              (when (zero? (rem (count frames) 25)) (report (count frames)))
              (recur (next-input bundle codes pad)
                     1
                     (+ past rows)
                     (mapv (fn [[_ present-name]]
                             (vv/output r present-name))
                           (kv-feed-names))
                     (conj history code0)
                     (conj frames codes))))))
      (finally (vv/close-all! slots)
               (doseq [^OnnxTensor t empty-kv]
                 (.close t))))))

(defn decode-frames
  "The 24 kHz samples of `frames`, decoded in windows of [[decode-window]] frames
   with [[decode-context]] frames of context before each window."
  ^floats [{:keys [^OrtEnvironment env decoder]} frames]
  (let [n
        (count frames)

        out
        (float-array (* n (long frame-samples)))]

    (loop [start 0]
      (when (< start n)
        (let [from (max 0 (- start (long decode-context)))
              end (min n (+ start (long decode-window)))
              width (inc (long residual-groups))
              flat (long-array (* (- end from) width))]

          (doseq [i (range from end)]
            (System/arraycopy ^longs (nth frames i) 0 flat (* (- (long i) from) width) width))
          (with-open [codes (vv/long-tensor env flat [1 (- end from) width])
                      r (vv/run-session decoder (doto (HashMap.) (.put "codes" codes)))]

            (let [audio (vv/floats-of (vv/output r "audio"))
                  skip (* (- start from) frame-samples)
                  length (min (- (alength audio) skip) (* (- end start) frame-samples))]

              (System/arraycopy audio skip out (* start frame-samples) length)))
          (recur end))))
    out))

(defn- chunk-prompt
  [{:keys [config tokenizer]} voice text]
  (let [talker
        (get config "talker_config")

        ids
        (vv/token-ids tokenizer
                      (str "<|im_start|>assistant\n" text "<|im_end|>\n<|im_start|>assistant\n"))

        language
        (language-id config voice text)

        codec-ids
        (concat (if language
                  [(get talker "codec_think_id") (get talker "codec_think_bos_id") language
                   (get talker "codec_think_eos_id")]
                  [(get talker "codec_nothink_id") (get talker "codec_think_bos_id")
                   (get talker "codec_think_eos_id")])
                [(get-in talker ["spk_id" voice]) (get talker "codec_pad_id")
                 (get talker "codec_bos_id")])]

    {:ids ids :codec-ids (mapv long codec-ids)}))

(defn speak
  "Speak `text` in `voice` with the bundle in `dir`. Returns the mono samples and
   their rate."
  ([dir text voice] (speak dir text voice nil))
  ([dir text voice
    {:keys [seed report]
     :or {seed default-seed
          report (fn [_])}}]
   (let [bundle
         (vv/loaded :qwen-tts (io/file dir) open-bundle)

         pieces
         (chunks text)

         noise
         (vv/noise-state seed)

         gap
         (float-array (long (* chunk-gap-seconds sample-rate)))

         progress
         (fn [^long i piece ^long frames]
           ;; About one frame for each character of text.
           (let [part (min 0.95 (/ (double frames) (max 20.0 (double (count piece)))))]
             (report {:phase :synthesizing
                      :progress (min 99 (long (/ (* 100.0 (+ i part)) (count pieces))))})))

         parts
         (vec (map-indexed (fn [i piece]
                             (let [{:keys [ids codec-ids]} (chunk-prompt bundle voice piece)]
                               (decode-frames bundle
                                              (generate-frames
                                                bundle
                                                (prefill-embeddings bundle ids codec-ids)
                                                noise
                                                #(progress i piece %)))))
                           pieces))

         joined
         (float-array (+ (reduce + 0 (map #(alength ^floats %) parts))
                         (* (alength gap) (max 0 (dec (count parts))))))]

     (loop [i
            0

            at
            0]

       (when (< i (count parts))
         (let [^floats part
               (nth parts i)

               at
               (if (pos? i) (+ at (alength gap)) at)]

           (System/arraycopy part 0 joined at (alength part))
           (recur (inc i) (+ at (alength part))))))
     {:samples joined :sample-rate sample-rate})))

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
      (throw (ex-info "Nothing to speak" {:type :speech-tts/blank-text :family :qwen})))
    (when-not (known-voice? voice)
      (throw (ex-info (str "No Qwen3-TTS voice named " (pr-str voice-id))
                      {:type :speech-tts/unknown-voice
                       :family :qwen
                       :voice-id voice-id
                       :known (mapv #(name (:id %)) (voices))})))
    (let [dir
          (assets/ensure! (asset) #(report (assoc % :phase :preparing)))

          _
          (report {:phase :synthesizing :progress 0})

          {:keys [^floats samples sample-rate]}
          (speak dir spoken voice {:seed (or seed default-seed) :report report})

          out
          (File/createTempFile "vis-speech-" ".wav")]

      (vv/write-wav! out samples sample-rate)
      {:audio-path (str out)
       :media-type "audio/wav"
       :sample-rate sample-rate
       :duration-ms (long (* 1000.0 (/ (alength samples) (double sample-rate))))})))

;; Samples - one English sentence in each voice, spoken on this machine

(defn- spoken-sample
  ^File [voice-id]
  (io/file (assets/models-root) "qwen3-tts-samples-spoken" (str (name voice-id) ".wav")))

(defn- sample-on-disk
  [voice-id]
  (let [f (spoken-sample voice-id)]
    (when (.isFile f) {:audio-path (str f) :media-type "audio/wav"})))

(defn voice-sample
  "The sample of `voice-id`, without making one: `{:audio-path …}` on disk,
   `{:is-preparable true}` when the installed model can speak it, or nil."
  [voice-id]
  (when (known-voice? voice-id)
    (or (sample-on-disk voice-id) (when (assets/installed? (asset)) {:is-preparable true}))))

(defn prepare-voice-sample!
  "Speak the shared sample sentence in `voice-id` with the installed model and keep
   the file. Never downloads the model."
  [voice-id]
  (when-not (known-voice? voice-id)
    (throw (ex-info (str "No Qwen3-TTS voice named " (pr-str voice-id))
                    {:type :speech-tts/unknown-voice :family :qwen :voice-id voice-id})))
  (or (sample-on-disk voice-id)
      (when (assets/installed? (asset))
        (let [{:keys [^floats samples sample-rate]}
              (speak (assets/install-dir (asset)) tts/sample-text (name voice-id))

              out
              (spoken-sample voice-id)

              part
              (io/file (str out ".part"))]

          (io/make-parents out)
          (vv/write-wav! part (vv/level samples) (long sample-rate))
          (when-not (.renameTo part out) (io/copy part out) (.delete part))
          (sample-on-disk voice-id)))
      (throw (ex-info (str "No sample for " (name voice-id) " without the Qwen3-TTS model.")
                      {:type :speech-tts/sample-unavailable :family :qwen :voice-id voice-id}))))

(def descriptor
  {:id :qwen3-tts-local
   :label "Qwen3-TTS (local)"
   :synthesize synthesize!
   :voices voices
   :model-state #(tts/model-state-of [(asset)])
   :start-download #(tts/start-downloads! [(asset)])
   :voice-sample voice-sample
   :prepare-voice-sample prepare-voice-sample!})
