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

   A voice imported from a recording uses the Base model, the `cpu_int4` export of
   onnx-community. It has its own codec_embed and residual_embed, and these graphs:

     speaker_encoder  24 kHz audio to the speaker vector of the codec prompt
     tok_encoder      one second of 24 kHz audio to 13 frames of codes

   `bin/export-qwen-base` makes its other graphs. Its text_embed and talker_cache
   are the onnx-community graphs with int8 activations, which are several times faster
   on a CPU. Its code predictor has a KV cache, so each codebook is one step of one row:

     code_predictor_step   rows and a KV cache to the logits of one codebook
     code_predictor_embed  a code of one codebook to its input row

   It uses the tokenizer and the decoder of the speaker bundle.

   The prompt and the sampling follow the reference generation of Qwen3-TTS
   (`Qwen3TTSModel.generate` in mlx-audio): the whole text goes into the prefill,
   and each later step feeds the embedded frame plus the pad embedding. The KV
   caches stay in native memory, as in [[com.blockether.vis.internal.speech.vibevoice]]."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.vibevoice :as vv]
            [com.blockether.vis.internal.speech.voices :as voices])
  (:import [ai.onnxruntime OnnxTensor OrtEnvironment OrtSession OrtSession$Result]
           [java.io File]
           [java.util Arrays HashMap]))

(set! *warn-on-reflection* true)

(def ^:const asset-id "qwen3-tts-12hz-1.7b-customvoice-int4")

(def ^:const clone-asset-id
  "The Base model, which speaks in an imported voice."
  "qwen3-tts-12hz-1.7b-base-int4")

(def ^:const export-asset-id
  "The Vis export of the Base model: its code predictor, talker and text_embed."
  "qwen3-tts-12hz-1.7b-base-vis-export")

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

(defn frame-limit
  "The most frames for the text `piece`. One character takes about one frame, and a
   Chinese character about three. A clone that never ends its speech stops here,
   not at [[max-frames]]."
  ^long [piece]
  (min (long max-frames) (+ 75 (* 6 (count (str piece))))))

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

(def ^:const encoder-window "Samples that one tok_encoder call reads: one second." 24000)

(def ^:const encoder-stride
  "Frames kept from each encoder window. A second is 12.5 frames, so a stride of 12
   frames keeps every window on the 80 ms grid."
  12)

(def ^:const default-seed
  "Noise seed of a request without one, so the same text in the same voice gives
   the same audio."
  7)

;; Catalogue

(defn asset [] (assets/entry asset-id))

(defn clone-asset [] (assets/entry clone-asset-id))

(defn export-asset [] (assets/entry export-asset-id))

(defn- imported-voice
  "The imported voice `voice-id`, or nil. An import with the name of a built-in
   speaker wins, as in pocket-tts."
  [voice-id]
  (some-> voice-id
          name
          not-empty
          voices/imported-voice))

(defn voices
  "The speakers of the bundle, from the manifest, so a surface can list them before
   the download. Then the voices imported from a recording, which the Base model
   speaks."
  []
  (let [mine
        (mapv #(select-keys % [:id :label :language :clip-text :is-imported]) (voices/imported))

        taken
        (set (map :id mine))]

    (into (vec (for [v
                     (:voices (asset))

                     :when (not (taken (name (:id v))))]

                 (select-keys v [:id :label :language :gender])))
          mine)))

(defn- known-voice? [voice-id] (boolean (some #(= (name voice-id) (name (:id %))) (voices))))

(defn- voice-assets
  "The models that `voice-id` needs: the speaker bundle, and the Base model and its
   Vis export too for an imported voice."
  [voice-id]
  (if (imported-voice voice-id) [(asset) (clone-asset) (export-asset)] [(asset)]))

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

(defn- input-names [^OrtSession session] (vec (.getInputNames session)))

(defn- nth-output ^OnnxTensor [^OrtSession$Result r ^long i] (.get r (int i)))

(defn- open-bundle
  "The graphs in `dir`. The two exports name their tensors differently, so a graph
   with one input and one output is fed by position. A Base bundle takes the
   tokenizer and the decoder from the speaker bundle in `shared`, and the code
   predictor, the talker and text_embed from `exported`."
  ([env dir] (open-bundle env dir dir nil))
  ([^OrtEnvironment env ^File dir ^File shared exported]
   (let [f
         #(io/file dir %)

         g
         #(io/file (or exported dir) %)

         talker
         (vv/open-session env (g "talker_cache.onnx"))

         base
         {:env env
          :config (vv/read-json (f "config.json"))
          :tokenizer (vv/open-tokenizer (io/file shared "tokenizer.json"))
          :text-embed (vv/open-session env (g "text_embed.onnx"))
          :codec-embed (vv/open-session env (f "codec_embed.onnx"))
          :talker talker
          :past-names (subvec (input-names talker) 3)
          :residual-embed (vv/open-session env (f "residual_embed.onnx"))
          :decoder (vv/open-session env (io/file shared "tok_decoder.onnx"))}]

     (if exported
       (let [step (vv/open-session env (g "code_predictor_step.onnx"))]
         (assoc base
           :group-step step
           :group-past-names (subvec (input-names step) 2)
           :group-embed (vv/open-session env (g "code_predictor_embed.onnx"))
           :speaker-encoder (vv/open-session env (f "speaker_encoder.onnx"))
           :audio-encoder (vv/open-session env (f "tok_encoder.onnx"))))
       (assoc base :code-predictor (vv/open-session env (f "code_predictor_loop.onnx")))))))

(defn- run-one
  "Run a graph with one input and return its first output as floats."
  ^floats [^OrtSession session ^OnnxTensor input]
  (with-open [r (vv/run-session session
                                (doto (HashMap.) (.put (first (input-names session)) input)))]
    (vv/floats-of (nth-output r 0))))

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
  (with-open [input (vv/long-tensor env (long-array ids) [1 (count ids)])]
    (run-one session input)))

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
  "The talker input of the prefill for the text token `ids` and the flat `codec`
   prompt rows: the role, the codec prompt with the speaker, the whole text, then
   the codec start. A clone `ref` adds the transcript of the recording before the
   text and its frames after the codec start, as the in-context prompt of the
   reference does. Returns the flat embeddings and the pad embedding that each
   later step adds."
  ([bundle ids codec] (prefill-embeddings bundle ids codec nil))
  ([{:keys [config text-embed] :as bundle} ids ^floats codec {:keys [text-ids ^floats steps]}]
   (let [h
         (long hidden-size)

         text
         (embed bundle text-embed ids)

         special
         (embed bundle
                text-embed
                [(get config "tts_bos_token_id") (get config "tts_eos_token_id")
                 (get config "tts_pad_token_id")])

         t
         (count ids)

         n
         (quot (alength codec) h)

         r
         (count text-ids)

         ref-text
         (when (pos? r) (embed bundle text-embed text-ids))

         m
         (if steps (quot (alength steps) h) 0)

         start
         (+ 3 (dec n))

         end
         (+ start r (- t 8))

         rows
         (+ end 2 m)

         out
         (float-array (* rows h))]

     (System/arraycopy text 0 out 0 (* 3 h))
     ;; The codec prompt: pad text rows, then the text start, over all codec rows but the last.
     (dotimes [j (dec n)]
       (add-row! out (+ 3 j) special (if (< j (- n 2)) 2 0) codec j))
     ;; The transcript of the recording and the text without its role and closing
     ;; tokens, then the text end, over the codec pad.
     (dotimes [i r]
       (add-row! out (+ start i) ref-text i codec (- n 2)))
     (dotimes [i (- t 8)]
       (add-row! out (+ start r i) text (+ 3 i) codec (- n 2)))
     (add-row! out end special 1 codec (- n 2))
     (add-row! out (inc end) special 2 codec (dec n))
     ;; The frames of the recording over the text pad.
     (dotimes [k m]
       (add-row! out (+ end 2 k) steps k special 2))
     {:embeds out :rows rows :pad (Arrays/copyOfRange special (* 2 h) (* 3 h))})))

(defn- talker-step
  ^OrtSession$Result [{:keys [^OrtEnvironment env talker past-names]} ^floats embeds rows past kv]
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
        (doseq [[^String past-name tensor] (map vector past-names kv)]
          (.put m past-name tensor))
        (vv/run-session talker m)))))

(defn- draw
  "One token from the scores `v`: the [[top-k]] best, scaled by [[temperature]], then
   one draw from `noise`."
  ^long [^doubles v ^longs noise]
  (let [n
        (alength v)

        sorted
        (doto (aclone v) Arrays/sort)

        threshold
        (aget sorted (- n (long top-k)))

        top
        (aget sorted (dec n))

        weights
        (double-array n)]

    (dotimes [i n]
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

        (if (= i n)
          last-kept
          (let [w
                (aget weights i)

                acc
                (+ acc w)

                kept
                (if (pos? w) i last-kept)]

            (if (and (pos? w) (>= acc target)) i (recur (inc i) acc kept))))))))

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
    (draw v noise)))

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

(defn- group-frame-codes
  "All 16 codes of a frame with the Base model. The code predictor reads the talker
   hidden state and the first code, then one code at a time with its KV cache, and
   gives the logits of the next codebook. Each codebook is one draw from `noise`."
  ^longs
  [{:keys [^OrtEnvironment env codec-embed ^OrtSession group-step group-embed group-past-names]}
   ^floats hidden ^long code0 ^longs noise]
  (let [width
        (inc (long residual-groups))

        h
        (long hidden-size)

        codes
        (long-array width)

        first-rows
        (float-array (* 2 h))

        empty-kv
        (vec (repeatedly (count group-past-names)
                         #(vv/float-tensor env (float-array 0) [1 kv-heads 0 head-dim])))

        slots
        (vv/holder)]

    (aset codes 0 code0)
    (System/arraycopy hidden 0 first-rows 0 h)
    (with-open [c (vv/long-tensor env (long-array [code0]) [1 1])]
      (System/arraycopy (run-one codec-embed c) 0 first-rows h h))
    (try
      (loop [j
             1

             ^floats rows
             first-rows

             kv
             empty-kv]

        (let [n
              (quot (alength rows) h)

              r
              (with-open [x
                          (vv/float-tensor env rows [1 n h])

                          head
                          (vv/long-tensor env (long-array [(dec j)]) [1])]

                (let [m (doto (HashMap.) (.put "inputs_embeds" x) (.put "head" head))]
                  (doseq [[^String past-name tensor] (map vector group-past-names kv)]
                    (.put m past-name tensor))
                  (vv/hold! slots 0 (vv/run-session group-step m))))

              logits
              (vv/floats-of (nth-output r 0))

              v
              (double-array (alength logits))]

          (dotimes [i (alength logits)]
            (aset v i (double (aget logits i))))
          (aset codes j (draw v noise))
          (if (= j (long residual-groups))
            codes
            (recur (inc j)
                   (with-open [c
                               (vv/long-tensor env (long-array [(aget codes j)]) [1])

                               g
                               (vv/long-tensor env (long-array [(dec j)]) [1])]

                     (with-open [e (vv/run-session
                                     group-embed
                                     (doto (HashMap.) (.put "codes" c) (.put "group" g)))]
                       (vv/floats-of (nth-output e 0))))
                   (mapv #(nth-output r %) (range 1 (inc (count group-past-names))))))))
      (finally (vv/close-all! slots)
               (doseq [^OnnxTensor t empty-kv]
                 (.close t))))))

(defn- next-input
  ^floats [{:keys [^OrtEnvironment env residual-embed]} ^longs codes ^floats pad]
  (with-open [c (vv/long-tensor env codes [1 (inc residual-groups)])]
    (let [e (run-one residual-embed c)]
      (dotimes [i hidden-size]
        (aset e i (+ (aget e i) (aget pad i))))
      e)))

(defn generate-frames
  "Every frame of one text chunk as 16-code arrays, until the end-of-speech token
   or `limit` frames, [[max-frames]] by default. `report` gets the frame count now
   and then."
  [{:keys [^OrtEnvironment env] :as bundle} {:keys [embeds rows pad limit] :or {limit max-frames}}
   ^longs noise report]
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
              (vv/floats-of (nth-output r 0))

              hidden
              (vv/floats-of (nth-output r 1))

              code0
              (sample-first-code logits (* (dec rows) codec-vocab) history eos noise)]

          (if (or (= code0 eos) (>= (count frames) (long limit)))
            frames
            (let [codes ((if (:group-step bundle) group-frame-codes frame-codes)
                          bundle
                          (Arrays/copyOfRange hidden
                                              (int (* (dec rows) hidden-size))
                                              (int (* rows hidden-size)))
                          code0
                          noise)]
              (when (zero? (rem (count frames) 25)) (report (count frames)))
              (recur (next-input bundle codes pad)
                     1
                     (+ past rows)
                     (mapv #(nth-output r %) (range 2 (+ 2 (* 2 (long talker-layers)))))
                     (conj history code0)
                     (conj frames codes))))))
      (finally (vv/close-all! slots)
               (doseq [^OnnxTensor t empty-kv]
                 (.close t))))))

(defn- decode-all
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

(defn decode-frames
  "The 24 kHz samples of `frames`. The last [[decode-context]] frames of `context`,
   the frames of a recording, are decoded before them and then dropped, so the
   speech continues the recording as in the reference."
  (^floats [bundle frames] (decode-frames bundle frames nil))
  (^floats [bundle frames context]
   (let [before
         (vec (take-last (long decode-context) context))

         ^floats audio
         (decode-all bundle (into before frames))]

     (if (seq before)
       (Arrays/copyOfRange audio (int (* (count before) (long frame-samples))) (alength audio))
       audio))))

;; A recording as a prompt

(defn encode-reference
  "The frames of 24 kHz `samples`, one for each started 80 ms. The encoder reads one
   second at a time, and [[encoder-stride]] frames of each window are kept."
  [{:keys [^OrtEnvironment env audio-encoder]} ^floats samples]
  (let [n
        (alength samples)

        total
        (long (Math/ceil (/ (double n) (double frame-samples))))

        width
        (inc (long residual-groups))

        stride
        (long encoder-stride)]

    (loop [w
           0

           frames
           (transient [])]

      (let [first-frame (* w stride)]
        (if (>= first-frame total)
          (persistent! frames)
          (let [start (* first-frame (long frame-samples))
                window (float-array encoder-window)
                _ (System/arraycopy samples start window 0 (min (long encoder-window) (- n start)))
                codes (with-open [x (vv/float-tensor env window [1 1 encoder-window])
                                  r (vv/run-session audio-encoder
                                                    (doto (HashMap.) (.put "audio" x)))]

                        (longs-of (nth-output r 0)))
                keep (min stride (- total first-frame))]

            (recur (inc w)
                   (reduce (fn [acc k]
                             (conj! acc
                                    (Arrays/copyOfRange ^longs codes
                                                        (int (* (long k) width))
                                                        (int (* (inc (long k)) width)))))
                           frames
                           (range keep)))))))))

(defn- speaker-vector
  "The speaker vector of 24 kHz `samples`."
  ^floats [{:keys [^OrtEnvironment env speaker-encoder]} ^floats samples]
  (with-open [x (vv/float-tensor env samples [1 (alength samples)])]
    (run-one speaker-encoder x)))

(defn reference
  "The prompt of the imported `voice` for the Base `bundle`: the speaker vector of
   its recording, and with a transcript also the text ids, frames and frame
   embeddings for the in-context prompt."
  [{:keys [^OrtEnvironment env tokenizer residual-embed] :as bundle} {:keys [clip clip-text]}]
  (let [{rate :sample-rate recorded :samples}
        (vv/read-wav (io/file clip))

        samples
        (vv/resample recorded (long rate) (long sample-rate))

        speaker
        (speaker-vector bundle samples)

        spoken
        (str/trim (str clip-text))]

    (if (str/blank? spoken)
      {:speaker speaker}
      (let [ids
            (vv/token-ids tokenizer (str "<|im_start|>assistant\n" spoken "<|im_end|>\n"))

            frames
            (encode-reference bundle samples)

            width
            (inc (long residual-groups))

            flat
            (long-array (* (count frames) width))]

        (doseq [[k ^longs f] (map-indexed vector frames)]
          (System/arraycopy f 0 flat (* (long k) width) width))
        {:speaker speaker
         :text-ids (subvec ids 3 (- (count ids) 2))
         :frames frames
         :steps (with-open [c (vv/long-tensor env flat [(count frames) width])]
                  (run-one residual-embed c))}))))

(defn- chunk-prompt
  "The text ids and the codec rows of one chunk. `speaker` is the speaker vector of
   an imported voice, or nil for the built-in speaker `voice`."
  [{:keys [config tokenizer codec-embed] :as bundle} voice speaker text]
  (let [talker
        (get config "talker_config")

        ids
        (vv/token-ids tokenizer
                      (str "<|im_start|>assistant\n" text "<|im_end|>\n<|im_start|>assistant\n"))

        language
        (language-id config voice text)

        tags
        (if language
          [(get talker "codec_think_id") (get talker "codec_think_bos_id") language
           (get talker "codec_think_eos_id")]
          [(get talker "codec_nothink_id") (get talker "codec_think_bos_id")
           (get talker "codec_think_eos_id")])

        codec
        (embed bundle
               codec-embed
               (mapv long
                     (concat
                       tags
                       [(if speaker (get talker "codec_pad_id") (get-in talker ["spk_id" voice]))
                        (get talker "codec_pad_id") (get talker "codec_bos_id")])))]

    ;; The speaker vector of a recording takes the row of the speaker token.
    (when speaker
      (System/arraycopy ^floats speaker
                        0
                        codec
                        (* (count tags) (long hidden-size))
                        (long hidden-size)))
    {:ids ids :codec codec}))

(defn speak
  "Speak `text` in `voice` with the speaker bundle in `dir`. An imported voice uses
   the installed Base model. Returns the mono samples and their rate."
  ([dir text voice] (speak dir text voice nil))
  ([dir text voice
    {:keys [seed report]
     :or {seed default-seed
          report (fn [_])}}]
   (let [imported
         (imported-voice voice)

         bundle
         (if imported
           (vv/loaded
             :qwen-tts-clone
             (io/file (assets/install-dir (clone-asset)))
             #(open-bundle %1 %2 (io/file dir) (io/file (assets/install-dir (export-asset)))))
           (vv/loaded :qwen-tts (io/file dir) open-bundle))

         ref
         (when imported (reference bundle imported))

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
         (vec (map-indexed
                (fn [i piece]
                  (let [{:keys [ids codec]} (chunk-prompt bundle voice (:speaker ref) piece)]
                    (decode-frames bundle
                                   (generate-frames bundle
                                                    (assoc (prefill-embeddings bundle ids codec ref)
                                                      :limit (frame-limit piece))
                                                    noise
                                                    #(progress i piece %))
                                   (:frames ref))))
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
          (when (imported-voice voice)
            (doseq [entry [(clone-asset) (export-asset)]]
              (assets/ensure! entry #(report (assoc % :phase :preparing)))))

          _
          (report {:phase :synthesizing :progress 0})

          {:keys [^floats samples sample-rate]}
          (speak dir spoken voice {:seed (or seed default-seed) :report report})

          out
          (File/createTempFile "vis-speech-" ".wav" (paths/temp-dir))]

      (vv/write-wav! out samples sample-rate)
      {:audio-path (str out)
       :media-type "audio/wav"
       :sample-rate sample-rate
       :duration-ms (long (* 1000.0 (/ (alength samples) (double sample-rate))))})))

;; Samples - one English sentence in each voice, spoken on this machine

(defn- spoken-sample
  "Where this machine keeps the sample of `voice-id`. The name of an imported voice
   carries a digest of its recording and transcript, so an import again under the
   same name gets a new sample."
  ^File [voice-id]
  (let [stem (if-let [{:keys [clip clip-text]} (imported-voice voice-id)]
               (let [f (io/file clip)]
                 (str "imported-" (name voice-id)
                      "-" (Integer/toHexString (hash [(.length f) (.lastModified f) clip-text]))))
               (name voice-id))]
    (io/file (assets/models-root) "qwen3-tts-samples-spoken" (str stem ".wav"))))

(defn- installed-for? [voice-id] (every? assets/installed? (voice-assets voice-id)))

(defn- sample-on-disk
  [voice-id]
  (let [f (spoken-sample voice-id)]
    (when (.isFile f) {:audio-path (str f) :media-type "audio/wav"})))

(defn voice-sample
  "The sample of `voice-id`, without making one: `{:audio-path …}` on disk,
   `{:is-preparable true}` when the installed model can speak it, or nil."
  [voice-id]
  (when (known-voice? voice-id)
    (or (sample-on-disk voice-id) (when (installed-for? voice-id) {:is-preparable true}))))

(defn prepare-voice-sample!
  "Speak the shared sample sentence in `voice-id` with the installed model and keep
   the file. Never downloads the model."
  [voice-id]
  (when-not (known-voice? voice-id)
    (throw (ex-info (str "No Qwen3-TTS voice named " (pr-str voice-id))
                    {:type :speech-tts/unknown-voice :family :qwen :voice-id voice-id})))
  (or (sample-on-disk voice-id)
      (when (installed-for? voice-id)
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
   :voice-model-state #(tts/model-state-of (voice-assets %))
   :start-voice-download (fn [{:keys [voice-id]}]
                           (tts/start-downloads! (voice-assets voice-id)))
   :import-voice voices/import!
   :update-voice voices/update!
   :forget-voice voices/forget!
   :voice-sample voice-sample
   :prepare-voice-sample prepare-voice-sample!})
