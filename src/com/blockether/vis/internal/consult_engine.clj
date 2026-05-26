(ns com.blockether.vis.internal.consult-engine
  "Single-iter mini-SCI runner for consult intents.

   `run-consult!` is invoked on a side thread per intent. It builds
   a fresh SCI sandbox with `:consult` scope only (research bindings,
   future, no engine mutators), composes a consult-facing prompt
   (CONSULT_BASE_PROMPT + auto-gen BINDINGS), calls the router via
   `svar/ask-code!`, parses the returned fence, evaluates it ONCE,
   and captures the `(done {…})` emission. Same FORM as primary's
   done; the SCHEMA differs (see CONSULT_BASE_PROMPT).

   Token-cap retry-to-fit. After the first attempt resolves, the
   engine token-counts `:content` via `tokens/count-tokens` (jtokkit
   cl100k_base). When the count exceeds `consult/TOKEN_CAPS[preference]`
   the engine re-prompts the same provider ONE more time with a
   compression instruction carrying three explicit numbers: N
   (actual), M (cap), X = N - M (overage). The consult LLM sizes the
   cut from those numbers. After the retry: a fitting answer lands
   as `:status :active :retries 1`; a still-overflowing answer lands
   as `:status :failed :error :exceeds-cap
   :reason \"first N1 / cap M / second N2\" :retries 1`.

   The runner is provider-agnostic: it routes via the same `svar/ask-code!`
   pipeline that primary uses, but with `:routing {:provider … :model …}`
   resolved from the env's `:consult-config` map (or
   `consult/DEFAULT_PREFERENCE_MAP` when omitted).

   Thread isolation: the SCI ctx and the ctx snapshot are LOCAL to
   the runner. Primary's ctx-atom is NEVER mutated from this thread.
   The only allowed cross-boundary write is the atomic swap that
   appends a synthetic trailer pin after this future resolves."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.consult :as consult]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.tokens :as tokens]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

(def ^:private CONSULT_TIMEOUT_MS  60000)
(def ^:private CONSULT_TTFT_MS     30000)
(def ^:private CONSULT_IDLE_MS     20000)

(defn- resolve-routing
  "map `preference` → `{:provider :model}` via env's
   `:consult-config` map or `consult/DEFAULT_PREFERENCE_MAP` fallback."
  [env preference]
  (let [config (or (:consult-config env) consult/DEFAULT_PREFERENCE_MAP)]
    (get config preference)))

(defn- build-user-message
  "assemble the single user message the consult LLM sees.
   Carries the question, the focus vec, and the REQUEST-TIME ctx
   snapshot pinned on the intent (thread-isolation gate)."
  [intent]
  (let [{:keys [question focus preference ctx-snapshot]} intent
        focus-block (when (seq focus)
                      (str "\nFocus points:\n"
                        (str/join "\n"
                          (map-indexed (fn [i f] (str "  " (inc i) ". " f))
                            focus))))]
    (str "Preference: " (name preference) "\n"
      "Token cap for :content: " (get consult/TOKEN_CAPS preference) " tokens\n"
      focus-block
      "\n\nQuestion:\n" question
      "\n\nPrimary ctx snapshot (request-time pin; for grounding only,"
      " do NOT echo verbatim):\n"
      (or ctx-snapshot "")
      "\n\nReply with one ```clojure``` fence ending in (done {…}).")))

(defn- build-messages
  "assemble the consult message vector. Primary's
   CORE_SYSTEM_PROMPT is deliberately omitted (R1 contract); the
   consult LLM sees CONSULT_BASE_PROMPT + auto-gen BINDINGS only.

   The ctx snapshot is read off the intent (request-time pin), not
   off env's live ctx-atom (thread-isolation gate)."
  [env intent]
  (let [active-exts (or (:active-extensions env) [])
        sys-msgs    (prompt/assemble-consult-prompt-messages active-exts)
        user-text   (build-user-message intent)]
    (conj (vec sys-msgs) {:role "user" :content user-text})))

(defn- clip-excerpt
  "tail-clip an excerpt to <= consult/MAX_EXCERPT_TOKENS.
   A sliced abstract is still useful; no marker needed.
   Uses a token-count approximation (~4 chars/token) to find the cut,
   then re-counts and shrinks further if the approximation underclipped."
  [s]
  (if-not (string? s)
    s
    (let [tok (tokens/count-tokens s)]
      (if (<= tok consult/MAX_EXCERPT_TOKENS)
        s
        ;; Rough: 4 chars per token. Clip there, then iterate down.
        (let [approx-chars (max 1 (* 4 consult/MAX_EXCERPT_TOKENS))]
          (loop [n (min approx-chars (count s))]
            (let [head (subs s 0 n)]
              (if (or (zero? n)
                    (<= (tokens/count-tokens head) consult/MAX_EXCERPT_TOKENS))
                head
                ;; Drop another ~50 chars and retry; bounded by string length
                (recur (max 0 (- n 50)))))))))))

(defn- coerce-citations
  "validate + truncate citations.
     - drop entries missing both :url and :title
     - require :type ∈ #{:paper :web :code :doc}
     - tail-clip past MAX_CITATIONS
     - tail-clip each :excerpt past MAX_EXCERPT_TOKENS
   Returns `[citations-vec dropped-count]`."
  [raw]
  (let [allowed-types #{:paper :web :code :doc}
        valid (->> (or raw [])
                (filter (fn [c]
                          (and (map? c)
                            (contains? allowed-types (:type c))
                            (or (string? (:url c))
                              (string? (:title c))))))
                (mapv (fn [c]
                        (cond-> c
                          (string? (:excerpt c))
                          (update :excerpt clip-excerpt)))))
        kept  (subvec valid 0 (min (count valid) consult/MAX_CITATIONS))
        dropped (- (count (or raw [])) (count kept))]
    [kept (max 0 dropped)]))

(defn- build-done-binding
  "SCI binding `done` captures the single `(done {…})`
   payload onto a thread-local atom. Same FORM the primary uses
   (`(done {…})`) — the SCHEMA differs (consult expects :content +
   :citations + :confidence + :focus-met?, primary expects :answer +
   :trailer-drop + :trailer-summarize + :archive). One emission point
   across both engines means the model never has to learn two surfaces."
  [done-atom]
  (fn done [payload]
    (reset! done-atom payload)
    :vis/answer))

(defn- run-fence-eval!
  "evaluate the consult fence in a fresh `:consult`-scope
   SCI sandbox. Returns the captured (done {…}) payload, or nil when
   the fence did not call `done`. `env` reserved for future research
   tool routing."
  [_env intent fence-src]
  (let [done-atom (atom nil)
        ;; minimum consult bindings. `done` is the only
        ;; capture surface; engine mutators are explicitly absent (they
        ;; have `:engine-scope #{:main}`). Same form name as primary's
        ;; `done`; the schema differs but the emission point does not.
        bindings    {'done (build-done-binding done-atom)}
        sci-info    (env/create-sci-context bindings :consult)
        {:keys [sci-ctx sandbox-ns]} sci-info]
    (try
      (sci/eval-string+ sci-ctx fence-src {:ns sandbox-ns})
      (catch Throwable t
        (tel/log! {:level :warn :id ::consult-eval-error
                   :data {:id (:id intent)
                          :ex-class (.getName (class t))
                          :ex-msg (ex-message t)}}
          "consult fence eval threw")))
    @done-atom))

(defn- extract-fence-source
  "pull the consult LLM's clojure source out of `svar`'s
   result. svar returns `:blocks` (vec of {:lang :source}); we use only
   the FIRST clojure block. Returns nil when no usable block exists."
  [ask-result]
  (let [blocks (or (:blocks ask-result) [])
        clj    (some (fn [b]
                       (let [lang (str/lower-case (or (:lang b) ""))]
                         (when (or (= lang "clojure") (= lang "clj") (= lang ""))
                           (:source b))))
                 blocks)]
    (when (and (string? clj) (not (str/blank? clj))) clj)))

(defn- single-attempt
  "ONE LLM call + ONE SCI eval. Returns either:
     - {:ok? true  :payload <map>}        the consult's (done {…}) payload
     - {:ok? false :failure <entry-map>}  a populated failure entry"
  [env intent messages]
  (let [{id :id :keys [preference focus]} intent
        routing (resolve-routing env preference)
        started (System/currentTimeMillis)
        finish  (fn [err reason]
                  {:ok? false
                   :failure {:id id
                             :status :failed
                             :error err
                             :reason reason
                             :focus focus
                             :preference preference
                             :duration-ms (- (System/currentTimeMillis) started)
                             :retries 0}})]
    (try
      (let [ask-opts {:lang     "clojure"
                      :messages messages
                      :routing  {:provider (:provider routing)
                                 :model    (:model routing)}
                      :timeout-ms      CONSULT_TIMEOUT_MS
                      :ttft-timeout-ms CONSULT_TTFT_MS
                      :idle-timeout-ms CONSULT_IDLE_MS
                      :reasoning :minimal
                      :preserved-thinking? false}
            ask-result (svar/ask-code! (:router env) ask-opts)
            fence-src  (extract-fence-source ask-result)]
        (cond
          (nil? fence-src)
          (finish :no-fence "consult LLM returned no clojure fence")

          :else
          (let [payload (run-fence-eval! env intent fence-src)]
            (cond
              (nil? payload)
              (finish :missing-answer "consult fence did not call (done …)")

              (not (map? payload))
              (finish :malformed-answer
                (str "(done …) payload must be a map; got "
                  (pr-str (type payload))))

              (or (not (string? (:content payload)))
                (str/blank? (:content payload)))
              (finish :malformed-answer
                "(done …) :content must be a non-blank string")

              :else
              {:ok? true :payload payload :messages messages
               :assistant-fence fence-src
               :started started}))))
      (catch Throwable t
        (tel/log! {:level :warn :id ::consult-failed
                   :data {:id id
                          :preference preference
                          :ex-class (.getName (class t))
                          :ex-msg (ex-message t)}}
          "consult run failed")
        (finish :provider-error (or (ex-message t) "consult run threw"))))))

(defn- compression-retry-message
  "build the user retry message for an overflowed answer.
   Carries N (actual), M (cap), X = N - M so the consult LLM can size
   the cut. Engine never truncates; the consult LLM owns the rewrite."
  [n m]
  (str "Your previous answer was " n " tokens; cap for this preference is "
    m " tokens (over by " (- n m) " tokens). Compress :content to fit"
    " within " m " tokens. Keep the most-cited findings and critical"
    " citations; drop tangents, adjectives, repetition. Same :focus /"
    " :citations spirit, tighter prose. Re-emit one ```clojure``` fence"
    " ending in (done {…})."))

(defn- build-entry-from-payload
  "assemble the success entry map."
  [intent payload started retries]
  (let [{id :id :keys [preference focus]} intent
        [citations dropped] (coerce-citations (:citations payload))
        duration (- (System/currentTimeMillis) started)
        base {:id id
              :status     :active
              :content    (:content payload)
              :citations  citations
              :confidence (or (:confidence payload) :medium)
              :focus      focus
              :focus-met? (vec (or (:focus-met? payload) []))
              :preference preference
              :duration-ms duration
              :retries    retries}]
    (cond-> base
      (pos? dropped) (assoc :too-many-citations dropped))))

(defn run-consult!
  "run ONE consult intent on the current thread.
   Returns an entry-shaped map suitable for `consult/append-result!`.

   Pure-ish: side effects are (a) the LLM call(s) via the env's router
   and (b) the SCI eval(s) that run the consult-LLM's clojure fence(s).
   Primary's ctx-atom is NOT mutated from here; the parallel runner owns the
   atomic `append-result!` swap.

   Worst-case 2 LLM calls per intent: a successful first attempt
   completes in one call; an overflow on the first attempt triggers a
   single retry. If both attempts still overflow, the entry lands as
   :status :failed :error :exceeds-cap.

   Failure paths return `:status :failed :error <kw> :reason <string>`
   so the renderer  can surface them in the compact preview."
  [env intent]
  (let [{id :id :keys [preference focus]} intent
        started (System/currentTimeMillis)
        routing (resolve-routing env preference)
        runner-thread (.getName (Thread/currentThread))]
    (cond
      (nil? routing)
      {:id id
       :status :failed
       :error :consult-router-missing
       :reason (str "no provider/model resolved for preference "
                 (pr-str preference))
       :focus focus
       :preference preference
       :duration-ms 0
       :retries 0}

      :else
      (let [cap        (get consult/TOKEN_CAPS preference)
            initial-msgs (build-messages env intent)]
        (tel/log! {:level :debug :id ::consult-start
                   :data {:id id :preference preference
                          :provider (:provider routing) :model (:model routing)
                          :thread runner-thread}}
          "consult run starting")
        (let [first-att (single-attempt env intent initial-msgs)]
          (cond
            (not (:ok? first-att))
            (:failure first-att)

            :else
            (let [payload (:payload first-att)
                  n1 (tokens/count-tokens (:content payload))]
              (cond
                (<= n1 cap)
                ;; First attempt fits the cap.
                (build-entry-from-payload intent payload started 0)

                :else
                ;; Overflow: ONE retry with a compression instruction.
                (let [retry-msgs (-> (vec initial-msgs)
                                   (conj {:role "assistant"
                                          :content (str "```clojure\n"
                                                     (:assistant-fence first-att)
                                                     "\n```")})
                                   (conj {:role "user"
                                          :content (compression-retry-message n1 cap)}))
                      retry-att (single-attempt env intent retry-msgs)]
                  (cond
                    (not (:ok? retry-att))
                    (assoc (:failure retry-att) :retries 1)

                    :else
                    (let [payload2 (:payload retry-att)
                          n2 (tokens/count-tokens (:content payload2))]
                      (cond
                        (<= n2 cap)
                        (build-entry-from-payload intent payload2 started 1)

                        :else
                        {:id id
                         :status :failed
                         :error :exceeds-cap
                         :reason (str "first " n1 " / cap " cap
                                   " / second " n2 " (still over)")
                         :focus focus
                         :preference preference
                         :duration-ms (- (System/currentTimeMillis) started)
                         :retries 1}))))))))))))
