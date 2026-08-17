(ns com.blockether.vis.internal.vision-describe
  "Borrowed EYES for a blind target model.

   A session routes to ONE model, and that model may have no `:vision` capability
   (a Copilot tier without vision, glm-5-turbo, deepseek, most coding plans). The
   images are still real — the user attached a screenshot, an earlier iteration
   plotted a figure — so today they are dropped from the wire and the model is told
   to open them with PIL. PIL answers `(1920, 1080) RGB` and nothing else: pixel
   size is not meaning, and the agent burns a tool call to learn nothing.

   This namespace closes that gap WITHOUT switching the turn's model: one cheap
   side-channel `ask!` on the cheapest+fastest model in the SAME fleet that does
   carry `:vision` (svar picks it by capability, across providers) turns each image
   into text, and the text rides the prompt where the image would have been. The
   pinned model, its thinking chain and its tool continuity are untouched.

   Three properties make it affordable rather than wasteful:

   - CONTENT-KEYED CACHE. Attachments replay on every request for the rest of the
     session, so a per-request description would be re-billed forever. Keyed by the
     digest of the bytes, an image is described exactly ONCE per process.
   - OWN FAILURE POLICY. Like titling, the describe call never waits out a 429 and
     never fails over provider chains: the foreground turn owns the quota. A refusal
     or a deadline returns nil and the caller keeps today's behaviour.
   - SECOND-HAND BY CONTRACT. The description is labelled as another model's report
     in the prompt, never as the agent's own sight, so pixel-exact work still goes
     through the imaging path.

   A LEAF: svar + attachments + runtime-settings + toggles, never back on the loop."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.strutil :refer [truncate]]
            [com.blockether.vis.internal.toggles :as toggles]
            [taoensso.telemere :as tel])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64]))

(def TOGGLE_ID
  "Feature toggle gating the whole side-channel (registered in `toggles`)."
  "vision_fallback_describe")

(def ^:private DESCRIBE_TTFT_MS 20000)

(def ^:private DESCRIBE_IDLE_MS 15000)

(def ^:private DESCRIBE_SEMANTIC_MS 45000)

(def ^:private DESCRIBE_HARD_DEADLINE_MS
  "Absolute wall-clock cap on the whole describe pass, images included. svar's soft
   timeouts have been observed not to fire on some transport hangs, and this runs
   INSIDE the request build: an unbounded wait would park the user's turn."
  45000)

(def ^:private MAX_DESCRIBED_PER_PASS
  "How many UNCACHED images one pass may describe. The cache makes the steady state
   free, so this only bounds the burst when a turn arrives with a pile of new
   images; the rest keep the plain not-attached note."
  6)

(def ^:private DESCRIBE_CONTEXT_CHARS 600)

(def ^:private MAX_CACHE_ENTRIES 256)

(def ^:private DESCRIBE_ROUTING
  "Capability-first routing: the cheapest+fastest model in the WHOLE fleet that
   advertises `:vision`. svar treats `:capabilities` as a hard filter, so this
   resolves to nil (and the feature stays off) when nothing configured can see."
  {:capabilities #{:vision} :optimize [:cost :speed]})

(def ^:private DESCRIBE_RATE_LIMIT_POLICY
  "The foreground turn owns the rate-limit budget, not this side-channel. A describe
   call that sleeps through `Retry-After`, walks the same-provider delay schedule
   and then fails over to another plan spends exactly the quota the user's request
   needs — for a paragraph we can do without. So the 429 phase is EMPTY: the first
   refusal returns immediately and the caller keeps today's blind behaviour."
  {:same-provider-delays-ms []
   :fallback-after-ms 0
   :respect-retry-after? false
   :fallback-provider? false})

(defn- describe-router
  "`router` carrying the describe call's own failure policy: no rate-limit waiting
   and a single HTTP attempt, so a busy gateway costs one refused request instead
   of a minute of backoff on the user's turn."
  [router]
  (-> router
      (assoc :rate-limit DESCRIBE_RATE_LIMIT_POLICY)
      (update :network merge {:max-retries 1})))

(defn enabled?
  "Whether the vision-description fallback may run at all."
  []
  (toggles/enabled? TOGGLE_ID))

(defn sighted-model
  "Descriptor of the model this fleet would use to LOOK at an image, or nil when no
   configured provider carries `:vision`. Cheap: router arithmetic, no I/O.

   TOTAL BY CONTRACT. The probe runs INSIDE request assembly and its answer is only
   ever an OFFER — nil means the caller keeps today's blind behaviour, so nothing
   here is worth failing a turn over. Two ways it can fail on a healthy session:
   svar's resolver reads live provider state and throws on provider/model
   combinations it rejects, and Vis passes router-SHAPED config maps around (its own
   `resolve-effective-model` is structural for exactly that reason). A probe that
   propagated would abort turns that carry no images at all."
  [router]
  (when (map? router)
    (try (svar-router/resolve-effective-model router DESCRIBE_ROUTING)
         (catch Throwable t
           (tel/log! {:level :debug :id ::sight-probe-failed :data {:error (ex-message t)}}
                     "Vision-capability probe failed; treating the fleet as blind")
           nil))))

(defn available?
  "True when the fallback is on AND some configured model can actually see."
  [router]
  (boolean (and (enabled?) (some? (sighted-model router)))))

(defonce ^:private description-cache
  ;; {content-digest {:text "…" :model "glm-5v-turbo"}} — an image replays on every
  ;; request of the session, so its description is computed once per process.
  (atom {}))

(defn clear-cache! "Drop every memoized description. Tests only." [] (reset! description-cache {}))

(defn- content-digest
  "Cache key: the payload's own bytes plus the container they ride in."
  [{:keys [base64 media-type]}]
  (let
    [md
     (MessageDigest/getInstance "SHA-256")

     digest
     (.digest md (.getBytes (str media-type "|" base64) StandardCharsets/UTF_8))]

    (.encodeToString (Base64/getUrlEncoder) digest)))

(defn- cache-put!
  [k description]
  (swap! description-cache (fn [cache]
                             (assoc (if (>= (count cache) (long MAX_CACHE_ENTRIES)) {} cache)
                               k description)))
  description)

(def ^:private describe-spec
  "Structured output, not a fence: `ask!` + spec means a model that wraps its answer
   in markdown still yields the description field instead of leaking the fence."
  (svar/spec (svar/field
               svar/NAME
               :description
               svar/TYPE
               svar/TYPE_STRING
               svar/CARDINALITY
               svar/CARDINALITY_ONE
               svar/DESCRIPTION
               "Factual description of the image, dense and self-contained, plain text.")))

(def ^:private DESCRIBE_SYSTEM_PROMPT
  (str "You are the EYES of a coding agent that cannot see images. Describe the image so the "
       "agent can act on it without ever seeing it.\n"
       "- Name what the image IS first (terminal screenshot, UI screenshot, chart, diagram, "
       "photo, scanned document, error dialog).\n"
       "- TRANSCRIBE every readable text VERBATIM: commands, code, log lines, stack traces, "
       "error messages, labels, menu items, axis ticks, legends, numbers. Keep their order and "
       "grouping. Transcription matters more than prose.\n"
       "- Describe layout only where it carries meaning (which pane, which column, what is "
       "highlighted or selected, what is cut off).\n"
       "- Report colour or styling only when it means something (a red failing test, a "
       "highlighted diff line, a chart series).\n"
       "- State plainly what is unreadable or ambiguous. Never guess, never invent text, never "
       "give the agent advice.\n" "Plain text only, no markdown fences. Be complete but compact."))

(defn- describe-messages
  [context label {:keys [base64 media-type]}]
  [{:role "system" :content DESCRIBE_SYSTEM_PROMPT}
   (svar/user (str "Image: "
                   label
                   (when-let [c (not-empty (str/trim (str context)))]
                     (str
                       "\n\nThe agent is working on this request — favour what it needs, but never "
                       "let it invent detail that is not in the image:\n"
                       (truncate c DESCRIBE_CONTEXT_CHARS))))
              (svar/image base64 (or (not-empty (str media-type)) "image/png")))])

(defn- describe-future
  "Async `ask!` for ONE image. The future itself catches, so the caller only ever
   derefs a value: `{:ok result}` or `{:error t}`."
  [router context label image]
  (future (try {:ok (svar/ask! (describe-router router)
                               (rt/with-default-ask-code-idle-timeout
                                 {:messages (describe-messages context label image)
                                  :spec describe-spec
                                  :reasoning :off
                                  ;; An agent-initiated call, never a user interaction: the
                                  ;; coding plans bill an unmarked request as user initiated.
                                  :llm-headers rt/AGENT_INITIATOR_HEADERS
                                  :routing DESCRIBE_ROUTING
                                  :ttft-timeout-ms DESCRIBE_TTFT_MS
                                  :idle-timeout-ms DESCRIBE_IDLE_MS
                                  :semantic-timeout-ms DESCRIBE_SEMANTIC_MS}))}
               (catch Throwable t {:error t}))))

(defn- outcome->text
  [outcome]
  (some-> outcome
          :ok
          :result
          :description
          str
          str/trim
          not-empty))

(defn describe-images
  "Descriptions for already-wired images (the `attachments/wire-image` shape:
   `:base64`, `:media-type`), ALIGNED to the input order.

   Each entry is `{:text … :model …}` or nil — nil for an image this pass could not
   describe (burst cap, deadline, refusal), which the caller renders exactly as it
   does today. Returns nil outright when the fallback is off or nothing in the
   fleet can see, so `(when-let [ds (describe-images …)] …)` is the whole
   caller-side branch.

   Blocking, but bounded by `DESCRIBE_HARD_DEADLINE_MS` for the WHOLE pass: the
   asks run in PARALLEL against one shared wall clock, so a turn arriving with
   several new images costs about one call's latency, and every later request in
   the session answers from cache."
  [router context images]
  (when (and (seq images) (enabled?))
    (when-let [model (sighted-model router)]
      (let
        [started (System/currentTimeMillis)
         entries (mapv (fn [image]
                         (let [k (content-digest image)]
                           {:image image :key k :cached (get @description-cache k)}))
                       images)
         ;; Only an image nobody has described yet costs a call.
         pending (into []
                       (comp (remove :cached) (take MAX_DESCRIBED_PER_PASS))
                       (map-indexed (fn [idx entry]
                                      (assoc entry :idx idx))
                                    entries))
         futures (mapv (fn [{:keys [idx image]}]
                         [idx
                          (describe-future router context (attachments/image-label image) image)])
                       pending)
         texts
         (into {}
               (keep
                 (fn [[idx fut]]
                   (let
                     [remaining (max 1000
                                     (- (long DESCRIBE_HARD_DEADLINE_MS)
                                        (- (System/currentTimeMillis) (long started))))
                      outcome (deref fut remaining ::deadline)]

                     (cond
                       (= ::deadline outcome)
                       (do
                         (future-cancel fut)
                         (tel/log!
                           {:level :warn
                            :id ::describe-deadline
                            :data {:deadline-ms DESCRIBE_HARD_DEADLINE_MS :model (:name model)}}
                           "Vision description exceeded its hard deadline; image stays undescribed")
                         nil)
                       (:error outcome)
                       (do (tel/log! {:level :warn
                                      :id ::describe-failed
                                      :data {:error (ex-message (:error outcome))
                                             :model (:name model)}}
                                     "Vision description call failed; image stays undescribed")
                           nil)
                       :else (when-let [text (outcome->text outcome)]
                               [idx text])))))
               futures)]

        (mapv (fn [idx {:keys [key cached]}]
                (or cached
                    (when-let [text (get texts idx)]
                      (cache-put! key {:text text :model (str (:name model))}))))
              (range)
              entries)))))

(defn describe-attachments
  "Wire raw user attachments through the send-time image gate and describe what
   survives it, keyed by the LABEL the prompt manifest uses for the same row.

   Returns `{label {:text … :model …}}` — empty when the fallback is off, when no
   configured model can see, or when nothing describable came out of the gate.
   Human-only rows never reach the describer: `wire-images` skips them first, and
   bytes the caller deliberately kept off the wire are not this side-channel's
   business either."
  [router context attachments]
  (let
    [wired
     (:attached (attachments/wire-images attachments))

     descriptions
     (describe-images router context wired)]

    (if (seq descriptions)
      (into {}
            (keep identity)
            (map (fn [image description]
                   (when description [(attachments/image-label image) description]))
                 wired
                 descriptions))
      {})))

(defn descriptions-message
  "The plain-TEXT `{:role \"user\"}` message that stands in for images a blind target
   cannot be shown, or nil when nothing was described.

   `described` is `[{:label … :text … :model …}]`. The copy is deliberate: the
   description is attributed to the model that produced it and marked second-hand,
   because an agent that believes it SAW the pixels will happily testify about
   details no one described. Pixel-exact work still goes through the bytes."
  [described]
  (when (seq described)
    (let
      [models
       (into (sorted-set) (keep #(not-empty (str (:model %)))) described)

       body
       (str/join "\n"
                 (map (fn [{:keys [label text]}]
                        (str "- " label ": " text))
                      described))]

      {:role "user"
       :content (str "["
                     (count described)
                     " image(s) here are stored but NOT in this request: the active model has no"
                     " vision. "
                     (if (seq models)
                       (str "A vision-capable model (" (str/join ", " models) ") looked at them")
                       "Another model looked at them")
                     " and reported:\n"
                     body
                     "\nThat is a second-hand report, not your own sight — do not claim to have"
                     " seen anything it does not mention. For pixel-exact work open the bytes"
                     " with read_attachment(\"<id>\") in Python.]")})))
