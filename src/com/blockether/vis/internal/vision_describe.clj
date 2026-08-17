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
            [com.blockether.vis.internal.provider-error :as perr]
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

(def ^:private MAX_DESCRIPTION_CHARS
  "Cap on ONE image's description, in characters. The text is quoted verbatim into
   the prompt and then into every later request of the session, so a model that
   answers a dense screenshot with a novel must not be able to spend the turn's
   whole context on one picture."
  4000)

(def ^:private MAX_DESCRIBE_ROUNDS
  "Hard ceiling on how many providers one image may be offered to inside a single
   pass, before the fleet's own size lowers it.

   Measured on a seven-provider fleet, three was not enough: the first offer hit a
   20s stream timeout, the second a 400, the third a plan whose credential the
   process could not resolve — and the pass gave up two providers short of one that
   could actually see. Every round removes at least one provider, so the real bound
   is the fleet, and the wall clock is what keeps a hopeless one honest."
  6)

(def ^:private DESCRIBE_NETWORK
  "Network policy for the side-channel. One HTTP attempt, and a provider that has not
   even answered with headers inside `:ttft-timeout-ms` is abandoned so the pass can
   still afford the next one — svar's 20s default spends nearly half the pass deadline
   on ONE hung provider."
  {:max-retries 1 :ttft-timeout-ms 8000 :idle-timeout-ms 15000})

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
      (update :network merge DESCRIBE_NETWORK)))

(defn enabled?
  "Whether the vision-description fallback may run at all."
  []
  (toggles/enabled? TOGGLE_ID))

;; ── Learned wire facts ───────────────────────────────────────────────────────

(defonce ^:private image-blind-providers
  ;; #{provider-id} — providers whose WIRE refused an image content part outright
  ;; (HTTP 400 `unknown variant image_url, expected text`). Catalog and config both
  ;; describe the MODEL; only a request settles what the endpoint in front of it
  ;; accepts, so the answer it gave is remembered for the rest of the process and
  ;; no later image is offered there again.
  (atom #{}))

(defonce ^:private image-blind-models
  ;; {model-name #{provider-id …}} — models whose provider answered "this model
  ;; does not support image input" while carrying pixels for its OTHER models.
  ;; That is a fact about the MODEL, so the NAME is out wherever it is served
  ;; from; the provider ids are kept only to say where it was learned.
  (atom {}))

(defonce ^:private proven-eye
  ;; {:provider-id :opencode-go :model "mimo-v2.5"} — the pair that last ANSWERED with a
  ;; description, or nil. Capability metadata only says who COULD look; a 200 says who did.
  ;; Without this the cheapest sighted candidate is re-offered first on every pass, so a fleet
  ;; whose cheapest eyes are broken pays their failures again for every new image — measured on a
  ;; live 7-provider fleet: two Copilot providers, ~20s of TTFT timeout each, in front of the
  ;; model that actually answered.
  (atom nil))

(defn remember-working-eye!
  "Remember the provider/model pair that just described an image. Idempotent.

   The id is kept exactly as svar reported it — provider ids travel as keywords through routing,
   and a stringified one would silently never match a preference or an exclusion."
  [provider-id model]
  (when-not (str/blank? (str provider-id))
    (reset! proven-eye {:provider-id provider-id
                        :model (some-> model
                                       str
                                       not-empty)}))
  nil)

(defn forget-working-eye!
  "Drop the remembered eye when it is the one that just failed.

   Called for EVERY failure, not only a permanent refusal: an expired credential is no fact about
   the model, but it does mean this provider is not the one to try FIRST any more. Whoever answers
   the next image is remembered in its place."
  [{:keys [provider-id model]}]
  (swap! proven-eye (fn [eye]
                      (when-not (or (and provider-id (= provider-id (:provider-id eye)))
                                    (and model (= model (:model eye))))
                        eye)))
  nil)

(defn working-eye
  "The provider/model pair that last described an image in this process, or nil."
  []
  @proven-eye)

(defn- describe-routing
  "`DESCRIBE_ROUTING` narrowed by everything this session already learned: providers whose wire
   refused pixels and model NAMES that cannot read them are excluded, and the provider that last
   ANSWERED is preferred.

   The preference names a PROVIDER, never a forced model — svar still optimizes for cost and speed
   within it, the capability filter still holds, and the preference disappears the moment that
   provider is excluded, so nothing learned here can pin a pass to a broken endpoint."
  [excluded excluded-models]
  (let [preferred (:provider-id @proven-eye)]
    (cond-> DESCRIBE_ROUTING
      (seq excluded)
      (assoc :exclude-providers excluded)

      (seq excluded-models)
      (assoc :exclude-models excluded-models)

      (and preferred (not (contains? (set excluded) preferred)))
      (assoc :prefer-providers [preferred]))))

(defn remember-image-blind!
  "Record `provider-id` as unable to carry an image content part at all. Idempotent."
  [provider-id]
  (when provider-id
    (when-not (contains? @image-blind-providers provider-id)
      (tel/log! {:level :warn :id ::provider-image-blind :data {:provider provider-id}}
                (str "Provider " provider-id
                     " rejected an image content part; "
                     "sending it text only for the rest of this process")))
    (forget-working-eye! {:provider-id provider-id})
    (swap! image-blind-providers conj provider-id))
  provider-id)

(defn remember-image-blind-model!
  "Record `model` as unable to READ pixels, as learned on `provider-id`. Idempotent.

   Deliberately NOT a provider verdict: the same endpoint keeps serving whatever
   else it has eyes for, and the name stays out everywhere it is offered."
  [model provider-id]
  (when-let
    [name (some-> model
                  str
                  not-empty)]
    (when-not (contains? @image-blind-models name)
      (tel/log! {:level :warn :id ::model-image-blind :data {:model name :provider provider-id}}
                (str "Model " name
                     " rejected image input; "
                     "no image is offered to that model for the rest of this process")))
    (forget-working-eye! {:model name})
    (swap! image-blind-models update name (fnil conj #{}) provider-id)
    name))

(defn remember-image-refusal!
  "Learn ONE `provider-error/image-rejections` row at the scope it actually proves:
   a wire that has no image variant blinds the PROVIDER, a model that cannot read
   pixels blinds only that NAME."
  [{:keys [provider model scope]}]
  (case scope
    :wire
    (remember-image-blind! provider)

    :model
    (remember-image-blind-model! model provider)

    nil))

(defn image-blind-provider?
  "True when `provider-id` already proved its wire cannot carry an image."
  [provider-id]
  (contains? @image-blind-providers provider-id))

(defn image-blind-model?
  "True when `model` already proved it cannot read pixels, on any provider."
  [model]
  (contains? @image-blind-models
             (some-> model
                     str
                     not-empty)))

(defn blind-model-names
  "Model NAMES no request may show an image to."
  []
  (set (keys @image-blind-models)))

(defn clear-image-blind!
  "Forget everything this session learned about which eyes work. Tests only."
  []
  (reset! image-blind-providers #{})
  (reset! image-blind-models {})
  (reset! proven-eye nil))

(defn sighted-model
  "Descriptor of the model this fleet would use to LOOK at an image, or nil when no
   configured provider carries `:vision` — one that already refused pixels, on the
   wire or by name, does not count. Cheap: router arithmetic, no I/O.

   TOTAL BY CONTRACT. The probe runs INSIDE request assembly and its answer is only
   ever an OFFER — nil means the caller keeps today's blind behaviour, so nothing
   here is worth failing a turn over. Two ways it can fail on a healthy session:
   svar's resolver reads live provider state and throws on provider/model
   combinations it rejects, and Vis passes router-SHAPED config maps around (its own
   `resolve-effective-model` is structural for exactly that reason). A probe that
   propagated would abort turns that carry no images at all."
  [router]
  (when (map? router)
    (let
      [blind
       @image-blind-providers

       blind-models
       (blind-model-names)]

      (try (svar-router/resolve-effective-model router (describe-routing blind blind-models))
           (catch Throwable t
             (tel/log! {:level :debug :id ::sight-probe-failed :data {:error (ex-message t)}}
                       "Vision-capability probe failed; treating the fleet as blind")
             nil)))))

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

(defn- distinct-by
  "`coll` keeping only the FIRST element per `(f element)`, order preserved."
  [f coll]
  (second (reduce (fn [[seen out] x]
                    (let [k (f x)]
                      (if (contains? seen k) [seen out] [(conj seen k) (conj out x)])))
                  [#{} []]
                  coll)))

(defn- describe-future
  "Async `ask!` for ONE image on `routing`. The future itself catches, so the caller
   only ever derefs a value: `{:ok result}` or `{:error t}`."
  [router routing context label image]
  (future (try {:ok (svar/ask! (describe-router router)
                               (rt/with-default-ask-code-idle-timeout
                                 {:messages (describe-messages context label image)
                                  :spec describe-spec
                                  :reasoning :off
                                  ;; An agent-initiated call, never a user interaction: the
                                  ;; coding plans bill an unmarked request as user initiated.
                                  :llm-headers rt/AGENT_INITIATOR_HEADERS
                                  :routing routing
                                  :ttft-timeout-ms DESCRIBE_TTFT_MS
                                  :idle-timeout-ms DESCRIBE_IDLE_MS
                                  :semantic-timeout-ms DESCRIBE_SEMANTIC_MS}))}
               (catch Throwable t {:error t}))))

(defn- outcome->description
  "`{:text … :model …}` for a finished ask, or nil when nothing usable came back.

   The model NAMED is the one svar actually routed to, not the one the probe
   guessed, so an image described after a cross-provider retry is attributed to
   the model that really looked at it."
  [outcome fallback-model]
  (when-let
    [text (some-> outcome
                  :ok
                  :result
                  :description
                  str
                  str/trim
                  not-empty)]
    {:text (truncate text MAX_DESCRIPTION_CHARS)
     :model (or (not-empty (str (:routed/model (:ok outcome)))) (str fallback-model))}))

(defn- describe-round
  "ONE parallel round of asks over `pairs` (`[[idx image] …]`), all sharing the
   `deadline-at` wall clock.

   Returns `{:done {idx description} :failed [[idx image] …] :broken #{provider-id}
   :blinded #{model-name}}`. `:failed` collects only calls that ERRORED — a refusal
   and a deadline are answers about the image, a broken provider is not — while
   `:broken` names the providers to drop for the next round and `:blinded` the model
   NAMES that answered they cannot read pixels, which their provider survives."
  [router routing context deadline-at fallback-model pairs]
  (reduce (fn [acc [idx fut image]]
            (let
              [remaining
               (max 0 (- (long deadline-at) (System/currentTimeMillis)))

               outcome
               (deref fut remaining ::deadline)]

              (cond (= ::deadline outcome)
                    (do (future-cancel fut)
                        (tel/log!
                          {:level :warn
                           :id ::describe-deadline
                           :data {:deadline-ms DESCRIBE_HARD_DEADLINE_MS :model fallback-model}}
                          "Vision description exceeded its hard deadline; image stays undescribed")
                        acc)
                    (:error outcome)
                    (let
                      [err
                       (:error outcome)

                       provider-id
                       (:provider-id (ex-data err))

                       ;; A wire with no image part and a model that cannot read
                       ;; pixels are both permanent, but they generalize
                       ;; differently: learn each row at the scope it proves
                       ;; instead of taking the whole endpoint down for either.
                       learned
                       (perr/image-rejections err)

                       blinded
                       (into #{} (comp (filter #(= :model (:scope %))) (keep :model)) learned)]

                      (run! remember-image-refusal! learned)
                      (forget-working-eye! {:provider-id provider-id})
                      (tel/log! {:level :warn
                                 :id ::describe-failed
                                 :data {:error (ex-message err)
                                        :provider provider-id
                                        :model fallback-model
                                        :learned learned}}
                                "Vision description call failed; image stays undescribed")
                      (cond-> (update acc :failed conj [idx image])
                        (seq blinded)
                        (update :blinded into blinded)

                        ;; A NAMED model's blindness leaves its provider working, so
                        ;; the next round stays there and asks its other eyes. Any
                        ;; other failure — credentials, a 5xx, a wire refusal — is the
                        ;; provider's, and the next round goes elsewhere.
                        (and provider-id (empty? blinded))
                        (update :broken conj provider-id)))
                    :else (if-let [description (outcome->description outcome fallback-model)]
                            (do (remember-working-eye! (:routed/provider-id (:ok outcome))
                                                       (:routed/model (:ok outcome)))
                                (assoc-in acc [:done idx] description))
                            acc))))
          {:done {} :failed [] :broken #{} :blinded #{}}
          ;; Eager on purpose: every ask of a round is in flight before the first deref.
          (mapv (fn [[idx image]]
                  [idx
                   (describe-future router routing context (attachments/image-label image) image)
                   image])
                pairs)))

(defn- fleet-offers
  "How many distinct OFFERS this fleet can make: one per provider/model pair.

   Every round of a pass removes at least one provider (its wire broke) or one
   model NAME (it cannot read pixels), so this is the loop's own bound. Counting
   providers alone was wrong the moment a refusal could be model-scoped: a single
   provider serving a small blind model in front of a seeing one got no second
   round and the image stayed undescribed."
  [router]
  (reduce +
          0
          (map (fn [provider]
                 (max 1 (count (:models provider))))
               (:providers router))))

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
        [entries (mapv (fn [image]
                         (let [k (content-digest image)]
                           {:image image :key k :cached (get @description-cache k)}))
                       images)
         ;; ONE ask per distinct payload. The same picture attached twice, or a
         ;; figure replayed under a second name, otherwise pays twice and burns
         ;; two of the burst slots for one description.
         pending (->> entries
                      (map-indexed (fn [idx entry]
                                     (assoc entry :idx idx)))
                      (remove :cached)
                      (remove #(str/blank? (str (:base64 (:image %)))))
                      (distinct-by :key)
                      (take MAX_DESCRIBED_PER_PASS)
                      vec)
         deadline-at (+ (System/currentTimeMillis) (long DESCRIBE_HARD_DEADLINE_MS))
         ;; A provider that ERRORS — stale credentials, a gateway 400, a 5xx — must
         ;; not take the whole fleet's eyes down with it. Every round excludes the
         ;; providers that already broke (the error itself names them) and offers
         ;; what they dropped to the next-best model. Proven live: two Copilot
         ;; providers failed in a row on one absent credential, and the image was
         ;; only described because a third provider got the offer.
         by-index (loop
                    [pairs (mapv (juxt :idx :image) pending)
                     ;; Start from what earlier passes already learned: a wire that has no
                     ;; image part never grows one mid-session, and a model that cannot read
                     ;; pixels does not learn to.
                     excluded @image-blind-providers
                     excluded-models (blind-model-names)
                     round 0
                     done {}]

                    (if (or (empty? pairs)
                            ;; Bounded by the FLEET, not by a fixed number: every round removes
                            ;; at least one provider or one model, and a bigger fleet is exactly
                            ;; the case where a broken family sits in front of a working pair of
                            ;; eyes.
                            (>= (long round)
                                (min (long MAX_DESCRIBE_ROUNDS) (long (fleet-offers router))))
                            ;; Every provider in the fleet already broke: there is nobody left
                            ;; to offer the image to, and svar would only throw locally.
                            (empty? (remove #(contains? excluded (:id %)) (:providers router)))
                            (<= (- (long deadline-at) (System/currentTimeMillis)) 0))
                      done
                      (let
                        [outcome (describe-round router
                                                 (describe-routing excluded excluded-models)
                                                 context
                                                 deadline-at
                                                 (:name model)
                                                 pairs)
                         learned (into #{} (remove excluded) (:broken outcome))
                         learned-models (into #{} (remove excluded-models) (:blinded outcome))
                         described (merge done (:done outcome))]

                        ;; Nothing NEW is out: another round would call the same provider with
                        ;; the same model and the same result, so stop and let the caller
                        ;; degrade.
                        (if (or (seq learned) (seq learned-models))
                          (recur (:failed outcome)
                                 (into excluded learned)
                                 (into excluded-models learned-models)
                                 (inc (long round))
                                 described)
                          described))))
         ;; Keyed by payload, not position: every entry sharing a digest with a
         ;; described one gets the same text, cached once.
         by-digest (into {}
                         (keep (fn [{:keys [idx key]}]
                                 (when-let [description (get by-index idx)]
                                   [key (cache-put! key description)])))
                         pending)]

        (mapv (fn [{:keys [key cached]}]
                (or cached (get by-digest key)))
              entries)))))

(defn describe-attachments
  "Wire raw user attachments through the send-time image gate and describe what
   survives it, keyed by the LABEL the prompt manifest uses for the same row.

   Returns `{label {:text … :model …}}` — empty when the fallback is off, when no
   configured model can see, or when nothing describable came out of the gate.
   Human-only rows never reach the describer: `wire-images` skips them first, and
   bytes the caller deliberately kept off the wire are not this side-channel's
   business either.

   An AMBIGUOUS label (two rows the manifest names the same) is dropped before the
   call. The map can hold one text per label, so the second row would silently
   inherit the first row's report — and an agent reading a description under the
   wrong picture testifies to something nobody saw."
  [router context attachments]
  (let
    [wired
     (:attached (attachments/wire-images attachments))

     label-counts
     (frequencies (map attachments/image-label wired))

     addressable
     (filterv #(= 1 (get label-counts (attachments/image-label %))) wired)

     descriptions
     (describe-images router context addressable)]

    (if (seq descriptions)
      (into {}
            (keep identity)
            (map (fn [image description]
                   (when description [(attachments/image-label image) description]))
                 addressable
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
