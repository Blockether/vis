(ns com.blockether.vis.internal.safe-guards
  "View-time clipping for the prompt-side rendering of CTX.

   Engine keeps full payloads in `:ctx-atom` and the DB; this namespace
   ONLY decides what fragment of each payload makes it into the
   rendered prompt the next provider call sees.

   Two layers, both deterministic, both lossless on disk:

     1. PER-ENTRY clip (form-result, fact-content).
        When a single value's `pr-str` weight crosses its limit, the
        renderer replaces it with a head + tail preview plus a handle:
          :vis/head    first N tokens of pr-str
          :vis/tail    last M tokens of pr-str
          :vis/size    total tokens
          :vis/full    recall call string (window the stored value)

     2. PER-TRAILER fold.
        When the WHOLE prompt total exceeds the budget, oldest
        trailer pins fold into one stub with :scope-start, :scope-end,
        :folded-pin-count, :note, :born, :vis/auto?. No semantic
        summary, no LLM call.

   Hard rules:
     * NEVER an LLM / companion call. Pure deterministic.
     * NEVER drop data. Fold leaves enough scope info that
       `(recall ...)` reaches every collapsed iter verbatim.
     * NEVER silent. Every guard fires a warning so the model knows.

   Decision authority stays with the model: it can issue its own
   `(summarize {:trailer ...})` to override the engine fold. Engine
   never strips data behind the model's back."
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.tokens :as tokens]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Budget thresholds (token-based via jtokkit cl100k_base approx)
;; =============================================================================

(def DEFAULT_PROMPT_BUDGET_TOKENS
  "Total prompt-token ceiling for a single provider call. Defaults to
   72% of a typical 200k-token window — leaves ~56k tokens of headroom
   for the model's own output plus margin for tokenizer disagreement
   (cl100k vs provider-native, ~10-30%)."
  144000)

(def ^:private MAX_FOLD_ROUNDS
  "Hard cap on fold round-trips per render. In practice one round is
   enough — fold takes the oldest batch in one shot."
  4)

(def ^:private FOLD_SAFETY_MARGIN_TOKENS
  "Each fold round frees AT LEAST (over-budget delta + this) tokens so
   a single long-tail iteration's growth does not immediately push the
   prompt back over budget."
  4000)

;; =============================================================================
;; Per-entry clip shape — shared by form-result / fact-content guards
;; =============================================================================

(def CLIP_HEAD_TOKENS
  "Tokens kept from the START of an over-limit value."
  120)

(def CLIP_TAIL_TOKENS
  "Tokens kept from the END of an over-limit value."
  60)

(defn- clip-pr-segment
  "Return a `pr-str` slice approximating `target-tokens` worth of
   tokens. Coarse char->token ratio (cl100k typical 3-4 chars/token);
   over-allocate chars then verify with `tokens/count-tokens` and
   shorten if needed. `from` is :head or :tail."
  [v target-tokens from]
  (let [s            (try (pr-str v) (catch Throwable _ ""))
        approx-chars (* target-tokens 4)
        cap-chars    (min (count s) approx-chars)]
    (if (zero? cap-chars)
      ""
      (case from
        :head (subs s 0 cap-chars)
        :tail (subs s (- (count s) cap-chars))))))

(defn clip-value
  "Build the head+tail stub for `v` when its `pr-str` weight (in
   tokens) exceeds `limit-tokens`. Returns the original value
   otherwise so call sites can use this as a pure
   `(cond-> v over-limit? clip)` helper.

   `full-call` is the literal string the model emits to recover the
   verbatim payload (e.g. a recall invocation).

   Always lossless on disk — head/tail clip is a render-time view, not
   a mutation of CTX or DB row."
  [v limit-tokens full-call]
  (let [size (tokens/count-pr-tokens v)]
    (if (<= size limit-tokens)
      v
      {:vis/head (clip-pr-segment v CLIP_HEAD_TOKENS :head)
       :vis/tail (clip-pr-segment v CLIP_TAIL_TOKENS :tail)
       :vis/size size
       :vis/full full-call})))

;; =============================================================================
;; Per-trailer fold — deterministic, oldest-first
;; =============================================================================

(defn- iter-ctx-msg
  "Wrap the rendered ctx string into the user-role message shape that
   the loop hands off to svar."
  [ctx-rendered]
  {:role "user" :content (or ctx-rendered "")})

(defn estimate-prompt-tokens
  "Sum the tokens for the full message vec that will go to the provider
   this iteration: stable system prompts + initial user message +
   per-iter ctx. Uses jtokkit cl100k_base — approximation only,
   ~10-30% margin per provider; precision is sufficient for fold
   triggers."
  [stable-msgs ctx-rendered]
  (+ (tokens/count-prompt-tokens (or stable-msgs []))
    (tokens/count-prompt-tokens [(iter-ctx-msg ctx-rendered)])))

