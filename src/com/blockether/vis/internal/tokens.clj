(ns com.blockether.vis.internal.tokens
  "Approximate token counting for engine-side safe-guards and
   summarization triggers.

   Uses jtokkit cl100k_base BPE — OpenAI GPT-4 family tokenizer.
   Margin vs other providers (Anthropic Claude, Z.AI GLM, OpenAI o200k):
   typically 10-30%. Good enough for budget decisions like \"this fact
   :content is too big to inline\" or \"trailer total exceeds 60k —
   auto-summarise\"; NOT precise enough for billing claims.

   For exact provider-reported counts use `session_turn_iteration`'s
   `:input-tokens` / `:output-tokens` (post-response). This helper is
   PRE-response — engine renders, counts, decides to compact, then
   renders again.

   Encoding is a `delay` so the ~10MB jtokkit init pays only on first
   use. All calls reuse the singleton. Per-call cost: O(n) over UTF-8
   bytes of input."
  (:import
   [com.knuddels.jtokkit Encodings]
   [com.knuddels.jtokkit.api Encoding EncodingType]))

(def ^:private ^Encoding cl100k
  ;; `delay` to defer the ~10MB BPE rank table load until first use;
  ;; deref is thread-safe and cached for the lifetime of the JVM.
  (delay
    (.getEncoding (Encodings/newDefaultEncodingRegistry)
      EncodingType/CL100K_BASE)))

(defn count-tokens
  "Approximate token count for `s` under cl100k_base. Returns 0 for
   nil, blank, or non-string input. Safe to call frequently — the
   encoder is reused; per-call cost is O(n) over UTF-8 bytes.

   Use this for engine budget decisions; do NOT use for billing
   claims (provider tokenizer may differ by 10-30%)."
  ^long [s]
  (if (and (string? s) (not= "" s))
    (long (.countTokens ^Encoding @cl100k ^String s))
    0))

(defn count-pr-tokens
  "Token count of `(pr-str v)`. Wraps the pr-str + count-tokens combo
   used by every safe-guard call site (\"is this Clojure value too big
   to inline?\"). Returns 0 for nil — nil pr-strs to \"nil\" which is 1
   token, but for budget purposes a nil result IS the absence of value."
  ^long [v]
  (if (nil? v)
    0
    (count-tokens
      (try (pr-str v)
        (catch Throwable _ "")))))

(def MSG_OVERHEAD_TOKENS
  "Per-message wire overhead estimate for chat-completion / messages
   APIs: ~4 tokens for role + start/end delimiters. Cumulative when
   counting a whole `messages` vec."
  4)

(defn count-prompt-tokens
  "Total approximate tokens for a `messages` vec as it would be sent
   to a provider. Sums `count-tokens` over each `:content` plus a small
   per-message overhead.

   Used by the summarization trigger: before each provider call the engine
   measures the full prompt; if over budget it runs the cascade
   (trailer compact → facts compact → panic drop)."
  ^long [messages]
  (reduce
    (fn [^long acc msg]
      (let [c (:content msg)]
        (+ acc MSG_OVERHEAD_TOKENS
          (cond
            (string? c) (count-tokens c)
            (sequential? c)
            ;; multi-part content (e.g. Anthropic multimodal blocks);
            ;; sum text parts, ignore non-text approx
            (reduce
              (fn [^long pa part]
                (+ pa
                  (cond
                    (string? part) (count-tokens part)
                    (map? part) (count-tokens (or (:text part) ""))
                    :else 0)))
              0 c)
            :else 0))))
    0
    (or messages [])))
