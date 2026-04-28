(ns com.blockether.vis-loop.loop.runtime.conversation.environment.query.runtime
  "Runtime settings shared by the query and iteration engines.")

;; =============================================================================
;; Eval timeout
;; =============================================================================

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in the SCI sandbox."
  120000)

(def MIN_EVAL_TIMEOUT_MS
  "Floor for :eval-timeout-ms. 3 s gives filesystem tools (grep, list-dir)
   headroom on medium-sized repos. Below about 1 s nearly every grep timed out
   at the race boundary; 3 s leaves comfortable margin without masking
   genuine infinite loops."
  3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms to prevent runaway SCI futures.
   30 minutes — anything longer is a bug, not a feature."
  (* 30 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for SCI code evaluation. Bound per query!
   call via :eval-timeout-ms. Nested queries inherit the outer binding.
   Clamped at the query API boundary to
   [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  DEFAULT_EVAL_TIMEOUT_MS)

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS].
   `candidate` may be nil — callers should resolve the fallback before calling.
   Accepts any integer, coerces to long. Prevents runaway SCI futures from
   absurdly high values and sub-second timeouts from absurdly low values."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

;; =============================================================================
;; Concurrency settings
;; =============================================================================

(def DEFAULT_CONCURRENCY
  "Default concurrency settings. Applied when :concurrency is absent from query!."
  {:max-parallel-llm 8
   :http-timeout-ms  20000})

(def ^:dynamic *concurrency*
  "Merged concurrency settings for the current query! process."
  DEFAULT_CONCURRENCY)

;; =============================================================================
;; RLM debug context
;; =============================================================================

(def ^:dynamic *rlm-context*
  "Dynamic context for RLM debug logging. Bind with
   {:rlm-debug? true :rlm-phase :phase-name :rlm-environment-id \"...\"}."
  nil)
