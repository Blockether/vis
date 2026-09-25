(ns com.blockether.vis.test-provider-policies
  "The routing policies first-party providers declare, for engine tests that route
   through them without registering the real providers. `catalog-test` registers
   the real vendors and checks their declarations against `first-party`, so this
   fixture cannot drift from the source."
  (:require [com.blockether.vis.internal.provider.catalog :as catalog]))

(def ^:private claude-5-refusal-fallback
  {:models "(?i)claude-(opus|fable|sonnet)-5" :fallbacks ["claude-opus-4-8"]})

(def first-party
  "`{provider-id policy}` exactly as the first-party vendors declare it."
  {:openai {:preset-rank 0}
   :anthropic {:preset-rank 1 :prompt-cache {:ttl :1h} :refusal-fallback claude-5-refusal-fallback}
   :anthropic-coding-plan {:preset-rank 2
                           :title-rank 3
                           :prompt-cache {:ttl :1h}
                           :refusal-fallback claude-5-refusal-fallback}
   :openai-codex {:preset-rank 3
                  :title-rank 2
                  :prompt-cache {:strategy :server-continuation}
                  :fast-mode
                  {:turn-feature "codex_fast_mode" :service-tier "priority" :cost-multiplier 2.0}}
   :github-copilot {:preset-rank 4
                    :title-rank 4
                    :initiator-header "X-Initiator"
                    :adaptive-reasoning-models "(?i)claude"
                    :refusal-fallback claude-5-refusal-fallback}
   :zai {:preset-rank 5}
   :zai-coding-plan {:preset-rank 6 :title-rank 0}
   :alibaba-coding-plan {:preset-rank 7 :title-rank 1}
   :alibaba-token-plan {:preset-rank 8}
   :openrouter {:preset-rank 9}
   :ollama {:preset-rank 10}
   :lmstudio {:preset-rank 11}})

(defn with-policies
  "Run `f` with `policies` (default `first-party`) standing in for every
   registered provider's routing policy."
  ([f] (with-policies first-party f))
  ([policies f]
   (with-redefs [catalog/policy
                 (fn [pid]
                   (get policies
                        (some-> pid
                                keyword)
                        {}))

                 catalog/policies
                 (constantly policies)]

     (f))))
