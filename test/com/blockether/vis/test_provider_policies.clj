(ns com.blockether.vis.test-provider-policies
  "The routing policies first-party providers declare, for engine tests that route
   through them without registering the real providers. `catalog-test` registers
   the real vendors and checks their declarations against `first-party`, so this
   fixture cannot drift from the source."
  (:require [com.blockether.vis.internal.provider.catalog :as catalog]))

(def first-party
  "`{provider-id policy}` exactly as the first-party vendors declare it."
  {:openai {:preset-rank 0}
   :anthropic {:preset-rank 1}
   :anthropic-coding-plan {:preset-rank 2 :title-rank 3}
   :openai-codex {:preset-rank 3 :title-rank 2 :fast-mode {:turn-feature "codex_fast_mode"}}
   :github-copilot {:preset-rank 4
                    :title-rank 4
                    :initiator-header "X-Initiator"
                    :adaptive-reasoning-models "(?i)claude"}
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
