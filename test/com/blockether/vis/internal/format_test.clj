(ns com.blockether.vis.internal.format-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.format :as fmt]
            [lazytest.core :refer [defdescribe expect it]]))

;; =============================================================================
;; strip-def-docstrings — channel-side rendering helper that removes the
;; mandatory docstring slot from `(def NAME "doc" …)` shapes so human
;; readers do not see the model-forced doc noise. Persisted source
;; (with docstring) stays intact; this is a render-time transform.
;; =============================================================================

(defn- normalize-ws
  "Drop trailing per-line whitespace + collapse runs of blank lines so
   docstring-stripped output can be compared with a stable expected
   string regardless of how zprint normalizes spacing."
  [s]
  (-> s
      (str/replace #"[ \t]+\n" "\n")
      (str/replace #"\n{2,}" "\n")
      str/trim))

(defdescribe humanize-fact-key-test
             (it "renders turn_<N> as Turn <N>, capitalizing other keys"
                 ;; Fact keys are model-authored strings (strings-only boundary).
                 (expect (= "Turn 1" (fmt/humanize-fact-key "turn_1")))
                 (expect (= "Turn 12" (fmt/humanize-fact-key "turn_12")))
                 (expect (= "Turn 3" (fmt/humanize-fact-key "turn_3")))
                 ;; non-turn keys: first letter capitalized, rest untouched
                 (expect (= "Oauth" (fmt/humanize-fact-key "oauth")))
                 (expect (= "T3/auth" (fmt/humanize-fact-key "t3/auth")))
                 ;; only an EXACT turn_<digits> match humanizes — no partial/embedded match
                 (expect (= "Turn 1 i2 x" (fmt/humanize-fact-key "turn_1_i2_x")))
                 (expect (= "My turn 1" (fmt/humanize-fact-key "my_turn_1")))))

(defdescribe
  format-tokens-test
  (it "omits zero cached input tokens"
      (expect (= "tok 100→20" (fmt/format-tokens {:input 100 :output 20 :cached 0})))
      (expect (= "tok 100→20" (fmt/format-tokens {:input 100 :output 20}))))
  (it "shows positive cached input tokens"
      (expect (= "tok 100→20 (cached 70)" (fmt/format-tokens {:input 100 :output 20 :cached 70})))
      (expect (= "tok 0→20 (cached 70)" (fmt/format-tokens {:output 20 :cached-input 70}))))
  (it "shows positive cache write tokens"
      (expect (= "tok 112→69 (cache-write 8777)"
                 (fmt/format-tokens {:input 112 :output 69 :cache-created 8777})))
      (expect (= "tok 112→69 (cached 70, cache-write 8777)"
                 (fmt/format-tokens {:input 112 :output 69 :cached 70 :cache-created 8777})))))

;; =============================================================================
;; Shared humanized turn-summary line — the SAME formatter the CLI bracket, TUI
;; bubble footer, and Telegram tagline all render. `meta-summary-line` is the
;; clean main line; `meta-fallback-note` is the routing note the TUI floats on a
;; second row and `format-meta-line` folds inline.
;; =============================================================================

(def ^:private normal-result
  {:llm-actual {:provider :openai :model "gpt-4o"}
   :tokens {:input 11461 :output 35 :cached 4096}
   :cost {:total-cost 0.006954}
   :duration-ms 4900
   :iteration-count 3})

(def ^:private fallback-result
  {:llm-actual {:provider :openai :model "gpt-4o"}
   :llm-selected {:provider :blockether :model "glm-5.1"}
   :llm-fallback? true
   :llm-routing-trace [{:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-fallback :status 429}]
   :tokens {:input 11461 :output 35 :cached 4096}
   :cost {:total-cost 0.006954}
   :duration-ms 4900})

(defdescribe meta-summary-line-test
             (it "humanizes tokens + cost, keeps cache, drops iterations"
                 (expect (= "openai/gpt-4o  ·  11.5k→35 (cached 4.1k)  ·  ~$0.0070  ·  4.9s"
                            (fmt/meta-summary-line normal-result))))
             (it "suppresses zero-usage / zero-cost slots — no \"0→0\", no \"$0\""
                 (expect (= "openai/gpt-4o  ·  4.9s"
                            (fmt/meta-summary-line {:provider :openai
                                                    :model "gpt-4o"
                                                    :tokens {:input 0 :output 0}
                                                    :cost 0
                                                    :duration-ms 4900}))))
             (it "keeps sub-cent costs honest (no round-to-zero)"
                 (expect (= "glm-5.1  ·  500→12  ·  ~$0.000020  ·  800ms"
                            (fmt/meta-summary-line {:model "glm-5.1"
                                                    :tokens {:input 500 :output 12}
                                                    :cost {:total-cost 0.00002}
                                                    :duration-ms 800}))))
             (it "carries the model that ANSWERED, not the one that bailed"
                 (expect (str/starts-with? (fmt/meta-summary-line fallback-result)
                                           "openai/gpt-4o"))))

(defdescribe meta-fallback-note-test
             (it "tells the routing story: from <selected> — <status>, retried N×"
                 (expect (= "↳ from blockether/glm-5.1 — 429, retried 3×"
                            (fmt/meta-fallback-note fallback-result))))
             (it "is nil when there was no fallback"
                 (expect (nil? (fmt/meta-fallback-note normal-result)))))

(defdescribe format-meta-line-test
             (it "is identical to meta-summary-line when there's no fallback"
                 (expect (= (fmt/meta-summary-line normal-result)
                            (fmt/format-meta-line normal-result))))
             (it "folds the fallback note inline for single-line surfaces (CLI/Telegram)"
                 (expect (= (str (fmt/meta-summary-line fallback-result)
                                 fmt/meta-separator
                                 (fmt/meta-fallback-note fallback-result))
                            (fmt/format-meta-line fallback-result)))))

