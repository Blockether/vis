(ns com.blockether.vis.internal.provider.catalog-test
  "The provider catalog merges svar's public defaults with the metadata each
   registered provider owns, and it is the engine's only view of svar's catalog."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.vendor.alibaba :as alibaba]
            [com.blockether.vis.internal.provider.vendor.anthropic :as anthropic]
            [com.blockether.vis.internal.provider.vendor.github-copilot :as github-copilot]
            [com.blockether.vis.internal.provider.vendor.lmstudio :as lmstudio]
            [com.blockether.vis.internal.provider.vendor.ollama :as ollama]
            [com.blockether.vis.internal.provider.vendor.openai :as openai]
            [com.blockether.vis.internal.provider.vendor.openai-codex :as openai-codex]
            [com.blockether.vis.internal.provider.vendor.openrouter :as openrouter]
            [com.blockether.vis.internal.provider.vendor.zai :as zai]
            [com.blockether.vis.test-provider-policies :as policies]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(def ^:private svar-defaults
  {:svar-only {:base-url "https://svar.example.com/v1" :api-style :openai-compatible-chat}
   :local {:base-url "http://127.0.0.1:1234/v1" :api-key "local-placeholder" :rpm 60 :tpm 1000.0}
   :openai {:base-url "https://api.example.com/v1"}})

(def ^:private providers
  {:openai
   {:provider/id :openai :provider/label "OpenAI" :provider/policy {:preset-rank 0 :title-rank 2}}
   :anthropic {:provider/id :anthropic
               :provider/label "Anthropic"
               :provider/preset {:base-url "https://anthropic.example.com" :api-style :anthropic}
               :provider/policy {:preset-rank 1 :title-rank 0 :initiator-header "X-Initiator"}}
   :late {:provider/id :late :provider/label "Late"}
   :hidden {:provider/id :hidden :provider/label "Hidden" :provider/preset {:is-hidden true}}
   :github-models {:provider/id :github-models :provider/label "GitHub Models"}})

(defn- with-catalog
  [f]
  (with-redefs [svar/KNOWN_PROVIDERS
                svar-defaults

                registry/provider-by-id
                providers

                registry/registered-providers
                (fn []
                  (vec (vals providers)))]

    (f)))

(deftest template-layers-provider-metadata-over-svar-defaults
  (with-catalog
    (fn []
      (testing "a registered provider's preset wins over svar's defaults"
        (is (= {:id :anthropic
                :label "Anthropic"
                :base-url "https://anthropic.example.com"
                :api-style :anthropic}
               (catalog/template :anthropic))))
      (testing "svar supplies what the provider leaves out"
        (is (= {:id :openai :label "OpenAI" :base-url "https://api.example.com/v1"}
               (catalog/template :openai))))
      (testing "an id only svar knows keeps svar's defaults and no label"
        (is (= {:id :svar-only
                :base-url "https://svar.example.com/v1"
                :api-style :openai-compatible-chat}
               (catalog/template :svar-only))))
      (testing "withdrawn and unknown ids have no template"
        (is (nil? (catalog/template :github-models)))
        (is (nil? (catalog/template :unknown)))))))

(deftest presets-list-labeled-visible-providers-in-picker-order
  (with-catalog
    (fn []
      (is (= [:openai :anthropic :late] (mapv :id (catalog/presets)))
          "ranked presets first, unranked after, no hidden, withdrawn or unlabeled rows"))))

(deftest svar-defaults-answer-local-keys-and-static-limits
  (with-catalog (fn []
                  (is (= "local-placeholder" (catalog/placeholder-api-key :local)))
                  (is (nil? (catalog/placeholder-api-key :openai)))
                  (is (= {:rpm 60 :tpm 1000} (catalog/static-limits :local)))
                  (is (= {} (catalog/static-limits :openai)))
                  (is (= "https://anthropic.example.com" (catalog/base-url :anthropic)))
                  (is (= "http://127.0.0.1:1234/v1" (catalog/base-url :local)))
                  (is (= "Anthropic" (catalog/label :anthropic)))
                  (is (nil? (catalog/label :local))))))

(deftest model-pricing-reads-svar-pricing-table
  (let [[model price] (first svar/MODEL_PRICING)]
    (is (= price (catalog/model-pricing model)))
    (is (nil? (catalog/model-pricing nil)))
    (is (nil? (catalog/model-pricing "no-such-model-anywhere")))))

(deftest engine-reads-svar-only-through-its-public-api
  (let [offenders (for [file (file-seq (io/file "src"))
                        :when (and (.isFile ^java.io.File file)
                                   (str/ends-with? (.getName ^java.io.File file) ".clj")
                                   (str/includes? (slurp file) "com.blockether.svar.internal"))]

                    (str file))]
    (is (empty? offenders)
        "engine namespaces require com.blockether.svar.core, never svar internals")))

(deftest provider-policy-is-owned-by-the-registered-provider
  (with-catalog (fn []
                  (testing "a provider's policy answers by keyword or string id"
                    (is (= {:preset-rank 1 :title-rank 0 :initiator-header "X-Initiator"}
                           (catalog/policy :anthropic)))
                    (is (= (catalog/policy :anthropic) (catalog/policy "anthropic"))))
                  (testing "a provider without a policy and an unknown id both answer {}"
                    (is (= {} (catalog/policy :late)))
                    (is (= {} (catalog/policy :unknown)))
                    (is (= {} (catalog/policy nil))))
                  (testing "policies lists only the providers that declare one"
                    (is (= #{:openai :anthropic} (set (keys (catalog/policies))))))
                  (testing "the title chain follows :title-rank"
                    (is (= [:anthropic :openai] (catalog/title-providers)))))))

(deftest background-calls-carry-every-declared-initiator-header
  (with-catalog
    (fn []
      (is (= {"X-Initiator" "agent"} (catalog/agent-initiator-headers)))
      (is (= {:x 1 :llm-headers {"X-Initiator" "agent"}} (catalog/with-agent-initiator {:x 1})))
      (is (= {:llm-headers {"X-Initiator" "user" "X-Other" "1"}}
             (catalog/with-agent-initiator {:llm-headers {"X-Initiator" "user" "X-Other" "1"}}))
          "a caller answering a person keeps its own initiator")
      (with-redefs [registry/registered-providers (constantly [])]
        (is (= {} (catalog/agent-initiator-headers)))
        (is (= {:x 1} (catalog/with-agent-initiator {:x 1}))
            "no provider reads an initiator header, so none is added")))))

;; Engine tests route through `test-provider-policies/first-party` instead of
;; registering the real providers; this keeps that fixture equal to what the
;; first-party vendors declare.
(deftest first-party-policies-match-the-engine-test-fixture
  (doseq [register! [openai/register! anthropic/register! openai-codex/register!
                     github-copilot/register! zai/register! alibaba/register! openrouter/register!
                     ollama/register! lmstudio/register!]]
    (register!))
  (is (= policies/first-party
         (into {}
               (map (fn [id]
                      [id (catalog/policy id)]))
               (keys policies/first-party))))
  (is (= [:openai :anthropic :anthropic-coding-plan :openai-codex :github-copilot :zai
          :zai-coding-plan :alibaba-coding-plan :alibaba-token-plan :openrouter :ollama :lmstudio]
         (filterv (set (keys policies/first-party)) (mapv :id (catalog/presets))))
      "the Add-provider picker keeps its first-party order"))
