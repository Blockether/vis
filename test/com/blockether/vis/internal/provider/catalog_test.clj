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
            [lazytest.core :refer [defdescribe expect it]]))

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

(defdescribe template-layers-provider-metadata-over-svar-defaults
             (it "template layers provider metadata over svar defaults"
                 (with-catalog
                   (fn []
                     ;; a registered provider's preset wins over svar's defaults
                     (expect (= {:id :anthropic
                                 :label "Anthropic"
                                 :base-url "https://anthropic.example.com"
                                 :api-style :anthropic}
                                (catalog/template :anthropic)))
                     ;; svar supplies what the provider leaves out
                     (expect (= {:id :openai :label "OpenAI" :base-url "https://api.example.com/v1"}
                                (catalog/template :openai)))
                     ;; an id only svar knows keeps svar's defaults and no label
                     (expect (= {:id :svar-only
                                 :base-url "https://svar.example.com/v1"
                                 :api-style :openai-compatible-chat}
                                (catalog/template :svar-only)))
                     ;; withdrawn and unknown ids have no template
                     (expect (nil? (catalog/template :github-models)))
                     (expect (nil? (catalog/template :unknown)))))))

(defdescribe
  model-allowlists-are-managed-declarations
  (it "normalizes string and mapped declarations without using catalog defaults"
      (with-redefs [registry/provider-by-id {:managed-fixture {:provider/is-managed true
                                                               :provider/preset
                                                               {:default-models
                                                                [" corp-large " {:name "corp-small"}
                                                                 "corp-large" ""]}}}]
        (expect (= #{"corp-large" "corp-small"} (catalog/model-allowlist :managed-fixture)))))
  (it "returns an empty allowlist for a missing or empty managed declaration"
      (doseq [preset [nil {} {:default-models []}]]
        (with-redefs [registry/provider-by-id {:openai {:provider/is-managed true
                                                        :provider/preset preset}}]
          (expect (= #{} (catalog/model-allowlist :openai))))))
  (it "leaves discovery unrestricted for ordinary and unregistered providers"
      (doseq [managed? [nil false]]
        (with-redefs [registry/provider-by-id {:ordinary-fixture
                                               {:provider/is-managed managed?
                                                :provider/preset {:default-models ["corp-large"]}}}]
          (expect (nil? (catalog/model-allowlist :ordinary-fixture)))
          (expect (nil? (catalog/model-allowlist :unknown)))))))

(defdescribe
  presets-list-labeled-visible-providers-in-picker-order
  (it "presets list labeled visible providers in picker order"
      (with-catalog
        (fn []
          (expect
            (= [:openai :anthropic :late] (mapv :id (catalog/presets)))
            "ranked presets first, unranked after, no hidden, withdrawn or unlabeled rows")))))

(defdescribe svar-defaults-answer-local-keys-and-static-limits
             (it "svar defaults answer local keys and static limits"
                 (with-catalog
                   (fn []
                     (expect (= "local-placeholder" (catalog/placeholder-api-key :local)))
                     (expect (nil? (catalog/placeholder-api-key :openai)))
                     (expect (= {:rpm 60 :tpm 1000} (catalog/static-limits :local)))
                     (expect (= {} (catalog/static-limits :openai)))
                     (expect (= "https://anthropic.example.com" (catalog/base-url :anthropic)))
                     (expect (= "http://127.0.0.1:1234/v1" (catalog/base-url :local)))
                     (expect (= "Anthropic" (catalog/label :anthropic)))
                     (expect (nil? (catalog/label :local)))))))

(defdescribe model-pricing-reads-svar-pricing-table
             (it "model pricing reads svar pricing table"
                 (let [[model price] (first svar/MODEL_PRICING)]
                   (expect (= price (catalog/model-pricing model)))
                   (expect (nil? (catalog/model-pricing nil)))
                   (expect (nil? (catalog/model-pricing "no-such-model-anywhere"))))))

(defdescribe
  engine-reads-svar-only-through-its-public-api
  (it "engine reads svar only through its public api"
      (let [offenders
            (for [file (file-seq (io/file "src"))
                  :when (and (.isFile ^java.io.File file)
                             (str/ends-with? (.getName ^java.io.File file) ".clj")
                             (str/includes? (slurp file) "com.blockether.svar.internal"))]

              (str file))]
        (expect (empty? offenders)
                "engine namespaces require com.blockether.svar.core, never svar internals"))))

(defdescribe provider-policy-is-owned-by-the-registered-provider
             (it "provider policy is owned by the registered provider"
                 (with-catalog
                   (fn []
                     ;; a provider's policy answers by keyword or string id
                     (expect (= {:preset-rank 1 :title-rank 0 :initiator-header "X-Initiator"}
                                (catalog/policy :anthropic)))
                     (expect (= (catalog/policy :anthropic) (catalog/policy "anthropic")))
                     ;; a provider without a policy and an unknown id both answer {}
                     (expect (= {} (catalog/policy :late)))
                     (expect (= {} (catalog/policy :unknown)))
                     (expect (= {} (catalog/policy nil)))
                     ;; policies lists only the providers that declare one
                     (expect (= #{:openai :anthropic} (set (keys (catalog/policies)))))
                     ;; the title chain follows :title-rank
                     (expect (= [:anthropic :openai] (catalog/title-providers)))))))

(defdescribe background-calls-carry-every-declared-initiator-header
             (it "background calls carry every declared initiator header"
                 (with-catalog
                   (fn []
                     (expect (= {"X-Initiator" "agent"} (catalog/agent-initiator-headers)))
                     (expect (= {:x 1 :llm-headers {"X-Initiator" "agent"}}
                                (catalog/with-agent-initiator {:x 1})))
                     (expect (= {:llm-headers {"X-Initiator" "user" "X-Other" "1"}}
                                (catalog/with-agent-initiator {:llm-headers {"X-Initiator" "user"
                                                                             "X-Other" "1"}}))
                             "a caller answering a person keeps its own initiator")
                     (with-redefs [registry/registered-providers (constantly [])]
                       (expect (= {} (catalog/agent-initiator-headers)))
                       (expect (= {:x 1} (catalog/with-agent-initiator {:x 1}))
                               "no provider reads an initiator header, so none is added"))))))

;; Engine tests route through `test-provider-policies/first-party` instead of
;; registering the real providers; this keeps that fixture equal to what the
;; first-party vendors declare.
(defdescribe first-party-policies-match-the-engine-test-fixture
             (it "first party policies match the engine test fixture"
                 (doseq [register! [openai/register! anthropic/register! openai-codex/register!
                                    github-copilot/register! zai/register! alibaba/register!
                                    openrouter/register! ollama/register! lmstudio/register!]]
                   (register!))
                 (expect (= policies/first-party
                            (into {}
                                  (map (fn [id]
                                         [id (catalog/policy id)]))
                                  (keys policies/first-party))))
                 (expect (= [:openai :anthropic :anthropic-coding-plan :openai-codex :github-copilot
                             :zai :zai-coding-plan :alibaba-coding-plan :alibaba-token-plan
                             :openrouter :ollama :lmstudio]
                            (filterv (set (keys policies/first-party))
                              (mapv :id (catalog/presets))))
                         "the Add-provider picker keeps its first-party order")))

(defdescribe first-party-presets-spell-request-bodies-with-json-names
             (it "first party presets spell request bodies with json names"
                 ;; vis.yml, gateway clients and Python extensions spell `extra_body` members as JSON
                 ;; names. A keyword preset member would sit next to the configured member instead
                 ;; of being replaced by it.
                 (doseq [register! [openai/register! anthropic/register! openai-codex/register!
                                    github-copilot/register! zai/register! alibaba/register!
                                    openrouter/register! ollama/register! lmstudio/register!]]
                   (register!))
                 (expect (seq (:extra-body (catalog/template :lmstudio))))
                 (doseq [id
                         (map :id (catalog/presets))

                         :let [extra-body
                               (:extra-body (catalog/template id))]
                         :when extra-body]

                   (expect (every? string?
                                   (mapcat keys (filter map? (tree-seq coll? seq extra-body))))
                           (str id " spells its preset extra-body members as JSON names")))))
