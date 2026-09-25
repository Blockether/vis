(ns com.blockether.vis.internal.provider.catalog
  "Provider catalog: the one place Vis reads provider metadata.

   Svar's public catalog supplies the defaults - base URLs, dialects, the
   placeholder keys of local presets, rate limits, model filters and pricing.
   A registered provider extension owns its label, preset and transport
   overrides, and those win over svar's defaults. Config, limits, the gateway
   and the turn loop read provider facts here instead of merging svar's table
   themselves."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.extension.registry :as registry]))

;; Presets withdrawn from Vis: their stale catalog keys must never return as
;; pickable rows. Copilot needs no entry - svar ships ONE `:github-copilot`
;; id, and a seat tier is what the signed-in account reports.
(def ^:private removed-provider-ids #{:blockether :github-models})

(def ^:private PRESET_ORDER
  "Stable display order in the 'Add Provider' picker. Most-likely-used
   first. Anything not in this vec lands at the end."
  [:openai :anthropic :anthropic-coding-plan :openai-codex :github-copilot :zai :zai-coding-plan
   :alibaba-coding-plan :alibaba-token-plan :openrouter :ollama :lmstudio])

(defn- svar-defaults
  "Svar's catalog defaults for a provider id, or nil."
  [pid]
  (get svar/KNOWN_PROVIDERS pid))

(defn- registered-metadata
  "Provider-owned preset metadata. First-party provider extensions put
   labels, base URLs, default models, and transport overrides here so
   internal config stays provider-agnostic."
  [pid]
  (when-let [provider (registry/provider-by-id pid)]
    (merge (:provider/preset provider)
           (when-let [label (:provider/label provider)]
             {:label label}))))

(defn label
  "The label a registered provider extension gives `pid`, or nil."
  [pid]
  (:label (registered-metadata pid)))

(defn base-url
  "Base URL for a provider id: provider extension first, svar catalog last."
  [pid]
  (or (:base-url (registered-metadata pid)) (:base-url (svar-defaults pid))))

(defn placeholder-api-key
  "The placeholder key svar's catalog ships for a local no-auth preset such as
   Ollama or LM Studio, or nil. Cloud presets have none."
  [pid]
  (:api-key (svar-defaults pid)))

(defn static-limits
  "Static request limits from svar's catalog as `{:rpm n :tpm n}`, each key
   present only when the catalog knows it."
  [pid]
  (let [known (svar-defaults pid)]
    (cond-> {}
      (some? (:rpm known))
      (assoc :rpm (long (:rpm known)))

      (some? (:tpm known))
      (assoc :tpm (long (:tpm known))))))

(defn template
  "Preset descriptor for a provider id, merged from a provider
   extension's metadata and svar's catalog. Returns nil for unknown or
   intentionally removed ids."
  [pid]
  (when-not (contains? removed-provider-ids pid)
    (let [provider-md
          (registered-metadata pid)

          svar-md
          (svar-defaults pid)]

      (when (or provider-md svar-md (registry/provider-by-id pid))
        (cond-> {:id pid}
          (:label provider-md)
          (assoc :label (:label provider-md))

          (base-url pid)
          (assoc :base-url (base-url pid))

          (or (:api-style provider-md) (:api-style svar-md))
          (assoc :api-style (or (:api-style provider-md) (:api-style svar-md)))

          (:default-models provider-md)
          (assoc :default-models (:default-models provider-md))

          (:responses-path provider-md)
          (assoc :responses-path (:responses-path provider-md))

          (:llm-headers provider-md)
          (assoc :llm-headers (:llm-headers provider-md))

          (:extra-body provider-md)
          (assoc :extra-body (:extra-body provider-md))

          (:network provider-md)
          (assoc :network (:network provider-md))

          (:is-hidden provider-md)
          (assoc :is-hidden true))))))

(defn presets
  "All known provider presets, sorted for the 'Add Provider' picker."
  []
  (let [order-rank
        (zipmap PRESET_ORDER (range))

        ids
        (into #{}
              (concat (keys svar/KNOWN_PROVIDERS)
                      (map :provider/id (registry/registered-providers))))]

    (->> ids
         (remove removed-provider-ids)
         (keep template)
         (remove :is-hidden)
         ;; Drop presets with no human label. A label is only set when a vis
         ;; provider extension is registered for the id; svar `KNOWN_PROVIDERS`
         ;; keys with no matching extension (e.g. :zai-coding) would
         ;; otherwise render as blank, selectable rows after
         ;; the last named preset in the "Add Provider" picker — and the TUI has
         ;; no handling for them anyway.
         (remove #(str/blank? (:label %)))
         (sort-by #(or (order-rank (:id %)) Long/MAX_VALUE))
         vec)))

(defn model-visible?
  "True when svar's provider-scoped model filters allow this model id."
  [provider-id model-id]
  (boolean (svar/provider-model-visible? provider-id model-id)))

(defn model-metadata
  "Metadata for one model as `provider-id` serves it - capabilities, pricing and
   context limits - without building a router. `model` is a map with at least
   `:name`."
  [provider-id model]
  (svar/provider-model-metadata provider-id model))

(defn normalize-models
  "The models of one provider entry, normalized the way svar's router does:
   catalog defaults, provider-scoped pricing and context limits. Builds no
   router and resolves no credentials."
  [priority provider]
  (:models (svar/normalize-provider priority provider)))

(defn model-pricing
  "Price table entry (USD per million tokens) for a model name - `{:input
   :output :cache-read :cached-input …}` - or nil when the model is not priced."
  [model]
  (when model (get svar/MODEL_PRICING (str model))))

(defn estimate-cost
  "Estimated USD cost of a request from its input and output token counts,
   priced from svar's model pricing table. `opts` carries the cached-input and
   cache-creation token counts."
  [model input-tokens output-tokens opts]
  (svar/estimate-cost model input-tokens output-tokens svar/MODEL_PRICING opts))
