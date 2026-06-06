(ns com.blockether.vis.ext.provider-lmstudio
  "LM Studio local provider preset extension."
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]))

(defn- status []
  {:authenticated? false
   :provider-id :lmstudio
   :source :local
   :base-url "http://localhost:1234/v1"})

(def ^:private model-defaults
  "Defaults applied to every LM Studio model. LM Studio's API reports
   `tool_use` capability but NO reasoning signal, yet local models served here
   are commonly reasoning-capable — and svar's `ask-code!` refuses to route to
   a model lacking `:reasoning?` whenever a `:reasoning` arg is present. So we
   default local models reasoning-capable with `:reasoning-style :server-managed`:
   svar then injects NO reasoning params (see `reasoning-extra-body`), leaving
   the model's own chat template to decide whether to think. Harmless for
   non-reasoning models (they just answer normally); the only effect is routing
   eligibility. Anything explicitly set per model wins over these defaults."
  {:reasoning? true
   :reasoning-style :server-managed})

(defn- enrich-models
  "Resolve each model's real context window (and tool-use capability) from LM
   Studio's native model endpoint, and apply `model-defaults`. LM Studio's
   OpenAI-compatible `/v1/models` omits context length, so without this every
   model falls back to svar's conservative default; `svar/models!` reads the
   native `/api/v0/models` (handled by svar's `:lmstudio` profile) which reports
   the real window plus `tool_use`.

   Contract (`:provider/enrich-models-fn`): `(svar-provider router-opts) ->
   models-vec`. Precedence per key: explicit model value (config override) >
   detected (`:context`/`:tool-call?`) > `model-defaults`. Skips the network
   when every model already has an explicit `:context`; on any failure returns
   the defaulted models so the router still builds."
  [svar-provider router-opts]
  (let [models (:models svar-provider)
        ;; defaults first, model values win over them
        base   (mapv #(merge model-defaults %) models)]
    (if (every? :context base)
      base
      (try
        (let [probe   (cond-> svar-provider
                        (empty? models) (assoc :models [{:name "probe"}]))
              router  (svar/make-router [probe] (or router-opts {}))
              by-name (into {}
                        (keep (fn [m]
                                (when-let [nm (or (:id m) (:name m))]
                                  (when (:context m)
                                    [nm (select-keys m [:context :tool-call?])]))))
                        (svar/models! router))]
          ;; detection fills gaps; explicit model values still win per key
          (mapv (fn [m] (merge (get by-name (:name m)) m)) base))
        (catch Throwable _ base)))))

(vis/register-extension!
  (vis/extension
    {:ext/name      "provider-lmstudio"
     :ext/description "LM Studio local OpenAI-compatible provider preset."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers [{:provider/id                :lmstudio
                      :provider/label             "LM Studio"
                      :provider/preset            {:base-url "http://localhost:1234/v1"}
                      :provider/status-fn         #'status
                      :provider/enrich-models-fn  #'enrich-models}]}))
