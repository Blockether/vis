(ns com.blockether.vis.ext.provider-lmstudio
  "LM Studio local provider preset extension."
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]))

(defn- status []
  {:authenticated? false
   :provider-id :lmstudio
   :source :local
   :base-url "http://localhost:1234/v1"})

(defn- enrich-models
  "Resolve real context windows for the provider's models from LM Studio's
   native model endpoint. LM Studio's OpenAI-compatible `/v1/models` omits
   context length, so without this every model falls back to svar's
   conservative default; `svar/models!` reads the native `/api/v0/models`
   (handled by svar's `:lmstudio` profile) which reports the real window.

   Contract (`:provider/enrich-models-fn`): `(svar-provider router-opts) ->
   models-vec`. Models that already carry an explicit `:context` (user
   override) are left untouched. On any failure returns the input models
   unchanged so the router still builds."
  [svar-provider router-opts]
  (let [models (:models svar-provider)]
    (if (every? :context models)
      models
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
          (mapv (fn [m]
                  (if (:context m)
                    m
                    (merge m (get by-name (:name m)))))
            models))
        (catch Throwable _ models)))))

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
