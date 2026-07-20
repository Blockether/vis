(ns com.blockether.vis.ext.provider-lmstudio
  "LM Studio local provider preset extension."
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]))

(defn- status
  []
  {:authenticated? false
   :provider-id :lmstudio
   :source :local
   :base-url "http://localhost:1234/v1"})

(defn- enrich-models
  "Resolve each model's real context window (and tool-use capability) from LM
   Studio's native model endpoint. LM Studio's OpenAI-compatible `/v1/models`
   omits context length, so without this every model falls back to svar's
   conservative default; `svar/models!` reads the native `/api/v0/models`
   (handled by svar's `:lmstudio` profile) which reports the real window plus
   `tool_use`.

   We deliberately do NOT infer `:reasoning?` — LM Studio's API exposes no
   reasoning signal, and since svar 0.7.4 routing no longer requires it, a
   non-reasoning model still routes (reasoning is best-effort).

   Contract (`:provider/enrich-models-fn`): `(svar-provider router-opts) ->
   models-vec`. Precedence per key: explicit model value (config override) >
   detected `:context`/`:tool-call?`. Skips the network when every model already
   has an explicit `:context`; on any failure returns the input models so the
   router still builds."
  [svar-provider router-opts]
  (let [models (:models svar-provider)]
    (if (every? :context models)
      models
      (try (let
             [probe (cond-> svar-provider
                      (empty? models)
                      (assoc :models [{:name "probe"}]))
              router (svar/make-router [probe] (or router-opts {}))
              by-name (into {}
                            (keep (fn [m]
                                    (when-let [nm (or (:id m) (:name m))]
                                      (when (:context m)
                                        [nm (select-keys m [:context :tool-call?])]))))
                            (svar/models! router))]

             ;; detection fills gaps; explicit model values still win per key
             (mapv (fn [m]
                     (merge (get by-name (:name m)) m))
                   models))
           (catch Throwable _ models)))))

(vis/register-extension!
  (vis/extension
    {:ext/name "provider-lmstudio"
     :ext/description "LM Studio local OpenAI-compatible provider preset."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/providers [{:provider/id :lmstudio
                      :provider/label "LM Studio"
                      :provider/preset
                      {:base-url "http://localhost:1234/v1"
                       ;; Sampler defaults for local LM Studio models.
                       ;; Qwen3/Qwen3.5 thinking models (and DeepSeek-R1
                       ;; distills) fall into endless reasoning loops under
                       ;; the default greedy sampler — emitting tens of
                       ;; thousands of `reasoning_content` chars and almost
                       ;; no answer (see LM Studio bug-tracker #1018, QwenLM
                       ;; #145). These are the vendor-recommended anti-loop
                       ;; params: temp 0.6 (DeepSeek's "avoid endless
                       ;; repetition" value), top_p/top_k/min_p per Qwen,
                       ;; and presence_penalty 1.5 to break repetition.
                       ;; Merged as the provider base layer in svar
                       ;; (router/inject-routed-params) — a per-turn
                       ;; `:extra-body` still overrides any of these.
                       :extra-body
                       {:temperature 0.6 :top_p 0.95 :top_k 20 :min_p 0.0 :presence_penalty 1.5}}
                      :provider/status-fn #'status
                      :provider/enrich-models-fn #'enrich-models}]}))
