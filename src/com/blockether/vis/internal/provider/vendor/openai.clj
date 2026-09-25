(ns com.blockether.vis.internal.provider.vendor.openai
  "OpenAI provider preset extension. API keys are configured by channels."
  (:require [com.blockether.vis.extension :as ext]
            [com.blockether.svar.core :as svar]))

(defn register!
  []
  (ext/register-extension! (ext/extension {:ext/name "provider-openai"
                                           :ext/description "OpenAI API-key provider preset."
                                           :ext/version "0.1.0"
                                           :ext/author "Blockether"
                                           :ext/owner "vis"
                                           :ext/license "Apache-2.0"
                                           :ext/providers [{:provider/id :openai
                                                            :provider/label "OpenAI"
                                                            :provider/preset
                                                            {:default-models
                                                             (svar/provider-default-models :openai)}
                                                            :provider/policy {:preset-rank 0}}]})))
