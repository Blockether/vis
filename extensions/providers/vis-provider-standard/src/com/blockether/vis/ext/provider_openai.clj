(ns com.blockether.vis.ext.provider-openai
  "OpenAI provider preset extension. API keys are configured by channels."
  (:require [com.blockether.vis.core :as vis]))

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-openai
     :ext/doc       "OpenAI API-key provider preset."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers [{:provider/id     :openai
                      :provider/label  "OpenAI"
                      :provider/preset {:default-models ["gpt-5" "gpt-5-mini" "gpt-4o" "gpt-4o-mini" "o3-mini"]}}]}))
