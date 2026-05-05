(ns com.blockether.vis.ext.provider-anthropic
  "Anthropic provider preset extension. API keys are configured by channels."
  (:require [com.blockether.vis.core :as vis]))

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-anthropic
     :ext/doc       "Anthropic API-key provider preset."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers [{:provider/id     :anthropic
                      :provider/label  "Anthropic"
                      :provider/preset {:default-models ["claude-opus-4-6" "claude-sonnet-4-6" "claude-haiku-4-5"]}}]}))
