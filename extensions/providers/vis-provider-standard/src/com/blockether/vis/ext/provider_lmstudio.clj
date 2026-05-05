(ns com.blockether.vis.ext.provider-lmstudio
  "LM Studio local provider preset extension."
  (:require [com.blockether.vis.core :as vis]))

(defn- status []
  {:authenticated? false
   :provider-id :lmstudio
   :source :local
   :base-url "http://localhost:1234/v1"})

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-lmstudio
     :ext/doc       "LM Studio local OpenAI-compatible provider preset."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers [{:provider/id        :lmstudio
                      :provider/label     "LM Studio"
                      :provider/preset    {:base-url "http://localhost:1234/v1"}
                      :provider/status-fn #'status}]}))
