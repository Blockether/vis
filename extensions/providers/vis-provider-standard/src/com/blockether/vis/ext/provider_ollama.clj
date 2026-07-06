(ns com.blockether.vis.ext.provider-ollama
  "Ollama local provider preset extension."
  (:require [com.blockether.vis.core :as vis]))

(defn- status
  []
  {:authenticated? false :provider-id :ollama :source :local :base-url "http://localhost:11434/v1"})

(vis/register-extension!
  (vis/extension {:ext/name "provider-ollama"
                  :ext/description "Ollama local OpenAI-compatible provider preset."
                  :ext/version "0.1.0"
                  :ext/author "Blockether"
                  :ext/owner "vis"
                  :ext/license "Apache-2.0"
                  :ext/providers [{:provider/id :ollama
                                   :provider/label "Ollama"
                                   :provider/preset {:base-url "http://localhost:11434/v1"}
                                   :provider/status-fn #'status}]}))
