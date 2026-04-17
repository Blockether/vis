(ns com.blockether.vis.loop.conversations.shared
  "Reusable helpers for RLM conversation lifecycle."
  (:require [com.blockether.vis.languages.commons.edit :as edit]
            [com.blockether.vis.languages.commons.list :as list]
            [com.blockether.vis.languages.commons.read :as read]
            [com.blockether.vis.languages.commons.write :as write]
            [com.blockether.vis.core :as core]))

;; conv-id (string) -> {:env env :lock Object}
(defonce cache (atom {}))

(def base-tools
  "File-system tools registered on every conversation env (all channels).
   Shell is intentionally NOT here — Telegram and any other untrusted caller
   would otherwise have arbitrary code execution on the host. If a caller
   truly needs shell, wire a scoped tool into the agent-def for that run."
  [read/tool-def
   write/tool-def
   edit/tool-def
   list/tool-def])

(defn register-base-tools!
  [env]
  (reduce (fn [e {:keys [sym fn] :as t}]
            (core/register-env-fn! e sym fn (dissoc t :sym :fn)))
    env
    base-tools))

(defn cache-env!
  [id env]
  (swap! cache assoc id {:env env :lock (Object.)})
  env)
