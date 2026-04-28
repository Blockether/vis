(ns com.blockether.vis-loop.providers
  "Cross-channel provider state.

   Single source of truth for the active LLM provider config used by
   every surface (TUI, Telegram, CLI). The active config lives on
   disk (`~/.vis/config.edn`, owned by `vis-loop.config`) and is
   mirrored in the in-memory `active-config` atom for fast reads.
   Every mutation goes through `set-provider!` / `remove-provider!`,
   which writes to disk AND updates the atom AND rebuilds the global
   router AND reseats it on every cached env — skipping any of those
   four would let a long-lived env (TUI session, Telegram bot) keep
   talking to the previous model.

   Lives in vis-loop (not in some `channels/` directory under it,
   not in vis-cli) because changing the provider IS a runtime
   concern: it tears down and rebuilds the router, and refreshes
   live conversation envs."
  (:require [com.blockether.vis-loop.config :as config]
            [com.blockether.vis-loop.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis-loop.loop.runtime.conversation.environment.query.core :as query-core]
            [taoensso.telemere :as tel]))

(defonce ^:private active-config (atom nil))

(defn current-config
  "Return the current provider config. Loads from disk on first call."
  []
  (or @active-config
    (let [cfg (config/load-config)]
      (reset! active-config cfg)
      cfg)))

(defn set-provider!
  "Set the single active provider config. Persists to disk and updates in-memory state.
   `provider` is a svar-native provider map {:id :base-url :api-key :models [...]}.
   Replaces any existing provider with the same :id, or adds if new."
  [provider]
  (let [cfg   (or (current-config) {:providers []})
        pid   (:id provider)
        provs (vec (:providers cfg))
        ;; Replace existing or append
        updated (let [idx (some (fn [[i p]] (when (= (:id p) pid) i))
                            (map-indexed vector provs))]
                  (if idx
                    (assoc provs idx provider)
                    (conj provs provider)))
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
    (reset! active-config new-cfg)
    ;; Rebuild the global router AND reseat it on every cached env.
    ;; Skipping the second step lets long-lived envs (TUI session,
    ;; Telegram bot) keep talking to the previous model even though
    ;; the singleton already advertises the new one.
    (try (let [r (query-core/rebuild-router! new-cfg)]
           (conversations/refresh-cached-routers! r))
      (catch Exception e
        (tel/log! {:level :warn :data {:error (ex-message e)}}
          "Failed to rebuild router after provider change")))
    new-cfg))

(defn remove-provider!
  "Remove a provider by :id. Persists to disk."
  [provider-id]
  (let [cfg     (or (current-config) {:providers []})
        updated (vec (remove #(= (:id %) provider-id) (:providers cfg)))
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
    (reset! active-config new-cfg)
    new-cfg))

(defn active-provider
  "Return the first (primary) provider from config, or nil."
  []
  (first (:providers (current-config))))

(defn active-model
  "Return the primary model name string, or nil."
  []
  (some-> (active-provider) :models first config/model-name))

(defn provider-ids
  "Return set of configured provider :id keywords."
  []
  (into #{} (map :id) (:providers (or (current-config) {:providers []}))))

(defn has-provider?
  "True if a provider with the given :id is already configured."
  [provider-id]
  (contains? (provider-ids) provider-id))

(defn reload-config!
  "Force reload config from disk."
  []
  (reset! active-config (config/load-config)))
