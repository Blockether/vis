(ns com.blockether.vis.channels.core
  "Cross-channel shared functions.

   This namespace provides the shared provider/config management layer
   used by TUI, web, CLI, and Telegram channels. Single source of truth
   for provider state — changes here are reflected everywhere."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core]
            [taoensso.telemere :as tel]))

;;; ── Provider state ─────────────────────────────────────────────────────────
;;
;; The active config is always in ~/.vis/config.edn (persisted) and mirrored
;; in this atom (in-memory for fast reads). Every mutation goes through
;; set-provider! which writes to disk AND updates the atom.

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
    ;; Rebuild the router so the current conversation picks up the change
    (try (query-core/rebuild-router! new-cfg)
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

;;; ── Lifecycle helpers ──────────────────────────────────────────────────────

(defn register-conversation-shutdown-hook!
  "Register a JVM shutdown hook that prints the conversation resume command.
   Safe to call multiple times — each call replaces the previous hook."
  [conversation-id]
  (let [hook (Thread. (fn []
                        (let [out config/original-stdout]
                          (.println out "")
                          (.println out (str "  vis chat --conversation-id " conversation-id))
                          (.println out "")
                          (.flush out))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

;;; ── Streaming helpers ─────────────────────────────────────────────────────
;;
;; Reusable across TUI, web, Telegram, CLI.
;; The on-chunk callback receives streaming chunks from svar's ask!:
;;   {:iteration N :thinking str :code [str] :done? bool}
;;
;; Thinking (reasoning) streams live as the LLM thinks.
;; Code/expressions arrive after the LLM finishes its response.
;; :done? true marks the final chunk of an iteration.

(defn make-progress-tracker
  "Create a progress tracker for streaming iteration chunks.
   Returns {:on-chunk fn, :get-timeline fn}.

   `on-update` is called (on-update timeline chunk) on every chunk.
   Timeline is a vec of chunk maps, deduplicated by iteration."
  ([] (make-progress-tracker nil))
  ([{:keys [on-update]}]
   (let [timeline (atom {})  ;; iteration-num → latest chunk
         as-vec   #(mapv val (sort-by key %))]
     {:on-chunk  (fn [chunk]
                  (let [iter (:iteration chunk)
                        tl   (swap! timeline assoc iter chunk)]
                    (when on-update
                      (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))

(defn format-date
  "Format a java.util.Date as dd-MM-yyyy HH:mm in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))
