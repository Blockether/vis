(ns com.blockether.vis.internal.foundation.session-slashes
  "Declarative session-level slash commands shared by every channel.

   These are channel-agnostic: the engine dispatches them for the TUI, the
   web, and Telegram alike through the same `slash/dispatch` path, and each
   handler mutates state via the gateway so the change fans out everywhere.

     /rename <new title>   set this session's title

   `/rename` routes through `titling/set-title-with-broadcast!` — the single
   title mutation point."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.titling :as titling]))

(defn- err [msg & {:as extras}]
  (merge {:slash/status :error, :slash/title msg} extras))

(defn- handle-rename
  "`/rename <new title>` — set the current session's title. Reuses the
   gateway's single title mutation point."
  [ctx]
  (let [sid   (or (:session/id ctx) (:session-id ctx))
        db    (or (:db-info ctx) (:db ctx))
        atom* (:session-title-atom ctx)
        title (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? sid)   (err "Send a message first, then /rename <title> (session not ready yet)")
      (nil? title) (err "Name it: /rename <new title>")
      :else
      (do
        (titling/set-title-with-broadcast! db sid atom* title)
        {:slash/status :ok,
         :slash/title  (str "Renamed session to '" title "'"),
         :slash/data   {:session-id sid, :title title}}))))

(def specs
  "Declarative session slash specs, hooked onto foundation-core's manifest
   via `:ext/slash-commands` (concatenated with the workspace slashes)."
  [{:slash/name       "rename",
    :slash/doc        "Rename this session's title.",
    :slash/usage      "/rename <new title>",
    :slash/prompt-arg "New session title",
    :slash/requires   #{:session},
    :slash/run-fn     handle-rename}])
