(ns com.blockether.vis.internal.foundation.session-slashes
  "Declarative session-level slash commands shared by every channel.

   These are channel-agnostic: the engine dispatches them for the TUI, the
   web, and Telegram alike through the same `slash/dispatch` path, and each
   handler mutates state via the gateway so the change fans out everywhere.

     /rename <new title>   set this session's title

   `/rename` routes through `titling/set-title-with-broadcast!` — the single
   title mutation point."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.titling :as titling]))

(defn- export-html
  "Standalone, vis-light-styled HTML transcript for a session — the canonical
   `transcript/transcript->html` render (summary card + turn-by-turn forensic
   body, all CSS inlined). No web extension required."
  [db sid]
  ((requiring-resolve 'com.blockether.vis.internal.foundation.transcript/transcript->html)
   ((requiring-resolve 'com.blockether.vis.internal.foundation.transcript/transcript) db sid)))

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- handle-rename
  "`/rename <new title>` — set the current session's title. Reuses the
   gateway's single title mutation point."
  [ctx]
  (let [sid
        (or (:session/id ctx) (:session-id ctx))

        db
        (or (:db-info ctx) (:db ctx))

        atom*
        (:session-title-atom ctx)

        title
        (some-> (str/join " " (:command/argv ctx))
                str/trim
                not-empty)]

    (cond (nil? sid) (err "Send a message first, then /rename <title> (session not ready yet)")
          (nil? title) (err "Name it: /rename <new title>")
          :else (do (titling/set-title-with-broadcast! db sid atom* title)
                    {:slash/status :ok
                     :slash/title (str "Renamed session to '" title "'")
                     :slash/data {:session-id sid :title title}}))))

(defn- handle-export
  "`/export-html [path]` — write this session's transcript to a STANDALONE,
   vis-light-styled HTML file (same renderer as `vis sessions export
   --html` and the web download). Defaults to `vis-transcript-<id8>.html`
   in the working directory."
  [ctx]
  (let [sid
        (or (:session/id ctx) (:session-id ctx))

        db
        (or (:db-info ctx) (:db ctx))

        path
        (some-> (str/join " " (:command/argv ctx))
                str/trim
                not-empty)]

    (cond (nil? sid) (err "Send a message first, then /export-html (session not ready yet)")
          (nil? db) (err "No database available to export from.")
          :else (let [fname
                      (or path (str "vis-transcript-" (subs (str sid) 0 8) ".html"))

                      target
                      (io/file fname)]

                  (when-let [parent (.getParentFile ^java.io.File target)]
                    (.mkdirs parent))
                  (spit target (export-html db sid))
                  {:slash/status :ok
                   :slash/title (str "Exported HTML transcript to " (.getPath target))
                   :slash/data {:session-id sid :path (.getPath target)}}))))

(def specs
  "Declarative session slash specs, hooked onto foundation-core's manifest
   via `:ext/slash-commands` (concatenated with the workspace slashes)."
  [{:slash/name "rename"
    :slash/doc "Rename this session's title."
    :slash/usage "/rename <new title>"
    :slash/prompt-arg "New session title"
    :slash/requires #{:session}
    :slash/run-fn handle-rename}
   {:slash/name "export-html"
    :slash/doc "Export this session's transcript as styled HTML."
    :slash/usage "/export-html [path]"
    :slash/prompt-arg "Output .html path (optional)"
    :slash/requires #{:session}
    :slash/run-fn handle-export}])
