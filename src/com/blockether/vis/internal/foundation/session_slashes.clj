(ns com.blockether.vis.internal.foundation.session-slashes
  "Declarative session-level slash commands shared by every channel.

   These are channel-agnostic: the engine dispatches them for every channel
   through the same `slash/dispatch` path, and each
   handler mutates state via the gateway so the change fans out everywhere.

     /rename <new title>   set this session's title

   `/rename` routes through `titling/set-title-with-broadcast!` — the single
   title mutation point."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.titling :as titling]))

(defn- render-transcript
  "Render a session's transcript as Markdown (`:md`) or a STANDALONE,
   vis-light-styled HTML document (`:html`). Both go through the canonical
   `transcript` data projection and its pure renderers — no web extension
   required."
  [db sid fmt]
  (let
    [data
     ((requiring-resolve 'com.blockether.vis.internal.foundation.transcript/transcript) db sid)

     render
     (if (= fmt :html)
       'com.blockether.vis.internal.foundation.transcript/transcript->html
       'com.blockether.vis.internal.foundation.transcript/transcript->md)]

    ((requiring-resolve render) data)))

(defn- path->fmt
  "Infer the export format from a target path's extension: `.html`/`.htm`
   → `:html`, anything else → `:md`."
  [path]
  (if (and path (re-find #"(?i)\.html?$" path)) :html :md))

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- handle-rename
  "`/rename <new title>` — set the current session's title. Reuses the
   gateway's single title mutation point."
  [ctx]
  (let
    [sid
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

(defn- export!
  "Core `/export` implementation. Writes this session's transcript to a file,
   choosing Markdown or HTML by `forced-fmt` when non-nil, otherwise the path
   extension. Defaults to `vis-transcript-<id8>.<ext>` in the working
   directory. Same renderers as `vis sessions export --md|--html`."
  [ctx forced-fmt]
  (let
    [sid
     (or (:session/id ctx) (:session-id ctx))

     db
     (or (:db-info ctx) (:db ctx))

     path
     (some-> (str/join " " (:command/argv ctx))
             str/trim
             not-empty)]

    (cond (nil? sid) (err "Send a message first, then /export (session not ready yet)")
          (nil? db) (err "No database available to export from.")
          :else (let
                  [fmt
                   (or forced-fmt (path->fmt path))

                   ext
                   (if (= fmt :html) "html" "md")

                   fname
                   (or path (str "vis-transcript-" (subs (str sid) 0 8) "." ext))

                   target
                   (io/file fname)]

                  (when-let [parent (.getParentFile ^java.io.File target)]
                    (.mkdirs parent))
                  (spit target (render-transcript db sid fmt))
                  {:slash/status :ok
                   :slash/title (str "Exported " (str/upper-case ext)
                                     " transcript to " (.getPath target))
                   :slash/data {:session-id sid :path (.getPath target) :format fmt}}))))

(defn- handle-export
  "`/export [path]` — write this session's transcript to a file, format chosen
   by the path extension (`.html`/`.htm` → styled standalone HTML, otherwise
   Markdown). Defaults to `vis-transcript-<id8>.md`."
  [ctx]
  (export! ctx nil))

(defn- handle-export-html
  "`/export-html [path]` — always write a STANDALONE, vis-light-styled HTML
   transcript. Kept for back-compat; `/export foo.html` is equivalent."
  [ctx]
  (export! ctx :html))

(def specs
  "Declarative session slash specs, hooked onto foundation-core's manifest
   via `:ext/slash-commands` (concatenated with the workspace slashes)."
  [{:slash/name "rename"
    :slash/doc "Rename this session's title."
    :slash/usage "/rename <new title>"
    :slash/prompt-arg "New session title"
    :slash/requires #{:session}
    :slash/run-fn handle-rename}
   {:slash/name "export"
    :slash/doc
    "Export this session's transcript to a file (Markdown, or HTML when the path ends in .html)."
    :slash/usage "/export [path]"
    :slash/prompt-arg "Output path (.md or .html, optional)"
    :slash/requires #{:session}
    :slash/run-fn handle-export}
   {:slash/name "export-html"
    :slash/doc "Export this session's transcript as styled HTML."
    :slash/usage "/export-html [path]"
    :slash/prompt-arg "Output .html path (optional)"
    :slash/requires #{:session}
    :slash/run-fn handle-export-html}])
