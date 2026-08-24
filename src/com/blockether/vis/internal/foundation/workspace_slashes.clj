(ns com.blockether.vis.internal.foundation.workspace-slashes
  "Declarative filesystem-root slash command.

   `/cd` is session-scoped and available in every channel. What the jail ALLOWS
   comes from `jail.filesystem` in merged config; the command only moves the
   session's primary live root within that grant."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]))

(defn- ctx-session-state-id [ctx] (:session/state-id ctx))

(defn- ctx-db [ctx] (or (:db-info ctx) (:db ctx)))

(defn- session-workspace
  "The workspace backing the current session."
  [ctx]
  (let [db (ctx-db ctx)]
    (or (when-let [state-id (ctx-session-state-id ctx)]
          (workspace/for-session db state-id))
        (when-let [wid (:workspace/id ctx)]
          (workspace/get db wid)))))

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- sync-confinement!
  "Push `ws` into the live sandbox confinement pointer for this turn."
  [ctx ws]
  (when ws
    (some-> (:workspace-atom ctx)
            (reset! ws)))
  ws)

(defn- argv-path
  "The handler's whole argv as one `~`-expanded path string, or nil."
  [ctx]
  (some-> (str/join " " (:command/argv ctx))
          str/trim
          not-empty
          paths/expand-home))

(defn- handle-fs-root
  "`/cd <path>` changes the session's primary workspace root; bare `/cd` shows it."
  [ctx]
  (let [db
        (ctx-db ctx)

        state-id
        (ctx-session-state-id ctx)

        current
        (session-workspace ctx)

        path
        (argv-path ctx)]

    (cond (nil? path) {:slash/status :ok
                       :slash/title (str "Root: " (or (:root current) "(none)"))
                       :slash/body "/cd <path> to work in a different directory."
                       :slash/data {:root (:root current)}}
          (nil? state-id) (err "Send a message first, then /cd <path> (session not ready yet)")
          :else (try (let [ws (sync-confinement! ctx (workspace/change-root! db state-id path))]
                       {:slash/status :ok
                        :slash/title (str "Root changed — now working in " (:root ws))
                        :slash/body "Shell, file tools, and search operate here from the next turn."
                        :slash/data {:root (:root ws) :workspace-id (:id ws)}})
                     (catch Exception e
                       (err (str "Can't change root to '" path
                                 "': " (or (ex-message e) (str e)))))))))

(defn- build-specs
  "Slash specs owned by the workspace foundation."
  []
  [{:slash/name "cd"
    :slash/doc "Show or change the session's filesystem root (the directory vis works in)."
    :slash/usage "/cd [path]"
    :slash/run-fn handle-fs-root}])

(def specs "Declarative slash specs hooked onto foundation-core's manifest." (build-specs))
