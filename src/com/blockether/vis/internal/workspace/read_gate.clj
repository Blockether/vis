(ns com.blockether.vis.internal.workspace.read-gate
  "The one read gate for a file that a person opens on the gateway machine: the
   session's filesystem roots, its draft rules, the configured deny rules and the
   `:fs/access` gate hooks. The file preview, the media stream and an attachment
   that keeps a reference to a file (#450) all ask here, so none of them can show
   a file that another of them refuses."
  (:require [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(defn env-root
  "The canonical root of the live workspace of `env`, or nil."
  [env]
  (let [ws (if-let [a (:workspace-atom env)]
             @a
             (:workspace env))]
    (workspace/normalize-root (or (:root ws) (:workspace/root env)))))

(defn readable-file
  "Resolve `requested` under `root` with the filesystem roots and read gates of `env`.
   Returns `{:file f}` for a readable regular file, else `{:refusal {:status :type
   :message :path?}}` with the HTTP status that a route answers."
  [env root requested]
  (try
    (binding [workspace/*workspace-root*
              root

              workspace/*filesystem-roots*
              (workspace/env-filesystem-roots env)]

      (let [^java.io.File target
            (workspace/resolve-file-path requested)

            path
            (.getPath target)

            refusal
            (or (when-let [reason (process-jail/deny-refusal env "file-read" path)]
                  {:reason reason})
                (when (extension/gate-hooked? :fs/access)
                  (extension/run-gate-hooks :fs/access env {:operation "file-read" :path path})))]

        (cond refusal
              {:refusal
               {:status 403 :type :file-access-denied :message (:reason refusal) :path path}}
              (not (.isFile target))
              {:refusal {:status 404 :type :not-a-file :message "no such file" :path path}}
              :else {:file target})))
    (catch clojure.lang.ExceptionInfo e
      {:refusal (case (:type (ex-data e))
                  (:workspace/path-escape :workspace/path-denied)
                  {:status 403 :type :file-access-denied :message (ex-message e)}

                  {:status 400 :type :invalid-request :message "that path could not be resolved"})})
    (catch java.io.IOException _
      {:refusal {:status 400 :type :invalid-request :message "that path could not be resolved"}})
    (catch java.nio.file.InvalidPathException _
      {:refusal {:status 400 :type :invalid-request :message "that path could not be resolved"}})))
