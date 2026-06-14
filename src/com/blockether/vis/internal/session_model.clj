(ns com.blockether.vis.internal.session-model
  "Persistent, channel-NEUTRAL per-session model preference.

   ONE store for every channel (web gateway + TUI) so a session routes
   through the same model wherever it's opened, and the choice survives
   process restarts. The engine reads it at turn start (`prepare-turn-context`
   in loop.clj) as the default `:model` when the caller passes none — so a
   channel only has to SET the preference and routing follows everywhere;
   `router-for-model` hoists the chosen model with the rest of the fleet kept
   behind it as fallback (a blank/unknown name no-ops to the config order).

   Keyed by the session id (the session-soul id — the same id the gateway's
   `sid` and the engine env's `:session-id` carry). Backed by a tiny sidecar
   EDN beside the gateway token; this ns depends only on edn + telemere, so
   loop.clj and the channels can require it with no cycle."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [taoensso.telemere :as tel]))

(defonce ^:private write-lock (Object.))
(defonce ^:private cache (atom nil)) ; nil = unloaded; map = {sid-string -> model}

(def ^:private prefs-file
  (delay (str (System/getProperty "user.home") "/.vis/session-model-prefs.edn")))

(defn- read-file* []
  (let [f (java.io.File. ^String @prefs-file)]
    (if (.isFile f)
      (try
        (let [m (edn/read-string (slurp f))] (if (map? m) m {}))
        (catch Throwable t
          (tel/log! {:level :warn :id ::prefs-read-failed
                     :data {:error (ex-message t)}
                     :msg "session model prefs unreadable — starting empty"})
          {}))
      {})))

(defn- prefs [] (or @cache (reset! cache (read-file*))))

(defn model-of
  "The persisted model name preferred for session `sid` (uuid or string),
   or nil for the router default."
  [sid]
  (when sid (get (prefs) (str sid))))

(defn set-model!
  "Persist (or clear, with nil/blank) the model preference for session `sid`.
   Atomic write (temp + rename); updates the in-memory cache. Returns the
   normalized model (or nil)."
  [sid model]
  (when sid
    (locking write-lock
      (let [model (some-> model str str/trim not-empty)
            m     (or @cache (read-file*))
            m'    (if model (assoc m (str sid) model) (dissoc m (str sid)))
            path  ^String @prefs-file
            tmp   (java.io.File. (str path ".tmp"))]
        (.mkdirs (java.io.File. (str (System/getProperty "user.home") "/.vis")))
        (spit tmp (pr-str m'))
        (.renameTo tmp (java.io.File. path))
        (reset! cache m')
        model))))
