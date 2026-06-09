(ns com.blockether.vis.internal.resources
  "Canonical registry of VIS-MANAGED STATEFUL RESOURCES — the one interface every
   long-lived thing vis spawns (an nREPL, a daemon, a background shell, a file
   watch, a capture) registers under, so a single definition drives THREE spouts:

     1. the agent     — `:session/resources` in ctx + `resource_stop`/`resource_restart`
     2. the footer/TUI — a live count + a dialog that stops/restarts by id
     3. the engine     — teardown on shutdown

   SESSION-SCOPED. The registry is partitioned by `session-id`: a resource one
   session registers is INVISIBLE to every other session, and `id`s only need to
   be unique WITHIN a session (the `(session, id)` pair is the global key). Every
   public verb takes the owning `session` as its first arg; the per-session
   sandbox tools + ctx are closed over that id, so the agent in session A can
   neither see nor stop session B's resources.

   B-DISPATCH model. A resource is split into:

     - DATA (serializable): `:id :kind :label :status :detail :pid :owner :session
       :can-stop :can-restart :created-at`. This is what ctx carries, what the
       footer renders, and what persists to `~/.vis/resources.edn` so a resource
       survives a vis restart (display + pid re-attach).
     - LIFECYCLE THUNKS (live, in-memory only): `:stop-fn :restart-fn :alive-fn`.
       Never serialized. Across a restart the OWNER re-registers them (e.g. the
       Clojure pack re-attaches its nREPLs by pid on init) — exactly the pattern
       `repl-manager` already uses, lifted here so EVERY kind gets it.

   `:kind` is OPEN-ENDED — a bare keyword, no closed enum. The registry never
   switches on it; only owners do.

   ctx stays PURE DATA: it advertises `can_stop`/`can_restart` but never carries a
   callable. Killing goes through `stop!`/`restart!` (by session + id) — the
   single path the agent tool AND the footer both call. `id` IS the binding."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io])
  (:import
   (java.lang ProcessHandle)))

;; ---------------------------------------------------------------------------
;; In-memory registry: { session-id -> { id -> {:data {..} :stop-fn :restart-fn
;; :alive-fn} } }. defonce so a `(require :reload)` during dev never drops live
;; resources.
;; ---------------------------------------------------------------------------
(defonce ^:private registry (atom {}))

(def ^:private state-dir
  (io/file (System/getProperty "user.home") ".vis"))

(def ^:private registry-file (io/file state-dir "resources.edn"))

(defonce ^:private registry-lock (Object.))

(defn- skey [session] (str session))

;; ---------------------------------------------------------------------------
;; Persistence — the DATA part only (thunks can't serialize), partitioned by
;; session. Survives restart for display + pid re-attach; owners re-register
;; thunks on init.
;; ---------------------------------------------------------------------------

(defn- read-persisted []
  (locking registry-lock
    (try
      (when (.isFile registry-file)
        (let [m (edn/read-string (slurp registry-file))]
          (when (map? m) m)))
      (catch Throwable _ nil))))

(defn- write-persisted! [session->id->data]
  (locking registry-lock
    (try
      (.mkdirs state-dir)
      (spit registry-file (pr-str (or session->id->data {})))
      (catch Throwable _ nil))))

(defn- persist-snapshot!
  "Persist the DATA of every registered resource, partitioned by session."
  []
  (write-persisted!
    (into {}
      (map (fn [[sid id->rec]]
             [sid (into {} (map (fn [[id r]] [id (:data r)])) id->rec)]))
      @registry)))

;; ---------------------------------------------------------------------------
;; Liveness — generic pid check; owners may supply a richer `:alive-fn`.
;; ---------------------------------------------------------------------------

(defn pid-alive?
  "Best-effort: is OS process `pid` still running? nil pid -> true (can't tell,
   assume the in-process resource is alive until explicitly unregistered)."
  [pid]
  (if (nil? pid)
    true
    (boolean (some-> (.orElse (ProcessHandle/of (long pid)) nil) .isAlive))))

(defn- record-alive? [{:keys [data alive-fn]}]
  (cond
    alive-fn (boolean (try (alive-fn) (catch Throwable _ false)))
    :else    (pid-alive? (:pid data))))

;; ---------------------------------------------------------------------------
;; Data shape
;; ---------------------------------------------------------------------------

(defn- ->data
  "Normalise a caller resource map into the canonical serializable DATA map.
   `:can-stop`/`:can-restart` reflect whether a thunk was supplied; `:session`
   is stamped from the owning session."
  [session {:keys [id kind label status detail pid owner language]} {:keys [stop-fn restart-fn]}]
  (cond-> {:id          (str id)
           :session     (skey session)
           :kind        (or kind :resource)
           :label       (str (or label id))
           :status      (or status :up)
           :can-stop    (boolean stop-fn)
           :can-restart (boolean restart-fn)
           :created-at  (System/currentTimeMillis)}
    detail (assoc :detail detail)
    pid    (assoc :pid pid)
    owner    (assoc :owner owner)
    language (assoc :language language)))

;; ---------------------------------------------------------------------------
;; Public API — every verb is scoped to the owning `session`.
;; ---------------------------------------------------------------------------

(defn register!
  "Register (or replace) a resource UNDER `session`. `resource` is the DATA map
   (needs at least `:id`, unique within the session; `:kind` defaults to
   `:resource`). `fns` carries the live lifecycle thunks `{:stop-fn :restart-fn
   :alive-fn}` (all optional — a resource with no `:stop-fn` reports
   `can_stop false`). Returns the stored DATA map."
  ([session resource] (register! session resource nil))
  ([session resource {:keys [stop-fn restart-fn alive-fn] :as fns}]
   (let [sid  (skey session)
         id   (str (:id resource))
         data (->data session resource fns)]
     (swap! registry assoc-in [sid id]
       (cond-> {:data data}
         stop-fn    (assoc :stop-fn stop-fn)
         restart-fn (assoc :restart-fn restart-fn)
         alive-fn   (assoc :alive-fn alive-fn)))
     (persist-snapshot!)
     data)))

(defn update!
  "Patch the DATA of an existing resource (e.g. flip `:status`, refresh
   `:detail`). No-op if unknown. Returns the updated DATA map or nil."
  [session id patch]
  (let [sid (skey session) id (str id)]
    (when (get-in @registry [sid id])
      (let [data (-> (swap! registry update-in [sid id :data] merge patch)
                   (get-in [sid id :data]))]
        (persist-snapshot!)
        data))))

(defn unregister!
  "Drop a resource from `session` (does NOT run its stop-fn — caller decides).
   Returns true if something was removed."
  [session id]
  (let [sid (skey session) id (str id)
        present (boolean (get-in @registry [sid id]))]
    (when present
      (swap! registry update sid dissoc id)
      (when (empty? (get @registry sid))
        (swap! registry dissoc sid))
      (persist-snapshot!))
    present))

(defn prune!
  "Drop `session` resources whose process is gone (per `:alive-fn`/pid). Returns
   the vector of pruned ids. Cheap enough to call before every list/render."
  [session]
  (let [sid  (skey session)
        snap (get @registry sid)
        dead (into [] (comp (filter (fn [[_ r]] (not (record-alive? r))))
                        (map key))
               snap)]
    (when (seq dead)
      (apply swap! registry update sid dissoc dead)
      (when (empty? (get @registry sid))
        (swap! registry dissoc sid))
      (persist-snapshot!))
    dead))

(defn list-resources
  "Vector of live resource DATA maps for `session`, dead ones pruned first. This
   is what the footer renders and what `:session/resources` carries into ctx —
   ONLY the calling session's resources."
  [session]
  (prune! session)
  (mapv :data (vals (get @registry (skey session)))))

(defn get-resource
  "DATA map for `session`+`id`, or nil."
  [session id]
  (get-in @registry [(skey session) (str id) :data]))

(defn stop!
  "Run a resource's `:stop-fn` and unregister it. THE single stop path — the
   agent tool and the footer both land here, always scoped to `session` so no
   session can stop another's resource. Returns a result map."
  [session id]
  (let [sid (skey session) id (str id)
        r   (get-in @registry [sid id])]
    (cond
      (nil? r)            {:result :unknown :id id
                           :message "No such resource in this session."}
      (nil? (:stop-fn r)) {:result :not-stoppable :id id
                           :message "Resource has no stop handle (owner must re-register after a restart)."}
      :else
      (let [res (try {:ok (do ((:stop-fn r)) true)}
                  (catch Throwable t {:error (ex-message t)}))]
        (unregister! session id)
        (if (:error res)
          {:result :error :id id :message (:error res)}
          {:result :stopped :id id})))))

(defn restart!
  "Run a resource's `:restart-fn` (kept registered). Scoped to `session`. The
   restart-fn owns re-registration of any changed DATA (e.g. a new port)."
  [session id]
  (let [sid (skey session) id (str id)
        r   (get-in @registry [sid id])]
    (cond
      (nil? r)               {:result :unknown :id id
                              :message "No such resource in this session."}
      (nil? (:restart-fn r)) {:result :not-restartable :id id
                              :message "Resource has no restart handle."}
      :else
      (let [res (try {:ok ((:restart-fn r))}
                  (catch Throwable t {:error (ex-message t)}))]
        (if (:error res)
          {:result :error :id id :message (:error res)}
          {:result :restarted :id id})))))

(defn stop-all!
  "Teardown spout for one `session` (engine end-of-session): stop every resource
   that session registered. Best-effort; returns the vector of stop! results."
  [session]
  (mapv (partial stop! session) (keys (get @registry (skey session)))))

;; ---------------------------------------------------------------------------
;; Agent surface — B-dispatch. The sandbox gets two engine-builtin tools, each
;; CLOSED OVER the owning session, that act on a resource purely by its `:id`;
;; ctx (`:session/resources`) already advertises which ids are
;; `can_stop`/`can_restart`. Wired by the loop via env/set-python-binding!,
;; which snake-cases the symbol: `resource-stop` -> `resource_stop(id)`.
;; ---------------------------------------------------------------------------

(defn sandbox-bindings
  "Map of engine-builtin tool fns the loop merges into `session`'s agent sandbox.
   Closures bind the session so the tools are session-scoped by construction."
  [session]
  {'resource-stop    (fn [id] (stop! session id))
   'resource-restart (fn [id] (restart! session id))})
