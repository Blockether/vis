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

     - DATA (serializable, STRING-KEYED): id, kind, label, status, detail,
       pid, owner, session, can_stop, can_restart, created_at. This is
       what the footer renders and what persists to `~/.vis/resources.edn` so a
       resource survives a vis restart (display + pid re-attach).
     - LIFECYCLE THUNKS (live, in-memory only): `:stop-fn :restart-fn :alive-fn
       :logs-fn :health-fn`. Never serialized. Across a restart the OWNER
       re-registers them (e.g. the Clojure pack re-attaches its nREPLs by pid on
       init) — exactly the pattern `repl-manager` already uses, lifted here so
       EVERY kind gets it. `:health-fn` answers \"alive, but is it WORKING?\"
       (:up :starting :failed :down …) and is probed — parallel, hard timeout —
       on every `list-resources`, flipping the stored `status` to reality.

   `:kind` is OPEN-ENDED — a bare keyword, no closed enum. The registry never
   switches on it; only owners do.

   Model ctx stays PURE DATA and groups the flat registry through `model-view`:
   REPLs live at `session["
  resources
  "]["
  repls
  "][language][dir]`. It advertises
   `can_stop`/`can_restart` but never carries a
   callable. Killing goes through `stop!`/`restart!` (by session + id) — the
   single path the agent tool AND the footer both call. `id` IS the binding."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.lang ProcessHandle)))

;; ---------------------------------------------------------------------------
;; In-memory registry: { session-id -> { id -> {:data {..} :stop-fn :restart-fn
;; :alive-fn} } }. defonce so a `(require :reload)` during dev never drops live
;; resources.
;; ---------------------------------------------------------------------------
(defonce ^:private registry (atom {}))

(def ^:private ^java.io.File state-dir (io/file (System/getProperty "user.home") ".vis"))

(def ^:private registry-file (io/file state-dir "resources.edn"))

(defonce ^:private registry-lock (Object.))

(defn- skey [session] (str session))

;; ---------------------------------------------------------------------------
;; Persistence — the DATA part only (thunks can't serialize), partitioned by
;; session. Survives restart for display + pid re-attach; owners re-register
;; thunks on init.
;; ---------------------------------------------------------------------------

(defn- write-persisted!
  [session->id->data]
  (locking registry-lock
    (try (.mkdirs state-dir)
         (spit registry-file (pr-str (or session->id->data {})))
         (catch Throwable _ nil))))

(defn- persist-snapshot!
  "Persist the DATA of every registered resource, partitioned by session."
  []
  (write-persisted! (into {}
                          (map (fn [[sid id->rec]]
                                 [sid
                                  (into {}
                                        (map (fn [[id r]]
                                               [id (:data r)]))
                                        id->rec)]))
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
    (let [^ProcessHandle ph (.orElse (ProcessHandle/of (long pid)) nil)]
      (boolean (and ph (.isAlive ph))))))

(defn- record-alive?
  [{:keys [data alive-fn]}]
  (cond alive-fn (boolean (try (alive-fn) (catch Throwable _ false)))
        :else (pid-alive? (get data "pid"))))

;; ---------------------------------------------------------------------------
;; Data shape — STRING-KEYED. The DATA map is what ctx carries into
;; `session["resources"]`, so it crosses the Python boundary: keys and enum
;; values (kind/status/owner/language) are strings, never keywords. Callers
;; still hand a keyword-keyed INPUT map (a host-side API arg); `->data` and
;; `normalize-patch` are the single construction boundary that stringifies it.
;; ---------------------------------------------------------------------------

(defn- sval
  "Stringify a keyword enum VALUE so it can cross the boundary; other scalars
   pass through unchanged."
  [v]
  (if (keyword? v) (name v) v))

(defn- data-key
  "A caller patch KEY (kebab keyword like `:can-stop`) → the canonical DATA
   string key (`\"can_stop\"`). Non-keywords (already-canonical strings) pass
   through."
  [k]
  (if (keyword? k) (.replace (name k) "-" "_") k))

(defn- normalize-patch
  "Project a caller `update!` patch (keyword-keyed host-side map) onto the
   canonical string-keyed DATA shape so a merge can't mix key types."
  [patch]
  (reduce-kv (fn [m k v]
               (assoc m (data-key k) (sval v)))
             {}
             patch))

(defn- ->data
  "Normalise a caller resource map into the canonical serializable DATA map.
   `\"can_stop\"`/`\"can_restart\"` reflect whether a thunk was supplied;
   `\"session\"` is stamped from the owning session."
  [session {:keys [id kind label status detail pid owner language]}
   {:keys [stop-fn restart-fn logs-fn health-fn]}]
  (cond-> {"id" (str id)
           "session" (skey session)
           "kind" (sval (or kind :resource))
           "label" (str (or label id))
           "status" (sval (or status :up))
           "can_stop" (boolean stop-fn)
           "can_restart" (boolean restart-fn)
           "can_logs" (boolean logs-fn)
           "can_health" (boolean health-fn)
           "created_at" (System/currentTimeMillis)}
    detail
    (assoc "detail" detail)

    pid
    (assoc "pid" pid)

    owner
    (assoc "owner" (sval owner))

    language
    (assoc "language" (sval language))))

;; ---------------------------------------------------------------------------
;; Public API — every verb is scoped to the owning `session`.
;; ---------------------------------------------------------------------------

(defn register!
  "Register (or replace) a resource UNDER `session`. `resource` is the DATA map
   (needs at least `:id`, unique within the session; `:kind` defaults to
   `:resource`). `fns` carries the live lifecycle thunks `{:stop-fn :restart-fn
   :alive-fn :logs-fn :health-fn}` (all optional — a resource with no `:stop-fn`
   reports `can_stop false`). Returns the stored DATA map."
  ([session resource] (register! session resource nil))
  ([session resource {:keys [stop-fn restart-fn alive-fn logs-fn health-fn] :as fns}]
   (let [sid
         (skey session)

         id
         (str (:id resource))

         data
         (->data session resource fns)]

     (swap! registry assoc-in
       [sid id]
       (cond-> {:data data}
         stop-fn
         (assoc :stop-fn stop-fn)

         restart-fn
         (assoc :restart-fn restart-fn)

         alive-fn
         (assoc :alive-fn alive-fn)

         logs-fn
         (assoc :logs-fn logs-fn)

         health-fn
         (assoc :health-fn health-fn)))
     (persist-snapshot!)
     data)))

(defn update!
  "Patch the DATA of an existing resource (e.g. flip `:status`, refresh
   `:detail`). No-op if unknown. Returns the updated DATA map or nil."
  [session id patch]
  (let [sid
        (skey session)

        id
        (str id)]

    (when (get-in @registry [sid id])
      (let [data (-> (swap! registry update-in [sid id :data] merge (normalize-patch patch))
                     (get-in [sid id :data]))]
        (persist-snapshot!)
        data))))

(defn unregister!
  "Drop a resource from `session` (does NOT run its stop-fn — caller decides).
   Returns true if something was removed."
  [session id]
  (let [sid
        (skey session)

        id
        (str id)

        present
        (boolean (get-in @registry [sid id]))]

    (when present
      (swap! registry update sid dissoc id)
      (when (empty? (get @registry sid)) (swap! registry dissoc sid))
      (persist-snapshot!))
    present))

(defn prune!
  "Drop `session` resources whose process is gone (per `:alive-fn`/pid). Returns
   the vector of pruned ids. Cheap enough to call before every list/render."
  [session]
  (let [sid
        (skey session)

        snap
        (get @registry sid)

        dead
        (into []
              (comp (filter (fn [[_ r]]
                              (not (record-alive? r))))
                    (map key))
              snap)]

    (when (seq dead)
      (apply swap! registry update sid dissoc dead)
      (when (empty? (get @registry sid)) (swap! registry dissoc sid))
      (persist-snapshot!))
    dead))

;; Hard per-probe deadline for a `:health-fn`. One wedged health thunk (a hung
;; server, a blocking connect) must NEVER stall a footer render or a ctx build.
(def ^:private health-timeout-ms 300)

(defn- refresh-health!
  "Probe every health-capable resource of `session` IN PARALLEL, each under a
   hard `health-timeout-ms` deadline, and flip the stored `status` where it
   changed. A `:health-fn` returns a keyword status (:up :starting :failed
   :down …); nil / a throw / a timeout means UNKNOWN and leaves the stored
   status alone. Best-effort; persists once when anything flipped."
  [session]
  (let [sid
        (skey session)

        probes
        (into []
              (keep (fn [[id {:keys [health-fn]}]]
                      (when health-fn [id (future (try (health-fn) (catch Throwable _ nil)))])))
              (get @registry sid))

        changed?
        (volatile! false)]

    (doseq [[id fut] probes]
      (let [st (deref fut health-timeout-ms nil)]
        (when-not (realized? fut) (future-cancel fut))
        (when (keyword? st)
          (let [s (name st)]
            (when (and (get-in @registry [sid id])
                       (not= s (get-in @registry [sid id :data "status"])))
              (swap! registry assoc-in [sid id :data "status"] s)
              (vreset! changed? true))))))
    (when @changed? (persist-snapshot!))))

(defn list-resources
  "Vector of live resource DATA maps for `session`, dead ones pruned and every
   health-capable `status` refreshed first (parallel, hard-timeout probes).
   This is what the footer renders and what `:session/resources` carries into
   ctx — ONLY the calling session's resources."
  [session]
  (prune! session)
  (refresh-health! session)
  (mapv :data (vals (get @registry (skey session)))))

(defn- repl-kind? [kind] (str/ends-with? (str/lower-case (str kind)) "repl"))

(defn- model-dir
  "Canonical workspace-relative REPL directory for model lookup. The workspace
   root is `\".\"`; nested dirs use forward slashes; external dirs stay absolute."
  [root dir]
  (try (let [root
             (some-> root
                     io/file
                     .getCanonicalFile)

             target
             (cond (and dir (.isAbsolute (io/file (str dir)))) (.getCanonicalFile (io/file (str
                                                                                             dir)))
                   root (.getCanonicalFile (io/file root (str (or dir ""))))
                   dir (.getCanonicalFile (io/file (str dir)))
                   :else nil)]

         (cond (nil? target) "."
               (nil? root) (.getPath target)
               (.startsWith (.toPath target) (.toPath root))
               (let [rel (str (.relativize (.toPath root) (.toPath target)))]
                 (if (str/blank? rel) "." (str/replace rel java.io.File/separator "/")))
               :else (.getPath target)))
       (catch Throwable _
         (or (some-> dir
                     str)
             "."))))

(defn model-view
  "Nested model-facing resource ground truth. REPLs are directly addressable as
   `[\"repls\"][language][workspace-relative-dir]`; active languages are seeded
   with empty maps so absence at a dir means inactive. Non-REPL resources live
   under `[\"other\"][kind][id]`. The registry/footer keep their flat DATA API."
  [resource-data {:keys [root languages]}]
  (let [seed-repls
        (into (sorted-map)
              (map (fn [language]
                     [(str/lower-case (str language)) (sorted-map)]))
              languages)

        {:keys [repls other]}
        (reduce (fn [{:keys [repls other] :as acc} resource]
                  (let [kind (get resource "kind")]
                    (if (repl-kind? kind)
                      (let [language (str/lower-case (str (or (get resource "language") "unknown")))
                            detail (or (get resource "detail") {})
                            dir (model-dir root (get detail "dir"))
                            leaf (-> resource
                                     (dissoc "detail")
                                     (merge detail)
                                     (assoc "dir" dir))]

                        (assoc acc :repls (assoc-in repls [language dir] leaf)))
                      (assoc acc
                        :other (assoc-in other [(str kind) (get resource "id")] resource)))))
                {:repls seed-repls :other (sorted-map)}
                (sort-by #(get % "id") resource-data))]

    (cond-> (array-map)
      (seq repls)
      (assoc "repls" repls)

      (seq other)
      (assoc "other" other))))

(defn get-resource
  "DATA map for `session`+`id`, or nil."
  [session id]
  (get-in @registry [(skey session) (str id) :data]))

(defn logs
  "Captured output lines for `session`+`id`, via the resource's `:logs-fn` thunk.
   Returns a vector of line strings (newest last), or nil when the resource has
   no logs-fn (`can_logs false`) or is unknown. Shell backgrounds expose their
   ring buffer; managed language REPLs can expose launcher logs."
  [session id]
  (when-let [f (get-in @registry [(skey session) (str id) :logs-fn])]
    (try (vec (f)) (catch Throwable _ nil))))

(defn stop!
  "Run a resource's `:stop-fn` and unregister it. THE single stop path — the
   agent tool and the footer both land here, always scoped to `session` so no
   session can stop another's resource. Returns a result map."
  [session id]
  (let [sid
        (skey session)

        id
        (str id)

        r
        (get-in @registry [sid id])]

    (cond (nil? r) {:result :unknown :id id :message "No such resource in this session."}
          (nil? (:stop-fn r))
          {:result :not-stoppable
           :id id
           :message "Resource has no stop handle (owner must re-register after a restart)."}
          :else (let [res (try {:ok (do ((:stop-fn r)) true)}
                               (catch Throwable t {:error (ex-message t)}))]
                  (unregister! session id)
                  (if (:error res)
                    {:result :error :id id :message (:error res)}
                    {:result :stopped :id id})))))

(defn restart!
  "Run a resource's `:restart-fn` (kept registered). Scoped to `session`. The
   restart-fn owns re-registration of any changed DATA (e.g. a new port)."
  [session id]
  (let [sid
        (skey session)

        id
        (str id)

        r
        (get-in @registry [sid id])]

    (cond (nil? r) {:result :unknown :id id :message "No such resource in this session."}
          (nil? (:restart-fn r))
          {:result :not-restartable :id id :message "Resource has no restart handle."}
          :else (let [res (try {:ok ((:restart-fn r))} (catch Throwable t {:error (ex-message t)}))]
                  (if (:error res)
                    {:result :error :id id :message (:error res)}
                    {:result :restarted :id id})))))

(defn stop-all!
  "Teardown spout for one `session` (engine end-of-session): stop every resource
   that session registered. Best-effort; returns the vector of stop! results."
  [session]
  (mapv (partial stop! session) (keys (get @registry (skey session)))))

(defn shutdown!
  "Process-wide teardown spout (daemon/engine shutdown): stop EVERY registered
   resource across ALL sessions, so no background child (a shell_bg server, a
   REPL) outlives the process that owns its stop-fn. Best-effort; returns
   `{session-id [stop! results]}`."
  []
  (into {}
        (map (fn [sid]
               [sid (stop-all! sid)]))
        (keys @registry)))

;; ---------------------------------------------------------------------------
;; Agent surface — B-dispatch. The sandbox gets two engine-builtin tools, each
;; CLOSED OVER the owning session, that act on a resource purely by its `:id`;
;; ctx (`session["resources"]`) already advertises which ids are
;; `can_stop`/`can_restart`. Wired by the loop via env/set-python-binding!,
;; which snake-cases the symbol: `resource-stop` -> `resource_stop(id)`.
;; ---------------------------------------------------------------------------

(defn- ->model-result
  "Project an internal `stop!`/`restart!` result map (`{:result :stopped
   :id ...}`) to a strings-only payload for the model-facing tools — the return
   value crosses the Python boundary, so nothing keyword may survive."
  [{:keys [result id message]}]
  (cond-> {"result" (name result) "id" (str id)}
    message
    (assoc "message" message)))

(defn- ->id
  "Normalize a model-supplied resource id. The tool is documented as taking a
   positional string, but a model frequently calls `resource_stop({\"id\": x})`
   (or `resource_stop({:id x})`) — unwrap that single-key map to its id value so
   the call resolves instead of stringifying to a bogus `{id x}` literal."
  [id]
  (if (map? id) (str (or (get id "id") (get id :id) id)) (str id)))

(defn sandbox-bindings
  "Map of engine-builtin tool fns the loop merges into `session`'s agent sandbox.
   Closures bind the session so the tools are session-scoped by construction.
   Returns are projected to strings-only (`->model-result`) since they cross the
   boundary as the tool result; `stop!`/`restart!` stay keyword-keyed for
   internal callers (e.g. the REPL pack's `repl_stop`)."
  [session]
  {'resource-stop (fn [id]
                    (->model-result (stop! session (->id id))))
   'resource-restart (fn [id]
                       (->model-result (restart! session (->id id))))})
