(ns com.blockether.vis.internal.loop
  "Session API over the turn engine.

   Creates, opens, sends to, closes and deletes sessions, and manages projects,
   session groups and read marks."
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.extension.client :as client-extensions]
            [com.blockether.vis.internal.foundation.shell-log :as shell-log]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.session.titling :as titling]))

(defn db-info
  "Return the process-wide shared DB connection bound to
   `(config/resolve-db-spec)`. Thin wrapper over
   `persistance.core/db-shared-connection!` that fills in the default db-spec
   so frontend callers stay clear of config resolution."
  []
  (persistance/db-shared-connection! (config/resolve-db-spec)))

(defn create!
  "Create a brand-new session.

   Opts (all optional):
     :title         display title
     :external-id   channel-specific external id
     :workspace-id  pre-spawned workspace to pin the new session to.
                    When omitted, a trunk workspace is auto-minted in
                    create-environment."
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id workspace-id]}]
   (let [env
         (loop-env/open-env! nil
                             (cond-> {:channel channel
                                      :external-id (some-> external-id
                                                           str)
                                      :title title}
                               workspace-id
                               (assoc :workspace-id workspace-id)))

         id
         (:session-id env)

         _
         (loop-env/cache-env! id env)]

     {:id id ; UUID
      :channel channel
      :external-id (some-> external-id
                           str)
      :title title
      :workspace-id (:workspace/id env)})))

(defn by-id
  "Return the session record (UUID `:id`) or nil."
  [id]
  (when-let [session (persistance/db-get-session (db-info) id)]
    {:id (:id session) ; UUID
     :channel (:channel session)
     :external-id (:external-id session)
     :system-prompt (:system-prompt session)
     :model (:model session) ; the state's ROOT model, not the user's pin
     :model-pref (:model-pref session) ; {:provider :model} pin, or nil for router default
     :title (:title session)
     :goal (:goal session)
     :created-at (:created-at session)
     :owner-id (:owner-id session)
     :project-id (:project-id session)
     :project-name (:project-name session)
     :project-position (:project-position session)
     :group-id (:group-id session)
     :favorite-rank (:favorite-rank session)
     :archived-at (:archived-at session)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id (:id c) ; UUID
           :channel (:channel c)
           :external-id (:external-id c)
           :title (:title c)
           :goal (:goal c)
           :created-at (:created-at c)
           :owner-id (:owner-id c)
           :project-id (:project-id c)
           :project-name (:project-name c)
           :project-position (:project-position c)
           :group-id (:group-id c)
           :favorite-rank (:favorite-rank c)
           :archived-at (:archived-at c)})
        (persistance/db-list-sessions (db-info) channel)))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn projects
  "List projects (cross-channel). `opts`: :owner-id (default \"local\"),
   :archived (:exclude, :include or :only). Each carries a live :session-count."
  ([] (projects {}))
  ([opts] (persistance/db-list-projects (db-info) opts)))

(defn get-project [project-id] (persistance/db-get-project (db-info) project-id))

(defn create-project! [opts] (persistance/db-create-project! (db-info) opts))

(defn get-project-by-root
  "Project bound to canonical workspace `root` for `owner-id` (default
   \"local\"), or nil."
  ([root] (get-project-by-root "local" root))
  ([owner-id root] (persistance/db-get-project-by-root (db-info) owner-id root)))

(defn ensure-project-for-root!
  "Get-or-create the project bound to canonical workspace `root` (a project IS a
   tab set). Race-safe: on a UNIQUE(owner_id, workspace_root) collision from a
   creator the insert throws and we re-read. `name` seeds a freshly created
   project (falls back to the root path)."
  ([root] (ensure-project-for-root! "local" root nil))
  ([owner-id root name]
   (or (get-project-by-root owner-id root)
       (try (create-project! {:name (or (not-empty (str name)) (str root))
                              :owner-id (or owner-id "local")
                              :workspace-root root})
            ;; ONLY a lost get-or-create race is expected here (the partial
            ;; UNIQUE index rejects the duplicate) -> re-read the winner. Any
            ;; OTHER failure (disk full, real constraint break) must NOT be
            ;; swallowed as nil: re-read, and if there's still no project the
            ;; original error was the true cause, so rethrow it.
            (catch Throwable e (or (get-project-by-root owner-id root) (throw e)))))))

(defn update-project! [project-id opts] (persistance/db-update-project! (db-info) project-id opts))

(defn delete-project! [project-id] (persistance/db-delete-project! (db-info) project-id))

(defn project-session-ids
  "Ids of every session soul belonging to `project-id`, across channels, in the
   project's own tab order.

   This is MEMBERSHIP, not a client's visible list: an untitled or empty
   conversation is a member too, and a caller that fans out over what it can see
   would delete the visible rows and silently keep the rest. One indexed read -
   membership is a WHERE clause, never a walk of the whole store."
  [project-id]
  (persistance/db-project-session-ids (db-info) project-id))

(defn assign-project!
  "Assign the session soul to `project-id` (nil clears / removes from project)."
  [session-id project-id]
  (persistance/db-set-session-project! (db-info) session-id project-id))

(defn set-favorite!
  "Star (`true`) or unstar (`false`) the session soul. Returns the rank it now
   holds, or nil once it is unstarred."
  [session-id is-favorite]
  (persistance/db-set-session-favorite! (db-info) session-id is-favorite))

(defn set-archived!
  "Archive (`true`) or unarchive (`false`) the session soul. Returns the stamp it
   now carries, or nil once it is unarchived."
  [session-id archived?]
  (persistance/db-set-session-archived! (db-info) session-id archived?))

(defn reorder-project-sessions!
  "Atomically adopt any loose named session into `project-id`, then persist the
   manual order. Guests owned by another project are never stolen."
  [project-id session-ids]
  (persistance/db-adopt-and-reorder-project-sessions! (db-info) project-id session-ids))

;; --- Session groups: the human's own groups inside ONE project (V8) ---

(defn session-groups
  "List the groups of `project-id`, in their manual order, each with a live
   :session-count. `opts`: :archived (:exclude, :include or :only). `[]` when
   the project has none."
  ([project-id] (session-groups project-id {}))
  ([project-id opts] (persistance/db-list-session-groups (db-info) project-id opts)))

(defn archived-session-group-ids
  "The ids (strings) of every archived group, across every project. One read backs
   the whole sessions listing: a group's archive hides its sessions without
   stamping them."
  []
  (persistance/db-archived-session-group-ids (db-info)))

(defn get-session-group [group-id] (persistance/db-get-session-group (db-info) group-id))

(defn create-session-group!
  "Create a group inside `project-id`. `opts`: :name (required, unique in the
   project), :color (palette token, defaults to \"slate\"), :position."
  [project-id opts]
  (persistance/db-create-session-group! (db-info) project-id opts))

(defn update-session-group!
  "Patch a group: :name, :color, :position and/or :archived?."
  [group-id opts]
  (persistance/db-update-session-group! (db-info) group-id opts))

(defn delete-session-group!
  "Delete a group. Its sessions stay in the project, ungrouped."
  [group-id]
  (persistance/db-delete-session-group! (db-info) group-id))

(defn session-group-session-ids
  "Ids of every session soul filed under `group-id`, across channels, newest first.

   MEMBERSHIP, like `project-session-ids`: an untitled conversation counts too,
   and the group's own index answers it."
  [group-id]
  (persistance/db-session-group-session-ids (db-info) group-id))

(defn assign-session-group!
  "File the session soul under `group-id` (nil leaves it ungrouped). Joining a
   group of another project adopts the session into that project as well."
  [session-id group-id]
  (persistance/db-set-session-group! (db-info) session-id group-id))

;; --- Read marks: how far a reader has read (the gateway's own "NEW") ---

(defn session-read-marks
  "How far `reader-id` has read each conversation, as `{session-id-string
   seen-answers}` - one indexed read of that reader's own rows. An id ABSENT from
   the map has never been shown to this reader, which is what separates a first
   sight from an unread answer."
  [reader-id]
  (persistance/db-session-read-marks (db-info) reader-id))

(defn seed-session-read-marks!
  "Write a FIRST-SIGHT watermark for every `{session-id seen-answers}` of `marks`
   this reader holds no mark for, leaving the marks they already have alone.
   Returns the ids seeded."
  [reader-id marks]
  (persistance/db-seed-session-read-marks! (db-info) reader-id marks))

(defn mark-session-read!
  "Advance `reader-id`'s watermark on `session-id` to `seen-answers` settled
   answers. Never moves backwards. Returns the watermark the store now holds."
  [reader-id session-id seen-answers]
  (persistance/db-mark-session-read! (db-info) reader-id session-id seen-answers))

;; Host title setter + public env accessor

(defn env-for [id] (:environment (loop-env/ensure-env! id)))

(defn register-client-extensions!
  "Install application-owned declarations under the session's one-turn lock."
  [id owner payload live?]
  (let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock] :as entry}
        (loop-env/ensure-env! id)]
    (when-not (.tryLock lock)
      (throw (ex-info "Cannot register extensions during a turn"
                      {:status 409 :code :client_extension_busy})))
    (try (when-not (identical? entry (get @loop-env/cache (loop-env/cache-key id)))
           (throw (ex-info "Session environment changed; retry registration"
                           {:status 409 :code :client_extension_busy})))
         (doseq [ext (client-extensions/register! id owner payload live? environment)]
           (loop-env/install-extension! environment ext))
         {}
         (finally (.unlock lock)))))

(defn set-title!
  "Host-driven title change. Resolves the live env (if any) so the
   in-memory atom + listener fan-out stay in sync; falls back to a
   plain DB write when no env is live for this session (e.g.
   `vis-agent sessions` rename ops)."
  [id title]
  (let [env (env-for id)]
    (titling/set-title-with-broadcast! (or (:db-info env) (db-info))
                                       id
                                       (:session-title-atom env)
                                       title))
  nil)

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [k
         (loop-env/cache-key id)

         message-vec
         (if (string? messages) [(svar/user messages)] messages)

         ;; ONE turn per session, and the lock is TAKEN right here — through
         ;; `acquire-turn-lock!`, which never parks on a lock a wedged turn is
         ;; never going to release. Extension reload marks envs dirty; the actual
         ;; sandbox reset happens under the lock below, after prior IR/render is
         ;; finished and before the next user code executes.
         {:keys [^java.util.concurrent.locks.ReentrantLock lock] :as entry}
         (loop-env/acquire-turn-lock! id)]

     (try
       ;; Apply a pending `/reload` FIRST: if this entry was built under an
       ;; older policy epoch, recycle it now so the turn runs against a
       ;; security-policy snapshot rebuilt from the freshly-reloaded vis.yml
       ;; (new network domains / filesystem roots take effect here). Done under
       ;; the lock, before the turn, so no eval races the swap.
       (when (loop-env/policy-stale? (or (get @loop-env/cache k) entry)) (loop-env/recycle-env! k))
       ;; Re-read :environment UNDER the lock: a between-turns turn-cap recycle
       ;; or a router/extension reseat may have swapped it since we captured
       ;; `entry`, so the queued turn runs against the CURRENT context.
       (turn/turn! (:environment (or (get @loop-env/cache k) entry)) message-vec opts)
       (finally
         ;; Housekeeping must NEVER strand the lock. A throw from `touch-entry!`
         ;; or `bump-turns!` used to skip the `.unlock` below, pinning this
         ;; session's entry as permanently "busy": `evict-if-idle!` tryLocks,
         ;; fails forever, and the session — plus every door and namespace it
         ;; holds — leaks for the life of the gateway. Unlock in an inner
         ;; `finally` so it is unconditional.
         (try (let [cur (or (get @loop-env/cache k) entry)]
                (loop-env/touch-entry! cur)
                (let [n (loop-env/bump-turns! cur)]
                  ;; Recycle this session's interpreter namespace between turns so a
                  ;; single never-idle session cannot grow it unbounded.
                  (when (and (not (loop-env/policy-stale? cur))
                             (pos? (long @loop-env/env-max-turns-per-ctx))
                             (>= (long n) (long @loop-env/env-max-turns-per-ctx)))
                    (try (loop-env/recycle-env! k) (catch Throwable _ nil)))))
              (catch Throwable _ nil)
              (finally (.unlock lock)
                       ;; After unlocking: a reload racing this handoff either closes
                       ;; the idle sandbox itself or is picked up here. Never wait for
                       ;; another turn (or the idle TTL) to reclaim the old worker.
                       (loop-env/dispose-reloaded-sandbox! k))))))))

(defn close!
  [id]
  (let [k (loop-env/cache-key id)]
    (when-let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
               (clojure.core/get @loop-env/cache k)]
      ;; BOUNDED: a running turn holds the lock for the whole turn (minutes),
      ;; and a wedged one holds it forever. Wait briefly for a clean handoff,
      ;; then force-dispose anyway — an in-flight turn fails fast against a
      ;; disposed env, which beats blocking close/delete forever behind a
      ;; hung provider stream.
      (if (.tryLock lock 5 java.util.concurrent.TimeUnit/SECONDS)
        (try (try (loop-env/dispose-environment! environment) (catch Exception _ nil))
             (finally (.unlock lock)))
        (try (loop-env/dispose-environment! environment) (catch Exception _ nil))))
    (swap! loop-env/cache dissoc k)))

(defn delete!
  [id]
  (close! id)
  ;; A shell log dies with the session that produced it, and with nothing else:
  ;; the bytes on disk outlive the process, so the delete has to name them.
  (shell-log/delete-session-logs! id)
  (let [d (db-info)]
    (try (persistance/db-delete-session-tree! d id) (catch Exception _ nil))))

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted - the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn db-sweep-orphaned-running-turns!
  "Mark every `:running` turn as `:interrupted`. Run at process start
   to clean up turns that crashed or were killed mid-write so the next
   turn's handover digest renders the right outcome instead of guessing.
   Returns the number of turns swept."
  ([] (db-sweep-orphaned-running-turns! (db-info)))
  ([db]
   (let [orphans (try (persistance/db-list-session-turns-by-status db :running)
                      (catch Exception _ []))]
     (doseq [{:keys [id iteration-count duration-ms]} orphans]
       (transcript/persist-turn-outcome!
         db
         id
         {:content [(content/error "turn_interrupted" ORPHAN_INTERRUPTED_ANSWER true)]
          :iteration-count (or iteration-count 0)
          :duration-ms (or duration-ms 0)
          :status :interrupted
          :prior-outcome :cancelled}))
     (count orphans))))

(defn close-all!
  []
  ;; Process-shutdown path: never let one wedged turn hang the whole
  ;; shutdown. Bounded 2s wait per session, then force-dispose.
  (doseq [[_ {:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}] @loop-env/cache]
    (if (.tryLock lock 2 java.util.concurrent.TimeUnit/SECONDS)
      (try (try (loop-env/dispose-environment! environment) (catch Exception _ nil))
           (finally (.unlock lock)))
      (try (loop-env/dispose-environment! environment) (catch Exception _ nil))))
  (reset! loop-env/cache {})
  (persistance/db-dispose-shared-connection!))
