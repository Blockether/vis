(ns com.blockether.vis.ext.persistance-sqlite.workspace-store
  "Workspace, repo-focus, and session workspace-pin SQL for the SQLite store.

   This namespace owns table-level persistence for workspace identity. The
   extension's public API vars remain in `core.clj` because Vis dispatches
   storage functions through that namespace with `ns-resolve`."
  (:require
   [charred.api :as json]
   [com.blockether.vis.core :as vis]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.nippy :as nippy]))

(def ds      vis/ds)
(def now-ms  vis/now-ms)
(def ->id    vis/->id)
(def ->uuid  vis/->uuid)
(def ->ref   vis/->ref)
(def ->kw    vis/->kw)
(def ->kw-back vis/->kw-back)
(def ->date  vis/->date)

(defn- query!
  [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q) {:builder-fn rs/as-unqualified-lower-maps}))

(defn- query-one!
  [db-info q]
  (first (query! db-info q)))

(defn- execute!
  [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q)))

(defn- new-uuid []
  (->uuid (str (java.util.UUID/randomUUID))))

(defn- ->json [m] (when m (json/write-json-str m)))
(defn- <-json [s] (when s (json/read-json s :key-fn keyword)))

(defn- ->blob
  ^bytes [v]
  (nippy/freeze v))

(defn- <-blob
  [^bytes bs]
  (when bs (nippy/thaw bs)))

(defn- row->workspace
  "Project a `workspace` row from SQLite into the canonical Clojure shape
   used by the workspace facade. Keys mirror `db-get-session` style
   (plain, not namespaced); the workspace ns adds `:workspace/*` aliases
   when publishing into an environment."
  [row]
  (when row
    (cond-> {:id         (->uuid (:id row))
             :type       :workspace
             :repo-id    (:repo_id row)
             :repo-root  (:repo_root row)
             :root       (:root row)
             :state      (->kw-back (:state row))
             :created-at (->date (:created_at row))}
      (:fork_ms row)             (assoc :fork-ms (:fork_ms row))
      (:apply_fork_ms row)       (assoc :apply-fork-ms (:apply_fork_ms row))
      (:workspace_kind row)      (assoc :workspace-kind (->kw-back (:workspace_kind row)))
      (:workspace_backend row)   (assoc :workspace-backend (->kw-back (:workspace_backend row)))
      (:parent_workspace_id row) (assoc :parent-workspace-id (->uuid (:parent_workspace_id row)))
      (:discarded_at row)        (assoc :discarded-at (->date (:discarded_at row)))
      ;; Surfaced for the workspace facade's label/focus helpers.
      ;; NULL columns are skipped so callers can treat absence as "fall back
      ;; to default" (label -> session.title; last-focused -> created_at).
      (:label row)               (assoc :label (:label row))
      (:last_focused_at_ms row)  (assoc :last-focused-at-ms (:last_focused_at_ms row))
      (:context_roots row)       (assoc :context-roots (or (<-json (:context_roots row)) [])))))

(defn insert!
  "Insert a workspace row. Returns the inserted record (canonical shape)."
  [write-tx! db-info {:keys [id repo-id repo-root root label fork-ms apply-fork-ms
                             workspace-kind workspace-backend parent-workspace-id state]}]
  (when (ds db-info)
    (let [ws-id (->id (or id (new-uuid)))
          now   (now-ms)]
      (write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:insert-into :workspace
             :values [{:id                  ws-id
                       :repo_id             repo-id
                       :repo_root           repo-root
                       :root                root
                       :label               label
                       :fork_ms             fork-ms
                       :apply_fork_ms       apply-fork-ms
                       :workspace_kind      (->kw (or workspace-kind
                                                    (if fork-ms :draft :trunk)))
                       :workspace_backend   (->kw (or workspace-backend :live))
                       :parent_workspace_id (some-> parent-workspace-id ->ref)
                       :state               (->kw (or state :active))
                       :created_at          now}]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id ws-id]})))))))

(defn update-state!
  "Transition `workspace-id` to `new-state` (`:active` | `:discarded`).
   Stamps `discarded_at` on :discarded. Returns the updated record."
  [write-tx! db-info workspace-id new-state]
  (when (and (ds db-info) workspace-id)
    (let [id  (->ref workspace-id)
          now (now-ms)
          to  (->kw new-state)
          set (cond-> {:state to}
                (= "discarded" to) (assoc :discarded_at now))]
      (write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    set
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn update-label!
  "Set the human-friendly `:label` override. Pass nil to clear the label and
   fall back to the heuristic. Returns the updated record."
  [write-tx! db-info workspace-id label]
  (when (and (ds db-info) workspace-id)
    (let [id (->ref workspace-id)]
      (write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:label label}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn set-context-roots!
  "Persist the workspace's extra context roots as a JSON array of canonical
   path strings. `roots` is a coll of strings (deduped/canonicalized by the
   caller). Empty/nil stores NULL (no extra roots). Returns the updated record."
  [write-tx! db-info workspace-id roots]
  (when (and (ds db-info) workspace-id)
    (let [id     (->ref workspace-id)
          rs     (vec (distinct (filter some? roots)))
          stored (when (seq rs) (->json rs))]
      (write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:context_roots stored}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn touch-focus!
  "Stamp `last_focused_at_ms` to now-ms on the workspace row. Called by
   `workspace/focus!`. Returns the updated record."
  [write-tx! db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let [id  (->ref workspace-id)
          now (now-ms)]
      (write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:last_focused_at_ms now}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn repo-focus-get
  "Return the `workspace_id` currently pinned as the focus pointer for
   `repo-id`. Nil when no entry exists yet."
  [db-info repo-id]
  (when (and (ds db-info) repo-id)
    (some-> (query-one! db-info
              {:select [:workspace_id :updated_at_ms]
               :from   :repo_focus
               :where  [:= :repo_id repo-id]})
      (as-> r {:workspace-id  (->uuid (:workspace_id r))
               :updated-at-ms (:updated_at_ms r)}))))

(defn repo-focus-set!
  "Upsert the per-repo focus pointer to `workspace-id`. Updates
   `updated_at_ms` to now. Returns the new pointer map."
  [write-tx! db-info repo-id workspace-id]
  (when (and (ds db-info) repo-id workspace-id)
    (let [ws-id (->ref workspace-id)
          now   (now-ms)]
      (write-tx! db-info
        (fn [tx-info]
          ;; SQLite UPSERT: INSERT ... ON CONFLICT REPLACE the row.
          (execute! tx-info
            {:insert-into :repo_focus
             :values [{:repo_id       repo-id
                       :workspace_id  ws-id
                       :updated_at_ms now}]
             :on-conflict :repo_id
             :do-update-set {:workspace_id  ws-id
                             :updated_at_ms now}})
          {:workspace-id  (->uuid ws-id)
           :updated-at-ms now})))))

(defn get-workspace
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (row->workspace
      (query-one! db-info
        {:select [:*] :from :workspace
         :where  [:= :id (->ref workspace-id)]}))))

(defn list-by-repo
  "List workspaces in `repo-id`, optionally filtered to a `state-set` of
   keywords (e.g. #{:active :merging}). Newest first."
  ([db-info repo-id] (list-by-repo db-info repo-id nil))
  ([db-info repo-id state-set]
   (when (ds db-info)
     (let [where (cond-> [:and [:= :repo_id repo-id]]
                   (seq state-set)
                   (conj [:in :state (mapv ->kw state-set)]))]
       (mapv row->workspace
         (query! db-info
           {:select   [:*]
            :from     :workspace
            :where    where
            :order-by [[:created_at :desc]]}))))))

(defn for-session
  "Return the workspace pinned to `session-state-id`, or nil."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (let [sid (->ref session-state-id)]
      (row->workspace
        (query-one! db-info
          {:select [:w.*]
           :from   [[:session_state :s]]
           :join   [[:workspace :w] [:= :w.id :s.workspace_id]]
           :where  [:= :s.id sid]})))))

(defn session-state-list-for-workspace
  "List session_state rows whose `workspace_id` = `workspace-id`, newest first."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let [ws-id (->ref workspace-id)]
      (mapv
        (fn [r]
          {:id              (->uuid (:id r))
           :session-soul-id (->uuid (:session_soul_id r))
           :title           (:title r)
           :version         (:version r)
           :workspace-id    (->uuid (:workspace_id r))
           :created-at      (->date (:created_at r))})
        (query! db-info
          {:select   [:*]
           :from     :session_state
           :where    [:= :workspace_id ws-id]
           :order-by [[:version :desc]]})))))

(defn session-state-set-workspace!
  "Pin `session-state-id` to `workspace-id`."
  [write-tx! db-info session-state-id workspace-id]
  (when (and (ds db-info) session-state-id workspace-id)
    (write-tx! db-info
      (fn [tx-info]
        (execute! tx-info
          {:update :session_state
           :set    {:workspace_id (->ref workspace-id)}
           :where  [:= :id (->ref session-state-id)]})
        {:session-state-id (->uuid session-state-id)
         :workspace-id     (->uuid workspace-id)}))))

(defn session-state-cas-workspace!
  "Atomically repin `session-state-id` from `expected-workspace-id` to
   `workspace-id`. Returns true only when the expected workspace was still the
   current value at commit time."
  [write-tx! db-info session-state-id expected-workspace-id workspace-id]
  (when (and (ds db-info) session-state-id expected-workspace-id workspace-id)
    (write-tx! db-info
      (fn [tx-info]
        (let [result (execute! tx-info
                       {:update :session_state
                        :set    {:workspace_id (->ref workspace-id)}
                        :where  [:and
                                 [:= :id (->ref session-state-id)]
                                 [:= :workspace_id (->ref expected-workspace-id)]]})]
          (= 1 (or (:next.jdbc/update-count (first result)) 0)))))))

(defn- graph-revision-row
  [row]
  (when row
    {:workspace-id        (->uuid (:workspace_id row))
     :session-state-id    (->uuid (:session_state_id row))
     :parent-workspace-id (some-> (:parent_workspace_id row) ->uuid)
     :ctx                 (<-blob (:ctx row))
     :advance             (<-blob (:advance row))
     :receipt             (<-blob (:receipt row))
     :created-at-ms       (:created_at row)
     :updated-at-ms       (:updated_at row)}))

(defn- graph-revision-exists?
  [db-info workspace-id]
  (some? (query-one! db-info
           {:select [:workspace_id]
            :from   :workspace_graph_revision
            :where  [:= :workspace_id (->ref workspace-id)]})))

(defn- insert-graph-revision-if-absent!
  [freeze-safe tx-info {:keys [workspace-id session-state-id parent-workspace-id ctx
                               advance receipt]}]
  (when (and ctx (not (graph-revision-exists? tx-info workspace-id)))
    (let [now (now-ms)]
      (execute! tx-info
        {:insert-into :workspace_graph_revision
         :values [{:workspace_id        (->ref workspace-id)
                   :session_state_id    (->ref session-state-id)
                   :parent_workspace_id (some-> parent-workspace-id ->ref)
                   :ctx                 (->blob (freeze-safe ctx))
                   :advance             (some-> advance freeze-safe ->blob)
                   :receipt             (some-> receipt freeze-safe ->blob)
                   :created_at          now
                   :updated_at          now}]}))))

(defn- put-graph-revision!
  [freeze-safe tx-info {:keys [workspace-id session-state-id parent-workspace-id ctx
                               advance receipt]}]
  (when ctx
    (if (graph-revision-exists? tx-info workspace-id)
      (execute! tx-info
        {:update :workspace_graph_revision
         :set    {:session_state_id    (->ref session-state-id)
                  :parent_workspace_id (some-> parent-workspace-id ->ref)
                  :ctx                 (->blob (freeze-safe ctx))
                  :advance             (some-> advance freeze-safe ->blob)
                  :receipt             (some-> receipt freeze-safe ->blob)
                  :updated_at          (now-ms)}
         :where  [:= :workspace_id (->ref workspace-id)]})
      (insert-graph-revision-if-absent! freeze-safe tx-info
        {:workspace-id workspace-id
         :session-state-id session-state-id
         :parent-workspace-id parent-workspace-id
         :ctx ctx
         :advance advance
         :receipt receipt}))))

(defn refresh-current-graph-revision!
  "Refresh a tracked workspace's CTX at normal turn completion. Metadata stays
   attached to the advance that created the workspace revision."
  [freeze-safe tx-info session-state-id ctx]
  (when-let [workspace-id (:workspace_id
                           (query-one! tx-info
                             {:select [:workspace_id]
                              :from   :session_state
                              :where  [:= :id (->ref session-state-id)]}))]
    (execute! tx-info
      {:update :workspace_graph_revision
       :set    {:ctx        (->blob (freeze-safe ctx))
                :updated_at (now-ms)}
       :where  [:= :workspace_id workspace-id]})))

(defn checkpoint-accept!
  "Atomically repin a session from checkpoint parent to child. When CTX is
   supplied, persist parent and child graph revisions and refresh CTX stores."
  [write-tx! freeze-safe write-through-ctx-stores! db-info
   {:keys [session-state-id parent-workspace-id checkpoint-id
           parent-ctx ctx advance receipt]}]
  (when (and (ds db-info) session-state-id parent-workspace-id checkpoint-id)
    (write-tx! db-info
      (fn [tx-info]
        (let [result (execute! tx-info
                       {:update :session_state
                        :set    {:workspace_id (->ref checkpoint-id)}
                        :where  [:and
                                 [:= :id (->ref session-state-id)]
                                 [:= :workspace_id (->ref parent-workspace-id)]]})
              moved? (= 1 (or (:next.jdbc/update-count (first result)) 0))]
          (when (and moved? ctx)
            (insert-graph-revision-if-absent! freeze-safe tx-info
              {:workspace-id parent-workspace-id
               :session-state-id session-state-id
               :ctx parent-ctx})
            (put-graph-revision! freeze-safe tx-info
              {:workspace-id checkpoint-id
               :session-state-id session-state-id
               :parent-workspace-id parent-workspace-id
               :ctx ctx
               :advance advance
               :receipt receipt})
            (write-through-ctx-stores! tx-info session-state-id ctx))
          moved?)))))

(defn checkpoint-move!
  "Atomically repin a session to an existing checkpoint revision and restore
   its CTX stores. Pointer-only legacy checkpoints remain movable and return a
   nil `:ctx`; graph-tracked checkpoints restore both halves of the state."
  [write-tx! write-through-ctx-stores! db-info
   {:keys [session-state-id expected-workspace-id workspace-id]}]
  (when (and (ds db-info) session-state-id expected-workspace-id workspace-id)
    (write-tx! db-info
      (fn [tx-info]
        (let [revision (graph-revision-row
                         (query-one! tx-info
                           {:select [:*]
                            :from   :workspace_graph_revision
                            :where  [:= :workspace_id (->ref workspace-id)]}))
              result   (execute! tx-info
                         {:update :session_state
                          :set    {:workspace_id (->ref workspace-id)}
                          :where  [:and
                                   [:= :id (->ref session-state-id)]
                                   [:= :workspace_id (->ref expected-workspace-id)]]})
              moved?   (= 1 (or (:next.jdbc/update-count (first result)) 0))]
          (when (and moved? (:ctx revision))
            (write-through-ctx-stores! tx-info session-state-id (:ctx revision)))
          {:moved? moved?
           :ctx (:ctx revision)
           :graph? (boolean revision)})))))

(defn graph-revision
  "Return the graph revision attached to `workspace-id`, including decoded CTX
   and advance receipt, or nil when the workspace is not graph-tracked."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (graph-revision-row
      (query-one! db-info
        {:select [:*]
         :from   :workspace_graph_revision
         :where  [:= :workspace_id (->ref workspace-id)]}))))
