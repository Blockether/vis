(ns com.blockether.vis.conversations
  "Channel-scoped conversation API for every vis frontend.

   A conversation is identified by a UUID (`:id`) — the same rlm-generated
   entity id svar uses for `:conversation`. Channels partition the shared
   DB so web+TUI (`:vis`) and Telegram (`:telegram`) don't see each other's
   conversations when listing:

     (conv/create! :vis)                     ;; → new conversation in :vis
     (conv/create! :vis {:title \"…\"})
     (conv/by-id conv-id)                    ;; conv map or nil
     (conv/by-channel :vis)                  ;; web/TUI sidebar; recent first
     (conv/for-telegram-chat! chat-id)       ;; find-or-create by Telegram id
     (conv/send! conv-id msgs opts)          ;; locked per conv-id
     (conv/close! conv-id)                   ;; release env handle, keep data
     (conv/delete! conv-id)                  ;; close + purge from DB

   Envs are cached per conv-id in this namespace; a dedicated `Object` lock
   serializes concurrent asks for the same conversation. The vis-level
   `vis_conversation` sidecar table (see `conversations.schema`) stores
   channel + external-id + title; the rlm entity tree stores queries,
   iterations, and vars under the same UUID."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.conversations.schema :as schema]
            [com.blockether.vis.languages.commons.edit :as edit]
            [com.blockether.vis.languages.commons.list :as list]
            [com.blockether.vis.languages.commons.read :as read]
            [com.blockether.vis.languages.commons.write :as write]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.rlm :as rlm]
            [com.blockether.vis.rlm.db :as rlm-db]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql])
  (:import [java.time Instant]
           [java.util UUID]))

;; conv-id (string) → {:env env :lock Object}
(defonce ^:private cache (atom {}))

;; Process-wide SQLite handle for sidecar CRUD — opened lazily on first use,
;; reused across every API call. svar's SQLite pool is shared per-path anyway,
;; but keeping one db-info wrapper avoids rebuilding it per `conv/by-id`.
(defonce ^:private shared-db (atom nil))

;; Guard: run the sidecar DDL once per process.
(defonce ^:private schema-installed? (atom false))

;;; ── DB helpers ──────────────────────────────────────────────────────────

(defn- db-info []
  (or @shared-db
      (swap! shared-db
             (fn [cur]
               (or cur
                   (let [d (rlm-db/create-rlm-conn config/db-path)]
                     (when-not @schema-installed?
                       (schema/install! d)
                       (reset! schema-installed? true))
                     d))))))

(defn- row->conv [row]
  (when row
    {:id          (:vis_conversation/conversation_id row)
     :channel     (keyword (:vis_conversation/channel row))
     :external-id (:vis_conversation/external_id row)
     :title       (:vis_conversation/title row)
     :created-at  (:vis_conversation/created_at row)}))

(defn- db-insert! [db-info {:keys [id channel external-id title]}]
  (sql/insert! (:datasource db-info) :vis_conversation
               {:conversation_id id
                :channel         (name channel)
                :external_id     external-id
                :title           title
                :created_at      (.toEpochMilli (Instant/now))}))

(defn- db-find-by-id [db-info id]
  (row->conv (sql/get-by-id (:datasource db-info) :vis_conversation id :conversation_id {})))

(defn- db-find-by-external [db-info channel external-id]
  (row->conv
    (first (sql/find-by-keys (:datasource db-info) :vis_conversation
                             {:channel (name channel)
                              :external_id (str external-id)}))))

(defn- db-list [db-info channel]
  (->> (sql/find-by-keys (:datasource db-info) :vis_conversation
                         {:channel (name channel)}
                         {:order-by [[:created_at :desc]]})
       (mapv row->conv)))

(defn- db-update-title! [db-info id title]
  (sql/update! (:datasource db-info) :vis_conversation
               {:title title}
               {:conversation_id id}))

(defn- db-delete! [db-info id]
  (sql/delete! (:datasource db-info) :vis_conversation
               {:conversation_id id}))

;;; ── Env lifecycle ───────────────────────────────────────────────────────

(def base-tools
  "File-system tools registered on every conversation env (all channels).
   Shell is intentionally NOT here — Telegram and any other untrusted caller
   would otherwise have arbitrary code execution on the host. If a caller
   truly needs shell, wire a scoped tool into the agent-def for that run."
  [read/tool-def
   write/tool-def
   edit/tool-def
   list/tool-def])

(defn- register-base-tools! [env]
  (reduce (fn [e {:keys [sym fn] :as t}]
            (rlm/register-env-fn! e sym fn (dissoc t :sym :fn)))
          env
          base-tools))

(defn- open-env!
  "Open an rlm env resumed on an existing conversation-id, or fresh if `id`
   is nil (rlm generates one)."
  [id]
  (config/resolve-config nil)
  (let [router (config/get-router)
        sel    (when id [:id (UUID/fromString id)])
        env    (rlm/create-env router
                 (cond-> {:db config/db-path}
                   sel (assoc :conversation sel)))]
    (register-base-tools! env)))

(defn- cache-env! [id env]
  (swap! cache assoc id {:env env :lock (Object.)})
  env)

(defn- ensure-env!
  "Ensure an env is open for `id`; reopen from DB if not cached."
  [id]
  (if-let [entry (get @cache id)]
    entry
    (let [env (open-env! id)]
      (swap! cache
             (fn [m]
               (if (contains? m id) m
                   (assoc m id {:env env :lock (Object.)}))))
      (get @cache id))))

;;; ── Public API ──────────────────────────────────────────────────────────

(defn create!
  "Create a new conversation in `channel` (`:vis` or `:telegram`). Returns
   `{:id :channel :external-id :title :created-at}`. `opts` supports
   `:title` and `:external-id`."
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id]}]
   (let [env  (open-env! nil)
         id   (str (second (:conversation-ref env)))
         _    (cache-env! id env)
         _    (db-insert! (db-info) {:id id :channel channel
                                     :external-id (some-> external-id str)
                                     :title title})]
     {:id          id
      :channel     channel
      :external-id (some-> external-id str)
      :title       title})))

(defn by-id
  "Look up a conversation by id (UUID string). Returns the conv map or nil."
  [id]
  (db-find-by-id (db-info) id))

(defn by-channel
  "List all conversations in `channel`, most recent first."
  [channel]
  (db-list (db-info) channel))

(defn for-telegram-chat!
  "Find-or-create the conversation for a Telegram chat-id. Returns the conv
   map. Chat-id may be a long or string — we always store the string form."
  [chat-id]
  (let [ext (str chat-id)]
    (or (db-find-by-external (db-info) :telegram ext)
        (create! :telegram {:external-id ext}))))

(defn set-title!
  "Update the display title. No-op for unknown ids."
  [id title]
  (db-update-title! (db-info) id title)
  nil)

(defn env-for
  "Return the raw rlm env for a conv-id, opening it if needed. Useful for
   code that needs to reach into env internals (db-info, conversation-ref)."
  [id]
  (:env (ensure-env! id)))

(defn send!
  "Advance the conversation with id `id` by one turn: run `messages` through
   the rlm and return the answer. Concurrent sends to the same conversation
   are serialized via a per-id `Object` lock so svar only sees one query in
   flight per conversation.

   `messages` may be either a string (wrapped into a single user message) or
   a vector of svar message maps like `[(llm/user \"…\")]`.

   `opts` is a map forwarded verbatim to `rlm/query-env!`. Every option it
   accepts:

     :system-prompt      String. Prepended to svar's RLM system prompt; use
                         for per-channel persona (e.g. Telegram's short-reply
                         style).
     :spec               Svar output spec for structured responses.
     :model              Override the router's default model.
     :max-iterations     Int. Max code iterations before giving up.
                         Default `schema/MAX_ITERATIONS` (≈50).
     :max-refinements    Int. Max refinement passes. Default 1.
     :threshold          Double. Eval score 0.0-1.0 for refinement early
                         stop. Default 0.8.
     :max-context-tokens Int. Token budget for the context window.
     :max-recursion-depth Int. Cap on sub-rlm recursion.
                         Default `schema/DEFAULT_RECURSION_DEPTH`.
     :verify?            Boolean. Enable claim verification (CITE, CITE-
                         UNVERIFIED). Default false.
     :concurrency        Map merged into svar's concurrency config (e.g.
                         `:max-parallel-llm`).
     :plan?              Boolean. Run a planning-phase LLM call before
                         iterating. Default false.
     :debug?             Boolean. Verbose debug logging with `:rlm-phase`
                         context. Default false.
     :hooks              Map of streaming callbacks.
                         `:on-chunk (fn [{:iteration :thinking :code :final
                         :done?}])` fires during streaming and once per
                         finalized iteration.
     :cancel-atom        `atom` of boolean. Set truthy to request
                         cancellation.
     :eval-timeout-ms    Int. SCI code-block eval timeout.
     :reasoning-default  `:low`/`:medium`/`:high` — base effort for
                         reasoning-capable models. Adaptive escalation
                         still applies.

   Returns the raw svar result map: `{:answer :raw-answer :trace :iterations
   :duration-ms :tokens :cost :eval-scores :refinement-count :confidence
   :sources :verified-claims :reasoning :status}`."
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [env lock]} (ensure-env! id)
         msgs (if (string? messages) [(llm/user messages)] messages)]
     (locking lock
       (rlm/query-env! env msgs opts)))))

(defn close!
  "Release the env handle for `id`. DB data (queries, iterations, sidecar
   row) is preserved — the conversation can be reopened later via any lookup
   fn. Idempotent."
  [id]
  (when-let [{:keys [env]} (clojure.core/get @cache id)]
    (try (rlm/dispose-env! env) (catch Exception _ nil)))
  (swap! cache dissoc id))

(defn delete!
  "Permanently remove the conversation: close the env, retract the rlm
   entity tree (queries + iterations + vars), and drop the sidecar row.
   Idempotent."
  [id]
  (close! id)
  (let [d (db-info)]
    (try (rlm-db/delete-entity-tree! d (UUID/fromString id))
         (catch Exception _ nil))
    (try (db-delete! d id)
         (catch Exception _ nil))))

(defn close-all!
  "Close every cached env and release the shared sidecar handle. Called on
   process shutdown — DB data is preserved."
  []
  (doseq [[_ {:keys [env]}] @cache]
    (try (rlm/dispose-env! env) (catch Exception _ nil)))
  (reset! cache {})
  (when-let [d @shared-db]
    (try (rlm-db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! shared-db nil)
    (reset! schema-installed? false)))
