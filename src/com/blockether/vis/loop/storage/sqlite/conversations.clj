(ns com.blockether.vis.loop.storage.sqlite.conversations
  "Conversation / query / iteration / iteration-var entity tree.

   One :conversation entity owns many :query children (one per user turn);
   each :query owns many :iteration children; each :iteration owns many
   :iteration-var children."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [taoensso.trove :as trove]))

;; =============================================================================
;; Conversation
;; =============================================================================

(defn store-conversation!
  "Create a new :conversation entity and return its lookup ref. Callers that
   want to resume an existing conversation should pass `[:id uuid]` to
   `db-resolve-conversation-ref` directly instead of calling this."
  [db-info {:keys [system-prompt model]}]
  (when (core/ds db-info)
    (core/store-entity! db-info
      {:type          :conversation
       :system-prompt (or system-prompt "")
       :model         (or model "")})))

(defn db-get-conversation
  "Returns a conversation entity by lookup ref or nil."
  [db-info conversation-ref]
  (when (and (core/ds db-info) (vector? conversation-ref))
    (core/fetch-entity db-info (core/entity-ref->id conversation-ref))))

(defn db-find-latest-conversation-ref
  "Returns lookup ref for the most recently created conversation, or nil."
  [db-info]
  (when (core/ds db-info)
    (when-let [row (core/query-one! db-info
                     {:select [:id]
                      :from :entity
                      :where [:= :type "conversation"]
                      :order-by [[:created_at :desc] [:id :desc]]
                      :limit 1})]
      (core/id->entity-ref (:id row)))))

(defn db-resolve-conversation-ref
  "Resolve a conversation selector to a lookup ref. Accepts:
     nil              → nil (caller should then create a new conversation)
     :latest          → the most recent :conversation entity
     [:id uuid]       → returned unchanged
     uuid             → wrapped as [:id uuid]"
  [db-info selector]
  (cond
    (nil? selector) nil
    (= :latest selector) (db-find-latest-conversation-ref db-info)
    (and (vector? selector) (= :id (first selector))) selector
    (uuid? selector) [:id selector]
    :else nil))

;; =============================================================================
;; Query
;; =============================================================================

(defn store-query!
  "Stores a query entity linked to a parent (conversation by default,
   or an :iteration when the caller is spawning a sub-RLM).

   `:parent-ref` takes precedence over `:conversation-ref` for the
   parent-id link. When only `:conversation-ref` is supplied (the
   normal top-level query case), it's used as the parent. Sub-RLM
   calls pass both — `:conversation-ref` for ownership/membership and
   `:parent-ref [:id iteration-uuid]` for the hierarchical link."
  [db-info {:keys [conversation-ref parent-ref text messages answer iterations duration-ms status eval-score]}]
  (let [parent-id (second (or parent-ref conversation-ref))]
    (core/store-entity! db-info
      (cond-> {:type :query
               :name (let [t (or text "")]
                       (subs t 0 (min (count t) 100)))
               :parent-id parent-id
               :text (or text "")
               :answer (or (when answer (pr-str answer)) "")
               :iterations (or iterations 0)
               :duration-ms (or duration-ms 0)
               :status (or status :unknown)}
        messages (assoc :messages (pr-str messages))
        eval-score (assoc :eval-score (float eval-score))))))

(defn update-query!
  "Updates a query entity with final outcome, including optional cost/token
   metadata so the UI can reconstruct meta lines after a restart."
  [db-info query-ref {:keys [answer iterations duration-ms status eval-score tokens cost]}]
  (core/update-entity! db-info query-ref
    (cond-> {:answer (or (when answer (pr-str answer)) "")
             :iterations (or iterations 0)
             :duration-ms (or duration-ms 0)
             :status (or status :unknown)}
      eval-score             (assoc :eval-score (float eval-score))
      (:input tokens)        (assoc :input-tokens     (long (:input tokens)))
      (:output tokens)       (assoc :output-tokens    (long (:output tokens)))
      (:reasoning tokens)    (assoc :reasoning-tokens (long (:reasoning tokens)))
      (:cached tokens)       (assoc :cached-tokens    (long (:cached tokens)))
      (:total-cost cost)     (assoc :total-cost       (double (:total-cost cost)))
      (:model cost)          (assoc :model            (str (:model cost))))))

;; =============================================================================
;; Iteration + iteration-vars
;; =============================================================================

;; Decoupled SCI-var detection. We can't `(require 'sci.core)` here
;; without dragging the SCI runtime into the storage namespace, AND we
;; can't cache `(Class/forName "sci.lang.Var")` at top-level — when this
;; ns loads (Flyway migrations, app boot), `sci.core` hasn't initialised
;; its defrecords yet, so the class lookup returns ClassNotFoundException
;; and we silently drop the substitution. By the time `store-iteration!`
;; actually runs, SCI is loaded — but the cached nil sticks. Bug #1.
;;
;; Fix: detect by class name string (`"sci.lang.Var"`). Pure data check,
;; no class lookup, no boot-order dependency. `(.getName (class v))` is
;; ~50ns; we run it once per per-exec result, not in a hot loop.
(defn- sci-var?
  "True when v is an SCI var. Detection is by class name to avoid
   coupling this ns to `sci.core`'s load order — see ns docstring of
   the surrounding edn-safe block."
  [v]
  (and (some? v)
    (= "sci.lang.Var" (.getName (class v)))))

(declare edn-safe)

(defn- sci-var->surrogate
  "Convert a SCI var to a roundtrip-safe surrogate map.

   `(def foo 42)` returns the var object, which pr-str's as
   `#'sandbox/foo` — that breaks edn reading. We persist a tagged
   map `{::var-ref \"foo\" ::var-value 42}` instead, so the read
   side recovers BOTH the bound name (`*foo* =` in journal) and the
   bound value (the `42`) without losing fidelity. Plain string
   surrogates worked for display but stripped the value, leaving the
   journal renderer with no way to render `*foo* = 42` from a string
   that says `\"#'sandbox/foo\"`.

   Reflective call to `.getRawRoot` — sci.lang.Var isn't on this ns'
   compile-time classpath (see `sci-var?` for why), so we eat the
   reflection cost (~µs once per def). Negligible vs the surrounding
   pr-str / I/O cost. `depth` threads through to guard against pathological
   nested data when the var's value is itself a deep structure."
  [v depth]
  (let [s (str v)                                         ;; e.g. "#'sandbox/foo"
        bare-name (-> s (subs 2)                          ;; drop "#'"
                    (str/replace #"^[^/]+/" ""))          ;; drop "sandbox/"
        bound (try (.getRawRoot v) (catch Throwable _ nil))]
    ;; `:rlm/*` keys (not ::-namespaced) so consumers in other ns'
    ;; (loop.core's journal renderer, web's render-exec) can match on
    ;; them without aliasing this storage namespace.
    {:rlm/var-ref bare-name
     :rlm/var-value (edn-safe bound (dec depth))}))

(defn- edn-safe
  "Recursively replace values that pr-str cannot round-trip through edn
   with a printable surrogate. Currently handles SCI vars (`(def foo 42)`
   returns one) — they pr-str as `#'sandbox/foo`, which the edn reader
   rejects (`No dispatch macro for: '`). Replacing with a tagged map
   `{::var-ref name ::var-value bound}` keeps the envelope roundtrippable
   AND preserves enough info for the journal to render `*foo* = 42`.

   Walks maps/vectors/sets/seqs to a small fixed depth so nested var refs
   inside collections also survive. Anything else is returned untouched."
  ([v] (edn-safe v 6))
  ([v depth]
   (cond
     (zero? depth) v
     (sci-var? v) (sci-var->surrogate v depth)
     (map? v)    (persistent! (reduce-kv (fn [m k val] (assoc! m k (edn-safe val (dec depth)))) (transient {}) v))
     (vector? v) (mapv #(edn-safe % (dec depth)) v)
     (set? v)    (into #{} (map #(edn-safe % (dec depth))) v)
     (seq? v)    (doall (map #(edn-safe % (dec depth)) v))
     :else v)))

(defn store-iteration!
  "Stores an iteration entity linked to a query via parent-id, plus child
   iteration-var entities for any restorable vars."
  [db-info {:keys [query-ref executions thinking answer duration-ms vars error]}]
  (let [parent-id (when query-ref (second query-ref))
        executions (or executions [])
        code-strs (mapv :code executions)
        ;; Each result element now stores {:result v :error e} instead of
        ;; just `v`. Per-execution errors (timeouts, exceptions, literal
        ;; guards) used to vanish here — the frontend saw an empty card
        ;; between successful ones and it looked like the order was
        ;; scrambled. The read-side tolerates both shapes so legacy rows
        ;; still render as pure results.
        blank? (fn [s] (or (nil? s) (and (string? s) (str/blank? s))))
        result-strs (mapv (fn [exec]
                            (try
                              (pr-str
                                (cond-> {:result (edn-safe (:result exec))}
                                  (some? (:error exec))             (assoc :error (str (:error exec)))
                                  (not (blank? (:stdout exec)))     (assoc :stdout (:stdout exec))
                                  (not (blank? (:stderr exec)))     (assoc :stderr (:stderr exec))
                                  (some? (:execution-time-ms exec)) (assoc :time-ms (:execution-time-ms exec))
                                  (true? (:timeout? exec))          (assoc :timeout? true)
                                  (true? (:repaired? exec))         (assoc :repaired? true)))
                              (catch Exception e
                                (trove/log! {:level :warn :data {:error (ex-message e)}
                                             :msg "Failed to serialize execution result"})
                                "???")))
                      executions)
        iter-ref (core/store-entity! db-info
                   (cond-> {:type :iteration
                            :parent-id parent-id
                            :code (pr-str code-strs)
                            :results (pr-str result-strs)
                            :thinking (or thinking "")
                            :duration-ms (or duration-ms 0)}
                     answer (assoc :answer answer)
                     error  (assoc :error (pr-str error))))]
    (doseq [{:keys [name value code time-ms metadata]} (or vars [])]
      (when name
        (let [rich-code (pr-str (cond-> {}
                                  code     (assoc :expr code)
                                  time-ms  (assoc :time-ms time-ms)
                                  metadata (assoc :metadata metadata)))]
          (core/store-entity! db-info
            {:type :iteration-var
             :name (str name)
             :parent-id (second iter-ref)
             :value (pr-str (edn-safe value))
             ;; `:code` column now carries a pr-str'd EDN map
             ;; `{:expr :time-ms :metadata}` so downstream consumers
             ;; (var_index, var-history) get rich per-var provenance
             ;; without a schema migration. Legacy bare-string rows are
             ;; handled on read via `parse-rich-code` — no data loss.
             :code rich-code}))))
    iter-ref))

(defn db-list-iteration-vars
  "Lists persisted restorable vars for an iteration. Returns plain {:name :value :code} maps,
   matching the db.clj contract."
  [db-info iteration-ref]
  (if (and (core/ds db-info) iteration-ref)
    (let [iter-id (core/entity-ref->id iteration-ref)
          rows (core/query! db-info
                 {:select [:e.created_at :v.name :v.value :v.code]
                  :from [[:entity :e]]
                  :join [[:iteration_var_attrs :v] [:= :e.id :v.entity_id]]
                  :where [:and [:= :e.type "iteration-var"]
                          [:= :e.parent_id iter-id]]
                  :order-by [[:e.created_at :asc] [:e.id :asc]]})]
      (mapv (fn [r] {:name (:name r)
                     :value (core/read-edn-safe (:value r) nil)
                     :code  (:code r)}) rows))
    []))

(defn db-list-conversation-queries
  "Lists query entities for a conversation ordered by created-at."
  [db-info conversation-ref]
  (if (and (core/ds db-info) conversation-ref)
    (let [conv-id (core/entity-ref->id conversation-ref)
          ids (mapv :id (core/query! db-info
                          {:select [:id]
                           :from :entity
                           :where [:and [:= :type "query"]
                                   [:= :parent_id conv-id]]
                           :order-by [[:created_at :asc] [:id :asc]]}))]
      (core/fetch-entities db-info ids))
    []))

(defn db-list-query-iterations
  "Lists iteration entities for a query ordered by created-at."
  [db-info query-ref]
  (if (and (core/ds db-info) query-ref)
    (let [q-id (core/entity-ref->id query-ref)
          ids (mapv :id (core/query! db-info
                          {:select [:id]
                           :from :entity
                           :where [:and [:= :type "iteration"]
                                   [:= :parent_id q-id]]
                           :order-by [[:created_at :asc] [:id :asc]]}))]
      (core/fetch-entities db-info ids))
    []))
