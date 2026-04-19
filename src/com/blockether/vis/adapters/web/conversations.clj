(ns com.blockether.vis.adapters.web.conversations
  "Web-facing conversation projection/cache module.

   Owns sidebar/page projections, message cache hydration from RLM DB,
   title generation, and context payload shaping for the web adapter."
  (:require [com.blockether.vis.loop.conversations.core :as conversations]
   [com.blockether.vis.core :as core]
            [com.blockether.vis.loop.core :as loop-core]
            [com.blockether.vis.loop.runtime.query.routing :as rlm-routing]
            [com.blockether.vis.loop.storage.db :as rlm-db]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; conv-id -> [{:role :text :ts :result?} ...]
(defonce messages-cache (atom {}))

;; conv-id -> {:current str? :iterations [...]} (streamed by executor)
(defonce live-status (atom {}))

;; Tracks conversation-ids where we've already ingested git history this
;; process lifetime. Without this, every page view that missed the messages
;; cache re-ingested the whole repo — slow on big repos, noisy in logs,
;; and redundant because `core/ingest-git!` already dedupes by :repo/name
;; in the DB. This atom just dedupes the WORK (file walking + parsing)
;; upstream.
(defonce ^:private git-ingested (atom #{}))

(defn generate-conversation-title
  "Generate a short title (<= 5 words) from the first user message."
  [first-message]
  (try
    (let [result (rlm-routing/ask!
                   {:messages [{:role "system" :content "Generate a short title (max 5 words) for this chat. Reply with ONLY the title."}
                               {:role "user" :content first-message}]
                    :spec {:title {:type :string :description "Short chat title, max 5 words, no quotes or markup"}}
                    :prefer :speed :capabilities #{:chat}})
          title  (str/trim (or (:title (:result result)) ""))
          raw    (if (str/blank? title) first-message title)]
      (if (> (count raw) 65)
        (str (subs raw 0 62) "...")
        raw))
    (catch Exception _
      (if (> (count first-message) 65)
        (str (subs first-message 0 62) "...")
        first-message))))

(defn- maybe-ingest-git!
  "Ingest the current project's git history into the conversation's env.
   No-ops after the first call for a given conversation in this process —
   the underlying core/ingest-git! dedupes by :repo/name in the DB anyway,
   but re-walking the repo every page view wastes wall time and fills the
   log with noise."
  [env conversation-id]
  (when-not (contains? @git-ingested conversation-id)
    (let [cwd (System/getProperty "user.dir")]
      (try
        (core/ingest-git! env {:repo-path cwd :n 500})
        (swap! git-ingested conj conversation-id)
        (println (str "[web] ingested git history from " cwd
                   " (conv " (subs (str conversation-id) 0 (min 8 (count (str conversation-id)))) ")"))
        (catch Exception e
          (println (str "[web] git ingestion skipped (" cwd "): " (ex-message e)))
          nil)))))

(defn- safe-read-edn
  "Parse `s` as edn. Returns `fallback` when `s` is blank or unparseable."
  [s fallback]
  (if (and (string? s) (seq s))
    (try (edn/read-string s) (catch Exception _ fallback))
    fallback))

(defn- edn-data-like?
  "True when `s` starts with an edn data shape marker. Distinguishes prose
   answers (stored raw) from code answers (stored via pr-str, always starting
   with `{`, `[`, `(`, `#` or `\"`)."
  [s]
  (when (and (string? s) (seq s))
    (boolean (#{\{ \[ \( \# \"} (first (str/triml s))))))

(defn- read-answer
  "Assistant answers are written by `rlm/answer-str`: prose strings pass
   through raw, non-strings are pr-str'd. Reverse that here — only edn-parse
   when the stored string looks like an edn data literal, otherwise return
   the raw prose."
  [s]
  (if (edn-data-like? s)
    (safe-read-edn s s)
    s))

(defn- exec-envelope?
  "Recognize the new-shape execution record stored per call. Shape:
   `{:result v :error e? :stdout s?}`. Old rows stored bare results, so
   maps that happen to look similar still parse correctly — the envelope
   must have the `:result` key but nothing more restrictive."
  [x]
  (and (map? x) (contains? x :result)))

(defn- iteration-entity->exec [iter-entity]
  (let [codes   (safe-read-edn (:code iter-entity) [])
        results (safe-read-edn (:results iter-entity) [])]
    (mapv (fn [code raw]
            (let [parsed (safe-read-edn raw raw)
                  envelope? (exec-envelope? parsed)
                  result-val (if envelope? (:result parsed) parsed)
                  exec-err   (when envelope? (:error parsed))
                  lint-err   (loop-core/literal-code-block-error code)]
              (cond-> {:code    code
                       :result  result-val
                       :error   (or exec-err lint-err)}
                envelope? (assoc :stdout  (:stdout parsed)
                                 :stderr  (:stderr parsed)
                                 :time-ms (:time-ms parsed)
                                 :timeout? (boolean (:timeout? parsed))
                                 :repaired? (boolean (:repaired? parsed))))))
      codes
      (concat results (repeat nil)))))

(defn- type-label
  "Short human label for a value's type. Mirrors `context-payload`'s
   cond chain so iteration cards and sidebar cards agree."
  [value]
  (cond
    (nil? value)        "nil"
    (map? value)        "map"
    (vector? value)     "vector"
    (set? value)        "set"
    (sequential? value) "seq"
    (string? value)     "string"
    (integer? value)    "int"
    (float? value)      "float"
    (boolean? value)    "bool"
    (keyword? value)    "keyword"
    :else (try (.getSimpleName (class value)) (catch Throwable _ "?"))))

(defn- truncate-str
  "Trim `s` to at most `n` chars, appending `…` on overflow."
  [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (max 0 (- n 1))) "…") s)))

(defn- value->preview
  "Render a var's value as a human-friendly preview for the iteration card.
   Strings pass through raw (newlines preserved); other values pr-str so
   maps/vecs/seqs are readable. Truncated at 400 chars — the CSS gives the
   preview cell max-height with scroll, so we can afford a roomier window."
  [value]
  (let [raw (cond
              (string? value) value
              :else           (pr-str value))]
    (truncate-str raw 400)))

(defn- var-preview
  [name value]
  (let [n (str name)]
    (cond
      (and (= n "*answer*") (map? value) (contains? value :result))
      (let [r (:result value)
            text (cond
                   (string? r) r
                   (sequential? r) (str/join "\n" (map str r))
                   (nil? r) ""
                   :else (pr-str r))]
        (truncate-str text 400))

      :else
      (value->preview value))))

(defn- var-history-for
  [cache db-info conv-ref sym]
  (if-let [rows (get @cache sym)]
    rows
    (let [rows (try (rlm-db/db-var-history db-info conv-ref sym)
                 (catch Exception _ []))]
      (swap! cache assoc sym rows)
      rows)))

(defn- resolve-var-version
  [history target-query-id iter-created-at current-value current-code]
  (let [same-iter (->> history
                    (filter (fn [{:keys [query-id created-at]}]
                              (and (= query-id target-query-id)
                                (= created-at iter-created-at)))))
        exact     (first (filter (fn [{:keys [value code]}]
                                   (and (= value current-value)
                                     (= code current-code)))
                            same-iter))
        current   (or exact (last same-iter))
        version   (:version current)
        prev      (when (and (number? version) (> version 1))
                    (nth history (- version 2) nil))]
    {:version      version
     :prev-version (:version prev)
     :prev-preview (some-> prev :value value->preview)}))

(defn- iteration-entity->vars
  "Project the vars written in this iteration into a compact table shape
   the frontend can render without re-parsing values."
  [db-info conv-ref query-id var-history-cache iter-entity]
  (try
    (let [rows         (rlm-db/db-list-iteration-vars db-info [:id (:id iter-entity)])
          iter-created (:created-at iter-entity)]
      (into []
        (keep (fn [{:keys [name value code]}]
                (when name
                  (let [sym       (symbol name)
                        history   (var-history-for var-history-cache db-info conv-ref sym)
                        versioned (resolve-var-version history query-id iter-created value code)]
                    {:name         name
                     :type         (type-label value)
                     :preview      (var-preview name value)
                     :code         code
                     :version      (:version versioned)
                     :prev-version (:prev-version versioned)
                     :prev-preview (:prev-preview versioned)})))
         rows)))
    (catch Exception _ [])))

(defn- iteration-entity->trace-entry [db-info conv-ref query-id var-history-cache idx iter-entity]
  (let [err (some-> (:error iter-entity) (safe-read-edn nil))]
    (cond-> {:iteration  idx
             :thinking   (:thinking iter-entity)
             :executions (iteration-entity->exec iter-entity)
             :vars       (iteration-entity->vars db-info conv-ref query-id var-history-cache iter-entity)}
      (some? (:answer iter-entity)) (assoc :final? true)
      err (assoc :error err))))

(defn- query-entity->message-pair [db-info conv-ref var-history-cache query-entity]
  (let [query-id    (:id query-entity)
        query-ref   [:id query-id]
        iterations  (rlm-db/db-list-query-iterations db-info query-ref)
        trace       (vec (map-indexed #(iteration-entity->trace-entry db-info conv-ref query-id var-history-cache %1 %2) iterations))
        final-iter  (last (filter :answer iterations))
        answer      (or (some-> final-iter   :answer read-answer)
                      (some-> query-entity :answer read-answer))
        tokens      (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} query-entity]
                      (when (or input-tokens output-tokens reasoning-tokens cached-tokens)
                        (cond-> {}
                          input-tokens     (assoc :input     input-tokens)
                          output-tokens    (assoc :output    output-tokens)
                          reasoning-tokens (assoc :reasoning reasoning-tokens)
                          cached-tokens    (assoc :cached    cached-tokens))))
        cost        (let [{:keys [total-cost model]} query-entity]
                      (when (or total-cost model)
                        (cond-> {}
                          total-cost (assoc :total-cost total-cost)
                          model      (assoc :model      model))))
        status      (:status query-entity)
        result-map  (cond-> {:trace       trace
                             :iterations  (count iterations)
                             :duration-ms (:duration-ms query-entity)}
                      answer (assoc :answer answer)
                      tokens (assoc :tokens tokens)
                      cost   (assoc :cost   cost)
                      ;; Forward non-:success statuses so the UI can badge
                      ;; max-iterations / error-budget-exhausted turns.
                      (and status (not= status :success))
                      (assoc :status status))]
    [{:role :user :text (or (:text query-entity) "")}
     {:role :assistant
      :text (when (string? answer) answer)
      :result result-map}]))

(defn- load-messages-from-db [env]
  (try
    (let [db-info  (:db-info env)
          conv-ref (:conversation-ref env)
          queries  (rlm-db/db-list-conversation-queries db-info conv-ref)
          var-history-cache (atom {})]
      (into [] (mapcat #(query-entity->message-pair db-info conv-ref var-history-cache %)) queries))
    (catch Exception e
      (println (str "[web] load-messages-from-db failed: " (ex-message e)))
      [])))

(defn messages-for
  [conversation-id]
  (or (get @messages-cache conversation-id)
    (let [env  (conversations/env-for conversation-id)
          _    (maybe-ingest-git! env conversation-id)
          msgs (load-messages-from-db env)]
      (swap! messages-cache
        (fn [m]
          (if (contains? m conversation-id)
            m
            (assoc m conversation-id msgs))))
      (get @messages-cache conversation-id))))

(defn append-message!
  [conversation-id msg]
  (swap! messages-cache update conversation-id (fnil conj []) msg))

(defn create-conversation!
  ([] (create-conversation! "New Chat"))
  ([title]
   (let [{:keys [id title created-at]} (conversations/create! :vis {:title title})]
     (swap! messages-cache assoc id [])
     {:id id :name title :messages [] :created-at created-at})))

(defn get-conversation
  [conversation-id]
  (when-let [c (conversations/by-id conversation-id)]
    {:id         (:id c)
     :name       (or (:title c) "New Chat")
     :messages   (messages-for conversation-id)
     :created-at (:created-at c)}))

(defn delete-conversation!
  [conversation-id]
  (conversations/delete! conversation-id)
  (swap! messages-cache dissoc conversation-id)
  (swap! live-status dissoc conversation-id)
  (swap! git-ingested disj conversation-id))

(defn conversations-list
  []
  (->> (conversations/by-channel :vis)
    (mapv (fn [c] {:id (:id c) :name (or (:title c) "New Chat")}))))

(defn set-conversation-title!
  [conversation-id title]
  (conversations/set-title! conversation-id title))

(defn- var-version-entries
  "Fetch every historical write of `sym` in this conversation, oldest → newest,
   shaped for the sidebar. Preview uses 200 chars to match the flat-list limit."
  [db-info conv-ref sym]
  (try
    (->> (rlm-db/db-var-history db-info conv-ref sym)
      (mapv (fn [{:keys [version value code created-at]}]
              {:version    version
               :type       (type-label value)
               :preview    (truncate-str (pr-str value) 200)
               :code       code
               :created-at (str created-at)})))
    (catch Exception _ [])))

(defn- system-var?
  "Agent-loop SYSTEM vars are earmuffed (`*foo*`) and rebound by the loop
   itself rather than by user code. They are surfaced in their own collapsed
   sub-section so they don't drown out the user-defined variables."
  [sym]
  (let [n (str sym)]
    (and (> (count n) 2)
      (= \* (.charAt n 0))
      (= \* (.charAt n (dec (count n)))))))

(defn- system-display-name
  "Human-friendly label for the three canonical SYSTEM vars; falls back to
   the raw symbol name (without earmuffs) for any other earmuffed var."
  [sym]
  (let [n (str sym)]
    (case n
      "*query*"     "query"
      "*reasoning*" "thinking"
      "*answer*"    "answer"
      (let [stripped (subs n 1 (dec (count n)))]
        (if (seq stripped) stripped n)))))

(defn- ->var-entry
  [db-info conv-ref system? [sym {:keys [value code version]}]]
  (let [versions (var-version-entries db-info conv-ref sym)]
    (cond-> {:name     (str sym)
             :value    (truncate-str (pr-str (if (and system? (= "*answer*" (str sym))
                                                (map? value) (contains? value :result))
                                               (:result value)
                                               value))
                         200)
             :code     code
             :type     (type-label value)
             :version  (or version (count versions))
             :versions versions
             :system?  (boolean system?)}
      system? (assoc :display-name (system-display-name sym)))))

(defn context-payload
  "Sidebar payload for `GET /conversations/:id/context`.
   Returns `{:variables [...] :system-variables [...]}` — user-defined vars
   and agent-loop SYSTEM vars (earmuffed names like `*query*`) are split into
   two distinct sections so the SYSTEM trio doesn't dominate the view."
  [conversation-id]
  (when (conversations/by-id conversation-id)
    (let [env          (conversations/env-for conversation-id)
          conv-ref     (:conversation-ref env)
          db-info      (:db-info env)
          var-registry (try (when (and db-info conv-ref)
                              (rlm-db/db-latest-var-registry db-info conv-ref))
                         (catch Exception _ nil))
          {system-pairs true user-pairs false}
          (when (seq var-registry)
            (group-by (fn [[sym _]] (system-var? sym)) (sort-by first var-registry)))
          user-vars    (mapv #(->var-entry db-info conv-ref false %) user-pairs)
          system-vars  (mapv #(->var-entry db-info conv-ref true  %) system-pairs)]
      (cond-> {:variables user-vars}
        (seq system-vars) (assoc :system-variables system-vars)))))
