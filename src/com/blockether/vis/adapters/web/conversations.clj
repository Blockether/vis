(ns com.blockether.vis.adapters.web.conversations
  "Web-facing conversation projection/cache module.

   Owns sidebar/page projections, message cache hydration from RLM DB,
   title generation, and context payload shaping for the web adapter."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.rlm.conversations.core :as conversations]
            [com.blockether.vis.rlm :as rlm]
            [com.blockether.vis.rlm.persistence.db :as rlm-db]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; conv-id -> [{:role :text :ts :result?} ...]
(defonce messages-cache (atom {}))

;; conv-id -> {:current str? :iterations [...]} (streamed by executor)
(defonce live-status (atom {}))

(defn generate-conversation-title
  "Generate a short title (<= 5 words) from the first user message."
  [first-message]
  (try
    (let [result (config/ask!
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
  [env]
  (let [cwd (System/getProperty "user.dir")]
    (try
      (rlm/ingest-git! env {:repo-path cwd :n 500})
      (println (str "[web] ingested git history from " cwd))
      (catch Exception e
        (println (str "[web] git ingestion skipped (" cwd "): " (ex-message e)))
        nil))))

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

(defn- iteration-entity->exec [iter-entity]
  (let [codes   (safe-read-edn (:code iter-entity) [])
        results (safe-read-edn (:results iter-entity) [])]
    (mapv (fn [code result]
            {:code   code
             :result (safe-read-edn result result)})
      codes
      (concat results (repeat nil)))))

(defn- iteration-entity->trace-entry [idx iter-entity]
  (cond-> {:iteration  idx
           :thinking   (:thinking iter-entity)
           :executions (iteration-entity->exec iter-entity)}
    (some? (:answer iter-entity)) (assoc :final? true)))

(defn- query-entity->message-pair [db-info query-entity]
  (let [query-ref   [:id (:id query-entity)]
        iterations  (rlm-db/db-list-query-iterations db-info query-ref)
        trace       (vec (map-indexed iteration-entity->trace-entry iterations))
        final-iter  (last (filter :answer iterations))
        answer      (or (some-> final-iter   :answer read-answer)
                        (some-> query-entity :answer read-answer))
        result-map  (cond-> {:trace       trace
                             :iterations  (count iterations)
                             :duration-ms (:duration-ms query-entity)}
                      answer (assoc :answer answer))]
    [{:role :user :text (or (:text query-entity) "")}
     {:role :assistant
      :text (when (string? answer) answer)
      :result result-map}]))

(defn- load-messages-from-db [env]
  (try
    (let [db-info  (:db-info env)
          conv-ref (:conversation-ref env)
          queries  (rlm-db/db-list-conversation-queries db-info conv-ref)]
      (into [] (mapcat #(query-entity->message-pair db-info %)) queries))
    (catch Exception e
      (println (str "[web] load-messages-from-db failed: " (ex-message e)))
      [])))

(defn messages-for
  [conversation-id]
  (or (get @messages-cache conversation-id)
    (let [env  (conversations/env-for conversation-id)
          _    (maybe-ingest-git! env)
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
  (swap! live-status dissoc conversation-id))

(defn conversations-list
  []
  (->> (conversations/by-channel :vis)
    (mapv (fn [c] {:id (:id c) :name (or (:title c) "New Chat")}))))

(defn set-conversation-title!
  [conversation-id title]
  (conversations/set-title! conversation-id title))

(defn context-payload
  [conversation-id]
  (when (conversations/by-id conversation-id)
    (let [env          (conversations/env-for conversation-id)
          conv-ref     (:conversation-ref env)
          db-info      (:db-info env)
          var-registry (try (when (and db-info conv-ref)
                              (rlm-db/db-latest-var-registry db-info conv-ref))
                         (catch Exception _ nil))
          vars         (when (seq var-registry)
                         (->> var-registry
                           (sort-by first)
                           (mapv (fn [[sym {:keys [value code]}]]
                                   (let [s (pr-str value)]
                                     {:name  (str sym)
                                      :value (if (> (count s) 200) (str (subs s 0 197) "...") s)
                                      :code  code
                                      :type  (cond
                                               (nil? value) "nil"
                                               (map? value) "map"
                                               (vector? value) "vector"
                                               (set? value) "set"
                                               (sequential? value) "seq"
                                               (string? value) "string"
                                               (integer? value) "int"
                                               (float? value) "float"
                                               (boolean? value) "bool"
                                               (keyword? value) "keyword"
                                               :else (.getSimpleName (class value)))})))))]
      (cond-> {:context [] :learnings []}
        (seq vars) (assoc :variables vars)))))
