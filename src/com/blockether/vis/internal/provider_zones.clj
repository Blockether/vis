(ns com.blockether.vis.internal.provider-zones
  "Provider request zone classification.

   This is the single classifier for both runtime persistence and historical
   report inference. Reports may mark rows as inferred, but zone boundaries must
   not drift from the write path."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.internal.tokens :as tokens]))

(defn content-text
  [content]
  (cond
    (string? content) content
    (sequential? content)
    (or (some (fn [part]
                (cond
                  (string? part) part
                  (map? part) (or (:text part) (:content part))))
          content)
      (try (json/write-json-str content)
        (catch Throwable _ (pr-str content))))
    :else
    (try (json/write-json-str content)
      (catch Throwable _ (pr-str content)))))

(defn- utf8-byte-count
  ^long [s]
  (long (alength (.getBytes (str s) "UTF-8"))))

(defn- request-zone
  [{:keys [message-index zone-index role zone cache-class source scope content inferred?]}]
  (let [content (str (or content ""))
        prefix (when inferred? "inferred/")]
    (cond-> {:message-index message-index
             :zone-index zone-index
             :role (str role)
             :zone zone
             :zone-id (str prefix "m" message-index "/z" zone-index "/" (name zone))
             :cache-class cache-class
             :content content
             :char-count (count content)
             :byte-count (utf8-byte-count content)
             :estimated-tokens (tokens/count-tokens content)}
      source (assoc :source source)
      scope  (assoc :scope scope)
      inferred? (assoc :inferred? true))))

(defn- marker-splits
  [text markers]
  (let [hits (->> markers
               (keep (fn [{:keys [needle] :as marker}]
                       (when-let [idx (str/index-of text needle)]
                         (assoc marker :idx idx))))
               (sort-by :idx)
               vec)]
    (when (seq hits)
      (mapv (fn [i]
              (let [{:keys [idx] :as hit} (nth hits i)
                    end (if (< (inc i) (count hits))
                          (:idx (nth hits (inc i)))
                          (count text))]
                (assoc hit :content (subs text idx end))))
        (range (count hits))))))

(defn- results-scope
  [text]
  (some-> (re-find #"<results(?: scope=\"([^\"]+)\")?" text) second))

(defn- current-user-message?
  [text]
  (str/includes? text ";; -- CURRENT-USER-MESSAGE --"))

(defn- results-ledger-zone
  [text before-current-user?]
  (cond
    (str/includes? text " folded>")
    :compaction-ledger

    (or (str/includes? text "graph_diff")
      (str/includes? text "resolved_evidence")
      (str/includes? text "transaction_mode"))
    :dag-ledger

    before-current-user?
    :frozen-ledger

    :else
    :current-turn-ledger))

(defn- system-zone
  [text]
  (cond
    (str/includes? text ";; -- PROJECT-INSTRUCTIONS --") :project-instructions
    (str/includes? text ";; -- TURN-SYSTEM-CONTEXT --") :capability-system-context
    (str/includes? text ";; -- CLI-AUTONOMOUS --") :capability-system-context
    (str/includes? text ";; -- SYSTEM-PROMPT --") :stable-system
    :else :stable-system))

(defn- assistant-source
  [content]
  (if (and (sequential? content)
        (some (fn [part]
                (and (map? part)
                  (not (str/blank? (str (or (:text part) (:content part) ""))))))
          content))
    :svar/assistant-message
    :svar/preserved-thinking))

(defn- user-message-zones
  [idx role text {:keys [current-user-message-index inferred?]}]
  (or
    (some->> (marker-splits text
               [{:needle ";; -- PREVIOUS-TURN-CONTEXT --"
                 :zone :previous-turn-context
                 :cache-class :turn-prefix
                 :source :prompt/previous-turn}
                {:needle ";; -- CURRENT-USER-MESSAGE --"
                 :zone :current-user-request
                 :cache-class :turn-prefix
                 :source :prompt/current-user}])
      (map-indexed (fn [zone-idx split]
                     (request-zone
                       {:message-index idx
                        :zone-index zone-idx
                        :role role
                        :zone (:zone split)
                        :cache-class (:cache-class split)
                        :source (:source split)
                        :content (:content split)
                        :inferred? inferred?})))
      vec
      not-empty)
    [(cond
       (str/starts-with? text "<results")
       (let [before-current-user? (and current-user-message-index
                                    (< idx current-user-message-index))
             zone (results-ledger-zone text before-current-user?)]
         (request-zone {:message-index idx
                        :zone-index 0
                        :role role
                        :zone zone
                        :cache-class :append-only-prefix
                        :source (case zone
                                  :compaction-ledger :ctx/compaction-ledger
                                  :dag-ledger :ctx/dag-ledger
                                  :frozen-ledger :ctx/frozen-ledger
                                  :current-turn-ledger :ctx/current-turn-ledger)
                        :scope (results-scope text)
                        :content text
                        :inferred? inferred?}))

       (str/starts-with? text "<context")
       (request-zone {:message-index idx
                      :zone-index 0
                      :role role
                      :zone :mutable-context
                      :cache-class :mutable-tail
                      :source :ctx/render-mutable
                      :content text
                      :inferred? inferred?})

       (str/includes? text ";; -- ITERATION-ERROR")
       (request-zone {:message-index idx
                      :zone-index 0
                      :role role
                      :zone :provider-extra
                      :cache-class :non-cacheable
                      :source :loop/error-feedback
                      :content text
                      :inferred? inferred?})

       :else
       (request-zone {:message-index idx
                      :zone-index 0
                      :role role
                      :zone :provider-extra
                      :cache-class :unknown
                      :source (if inferred? :report/inferred :loop/unclassified-user)
                      :content text
                      :inferred? inferred?}))]))

(defn provider-request-zones
  "Classify lowered provider messages into persisted/reportable context zones.

   `:inferred? true` is for historical rows that lack persisted zones. It uses
   the same classifier but marks rows and prefixes `zone-id` with `inferred/` so
   reports cannot confuse runtime-truth rows with best-effort reconstruction."
  ([messages] (provider-request-zones messages nil))
  ([messages {:keys [inferred?]}]
   (let [indexed (map-indexed vector (or messages []))
         current-user-message-index
         (some (fn [[idx {:keys [role content]}]]
                 (when (and (= "user" (str role))
                         (current-user-message? (content-text content)))
                   idx))
           indexed)]
     (vec
       (mapcat
         (fn [[idx {:keys [role content]}]]
           (let [role (str role)
                 text (content-text content)
                 before-current-user? (and current-user-message-index
                                        (< idx current-user-message-index))]
             (cond
               (= "system" role)
               [(request-zone {:message-index idx
                               :zone-index 0
                               :role role
                               :zone (system-zone text)
                               :cache-class :stable-prefix
                               :source :prompt/stable
                               :content text
                               :inferred? inferred?})]

               (= "assistant" role)
               [(request-zone {:message-index idx
                               :zone-index 0
                               :role role
                               :zone (if before-current-user?
                                       :frozen-ledger
                                       :current-turn-ledger)
                               :cache-class :append-only-prefix
                               :source (assistant-source content)
                               :content text
                               :inferred? inferred?})]

               (= "user" role)
               (user-message-zones idx role text
                 {:current-user-message-index current-user-message-index
                  :inferred? inferred?})

               :else
               [(request-zone {:message-index idx
                               :zone-index 0
                               :role role
                               :zone :provider-extra
                               :cache-class :unknown
                               :source (if inferred? :report/inferred :loop/unclassified)
                               :content text
                               :inferred? inferred?})])))
         indexed)))))
