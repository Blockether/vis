(ns com.blockether.vis.internal.wire-view
  "Derived, process-INVARIANT view of a session for the provider send.

   The provider wire must be a pure function of persisted state, so it is
   byte-identical whether we are in the same long-lived process or a fresh one.
   vis's `session_turn_iteration` rows ARE an append-only event log (one row per
   iteration, carrying its `forms` blob); this ns folds that log — plus the
   `:session/summaries` compaction intents — into an ordered EVENT stream and a
   JSONL projection of it. SQLite stays the source of truth (see DERIVED_WIRE.md);
   this is a read-only view that cannot diverge because it is *derived*."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.persistance :as persistance]
   [taoensso.telemere :as tel]))

(def ^:private sentinels #{"vis_answer" "vis_silent"})

(defn- turn-events
  "Events for one persisted turn, in order: the user message, each result-
   producing form's scope index (`scope` + the call `src`), and the done()
   answer. Heavy result VALUES are not embedded — they stay fetchable via the
   cross-turn rebind; the event records the addressable structure only."
  [db-info {:keys [id position user-request answer-markdown]}]
  (let [results (->> (try (persistance/db-list-session-turn-iterations db-info id)
                       (catch Throwable _ []))
                  (filter #(= :done (:status %)))
                  (sort-by :position)
                  (mapcat (fn [it]
                            (keep (fn [f]
                                    (let [sc (:scope f) r (:result f)]
                                      (when (and sc (some? r) (not (contains? sentinels r)))
                                        {:type "result"
                                         :scope sc
                                         :src (ctx-engine/compact-src (:src f))})))
                              (:forms it)))))]
    (concat
      (when (seq (str user-request)) [{:type "user" :turn position :content (str user-request)}])
      results
      (when (seq (some-> answer-markdown str str/trim))
        [{:type "answer" :turn position :content (str/trim (str answer-markdown))}]))))

(defn- summary-events
  "Compaction events from `:session/summaries` (model summarize/drop intents):
   `{:type summary|drop :scopes [...] :gist?}`. Placed up front so the fold knows
   which scopes are collapsed/removed before it walks the turns."
  [summaries]
  (keep (fn [s]
          (when-let [scopes (seq (sort (:scopes s)))]
            (cond-> {:type (if (:gist s) "summary" "drop") :scopes (vec scopes)}
              (:gist s) (assoc :gist (:gist s)))))
    summaries))

(defn derive-session-events
  "PURE projection of a persisted session into the ordered provider-send EVENT
   stream. Reads ONLY the DB (turns + iteration forms) and the supplied
   `summaries`, so ANY process yields the identical sequence. Oldest→newest."
  ([db-info session-id] (derive-session-events db-info session-id nil))
  ([db-info session-id summaries]
   (try
     (when (and db-info session-id)
       (vec
         (concat
           (summary-events summaries)
           (mapcat #(turn-events db-info %)
             (persistance/db-list-session-turns db-info session-id)))))
     (catch Throwable t
       (tel/log! {:level :warn :id ::derive-session-events-failed
                  :data {:session-id session-id :error (ex-message t)}}
         "wire-view: could not derive session events")
       []))))

(defn events->jsonl
  "Serialize an event stream to JSONL (one compact JSON object per line)."
  ^String [events]
  (str/join "\n" (map #(json/write-json-str %) events)))

(defn session-jsonl
  "The derived JSONL for a session — the browseable, process-invariant record of
   what the provider sees, projected from the DB."
  ^String [db-info session-id]
  (events->jsonl (derive-session-events db-info session-id)))
