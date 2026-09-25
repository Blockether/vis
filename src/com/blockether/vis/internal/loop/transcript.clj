(ns com.blockether.vis.internal.loop.transcript
  "Prompt and transcript assembly for provider requests.

   Rebuilds prior-turn context, freezes iteration results into append-only
   messages, replays images and preserved reasoning, exposes the single
   `python_execution` tool, places prompt-cache breakpoints, tracks cache reuse,
   and sends an assembled request through the session's LLM client."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.content :as content-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.attachment.vision-describe :as vision-describe]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.loop.errors :as loop-errors]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(defn needs-input-answer?
  "True for explicit clarification/needs-input answer payloads.

   Foundation exposes this through `(needs-input ...)`; the loop
   keeps the predicate data-shaped instead of depending on foundation
   namespaces so the core runtime has no extension cycle."
  [v]
  (and (map? v)
       (= :needs-input (:vis/answer-mode v))
       (string? (:answer/text v))
       (not (str/blank? (:answer/text v)))))

(defn markdown-answer?
  "True for the canonical final-answer VALUE: `{:answer string}`.
   The answer is the plain prose the model replies with; `answer-fn` wraps that
   string into this `{:answer string}` shape. The only other accepted value is
   the `needs-input-answer?` map."
  [v]
  (and (map? v) (string? (:answer v))))

(defn answer-markdown
  "Disposable model-facing text projection of a final typed answer. Canonical
   content remains structured and this projection is never transported as a
   second answer shape."
  [answer]
  (let [v (:result answer answer)]
    (cond (needs-input-answer? v) (:answer/text v)
          (markdown-answer? v) (:answer v)
          (and (vector? v) (every? content-contract/block-valid? v))
          (not-empty (str/trim (content/text-projection v)))
          :else nil)))

(defn turn-error-data
  "First canonical error block from a final answer, or nil."
  [answer]
  (let [v (:result answer answer)]
    (when (vector? v) (some #(when (= "error" (get % "type")) %) v))))

(defn failed-turn-content
  "Content blocks for a turn that ended in FAILURE - never a throw.

   `content/answer-content` VALIDATES, and the fallback answer a failed turn
   carries is frequently not answer-shaped: a provider-exhaustion turn hands back
   a raw error value. Unguarded, that validation throw escapes `send!` BEFORE
   [[persist-turn-outcome!]] runs, so the turn keeps no status, no error and no
   counters, the UI shows an empty turn, and \"Final answer must be canonical
   content or Markdown prose\" replaces the provider failure that actually killed
   it in the log. Measured: an upstream stream timeout turned into a turn with no
   answer and no error card at all, and the human had to type 'Continue'.

   On a throw the content is rebuilt from the last iteration error in `trace` -
   the same provider failure the gateway card names."
  [answer trace]
  (try (content/answer-content answer)
       (catch Throwable _
         (or (try (some-> (some :error (reverse trace))
                          perr/provider-error-content
                          seq
                          vec)
                  (catch Throwable _ nil))
             [(content/error "turn_failed" "Turn failed" false)]))))

(defn persist-turn-outcome!
  "Persist a claimed terminal outcome. If its snapshot fails, retry without CTX
   before degrading to bounded error content. Preserve status and counters even
   when neither the answer nor context can be stored. Log every failed write;
   never include a potentially unbounded or private payload in the fallback.
   Returns false if another terminal owns the turn or all writes fail."
  ([db-info session-turn-id opts] (persist-turn-outcome! db-info session-turn-id opts nil))
  ([db-info session-turn-id opts claim!]
   (if (and claim! (not (claim!)))
     false
     (letfn
       [(store! [payload stage]
          (try (persistance/db-update-session-turn! db-info session-turn-id payload)
               true
               (catch Throwable t
                 (tel/log! {:level (if (= stage :minimal) :error :warn)
                            :id (case stage
                                  :full
                                  ::turn-outcome-persist-failed

                                  :without-context
                                  ::turn-outcome-without-context-failed

                                  :minimal
                                  ::turn-outcome-lost)
                            :data {:session-turn-id (str session-turn-id)
                                   :status (:status opts)
                                   :stage stage
                                   :error-class (.getName (class t))
                                   :cause-class (some-> (ex-cause t)
                                                        class
                                                        .getName)}})
                 false)))]
       (or
         (store! opts :full)
         (and (contains? opts :ctx) (store! (dissoc opts :ctx) :without-context))
         (let
           [block
            (content/error
              "turn_outcome_persist_failed"
              "The turn finished, but its answer could not be stored. The outcome is recorded without its answer or context."
              true)]
           (store! (assoc (select-keys opts
                                       [:iteration-count :duration-ms :tokens :cost :prior-outcome])
                     :status (or (:status opts) :error)
                     :content [block]
                     :error block)
                   :minimal)))))))

(defn session-turn-position
  [environment session-turn-id]
  (or (try (when-let [session-id (:session-id environment)]
             (some (fn [turn]
                     (when (= (str (:id turn)) (str session-turn-id)) (:position turn)))
                   (persistance/db-list-session-turns-meta (:db-info environment) session-id)))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::session-turn-position-failed
                        :data {:session-id (:session-id environment)
                               :session-turn-id session-turn-id
                               :error (ex-message t)}}
                       "Could not resolve session turn position for iteration hooks")
             nil))
      1))

;; The selector RESOLVER (`scope-key` / `turn-key` / `expand-through` /
;; `supersede-summaries`) lives in `ctx-engine` so the wire (`apply-summaries`)
;; and the render-time ledger (`ctx-engine/folds-view`) share ONE resolver.
(defn- iter-of-scope
  "Form scope `\"t1/i2/f3\"` → its iteration scope `\"t1/i2\"` (drops the `/fN`).
   nil for non-form scopes (e.g. the synthetic `:summary` keyword)."
  [scope]
  (when (string? scope)
    (let [parts (str/split scope #"/")]
      (when (>= (count parts) 2) (str (nth parts 0) "/" (nth parts 1))))))

(defn- prior-turn-scope-index
  "Lean per-form scope index for ONE prior turn's `forms`, reshaped by the model's
   fold summaries — the cross-process RESUME view. Folds are recorded at
   ITERATION granularity (`tN/iN`) but forms carry FORM scopes (`tN/iN/fN`), so
   each form scope is normalized via `iter-of-scope` before matching.

   Each fold collapses to ONE breadcrumb, not one per form or covered iteration.
   Dedup keys on breadcrumb content: a fold with a gist becomes
   `{:scope tN/iN :gist g}`; a gist-less fold becomes
   `{:scope tN/iN :dropped? true :note why}`. `:scope` is the first covered
   iteration. Every uncovered form keeps its `{:scope tN/iN/fN :src …}` line.
   An open-start (`-tN/iK`) range is resolved against this turn's iterations.
   Pure."
  [forms summaries]
  (let [universe
        (distinct (keep #(iter-of-scope (:scope %)) forms))

        sums
        (ctx-engine/supersede-summaries (ctx-engine/expand-through (or summaries []) universe))

        ;; Summary intents are string-keyed because they persist in the ctx blob.
        ;; A canonical gist-less fold is a drop; there is no second flag.
        drop-of
        (into {}
              (mapcat (fn [s]
                        (when (nil? (get s "gist"))
                          (map (fn [sc]
                                 [sc (get s "note")])
                               (get s "scopes"))))
                      sums))

        gist-of
        (into {}
              (mapcat (fn [s]
                        (when-let [gist (get s "gist")]
                          (map (fn [sc]
                                 [sc gist])
                               (get s "scopes"))))
                      sums))]

    (first
      (reduce (fn [[acc seen] f]
                (let [sc
                      (:scope f)

                      isc
                      (iter-of-scope sc)]

                  (cond (and isc (contains? drop-of isc)) ; dropped → ONE audit line per reason
                        (let [note
                              (get drop-of isc)

                              k
                              [:dropped note]]

                          (if (contains? seen k)
                            [acc seen]
                            [(conj acc
                                   (cond-> {:scope isc :dropped? true}
                                     note
                                     (assoc :note note))) (conj seen k)]))
                        (and isc (contains? gist-of isc)) ; folded → ONE line per distinct gist
                        (let [gist
                              (get gist-of isc)

                              k
                              [:gist gist]]

                          (if (contains? seen k)
                            [acc seen]
                            [(conj acc {:scope isc :gist gist}) (conj seen k)]))
                        (:live-record f) [(conj acc {:scope sc :live-record (:live-record f)}) seen]
                        (and sc (or (some? (:stdout f)) (some? (:error f)) (some? (:activity f))))
                        [(conj acc {:scope sc :src (ctx-engine/compact-src (:src f))}) seen]
                        :else [acc seen])))
              [[] #{}]
              forms))))

(defn user-slash-iteration?
  "True for a synthetic slash-command iteration. These rows stay in local
   transcript/audit history but must never enter a later provider request."
  [iteration]
  (boolean (some #(= "user-slash"
                     (some-> (:tag %)
                             name))
                 (:forms iteration))))

(def ^:private terminal-incomplete-turn-statuses #{:interrupted :error :cancelled})

(def ^:private interrupted-turn-statuses #{:interrupted :error})

(defn terminal-incomplete-turn-status?
  [status]
  (contains? terminal-incomplete-turn-statuses status))

(defn- interrupted-turn-status? [status] (contains? interrupted-turn-statuses status))

(def ^:private live-record-media-type "application/vnd.vis.live+ndjson")

(defn- model-live-record?
  "True for a semantic extension-owned live-view record."
  [attachment]
  (= live-record-media-type (or (:media-type attachment) (:media_type attachment))))

(defn- live-record-context-line
  "Tell a later model where one settled live-view record can be reopened."
  [att]
  (str "Live-view record filed: "
       (:filename att)
       " (attachment id "
       (:id att)
       "; read_attachment(\""
       (:id att)
       "\") opens it)."))

(defn provider-history-metadata
  "Read byte-free history, validating only explicitly identified local-command candidates."
  [db turn-ids]
  (let [iterations
        (persistance/db-list-session-turns-iterations-meta db turn-ids)

        candidates
        (filter :local-command-candidate? (mapcat val iterations))

        candidate-rows
        (persistance/db-list-iterations db (map :id candidates))

        local-ids
        (into #{}
              (keep (fn [[id row]]
                      (when (user-slash-iteration? row) id)))
              candidate-rows)]

    {:iterations iterations
     :local-turn-ids (into #{}
                           (keep (fn [[id rows]]
                                   (when (some #(contains? local-ids (str (:id %))) rows) id)))
                           iterations)}))

(defn previous-turn-context
  "Prior provider-visible turns as an append-only RESUME sequence, compacted by
   the persisted fold ledger. Q/A removal keys off EXPLICIT whole-turn intent
   only (`\"turns\"` stamped by expand-through: a bare `tN` or a range selector
   spanning the turn) — an enumerated iteration fold that happens to name every
   iteration keeps the turn's Q/A recap with folded result lines. A turn covered
   with explicit intent loses its complete Q/A + result recap here; the trailer
   (`apply-summaries`) owns the ONE durable checkpoint anchored at its folded
   iterations. A covered turn with NO done iterations has no trailer anchor, so
   it materializes a minimal `:checkpoint?` entry here instead of vanishing —
   nothing leaves the wire without a visible tombstone. Broader/newer summaries
   are resolved first (supersede merges whole-turn intent), so a fold-of-fold
   cannot leave older Q/A or breadcrumbs beside the checkpoint.

   Synthetic slash turns remain local-only. Oldest→newest; current/running turns
   are excluded. Cancelled/error/interrupted turns remain even without an answer
   so settled work and the unfinished boundary survive; nil when no
   provider-visible representation remains."
  [environment current-turn-id & [model]]
  (try
    (when-let [session-id (:session-id environment)]
      (let [d (:db-info environment)
            summaries (some-> (:ctx-atom environment)
                              deref
                              (get "session_summaries"))
            turns (remove #(or (= (str (:id %)) (str current-turn-id)) (= :running (:status %)))
                    (persistance/db-list-session-turns-meta d session-id))
            history (provider-history-metadata d (map :id turns))
            iterations-by-turn (:iterations history)
            turns (remove #(contains? (:local-turn-ids history) (str (:id %))) turns)
            turn-attachments (filter #(nil? (:iteration-id %))
                                     (persistance/db-list-session-attachments-meta d session-id))
            recordings-by-turn (group-by (comp str :turn-soul-id)
                                         (filter #(and (attachments/audio-media-type? (:media-type
                                                                                        %))
                                                       (not (str/blank? (:transcription %))))
                                                 turn-attachments))
            ;; Everything else the human attached to a PRIOR turn (screenshots,
            ;; documents). Naming the id in the recap is what lets the next turn
            ;; open it directly instead of re-listing the session's attachments.
            files-by-turn (group-by (comp str :turn-soul-id)
                                    (remove #(attachments/audio-media-type? (:media-type %))
                                      turn-attachments))
            turn-metadata
            (mapv (fn [turn]
                    (let [iterations (filter #(= :done (:status %))
                                             (get iterations-by-turn (str (:id turn))))]
                      (assoc turn
                        :turn (:position turn)
                        :iterations iterations
                        :iter-scopes (mapv #(str "t" (:position turn) "/i" (:position %))
                                           iterations))))
                  turns)
            resolved (ctx-engine/supersede-summaries (ctx-engine/expand-through
                                                       (or summaries [])
                                                       (mapcat :iter-scopes turn-metadata)
                                                       (map :turn turn-metadata)))
            covering-summary (fn [{:keys [turn]}]
                               (last (filter #(and (contains? (set (get % "turns")) turn)
                                                   (integer? (get % "issued_turn"))
                                                   (> (long (get % "issued_turn")) (long turn)))
                                             resolved)))
            folded-scopes (into #{} (mapcat #(get % "scopes")) resolved)
            ;; Iteration bodies of UNFINISHED turns only, in ONE query. A cancelled
            ;; turn used to reach the next request as `tN/iM (stored iteration)`:
            ;; its outputs replayed, the code that produced them did not, so the
            ;; agent re-discovered the repository instead of continuing its own
            ;; work. An answered turn keeps the cheap placeholder — its recap
            ;; already carries the answer that settled it.
            unfinished-bodies (persistance/db-list-iterations
                                d
                                (for [{:keys [status iterations iter-scopes] :as metadata}
                                      turn-metadata
                                      :when (and (terminal-incomplete-turn-status? status)
                                                 (not (covering-summary metadata)))
                                      [iteration scope] (map vector iterations iter-scopes)
                                      :when (not (contains? folded-scopes scope))]

                                  (:id iteration)))
            turn-data
            (into
              []
              (keep
                (fn [{:keys [iterations iter-scopes] :as metadata}]
                  ;; Covered Q/A and all iteration bodies stay on disk. The existing
                  ;; fold materializer below decides whether the trailer owns the gist.
                  (if (covering-summary metadata)
                    (dissoc metadata :iterations)
                    (let [turn (persistance/db-read-session-turn d session-id (:id metadata))
                          unfinished? (terminal-incomplete-turn-status? (:status turn))
                          ;; An unfinished turn still holds the sticky best-answer it
                          ;; produced before the cancel. That is not a final answer, so it
                          ;; rides beside the cancellation boundary as the partial one
                          ;; instead of being dropped.
                          answer-md (answer-markdown (:content turn))
                          answer (when-not unfinished? answer-md)
                          ;; An ERROR turn files the failure itself as its content. That
                          ;; is not prose the model offered the user, so it never counts
                          ;; as a partial answer.
                          partial-answer (when (and unfinished?
                                                    (not-any? #(= "error" (get % "type"))
                                                              (:content turn)))
                                           (not-empty (str/trim (str answer-md))))
                          visible-ids (keep (fn [[iteration scope]]
                                              (when-not (contains? folded-scopes scope)
                                                (:id iteration)))
                                            (map vector iterations iter-scopes))
                          artifacts (persistance/db-list-iterations-attachments-meta d visible-ids)
                          ;; What the iteration actually RAN, one line per form. A slash
                          ;; iteration stays local-only and an unread body keeps the bare
                          ;; stored-iteration location.
                          iteration-sources
                          (fn [iteration scope]
                            (let [body (get unfinished-bodies (str (:id iteration)))]
                              (or (when-not (user-slash-iteration? body)
                                    (seq (keep #(not-empty (str/trim (str (:src %))))
                                               (:forms body))))
                                  [(str scope " (stored iteration)")])))
                          forms (into []
                                      (mapcat (fn [[iteration scope]]
                                                (concat
                                                  (for [src (iteration-sources iteration scope)]
                                                    {:scope scope :stdout "" :src src})
                                                  (for [att (get artifacts (str (:id iteration)))
                                                        :when (model-live-record? att)]

                                                    {:scope scope
                                                     :live-record (live-record-context-line
                                                                    att)}))))
                                      (map vector iterations iter-scopes))]

                      (when (or unfinished? (not (str/blank? answer)))
                        (cond-> {:turn (:turn metadata)
                                 :user-request
                                 (str (:user-request turn)
                                      (apply str
                                        (for [recording (get recordings-by-turn (str (:id turn)))]
                                          (str "\n\nAttached recording: " (:filename recording)
                                               " (attachment id: " (:id recording)
                                               ")" (prompt/recording-transcript (:transcription
                                                                                  recording)
                                                                                nil))))
                                      (apply str
                                        (for [att (get files-by-turn (str (:id turn)))]
                                          (str "\n\nAttached file: "
                                               (:filename att)
                                               " (attachment id: "
                                               (:id att)
                                               ")"))))
                                 :answer answer
                                 :interrupted? (interrupted-turn-status? (:status turn))
                                 :cancelled? (= :cancelled (:status turn))
                                 :forms forms
                                 :iter-scopes iter-scopes}
                          partial-answer
                          (assoc :partial-answer partial-answer)))))))
              turn-metadata)
            ;; Blockether/vis#174: user requests can be dense code, not prose.
            ;; Price the rendered recap in the same tokenizer units as iteration weights.
            _ (when-let [ca (:ctx-atom environment)]
                (try (let [model (or model "unknown")
                           priming (svar/count-messages model [])]

                       (swap! ca assoc
                         "engine_turn_weights"
                         (into {}
                               (map (fn [{:keys [turn] :as entry}]
                                      [turn
                                       (- (svar/count-messages model
                                                               [{:role "user"
                                                                 :content
                                                                 (prompt/previous-turn-context-block
                                                                   [entry])}])
                                          priming)]))
                               turn-data)))
                     (catch Exception _ nil)))]

        (some->>
          (reduce
            (fn [out
                 {:keys [turn user-request answer partial-answer interrupted? cancelled? forms
                         iter-scopes]
                  :as td}]
              (if-let [summary (covering-summary td)]
                (if (seq iter-scopes)
                  ;; The trailer's apply-summaries path owns the ONE durable
                  ;; breadcrumb (anchored at this turn's folded iterations).
                  ;; Removing the complete Q/A representation here avoids
                  ;; echoing that checkpoint in a second wire location.
                  out
                  ;; No done iterations → no trailer anchor exists anywhere.
                  ;; Materialize the checkpoint HERE so the fold never
                  ;; erases a turn without a visible tombstone. Consecutive
                  ;; turns covered by the SAME summary share one entry.
                  (let [prev (peek out)]
                    (if (and (:checkpoint? prev) (identical? (:summary prev) summary))
                      (conj (pop out) (update prev :turns conj turn))
                      (conj out
                            {:checkpoint? true
                             :summary summary
                             :turns [turn]
                             :gist (or (some-> (get summary "gist")
                                               str
                                               str/trim
                                               not-empty)
                                       (str "(dropped — raw turn data remains in session storage"
                                            (when (toggles/enabled? "introspection")
                                              "; recover via `await read_session()`")
                                            ")"))}))))
                (conj out
                      (cond-> {:turn turn
                               :user-request user-request
                               :answer answer
                               :interrupted? interrupted?
                               :results (vec (take 40 (prior-turn-scope-index forms resolved)))}
                        partial-answer
                        (assoc :partial-answer partial-answer)

                        cancelled?
                        (assoc :cancelled? true)))))
            []
            turn-data)
          not-empty
          (mapv #(dissoc % :summary)))))
    (catch Throwable t
      (tel/log! {:level :warn
                 :id ::previous-turn-context-failed
                 :data {:session-id (:session-id environment)
                        :session-turn-id current-turn-id
                        :error (ex-message t)}}
                "Could not load previous turn context; continuing without Q/A carry")
      nil)))

(defn previous-request-usage
  "Return the latest persisted provider request before `current-turn-id`.

   `:session/utilization` is rendered before the next provider call, so iteration 1
   cannot use current-turn API usage yet. Seed only the last request size used for
   context pressure. Provider-cache telemetry is Svar-owned, process-local, and
   intentionally never reconstructed from historical database rows."
  [environment current-turn-id]
  (try
    (when-let [session-id (:session-id environment)]
      (let [db (:db-info environment)
            turns (or (persistance/db-list-session-turns-meta db session-id) [])
            current-id (str current-turn-id)
            prior (reverse (remove #(= (str (:id %)) current-id) turns))]

        (loop [remaining prior]
          (when-let [turn (first remaining)]
            (let [measured
                  (try (persistance/db-latest-turn-request-usage db (:id turn))
                       (catch Throwable t
                         (tel/log! {:level :warn
                                    :id ::previous-request-iterations-failed
                                    :data {:session-id session-id
                                           :session-turn-id (:id turn)
                                           :error (ex-message t)}}
                                   "Could not load prior turn iterations while seeding utilization")
                         nil))]
              (if-let [it measured]
                {:last-request-tokens (long (:input-tokens it))
                 :last-request-turn-id (:id turn)
                 :last-request-turn-position (:position turn)
                 :last-request-iteration (:position it)}
                (recur (rest remaining))))))))
    (catch Throwable t
      (tel/log! {:level :warn
                 :id ::previous-request-usage-failed
                 :data {:session-id (:session-id environment)
                        :session-turn-id current-turn-id
                        :error (ex-message t)}}
                "Could not load previous request usage; first iteration will omit utilization")
      nil)))

(defn stamp-utilization!
  "Monotonic update of `\"engine_utilization\"` on the ctx-atom. UPGRADES when a
   real measurement (`util`) exists; NEVER removes an existing value. A
   transient nil — iter-1 seed miss, or an errored iteration that returned no
   usage — must not BLANK an already-shown utilization; that flicker is the
   `sometimes works / sometimes doesn't` bug. The last value carries on the
   per-session live atom (`:engine/*` is stripped only at persist time) until
   a fresh request refreshes it; a brand-new session starts blank because
   nothing was ever stamped."
  [ctx-atom util]
  (when (and ctx-atom util)
    (swap! ctx-atom (fn [ctx]
                      ;; Arm at 75% of the operating budget. Pressure guidance escalates
                      ;; before overflow and remains armed until a measured request falls
                      ;; below that threshold; ignored warnings never silently expire.
                      (let [turn
                            (long (or (get ctx "session_turn") 1))

                            req
                            (long (or (get util "last_request_tokens") 0))

                            cap
                            (long (or (get util "auto_compress_above") 0))

                            pressured?
                            (and (pos? cap) (>= (* req 4) (* cap 3)))

                            since
                            (get ctx "engine_overbudget_hint_turn")]

                        (cond-> (assoc ctx "engine_utilization" util)
                          (and pressured? (nil? since))
                          (assoc "engine_overbudget_hint_turn" turn)

                          (not pressured?)
                          (dissoc "engine_overbudget_hint_turn")))))))

(defn record-provider-input
  "Stamp this response's input, even when missing, and settle a pending fold batch.
   The signed reduction is net request shrinkage, not isolated fold savings or cost."
  [ctx response]
  (let [input
        (get-in response [:api-usage :input-tokens])

        sample
        {"turn" (get ctx "session_turn")
         "provider" (some-> (:llm-provider response)
                            name
                            str/trim
                            not-empty)
         "model" (some-> (:llm-model response)
                         str
                         str/trim
                         not-empty)
         "input_tokens" (when (and (integer? input) (pos? (long input))) (long input))}

        pending
        (get ctx "engine_fold_measurement")

        before
        (get pending "before_input_tokens")

        after
        (get sample "input_tokens")

        reason
        (cond (nil? before) "missing_before_usage"
              (nil? after) "missing_after_usage"
              (not (every? some?
                           [(get pending "provider") (get pending "model") (get sample "provider")
                            (get sample "model")]))
              "unknown_route"
              (not= (get pending "turn") (get sample "turn")) "turn_changed"
              (not= (select-keys pending ["provider" "model"])
                    (select-keys sample ["provider" "model"]))
              "route_changed")]

    (cond-> (assoc ctx "engine_provider_input" sample)
      (= "pending" (get pending "status"))
      (assoc "engine_fold_measurement"
        (cond-> (assoc pending
                  "status" (if reason "unavailable" "measured")
                  "source" "provider_usage"
                  "after_input_tokens" after)
          reason
          (assoc "reason" reason)

          (nil? reason)
          (assoc "net_reduction_tokens" (- (long before) (long after))))))))

(defn stamp-prompt-cache-status!
  "Store Svar's opaque current-turn prompt-cache status for diagnostics."
  [ctx-atom status]
  (when (and ctx-atom (map? status))
    (swap! ctx-atom assoc ctx-engine/prompt-cache-status-key (wire/->wire status))))

(defn stamp-served-route!
  "Record the provider/model that actually answered this iteration."
  [env iteration-result]
  (when-let [ctx-atom (:ctx-atom env)]
    (swap! ctx-atom ctx-engine/stamp-served-route
      (:llm-provider iteration-result)
      (:llm-model iteration-result))))

(defn estimator-undercount
  "How far the local estimate undercounts a rejection's reported size.

   The rejection may be preflight or provider-side; this is a conservative budget
   factor, not provider-usage telemetry. nil when either side is missing, never below 1.0."
  [reported-tokens local-tokens]
  (when (and (number? reported-tokens)
             (number? local-tokens)
             (pos? (long reported-tokens))
             (pos? (long local-tokens)))
    (max 1.0 (/ (double reported-tokens) (double local-tokens)))))

(defn- messages-wire-tokens
  "Svar's tokenized marginal cost for a group of canonical messages inside a larger
   request. `count-messages` includes the array's one reply-priming charge, so remove
   the empty-array baseline before per-iteration groups are summed."
  ^long [model messages]
  (max 0 (- (long (svar/count-messages model messages)) (long (svar/count-messages model [])))))

(defn runtime-turn-prefix
  [environment]
  (let [id-s
        (str (or (:session-turn-id (ctx-loop/read-turn-state environment))
                 (:environment-id environment)
                 "00000000"))

        prefix
        (subs id-s 0 (min 8 (count id-s)))]

    (if (re-matches #"(?i)[0-9a-f]{8}" prefix) prefix "00000000")))

(defn eval-block-role
  "Block role for the outer lifecycle event — one of the four values
   in the iteration-block role enum:
     :answer    the model's final answer to the user
     :tool      any Python evaluation (tool call OR raw user code)
     :nudge     system-emitted reminders / diagnostics
     :thinking  model reasoning blocks
   The previous `:vis/error` role is gone — errors are derived from
   `:success?` on the envelope (or block-level `:error` slot for
   non-tool evals). Replaces the prior `eval-rendering-kind` fn."
  [result]
  (cond (= :answer (:role result)) :answer
        (= :tool (:role result)) :tool
        (= :nudge (:role result)) :nudge
        (= :thinking (:role result)) :thinking
        (keyword? (:role result)) (:role result)
        :else :tool))

(defn eval-envelope
  "Generic canonical envelope for every executed block that passes
   through the Vis eval pipeline. Tool calls can add nested metadata
   in their returned envelope; this records the outer block
   evaluation so plain calls and tool calls share a common block-level
   trace."
  [turn-prefix iteration form-idx form-count result rendering-kind]
  (let [finished
        (long (or (:execution-finished-at-ms result) (util/now-ms)))

        duration
        (long (or (:duration-ms result) 0))

        started
        (long (or (:execution-started-at-ms result) (max 0 (- finished duration))))

        form-position
        (inc (long form-idx))]

    {:op (or (:op result)
             (case rendering-kind
               :nudge
               :vis/system

               :answer
               :vis/answer

               :python/eval))
     :started-at-ms started
     :finished-at-ms finished
     :status (cond (:timeout? result) :timeout
                   (:error result) :error
                   :else :done)
     :iteration iteration
     :form-position form-position
     :form-count form-count
     :ref (str "turn/" turn-prefix "/iteration/" iteration "/block/" form-position)
     :timeout? (boolean (:timeout? result))
     :repaired? (boolean (:repaired? result))}))

(defn- envelope-timestamps-ordered?
  [envelope]
  (<= (long (:started-at-ms envelope)) (long (:finished-at-ms envelope))))

(defn- envelope-form-position-valid?
  [envelope]
  (<= (long (:form-position envelope)) (long (:form-count envelope))))

(defn- envelope-ref-consistent?
  [envelope]
  (let [[_ iteration block] (re-matches
                              #"(?i)^turn/[0-9a-f]{8}/iteration/([1-9][0-9]*)/block/([1-9][0-9]*)$"
                              (:ref envelope))]
    (and iteration
         block
         (= (Long/parseLong iteration) (long (:iteration envelope)))
         (= (Long/parseLong block) (long (:form-position envelope))))))

(defn- envelope-has-no-derived-duration? [envelope] (not (contains? envelope :duration-ms)))

(defn block-duration-ms [block] (or (form/envelope-duration-ms (:envelope block)) 0))

(defn- nil-or-boolean? [x] (or (nil? x) (boolean? x)))

(defn- iteration-envelope?
  [envelope]
  (and (map? envelope)
       (#{:python/eval :vis/guard :vis/system :vis/answer} (:op envelope))
       (#{:done :error :timeout} (:status envelope))
       (pos-int? (:iteration envelope))
       (pos-int? (:form-position envelope))
       (pos-int? (:form-count envelope))
       (nat-int? (:started-at-ms envelope))
       (nat-int? (:finished-at-ms envelope))
       (string? (:ref envelope))
       (re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/[1-9][0-9]*/block/[1-9][0-9]*$"
                   (:ref envelope))
       (or (not (contains? envelope :timeout?)) (nil-or-boolean? (:timeout? envelope)))
       (or (not (contains? envelope :repaired?)) (nil-or-boolean? (:repaired? envelope)))
       (envelope-timestamps-ordered? envelope)
       (envelope-form-position-valid? envelope)
       (envelope-ref-consistent? envelope)
       (envelope-has-no-derived-duration? envelope)))

(defn- iteration-block?
  [block]
  (and (map? block)
       (nat-int? (:id block))
       (string? (:code block))
       (or (nil? (:error block)) (map? (:error block)))
       (iteration-envelope? (:envelope block))
       (or (not (contains? block :timeout?)) (nil-or-boolean? (:timeout? block)))
       (or (not (contains? block :repaired?)) (nil-or-boolean? (:repaired? block)))
       (or (not (contains? block :comment)) (string? (:comment block)))))

(defn validate-iteration-blocks!
  "Fail fast if a stored block lost its execution envelope."
  [blocks]
  (let [blocks (mapv (fn [block]
                       (cond-> block
                         (contains? block :error)
                         (update :error
                                 loop-errors/op-error
                                 {:code (:code block) :phase (get-in block [:envelope :op])})))
                     (or blocks []))]
    (doseq [block blocks]
      (when-not (iteration-block? block)
        (throw (ex-info "Invalid iteration block"
                        {:type :vis/invalid-iteration-block :block block}))))
    blocks))

(defn reasoning-effort-configurable?
  "True when a model accepts a CALLER-selected reasoning effort.

   svar decides this, not Vis: `:reasoning-effort?` is stamped on every model
   the router normalizes, from the WIRE that model rides. `:reasoning?` only
   says the model thinks — GitHub Copilot's Gemini/Grok tiers think but are
   `:server-managed` on the OpenAI-compatible wire, and Z.ai GLM thinking is
   binary, so neither accepts a depth and neither may show a depth control.
   Copilot's Claude tier rides the native Anthropic wire and DOES take one."
  [resolved-model]
  (boolean (:reasoning-effort? resolved-model)))

(defn verbosity-configurable?
  "True when a model accepts a caller-selected answer verbosity.

   Also svar's call: `:verbosity-style` is stamped from the wire, so every
   provider on the OpenAI Responses endpoint (Codex AND GitHub Copilot's GPT
   tier) gets the knob and nothing else does. Never test a provider id here."
  [resolved-model]
  (some? (:verbosity-style resolved-model)))

(defn- ^:private replay-reasoning-chars
  "Total `:thinking-signature` (or `:thinking` fallback) char count for
   the canonical thinking blocks on `assistant-message`. 0 when nil.
   The signature field is what svar's wire serializer hoists into
   `reasoning_content` — that is what counts against the budget."
  [assistant-message]
  (->> (get assistant-message :content)
       (filter (fn [b]
                 (= "thinking" (:type b))))
       (map (fn [b]
              (count (or (:thinking-signature b) (:thinking b) ""))))
       (reduce + 0)))

(defn- preserved-thinking-replay-messages
  "Provider-agnostic preserved-thinking replay. Returns every compatible
   `:assistant-message` from `trailer-iters` in arrival order.

   Why every message, not just the last:
     - Z.ai / GLM-5.x preserved thinking (`clear_thinking: false`) keeps
       reasoning_content across assistant turns only when each prior
       assistant message echoes the model's full reasoning back. Drop a
       step and GLM either re-derives the same scratch state at every
       iteration (re-reading the same file with `cached_tokens` pinned
       across many iterations) or starts to
       hallucinate that an earlier conclusion is still live.
     - Anthropic extended thinking signs each block with an HMAC and
       refuses replay if the chain is broken; sending only the last
       block fails signature validation as soon as the model produced
       more than one block since the user message.
     - OpenAI Responses encrypted reasoning items must replay in order
       — the next call rejects a single isolated item with
       'reasoning without following item'.

   The earlier conservative 'last-only' policy was tuned for
   pre-`clear_thinking`
   GLM-4.6 where any replay contaminated the next step. The modern
   GLM-5.1 + Anthropic 4.x + OpenAI Responses contract all want full
   chains; pi-ai's `transform-messages.js` follows the same approach
   (every prior assistant `thinking` block preserved when same model).

   `compatible-preserved-thinking-trailer-iters` upstream has already
   filtered iterations to (a) same provider+model as the target call,
   (b) opted in via `:preserved-thinking/replay?` (live-turn freshly
   produced iterations), (c) carrying a valid `:assistant-message`,
   (d) signature-compatible with the replay target. Anything that
   reaches this fn is safe to replay verbatim.

   The wire serializer for the active model translates each canonical
   message to its native shape; iteration-loop never branches on
   provider."
  [trailer-iters]
  (let [msgs (vec (keep #(some-> %
                                 second
                                 :assistant-message)
                        trailer-iters))]
    (when (seq msgs)
      ;; Keep this call so oversized reasoning chains are observable to
      ;; future budget instrumentation. Sum across the full chain instead
      ;; of just the latest step — budget watchers care about cumulative
      ;; replay size, not single-step size.
      (doseq [m msgs]
        (replay-reasoning-chars m)))
    msgs))

(defn replay-context
  "Small identity map for the model the next provider call will run against:
   provider, model name, and the `:capabilities` its config entry declared.

   Thinking replay reads the first two. Provider-native thinking signatures are
   not portable: z.ai stores reasoning text under `:thinking-signature`,
   Anthropic expects an HMAC signature, and OpenAI Responses stores a JSON
   reasoning item. Replaying across a provider/model switch corrupts the next
   request (Anthropic 400: invalid signature in thinking block).

   The image gate reads all three — see `target-supports-vision?`."
  [resolved-model]
  {:provider (:provider resolved-model)
   :model (some-> (:name resolved-model)
                  str)
   :capabilities (:capabilities resolved-model)})

(defn- anthropic-replay-context?
  [{:keys [provider model]}]
  (or (boolean (re-find #"(?i)anthropic" (str provider)))
      (boolean (re-find #"(?i)^claude" (str model)))))

(defn- thinking-blocks
  [assistant-message]
  (filterv #(= "thinking" (:type %)) (:content assistant-message)))

(defn- anthropic-invalid-thinking-replay-block?
  "True for poisoned Anthropic replay state. In bad historical rows,
   Vis recorded a fallback z.ai response as Anthropic; z.ai stores raw
   reasoning text as `:thinking-signature`, so signature == thinking.
   Anthropic signatures are opaque HMACs and must not equal prose."
  [block]
  (let [thinking
        (:thinking block)

        signature
        (:thinking-signature block)]

    (and (util/non-blank-string? thinking) (string? signature) (= thinking signature))))

(defn- assistant-message-compatible-with-replay-target?
  [target assistant-message]
  (not (and (anthropic-replay-context? target)
            (some anthropic-invalid-thinking-replay-block? (thinking-blocks assistant-message)))))

(defn actual-llm-provider
  "Provider that actually served an ask-result. svar may route/fallback
   inside ask-code!, so prefer routed metadata over Vis' pre-call guess."
  [resolved-model ask-result]
  (or (:routed/provider-id ask-result) (:provider resolved-model)))

(defn actual-llm-model
  "Model that actually served an ask-result. See `actual-llm-provider`."
  [resolved-model ask-result]
  (or (:routed/model ask-result)
      (some-> (:name resolved-model)
              str)))

(defn llm-id
  [provider model]
  (cond-> {}
    provider
    (assoc :provider (name (keyword provider)))

    model
    (assoc :model (str model))))

(defn llm-routing-summary
  [selected-model iteration-result]
  (let [routing-trace
        (vec (or (:llm-routing-trace iteration-result) []))

        fallback-ev
        ;; A `:session-pick` event records that the SESSION was repointed, not how THIS
        ;; turn was routed; anchoring on it would relabel the turn's own route.
        (first (filter #(and (contains? #{:llm.routing/provider-fallback :llm.routing/model-fallback
                                          :llm.routing/format-fallback}
                                        (:event/type %))
                             (not= :session-pick (:scope %)))
                       routing-trace))

        ;; The authoritative anchors are the fallback event's from/to when a
        ;; real fallback was traced: the router may pre-resolve so the iteration
        ;; result's provider/model already reflect the FALLBACK, which would
        ;; otherwise collapse selected==actual and drop the '↳ from …' note.
        selected
        (llm-id (or (:from-provider fallback-ev) (:provider selected-model))
                (or (:from-model fallback-ev)
                    (some-> (:name selected-model)
                            str)))

        actual
        (llm-id (or (:to-provider fallback-ev)
                    (:llm-provider iteration-result)
                    (:provider selected-model))
                (or (:to-model fallback-ev)
                    (:llm-model iteration-result)
                    (some-> (:name selected-model)
                            str)))]

    (cond-> {:selected selected
             :actual actual
             :fallback? (boolean (or (not= selected actual)
                                     (some #(not= :llm.routing/provider-retry (:event/type %))
                                           routing-trace)))}
      (seq routing-trace)
      (assoc :trace routing-trace))))

(defn attach-llm-routing-summary
  [result selected-model iteration-result]
  (let [routing
        (llm-routing-summary selected-model iteration-result)

        actual
        (:actual routing)

        selected
        (:selected routing)]

    (cond-> (assoc result
              :provider (:provider actual)
              :model (:model actual)
              :llm-selected selected
              :llm-actual actual
              :llm-fallback? (:fallback? routing))
      (seq (:trace routing))
      (assoc :llm-routing-trace (:trace routing))

      (:cost result)
      (update :cost merge (select-keys actual [:provider :model])))))

(defn reasoning-effort-iteration-evidence
  [iteration requested selected-model iteration-result]
  (let [routing
        (llm-routing-summary selected-model iteration-result)

        resolution
        (:reasoning-effort-resolution iteration-result)

        actual
        (:actual routing)]

    {:iteration (inc (long iteration))
     :provider (:provider actual)
     :model (:model actual)
     :effective (:effective resolution)
     :wire-style (:wire-style resolution)
     :wire-fragment (:extra-body resolution)
     :fallback? (:fallback? routing)
     :selected (:selected routing)
     :requested requested}))

(defn turn-eval-evidence
  [requested trace]
  (when requested
    (let [iterations
          (vec (keep :reasoning-effort trace))

          missing-count
          (- (count trace) (count iterations))

          fallback-reasons
          (for [{:keys [iteration fallback? selected provider model]}
                iterations

                :when fallback?]

            {:type :provider-model-fallback
             :iteration iteration
             :selected selected
             :actual {:provider provider :model model}})

          mismatch-reasons
          (for [{:keys [iteration effective provider model]}
                iterations

                :when (not= requested effective)]

            {:type :reasoning-effort-mismatch
             :iteration iteration
             :requested requested
             :effective effective
             :provider provider
             :model model})

          reasons
          (vec (concat (when (or (empty? trace) (pos? missing-count))
                         [{:type :missing-reasoning-effort-evidence :iterations missing-count}])
                       fallback-reasons
                       mismatch-reasons))]

      {:valid? (boolean (and (seq iterations) (empty? reasons)))
       :invalid-reasons reasons
       :reasoning-effort {:requested requested :iterations iterations}})))

(defn- compatible-preserved-thinking-trailer-iters
  "Keep only explicitly replayable iterations whose provider-native thinking is
   compatible with the next provider call. Cross-turn seeds opt out; fresh
   live-turn iterations opt in. Missing ownership is never replay consent."
  [trailer-iters target]
  (let [{target-provider :provider target-model :model} target]
    (filterv (fn [[_
                   {:keys [assistant-message llm-provider llm-model]
                    replay? :preserved-thinking/replay?}]]
               (and (true? replay?)
                    assistant-message
                    (= target-provider llm-provider)
                    (= target-model llm-model)
                    (assistant-message-compatible-with-replay-target? target assistant-message)))
      (or trailer-iters []))))

(defn iteration-record-scope
  "Use an input-only iteration's scope without inventing a tool form."
  [rec]
  (or (:iteration-scope rec) (some iter-of-scope (keep :scope (:forms-vec rec)))))

(defn apply-summaries
  "Compact `trailer-iters` using `fold_session` intents, releasing covered payloads at
   iteration granularity. A summary carries concrete `scopes`, an optional
   `gist`, and its owning `at_turn`; a gist-less intent is a drop. Range intents
   are resolved by `expand-through` against the trailer's iteration scopes.

   Every covered iteration collapses: its output and assistant/tool-result pair
   leave memory as well as the wire. The earliest covered iteration receives one synthetic form
   containing the gist or a dropped marker. Pure and deterministic; persisted
   iteration records are untouched. Real compaction, not presentation."
  [trailer-iters summaries]
  (if (empty? summaries)
    (vec trailer-iters)
    (let [iter-scope-of
          iteration-record-scope

          ;; Resolve ranges against this trailer, then keep only scopes owned by
          ;; the intent's canonical at_turn. A turn may fold its own settled work
          ;; and every prior turn, never a future turn; unstamped intents own nothing.
          scope-turn
          (fn [scope]
            (or (first (ctx-engine/scope-key scope)) (ctx-engine/turn-key scope)))

          summaries
          (->> (ctx-engine/expand-through summaries (keep iter-scope-of (map second trailer-iters)))
               (keep (fn [summary]
                       (let [owner
                             (get summary "at_turn")

                             scopes
                             (when (integer? owner)
                               (into #{}
                                     (filter (fn [scope]
                                               (when-let [turn (scope-turn scope)]
                                                 (<= (long turn) (long owner)))))
                                     (get summary "scopes")))]

                         (when (seq scopes) (assoc summary "scopes" scopes)))))
               (ctx-engine/supersede-summaries))

          summarized
          (into #{} (mapcat #(get % "scopes")) summaries)

          ; set of "tN/iN"
          ;; summary → earliest trailer index whose iteration scope it names
          anchors
          (reduce
            (fn [m s]
              (if-let [idx (some (fn [[i [_ rec]]]
                                   (when (contains? (set (get s "scopes")) (iter-scope-of rec)) i))
                                 (map-indexed vector trailer-iters))]
                (update m
                        idx
                        (fnil conj [])
                        {:gist (get s "gist")
                         :drop? (nil? (get s "gist"))
                         :summary-iters (vec (sort (get s "scopes")))
                         :note (get s "note")})
                m))
            {}
            summaries)]

      (vec
        (map-indexed
          (fn [i [pos rec]]
            (let [collapsed?
                  (contains? summarized (iter-scope-of rec))

                  gists
                  (get anchors i)

                  gist-forms
                  (when gists
                    (mapv (fn [g]
                            {:scope :summary
                             :summary? true
                             :summary-gist (:gist g)
                             :summary-drop? (:drop? g)
                             :summary-iters (:summary-iters g)
                             :summary-note (:note g)})
                          gists))]

              [pos
               (cond-> (if collapsed?
                         {:iteration-scope (iter-scope-of rec) :collapsed? true :forms-vec []}
                         rec)
                 gist-forms
                 (assoc :forms-vec (vec gist-forms)))]))
          trailer-iters)))))

(defn- error->display
  "LLM-legible rendering of a form `:error` for the model wire. The human
   `:message` (which may already carry a multi-line babashka-style source
   excerpt with a caret) is shown with REAL newlines — NEVER an escaped
   one-line Python/JSON literal, which turns a caret excerpt into an
   unreadable `\n`-wall the model can't parse. The failure phase rides in the
   header (`✗ runtime error:` / `syntax` / `host`); the precise line/col are
   already visible under the caret, so no redundant `:data` blob is emitted. A
   `:hint` not already folded into the message is appended on its own line.
   Falls back to the plain value for a non-map error."
  [error]
  (if-not (map? error)
    (str "✗ error: " error)
    (let [msg
          (or (:message error)
              (some-> (:type error)
                      name)
              "error")

          phase
          (some-> (get-in error [:data :phase])
                  (#(if (keyword? %) (name %) (str %))))

          hint
          (:hint error)]

      (cond-> (str "✗ " (when phase (str phase " ")) "error: " msg)
        (and hint (not (str/includes? (str msg) (str hint))))
        (str "\nhint: " hint)))))

(defn- elide-table-fences
  "Drop the ROWS out of every ````vis-table` fence in model-facing output.

   A CSV/TSV `attach` is DATA for the HUMAN: the fence rides the transcript
   verbatim (see `form/stdout-display`) and both surfaces paint it as a live
   grid — sortable, pageable, openable as a full-screen sheet. The model needs
   none of that. Sending the payload would re-upload the whole sheet on EVERY
   later request, because tool results replay: one 500-row export then costs
   more context than the rest of the turn, forever, and teaches nothing the
   `[Table: …]` headline does not already say.

   So the wire keeps the headline (name, rows × cols, size, caption) and loses
   the rows; the bytes stay in the DB as a durable attachment, one
   `read_attachment` away. Everything outside a table fence — including a
   `vis-image` fence, which carries only a path — passes through untouched."
  [s]
  (let [text
        (str s)

        fence
        "````"

        marker
        (str fence "vis-table")]

    (if-not (str/includes? text marker)
      text
      (str/join "\n"
                (loop [lines
                       (str/split-lines text)

                       out
                       []]

                  (if (empty? lines)
                    out
                    (let [[line & more] lines]
                      (if (= (str/trim line) marker)
                        (let [summary (str/trim (str (first more)))
                              after (drop-while #(not= (str/trim %) fence) (rest more))]

                          (recur (rest after)
                                 (conj out
                                       (str (if (str/blank? summary) "[Table]" summary)
                                            " — rows are NOT in this context: the grid is rendered"
                                            " in the transcript and the bytes are a stored"
                                            " attachment (list_attachments() lists it,"
                                            " read_attachment(id) opens it)."))))
                        (recur more (conj out line))))))))))

(defn- iteration-results-message
  "Render ONE prior tool-call iteration as the `tool_result` user message that
   answers its `tool_use`(s): the canonical stdout projection, plus errors and
   any `summarize`/`drop` fold lines.
   One `tool_result` block per `tool_use`, each carrying ITS OWN forms' output (forms are grouped
   by `:svar/tool-call-id`), because one reply may carry several
   `python_execution` calls.
   Falls back to a plain text user message when no tool calls are recorded.
   `:echo-source?` on the record also prefixes each form's output with the source
   that produced it, for the degraded replays where the assistant message
   carrying that source never reaches the wire."
  [iter-record]
  (let [;; ONE scope source: the `forms-vec` (each carrying stdout/error facts).
        ;; Falls back to scoped `:blocks` forms.
        forms
        (or (:forms-vec iter-record)
            (mapcat (fn [b]
                      (or (seq (:forms b)) [b]))
                    (:blocks iter-record)))

        ;; Synthetic forms injected by apply-summaries render first as one Python
        ;; comment naming the replaced scopes. A gist-less fold uses the dropped
        ;; label; a fold with a gist carries its takeaway:
        ;;   # ⋯ folded t1/i1-i2 · <gist>
        ;;   # ⋯ dropped t1/i3 · <note>
        summary-lines
        (keep (fn [f]
                (when (:summary? f)
                  (let [at
                        (or (ctx-engine/pretty-scopes (:summary-iters f) nil)
                            (str/join "," (:summary-iters f)))

                        note
                        (:summary-note f)

                        g
                        (:summary-gist f)]

                    (str "# ⋯ "
                         (if (:summary-drop? f) "dropped " "folded ")
                         at
                         note
                         (when g (str " · " g))))))
              forms)

        ;; Printed stdout is the only successful output. The same bounded projection
        ;; reaches model replay, live gateway events and human cards. A restored
        ;; iteration provides its serving model even for older forms without the key.
        stdout-wire
        (fn [f]
          (when-not (str/blank? (str (:stdout f)))
            (form/clip-to-wire (elide-table-fences (:stdout f))
                               (assoc f :llm-model (or (:llm-model f) (:llm-model iter-record))))))

        form-output
        (fn [f]
          (cond (:summary? f) nil
                (:error f) (let [err (error->display (:error f))]
                             (if-let [out (stdout-wire f)]
                               (str out "\n" err)
                               err))
                :else (stdout-wire f)))

        ;; Output alone is unattributable once the assistant message that carried
        ;; the code is gone (a cancelled turn's cross-turn seed, a dropped
        ;; thinking replay). `:echo-source?` puts each form's own source back in
        ;; front of its output, so the next request continues the work instead of
        ;; re-running it to find out what it already did.
        form-line
        (fn [f]
          (let [out (form-output f)]
            (if-let [src (and (:echo-source? iter-record)
                              (not (:summary? f))
                              (not-empty (str/trim (str (:src f)))))]
              (str "```python\n" src "\n```" (when out (str "\n" out)))
              out)))

        ;; ctx structural delta (executable `ctx["a"]["b"] = …` / `del ctx[…]`),
        ;; emitted only when ctx changed — rides the SAME message, append-only.
        ctx-diff
        (not-empty (some-> (:ctx-diff iter-record)
                           str
                           str/trim))

        ;; A semantic live view may close on a gateway thread AFTER its block returned.
        ;; Its record is then appended to the iteration, not to the already-frozen form
        ;; output. Reintroduce that durable descriptor into later model context so the
        ;; next request knows the interrupted picture exists and can open it.
        live-records
        (filter model-live-record? (:attachments iter-record))

        tool-calls
        (seq (:tool-calls iter-record))

        ;; Forms grouped by the tool_use they answer. Ownerless summarize/drop fold
        ;; forms belong to the iteration and ride on its first call.
        forms-by-id
        (group-by :svar/tool-call-id forms)

        orphan-forms
        (get forms-by-id nil)

        ;; Build the wire body for ONE tool-call from ITS OWN forms, plus the
        ;; iteration-level lines (folds / form-budget / ctx delta) carried on the
        ;; first call only (they describe the whole reply, not a single call).
        call-content
        (fn [idx tc]
          (let [own
                (cond-> (vec (get forms-by-id (:id tc)))
                  (zero? (long idx))
                  (into (or orphan-forms [])))

                records
                (filter (fn [att]
                          (let [owner (or (:tool-call-id att) (:tool_call_id att))]
                            (if owner (= (str owner) (str (:id tc))) (zero? (long idx)))))
                        live-records)

                lines
                (concat (keep form-line own) (map live-record-context-line records))

                iscope
                (some #(iter-of-scope (:scope %)) own)

                header
                (when (and iscope (seq lines)) (str "# " iscope))

                body-ls
                (concat (when (zero? (long idx)) summary-lines) (when header [header]) lines)

                body
                (when (seq body-ls) (str/join "\n" body-ls))]

            (str/join "\n\n" (remove str/blank? [body (when (zero? (long idx)) ctx-diff)]))))

        ;; Text-only iteration with no tool calls: join its forms.
        fallback-content
        (let [lines
              (concat (keep form-line forms) (map live-record-context-line live-records))

              iscope
              (some #(iter-of-scope (:scope %)) forms)

              header
              (when (and iscope (seq lines)) (str "# " iscope))

              body-ls
              (concat summary-lines (when header [header]) lines)

              body
              (when (seq body-ls) (str/join "\n" body-ls))]

          (str/join "\n\n" (remove str/blank? [body (:goal-continuation iter-record) ctx-diff])))]

    (cond
      ;; Collapsed by summarize/drop: the whole iteration is gone — emit ONLY the
      ;; gist line as plain text (conversation-suffix drops its assistant +
      ;; tool_result pair, so there is no tool_use to answer here).
      (:collapsed? iter-record) (when-let [body (not-empty (str/join "\n" summary-lines))]
                                  {:role "user" :content body})
      ;; Native/tool-call iteration: emit ONE `tool_result` per `tool_use` (the
      ;; API requires every call be answered), each carrying ITS OWN forms'
      ;; output. One of the calls may be python_execution, the rest direct
      ;; file tools, and each owns its result.
      tool-calls
      {:role "user"
       :content
       (vec
         (map-indexed
           (fn [idx tc]
             (let [own
                   (cond-> (vec (get forms-by-id (:id tc)))
                     (zero? (long idx))
                     (into (or orphan-forms [])))

                   ;; A tool call FAILED when any of its forms errored.
                   ;; Flag the tool_result `:is_error true` so the model
                   ;; treats it as a failure, not an empty success.
                   ;; svar passes it to Anthropic as `is_error: true`;
                   ;; on OpenAI/Gemini (no structured flag) the error TEXT
                   ;; in :content carries the signal.
                   missing-execution?
                   (not-any? (complement :summary?) own)

                   errored?
                   (or missing-execution? (boolean (some :error own)))

                   c
                   (call-content idx tc)]

               (cond->
                 {:type "tool_result"
                  :tool_use_id (:id tc)
                  :content
                  (cond
                    missing-execution? (str
                                         "Tool call was not executed or its result is unavailable."
                                         (when-not (str/blank? c) (str "\n" c)))
                    (str/blank? c)
                    "(no return — python_execution returns what it print()s; this call printed nothing. print() what you want to see.)"
                    :else c)}
                 errored?
                 (assoc :is_error true))))
           tool-calls))}
      ;; Text-only iteration with no tool calls.
      (not (str/blank? fallback-content)) {:role "user" :content fallback-content})))

(defn- strip-assistant-thinking
  "Cross-provider/model-SAFE version of a canonical assistant replay: drop
   the `thinking` / `redacted_thinking` blocks (opaque provider-native state
   — z.ai raw text, Anthropic HMAC, Responses encrypted items — none of
   which survive a provider/model switch) but KEEP the text and `tool_use`
   blocks, so the paired `<results>` tool_result message still answers a
   tool_use on the wire. Returns nil when nothing but thinking remains (an
   empty assistant message is a 400 on every wire)."
  [assistant-message]
  (when assistant-message
    (let [content (vec (remove #(contains? #{"thinking" "redacted_thinking"} (:type %))
                         (:content assistant-message)))]
      (when (seq content) (assoc assistant-message :content content)))))

(defn- attachment->image-block
  "Canonical multimodal image block for one stored iteration attachment. The
   `image_url` data-URI shape is svar's cross-wire canonical form — it
   translates to Anthropic `image` / OpenAI `image_url` / Gemini inline data
   per provider, and svar auto-flags the Copilot vision header when present."
  [{:keys [media-type base64]}]
  {:type "image_url"
   :image_url {:url (str "data:" (or (not-empty (str media-type)) "image/png") ";base64," base64)}})

(defn target-supports-vision?
  "True when THIS PROVIDER's serving of the replay `target` takes image input.

   Provider-scoped on purpose. The model NAME is not the answer: Copilot proxies
   vision-capable Claude/GPT under names svar's static table never learned, every
   OpenRouter model is a namespaced slug, and `gpt-4o-search-preview` matches the
   `gpt-4o` vision pattern while serving text only — an image block sent there is a
   400 that repeats on every later turn, because attachments replay. So the question
   goes to `provider-model-metadata`, which reads models.dev's per-provider input
   modalities and lets a `:capabilities` set written in config override them.

   A provider that already ANSWERED the question outranks every table, at the scope
   its answer proves: an endpoint whose WIRE refused an image content part outright
   is blind for everything it serves (`vision-describe/image-blind-provider?`), while
   a model that answered it cannot read pixels is blind by NAME wherever it is served
   from (`vision-describe/image-blind-model?`) and leaves its provider's other models
   seeing."
  [target]
  (and (not (vision-describe/image-blind-provider? (:provider target)))
       (not (vision-describe/image-blind-model? (:model target)))
       (contains? (:capabilities (catalog/model-metadata (:provider target)
                                                         (cond-> {:name (str (:model target))}
                                                           (seq (:capabilities target))
                                                           (assoc :capabilities
                                                             (:capabilities target)))))
                  :vision)))

(defn- wire-image-attachment
  "One stored iteration attachment as the wire will carry it, or nil.

   The whole verdict lives in `attachments/wire-image` (the ONE send-time image
   gate): a generic `attach` artifact (csv/json/pdf/wav/…) is DB- and
   display-only, an `image/svg+xml` figure or a BMP is re-containered to PNG,
   and a payload the decoder cannot turn into pixels — a corrupt raster whose
   header sniffs perfectly — is DROPPED. Dropping matters more here than
   anywhere: handing such a row over as an `image_url` block is a hard 400 that
   repeats on EVERY later turn, because attachments replay, so one bad row
   otherwise kills the whole session for good. Judged on the way out, it costs
   that one figure and the session lives.

   A row whose audience is the HUMAN alone never even reaches the decoder: it
   was recorded for the human, and its bytes are not this model's business."
  [attachment]
  (when-not (attachments/hidden-from-model? attachment)
    (let [stored
          (if-let [db (::attachment-db attachment)]
            (persistance/db-read-attachment db (:id attachment))
            attachment)

          wired
          (some-> stored
                  attachment-storage/hydrate
                  attachments/wire-image)]

      (when (:base64 wired) wired))))

(def ^:private max-replay-image-bytes
  "Base64 budget for ALL produced images replayed in ONE request.

   Multimodal history is not a reference: every prior figure is re-uploaded, in
   full, on every single request for the rest of the session. Unbudgeted that
   grows without bound until the provider rejects the request outright — and
   then EVERY later turn fails too, including plain text ones, because the same
   oversized history is rebuilt each time. A session must not be able to brick
   itself by plotting one figure too many, so the newest images ride and the
   older ones step off (and are NAMED, see `dropped-images-note`)."
  (* 8 1024 1024))

(def ^:private max-replay-images
  "Hard count ceiling for replayed images, independent of bytes: many small
   figures still cost real vision tokens on every request."
  16)

(defn- replay-image-plan
  "Decide, for ONE request, which produced images still ride and which step off.

   Newest-first greedy fill of [[max-replay-image-bytes]] / [[max-replay-images]]:
   the freshest figure is the one the model is actually reasoning about, and the
   oldest are the ones a summary already covers. The single newest image is
   ALWAYS kept, even alone over budget — a request that shows the model nothing
   is worse than a large one. `:collapsed?` iterations are skipped outright:
   `fold_session` already removed their whole pair.

   Read only budget-selected image payloads. Returns `{pos {:images [...] :dropped [...]}}` keyed by trailer
   position, so each iteration's verdict lands in ITS place in the transcript."
  [entries]
  (first
    (reduce
      (fn [[plan used-bytes used-count] [pos iter-rec]]
        (let [imgs (when-not (:collapsed? iter-rec)
                     (filter #(and (not (attachments/hidden-from-model? %))
                                   (str/starts-with? (str (:media-type %)) "image/"))
                             (concat (:attachments iter-rec) (:reinspect-attachments iter-rec))))]
          (if (empty? imgs)
            [plan used-bytes used-count]
            (let [[kept dropped b c]
                  (reduce
                    (fn [[kept dropped b c] img]
                      (let [estimate (if (:base64 img)
                                       (count (:base64 img))
                                       (* 4 (quot (+ (long (or (:size img) 0)) 2) 3)))
                            fits? (or (zero? (long c))
                                      (and (<= (+ (long b) estimate) (long max-replay-image-bytes))
                                           (< (long c) (long max-replay-images))))]

                        (if-not fits?
                          [kept (conj dropped (dissoc img :base64 ::attachment-db)) b c]
                          (if-let [wired (wire-image-attachment img)]
                            (let [sz (count (:base64 wired))]
                              (if (or (zero? (long c))
                                      (<= (+ (long b) sz) (long max-replay-image-bytes)))
                                [(conj kept wired) dropped (+ (long b) sz) (inc (long c))]
                                [kept (conj dropped (dissoc img :base64 ::attachment-db)) b c]))
                            [kept dropped b c]))))
                    [[] [] used-bytes used-count]
                    imgs)]
              [(assoc plan pos {:images kept :dropped dropped}) b c]))))
      [{} 0 0]
      ;; newest first: recency wins the budget, distance pays for it
      (reverse (vec entries)))))

(defn- attachment-recovery-label
  "How the model names a dropped image when asking for it back — its stored
   attachment id when there is one, otherwise the filename it was given."
  [attachment]
  (or (not-empty (str (:id attachment))) (not-empty (str (:filename attachment))) "image"))

(defn- dropped-images-note
  "Plain-text stand-in for images this request could not afford, or nil.

   An image that silently disappears from the history is a model hallucinating
   about pixels it can no longer see. Naming the rows — with the id that brings
   one BACK — turns a byte-budget decision into an ordinary tool call."
  [dropped]
  (when (seq dropped)
    {:role "user"
     :content (str "["
                   (count dropped)
                   " image(s) from this step are stored but NOT in this request"
                   " (image replay budget): " (str/join ", "
                                                        (map attachment-recovery-label dropped))
                   ". Call show_attachment(\"<id>\") to put one back on the next request,"
                   " or read_attachment(\"<id>\") to open its bytes in Python.]")}))

(defn- iteration-image-messages
  "The messages one prior iteration contributes AFTER its `<results>`: a
   `{:role \"user\"}` message of canonical `image_url` blocks for the images that
   fit this request's budget, then a note naming any that did not. Possibly
   empty; always a VECTOR, so callers splice rather than branch.

   Emitted as its OWN message right AFTER the iteration's `<results>` so an
   image never sits between an assistant `tool_use` and its answering
   `tool_result` (which would break tool-call adjacency on the OpenAI chat
   wire)."
  [{:keys [images dropped]}]
  (cond-> []
    (seq images)
    (conj {:role "user" :content (mapv attachment->image-block images)})

    (seq dropped)
    (conj (dropped-images-note dropped))))

(defn- iteration-description-messages
  "What one prior iteration contributes when the target CANNOT see: the images the
   budget kept, replaced by what a sighted model reported about each, then the usual
   note for the ones the budget pushed out. Possibly empty; always a VECTOR, so
   callers splice rather than branch.

   `descriptions` is the request-wide `{image description}` lookup built by
   `conversation-suffix` — a LOOKUP, not a describer, so no iteration can start a
   round trip of its own while the transcript is being rendered."
  [descriptions {:keys [images dropped]}]
  (let [described (into []
                        (keep (fn [image]
                                (when-let [description (get descriptions image)]
                                  (assoc description :label (attachment-recovery-label image)))))
                        images)]
    (cond-> []
      (seq described)
      (conj (vision-describe/descriptions-message described))

      (seq dropped)
      (conj (dropped-images-note dropped)))))

(defn replay-image-describer
  "Describer for replayed figures, or nil when the vision fallback is off or nothing
   in the fleet can see. Resolved per request (a provider switch mid-session takes
   effect immediately) but it costs only router arithmetic: the calls happen lazily,
   per image actually in play, and each image is described once per process."
  [environment context preferred-provider]
  (let [router (:router environment)]
    (when (vision-describe/available? router)
      (fn [images]
        (vision-describe/describe-images router context images preferred-provider)))))

(defn- conversation-suffix-groups
  "Append-only conversation suffix for the current turn, kept as `[pos messages]`
   GROUPS so a caller can price ONE iteration: each prior iteration
   as an `[assistant-replay, <results> user message]` PAIR, in iteration
   order — the tool-call/tool-result shape (see the wire shape documented
   above). The assistant replay carries provider-native thinking payloads
   (signed Anthropic thinking / z.ai reasoning / Responses items) so the model
   keeps its reasoning session; the results message carries what running that
   iteration's code actually returned.

   A provider/model MISMATCH (mid-turn fallback, health-gate demotion making
   selected≠actual, model-name aliasing) must NOT blind the model: the old
   behaviour dropped the whole pair, so the model never saw its own tool
   results and re-issued the identical call every iteration. Now only the
   opaque THINKING is dropped (`strip-assistant-thinking`) — the tool_use +
   results still replay. When an entry has no assistant message at all
   (or nothing but thinking), its results degrade to a PLAIN TEXT user
   message (a tool_result with no answering tool_use is a wire error).

   Cross-turn seeds (`:preserved-thinking/replay? false`) from completed turns
   stay fully excluded when the canonical recap carries their outcome. An exact
   prior-request prefix excludes them one layer earlier as well; otherwise the
   carried request and the seeded trailer would duplicate the same turn. Seeds
   from terminal incomplete turns replay only their settled results as plain text
   (never opaque thinking or orphaned tool_result blocks), preserving
   cancellation/error continuity without duplicating successful-turn evidence.

   Compatible entries route through `preserved-thinking-replay-messages`
   so the oversized-chain telemetry stays.

   Produced IMAGES replay under a per-request budget (`replay-image-plan`):
   images are re-uploaded in full on every request, so an unbounded history
   eventually exceeds the provider's request limit and breaks every later
   turn. Newest wins; older ones are named instead of sent. A target with no
   vision takes the same plan through `:describe-images` instead: the figures
   become one sighted model's report, so a blind model still knows what it drew."
  ;; 2-arity: no side-channel at all — used by the emergency-fold ESTIMATOR, which
  ;; re-prices the same trailer repeatedly and must never make a network call.
  ([trailer-iters target] (conversation-suffix-groups trailer-iters target nil))
  ([trailer-iters target {:keys [describe-images]}]
   (let [iters
         (vec (or trailer-iters []))

         compatible
         (into #{} (map first) (compatible-preserved-thinking-trailer-iters iters target))

         ;; Generated figures replay only to a vision-capable target; a
         ;; text-only model gets the fence's summary/ASCII already carried in
         ;; the results text, never image blocks it can't consume.
         vision?
         (target-supports-vision? target)

         ;; A blind target still gets the figures — as TEXT. The same newest-first plan
         ;; decides which ones are in play, then each rides as another model's report
         ;; instead of pixels this one cannot read.
         describer
         (when-not vision? describe-images)

         ;; ONE newest-first pass over the whole trailer, so the byte budget is
         ;; decided for the request as a whole rather than per iteration.
         image-plan
         (when (or vision? describer) (replay-image-plan iters))

         ;; ONE describe pass for the WHOLE trailer. The deadline and the burst cap
         ;; belong to the REQUEST: called per iteration, eight figures from eight
         ;; steps cost eight serial round trips and eight deadlines, all of it inside
         ;; request assembly with the user waiting.
         replay-descriptions
         (when describer
           (let [planned (into [] (mapcat :images) (vals image-plan))]
             (when (seq planned) (zipmap planned (or (describer planned) (repeat nil))))))

         group-of
         (fn [[pos iter-rec :as entry]]
           (let [results
                 (iteration-results-message iter-rec)

                 ;; Image artifacts this iteration produced, as their OWN
                 ;; message(s) appended AFTER the results (keeps tool_use/tool_result
                 ;; adjacency intact). Empty for text targets, image-less iters, and
                 ;; iterations the budget pushed out (which contribute a note).
                 img
                 (cond vision? (iteration-image-messages (get image-plan pos))
                       describer (iteration-description-messages replay-descriptions
                                                                 (get image-plan pos))
                       :else nil)

                 +img
                 (fn [msgs]
                   (into (vec msgs) img))]

             (cond
               ;; Collapse wins over provenance: a `fold_session` that covered
               ;; this iteration removes its whole assistant +
               ;; tool_result pair AND its generated image. The figure's vision
               ;; visibility TRACKS its iteration's textual visibility (one
               ;; invariant), so a folded step keeps only its one-line gist
               ;; (plain text) — real compaction, bytes and all. Checked BEFORE
               ;; the cross-turn seed branch so a folded seed also drops its
               ;; image; otherwise a prior-turn figure would be byte-immune to
               ;; compaction and re-billed to the vision model every turn.
               (:collapsed? iter-rec) (if results [results] [])
               ;; Cross-turn seed (NOT collapsed): never replay opaque thinking.
               ;; A terminal incomplete turn has no reliable answer summary, so
               ;; preserve its settled outputs as ordinary text; removing
               ;; :tool-calls prevents orphaned tool_result blocks. Successful
               ;; turns already carry their outcome in the prior-turn recap and
               ;; continue to emit only any previously-unwired image artifacts.
               (false? (:preserved-thinking/replay? iter-rec))
               (if (terminal-incomplete-turn-status? (:cross-turn/turn-status iter-rec))
                 (if-let [textual (iteration-results-message (-> iter-rec
                                                                 (dissoc :tool-calls)
                                                                 (assoc :echo-source? true)))]
                   (+img [textual])
                   (vec img))
                 (vec img))
               ;; Same provider+model, valid signature → verbatim replay
               ;; with the full thinking chain.
               (contains? compatible pos)
               (+img (let [replay (first (preserved-thinking-replay-messages [entry]))]
                       (cond-> [replay]
                         results
                         (conj results))))
               ;; Mismatched provider/model or poisoned signature: replay
               ;; SANS thinking so the tool_use ids stay answerable, then
               ;; the results.
               :else (if-let [stripped (strip-assistant-thinking (:assistant-message iter-rec))]
                       (+img (cond-> [stripped]
                               results
                               (conj results)))
                       ;; No assistant message (errored before one landed) or
                       ;; nothing but thinking: no tool_use to answer — degrade
                       ;; the results to plain text.
                       (if-let [textual (iteration-results-message (-> iter-rec
                                                                       (dissoc :tool-calls)
                                                                       (assoc :echo-source? true)))]
                         (+img [textual])
                         [])))))]

     (mapv (fn [[pos iter-rec :as entry]]
             [pos
              (vec (concat (when (and (not (:collapsed? iter-rec)) (:council-input iter-rec))
                             [(council/input-message (:council-input iter-rec))])
                           (group-of entry)))])
           iters))))

(defn conversation-suffix
  "The append-only conversation suffix itself: `conversation-suffix-groups`
   concatenated in iteration order. Callers that need to know WHICH iteration a
   message came from (the fold estimator, which prices what one fold removes) take
   the groups; everyone building a request takes this."
  ([trailer-iters target] (conversation-suffix trailer-iters target nil))
  ([trailer-iters target opts]
   (into [] (mapcat second) (conversation-suffix-groups trailer-iters target opts))))

(defn- form-wire-text
  "The visible source, bounded stdout and error removed when this form is folded."
  [f]
  (when-not (:summary? f)
    (str (:code f)
         "\n" (form/clip-to-wire (elide-table-fences (:stdout f)) f)
         "\n" (when (:error f) (error->display (:error f))))))

(def ^:private MESSAGE_FRAME_TOKENS
  "What ONE iteration costs BESIDE its text, for the same fallback: the assistant
   message and the `tool_result` user message answering it — role envelopes, the
   `tool_use` id and name, the per-call result header, and the assistant PROSE that
   rides with the call. A fold removes that pair whole, so pricing text alone made a
   sweep of many small steps look nearly free. 250 is the conservative side of the
   ~277 the measured folds imply."
  250)

(defn- estimated-iteration-tokens
  "Tokenizer-backed fallback for an iteration before its model is resolved. The
   same Svar unknown-model fallback prices the canonical bounded stdout."
  ^long [wire-rec]
  (+ (long MESSAGE_FRAME_TOKENS)
     (long (svar/count-tokens "unknown"
                              (str (:thinking wire-rec)
                                   "\n"
                                   (str/join "\n" (keep form-wire-text (:forms-vec wire-rec))))))))

(defn- measured-iteration-tokens
  "`{pos tokens}` for the visible projection, tokenized from the canonical messages
   the next request will carry.

   `conversation-suffix-groups` keeps the one render attributed by iteration, so the
   count includes assistant thinking/prose/tool calls, matching tool results, message
   envelopes and image geometry. The no-describer arity keeps pricing offline."
  [model replay-target wire-iters]
  (into {}
        (map (fn [[pos msgs]]
               [pos (messages-wire-tokens model msgs)]))
        (conversation-suffix-groups wire-iters replay-target)))

(defn stamp-iter-universe!
  "Record the raw iteration universe while pricing only `wire-iters` — the current
   provider-visible projection. A resolved model tokenizes each iteration's rendered
   messages; without one, weights degrade to `estimated-iteration-tokens`."
  ([ctx-atom trailer-iters] (stamp-iter-universe! ctx-atom trailer-iters nil nil))
  ([ctx-atom trailer-iters wire-iters] (stamp-iter-universe! ctx-atom trailer-iters wire-iters nil))
  ([ctx-atom trailer-iters wire-iters pricing]
   (when ctx-atom
     (let [scope-of
           iteration-record-scope

           uni
           (into []
                 (comp (keep (fn [[_ rec]]
                               (scope-of rec)))
                       (distinct))
                 trailer-iters)

           ;; Price the visible projection: already-folded and completed off-wire seeds weigh zero;
           ;; incomplete seeds retain weight because their settled results replay.
           off-wire-seed?
           (fn [rec]
             (and (false? (:preserved-thinking/replay? rec))
                  (not (terminal-incomplete-turn-status? (:cross-turn/turn-status rec)))))

           visible
           (or wire-iters trailer-iters)

           model
           (when (util/non-blank-string? (:model pricing)) (:model pricing))

           measured
           (when model (measured-iteration-tokens model (:replay-target pricing) visible))

           ;; What a fold of this iteration REMOVES from the wire. An already-collapsed
           ;; record has nothing left to remove — the render would still price its gist
           ;; line — so it prices ZERO rather than recharging what an earlier fold freed.
           weights
           (persistent! (reduce (fn [m [[_ raw-rec] [pos wire-rec]]]
                                  (if-let [sc (scope-of raw-rec)]
                                    (let [toks (cond (or (:collapsed? wire-rec)
                                                         (off-wire-seed? raw-rec)
                                                         (off-wire-seed? wire-rec))
                                                     0
                                                     measured (long (get measured pos 0))
                                                     :else (estimated-iteration-tokens wire-rec))]
                                      (assoc! m sc (+ (long (get m sc 0)) toks)))
                                    m))
                                (transient {})
                                (map vector trailer-iters visible)))]

       (swap! ctx-atom assoc "engine_iter_universe" uni "engine_iter_weights" weights)))))

;; ── The model-facing surface: ONE tool ───────────────────────────────────────
;; `python_execution` is the only call the provider ever sees. Every capability is
;; a plain Python name bound into CPython inside it, so confinement, rendering and
;; docs have exactly one home and the model never routes between surfaces.
;; Replying with plain text and NO tool call ends the turn.

(defn- python-execution-capability-line
  "Describe confirmed sandbox capabilities without duplicating the detailed,
   immutable policy exposed in `session[\"access\"]`."
  [caps]
  (when caps
    (let
      [net
       (:network caps)

       net-on?
       (boolean (:enabled? net))

       allowed
       (seq (remove #(= "*" (str %)) (:allowed-domains net)))

       star?
       (some #(= "*" (str %)) (:allowed-domains net))

       fs-part
       (if (:fs? caps)
         "FS: see `session[\"access\"][\"filesystem\"]` for effective roots and modes; prefer `ls`/`grep` over shell."
         "FS: unavailable.")

       net-part
       (cond (not net-on?) "Network: off."
             allowed (str "Network: on, reachable hosts: " (str/join ", " allowed) ".")
             star? "Network: on (any host except blocked defaults)."
             :else "Network: on; see `session[\"access\"][\"network\"]`.")]

      (str fs-part " " net-part))))

(defn- python-execution-tool
  "The engine-level `python_execution` tool schema — the ONLY tool the provider
   is given. Batched, transformed, filtered, chained and structural workflows all
   run here, so intermediate data never lands in context. The capability line is
   built from `caps` so fs/network claims match what the sandbox can actually do."
  [caps]
  {:name "python_execution"
   :description
   (str
     "Run Python in the session sandbox — the only call. `print(...)` is the ONLY channel back: the block "
     "runs as a script, so what it prints is what returns, and it ends by printing exactly what the answer "
     "needs. Batch, filter and chain work here: "
     "`await gather(...)` runs independent calls together. State persists; "
     "project packages need a project REPL. "
     "Nothing is silent: errors surface whether the block printed or not. "
     (when (toggles/enabled? "improve")
       (str
         "Each failed python_execution is automatically saved in Improve as complain from autocomplain, "
         "with its session and turn/iteration/form; it never auto-pings peers. "
         "The failure reports those coordinates and its entry ID. "))
     "Every capability is a plain Python "
     "name here, so a result is an ordinary value you keep in a variable, and printing is what carries it "
     "into the transcript. A shell is WATCHED here: `sh = await shell(...)`, then a BOUNDED "
     "loop that calls `sh.logs()` on the handle it got back and breaks on what it read (an error line, "
     "a parsed port); `sh.wait(secs)` is that loop already written — no tool "
     "waits for you. A file or socket you drop is closed for you; close what you KEEP "
     "(`with open(...)`, `sh.stop()`) — the sandbox refuses an open past its descriptor "
     "ceiling (`VIS_PY_MAX_OPEN_FILES`), because a full table stops `shell` children spawning."
     (when-let [cap (python-execution-capability-line caps)]
       (str " " cap)))
   :result
   "Exactly captured `print(...)` output (empty string when the block printed nothing — an unprinted value does not come back); evaluation failures are failed tool results, not result objects."
   :schema {:type "object"
            :properties {"code" {:type "string" :description "Python source."}}
            :required ["code"]
            :additionalProperties false}})

(defn model-facing-tools
  "The ONE provider-visible tool. `python_execution` IS the model-facing surface:
   every other capability is already a bare Python name inside that sandbox, so a
   second JSON schema advertises a door the model can open anyway — and charges
   for it on every request. Discovery of the rest is pulled, not pushed:
   `apropos(pattern)` filters names and `doc(name)` retrieves, both from inside a block.

   The raw-result contract is folded into the description here, so the one tool
   cannot reach a provider without saying what it hands back. Nothing is
   advertised `strict`: a per-wire grammar opt-in has no place on a surface that
   must reach every provider."
  [caps]
  (let [{:keys [description result] :as tool} (python-execution-tool caps)]
    [(-> tool
         (assoc :description (str description "\n\nRaw result: " result))
         (dissoc :result))]))

(def ^:private tool-protocol-leak-re
  "A LONE closing tool-call tag. `invoke`/`parameter` are the provider's own
   tool-call encoding, never data, and a mangled close tag (`</antmlutparameter>`
   instead of the real one) makes the API hand the tag itself over as the
   parameter's VALUE. The tag name is matched loosely on both sides so the
   mangling — the whole reason this arrives at all — is still recognized."
  #"\s*</[A-Za-z0-9_:.-]*(?:invoke|parameter|function_calls|function_results)>\s*")

(defn- tool-protocol-leak?
  "True when `v` is a string that is NOTHING BUT a tool-call closing tag, so it
   is transport wreckage rather than an argument. A value that merely MENTIONS
   the tag (a grep query, a paragraph about the protocol) is left alone."
  [v]
  (and (string? v) (some? (re-matches tool-protocol-leak-re v))))

(defn- report-tool-protocol-leak!
  "Record the wreckage `normalize-tool-input` is about to drop; always nil.

   Dropping it is the right repair, but the drop is also what ERASES the fault:
   the second instance of it was found solely because the mangled tag had been
   persisted in `session_turn_iteration.tool_calls`. Nothing corrupt reaches
   engine data any more, so this warning is the only trace left of a provider
   that mangled its own tool-call encoding."
  [id data]
  (tel/log! {:level :warn :id id :data data} "Dropped provider tool-call transport wreckage")
  nil)

(defn- normalize-tool-input
  "MODEL-DRIFT + EXTENSION-EDN adapter for ONE tool call's arguments.

   NOT a svar workaround: svar decodes tool arguments strings-only at the wire
   edge — it parses a provider body with the tool-argument subtrees left
   UNINTERNED (`RAW_TOOL_ARG_KEYS` / `keywordize-response`) — so nothing that
   arrives from a provider is ever a keyword. What this pass owns is the three
   things svar rightly refuses to touch:

     1. MODEL DRIFT — a model that literally writes `\":path\"` as a JSON key.
        `env/normalize-dict-key` strips that leading colon so positional
        extraction still finds the key and the call just works.
     2. EXTENSION-AUTHORED EDN — `:call` shapes are Clojure data written by
        humans, so they legitimately carry keywords.
     3. TRANSPORT WRECKAGE — the model's tool-call close tag arrives mangled
        and the provider hands the TAG over as an argument value
        (`apropos(\"</antmlutparameter>\\n\")`, `grep` with `\"\\n</invoke>\\n\"`
        under an entity-escaped key). Such an entry is DROPPED
        (`tool-protocol-leak?`), which is what the model meant: an optional
        argument disappears and `apropos()` runs, a required one is missing and
        the tool says so, instead of the call silently answering a question
        nobody asked. The same wreckage can miss the object shape entirely:
        svar's decode is strict and FAITHFUL, so an `arguments` payload that
        parsed to a JSON string/array/number comes back as a String, a vector,
        a number. Engine data is a string-keyed MAP, so a non-map lands as `{}`
        rather than travelling on to call synthesis, receipts and persistence.

   DEEP: keys are normalized at EVERY depth, not just the top level. Tools like
   `patch` carries NESTED dicts (`edits [{\":from\" …}]`); a shallow pass
   left the drift colon on those nested keys, so the synthesized Python call
   leaked `patch(\"f.clj\", [{\":from\": …}])`.

   VALUES TOO, at every depth: a keyword/symbol VALUE (`{\"op\" :delete}` out of
   extension EDN) is stringified HERE — `:delete` -> `\"delete\"`, `:a/b` ->
   `\"a/b\"` — because the sandbox boundary refuses a keyword value and
   throws `boundary-violation!`, which killed the whole tool call instead of
   running it. Everything else (edit `code` text, paths, numbers, booleans)
   passes through verbatim."
  [input]
  (letfn [(nk [k] (env/normalize-dict-key (if (keyword? k) (subs (str k) 1) (str k))))
          (nv [x] (if (keyword? x) (subs (str x) 1) (str x)))
          (walk [x]
            (cond (map? x) (into {}
                                 (keep (fn [[k v]]
                                         (if (tool-protocol-leak? v)
                                           (report-tool-protocol-leak! ::tool-protocol-leak
                                                                       {:argument (str k) :value v})
                                           [(nk k) (walk v)])))
                                 x)
                  (or (vector? x) (seq? x) (set? x)) (mapv walk x)
                  (or (keyword? x) (symbol? x)) (nv x)
                  :else x))]
    (let [normalized (walk (or input {}))]
      (if (map? normalized)
        normalized
        (do (report-tool-protocol-leak! ::tool-input-not-an-object
                                        {:type (str (type normalized)) :value (pr-str normalized)})
            {})))))

(defn normalize-tool-calls
  "THE DOOR: every tool call svar returns enters the engine HERE.

   svar hands model-authored arguments over strings-only — its response parse
   leaves tool-argument subtrees UNINTERNED, so a provider can no longer deliver
   `:path` — which makes this pass exactly one thing: the single place MODEL
   DRIFT is repaired (a literal `\":path\"` key, a stray keyword value out of an
   extension's EDN), at the point where `ask-code!`'s result becomes engine
   data.

   Vis is strings-only end to end — the sandbox, the synthesized Python,
   persistence and the wire all speak snake_case strings — so the whole
   tool-call vector is normalized ONCE. Everything downstream — call synthesis,
   replay elision, receipts and iteration records — reads plain
   string keys and must NOT re-check a keyword variant.

   A repeated call id is a provider defect: report it and pass every call
   through. Nothing here merges or drops calls."
  [tool-calls]
  (let [calls
        (vec tool-calls)

        repeated
        (->> calls
             (keep :id)
             frequencies
             (keep (fn [[id n]]
                     (when (> (long n) 1) id)))
             vec)]

    (when (seq repeated)
      (tel/log! {:level :warn :id ::duplicate-tool-call-ids :data {:ids repeated}}
                "Provider repeated tool-call ids in one response; every call still runs"))
    (mapv #(update % :input normalize-tool-input) calls)))

;; Use Anthropic's four cache breakpoints on the final system message and last three
;; transcript messages. This keeps the standing context and prior write anchors reusable;
;; explicit placement disables svar's automatic marker.
(def ^:private TRANSCRIPT_CACHE_BREAKPOINTS
  "Trailing transcript messages that carry a breakpoint. Anthropic's cap is four
   per call and the frozen system prefix always takes the first slot."
  3)

(defn- provider-prompt-cache
  "The prompt cache `provider` asks for in its policy (`:prompt-cache`), over
  the defaults every other provider gets: explicit breakpoints on the 5-minute
  tier. A 1-hour tier also widens [[prompt-cache-window-ms]], so only a provider
  whose live route honours it may claim it."
  [provider]
  (merge {:strategy :explicit-breakpoints :ttl :5m} (:prompt-cache (catalog/policy provider))))

(defn prompt-cache-policy
  "Static descriptor shared with Svar's cache-context fingerprint. It describes the
   placement [[apply-cache-breakpoints]] owns; Vis exposes one provider tool,
   `python_execution`, whose final wire schema is fingerprinted separately by Svar."
  [provider]
  (let [{:keys [strategy ttl]} (provider-prompt-cache provider)]
    {:version 1
     :strategy strategy
     :system-anchor :last-system
     :transcript-anchors TRANSCRIPT_CACHE_BREAKPOINTS
     :ttl ttl}))

(def ^:private UNCACHEABLE_BLOCK_TYPES
  "Content-block types that refuse a `cache_control` marker: preserved thinking is
   signed reasoning, not a cache anchor."
  #{"thinking" "redacted_thinking"})

(defn- cacheable-block-index
  "Index of the LAST block of `content` that may carry a breakpoint, or nil when
   every block refuses one."
  [content]
  (last (keep-indexed (fn [i blk]
                        (when-not (and (map? blk)
                                       (contains? UNCACHEABLE_BLOCK_TYPES
                                                  (some-> (or (:type blk) (get blk "type"))
                                                          name)))
                          i))
                      content)))

(defn- tag-block-cached
  "Mark the last CACHEABLE content block of message `m` with `:svar/cache true`,
   asking for `ttl` (`:1h`, or nil for the provider's 5-minute default). Coerces a
   bare-string `:content` into a text block first; leaves other shapes — and a
   message with nothing cacheable in it — untouched."
  [m ttl]
  (let [mark
        (fn [blk]
          (cond-> (assoc blk :svar/cache true)
            ttl
            (assoc :svar/cache-ttl ttl)))

        content
        (:content m)]

    (cond (string? content) (assoc m :content [(mark {:type "text" :text content})])
          (and (vector? content) (seq content))
          (if-let [i (cacheable-block-index content)]
            (let [blk (nth content i)
                  blk (if (map? blk) (mark blk) (mark {:type "text" :text (str blk)}))]

              (assoc m :content (assoc content i blk)))
            m)
          :else m)))

(defn- cache-breakpoint-indexes
  "Ordered message indexes marked by [[apply-cache-breakpoints]]."
  [messages]
  (let [messages
        (vec messages)

        n
        (count messages)

        last-system
        (last (keep-indexed (fn [i message]
                              (when (= "system"
                                       (some-> (:role message)
                                               name))
                                i))
                            messages))

        transcript-from
        (max (long (if last-system (inc (long last-system)) 0))
             (- (long n) (long TRANSCRIPT_CACHE_BREAKPOINTS)))]

    (vec (cond->> (range transcript-from n)
           last-system
           (cons last-system)))))

(defn- apply-cache-breakpoints
  "Place the four prompt-cache breakpoints on `messages` for `provider`: the last
   system-role message (frozen prefix) plus the last [[TRANSCRIPT_CACHE_BREAKPOINTS]]
   messages after it (moving recency and the
   previous request's write anchor). Anchors COLLAPSE instead of overlapping, so a
   system-only first call marks exactly one. No-op on empty."
  [messages provider]
  (let [messages
        (vec messages)

        ttl
        (let [ttl (:ttl (provider-prompt-cache provider))]
          (when-not (= :5m ttl) ttl))]

    (reduce (fn [ms i]
              (update ms i tag-block-cached ttl))
            messages
            (cache-breakpoint-indexes messages))))

(defn model-accounting-routing
  "Inspect this resolved route, even when inference is allowed to fall back elsewhere."
  [routing {:keys [provider name model]}]
  (cond-> (or routing {})
    (and provider (or name model))
    (assoc :provider
      provider :model
      (or name model))))

(defn resolved-prompt-cache-context
  "Ask Svar for the opaque fixed-prefix/cache-namespace identity of one pinned route."
  [environment resolved-model routing extra-body]
  (svar/prompt-cache-context (:router environment)
                             (cond-> {:routing (model-accounting-routing routing resolved-model)
                                      ;; There is exactly ONE model-facing tool. Sandbox capabilities alter
                                      ;; this Python schema, so they alter the context id rather than being
                                      ;; mistaken for an append-only provider prefix.
                                      :tools (model-facing-tools (:sandbox-caps environment))
                                      :tool-choice :auto
                                      :prompt-cache-policy (prompt-cache-policy (:provider
                                                                                  resolved-model))}
                               (:session-id environment)
                               (assoc :cache-key (str (:session-id environment)))

                               extra-body
                               (assoc :extra-body extra-body))))

(defn prose-beyond-code
  "The assistant `prose` (a model `:content` string streamed ALONGSIDE a tool
   call) is worth showing ONLY when it carries commentary BEYOND the code it's
   about to run. Models frequently restate the exact `python_execution` code in their
   message — as a ```fenced``` block or verbatim — which then renders as a dim
   DUPLICATE of the real code block. So strip any fenced code from the prose and
   compare what's left (and the whole prose, de-whitespaced) against the
   concatenated tool-call code; return the prose when it still says something,
   else nil. `tool-calls` are the model's `python_execution` calls; their `:input` carries
   `code`."
  [prose tool-calls]
  (when-let [p (some-> prose
                       str
                       str/trim
                       not-empty)]
    (let [code (->> tool-calls
                    (map (fn [tc]
                           (get (:input tc) "code" "")))
                    (str/join "\n"))
          squash #(str/replace (str %) #"\s+" "")
          fenced-stripped (-> p
                              (str/replace #"(?s)```.*?```" "")
                              str/trim)]

      (when-not (or (str/blank? fenced-stripped)  ;; prose was ONLY fenced code
                    (= (squash p) (squash code))) ;; prose IS the code verbatim
        p))))

(defn- provider-call-reason
  "WHY this provider request exists. The FIRST call of a turn answers the human's
   own submit; every later one is the agent loop continuing by itself on the tool
   results it just produced. A retry is never a `:provider-call` — it carries its
   own `:provider-retry-reset` marker — so those are the only two reasons."
  [^long iteration-position]
  (if (<= iteration-position 1) :user-submit :tool-result))

(defn provider-call-chunk
  "The lifecycle marker that opens ONE provider call.

   It names the provider and the model the request is dispatched to: when the
   stream then goes silent, that marker is the only thing the gateway watchdog
   has left to attribute the stall to, and a failure card that cannot say WHICH
   provider went quiet tells the human nothing.

   It also names WHY the request is being made (`:reason`): a tool-result
   continuation that the loop decided on its own must not look like the human
   pressing enter again."
  ([iteration-position resolved-model started-at-ms]
   (provider-call-chunk iteration-position resolved-model started-at-ms nil))
  ([iteration-position resolved-model started-at-ms watchdog-timeouts]
   (merge {:phase :provider-call
           :iteration iteration-position
           :reason (provider-call-reason iteration-position)
           :started-at-ms started-at-ms
           :provider (some-> (:provider resolved-model)
                             name)
           :model (some-> (:name resolved-model)
                          str)}
          (into {} (remove (comp nil? val)) watchdog-timeouts))))

(defn close-llm-session!
  "Close and forget an environment-owned stateful provider session."
  [session-atom]
  (when-let [session (:session @session-atom)]
    (try (svar/close-session! session)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::llm-session-close-failed
                      :data {:error (ex-message t)}
                      :msg "Could not close the stateful provider session"}))))
  (reset! session-atom nil))

(defn- session-message-suffix
  "Return the unsent suffix when history is an exact prefix, else nil."
  [history messages]
  (let [history
        (vec history)

        messages
        (vec messages)

        n
        (count history)]

    (when (and (<= n (count messages)) (= history (subvec messages 0 n))) (subvec messages n))))

(def ^:private PROMPT_CACHE_REUSE_FRESH_MS
  "How long a same-route prefix is expected to remain resident in the provider
   cache on the DEFAULT 5-minute tier. It gates exact cross-turn restoration and
   labels continuity samples; a stale request falls back to the compact canonical
   recap instead of replaying a large cold prefix."
  300000)

(def ^:private EXTENDED_PROMPT_CACHE_REUSE_FRESH_MS
  "The same window for a route whose provider policy asks for the 1-hour tier,
   whose breakpoints are written with `:svar/cache-ttl :1h`."
  3600000)

(defn- prompt-cache-window-ms
  "How long `provider`'s prefix is assumed to outlive the request that wrote it —
   the tier [[apply-cache-breakpoints]] asked THAT provider for, nothing else."
  ^long [provider]
  (if (= :1h (:ttl (provider-prompt-cache provider)))
    (long EXTENDED_PROMPT_CACHE_REUSE_FRESH_MS)
    (long PROMPT_CACHE_REUSE_FRESH_MS)))

(defn- valid-prompt-cache-context?
  "True for Svar's persisted opaque cache-context contract."
  [context]
  (and (map? context)
       (util/non-blank-string? (:id context))
       (integer? (:fixed-prefix-weight context))
       (<= 0 (long (:fixed-prefix-weight context)))))

(defn same-prompt-cache-context?
  [left right]
  (and (valid-prompt-cache-context? left)
       (valid-prompt-cache-context? right)
       (= (:id left) (:id right))
       (= (:fixed-prefix-weight left) (:fixed-prefix-weight right))))

(defn- shared-cache-breakpoint-prefix-count
  "Message count through the last prior breakpoint contained in `shared-count`."
  [indexes shared-count]
  (last (keep (fn [i]
                (when (< (long i) (long shared-count)) (inc (long i))))
              indexes)))

(def ^:private PROMPT_CACHE_ROUTE_LIMIT 8)

(def ^:private PROMPT_CACHE_MESSAGE_LIMIT 4096)

(defn message-cache-data
  "Fingerprint messages without retaining their payloads. Weak identity hints avoid
   serializing unchanged live messages again; collection only costs a rehash."
  [prior messages]
  (let [data (mapv (fn [i message]
                     (let [reference (get (:message-refs prior) i)]
                       (if (and reference
                                (identical? message (.get ^java.lang.ref.WeakReference reference)))
                         [(get (:fingerprints prior) i) (get (:weights prior) i) reference]
                         (let [rendered (pr-str message)]
                           [(util/sha256-hex rendered) (count rendered)
                            (java.lang.ref.WeakReference. message)]))))
                   (range (count messages))
                   messages)]
    {:fingerprints (mapv first data)
     :weights (mapv second data)
     :message-refs (mapv #(nth % 2) data)}))

(defn compact-prompt-cache-entry
  "Restore telemetry from a disk checkpoint without retaining request or answer text."
  [entry]
  (cond-> (select-keys entry [:input-tokens :at-ms :prompt-cache-context])
    (<= (count (:messages entry)) (long PROMPT_CACHE_MESSAGE_LIMIT))
    (merge (message-cache-data nil (:messages entry))
           {:breakpoints (cache-breakpoint-indexes (:messages entry))})))

(defn- bounded-prompt-cache-routes
  "Bound compact samples while retaining stale denominators for expiry telemetry."
  [routes]
  (into {} (take-last (long PROMPT_CACHE_ROUTE_LIMIT) (sort-by (comp :at-ms val) routes))))

(defn- common-prefix-count
  "How many leading messages two request vectors still share."
  ^long [left right]
  (let [n (min (count left) (count right))]
    (loop [i 0]
      (if (and (< i n) (= (nth left i) (nth right i))) (recur (inc i)) i))))

(defn note-prompt-cache-request!
  "Replace one route's request baseline and answer `{:reusable-tokens n
   :continuity kw :reuse-kind :exact|:estimated|nil}`.

   Exact append-only requests use the prior provider-counted input. Rewrites use a
   serialized-size estimate in provider cache order: Svar's fixed Python tool/preamble,
   then Vis messages, but only through the last breakpoint the shared prefix reached.
   Bytes after that anchor were never independently cacheable. A provider cache-read is
   hard evidence and floors an estimate; the current input always caps it.

   A changed Svar context id means tools, account namespace, adapter preamble, route
   or cache policy changed. Its baseline rotates under `:cache-context-changed` and
   cannot authorize replay of the old large request."
  [history-atom provider model prompt-cache-context messages input-tokens cache-read-tokens
   request-start-ms]
  (when (and history-atom provider model)
    (let [route
          [provider (str model)]

          messages
          (vec messages)

          input
          (long (or input-tokens 0))

          cached
          (long (or cache-read-tokens 0))

          at-ms
          (long request-start-ms)

          sample
          (volatile! nil)]

      (swap! history-atom
        (fn [routes]
          (let [prior
                (get routes route)

                prior-input
                (long (or (:input-tokens prior) 0))

                prior-messages
                (:fingerprints prior)

                prior-weights
                (:weights prior)

                same-context?
                (same-prompt-cache-context? (:prompt-cache-context prior) prompt-cache-context)

                data
                (message-cache-data (when same-context? prior) messages)

                prefix
                (if same-context? (common-prefix-count prior-messages (:fingerprints data)) 0)

                exact?
                (and same-context? (pos? prefix) (= prefix (count prior-messages)))

                fixed-weight
                (double (long (or (get-in prior [:prompt-cache-context :fixed-prefix-weight]) 0)))

                message-weight
                (double (reduce + 0 prior-weights))

                total-weight
                (+ fixed-weight message-weight)

                cache-prefix-count
                (long (or (shared-cache-breakpoint-prefix-count (:breakpoints prior) prefix) 0))

                prefix-message-weight
                (double (reduce + 0 (take cache-prefix-count prior-weights)))

                prefix-weight
                (+ prefix-message-weight
                   (if (and same-context? (pos? cache-prefix-count)) fixed-weight 0.0))

                estimated
                (long (cond (or (zero? prior-input) (zero? cache-prefix-count)) 0
                            exact? prior-input
                            (pos? total-weight) (Math/floor (* prior-input
                                                               (/ prefix-weight total-weight)))
                            :else 0))

                measured-floor
                (min input cached)

                reusable
                ;; A long: `pos?` on a boxed cond result is boxed math the lint gate names.
                (long (cond (nil? prior) 0
                            exact? (min input prior-input)
                            :else (min input (max measured-floor estimated))))

                age-ms
                (- at-ms (long (or (:at-ms prior) at-ms)))

                expired?
                (and exact?
                     (pos? reusable)
                     (zero? cached)
                     (> age-ms (prompt-cache-window-ms provider)))

                continuity
                (cond (nil? prior) :initial
                      (not same-context?) :cache-context-changed
                      expired? :expired
                      exact? :append-only
                      :else :rewrite)

                reuse-kind
                (when (pos? reusable) (if exact? :exact :estimated))]

            (vreset! sample
                     {:reusable-tokens reusable :continuity continuity :reuse-kind reuse-kind})
            (bounded-prompt-cache-routes
              (assoc routes
                route (cond-> {:input-tokens input
                               :at-ms at-ms
                               :prompt-cache-context prompt-cache-context}
                        (<= (count messages) (long PROMPT_CACHE_MESSAGE_LIMIT))
                        (merge data {:breakpoints (cache-breakpoint-indexes messages)})))))))
      @sample)))

(defn current-session-summaries
  "Immutable fold-ledger value used to decide whether a provider prefix is still semantic."
  [environment]
  (some-> (:ctx-atom environment)
          deref
          (get "session_summaries")))

(defn- prompt-cache-entry-fresh?
  "True while ENTRY's last request is inside PROVIDER's conservative cache window."
  [provider entry]
  (when (integer? (:at-ms entry))
    (let [age-ms (- (long (util/now-ms)) (long (:at-ms entry)))]
      (<= 0 age-ms (prompt-cache-window-ms provider)))))

(defn- valid-prompt-cache-state?
  "Validate the single exact-prefix checkpoint accepted from persistence."
  [{:keys [route entry standing-ctx]}]
  (let [[provider model]
        route

        messages
        (:messages entry)

        completed
        (:completed-turn entry)]

    (and (vector? route)
         (= 2 (count route))
         (some? provider)
         (string? model)
         (map? entry)
         (valid-prompt-cache-context? (:prompt-cache-context entry))
         (vector? messages)
         (seq messages)
         (every? map? messages)
         (vector? (:weights entry))
         (= (count messages) (count (:weights entry)))
         (integer? (:input-tokens entry))
         (map? completed)
         (integer? (:turn-position completed))
         (integer? (:stable-message-count completed))
         (map? (:assistant-message completed))
         (map? standing-ctx)
         (string? (:block standing-ctx))
         (map? (:baseline standing-ctx))
         (prompt-cache-entry-fresh? provider entry))))

(defn load-prompt-cache-state
  "Load one fresh exact-prefix checkpoint; malformed or stale state is a safe miss."
  [db-info session-state-id]
  (when (and db-info session-state-id)
    (try (let [state (persistance/db-get-session-prompt-cache-state db-info session-state-id)]
           (when (valid-prompt-cache-state? state) state))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::prompt-cache-state-load-failed
                      :data {:session-state-id (str session-state-id) :error (ex-message t)}
                      :msg "could not restore the provider prefix checkpoint"})
           nil))))

(defn- completed-prompt-cache-entry
  "Build a transient exact checkpoint after the terminal outcome is accepted.
   Neither the request nor completed answer is attached to live route history."
  [entry messages turn-position summaries stable-message-count assistant-message]
  (when (and entry
             (seq messages)
             (integer? turn-position)
             (integer? stable-message-count)
             (map? assistant-message))
    (assoc (select-keys entry [:weights :input-tokens :at-ms :prompt-cache-context])
      :messages (vec messages)
      :weights (if (= (count messages) (count (:weights entry)))
                 (:weights entry)
                 (mapv #(count (pr-str %)) messages))
      :completed-turn {:turn-position (long turn-position)
                       :summaries summaries
                       :stable-message-count (long stable-message-count)
                       :assistant-message assistant-message})))

(defn persist-prompt-cache-state!
  "Best-effort overwrite of the one restart-safe exact-prefix checkpoint."
  [environment provider model completion]
  (when (and (:db-info environment)
             (:session/state-id environment)
             (:prompt-cache-history-atom environment)
             provider
             (some? model))
    (let [route
          [provider (str model)]

          entry
          (completed-prompt-cache-entry (get @(:prompt-cache-history-atom environment) route)
                                        (:messages completion)
                                        (:turn-position completion)
                                        (:summaries completion)
                                        (:stable-message-count completion)
                                        (:assistant-message completion))

          standing-ctx
          (some-> (:standing-ctx-atom environment)
                  deref)]

      (when (and (:completed-turn entry) (map? standing-ctx))
        (try (persistance/db-set-session-prompt-cache-state!
               (:db-info environment)
               (:session/state-id environment)
               {:route route :entry entry :standing-ctx standing-ctx})
             (catch Throwable t
               (tel/log! {:level :warn
                          :id ::prompt-cache-state-store-failed
                          :data {:session-state-id (str (:session/state-id environment))
                                 :error (ex-message t)}
                          :msg "could not store the provider prefix checkpoint"})))))))

(defn resumable-prompt-message-base
  "Return an exact cross-turn provider prefix, or nil when any safety key changed.

   A hit preserves the complete final request byte-for-byte, then appends the
   accepted assistant answer and this turn's user message. Same route, adjacent
   turn, fresh cache residency, unchanged fold ledger, and an identical stable
   system prefix are all required; canonical recap assembly owns every miss."
  [state provider model prompt-cache-context turn-position summaries stable-messages turn-messages]
  (when (and state provider (some? model) (integer? turn-position))
    (let [entry
          (when (= [provider (str model)] (:route state)) (:entry state))

          completed
          (:completed-turn entry)

          request
          (vec (:messages entry))

          stable
          (vec stable-messages)

          stable-count
          (count stable)]

      (when (and completed
                 (same-prompt-cache-context? (:prompt-cache-context entry) prompt-cache-context)
                 (prompt-cache-entry-fresh? provider entry)
                 (seq stable)
                 (= (long turn-position) (inc (long (:turn-position completed))))
                 (= summaries (:summaries completed))
                 (= stable-count (:stable-message-count completed))
                 (map? (:assistant-message completed))
                 (<= stable-count (count request))
                 (= stable (subvec request 0 stable-count)))
        {:messages (into (conj request (:assistant-message completed)) turn-messages)
         :summaries summaries
         :resumed? true}))))

(defn- prompt-message-base
  "Project a fold-ledger change using the same canonical base as the next request."
  [base summaries canonical-messages-fn]
  (if (= summaries (:summaries base))
    base
    {:messages (vec (canonical-messages-fn)) :summaries summaries :resumed? false}))

(defn prompt-message-base!
  "Keep BASE while its fold ledger is unchanged; otherwise canonicalize exactly once."
  [base-atom summaries canonical-messages-fn]
  (reset! base-atom (prompt-message-base @base-atom summaries canonical-messages-fn)))

(defn conversation-trailer-for-base
  "Hide cross-turn seeds already present in an exact carried request prefix."
  [trailer-iters resumed?]
  (if resumed?
    (filterv (fn [[_ iter-rec]]
               (not (false? (:preserved-thinking/replay? iter-rec))))
      (or trailer-iters []))
    (vec (or trailer-iters []))))

(defn conversation-messages
  "Combine the selected base and folded trailer exactly as sent to the provider."
  [base trailer-iters replay-target options]
  (into (vec (:messages base))
        (conversation-suffix (conversation-trailer-for-base trailer-iters (:resumed? base))
                             replay-target
                             options)))

(defn request-fold-estimator
  "Price the before/after request projections, including carried prefixes and replacement gists.
   Neither the live base nor its fold ledger is changed by this local-tokenizer estimate."
  [{:keys [message-base-atom canonical-messages-fn trailer-iters emergency-summaries-atom
           replay-target conversation-options count-messages-fn]}]
  (fn [before after]
    (let [project (fn [ctx]
                    (let [summaries (get ctx "session_summaries")
                          base (prompt-message-base @message-base-atom
                                                    summaries
                                                    #(canonical-messages-fn ctx))]

                      (conversation-messages
                        base
                        (apply-summaries trailer-iters (into @emergency-summaries-atom summaries))
                        replay-target
                        conversation-options)))]
      (- (long (count-messages-fn (:model replay-target) (project before)))
         (long (count-messages-fn (:model replay-target) (project after)))))))

(defn- same-effective-router?
  "True when two hydrated routers carry the same values and reload generation."
  [left right]
  (and (= left right) (= (meta left) (meta right))))

(defn- pin-session-route
  "Pin a session turn to the model Vis already resolved."
  [opts provider model]
  (assoc opts
    :routing (assoc (:routing opts)
               :provider provider
               :model model)))

(defn- ask-code-with-session!
  "Keep one opaque Svar session per effective router for a provider whose prompt
   cache is a server continuation; other providers stay one-shot."
  [environment resolved-model ask-opts]
  (let [provider
        (:provider resolved-model)

        model
        (:name resolved-model)

        router
        (:router environment)

        session-atom
        (:llm-session-atom environment)]

    (if (and (= :server-continuation (:strategy (provider-prompt-cache provider))) session-atom)
      (locking session-atom
        (let [entry
              @session-atom

              current
              (when (and (= provider (:provider entry))
                         (same-effective-router? router (:router entry)))
                entry)

              _
              (when (and entry (nil? current)) (close-llm-session! session-atom))

              session
              (or (:session current)
                  (svar/open-session router
                                     (-> ask-opts
                                         (dissoc :messages)
                                         (pin-session-route provider model))))

              _
              (when-not current
                (reset! session-atom {:provider provider :router router :session session}))

              messages
              (vec (:messages ask-opts))

              suffix
              (session-message-suffix (svar/session-history session) messages)

              turn-opts
              (if (some? suffix)
                (assoc ask-opts :messages suffix)
                ;; Mutable context tails and compaction can rewrite canonical history.
                ;; Reset only the logical response chain; the physical socket stays hot.
                (-> ask-opts
                    (dissoc :messages)
                    (assoc :history messages)))]

          (svar/ask! session (pin-session-route turn-opts provider model))))
      (do (when (and session-atom @session-atom)
            (locking session-atom (close-llm-session! session-atom)))
          (svar/ask-code! (:router environment)
                          (update ask-opts :messages apply-cache-breakpoints provider))))))

(defn ask-code-with-first-output-timeout!
  "Bound one provider attempt without cancelling its turn. Svar polls this attempt's
   cancel predicate on both SSE and WebSocket transports. Only an abort initiated
   by this deadline becomes a retryable stream timeout; a user Stop keeps its type.
   Text, reasoning or tool-input progress disables the first-output deadline.
   Tag truncated streams with observed output before this call can return any code."
  [environment resolved-model ask-opts timeout-ms]
  (let [started
        (System/nanoTime)

        timeout-ns
        (* (long timeout-ms) 1000000)

        phase
        (atom :waiting)

        stream-output
        (atom :none)

        caller-cancel?
        (:cancel-fn ask-opts)

        cancelled?
        (fn []
          (boolean (and caller-cancel? (caller-cancel?))))

        on-chunk
        (:on-chunk ask-opts)

        opts
        (assoc ask-opts
          :cancel-fn (fn []
                       (or (cancelled?)
                           (= :timed-out @phase)
                           (and (>= (- (System/nanoTime) started) timeout-ns)
                                (compare-and-set! phase :waiting :timed-out))))
          :on-chunk
          (fn [chunk]
            (when (some seq
                        ((juxt :content :reasoning :tool-input :tool-call-preview :tool-calls)
                          chunk))
              (compare-and-set! phase :waiting :output))
            ;; Tool arguments may never become visible text. Any such progress
            ;; disqualifies a reasoning-only retry, even if later frames are empty.
            (cond (some seq ((juxt :content :tool-input :tool-call-preview :tool-calls) chunk))
                  (reset! stream-output :content)
                  (seq (:reasoning chunk)) (compare-and-set! stream-output :none :reasoning))
            ;; A late frame must not become visible before this attempt is retried.
            (when (and on-chunk (not= :timed-out @phase)) (on-chunk chunk))))]

    (try (ask-code-with-session! environment resolved-model opts)
         (catch Exception e
           (if (and (= :timed-out @phase)
                    (not (cancelled?))
                    (some #(or (= :svar.core/stream-cancelled (:type (ex-data %)))
                               (instance? InterruptedException %))
                          (loop-errors/bounded-cause-chain e)))
             (do
               ;; Svar/HTTP wrappers may restore the interrupt after classifying the
               ;; abort. Consume only our own deadline's interrupt before retry/backoff.
               (Thread/interrupted)
               (throw (ex-info (str "Provider produced no output for " timeout-ms "ms.")
                               {:type :svar.core/stream-semantic-timeout
                                :source :vis-first-output-watchdog
                                :stream? true
                                :first-output-timeout? true
                                :semantic-timeout-ms timeout-ms}
                               e)))
             (if-let [truncated (some #(when (perr/stream-truncated-error? %) %)
                                      (loop-errors/bounded-cause-chain e))]
               (let [data (ex-data truncated)
                     output (if (or (pos? (long (or (:content-acc-len data) 0)))
                                    (seq (:partial-content data))
                                    (seq (:tool-calls data)))
                              :content
                              @stream-output)]

                 (throw (ex-info (ex-message truncated) (assoc data :stream-output output) e)))
               (throw e)))))))

(defn context-overflow-token-data
  "Keep rejection counts separate from response usage. Preflight may count remotely;
   the tokens error type alone identifies neither a provider refusal nor a local count."
  [overflow]
  (let [source (cond (= :svar.core/context-overflow (:type overflow)) :preflight
                     (#{:preflight :provider} (:source overflow)) (:source overflow)
                     :else :unknown)]
    {:rejection-source source
     :reported-count-source (if (= :provider source) :provider-error :unspecified)
     :error-type (:type overflow)
     :reported-input-tokens (when (number? (:input-tokens overflow)) (:input-tokens overflow))
     :reported-input-limit (when (number? (:max-input-tokens overflow))
                             (:max-input-tokens overflow))}))

(defn request-log-context
  "Content-free identity shared by Svar dispatch, token comparison and rescue logs."
  [environment iteration context]
  (let [turn-state (ctx-loop/read-turn-state environment)]
    (merge (select-keys context
                        [:context-recovery-attempt :prompt-base :base-message-count
                         :trailer-iteration-count])
           {:request-id (or (:request-id context) (random-uuid))
            :session-id (:session-id environment)
            :session-turn-id (or (:session-turn-id turn-state) (:environment-id environment))
            :turn (or (:turn-position turn-state) 1)
            :iteration (inc (long (or iteration 0)))})))

(defn log-context-token-counts!
  "Compare the persisted request estimate with its own response usage, not a prior
   request or retry. The health record names the prepared or logical projection.
   Reasoning contributes only logical aggregate sizes; counting failure is non-fatal."
  [messages health provider model request-context observation]
  (let [local-tokens
        (:estimated-input-tokens health)

        provider-tokens
        (:provider-input-tokens observation)

        thinking
        (for [{:keys [content]}
              messages

              :when (sequential? content)
              block
              content

              :when (= "thinking" (:type block))]

          block)

        data
        (merge
          request-context
          {:provider provider
           :model model
           :counted-projection (:counted-projection health)
           :local-estimate-model model
           :local-count-source (:token-count-source health)
           :local-input-tokens local-tokens
           :provider-count-source (if (number? provider-tokens) :provider-usage :unavailable)
           :message-count (count messages)
           :thinking-block-count (count thinking)
           :thinking-text-chars (reduce + 0 (map #(count (:thinking %)) thinking))
           :thinking-signature-chars (reduce + 0 (map #(count (:thinking-signature %)) thinking))}
          observation
          (when (and (number? local-tokens) (number? provider-tokens))
            (cond-> {:local-minus-provider-tokens (- (long local-tokens) (long provider-tokens))}
              (pos? (long provider-tokens))
              (assoc :local-to-provider-ratio
                (/ (double local-tokens) (double provider-tokens))))))]

    (tel/log! {:level (if (= :succeeded (:outcome observation)) :info :warn)
               :id ::context-token-counts
               :data data
               :msg "Context counts: local estimate, provider usage and rejection evidence"})))
