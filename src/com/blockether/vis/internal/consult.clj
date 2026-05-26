(ns com.blockether.vis.internal.consult
  "Secondary-model consultation as an async cross-iter request/result
   protocol.

   STORAGE
     Resolved consult entries land on `:session/trailer` as synthetic
     form pins at iter end. The trailer IS the home for the entry
     until the model promotes it (→ fact) or dismisses it.

     Pin shape (appended to the iter's `:forms` vec after the model's
     own forms):
       {:scope       \"tN/iM/c-K\"
        :tag         :consult
        :id  :K
        :src         \"(consult-resolved :K)\"
        :result      <entry-map>}

     The renderer surfaces it like any other observation. The model
     reads `:result` to inspect `:content`/`:citations`/`:confidence`
     and decides what to do.

   PRIMARY SURFACE
     `(consult-request! :id :pref {:focus [...] :question \"…\"})`
       → :vis/silent. Pushes intent + ctx-snapshot onto
         `:engine/pending-consults`. Engine fires a side-thread future.
     `(consult-promote! :id :fact-key)`
       → :vis/silent. Finds the resolved trailer pin by :id,
         copies its :result into a new fact under :fact-key, scrubs
         the trailer pin.
     `(consult-dismiss! :id)`
       → :vis/silent. Scrubs the trailer pin without promoting.

   Done gate refuses close while `:engine/pending-consults` is
   non-empty."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Constants
;; =============================================================================

(def DEFAULT_CONSULT_BUDGET 20)

(def DEFAULT_PREFERENCE_MAP
  "Default semantic preference → provider+model. Operators override
   via `~/.vis/config.edn` `:consult` block (env carries the resolved
   map under `:consult-config`)."
  {:fast     {:provider :anthropic-coding-plan :model "claude-haiku-4-5"}
   :balanced {:provider :anthropic-coding-plan :model "claude-sonnet-4-6"}
   :deep     {:provider :anthropic-coding-plan :model "claude-opus-4-6"}})

(def TOKEN_CAPS
  "Per-preference `:content` token cap (jtokkit cl100k_base). Engine
   re-prompts the consult LLM once when the answer overflows."
  {:fast      1000
   :balanced  4000
   :deep     12000})

(def MAX_EXCERPT_TOKENS 500)
(def MAX_CITATIONS      15)

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- warn!
  "Append a soft warning to `:engine/warnings` on the ctx-atom."
  [env code message & {:as extra}]
  (when-let [ctx-atom (:ctx-atom env)]
    (swap! ctx-atom update :engine/warnings (fnil conj [])
      (merge {:code code :message message} extra))))

(defn- budget-exhausted? [env]
  (when-let [atm (:consult-budget-atom env)]
    (let [{:keys [used cap]} @atm
          cap (or cap DEFAULT_CONSULT_BUDGET)]
      (>= (or used 0) cap))))

(defn fresh-budget-atom
  ([] (fresh-budget-atom DEFAULT_CONSULT_BUDGET))
  ([cap] (atom {:used 0 :cap cap})))

(defn- current-scope [env]
  (or (:current-form-scope env) "tN/iM/f?"))

(defn- valid-request?
  "Accepted shape is a map carrying a non-blank `:question` and an
   optional `:focus` vec of strings."
  [arg]
  (and (map? arg)
    (let [{:keys [focus question]} arg]
      (and (string? question)
        (not (str/blank? question))
        (or (nil? focus)
          (and (vector? focus) (every? string? focus)))))))

;; =============================================================================
;; consult-request! — async push
;; =============================================================================

(defn request-consult!
  "Declare an async consult intent. Returns `:vis/silent`.

   Signature:
     (consult-request! :id :preference {:focus [...] :question \"…\"})

   `:focus` OPTIONAL; when present a vec of strings.
   `:question` REQUIRED non-blank string.

   The engine pins the ctx snapshot onto the intent at request time
   so a later mutation between request and future-eval is invisible
   to the consult side."
  [env consult-id preference arg]
  (cond
    (not (keyword? consult-id))
    (warn! env :consult-invalid-id
      (str "consult-id must be a keyword; got " (pr-str consult-id)))

    (not (#{:fast :balanced :deep} preference))
    (warn! env :consult-unknown-preference
      (str "preference " (pr-str preference) " not in #{:fast :balanced :deep}")
      :anchor [consult-id])

    (not (valid-request? arg))
    (warn! env :consult-invalid-request
      (str "consult request must be a map of "
        "{:focus [string*] :question non-blank-string}; got "
        (pr-str arg))
      :anchor [consult-id])

    (budget-exhausted? env)
    (warn! env :consult-budget-exhausted
      (str "session consult budget exhausted at "
        (some-> (:consult-budget-atom env) deref :used)
        "/" (or (some-> (:consult-budget-atom env) deref :cap)
              DEFAULT_CONSULT_BUDGET))
      :anchor [consult-id])

    :else
    (let [{:keys [focus question]} arg
          snapshot (try
                     (when-let [ctx-atom (:ctx-atom env)]
                       (let [c @ctx-atom]
                         (pr-str
                           (select-keys c
                             [:session/id :session/turn :session/scope
                              :session/specs :session/tasks :session/facts
                              :session/trailer]))))
                     (catch Throwable _ ""))
          intent {:id consult-id
                  :preference preference
                  :focus      (vec (or focus []))
                  :question   question
                  :born       (current-scope env)
                  :ctx-snapshot snapshot}]
      (when-let [ctx-atom (:ctx-atom env)]
        (swap! ctx-atom
          (fn [c]
            (update c :engine/pending-consults (fnil conj []) intent))))
      (when-let [fire (:consult-fire env)]
        (fire env intent))))
  :vis/silent)

;; =============================================================================
;; Trailer pin synthesis + lookup
;; =============================================================================
;;
;; Resolved entries land on `:session/trailer` as synthetic form pins.
;; The trailer is the home until the model promotes/dismisses.

(defn- consult-pin?
  "True when a trailer form is a synthetic consult-resolution pin."
  [form]
  (= :consult (:tag form)))

(defn find-trailer-consult-pin
  "Walk `:session/trailer` and return the LAST consult pin whose
   `:id` matches `id`, or nil. Last-wins so a re-issued
   consult sees its fresh resolution."
  [ctx id]
  (let [matches (for [iter-pin (or (:session/trailer ctx) [])
                      form     (or (:forms iter-pin) [])
                      :when (and (consult-pin? form)
                              (= id (:id form)))]
                  form)]
    (last matches)))

(defn append-resolution-pin!
  "Append a synthetic consult-resolution pin to the current iter's
   trailer entry. The pin renders inline with the model's own forms
   so the model reads :result directly.

   Pin scope follows the convention `tN/iM/c-K` (`c-` prefix, `K` =
   id name). Uses the LATEST iter in the trailer; when the trailer
   is empty, synthesizes a fresh pin entry.

   Idempotent on `:id`: if a pin for the same id already
   exists in the latest iter entry, it is REPLACED rather than
   duplicated."
  [ctx-atom entry]
  (swap! ctx-atom
    (fn [c]
      (let [id (:id entry)
            trailer (vec (or (:session/trailer c) []))
            cursor  (:session/scope c)
            iter-scope (str "t" (:turn cursor) "/i" (:iter cursor))
            pin-form {:scope       (str iter-scope "/c-" (name id))
                      :tag         :consult
                      :id  id
                      :src         (str "(consult-resolved " id ")")
                      :result      entry}
            ;; Find/create the iter pin entry to append into.
            [idx existing]
            (loop [i (dec (count trailer))]
              (if (neg? i)
                [nil nil]
                (let [pin (nth trailer i)]
                  (if (and (= iter-scope (:scope pin))
                        (contains? pin :forms))
                    [i pin]
                    (recur (dec i))))))
            trailer'
            (cond
              ;; Replace within existing iter entry
              existing
              (let [forms (vec (or (:forms existing) []))
                    forms-without (vec (remove
                                         #(and (consult-pin? %)
                                            (= id (:id %)))
                                         forms))
                    forms+ (conj forms-without pin-form)]
                (assoc trailer idx (assoc existing :forms forms+)))

              ;; No iter entry yet — append a new one
              :else
              (conj trailer {:scope iter-scope :forms [pin-form]}))]
        (assoc c :session/trailer trailer')))))

(defn- scrub-trailer-pin!
  "Remove every trailer pin whose `:id` matches `id`. Walks
   the entire trailer (cross-iter) and cleans empty iter entries.
   Both `consult-promote!` and `consult-dismiss!` use this."
  [ctx-atom id]
  (swap! ctx-atom
    (fn [c]
      (let [trailer (or (:session/trailer c) [])
            trailer'
            (->> trailer
              (mapv (fn [pin]
                      (if-let [forms (:forms pin)]
                        (let [forms' (vec
                                       (remove
                                         #(and (consult-pin? %)
                                            (= id (:id %)))
                                         forms))]
                          (if (empty? forms')
                            ::drop
                            (assoc pin :forms forms')))
                        pin)))
              (remove #(= ::drop %))
              vec)]
        (assoc c :session/trailer trailer')))))

;; =============================================================================
;; consult-promote! / consult-dismiss!
;; =============================================================================

(defn promote-consult!
  "Find the resolved consult trailer pin for `:id`, copy its
   :content + :citations + :focus + :confidence into a new fact under
   `:fact-key`, and scrub the trailer pin (cross-iter).

   Returns `:vis/silent`. Warnings:
     :consult-invalid-id       consult-id not a keyword
     :consult-invalid-fact-key fact-key not a keyword
     :consult-unknown-id       no resolved pin found
     :consult-promote-failed   pin found but :status :failed"
  [env consult-id fact-key]
  (cond
    (not (keyword? consult-id))
    (warn! env :consult-invalid-id
      (str "consult-id must be a keyword; got " (pr-str consult-id)))

    (not (keyword? fact-key))
    (warn! env :consult-invalid-fact-key
      (str "fact-key must be a keyword; got " (pr-str fact-key))
      :anchor [consult-id])

    :else
    (let [ctx (some-> (:ctx-atom env) deref)
          pin (find-trailer-consult-pin ctx consult-id)
          entry (:result pin)]
      (cond
        (nil? pin)
        (warn! env :consult-unknown-id
          (str "no resolved consult-pin found for " consult-id
            "; was it declared and resolved? Pending consults block done.")
          :anchor [consult-id])

        (= :failed (:status entry))
        (warn! env :consult-promote-failed
          (str "cannot promote failed consult " consult-id
            "; dismiss it or re-issue with narrower :focus")
          :anchor [consult-id])

        :else
        (when-let [ctx-atom (:ctx-atom env)]
          (swap! ctx-atom
            (fn [c]
              (let [fact {:content    (:content entry)
                          :citations  (:citations entry)
                          :focus      (:focus entry)
                          :confidence (:confidence entry)
                          :status     :active
                          :source     :consult
                          :born       (current-scope env)}]
                (assoc-in c [:session/facts fact-key] fact))))
          (scrub-trailer-pin! ctx-atom consult-id)))))
  :vis/silent)

(defn dismiss-consult!
  "Scrub the resolved consult trailer pin for `:id` without
   promoting. Returns `:vis/silent`."
  [env consult-id]
  (cond
    (not (keyword? consult-id))
    (warn! env :consult-invalid-id
      (str "consult-id must be a keyword; got " (pr-str consult-id)))

    :else
    (let [ctx (some-> (:ctx-atom env) deref)
          pin (find-trailer-consult-pin ctx consult-id)]
      (cond
        (nil? pin)
        (warn! env :consult-unknown-id
          (str "no resolved consult-pin found for " consult-id)
          :anchor [consult-id])

        :else
        (when-let [ctx-atom (:ctx-atom env)]
          (scrub-trailer-pin! ctx-atom consult-id)))))
  :vis/silent)

;; =============================================================================
;; Pending-consult helpers (used by the parallel runner + done gate)
;; =============================================================================

(defn pending-consult-ids
  "Vec of consult-ids currently enqueued. Used by the done gate to
   refuse close while research is in flight."
  [ctx]
  (mapv :id (or (:engine/pending-consults ctx) [])))

(defn clear-pending!
  "Atomically drain `:engine/pending-consults` and return the drained
   intents. Called by the parallel runner before spawning futures."
  [ctx-atom]
  (let [drained (atom [])]
    (swap! ctx-atom
      (fn [c]
        (reset! drained (or (:engine/pending-consults c) []))
        (assoc c :engine/pending-consults [])))
    @drained))
