(ns benches.ctx-metrics
  "W6 measurement backbone — extract ENGINE BEHAVIORAL SIGNALS from a stored
   session so prompt/summarization/task changes are judged, not asserted.

   Load in the dev nREPL and point it at the live store:

     (require 'benches.ctx-metrics :reload)
     (def db (benches.ctx-metrics/open-db \"/Users/<you>/.vis/vis.mdb\"))
     (benches.ctx-metrics/report db #uuid \"b117af1a-9cd8-4374-aeca-052b286b0a11\")

   `report` prints a human table; `session-signals` returns the raw map for
   A/B diffing two runs (baseline vs a prompt change). The signals are the
   ones the b117af1a teardown surfaced (see INVESTIGATION.md baseline table):
   tasks/facts authored by the MODEL, forms-per-iter (batching), recall use,
   locate-before-edit waste, final engine-state sizes, prompt size, tokens.

   Pure analysis over `persistance` reads — no model calls, no writes."
  (:require [com.blockether.vis.internal.persistance :as p]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn open-db
  "Open a read connection to a vis store dir (the `vis.mdb` folder)."
  [path]
  (p/db-create-connection! {:backend :sqlite :path path}))

;; --- form classification (matches the INVESTIGATION.md analysis) ---
;; foundation-core is a CORE built-in now → BARE tools (cat/ls/rg/patch/write),
;; not the old v/-alias. Match both (optional `v/`) with word boundaries so
;; bare `(cat` doesn't false-match `(catch`/`(category`.
(def ^:private locate-re #"\((?:v/)?(?:rg|cat|ls)\b|\((?:apropos|doc)\b")
(def ^:private mutate-re #"\((?:v/)?(?:patch|write|append)\b|\(clj/edit|\(git/")

(defn- has? [re it] (boolean (re-find re (str (:code it)))))
(defn- locate? [it] (has? locate-re it))
(defn- mutate? [it] (has? mutate-re it))

(defn- count-verb [re iters] (count (filter #(has? re %) iters)))

(defn- turn-iters
  "All iterations of one turn, position-sorted, tagged with :turn."
  [db turn]
  (->> (p/db-list-session-turn-iterations db (:id turn))
    (map #(assoc % :turn (:position turn)))
    (sort-by :position)))

(defn- locate-before-edit
  "Iters spent locating BEFORE the first mutation in a turn — the re-discovery
   waste. The first turn's discovery is legitimate; follow-up turns re-finding
   an already-edited file is the cost we want to drive to ~0."
  [iters]
  (let [first-mut (->> iters (filter mutate?) first :position)]
    (count (filter #(and (locate? %)
                      (or (nil? first-mut) (< (:position %) first-mut)))
             iters))))

(defn- form-failed?
  "A form whose eval/tool result signals failure: an edit/tool REJECTION
   (`:status :error` — clj/edit bad op or missing target, patch no-match,
   write error) or a downstream eval EXCEPTION (`:root-ex` non-nil — how a
   broken-paren patch surfaces at require/reload). The safety-vs-corruption
   signal for the patch-vs-clj/edit question."
  [form]
  (let [r (str (:result form))]
    (boolean (or (re-find #":status :error" r)
                 (and (re-find #":root-ex" r)
                   (not (re-find #":root-ex nil" r)))))))

(defn- count-failures [iters] (count (filter form-failed? (mapcat :forms iters))))

(defn- reread-waste
  "Confirm-read-forgiving locate waste: like `locate-before-edit` but turn 1's
   discovery is free (legit) and ONE pre-edit read per follow-up turn is
   forgiven (a single `cat` before a structural rewrite is prudent, not the
   amnesia the raw metric flags). Counts only the EXCESS re-reads."
  [turn-pos iters]
  (if (= 1 turn-pos) 0 (max 0 (dec (locate-before-edit iters)))))

(defn session-signals
  "Raw signal map for a session — safe to diff across runs."
  [db sid]
  (let [sess   (p/db-get-session db sid)
        turns  (sort-by :position (p/db-list-session-turns db sid))
        per    (for [t turns]
                 (let [its (turn-iters db t)]
                   {:turn         (:position t)
                    :request      (some-> (:user-request t) str/trim
                                    (#(subs % 0 (min 60 (count %)))))
                    :iters        (count its)
                    :task-set     (count-verb #"task-set!" its)
                    :fact-set     (count-verb #"fact-set!" its)
                    :recall       (count-verb #"\(recall" its)
                    :summarize    (count-verb #"summarize" its)
                    :multi-form   (count (filter #(> (count (:forms %)) 1) its))
                    :locate-waste (locate-before-edit its)
                    :reread-waste (reread-waste (:position t) its)
                    :edit-fails   (count-failures its)
                    :in-tokens    (:input-tokens t)
                    :out-tokens   (:output-tokens t)}))
        all-its (mapcat #(turn-iters db %) turns)
        latest  (p/db-load-latest-ctx db sid)
        sysp    (some-> all-its first :llm-system-prompt)]
    {:session        {:id sid :title (:title sess) :channel (:channel sess)}
     :turns          (count turns)
     :iters          (count all-its)
     ;; model-authored engine verbs — the System-2 usage signal
     :task-set-calls (count-verb #"task-set!" all-its)
     :fact-set-calls (count-verb #"fact-set!" all-its)
     :ctx-add-calls  (count-verb #"ctx-add!" all-its)
     :recall-calls   (count-verb #"\(recall" all-its)
     ;; batching signal
     :forms-per-iter (frequencies (map #(count (:forms %)) all-its))
     :multi-form-iters (count (filter #(> (count (:forms %)) 1) all-its))
     ;; re-discovery waste — raw (every pre-edit locate on follow-ups) and
     ;; confirm-read-forgiving (excess only). edit-failures = safety signal.
     :locate-waste-total (reduce + (map :locate-waste per))
     :reread-waste-total (reduce + (map :reread-waste per))
     :edit-failures-total (count-failures all-its)
     ;; final engine state — did anything durable accumulate?
     :final-tasks    (count (:session/tasks latest))
     :final-facts    (count (:session/facts latest))
     :final-facts-model-authored
     (count (remove (fn [[_ v]] (#{:done-auto :done-summarize} (:source v)))
              (:session/facts latest)))
     ;; prompt size (W4)
     :system-prompt-chars (count (str sysp))
     ;; cost
     :in-tokens-total  (reduce + 0 (keep :input-tokens turns))
     :out-tokens-total (reduce + 0 (keep :output-tokens turns))
     :per-turn per}))

(defn report
  "Print a readable scorecard for a session id."
  [db sid]
  (let [s (session-signals db sid)]
    (println "════════════════════════════════════════════════════════════")
    (println "SESSION" (str (get-in s [:session :title])) "—" (str sid))
    (println (format "turns=%d iters=%d  prompt=%d chars  tokens in=%d out=%d"
               (:turns s) (:iters s) (:system-prompt-chars s)
               (:in-tokens-total s) (:out-tokens-total s)))
    (println "────────────────────────────────────────────────────────────")
    (println "MODEL ENGINE-VERB USAGE (System-2):")
    (println (format "  task-set!=%d  fact-set!=%d  ctx-add!=%d  recall=%d"
               (:task-set-calls s) (:fact-set-calls s)
               (:ctx-add-calls s) (:recall-calls s)))
    (println (format "  forms-per-iter=%s  multi-form-iters=%d"
               (:forms-per-iter s) (:multi-form-iters s)))
    (println (format "  locate-before-edit waste=%d (reread-excess=%d)  edit-failures=%d"
               (:locate-waste-total s) (:reread-waste-total s) (:edit-failures-total s)))
    (println (format "  final engine state: tasks=%d facts=%d (model-authored facts=%d)"
               (:final-tasks s) (:final-facts s) (:final-facts-model-authored s)))
    (println "────────────────────────────────────────────────────────────")
    (println "PER-TURN:")
    (pp/print-table [:turn :iters :task-set :fact-set :recall :multi-form
                     :locate-waste :reread-waste :edit-fails :in-tokens :out-tokens :request]
      (:per-turn s))
    s))
