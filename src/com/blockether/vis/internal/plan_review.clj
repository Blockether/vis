(ns com.blockether.vis.internal.plan-review
  "Channel-neutral plan-review surface (Antigravity-style).

   When the model proposes a plan as `:candidate` steps and stops for
   approval (needs-input), every channel can offer a STRUCTURED review:
   per-step approve / reject / comment annotations plus an overall note.
   This namespace is the one definition both the TUI dialog and the web
   card (and the gateway's API `review` decision) compile through.

   DESIGN INVARIANT — approval IS the next user message (the same
   stop-and-wait contract `gateway.state/approve-turn!` encodes): a
   review never mutates `:session/tasks` host-side. It renders ONE
   deterministic `Plan review:` chat message; the MODEL re-emits the
   plan flipping approved steps, marking rejected ones, and re-proposing
   commented ones as fresh candidates. Single writer, no status desync,
   and the prompt's done-gates keep holding.

   Anchoring: verdicts key on the snake_case plan-step key (the SAME key
   the model sees in `context[\"tasks\"]` and types into `plan_step`).
   Step identity carries across `update_plan` re-emits by title slug, so
   an annotation written in round N still names the right step when the
   revised plan comes back in round N+1."
  (:require [clojure.string :as str]))

(defn- plan-entries
  "`[[k entry] …]` of the `:plan? true` tasks in `tasks`, in `:order`."
  [tasks]
  (->> tasks
    (filter (fn [[_ t]] (:plan? t)))
    (sort-by (fn [[_ t]] (or (:order t) Long/MAX_VALUE)))
    vec))

(defn reviewable-plan
  "The ordered review surface for a `:session/tasks` map: a vec of
   `{:key <snake_case string> :title <str> :status <kw>
     :acceptance <str|nil> :candidate? <bool>}` — one row per `:plan?`
   step, sorted by `:order`. Non-candidate steps ride along read-only
   (`:candidate? false`) so the UI can show the accepted context around
   the proposals without offering verdicts on it. Empty vec when no plan."
  [tasks]
  (mapv (fn [[k t]]
          {:key (name k)
           :title (str (:title t))
           :status (:status t)
           :acceptance (some-> (:acceptance t) str)
           :candidate? (= :candidate (:status t))})
    (plan-entries tasks)))

(defn review-pending?
  "True when the session has at least one `:candidate` plan step — the
   proposal-stop state in which a structured review applies. Channels
   gate the review affordance (TUI F7 hint, web card) on this."
  [tasks]
  (boolean (some (fn [[_ t]] (and (:plan? t) (= :candidate (:status t))))
             tasks)))

(def ^:private verdict-word
  "Canonical verdict words of the `Plan review:` line grammar — the SAME
   spellings the prompt teaches the model to parse (prompt.clj Planning)."
  {:approve "APPROVE" :reject "REJECT" :comment "COMMENT"})

(defn- one-line
  "Collapse a (possibly multiline textarea) note onto one line — the
   grammar is line-oriented, one step per `- key: VERDICT` line.
   `(?U)` makes `\\s` Unicode-aware: the default Java class misses
   U+2028/U+2029/U+0085, and a note carrying one of those could forge
   an extra grammar line for any consumer that treats them as newlines."
  [s]
  (let [t (str/trim (str/replace (str s) #"(?U)\s+" " "))]
    (when-not (str/blank? t) t)))

(defn- verdict-line
  "One `- <key>: <VERDICT>[ — <note>]` line, or nil when the entry says
   nothing: blank key, unknown verdict, or a COMMENT with no text (an
   empty comment is not feedback)."
  [{:keys [key verdict note]}]
  (let [k (one-line key)
        ;; Only keyword/string verdicts resolve; anything else (a number
        ;; or map smuggled through the gateway's JSON `:steps`) drops the
        ;; entry instead of throwing on `name`.
        word (when (or (keyword? verdict) (string? verdict))
               (get verdict-word (keyword (str/lower-case (name verdict)))))
        note (one-line note)]
    (when (and k word (or note (not= word "COMMENT")))
      (str "- " k ": " word (when note (str " — " note))))))

(defn review->message
  "Compile review annotations into the ONE canonical chat message the
   model is prompt-trained to parse:

     Plan review:
     - <step_key>: APPROVE
     - <step_key>: REJECT — <note>
     - <step_key>: COMMENT — <note>
     Overall: <overall>

   `verdicts` — seq of `{:key <str> :verdict :approve|:reject|:comment
   :note <str|nil>}` (string verdict names accepted too); `overall` — a
   free-text note for the whole plan, optional. Entries that say nothing
   (blank key, no verdict, empty comment) are dropped. Returns nil when
   the WHOLE review says nothing — callers send no message in that case."
  [verdicts overall]
  (let [lines (into [] (keep verdict-line) verdicts)
        overall (one-line overall)]
    (when (or (seq lines) overall)
      (str "Plan review:"
        (when (seq lines) (str "\n" (str/join "\n" lines)))
        (when overall (str "\nOverall: " overall))))))
