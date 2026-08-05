(ns com.blockether.vis.ext.channel-tui.drafts
  "Draft workspaces as a magit TRANSIENT — the PURE half.

   Managing drafts used to be a stack of modal windows: a full-screen picker,
   then a text-input dialog for the name, then a confirm dialog for the
   abandon. Three windows to answer one question, each of them hiding the very
   session the draft belongs to. A draft switch is exactly what a transient is
   for: a band inside the session's own frame, one keystroke per verb, flags
   that arm and disarm.

   This namespace owns WHAT the band offers and WHAT a keystroke means; it
   holds no screen, no Lanterna type and no gateway call, so every row, key and
   choice is testable without a terminal. `dialogs/draft-transient!` runs it.

   The transient reads:

     Drafts
     ──────
     Switch to
      t  ● Trunk
      a  ○ spike-parser
     Actions
      -c  Without my uncommitted changes  (--clean)
      n   New draft…
      k   Abandon draft…

   `t` and every draft letter are COMMANDS (fire once and close), `-c` is a
   magit FLAG that arms the gateway's clean fork for whatever `n` creates
   next."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private reserved-keys
  "Keys the band spends on its own verbs — a draft never gets one of these, so
   `t`runk, `c`lean, `n`ew and abandon (`k`, magit's own kill key) always mean
   the same thing no matter how many drafts exist."
  #{"t" "c" "n" "k"})

(def ^:private draft-key-pool
  "Letters handed to draft rows, in order. Lowercase only: transient keys are
   case-sensitive, and a shifted key on a row would be indistinguishable from
   the unshifted one at a glance."
  (vec (remove reserved-keys (map str "abcdefghijklmnopqrstuvwxyz"))))

(defn- mark
  "The exclusive ●/○ status glyph in front of a switch target. Same pair the
   settings rows, the footer and `dialogs/choice-mark` speak — a draft band is a
   \"choose one\" list, so it can never wear the inclusive `[✓]` box."
  [current?]
  (str (if current? p/STATUS_ON p/STATUS_OFF) " "))

(defn rows
  "PURE: the gateway's draft records as band rows, current draft(s) first:
   `{:key :workspace-id :label :root :is-current}`.

   Rows past the key pool are dropped rather than rendered unreachable — a
   keyless row in a keyboard-driven band is a lie."
  [drafts]
  (let
    [drafts
     (vec drafts)

     ordered
     (concat (filterv #(true? (get % "is_current")) drafts)
             (filterv #(not (true? (get % "is_current"))) drafts))]

    (mapv (fn [k draft]
            {:key k
             :workspace-id (get draft "workspace_id")
             :label (or (not-empty (str (get draft "label"))) "Untitled draft")
             :root (get draft "root")
             :is-current (true? (get draft "is_current"))})
          draft-key-pool
          ordered)))

(defn row-by-id
  "PURE: the row carrying `workspace-id`, or nil."
  [rows workspace-id]
  (some #(when (= workspace-id (:workspace-id %)) %) rows))

(defn in-draft?
  "PURE: is the session currently inside a draft (rather than on trunk)?"
  [rows]
  (boolean (some :is-current rows)))

(defn spec
  "PURE: the `transient/run!` spec for `drafts` (raw gateway rows).

   Every switch TARGET is a command, because picking one closes the band and
   performs the switch; `-c` is the only flag, and it belongs to `n` (the
   gateway's clean fork seeds from the last commit instead of the working
   tree). Abandon only exists when there is something to abandon."
  [drafts]
  (let
    [rs
     (rows drafts)

     switch-items
     (into
       [{:key "t" :type :action :id :trunk :label (str (mark (not (in-draft? rs))) "Trunk")}]
       (map
         (fn [{:keys [key label is-current workspace-id]}]
           {:key key :type :action :id [:draft workspace-id] :label (str (mark is-current) label)}))
       rs)]

    {:title "Drafts"
     :groups
     [{:title "Switch to" :items switch-items}
      {:title "Actions"
       :items
       (cond->
         [{:key "c" :type :switch :id :clean :label "Without my uncommitted changes" :arg "--clean"}
          {:key "n" :type :action :id :new :label "New draft…"}]
         (seq rs)
         (conj {:key "k" :type :action :id :abandon :label "Abandon draft…"}))}]}))

(defn choice
  "PURE: what one finished `transient/run!` result MEANS, in the shape the
   screen's draft executor already speaks — `{:action :trunk|:draft|:new|:abandon}`
   plus whatever that action needs. nil for Esc and for an action that no longer
   matches the rows (a draft abandoned by someone else mid-band).

   `:current?` is carried, not resolved: the executor answers \"Already on X\"
   without a second gateway round-trip."
  [drafts {:keys [action switches]}]
  (let
    [rs
     (rows drafts)

     clean?
     (boolean (contains? (set switches) :clean))]

    (cond (= :trunk action) {:action :trunk :label "Trunk" :current? (not (in-draft? rs))}
          (and (vector? action) (= :draft (first action)))
          (when-let [{:keys [workspace-id label is-current]} (row-by-id rs (second action))]
            {:action :draft :workspace-id workspace-id :label label :current? is-current})
          (= :new action) {:action :new :clean? clean?}
          (= :abandon action) {:action :abandon}
          :else nil)))

(defn abandon-choices
  "PURE: the inline single-key chooser rows for \"abandon WHICH draft\" —
   `{:key char :label str :id workspace-id}`, reusing each row's own band key so
   the letter that switches to a draft is the letter that abandons it."
  [drafts]
  (mapv (fn [{:keys [key label workspace-id]}]
          {:key (first key) :label label :id workspace-id})
        (rows drafts)))

(def start-in-spec
  "PURE: the transient asking WHERE a new session starts — the TUI twin of the
   companion's \"Start the session in\" menu, and the same vocabulary as the draft
   band: `-c` is the clean-fork flag, `t`/`d` are the two commands."
  {:title "New session in"
   :groups
   [{:title "Arguments"
     :items
     [{:key "c" :type :switch :id :clean :label "Without my uncommitted changes" :arg "--clean"}]}
    {:title "Start in"
     :items
     [{:key "t" :type :action :id :trunk :label "The project itself"}
      {:key "d" :type :action :id :draft :label "A new draft — a private copy of this project"}]}]})

(defn start-in-choice
  "PURE: `{:start-in :trunk}` or `{:start-in :draft :clean? bool}` from a
   `start-in-spec` run, nil on Esc."
  [{:keys [action switches]}]
  (case action
    :trunk
    {:start-in :trunk}

    :draft
    {:start-in :draft :clean? (boolean (contains? (set switches) :clean))}

    nil))

(defn draft-spec
  "PURE: the draft a `start-in-choice` + typed `label` asks for, or nil when the
   session simply starts in the real project (or the name was left empty).
   `:clean?` is the gateway's seed-from-the-COMMITTED-HEAD flag."
  [choice label]
  (when (= :draft (:start-in choice))
    (when-let
      [label (some-> label
                     str
                     str/trim
                     not-empty)]
      {:label label :clean? (boolean (:clean? choice))})))
