(ns com.blockether.vis.ext.channel-tui.drafts
  "Draft workspaces as a magit TRANSIENT — the PURE half.

   Managing drafts used to be a stack of modal windows: a full-screen picker,
   then a text-input dialog for the name, then a confirm dialog for the
   abandon. Three windows to answer one question, each of them hiding the very
   session the draft belongs to. A draft switch is exactly what a transient is
   for: a band inside the session's own frame, one keystroke per verb.

   This namespace owns WHAT the band offers and WHAT a keystroke means; it
   holds no screen, no Lanterna type and no gateway call, so every row, key and
   choice is testable without a terminal. `dialogs/draft-transient!` runs it.

   The transient reads:

     Drafts
     ──────
     Commands
      c  New draft from the committed HEAD
      d  New draft with my uncommitted changes
      s  Switch to another draft…
      k  Abandon draft…

   CREATING, SWITCHING and ABANDONING are three different questions, so they
   are three different keys and never one list. `c` and `d` say which working
   tree the new draft starts from — a command each, so nothing has to be armed
   as a flag and remembered — while `s` opens the SWITCH band (trunk and every
   draft, `●` on the one we are in) and `k` throws one away. `s` and `k` only
   exist while a draft exists: with none, there is nowhere to go and nothing to
   kill."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private reserved-keys
  "Keys the SWITCH band spends on its own verbs — a draft never gets one of
   these, so `t`runk always means trunk no matter how many drafts exist."
  #{"t"})

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
  (let [drafts
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
  "PURE: the `transient/run!` spec for the draft band — ONE group of commands.

   Creating is two commands instead of a command plus a flag: `c` seeds the
   draft from the committed HEAD and `d` carries the uncommitted working tree
   into it, so the band never asks anyone to arm `--clean` and remember it.
   Switching and abandoning are their own keys and only appear while there is
   a draft to switch to or throw away."
  [drafts]
  (let [rs (rows drafts)]
    {:title "Drafts"
     :groups
     [{:title "Commands"
       :items
       (cond-> [{:key "c" :type :action :id :new-clean :label "New draft from the committed HEAD"}
                {:key "d"
                 :type :action
                 :id :new-dirty
                 :label "New draft with my uncommitted changes"}]
         (seq rs)
         (conj {:key "s" :type :action :id :switch :label "Switch to another draft…"}
               {:key "k" :type :action :id :abandon :label "Abandon draft…"}))}]}))

(defn switch-spec
  "PURE: the SECOND band, the one `s` opens — every workspace this session can
   move to, trunk first, the one it is in already marked `●`.

   It is a band and not a one-row chooser because a draft list is a LIST: the
   labels have to stay readable, and the row you are on has to be visible."
  [drafts]
  (let [rs (rows drafts)]
    {:title "Drafts"
     :groups
     [{:title "Switch to"
       :items
       (into [{:key "t" :type :action :id :trunk :label (str (mark (not (in-draft? rs))) "Trunk")}]
             (map (fn [{:keys [key label is-current workspace-id]}]
                    {:key key
                     :type :action
                     :id [:draft workspace-id]
                     :label (str (mark is-current) label)}))
             rs)}]}))

(defn choice
  "PURE: what one finished `spec` run MEANS — `{:action :new :clean? bool}`,
   `{:action :switch}` (the caller opens `switch-spec` next) or
   `{:action :abandon}`. nil for Esc."
  [{:keys [action]}]
  (case action
    :new-clean
    {:action :new :clean? true}

    :new-dirty
    {:action :new :clean? false}

    :switch
    {:action :switch}

    :abandon
    {:action :abandon}

    nil))

(defn switch-choice
  "PURE: what one finished `switch-spec` run MEANS, in the shape the screen's
   draft executor already speaks — `{:action :trunk|:draft}` plus whatever that
   action needs. nil for Esc and for a draft that no longer matches the rows
   (one abandoned by someone else while the band was open).

   `:current?` is carried, not resolved: the executor answers \"Already on X\"
   without a second gateway round-trip."
  [drafts {:keys [action]}]
  (let [rs (rows drafts)]
    (cond (= :trunk action) {:action :trunk :label "Trunk" :current? (not (in-draft? rs))}
          (and (vector? action) (= :draft (first action)))
          (when-let [{:keys [workspace-id label is-current]} (row-by-id rs (second action))]
            {:action :draft :workspace-id workspace-id :label label :current? is-current})
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
   companion's \"Start the session in\" menu, and the same vocabulary as the
   draft band: `t` is the project itself, `c` and `d` are the two drafts you
   can fork."
  {:title "New session in"
   :groups
   [{:title "Commands"
     :items
     [{:key "t" :type :action :id :trunk :label "The project itself"}
      {:key "c" :type :action :id :new-clean :label "A new draft from the committed HEAD"}
      {:key "d" :type :action :id :new-dirty :label "A new draft with my uncommitted changes"}]}]})

(defn start-in-choice
  "PURE: `{:start-in :trunk}` or `{:start-in :draft :clean? bool}` from a
   `start-in-spec` run, nil on Esc."
  [{:keys [action]}]
  (case action
    :trunk
    {:start-in :trunk}

    :new-clean
    {:start-in :draft :clean? true}

    :new-dirty
    {:start-in :draft :clean? false}

    nil))

(defn draft-spec
  "PURE: the draft a `start-in-choice` + typed `label` asks for, or nil when the
   session simply starts in the real project (or the name was left empty).
   `:clean?` is the gateway's seed-from-the-COMMITTED-HEAD flag."
  [choice label]
  (when (= :draft (:start-in choice))
    (when-let [label (some-> label
                             str
                             str/trim
                             not-empty)]
      {:label label :clean? (boolean (:clean? choice))})))

;;; ── The `/draft …` slashes are the same band ────────────────────────────────

(def ^:private slash-bands
  "PURE: which band command each `/draft …` line already named. A slash that
   asks a question the band asks is answered BY the band, inside the session's
   own frame, instead of by a modal text prompt: `new` carries the working tree
   in (`d`), `clean` forks the committed HEAD (`c`), `resume` and `list` are the
   switch band, `abandon` is the abandon flow, and a bare `/draft` is the band
   itself.

   `/draft apply` and `/draft stash` are deliberately absent: they are verbs
   with nothing to ask, so they stay engine slashes and run as typed."
  {["draft"] nil
   ["draft" "new"] :new-dirty
   ["draft" "clean"] :new-clean
   ["draft" "resume"] :switch
   ["draft" "list"] :switch
   ["draft" "abandon"] :abandon})

(defn slash-band
  "PURE: `{:pressed id-or-nil}` when the typed slash `path` is a question this
   band answers — `:pressed` is the band command the slash already named (the
   key the human would otherwise have pressed), nil the band itself.

   nil when the line is not one of those: anything that is not `/draft`, and
   every `/draft <cmd> <arg>` — slash tokenising keeps the argument in the path,
   so a line that already carries its own answer runs as the engine slash it
   is."
  [path]
  (when-let [entry (find slash-bands (vec path))]
    {:pressed (val entry)}))
