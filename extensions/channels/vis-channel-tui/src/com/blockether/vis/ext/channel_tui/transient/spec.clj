(ns com.blockether.vis.ext.channel-tui.transient.spec
  "The executable contract of a magit TRANSIENT: `clojure.spec` over the three
   values the component takes, plus the ONE table its vocabulary lives in.

     `spec`    WHAT the popup is — closed DATA a pure producer writes
     `region`  WHERE it sits — a rectangle a surface builds
     `host`    HOW it reaches a terminal — the impure adapter

   Two rules keep this honest.

   **The vocabulary lives here.** [[item-types]] says what each kind of row
   MEANS — `:is-flag`, `:is-valued`, `:is-command` — and `transient.clj` derives
   magit's leading `-`, the hint bar, the keystroke reducer and the paint from
   that map. There is no second copy of the table and no `(= :action type)`
   scattered over the painter, so a new kind of row is added in one place.

   **This namespace only EXPLAINS.** Every `*-error` returns nil or ONE line of
   prose; `transient/run!` owns the throw, so the
   `:vis/transient-invalid-spec` / `-invalid-region` / `-invalid-host` /
   `-invalid-option` envelopes stay in one place, and `transient/check` is the
   same judge answering instead of throwing — which is how a PRODUCER pins its
   own band in a unit test without a terminal.

   The DATA is CLOSED: an item, a group and a spec carry exactly the keys
   declared here, because a mistyped key is otherwise a row that silently does
   nothing — a `:labl` that paints blank, an `:arg` on a command that never
   reaches git. The region and the host are ADAPTERS a surface assembles: they
   are checked for everything the component reads and may carry the host's own
   extras (`:restore!` snapshots, a paging host's bookkeeping).

   Checked ONCE, at the seam — `run!` entry, and every value an OPTION brings
   in — never per keystroke and never per painted frame."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; The vocabulary — one table, every surface reads it
;; ---------------------------------------------------------------------------

(def item-types
  "Every kind of row a transient offers, and what that kind MEANS:

     `:is-flag`     wears magit's leading `-`, toggles IN PLACE and keeps the
                    popup open
     `:is-valued`   carries a value the human is asked for, stored under
                    `[:options id]` (the spec's `:read-option` fetches it)
     `:is-command`  fires ONCE and ends the run, reported as `:action`

   A `:switch` is a flag that is armed and disarmed by its own key; an
   `:option` is a flag that also holds a value; an `:action` is a verb. The
   traits — not the type keyword — are what the component branches on."
  {:switch {:is-flag true :is-valued false :is-command false}
   :option {:is-flag true :is-valued true :is-command false}
   :action {:is-flag false :is-valued false :is-command true}})

(defn- types-with
  "The item types whose [[item-types]] entry carries `trait`."
  [trait]
  (into #{}
        (keep (fn [[type traits]]
                (when (get traits trait) type)))
        item-types))

(def flag-types
  "Types drawn with magit's leading `-` and toggled in place: `#{:switch :option}`."
  (types-with :is-flag))

(def valued-types
  "Types that carry a value read from the human: `#{:option}`."
  (types-with :is-valued))

(def command-types
  "Types that fire once and close the popup: `#{:action}`."
  (types-with :is-command))

;; ---------------------------------------------------------------------------
;; The keys — one table per shape
;; ---------------------------------------------------------------------------

(def value-keys
  "The keys only a VALUED item may carry: how its value is asked for and whether
   it may be echoed. On a switch or a command they describe a value that will
   never be read."
  #{:prompt :mask :secret?})

(def item-keys
  "Every key one item may carry. `:key` is the single keystroke that fires it,
   `:id` what the run reports, `:arg` the git argument a FLAG contributes."
  (into #{:key :type :id :label :arg} value-keys))

(def group-keys
  "Every key one group may carry: its heading and the items under it."
  #{:title :items})

(def spec-keys
  "Every key a transient spec may carry. `:read-option` is the only impure one —
   it fetches an OPTION's value and is the caller's, not the popup's."
  #{:title :groups :read-option})

(def region-keys
  "Every coordinate the component reads off a region. Documentation, not a
   closed set: a region is assembled by a surface and may carry its own extras."
  #{:left :inner-w :text-w :hint-row :min-row :cols :is-sideless :restore!})

(def host-keys
  "Everything the component asks of a terminal. Same story as [[region-keys]]:
   an adapter may carry more."
  #{:g :hint-bar! :refresh! :read-key!})

(def state-keys
  "The whole run state: which flags are armed, and what the options hold."
  #{:switches :options})

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(defn- closed?
  "True when `m` carries no key outside `allowed`. A producer's typo is a row
   that silently does nothing, so the DATA is closed in both directions."
  [allowed m]
  (and (map? m) (every? allowed (keys m))))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- some-items?
  "A group with no rows is a heading with nothing under it, and a spec with no
   groups is an empty popup: both are a producer bug, not an empty state."
  [xs]
  (boolean (seq xs)))

(defn- single-keystroke?
  "A binding is ONE character, because that is what a key loop can deliver. A
   two-character `:key` is a row nobody can ever press."
  [x]
  (and (string? x) (= 1 (count x))))

(defn- items-of [spec] (mapcat :items (:groups spec)))

(defn- flag-only-arg?
  "`:arg` is the git argument a FLAG contributes; a command contributes none, so
   an `:arg` on an `:action` is a producer that meant to write a switch."
  [{:keys [type arg]}]
  (or (nil? arg) (contains? flag-types type)))

(defn- valued-only-value-keys?
  "How a value is asked for, masked and hidden only means something for an item
   that HAS a value."
  [{:keys [type] :as item}]
  (or (contains? valued-types type) (not-any? value-keys (keys item))))

(defn- distinct-keys?
  "Two items sharing a keystroke make one of them unreachable: the first one
   found wins, whichever the human meant."
  [spec]
  (let [ks (map :key (items-of spec))]
    (or (empty? ks) (apply distinct? ks))))

(defn- distinct-ids?
  "An `:id` is what a finished run REPORTS and what a pre-pressed slash names
   (`transient/item-by-id`). Two rows under one id is an ambiguous answer."
  [spec]
  (let [ids (map :id (items-of spec))]
    (or (empty? ids) (apply distinct? ids))))

(defn- sideless-knows-width?
  "A SIDELESS band has no border columns and no paper of its own: it wipes the
   FULL terminal width, so it has to be told what that width is."
  [{:keys [is-sideless cols]}]
  (or (not is-sideless) (some? cols)))

;; ---------------------------------------------------------------------------
;; An item — one row, one keystroke
;; ---------------------------------------------------------------------------

(s/def ::key single-keystroke?)
(s/def ::type (set (keys item-types)))
(s/def ::id some?)
(s/def ::label string?)
(s/def ::arg non-blank-string?)
(s/def ::prompt non-blank-string?)
(s/def ::mask char?)
(s/def ::secret? boolean?)

(s/def ::item
  (s/and #(closed? item-keys %)
         (s/keys :req-un [::key ::type ::id ::label] :opt-un [::arg ::prompt ::mask ::secret?])
         flag-only-arg?
         valued-only-value-keys?))

;; ---------------------------------------------------------------------------
;; A group, and the spec that arranges them
;; ---------------------------------------------------------------------------

(s/def ::title string?)
(s/def ::items (s/and (s/coll-of ::item :kind sequential?) some-items?))
(s/def ::group (s/and #(closed? group-keys %) (s/keys :req-un [::title ::items])))
(s/def ::groups (s/and (s/coll-of ::group :kind sequential?) some-items?))
(s/def ::read-option ifn?)

(s/def ::spec
  (s/and #(closed? spec-keys %)
         (s/keys :req-un [::groups] :opt-un [::title ::read-option])
         distinct-keys?
         distinct-ids?))

;; ---------------------------------------------------------------------------
;; The region a band paints into
;; ---------------------------------------------------------------------------

(s/def ::left nat-int?)
(s/def ::inner-w pos-int?)
(s/def ::text-w pos-int?)
(s/def ::hint-row nat-int?)
(s/def ::min-row nat-int?)
(s/def ::cols pos-int?)
(s/def ::is-sideless boolean?)
(s/def ::restore! ifn?)

(s/def ::region
  (s/and (s/keys :req-un [::left ::inner-w ::text-w ::hint-row]
                 :opt-un [::min-row ::cols ::is-sideless ::restore!])
         sideless-knows-width?))

;; ---------------------------------------------------------------------------
;; The host that owns the terminal
;; ---------------------------------------------------------------------------

(s/def ::g some?)
;; Callbacks, and callbacks only: `ifn?` would accept a keyword or a map, and a
;; keyword answering `(read-key!)` is a loop that spins on nil forever.
(s/def ::hint-bar! fn?)
(s/def ::refresh! fn?)
(s/def ::read-key! fn?)

(s/def ::host (s/keys :req-un [::g ::hint-bar! ::refresh! ::read-key!]))

;; ---------------------------------------------------------------------------
;; The run state, and the values an OPTION brings into it
;; ---------------------------------------------------------------------------

(s/def ::option-value non-blank-string?)
(s/def ::switches (s/coll-of some? :kind set?))
(s/def ::options (s/map-of some? ::option-value))
(s/def ::state (s/and #(closed? state-keys %) (s/keys :req-un [::switches ::options])))

;; ---------------------------------------------------------------------------
;; Explaining a violation
;; ---------------------------------------------------------------------------

(defn- brief
  [x]
  (let [text (pr-str x)]
    (if (> (count text) 88) (str (subs text 0 88) "…") text)))

(defn- problem-str
  [{:keys [in val pred]}]
  (str (when (seq in) (str (str/join " -> " (map brief in)) ": "))
       (brief val)
       " fails "
       (brief pred)))

(defn- error
  "nil when `x` satisfies `spec`, else a one-line reason naming the first couple
   of problems — short enough to ride in a message a human reads on a hint row."
  [spec x]
  (when-let [problems (::s/problems (s/explain-data spec x))]
    (str/join "; " (map problem-str (take 2 problems)))))

(defn item-error
  "nil when `item` is a legal transient row, else why it is not."
  [item]
  (error ::item item))

(defn group-error
  "nil when `group` is a legal group, else why it is not. Its items are checked
   as the items they are, so one call covers the whole group."
  [group]
  (error ::group group))

(defn spec-error
  "nil when `spec` is a legal transient — every group, every row, every binding
   reachable — else why it is not. This is what a producer's own test asks."
  [spec]
  (error ::spec spec))

(defn region-error
  "nil when `region` is a rectangle the component can paint into, else why not."
  [region]
  (error ::region region))

(defn host-error
  "nil when `host` can paint, flush and answer keystrokes, else why not."
  [host]
  (error ::host host))

(defn state-error
  "nil when `state` is a legal run state, else why it is not."
  [state]
  (error ::state state))

(defn option-value-error
  "nil when `value` is what an OPTION may carry into the run state. The one
   impure value in the whole component: `:read-option` is the caller's function,
   and whatever it hands back is painted and returned to the caller's caller."
  [value]
  (error ::option-value value))
