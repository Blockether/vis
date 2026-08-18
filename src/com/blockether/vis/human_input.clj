(ns com.blockether.vis.human-input
  "Builders for the two things an extension shows the operator: the typed pause
   it WAITS on — `com.blockether.vis.core/request-human-input!` — and the live
   view it does not, `com.blockether.vis.core/with-live-view!`.

   A request is plain data, and it stays plain data: every builder here returns
   the very map you could have typed by hand. What it adds is that the two
   things a hand-typed map gets wrong cannot happen. The node TYPE is the
   function you called, so `:type \"plaintxt\"` is a compile-time unresolved
   symbol instead of a refused request at run time; and the node is VALIDATED
   the moment it is built, by the engine's own [[com.blockether.vis.internal.human-input/normalize-node]]
   seam, so a bad `:default`, an unknown key or a `:select` with no options
   throws at the line that built it rather than in front of the human.

       (require '[com.blockether.vis.core :as vis]
                '[com.blockether.vis.human-input :as hi])

       (vis/request-human-input!
         (hi/form {:title \"Deploy\" :description \"Where this build lands.\"}
                  (hi/heading \"Target\")
                  (hi/paragraph \"Staging pages nobody.\")
                  (hi/row (hi/select \"env\" [\"staging\" \"prod\"] {:label \"Environment\"
                                                                :is-required true})
                          (hi/slider \"canary\" {:label \"Canary %\" :min 0 :max 100 :step 5}))
                  (hi/password \"token\" {:label \"Deploy token\" :is-required true})))

   Three node contracts, exactly as the engine sees them: a FIELD holds one
   answer and is keyed by its name, a GROUP ([[row]] / [[column]]) only arranges
   the nodes below it, and a DECORATION ([[heading]] / [[paragraph]]) is ink —
   no name, never focusable, never in the answer map.

   Every optional key is the one the engine documents, in either spelling
   (`:is-required` or `\"is_required\"`): builders pass options through
   untouched instead of keeping a second copy of the vocabulary.

   A LIVE VIEW is the same discipline one `:kind` further: [[view]] and the node
   builders under it declare a picture the human WATCHES while the work runs. It
   asks nothing and parks no thread; it is patched by node id and ends in a
   verdict carrying the markdown the model reads.

       (vis/with-live-view!
         (hi/view {:title \"CI\"}
                  (hi/status \"now\" \"Polling GitHub…\" {:tone \"running\"})
                  (hi/table \"jobs\" [(hi/table-column \"job\" \"Job\")
                                    (hi/table-column \"took\" \"Took\" {:align \"right\"})]))
         (fn [view-id]
           (vis/patch-live-view!
             view-id
             [{:op \"set\" :node-id \"now\" :text \"18 jobs\" :tone \"ok\"}
              {:op \"append\" :node-id \"jobs\"
               :rows [(hi/table-row \"build\" [\"tests / ubuntu\" \"13m0s\"] {:tone \"ok\"})]}])))

   Python extensions get the SAME names on the `vis` module —
   `vis.select('env', ['staging', 'prod'], label='Environment')` — built by
   the same engine seam across the JSON boundary."
  (:require [com.blockether.vis.internal.human-input :as engine]))

(set! *warn-on-reflection* true)

;; The one validation seam

(defn- checked
  "`node` itself, once the engine has agreed it is a node.

   The normalized form is deliberately DROPPED: what an extension composes and
   what it can print is the spec map, while what the dialog runs on is built
   once, later, by `request!`. Normalizing here is only how a mistake is dated
   to the builder call that made it."
  [node]
  (engine/normalize-node node)
  node)

(defn- field
  "One answerable node of `type-name` named `field-name`, carrying `opts`."
  [type-name field-name opts]
  (checked (assoc opts
             :type type-name
             :name field-name)))

;; Fields — every node that holds exactly one answer

(defn plaintext
  "One typed line, answered as a string.

   `opts` may carry `:label`, `:description`, `:placeholder`, `:default`,
   `:is-required`, `:min-length`, `:max-length` and `:validate`."
  ([field-name] (plaintext field-name nil))
  ([field-name opts] (field "plaintext" field-name opts)))

(defn password
  "A typed line whose characters are masked, answered as an opaque
   `vis-secret:` HANDLE — never the plaintext. Read it with
   `com.blockether.vis.core/reveal-human-input-secret` on the trusted side.

   Takes the same `opts` as [[plaintext]]."
  ([field-name] (password field-name nil))
  ([field-name opts] (field "password" field-name opts)))

(defn multiline
  "A multi-line text box, answered as a string with its newlines and its
   leading whitespace intact. Takes the same `opts` as [[plaintext]]."
  ([field-name] (multiline field-name nil))
  ([field-name opts] (field "multiline" field-name opts)))

(defn select
  "Choose exactly ONE of `options`, answered as that option's value.

   `options` is a vector of plain values or [[option]] maps. A `:default` must
   be one of the values offered."
  ([field-name options] (select field-name options nil))
  ([field-name options opts] (field "select" field-name (assoc opts :options options))))

(defn multiselect
  "Choose ANY of `options`, answered as a vector of the chosen values (empty
   when nothing is ticked). Same `options` shape as [[select]]."
  ([field-name options] (multiselect field-name options nil))
  ([field-name options opts] (field "multiselect" field-name (assoc opts :options options))))

(defn checkbox
  "One box, answered as a boolean. `:is-required` means it must end up TICKED,
   which is how a consent line is expressed."
  ([field-name] (checkbox field-name nil))
  ([field-name opts] (field "checkbox" field-name opts)))

(defn slider
  "A number on a track, answered as a NUMBER: `:min` / `:max` / `:step`
   (0 / 100 / 1 when unsaid), `:default` inside its own track.

   The wire type is `range`; the builder is spelled `slider` so it never
   shadows `clojure.core/range` — and so the Python mirror never shadows the
   `range` builtin either."
  ([field-name] (slider field-name nil))
  ([field-name opts] (field "range" field-name opts)))

(defn otp
  "A one-time code in digit boxes, answered as an opaque `vis-secret:` handle —
   a code opens the account once, so it is a secret exactly like a password.
   `:min-length` / `:max-length` say how many digits (6 by default, 12 at most)."
  ([field-name] (otp field-name nil))
  ([field-name opts] (field "otp" field-name opts)))

(defn option
  "One entry for a [[select]] / [[multiselect]]: the `value` that is answered
   and, optionally, the `label` shown instead of it.

   An option is not a node, so it is checked by the field that offers it."
  ([value] {:value value})
  ([value label] {:value value :label label}))

;; Groups — upper control flow, no answer of their own

(defn row
  "Lay `nodes` out side by side. A group holds no value and never appears in
   the answer map; groups nest freely."
  [& nodes]
  (checked {:type "group" :direction "row" :fields (vec nodes)}))

(defn column
  "Stack `nodes` one under the next — the default arrangement, worth saying
   explicitly inside a [[row]]."
  [& nodes]
  (checked {:type "group" :direction "column" :fields (vec nodes)}))

;; Decoration — ink, so a long form reads like a page instead of a list

(defn heading
  "A section title: bold, unfocusable, answers nothing."
  [text]
  (checked {:type "heading" :text text}))

(defn paragraph
  "Prose under a title: dim italic, wrapped, unfocusable, answers nothing."
  [text]
  (checked {:type "paragraph" :text text}))

;; The request

(defn form
  "The request map `com.blockether.vis.core/request-human-input!` takes, built
   from `opts` and the `nodes` that follow it — and refused right here if it is
   not one.

   `opts` needs at least a `:title`, and may carry `:description`,
   `:submit-label`, `:cancel-label`, `:is-cancellable`, `:timeout-ms` (0 waits
   indefinitely) and `:channel-ids`. At least one node is required, and the
   answerable ones must have distinct names."
  [opts & nodes]
  (let [request (assoc opts :fields (vec nodes))]
    (engine/normalize-request request)
    request))

;; Live views — the picture the human WATCHES while the work runs
;;
;; Nodes are built and checked exactly like fields, through the engine's own
;; [[com.blockether.vis.internal.human-input/normalize-live-node]] seam. The four
;; STATE ops (`set`, `append`, `clear`, `remove`) deliberately get no builder:
;; each is a two-key map the engine already refuses by name against its closed
;; table, and an invented `set-node` here would mint a second vocabulary beside
;; the one the wire, the phone and the terminal all read. `add-node` and
;; `remove-node` do get one — they change the view's SHAPE and carry a whole node.

(defn- checked-node
  "`node` itself, once the engine has agreed it is a live node — the normalized
   form is DROPPED for the same reason [[checked]] drops it."
  [node]
  (engine/normalize-live-node node)
  node)

(defn- live-node
  "One live node of `type-name`, addressed by `id`, carrying `opts`."
  [type-name id opts]
  (checked-node (assoc opts
                  :id id
                  :type type-name)))

(defn status
  "One line saying what is happening RIGHT NOW — replaced in place, never
   appended, so the top of the view never scrolls. `opts` may carry `:label`,
   `:detail` and a `:tone`."
  ([id text] (status id text nil))
  ([id text opts] (live-node "status" id (assoc opts :text text))))

(defn progress
  "How far the work has come: `:done` of `:total`, or a `:value` between 0 and 1.
   Neither means INDETERMINATE, which is the honest picture while a job queues."
  ([id] (progress id nil))
  ([id opts] (live-node "progress" id opts)))

(defn stat
  "A strip of counters upserted by id — the score. Each entry of `stats` is
   `{:id … :label … :value-text … :tone …}`."
  ([id stats] (stat id stats nil))
  ([id stats opts] (live-node "stat" id (assoc opts :stats (vec stats)))))

(defn steps
  "An ORDERED checklist: the shape of a pipeline, in the order it runs. Each of
   `items` is `{:id … :label … :tone … :detail … :value …}` and carries its own
   tone, because a step is where a run goes wrong."
  ([id items] (steps id items nil))
  ([id items opts] (live-node "steps" id (assoc opts :steps (vec items)))))

(defn log
  "Append-only lines — the scrollback. `:lines` seeds it and `:window-lines` says
   how many a surface holds hot; the view's record on disk keeps every line
   either way, so a window is a paint budget, never a loss."
  ([id] (log id nil))
  ([id opts] (live-node "log" id opts)))

(defn table
  "Rows upserted and removed by row id, painted in the `:order` the view
   DECLARES — `columns` are [[table-column]]s, `:rows` seeds it and `:max-rows`
   bounds it by refusal."
  ([id columns] (table id columns nil))
  ([id columns opts] (live-node "table" id (assoc opts :columns (vec columns)))))

(defn link
  "Labeled pointers the human OPENS: each of `links` is
   `{:id … :label … :target … :target-kind …}` — an attachment, a path or a url."
  ([id links] (link id links nil))
  ([id links opts] (live-node "link" id (assoc opts :links (vec links)))))

(defn table-column
  "One column of a [[table]]: the `id` a cell is addressed by and the `label`
   over it, optionally `:align` `\"right\"` for numbers.

   A column is not a node, so it is checked by the table that declares it."
  ([id label] {:id id :label label})
  ([id label opts]
   (assoc opts
     :id id
     :label label)))

(defn table-row
  "One row of a [[table]], keyed by `id`: `cells` in the order the columns were
   declared, optionally `:tone`d. Built rather than typed because it is the one
   POSITIONAL thing here — a cell means whatever column stands over it.

   A row is not a node, so it is checked by the table, or the patch, carrying it."
  ([id cells] {:id id :cells (vec cells)})
  ([id cells opts]
   (assoc opts
     :id id
     :cells (vec cells))))

(defn- checked-op
  "`op` itself, once the engine has agreed it is a patch operation."
  [op]
  (engine/normalize-live-op op)
  op)

(defn add-node
  "Add a whole `node` to a RUNNING view — a second table, a per-device log.
   `after` names the node it lands behind; without it, it goes last."
  ([node] (checked-op {:op "add-node" :node-spec node}))
  ([node after] (checked-op {:op "add-node" :node-spec node :after after})))

(defn remove-node
  "Drop node `node-id` from a running view, its items with it."
  [node-id]
  (checked-op {:op "remove-node" :node-id node-id}))

(defn view
  "The live view `com.blockether.vis.core/open-live-view!` mounts, built from
   `opts` and the `nodes` that follow it — and refused right here if it is not
   one.

   `opts` needs at least a `:title`, and may carry `:description`, `:source`,
   `:channel-ids` and `:timeout-ms`. There is no cancellable flag: a human can
   stop watching ANY view. At least one node is required, and their ids must be
   distinct: every patch names the node it speaks to."
  [opts & nodes]
  (let [view (assoc opts :nodes (vec nodes))]
    (engine/normalize-live-view view)
    view))
