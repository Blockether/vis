(ns com.blockether.vis.human-input
  "Form builders for the typed pause an extension uses to ask the operator —
   `com.blockether.vis.core/request-human-input!`.

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
