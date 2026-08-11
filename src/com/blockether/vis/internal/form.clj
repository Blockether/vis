(ns com.blockether.vis.internal.form
  "The canonical per-form DISPLAY contract — ONE source of truth for the fields a
   channel reads to render an executed form, live (via the gateway) and restored
   (via the DB).

   Why this exists: the SAME field set used to be hand-listed in independent
   allowlists across the loop, persistence, gateway, progress, and restored display
   paths. Now every layer projects the WHOLE set through `->display` (outbound) /
   `<-wire` (inbound), so a new display field is a ONE-line change to
   `display-keys` and `form-roundtrip-test` fails if a boundary stops carrying it.

   Transformed fields (`:stdout`/`:error` bounded, `:silent`/`:duration_ms`
   renamed) stay as explicit gateway overrides — they are not carried verbatim, so
   they are NOT in this set."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.pyfmt :as pyfmt]))

(def ^:private display-fields
  "Every field carried VERBATIM from the loop to a channel to render a form,
   paired with its literal gateway wire key. This is the complete passthrough set,
   NOT the handful the gateway computes/renames itself (`:stdout`/`:error` are
   bounded, `:silent`/`:duration_ms` are derived; those stay explicit gateway
   overrides). Add a new verbatim display field HERE; `->display`/`<-wire` then
   flow it across every boundary without runtime key rewriting.

   Grouped: the source the model wrote, the result surfaces, the printed-result
   op, the per-form display projections, the tool-call linkage, and the
   repair/timeout flags channels surface."
  [;; source
   [:code "code"] [:display-code "display_code"] [:display-language "display_language"]
   [:comment "comment"] [:scope "scope"] [:started-at-ms "started_at_ms"]
   ;; result surfaces — the raw value, the pre-rendered op-card body, and the
   ;; op-card HEADLINE (a tool-authored summary, never a first-line body slice)
   [:result "result"] [:result-render "result_render"] [:result-summary "result_summary"]
   ;; MULTI-card: canonical MINI-FORMS, recursively normalized by `<-wire`.
   [:cards "cards"]
   ;; A printed result's OWN op ("grep", "attach") — the only identity a card has.
   ;; It is DATA the value carried out of the sandbox, never a symbol looked up in a
   ;; registry, so a card cannot drift from what actually ran. Absent on the block
   ;; itself: a form is always the model's python, and its card reads `RESULT`.
   [:op "op"]
   ;; display projections
   [:render-segments "render_segments"] [:result-kind "result_kind"]
   [:result-detail "result_detail"] [:tag "tag"]
   ;; tool-call linkage + status flags channels surface
   [:svar/tool-call-id "tool_call_id"] [:timeout? "is_timeout"] [:repaired? "is_repaired"]])

(def display-keys
  "The canonical engine keys projected by `->display` and recovered by `<-wire`."
  (mapv first display-fields))

(def block-label
  "The badge a python block's own output wears. One tool reaches the wire, so the
   block never needs naming — what the card announces is that what follows is the
   RESULT of the program printed above it."
  "RESULT")

(defn- card-label
  "The op-card badge LABEL: a printed result is titled by its OWN `:op`
   (`grep` → `RG`-style uppercase), and the block's own output by `block-label`.
   Derived from the value, so a card for an op with no extension registered still
   paints instead of falling back to raw EDN."
  [op]
  (or (some-> op
              name
              str/trim
              not-empty
              str/upper-case)
      block-label))

(defn result-card
  "Canonical result CARD descriptor — the ONE place the card / collapse decision is
   made, so the TUI and web AGREE on label/summary/collapsible instead of each
   re-deriving it from the raw form. Given an executed form map (or one of the
   canonical MINI-FORMS a python block carries in `:cards`, one per printed
   result), returns:

     {:label        RESULT              — the badge (`card-label`): the printed
                                          value's own `:op`, or `RESULT` for the
                                          block's own output
      :summary      12 results          — the HEADLINE (`:result-summary`), nil
                                          when the value carried no tally
      :body         …markdown…          — the detail body (`:result-render`), nil
                                          when there is nothing under the headline
      :collapsible? true}               — true ⇔ there's a body to fold under
                                          the summary (a chevron/`<details>`)

   `nil` when the form has neither headline nor body — there is no card, and the
   channel renders whatever the form itself carries."
  [{:keys [op result-summary result-render]}]
  (let
    [summary
     (some-> result-summary
             str
             str/trim
             not-empty)

     body
     (some-> result-render
             str
             str/trimr
             not-empty)]

    (when (or summary body)
      {:label (card-label op) :summary summary :body body :collapsible? (boolean body)})))

(defn result-cards
  "The card descriptor(s) a form renders — the ONE place a channel asks \"what
   cards does this form show?\" so the TUI and web never re-derive it differently.

   A python block that print()ed several results carries a `:cards` vector of
   canonical mini-forms; each becomes its OWN card via `result-card`. Any other
   form yields its single `result-card` (or none). Always a vector — channels just
   iterate. Empty when the form printed nothing and returned nothing."
  [form]
  (if-let [cards (seq (:cards form))]
    (into [] (keep result-card) cards)
    (if-let [c (result-card form)]
      [c]
      [])))

(defn with-display-code
  "Attach the canonical cached ruff rendering of a form's Python source.
   Channels render `:display-code` verbatim; local callers without it may use
   the same formatter. Nested result cards are normalized recursively.

   An AUTHORED `:display-code` is never overwritten: a form that already carries
   the source a channel must paint — paired with its `:display-language` — keeps
   it verbatim."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (pyfmt/beautify-python (:code form)))

    (seq (:cards form))
    (update :cards #(mapv with-display-code %))))

(defn ->display
  "Project the canonical display fields off a source map (loop chunk/block, a
   restored row) — the ONE projection every form builder + the gateway uses
   instead of hand-listing keys. Drops nils so a merge never stamps empty keys."
  [m]
  (reduce (fn [acc k]
            (if (some? (get m k)) (assoc acc k (get m k)) acc))
          {}
          display-keys))



(defn <-wire
  "Read the canonical display fields back off a gateway WIRE event into a form,
   using the literal wire spelling declared beside each engine key in
   `display-fields`. The single inbound projection channels use — the mirror of
   `->display`."
  [event]
  (reduce (fn [acc [k wire-k]]
            (let [v (get event wire-k)]
              (cond (nil? v) acc
                    ;; `:cards` is a vector of canonical MINI-FORMS.
                    (= k :cards) (assoc acc k (mapv <-wire v))
                    :else (assoc acc k v))))
          {}
          display-fields))
