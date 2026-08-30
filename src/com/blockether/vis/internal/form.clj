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
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.pyfmt :as pyfmt]
            [com.blockether.vis.internal.util :as util]))

(def ^:private display-fields
  "Every field carried VERBATIM from the loop to a channel to render a form,
   paired with its literal gateway wire key. This is the complete passthrough set,
   NOT the handful the gateway computes/renames itself (`:stdout`/`:error` are
   bounded, `:silent`/`:duration_ms` are derived; those stay explicit gateway
   overrides). Add a new verbatim display field HERE; `->display`/`<-wire` then
   flow it across every boundary without runtime key rewriting.

   Grouped: the source the model wrote, the result surfaces, the card op, the
   per-form display projections, the tool-call linkage, and the
   repair/timeout flags channels surface."
  [;; source
   [:code "code"] [:display-code "display_code"] [:display-language "display_language"]
   [:comment "comment"] [:scope "scope"] [:started-at-ms "started_at_ms"]
   ;; result surfaces — the raw value, the op-card BODY (a pure projection:
   ;; `result-render` DERIVES it, and it is stored only for the rare card no
   ;; projection reproduces) and the op-card HEADLINE (a tool-authored summary,
   ;; never a first-line body slice)
   [:result "result"] [:result-render "result_render"] [:result-summary "result_summary"]
   ;; A form's OWN op ("grep", "attach") — the only identity a card has. It is DATA
   ;; the executed form carried, never a symbol looked up in a registry, so a card
   ;; cannot drift from what actually ran. Absent on a python block: a form is
   ;; always the model's python, so its card carries no op.
   [:op "op"]
   ;; display projections
   [:render-segments "render_segments"] [:result-kind "result_kind"]
   [:result-detail "result_detail"] [:tag "tag"]
   ;; tool-call linkage + status flags channels surface
   [:svar/tool-call-id "tool_call_id"] [:timeout? "is_timeout"] [:repaired? "is_repaired"]])

(def display-keys
  "The canonical engine keys projected by `->display` and recovered by `<-wire`."
  (mapv first display-fields))

(defn envelope-duration-ms
  "Wall-clock ms an executed form took, derived from the timing keys its envelope
   already carries — nil when the envelope carries no complete pair, so a caller
   can tell \"took no measurable time\" from \"was never timed\".

   The ONE derivation of a form's duration: the loop, the CLI trace, the progress
   projection, the ctx envelope and a DB-restored transcript each used to carry a
   private copy of this arithmetic."
  [envelope]
  (when (and (map? envelope)
             (nat-int? (:started-at-ms envelope))
             (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))
(defn result-card
  "Canonical result CARD descriptor — the ONE place the card / collapse decision is
   made, so the TUI and web AGREE on summary/collapsible instead of each
   re-deriving it from the raw form. Given an executed form map, returns:

     {:op           `grep`             — the form's OWN op, verbatim data it
                                          carried, nil for a python block's own
                                          output. A channel titles the card from
                                          this or from nothing; no display NAME is
                                          minted here
      :summary      12 results          — the HEADLINE (`:result-summary`), nil
                                          when the value carried no tally
      :body         …markdown…          — the detail body (`:result-render`), nil
                                          when there is nothing under the headline
      :collapsible? true}               — true ⇔ there's a body to fold under
                                          the summary (a chevron/`<details>`)

   `nil` when the form has neither headline nor body — there is no card, and the
   channel renders whatever the form itself carries."
  [{:keys [op result-summary result-render]}]
  (let [summary
        (some-> result-summary
                str
                str/trim
                not-empty)

        body
        (some-> result-render
                str
                str/trimr
                not-empty)]

    (when (or summary body) {:op op :summary summary :body body :collapsible? (boolean body)})))

(def MAX_FORM_WIRE_CHARS
  "Per-block printed-output ceiling. A block's stdout is head-clipped to this
   many chars in the tool result — a universal backstop for a runaway print()
   that tool-level caps don't catch (the model can `print(open-ended
   composition)`). The block's values still live in the sandbox (persistent REPL
   vars the model can re-slice and print less of). ~64KB ≈ 16k tokens: generous
   for an intentional full-file read, tight enough that one runaway print can't
   blow the request."
  65536)

(defn clip-to-wire
  "Head-clip one form BODY to `MAX_FORM_WIRE_CHARS`, announcing what it dropped —
   the ONE clip shared by every surface that body reaches: the model's tool
   result, the card a channel paints, and the gateway's `stdout` copy of the same
   text. Each used to hand-roll its own cut at its own ceiling, so one printed
   output rode a single event twice at sizes an order of magnitude apart.

   `hint` is the calling surface's own advice, appended to the marker — the model
   is told to narrow its next read, a human card just says what was dropped. The
   cut is `util/truncate`, so it never splits a surrogate pair. nil for a string
   that is blank once trailing space is gone."
  ([s] (clip-to-wire s nil))
  ([s hint]
   (let [s
         (str/trimr (str s))

         n
         (long (count s))]

     (when (pos? n)
       (if (> n (long MAX_FORM_WIRE_CHARS))
         (let [head (util/truncate s MAX_FORM_WIRE_CHARS)]
           (str head
                "\n# ⋯ output clipped at " (count head)
                "/" n
                " chars" (when hint (str " — " hint))))
         s)))))

(defn result-display
  "The human-channel DISPLAY for one executed form as `{:summary :body}` — the
   ONE surface both the TUI and the web render, so they're unified:
     - `:stdout` → verbatim as the body, no summary. This is what a PYTHON form
       carries: print is its one channel, and a value it never printed does not
       reach the form at all;
     - a `:result` value → pretty-printed (Python-literal, fenced) as the body,
       no summary. Only a HOST-AUTHORED form has one (a `!cmd` shell card, a
       slash envelope, a native tool entry), because it has no stdout of its own.
   A wall-clock TIMEOUT gets NO card of its own: it is an error like any other, so
   the channel paints `:error` — `Timeout (300s)` — where it paints every failure,
   and whatever the block printed before the wall stays its ordinary stdout body.
   A second ⧖ card only re-showed the form already painted above it and repeated
   that one message.
   The body is head-clipped to `MAX_FORM_WIRE_CHARS`. Returns nil when there's
   nothing to show. NO symbol is consulted: a card is built from the FORM's own
   data, so it can never drift from what actually ran.

   A PURE projection of fields the form ALREADY carries (`:error`,
   `:result`, `:stdout`) — that is what lets the live loop and a
   DB-restored envelope paint the same card without either of them storing the
   rendered string."
  [form]
  (let [clip clip-to-wire]
    (cond
      ;; a result value → monospaced Python-literal body, so a dict/list reads as
      ;; structured data rather than reflowed prose.
      (some? (:result form)) (when-let [s (clip (env/ctx->python-str (:result form)))]
                               {:body (util/fenced s "python")})
      ;; A `vis-image` fence (matplotlib `plt.show()` → inline PNG, ASCII plot
      ;; carried as its fallback body), a `vis-table` fence (a CSV/TSV artifact
      ;; carried as its own grid) or a `vis-doc` fence (a PDF/HTML document,
      ;; carrying only its host path) rides stdout as MARKDOWN so the channel
      ;; paints it inline; wrapping it in a ``` block would escape the
      ;; 4-backtick fence, so pass the stdout through verbatim (unclipped — the
      ;; fence is self-bounded and row-capped at the source) whenever one is
      ;; present.
      (or (str/includes? (str (:stdout form)) "````vis-image")
          (str/includes? (str (:stdout form)) "````vis-doc")
          (str/includes? (str (:stdout form)) "````vis-table"))
      {:body (str (:stdout form))}
      ;; python_execution printed output → fenced so newlines are preserved verbatim
      ;; (plain stdout is NOT markdown; bare \n collapses to a space through the
      ;; CommonMark SoftLineBreak → :space path if left unwrapped).
      (not (str/blank? (str (:stdout form)))) {:body (util/fenced (clip (:stdout form)))}
      :else nil)))

(defn result-render
  "The detail BODY one form displays — `:result-render` DERIVED rather than read,
   so the rendered string never has to be persisted alongside the data it is a
   projection of.

   TOTAL where `result-display` is strict: the live loop renders the result as it
   executes (and a value that cannot cross to Python still fails THERE, loudly),
   but a READER is deriving the body of a row that was written long ago. A
   forensic report or a reopened session must not die because one archived
   result no longer renders — no body beats a throw."
  [form]
  (try (:body (result-display form)) (catch Throwable _ nil)))

(defn with-display
  "Attach the display projections a channel paints but the store does NOT keep:
   the cached ruff rendering of the form's Python source, and the rendered
   `:result-render` detail body.

   An AUTHORED value is never overwritten: a form that already carries the
   source a channel must paint — paired with its `:display-language` — keeps it
   verbatim, and so does a `:result-render` no projection reproduces (a `!cmd`
   bubble, whose body is the shell layer's own card markdown)."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (pyfmt/beautify-python (:code form)))

    (nil? (:result-render form))
    (as-> f (let [body (result-render f)]
              (cond-> f
                body
                (assoc :result-render body))))))

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
              (if (nil? v) acc (assoc acc k v))))
          {}
          display-fields))
