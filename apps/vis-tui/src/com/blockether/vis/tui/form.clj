(ns com.blockether.vis.tui.form
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
            [com.blockether.vis.tui.util :as util]))

(def ^:private display-fields
  "Every field carried VERBATIM from the loop to a channel to render a form,
   paired with its literal gateway wire key. This is the complete passthrough set,
   NOT the handful the gateway computes/renames itself (`:stdout`/`:error` are
   bounded, `:silent`/`:duration_ms` are derived; those stay explicit gateway
   overrides). Add a new verbatim display field HERE; `->display`/`<-wire` then
   flow it across every boundary without runtime key rewriting.

   Grouped: the source the model wrote, the card op, the per-form display projections,
   the tool-call linkage, and the repair/timeout flags channels surface."
  [;; source
   [:code "code"] [:display-code "display_code"] [:display-language "display_language"]
   [:comment "comment"] [:scope "scope"] [:started-at-ms "started_at_ms"]
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

(defn stdout-display
  "The human-channel DISPLAY for one executed form as `{:body}`. `:stdout` is
   the only successful output a Python form or `!cmd` form can publish; an
   unprinted Python value never reaches the form, database, or wire.

   A wall-clock timeout gets no card of its own: it is an error like any other,
   while output printed before the timeout remains the ordinary stdout body.
   The body is head-clipped to `MAX_FORM_WIRE_CHARS`. Returns nil when there is
   nothing to show.

   This is a pure local projection of the form's `:stdout`, so live wire and a
   database-restored envelope paint the same card without storing or transporting
   a rendered string."
  [form]
  (let [stdout (str (:stdout form))]
    (cond
      ;; These bounded artifact fences ride stdout as Markdown so channels can
      ;; paint them inline; wrapping one in another fence would escape it.
      (or (str/includes? stdout "````vis-image")
          (str/includes? stdout "````vis-doc")
          (str/includes? stdout "````vis-table"))
      {:body stdout}
      ;; Plain output is not Markdown: fence it so CommonMark preserves newlines.
      (not (str/blank? stdout)) {:body (util/fenced (clip-to-wire stdout))}
      :else nil)))

(defn result-card
  "Canonical result CARD descriptor derived only from the form's `:stdout`:

     {:op           `grep`       — optional form metadata
      :body         …markdown…    — local projection of printed output
      :collapsible? true}

   nil means the form printed nothing. A label or operation can never manufacture
   successful output."
  [{:keys [op] :as form}]
  (let [body (try (some-> (stdout-display form)
                          :body
                          str
                          str/trimr
                          not-empty)
                  (catch Throwable _ nil))]
    (when body {:op op :body body :collapsible? true})))

(defn with-display
  "Attach the cached ruff rendering of a form's Python source when the form did not
   author its own `:display-code`. Result presentation is always derived locally
   from canonical facts and is never attached to the form."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (:code form))))

(defn ->display
  "Project canonical display fields from a source map, dropping nils."
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
