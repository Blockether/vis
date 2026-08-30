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

   Grouped: the source the model wrote, the canonical result fact, the card op,
   the per-form display projections, the tool-call linkage, and the repair/timeout
   flags channels surface."
  [;; source
   [:code "code"] [:display-code "display_code"] [:display-language "display_language"]
   [:comment "comment"] [:scope "scope"] [:started-at-ms "started_at_ms"]
   ;; canonical result facts — a structured host/native value OR printed stdout,
   ;; never a rendered copy of either; the headline is concise tool-authored metadata
   [:result "result"] [:result-summary "result_summary"]
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

(defn result-display
  "The human-channel DISPLAY for one executed form as `{:body}`:
     - `:stdout` → verbatim as the body, no summary. This is what a PYTHON form and
       a `!cmd` form carry: output is one fact, and an unprinted Python value never
       reaches the form at all;
     - a structured `:result` → pretty-printed (Python-literal, fenced) as the body.
       Only a HOST-AUTHORED slash/native form has one, and only when it has no stdout.
   A wall-clock TIMEOUT gets NO card of its own: it is an error like any other, so
   the channel paints `:error` where it paints every failure, while output printed
   before the timeout remains the ordinary stdout body.

   The body is head-clipped to `MAX_FORM_WIRE_CHARS`. Returns nil when there is
   nothing to show. NO symbol is consulted: the form's own facts determine the view.

   This is a PURE local projection of fields the form ALREADY carries (`:result`,
   `:stdout`) — live wire and a DB-restored envelope therefore paint the same card
   without storing or transporting a rendered string."
  [form]
  (let [clip
        clip-to-wire

        stdout
        (str (:stdout form))]

    (cond
      ;; A `vis-image` fence (matplotlib `plt.show()` → inline PNG, ASCII plot
      ;; carried as its fallback body), a `vis-table` fence (a CSV/TSV artifact
      ;; carried as its own grid) or a `vis-doc` fence (a PDF/HTML document,
      ;; carrying only its host path) rides stdout as MARKDOWN so the channel
      ;; paints it inline; wrapping it in a ``` block would escape the
      ;; 4-backtick fence. These fences are bounded at their source.
      (or (str/includes? stdout "````vis-image")
          (str/includes? stdout "````vis-doc")
          (str/includes? stdout "````vis-table"))
      {:body stdout}
      ;; Plain output is not markdown: fence it so CommonMark preserves newlines.
      (not (str/blank? stdout)) {:body (util/fenced (clip stdout))}
      ;; A structured host/native value only exists when stdout does not.
      (some? (:result form)) (when-let [s (clip (env/ctx->python-str (:result form)))]
                               {:body (util/fenced s "python")})
      :else nil)))

(defn result-card
  "Canonical result CARD descriptor — the ONE collapse decision shared by Clojure
   channels. It combines the form's concise `:result-summary` headline with a body
   derived locally from canonical `:stdout`/`:result` facts:

     {:op           `grep`       — the form's own op, never registry-derived
      :summary      12 results    — optional tool-authored headline
      :body         …markdown…    — optional local projection of the result fact
      :collapsible? true}         — true exactly when a body exists

   nil means the form has neither headline nor output. Archived values that can no
   longer be formatted lose only their body; reopening a transcript must not fail."
  [{:keys [op result-summary] :as form}]
  (let [summary
        (some-> result-summary
                str
                str/trim
                not-empty)

        body
        (try (some-> (result-display form)
                     :body
                     str
                     str/trimr
                     not-empty)
             (catch Throwable _ nil))]

    (when (or summary body) {:op op :summary summary :body body :collapsible? (boolean body)})))

(defn with-display
  "Attach the cached ruff rendering of a form's Python source when the form did not
   author its own `:display-code`. Result presentation is always derived locally
   from canonical facts and is never attached to the form."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (pyfmt/beautify-python (:code form)))))

(defn ->display
  "Project canonical display fields from a source map. Drops nils, and when stdout
   exists drops `:result`: one form has one output fact, never two success channels."
  [m]
  (reduce (fn [acc k]
            (if (and (some? (get m k)) (not (and (= k :result) (some? (:stdout m)))))
              (assoc acc k (get m k))
              acc))
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
