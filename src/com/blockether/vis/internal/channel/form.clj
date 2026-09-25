(ns com.blockether.vis.internal.channel.form
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
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.python.format :as pyfmt]
            [com.blockether.vis.internal.util :as util]))

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
   ;; tool-call linkage, serving model for tokenizer-aware stdout, and status
   [:svar/tool-call-id "tool_call_id"] [:llm-model "llm_model"] [:timeout? "is_timeout"]
   [:repaired? "is_repaired"]])

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

(def MAX_FORM_OUTPUT_TOKENS
  "Maximum estimated tokens in one printed result, including its recovery pointer."
  4096)

(def ^:private MAX_FORM_PREVIEW_CHARS
  "Additional work bound for unusually compressible text before token counting."
  32768)

(defn clip-to-wire
  "Project one stdout body for model replay, gateway wire and human display. The
   same Svar tokenizer selection used for request estimates prices the body and its
   marker; a head/tail binary search keeps the entire excerpt inside the budget.
   Raw stdout remains in the saved form. `form` supplies the serving model and
   recoverable iteration scope/tool-call identity. Live events may add `/fN` to
   the display scope; saved `read_session()` blocks use only `tN/iM`."
  ([s] (clip-to-wire s {}))
  ([s form]
   (let [s
         (str/trimr (str (or s "")))

         n
         (count s)

         model
         (or (:llm-model form) "unknown")

         tokens
         #(long (svar/count-tokens model %))

         scope
         (when (string? (:scope form)) (str/replace (:scope form) #"/f[1-9]\d*$" ""))

         call-id
         (:svar/tool-call-id form)

         addressable?
         (and (string? scope)
              (re-matches #"t[1-9]\d*/i[1-9]\d*" scope)
              (string? call-id)
              (<= (count call-id) 512)
              (re-matches #"[A-Za-z0-9_|-]+" call-id))

         recovery
         (if addressable?
           (str "# Recover exact stdout: r = await read_session(); "
                "b = next(b for t in r[\"transcript\"][\"turns\"] "
                "for i in t[\"iterations\"] for b in i[\"blocks\"] "
                "if b.get(\"scope\") == "
                (pr-str scope)
                " and b.get(\"svar_tool_call_id\") == "
                (pr-str call-id)
                "); print(b[\"stdout\"][0:4096]) # adjust the slice as needed")
           "# To see more, slice or filter the result in the sandbox and print less.")

         preview
         (fn [kept]
           (let [head-len
                 (quot kept 4)

                 head
                 (util/truncate s head-len)

                 tail-start
                 (- n (- kept head-len))

                 tail-start
                 (if (and (< tail-start n)
                          (Character/isLowSurrogate (.charAt ^String s tail-start)))
                   (inc tail-start)
                   tail-start)

                 tail
                 (subs s tail-start)]

             (str head
                  "\n# ⋯ stdout clipped: kept " (+ (count head) (count tail))
                  "/" n
                  " chars (" MAX_FORM_OUTPUT_TOKENS
                  " estimated-token ceiling).\n" recovery
                  "\n" tail)))]

     (when (pos? n)
       (if (and (<= n MAX_FORM_PREVIEW_CHARS) (<= (tokens s) MAX_FORM_OUTPUT_TOKENS))
         s
         (loop [low
                0

                high
                (min n MAX_FORM_PREVIEW_CHARS)]

           (if (< low high)
             (let [mid (inc (quot (+ low high) 2))]
               (if (<= (tokens (preview mid)) MAX_FORM_OUTPUT_TOKENS)
                 (recur mid high)
                 (recur low (dec mid))))
             (preview low))))))))

(defn stdout-display
  "Project a form's printed stdout into a human card. Artifact fences are passed
   through only when complete; clipped fences become ordinary fenced text, never a
   broken inline image, document or table. Model, gateway and card all use the same
   tokenizer-bounded stdout projection."
  [form]
  (when-let [stdout (clip-to-wire (:stdout form) form)]
    (let [artifact? (and (= stdout (str/trimr (str (:stdout form))))
                         (or (str/includes? stdout "````vis-image")
                             (str/includes? stdout "````vis-doc")
                             (str/includes? stdout "````vis-table")))]
      {:body (if artifact? stdout (util/fenced stdout))})))

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
  "Attach the cached formatted rendering of a form's Python source when the form did not
   author its own `:display-code`. Result presentation is always derived locally
   from canonical facts and is never attached to the form."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (pyfmt/beautify-python (:code form)))))

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
