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
            [com.blockether.vis.internal.config.toggles :as toggles]
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

(defn- utf8-length
  "Exact UTF-8 size of `s` without encoding it. A lone surrogate counts as 2 bytes,
   which overstates the 1-byte replacement that the encoder writes."
  ^long [^String s]
  (let [n (.length s)]
    (loop [i 0
           total 0]

      (if (< i n)
        (let [c (int (.charAt s i))]
          (recur (inc i)
                 (+ total
                    (long (cond (< c 0x80) 1
                                (< c 0x800) 2
                                (Character/isSurrogate (char c)) 2
                                :else 3)))))
        total))))

(defn- clip-stdout
  [s model scope call-id max-output-tokens max-preview-chars]
  (let [s
        (str/trimr s)

        n
        (count s)

        tokens
        #(long (svar/count-tokens model %))

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
                (if (and (< tail-start n) (Character/isLowSurrogate (.charAt ^String s tail-start)))
                  (inc tail-start)
                  tail-start)

                tail
                (subs s tail-start)]

            (str head
                 "\n# ⋯ stdout clipped: kept " (+ (count head) (count tail))
                 "/" n
                 " chars (" max-output-tokens
                 " estimated-token ceiling).\n" recovery
                 "\n" tail)))]

    (when (pos? n)
      (if (and (<= n max-preview-chars) (<= (tokens s) max-output-tokens))
        s
        (loop [low
               0

               high
               (min n max-preview-chars)]

          (if (< low high)
            (let [mid (inc (quot (+ low high) 2))]
              (if (<= (tokens (preview mid)) max-output-tokens)
                (recur mid high)
                (recur low (dec mid))))
            (preview low)))))))

(defn- stdout-key
  "The cache key of one clip. The text is keyed by its SHA-256, so an equal string
   reloaded from the database hits, and the key holds no source text."
  [[s model scope call-id max-output-tokens max-preview-chars]]
  [(util/sha256-hex s) model scope call-id max-output-tokens max-preview-chars])

(def ^:private cached-stdout
  (util/bounded-memoize
    (fn [s model scope call-id max-output-tokens max-preview-chars]
      (let [result (clip-stdout s model scope call-id max-output-tokens max-preview-chars)]
        ;; Most results fit. Keep only this mark, so many fit in the weight limit.
        (if (= result (str/trimr s)) ::fits result)))
    4096
    (* 8 1024 1024)
    (fn [[_ model scope call-id] result]
      (+ 1024
         (*
           2
           (+ (count model) (count scope) (count call-id) (if (string? result) (count result) 0)))))
    stdout-key))

(defn clip-to-wire
  "Project stdout for model replay, gateway wire and human display. Keep raw stdout unchanged.
   `form` supplies the serving model, iteration scope and tool-call identity.
   Normalize live scopes from `tN/iM/fN` to `tN/iM` for saved-block recovery.

   Pass text within the ceiling by UTF-8 size without a count.
   Count the excerpt and recovery marker with Svar's tokenizer. Reuse up to 4096 projections.
   Keep only a mark for text that fits, and the excerpt for text that does not.
   Key source text by SHA-256, so equal text from a reload hits without holding the text.
   Limit retained results and metadata to an estimated 8 MiB."
  ([s] (clip-to-wire s {}))
  ([s form]
   (let [s
         (str (or s ""))

         ;; The recovery hint calls `read_session()`, which exists only while the
         ;; `introspection` toggle is on. Without a scope the hint says to slice instead.
         scope
         (when (and (string? (:scope form)) (toggles/enabled? "introspection"))
           (str/replace (:scope form) #"/f[1-9]\d*$" ""))

         call-id
         (:svar/tool-call-id form)

         trimmed
         (str/trimr s)]

     ;; A byte-pair token covers at least one UTF-8 byte. Text within the ceiling by
     ;; size cannot exceed it in tokens, so it needs no count and no cache slot.
     (cond (str/blank? trimmed) nil
           (<= (utf8-length trimmed) (long MAX_FORM_OUTPUT_TOKENS)) trimmed
           :else (let [result (cached-stdout
                                s
                                (or (:llm-model form) "unknown")
                                (when (and scope (re-matches #"t[1-9]\d*/i[1-9]\d*" scope)) scope)
                                (when (and (string? call-id)
                                           (<= (count call-id) 512)
                                           (re-matches #"[A-Za-z0-9_|-]+" call-id))
                                  call-id)
                                MAX_FORM_OUTPUT_TOKENS
                                MAX_FORM_PREVIEW_CHARS)]
                   (if (identical? ::fits result) trimmed result))))))

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
