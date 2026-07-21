(ns com.blockether.vis.internal.foundation.transcript
  "Full session transcript - DATA first, presentation second.

   `transcript` returns one canonical Clojure map with every turn,
   every iteration, every executed block plus the LLM-side context
   (system prompt, message envelope, reasoning trace, top-level
   provider error, per-iteration vars, answer-form pointer,
   returned-empty-blocks flag) and the per-block forensic detail
   (code, comment, result, error, duration, timeout?, repaired?).
   Pure data. The agent can pattern-match on it; the CLI
   renders Markdown on top; a future TUI screen, JSON exporter, or
   analytics extension consumes the same shape.

   Lives in foundation because it's an introspection surface, not host
   plumbing. The sandbox-visible public surface is `(session-state)`
   for data and `(session-report-html)` for an HTML report; this namespace
   owns the transcript portion behind that deeper interface.

   Public Clojure surface:

     `(transcript      db-info session-id)`  -> transcript data map
     `(transcript->md  data)`             -> Markdown string
     `(transcript-md   db-info session-id)`  -> DB lookup + Markdown string

   Canonical data shape:

     {:session {:id :title :channel :model :provider :created-at}
      :totals       {:turns N :iterations N
                     :tokens {:input :output :reasoning :cached}
                     :cost-usd D}
      :dialog      [{:role :turn-id :content}]
      :calls       [{:kind :ref :parent-ref :turn-id :iteration-id :op :tool
                     :var :code :status :duration-ms :command :target
                     :result :result-summary :info}]
      :timeline    [{:kind :ref :turn-id :iteration-id :content :code
                     :status :duration-ms :result-summary}]
      :turns
       [{:id :user-request :status :prior-outcome :provider :model
         :iteration-count :failure-count
         :tokens :cost-usd :content
         :iterations
          [{:id :position :status :duration-ms
            :provider :model :thinking :error
            :tokens :cost-usd
            :answer-position :returned-empty-blocks?
            :vars
            [{:name :code :value :version}]
            :attachments
            [{:id :source :tool-call-id :position :kind
              :media-type :filename :size :stored}]
            :blocks
            [{:position :code :comment :result :stdout :error
              :duration-ms :timeout? :repaired?}]}]}]}

   The Markdown renderer renders thinking, iteration-level errors,
   vars, per-block forensic previews, and final answer text. Large
   fields are bounded so reports stay safe to open."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.content :as content])
  (:import [java.util Locale]
           [java.time ZoneId]
           [java.time.format DateTimeFormatter]
           [org.commonmark.parser Parser]
           [org.commonmark.renderer.html HtmlRenderer]))

;; =============================================================================
;; Data layer.
;; =============================================================================

(def ^:private encrypted-reasoning-placeholder
  "[provider returned encrypted reasoning; plaintext reasoning is unavailable]")

(defn- visible-thinking
  [thinking]
  (let
    [s (some-> thinking
               str)]
    (when-not (or (str/blank? (or s "")) (= encrypted-reasoning-placeholder s)) s)))

(defn- iteration-rollup
  "Sum the per-iteration token + cost columns across a vec of
   iteration maps. Every value is numeric on the read side (the
   persistance layer defaults NULL columns to 0 / 0.0), so this is
   a clean reduce."
  [iterations]
  (reduce (fn [a it]
            (-> a
                ;; Phase B canonical iteration keys. `:input-tokens` is
                ;; TOTAL (Anthropic-additive raw values summed at the
                ;; canonical-normalizer boundary); details obey the
                ;; invariant on a per-row basis.
                (update-in [:tokens :input] + (long (or (:input-tokens it) 0)))
                (update-in [:tokens :output] + (long (or (:output-tokens it) 0)))
                (update-in [:tokens :reasoning] + (long (or (:output-reasoning-tokens it) 0)))
                (update-in [:tokens :cached] + (long (or (:input-cache-read-tokens it) 0)))
                (update-in [:tokens :cache-created] + (long (or (:input-cache-write-tokens it) 0)))
                (update :cost-usd + (double (or (:cost-usd it) 0.0)))))
          {:tokens {:input 0 :output 0 :reasoning 0 :cached 0 :cache-created 0} :cost-usd 0.0}
          iterations))

(defn- form-envelope->block
  "Project one per-form envelope from `:forms` into the transcript's
   `:blocks` shape. Each envelope carries `:scope :tag :src :result :error`
   and `:stdout` (what a `python_execution` block PRINTED — its primary
   output; a success value rides `:result` while printed context rides
   `:stdout`, and a failing block may carry partial `:stdout` alongside
   `:error`). All are surfaced, plus a 0-based `:position` derived from the
   form's index in the iter's `:forms` vec."
  [position envelope]
  (cond-> {:position position :code (or (:src envelope) "")}
    (:scope envelope)
    (assoc :scope (:scope envelope))

    (:tag envelope)
    (assoc :tag (:tag envelope))

    (contains? envelope :result)
    (assoc :result (:result envelope))

    ;; Printed output — the primary content of a `python_execution` block.
    ;; Rides `:stdout`, NOT `:result` (bare values aren't echoed), and also
    ;; appears alongside `:error` when a failing block printed before it threw.
    ;; Without this the forensic transcript shows `result: None` for every
    ;; python block and loses what it actually printed.
    (some? (:stdout envelope))
    (assoc :stdout (:stdout envelope))

    (contains? envelope :error)
    (assoc :error (:error envelope))))

(defn- attachment-descriptor
  "Lean, byte-free descriptor for ONE persisted iteration attachment (an element
   of `db-list-iteration-attachments`). Drops `:base64` — a produced artifact can
   be MBs and would bloat every transcript projection — keeping only the metadata
   a reader needs PLUS the bare row `:id` to fetch the bytes on demand via
   `db-read-attachment` (Clojure) / the sandbox `vis_read_attachment(id)` shim.
   `:stored` records where the bytes live (`:inline` DB blob, `:external` storage
   backend, or `:none`) without carrying them."
  [att]
  (-> (select-keys att [:id :source :tool-call-id :position :kind :media-type :filename :size])
      (assoc :stored (cond (:base64 att) :inline
                           (:storage-uri att) :external
                           :else :none))))

(defn- enrich-iteration
  "Attach `:blocks` and `:attachments` to one iteration row.

     `:blocks` - one entry per top-level form the iter executed,
                derived from the iter's `:forms` envelope vec on
                `session_turn_iteration.forms`. Cross-turn def
                rehydration is gone, so there is no separate `:vars`
                slice — every form lives on the iteration row.

     `:attachments` - byte-free descriptors for the OUTBOUND artifacts
                (matplotlib figures, `vis_attach` payloads) the iter's
                tool calls produced, joined from the `session_attachment`
                rail via `db-list-iteration-attachments`. Each carries a
                read-back `:id` so the bytes stay lazily fetchable
                (`db-read-attachment` / `vis_read_attachment`) instead of
                bloating the transcript. Absent when the iter produced none.

   Degrades silently to `[]` so the renderer never throws on a
   partial DB."
  [db-info iter]
  (let
    [forms
     (or (:forms iter) [])

     blocks
     (vec (map-indexed form-envelope->block forms))

     attachments
     (mapv attachment-descriptor
           (try (vis/db-list-iteration-attachments db-info (:id iter)) (catch Throwable _ [])))]

    (cond->
      (-> iter
          (update :thinking visible-thinking)
          (assoc :blocks blocks)
          (assoc :failure-count (count (filter :error blocks))))
      (seq attachments)
      (assoc :attachments attachments))))

(defn- build-turn
  "Pure projection: one session_turn_soul row + its iterations -> the
   turn-shaped data map the public `transcript` returns."
  [db-info turn]
  (let
    [raw-iters
     (try (vis/db-list-session-turn-iterations db-info (:id turn)) (catch Throwable _ []))

     iters
     (mapv (partial enrich-iteration db-info) raw-iters)

     totals
     (iteration-rollup iters)

     provider
     (some #(some-> %
                    :provider
                    name)
           iters)

     model
     (some :model iters)]

    (cond->
      {:id (:id turn)
       :position (:position turn)
       :created-at (:created-at turn)
       :duration-ms (:duration-ms turn)
       :user-request (or (:user-request turn) "")
       :status (:status turn)
       :prior-outcome (:prior-outcome turn)
       :iteration-count (count iters)
       :failure-count (reduce + 0 (map :failure-count iters))
       :iterations iters
       :tokens (:tokens totals)
       :cost-usd (:cost-usd totals)
       :content (vec (or (:content turn) []))}
      provider
      (assoc :provider provider)

      model
      (assoc :model model))))


(defn- session-totals
  "Sum tokens + cost + iteration counts across every turn."
  [turns]
  (reduce (fn [a t]
            (-> a
                (update :iterations + (long (or (:iteration-count t) 0)))
                (update-in [:tokens :input] + (long (or (:input (:tokens t)) 0)))
                (update-in [:tokens :output] + (long (or (:output (:tokens t)) 0)))
                (update-in [:tokens :reasoning] + (long (or (:reasoning (:tokens t)) 0)))
                (update-in [:tokens :cached] + (long (or (:cached (:tokens t)) 0)))
                (update-in [:tokens :cache-created] + (long (or (:cache-created (:tokens t)) 0)))
                (update :cost-usd + (double (or (:cost-usd t) 0.0)))))
          {:turns (count turns)
           :iterations 0
           :tokens {:input 0 :output 0 :reasoning 0 :cached 0 :cache-created 0}
           :cost-usd 0.0}
          turns))

(def ^:private transcript-known-channels
  ;; Channels scanned when resolving a session by short PREFIX (full UUIDs and
  ;; `db-resolve-session-id` hits don't need this). MUST include every channel
  ;; that persists sessions, or a prefix on a missing channel silently resolves
  ;; to nothing — e.g. `:api` (the gateway channel) must be included.
  [:tui :cli :api])

(defn- resolve-session-ref
  "Resolve one transcript/session reference to the canonical UUID.

   Accepted shapes:
     - UUID           => returned only when it exists
     - full UUID str  => parsed, then existence-checked
     - unique prefix  => scanned across every channel and expanded

   Returns nil on miss or ambiguous prefix. Unlike
   `db-resolve-session-id`, this helper is existence-aware - a
   well-formed but unknown UUID string must not masquerade as a real
   session."
  [db-info session-ref]
  (letfn [(existing-id [id]
            (when (and id (try (vis/db-get-session db-info id) (catch Throwable _ nil))) id))]
    (cond (nil? session-ref) nil
          (uuid? session-ref) (existing-id session-ref)
          :else (let [s (str session-ref)]
                  (or (existing-id (try (vis/db-resolve-session-id db-info s)
                                        (catch Throwable _ nil)))
                      (let
                        [matches (->> transcript-known-channels
                                      (mapcat #(or (vis/db-list-sessions db-info %) []))
                                      (filter (fn [session]
                                                (str/starts-with? (str (:id session)) s)))
                                      vec)]
                        (when (= 1 (count matches)) (:id (first matches)))))))))

(defn- preview-string
  [s ^long n]
  (let [s (str s)]
    (if (<= (count s) n) s (str (subs s 0 n) "..."))))

(defn- preview-value [v n] (preview-string (pr-str v) n))

(defn- runtime-ref? [v] (and (map? v) (= :expr (:vis/ref v))))

(defn- op-slug
  [op]
  (let
    [s (cond (keyword? op) (if (namespace op) (str (namespace op) "." (name op)) (name op))
             (symbol? op) (if (namespace op) (str (namespace op) "." (name op)) (name op))
             :else (str op))]
    (-> s
        (str/replace #"/" ".")
        (str/replace #"[^A-Za-z0-9_.:-]" "-"))))

(defn- form-index [block] (or (:position block) (:idx block) (:id block) 0))

(defn- block-ref
  [turn iteration block]
  (or (get-in block [:envelope :ref])
      (:scope block)
      ;; Canonical model/CTX scope. Avoid `turn/<uuid-prefix>` refs: they
      ;; look like impossible turn numbers (`turn/75797678/...`).
      (str "t" (:position turn) "/i" (:position iteration) "/f" (inc (long (form-index block))))))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
             (nat-int? (:started-at-ms envelope))
             (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))

(defn- block-duration-ms
  [block]
  (or (envelope-duration-ms (:envelope block)) (:duration-ms block) 0))

(defn- tool-result-envelope?
  [value]
  (and (map? value) (contains? value :success?) (contains? value :info)))

(defn- result-summary
  "Bounded, data-first result preview for timeline/call rows. The full
   values remain available where they were persisted (`:blocks` and
   `:calls :result`); this summary makes the timeline useful without
   forcing callers to inspect provider/tool-specific payloads."
  [result]
  (cond (runtime-ref? result) {:type :runtime-ref
                               :preview "<runtime value; see matching var/call row>"}
        (map? result)
        (cond-> {:type :map :keys (vec (take 16 (keys result))) :preview (preview-value result 400)}
          (contains? result :exit)
          (assoc :exit (:exit result))

          (contains? result :timed-out?)
          (assoc :timed-out? (:timed-out? result))

          (contains? result :command)
          (assoc :command (:command result))

          (contains? result :duration-ms)
          (assoc :duration-ms (:duration-ms result)))
        :else {:type (cond (nil? result) :nil
                           (string? result) :string
                           (keyword? result) :keyword
                           (number? result) :number
                           (coll? result) :collection
                           :else :value)
               :preview (preview-value result 400)}))

(defn- event-status
  [error success? timeout?]
  (cond timeout? :timeout
        error :error
        (false? success?) :error
        :else :done))

(defn- tool-call-row
  [turn iteration block var-row envelope]
  (let
    [tool-meta
     (or (:metadata envelope) (:info envelope))

     result
     (if (contains? envelope :result) (:result envelope) (:result envelope))

     success?
     (if (contains? envelope :success?) (:success? envelope) (:success? envelope))

     error
     (if (contains? envelope :error) (:error envelope) (:error envelope))

     op
     (or (:symbol envelope) (:op tool-meta) :tool)

     parent-ref
     (when block (block-ref turn iteration block))

     ref
     (when parent-ref (str parent-ref "/tool/" (op-slug op)))

     status
     (event-status error success? (or (:timed-out? result) (:timeout? tool-meta)))

     tool
     (:tool tool-meta)]

    (cond->
      {:kind :tool-call
       :ref ref
       :parent-ref parent-ref
       :turn-id (:id turn)
       :iteration-id (:id iteration)
       :iteration (:position iteration)
       :op op
       :tool (or (:symbol tool) (:call tool) tool)
       :status status
       :success? success?
       :duration-ms (or (:duration-ms tool-meta) (:duration-ms result) 0)
       :code (:code block)
       :result result
       :result-summary (result-summary result)
       :info tool-meta}
      var-row
      (assoc :var (:name var-row))

      (:command tool-meta)
      (assoc :command (:command tool-meta))

      (:command result)
      (assoc :command (:command result))

      (:target tool-meta)
      (assoc :target (:target tool-meta))

      error
      (assoc :error error))))

(defn- block-by-code
  [iteration]
  (reduce (fn [acc block]
            (if (contains? acc (:code block)) acc (assoc acc (:code block) block)))
          {}
          (:blocks iteration)))

(defn- iteration-tool-calls
  [turn iteration]
  (let
    [_blocks-by-code
     (block-by-code iteration)

     direct-calls
     (keep (fn [block]
             (when (tool-result-envelope? (:result block))
               (tool-call-row turn iteration block nil (:result block))))
           (:blocks iteration))

     {:keys [order rows]}
     (reduce (fn [{:keys [order rows] :as acc} call]
               (let [dedupe-key (or (:ref call) [(:parent-ref call) (:op call) (:code call)])]
                 (if (contains? rows dedupe-key)
                   (assoc acc :rows (update rows dedupe-key merge call))
                   {:order (conj order dedupe-key) :rows (assoc rows dedupe-key call)})))
             {:order [] :rows {}}
             direct-calls)]

    (mapv rows order)))

(defn- transcript-calls
  [turns]
  (vec (mapcat (fn [turn]
                 (mapcat #(iteration-tool-calls turn %) (:iterations turn)))
               turns)))

(defn- dialog-events
  [turns]
  (vec (mapcat (fn [turn]
                 (cond-> [{:role :user :turn-id (:id turn) :content (:user-request turn)}]
                   (seq (:content turn))
                   (conj {:role :assistant :turn-id (:id turn) :content (:content turn)})))
               turns)))

(defn- code-event
  [turn iteration block]
  (let [error (:error block)]
    (cond->
      {:kind :code
       :ref (block-ref turn iteration block)
       :turn-id (:id turn)
       :iteration-id (:id iteration)
       :iteration (:position iteration)
       :form-position (inc (long (form-index block)))
       :role (:role block)
       :status (event-status error true (:timeout? block))
       :duration-ms (block-duration-ms block)
       :code (:code block)}
      (contains? block :result)
      (assoc :result-summary (result-summary (:result block)))

      error
      (assoc :error error))))

(defn- transcript-timeline
  [turns calls]
  (let [calls-by-parent (group-by :parent-ref calls)]
    (vec (mapcat (fn [turn]
                   (concat
                     [{:kind :user-message :turn-id (:id turn) :content (:user-request turn)}]
                     (mapcat (fn [iteration]
                               (mapcat (fn [block]
                                         (let [ref (block-ref turn iteration block)]
                                           (cons (code-event turn iteration block)
                                                 (get calls-by-parent ref))))
                                       (:blocks iteration)))
                             (:iterations turn))
                     (when (:answer turn)
                       [{:kind :assistant-message :turn-id (:id turn) :content (:answer turn)}])))
                 turns))))

(defn transcript
  "Full session transcript as one Clojure data map. See ns
   docstring for the canonical shape. Returns nil when the
   session id does not resolve.

   `session-id` accepts either the canonical UUID or an
   unambiguous string prefix.

   Pure with respect to the database - no writes, no logging.
   `(:db-info env)` is the standard handle; the sandbox-bound symbol
   variant uses the live env automatically."
  [db-info session-id]
  (when-let [resolved-id (resolve-session-ref db-info session-id)]
    (when-let [session (try (vis/db-get-session db-info resolved-id) (catch Throwable _ nil))]
      (let
        [turn-rows (try (vis/db-list-session-turns db-info resolved-id) (catch Throwable _ []))
         turns (mapv (partial build-turn db-info) turn-rows)
         totals (session-totals turns)
         calls (transcript-calls turns)]

        {:session (cond->
                    {:id resolved-id
                     :title (:title session)
                     :channel (:channel session)
                     :model (:model session)
                     :created-at (:created-at session)}
                    (:provider session)
                    (assoc :provider (:provider session)))
         :totals totals
         :dialog (dialog-events turns)
         :calls calls
         :timeline (transcript-timeline turns calls)
         :turns turns}))))

;; =============================================================================
;; Markdown renderer. Pure transformation over `transcript`'s data
;; shape - no DB calls, no side effects.
;; =============================================================================

(defn- one-line
  [s]
  (-> (or s "")
      str
      (str/replace #"\s+" " ")
      str/trim))

(defn- format-cost-usd
  "Locale-stable USD formatter - always a dot separator (`$0.0042`),
   never a locale comma. nil collapses to `$0.0000` so callers don't
   `or`-pad."
  [c]
  (let [v (double (or c 0.0))]
    (String/format Locale/US "$%.4f" (object-array [v]))))

(defn- format-tokens
  [{:keys [input output reasoning cached cache-created]}]
  (let
    [base
     (str (long (or input 0)) "/" (long (or output 0)))

     suff
     (cond-> []
       (and reasoning (pos? (long reasoning)))
       (conj (str "r=" reasoning))

       (and cached (pos? (long cached)))
       (conj (str "c=" cached))

       (and cache-created (pos? (long cache-created)))
       (conj (str "w=" cache-created)))]

    (if (seq suff) (str base " (" (str/join ", " suff) ")") base)))

(defn- fmt-inst
  "Format a #inst / java.util.Date into a readable local date-time."
  [d]
  (when d
    (let
      [inst
       (.toInstant ^java.util.Date d)

       zone
       (ZoneId/systemDefault)

       fmt
       (.withLocale (DateTimeFormatter/ofPattern "MMM d, yyyy \u00b7 HH:mm z") Locale/US)]

      (.format (.atZone inst zone) fmt))))

(defn- fmt-duration-ms
  "Coarse human duration: `2h 57m`, `4m 12s`, `9s`."
  [ms]
  (when (and ms (pos? (long ms)))
    (let
      [s
       (quot (long ms) 1000)

       h
       (quot s 3600)

       m
       (quot (rem s 3600) 60)

       sec
       (rem s 60)]

      (cond (pos? h) (str h "h " m "m")
            (pos? m) (str m "m " sec "s")
            :else (str sec "s")))))

(defn- fmt-avg
  "Ratio to one decimal, locale-stable (`3.5`); `0` when the denominator is 0."
  [num den]
  (if (and den (pos? (long den)))
    (str (/ (Math/round (* 10.0 (/ (double num) (double den)))) 10.0))
    "0"))

(defn- most-common
  "The most frequent non-nil value in `xs`, or nil."
  [xs]
  (when-let [freqs (seq (frequencies (remove nil? xs)))]
    (key (apply max-key val freqs))))

(defn- session-finished-inst
  "Best estimate of when the session settled: the latest turn start plus its
   duration. nil when no turn carries a timestamp."
  [turns]
  (let
    [ends (keep (fn [t]
                  (when-let [c (:created-at t)]
                    (+ (.getTime ^java.util.Date c) (long (or (:duration-ms t) 0)))))
                turns)]
    (when (seq ends) (java.util.Date. (long (apply max ends))))))

(defn session-summary
  "Grouped [[group-label [[key value mono?] ...]] ...] rows shared by the
   Markdown summary and the HTML summary card. Pure over the `transcript`
   data map - one canonical summary shape for every surface."
  [{:keys [session totals calls turns]}]
  (let
    [n-turns
     (max 0 (long (or (:turns totals) 0)))

     n-iters
     (long (or (:iterations totals) 0))

     ;; Executed code blocks are the agent's tool-invocation units; the
     ;; richer `:calls` rows (nested tool envelopes) are used when present.
     n-blocks
     (reduce + 0 (map (comp count :blocks) (mapcat :iterations turns)))

     n-calls
     (max (count calls) (long n-blocks))

     providers
     (->> turns
          (map :provider)
          (remove nil?)
          distinct
          vec)

     models
     (->> turns
          (map :model)
          (remove nil?)
          distinct
          vec)

     started
     (:created-at session)

     finished
     (session-finished-inst turns)]

    [["Session"
      [["Name" (or (:title session) "Untitled session") false] ["ID" (str (:id session)) true]
       ["Channel"
        (or (some-> (:channel session)
                    name)
            "\u2014") false]]]
     ["Timing"
      [["Started" (or (fmt-inst started) "\u2014") false]
       ["Finished" (or (fmt-inst finished) "\u2014") false]
       ["Duration"
        (or (when (and started finished)
              (fmt-duration-ms (- (.getTime ^java.util.Date finished)
                                  (.getTime ^java.util.Date started))))
            "\u2014") false]]]
     ["Activity"
      [["Turns" (str n-turns) false] ["Iterations" (str n-iters) false]
       ["Avg iterations / turn" (fmt-avg n-iters n-turns) false] ["Tool calls" (str n-calls) false]
       ["Avg tool calls / turn" (fmt-avg n-calls n-turns) false]
       ["Avg tool calls / iteration" (fmt-avg n-calls n-iters) false]]]
     ["Providers & models"
      [["Top provider" (or (most-common (map :provider turns)) "\u2014") true]
       ["Top model" (or (most-common (map :model turns)) "\u2014") true]
       ["Providers used" (if (seq providers) (str/join ", " providers) "\u2014") true]
       ["Models used" (if (seq models) (str/join ", " models) "\u2014") true]]]
     ["Cost & tokens"
      [["Total cost" (format-cost-usd (:cost-usd totals)) false]
       ["Tokens (in / out)" (format-tokens (:tokens totals)) true]]]]))

(defn- fence-delimiter
  "Markdown fence delimiter longer than any backtick run in `body`.

   Forensic reports can include source/diffs/results that themselves contain
   Markdown fences (for example a Python patch string containing
   ```clojure). A fixed triple-backtick wrapper is then ambiguous and closes
   early, corrupting the rendered report. CommonMark permits longer fences, so
   choose the shortest safe delimiter."
  [body]
  (let
    [max-run (->> (re-seq #"`+" (str body))
                  (map count)
                  (reduce max 0))]
    (apply str (repeat (max 3 (inc (long max-run))) "`"))))

(defn- render-fenced
  [lang body]
  (let
    [s
     (str body)

     fence
     (fence-delimiter s)]

    (if (str/blank? s) "" (str fence (or lang "") "\n" s "\n" fence "\n"))))

(defn- display-result
  [result]
  (if (and (map? result) (= :expr (:vis/ref result)))
    "<runtime value; re-evaluate expression to restore>"
    (pr-str result)))

(defn- render-block-code-segments
  [code render-segments]
  (if (seq render-segments)
    (let
      [body (apply str
              (keep (fn [{:keys [kind source value]}]
                      (case kind
                        :code
                        (when-not (str/blank? (str source)) (render-fenced "python" source))

                        :title
                        (str "_session title:_ `" (or value "") "`\n")

                        :answer-ref
                        nil

                        nil))
                    render-segments))]
      (when-not (str/blank? body) body))
    (render-fenced "python" code)))

(defn- render-block-section
  "Per-block forensic dump: status header, optional comment, full code
   in a fenced ```python block, result line, fenced error. `answer?`
   flips on the block the iteration's `:answer-position` points at —
   the block that called `done(...)` — so the reader spots the
   terminal block at a glance.

   Verbatim: result strings, error blobs, and code segments are
   rendered without truncation. Forensic reports are useless when the
   first place you look has been clipped."
  [idx answer? {:keys [code comment render-segments result stdout error] :as block}]
  (let
    [marker
     (if error "✗" "✓")

     flags
     (cond-> []
       answer?
       (conj "answer")

       (:timeout? block)
       (conj "timeout")

       (:repaired? block)
       (conj "repaired")

       error
       (conj "error"))

     suffix
     (if (seq flags) (str " [" (str/join ", " flags) "]") "")

     has-result?
     (and (not error) (contains? block :result))]

    (str "##### Block "
         idx
         " - "
         marker
         " "
         (long (block-duration-ms block))
         "ms"
         suffix
         "\n"
         (when (not (str/blank? comment)) (str comment "\n"))
         (render-block-code-segments code render-segments)
         ;; What the block PRINTED — the primary output of a python_execution
         ;; block; rendered verbatim so forensic review sees the real content.
         (when (not (str/blank? stdout)) (str "\n_stdout:_\n" (render-fenced "text" stdout)))
         (when has-result? (str "\nResult: `" (display-result result) "`\n"))
         (when error (str "\n_error:_\n" (render-fenced "text" error)))
         "\n")))

(defn- render-thinking
  "Render the LLM's reasoning trace as a fenced text block. The
   transcript carries the reasoning verbatim — no truncation — so the
   reader has a forensic record of what the model thought before it
   acted."
  [thinking]
  (when (not (str/blank? thinking)) (str "_thinking:_\n" (render-fenced "text" thinking) "\n")))

(defn- render-iter-error
  "Render an iteration-level error - the provider call failed before
   any block could run. Persisted on `iteration.llm_error` as JSON.
   nil/blank -> nothing emitted."
  [error]
  (when (and error (not (str/blank? (str error))))
    (str "_iteration error:_\n" (render-fenced "text" (str error)) "\n")))

(defn- render-attachments
  "List the OUTBOUND artifacts (matplotlib figures, `vis_attach` payloads) an
   iteration's tool calls produced, each with its read-back id. Bytes are NEVER
   inlined — the reader fetches them on demand via `db-read-attachment` /
   `vis_read_attachment(<id>)`. nil when the iteration produced none."
  [attachments]
  (when (seq attachments)
    (str "_attachments:_\n"
         (apply str
           (map (fn [{:keys [filename media-type kind size stored id]}]
                  (str "- "
                       (or (not-empty (str filename)) "artifact")
                       " ("
                       (or media-type "?")
                       (when kind (str ", " kind))
                       (when size (str ", " size "B"))
                       (when (and stored (not= stored :none)) (str ", " (name stored)))
                       ") — read with `vis_read_attachment("
                       id
                       ")`\n"))
                attachments))
         "\n")))

(defn- render-iteration-section
  [iter]
  (let
    [pos
     (:position iter)

     status
     (or (some-> (:status iter)
                 name)
         "-")

     dur
     (or (:duration-ms iter) 0)

     in
     (or (:input-tokens iter) 0)

     out
     (or (:output-tokens iter) 0)

     cost
     (or (:cost-usd iter) 0.0)

     blocks
     (:blocks iter)

     ;; Index of the block that called `done(...)`. nil for
     ;; non-terminal iterations - the marker only fires on the
     ;; right block.
     ans-idx
     (:answer-position iter)]

    (str "\n#### Iteration "
         pos
         " - "
         status
         " ("
         in
         "/"
         out
         " tokens, "
         (format-cost-usd cost)
         ", "
         (long dur)
         "ms)\n\n"
         (render-thinking (:thinking iter))
         (render-iter-error (:error iter))
         (render-attachments (:attachments iter))
         (cond (empty? blocks) "_No code blocks (LLM returned an empty response)._\n"
               :else (apply str
                       (map-indexed (fn [idx block]
                                      (render-block-section idx (= idx ans-idx) block))
                                    blocks))))))

(defn- render-final-answer
  "Render canonical content blocks as a disposable Markdown projection."
  [blocks]
  (let [answer (content/text-projection blocks)]
    (when-not (str/blank? answer) (str "\n#### Final answer\n\n" answer "\n"))))

(defn- render-turn-block
  [{:keys [position user-request status prior-outcome provider model iteration-count failure-count
           iterations tokens cost-usd content]}]
  ;; Render `position` (int), never `:id` (uuid). UUID stays in
  ;; introspection responses for programmatic callers,
  ;; never in user/LLM-facing surfaces.
  (str "### Turn "
       (or position "?")
       "\n"
       "- **User request:** "
       (one-line user-request)
       "\n"
       "- **Status:** "
       (or (some-> status
                   name)
           "-")
       (when prior-outcome (str " (" (name prior-outcome) ")"))
       "\n"
       "- **Provider/model:** "
       (or (cond (and provider model) (str provider "/" model)
                 model model
                 provider provider)
           "-")
       "\n"
       "- **Iterations:** "
       iteration-count
       "\n"
       "- **Failures:** "
       failure-count
       "\n"
       "- **Tokens (in/out):** "
       (format-tokens tokens)
       "\n"
       "- **Cost:** "
       (format-cost-usd cost-usd)
       "\n"
       (apply str (map render-iteration-section iterations))
       (render-final-answer content)
       "\n"))

(defn- render-summary-md
  "Markdown session summary - the SAME grouped `session-summary` rows the HTML
   card renders, as a title heading plus per-group bullet lists. One canonical
   summary shape, rendered to two surfaces."
  [data]
  (let
    [title (or (some-> data
                       :session
                       :title)
               "vis transcript")]
    (str "# " title
         "\n\n" (apply str
                  (for [[label rows] (session-summary data)]
                    (str "## "
                         label
                         "\n\n"
                         (apply str
                           (for [[k v _] rows]
                             (str "- **" k ":** " v "\n")))
                         "\n"))))))

(defn- render-dialog-message
  [{:keys [role content]}]
  (let [body (if (vector? content) (content/text-projection content) (str content))]
    (str "### "
         (case role
           :user
           "User"

           :assistant
           "Assistant"

           (name role))
         "\n\n"
         (render-fenced "markdown" body)
         "\n")))

(defn- render-dialog-md
  [{:keys [session dialog]}]
  (str "# Dialog" (when-let [t (:title session)]
                    (str " - " t))
       "\n\n"
       (if (seq dialog) (apply str (map render-dialog-message dialog)) "_No dialog messages._\n")))

(defn- render-turns-md
  "Turn-by-turn forensic body (no summary header)."
  [data]
  (str "## Turn-by-turn breakdown\n\n" (apply str (map render-turn-block (:turns data)))))

(defn- render-full-md [data] (str (render-summary-md data) (render-turns-md data)))

(defn transcript->md
  "Render transcript data as Markdown. Pure transformation over
   `transcript`'s canonical data shape. Returns a string.

   Modes:
   - `:full`               - bounded diagnostic report (default).
   - `:dialog`             - user/assistant dialog only."
  ([data] (transcript->md data {:mode :full}))
  ([data {:keys [mode] :or {mode :full}}]
   (case mode
     :dialog
     (render-dialog-md data)

     :full
     (render-full-md data)

     (render-full-md data))))

(defn transcript-md
  "Render the session as Markdown. Single transformation over
   `transcript`'s data. Returns a string; returns
   `\"Session not found: <id>\\n\"` (no throw) on a missing id so
   shell pipelines stay clean."
  ([db-info session-id] (transcript-md db-info session-id {:mode :full}))
  ([db-info session-id opts]
   (if-let [data (transcript db-info session-id)]
     (transcript->md data opts)
     (str "Session not found: " session-id "\n"))))

(defn session-summary-md
  "Canonical Markdown session-summary block (title heading + grouped
   facts) for a session - the SAME summary the HTML card renders. DB
   lookup + render; returns \"\" (no throw) on a missing id so shell
   pipelines and headers degrade cleanly."
  [db-info session-id]
  (if-let [data (transcript db-info session-id)]
    (render-summary-md data)
    ""))

;; =============================================================================
;; HTML renderer. Renders the Markdown transcript to a STANDALONE HTML
;; document styled with the vis-light theme's shared web CSS variables
;; (`vis/web-css-root`), so an exported transcript reads the same colors
;; as the web TUI. Pure transformation over the Markdown surface - no DB
;; calls, no side effects. Every channel (web, TUI, CLI) exports through
;; here so the output is byte-identical across surfaces.
;; =============================================================================

(defn- html-escape
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn- render-inline
  "Escape HTML then apply the constrained inline Markdown the transcript
   emits: `code`, **bold**, _italic_. Code spans are pulled out first so
   their contents aren't re-interpreted as bold/italic or double-escaped."
  [s]
  (let
    [codes
     (atom [])

     with-holes
     (str/replace (str s)
                  #"`([^`]*)`"
                  (fn [[_ inner]]
                    (let [idx (count @codes)]
                      (swap! codes conj inner)
                      (str "\u0000" idx "\u0000"))))

     escaped
     (html-escape with-holes)

     bolded
     (str/replace escaped #"\*\*([^*]+)\*\*" "<strong>$1</strong>")

     italic
     (str/replace bolded #"(?<!\w)_([^_]+)_(?!\w)" "<em>$1</em>")]

    (str/replace italic
                 #"\u0000(\d+)\u0000"
                 (fn [[_ idx]]
                   (str "<code>" (html-escape (nth @codes (Long/parseLong idx))) "</code>")))))

(def ^:private ^Parser md-parser (.build (Parser/builder)))

(def ^:private ^HtmlRenderer md-renderer
  (-> (HtmlRenderer/builder)
      ;; Transcript HTML is a standalone local artifact assembled from model,
      ;; tool, and user text. Keep CommonMark's fence/list/header behavior, but
      ;; do not let raw Markdown HTML pass through unescaped.
      (.escapeHtml true)
      (.build)))

(defn- md->html
  "Render the Markdown emitted by the transcript renderer with commonmark-java.
   This keeps fenced code block semantics (including backtick fences longer than
   three ticks) aligned with the Markdown serializer instead of maintaining a
   second, regex-only parser here."
  [md]
  (.render md-renderer (.parse md-parser (str md))))

(def ^:private transcript-html-styles
  "Standalone stylesheet layered AFTER `web-css-root` so it consumes the
   theme's shared CSS vars. Maps the transcript's block structure onto the
   vis palette - ground, ink, accent headers, code panels, hairline rules."
  (str
    "body{margin:0;background:var(--bg);color:var(--fg);"
    "font:15px/1.65 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;}"
    ".transcript{max-width:860px;margin:0 auto;padding:2.5rem 1.5rem 6rem;}"
    "@media (max-width:640px){.transcript{padding:1.25rem .9rem 3.5rem;}}"
    "h1{font-size:1.9rem;margin:.2em 0 .6em;}"
    "h2{font-size:1.4rem;margin:2.2rem 0 .9rem;padding-bottom:.35rem;border-bottom:1px solid var(--line);}"
    "h3{font-size:1.18rem;margin:1.8rem 0 .6rem;color:var(--primary);}"
    "h4{font-size:1.02rem;margin:1.3rem 0 .5rem;color:var(--secondary);}"
    "h5{font-size:.9rem;margin:1.1rem 0 .4rem;color:var(--dim);"
    "font-family:ui-monospace,SFMono-Regular,Menlo,monospace;text-transform:none;}"
    "h6{font-size:.85rem;margin:1rem 0 .4rem;color:var(--dim);}" "a{color:var(--primary);}"
    "p{margin:.6em 0;}" "ul{margin:.4em 0;padding-left:1.4em;}"
    "li{margin:.2em 0;}" "strong{color:var(--fg);font-weight:650;}"
    "em{color:var(--dim);font-style:normal;}"
    "code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.88em;"
    "background:var(--code-bg);padding:.1em .35em;border-radius:4px;}"
    "pre{background:var(--code-bg);border:1px solid var(--line);border-radius:8px;"
    "padding:1rem 1.1rem;overflow-x:auto;margin:.85em 0;}"
    "pre code{background:none;padding:0;font-size:.85em;line-height:1.5;}"
    ;; Summary card - full-viewport responsive grid of stat cards (one per
    ;; session-summary group), breaking out of the 860px transcript column.
    ".tx-summary{position:relative;left:50%;transform:translateX(-50%);"
    "width:min(1360px,94vw);display:grid;gap:.9rem;align-items:start;margin:.4rem 0 2.2rem;"
    "grid-template-columns:repeat(auto-fit,minmax(16rem,1fr));font-size:.92rem;}"
    ".tx-card{border:1px solid var(--line);border-radius:10px;background:var(--code-bg);"
    "padding:.4rem 1rem .9rem;}"
    ".tx-card-title{color:var(--dim);font-family:ui-monospace,SFMono-Regular,Menlo,monospace;"
    "font-size:.78rem;text-transform:uppercase;letter-spacing:.08em;padding:.7rem 0 .1rem;}"
    ".tx-row{display:flex;justify-content:space-between;gap:1rem;align-items:baseline;"
    "padding:.4rem 0;border-top:1px solid var(--line);}" ".tx-card-title + .tx-row{border-top:0;}"
    ".tx-k{color:var(--dim);white-space:nowrap;flex:0 0 auto;}"
    ".tx-v{color:var(--fg);text-align:right;word-break:break-word;min-width:0;}"
    ".tx-v.tx-mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;" "font-size:.88em;}"
    "@media (max-width:640px){.tx-summary{width:100%;left:0;transform:none;}"
    ".tx-card{padding:.35rem .85rem .8rem;}}"))

(def ^:private prism-token-css
  "The vis-light Prism token theme. Uses the SAME `web-css-root` CSS vars the
   web /ui does, so exported code highlights IDENTICALLY. Tiny, stable 9-rule
   block kept in sync by hand."
  (str
    ".token.comment,.token.prolog,.token.doctype,.token.cdata{color:var(--dim);font-style:italic}"
    ".token.punctuation{color:var(--dim)}"
    ".token.keyword,.token.boolean,.token.important{color:var(--primary);font-weight:600}"
    ".token.string,.token.char,.token.attr-value,.token.triple-quoted-string{color:var(--ok)}"
    ".token.number,.token.constant,.token.symbol{color:var(--secondary)}"
    ".token.function,.token.class-name,.token.decorator{color:var(--warning)}"
    ".token.builtin,.token.attr-name,.token.property{color:var(--accent)}"
    ".token.operator,.token.entity,.token.url{color:var(--fg)}"
    ".token.variable,.token.regex{color:var(--err)}"))

(def ^:private prism-js
  "The vendored Prism highlighter (`resources/vis-transcript/prism.min.js`),
   read once and INLINED into HTML exports so a standalone file syntax-
   highlights code blocks with zero network fetch. nil when that resource
   isn't on the classpath (export still renders, just without highlighting)."
  (delay (some-> (io/resource "vis-transcript/prism.min.js")
                 slurp)))

(defn- render-summary-html
  "Standalone HTML summary card: a title `<h1>` plus a full-viewport responsive
   grid of stat cards - one per canonical `session-summary` group (Session /
   Timing / Activity / Providers & models / Cost & tokens)."
  [data]
  (let
    [title (or (some-> data
                       :session
                       :title)
               "vis transcript")]
    (str "<h1>"
         (render-inline title)
         "</h1>\n"
         "<div class=\"tx-summary\">\n"
         (apply str
           (for [[label rows] (session-summary data)]
             (str "<div class=\"tx-card\"><div class=\"tx-card-title\">"
                  (html-escape label)
                  "</div>\n"
                  (apply str
                    (for [[k v mono?] rows]
                      (str "<div class=\"tx-row\"><span class=\"tx-k\">"
                           (html-escape k)
                           "</span><span class=\"tx-v"
                           (when mono? " tx-mono")
                           "\">"
                           (render-inline v)
                           "</span></div>\n")))
                  "</div>\n")))
         "</div>\n")))

(defn transcript->html
  "Render transcript data as a STANDALONE HTML document, styled with the
   vis-light theme's shared web CSS variables so an exported transcript
   matches the web TUI's colors. Pure transformation over `transcript`'s
   data shape (via the Markdown surface). Returns a string.

   Opts:
   - `:mode`     - `:full` (default) or `:dialog`, forwarded to `transcript->md`.
   - `:theme-id` - theme id for the embedded CSS (default `:vis-light`)."
  ([data] (transcript->html data {:mode :full}))
  ([data {:keys [mode theme-id] :or {mode :full theme-id :vis-light}}]
   (let
     [title
      (or (some-> data
                  :session
                  :title)
          "vis transcript")

      dialog?
      (= mode :dialog)

      body
      (if dialog?
        (md->html (transcript->md data {:mode :dialog}))
        (str (render-summary-html data) (md->html (render-turns-md data))))]

     (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
          "<meta charset=\"utf-8\">\n"
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
          "<title>"
          (html-escape title)
          "</title>\n"
          "<style>\n"
          (vis/web-css-root theme-id)
          "\n"
          transcript-html-styles
          "\n"
          prism-token-css
          "\n"
          "</style>\n</head>\n<body>\n<main class=\"transcript\">\n"
          body
          "\n</main>\n"
          ;; Inline the vendored Prism highlighter so the standalone file
          ;; syntax-highlights `<code class=\"language-*\">` on open — same
          ;; highlighter + token theme as the web /ui, no network fetch.
          (when-let [js @prism-js]
            (str "<script>" js
                 "</script>\n"
                 "<script>window.Prism&&Prism.highlightAll&&Prism.highlightAll();</script>\n"))
          "</body>\n</html>\n"))))


