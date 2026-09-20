(ns com.blockether.vis.contract.view
  "View vocabulary and JSON Schema validation."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(set! *warn-on-reflection* true)

(def ^:private schema (document/schema-document "view"))

(defn- definition [name] (get-in schema ["$defs" name]))

(defn- variants
  [name discriminator]
  (into {}
        (map (fn [shape]
               [(get-in shape ["properties" discriminator "const"]) shape]))
        (get (definition name) "oneOf")))

(defn- property-keys [shape] (set (keys (wire/->engine (get shape "properties")))))

(defn- schema-keys [name] (property-keys (definition name)))

(defn- read-only-keys
  [shape]
  (property-keys {"properties" (into {}
                                     (filter (fn [[_ value]]
                                               (get value "readOnly")))
                                     (get shape "properties"))}))

(defn- keyword-map [names] (into {} (map (juxt identity keyword)) names))

(defn- keyword-set [names] (set (map keyword names)))

(defn- enum-values [name] (get (definition name) "enum"))

(def ^:private fields (variants "field" "type"))

(def ^:private live-nodes (variants "live_node" "type"))

(def ^:private operators (variants "operator_action" "action"))

(def ^:private patches (variants "live_op" "op"))

(def view-kinds
  "Wire View name to lifecycle kind."
  (keyword-map (keys (variants "open_event" "kind"))))

(def view-actions "Wire operator action to internal action." (keyword-map (keys operators)))

(def field-types "Wire answer-field name to internal field type." (keyword-map (keys fields)))

(def text-types
  "Answer field types carrying text, excluding fixed-length one-time codes."
  (keyword-set (for [[type shape]
                     fields

                     :when (and (contains? (get shape "properties") "min_length")
                                (not (some #{"min_length"} (get shape "required"))))]

                 type)))

(def choice-types
  "Answer field types carrying choices."
  (keyword-set (for [[type shape]
                     fields

                     :when (contains? (get shape "properties") "options")]

                 type)))

(def secret-types
  "Answer field types replaced by vault handles."
  (keyword-set (for [[type shape]
                     fields

                     :when (get-in shape ["properties" "is_secret" "const"])]

                 type)))

(def decor-types
  "Wire decoration name to internal decoration type."
  (keyword-map (enum-values "decor_type")))

(def group-type-name
  "Wire name of a layout group."
  (get-in (definition "group") ["properties" "type" "const"]))

(def group-type "Internal type of a layout group." (keyword group-type-name))

(def group-directions
  "Wire layout direction to internal direction."
  (keyword-map (enum-values "group_direction")))

(def otp-defaults
  "Default and maximum one-time-code lengths."
  {:length (get-in fields ["otp" "properties" "max_length" "default"])
   :ceiling (get-in fields ["otp" "properties" "max_length" "maximum"])})

(def range-defaults
  "Default numeric range."
  (into {}
        (map (fn [key]
               [(keyword key) (get-in fields ["range" "properties" key "default"])]))
        ["min" "max" "step"]))

(def secret-handle-prefix
  "Prefix of an opaque secret answer handle."
  (subs (get (definition "secret_handle") "pattern") 1))

(def live-node-types
  "Wire semantic live-node name to internal node type."
  (keyword-map (remove #{group-type-name} (keys live-nodes))))

(def link-targets
  "Wire link target name to internal target type."
  (keyword-map (enum-values "link_target")))

(def live-ops "Wire live patch operation to internal operation." (keyword-map (keys patches)))

(def live-tones "Wire live tone to internal tone." (keyword-map (enum-values "tone")))

(def live-orders "Wire table order to internal order." (keyword-map (enum-values "table_order")))

(def live-aligns
  "Wire table alignment to internal alignment."
  (keyword-map (enum-values "alignment")))

(def live-sort-dirs
  "Wire sort direction to internal direction."
  (keyword-map (enum-values "sort_direction")))

(def live-reasons
  "Wire settlement reason to internal reason."
  (keyword-map (enum-values "settlement_reason")))

(def log-defaults
  "Live log paint-window and patch bounds."
  {:window-lines (get-in live-nodes ["log" "properties" "window_lines" "default"])
   :window-lines-cap (get-in live-nodes ["log" "properties" "window_lines" "maximum"])
   :max-patch-lines (get-in patches ["append" "properties" "lines" "maxItems"])})

(def table-defaults
  "Live table collection and patch bounds."
  {:max-rows (get-in live-nodes ["table" "properties" "max_rows" "maximum"])
   :max-patch-rows (get-in patches ["append" "properties" "rows" "maxItems"])
   :max-groups (get-in live-nodes ["table" "properties" "groups" "maxItems"])})

(def stat-defaults
  "Live stat collection bound."
  {:max-stats (get-in live-nodes ["stat" "properties" "stats" "maxItems"])})

(def step-defaults
  "Live step collection bound."
  {:max-steps (get-in live-nodes ["steps" "properties" "steps" "maxItems"])})

(def link-defaults
  "Live link collection bound."
  {:max-links (get-in live-nodes ["link" "properties" "links" "maxItems"])})

(def view-defaults
  "Live node collection bound."
  {:max-nodes (get (definition "live_view") "x-vis-max-nodes")})

(def spinner-frames
  "Ten 100ms text frames per spinner variant, shared with Companion."
  (into {}
        (map (fn [variant]
               [(keyword (get variant "const")) (get variant "x-vis-frames")]))
        (get (definition "spinner_variant") "oneOf")))

(def spinner-variants
  "Wire spinner variant to internal keyword."
  (keyword-map (map name (keys spinner-frames))))

;; The executable View shapes

(def view-action-key-sets
  "Every key one operator action may carry, selected by `:action`. Keeping these
   maps closed prevents a misspelled value or selection address from disappearing."
  (into {}
        (map (fn [[action shape]]
               [(keyword action) (property-keys shape)]))
        operators))

(def ^:private decor-node-types (set (vals decor-types)))

(defn decoration?
  "True when this normalized node is a [[decor-types]] decoration — ink on the
   form rather than a question. Every surface asks this before it looks for a
   value, and the answer contract never sees such a node at all."
  [{:keys [type]}]
  (contains? decor-node-types type))

(defn secret-handle?
  "True when `value` is an opaque handle minted for a `secret-types` field."
  [value]
  (and (string? value) (str/starts-with? value secret-handle-prefix)))

(def note-chars
  "The most characters a human's stop note carries. Stopping a view is ALWAYS
   allowed, so a longer comment is cut to this rather than turned away: a refusal
   would leave the human watching work they already told to stop."
  (get-in (definition "live_result") ["properties" "note" "maxLength"]))

(def item-bounds
  "The keyed collection each live node type holds: which key carries it, and how
   many items it may hold before a patch is REFUSED. One table, so the
   materializer, the refusal message and the contract document all read it."
  {:stat {:key :stats :max (:max-stats stat-defaults)}
   :steps {:key :steps :max (:max-steps step-defaults)}
   :table {:key :rows :max (:max-rows table-defaults)}
   :link {:key :links :max (:max-links link-defaults)}})

;; The live keys — closed in both directions, exactly like the form's

(def live-view-stamp-keys
  "Keys the ENGINE stamps on a live view, never written in a spec: its own
   identity, owning Activity invocation, arrival time, and patch counter."
  (read-only-keys (definition "live_view")))

(def live-column-keys "Every key one declared table column may carry." (schema-keys "table_column"))

(def live-table-group-keys
  "Every key one declared table group may carry: the id rows point at, the label
   a surface paints instead of that id, a tone for the head, where the group
   sorts, and whether it starts open."
  (schema-keys "table_group"))

(def live-row-keys "Every key one table row may carry." (schema-keys "row"))

(def live-stat-keys "Every key one stat may carry." (schema-keys "stat"))

(def live-step-keys "Every key one step may carry." (schema-keys "step"))

(def live-link-keys "Every key one link may carry." (schema-keys "link"))

(def live-sorted-keys
  "Every key a `{:by …}` table order may carry."
  (property-keys (get-in live-nodes ["table" "properties" "order" "oneOf" 1])))

(def live-group-keys
  "Every key a live layout group may carry. Shares the row/column layout
   vocabulary of [[group-type]], not the complete input-group schema: live
   groups have ids and optional collapse state, while input groups describe
   answer fields. Nested groups use their own available width in each client.

   No `:name`: a live group holds no answer, so there is nothing to key."
  (property-keys (get live-nodes group-type-name)))

(def live-node-keys
  "Allowed keys for a live node."
  (reduce into #{} (map property-keys (vals (dissoc live-nodes group-type-name)))))

(def live-view-keys
  "Every key a live view may carry, engine stamps included."
  (schema-keys "live_view"))

(def live-picture-keys "Keys returned in a finished live-view picture." (schema-keys "view"))

(def live-elided-keys "Keys in an elision record." (schema-keys "elided"))

(def live-op-key-sets
  "Allowed keys for each patch operation."
  (into {}
        (map (fn [[op shape]]
               [(keyword op) (property-keys shape)]))
        patches))

(def live-op-keys
  "Every key any patch operation may carry — the union the parser derives its
   snake_case spellings from."
  (reduce into #{} (vals live-op-key-sets)))

(def live-patch-keys "Every key one patch carries." (schema-keys "live_patch"))

(def live-result-keys "Keys in the result returned to the model." (schema-keys "live_result"))

(def live-selection-snapshot-bytes
  "Maximum serialized archive-only selection pictures in one live record trailer."
  1000000)

(def live-artifact-media-type
  "Media type for the append-only live-view record."
  "application/vnd.vis.live+ndjson")

(def live-artifact-inline-bytes
  "Byte size under which a settled view ALSO travels inline, on top of the file it
   already points at (256 KiB — the same floor
   `attachment-storage/default-offload-floor-bytes` uses to decide that a small
   payload never earns an external round-trip; stated here because the View
   contract may not drag the imaging stack in to read it).

   A view this small survives a session sync to another machine; a build log does
   not, and must not — holding one in memory as base64 is the cost this whole
   phase exists to remove."
  (* 256 1024))

(def live-artifact-keys
  "Every key a settled view carries as an ARTIFACT: what it WAS (`:view-id`,
   `:session-id`, `:title`), how it ENDED (`:ended-at`, `:reason`), what a surface
   opens instantly (`:view`, the final materialized state) and where the bytes are
   (`:storage-uri`, `:size`, `:line-count`, and `:base64` only under
   [[live-artifact-inline-bytes]])."
  (schema-keys "live_artifact"))

;; Normalized and author-written field keys come from the executable schema.

(def derived-keys
  "Keys the ENGINE stamps on a normalized node, never written in a spec.
   `:is-secret` follows from the type, so a caller offering it is refused."
  (read-only-keys (get fields "checkbox")))

(def request-stamp-keys
  "Keys the ENGINE stamps on a pending REQUEST, so every channel sees them on the
   projected view although no spec may write one: the registry's arrival time.
   Same category as [[derived-keys]] one level up — `request-keys` deliberately
   refuses them on the way in, so a reader rebuilding a view that crossed a
   process boundary lifts them across instead of re-parsing them."
  (read-only-keys (definition "input_view")))

(def value-keys
  "Every key an answerable field may carry, whatever its type."
  (property-keys (get fields "checkbox")))

(def text-keys "Every key a typed field may carry." (property-keys (get fields "plaintext")))

(def choice-keys
  "Every key a field answered from `:options` may carry."
  (property-keys (get fields "select")))

(def range-keys
  "Every key a field answered on a track may carry."
  (property-keys (get fields "range")))

(def field-keys
  "Every key a field spec may be WRITTEN with: the union of the per-type sets,
   less what the engine derives. The parser accepts exactly this vocabulary in
   its snake_case spelling, so there is no second table of keys to keep in step."
  (apply disj (into text-keys (concat choice-keys range-keys)) derived-keys))

(def group-keys
  "Every key a layout group may carry. A node that holds no answer has no key
   that describes one."
  (schema-keys "group"))

(def layout-keys
  "The keys only a group has. A field carrying one meant to group and forgot to
   say so, which is worth its own message rather than an unknown-key refusal."
  (apply disj group-keys field-keys))

(def decor-keys
  "Every key a decoration may carry: its own type and the words it paints. A node
   nobody can answer has nothing else to say."
  (schema-keys "decor"))

(def option-keys "Every key one `:options` entry may carry." (schema-keys "option"))

(def request-keys "Every key a request may carry." (schema-keys "request"))

;; Semantic constraints that JSON Schema cannot express.

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- schema-error
  [definition value]
  (some-> (document/explain "view" definition value)
          pr-str
          (str/replace "_" "-")))

(defn- one-identity? [{:keys [id name]}] (= id name))

(defn- secret-marked? [{:keys [type is-secret]}] (= is-secret (contains? secret-types type)))

(defn- ordered-lengths?
  [{:keys [min-length max-length]}]
  (or (nil? min-length) (nil? max-length) (<= (long min-length) (long max-length))))

(defn- otp-fits-boxes? [{:keys [max-length]}] (<= (long max-length) (long (:ceiling otp-defaults))))

(defn- ascending-bounds? [{:keys [min max]}] (< (double min) (double max)))

(defn- positive-step?
  [{:keys [min max step]}]
  (and (pos? (double step)) (<= (double step) (- (double max) (double min)))))

(defn- option-values [field] (set (map :value (:options field))))

(defn- default-in-domain?
  [{:keys [type default] :as field}]
  (let [{lo :min hi :max :keys [min-length max-length]}
        field

        chosen
        (option-values field)]

    (cond (not (contains? field :default)) true
          (nil? default) false
          (contains? text-types type) (string? default)
          (= :select type) (contains? chosen default)
          (= :multiselect type)
          (and (vector? default) (every? chosen default) (= (count default) (count (set default))))
          (= :checkbox type) (boolean? default)
          (= :range type) (and (number? default) (<= (double lo) (double default) (double hi)))
          (= :otp type) (and (string? default)
                             (some? (re-matches #"\d+" default))
                             (<= (long min-length) (count default) (long max-length)))
          :else false)))

(defn field-error
  "nil when `field` is a valid normalized answer field, otherwise a reason."
  [field]
  (or (when-not (every? keyword? (keys field)) "field keys must be normalized keywords")
      (schema-error "field" field)
      (when-not (one-identity? field) "id and name must match")
      (when-not (secret-marked? field) "is-secret must follow type")
      (when (and (contains? field :validate) (not-every? ifn? (:validate field)))
        "validate must contain callables")
      (when-not (ordered-lengths? field) "min-length must not exceed max-length")
      (when (and (= :otp (:type field)) (not (otp-fits-boxes? field)))
        "OTP length exceeds its ceiling")
      (when (and (= :range (:type field)) (not (ascending-bounds? field)))
        "range min must be below max")
      (when (and (= :range (:type field)) (not (positive-step? field)))
        "range step must be positive and fit its bounds")
      (when-not (default-in-domain? field) "default is outside the field domain")))

(defn group-error
  "nil when `group` is a valid normalized layout group, otherwise a reason."
  [group]
  (or (schema-error "group" group) (when-not (one-identity? group) "id and name must match")))

(defn decor-error
  "nil when `decor` is a valid normalized decoration, otherwise a reason."
  [decor]
  (schema-error "decor" decor))

(defn- field-names
  [fields]
  (mapcat (fn [node]
            (cond-> (field-names (:fields node))
              (:name node)
              (conj (:name node))))
          fields))

(defn- distinct-names?
  [{:keys [fields]}]
  (let [names (field-names fields)]
    (= (count names) (count (set names)))))

(defn request-error
  "nil when `request` is a valid normalized request, otherwise a reason."
  [request]
  (or (schema-error "request" request)
      (when-not (distinct-names? request) "field names must be unique")))

(defn- typed-in-domain?
  [{:keys [is-required min-length max-length]} value]
  (cond (nil? value) (not is-required)
        (not (string? value)) false
        (str/blank? value) (not is-required)
        :else (and (or (nil? min-length) (<= (long min-length) (count value)))
                   (or (nil? max-length) (<= (count value) (long max-length))))))

(defn- secret-in-domain?
  [{:keys [is-required]} value]
  (if (nil? value) (not is-required) (secret-handle? value)))

(defn- selected-in-domain?
  [{:keys [is-required] :as field} value]
  (if (nil? value) (not is-required) (and (string? value) (contains? (option-values field) value))))

(defn- picked-in-domain?
  [{:keys [is-required] :as field} value]
  (and (vector? value)
       (every? string? value)
       (every? (option-values field) value)
       (= (count value) (count (set value)))
       (or (seq value) (not is-required))))

(defn- ticked-in-domain?
  [{:keys [is-required]} value]
  (and (boolean? value) (or (true? value) (not is-required))))

(defn- slid-in-domain?
  [{lo :min hi :max} value]
  (and (number? value) (<= (double lo) (double value) (double hi))))

(defn- answer-value-valid?
  [{:keys [type] :as field} value]
  (case type
    :plaintext
    (typed-in-domain? field value)

    :multiline
    (typed-in-domain? field value)

    :password
    (secret-in-domain? field value)

    :select
    (selected-in-domain? field value)

    :multiselect
    (picked-in-domain? field value)

    :checkbox
    (ticked-in-domain? field value)

    :range
    (slid-in-domain? field value)

    :otp
    (secret-in-domain? field value)

    false))

(defn- answerable
  [fields]
  (mapcat (fn [{:keys [type] :as node}]
            (cond (= group-type type) (answerable (:fields node))
                  (decoration? node) []
                  :else [node]))
          fields))

(defn- values-error
  [fields values]
  (let [by-name
        (into {} (map (juxt :name identity)) (answerable fields))

        answered
        (set (keys values))]

    (or (when-let [extra (seq (sort (remove by-name answered)))]
          (str "answers no such field: " (str/join ", " extra)))
        (when-let [missing (seq (sort (remove answered (keys by-name))))]
          (str "leaves unanswered: " (str/join ", " missing)))
        (some (fn [[field-name value]]
                (when-not (answer-value-valid? (get by-name field-name) value)
                  (str field-name ": value is outside the field domain")))
              values))))

(defn answer-error
  "nil when `answer` is valid for `fields`, otherwise a reason."
  [fields answer]
  (or (schema-error "answer" answer)
      (when-not (string? (:reason answer)) "answer reason must be a string")
      (when-not (if (:is-submitted answer) (map? (:values answer)) (not (contains? answer :values)))
        "values must be present only for a submitted answer")
      (when (and (seq fields) (:is-submitted answer)) (values-error fields (:values answer)))))

(defn- sorted-order?
  [x]
  (and (map? x)
       (non-blank-string? (:by x))
       (or (nil? (:dir x)) (contains? (set (vals live-sort-dirs)) (:dir x)))))

(defn- ordered-by-declared-column?
  [{:keys [type columns order]}]
  (or (not= :table type)
      (not (map? order))
      (and (sorted-order? order) (contains? (set (map :id columns)) (:by order)))))

(defn- selection-belongs-to-table?
  [{:keys [type rows is-selectable selected-ids] :as node}]
  (if (= :table type)
    (and (or (not (contains? node :selected-ids)) (true? is-selectable))
         (every? (set (map :id rows)) selected-ids))
    (and (not (contains? node :is-selectable)) (not (contains? node :selected-ids)))))

(defn live-node-error
  "nil when `node` is a valid normalized live node, otherwise a reason."
  [node]
  (or (schema-error "live_node" node)
      (when-not (ordered-by-declared-column? node) "table order must name a declared column")
      (when-not (selection-belongs-to-table? node) "selection must belong to a selectable table")))

(defn- live-tree
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cons node (live-tree (:fields node)))))
        nodes))

(defn live-view-error
  "nil when `view` is a valid normalized live view, otherwise a reason."
  [view]
  (or (schema-error "live_view" view)
      (let [nodes
            (live-tree (:nodes view))

            ids
            (map :id nodes)]

        (or (some live-node-error nodes)
            (cond (> (count nodes) (long (:max-nodes view-defaults))) "live view has too many nodes"
                  (not= (count ids) (count (set ids))) "live node ids must be unique")))))

(defn live-patch-error
  "nil when `patch` has a valid live patch shape, otherwise a reason."
  [patch]
  (schema-error "live_patch" patch))

(defn live-result-error
  "nil when `result` is a valid live result, otherwise a reason."
  [result]
  (or (schema-error "live_result" result)
      (when-not (= (:is-completed result) (= :completed (:reason result)))
        "completion flag and reason disagree")
      (when (and (contains? result :note) (not (:is-from-human result)))
        "only a human interruption may carry a note")
      (when (and (:is-from-human result) (not= :interrupted (:reason result)))
        "a human stop must be interrupted")))

(defn live-artifact-error
  "nil when `artifact` is a valid settled live artifact, otherwise a reason."
  [artifact]
  (or (schema-error "live_artifact" artifact)
      (when (and (contains? artifact :base64)
                 (> (long (:size artifact)) (long live-artifact-inline-bytes)))
        "only a small artifact may be inlined")))
