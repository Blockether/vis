(ns com.blockether.vis.contract.view
  "View vocabulary and JSON Schema validation."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(set! *warn-on-reflection* true)

(def ^:private source (delay (document/load! "view")))
(def ^:private document (delay (wire/->engine @source)))

(defn- keyword-map [names] (into {} (map (juxt identity keyword)) names))
(defn- keyword-set [names] (set (map keyword names)))


(def version "View contract document version." (:version @document))
(def view-kinds "Wire View name to lifecycle kind." (keyword-map (:kinds @document)))
(def view-actions "Wire operator action to internal action." (keyword-map (:actions @document)))
(def field-types
  "Wire answer-field name to internal field type."
  (keyword-map (:field-types @document)))
(def text-types "Answer field types carrying text." (keyword-set (:text-types @document)))
(def choice-types "Answer field types carrying choices." (keyword-set (:choice-types @document)))
(def secret-types
  "Answer field types replaced by vault handles."
  (keyword-set (:secret-types @document)))
(def decor-types
  "Wire decoration name to internal decoration type."
  (keyword-map (:decor-types @document)))
(def group-type-name "Wire name of a layout group." (:group-type @document))
(def group-type "Internal type of a layout group." (keyword group-type-name))
(def group-directions
  "Wire layout direction to internal direction."
  (keyword-map (:group-directions @document)))
(def otp-defaults "Default and maximum one-time-code lengths." (:otp @document))
(def range-defaults "Default numeric range." (:range @document))
(def secret-handle-prefix
  "Prefix of an opaque secret answer handle."
  (:secret-handle-prefix @document))
(def live-node-types
  "Wire semantic live-node name to internal node type."
  (keyword-map (get-in @document [:live :node-types])))
(def link-targets
  "Wire link target name to internal target type."
  (keyword-map (get-in @document [:live :link-targets])))
(def live-ops
  "Wire live patch operation to internal operation."
  (keyword-map (get-in @document [:live :ops])))
(def live-tones "Wire live tone to internal tone." (keyword-map (get-in @document [:live :tones])))
(def live-orders
  "Wire table order to internal order."
  (keyword-map (get-in @document [:live :orders])))
(def live-aligns
  "Wire table alignment to internal alignment."
  (keyword-map (get-in @document [:live :aligns])))
(def live-sort-dirs
  "Wire sort direction to internal direction."
  (keyword-map (get-in @document [:live :sort-dirs])))
(def live-reasons
  "Wire settlement reason to internal reason."
  (keyword-map (get-in @document [:live :reasons])))
(def log-defaults "Live log paint-window and patch bounds." (get-in @document [:live :log]))
(def table-defaults "Live table collection and patch bounds." (get-in @document [:live :table]))
(def stat-defaults "Live stat collection bound." {:max-stats (get-in @document [:live :max-stats])})
(def step-defaults "Live step collection bound." {:max-steps (get-in @document [:live :max-steps])})
(def link-defaults "Live link collection bound." {:max-links (get-in @document [:live :max-links])})
(def view-defaults "Live node collection bound." {:max-nodes (get-in @document [:live :max-nodes])})

(def vocabulary
  "Portable keyword-keyed View vocabulary from the JSON contract."
  {:view-kinds (:kinds @document)
   :view-actions (:actions @document)
   :field-types (:field-types @document)
   :text-types (:text-types @document)
   :choice-types (:choice-types @document)
   :secret-types (:secret-types @document)
   :decor-types (:decor-types @document)
   :group-type (:group-type @document)
   :group-directions (:group-directions @document)
   :otp (:otp @document)
   :range (:range @document)
   :secret-handle-prefix (:secret-handle-prefix @document)
   :live (:live @document)})


;; The executable View shapes

(def view-action-key-sets
  "Every key one operator action may carry, selected by `:action`. Keeping these
   maps closed prevents a misspelled value or selection address from disappearing."
  {:submit #{:action :values}
   :cancel #{:action}
   :select #{:action :node-id :item-ids}
   :interrupt #{:action :note}})

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
  500)

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
   identity, its arrival time, and the patch counter every surface orders by."
  #{:id :seq :created-at})

(def live-column-keys "Every key one declared table column may carry." #{:id :label :align})
(def live-row-keys "Every key one table row may carry." #{:id :cells :tone :branch})
(def live-stat-keys "Every key one stat may carry." #{:id :label :value-text :tone})
(def live-step-keys "Every key one step may carry." #{:id :label :tone :detail :value})
(def live-link-keys "Every key one link may carry." #{:id :label :target-kind :target :tone})
(def live-sorted-keys "Every key a `{:by …}` table order may carry." #{:by :dir})

(def live-group-keys
  "Every key a live layout GROUP may carry — the request's own [[group-type]]
   node, reused verbatim rather than reinvented, so a `row` arranges the same way
   whether a human is answering it or watching it.

   No `:name`: a group of a VIEW holds no answer, so there is nothing to key."
  #{:id :type :label :direction :fields})

(def live-node-keys
  "Allowed keys for a live node."
  #{:id :type :label :text :detail :tone :value :done :total :stats :steps :lines :window-lines
    :columns :rows :max-rows :order :is-selectable :selected-ids :links :total-lines})

(def live-view-keys
  "Every key a live view may carry, engine stamps included."
  (into #{:title :description :source :session-id :channel-ids :nodes :timeout-ms}
        live-view-stamp-keys))

(def live-picture-keys
  "Keys returned in a finished live-view picture."
  #{:title :description :nodes})

(def live-elided-keys "Keys in an elision record." #{:node-id :items})
(def live-op-key-sets
  "Allowed keys for each patch operation."
  {:set #{:op :node-id :text :detail :tone :label :value :done :total :stats :steps :selected-ids
          :links}
   :append #{:op :node-id :lines :rows :stats :steps :links}
   :remove #{:op :node-id :item-ids}
   :clear #{:op :node-id}
   :add-node #{:op :node-spec :after}
   :remove-node #{:op :node-id}})

(def live-op-keys
  "Every key any patch operation may carry — the union the parser derives its
   snake_case spellings from."
  (reduce into #{} (vals live-op-key-sets)))

(def live-patch-keys "Every key one patch carries." #{:view-id :seq :ops})

(def live-result-keys
  "Keys in the result returned to the model."
  #{:view-id :is-completed :reason :is-from-human :note :view :elided :summary :artifact-id :error})

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
  #{:id :view-id :session-id :title :media-type :audience :ended-at :reason :view :storage-uri :size
    :line-count :base64})

;; The keys — one table, and the parser reads it too
;;
;; Every map declared below is CLOSED, so each shape's key set is written down
;; exactly once here, and the snake_case spelling a wire spec may use is derived
;; from these very sets by the parser's `wire-keys`. A key added here reaches
;; both layers with no second table to keep in step.

(def derived-keys
  "Keys the ENGINE stamps on a normalized node, never written in a spec.
   `:is-secret` follows from the type, so a caller offering it is refused."
  #{:is-secret})

(def request-stamp-keys
  "Keys the ENGINE stamps on a pending REQUEST, so every channel sees them on the
   projected view although no spec may write one: the registry's arrival time.
   Same category as [[derived-keys]] one level up — `request-keys` deliberately
   refuses them on the way in, so a reader rebuilding a view that crossed a
   process boundary lifts them across instead of re-parsing them."
  #{:created-at})

(def value-keys
  "Every key an answerable field may carry, whatever its type."
  #{:id :name :type :label :description :placeholder :is-required :is-secret :default :validate})

(def text-keys "Every key a typed field may carry." (into value-keys [:min-length :max-length]))
(def choice-keys "Every key a field answered from `:options` may carry." (conj value-keys :options))
(def range-keys
  "Every key a field answered on a track may carry."
  (into value-keys [:min :max :step]))

(def field-keys
  "Every key a field spec may be WRITTEN with: the union of the per-type sets,
   less what the engine derives. The parser accepts exactly this vocabulary in
   its snake_case spelling, so there is no second table of keys to keep in step."
  (apply disj (into text-keys (concat choice-keys range-keys)) derived-keys))

(def group-keys
  "Every key a layout group may carry. A node that holds no answer has no key
   that describes one."
  #{:id :name :type :label :description :direction :fields})

(def layout-keys
  "The keys only a group has. A field carrying one meant to group and forgot to
   say so, which is worth its own message rather than an unknown-key refusal."
  (apply disj group-keys field-keys))

(def decor-keys
  "Every key a decoration may carry: its own type and the words it paints. A node
   nobody can answer has nothing else to say."
  #{:type :text})

(def option-keys "Every key one `:options` entry may carry." #{:value :label})

(def request-keys
  "Every key a request may carry."
  #{:id :title :description :source :fields :submit-label :cancel-label :is-cancellable :timeout-ms
    :channel-ids :session-id})


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
