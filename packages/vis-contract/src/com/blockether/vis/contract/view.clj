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

(def ^:private components (variants "component" "type"))

(def ^:private component-index
  "Wire component name to its position in the `component` union."
  (into {}
        (map-indexed (fn [index shape]
                       [(get-in shape ["properties" "type" "const"]) index]))
        (get (definition "component") "oneOf")))

(def ^:private fields
  "The input components. Each one holds exactly one answer."
  (into {}
        (filter (fn [[_ shape]]
                  (contains? (get shape "properties") "is_required")))
        components))

(def ^:private live-nodes
  "The components that hold no answer: display, data, action and layout."
  (apply dissoc components (keys fields)))

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

(def group-type-name
  "Wire name of a layout group."
  (get-in components ["group" "properties" "type" "const"]))

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
  "Wire display, data and action component name to internal type."
  (keyword-map (remove #{group-type-name} (keys live-nodes))))

(def component-types
  "Wire component name to internal type, for every component in the catalog."
  (keyword-map (keys components)))

(def placements
  "Wire live-view placement to internal placement."
  (keyword-map (enum-values "placement")))

(def button-actions
  "Wire button action to internal action."
  (keyword-map (get-in live-nodes ["button" "properties" "action" "enum"])))

(def button-variants
  "Wire button variant to internal variant."
  (keyword-map (get-in live-nodes ["button" "properties" "variant" "enum"])))

(def command-icons
  "Wire names of the pictures that a diff toolbar command can show."
  (set (enum-values "command_icon")))

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

(def grid-columns
  "Bounds of a grid group's `max_columns`: `[minimum maximum]`."
  (let [prop (get-in live-nodes [group-type-name "properties" "max_columns"])]
    [(get prop "minimum") (get prop "maximum")]))

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

(def lane-defaults
  "Live timeline lane bound."
  {:max-lanes (get-in live-nodes ["timeline" "properties" "lanes" "maxItems"])})

(def link-defaults
  "Live link collection bound."
  {:max-links (get-in live-nodes ["link" "properties" "links" "maxItems"])})

(def scene-defaults
  "Defaults and bounds of a scene node and of one scene object."
  (let [scene
        (get-in live-nodes ["scene" "properties"])

        object
        (get (definition "scene_object") "properties")]

    {:background (get-in scene ["background" "default"])
     :fov (get-in scene ["fov" "default"])
     :is-wrapping (get-in scene ["is_wrapping" "default"])
     :tick-ms (get-in scene ["tick_ms" "default"])
     :max-objects (get-in scene ["objects" "maxItems"])
     :max-meshes (get-in scene ["meshes" "maxItems"])
     :object (into {}
                   (keep (fn [[k v]]
                           (when (contains? v "default")
                             [(keyword (str/replace k "_" "-")) (get v "default")])))
                   object)}))

(def scene-meshes
  "Built-in mesh name to `{:vertices :faces}`. Every surface draws these arrays, so
   one shape looks the same on each screen."
  (into {}
        (map (fn [mesh]
               [(get mesh "const")
                {:vertices (get mesh "x-vis-vertices") :faces (get mesh "x-vis-faces")}]))
        (get (definition "scene_builtin_mesh") "oneOf")))

(def scene-light
  "The fixed light of every scene, in camera space: `:ambient`, `:diffuse` and the unit `:direction`."
  (let [light (get-in live-nodes ["scene" "x-vis-light"])]
    {:ambient (get light "ambient")
     :diffuse (get light "diffuse")
     :direction (get light "direction")}))

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

(def ^:private display-node-types (set (vals live-node-types)))

(defn decoration?
  "True when this normalized node is a display, data or action component. It
   holds no answer, so every surface asks this before it looks for a value, and
   the answer contract never sees such a node."
  [{:keys [type]}]
  (contains? display-node-types type))

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
   :link {:key :links :max (:max-links link-defaults)}
   :timeline {:key :lanes :max (:max-lanes lane-defaults)}
   :scene {:key :objects :max (:max-objects scene-defaults)}})

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

(def live-lane-keys "Every key one timeline lane may carry." (schema-keys "lane"))

(def live-link-keys "Every key one link may carry." (schema-keys "link"))

(def live-scene-object-keys "Every key one scene object may carry." (schema-keys "scene_object"))

(def live-scene-mesh-keys "Every key one scene mesh may carry." (schema-keys "scene_mesh"))

(def live-sorted-keys
  "Every key a `{:by …}` table order may carry."
  (property-keys (get-in live-nodes ["table" "properties" "order" "oneOf" 1])))

(def live-group-keys
  "Every key a layout group may carry, in a form and in a live view. Nested
   groups use their own available width in each client.

   No `:name`: a group holds no answer, so there is nothing to key."
  (property-keys (get live-nodes group-type-name)))

(def live-node-keys
  "Allowed keys for a live node."
  (reduce into #{} (map property-keys (vals (dissoc live-nodes group-type-name)))))

(def live-node-type-keys
  "Wire live component name to the keys that its own schema variant allows."
  (into {}
        (map (fn [[type-name shape]]
               [type-name (property-keys shape)]))
        live-nodes))

(defn- required-defaults
  "The schema default of each required key of `shape`, by engine key."
  [shape]
  (into {}
        (keep (fn [k]
                (let [rule (get-in shape ["properties" k])]
                  (when (contains? rule "default") [(wire/engine-key k) (get rule "default")]))))
        (get shape "required")))

(def live-node-defaults
  "Wire live component name to the schema default of each required key. The schema is
   the one owner of these defaults: every host fills them from here."
  (into {}
        (map (fn [[type-name shape]]
               [type-name (required-defaults shape)]))
        live-nodes))

(def live-item-defaults
  "Collection key to the schema default of each required key of one item."
  {:steps (required-defaults (definition "step")) :lanes (required-defaults (definition "lane"))})

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
  live-group-keys)

(def layout-keys
  "The keys only a group has. A field carrying one meant to group and forgot to
   say so, which is worth its own message rather than an unknown-key refusal."
  (apply disj group-keys field-keys))

(def option-keys "Every key one `:options` entry may carry." (schema-keys "option"))

(def request-keys "Every key a request may carry." (schema-keys "request"))

;; Semantic constraints that JSON Schema cannot express.

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- schema-error
  [definition value]
  (document/explain-text (document/explain "view" definition value) value))

(defn- variant-lines
  "The schema fault lines of wire node `node` against `component` variant `index`."
  [index node]
  (document/explain-lines (document/explain "view" (str "component/oneOf/" index) node) node))

(defn- component-error
  "The schema error of `value` against the one `component` variant `type-name`."
  [type-name value]
  (if-let [index (get component-index type-name)]
    (schema-error (str "component/oneOf/" index) value)
    "unknown component type"))

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
      (component-error (some-> (:type field)
                               name)
                       field)
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
  (component-error group-type-name group))

(defn- field-names
  [fields]
  (mapcat (fn [node]
            (cond-> (field-names (:children node))
              (:name node)
              (conj (:name node))))
          fields))

(defn- distinct-names?
  [{:keys [nodes]}]
  (let [names (field-names nodes)]
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
            (cond (= group-type type) (answerable (:children node))
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

(defn value-in-domain?
  "True when `value` fits the domain of the input component `field`. A blank
   value always fits: a change is a draft, and only a submit checks requiredness."
  [field value]
  (answer-value-valid? (assoc field :is-required false) value))

(defn answer-error
  "nil when `answer` is valid for `fields`, otherwise a reason."
  [fields answer]
  (or (schema-error "answer" answer)
      (when-not (string? (:reason answer)) "answer reason must be a string")
      (when-not (if (:is-submitted answer) (map? (:values answer)) (not (contains? answer :values)))
        "values must be present only for a submitted answer")
      (when (and (seq fields) (:is-submitted answer)) (values-error fields (:values answer)))))

(defn scene-merge
  "Scene `objects` with `changes` merged in by id. A change keeps every key it
   leaves out, so a moved object sends only its new place. A new id starts from
   the defaults and keeps its slot after the objects before it."
  [objects changes]
  (let [fresh
        (:object scene-defaults)

        start
        (into {}
              (map-indexed (fn [i one]
                             [(:id one) i]))
              objects)]

    (first (reduce (fn [[acc idx] change]
                     (if-let [at (get idx (:id change))]
                       [(update acc at merge change) idx]
                       [(conj acc (merge fresh change)) (assoc idx (:id change) (count acc))]))
                   [(vec objects) start]
                   changes))))

(declare live-node-lines wire-data)

(defn live-node-error
  "nil when `node` is a valid normalized live node, otherwise a reason."
  [node]
  (let [lines (live-node-lines (wire-data node) false)]
    (when (seq lines) (document/joined-lines lines))))

;; Pictures: the bytes behind an image node

(def ^:private picture-rule (definition "picture"))

(defn- hex-bytes
  "The bytes that hex text `hex` spells."
  [^String hex]
  (mapv (fn [^long at]
          (Long/parseLong (subs hex at (+ at 2)) 16))
        (range 0 (count hex) 2)))

(def ^:private picture-magic
  "Each media type with its lists of `[offset bytes]`; a picture matches one list."
  (into {}
        (map (fn [[media-type alternatives]]
               [media-type
                (mapv (fn [alternative]
                        (mapv (fn [{offset "offset" hex "hex"}]
                                [offset (hex-bytes hex)])
                              alternative))
                      alternatives)]))
        (get picture-rule "x-vis-magic")))

(def picture-media-types "The media types that an image node shows." (set (keys picture-magic)))

(def picture-bounds
  "The most pixels in one picture, the most base64 characters of its data, and the
   most picture bytes in one live view."
  {:pixels (get picture-rule "x-vis-max-pixels")
   :data-chars (get-in picture-rule ["properties" "data" "maxLength"])
   :view-bytes (get (definition "live_view") "x-vis-max-picture-bytes")})

(defn- byte-at ^long [^bytes data ^long at] (bit-and 0xFF (long (aget data (int at)))))

(defn- unsigned
  "The unsigned number in `width` bytes of `data` at `at`; `big?` sets the byte order."
  ^long [^bytes data ^long at ^long width big?]
  (reduce (fn [^long acc ^long i]
            (bit-or (bit-shift-left acc 8) (byte-at data (+ at (if big? i (- width 1 i))))))
          0
          (range width)))

(defn- matches-magic?
  [^bytes data media-type]
  (some (fn [alternative]
          (every? (fn [[^long offset expected]]
                    (and (<= (+ offset (count expected)) (alength data))
                         (every? (fn [^long i]
                                   (= (long (nth expected i)) (byte-at data (+ offset i))))
                                 (range (count expected)))))
                  alternative))
        (get picture-magic media-type)))

(defn- ascii-at [^bytes data ^long at ^long width] (String. data (int at) (int width) "ISO-8859-1"))

(defn- webp-size
  [^bytes data]
  (case (ascii-at data 12 4)
    "VP8 "
    [(bit-and 0x3FFF (unsigned data 26 2 false)) (bit-and 0x3FFF (unsigned data 28 2 false))]

    "VP8L"
    (let [b0
          (byte-at data 21)

          b1
          (byte-at data 22)

          b2
          (byte-at data 23)

          b3
          (byte-at data 24)]

      [(inc (bit-or (bit-shift-left (bit-and b1 0x3F) 8) b0))
       (inc (bit-or (bit-shift-left (bit-and b3 0x0F) 10)
                    (bit-shift-left b2 2)
                    (bit-shift-right (bit-and b1 0xC0) 6)))])

    "VP8X"
    [(inc (unsigned data 24 3 false)) (inc (unsigned data 27 3 false))]

    nil))

(def ^:private jpeg-frames
  "The JPEG markers of a frame header, which holds the picture size."
  (disj (set (range 0xC0 0xD0)) 0xC4 0xC8 0xCC))

(defn- jpeg-size
  [^bytes data]
  (loop [at 2]
    (when (<= (+ at 4) (alength data))
      (let [marker (byte-at data (inc at))]
        (cond (not= 0xFF (byte-at data at)) nil
              (= 0xFF marker) (recur (inc at))
              (or (= 0x01 marker) (<= 0xD0 marker 0xD9)) (recur (+ at 2))
              (jpeg-frames marker) [(unsigned data (+ at 7) 2 true) (unsigned data (+ at 5) 2 true)]
              :else (recur (+ at 2 (unsigned data (+ at 2) 2 true))))))))

(defn picture-size
  "`[width height]` from the header of picture bytes `data` of `media-type`, or nil
   when the header does not say."
  [^bytes data media-type]
  (try (case media-type
         "image/png"
         (when (= "IHDR" (ascii-at data 12 4))
           [(unsigned data 16 4 true) (unsigned data 20 4 true)])

         "image/gif"
         [(unsigned data 6 2 false) (unsigned data 8 2 false)]

         "image/webp"
         (webp-size data)

         "image/jpeg"
         (jpeg-size data)

         nil)
       (catch IndexOutOfBoundsException _ nil)))

(defn read-picture
  "`{:width :height}` of picture `raw` (`:media-type`, base64 `:data`, optional
   `:width` and `:height`), or `{:error reason}`. The bytes must start like the
   media type, the header must give a size within the pixel bound, and a declared
   size must match the header. Data over the length bound is refused before it is
   decoded."
  [{:keys [media-type data width height]}]
  (let [too-long?
        (and (string? data) (> (count data) (long (:data-chars picture-bounds))))

        decoded
        (when (and (string? data) (not too-long?))
          (try (.decode (java.util.Base64/getDecoder) ^String data)
               (catch IllegalArgumentException _ nil)))

        [w h]
        (when (and decoded (matches-magic? decoded media-type)) (picture-size decoded media-type))]

    (cond (not (picture-media-types media-type))
          {:error (str "needs a media type: " (str/join ", " (sort picture-media-types)))}
          too-long? {:error (str "data is " (count data)
                                 " characters; the most is " (:data-chars picture-bounds))}
          (nil? decoded) {:error "data must be base64 text"}
          (not (matches-magic? decoded media-type)) {:error (str "data is not " media-type " data")}
          (not (and w h (pos? (long w)) (pos? (long h))))
          {:error "the picture header does not give its size"}
          (> (* (long w) (long h)) (long (:pixels picture-bounds)))
          {:error (str "the picture has " w "x" h " pixels; the most is " (:pixels picture-bounds))}
          (or (and width (not= (long width) (long w))) (and height (not= (long height) (long h))))
          {:error
           (str "the declared size " width "x" height " differs from the header size " w "x" h)}
          :else {:width w :height h})))

(defn picture-byte-count
  "How many bytes the base64 `data` of a picture holds."
  ^long [data]
  (if (string? data)
    (let [n
          (count data)

          pad
          (count (take-while #{\=} (reverse (subs data (max 0 (- n 2))))))]

      (- (quot (* 3 n) 4) pad))
    0))

(defn- view-picture-bytes
  [nodes]
  (transduce (comp (filter #(contains? #{"image" :image} (:type %)))
                   (map #(+ (picture-byte-count (:data %))
                            (picture-byte-count (get-in % [:before :data])))))
             +
             0
             nodes))

(defn- live-tree
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cons node (live-tree (:children node)))))
        nodes))

(defn picture-budget-error
  "nil when the pictures of live `view` fit its byte bound, otherwise the reason."
  [view]
  (let [total (long (view-picture-bytes (live-tree (:nodes view))))]
    (when (> total (long (:view-bytes picture-bounds)))
      (str "the pictures of this view hold " total
           " bytes; the most is " (:view-bytes picture-bounds)))))

;; Live view checks: the one check of every host. The Python SDK runs the same
;; algorithm (`blockether.vis._contracts.live_view_faults`), and the shared cases in
;; `vis-contract/fixtures/live-view-checks.json` hold both to the same answer. The
;; check reads wire data. The schema owns the shape and the defaults of each node;
;; this code holds only the rules that JSON Schema cannot say.

(defn wire-data
  "`value` as JSON-shaped wire data: snake_case string keys and string enum values.
   Unlike [[wire/->wire]] it keeps a non-finite number, so the check can refuse it."
  [value]
  (cond (map? value) (into {}
                           (map (fn [[k v]]
                                  [(wire/wire-key k) (wire-data v)]))
                           value)
        (and (coll? value) (not (map? value))) (mapv wire-data value)
        (keyword? value) (name value)
        (symbol? value) (str value)
        (uuid? value) (str value)
        (ratio? value) (double value)
        :else value))

(defn- variant
  [type-name]
  (when (string? type-name)
    (when-let [index (get component-index type-name)]
      [index (get (get (definition "component") "oneOf") index)])))

(defn- ref-definition
  [shape]
  (some->> (get-in shape ["items" "$ref"])
           (re-matches #"#/\$defs/(.+)")
           second
           definition))

(defn- filled-defaults
  "`m` with the schema default of each required key of `shape` that it leaves out,
   then with each key that `x-vis-fill` fills: `true` takes the schema default, a
   property name takes the value of that property."
  [m shape]
  (let [properties
        (get shape "properties")

        m
        (reduce (fn [m k]
                  (let [rule (get properties k)]
                    (if (and (not (contains? m k)) (contains? rule "default"))
                      (assoc m k (get rule "default"))
                      m)))
                m
                (get shape "required"))]

    (reduce (fn [m [k rule]]
              (let [fill (get rule "x-vis-fill")]
                (cond (or (nil? fill) (contains? m k)) m
                      (true? fill) (assoc m k (get rule "default"))
                      (contains? m fill) (assoc m k (get m fill))
                      :else m)))
            m
            (sort-by key properties))))

(defn- filled-item
  "Wire item `item` of collection `k` as every host keeps it: the defaults of `shape`,
   and a link with a target that has a scheme is a `url` link."
  [shape k item]
  (cond-> item
    (map? item)
    (filled-defaults shape)

    (and (= "links" k)
         (map? item)
         (not (contains? item "target_kind"))
         (string? (get item "target"))
         (re-find #"^[A-Za-z][A-Za-z0-9+.-]*:" (get item "target")))
    (assoc "target_kind" "url")))

(defn fill-live-node
  "Live wire node `node` with the defaults that every host fills: a null key is an
   absent key, a node without a type is a `plaintext` input, an input takes its name
   and label from its id, a link with a scheme is a `url` link, and each required key
   with a schema `default` takes it. The children of a group are filled too."
  [node]
  (if-not (map? node)
    node
    (let [node
          (into {} (remove (comp nil? val)) node)

          node
          (cond-> node
            (not (contains? node "type"))
            (assoc "type" "plaintext"))

          [_ shape]
          (variant (get node "type"))

          input?
          (contains? (get shape "properties") "is_secret")

          node
          (cond-> node
            input?
            (-> (dissoc "is_secret")
                (as-> n
                  (cond-> n
                    (and (not (contains? n "id")) (contains? n "name"))
                    (assoc "id" (get n "name"))))
                (as-> n
                  (cond-> n
                    (and (not (contains? n "name")) (contains? n "id"))
                    (assoc "name" (get n "id"))

                    (and (not (contains? n "label")) (contains? n "id"))
                    (assoc "label" (get n "id"))))))

          node
          (if shape (filled-defaults node shape) node)

          node
          (reduce-kv (fn [n k rule]
                       (let [items
                             (get n k)

                             item-shape
                             (ref-definition rule)]

                         (if (and item-shape (sequential? items))
                           (assoc n k (mapv #(filled-item item-shape k %) items))
                           n)))
                     node
                     (or (get shape "properties") {}))]

      (cond-> node
        (sequential? (get node "children"))
        (update "children" #(mapv fill-live-node %))))))

(defn- non-finite-paths
  [value path]
  (cond (map? value) (mapcat (fn [[k v]]
                               (non-finite-paths v (str path "/" k)))
                             value)
        (sequential? value) (mapcat (fn [i v]
                                      (non-finite-paths v (str path "/" i)))
                                    (range)
                                    value)
        (and (double? value) (or (Double/isNaN value) (Double/isInfinite value))) [path]
        (and (float? value) (or (Float/isNaN value) (Float/isInfinite value))) [path]
        :else []))

(defn- child-path?
  "True for a fault inside a group child: the child is checked on its own. Only the
   rule that the group puts on the type of a child stays with the group."
  [line]
  (boolean (re-find #"^/children/\d+[/ ]" line)))

(defn- repeated
  [ids]
  (first (sort (keep (fn [[id n]]
                       (when (> (long n) 1) id))
                     (frequencies (keep #(some-> %
                                                 str)
                                        ids))))))

(defn- engine-field
  [node]
  (-> (wire/->engine node)
      (update :type keyword)))

(defn- input-lines
  "The input rules of filled node `node`. A default or a value is held to the domain
   only when the bounds of a range make a domain."
  [node]
  (let [field
        (engine-field node)

        kind
        (:type field)

        checked
        (cond-> field
          (= :range kind)
          (->> (merge range-defaults)))

        range-lines
        (when (= :range kind)
          (if-not (ascending-bounds? checked)
            [(document/named-line "/min" "range_order")]
            (when-not (positive-step? checked) [(document/named-line "/step" "range_step")])))]

    (concat (when (and (contains? secret-types kind) (some? (:value field)))
              [(document/named-line "/value" "secret_value")])
            (when (and (contains? secret-types kind) (contains? field :default))
              [(document/named-line "/default" "secret_default")])
            (when (contains? node "validate") [(document/named-line "/validate" "live_validate")])
            (when (and (sequential? (get node "options"))
                       (repeated (map #(when (map? %) (get % "value")) (get node "options"))))
              [(document/named-line "/options" "repeated_option")])
            (when (not= (:id field) (:name field)) [(document/named-line "/name" "id_name")])
            (when-not (ordered-lengths? field) [(document/named-line "/min_length" "length_order")])
            range-lines
            (when (and (empty? range-lines)
                       (contains? field :default)
                       (not (contains? secret-types kind))
                       (not (default-in-domain? checked)))
              [(document/named-line "/default" "default_domain")])
            (when (and (empty? range-lines)
                       (contains? field :value)
                       (not (contains? secret-types kind))
                       (not (value-in-domain? checked (:value field))))
              [(document/named-line "/value" "value_domain")]))))

(defn- toolbar-command?
  [node]
  (and (map? node)
       (or (= "button" (get node "type"))
           (and (= "group" (get node "type"))
                (sequential? (get node "children"))
                (every? toolbar-command? (get node "children"))))))

(defn- toolbar-stamp-lines
  "A `read_only` line for each key that only the host writes on a node of diff
   toolbar `children` at JSON pointer `path`, a menu child too. The diff node check
   does not see them, because a toolbar button is no node of the view."
  [children path]
  (when (sequential? children)
    (mapcat (fn [i child]
              (when (map? child)
                (let [at (str path "/" i)]
                  (concat (for [[k rule] (sort-by key
                                                  (get (second (variant (get child "type")))
                                                       "properties"))
                                :when (and (get rule "readOnly") (contains? child k))]

                            (document/named-line (str at "/" k) "read_only"))
                          (toolbar-stamp-lines (get child "children") (str at "/children"))))))
            (range)
            children)))

(defn- hand-lines
  "The rules of filled node `node` that JSON Schema cannot say. Each one is a case
   of `live-view-checks.json`."
  [node decl?]
  (let [kind
        (get node "type")

        [_ shape]
        (variant kind)

        line
        document/named-line]

    (concat
      (when decl?
        (for [[k rule]
              (sort-by key (get shape "properties"))

              :when (and (get rule "readOnly") (contains? node k))]

          (line (str "/" k) "read_only")))
      (when (contains? (get shape "properties") "is_required") (input-lines node))
      (case kind
        "log"
        (let [tones (get node "line_tones")]
          (when (and (sequential? tones) (not= (count tones) (count (get node "lines"))))
            [(line "/line_tones" "line_tones")]))

        "group"
        (when (and (contains? node "max_columns") (not= "grid" (get node "direction")))
          [(line "/max_columns" "grid_columns")])

        "diff"
        (concat (when decl? (toolbar-stamp-lines (get node "children") "/children"))
                (keep-indexed (fn [i child]
                                (when (and (map? child)
                                           (= "group" (get child "type"))
                                           (not (toolbar-command? child)))
                                  (line (str "/children/" i) "toolbar_menu")))
                              (get node "children"))
                (keep-indexed (fn [i thread]
                                (when (and (map? thread) (not (contains? thread "anchor")))
                                  (line (str "/threads/" i) "thread_anchor")))
                              (get node "threads"))
                (some->> (repeated (map #(get % "id") (get node "threads")))
                         (array-map "id")
                         (line "/threads" "repeated_item_id")
                         vector))

        ("stat" "steps")
        (let [k (if (= "stat" kind) "stats" "steps")]
          (some->> (when (sequential? (get node k))
                     (repeated (map #(when (map? %) (get % "id")) (get node k))))
                   (array-map "id")
                   (line (str "/" k) "repeated_item_id")
                   vector))

        "timeline"
        (concat (some->> (repeated (map #(get % "id") (get node "lanes")))
                         (array-map "id")
                         (line "/lanes" "repeated_item_id")
                         vector)
                (keep-indexed
                  (fn [i {:strs [start end]}]
                    (when (and (number? end) (number? start) (< (double end) (double start)))
                      (line (str "/lanes/" i) "lane_order")))
                  (get node "lanes")))

        "table"
        (let [order
              (get node "order")

              columns
              (set (map #(get % "id") (get node "columns")))

              rows
              (set (map #(get % "id") (get node "rows")))]

          (concat (when (and (map? order) (not (contains? columns (get order "by"))))
                    [(line "/order" "table_order")])
                  (let [most
                        (get node "max_rows")

                        held
                        (get node "rows")]

                    (when (and (number? most) (sequential? held) (> (count held) (long most)))
                      [(line "/rows" "table_rows" {"count" (count held) "limit" most})]))
                  (when (contains? node "selected_ids")
                    (cond (not (true? (get node "is_selectable"))) [(line "/selected_ids"
                                                                          "not_selectable")]
                          (not-every? rows (get node "selected_ids")) [(line "/selected_ids"
                                                                             "table_selection")]))))

        "scene"
        (let [meshes
              (get node "meshes")

              objects
              (get node "objects")

              known
              (into (set (keys scene-meshes)) (map #(get % "id")) meshes)]

          (concat (some->> (repeated (map #(get % "id") meshes))
                           (array-map "id")
                           (line "/meshes" "repeated_item_id")
                           vector)
                  (some->> (repeated (map #(get % "id") objects))
                           (array-map "id")
                           (line "/objects" "repeated_item_id")
                           vector)
                  (keep-indexed (fn [i {:strs [id vertices faces]}]
                                  (cond (contains? scene-meshes id) (line (str "/meshes/" i)
                                                                          "mesh_shadow")
                                        (not-every? #(< (long %) (count vertices)) (flatten faces))
                                        (line (str "/meshes/" i) "face_vertex")))
                                meshes)
                  (keep-indexed (fn [i {:strs [mesh]}]
                                  (when-not (contains? known mesh)
                                    (line (str "/objects/" i) "unknown_mesh")))
                                objects)))

        "image"
        (for [[path raw]
              [["/" node] ["/before" (get node "before")]]

              :when (and (map? raw) (contains? raw "data"))
              :let [reason
                    (:error (read-picture (wire/->engine raw)))]
              :when reason]

          (line path "picture" {"reason" reason}))

        nil))))

(defn live-node-lines
  "The sorted fault lines of wire node `node` after [[fill-live-node]], or an empty
   vector. A line names the path in the node, the broken rule and its keyword, never a
   value. With `decl?`, a key that only the host writes is a fault. The faults of a
   group child belong to the child, which the caller checks on its own."
  [node decl?]
  (let [node
        (fill-live-node node)

        kind
        (get node "type")]

    (cond (not (map? node)) [(document/named-line "/" "node_not_map")]
          (nil? (variant kind)) [(document/named-line "/type" "unknown_type")]
          :else (let [bad (non-finite-paths node "")]
                  (if (seq bad)
                    (vec (sort (distinct (map #(document/named-line % "not_finite") bad))))
                    (let [[index] (variant kind)
                          group? (= "group" kind)
                          schema (cond->> (vec (sort (distinct (variant-lines index node))))
                                   group?
                                   (remove child-path?))
                          ;; A group child is checked on its own, so it needs a map and an id.
                          schema
                          (concat
                            schema
                            (when group?
                              (keep-indexed
                                (fn [i child]
                                  (cond (not (map? child)) (document/named-line (str "/children/" i)
                                                                                "child_not_map")
                                        (not (non-blank-string? (get child "id")))
                                        (document/named-line (str "/children/" i)
                                                             (if (= "group" (get child "type"))
                                                               "group_needs_id"
                                                               "node_needs_id"))))
                                (when (sequential? (get node "children")) (get node "children")))))
                          lines (if (seq schema) schema (hand-lines node decl?))]

                      (vec (sort (distinct lines)))))))))

(def view-refusal-prefix
  "The head of the refusal of a live view in every host."
  "Invalid live view: ")

(def patch-refusal-prefix
  "The head of the refusal of a live-view patch in every host."
  "Invalid live-view patch: ")

(defn node-fault-text
  "`node <id> <lines>` of node `id`, at most five lines."
  [id lines]
  (document/render (get (document/message-table) "node")
                   {"id" (let [id (str id)]
                           (document/clip-name id))
                    "lines" (document/joined-lines lines)}))

(defn view-fault-text
  "`view <lines>` of the view faults `lines`."
  [lines]
  (document/render (get (document/message-table) "view") {"lines" (document/joined-lines lines)}))

(defn live-node-fault
  "The fault text of one wire node, or nil when it is valid."
  [node decl?]
  (let [lines (live-node-lines node decl?)]
    (when (seq lines) (node-fault-text (when (map? node) (get node "id")) lines))))

(defn error-card
  "The node that a host shows in place of node `id` with fault `text`."
  [id text]
  {"id" id "type" "status" "tone" "error" "text" text})

(def ^:private view-stamps
  {"id" "view" "channel_ids" ["app"] "timeout_ms" 0 "seq" 0 "created_at" 1})

(defn- wire-tree
  [nodes]
  (mapcat (fn [node]
            (when (map? node)
              (cons node (when (= "group" (get node "type")) (wire-tree (get node "children"))))))
          (when (sequential? nodes) nodes)))

(defn- command-tree
  "Each map node of wire `nodes`, also each child of a group or of a diff toolbar:
   the nodes that a shortcut can press."
  [nodes]
  (mapcat (fn [node]
            (when (map? node) (cons node (command-tree (get node "children")))))
          (when (sequential? nodes) nodes)))

(defn- pressable?
  [node]
  (or (= "button" (get node "type"))
      (and (= "card" (get node "type")) (true? (get node "is_selectable")))))

(defn- shortcut-lines
  "The view faults of the shortcuts of wire view `view` with filled `nodes`: only an
   app takes shortcuts, a key is bound once, and a `node_id` names a button or a
   selectable card of the view. The schema holds the shape of each shortcut."
  [view nodes]
  (let [shortcuts (get view "shortcuts")]
    (when (sequential? shortcuts)
      (if (not= "attachment" (get view "placement"))
        [(document/named-line "/shortcuts" "shortcut_needs_app")]
        (let [by-id (into {} (map (juxt #(get % "id") identity)) (command-tree nodes))]
          (concat (->> shortcuts
                       (map-indexed vector)
                       (reduce (fn [[seen lines] [i shortcut]]
                                 (let [k (when (and (map? shortcut) (contains? shortcut "key"))
                                           (str (get shortcut "key")))]
                                   (cond (contains? seen k) [seen
                                                             (conj lines
                                                                   (document/named-line
                                                                     (str "/shortcuts/" i)
                                                                     "repeated_shortcut_key"))]
                                         (some? k) [(conj seen k) lines]
                                         :else [seen lines])))
                               [#{} []])
                       second)
                  (keep-indexed (fn [i shortcut]
                                  (let [id (when (map? shortcut) (get shortcut "node_id"))
                                        node (get by-id id)]

                                    (cond (not (string? id)) nil
                                          (nil? node) (document/named-line (str "/shortcuts/" i)
                                                                           "shortcut_target")
                                          (not (pressable? node)) (document/named-line
                                                                    (str "/shortcuts/" i)
                                                                    "shortcut_not_pressable"))))
                                shortcuts)))))))

(defn- view-with-nodes
  "Wire view `view` with one channel list and its nodes filled, as the checks read it."
  [view]
  (cond-> view
    (and (contains? view "channel_id") (not (contains? view "channel_ids")))
    (-> (assoc "channel_ids" [(get view "channel_id")])
        (dissoc "channel_id"))

    (sequential? (get view "nodes"))
    (update "nodes" #(mapv fill-live-node %))))

(defn live-view-lines
  "The sorted view fault lines of live wire view `view`, which refuse the whole view:
   not a map, a view key that the schema refuses, a top node that is no map or has no
   id, a shortcut rule, two nodes with one id, too many nodes or too many picture
   bytes. A host checks a patched view with it, after each op had its node check."
  [view decl?]
  (if-not (map? view)
    [(document/named-line "/" "view_not_map")]
    (let [view
          (view-with-nodes view)

          nodes
          (get view "nodes")

          stamped
          (merge view-stamps
                 (dissoc view "nodes")
                 {"nodes" (if (sequential? nodes)
                            (mapv (fn [_]
                                    {"id" "node" "type" "divider"})
                                  nodes)
                            nodes)})

          schema
          (if (seq (non-finite-paths stamped ""))
            [(document/named-line "/" "view_not_json")]
            (mapv #(str/replace-first % #"^/nodes/\d+ .*" "")
                  (document/explain-lines (document/explain-json "view" "live_view" stamped)
                                          stamped)))

          shortcuts
          (shortcut-lines view nodes)

          ;; A shortcut rule names the problem of its path; the oneOf line says less.
          covered
          (set (map #(first (str/split % #" - " 2)) shortcuts))

          schema
          (vec (remove #(or (str/blank? %)
                            (and (str/ends-with? % "(oneOf)")
                                 (covered (first (str/split % #" - " 2)))))
                 schema))

          tops
          (when (sequential? nodes)
            (keep-indexed
              (fn [i node]
                (cond (not (map? node)) (document/named-line (str "/nodes/" i) "node_not_map")
                      (not (non-blank-string? (get node "id")))
                      (document/named-line
                        (str "/nodes/" i)
                        (if (= "group" (get node "type")) "group_needs_id" "node_needs_id"))))
              nodes))

          tree
          (wire-tree nodes)

          most
          (long (:max-nodes view-defaults))

          picture-total
          (long (view-picture-bytes (map wire/->engine tree)))

          lines
          (vec
            (sort
              (distinct
                (concat schema
                        tops
                        (when decl?
                          (for [[k rule]
                                (sort-by key (get (definition "live_view") "properties"))

                                :when (and (get rule "readOnly") (contains? view k))]

                            (document/named-line (str "/" k) "read_only")))
                        shortcuts
                        (some->> (repeated (keep #(get % "id") tree))
                                 (array-map "id")
                                 (document/named-line "/nodes" "repeated_node_id")
                                 vector)
                        (when (> (count tree) most)
                          [(document/named-line "/nodes"
                                                "too_many_nodes"
                                                {"count" (count tree) "limit" most})])
                        (when (> picture-total (long (:view-bytes picture-bounds)))
                          [(document/named-line "/nodes"
                                                "picture_budget"
                                                {"count" picture-total
                                                 "limit" (:view-bytes picture-bounds)})])))))]

      lines)))

(defn- fault-key
  "The path and the rule of fault line `line`, without the counts of its message."
  [line]
  [(first (str/split line #" - " 2)) (second (re-find #"\(([^()]*)\)$" line))])

(defn live-patched-lines
  "The fault lines of `after` whose path and rule `before` did not have: a patch is
   refused only for a fault that it adds, not for one that it makes smaller or larger.
   `lines-of` is [[live-view-lines]] or a node check."
  [lines-of before after]
  (let [had (if (some? before) (set (map fault-key (lines-of before))) #{})]
    (vec (remove (comp had fault-key) (lines-of after)))))

(defn live-view-faults
  "Check live wire view `view` as every host does. Answers `{:view lines :nodes
   {id text} :dropped notes}`. A line of [[live-view-lines]] refuses the whole view.
   Else each node that breaks its own rules maps its id to its fault text, and a host
   shows it as an error card. A shortcut that names such a node is dropped, with one
   note each."
  [view decl?]
  (let [lines (live-view-lines view decl?)]
    (if (seq lines)
      {:view lines :nodes {} :dropped []}
      (let [view (view-with-nodes view)
            nodes (get view "nodes")
            found (volatile! (array-map))
            broken-ids (volatile! #{})
            walk (fn walk [node]
                   (let [node-lines (live-node-lines node decl?)]
                     (if (seq node-lines)
                       (do (vswap! found
                                   assoc
                                   (get node "id")
                                   (node-fault-text (get node "id") node-lines))
                           (vswap! broken-ids into (keep #(get % "id")) (wire-tree [node])))
                       (when (= "group" (get node "type"))
                         (run! walk (filter map? (get node "children")))))))]

        (run! walk nodes)
        {:view []
         :nodes @found
         :dropped (vec (for [[i shortcut] (map-indexed vector (get view "shortcuts"))
                             :when (and (map? shortcut)
                                        (contains? @broken-ids (get shortcut "node_id")))]

                         {:key (get shortcut "key")
                          :node-id (get shortcut "node_id")
                          :note (document/named-line (str "/shortcuts/" i)
                                                     "shortcut_dropped"
                                                     {"id" (str (get shortcut "node_id"))})}))}))))

(defn live-view-error
  "nil when normalized live view `view` passes [[live-view-faults]], otherwise the
   view fault text, or the fault text of its first broken node."
  [view]
  (let [{lines :view nodes :nodes} (live-view-faults (wire-data view) false)]
    (cond (seq lines) (view-fault-text lines)
          (seq nodes) (val (first (sort-by key nodes))))))

(defn live-shortcuts-error
  "nil when the shortcuts of normalized live view `view` pass the shared shortcut
   rules of [[live-view-faults]], otherwise the view fault text."
  [view]
  (let [wire (wire-data view)]
    (when-let [lines (seq (shortcut-lines wire (get wire "nodes")))]
      (view-fault-text lines))))

(def ^:private live-op-index
  "Wire op name to its position in the `live_op` union."
  (into {}
        (map-indexed (fn [index shape]
                       [(get-in shape ["properties" "op" "const"]) index]))
        (get (definition "live_op") "oneOf")))

(defn- live-op-lines
  "The fault lines of wire patch op `op` at `path`. An op is checked against the one
   `live_op` variant that its `op` names, as a node is checked against its own
   `component` variant: a report against the whole union stops before the faults of
   the chosen form. The node of an `add-node` op is left to the node check after the
   op, which names the node. Each case is in `live-view-checks.json`."
  [path op]
  (if-let [index (when (map? op) (get live-op-index (get op "op")))]
    (document/explain-lines
      (update (document/explain "view" (str "live_op/oneOf/" index) op)
              :errors
              (partial remove #(str/starts-with? (str (:instanceLocation %)) "/node_spec")))
      op
      path)
    (let [table (get (document/message-table) "keywords")]
      (if (map? op)
        [(document/rule-line (str path "/op")
                             (document/render (get table "enum")
                                              {"allowed"
                                               (str/join ", " (keys (sort-by val live-op-index)))})
                             "enum")]
        [(document/rule-line path
                             (document/render (get table "type")
                                              {"expected" "object"
                                               "actual" (document/json-type op)})
                             "type")]))))

(def ^:private live-item-shapes
  "Collection key to the schema of one item, from the append targets of `live_op`, for
   the keyed items that every host fills in an op (`_LIVE_ITEMS` of the Python SDK)."
  (into {}
        (for [[node-type ks]
              (get-in (definition "live_op") ["x-vis-patch-targets" "append"])

              :when (sequential? ks)
              k
              ks

              :when (contains? #{"stats" "steps" "lanes" "rows" "links"} k)
              :let [shape
                    (ref-definition (get-in live-nodes [node-type "properties" k]))]
              :when shape]

          [k shape])))

(defn- filled-op
  "Wire op `op` with each item filled as the host keeps it: see [[filled-item]]. A lane
   without a `tone` is an `idle` lane, and a link with a scheme is a `url` link."
  [op]
  (if (map? op)
    (reduce-kv (fn [op k shape]
                 (cond-> op
                   (sequential? (get op k))
                   (update k #(mapv (partial filled-item shape k) %))))
               op
               live-item-shapes)
    op))

(defn live-patch-lines
  "The sorted fault lines of live patch `patch`, which every host gives before it applies
   an op. The patch keys and the op list are checked with a stand-in op, and each op on
   its own, so a fault of one op never hides the faults of the patch. An item gets its
   schema defaults first."
  [patch]
  (let [patch
        (wire-data patch)

        ops
        (get patch "ops")

        shell
        (cond-> patch
          (and (sequential? ops) (seq ops))
          (assoc "ops" [{"op" "clear" "node_id" "p"}]))]

    (->> (when (sequential? ops)
           (map-indexed (fn [i op]
                          (live-op-lines (str "/ops/" i) (filled-op op)))
                        ops))
         (apply concat (document/explain-lines (document/explain "view" "live_patch" shell) shell))
         distinct
         sort
         vec)))

(def ^:private patch-targets
  "Op name to wire node type to the op keys that type takes, from `x-vis-patch-targets`."
  (dissoc (get (definition "live_op") "x-vis-patch-targets") "description"))

(defn live-op-target-lines
  "The sorted fault lines of wire op `op` on a node of wire type `node-type`: the op
   does not apply to that type, or a key of the op that the type does not take. Every
   host checks this before it applies a node op. An op without a target table has no lines.
   A thread op on a thread node must name the id `node-id` of that node."
  [node-type node-id op]
  (let [op
        (wire-data op)

        op-name
        (get op "op")

        table
        (get patch-targets op-name)

        values
        {"op" op-name "type" node-type}]

    (cond (nil? table) []
          (not (contains? table node-type)) [(document/named-line "/op" "op_target" values)]
          :else (let [taken (set (get table node-type))]
                  (cond-> (->> (keys op)
                               (remove #{"op" "node_id"})
                               (remove taken)
                               sort
                               (mapv #(document/named-line (str "/" %) "op_key" values)))
                    (and (= ["thread" "thread"] [op-name node-type])
                         (not= node-id (get-in op ["thread" "id"])))
                    (conj (document/named-line "/thread/id" "own_thread")))))))

(defn live-patch-error
  "nil when `patch` has a valid live patch shape, otherwise a reason."
  [patch]
  (some-> (seq (live-patch-lines patch))
          vec
          document/joined-lines))

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
