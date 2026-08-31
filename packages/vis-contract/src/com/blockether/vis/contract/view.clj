(ns com.blockether.vis.contract.view
  "The executable contract for every View document.

   `vis-contract/view.edn` owns both View kinds' closed vocabulary and renderer
   bounds. This namespace validates that document, derives the Clojure values
   Core uses, declares every input field, answer, live node, patch, verdict and
   artifact shape, and renders the portable JSON section. It requires no Vis
   implementation namespace."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- closed-map? [m expected-keys] (and (map? m) (= expected-keys (set (keys m)))))
(defn- name-vector?
  [x]
  (and (vector? x)
       (seq x)
       (= x (vec (sort x)))
       (= (count x) (count (set x)))
       (every? #(and (non-blank-string? %) (re-matches #"[a-z][a-z0-9]*(?:-[a-z0-9]+)*" %)) x)))

(defn- valid-document?
  [{:contract/keys [version]
    :view/keys [kinds actions field-types text-types choice-types secret-types decor-types
                group-type group-directions otp range secret-handle-prefix live]
    :as document}]
  (let [{:keys [node-types link-targets ops tones orders aligns sort-dirs reasons log table
                max-stats max-steps max-links max-nodes]}
        live

        field-type-set
        (set field-types)]

    (and (closed-map? document
                      #{:contract/version :view/kinds :view/actions :view/field-types
                        :view/text-types :view/choice-types :view/secret-types :view/decor-types
                        :view/group-type :view/group-directions :view/otp :view/range
                        :view/secret-handle-prefix :view/live})
         (pos-int? version)
         (every? name-vector?
                 [kinds actions field-types text-types choice-types secret-types decor-types
                  group-directions])
         (every? field-type-set text-types)
         (every? field-type-set choice-types)
         (every? field-type-set secret-types)
         (non-blank-string? group-type)
         (not (contains? field-type-set group-type))
         (not (contains? (set decor-types) group-type))
         (closed-map? otp #{:length :ceiling})
         (every? pos-int? (vals otp))
         (<= (long (:length otp)) (long (:ceiling otp)))
         (closed-map? range #{:min :max :step})
         (every? number? (vals range))
         (< (double (:min range)) (double (:max range)))
         (pos? (double (:step range)))
         (non-blank-string? secret-handle-prefix)
         (closed-map? live
                      #{:node-types :link-targets :ops :tones :orders :aligns :sort-dirs :reasons
                        :log :table :max-stats :max-steps :max-links :max-nodes})
         (every? name-vector? [node-types link-targets ops tones orders aligns sort-dirs reasons])
         (closed-map? log #{:window-lines :window-lines-cap :max-patch-lines})
         (every? pos-int? (vals log))
         (<= (long (:window-lines log)) (long (:window-lines-cap log)))
         (closed-map? table #{:max-rows :max-patch-rows})
         (every? pos-int? (vals table))
         (every? pos-int? [max-stats max-steps max-links max-nodes]))))

(s/def :contract/view valid-document?)

(def ^:private resource-path "vis-contract/view.edn")

(def ^:private document
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the View contract is missing from the classpath: " resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/view parsed)
        (throw (ex-info (str resource-path " is not a valid View contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/view parsed)})))
      parsed)))

(defn- keyword-map [names] (into {} (map (juxt identity keyword)) names))
(defn- keyword-set [names] (set (map keyword names)))

(def version "View contract document version." (:contract/version @document))
(def view-kinds "Wire View name to lifecycle kind." (keyword-map (:view/kinds @document)))
(def view-actions
  "Wire operator action to internal action."
  (keyword-map (:view/actions @document)))
(def field-types
  "Wire answer-field name to internal field type."
  (keyword-map (:view/field-types @document)))
(def text-types "Answer field types carrying text." (keyword-set (:view/text-types @document)))
(def choice-types
  "Answer field types carrying choices."
  (keyword-set (:view/choice-types @document)))
(def secret-types
  "Answer field types replaced by vault handles."
  (keyword-set (:view/secret-types @document)))
(def decor-types
  "Wire decoration name to internal decoration type."
  (keyword-map (:view/decor-types @document)))
(def group-type-name "Wire name of a layout group." (:view/group-type @document))
(def group-type "Internal type of a layout group." (keyword group-type-name))
(def group-directions
  "Wire layout direction to internal direction."
  (keyword-map (:view/group-directions @document)))
(def otp-defaults "Default and maximum one-time-code lengths." (:view/otp @document))
(def range-defaults "Default numeric range." (:view/range @document))
(def secret-handle-prefix
  "Prefix of an opaque secret answer handle."
  (:view/secret-handle-prefix @document))
(def live-node-types
  "Wire semantic live-node name to internal node type."
  (keyword-map (get-in @document [:view/live :node-types])))
(def link-targets
  "Wire link target name to internal target type."
  (keyword-map (get-in @document [:view/live :link-targets])))
(def live-ops
  "Wire live patch operation to internal operation."
  (keyword-map (get-in @document [:view/live :ops])))
(def live-tones
  "Wire live tone to internal tone."
  (keyword-map (get-in @document [:view/live :tones])))
(def live-orders
  "Wire table order to internal order."
  (keyword-map (get-in @document [:view/live :orders])))
(def live-aligns
  "Wire table alignment to internal alignment."
  (keyword-map (get-in @document [:view/live :aligns])))
(def live-sort-dirs
  "Wire sort direction to internal direction."
  (keyword-map (get-in @document [:view/live :sort-dirs])))
(def live-reasons
  "Wire settlement reason to internal reason."
  (keyword-map (get-in @document [:view/live :reasons])))
(def log-defaults "Live log paint-window and patch bounds." (get-in @document [:view/live :log]))
(def table-defaults
  "Live table collection and patch bounds."
  (get-in @document [:view/live :table]))
(def stat-defaults
  "Live stat collection bound."
  {:max-stats (get-in @document [:view/live :max-stats])})
(def step-defaults
  "Live step collection bound."
  {:max-steps (get-in @document [:view/live :max-steps])})
(def link-defaults
  "Live link collection bound."
  {:max-links (get-in @document [:view/live :max-links])})
(def view-defaults
  "Live node collection bound."
  {:max-nodes (get-in @document [:view/live :max-nodes])})

(def vocabulary
  "Portable keyword-keyed View vocabulary derived from the owning EDN document."
  {:view-kinds (:view/kinds @document)
   :view-actions (:view/actions @document)
   :field-types (:view/field-types @document)
   :text-types (:view/text-types @document)
   :choice-types (:view/choice-types @document)
   :secret-types (:view/secret-types @document)
   :decor-types (:view/decor-types @document)
   :group-type (:view/group-type @document)
   :group-directions (:view/group-directions @document)
   :otp (:view/otp @document)
   :range (:view/range @document)
   :secret-handle-prefix (:view/secret-handle-prefix @document)
   :live (:view/live @document)})

(defn package-document
  "Deterministic JSON-ready View section for every generated language contract."
  []
  (let [{:keys [view-kinds view-actions field-types text-types choice-types secret-types decor-types
                group-type group-directions otp range secret-handle-prefix live]}
        vocabulary]
    (array-map "kinds" view-kinds
               "actions" view-actions
               "field_types" field-types
               "text_types" text-types
               "choice_types" choice-types
               "secret_types" secret-types
               "decor_types" decor-types
               "group_type" group-type
               "group_directions" group-directions
               "otp" (array-map "length" (:length otp) "ceiling" (:ceiling otp))
               "range" (array-map "min" (:min range) "max" (:max range) "step" (:step range))
               "secret_handle_prefix" secret-handle-prefix
               "live" (array-map
                        "node_types" (:node-types live)
                        "link_targets" (:link-targets live)
                        "ops" (:ops live)
                        "tones" (:tones live)
                        "orders" (:orders live)
                        "aligns" (:aligns live)
                        "sort_dirs" (:sort-dirs live)
                        "reasons" (:reasons live)
                        "log" (array-map "window_lines" (get-in live [:log :window-lines])
                                         "window_lines_cap" (get-in live [:log :window-lines-cap])
                                         "max_patch_lines" (get-in live [:log :max-patch-lines]))
                        "table" (array-map "max_rows" (get-in live [:table :max-rows])
                                           "max_patch_rows" (get-in live [:table :max-patch-rows]))
                        "max_stats" (:max-stats live)
                        "max_steps" (:max-steps live)
                        "max_links" (:max-links live)
                        "max_nodes" (:max-nodes live)))))

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
  "Every key a live NODE may be written with, whatever its type. Closed as one
   set for the same reason [[field-keys]] is: the parser derives the snake_case
   spellings from it, so no key is written down twice."
  #{:id :type :label :text :detail :tone :value :done :total :stats :steps :lines :window-lines
    :columns :rows :max-rows :order :is-selectable :selected-ids :links
    ;; A layout GROUP's own two keys: which way its children run, and the
    ;; children themselves. DECLARED once and never `set` — both are absent from
    ;; every op in [[live-op-key-sets]], so the arrangement a reader learned
    ;; cannot jump around while they are reading it.
    :direction :fields
    ;; ENGINE stamp, never written in a spec: how many lines a `:log`'s RECORD
    ;; holds, so `… N earlier lines` is counted rather than guessed.
    :total-lines})

(def live-view-keys
  "Every key a live view may carry, engine stamps included."
  (into #{:title :description :source :session-id :channel-ids :nodes :timeout-ms}
        live-view-stamp-keys))

(def live-picture-keys
  "Every key the PICTURE carries — a finished view as data, with the mount
   bookkeeping (`:id`, `:session-id`, `:channel-ids`, the stamps) left behind. It
   is what the verdict hands the model and what a parsed document answers, so one
   shape crosses in both directions."
  #{:title :description :nodes})

(def live-elided-keys
  "Every key one elision carries: which node a budget cut and how many items it
   did not carry. The record still holds them."
  #{:node-id :items})
(def live-op-key-sets
  "Every key ONE patch operation may carry, per operation. Closed per op rather
   than as a union, because `s/keys` accepts any key it was not told about: a
   `clear` carrying lines meant to `append` and must hear so.

   `:node-id` is the ADDRESS of the node an op speaks to — spelled apart from the
   form tree's `:node`, which is a whole field; `:node-spec` is the whole node
   `add-node` introduces."
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
  "Every key the verdict carries — the ONE thing the model reads back, and it
   reads DATA: `:view` is the finished picture with its ids and tones intact, so
   the model acts on values instead of recovering them from prose. Markdown is the
   human's document (`live/->markdown`), never the model's contract.

   `:is-from-human` says a PERSON stopped it — Escape in the terminal, Stop in the
   app — and `:note` carries the words they left with that stop, so a run cut short
   reads WHO cut it, and why, before it reads the picture."
  #{:view-id :is-completed :reason :is-from-human :note :view :elided :summary :artifact-id :error})

(def live-selection-snapshot-bytes
  "Maximum serialized archive-only selection pictures in one live record trailer."
  1000000)

(def live-artifact-media-type
  "The media type a settled live view carries. Its bytes are the view's RECORD —
   the append-only NDJSON `view.sink` has been writing since `open` —
   so the artifact IS that file rather than a re-encoded copy of it."
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

(def ^:private answer-keys #{:is-submitted :reason :request-id :values})

;; Predicates

(defn- closed?
  "True when `m` carries no key outside `allowed`. The internal form is closed
   in both directions: a wire key that survived normalization is a normalizer
   bug, not a harmless extra, and a surface reading the map has one shape to
   paint."
  [allowed m]
  (and (map? m) (every? allowed (keys m))))

(defn- one-identity?
  "`:name` is `:id` under its legacy spelling. Two spellings of one identity
   have to agree, or a value comes back keyed by the name the caller did not
   use."
  [{:keys [id name]}]
  (= id name))

(defn- secret-marked?
  "`:is-secret` is derived from the type, never guessed per field: a password and
   a one-time code are secret and nothing else is silently promoted or demoted."
  [{:keys [type is-secret]}]
  (= is-secret (contains? secret-types type)))

(defn- ordered-lengths?
  [{:keys [min-length max-length]}]
  (or (nil? min-length) (nil? max-length) (<= (long min-length) (long max-length))))

(defn- otp-fits-boxes? [{:keys [max-length]}] (<= (long max-length) (long (:ceiling otp-defaults))))

(defn- ascending-bounds? [{:keys [min max]}] (< (double min) (double max)))

(defn- positive-step?
  [{:keys [min max step]}]
  (and (pos? (double step)) (<= (double step) (- (double max) (double min)))))

(defn- option-values [field] (set (map :value (:options field))))

(defn- non-empty?
  "A collection with something in it. Spelled out rather than left to
   `:min-count`, whose expansion counts in boxed math."
  [coll]
  (boolean (seq coll)))

(defn- all-distinct? [coll] (apply distinct? coll))

(defn- default-in-domain?
  "A `:default` is a value of the field's OWN type, already coerced. This is the
   invariant a surface leans on when it paints a dialog before the human has
   touched it: the slider's knob starts inside its track, the picker starts on
   an option that exists, and a checkbox starts on a boolean rather than on the
   string `\"false\"`."
  [{:keys [type default] :as field}]
  (let [{lo :min hi :max :keys [min-length max-length]}
        field

        chosen
        (option-values field)]

    (cond (nil? default) true
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

;; A field — the leaf that holds one answer

(s/def ::id non-blank-string?)
(s/def ::branch non-blank-string?)
(s/def ::name non-blank-string?)
;; One dispatch key, two vocabularies: a form field and a live node can never be
;; mistaken for each other, because neither multimethod has a method for the
;; other's type.
(s/def ::type (set (concat (vals field-types) (vals live-node-types))))
(s/def ::label non-blank-string?)
(s/def ::description non-blank-string?)
(s/def ::placeholder non-blank-string?)
(s/def ::is-required boolean?)
(s/def ::is-secret boolean?)
(s/def ::default some?)
(s/def ::validate (s/and (s/coll-of ifn? :kind vector?) non-empty?))
(s/def ::min-length pos-int?)
(s/def ::max-length pos-int?)
(s/def ::min number?)
(s/def ::max number?)
(s/def ::step number?)
(s/def ::direction (set (vals group-directions)))
;; One key, two domains: an OPTION's value is the string an answer comes back
;; as, a live `:progress` or step value is a fraction of one. Each shape pins
;; its own side down, so the union here never loosens either.
;; One key, two domains: an OPTION's value is the string an answer comes back
;; as, a live `:progress` or step value is a fraction of one. A bare predicate,
;; not an `s/or`, because a conforming branch would reach every `s/and` that
;; follows as a tagged pair instead of the value itself.
(s/def ::value #(or (string? %) (number? %) (nil? %)))
(s/def ::text non-blank-string?)

(s/def ::option
  (s/and #(closed? option-keys %)
         (s/keys :req-un [::value ::label])
         ;; An option's value is what the answer map comes back holding.
         #(non-blank-string? (:value %))))

(s/def ::options
  (s/and (s/coll-of ::option :kind vector?)
         non-empty?
         ;; Two options sharing a value make one of them unpickable: whichever
         ;; the human chose, the answer names the first.
         #(apply distinct? (map :value %))))

(s/def ::value-field
  (s/and (s/keys :req-un [::id ::name ::type ::label ::is-required ::is-secret]
                 :opt-un [::description ::placeholder ::default ::validate])
         one-identity?
         secret-marked?
         default-in-domain?))

(def ^:private text-field
  (s/and #(closed? text-keys %)
         ::value-field
         (s/keys :opt-un [::min-length ::max-length])
         ordered-lengths?))

(def ^:private choice-field
  (s/and #(closed? choice-keys %)
         ::value-field
         (s/keys :req-un [::options])))

(def ^:private checkbox-field (s/and #(closed? value-keys %) ::value-field))

(def ^:private range-field
  (s/and #(closed? range-keys %)
         ::value-field
         (s/keys :req-un [::min ::max ::step])
         ascending-bounds?
         positive-step?))

(def ^:private otp-field
  (s/and #(closed? text-keys %)
         ::value-field
         ;; A code's width is not optional: the boxes are painted from it.
         (s/keys :req-un [::min-length ::max-length])
         ordered-lengths?
         otp-fits-boxes?))

(defmulti ^:private field-form "The form one field type must take once normalized." :type)

(defmethod field-form :plaintext [_] text-field)
(defmethod field-form :password [_] text-field)
(defmethod field-form :multiline [_] text-field)
(defmethod field-form :select [_] choice-field)
(defmethod field-form :multiselect [_] choice-field)
(defmethod field-form :checkbox [_] checkbox-field)
(defmethod field-form :range [_] range-field)
(defmethod field-form :otp [_] otp-field)

(s/def ::field (s/multi-spec field-form :type))

;; A group — the control flow above the fields
;;
;; A request carries a TREE of three kinds of node, not a list of ten kinds of
;; field: a group ARRANGES children, a decoration is READ, a field HOLDS one
;; value. [[::node]] is where that choice is made — once, on the way in — and the
;; only place the three contracts meet.

(defn- grouped?
  "A group says so in its own `:type`. Every surface walks the tree by that key,
   so a layout node that lost it is drawn as a field."
  [{:keys [type]}]
  (= group-type type))

(s/def ::group
  (s/and #(closed? group-keys %)
         grouped?
         (s/keys :req-un [::id ::name ::direction ::fields] :opt-un [::label ::description])
         one-identity?))

;; A decoration — ink that answers nothing

(s/def ::decor
  ;; No `:id` and no `:name`: a decoration is never keyed, so it carries its own
  ;; type and the words it paints, and refusing everything else is what keeps a
  ;; disabled-looking field out of the vocabulary of answers.
  (s/and #(closed? decor-keys %)
         decoration?
         (s/keys :req-un [::text])))

;; A node — the one place the three contracts meet

(defmulti ^:private node-form
  "Which of the three contracts one node of the field tree answers to."
  :type)

(defmethod node-form group-type [_] ::group)

;; The decoration vocabulary is [[decor-types]] and lives in ONE place, so the
;; fallback asks it instead of listing `:heading` and `:paragraph` again here.
(defmethod node-form :default [node] (if (decoration? node) ::decor ::field))

(s/def ::node (s/multi-spec node-form :type))

(s/def ::fields (s/and (s/coll-of ::node :kind vector?) non-empty?))

;; A request

(s/def ::title non-blank-string?)
(s/def ::source non-blank-string?)
(s/def ::session-id non-blank-string?)
(s/def ::submit-label non-blank-string?)
(s/def ::cancel-label non-blank-string?)
(s/def ::is-cancellable boolean?)
(s/def ::timeout-ms nat-int?)

(s/def ::channel-ids
  ;; One id twice would open the same dialog twice and answer it once.
  (s/and (s/coll-of keyword? :kind vector?)
         non-empty?
         all-distinct?))

(defn- field-names
  [fields]
  (mapcat (fn [node]
            ;; A decoration has no `:name` — nothing keys it — so it contributes
            ;; nothing to key uniqueness either.
            (cond-> (field-names (:fields node))
              (:name node)
              (conj (:name node))))
          fields))

(defn- distinct-names?
  "Names are the keys of the answer map, across groups too: a collision loses a
   value silently."
  [{:keys [fields]}]
  (let [names (field-names fields)]
    (= (count names) (count (set names)))))

(s/def ::request
  (s/and #(closed? request-keys %)
         (s/keys :req-un [::id ::title ::fields ::submit-label ::cancel-label ::is-cancellable
                          ::timeout-ms ::channel-ids]
                 :opt-un [::description ::source ::session-id])
         distinct-names?))

;; An answer

(s/def ::is-submitted boolean?)
;; A form settles with a one-line reason a human reads; a live view ends on one
;; of [[live-reasons]]. Each shape pins its own side down below.
(s/def ::reason #(or (non-blank-string? %) (keyword? %)))
(s/def ::request-id non-blank-string?)

(s/def ::answer-value
  (s/nilable (s/or :text string?
                   :flag boolean?
                   :number number?
                   :choices (s/coll-of string? :kind vector?))))

(s/def ::values (s/map-of non-blank-string? ::answer-value))

(defn- values-iff-submitted?
  "`:values` is the proof the human answered. A cancelled or timed-out request
   carries none, so a caller cannot read a half-filled form as a submission."
  [{:keys [is-submitted values]}]
  (if is-submitted (map? values) (nil? values)))

(s/def ::answer
  (s/and #(closed? answer-keys %)
         (s/keys :req-un [::is-submitted ::reason ::request-id] :opt-un [::values])
         #(string? (:reason %))
         values-iff-submitted?))

;; An answered VALUE — the field's own domain
;;
;; [[::answer-value]] says what a value may LOOK like; these say what it may BE
;; for the field that asked the question. Coercion is hand-written per type and
;; nothing declared what it produces, so a `:select` could come back on an
;; option nobody offered, a slider outside its own track, a `:password` as the
;; plaintext a transcript must never hold. Each domain is built FROM the field,
;; so the declaration cannot drift from the question.

(defn- typed-in-domain?
  "Blank is how a surface says the human left a text field alone, and only an
   OPTIONAL field may be left alone. A value that IS there fits the declared
   length bounds — a blank one is not a short one, exactly as the parser reads
   it."
  [{:keys [is-required min-length max-length]} value]
  (cond (nil? value) (not is-required)
        (str/blank? value) (not is-required)
        :else (and (or (nil? min-length) (<= (long min-length) (count value)))
                   (or (nil? max-length) (<= (count value) (long max-length))))))

(defn- secret-in-domain?
  "A `:password` and an `:otp` both answer with a vault HANDLE. The plaintext has
   no business in an answer map at all, and the length bounds — or the digit
   width of a code — describe what the human typed rather than the handle it
   became."
  [{:keys [is-required]} value]
  (if (nil? value) (not is-required) (secret-handle? value)))

(defn- selected-in-domain?
  [{:keys [is-required] :as field} value]
  (if (nil? value) (not is-required) (contains? (option-values field) value)))

(defn- picked-in-domain?
  "Every pick is an option that exists, named once; a required multiselect needs
   at least one of them."
  [{:keys [is-required] :as field} value]
  (let [chosen (option-values field)]
    (and (every? chosen value)
         (= (count value) (count (set value)))
         (or (non-empty? value) (not is-required)))))

(defn- ticked-in-domain?
  "A required checkbox is a consent box: `false` is not an answer to it."
  [{:keys [is-required]} value]
  (or (true? value) (not is-required)))

(defn- slid-in-domain? [{lo :min hi :max} value] (<= (double lo) (double value) (double hi)))

(defmulti ^:private answer-form
  "The form ONE answered value must take, derived from the very field that asked
   for it: its own options, its own track, its own width."
  :type)

(defmethod answer-form :plaintext
  [field]
  (s/and (s/nilable string?) (partial typed-in-domain? field)))

(defmethod answer-form :multiline
  [field]
  (s/and (s/nilable string?) (partial typed-in-domain? field)))

(defmethod answer-form :password
  [field]
  (s/and (s/nilable string?) (partial secret-in-domain? field)))

(defmethod answer-form :select
  [field]
  (s/and (s/nilable string?) (partial selected-in-domain? field)))

(defmethod answer-form :multiselect
  [field]
  (s/and (s/coll-of string? :kind vector?) (partial picked-in-domain? field)))

(defmethod answer-form :checkbox [field] (s/and boolean? (partial ticked-in-domain? field)))

(defmethod answer-form :range [field] (s/and number? (partial slid-in-domain? field)))

(defmethod answer-form :otp [field] (s/and (s/nilable string?) (partial secret-in-domain? field)))

(defn- answerable
  "Every field that holds an ANSWER. A group is control flow: it never appears in
   a values map, its children answer in its place. A decoration answers nothing
   at all and has no children to answer for it."
  [fields]
  (mapcat (fn [{:keys [type] :as node}]
            (cond (= group-type type) (answerable (:fields node))
                  (decoration? node) []
                  :else [node]))
          fields))

;; A live view — the second kind of interaction
;;
;; Everything below is the SAME skeleton as a request: closed maps, engine
;; stamps the spec never writes, one dispatch key per multi-spec. The one axis
;; that differs is time — a view carries NODES that patches keep mutating, so
;; `::id` is an ADDRESS named by every patch rather than a key in an answer map.

(s/def ::tone (set (vals live-tones)))
(s/def ::detail non-blank-string?)
(s/def ::line string?)                                      ; a blank line is a line
;; A node's `:lines` are its hot WINDOW, so this bound is the window cap, not
;; the per-patch one: how many lines ONE patch may carry
;; (`:max-patch-lines`) is a REFUSAL the materializer makes, with `split it`
;; in the reason — a shape check cannot say that.
(s/def ::lines (s/coll-of ::line :kind vector? :max-count (long (:window-lines-cap log-defaults))))
;; The PAINT window; the sink keeps every line that was accepted.
(s/def ::window-lines (s/int-in 1 (inc (long (:window-lines-cap log-defaults)))))
(s/def ::done nat-int?)
(s/def ::total pos-int?)
(s/def ::path non-blank-string?)
(s/def ::language non-blank-string?)
(s/def ::align (set (vals live-aligns)))
(s/def ::cells (s/coll-of string? :kind vector?))
(s/def ::value-text string?)                                ; a stat's value AS SHOWN ("3.4 MB/s")
(s/def ::target non-blank-string?)                          ; attachment id, workspace path, or url
(s/def ::target-kind (set (vals link-targets)))
(s/def ::by non-blank-string?)
(s/def ::dir (set (vals live-sort-dirs)))
(s/def ::node-id non-blank-string?)                         ; the ADDRESS a patch speaks to
(s/def ::view-id non-blank-string?)
(s/def ::is-completed boolean?)
(s/def ::is-from-human boolean?)                            ; a PERSON stopped it, not the run
(s/def ::is-selectable boolean?)
(s/def ::selected-ids (s/and (s/coll-of ::id :kind vector?) #(= (count %) (count (distinct %)))))
(s/def ::note (s/and non-blank-string? #(<= (count %) (long note-chars))))
(s/def ::summary non-blank-string?)
(s/def ::total-lines nat-int?)                              ; a log's record since its last clear, engine-stamped
(s/def ::artifact-id non-blank-string?)

;; What a SETTLED view is: the record it wrote, addressed rather than copied.
(s/def ::media-type #{live-artifact-media-type})            ; one kind of artifact, one media type
(s/def ::audience
  ;; From `attachments/audiences`, narrowed to the ONE member a settled view may
  ;; carry: it is human-only. The model is told the artifact exists and reads
  ;; `::view`; a build log never reaches a provider request.
  #{"user"})
(s/def ::ended-at pos-int?)                                 ; epoch millis the verdict was sealed at
(s/def ::storage-uri (s/and non-blank-string? #(str/includes? % "://")))
(s/def ::size nat-int?)                                     ; bytes of the record the run wrote
(s/def ::line-count nat-int?)                               ; NDJSON lines of that record
(s/def ::base64 non-blank-string?)                          ; only under `live-artifact-inline-bytes`
(s/def ::error non-blank-string?)
(s/def ::op (set (vals live-ops)))
(s/def ::after (s/nilable ::node-id))                       ; place it after this node; nil means last
(s/def ::item-ids (s/and (s/coll-of ::id :kind vector?) non-empty?))
(s/def ::max-rows (s/int-in 1 (inc (long (:max-rows table-defaults)))))  ; the REFUSAL bound, not a ring

(defn- fraction?
  "A progress value: a fraction of one, or nil for INDETERMINATE. Zero is a
   started job, nil is a job whose size nobody knows — a surface paints them
   differently, so they may not be spelled the same."
  [x]
  (or (nil? x) (and (number? x) (<= 0 (double x) 1))))

(defn- live-reason?
  "True when this is one of the five endings an extension may branch on."
  [x]
  (contains? (set (vals live-reasons)) x))

(s/def ::table-column
  (s/and #(closed? live-column-keys %) (s/keys :req-un [::id ::label] :opt-un [::align])))

(s/def ::columns
  (s/and (s/coll-of ::table-column :kind vector?)
         non-empty?
         #(apply distinct? (map :id %))))

(s/def ::row
  (s/and #(closed? live-row-keys %) (s/keys :req-un [::id ::cells] :opt-un [::tone ::branch])))
;; A node's `:rows` are everything the table HOLDS; `:max-patch-rows` is the
;; materializer's refusal, for the same reason `::lines` carries the window.
(s/def ::rows (s/coll-of ::row :kind vector? :max-count (long (:max-rows table-defaults))))

(s/def ::stat
  (s/and #(closed? live-stat-keys %) (s/keys :req-un [::id ::label ::value-text] :opt-un [::tone])))

(s/def ::stats (s/coll-of ::stat :kind vector? :max-count (long (:max-stats stat-defaults))))

(s/def ::live-step
  (s/and #(closed? live-step-keys %)
         (s/keys :req-un [::id ::label ::tone] :opt-un [::detail ::value])
         #(fraction? (:value %))))

(s/def ::steps (s/coll-of ::live-step :kind vector? :max-count (long (:max-steps step-defaults))))

(s/def ::link
  (s/and #(closed? live-link-keys %)
         (s/keys :req-un [::id ::label ::target-kind ::target] :opt-un [::tone])))

(s/def ::links (s/coll-of ::link :kind vector? :max-count (long (:max-links link-defaults))))


;; A table is a KEYED collection, so its paint order has to be DECLARED or the
;; terminal and the phone are free to disagree. `:insertion` (the default) keeps
;; first-seen order and an upsert NEVER moves a row — a row that changes stays
;; where the eye left it. `:newest-first` is insertion reversed (a live feed).
;; `{:by "col" :dir :asc|:desc}` sorts by one DECLARED column id, ties broken by
;; insertion order so the order is TOTAL and reproducible on every surface.
(defn- sorted-order?
  "A `{:by \"col\"}` order, written as a bare predicate: an `s/or` here would
   CONFORM the value to a tagged pair, and every check that follows — including
   the one that asks whether the column exists — would then be handed the tag
   instead of the order."
  [x]
  (and (map? x)
       (closed? live-sorted-keys x)
       (non-blank-string? (:by x))
       (or (nil? (:dir x)) (contains? (set (vals live-sort-dirs)) (:dir x)))))

(s/def ::order #(or (contains? (set (vals live-orders)) %) (sorted-order? %)))

(defmulti ^:private live-node-form "The form one live node type takes once normalized." :type)

(defmethod live-node-form :status
  [_]
  (s/keys :req-un [::id ::type ::text ::tone] :opt-un [::label ::detail]))
(defmethod live-node-form :progress
  [_]
  (s/and (s/keys :req-un [::id ::type] :opt-un [::label ::value ::done ::total])
         #(fraction? (:value %))))
(defmethod live-node-form :stat [_] (s/keys :req-un [::id ::type ::stats] :opt-un [::label]))
(defmethod live-node-form :steps [_] (s/keys :req-un [::id ::type ::steps] :opt-un [::label]))
(defmethod live-node-form :log
  [_]
  (s/keys :req-un [::id ::type ::lines ::window-lines] :opt-un [::label ::total-lines]))
(defmethod live-node-form :table
  [_]
  (s/keys :req-un [::id ::type ::columns ::rows ::max-rows ::order]
          :opt-un [::label ::is-selectable ::selected-ids]))
(defmethod live-node-form :link [_] (s/keys :req-un [::id ::type ::links] :opt-un [::label]))
;; Layout is the request's own group, so a view arranges its work with the
;; vocabulary the form already speaks. `::type` is deliberately absent from the
;; keys checked here, exactly as it is on [[::group]]: `:group` belongs to the
;; layout vocabulary, not to either list of leaves.
(defmethod live-node-form group-type
  [_]
  (s/and #(closed? live-group-keys %)
         (s/keys :req-un [::id ::direction :live-view/fields] :opt-un [::label])))

(defn- live-node-typed?
  "A live node says so in its own `:type`: one of [[live-node-types]], or the
   form's [[group-type]] when the node only ARRANGES the ones inside it. Spelled
   out because [[::type]] is the union of both vocabularies — the one key both
   kinds of node dispatch on — and nothing but this stops a form field from being
   read as a node of a view."
  [{:keys [type]}]
  (or (contains? (set (vals live-node-types)) type) (= group-type type)))

(defn- ordered-by-declared-column?
  "A `{:by \"col\"}` order names a column the table DECLARES. Refused here, at
   declaration, rather than ignored at paint time three surfaces later."
  [{:keys [type columns order]}]
  (or (not= :table type) (not (map? order)) (contains? (set (map :id columns)) (:by order))))

(defn- selection-belongs-to-table?
  "Selection is control state of a selectable TABLE, and can only name rows the table
   still holds. Other node types cannot carry either table-only key."
  [{:keys [type rows is-selectable selected-ids] :as node}]
  (if (= :table type)
    (and (or (not (contains? node :selected-ids)) (true? is-selectable))
         (every? (set (map :id rows)) selected-ids))
    (and (not (contains? node :is-selectable)) (not (contains? node :selected-ids)))))

;; The children of a live group are live NODES, while [[::fields]] is the FORM
;; tree's children — so a group's `:fields` is spelled under its own spec name
;; and `s/keys` checks it against the right vocabulary.
(s/def :live-view/fields (s/and (s/coll-of ::live-node :kind vector?) non-empty?))

(defn- layout-only-when-grouped?
  "Only a group arranges. `:direction` or `:fields` on a leaf would be layout no
   painter reads, so it is refused where it was written rather than dropped."
  [{:keys [type] :as node}]
  (or (= group-type type) (and (not (contains? node :direction)) (not (contains? node :fields)))))

(s/def ::live-node
  (s/and #(closed? live-node-keys %)
         live-node-typed?
         layout-only-when-grouped?
         (s/multi-spec live-node-form :type)
         ordered-by-declared-column?
         selection-belongs-to-table?))

(defn- live-tree
  "Every node of a view depth first, the children of a layout group included. The
   two laws that hold across the WHOLE view — ids are unique, and a view may only
   declare so many nodes — count the tree rather than the top row."
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cons node (live-tree (:fields node)))))
        nodes))

;; `::id` is the ADDRESS: chosen by the extension, unique inside the view, and
;; named by every patch. Two tables are two ids, not two views. A group is
;; addressed the same way: `add-node :after` and `remove-node` name it by id.
(s/def ::nodes
  (s/and (s/coll-of ::live-node :kind vector?)
         #(<= (count (live-tree %)) (long (:max-nodes view-defaults)))
         #(or (empty? %) (apply distinct? (map :id (live-tree %))))))

(s/def ::live-view
  (s/and #(closed? live-view-keys %)
         (s/keys :req-un [::id ::title ::channel-ids ::nodes ::timeout-ms ::seq ::created-at]
                 :opt-un [::description ::source ::session-id])
         #(seq (:nodes %))))

;; One patch. `:seq` is monotonic PER VIEW, so a surface that sees a gap re-reads
;; the snapshot instead of painting a torn view.

(s/def ::node-spec ::live-node)

(defmulti ^:private live-op-form "The form one patch operation takes." :op)

(defmethod live-op-form :set
  [_]
  (s/keys :req-un [::op ::node-id]
          :opt-un [::text ::detail ::tone ::label ::value ::done ::total ::stats ::steps
                   ::selected-ids ::links]))
(defmethod live-op-form :append
  [_]
  (s/keys :req-un [::op ::node-id] :opt-un [::lines ::rows ::stats ::steps ::links]))
(defmethod live-op-form :remove [_] (s/keys :req-un [::op ::node-id ::item-ids]))
(defmethod live-op-form :clear [_] (s/keys :req-un [::op ::node-id]))
(defmethod live-op-form :add-node [_] (s/keys :req-un [::op ::node-spec] :opt-un [::after]))
(defmethod live-op-form :remove-node [_] (s/keys :req-un [::op ::node-id]))

(s/def ::live-op
  (s/and #(closed? (get live-op-key-sets (:op %) #{}) %) (s/multi-spec live-op-form :op)))
(s/def ::ops (s/and (s/coll-of ::live-op :kind vector?) non-empty?))
(s/def ::live-patch (s/and #(closed? live-patch-keys %) (s/keys :req-un [::view-id ::seq ::ops])))

;; The PICTURE: a finished view as data, with the mount bookkeeping left behind.
;; What the verdict hands the model and what a parsed document answers, so one
;; shape crosses in both directions.
(s/def ::view
  (s/and #(closed? live-picture-keys %) (s/keys :req-un [::title ::nodes] :opt-un [::description])))

;; What a budget left behind, per node. Never a key ON the node: truncation is a
;; RENDER artifact and the record still holds every item.
(s/def ::items pos-int?)

(s/def ::elided
  (s/coll-of (s/and #(closed? live-elided-keys %) (s/keys :req-un [::node-id ::items]))
             :kind vector?))
;; What the blocked extension receives, and what the close event carries. It is
;; deliberately NOT an `::answer`: a verdict carries no field values, and a form
;; carries no artifact, so merging them would leave half of every map nil.

(s/def ::live-result
  (s/and #(closed? live-result-keys %)
         (s/keys :req-un [::view-id ::is-completed ::reason ::is-from-human ::view]
                 :opt-un [::note ::summary ::artifact-id ::error ::elided])
         #(live-reason? (:reason %))
         ;; The cross-key rules: completion is exactly the `completed` ending, so
         ;; nothing reports success while naming why it stopped; only a HUMAN
         ;; leaves a note, and a human's stop is always `interrupted`.
         #(= (:is-completed %) (= :completed (:reason %)))
         #(or (:is-from-human %) (not (contains? % :note)))
         #(or (not (:is-from-human %)) (= :interrupted (:reason %)))))

;; The SETTLED view: reachability after the pane is gone. Nothing here copies a
;; buffer — the sink has been the record since `open`, so the artifact addresses
;; that file and carries the final materialized state a surface opens instantly.
(s/def ::live-artifact
  (s/and #(closed? live-artifact-keys %)
         (s/keys :req-un [::id ::view-id ::session-id ::title ::media-type ::audience ::ended-at
                          ::reason ::view ::storage-uri ::size ::line-count]
                 :opt-un [::base64])
         #(live-reason? (:reason %))
         ;; Inlining is a SIZE decision, never a preference: past the floor the
         ;; bytes stay on disk, which is the whole point of addressing the record.
         #(or (not (contains? % :base64)) (<= (long (:size %)) (long live-artifact-inline-bytes)))))
;; Explaining a violation

(defn- brief
  [x]
  (let [text (pr-str x)]
    (if (> (count text) 88) (str (subs text 0 88) "…") text)))

(defn- problem-str
  [{:keys [in val pred]}]
  (str (when (seq in) (str (str/join " -> " (map brief in)) ": "))
       (brief val)
       " fails "
       (brief pred)))

(defn- error
  "nil when `x` satisfies `spec`, else a one-line reason naming the first
   couple of problems — short enough to ride in an error message a human reads
   in a dialog."
  [spec x]
  (when-let [problems (::s/problems (s/explain-data spec x))]
    (str/join "; " (map problem-str (take 2 problems)))))

(defn field-error
  "nil when `field` is a legal normalized FIELD — a leaf holding one answer —
   else why it is not. A layout group is not a field ([[group-error]]) and
   neither is a decoration ([[decor-error]])."
  [field]
  (error ::field field))

(defn group-error
  "nil when `group` is a legal normalized layout group, else why it is not. The
   children it arranges are checked as the nodes they are, so one call covers
   the whole subtree."
  [group]
  (error ::group group))

(defn decor-error
  "nil when `decor` is a legal normalized decoration, else why it is not. The
   three node contracts are checked apart, so each refuses the other two: a
   heading carrying a `:default` is a spec that meant to ask something."
  [decor]
  (error ::decor decor))

(defn request-error
  "nil when `request` is a legal normalized request, else why it is not."
  [request]
  (error ::request request))

(defn- values-error
  "nil when `values` answers EXACTLY the answerable `fields` — no field left out,
   none invented — with every value inside the domain its own field declared."
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
                (when-let [why (error (answer-form (get by-name field-name)) value)]
                  (str field-name ": " why)))
              values))))

(defn answer-error
  "nil when `answer` is a legal answer to hand a blocked extension, else why it
   is not.

   `fields` are the fields of the request being answered: a SUBMITTED answer is
   checked against them too, so a value can only reach an extension for a field
   that asked for it and only inside that field's own domain. nil `fields` means
   the request settled while this answer was in flight — nobody is left to read
   it, so its own shape is all there is to check."
  [fields answer]
  (or (error ::answer answer)
      (when (and (seq fields) (:is-submitted answer)) (values-error fields (:values answer)))))

(defn live-node-error
  "nil when `node` is a legal normalized LIVE node, else why it is not. A form
   field is not a live node and refuses here, because the two multimethods share
   one dispatch key and neither answers for the other's types."
  [node]
  (error ::live-node node))

(defn live-view-error
  "nil when `view` is a legal normalized live view, else why it is not."
  [view]
  (error ::live-view view))

(defn live-patch-error
  "nil when `patch` is a legal patch, else why it is not. Shape only: whether the
   node it names EXISTS, and whether it would cross a bound, is the
   materializer's answer — this one is about vocabulary."
  [patch]
  (error ::live-patch patch))

(defn live-result-error
  "nil when `result` is a legal verdict to hand a blocked extension, else why it
   is not."
  [result]
  (error ::live-result result))

(defn live-artifact-error
  "nil when `artifact` is a legal settled view, else why it is not."
  [artifact]
  (error ::live-artifact artifact))
