(ns com.blockether.vis.contract.view
  "Canonical View vocabulary shared by Core and every language SDK.

   `vis-contract/view.edn` is the source for both View kinds: answer fields,
   semantic live nodes and patch operations, lifecycle actions and the bounds
   every renderer must enforce. This namespace validates that closed document,
   derives Clojure keyword values for the engine and renders the portable JSON
   section without requiring any Vis implementation namespace."
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
