(ns com.blockether.vis.contract.view-test
  "View wire lifecycle is the same input/live contract in SDK, gateway and UI."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.contract.view :as contract]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  shared-view-lifecycle-test
  (it "shared view lifecycle"
      (let [samples (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json")))]
        (doseq [[sample definition] [["input" "input_view"] ["live" "live_view"]
                                     ["patch" "live_patch"] ["answer" "answer"]
                                     ["input_result" "input_result"] ["result" "live_result"]]]
          (expect (document/valid-json? "view" definition (get samples sample)))
          (expect (not (document/valid-json? "view"
                                             definition
                                             (assoc (get samples sample) "legacy" true)))))
        (doseq [action [{"action" "submit" "values" {"name" "Ada"}} {"action" "cancel"}
                        {"action" "select" "node_id" "table" "item_ids" ["one"]}
                        {"action" "interrupt" "note" "Stop"}]]
          (expect (document/valid-json? "view" "operator_action" action))
          (expect
            (not (document/valid-json? "view" "operator_action" (assoc action "legacy" true))))))))

(defdescribe input-view-fixture-is-the-engine-projection
             (it "input view fixture is the engine projection"
                 ;; The real SDK flow exposed a false fixture: request includes channel routing,
                 ;; whereas request->view removes it and carries the engine's created_at stamp.
                 (let [sample
                       (get (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json")))
                            "input")

                       projected
                       (-> {:id "input-one"
                            :title "Name"
                            :timeout-ms 30000
                            :nodes [{:type "plaintext" :id "name" :label "Name" :is-required true}]}
                           view/normalize-request
                           (assoc :created-at 1)
                           view/request->view
                           wire/->wire)]

                   (expect (= sample projected))
                   (expect (document/valid-json? "view" "input_view" projected))
                   (expect (not (document/valid-json? "view"
                                                      "input_view"
                                                      (assoc projected "channel_ids" ["app"])))))))

(defdescribe every-input-field-crosses-the-real-projection
             (it "every input field crosses the real projection"
                 (doseq [type ["checkbox" "multiline" "multiselect" "otp" "password" "plaintext"
                               "range" "select"]]
                   (let [field (cond-> {:type type :id "field" :label "Value"}
                                 (#{"select" "multiselect"} type)
                                 (assoc :options ["one" "two"]))
                         projected (-> {:title "Fields" :nodes [{:type "group" :children [field]}]}
                                       view/normalize-request
                                       (assoc :created-at 1)
                                       view/request->view
                                       wire/->wire)]

                     (expect (document/valid-json? "view" "input_view" projected))
                     (doseq [[key value] [["is_secret" false] ["validate" ["callback"]]]]
                       (expect (not (document/valid-json? "view"
                                                          "input_view"
                                                          (assoc-in projected
                                                            ["nodes" 0 "children" 0 key]
                                                            value)))))))))

(defdescribe
  view-schema-is-the-source-of-runtime-vocabulary
  (it
    "view schema is the source of runtime vocabulary"
    (let [schema
          (document/schema-document "view")

          components
          (get-in schema ["$defs" "component" "oneOf"])

          fields
          (filter #(contains? (get % "properties") "is_required") components)

          names
          #(set (map (fn [shape]
                       (get-in shape ["properties" "type" "const"]))
                     %))]

      (expect (nil? (io/resource "vis-contract/view.json")))
      (expect (= (names fields) (set (keys contract/field-types))))
      (expect (= (names components) (set (keys contract/component-types))))
      (expect (= (apply disj (names components) "group" (names fields))
                 (set (keys contract/live-node-types))))
      (expect (= #{:otp :password} contract/secret-types))
      (expect (= #{:multiline :password :plaintext} contract/text-types))
      (expect (= #{:multiselect :select} contract/choice-types))
      (expect (= {:min 0 :max 100 :step 1} contract/range-defaults))
      (expect (= {:length 6 :ceiling 12} contract/otp-defaults))
      (expect (= #{:id :seq :created-at :owner :extension :is-stale :submissions :is-closed
                   :is-archived :settings-key :log}
                 contract/live-view-stamp-keys))
      (expect (= #{:created-at :origin} contract/request-stamp-keys))
      (expect (= #{:is-secret} contract/derived-keys))
      (doseq [[variant frames] contract/spinner-frames]
        (expect (document/valid-json? "view" "spinner_variant" (name variant)))
        (expect (= 10 (count frames))))
      (doseq [sample ["input" "live"]]
        (expect (document/valid? "view"
                                 (get (json/read-json (slurp (io/resource
                                                               "vis-contract/fixtures/view.json")))
                                      sample))))
      (expect (not (document/valid? "view" {"field_types" ["plaintext"]}))))))

(defdescribe schema-derived-key-sets-cover-each-real-operator
             (it "schema derived key sets cover each real operator"
                 (let [schema (document/schema-document "view")]
                   (doseq [[definition discriminator actual]
                           [["operator_action" "action" contract/view-action-key-sets]
                            ["live_op" "op" contract/live-op-key-sets]]
                           shape (get-in schema ["$defs" definition "oneOf"])]

                     (expect (= (set (keys (wire/->engine (get shape "properties"))))
                                (get actual
                                     (keyword (get-in shape
                                                      ["properties" discriminator "const"])))))))))

(defn- fixture-picture
  [wire]
  (into {}
        (map (fn [[k v]]
               [(keyword (str/replace k "_" "-")) v]))
        wire))

(defdescribe
  picture-test
  ;; Review audit (goal 4b41c770): the engine and the SDK checked picture bytes
  ;; differently, and a WAV file passed as WebP. Both read fixtures/app-pictures.json.
  (let [fixture (json/read-json (slurp (io/resource "vis-contract/fixtures/app-pictures.json")))]
    (it "reads the size of each accepted picture from its header"
        (doseq [{:strs [name picture width height]} (get fixture "accepted")]
          (expect (= {:width width :height height}
                     (contract/read-picture (fixture-picture picture)))
                  name)))
    (it "refuses each refused picture"
        (doseq [{:strs [name picture]} (get fixture "refused")]
          (expect (string? (:error (contract/read-picture (fixture-picture picture)))) name)))
    (it "refuses a view whose pictures pass the byte bound"
        (let [most (long (:view-bytes contract/picture-bounds))
              half (apply str (repeat (* 4 (inc (quot most 6))) "A"))
              image (fn [id]
                      {:id id :type "image" :data half})]

          (expect (nil? (contract/picture-budget-error {:nodes [(image "a")]})))
          (expect (string? (contract/picture-budget-error
                             {:nodes [(image "a")
                                      {:id "row" :type "row" :children [(image "b")]}]}))
                  "a picture inside a layout group counts too")))
    (it "refuses data over the length bound before it decodes it"
        ;; Review audit (goal 4b41c770): a valid picture with 4 MB of padding was
        ;; decoded and accepted.
        (let [png (some (fn [{:strs [picture]}]
                          (when (= "image/png" (get picture "media_type")) (get picture "data")))
                        (get fixture "accepted"))
              most (long (:data-chars contract/picture-bounds))
              data (str png (apply str (repeat (inc (quot (- most (count png)) 4)) "AAAA")))]

          (expect (re-find #"characters; the most is"
                           (str (:error (contract/read-picture {:media-type "image/png"
                                                                :data data})))))))))
