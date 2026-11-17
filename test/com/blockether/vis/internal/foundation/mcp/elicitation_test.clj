(ns com.blockether.vis.internal.foundation.mcp.elicitation-test
  (:require [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.foundation.mcp.elicitation :as elicitation]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- person
  "A stand-in for `view/request!`: the person sees the real form and submits `typed`
   (field name -> raw value). The answer is the one the View gives for it. `seen`
   receives the form."
  [seen typed]
  (fn [request]
    (let [form
          (view/normalize-request request)

          checked
          (view/validate-values (:nodes form) typed)]

      (reset! seen form)
      (if (:is-accepted checked)
        {:is-submitted true :reason "submitted" :values (:values checked)}
        {:is-submitted false :reason "invalid" :errors (:errors checked)}))))

(defn- ask
  "The reply to a form elicitation with `schema` when the person submits `typed`,
   as the server gets it: action and content."
  ([schema typed] (ask schema typed (atom nil)))
  ([schema typed seen]
   (with-redefs [view/request! (person seen typed)]
     (dissoc (elicitation/ask! "session-1"
                               "tracker"
                               {"message" "Fill it in" "requestedSchema" schema})
       "fields"))))

(defdescribe
  form-elicitation-test
  (it
    "sends each answer in the JSON type of its property"
    (expect
      (= {"action" "accept"
          "content" {"name" "Ada" "age" 36 "score" 9.5 "admin" false "zone" "eu" "tags" ["a" "c"]}}
         (ask {"type" "object"
               "properties" (array-map "name" {"type" "string"}
                                       "age" {"type" "integer" "minimum" 0}
                                       "score" {"type" "number"}
                                       "admin" {"type" "boolean"}
                                       "zone" {"type" "string" "enum" ["eu" "us"]}
                                       "tags" {"type" "array"
                                               "items" {"anyOf" [{"const" "a" "title" "A"}
                                                                 {"const" "b" "title" "B"}
                                                                 {"const" "c" "title" "C"}]}})
               "required" ["name" "admin"]}
              {"name" "Ada" "age" "36" "score" "9.5" "admin" false "zone" "eu" "tags" ["a" "c"]}))))
  (it "leaves a blank optional answer out of the content"
      (expect (= {"action" "accept" "content" {"name" "Ada"}}
                 (ask {"type" "object"
                       "properties" (array-map "name" {"type" "string"}
                                               "nick" {"type" "string"}
                                               "age" {"type" "integer"})
                       "required" ["name"]}
                      {"name" "Ada" "nick" "" "age" ""}))))
  (it "does not send a number that the schema refuses"
      (expect (= {"action" "cancel"}
                 (ask {"type" "object"
                       "properties" {"age" {"type" "integer" "minimum" 18}}
                       "required" ["age"]}
                      {"age" "17"})))
      (expect (= {"action" "cancel"}
                 (ask {"type" "object" "properties" {"age" {"type" "integer"}} "required" ["age"]}
                      {"age" "1.5"}))))
  (it "shows the fields in the order of the server, also above eight fields"
      (let [fields
            (mapv #(str "field-" %) (range 12))

            seen
            (atom nil)]

        (ask {"type" "object"
              "properties" (apply array-map
                             (mapcat (fn [field]
                                       [field {"type" "string"}])
                                     fields))}
             {}
             seen)
        (expect (= fields (mapv :name (:nodes @seen))))))
  (it "does not put back a default that the person cleared"
      ;; The form shows each default, so a blank optional answer is the choice of the person.
      (let [schema
            {"type" "object"
             "properties" (array-map "name" {"type" "string"}
                                     "zone" {"type" "string" "default" "UTC"}
                                     "age" {"type" "integer" "default" 30})
             "required" ["name"]}

            reply
            (fn [typed]
              (get (#'mcp/elicitation-reply
                    {"id" 1 "params" {"message" "Fill it in" "requestedSchema" schema}}
                    (fn [params]
                      (with-redefs [view/request! (person (atom nil) typed)]
                        (elicitation/ask! "session-1" "tracker" params))))
                   "result"))]

        (expect (= {"action" "accept" "content" {"name" "Ada"}}
                   (reply {"name" "Ada" "zone" "" "age" ""})))
        (expect (= {"action" "accept" "content" {"name" "Ada" "zone" "UTC" "age" 30}}
                   (reply {"name" "Ada" "zone" "UTC" "age" "30"})))))
  (it "asks a schema with no field as one question"
      (expect (= {"action" "accept" "content" {}} (ask {"type" "object" "properties" {}} {}))))
  (it "declines when the person refuses and cancels when nobody answers"
      (with-redefs [view/request! (constantly {:is-submitted false :reason "cancelled"})]
        (expect (= {"action" "decline"}
                   (elicitation/ask! "session-1" "tracker" {"message" "?" "requestedSchema" {}}))))
      (with-redefs [view/request! (constantly {:is-submitted false :reason "timeout"})]
        (expect (=
                  {"action" "cancel"}
                  (elicitation/ask! "session-1" "tracker" {"message" "?" "requestedSchema" {}}))))))
