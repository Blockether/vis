(ns com.blockether.vis.human-input-test
  "The public form builders — `com.blockether.vis.human-input`.

   Two promises are tested here and nothing else: a builder returns the plain
   spec map an extension could have typed by hand, and a mistake in it is
   refused AT THE BUILDER CALL with the engine's own one-line reason instead of
   in front of the human."
  (:require [com.blockether.vis.human-input :as hi]
            [com.blockether.vis.internal.human-input :as engine]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- refusal
  "The one-line reason `f` was refused, or nil when it was accepted."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(def ^:private deploy-form
  #(hi/form
     {:title "Deploy" :description "Where this build lands." :submit-label "Ship it" :timeout-ms 0}
     (hi/heading "Target")
     (hi/paragraph "Staging pages nobody.")
     (hi/row (hi/select "env"
                        ["staging" "prod"]
                        {:label "Environment" :is-required true :default "staging"})
             (hi/slider "canary" {:label "Canary %" :min 0 :max 100 :step 5 :default 10}))
     (hi/column (hi/plaintext "who" {:label "Deployer" :max-length 40})
                (hi/multiline "note" {:placeholder "Why now?"})
                (hi/multiselect "regions" [(hi/option "eu" "Europe") (hi/option "us")])
                (hi/otp "code" {:min-length 6 :max-length 6})
                (hi/checkbox "ack" {:label "I read the runbook" :is-required true}))
     (hi/password "token" {:label "Deploy token" :is-required true})))

(defdescribe
  builders-test
  (describe
    "shape"
    (it "returns the plain map an extension could have typed by hand"
        (expect (= {:type "plaintext" :name "who" :label "Deployer"}
                   (hi/plaintext "who" {:label "Deployer"})))
        (expect (= {:type "password" :name "token"} (hi/password "token")))
        (expect (= {:type "multiline" :name "note"} (hi/multiline "note")))
        (expect (= {:type "checkbox" :name "ack"} (hi/checkbox "ack")))
        (expect (= {:type "otp" :name "code"} (hi/otp "code")))
        (expect (= {:type "select" :name "env" :options ["a" "b"]} (hi/select "env" ["a" "b"])))
        (expect (= {:type "multiselect" :name "tags" :options ["a"]}
                   (hi/multiselect "tags" ["a"]))))
    (it "spells the range field `slider`, so neither mirror shadows a builtin"
        (expect (= {:type "range" :name "canary" :max 100} (hi/slider "canary" {:max 100}))))
    (it "builds an option with and without its label"
        (expect (= {:value "eu"} (hi/option "eu")))
        (expect (= {:value "eu" :label "Europe"} (hi/option "eu" "Europe"))))
    (it "nests groups, and a group carries only its direction and children"
        (expect (= {:type "group"
                    :direction "row"
                    :fields
                    [{:type "group" :direction "column" :fields [{:type "plaintext" :name "a"}]}]}
                   (hi/row (hi/column (hi/plaintext "a"))))))
    (it "makes a decoration ink: a type, a text, and no name at all"
        (expect (= {:type "heading" :text "Target"} (hi/heading "Target")))
        (expect (= {:type "paragraph" :text "Prose."} (hi/paragraph "Prose."))))
    (it "assembles a request whose :fields are exactly the nodes, in order"
        (let [request (deploy-form)]
          (expect (= "Deploy" (:title request)))
          (expect (= "Ship it" (:submit-label request)))
          (expect (= ["heading" "paragraph" "group" "group" "password"]
                     (mapv :type (:fields request)))))))
  (describe "the engine is the only judge"
            (it "accepts the whole assembled request, ink and groups and all"
                (let [nodes (:fields (engine/normalize-request (deploy-form)))]
                  (expect (nil? (hi/check (deploy-form))))
                  ;; ink is nameless even after normalization; only fields can be answered
                  (expect (= [nil nil] (mapv :name (take 2 nodes))))
                  (expect (= "token" (:name (last nodes))))))
            (it "dates a bad :default to the builder call that made it"
                (expect (re-find #"^Invalid human-input field env: invalid :default"
                                 (refusal
                                   #(hi/select "env" ["staging" "prod"] {:default "nope"})))))
            (it "refuses a select with nothing to choose from"
                (expect (= "Invalid human-input field env: select needs at least one option"
                           (refusal #(hi/select "env" [])))))
            (it "refuses an upside-down slider track"
                (expect (= "Invalid human-input field canary: :max must be greater than :min"
                           (refusal #(hi/slider "canary" {:min 5 :max 2})))))
            (it "refuses a key that field type never had"
                (expect (re-find #"unknown field key :required"
                                 (refusal #(hi/plaintext "who" {:required true})))))
            (it "refuses ink with nothing to paint" (expect (some? (refusal #(hi/heading "   ")))))
            (it "refuses a group whose child is not a node"
                (expect (some? (refusal #(hi/row {:type "plaintxt" :name "who"})))))
            (it "refuses a request with no title, and one with no nodes"
                (expect (= "Invalid human-input request: request needs a non-blank :title"
                           (refusal #(hi/form {} (hi/plaintext "who")))))
                (expect (some? (refusal #(hi/form {:title "Deploy"})))))
            (it "refuses two fields answering to the same name, however deeply nested"
                (expect (= "Invalid human-input request: field names must be distinct"
                           (refusal #(hi/form {:title "Deploy"}
                                              (hi/plaintext "who")
                                              (hi/row (hi/column (hi/password "who")))))))))
  (describe
    "check"
    (it "answers instead of throwing, on data nobody built with a builder"
        (expect (nil? (hi/check {:title "Deploy" :fields [{:type "plaintext" :name "who"}]})))
        (expect (re-find #"unknown type \"plaintxt\""
                         (hi/check {:title "Deploy" :fields [{:type "plaintxt" :name "who"}]})))
        (expect (some? (hi/check {:fields [{:type "plaintext" :name "who"}]})))
        (expect (some? (hi/check {:title "Deploy" :fields []})))
        (expect (some? (hi/check nil))))
    (it "reads the wire spelling of every key too"
        (expect (nil? (hi/check {"title" "Deploy"
                                 "fields" [{"type" "select"
                                            "name" "env"
                                            "options" [{"value" "a"}]
                                            "is_required" true}]}))))))
