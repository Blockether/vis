(ns com.blockether.vis.human-input-test
  "The public builders — `com.blockether.vis.human-input`.

   Two promises are tested here and nothing else, for a form and for a live view
   alike: a builder returns the plain spec map an extension could have typed by
   hand, and a mistake in it is refused AT THE BUILDER CALL with the engine's own
   one-line reason instead of in front of the human."
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
    "a spec nobody built with a builder"
    (it "is refused by the engine on exactly the same terms"
        (expect (nil? (refusal #(engine/normalize-request
                                  {:title "Deploy" :fields [{:type "plaintext" :name "who"}]}))))
        (expect (re-find #"unknown type \"plaintxt\""
                         (refusal #(engine/normalize-request
                                     {:title "Deploy" :fields [{:type "plaintxt" :name "who"}]}))))
        (expect (some? (refusal #(engine/normalize-request {:fields [{:type "plaintext"
                                                                      :name "who"}]}))))
        (expect (some? (refusal #(engine/normalize-request {:title "Deploy" :fields []}))))
        (expect (some? (refusal #(engine/normalize-request nil)))))
    (it "reads the wire spelling of every key too"
        (expect (nil? (refusal #(engine/normalize-request {"title" "Deploy"
                                                           "fields" [{"type" "select"
                                                                      "name" "env"
                                                                      "options" [{"value" "a"}]
                                                                      "is_required" true}]}))))))
  (describe
    "validate"
    (it "keeps the validator itself in the map, in either shape"
        ;; A validator is a FUNCTION, and a function is not wire data: the builder
        ;; hands it straight back, and the engine calls it on the answered value
        ;; (one argument) or on the value and every value (two).
        (let [required
              (fn [value]
                (when (= "" value) "who?"))

              agrees
              (fn [value values]
                (when-not (= value (get values "who")) "must match"))]

          (expect (= {:type "plaintext" :name "who" :validate required}
                     (hi/plaintext "who" {:validate required})))
          (expect (= {:type "password" :name "token" :validate [required agrees]}
                     (hi/password "token" {:validate [required agrees]})))
          (expect (nil? (refusal #(engine/normalize-request
                                    (hi/form {:title "Deploy"}
                                             (hi/plaintext "who" {:validate required})
                                             (hi/password "token"
                                                          {:validate [required agrees]}))))))))
    (it "refuses at the builder call what is not a function at all"
        (expect (re-find #":validate takes a FUNCTION"
                         (refusal #(hi/plaintext "who" {:validate "nope"}))))
        (expect (re-find #":validate takes a FUNCTION"
                         (refusal #(hi/plaintext "who" {:validate ["nope"]})))))
    (it "refuses a function the dialog could never call"
        ;; It receives the value, so a validator that takes no argument is a bug in
        ;; the extension, caught on this line instead of at submit time.
        (expect (re-find #"takes neither"
                         (refusal #(hi/checkbox "ack"
                                                {:validate (fn []
                                                             nil)}))))
        (expect (re-find #"takes neither"
                         (refusal #(hi/plaintext "who"
                                                 {:validate (fn [a b c]
                                                              [a b c])})))))
    (it "says the same thing about a spec that never met a builder"
        (expect (re-find #":validate takes a FUNCTION"
                         (refusal #(engine/normalize-request {:title "Deploy"
                                                              :fields [{:type "plaintext"
                                                                        :name "who"
                                                                        :validate "nope"}]})))))))

;; Live views — the picture the human WATCHES while an extension works. Same two
;; promises: the plain map, and the mistake dated to the line that made it.

(def ^:private ci-view
  #(hi/view
     {:title "CI" :description "Blockether/vis · 42"}
     (hi/status "now" "Polling GitHub…" {:tone "running"})
     (hi/progress "done" {:done 3 :total 18 :label "Jobs"})
     (hi/stat "score" [{:id "passed" :label "passed" :value-text "3" :tone "ok"}])
     (hi/steps "checks" [{:id "checkout" :label "Checkout" :tone "ok"}])
     (hi/log "tail" {:window-lines 120})
     (hi/table "jobs"
               [(hi/table-column "job" "Job") (hi/table-column "took" "Took" {:align "right"})]
               {:order "newest-first"})
     (hi/link "run" [{:id "html" :label "Open the run" :target "https://example.com/run/42"}])))

(defdescribe
  live-builders-test
  (describe
    "shape"
    (it "returns the plain map an extension could have typed by hand"
        (expect (= {:type "status" :id "now" :text "Polling…"} (hi/status "now" "Polling…")))
        (expect (= {:type "status" :id "now" :text "Done" :tone "ok"}
                   (hi/status "now" "Done" {:tone "ok"})))
        (expect (= {:type "progress" :id "done" :done 3 :total 18}
                   (hi/progress "done" {:done 3 :total 18})))
        (expect (= {:type "log" :id "tail"} (hi/log "tail")))
        (expect (=
                  {:type "stat" :id "score" :stats [{:id "passed" :label "passed" :value-text "3"}]}
                  (hi/stat "score" [{:id "passed" :label "passed" :value-text "3"}])))
        (expect (= {:type "steps" :id "checks" :steps [{:id "s" :label "Set up" :tone "ok"}]}
                   (hi/steps "checks" [{:id "s" :label "Set up" :tone "ok"}])))
        (expect (= {:type "table" :id "jobs" :columns [{:id "job" :label "Job"}]}
                   (hi/table "jobs" [(hi/table-column "job" "Job")])))
        (expect (= {:type "link"
                    :id "run"
                    :links [{:id "html" :label "Run" :target "/tmp/run" :target-kind "path"}]}
                   (hi/link "run"
                            [{:id "html" :label "Run" :target "/tmp/run" :target-kind "path"}]))))
    (it "keys a column by its id and a row by the columns standing over its cells"
        (expect (= {:id "job" :label "Job"} (hi/table-column "job" "Job")))
        (expect (= {:id "took" :label "Took" :align "right"}
                   (hi/table-column "took" "Took" {:align "right"})))
        (expect (= {:id "b1" :cells ["tests / ubuntu" "13m0s"]}
                   (hi/table-row "b1" ["tests / ubuntu" "13m0s"])))
        (expect (= {:id "b1" :cells ["tests / ubuntu" "13m0s"] :tone "ok"}
                   (hi/table-row "b1" ["tests / ubuntu" "13m0s"] {:tone "ok"}))))
    (it "builds the view the engine mounts, nodes in the order they were declared"
        (expect (= ["now" "done" "score" "checks" "tail" "jobs" "run"]
                   (mapv :id (:nodes (ci-view)))))
        (expect (= "CI" (:title (ci-view))))
        (expect (nil? (refusal #(engine/normalize-live-view (ci-view))))))
    (it "leaves the session to the MOUNT, exactly as a form does"
        ;; A builder runs wherever an extension is written; which session the view
        ;; runs in is `open-live-view!`'s business, and it refuses there.
        (expect (nil? (refusal #(hi/view {:title "CI"} (hi/status "now" "Polling…")))))))
  (describe
    "refusal at the builder call"
    (it "refuses a tone outside the closed table, naming the node"
        (expect (re-find #"node now: :tone must be one of"
                         (refusal #(hi/status "now" "Polling…" {:tone "bright"})))))
    (it "refuses an order no surface knows how to paint"
        (expect (re-find #":order must be one of"
                         (refusal
                           #(hi/table "jobs" [(hi/table-column "job" "Job")] {:order "random"})))))
    (it "refuses a node with nothing to address it by"
        (expect (re-find #":id" (refusal #(hi/status "" "Polling…")))))
    (it "refuses a key the vocabulary does not have"
        (expect (re-find #"colour" (refusal #(hi/status "now" "Polling…" {:colour "red"})))))
    (it "refuses a view with no nodes, and one with no title"
        (expect (re-find #":nodes must not be empty" (refusal #(hi/view {:title "CI"}))))
        (expect (re-find #":title" (refusal #(hi/view {} (hi/status "now" "Polling…"))))))
    (it "refuses two nodes answering to the same id"
        (expect (re-find #"node ids must be distinct"
                         (refusal
                           #(hi/view {:title "CI"} (hi/status "now" "Polling…") (hi/log "now")))))))
  (describe
    "the two ops that change a running view's shape"
    (it "builds them as the maps the patch takes"
        (expect (= {:op "add-node" :node-spec {:type "log" :id "tail"}}
                   (hi/add-node (hi/log "tail"))))
        (expect (= {:op "add-node" :node-spec {:type "log" :id "tail"} :after "now"}
                   (hi/add-node (hi/log "tail") "now")))
        (expect (= {:op "remove-node" :node-id "tail"} (hi/remove-node "tail"))))
    (it "refuses a node the running view could never paint"
        (expect (re-find #"a status' :text" (refusal #(hi/add-node {:id "now" :type "status"})))))
    (it "refuses a removal that names nothing" (expect (some? (refusal #(hi/remove-node "")))))))
