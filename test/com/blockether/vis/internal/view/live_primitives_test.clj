(ns com.blockether.vis.internal.view.live-primitives-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.view.core :as engine]
            [com.blockether.vis.internal.view.materializer :as materializer]
            [com.blockether.vis.internal.view.sink :as sink]
            [com.blockether.vis.view :as v]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- nodes [view] (mapcat #(tree-seq :fields :fields %) (:nodes view)))

(defn- node [view id] (first (filter #(= id (:id %)) (nodes view))))

(defn- watching
  [f]
  (let [dir
        (io/file (System/getProperty "java.io.tmpdir") (str "vis-primitives-" (random-uuid)))

        views-dir
        (requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir)]

    (with-redefs-fn {views-dir (constantly dir)}
      #(let [view
             (engine/open-live!
               (v/view {:title "Presentation" :session-id (str (random-uuid))}
                       (v/heading "title" "Build results" {:level 1})
                       (v/paragraph "intro" "A **live** paragraph")
                       (v/code "source" "  <button>\n" {:language "html"})
                       (v/spinner "wait" "Loading" {:variant :dots})
                       (v/disclosure "details"
                                     "Details"
                                     [(v/button "refresh" "Refresh")
                                      (v/button "disabled" "Unavailable" {:is-disabled true})
                                      (v/log "a" {:lines ["retained"]})
                                      (v/log "b"
                                             {:lines ["independent"] :default-expanded true})])))]
         (try (f view)
              (finally (engine/close-live! (:id view))
                       (doseq [file (reverse (file-seq dir))]
                         (io/delete-file file true))))))))

(deftest all-primitives-contract-test
  (let [wire-view
        (json/read-json (slurp (io/resource "vis-contract/fixtures/live-primitives.json")))

        ;; Normalize the wire declaration before materializing the complete vocabulary.
        declared
        (update wire-view
                "nodes"
                (fn strip [items]
                  (mapv (fn [n]
                          (cond-> (dissoc n "clicks" "total_lines")
                            (get n "fields")
                            (update "fields" strip)))
                        items)))

        raw
        (select-keys declared ["title" "description" "nodes"])

        view
        (materializer/materialize (engine/normalize-live-view raw))]

    (is (nil? (spec/live-view-error view)))
    (is (= (set (vals spec/live-node-types))
           (set (map :type (remove #(= :group (:type %)) (nodes view))))))
    (is (= #{1 2 3 4 5 6} (set (map :level (filter #(= :heading (:type %)) (nodes view))))))
    (is (= (set (vals spec/spinner-variants))
           (set (map :variant (filter #(= :spinner (:type %)) (nodes view))))))
    (is (= 0 (:clicks (node view "refresh"))))
    (is (= "button" (get (wire/->wire (node view "refresh")) "type")))
    (is (str/includes? (materializer/->markdown view) "<button> is literal text"))
    (let [markdown
          (materializer/->markdown view)

          parsed
          (materializer/parse-markdown markdown)]

      (is (= markdown (materializer/->markdown (:view parsed)))))))

(deftest builders-and-validation-test
  (is (= "heading" (:type (v/heading "Form heading"))))
  (is (= "paragraph" (:type (v/paragraph "Form prose"))))
  (doseq [bad [{:type :heading :id "x" :text "Heading" :level 0}
               {:type :heading :id "x" :text "Heading" :level 7}
               {:type :spinner :id "x" :text "Wait" :variant "unknown"}
               {:type :button :id "x" :label "Go" :clicks 10}
               {:type :log :id "x" :default-expanded "yes"} {:type :code :id "x" :text 4}]]
    (is (try (engine/normalize-live-view {:title "Invalid" :nodes [bad]})
             false
             (catch clojure.lang.ExceptionInfo _ true))))
  (is (empty? (:text (v/code "empty" "")))))

(deftest live-primitives-lifecycle-test
  (watching
    (fn [view]
      (let [id (:id view)]
        (is (= :dots (:variant (node view "wait"))))
        (is (false? (:is-accepted (engine/action! id {:action :activate :node-id "disabled"}))))
        (is (try (engine/action! id {:action :activate :node-id "a"})
                 false
                 (catch clojure.lang.ExceptionInfo _ true)))
        (let [presses (mapv (fn [_]
                              (future (engine/action! id
                                                      {"action" "activate" "node_id" "refresh"})))
                            (range 12))]
          (is (every? :is-accepted (map deref presses))))
        (is (= 12 (:clicks (node (engine/live-view id) "refresh"))))
        (engine/patch-live! id
                            [{:op :set :node-id "source" :text ""}
                             {:op :set :node-id "title" :text "Finished" :level 3}
                             {:op :set :node-id "intro" :text "Updated paragraph"}
                             {:op :set :node-id "wait" :variant :pulse :is-active false}
                             {:op :set :node-id "refresh" :is-disabled true}
                             {:op :append :node-id "a" :lines ["new line"]}])
        ;; Regression #189: visibility never clears either log's retained lines.
        (is (= ["retained" "new line"] (:lines (node (engine/live-view id) "a"))))
        (is (= ["independent"] (:lines (node (engine/live-view id) "b"))))
        (is (false? (:is-accepted (engine/action! id {:action :activate :node-id "refresh"}))))
        (let [result (engine/close-live! id)
              entries (sink/read-range (sink/view-file (:session-id view) id) 0 100)]

          (is (:is-completed result))
          (is (= 12 (:clicks (node (:view result) "refresh"))))
          (is (nil? (spec/live-result-error result)))
          (is (= 12 (count (filter #(some :clicks (get-in % [:patch :ops])) entries))))
          (is (false? (:is-accepted (engine/action! id
                                                    {:action :activate :node-id "refresh"})))))))))

(deftest nested-history-test
  ;; Regression #189: nesting a log does not remove its opening lines from history.
  (watching
    (fn [view]
      (let [id
            (:id view)

            file
            (sink/view-file (:session-id view) id)

            lines
            (mapv #(str "line " %) (range 40))]

        (is (= ["retained"] (:lines (sink/log-range file "a" 0 100))))
        (engine/patch-live! id [{:op :append :node-id "a" :lines lines}])
        (is (= (into ["retained"] lines) (:lines (sink/log-range file "a" 0 100))))
        (engine/patch-live! id
                            [{:op :add-node
                              :node-spec (v/disclosure "late"
                                                       "Added section"
                                                       [(v/log "late-log" {:lines ["opening"]})])}
                             {:op :append :node-id "late-log" :lines ["next"]}])
        (is (= ["opening" "next"] (:lines (sink/log-range file "late-log" 0 100))))))))

(deftest receipt-retains-disclosure-tree-test
  ;; Regression #189: the human receipt is not the model's flattened picture.
  (watching
    (fn [view]
      (let [id
            (:id view)

            file
            (sink/view-file (:session-id view) id)

            _
            (engine/patch-live! id [{:op :append :node-id "a" :lines (mapv str (range 200))}])

            model
            (engine/close-live! id)

            receipt
            (:result (last (sink/read-range file 0 100)))]

        (is (nil? (spec/live-result-error (update receipt :reason keyword))))
        (is (not-any? #(= :group (:type %)) (get-in model [:view :nodes])))
        (is (= "group" (:type (node (:view receipt) "details"))))
        (is (= 201 (count (:lines (node (:view receipt) "a")))))
        (is (seq (:elided model)))
        (is (nil? (:elided receipt)))))))

(deftest initial-history-exceeds-hot-window-test
  ;; Regression #189: the display window never limits the durable record.
  (watching
    (fn [view]
      (let [lines
            (mapv #(str "line " %) (range 40))

            declared
            (v/view
              {:title "Initial history" :session-id (:session-id view)}
              (v/disclosure "section" "Section" [(v/log "nested" {:lines lines :window-lines 2})]))

            opened
            (engine/open-live! declared)]

        (try (engine/patch-live! (:id opened) [{:op :append :node-id "nested" :lines ["later"]}])
             (is (= 2 (count (:lines (node (engine/live-view (:id opened)) "nested")))))
             (is (= (conj lines "later")
                    (:lines (sink/log-range (sink/view-file (:session-id opened) (:id opened))
                                            "nested"
                                            0
                                            100))))
             (finally (engine/close-live! (:id opened))))))))

(deftest literal-markdown-roundtrip-test
  (doseq [type
          [:paragraph :code]

          text
          ["**looks like status**" "# literal\n\n<!-- vis:heading 1 -->\n# still literal\n```\n\n"]]

    (let [view
          {:title "Literal content" :nodes [{:id "text" :type type :text text}]}

          markdown
          (materializer/->markdown view)

          parsed
          (:view (materializer/parse-markdown markdown))]

      (is (= text (get-in parsed [:nodes 0 :text])))
      (is (= markdown (materializer/->markdown parsed))))))

(deftest retained-log-search-test
  ;; #189: search the record, including nested logs, not just the painted window.
  (watching
    (fn [view]
      (let [id
            (:id view)

            file
            (sink/view-file (:session-id view) id)

            search
            #(sink/log-range file "a" %1 %2 %3)]

        (engine/patch-live!
          id
          [{:op :append :node-id "a" :lines ["ERROR [disk]" "ok" "error [disk] again" "ŁÓDŹ"]}])
        (is (= {:node-id "a"
                :from 1
                :total 5
                :matched 2
                :lines ["error [disk] again"]
                :line-numbers [4]}
               (search 1 1 "[DISK]")))
        (is (= [5] (:line-numbers (search 0 10 "łódź"))))
        (is (= 0 (:matched (search 0 10 ".*"))))
        (is (= 5 (:matched (search 0 10 ""))))
        (is (= [] (:lines (search 8 2 "error"))))
        (engine/patch-live! id
                            [{:op :clear :node-id "a"}
                             {:op :append :node-id "a" :lines ["ERROR fresh"]}])
        (is (= [1] (:line-numbers (search 0 10 "error"))))
        (is (= 1 (:total (search 0 10 "error"))))
        (engine/close-live! id)
        (is (= ["ERROR fresh"] (:lines (search 0 10 "error"))))))))

(deftest retained-log-search-is-bounded-beyond-the-hot-window-test
  (watching
    (fn [view]
      (let [id
            (:id view)

            lines
            (mapv #(str (if (even? %) "ERROR " "ok ") %) (range 2500))

            file
            (sink/view-file (:session-id view) id)]

        (doseq [chunk (partition-all 100 lines)]
          ;; #209: styling processes only arriving chunks, never the retained file.
          (engine/patch-live! id [{:op :append :node-id "a" :lines (vec chunk) :tone :error}]))
        (let [page (sink/log-range file "a" 0 2 "error")]
          (is (= 2501 (:total page)))
          (is (= 1250 (:matched page)))
          (is (= ["ERROR 0" "ERROR 2"] (:lines page)))
          (is (= [2 4] (:line-numbers page)))
          (is (= ["error" "error"] (:line-tones page)))
          (is (= 2000 (count (:line-tones (node (engine/live-view id) "a"))))))
        (engine/patch-live! id [{:op :set :node-id "wait" :text "Failed promptly"}])
        (is (= "Failed promptly" (:text (node (engine/live-view id) "wait"))))
        (is (:is-accepted (engine/action! id {:action :interrupt})))))))

(deftest styled-output-history-test
  ;; #209: styles stay alongside complete text through appends, redaction and closure.
  (watching
    (fn [view]
      (let [id
            (:id view)

            file
            (sink/view-file (:session-id view) id)]

        (engine/patch-live!
          id
          [{:op :append :node-id "a" :lines ["WARN disk" "password=fixture-log-secret"] :tone :warn}
           {:op :append :node-id "a" :lines ["ERROR <script>literal</script>"] :tone :error}
           {:op :append :node-id "a" :lines ["last"]}])
        (let [log
              (node (engine/live-view id) "a")

              page
              (sink/log-range file "a" 0 10)]

          (is (= [nil :warn :warn :error nil] (:line-tones log)))
          (is (= [nil "warn" "warn" "error" nil] (:line-tones page)))
          (is (= (:lines log) (:lines page)))
          (is (= "password=[REDACTED]" (nth (:lines log) 2)))
          (is (= log (node (engine/live-view<-wire (wire/->wire (engine/live-view id))) "a")))
          (is (= [4] (:line-numbers (sink/log-range file "a" 0 2 "error"))))
          (is (= ["error"] (:line-tones (sink/log-range file "a" 0 2 "error"))))
          (is (not (str/includes? (slurp file) "fixture-log-secret")))
          (engine/close-live! id)
          (is (= page (sink/log-range file "a" 0 10))))))))

(deftest log-controls-and-tone-validation-test
  (let [log (engine/normalize-live-node
              {:type :log :id "safe" :lines [(str "literal" (char 27) "[2J" (char 7))]})]
    (is (= ["literal\\u001b[2J\\u0007"] (:lines log))))
  (doseq [tones [[:unknown] []]]
    (is (try (engine/normalize-live-node {:type :log :id "bad" :lines ["x"] :line-tones tones})
             false
             (catch clojure.lang.ExceptionInfo _ true)))))
