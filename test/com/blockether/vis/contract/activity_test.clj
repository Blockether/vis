(ns com.blockether.vis.contract.activity-test
  "The same Activity receipt is consumed by Python, Companion and TUI."
  (:require [clojure.data.json :as json]
            [com.blockether.vis.contract.document :as document]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.activity :as activity]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest shared-activity-copy-test
  (doseq [{:strs [name projection text]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-copy.json")))]
    (let [receipt (activity/from-wire projection)]
      (is (some? receipt) name)
      (is (= text (activity/copy-text receipt)) name))))

(deftest portable-activity-contract-test
  (let [fixture (json/read-str (slurp (io/resource "vis-contract/fixtures/activity.json")))]
    (is (nil? (io/resource "vis-contract/activity.json")))
    (is (document/valid? "activity" fixture))
    (is (document/valid-json? "activity" "projection" fixture))
    (doseq [old-key ["anchor" "schema_version" "view_id"]]
      (is (not (document/valid-json? "activity" "projection" (assoc fixture old-key 1)))))
    (doseq [show-start [true false]]
      (is (document/valid-json?
            "activity"
            "declaration"
            {"presenter" "tests" "label" "Check components" "show_start" show-start})))
    (doseq [show-start [nil 0 1 "false"]]
      (is (not (document/valid-json? "activity"
                                     "declaration"
                                     {"presenter" "tests" "show_start" show-start}))))
    (is (not (document/valid-json? "activity"
                                   "declaration"
                                   {"presenter" "tests" "state" "succeeded"})))))

(deftest persistent-handle-id-contract-test
  (let [fixture
        (json/read-str (slurp (io/resource "vis-contract/fixtures/activity.json")))

        view
        {"headline" "Checking visual differences" "summary" "Done" "content" []}]

    (doseq [id ["comparison-1" (apply str (repeat 256 "é"))]]
      (is (activity/valid-handle-id? id))
      (is (activity/valid-presentation? (assoc view "handle_id" id)))
      (is (activity/valid-projection? (assoc-in fixture ["rows" 0 "handle_id"] id))))
    (doseq [id ["" "  " (apply str (repeat 257 "é")) (apply str (repeat 171 "界")) "line\nbreak"
                (str "line" (char 8232) "break") true]]
      (is (not (activity/valid-handle-id? id)))
      (is (not (activity/valid-presentation? (assoc view "handle_id" id))))
      (is (not (activity/valid-projection? (assoc-in fixture ["rows" 0 "handle_id"] id)))))))

(deftest shared-activity-admission-test
  (doseq [{:strs [name valid projection]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-cases.json")))]
    (is (= valid (boolean (activity/from-wire projection))) name)
    (when valid (is (= projection (wire/->wire (activity/from-wire projection))) name))))

(deftest shared-activity-digest-admission-test
  (doseq [{:strs [name valid digest]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-digest.json")))]
    (is (= valid (boolean (activity/digest-from-wire digest))) name)
    (when valid (is (= digest (wire/->wire (activity/digest-from-wire digest))) name))))

(deftest shared-operation-groups-test
  ;; Regression #201: extension labels must agree with Companion without changing receipts.
  (doseq [{:strs [name projection groups]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-groups.json")))]
    (let [receipt (activity/from-wire projection)
          group-rows (ns-resolve 'com.blockether.vis.contract.activity 'operation-groups)]

      (is (some? receipt) name)
      (is (= projection (wire/->wire receipt)) name)
      (is (some? group-rows) "The contract owns chronological grouping")
      (when group-rows
        (is (= groups
               (mapv (fn [{:keys [id label rows]}]
                       {"id" id "label" label "rows" (mapv :id rows)})
                     (group-rows (:rows receipt))))
            name)))))

(deftest shared-argument-groups-test
  (doseq [{:strs [name projection groups]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-arguments.json")))]
    (let [receipt (activity/from-wire projection)
          group-rows (ns-resolve 'com.blockether.vis.contract.activity 'argument-groups)]

      (is (some? receipt) name)
      (is (= projection (wire/->wire receipt)) name)
      (is (some? group-rows))
      (when group-rows
        (is (= groups
               (mapv (fn [{:keys [id rows]}]
                       {"id" id "rows" (mapv :id rows)})
                     (group-rows (:rows receipt))))
            name)))))

(deftest complete-presentation-content-test
  ;; Regression #218: retain full result bodies rather than failing size admission.
  (let [text
        (apply str (repeat 20000 "界"))

        block
        {"type" "code" "text" text}

        table
        {"type" "table"
         "columns" (vec (repeat 17 (apply str (repeat 300 "C"))))
         "rows" (vec (repeat 201 (vec (repeat 17 (apply str (repeat 300 "x"))))))}

        content
        (conj (vec (repeat 40 block)) table)

        section
        {"headline" "Results" "summary" "Complete" "content" content}

        presentation
        (assoc section "sections" (vec (repeat 9 section)))]

    (is (activity/valid-presentation? presentation))
    (is (not (activity/valid-presentation? (assoc presentation "headline" "two\nlines"))))
    (is (not (activity/valid-presentation?
               (assoc presentation
                 "content" [{"type" "table" "columns" ["A"] "rows" [["A" "B"]]}]))))))

(deftest table-row-paths-test
  ;; BLO-172: a table row may NAME the file it lists, so a reader can open it from
  ;; the row. One path per row or none at all — a shorter list presses the wrong row.
  (let [table
        {"type" "table" "columns" ["Name"] "rows" [["a.clj"] ["b.clj"]]}

        presentation
        {"headline" "Listed directory"
         "summary" "2 files"
         "content" [(assoc table "paths" ["/w/a.clj" ""])]}]

    (is (activity/valid-presentation? presentation))
    (is (activity/valid-presentation? (assoc presentation "content" [table])))
    (is (not (activity/valid-presentation? (assoc presentation
                                             "content" [(assoc table "paths" ["/w/a.clj"])]))))))

(deftest presentation-summary-format-test
  ;; Regression #254: only explicitly marked summaries opt into inline Markdown.
  (let [summary
        "Issues: [#252](https://github.com/Blockether/vis/issues/252)"

        section
        {"headline" "Matches" "summary" summary "content" []}]

    (is (activity/valid-presentation? section))
    (doseq [format ["inline" "markdown"]]
      (let [formatted (assoc section "summary_format" format)
            presentation (assoc formatted "sections" [formatted])]

        (is (activity/valid-presentation? presentation))
        (is (not (activity/valid-presentation? (assoc presentation "summary" "two\nlines")))))
      (is (not (activity/valid-presentation? (assoc section
                                               "summary_format" format
                                               "summary" (apply str (repeat 257 "é")))))))
    (doseq [format [nil "" "html" true 7 []]]
      (is (not (activity/valid-presentation? (assoc section "summary_format" format)))))))

(deftest single-oversized-invocation-page-test
  ;; Regression #218: page bytes are a target; one complete invocation always fits.
  (let [fixture
        (json/read-str (slurp (io/resource "vis-contract/fixtures/activity.json")))

        row
        (assoc (first (get fixture "rows"))
          "presentation" {"headline" "Result"
                          "summary" "Complete"
                          "content" [{"type" "code" "text" (apply str (repeat 400000 "界"))}]})

        page
        (assoc fixture
          "rows" [row]
          "history" {"id" "00000000-0000-4000-8000-000000000218"
                     "revision" 1
                     "total" 1
                     "after" 0
                     "next_after" nil})]

    (is (activity/valid-projection? page))
    (is (= page (wire/->wire (activity/from-wire page))))
    (is (not (activity/valid-projection? (update page
                                                 "rows"
                                                 conj
                                                 (assoc row
                                                   "id" "second"
                                                   "sequence" 2)))))))
