(ns com.blockether.vis.loop.runtime.formatters-test
  "Invariant + shape tests for built-in tool formatters."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.formatters :as fmt]
    [com.blockether.vis.loop.tool-formatter-invariants-test :as inv]))

;; =============================================================================
;; Sample fixtures — one per formatter. Kept realistic but minimal.
;; =============================================================================

(def sample-conversation-history
  [{:query-pos 0 :query-id (random-uuid) :text "refactor the parser" :status "done"
    :iterations 3 :key-vars '[parsed tokens] :answer-preview "done, see parsed"
    :created-at #inst "2025-04-10T10:00:00Z"}
   {:query-pos 1 :query-id (random-uuid) :text "now write tests" :status "done"
    :iterations 2 :key-vars '[tests] :answer-preview "tests pass"
    :created-at #inst "2025-04-10T10:05:00Z"}])

(def sample-conversation-code
  [{:iteration-pos 0 :created-at #inst "2025-04-10T10:00:00Z"
    :code ['(read-file "src/a.clj") '(def x 1)]
    :answer nil}
   {:iteration-pos 1 :created-at #inst "2025-04-10T10:00:15Z"
    :code ['(println x)]
    :answer "final"}])

(def sample-conversation-results
  [{:iteration-pos 0 :created-at #inst "2025-04-10T10:00:00Z"
    :results ['{:path "/a"} '1]
    :vars [{:name 'x :value "1" :code "(def x 1)"}]
    :answer nil}])

(def sample-restore-vars
  {'anomalies [1 2 3]
   'broken    {:error {:type :rlm/restore-var-missing :symbol 'broken :message "not found"}}
   'summary   "all good"})

(def sample-var-history
  [{:version 1 :value [1 2] :code "(def xs [1 2])" :diffable? true
    :query-id (random-uuid) :created-at #inst "2025-04-10T10:00:00Z"}
   {:version 2 :value [1 2 3] :code "(def xs [1 2 3])" :diffable? true
    :query-id (random-uuid) :created-at #inst "2025-04-10T10:05:00Z"}])

(def sample-var-diff-structural
  {:type :structural :from-version 1 :to-version 2 :edit-count 1
   :edits [{:op :added :path [2] :value 3}]})

(def sample-var-diff-string
  {:type :string-diff :from-version 1 :to-version 2 :edit-count 2
   :unified "@@ -1,1 +1,1 @@\n-hello\n+world\n"})

(def sample-var-diff-number
  {:type :number-delta :from-version 1 :to-version 2 :from 10 :to 15 :delta 5 :pct-change 50.0})

(def sample-var-diff-replacement
  {:type :replacement :from-version 1 :to-version 2 :from :a :to :b})

(def sample-commits
  [{:sha "abc1234567" :date #inst "2025-04-10T10:00:00Z" :author "Jane"
    :author-email "j@x" :category :fix :name "fix: parser crash on empty"
    :ticket-refs ["SVAR-42"] :file-paths ["src/parser.clj" "test/parser_test.clj"]}
   {:sha "def7890abc" :date #inst "2025-04-09T09:00:00Z"
    :subject "feat: add formatter dispatcher" :author "John"
    :file-paths ["src/runtime/core.clj"]}])

(def sample-commit-parents ["abc1234567" "def7890abc"])

(def sample-blame
  [{:line 1 :sha "abc1234567890" :short "abc1234" :author "Jane"
    :email "j@x" :date #inst "2025-04-10T10:00:00Z" :content "(ns foo)"}
   {:line 2 :sha "def7890abcdef" :short "def7890" :author "John"
    :email "jo@x" :date #inst "2025-04-09T09:00:00Z" :content "(defn bar [] 1)"}])

(def sample-concept-info
  {:term "Schema" :definition "A structural mental pattern learned early."
   :group "Foundations" :status "approved"
   :aliases ["Early Maladaptive Schema"]
   :sources [{:document-id "d1" :page 42 :excerpt "..."}]})

;; =============================================================================
;; Invariant tests — every formatter × its samples.
;; =============================================================================

(defdescribe built-in-formatter-invariants
  (describe "all built-in formatters satisfy the contract"
    (it "conversation-history"
      (doseq [v [nil [] sample-conversation-history]]
        (inv/check-formatter-invariants! fmt/format-conversation-history v)))
    (it "conversation-code"
      (doseq [v [nil [] sample-conversation-code]]
        (inv/check-formatter-invariants! fmt/format-conversation-code v)))
    (it "conversation-results"
      (doseq [v [nil [] sample-conversation-results]]
        (inv/check-formatter-invariants! fmt/format-conversation-results v)))
    (it "restore-vars"
      (doseq [v [nil {} sample-restore-vars]]
        (inv/check-formatter-invariants! fmt/format-restore-vars v)))
    (it "var-history"
      (doseq [v [nil [] sample-var-history]]
        (inv/check-formatter-invariants! fmt/format-var-history v)))
    (it "var-diff (all four :type dispatches)"
      (doseq [v [nil sample-var-diff-structural sample-var-diff-string
                 sample-var-diff-number sample-var-diff-replacement]]
        (inv/check-formatter-invariants! fmt/format-var-diff v)))
    (it "commit-list"
      (doseq [v [nil [] sample-commits]]
        (inv/check-formatter-invariants! fmt/format-commit-list v)))
    (it "commit-parents"
      (doseq [v [nil [] sample-commit-parents]]
        (inv/check-formatter-invariants! fmt/format-commit-parents v)))
    (it "blame"
      (doseq [v [nil [] sample-blame]]
        (inv/check-formatter-invariants! fmt/format-blame v)))
    (it "concept-info"
      (doseq [v [nil sample-concept-info]]
        (inv/check-formatter-invariants! fmt/format-concept-info v)))
    (it "concept-mutation"
      (doseq [v [nil {:removed "Foo" :rationale "dup"}
                 {:edited "Bar" :updates {:definition "new"}}]]
        (inv/check-formatter-invariants! fmt/format-concept-mutation v)))))

;; =============================================================================
;; Shape tests — assert the most important user-visible substrings.
;; =============================================================================

(defdescribe built-in-formatter-shapes
  (describe "conversation-history"
    (it "shows count + query rows"
      (let [out (fmt/format-conversation-history sample-conversation-history)]
        (expect (re-find #"2 query/ies" out))
        (expect (re-find #"refactor the parser" out)))))

  (describe "restore-vars"
    (it "distinguishes ok vs failed"
      (let [out (fmt/format-restore-vars sample-restore-vars)]
        (expect (re-find #"2/3 var\(s\)" out))
        (expect (re-find #"anomalies" out))
        (expect (re-find #"summary" out))
        (expect (re-find #"broken" out))
        (expect (re-find #"not found" out)))))

  (describe "var-diff dispatches by :type"
    (it ":structural"
      (expect (re-find #"structural edit"
                (fmt/format-var-diff sample-var-diff-structural))))
    (it ":string-diff"
      (expect (re-find #"\+world"
                (fmt/format-var-diff sample-var-diff-string))))
    (it ":number-delta"
      (let [out (fmt/format-var-diff sample-var-diff-number)]
        (expect (re-find #"10 → 15" out))
        (expect (re-find #"Δ 5" out))
        (expect (re-find #"50\.0%" out))))
    (it ":replacement"
      (expect (re-find #":a" (fmt/format-var-diff sample-var-diff-replacement)))))

  (describe "commit-list"
    (it "shows short shas + titles"
      (let [out (fmt/format-commit-list sample-commits)]
        (expect (re-find #"2 commit\(s\)" out))
        (expect (re-find #"abc1234" out))
        (expect (re-find #"def7890" out))
        (expect (re-find #"fix: parser crash on empty" out))
        (expect (re-find #"formatter dispatcher" out))
        (expect (re-find #"SVAR-42" out)))))

  (describe "blame"
    (it "shows per-line attribution"
      (let [out (fmt/format-blame sample-blame)]
        (expect (re-find #"blame — 2 line" out))
        (expect (re-find #"Jane" out))
        (expect (re-find #"\(ns foo\)" out)))))

  (describe "concept-info"
    (it "handles nil as 'not found'"
      (expect (= "concept not found" (fmt/format-concept-info nil))))
    (it "shows term, group, aliases, definition"
      (let [out (fmt/format-concept-info sample-concept-info)]
        (expect (re-find #"Schema \[Foundations\]" out))
        (expect (re-find #"approved" out))
        (expect (re-find #"Early Maladaptive Schema" out))
        (expect (re-find #"structural mental pattern" out))))))
