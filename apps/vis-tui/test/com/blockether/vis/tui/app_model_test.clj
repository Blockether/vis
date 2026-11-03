(ns com.blockether.vis.tui.app-model-test
  (:require [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.tui.app-model :as model]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private patch
  (str "diff --git a/src/x.clj b/src/x.clj\n"
       "index 1..2 100644\n" "--- a/src/x.clj\n"
       "+++ b/src/x.clj\n" "@@ -10,3 +10,3 @@\n"
       " keep\n" "-old line\n"
       "+new line\n" " tail\n"))

(def ^:private diff-node
  {:id "diff"
   :type :diff
   :label "Changes"
   :old-revision "a1"
   :new-revision "b2"
   :patch patch
   :threads [{:id "t1"
              :anchor {:path "src/x.clj" :side "new" :line 11}
              :messages [{:id "m1" :author {:kind "agent"} :body "Why?" :created-at 1}]
              :draft "Because"}
             {:id "t2"
              :anchor {:path "gone.clj" :side "old" :line 3}
              :messages
              [{:id "m2" :author {:kind "human" :name "Ana"} :body "Old" :created-at 2}]}]})

(defdescribe
  diff-lines
  (it "numbers every line on its side and anchors it"
      (let [code (filterv :anchor (model/parse-patch patch))]
        (expect (= [[:ctx 10 10] [:del 11 nil] [:add nil 11] [:ctx 12 12]]
                   (mapv (juxt :kind :old :new) code)))
        (expect
          (= [{:path "src/x.clj" :side "new" :line 10} {:path "src/x.clj" :side "old" :line 11}
              {:path "src/x.clj" :side "new" :line 11} {:path "src/x.clj" :side "new" :line 12}]
             (mapv :anchor code)))))
  (it "drops the git headers and keeps one file row"
      (let [rows (model/parse-patch patch)]
        (expect (= ["src/x.clj"] (model/files patch)))
        (expect (not-any? #(re-find #"^(diff --git|index |--- |\+\+\+ )" (:text %)) rows))))
  (it "counts added and removed lines" (expect (= {:add 1 :del 1} (model/change-counts patch)))))

(defdescribe diff-rows
             (it "paints a thread under its anchor line and an orphan thread at the end"
                 (let [rows
                       (model/diff-rows diff-node 60 #{})

                       kinds
                       (mapv :kind rows)

                       at-add
                       (.indexOf ^java.util.List kinds :add)]

                   (expect (= :diff-head (first kinds)))
                   (expect (= :thread-head (nth kinds (inc at-add))))
                   (expect (= "t1" (:thread-id (nth rows (inc at-add)))))
                   (expect (= "t2" (:thread-id (last (filter #(= :thread-head (:kind %)) rows)))))))
             (it "hides the lines of a folded file and marks its row"
                 (let [rows
                       (model/diff-rows diff-node 60 #{"src/x.clj"})

                       file
                       (first (filter #(= :file (:kind %)) rows))]

                   (expect (:is-folded file))
                   (expect (not-any? #(#{:add :del :ctx :hunk} (:kind %)) rows)))))

(defdescribe
  actions
  (describe
    "draft"
    (it "edits an existing thread against the state version and the revision"
        (let [view {:seq 7 :nodes [diff-node]}]
          (expect
            (=
              {:action :draft :node-id "diff" :body "Hi" :base-seq 7 :thread-id "t1" :revision "b2"}
              (model/draft-action view {:node-id "diff" :thread-id "t1"} "Hi")))))
    (it "starts a thread at an anchor"
        (let [anchor {:path "src/x.clj" :side "old" :line 11}]
          (expect (= anchor
                     (:anchor (model/draft-action {:seq 1 :nodes [diff-node]}
                                                  {:node-id "diff" :anchor anchor}
                                                  "x")))))))
  (describe "submit"
            (it "uses a valid idempotency key and the state version"
                (let [k
                      (model/idempotency-key)

                      action
                      (model/submit-action {:seq 3} k nil)]

                  (expect (app/idempotency-key? k))
                  (expect (= {:action :submit :idempotency-key k :base-seq 3 :values {}} action))
                  (expect (not= k (model/idempotency-key))))))
  (describe "outcomes"
            (it "names each refusal"
                (expect (= :saved (model/outcome-state {:is-accepted true})))
                (expect (= :offline (model/outcome-state {:reason "offline"})))
                (expect (= :conflict (model/outcome-state {:reason "conflict"})))
                (expect (= :conflict (model/outcome-state {:reason "stale_revision"})))
                (expect (= :closed (model/outcome-state {:reason "closed"})))
                (expect (= :invalid (model/outcome-state {:reason "invalid"})))
                (expect (= :failed (model/outcome-state {:http-status 500}))))))

(defdescribe lifecycle
             (it "reads the state an app shows"
                 (expect (= :live (model/status {})))
                 (expect (= :stale (model/status {:is-stale true})))
                 (expect (= :closed (model/status {:is-stale true :is-closed true})))
                 (expect (model/read-only? {:is-closed true}))
                 (expect (not (model/read-only? {:is-stale true}))))
             (it "lists receipts newest first and counts the saved drafts"
                 (let [view {:nodes [diff-node]
                             :submissions [{:id "s1" :status "answered"}
                                           {:id "s2" :status "accepted" :comments [{}]}]}]
                   (expect (= ["s2" "s1"] (mapv :submission-id (model/receipt-rows view))))
                   (expect (= 1 (model/pending-drafts view)))))
             (it "finds the view id of an attachment row"
                 (expect (= "v1" (model/row-view-id {:view-id "v1"})))
                 (expect (nil? (model/row-view-id {:view-id " "})))
                 (expect (nil? (model/row-view-id {})))
                 (expect (model/app-row? {:media-type model/media-type}))
                 (expect (not (model/app-row? {:media-type "text/plain"})))))

(defdescribe
  diff-shape
  ;; Council #9354: a file row counts its changes and a hunk names its gap.
  (it "a file row counts its changes and a hunk names the lines it skips"
      (let [two
            (str patch "@@ -40,2 +40,2 @@\n" " a\n" "-b\n" "+c\n")

            rows
            (model/diff-shape (model/parse-patch two))

            file
            (first (filter #(= :file (:kind %)) rows))

            hunks
            (filter #(= :hunk (:kind %)) rows)]

        (expect (= "-2 +2" (model/file-counts file)))
        (expect (= [9 27] (mapv :gap hunks)))
        (expect (= ["⋯ 9 unmodified lines" "⋯ 27 unmodified lines"] (mapv model/hunk-text hunks)))))
  (it "a new file shows only its added count"
      (expect (= "+3" (model/file-counts {:added 3 :removed 0}))))
  ;; T24 user report: a picture or a binary file has no lines to count.
  (it "a binary file shows no line counts"
      (let [rows (model/diff-shape (model/parse-patch
                                     (str "diff --git a/logo.png b/logo.png\n"
                                          "index 8fc1a2b..4de6e13 100644\n"
                                          "Binary files a/logo.png and b/logo.png differ\n")))]
        (expect (nil? (model/file-counts (first (filter #(= :file (:kind %)) rows)))))))
  (it "every code row takes the width of the widest line number"
      (let [rows
            (model/diff-shape (model/parse-patch
                                (str patch "@@ -1200,1 +1200,1 @@\n" "-x\n" "+y\n")))

            code
            (filter #(#{:add :del :ctx} (:kind %)) rows)]

        (expect (= #{4} (set (map :num-w code))))
        (expect (apply = (map (comp count :gutter model/row-parts) code))))))

;; Review look (Council #9363): a changed word shows on the paired lines.
(defdescribe diff-word-marks
             (it "marks only the changed word of a paired line"
                 (expect (= [[6 9] [6 9]] (model/word-span "(defn foo [x])" "(defn bar [x])")))
                 (expect (= [[4 4] [4 12]] (model/word-span "let total = 1" "let subtotal = 1"))))
             (it "marks nothing when the lines are equal or the whole line changed"
                 (expect (nil? (model/word-span "same" "same")))
                 (expect (nil? (model/word-span "alpha" "omega"))))
             (it "pairs the n-th removed line with the n-th added line of a block"
                 (let [rows (model/pair-words [{:kind :ctx :text "a b"} {:kind :del :text "x = 1"}
                                               {:kind :del :text "y = 2"} {:kind :add :text "x = 3"}
                                               {:kind :ctx :text "z"} {:kind :add :text "q = 9"}])]
                   (expect (= [nil [4 5] nil [4 5] nil nil] (mapv :mark rows))))))

;; Frame dump at 40 columns (Council #9379): thread text was cut with an ellipsis.
(defdescribe
  thread-text-fits
  (it "wraps each message so its rail and text fit the width"
      (let [node
            {:id "d"
             :type :diff
             :patch "--- a/x\n+++ b/x\n@@ -1,1 +1,1 @@\n-a\n+b\n"
             :threads [{:id "t"
                        :anchor {:path "x" :side "new" :line 1}
                        :messages
                        [{:id "m"
                          :author {:kind "agent"}
                          :body "Why rename total to subtotal? Callers in cart.clj still use total."
                          :created-at 1}]}]}

            rows
            (filter #(= :message (:kind %)) (model/diff-rows node 30 #{}))]

        (expect (< 1 (count rows)))
        (expect (every? (fn [row]
                          (let [{:keys [gutter body]} (model/row-parts row)]
                            (<= (count (str gutter body)) 30)))
                        rows)))))

;; Review fixture, Council #9489: binary and rename-only files disappeared.
(defdescribe
  real-git-patches
  (it "keeps header-only files and does not treat metadata as code"
      (let [patch
            (str "diff --git a/blob.bin b/blob.bin\nindex 1..2 100644\n"
                 "Binary files a/blob.bin and b/blob.bin differ\n"
                 "diff --git a/old.py b/new.py\nsimilarity index 100%\n"
                 "rename from old.py\nrename to new.py\n"
                 "diff --git a/a.py b/a.py\n--- a/a.py\n+++ b/a.py\n" "@@ -1 +1 @@\n-old\n+new\n"
                 "diff --git a/b.py b/b.py\nnew file mode 100644\n"
                 "--- /dev/null\n+++ b/b.py\n@@ -0,0 +1 @@\n+created\n")

            rows
            (model/parse-patch patch)

            files
            (filter #(= :file (:kind %)) rows)]

        (expect (= ["blob.bin" "new.py" "a.py" "b.py"] (mapv :path files)))
        (expect (= [:modified :renamed :modified :added] (mapv :status files)))
        (expect (= "old.py" (:old-path (second files))))
        (expect (= ["old" "new" "created"] (mapv :text (filter #(#{:add :del} (:kind %)) rows))))))
  (it
    "keeps deleted files under their old path"
    (let
      [rows
       (model/parse-patch
         "diff --git a/gone b/gone\ndeleted file mode 100644\n--- a/gone\n+++ /dev/null\n@@ -1 +0,0 @@\n-x\n")]
      (expect (= "gone" (:path (first rows))))
      (expect (= :deleted (:status (first rows))))))
  (it "sizes line numbers per file rather than for the whole patch"
      (let [rows
            (model/diff-shape (model/parse-patch
                                (str "--- a/short\n+++ b/short\n@@ -1 +1 @@\n-a\n+b\n"
                                     "--- a/long\n+++ b/long\n@@ -1234 +1234 @@\n-a\n+b\n")))

            widths
            (into {} (map (juxt :path :num-w)) (filter #(= :add (:kind %)) rows))]

        (expect (= {"short" 1 "long" 4} widths)))))

;; Review scenarios S05/S06/S07/S21, Council #9489.
(defdescribe
  review-filters
  (it "hides test-file bodies and their file-list entries without hiding source files"
      (let [node
            {:id "d"
             :type :diff
             :patch (str "--- a/src/a.py\n+++ b/src/a.py\n@@ -1 +1 @@\n-a\n+b\n"
                         "--- a/tests/a_test.py\n+++ b/tests/a_test.py\n@@ -1 +1 @@\n-a\n+b\n")}

            rows
            (model/diff-rows node 40 #{} {:hide-tests? true :files? true})]

        (expect (= ["src/a.py"] (mapv :path (filter #(= :file (:kind %)) rows))))
        (expect (not-any? #(= "tests/a_test.py" (:path %)) rows))))
  (it "shows the file list only when requested and keeps the file headers"
      (let [node
            {:id "d"
             :type :diff
             :patch
             "--- a/a\n+++ b/a\n@@ -1 +1 @@\n-a\n+b\n--- a/b\n+++ b/b\n@@ -1 +1 @@\n-a\n+b\n"}

            rows
            (model/diff-rows node 40 #{} {:files? false})]

        (expect (= 2 (count (filter #(= :file (:kind %)) rows))))
        (expect (not-any? #(= :file-item (:kind %)) rows))))
  (it "shows an empty-state row when the selection has no file changes"
      (let [rows (model/diff-rows {:id "d" :type :diff :patch ""} 40 #{})]
        (expect (some #(= :diff-empty (:kind %)) rows)))))

;; Review scenario S05, Council #9489: whitespace-only edits must not obscure real edits.
(defdescribe
  whitespace-selection
  (it "removes whitespace-only changes but keeps semantic changes and their anchors"
      (let [node
            {:id "d"
             :type :diff
             :patch
             "--- a/a.py\n+++ b/a.py\n@@ -1,3 +1,3 @@\n-x = 1\n+x=1\n context\n-y = 2\n+y = 3\n"}

            rows
            (model/diff-rows node 40 #{} {:hide-whitespace? true})

            changes
            (filter #(#{:add :del} (:kind %)) rows)]

        (expect (= ["y = 2" "y = 3"] (mapv :text changes)))
        (expect (= [3 3] (mapv (comp :line :anchor) changes))))))

;; Code search, Council #9489: matches must navigate real code rows only.
(defdescribe code-search
             (it "wraps to matching code, ignores chrome, and leaves no result for an empty query"
                 (let [rows [{:kind :file :text "needle"} {:kind :add :text "a needle"}
                             {:kind :ctx :text "plain"} {:kind :split :right {:text "Needle b"}}]]
                   (expect (= 3 (model/find-code rows "needle" 2)))
                   (expect (= 1 (model/find-code rows "needle" 4)))
                   (expect (nil? (model/find-code rows "missing" 0)))
                   (expect (nil? (model/find-code rows " " 0)))
                   (expect (nil? (model/find-code [] "needle" 0))))))

;; Real Git path quoting, Council #9489: quoted names must stay valid anchors.
(defdescribe git-path-names
             (it "decodes Git octal UTF-8 and keeps spaces in ordinary file names"
                 (doseq [[header old-line new-line path]
                         [["diff --git a/a name.py b/a name.py" "--- a/a name.py" "+++ b/a name.py"
                           "a name.py"]
                          ["diff --git \"a/caf\\303\\251.py\" \"b/caf\\303\\251.py\""
                           "--- \"a/caf\\303\\251.py\"" "+++ \"b/caf\\303\\251.py\"" "café.py"]]]
                   (let [rows (model/parse-patch
                                (str header "\n" old-line "\n" new-line "\n@@ -1 +1 @@\n-a\n+b\n"))]
                     (expect (= path (:path (first rows))))
                     (expect (= path (get-in (last rows) [:anchor :path])))))))

;; Review scenario S06, Council #9489: comments on a hidden test file must stay hidden with it.
(defdescribe
  hidden-test-threads
  (it
    "hides the anchored comments with their test file and keeps source-file comments"
    (let
      [node
       {:id "d"
        :type :diff
        :patch
        "--- a/a.py\n+++ b/a.py\n@@ -1 +1 @@\n-a\n+b\n--- a/test_a.py\n+++ b/test_a.py\n@@ -1 +1 @@\n-a\n+b\n"
        :threads [{:id "source" :anchor {:path "a.py" :side "new" :line 1} :messages []}
                  {:id "test" :anchor {:path "test_a.py" :side "new" :line 1} :messages []}]}

       rows
       (model/diff-rows node 40 #{} {:hide-tests? true})]

      (expect (some #(= "source" (:thread-id %)) rows))
      (expect (not-any? #(= "test" (:thread-id %)) rows))))
  (it "recognizes test directories, prefix names, and suffix names without hiding source files"
      (doseq [[path test?] [["tests/a.py" true] ["pkg/__tests__/a.ts" true] ["test_a.py" true]
                            ["pkg/a.test.tsx" true] ["pkg/a_spec.rb" true] ["src/contest.py" false]
                            ["src/testkit.py" false]]]
        (expect (= test? (model/test-file? path))))))
