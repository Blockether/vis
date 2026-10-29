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
