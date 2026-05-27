(ns com.blockether.vis.ext.foundation-git.render-test
  "Channel renderer shape tests for the `git/*` surface.

   These tests pin the IR shape (`[:ir]` root + paragraphs + code
   blocks + `[:strong]` badges) so the foundation-git preview stays
   structurally identical to the foundation-core editing previews."
  (:require
   [com.blockether.vis.ext.foundation-git.render :as gr]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- ir? [x] (and (vector? x) (= :ir (first x))))
(defn- strong-in? [v needle]
  (boolean
    (re-find (re-pattern (str "\\[:strong \\{\\} \"" needle "\"\\]"))
      (pr-str v))))

(defdescribe render-status-test
  (it "labels a clean tree CLEAN and a dirty tree DIRTY"
    (let [clean (gr/render-status {:branch "main" :head "abcdef1234" :clean? true :entries []})
          dirty (gr/render-status {:branch "main" :head "abcdef1234" :clean? false
                                   :entries [{:status "M" :file "x.clj"}]})]
      (expect (ir? clean))
      (expect (strong-in? clean "CLEAN"))
      (expect (strong-in? dirty "DIRTY"))
      (expect (re-find #"x\.clj" (pr-str dirty))))))

(defdescribe render-diff-test
  (it "renders header + stat + numstat table"
    (let [v (gr/render-diff {:branch "main" :head "abcdef1234567"
                             :kind :branch :from "HEAD~1" :to nil
                             :stat {:files 2 :+ 5 :- 1}
                             :files [{:file "a.clj" :+ 3 :- 0}
                                     {:file "b.clj" :+ 2 :- 1}]
                             :porcelain []})
          s (pr-str v)]
      (expect (ir? v))
      (expect (strong-in? v "DIFF"))
      (expect (re-find #"2 files" s))
      (expect (re-find #"\+5" s))
      (expect (re-find #"a\.clj" s))))

  (it "embeds per-file patches when present"
    (let [v (gr/render-diff
              {:branch "main" :kind :workspace :from "HEAD" :to nil
               :stat {:files 1 :+ 1 :- 0}
               :files [{:file "a.clj" :+ 1 :- 0
                        :patch "diff --git a/a.clj b/a.clj\n+(def x 1)\n"}]
               :porcelain []})]
      (expect (re-find #"diff --git" (pr-str v))))))

(defdescribe render-log-test
  (it "renders a row per commit"
    (let [v (gr/render-log
              {:branch "main"
               :commits [{:short-sha "abc1234" :author "Vi" :at "2025-01-01" :subject "init"}
                         {:short-sha "def5678" :author "Vi" :at "2025-01-02" :subject "feat"}]})
          s (pr-str v)]
      (expect (strong-in? v "LOG"))
      (expect (re-find #"abc1234" s))
      (expect (re-find #"def5678" s))
      (expect (re-find #"feat" s)))))

(defdescribe render-show-test
  (it "renders commit metadata + numstat"
    (let [v (gr/render-show
              {:short-sha "abc1234" :sha "abc1234567" :author "Vi"
               :email "vi@example.org" :at "2025-01-01"
               :subject "subj" :body "body\nmore"
               :files [{:file "a.clj" :+ 2 :- 1}]
               :stat {:files 1 :+ 2 :- 1}})
          s (pr-str v)]
      (expect (strong-in? v "SHOW"))
      (expect (re-find #"subj" s))
      (expect (re-find #"body" s))
      (expect (re-find #"a\.clj" s)))))

(defdescribe render-blame-test
  (it "renders one row per blame line"
    (let [v (gr/render-blame
              {:path "x.clj" :head "abcdef0" :total 2 :ignored-revs []
               :lines [{:line 1 :short-sha "aaa1111" :author "A" :at "2025-01-01" :content "(ns x)"}
                       {:line 2 :short-sha "bbb2222" :author "B" :at "2025-01-02" :content "(def y 1)"}]})
          s (pr-str v)]
      (expect (strong-in? v "BLAME"))
      (expect (re-find #"x\.clj" s))
      (expect (re-find #"\(def y 1\)" s)))))

(defdescribe render-merge-status-test
  (it "says NO MERGE outside of a merge"
    (let [v (gr/render-merge-status {:in-progress? false})]
      (expect (strong-in? v "NO MERGE"))))

  (it "says MERGING with a conflict listing when in-progress"
    (let [v (gr/render-merge-status
              {:in-progress? true :branch "feature" :head "aaa1111" :merge-head "bbb2222"
               :conflicts [{:path "x.clj" :state "UU"}]})
          s (pr-str v)]
      (expect (strong-in? v "MERGING"))
      (expect (re-find #"x\.clj" s))
      (expect (re-find #"1 conflict" s))))

  (it "calls out 'ready for git/merge-continue!' when no conflicts remain"
    (let [v (gr/render-merge-status
              {:in-progress? true :branch "f" :head "a" :merge-head "b" :conflicts []})]
      (expect (re-find #"ready for" (pr-str v))))))

(defdescribe render-merge-op-test
  (it "renders op + path as a single line"
    (let [v (gr/render-merge-op {:path "x.clj" :op :git/merge-accept-ours})]
      (expect (strong-in? v "ACCEPT-OURS"))
      (expect (re-find #"x\.clj" (pr-str v))))))

(defdescribe render-merge-continue-test
  (it "renders the new head and message"
    (let [v (gr/render-merge-continue {:result :continued :head "abc1234" :message "merge-resolve"})
          s (pr-str v)]
      (expect (strong-in? v "MERGED"))
      (expect (re-find #"abc1234" s))
      (expect (re-find #"merge-resolve" s)))))

(defdescribe render-merge-abort-test
  (it "labels an aborted merge"
    (let [v (gr/render-merge-abort {:result :aborted})]
      (expect (strong-in? v "ABORTED")))))
