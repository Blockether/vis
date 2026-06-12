(ns com.blockether.vis.ext.foundation-git.render-test
  "Channel renderer shape tests for the `git/*` surface.

   Every `render-*` fn returns the Phase 1 `{:summary :display}`
   contract (`::extension/render-fn-result`). These tests pin that
   contract: `:summary` is IR-or-zones, `:display` is the full
   `[:ir]` body (paragraphs + code blocks + `[:strong]` badges). The
   badge label is the first `[:strong]` in the summary."
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.env-python :as ep]
   [com.blockether.vis.ext.foundation-git.render :as gr]
   [com.blockether.vis.ext.foundation-git.write-ops :as write-ops]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- ir? [x] (and (vector? x) (= :ir (first x))))

(defn- contract? [v]
  (and (extension/render-fn-result? v)
    (some? (:summary v))
    (some? (:display v))))

;; The badge label is the first `[:strong]` of the summary. After
;; normalization a `[:strong]` wraps `[:span {} text]`; match the text
;; anywhere inside a strong node.
(defn- strong-in? [v needle]
  (let [s (pr-str (extension/summary->ir (:summary v)))]
    (boolean (re-find (re-pattern (str "\\[:strong \\{\\} \\[:span \\{\\} \"" needle "\"\\]\\]")) s))))

;; Full rendered text across both summary and display, for free-form
;; body assertions (file names, counts, patch text).
(defn- text-of [v]
  (str (pr-str (extension/summary->ir (:summary v)))
    (pr-str (:display v))))

(defdescribe render-status-test
  (it "labels a clean tree CLEAN and a dirty tree DIRTY"
    (let [clean (gr/render-status {:branch "main" :head "abcdef1234" :clean? true :changes {}})
          dirty (gr/render-status {:branch "main" :head "abcdef1234" :clean? false
                                   :changes {:modified ["x.clj"]}})]
      (expect (contract? clean))
      (expect (contract? dirty))
      (expect (ir? (:display clean)))
      (expect (strong-in? clean "CLEAN"))
      (expect (strong-in? dirty "DIRTY"))
      (expect (re-find #"x\.clj" (text-of dirty))))))

(defdescribe render-diff-test
  (it "renders header + stat + numstat table"
    (let [v (gr/render-diff {:branch "main" :head "abcdef1234567"
                             :kind :branch :from "HEAD~1" :to nil
                             :stat {:files 2 :add 5 :del 1}
                             :files [{:file "a.clj" :add 3 :del 0}
                                     {:file "b.clj" :add 2 :del 1}]})
          s (text-of v)]
      (expect (contract? v))
      (expect (ir? (:display v)))
      (expect (strong-in? v "DIFF"))
      (expect (re-find #"2 file" s))
      (expect (re-find #"\+5" s))
      (expect (re-find #"a\.clj" s))))

  (it "embeds per-file patches when present"
    (let [v (gr/render-diff
              {:branch "main" :kind :workspace :from "HEAD" :to nil
               :stat {:files 1 :add 1 :del 0}
               :files [{:file "a.clj" :add 1 :del 0
                        :patch "diff --git a/a.clj b/a.clj\n+(def x 1)\n"}]})]
      (expect (contract? v))
      (expect (re-find #"diff --git" (pr-str (:display v))))))

  (it "the display body never duplicates the summary header, drops `──` separators, and lists untracked once"
    (let [v (gr/render-diff
              {:branch "main" :head "3e48931cabc" :kind :workspace :from "HEAD" :to nil
               :stat {:files 2 :add 5 :del 6}
               :files [{:file "a.clj" :add 1 :del 1
                        :patch "diff --git a/a.clj b/a.clj\n@@ -1 +1 @@\n-x\n+y\n"}
                       {:file "b.clj" :add 4 :del 5
                        :patch "diff --git a/b.clj b/b.clj\n@@ -1 +1 @@\n-p\n+q\n"}]
               ;; the tool already de-dups against numstat: only paths
               ;; numstat can't line-count arrive here, as bare strings
               :untracked [".junk"]})
          disp (pr-str (:display v))]
      (expect (contract? v))
      ;; No `── file ──` separator rows — JGit patches self-label.
      (expect (not (re-find #"──" disp)))
      ;; The DIFF label, range and +/- counts live ONLY in the summary; the
      ;; display must not repeat them (the expanded row showed twice before).
      (expect (not (re-find #"DIFF" disp)))
      (expect (not (re-find #"HEAD\.\." disp)))
      (expect (not (re-find #"2 file" disp)))
      ;; The untracked entry IS shown exactly once, with the ?? marker.
      (expect (= 1 (count (re-seq #"\?\? \.junk" disp))))
      ;; Both patches survive, concatenated into one diff block.
      (expect (= 2 (count (re-seq #"diff --git" disp)))))))

(defdescribe render-log-test
  (it "renders a row per commit"
    (let [v (gr/render-log
              {:branch "main"
               :commits [{:short-sha "abc1234" :author "Vi" :at "2025-01-01" :subject "init"}
                         {:short-sha "def5678" :author "Vi" :at "2025-01-02" :subject "feat"}]})
          s (text-of v)]
      (expect (contract? v))
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
               :files [{:file "a.clj" :add 2 :del 1}]
               :stat {:files 1 :add 2 :del 1}})
          s (text-of v)]
      (expect (contract? v))
      (expect (strong-in? v "SHOW"))
      (expect (re-find #"subj" s))
      (expect (re-find #"body" s))
      (expect (re-find #"a\.clj" s)))))

(defdescribe render-blame-test
  (it "renders the commit legend once, then one row per blame line"
    (let [v (gr/render-blame
              {:path "x.clj" :head "abcdef0" :total 2 :ignored-revs []
               :commits {"aaa1111" {:sha "aaa1111deadbeef" :author "A"
                                    :email "a@x" :at "2025-01-01"}
                         "bbb2222" {:sha "bbb2222deadbeef" :author "B"
                                    :email "b@x" :at "2025-01-02"}}
               :lines [{:line 1 :sha "aaa1111" :content "(ns x)"}
                       {:line 2 :sha "bbb2222" :content "(def y 1)"}]})
          s (text-of v)]
      (expect (contract? v))
      (expect (strong-in? v "BLAME"))
      (expect (re-find #"x\.clj" s))
      (expect (re-find #"\(def y 1\)" s))
      ;; legend states each author ONCE
      (expect (re-find #"\bA\b" s))
      (expect (re-find #"\bB\b" s))
      ;; lines reference the short-sha (legend key)
      (expect (re-find #"aaa1111" s))
      (expect (re-find #"bbb2222" s)))))

(defdescribe render-merge-status-test
  (it "says NO MERGE outside of a merge"
    (let [v (gr/render-merge-status {:in-progress? false})]
      (expect (contract? v))
      (expect (strong-in? v "NO MERGE"))))

  (it "says MERGING with a conflict listing when in-progress"
    (let [v (gr/render-merge-status
              {:in-progress? true :branch "feature" :head "aaa1111" :merge-head "bbb2222"
               :conflicts [{:path "x.clj" :state "UU"}]})
          s (text-of v)]
      (expect (contract? v))
      (expect (strong-in? v "MERGING"))
      (expect (re-find #"x\.clj" s))
      (expect (re-find #"1 conflict" s))))

  (it "calls out 'ready for git/merge-continue!' when no conflicts remain"
    (let [v (gr/render-merge-status
              {:in-progress? true :branch "f" :head "a" :merge-head "b" :conflicts []})]
      (expect (contract? v))
      (expect (re-find #"ready for" (pr-str (:display v)))))))

(defdescribe render-merge-op-test
  (it "renders op + path as a single line"
    (let [v (gr/render-merge-op {:path "x.clj" :op :git/merge-accept-ours})]
      (expect (contract? v))
      (expect (strong-in? v "MERGE-ACCEPT-OURS"))
      (expect (re-find #"x\.clj" (text-of v))))))

(defdescribe render-merge-continue-test
  (it "renders the new head and message"
    (let [v (gr/render-merge-continue {:result :continued :head "abc1234" :message "merge-resolve"})
          s (text-of v)]
      (expect (contract? v))
      (expect (strong-in? v "MERGED"))
      (expect (re-find #"abc1234" s))
      (expect (re-find #"merge-resolve" s)))))

(defdescribe render-merge-abort-test
  (it "labels an aborted merge"
    (let [v (gr/render-merge-abort {:result :aborted})]
      (expect (contract? v))
      (expect (strong-in? v "ABORTED")))))

;; The op-row paints the badge label from the :summary. The :display body must
;; NEVER repeat that label as its own `[:strong]` heading, or the badge renders
;; twice (the duplication the user hit on DIFF / ADD / COMMIT / …).
(defn- display-repeats-label? [v label]
  (boolean
    (re-find (re-pattern (str "\\[:strong \\{\\} \\[:span \\{\\} \""
                           (java.util.regex.Pattern/quote label) "\"\\]\\]"))
      (pr-str (:display v)))))

(defdescribe display-never-repeats-summary-label-test
  (it "no observation render echoes its badge label into the display body"
    (let [cases [["DIRTY" (gr/render-status {:branch "main" :head "abc1234" :clean? false
                                             :changes {:modified ["a.clj"]}})]
                 ["DIFF"  (gr/render-diff {:branch "main" :head "abc1234" :kind :workspace
                                           :from "HEAD" :to nil :stat {:files 1 :add 1 :del 0}
                                           :files [{:file "a.clj" :add 1 :del 0}]})]
                 ["LOG"   (gr/render-log {:branch "main"
                                          :commits [{:short-sha "abc1234" :author "Vi" :at "d" :subject "s"}]})]
                 ["SHOW"  (gr/render-show {:short-sha "abc1234" :sha "abc1234567" :author "Vi"
                                           :subject "subj" :body "body"
                                           :files [{:file "a.clj" :add 1 :del 0}] :stat {:files 1 :add 1 :del 0}})]
                 ["BLAME" (gr/render-blame {:path "x.clj" :head "abc1234" :total 1 :ignored-revs []
                                            :commits {"aaa1111" {:sha "aaa1111deadbeef" :author "A" :email "a@x" :at "d"}}
                                            :lines [{:line 1 :sha "aaa1111" :content "(ns x)"}]})]
                 ["MERGING" (gr/render-merge-status {:in-progress? true :branch "main" :head "abc1234"
                                                     :merge-head "def5678" :conflicts [{:path "a" :state "UU"}]})]]]
      (doseq [[label v] cases]
        (expect (contract? v))
        (expect (strong-in? v label))
        (expect (not (display-repeats-label? v label)))))))

(defdescribe model-render-boundary-test
  ;; Every `:model-render-fn` receives the tool result AFTER it crossed the
  ;; GraalPy boundary (the model's `git_status()` call returns a Python dict;
  ;; the engine converts it BACK via `->clj`, which snake-keywordizes every
  ;; key). These tests feed `ep/boundary-view` — the mechanical replica of
  ;; that round trip — so a render fn written against the RAW tool shape can
  ;; never silently drop data again. Regression: a dirty git_status pinned as
  ;; a bare `main @sha` header (no rows, no `· clean`) and the model read it
  ;; as a clean tree -> told the user "nothing to commit" while the user's
  ;; work sat uncommitted (session f5aba6d4, turn 4).
  (it "status: dirty tree renders its rows IDENTICALLY before and after the boundary"
    (let [raw {:branch "main" :head "365916de04"
               :changes {:modified ["a.clj" "b.clj"] :untracked ["new.txt"]}}
          s   (gr/model-render-status raw)]
      (expect (= s (gr/model-render-status (ep/boundary-view raw))))
      (expect (re-find #"M  a\.clj b\.clj" s))
      (expect (re-find #"\?\? {2}new\.txt" s))
      (expect (not (re-find #"clean" s)))))

  (it "status: clean tree says `· clean` after the boundary"
    (let [raw {:branch "main" :head "365916de04" :changes {}}]
      (expect (= "main @365916de04 · clean"
                (gr/model-render-status (ep/boundary-view raw))))))

  (it "status: FAILS CLOSED (nil -> generic dict fallback) on an unknown changes shape"
    ;; the pre-fix shape: porcelain codes as STRING keys boundary-keywordize
    ;; to :M / :?? which no bucket matches — the render must return nil so
    ;; the pin falls back to printing the raw dict, never a header that
    ;; reads as a clean tree
    (let [legacy {:branch "main" :head "365916de04"
                  :changes {"M" ["a.clj"] "??" ["new.txt"]}}]
      (expect (nil? (gr/model-render-status (ep/boundary-view legacy))))))

  (it "log: subject_only commits keep their sha after the boundary (no `?` columns)"
    ;; regression: slim-commit emitted :short-sha (kebab) which round-trips
    ;; to :short_sha — the model saw `?    ?  subject` rows
    (let [raw {:branch "main"
               :commits [{:short_sha "abc1234" :subject "fix: thing"}
                         {:short_sha "def5678" :subject "feat: other"}]}
          s   (gr/model-render-log (ep/boundary-view raw))]
      (expect (= s (gr/model-render-log raw)))
      (expect (re-find #"abc1234" s))
      (expect (re-find #"def5678" s))
      (expect (not (re-find #"\?" s)))))

  (it "log: full commits render author/at/subject after the boundary"
    (let [raw {:branch "main"
               :commits [{:sha "abc1234567890" :short_sha "abc1234" :author "Vi"
                          :email "v@x" :at 1781265158162 :subject "fix: thing"}]}
          s   (gr/model-render-log (ep/boundary-view raw))]
      (expect (re-find #"abc1234" s))
      (expect (re-find #"fix: thing" s))
      (expect (re-find #"Vi" s))))

  (it "show: commit header survives the boundary (short-sha kebab key tolerated)"
    ;; git_show returns the canonical commit map from internal/git which
    ;; still spells :short-sha — after the boundary that is :short_sha;
    ;; raw (kebab) reaches the CHANNEL render. Both must show the sha.
    (let [raw {:short-sha "abc1234" :sha "abc1234567890" :author "Vi"
               :at 1781265158162 :subject "subj" :body "body text"
               :files [{:file "a.clj" :add 1 :del 0}] :stat {:files 1 :add 1 :del 0}}
          before (gr/model-render-show raw)
          after  (gr/model-render-show (ep/boundary-view raw))]
      (doseq [s [before after]]
        (expect (re-find #"abc1234" s))
        (expect (re-find #"subj" s))
        (expect (re-find #"\+1 -0 {2}a\.clj" s)))))

  (it "diff: numstat + untracked rows survive the boundary"
    (let [raw {:branch "main" :head "abc1234567" :kind :trunk
               :from "HEAD" :stat {:files 2 :add 5 :del 1}
               :files [{:file "a.clj" :add 5 :del 1}]
               :untracked ["new.txt"]}
          s   (gr/model-render-diff (ep/boundary-view raw))]
      (expect (= s (gr/model-render-diff raw)))
      (expect (re-find #"\+5 -1 {2}a\.clj" s))
      (expect (re-find #"\?\? new\.txt" s))))

  (it "write ops: the op badge survives the boundary (keyword :op becomes a snake string)"
    (let [raw {:op :git/commit :short-sha "abc1234" :branch "main"}
          before (write-ops/model-render-write raw)
          after  (write-ops/model-render-write (ep/boundary-view raw))]
      (expect (re-find #"^COMMIT" before))
      (expect (re-find #"^COMMIT" after))
      (expect (re-find #"abc1234" after)))))
