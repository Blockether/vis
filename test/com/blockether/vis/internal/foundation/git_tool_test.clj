(ns com.blockether.vis.internal.foundation.git-tool-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.foundation.git-tool :as gt]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private render #'gt/render-git-result)
(def ^:private verbose-add #'gt/verbose-add-tokens)

(defdescribe verbose-add-tokens-test
  ;; `git add` is silent, so a bare `add` gets --verbose appended for the
  ;; SUBPROCESS run — git then lists each staged path — while the echoed
  ;; command stays the caller's original tokens.
  (it "appends --verbose to a bare add so it reports what it staged"
    (expect (= ["add" "-A" "--verbose"] (verbose-add ["add" "-A"])))
    (expect (= ["add" "." "--verbose"] (verbose-add ["add" "."]))))

  (it "never double-adds when a reporting flag is already present"
    (expect (= ["add" "-A" "--verbose"] (verbose-add ["add" "-A" "--verbose"])))
    (expect (= ["add" "-v" "-A"] (verbose-add ["add" "-v" "-A"])))
    ;; --dry-run / -n already self-reports, so leave it be.
    (expect (= ["add" "-n" "-A"] (verbose-add ["add" "-n" "-A"])))
    (expect (= ["add" "--dry-run" "."] (verbose-add ["add" "--dry-run" "."]))))

  (it "leaves every other subcommand untouched"
    (expect (= ["commit" "-m" "x"] (verbose-add ["commit" "-m" "x"])))
    (expect (= ["push"] (verbose-add ["push"])))
    (expect (= ["status" "--short"] (verbose-add ["status" "--short"])))))

(defdescribe render-git-result-test
  ;; The op-card a LONE `git` call paints (a single commit that is NOT part of a
  ;; grouped GIT band). The commit subject rides the collapsed headline so the
  ;; message is visible without expanding; the full message stays as the
  ;; blockquote body.
  (it "lifts a single commit's subject onto the headline and drops the -m noise"
    (let [{:keys [summary body]}
          (render {"args" ["commit" "-m" "tui: nicer git band" "-m" "explanatory body"]
                   "stdout" "[main f5a408ab] tui: nicer git band\n 2 files changed"
                   "exit" 0})]
      ;; Collapsed headline shows WHAT was committed, no crammed `-m -m`.
      (expect (= "commit — tui: nicer git band" summary))
      ;; Full multi-paragraph message still renders as a blockquote body.
      (expect (re-find #"(?m)^> tui: nicer git band" body))
      (expect (re-find #"(?m)^> explanatory body" body))))

  (it "keeps non-message flags but drops only -m/--message"
    (expect (= "commit -a — fix: thing"
              (:summary (render {"args" ["commit" "-a" "-m" "fix: thing"] "exit" 0}))))
    (expect (= "commit --amend — reword: x"
              (:summary (render {"args" ["commit" "--amend" "-m" "reword: x"] "exit" 0}))))
    (expect (= "commit — inline"
              (:summary (render {"args" ["commit" "--message=inline"] "exit" 0})))))

  (it "clips a really long commit subject on the headline; full message stays in the body"
    (let [subject (apply str "feat: " (repeat 30 "long-word "))
          {:keys [summary body]}
          (render {"args" ["commit" "-m" subject]
                   "stdout" "[main abc] x"
                   "exit" 0})]
      ;; Headline is bounded (72-char subject cap + ellipsis) so it can't blow
      ;; out the collapsed card, no matter how long the subject.
      (expect (<= (count summary) 82))
      (expect (str/ends-with? summary "\u2026"))
      (expect (str/starts-with? summary "commit \u2014 feat: long-word"))
      ;; The FULL subject is preserved untruncated in the blockquote body.
      (expect (re-find #"(?m)^> feat: long-word" body))
      (expect (str/includes? body (str/trim subject)))))

  (it "a FAILED commit keeps the (exit N) note as the headline's focus, no subject"
    (let [{:keys [summary]}
          (render {"args" ["commit" "-m" "wip"] "exit" 1 "stderr" "nothing to commit"})]
      (expect (= "commit -m (exit 1)" summary))))

  (it "a non-commit renders just its args (with any exit/timeout note)"
    (expect (= "push" (:summary (render {"args" ["push"] "exit" 0}))))
    (expect (= "push origin main (exit 1)"
              (:summary (render {"args" ["push" "origin" "main"] "exit" 1}))))
    (expect (= "status --short (timed out)"
              (:summary (render {"args" ["status" "--short"] "timed_out" true}))))))
