(ns com.blockether.vis.internal.foundation.git-tool-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.git-tool :as gt]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private render #'gt/render-git-result)

(def ^:private render-batch #'gt/render-git-batch-result)

(def ^:private verbose-add #'gt/verbose-add-tokens)

(def ^:private git-impl #'gt/git-impl)

(def ^:private activate? (:ext/activation-fn gt/vis-extension))

(defdescribe git-activation-test
             (it "activates for nested repositories but stays absent from Git-free workspaces"
                 (let [repositories (atom [])]
                   (with-redefs
                     [git/cwd-file (constantly (java.io.File. "/workspace"))
                      git/in-repository? (constantly false)
                      vis/repository-inventory (fn [_]
                                                 {:repositories @repositories})]

                     (expect (false? (boolean (activate? {}))))
                     (reset! repositories [{:root "/workspace/vis"}])
                     (expect (true? (boolean (activate? {}))))))))

(defdescribe
  git-native-contract-test
  (it "documents the direct native call shape and result semantics"
      (let [description (:ext.symbol/description gt/git-symbol)]
        (expect (str/includes? description "session[\"workspace\"]"))
        ;; The argv shape belongs on the `commands` parameter it governs,
        ;; stated once rather than mirrored into the tool description.
        (let
          [commands (get-in gt/git-symbol [:ext.symbol/schema :properties "commands" :description])]
          (expect (str/includes? commands "[[\"status\", \"--short\"]]"))
          (expect (str/includes? commands "`git` omitted")))
        (expect (str/includes? description "non-zero exits"))
        (expect (< (count description) 300))))
  (it "makes command batches the closed native contract"
      (let [schema (:ext.symbol/schema gt/git-symbol)]
        (expect (= ["commands"] (:required schema)))
        (expect (= ["commands"] (get-in gt/git-symbol [:ext.symbol/call :pos])))
        (expect (false? (:additionalProperties schema)))
        (expect (= "array" (get-in schema [:properties "commands" :type])))
        (expect (= 1 (get-in schema [:properties "commands" :minItems])))
        (expect (= "array" (get-in schema [:properties "commands" :items :type])))
        (expect (= "string" (get-in schema [:properties "commands" :items :items :type])))
        (expect (str/includes? (:doc (meta #'gt/git)) "await git")))))
(defdescribe
  git-python-sandbox-test
  (it "await git exposes each serial command's stdout, stderr, and exit as plain Python data"
      (let [seen (atom [])]
        (with-redefs
          [shell/run-argv
           (fn [_ argv _]
             (swap! seen conj (vec (rest argv)))
             (case (second argv)
               "status"
               {"exit" 0 "stdout" "clean\n" "stderr" "" "timed_out" false "duration_ms" 1}

               "show"
               {"exit" 128 "stdout" "" "stderr" "bad revision\n" "timed_out" false "duration_ms" 2}

               "diff"
               {"exit" 0 "stdout" "diff\n" "stderr" "warning\n" "timed_out" false "duration_ms" 3}))]
          (let
            [{:keys [python-context]} (ep/create-python-context
                                        {'git (fn [commands]
                                                ;; The Python bridge receives the tool's result, not
                                                ;; its internal extension envelope.
                                                (:result (git-impl {} commands nil)))})
             result
             (ep/run-python-block
               python-context
               (str
                 "r = await git([['status', '--short'], ['show', 'missing'], ['diff', '--stat']])\n"
                 "[(c['stdout'], c['stderr'], c['exit']) for c in r['commands']]")
               "t1/i1")]

            (expect (nil? (:error result)))
            (expect (= [["status" "--short"] ["show" "missing"] ["diff" "--stat"]] @seen))
            (expect (= [["clean\n" "" 0] ["" "bad revision\n" 128] ["diff\n" "warning\n" 0]]
                       (:result result))))))))


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

(defdescribe
  shared-git-routing-test
  (it
    "delegates literal argv to the shared Git command adapter"
    (let
      [root
       (.toFile (java.nio.file.Files/createTempDirectory
                  "vis-git-tool-routing"
                  (make-array java.nio.file.attribute.FileAttribute 0)))

       seen
       (atom nil)]

      (try (with-redefs
             [workspace/cwd
              (fn []
                root)

              shell/run-argv
              (fn [_ argv opts]
                (reset! seen [argv opts])
                {"exit" 1 "stdout" "" "stderr" "verification required" "timed_out" false "duration_ms" 3})]

             (let
               [args
                ["-C" "other" "commit" "-m" "x"]

                result
                (:result (git-impl {} [args] nil))

                command
                (first (get result "commands"))]

               (expect (= 1 (get command "exit")))
               (expect (= "verification required" (get command "stderr")))
               (expect (= (into ["git"] args) (first @seen)))
               (expect (= {"timeout_secs" 120} (second @seen)))))
           (finally (.delete root))))))
(defdescribe
  git-batch-test
  (it
    "keeps stdout, stderr, and partial failures with their own serial commands"
    (let [seen (atom [])]
      (with-redefs
        [shell/run-argv
         (fn [_ argv _]
           (swap! seen conj (vec (rest argv)))
           (case (second argv)
             "status"
             {"exit" 0 "stdout" " M src/core.clj\n" "stderr" "" "timed_out" false "duration_ms" 1}

             "show"
             {"exit" 128 "stdout" "" "stderr" "bad revision" "timed_out" false "duration_ms" 2}

             "diff"
             {"exit" 0
              "stdout" " src/core.clj | 2 +-\n"
              "stderr" "warning: renamed"
              "timed_out" false
              "duration_ms" 3}))]
        (let
          [result (:result
                    (git-impl {} [["status" "--short"] ["show" "missing"] ["diff" "--stat"]] nil))
           commands (get result "commands")
           {:keys [summary body]} (render-batch result)]

          (expect (= [["status" "--short"] ["show" "missing"] ["diff" "--stat"]] @seen))
          (expect (= 3 (count commands)))
          (expect (= " M src/core.clj\n" (get-in commands [0 "stdout"])))
          (expect (= "bad revision" (get-in commands [1 "stderr"])))
          (expect (= 128 (get-in commands [1 "exit"])))
          (expect (= " src/core.clj | 2 +-\n" (get-in commands [2 "stdout"])))
          (expect (= "warning: renamed" (get-in commands [2 "stderr"])))
          (expect (= "⎇ 3 git commands — 2 succeeded, 1 failed" summary))
          (expect (str/includes? body " M src/core.clj"))
          (expect (str/includes? body "bad revision"))
          ;; One blank row on each side of the divider keeps command cards distinct.
          (expect (re-find #"(?s)### 1[.].*?\n\n────────────\n\n### 2[.]" body))
          (expect (= 2 (count (re-seq #"────────────" body))))
          (expect (str/includes? body "warning: renamed")))))))


(defdescribe
  render-git-result-test
  ;; The op-card a LONE `git` call paints (a single commit that is NOT part of a
  ;; grouped GIT band). The commit subject rides the collapsed headline so the
  ;; message is visible without expanding; the full message stays as the
  ;; blockquote body.
  (it "lifts a single commit's subject onto the headline and drops the -m noise"
      (let
        [{:keys [summary body]}
         (render {"args" ["commit" "-m" "tui: nicer git band" "-m" "explanatory body"]
                  "stdout" "[main f5a408ab] tui: nicer git band\n 2 files changed"
                  "exit" 0})]
        ;; Collapsed headline shows WHAT was committed, no crammed `-m -m`.
        (expect (= "⎇ commit — tui: nicer git band" summary))
        ;; Expanded body now matches shell/repl: labeled details with the full
        ;; multi-paragraph commit message preserved as a blockquote.
        (expect (str/includes? body "**COMMAND**"))
        (expect (str/includes? body "**STATUS**"))
        (expect (str/includes? body "**MESSAGE**"))
        (expect (re-find #"(?m)^> tui: nicer git band" body))
        (expect (re-find #"(?m)^> explanatory body" body))))
  (it "keeps non-message flags but drops only -m/--message"
      (expect (= "⎇ commit -a — fix: thing"
                 (:summary (render {"args" ["commit" "-a" "-m" "fix: thing"] "exit" 0}))))
      (expect (= "⎇ commit --amend — reword: x"
                 (:summary (render {"args" ["commit" "--amend" "-m" "reword: x"] "exit" 0}))))
      (expect (= "⎇ commit — inline"
                 (:summary (render {"args" ["commit" "--message=inline"] "exit" 0})))))
  (it "clips a really long commit subject on the headline; full message stays in the body"
      (let
        [subject
         (apply str "feat: " (repeat 30 "long-word "))

         {:keys [summary body]}
         (render {"args" ["commit" "-m" subject] "stdout" "[main abc] x" "exit" 0})]

        ;; Headline is bounded (72-char subject cap + ellipsis) so it can't blow
        ;; out the collapsed card, no matter how long the subject.
        (expect (<= (count summary) 84))
        (expect (str/ends-with? summary "\u2026"))
        (expect (str/starts-with? summary "⎇ commit \u2014 feat: long-word"))
        ;; The FULL subject is preserved untruncated in the blockquote body.
        (expect (re-find #"(?m)^> feat: long-word" body))
        (expect (str/includes? body (str/trim subject)))))
  (it "a FAILED commit keeps the (exit N) note as the headline's focus, no subject"
      (let
        [{:keys [summary]} (render
                             {"args" ["commit" "-m" "wip"] "exit" 1 "stderr" "nothing to commit"})]
        (expect (= "⎇ commit -m (exit 1)" summary))))
  (it "a non-commit renders just its args (with any exit/timeout note)"
      (expect (= "⎇ push" (:summary (render {"args" ["push"] "exit" 0}))))
      (expect (= "⎇ push origin main (exit 1)"
                 (:summary (render {"args" ["push" "origin" "main"] "exit" 1}))))
      (expect (= "⎇ status --short (timed out)"
                 (:summary (render {"args" ["status" "--short"] "timed_out" true}))))))
