(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe prompt-assembly-test
  (it "normalizes core addendum and extension prompt text"
    (let [ext {:ext/name "test.prompt"
               :ext/engine {:ext.engine/alias 't}
               :ext/prompt-fn (fn [_]
                                "\n\n    Extension line\n\n\n\n      Nested extension line\n")}
          env {:extensions (atom [ext])}
          messages (prompt/assemble-stable-prompt-messages env
                     {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                      :active-extensions [ext]})
          text (prompt/stable-prompt-text messages)]
      (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
      (expect (str/includes? text "Extension line\n\n  Nested extension line"))
      (expect (not (str/includes? text "\n\n\n"))))))

(defdescribe cli-autonomous-override-test
  (it "drops the candidate approval STOP for the non-interactive :cli channel only"
    (let [text-for (fn [ch]
                     (-> (prompt/assemble-stable-prompt-messages
                           {:channel ch} {:active-extensions []})
                       prompt/stable-prompt-text))
          marker "NON-INTERACTIVE ONE-SHOT RUN"]
      ;; :cli (headless one-shot — no approver) gets the override
      (expect (str/includes? (text-for :cli) marker))
      (expect (str/includes? (text-for :cli) "NEVER stop to wait for approval"))
      ;; interactive / card-bearing channels keep the approval flow
      (expect (not (str/includes? (text-for :tui) marker)))
      (expect (not (str/includes? (text-for :web) marker)))
      (expect (not (str/includes? (text-for nil) marker))))))

(defdescribe prompt-core-test
  (it "documents engine-owned forms as bare, not extension tools"
    ;; CORE_SYSTEM_PROMPT pins: bare-symbol ENGINE FNS section.
    ;; Engine fns are emitted without namespace qualification.
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "bare snake_case"))
      (expect (str/includes? text "namespace-qualif"))
      (expect (str/includes? text "Session titles are host-generated"))))

  (it "carries Epistemic + Identity stance so the model probes the project first"
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "Epistemic stance"))
      (expect (str/includes? text "runtime > source > docs > assumption"))
      (expect (str/includes? text "Identity"))
      (expect (str/includes? text "host project"))
      ;; Identity must be project-agnostic: it has to work in any repo.
      (expect (not (str/includes? text "the Vis PROJECT")))))

  (it "teaches anchored editing: cat's lineno:hash passed straight to patch from_anchor"
    ;; Native `patch` takes the `lineno:hash` the model sees in cat output
    ;; directly as `from_anchor` — the old hunk/anchor Python helpers are gone.
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "lineno:hash"))
      (expect (str/includes? text "from_anchor")))))

(defdescribe project-instructions-hoist-test
  (it "injects AGENTS.md contents as a dedicated PROJECT-INSTRUCTIONS system block"
    (with-redefs [agents/instructions
                  (constantly {:found? true
                               :source :repo
                               :path "/tmp/repo/AGENTS.md"
                               :content "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
        (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
        (expect (str/includes? text "/tmp/repo/AGENTS.md"))
        ;; Send order: SYSTEM-PROMPT first, then PROJECT-INSTRUCTIONS.
        (expect (< (str/index-of text "SYSTEM-PROMPT")
                  (str/index-of text "PROJECT-INSTRUCTIONS"))))))

  (it "falls back to CLAUDE.md when AGENTS.md is absent and labels the source"
    (with-redefs [agents/instructions
                  (constantly {:found? true
                               :source :repo:claude-md-fallback
                               :path "/tmp/repo/CLAUDE.md"
                               :content "CLAUDE-FALLBACK-RULE"})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
        (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
        (expect (str/includes? text "CLAUDE.md")))))

  (it "emits no PROJECT-INSTRUCTIONS block when no guidance file is present"
    (with-redefs [agents/instructions (constantly {:found? false})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (not (str/includes? text "PROJECT-INSTRUCTIONS")))))))

(defdescribe extension-activation-test
  (it "assembles from precomputed active extensions without activating again"
    (let [calls (atom 0)
          ext {:ext/name "test.activation"
               :ext/activation-fn (fn [_]
                                    (swap! calls inc)
                                    true)
               :ext/prompt-fn (constantly "Active prompt")}
          env {:extensions (atom [ext])}
          active (prompt/active-extensions env)]
      (expect (= 1 @calls))
      (prompt/assemble-stable-prompt-messages env {:active-extensions active})
      (expect (= 1 @calls)))))

(defdescribe reasoning-via-comments-nudge-test
  "Reason-via-code-comments fallback for non-reasoning models."
  (it "inserts a system nudge after leading system messages, before the conversation"
    (let [msgs [{:role "system" :content "core"}
                {:role "system" :content "project"}
                {:role "user" :content "do the thing"}]
          out  (prompt/with-reasoning-comments-nudge msgs)]
      ;; one message added
      (expect (= (inc (count msgs)) (count out)))
      ;; inserted at the system/conversation boundary (index 2), still a system msg
      (expect (= "system" (:role (nth out 2))))
      (expect (str/includes? (:content (nth out 2)) ";;"))
      ;; leading systems untouched, user message still last
      (expect (= "core" (:content (first out))))
      (expect (= "user" (:role (last out))))))

  (it "prepends when there are no leading system messages"
    (let [out (prompt/with-reasoning-comments-nudge [{:role "user" :content "x"}])]
      (expect (= "system" (:role (first out))))
      (expect (= 2 (count out))))))

(defdescribe assemble-initial-messages-images-test
  "Image attachments turn the initial user message multimodal."
  (it "keeps text-only messages as a plain content string"
    (let [msgs (prompt/assemble-initial-messages
                 {:stable-prompt-messages [{:role "system" :content "sys"}]
                  :initial-user-content   "hello"})
          user (last msgs)]
      (expect (= "user" (:role user)))
      (expect (string? (:content user)))
      (expect (str/includes? (:content user) "CURRENT-USER-MESSAGE"))
      (expect (not (str/includes? (:content user) "ATTACHED-IMAGES")))))

  (it "rides svar image blocks ahead of the text block and lists a manifest"
    (let [msgs (prompt/assemble-initial-messages
                 {:stable-prompt-messages []
                  :initial-user-content   "what is on /tmp/shot.png?"
                  :user-images  [{:path "/tmp/shot.png" :media-type "image/png"
                                  :base64 "aGVsbG8=" :size 5 :size-label "5B"}]
                  :skipped-images [{:path "/tmp/huge.png" :reason "6.0MB exceeds the 5.0MB attachment limit"}]})
          user (last msgs)
          blocks (:content user)]
      (expect (= "user" (:role user)))
      (expect (vector? blocks))
      ;; image block first (svar/user contract), text block last
      (expect (= "image_url" (:type (first blocks))))
      (expect (str/includes? (get-in (first blocks) [:image_url :url])
                "data:image/png;base64,aGVsbG8="))
      (let [text (:text (last blocks))]
        (expect (str/includes? text "CURRENT-USER-MESSAGE"))
        (expect (str/includes? text "ATTACHED-IMAGES"))
        (expect (str/includes? text "/tmp/shot.png (image/png, 5B)"))
        (expect (str/includes? text "NOT attached"))
        (expect (str/includes? text "/tmp/huge.png")))))

  (it "omits the manifest when there is no user content at all"
    (let [msgs (prompt/assemble-initial-messages
                 {:stable-prompt-messages [{:role "system" :content "sys"}]
                  :user-images [{:path "p" :media-type "image/png"
                                 :base64 "eA==" :size 1 :size-label "1B"}]})]
      ;; no user message without initial-user-content — images can't ride alone
      (expect (= 1 (count msgs)))
      (expect (= "system" (:role (first msgs)))))))
