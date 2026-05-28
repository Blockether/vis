(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe prompt-assembly-test
  (it "normalizes core addendum and extension prompt text"
    (let [ext {:ext/name "test.prompt"
               :ext/sci {:ext.sci/alias 't}
               :ext/prompt (fn [_]
                             "\n\n    Extension line\n\n\n\n      Nested extension line\n")}
          env {:extensions (atom [ext])}
          messages (prompt/assemble-stable-prompt-messages env
                     {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                      :active-extensions [ext]})
          text (prompt/stable-prompt-text messages)]
      (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
      (expect (str/includes? text "Extension line\n\n  Nested extension line"))
      (expect (not (str/includes? text "\n\n\n"))))))

(defdescribe prompt-core-test
  (it "documents engine-owned forms as bare, not extension tools"
    ;; CORE_SYSTEM_PROMPT pins: bare-symbol ENGINE FNS section.
    ;; Engine fns are emitted without namespace qualification.
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "bare symbols"))
      (expect (str/includes? text "never namespace-qualify"))
      (expect (str/includes? text "Session titles are host-generated"))))

  (it "carries EPISTEMIC + IDENTITY stance so the model probes the project first"
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "EPISTEMIC"))
      (expect (str/includes? text "runtime > source > docs > assumption"))
      (expect (str/includes? text "IDENTITY"))
      (expect (str/includes? text "HOST PROJECT around the sandbox"))
      ;; IDENTITY must be project-agnostic: it has to work in any repo.
      (expect (not (str/includes? text "the Vis PROJECT"))))))

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

;; ---------------------------------------------------------------------------
;; per-scope prompt assembly + CONSULT_BASE_PROMPT.
;;
;; The consult mini-SCI engine must NOT see primary's CORE_SYSTEM_PROMPT,
;; project rules, or per-extension :ext/prompt fragments. It DOES see a
;; minimal consult role + auto-generated BINDINGS docs from every symbol
;; whose :ext.symbol/engine-scope contains :consult.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; CORE_SYSTEM_PROMPT carries the consult surface
;; ---------------------------------------------------------------------------

(defdescribe core-system-prompt-consult-surface-test
  (describe "primary system prompt advertises the consult API"
    (let [text (prompt/build-system-prompt {})]
      (it "mentions (consult-request! :id :pref {:focus [...] :question …})"
        (expect (re-find #"consult-request!\s*:id\s*:preference\s*\{:focus" text)))
      (it "mentions consult-promote! / consult-dismiss!"
        (expect (str/includes? text "consult-promote!"))
        (expect (str/includes? text "consult-dismiss!")))
      (it "describes the synthetic trailer-pin storage model"
        (expect (str/includes? text ":tag :consult"))
        (expect (str/includes? text "trailer")))
      (it "mentions the (done {…}) unified emission point"
        (expect (str/includes? text "(done {…})")))
      (it "mentions the done gate / refusal while consults pending"
        (expect (re-find #"(?s)done IS\s+REFUSED|done blocked" text))))

    (let [text (prompt/build-system-prompt {})]
      (it "no `(consult-fast)` / `(consult-balanced)` / `(consult-deep)`"
        (expect (not (re-find #"\(consult-fast\b" text)))
        (expect (not (re-find #"\(consult-balanced\b" text)))
        (expect (not (re-find #"\(consult-deep\b" text))))
      (it "no `await-consult!` (resolved entries are inline trailer pins)"
        (expect (not (str/includes? text "await-consult!"))))
      (it "no `:session/consult-results` (storage is the trailer)"
        (expect (not (str/includes? text ":session/consult-results")))))))

(defdescribe consult-system-prompt-test
  (it "CONSULT_BASE_PROMPT exists and carries a SECONDARY CONSULTANT role line"
    (expect (string? prompt/CONSULT_BASE_PROMPT))
    (expect (str/includes? prompt/CONSULT_BASE_PROMPT "SECONDARY CONSULTANT")))

  (it "consult system prompt does NOT carry CORE_SYSTEM_PROMPT marker text"
    ;; Strong contract: consult LLM must not be primed with primary's FSM,
    ;; engine mutator docs, or the bare-symbols ENGINE FNS section.
    (let [text (prompt/build-consult-system-prompt [])]
      (expect (not (str/includes? text "TURN LIFECYCLE")))
      (expect (not (str/includes? text "spec-set!")))
      (expect (not (str/includes? text "bare symbols")))
      (expect (not (str/includes? text "EPISTEMIC")))))

  (it "consult system prompt carries a DONE SCHEMA + cap rules + (done …) emission"
    ;; consult emits `(done {…})` — SAME form as primary,
    ;; different schema. One mental model across both engines.
    (let [text (prompt/build-consult-system-prompt [])]
      (expect (str/includes? text "DONE SCHEMA"))
      (expect (str/includes? text "TOKEN CAPS"))
      (expect (str/includes? text "(done"))
      ;; Must NOT promote a separate `(answer …)` form — we unified.
      (expect (not (str/includes? text "(answer")))))

  (it "BINDINGS block auto-generates docs from :consult-scope symbols only"
    (let [search-ext {:ext/name "test.search"
                      :ext/sci
                      {:ext.sci/alias 'search
                       :ext.sci/symbols
                       [{:ext.symbol/symbol 'web
                         :ext.symbol/fn (fn [_q] [])
                         :ext.symbol/doc "Live web search via exa.ai"
                         :ext.symbol/arglists '([query])
                         :ext.symbol/engine-scope #{:consult}}
                        {:ext.symbol/symbol 'papers
                         :ext.symbol/fn (fn [_q] [])
                         :ext.symbol/doc "arxiv paper search"
                         :ext.symbol/arglists '([query])
                         :ext.symbol/engine-scope #{:consult}}]}}
          v-ext      {:ext/name "test.v"
                      :ext/sci
                      {:ext.sci/alias 'v
                       :ext.sci/symbols
                       [{:ext.symbol/symbol 'patch
                         :ext.symbol/fn (fn [_x] :ok)
                         :ext.symbol/doc "Filesystem mutation"
                         :ext.symbol/arglists '([edits])
                         :ext.symbol/engine-scope #{:main}}
                        {:ext.symbol/symbol 'cat
                         :ext.symbol/fn (fn [_p] "")
                         :ext.symbol/doc "Read a file fully"
                         :ext.symbol/arglists '([path])
                         :ext.symbol/engine-scope #{:main :consult}}]}}
          text (prompt/build-consult-system-prompt [search-ext v-ext])]
      (expect (str/includes? text "search/web"))
      (expect (str/includes? text "search/papers"))
      (expect (str/includes? text "v/cat"))           ;; dual-scope read-only
      (expect (not (str/includes? text "v/patch")))))) ;; :main-only excluded

(defdescribe consult-prompt-messages-test
  (it "assemble-consult-prompt-messages returns a one-element system message"
    (let [msgs (prompt/assemble-consult-prompt-messages [])]
      (expect (= 1 (count msgs)))
      (expect (= "system" (:role (first msgs))))))

  (it "the system message is the same string build-consult-system-prompt returns"
    (let [exts []
          msgs (prompt/assemble-consult-prompt-messages exts)]
      (expect (= (prompt/build-consult-system-prompt exts)
                (:content (first msgs)))))))

(defdescribe extension-activation-test
  (it "assembles from precomputed active extensions without activating again"
    (let [calls (atom 0)
          ext {:ext/name "test.activation"
               :ext/activation-fn (fn [_]
                                    (swap! calls inc)
                                    true)
               :ext/prompt (constantly "Active prompt")}
          env {:extensions (atom [ext])}
          active (prompt/active-extensions env)]
      (expect (= 1 @calls))
      (prompt/assemble-stable-prompt-messages env {:active-extensions active})
      (expect (= 1 @calls)))))
