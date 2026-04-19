(ns com.blockether.vis.loop.runtime.prompt-activation-test
  "Tests that build-system-prompt only lists tools whose activation-fn
   would be true for the current env. Keeps the LLM from seeing tool
   names that resolve to inactive stubs in the sandbox."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.prompt :as prompt]))

(defn- build
  "Invoke build-system-prompt with the given activation flags. Non-tool
   opts are stubbed with minimal valid values."
  [flags]
  (prompt/build-system-prompt
    (merge
      {:has-reasoning? false
       :has-documents? false
       :has-conversation? false
       :has-concepts? false
       :document-summary nil
       :concept-graph-prompt nil
       :git-repos []
       :skill-registry nil}
      flags)))

(defdescribe prompt-gated-on-conversation
  (describe "BEFORE WRITING CODE, DECIDE block"
    (it "is included when a conversation is active"
      (let [p (build {:has-conversation? true})]
        (expect (str/includes? p "BEFORE WRITING CODE, DECIDE"))
        (expect (str/includes? p "(var-history '*reasoning*)"))
        (expect (str/includes? p "(conversation-history)"))
        (expect (str/includes? p "(restore-var 'sym)"))))

    (it "is elided for a fresh env with no conversation"
      ;; No conversation → history/restore/var tools are NOT bound → don't
      ;; advertise them. The LLM shouldn't be told to call things that
      ;; throw an inactive-tool error.
      (let [p (build {:has-conversation? false})]
        (expect (not (str/includes? p "BEFORE WRITING CODE, DECIDE")))
        (expect (not (str/includes? p "(var-history '*reasoning*)")))
        (expect (not (str/includes? p "(conversation-history)")))
        (expect (not (str/includes? p "(restore-var")))))

    (it "ARCH block's (var-history 'x) hint follows conversation activation"
      (let [active   (build {:has-conversation? true})
            inactive (build {:has-conversation? false})]
        (expect (str/includes? active "(var-history 'x)"))
        (expect (not (str/includes? inactive "(var-history 'x)")))))))

(defdescribe prompt-gated-on-concepts
  (describe "Concept navigation block"
    (it "is included when the DB holds extracted concepts"
      (let [p (build {:has-concepts? true :concept-graph-prompt "fake graph"})]
        (expect (str/includes? p "Concept navigation"))
        (expect (str/includes? p "(concept-info"))
        (expect (str/includes? p "(remove-concept"))
        (expect (str/includes? p "(edit-concept"))))

    (it "is elided when there are no concepts extracted"
      ;; Even if a concept-graph-prompt string is passed (shouldn't be, but
      ;; defensive), no concepts = no navigation block. The concept-graph
      ;; prompt only makes sense next to callable tools.
      (let [p (build {:has-concepts? false :concept-graph-prompt "should-not-leak"})]
        (expect (not (str/includes? p "Concept navigation")))
        (expect (not (str/includes? p "(concept-info")))
        (expect (not (str/includes? p "should-not-leak")))))))

(defdescribe prompt-gated-on-documents
  (describe "Documents block"
    (it "is included when documents are ingested"
      (let [p (build {:has-documents? true :document-summary "5 docs"})]
        (expect (str/includes? p "DOCUMENTS:"))
        (expect (str/includes? p "(search-documents"))
        (expect (str/includes? p "(fetch-document-content"))))

    (it "is elided when no documents are ingested"
      (let [p (build {:has-documents? false})]
        (expect (not (str/includes? p "DOCUMENTS:")))
        (expect (not (str/includes? p "(search-documents")))
        (expect (not (str/includes? p "(fetch-document-content"))))))

  (describe ":sources field in RESPONSE FORMAT spec"
    (it "is present when documents are active"
      (let [p (build {:has-documents? true :document-summary "5 docs"})]
        (expect (str/includes? p "sources:"))
        (expect (str/includes? p "IDs of sources"))))

    (it "is OMITTED entirely when no documents are ingested"
      ;; The LLM can't produce valid source IDs without retrieval tools —
      ;; don't even advertise :sources as an option. This is the schema-
      ;; level counterpart to dissocing inactive tools from the sandbox.
      (let [p (build {:has-documents? false})]
        (expect (not (str/includes? p "sources:")))
        (expect (not (str/includes? p "IDs of sources")))))))

(defdescribe prompt-gated-on-git
  (describe "Git context block"
    (it "is included when repos are attached"
      (let [p (build {:git-repos [{:name "svar" :path "/tmp/svar" :head-short "abc1234"
                                    :branch "main" :commits-ingested 10}]})]
        (expect (str/includes? p "GIT REPO"))
        (expect (str/includes? p "(git-search-commits"))))

    (it "is elided when no repos attached"
      (let [p (build {:git-repos []})]
        (expect (not (str/includes? p "GIT REPO")))
        (expect (not (str/includes? p "(git-search-commits")))))))

(def ^:private ALL_GATED_TOOL_NAMES
  "Every built-in tool that has an activation-fn. A fresh env (all flags
   off) MUST NOT mention any of these names — not in docs, not in spec
   descriptions, not in navigation hints. If a new gated tool is added
   without showing up here, this list needs updating."
  '[search-documents fetch-document-content search-batch
    conversation-history conversation-code conversation-results
    restore-var restore-vars var-history var-diff
    git-search-commits git-commit-history git-blame git-commit-diff
    git-file-history git-commit-parents git-commits-by-ticket
    concept-info remove-concept edit-concept])

(defdescribe prompt-fresh-env
  (describe "a fresh env (no conv, no docs, no concepts, no repos) produces minimal prompt"
    (it "omits every gated tool name — not even as a passing reference"
      ;; This is the hard invariant. The LLM MUST NOT see any gated tool
      ;; name anywhere in a fresh-env prompt. Otherwise it'll try to call
      ;; an unbound symbol. Includes iteration-spec field descriptions
      ;; (which share the prompt surface) — they must stay tool-agnostic.
      (let [p (build {})
            leaks (filter #(str/includes? p (str %)) ALL_GATED_TOOL_NAMES)]
        (expect (= [] leaks))))

    (it "omits every tool-group heading"
      (let [p (build {})]
        (expect (not (str/includes? p "BEFORE WRITING CODE, DECIDE")))
        (expect (not (str/includes? p "Concept navigation")))
        (expect (not (str/includes? p "DOCUMENTS:")))
        (expect (not (str/includes? p "GIT REPO")))))

    (it "still contains the core MINDSET/ARCH/SUB-CALLS/CLJ blocks"
      ;; Gated sections go away; the universal agent contract stays.
      (let [p (build {})]
        (expect (str/includes? p "MINDSET:"))
        (expect (str/includes? p "CONTEXT MODEL"))
        (expect (str/includes? p "ARCH:"))
        (expect (str/includes? p "SUB-CALLS:"))
        (expect (str/includes? p "PERF:"))
        (expect (str/includes? p "CLJ:"))))))
