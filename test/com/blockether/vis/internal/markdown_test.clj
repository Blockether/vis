(ns com.blockether.vis.internal.markdown-test
  "Pure-function coverage for the conversation -> Markdown exporter.

   Why it exists: AGENTS.md hard rule \u2014 every namespace ships a
   matching `_test.clj`. We pin the projection's shape (header, blockquoted
   user text, italic meta, turn separators, fall-back placeholders) by
   stubbing the two persistance reads it depends on; no SQLite needed.

   These tests are the spec of `conversation->markdown`. Anyone changing
   the output shape MUST update this file in the same commit \u2014 every
   downstream consumer (TUI `Copy as Markdown`, third-party channels,
   future `vis export` CLI) reads the same projection."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.markdown :as md]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; -----------------------------------------------------------------------------
;; Stubs
;;
;; The exporter pulls two facts from persistance: `db-get-conversation`
;; and `db-list-conversation-turns`. We bypass the registry by binding
;; the dispatch fn to a synthetic backend keyed off `db-info :stub`.

(defmacro ^:private with-stubbed-persistance [conversation queries & body]
  `(with-redefs [persistance/db-get-conversation
                 (fn [~'_ ~'_] ~conversation)
                 persistance/db-list-conversation-turns
                 (fn [~'_ ~'_] ~queries)]
     ~@body))

(def ^:private stub-db {:type :stub})

(def ^:private fixed-conversation
  {:id          (java.util.UUID/fromString "1f9bf3ac-1234-4567-8901-abcdefabcdef")
   :type        :conversation
   :channel     :tui
   :title       "Markdown export sanity check"
   :model       "gpt-4o"
   :provider    :openai
   :created-at  (java.util.Date. 0)
   :system-prompt "You are a careful assistant."})

(def ^:private fixed-turns
  [{:id              (java.util.UUID/randomUUID)
    :type            :query
    :text            "What is 2+2?"
    :answer          "4"
    :status          :ok
    :iteration-count 1
    :duration-ms     1234
    :total-cost      0.000123
    :provider        :openai
    :model           "gpt-4o"
    :input-tokens    1024
    :output-tokens   12
    :reasoning-tokens 5
    :cached-tokens   0}
   {:id              (java.util.UUID/randomUUID)
    :type            :query
    :text            "Multi-line\nuser request\nwith blanks\n\ninside."
    :answer          "Multi-line\n\nanswer body."
    :status          :ok
    :iteration-count 3
    :duration-ms     5678
    :total-cost      0.001
    :provider        :openai
    :model           "gpt-4o"
    :input-tokens    4096
    :output-tokens   256}])

;; -----------------------------------------------------------------------------

(defdescribe conversation->markdown-test
  (describe "Header"
    (it "Renders the title as a top-level H1"
      (with-stubbed-persistance fixed-conversation []
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/starts-with? out "# Markdown export sanity check")))))

    (it "Falls back to 'Untitled conversation' when the conversation has no title"
      (with-stubbed-persistance (assoc fixed-conversation :title nil) []
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "# Untitled conversation")))))

    (it "Surfaces id, channel, provider, model and turn count in the meta blockquote"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "id `1f9bf3ac`"))
          (expect (str/includes? out "channel `tui`"))
          (expect (str/includes? out "openai"))
          (expect (str/includes? out "gpt-4o"))
          (expect (str/includes? out "2 turns")))))

    (it "Singularises `1 turn` (no trailing 's')"
      (with-stubbed-persistance fixed-conversation [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "1 turn"))
          (expect (not (str/includes? out "1 turns")))))))

  (describe "Turns"
    (it "Numbers turns 1-based with `## Turn N` headings"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "## Turn 1"))
          (expect (str/includes? out "## Turn 2")))))

    (it "Quotes each line of the user request with `> `"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "> What is 2+2?"))
          (expect (str/includes? out "> Multi-line"))
          (expect (str/includes? out "> user request"))
          ;; Blank source lines render as a bare `>` so the blockquote
          ;; doesn't visually break apart in a renderer.
          (expect (str/includes? out "\n>\n")))))

    (it "Emits the answer body verbatim, NOT inside a blockquote"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "\n\n4"))
          (expect (str/includes? out "Multi-line\n\nanswer body.")))))

    (it "Separates turns with a horizontal rule by default"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "\n\n---\n\n## Turn 2")))))

    (it "Suppresses horizontal rules when :turn-separator is empty"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation)
                    {:turn-separator ""})]
          (expect (not (str/includes? out "---"))))))

    (it "Falls back to a placeholder when a turn has no answer (in-flight)"
      (with-stubbed-persistance fixed-conversation
        [{:text "still working" :status :running :iteration-count 1}]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "*(no answer recorded yet)*")))))

    (it "Falls back to an error placeholder when a turn errored without an answer"
      (with-stubbed-persistance fixed-conversation
        [{:text "boom" :status :error}]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "*(turn errored \u2014 no answer recorded)*"))))))

  (describe "Italic meta line on the assistant header"
    (it "Surfaces provider, model, iteration count, duration, cost AND tokens"
      (with-stubbed-persistance fixed-conversation [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "**Assistant:** *\u2014 openai \u00b7 gpt-4o"))
          (expect (str/includes? out "1 iter"))
          (expect (str/includes? out "1,024 in"))
          (expect (str/includes? out "12 out"))
          (expect (str/includes? out "5 think")))))

    (it "Pluralises iteration counts above 1"
      (with-stubbed-persistance fixed-conversation [(second fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "3 iters")))))

    (it "Drops zero / nil token components instead of emitting empty units"
      (with-stubbed-persistance fixed-conversation
        [{:text  "q" :answer "a" :status :ok
          :input-tokens 100 :output-tokens 0 :reasoning-tokens nil :cached-tokens 0}]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/includes? out "100 in"))
          (expect (not (str/includes? out " out")))
          (expect (not (str/includes? out " think")))
          (expect (not (str/includes? out " cached"))))))

    (it "Strips the italic suffix on `**Assistant:**` when :include-meta? is false"
      (with-stubbed-persistance fixed-conversation [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation)
                    {:include-meta? false})]
          ;; Conversation-header blockquote still mentions provider /
          ;; model — that's a property of the conversation, not the
          ;; turn. The flag only suppresses the per-turn italic suffix.
          (expect (str/includes? out "**Assistant:**\n\n4"))
          (expect (not (str/includes? out "**Assistant:** *\u2014")))))))

  (describe "Optional system prompt section"
    (it "Off by default"
      (with-stubbed-persistance fixed-conversation [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (not (str/includes? out "## System prompt"))))))

    (it "Renders the prompt as a blockquote when :include-system? is true"
      (with-stubbed-persistance fixed-conversation [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation)
                    {:include-system? true})]
          (expect (str/includes? out "## System prompt"))
          (expect (str/includes? out "> You are a careful assistant.")))))

    (it "Skips the section silently when the conversation has no stored prompt"
      (with-stubbed-persistance (dissoc fixed-conversation :system-prompt)
        [(first fixed-turns)]
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation)
                    {:include-system? true})]
          (expect (not (str/includes? out "## System prompt")))))))

  (describe "Edge cases"
    (it "Returns nil when the conversation cannot be located"
      (with-redefs [persistance/db-get-conversation (fn [_ _] nil)
                    persistance/db-list-conversation-turns (fn [_ _] [])]
        (expect (nil? (md/conversation->markdown stub-db (java.util.UUID/randomUUID))))))

    (it "Returns nil when db-info is nil"
      (expect (nil? (md/conversation->markdown nil (java.util.UUID/randomUUID)))))

    (it "Returns nil when conversation-ref is nil"
      (expect (nil? (md/conversation->markdown stub-db nil))))

    (it "Header-only output (no turns) still renders the H1 + meta blockquote"
      (with-stubbed-persistance fixed-conversation []
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation))]
          (expect (str/starts-with? out "# Markdown export sanity check"))
          (expect (str/includes? out "channel `tui`"))
          (expect (not (str/includes? out "## Turn"))))))

    (it "Custom user / assistant labels propagate through every turn"
      (with-stubbed-persistance fixed-conversation fixed-turns
        (let [out (md/conversation->markdown stub-db (:id fixed-conversation)
                    {:user-label "Q" :assistant-label "A"})]
          (expect (str/includes? out "**Q:**"))
          (expect (str/includes? out "**A:**"))
          (expect (not (str/includes? out "**You:**")))
          (expect (not (str/includes? out "**Assistant:**"))))))))
