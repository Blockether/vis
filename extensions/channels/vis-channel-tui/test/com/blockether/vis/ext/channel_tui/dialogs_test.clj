(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]))

;; The dialog functions require a live TerminalScreen, so direct unit
;; testing is impractical. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(deftest smoke-test
  (testing "dialogs namespace loads and text-input-dialog! is public"
    (is (fn? (var-get #'dlg/text-input-dialog!)))))

(deftest apply-settings-option-test
  (let [apply-settings-option (var-get #'dlg/apply-settings-option)
        settings-option-label (var-get #'dlg/settings-option-label)
        settings-rows         (var-get #'dlg/settings-rows)
        palette-commands      (var-get #'dlg/palette-commands)]
    (testing "toggle rows flip booleans"
      (is (= {:show-thinking false}
            (apply-settings-option {:show-thinking true}
              {:key :show-thinking :type :toggle}))))

    (testing "choice rows cycle quick -> balanced -> deep -> quick"
      (is (= {:reasoning-level :balanced}
            (apply-settings-option {:reasoning-level :quick}
              {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
      (is (= {:reasoning-level :quick}
            (apply-settings-option {:reasoning-level :deep}
              {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
      (is (= {:openai-codex-verbosity :high}
            (apply-settings-option {:openai-codex-verbosity :medium}
              {:key :openai-codex-verbosity :type :choice :choices [:low :medium :high]}))))

    (testing "choice labels surface the live value"
      (is (= "Reasoning effort: deep"
            (settings-option-label {:key :reasoning-level
                                    :type :choice
                                    :choices [:quick :balanced :deep]
                                    :label "Reasoning effort"}
              {:reasoning-level :deep})))
      (is (= "OpenAI Codex verbosity: high"
            (settings-option-label {:key :openai-codex-verbosity
                                    :type :choice
                                    :choices [:low :medium :high]
                                    :label "OpenAI Codex verbosity"}
              {:openai-codex-verbosity :high}))))

    (testing "settings rows are grouped under Extensions / UI"
      (is (= ["Extensions" "UI"]
            (->> (settings-rows)
              (filter #(= :section (:type %)))
              (mapv :label))))
      (is (some #(= :mouse-selection-copy (:key %)) (settings-rows))))

    (testing "command palette exposes Providers outside Settings"
      (is (= ["Providers"
              "Settings"
              "Copy Messages"
              "Copy Conversation as Markdown"]
            (mapv :label palette-commands))))))
