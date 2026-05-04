(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.input MouseActionType]))

;; The dialog functions require a live TerminalScreen, so direct unit
;; testing is impractical. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(deftest smoke-test
  (testing "dialogs namespace loads and text-input-dialog! is public"
    (is (fn? (var-get #'dlg/text-input-dialog!)))))

(deftest resource-dialog-items-test
  (testing "resources popup rows keep click target fields and rendered labels"
    (is (= [{:text "Book"
             :url "https://example.com/book"
             :display "📚 Book → https://example.com/book"
             :markdown "- [Book](https://example.com/book)"
             :label "📚 Book → https://example.com/book"}]
          (dlg/resource-dialog-items
            [{:text "Book"
              :url "https://example.com/book"
              :display "📚 Book → https://example.com/book"}]))))

  (testing "resource rows use a single selector, not selector plus bullet"
    (let [resource-row-label (var-get #'dlg/resource-row-label)]
      (is (= "▸ [Book]" (resource-row-label true "Book" 80)))
      (is (= "  [Book]" (resource-row-label false "Book" 80)))))

  (testing "resource mouse open accepts normal down and release-only terminals"
    (let [resource-open-action? (var-get #'dlg/resource-open-action?)]
      (is (resource-open-action? MouseActionType/CLICK_DOWN))
      (is (resource-open-action? MouseActionType/CLICK_RELEASE))
      (is (not (resource-open-action? MouseActionType/MOVE))))))

(deftest file-picker-opener-test
  (testing "file picker can hand the selected path to the shared external opener"
    (let [calls (atom [])
          open-picker-item! (var-get #'dlg/open-picker-item!)]
      (with-redefs [opener/open! (fn [path]
                                   (swap! calls conj path)
                                   {:status :ok :target path})]
        (is (= {:status :ok :target "deps.edn"}
              @(open-picker-item! {:path "deps.edn"})))
        (is (= ["deps.edn"] @calls))))))

(deftest file-picker-table-test
  (let [table-widths    (var-get #'dlg/file-picker-table-widths)
        border-line     (var-get #'dlg/file-picker-table-border-line)
        row-line        (var-get #'dlg/file-picker-table-row-line)
        cells           (var-get #'dlg/file-picker-table-cells)
        headers         (var-get #'dlg/file-picker-table-headers)
        content-lines   (var-get #'dlg/file-picker-content-lines)
        body-height     (var-get #'dlg/file-picker-table-body-height)
        scrollbar-geom  (var-get #'dlg/file-picker-scrollbar-geometry)
        widths          (table-widths 72)]
    (testing "file picker renders a table with headers and no outer side borders"
      (let [top-line    (border-line widths :top)
            header-line (row-line widths headers)]
        (is (= 72 (count top-line)))
        (is (not= \│ (first top-line)))
        (is (not= \│ (last top-line)))
        (is (= \space (first header-line)))
        (is (= \space (last header-line)))
        (is (re-find #"Status.*File.*Size.*Modified" header-line))))

    (testing "status is rendered as a word, not a bracket badge"
      (let [line (row-line widths
                   (cells {:status-label "modified"
                           :path "src/com/blockether/vis/core.clj"
                           :size-label "14.0K"
                           :age-label "1m"}))]
        (is (str/includes? line "modified"))
        (is (not (str/includes? line "[M]")))))

    (testing "picker body has a constant height and visible scrollbar geometry"
      (is (= 20 (content-lines)))
      (is (= 10 (body-height 50)))
      (is (= {:track-h 10 :thumb-h 5 :thumb-top 2}
            (scrollbar-geom 10 20 5)))
      (is (= {:track-h 10 :thumb-h 10 :thumb-top 0}
            (scrollbar-geom 10 3 0))))))

(deftest apply-settings-option-test
  (let [apply-settings-option (var-get #'dlg/apply-settings-option)
        settings-option-label (var-get #'dlg/settings-option-label)
        settings-rows         (var-get #'dlg/settings-rows)
        palette-commands      (var-get #'dlg/palette-commands)
        conversation-items     dlg/conversation-dialog-items]
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
      (is (some #(= :differentiate-turns (:key %)) (settings-rows)))
      (is (some #(= :mouse-selection-copy (:key %)) (settings-rows))))

    (testing "conversation picker formats command rows and column-aligned conversation table rows"
      (let [body-w 96
            rows (conversation-items [{:id "123e4567-e89b-12d3-a456-426614174000"
                                       :title (str "Title " (apply str (repeat 80 "汉")))
                                       :turn-count 2
                                       :modified-at #inst "2024-01-03T04:05:00.000-00:00"
                                       :created-at #inst "2024-01-01T01:02:00.000-00:00"}
                                      {:id "abcdef00-e89b-12d3-a456-426614174000"
                                       :title ""
                                       :turn-count 0
                                       :modified-at nil
                                       :created-at #inst "2024-01-02T01:02:00.000-00:00"}]
                   "123e4567-e89b-12d3-a456-426614174000"
                   body-w)
            action-labels (mapv :label (take 2 rows))
            active-label (:label (nth rows 2))
            inactive-label (:label (nth rows 3))]
        (is (= [:new :fork :switch :switch] (mapv :action rows)))
        (is (= [body-w body-w body-w body-w]
              (mapv (comp p/display-width :label) rows)))
        (is (every? #(= body-w (p/display-width %))
              [(dlg/conversation-dialog-header body-w)]))
        (is (str/includes? (first action-labels) "new"))
        (is (str/includes? (second action-labels) "fork"))
        (is (str/includes? active-label "● 123e4567"))
        (is (str/includes? active-label "    2"))
        (is (str/includes? active-label "2024-01-03 04:05"))
        (is (str/includes? active-label "2024-01-01 01:02"))
        (is (str/includes? active-label "Title"))
        (is (str/includes? active-label "…"))
        (is (str/includes? inactive-label "  abcdef00"))
        (is (str/includes? inactive-label "    0"))
        (is (str/includes? inactive-label "—"))
        (is (str/includes? inactive-label "Untitled conversation"))))

    (testing "command palette exposes conversation actions before Providers and Settings"
      (is (= ["New Conversation"
              "Fork Conversation"
              "Switch Conversation"
              "Configure Providers"
              "Settings"]
            (mapv :label palette-commands))))))
