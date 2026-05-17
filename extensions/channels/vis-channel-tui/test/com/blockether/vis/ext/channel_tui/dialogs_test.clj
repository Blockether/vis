(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

;; Most dialog functions require a live TerminalScreen, so direct unit
;; testing is narrow. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(deftest smoke-test
  (testing "dialogs namespace loads and text-input-dialog! is public"
    (is (fn? (var-get #'dlg/text-input-dialog!)))))

(defn- virtual-screen
  []
  (let [terminal (DefaultVirtualTerminal. (TerminalSize. 80 30))
        screen   (TerminalScreen. terminal)]
    (.startScreen screen)
    {:terminal terminal :screen screen}))

(defn- wheel-down
  []
  (MouseAction. MouseActionType/SCROLL_DOWN 0 (TerminalPosition. 10 10)))

(deftest modal-wheel-input-test
  (testing "modal input coalesces wheel floods and preserves the next non-wheel key"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (virtual-screen)
          read-modal-input! (var-get #'dlg/read-modal-input!)]
      (try
        (dotimes [_ 300]
          (.addInput terminal (wheel-down)))
        (.addInput terminal (KeyStroke. KeyType/Enter))
        (is (= {:scroll-delta 300}
              (read-modal-input! screen)))
        (is (= KeyType/Enter
              (-> (read-modal-input! screen) :key .getKeyType)))
        (finally
          (.stopScreen screen))))))

(deftest select-dialog-wheel-test
  (testing "selection menu applies a wheel burst as one scroll movement"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (virtual-screen)
          items (mapv #(hash-map :label (str "Item " %) :id %) (range 20))]
      (try
        (dotimes [_ 5]
          (.addInput terminal (wheel-down)))
        (.addInput terminal (KeyStroke. KeyType/Enter))
        (is (= 1 (:id (dlg/select-dialog! screen "Items" items))))
        (finally
          (.stopScreen screen))))))

(deftest conversation-dialog-wheel-test
  (testing "conversation picker coalesces wheel floods and moves selection"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (virtual-screen)
          conversations (mapv (fn [idx]
                                {:id idx
                                 :title (str "Conversation " idx)
                                 :turn-count idx})
                          (range 20))]
      (try
        (dotimes [_ 5]
          (.addInput terminal (wheel-down)))
        (.addInput terminal (KeyStroke. KeyType/Enter))
        (is (= {:action :switch :id "1"}
              (dlg/conversation-picker-dialog! screen conversations nil)))
        (finally
          (.stopScreen screen))))))

(deftest extension-display-label-namespace-test
  (testing "namespace-derived labels titleize the meaningful tail segment, NEVER the vendor prefix"
    (let [label (var-get #'dlg/extension-display-label)]
      (is (= "Voice"
            (label {:ext/name "voice"}))
        "plain ns -> tail segment titleized; vendor prefix dropped")
      (is (= "Goal"
            (label {:ext/name "goal"}))
        "trailing 'core' segment is dropped")
      (is (= "Channel Tui"
            (label {:ext/name "channel-tui"}))
        "hyphenated segment is split + titleized like other labels")
      (is (not (str/starts-with?
                 (label {:ext/name "voice"})
                 "Com.blockether"))
        "regression: was rendered as 'Com.blockether.vis.ext.voice.core'")))

  (testing "provider / channel / alias labels still take precedence"
    (let [label (var-get #'dlg/extension-display-label)]
      (is (= "Anthropic"
            (label {:ext/providers [{:provider/label "Anthropic (API Key)"}]
                    :ext/name "provider-anthropic"})))
      (is (= "Tui"
            (label {:ext/channels [{:channel/cmd "tui"}]
                    :ext/name "channel-tui"})))
      (is (= "V"
            (label {:ext/sci {:ext.sci/alias 'v}
                    :ext/name "foundation"}))))))

(deftest resource-dialog-items-test
  (testing "resources popup rows keep click target fields and rendered labels"
    (is (= [{:text "Book"
             :url "https://example.com/book"
             :display "📚 Book -> https://example.com/book"
             :markdown "- [Book](https://example.com/book)"
             :label "📚 Book -> https://example.com/book"}]
          (dlg/resource-dialog-items
            [{:text "Book"
              :url "https://example.com/book"
              :display "📚 Book -> https://example.com/book"}]))))

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
        (is (= ["deps.edn"] @calls)))))

  (testing "file picker opener preserves active workspace root across worker thread"
    (let [seen-root (promise)
          open-picker-item! (var-get #'dlg/open-picker-item!)]
      (with-redefs [opener/open! (fn [path]
                                   (deliver seen-root workspace/*workspace-root*)
                                   {:status :ok :target path})]
        (binding [workspace/*workspace-root* "/tmp/vis-dialog-ws"]
          @(open-picker-item! {:path "deps.edn"}))
        (is (= "/tmp/vis-dialog-ws" (deref seen-root 1000 ::timeout)))))))

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

    (testing "picker body has a constant height and hides unused scrollbar geometry"
      (is (= 20 (content-lines)))
      (is (= 10 (body-height 50)))
      (is (= {:track-h 10 :thumb-h 5 :thumb-top 2}
            (scrollbar-geom 10 20 5)))
      (is (nil? (scrollbar-geom 10 3 0))))))

(deftest settings-dialog-footprint-and-indent-test
  (let [settings-content-width  (var-get #'dlg/settings-content-width)
        settings-content-height (var-get #'dlg/settings-content-height)
        settings-subsection-text (var-get #'dlg/settings-subsection-text)
        theme-picker-content-width (var-get #'dlg/theme-picker-content-width)
        theme-picker-content-height (var-get #'dlg/theme-picker-content-height)]
    (testing "shared dialogs use the same footprint as settings"
      (is (= (dlg/default-content-width 160) (settings-content-width 160)))
      (is (= (dlg/default-content-height 50) (settings-content-height 50)))
      (is (= (settings-content-width 160) (theme-picker-content-width 160)))
      (is (= (settings-content-height 50) (theme-picker-content-height 50)))
      (is (<= (+ (dlg/default-content-width 60) 4) 60))
      (is (<= (+ (dlg/default-content-height 16) 6) 16)))
    (testing "extension headings are flush; options are indented by renderer"
      (is (= "◆ Exa" (settings-subsection-text "Exa" 80))))))

(deftest apply-settings-option-test
  (let [apply-settings-option  (var-get #'dlg/apply-settings-option)
        activate-settings-row! (var-get #'dlg/activate-settings-row!)
        settings-option-label  (var-get #'dlg/settings-option-label)
        settings-rows          (var-get #'dlg/settings-rows)
        theme-picker-items     (var-get #'dlg/theme-picker-items)
        palette-commands       (var-get #'dlg/palette-commands)
        conversation-items      dlg/conversation-dialog-items]
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

    (testing "choice labels do not crash when row also carries a nil name field"
      (is (= "Reasoning effort: quick"
            (settings-option-label {:key :reasoning-level
                                    :type :choice
                                    :choices [:quick :balanced :deep]
                                    :label "Reasoning effort"
                                    :name nil}
              {}))))

    (testing "settings row activation immediately notifies on-change callbacks and requests modal-background redraw"
      (let [values  (atom {:show-timestamps false})
            changed (atom nil)
            calls   (atom [])]
        (activate-settings-row! nil values {:on-change #(do (reset! changed %)
                                                          (swap! calls conj [:change %]))
                                            :redraw-ui #(swap! calls conj [:redraw @values])}
          {:key :show-timestamps :type :toggle})
        (is (= {:show-timestamps true} @values))
        (is (= {:show-timestamps true} @changed))
        (is (= [[:change {:show-timestamps true}]
                [:redraw {:show-timestamps true}]]
              @calls))))

    (testing "theme picker rows label registered themes"
      (is (= [{:theme-id :vis-dark :label "Vis Dark"}
              {:theme-id :vis-light :label "Vis Light"}]
            (theme-picker-items [:vis-dark :vis-light]))))

    (testing "settings rows separate channel, provider, and extension-owned settings"
      (with-redefs [vis/registered-extensions (constantly [])
                    vis/get-router (constantly nil)]
        (is (= ["Terminal UI"]
              (->> (settings-rows :channels)
                (filter #(= :section (:type %)))
                (mapv :label))))
        (is (= ["Models"]
              (->> (settings-rows :providers)
                (filter #(= :section (:type %)))
                (mapv :label))))
        (is (= ["Extensions"]
              (->> (settings-rows :extensions)
                (filter #(= :section (:type %)))
                (mapv :label))))
        (is (some #(= :theme-name (:key %)) (settings-rows :channels)))
        (is (= [:vis-dark :vis-light]
              (:choices (first (filter #(= :theme-name (:key %)) (settings-rows :channels))))))
        (is (some #(= :differentiate-turns (:key %)) (settings-rows :channels)))
        (is (some #(= :mouse-selection-copy (:key %)) (settings-rows :channels)))
        (is (not-any? #(= :show-thinking (:key %)) (settings-rows :channels)))
        (is (some #(= :reasoning-level (:key %)) (settings-rows :providers)))
        (is (not-any? #(= :providers (:id %)) (settings-rows :providers)))
        (is (some #(= :info (:type %)) (settings-rows :extensions)))))

    (testing "registered extension themes appear in the channel Theme setting"
      (try
        (vis/register-themes! {"THEME_NAME" {"PADDING" "0px"}})
        (with-redefs [vis/get-router (constantly nil)]
          (let [row (first (filter #(= :theme-name (:key %)) (settings-rows :channels)))]
            (is (= [:THEME_NAME :vis-dark :vis-light] (:choices row)))
            (is (= "Theme: THEME_NAME"
                  (settings-option-label row {:theme-name :THEME_NAME})))))
        (finally
          (vis/reset-themes!))))

    (testing "extension-declared env vars render under Extensions / Exa without UNKNOWN labels"
      (with-redefs [vis/get-router (constantly nil)
                    vis/registered-extensions (fn [] [{:ext/name "test.ext"
                                                       :ext/sci {:ext.sci/alias 'exa}
                                                       :ext/env [{:name "EXA_API_KEY"
                                                                  :label "Exa API key"
                                                                  :description "Optional key."
                                                                  :secret? true}]}])
                    vis/extension-env-status (fn [name]
                                               {:name name :source :config :value "secret"})]
        (let [rows (settings-rows :extensions)
              row  (first (filter #(= [:environment "EXA_API_KEY"] (:id %)) rows))]
          (is (= ["Extensions"]
                (->> rows
                  (filter #(= :section (:type %)))
                  (mapv :label))))
          (is (= ["Exa"]
                (->> rows
                  (filter #(= :subsection (:type %)))
                  (mapv :label))))
          (is (= :env-var (:type row)))
          (is (= "Exa API key: set in Vis config"
                (settings-option-label row {})))
          (is (not (str/includes? (settings-option-label row {}) "UNKNOWN"))))))

    (testing "provider-declared settings render under Providers & Models, not Extensions"
      (with-redefs [vis/get-router (constantly nil)
                    vis/registered-extensions (fn [] [{:ext/name "provider-openai-codex"
                                                       :ext/providers [{:provider/id :openai-codex
                                                                        :provider/label "OpenAI Codex (ChatGPT OAuth)"}]
                                                       :ext/settings [{:key :openai-codex-verbosity
                                                                       :type :choice
                                                                       :choices [:low :medium :high]
                                                                       :label "Codex verbosity"
                                                                       :description "Output detail."}]}])]
        (let [provider-rows (settings-rows :providers)
              extension-rows (settings-rows :extensions)
              row  (first (filter #(= [:extension-setting "provider-openai-codex" :openai-codex-verbosity]
                                     (:id %))
                            provider-rows))]
          (is (= ["Models" "Provider Settings"]
                (->> provider-rows
                  (filter #(= :section (:type %)))
                  (mapv :label))))
          (is (= ["OpenAI Codex"]
                (->> provider-rows
                  (filter #(= :subsection (:type %)))
                  (mapv :label))))
          (is (nil? (first (filter #(= [:extension-setting "provider-openai-codex" :openai-codex-verbosity]
                                      (:id %))
                             extension-rows))))
          (is (= :choice (:type row)))
          (is (= "Codex verbosity: high"
                (settings-option-label row {:openai-codex-verbosity :high}))))))

    (testing "active Z.ai hides reasoning effort and Codex-only provider settings"
      (with-redefs [vis/get-router (constantly :router)
                    vis/resolve-effective-model (fn [_] {:provider :zai
                                                         :name "glm-4.7"
                                                         :reasoning? true
                                                         :reasoning-style :zai-thinking
                                                         :reasoning-effort? false})
                    vis/registered-extensions (fn [] [{:ext/name "provider-openai-codex"
                                                       :ext/providers [{:provider/id :openai-codex
                                                                        :provider/label "OpenAI Codex (ChatGPT OAuth)"}]
                                                       :ext/settings [{:key :openai-codex-verbosity
                                                                       :type :choice
                                                                       :choices [:low :medium :high]
                                                                       :label "Codex verbosity"
                                                                       :description "Output detail."}]}])]
        (let [provider-rows (settings-rows :providers)]
          (is (not-any? #(= :reasoning-level (:key %)) provider-rows))
          (is (some #(= "Reasoning effort unavailable" (:label %)) provider-rows))
          (is (not-any? #(= :openai-codex-verbosity (:key %)) provider-rows)))))

    (testing "channel-declared settings render under Channels, not provider or extension tabs"
      (with-redefs [vis/get-router (constantly nil)
                    vis/registered-extensions (fn [] [{:ext/name "channel-telegram"
                                                       :ext/channels [{:channel/id :telegram
                                                                       :channel/cmd "telegram"}]
                                                       :ext/settings [{:key :telegram-notify
                                                                       :type :toggle
                                                                       :label "Telegram notifications"
                                                                       :description "Send channel notifications."}]}])]
        (let [channel-rows (settings-rows :channels)
              provider-rows (settings-rows :providers)
              extension-rows (settings-rows :extensions)
              row-id [:extension-setting "channel-telegram" :telegram-notify]
              row (first (filter #(= row-id (:id %)) channel-rows))]
          (is (= ["Terminal UI" "Channel Settings"]
                (->> channel-rows
                  (filter #(= :section (:type %)))
                  (mapv :label))))
          (is (= ["Telegram"]
                (->> channel-rows
                  (filter #(= :subsection (:type %)))
                  (mapv :label))))
          (is (= :toggle (:type row)))
          (is (nil? (first (filter #(= row-id (:id %)) provider-rows))))
          (is (nil? (first (filter #(= row-id (:id %)) extension-rows)))))))

    (testing "conversation picker keeps new/fork out of the table and renders justified cells"
      (let [body-w 96
            header (dlg/conversation-dialog-header body-w)
            rows (conversation-items [{:id "123e4567-e89b-12d3-a456-426614174000"
                                       :title (str "Title " (apply str (repeat 80 "汉")))
                                       :turn-count 2
                                       :fork-count 3
                                       :modified-at #inst "2024-01-03T04:05:00.000-00:00"
                                       :created-at #inst "2024-01-01T01:02:00.000-00:00"}
                                      {:id "abcdef00-e89b-12d3-a456-426614174000"
                                       :title ""
                                       :turn-count 0
                                       :modified-at nil
                                       :created-at #inst "2024-01-02T01:02:00.000-00:00"}]
                   "123e4567-e89b-12d3-a456-426614174000"
                   body-w)
            active-label (:label (nth rows 0))
            inactive-label (:label (nth rows 1))
            fork-label (dlg/conversation-dialog-label
                         {:id "fedcba00-e89b-12d3-a456-426614174000"
                          :title "Forkable"
                          :turn-count 4
                          :fork-count 3
                          :modified-at #inst "2024-01-04T04:05:00.000-00:00"
                          :created-at #inst "2024-01-01T01:02:00.000-00:00"}
                         nil
                         body-w)]
        (is (= [:switch :switch] (mapv :action rows)))
        (is (not-any? #{:new :fork} (map :action rows)))
        (is (= [] (conversation-items [] nil body-w)))
        (is (= [body-w body-w body-w body-w]
              (mapv p/display-width [header active-label inactive-label fork-label])))
        (is (every? #(str/includes? % "│") [header active-label inactive-label]))
        (is (str/includes? header "ID"))
        (is (str/includes? header "Turns"))
        (is (str/includes? active-label "●"))
        (is (str/includes? active-label "│ 123e4567 │"))
        (is (str/includes? active-label "│     2 │"))
        (is (str/includes? active-label "2024-01-03 04:05"))
        (is (str/includes? active-label "2024-01-01 01:02"))
        (is (str/includes? active-label "Title"))
        (is (str/includes? fork-label "[forks:3]"))
        (is (str/includes? active-label "..."))
        (is (str/includes? inactive-label "│ abcdef00 │"))
        (is (str/includes? inactive-label "│     0 │"))
        (is (str/includes? inactive-label "-"))
        (is (str/includes? inactive-label "Untitled conversation"))))

    (testing "command palette keeps Configure Providers separate from Settings"
      (is (= ["New Conversation"
              "New Tab"
              "New Worktree"
              "Fork Conversation"
              "Switch Conversation"
              "Configure Providers"
              "Settings"]
            (mapv :label palette-commands))))

    (testing "command palette content fits the default dialog without an unused scrollbar"
      (let [scrollbar-geom (var-get #'dlg/scrollbar-geometry)]
        (is (nil? (scrollbar-geom 10 (count palette-commands) 0)))))))
