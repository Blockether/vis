(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.table :as table]
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

(deftest modal-key-normalization-test
  (testing "modal helpers accept Lanterna Enter/Escape and raw terminal CR/LF/ESC strokes"
    (is (dlg/modal-enter-key? (KeyStroke. KeyType/Enter)))
    (is (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \newline) false false false)))
    (is (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \return) false false false)))
    (is (dlg/modal-escape-key? (KeyStroke. KeyType/Escape)))
    (is (dlg/modal-escape-key? (KeyStroke. (Character/valueOf (char 27)) false false false)))))

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

(deftest session-dialog-wheel-test
  (testing "session picker coalesces wheel floods and moves selection"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (virtual-screen)
          sessions (mapv (fn [idx]
                           {:id idx
                            :title (str "Session " idx)
                            :turn-count idx})
                     (range 20))]
      (try
        (dotimes [_ 5]
          (.addInput terminal (wheel-down)))
        (.addInput terminal (KeyStroke. KeyType/Enter))
        (is (= {:action :switch :id "1"}
              (dlg/session-picker-dialog! screen sessions nil)))
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
      (is (= "• [Book]" (resource-row-label true "Book" 80)))
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

(deftest reusable-table-test
  (testing "table rows keep fixed width and expose shared filtering"
    (let [columns [{:id :kind :label "Kind" :width 8}
                   {:id :label :label "Name" :flex 1}
                   {:id :status :label "Status" :width 8}]
          row     {:kind "session" :label "Untitled session" :status "active"}
          line    (table/row-line columns row 48 nil)]
      (is (= 48 (p/display-width line)))
      (is (str/includes? line "session"))
      (is (str/includes? (table/header-line columns 48) "Kind"))
      (is (= \┌ (first (table/boxed-border-line [8 27 8] :top))))
      (is (= \│ (first (table/boxed-row-line [8 27 8] ["Kind" "Name" "Status"] [:left :left :left]))))
      (is (table/row-matches? row "untitled"))
      (is (not (table/row-matches? row "workspace"))))))

(deftest session-dialog-table-model-test
  (let [items       (dlg/session-dialog-items
                      [{:id "old"
                        :title "Old"
                        :turn-count 1
                        :created-at #inst "2024-01-01T09:30:00.000Z"
                        :modified-at #inst "2024-01-01T10:45:00.000Z"}
                       {:id "new"
                        :title "New"
                        :turn-count 2
                        :created-at #inst "2024-01-02T08:15:00.000Z"
                        :modified-at #inst "2024-01-02T11:05:00.000Z"}]
                      "new"
                      96)
        header      (dlg/session-dialog-header 96)
        border-line (var-get #'dlg/session-table-border-line)]
    (testing "session rows sort by modified-at desc and split date/time columns"
      (is (= ["new" "old"] (mapv :id items)))
      (is (str/includes? header "Created at"))
      (is (str/includes? header "Modified at"))
      (is (str/includes? (:label (first items)) "2024-01-02"))
      (is (str/includes? (:label (first items)) "11:05")))
    (testing "session table uses boxed dialog-style borders with fixed width"
      (is (= \┌ (first (border-line 96 :top))))
      (is (= 96 (p/display-width (border-line 96 :top))))
      (is (= 96 (p/display-width (:label (first items))))))))

(deftest navigator-row-model-test
  (let [all-rows     (var-get #'dlg/navigator-all-rows)
        visible-rows (var-get #'dlg/navigator-visible-rows)
        rows         (all-rows {:active-session-id "s1"
                                :sessions [{:id "s1"
                                            :title nil
                                            :turn-count 2
                                            :created-at 0
                                            :modified-at 3600000}
                                           {:id "s2"
                                            :title "Second"
                                            :turn-count 5
                                            :created-at 0
                                            :modified-at 0}]})]
    ;; 1:1 session<->workspace: one unified row per session, NOT a
    ;; duplicated session row + workspace row with a contradictory :kind.
    (testing "one unified row per session, no :kind / :switch-workspace"
      (is (= 2 (count rows)))
      (is (every? #(not (contains? % :kind)) rows))
      (is (= [{:action :switch :id "s1"} {:action :switch :id "s2"}]
            (mapv :target rows))))
    (testing "title / session / status columns"
      (let [r1 (first rows)]
        (is (= "Untitled session" (:title r1)))
        (is (= "s1" (:session r1)))
        (is (= "focused" (:status r1)))))
    (testing "non-active session shows its turn count"
      (is (= "5 turns" (:status (second rows)))))
    (testing "compact MM-dd HH:mm timestamps (UTC)"
      (is (= "01-01 00:00" (:created (first rows))))
      (is (= "01-01 01:00" (:modified (first rows)))))
    (testing "visible-rows filters by query only"
      (is (= 1 (count (visible-rows rows "second"))))
      (is (= 2 (count (visible-rows rows "")))))))

(deftest file-picker-table-test
  (let [table-widths    (var-get #'dlg/file-picker-table-widths)
        border-line     (var-get #'dlg/file-picker-table-border-line)
        row-line        (var-get #'dlg/file-picker-table-row-line)
        cells           (var-get #'dlg/file-picker-table-cells)
        headers         (var-get #'dlg/file-picker-table-headers)
        content-lines   (var-get #'dlg/file-picker-content-lines)
        body-height     (var-get #'dlg/file-picker-table-body-height)
        scrollbar-geom  (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)
        widths          (table-widths 72)]
    (testing "file picker renders a full boxed table with headers and side borders"
      (let [top-line    (border-line widths :top)
            header-line (row-line widths headers)]
        (is (= 72 (count top-line)))
        (is (= \┌ (first top-line)))
        (is (= \┐ (last top-line)))
        (is (= \│ (first header-line)))
        (is (= \│ (last header-line)))
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
      ;; Canonical primitive: 20 items in a 10-row viewport, scroll=5
      ;; ⇒ 1-cell thumb halfway down the 10-row track. Overflow gone
      ;; when total ≤ inner (3 items in a 10-row view).
      (let [g (scrollbar-geom 20 10 5)]
        (is (= 1 (:thumb-h g)))
        (is (= 10 (:track-h g)))
        (is (= 10 (:max-scroll g)))
        (is (= 4 (:thumb-top-rel g))))
      (is (nil? (scrollbar-geom 3 10 0))))))

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
        settings-render-entries (var-get #'dlg/settings-render-entries)
        settings-rows          (var-get #'dlg/settings-rows)
        theme-picker-items     (var-get #'dlg/theme-picker-items)
        palette-commands       (var-get #'dlg/palette-commands)
        session-items      dlg/session-dialog-items]
    (testing "toggle rows flip booleans"
      (is (= {:show-thinking false}
            (apply-settings-option {:show-thinking true}
              {:key :show-thinking :type :toggle}))))

    (testing "registry-toggle rows route through the toggles registry, not the local settings map"
      ;; Use a throwaway test toggle so we don't disturb the canonical
      ;; host toggles (`:vis/show-raw-code`, ...). Settings map stays
      ;; UNTOUCHED: registry rows are side-effecting and the apply
      ;; path must return `values` unchanged so no stale local copy
      ;; bleeds into persistence.
      (let [id :dialogs-test/registry-row
            _  (vis/register-toggle! {:id id :label "Test" :default false})]
        (try
          (is (false? (vis/toggle-enabled? id)))
          (let [out (apply-settings-option {:something "else"}
                      {:type :registry-toggle :toggle-id id})]
            (is (= {:something "else"} out))
            (is (true? (vis/toggle-enabled? id))))
          (let [label-on  (settings-option-label
                            {:type :registry-toggle :toggle-id id :label "Test"}
                            {})]
            (is (str/includes? label-on "on")))
          (vis/toggle-reset-to-default! id)
          (let [label-off (settings-option-label
                            {:type :registry-toggle :toggle-id id :label "Test"}
                            {})]
            (is (str/includes? label-off "off")))
          (finally
            (vis/toggle-reset-to-default! id)))))

    (testing "registry rows normalize fallback labels instead of leaking raw ids"
      (let [id :dialogs-test/raw-label]
        (vis/register-toggle! {:id id :default false})
        (try
          (let [row (first (filter #(= id (:toggle-id %)) (settings-rows :general)))]
            (is (= "Dialogs Test Raw Label (off)" (settings-option-label row {}))))
          (finally
            (vis/toggle-reset-to-default! id)))))

    (testing "registry enum rows cycle through the toggles registry"
      (let [id :dialogs-test/registry-enum]
        (vis/register-toggle! {:id id
                               :label "Enum Test"
                               :type :enum
                               :choices [:low :medium :high]
                               :default :low})
        (try
          (is (= "Enum Test: low"
                (settings-option-label {:type :registry-toggle :toggle-id id :label "Enum Test"}
                  {})))
          (let [out (apply-settings-option {:something "else"}
                      {:type :registry-toggle :toggle-id id})]
            (is (= {:something "else"} out))
            (is (= :medium (vis/toggle-value id)))
            (is (= "Enum Test: medium"
                  (settings-option-label {:type :registry-toggle :toggle-id id :label "Enum Test"}
                    {}))))
          (finally
            (vis/toggle-reset-to-default! id)))))

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

    (testing "settings row activation notifies on-change without redrawing behind the modal"
      (let [values  (atom {:show-timestamps false})
            changed (atom nil)
            calls   (atom [])]
        (activate-settings-row! nil values {:on-change #(do (reset! changed %)
                                                          (swap! calls conj [:change %]))
                                            :redraw-ui #(swap! calls conj [:redraw @values])}
          {:key :show-timestamps :type :toggle})
        (is (= {:show-timestamps true} @values))
        (is (= {:show-timestamps true} @changed))
        (is (= [[:change {:show-timestamps true}]] @calls))))

    (testing "settings descriptions wrap into paint rows instead of truncating inline"
      (let [rows    [{:type :section :label "Terminal UI"}
                     {:key :show-thinking
                      :type :toggle
                      :label "Show model thinking"
                      :description "Stream reasoning deltas inside each iteration bubble without collapsing this text into ellipsis."}]
            entries (settings-render-entries rows 24 16)]
        (is (< 2 (count entries)))
        (is (some #(= :option-desc (:part %)) entries))
        (is (every? #(not (str/includes? (str (:text %)) "...")) entries))))

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
        (is (not-any? #(= :differentiate-turns (:key %)) (settings-rows :channels)))
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

    (testing "retired extension setting declarations are dropped, registry owns the rows"
      (with-redefs [vis/get-router (constantly nil)
                    vis/registered-extensions (fn [] [{:ext/name "voice"
                                                       :ext/settings [{:key :voice/respond?
                                                                       :type :toggle
                                                                       :label "Voice responses"}
                                                                      {:key :voice/tui-auto-read?
                                                                       :type :toggle
                                                                       :label "TUI auto-read"}]}
                                                      {:ext/name "provider-openai-codex"
                                                       :ext/providers [{:provider/id :openai-codex
                                                                        :provider/label "OpenAI Codex"}]
                                                       :ext/settings [{:key :openai-codex-verbosity
                                                                       :type :choice
                                                                       :choices [:low :medium :high]
                                                                       :label "Codex verbosity"}
                                                                      {:key :openai-codex/verbosity
                                                                       :type :choice
                                                                       :choices [:low :medium :high]
                                                                       :label "Codex verbosity"}]}])]
        (let [general-rows    (settings-rows :general)
              extension-rows  (settings-rows :extensions)
              extension-ids   (set (map :id extension-rows))
              general-toggles (set (keep :toggle-id general-rows))]
          (is (contains? general-toggles :voice/respond?))
          (is (contains? general-toggles :vis/reasoning-level))
          (is (contains? general-toggles :openai-codex/verbosity))
          (is (contains? extension-ids [:extension-setting "voice" :voice/tui-auto-read?]))
          (is (not (contains? extension-ids [:extension-setting "voice" :voice/respond?])))
          (is (not (contains? extension-ids [:extension-setting "provider-openai-codex" :openai-codex-verbosity])))
          (is (not (contains? extension-ids [:extension-setting "provider-openai-codex" :openai-codex/verbosity]))))))

    (testing "provider-declared legacy settings are ignored"
      (with-redefs [vis/get-router (constantly nil)
                    vis/registered-extensions (fn [] [{:ext/name "provider-openai-codex"
                                                       :ext/providers [{:provider/id :openai-codex
                                                                        :provider/label "OpenAI Codex (ChatGPT OAuth)"}]
                                                       :ext/settings [{:key :openai-codex-verbosity
                                                                       :type :choice
                                                                       :choices [:low :medium :high]
                                                                       :label "Codex verbosity"
                                                                       :description "Output detail."}]}])]
        (let [extension-rows (settings-rows :extensions)]
          (is (not-any? #(= [:extension-setting "provider-openai-codex" :openai-codex-verbosity]
                           (:id %))
                extension-rows)))))

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

    (testing "session picker keeps new/fork out of the table and renders justified cells"
      (let [body-w 96
            header (dlg/session-dialog-header body-w)
            rows (session-items [{:id "123e4567-e89b-12d3-a456-426614174000"
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
            fork-label (dlg/session-dialog-label
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
        (is (= [] (session-items [] nil body-w)))
        (is (= [body-w body-w body-w body-w]
              (mapv p/display-width [header active-label inactive-label fork-label])))
        (is (every? #(str/includes? % "│") [header active-label inactive-label]))
        (is (str/includes? header "ID"))
        (is (str/includes? header "Turns"))
        (is (str/includes? active-label "●"))
        (is (str/includes? active-label "│ 123e4567 │"))
        (is (str/includes? active-label "│     2 │"))
        (is (str/includes? active-label "2024-01-03"))
        (is (str/includes? active-label "04:05"))
        (is (str/includes? active-label "2024-01-01"))
        (is (str/includes? active-label "01:02"))
        (is (str/includes? active-label "Title"))
        (is (str/includes? fork-label "[forks:3]"))
        (is (str/includes? active-label "…"))
        (is (str/includes? inactive-label "│ abcdef00 │"))
        (is (str/includes? inactive-label "│     0 │"))
        (is (str/includes? inactive-label "-"))
        (is (str/includes? inactive-label "Untitled session"))))

    (testing "command palette keeps Configure Providers separate from Settings"
      (is (= ["New Session"
              "New Tab"
              "New Worktree"
              "Fork Session"
              "Switch Session"
              "Configure Providers"
              "Settings"]
            (mapv :label palette-commands))))

    (testing "command palette content fits the default dialog without an unused scrollbar"
      (let [scrollbar-geom (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)]
        ;; Canonical `geometry` arity is (total inner [track] scroll) —
        ;; for the dialog adapter shape callers used `(height total scroll)`
        ;; with track == height. Pass `(count palette-commands)` as total,
        ;; 10 as the viewport, scroll 0.
        (is (nil? (scrollbar-geom (count palette-commands) 10 0)))))))
