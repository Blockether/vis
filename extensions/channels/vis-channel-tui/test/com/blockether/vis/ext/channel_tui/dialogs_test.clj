(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.core :as vis]
            ;; Loaded for its side effect: registers the :shell/enabled toggle
            ;; (internal foundation, at ns load), which the settings-rows test asserts.
            [com.blockether.vis.internal.foundation.shell])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

;; Most dialog functions require a live TerminalScreen, so direct unit
;; testing is narrow. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(defdescribe smoke-test
             (it "dialogs namespace loads and text-input-dialog! is public"
                 (expect (fn? (var-get #'dlg/text-input-dialog!)))))

(defdescribe
  modal-key-normalization-test
  (it "modal helpers accept Lanterna Enter/Escape and raw terminal CR/LF/ESC strokes"
      (expect (dlg/modal-enter-key? (KeyStroke. KeyType/Enter)))
      (expect (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \newline) false false false)))
      (expect (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \return) false false false)))
      (expect (dlg/modal-escape-key? (KeyStroke. KeyType/Escape)))
      (expect (dlg/modal-escape-key?
                (KeyStroke. (Character/valueOf (char 27)) false false false)))))

(defn- virtual-screen
  []
  ;; Clear any interrupt flag leaked onto this (lazytest-reused) thread by a
  ;; prior cancellation test. Lanterna's DefaultVirtualTerminal.readInput
  ;; throws "Unexpected interrupt" when Thread.interrupted() is set, which
  ;; made the wheel-coalescing reads flaky depending on test order.
  (Thread/interrupted)
  (let
    [terminal
     (DefaultVirtualTerminal. (TerminalSize. 80 30))

     screen
     (TerminalScreen. terminal)]

    (.startScreen screen)
    {:terminal terminal :screen screen}))

(defn- wheel-down [] (MouseAction. MouseActionType/SCROLL_DOWN 0 (TerminalPosition. 10 10)))

(defdescribe modal-wheel-input-test
             (it "modal input coalesces wheel floods and preserves the next non-wheel key"
                 (let
                   [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                    (virtual-screen)

                    read-modal-input!
                    (var-get #'dlg/read-modal-input!)]

                   (try (dotimes [_ 300]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= {:scroll-delta 300} (read-modal-input! screen)))
                        (expect (= KeyType/Enter
                                   (.getKeyType ^com.googlecode.lanterna.input.KeyStroke
                                                (:key (read-modal-input! screen)))))
                        (finally (.stopScreen screen))))))

(defdescribe select-dialog-wheel-test
             (it "selection menu applies a wheel burst as one scroll movement"
                 (let
                   [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                    (virtual-screen)

                    items
                    (mapv #(hash-map :label (str "Item " %) :id %) (range 20))]

                   (try (dotimes [_ 5]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= 1 (:id (dlg/select-dialog! screen "Items" items))))
                        (finally (.stopScreen screen))))))

;; ── run-modal! component spike ──────────────────────────────────────────────
;; The React-like payoff: `select-modal-component` is the pure heart of
;; `list-dialog!` — its :measure (geometry), :reconcile (scroll window) and
;; :on-key (nav / filter / select) are plain functions of immutable state, so
;; they test with NO live terminal at all (contrast every other dialog test
;; above, which must spin up a DefaultVirtualTerminal).
(defn- ks [t] (KeyStroke. ^KeyType t))

(defn- char-key [c] (KeyStroke. (Character/valueOf c) false false false))

(def ^:private done-key :com.blockether.vis.ext.channel-tui.dialogs/done)

(defdescribe
  select-modal-component-pure-test
  (it "arrow nav + Enter selects the right item with no screen"
      (let
        [items
         (mapv #(hash-map :label (str "Item " %) :id %) (range 20))

         {:keys [init measure reconcile on-key]}
         (dlg/select-modal-component "Items" items {:height :content})

         geom
         (measure init 80 30)

         s0
         (reconcile init geom)

         down
         (ks KeyType/ArrowDown)

         s3
         (nth (iterate #(on-key % down geom) s0) 3)

         done
         (on-key s3 (ks KeyType/Enter) geom)]

        (expect (= 20 (:total geom)))
        (expect (= 0 (:selected s0)))
        (expect (= 3 (:selected s3)))
        (expect (= {:label "Item 3" :id 3} (done-key done)))))
  (it "Escape closes with nil"
      (let
        [{:keys [init measure on-key]}
         (dlg/select-modal-component "X" [{:label "a"}] {})

         geom
         (measure init 80 30)]

        (expect (contains? (on-key init (ks KeyType/Escape) geom) done-key))
        (expect (nil? (done-key (on-key init (ks KeyType/Escape) geom))))))
  (it "type-to-filter narrows :filtered and backspace widens it again"
      (let
        [items
         (mapv #(hash-map :label %) ["apple" "apricot" "banana" "cherry"])

         {:keys [init measure reconcile on-key]}
         (dlg/select-modal-component "F" items {:filter? true :height :content})

         step
         (fn [s k]
           (on-key s k (measure s 80 30)))

         s1
         (-> (reconcile init (measure init 80 30))
             (step (char-key \a))
             (step (char-key \p)))

         s2
         (step s1 (ks KeyType/Backspace))]

        (expect (= "ap" (:query s1)))
        (expect (= ["apple" "apricot"] (mapv :label (:filtered (measure s1 80 30)))))
        (expect (= "a" (:query s2)))
        (expect (= ["apple" "apricot" "banana"] (mapv :label (:filtered (measure s2 80 30)))))))
  (it "a wheel burst moves selection by the coalesced step"
      (let
        [items
         (mapv #(hash-map :label (str %) :id %) (range 20))

         {:keys [init measure reconcile on-key]}
         (dlg/select-modal-component "W" items {:height :content})

         geom
         (measure init 80 30)

         s0
         (reconcile init geom)

         burst
         (MouseAction. MouseActionType/SCROLL_DOWN 5 (TerminalPosition. 10 10))]

        (expect (= 5 (:selected (on-key s0 burst geom)))))))

(defdescribe
  magit-transient-toggle-test
  (let
    [spec
     {:groups [{:title "Arguments"
                :items [{:key "f" :type :switch :id :force :label "Force"}
                        {:key "u" :type :switch :id :set-upstream :label "Upstream"}
                        {:key "t" :type :option :id :topic :label "Topic"}]}
               {:title "Push" :items [{:key "p" :type :action :id :push :label "Push"}]}]}

     init
     {:switches #{} :options {}}]

    (it "binds a key to its item across every group"
        (expect (= :force (:id (dlg/transient-item-by-key spec \f))))
        (expect (= :push (:id (dlg/transient-item-by-key spec \p))))
        (expect (nil? (dlg/transient-item-by-key spec \z))))
    (it "a switch flips on then off; an unbound key is a no-op"
        (let
          [on
           (dlg/transient-toggle spec init \f)

           off
           (dlg/transient-toggle spec (:state on) \f)]

          (expect (= :continue (:kind on)))
          (expect (= #{:force} (:switches (:state on))))
          (expect (= #{} (:switches (:state off))))
          (expect (= init (:state (dlg/transient-toggle spec init \z))))))
    (it "two switches accumulate independently"
        (let
          [s (-> (dlg/transient-toggle spec init \f)
                 :state
                 (->> (#(dlg/transient-toggle spec % \u)))
                 :state)]
          (expect (= #{:force :set-upstream} (:switches s)))))
    (it "an option key asks the caller to read a value"
        (let [r (dlg/transient-toggle spec init \t)]
          (expect (= :option (:kind r)))
          (expect (= :topic (:id (:item r))))))
    (it "an action key fires with the item, leaving state untouched"
        (let [r (dlg/transient-toggle spec {:switches #{:force} :options {}} \p)]
          (expect (= :action (:kind r)))
          (expect (= :push (:id (:item r))))))))


(defdescribe session-dialog-wheel-test
             (it "session picker coalesces wheel floods and moves selection"
                 (let
                   [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                    (virtual-screen)

                    sessions
                    (mapv (fn [idx]
                            {"id" idx "title" (str "Session " idx) "turn_count" idx})
                          (range 20))]

                   (try (dotimes [_ 5]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= {:action :switch :id "1"}
                                   (dlg/session-picker-dialog! screen sessions nil)))
                        (finally (.stopScreen screen))))))

(defdescribe startable-fields-form-test
             (it
               "the inline multi-field form collects every field at once (type, Tab, Enter)"
               (let
                 [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                  (virtual-screen)

                  startable-fields-form!
                  (var-get #'dlg/startable-fields-form!)

                  sr
                  {:label "remote HTTP MCP server"
                   :fields [{:name :name :label "Name" :required true :placeholder "remote"}
                            {:name :url :label "URL" :required true :placeholder "https://…"}]}

                  type!
                  (fn [^String s]
                    (doseq [c s]
                      (.addInput terminal (KeyStroke. (Character/valueOf c) false false false))))]

                 (try (type! "fs") ;; into field 0 (focus starts at 0)
                      (.addInput terminal (KeyStroke. KeyType/Tab)) ;; → field 1
                      (type! "http://x") ;; into field 1
                      (.addInput terminal (KeyStroke. KeyType/Enter)) ;; submit (both required filled)
                      (expect (= {:name "fs" :url "http://x"} (startable-fields-form! screen sr)))
                      (finally (.stopScreen screen)))))
             (it "Esc cancels the whole form (no per-field modals)"
                 (let
                   [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                    (virtual-screen)

                    startable-fields-form!
                    (var-get #'dlg/startable-fields-form!)

                    sr
                    {:label "x" :fields [{:name :name :label "Name" :required true}]}]

                   (try (.addInput terminal (KeyStroke. KeyType/Escape))
                        ;; ::dlg/cancel resolves to the same namespaced keyword the form returns.
                        (expect (= ::dlg/cancel (startable-fields-form! screen sr)))
                        (finally (.stopScreen screen))))))

(defdescribe
  extension-display-label-namespace-test
  (it "namespace-derived labels titleize the meaningful tail segment, NEVER the vendor prefix"
      (let [label (var-get #'dlg/extension-display-label)]
        ;; plain ns -> tail segment titleized; vendor prefix dropped
        (expect (= "Voice" (label {:ext/name "voice"})))
        ;; trailing 'core' segment is dropped
        (expect (= "Goal" (label {:ext/name "goal"})))
        ;; hyphenated segment is split + titleized like other labels
        (expect (= "Channel Tui" (label {:ext/name "channel-tui"})))
        ;; regression: was rendered as 'Com.blockether.vis.ext.voice.core'
        (expect (not (str/starts-with? (label {:ext/name "voice"}) "Com.blockether")))))
  (it "provider / channel / alias labels still take precedence"
      (let [label (var-get #'dlg/extension-display-label)]
        (expect (= "Anthropic"
                   (label {:ext/providers [{:provider/label "Anthropic (API Key)"}]
                           :ext/name "provider-anthropic"})))
        (expect (= "Tui" (label {:ext/channels [{:channel/cmd "tui"}] :ext/name "channel-tui"})))
        (expect (= "V" (label {:ext/engine {:ext.engine/alias 'v} :ext/name "foundation"}))))))

(defdescribe reusable-table-test
             (it "table rows keep fixed width and expose shared filtering"
                 (let
                   [columns
                    [{:id :kind :label "Kind" :width 8} {:id :label :label "Name" :flex 1}
                     {:id :status :label "Status" :width 8}]

                    row
                    {:kind "session" :label "Untitled session" :status "active"}

                    line
                    (table/row-line columns row 48 nil)]

                   (expect (= 48 (p/display-width line)))
                   (expect (str/includes? line "session"))
                   (expect (str/includes? (table/header-line columns 48) "Kind"))
                   (expect (= \┌ (first (table/boxed-border-line [8 27 8] :top))))
                   (expect (= \│
                              (first (table/boxed-row-line [8 27 8]
                                                           ["Kind" "Name" "Status"]
                                                           [:left :left :left]))))
                   (expect (table/row-matches? row "untitled"))
                   (expect (not (table/row-matches? row "workspace")))
                   ;; Boolean row flags (e.g. :focused?) are never search text.
                   (expect (not (table/row-matches? (assoc row :focused? true) "true"))))))

(defdescribe session-dialog-table-model-test
             (it "session rows sort by modified-at desc and split date/time columns"
                 (let
                   [items
                    (dlg/session-dialog-items [{"id" "old"
                                                "title" "Old"
                                                "turn_count" 1
                                                "created_at" #inst "2024-01-01T09:30:00.000Z"
                                                "modified_at" #inst "2024-01-01T10:45:00.000Z"}
                                               {"id" "new"
                                                "title" "New"
                                                "turn_count" 2
                                                "created_at" #inst "2024-01-02T08:15:00.000Z"
                                                "modified_at" #inst "2024-01-02T11:05:00.000Z"}]
                                              "new"
                                              96)

                    header
                    (dlg/session-dialog-header 96)]

                   (expect (= ["new" "old"] (mapv :id items)))
                   (expect (str/includes? header "Created at"))
                   (expect (str/includes? header "Modified at"))
                   (expect (str/includes? (:label (first items)) "2024-01-02"))
                   (expect (str/includes? (:label (first items)) "11:05"))))
             (it "session table uses boxed dialog-style borders with fixed width"
                 (let
                   [items
                    (dlg/session-dialog-items [{:id "new"
                                                :title "New"
                                                :turn-count 2
                                                :created-at #inst "2024-01-02T08:15:00.000Z"
                                                :modified-at #inst "2024-01-02T11:05:00.000Z"}]
                                              "new"
                                              96)

                    border-line
                    (var-get #'dlg/session-table-border-line)]

                   (expect (= \┌ (first (border-line 96 :top))))
                   (expect (= 96 (p/display-width (border-line 96 :top))))
                   (expect (= 96 (p/display-width (:label (first items))))))))

;; 1:1 session<->workspace: one unified row per session, NOT a
;; duplicated session row + workspace row with a contradictory :kind.
;; The session you are currently in is the FOCUSED row: flagged, pinned
;; to the top, marked "● focused".
(defdescribe
  navigator-row-model-test
  (let
    [sessions [{"id" "empty" "title" nil "turn_count" 0 "created_at" 0 "modified_at" 7200000}
               {"id" "s1" "title" nil "turn_count" 2 "created_at" 0 "modified_at" 3600000}
               {"id" "s2" "title" "Second" "turn_count" 5 "created_at" 0 "modified_at" 0}]]
    (it "one unified row per session, no :kind / :switch-workspace"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})]

          (expect (= 2 (count rows)))
          (expect (every? #(not (contains? % :kind)) rows))
          (expect (= [{:action :switch :id "s1"} {:action :switch :id "s2"}] (mapv :target rows)))))
    (it "empty untitled shells hidden by default; focused session pinned to top"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})
           all-visible (all-rows
                         {:active-session-id "s1" :sessions sessions :show-empty-untitled? true})]

          (expect (= ["s1" "s2"] (mapv (comp str :id :target) rows)))
          ;; Focused (s1) pinned first; the rest keep recency order below,
          ;; so all-visible is [s1 empty s2], not [empty s1 s2].
          (expect (= ["s1" "empty" "s2"] (mapv (comp str :id :target) all-visible)))))
    (it "focused session is flagged + pinned, marked '● focused'"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})
           r1 (first rows)]

          (expect (= "Untitled session" (:title r1)))
          (expect (= "s1" (:session r1)))
          (expect (:focused? r1))
          (expect (= "● focused" (:status r1)))))
    (it "non-active session is not focused and shows its turn count"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})]

          (expect (not (:focused? (second rows))))
          (expect (= "5 turns" (:status (second rows))))))
    (it "compact MM-dd HH:mm timestamps (UTC)"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})
           r1 (first rows)]

          (expect (= "01-01 00:00" (:created r1)))
          (expect (= "01-01 01:00" (:modified r1)))))
    (it "transcript-only matches are tagged and show WHERE they hit"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           visible-rows (var-get #'dlg/navigator-visible-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})
           id2 (str (:id (:target (second rows))))
           ;; Query matches no title/project cell; the id arrives ONLY from the
           ;; transcript (body) search as a MAP {id kind}, so the row is kept AND
           ;; its Status cell shows WHERE the hit is.
           both (visible-rows rows "zzz-no-title-match" {id2 :both})
           req (visible-rows rows "zzz-no-title-match" {id2 :request})
           rep (visible-rows rows "zzz-no-title-match" {id2 :reply})]

          (expect (= 1 (count both)))
          (expect (:transcript-match? (first both)))
          (expect (= "in chat" (:status (first both))))
          (expect (= "in request" (:status (first req))))
          (expect (= "in reply" (:status (first rep))))))
    (it "visible-rows filters by query only"
        (let
          [all-rows (var-get #'dlg/navigator-all-rows)
           visible-rows (var-get #'dlg/navigator-visible-rows)
           rows (all-rows {:active-session-id "s1" :sessions sessions})]

          (expect (= 1 (count (visible-rows rows "second" {}))))
          (expect (= 2 (count (visible-rows rows "" {}))))))))

(defdescribe scrollbar-geometry-test
             (it "scrollbar geometry sanity (canonical primitive)"
                 ;; Canonical primitive: 20 items in a 10-row viewport, scroll=5
                 ;; ⇒ 1-cell thumb halfway down the 10-row track. Overflow gone
                 ;; when total ≤ inner (3 items in a 10-row view).
                 (let
                   [scrollbar-geom
                    (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)

                    g
                    (scrollbar-geom 20 10 5)]

                   (expect (= 1 (:thumb-h g)))
                   (expect (= 10 (:track-h g)))
                   (expect (= 10 (:max-scroll g)))
                   (expect (= 4 (:thumb-top-rel g)))
                   (expect (nil? (scrollbar-geom 3 10 0))))))

(defdescribe settings-dialog-footprint-and-indent-test
             (it "shared dialogs use the same footprint as settings"
                 (let
                   [settings-content-width
                    (var-get #'dlg/settings-content-width)

                    settings-content-height
                    (var-get #'dlg/settings-content-height)

                    theme-picker-content-width
                    (var-get #'dlg/theme-picker-content-width)]

                   (expect (= (dlg/default-content-width 160) (settings-content-width 160)))
                   (expect (= (dlg/default-content-height 50) (settings-content-height 50)))
                   (expect (= (settings-content-width 160) (theme-picker-content-width 160)))
                   (expect (<= (+ (dlg/default-content-width 60) 4) 60))
                   (expect (<= (+ (dlg/default-content-height 16) 6) 16))))
             (it "extension headings are flush; options are indented by renderer"
                 (let [settings-subsection-text (var-get #'dlg/settings-subsection-text)]
                   (expect (= "◆ Exa" (settings-subsection-text "Exa" 80))))))

(defdescribe
  apply-settings-option-test
  (it "toggle rows flip booleans"
      (let [apply-settings-option (var-get #'dlg/apply-settings-option)]
        (expect (= {:show-thinking false}
                   (apply-settings-option {:show-thinking true}
                                          {:key :show-thinking :type :toggle})))))
  (it "registry-toggle rows route through the toggles registry, not the local settings map"
      ;; Use a throwaway test toggle so we don't disturb the canonical
      ;; host toggles. Settings map stays UNTOUCHED: registry rows are
      ;; side-effecting and the apply path returns `values` unchanged.
      (let
        [apply-settings-option
         (var-get #'dlg/apply-settings-option)

         settings-row-mark
         (var-get #'dlg/settings-row-mark)

         id
         "dialogs_test_registry_row"

         _
         (vis/register-toggle! {:id id :label "Test" :default false})]

        (try (expect (false? (vis/toggle-enabled? id)))
             (let
               [out (apply-settings-option {:something "else"}
                                           {:type :registry-toggle :toggle-id id})]
               (expect (= {:something "else"} out))
               (expect (true? (vis/toggle-enabled? id))))
             ;; Boolean state is now carried by the leading status glyph (●/○), not
             ;; "(on)/(off)" text in the label.
             (let [[on-glyph] (settings-row-mark {:type :registry-toggle :toggle-id id} {})]
               (expect (= "●" on-glyph)))
             (vis/toggle-reset-to-default! id)
             (let [[off-glyph] (settings-row-mark {:type :registry-toggle :toggle-id id} {})]
               (expect (= "○" off-glyph)))
             (finally (vis/toggle-reset-to-default! id)))))
  ;; NOTE: the old "registry rows normalize fallback labels instead of
  ;; leaking raw ids" case was retired — the toggles registry now
  ;; REQUIRES a :label (register-toggle! rejects label-less specs), so the
  ;; id-derived fallback-label path no longer exists.
  (it "registry enum rows cycle through the toggles registry"
      (let
        [apply-settings-option
         (var-get #'dlg/apply-settings-option)

         settings-option-label
         (var-get #'dlg/settings-option-label)

         id
         "dialogs_test_registry_enum"]

        (vis/register-toggle!
          {:id id :label "Enum Test" :type :enum :choices [:low :medium :high] :default :low})
        (try (expect (= "Enum Test: low"
                        (settings-option-label
                          {:type :registry-toggle :toggle-id id :label "Enum Test"}
                          {})))
             (let
               [out (apply-settings-option {:something "else"}
                                           {:type :registry-toggle :toggle-id id})]
               (expect (= {:something "else"} out))
               (expect (= "medium" (vis/toggle-value id)))
               (expect (= "Enum Test: medium"
                          (settings-option-label
                            {:type :registry-toggle :toggle-id id :label "Enum Test"}
                            {}))))
             (finally (vis/toggle-reset-to-default! id)))))
  (it "choice rows cycle quick -> balanced -> deep -> quick"
      (let [apply-settings-option (var-get #'dlg/apply-settings-option)]
        (expect (= {:reasoning-level :balanced}
                   (apply-settings-option
                     {:reasoning-level :quick}
                     {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
        (expect (= {:reasoning-level :quick}
                   (apply-settings-option
                     {:reasoning-level :deep}
                     {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
        (expect (= {:openai-codex-verbosity :high}
                   (apply-settings-option
                     {:openai-codex-verbosity :medium}
                     {:key :openai-codex-verbosity :type :choice :choices [:low :medium :high]})))))
  (it "choice labels surface the live value"
      (let [settings-option-label (var-get #'dlg/settings-option-label)]
        (expect (= "Reasoning effort: deep"
                   (settings-option-label {:key :reasoning-level
                                           :type :choice
                                           :choices [:quick :balanced :deep]
                                           :label "Reasoning effort"}
                                          {:reasoning-level :deep})))
        (expect (= "Verbosity: high"
                   (settings-option-label {:key :openai-codex-verbosity
                                           :type :choice
                                           :choices [:low :medium :high]
                                           :label "Verbosity"}
                                          {:openai-codex-verbosity :high})))))
  (it "choice labels do not crash when row also carries a nil name field"
      (let [settings-option-label (var-get #'dlg/settings-option-label)]
        (expect (= "Reasoning effort: quick"
                   (settings-option-label {:key :reasoning-level
                                           :type :choice
                                           :choices [:quick :balanced :deep]
                                           :label "Reasoning effort"
                                           :name nil}
                                          {})))))
  (it "settings row activation notifies on-change without redrawing behind the modal"
      (let
        [activate-settings-row!
         (var-get #'dlg/activate-settings-row!)

         values
         (atom {:show-timestamps false})

         changed
         (atom nil)

         calls
         (atom [])]

        (activate-settings-row! nil
                                values
                                {:on-change #(do (reset! changed %) (swap! calls conj [:change %]))
                                 :redraw-ui #(swap! calls conj [:redraw @values])}
                                {:key :show-timestamps :type :toggle})
        (expect (= {:show-timestamps true} @values))
        (expect (= {:show-timestamps true} @changed))
        (expect (= [[:change {:show-timestamps true}]] @calls))))
  (it
    "settings descriptions wrap into paint rows instead of truncating inline"
    (let
      [settings-render-entries
       (var-get #'dlg/settings-render-entries)

       rows
       [{:type :section :label "Terminal UI"}
        {:key :show-thinking
         :type :toggle
         :label "Show model thinking"
         :description
         "Stream reasoning deltas inside each iteration bubble without collapsing this text into ellipsis."}]

       entries
       (settings-render-entries rows 24 16)]

      (expect (< 2 (count entries)))
      (expect (some #(= :option-desc (:part %)) entries))
      (expect (every? #(not (str/includes? (str (:text %)) "...")) entries))))
  (it "theme picker rows label registered themes"
      (let [theme-picker-items (var-get #'dlg/theme-picker-items)]
        (expect (= [{:theme-id :vis-dark :label "Vis Dark"}
                    {:theme-id :vis-light :label "Vis Light"}]
                   (theme-picker-items [:vis-dark :vis-light])))))
  (it
    "Settings is ONE flat list (no tabs): Terminal UI + grouped toggles + Models"
    (let [settings-rows (var-get #'dlg/settings-rows)]
      (with-redefs
        [vis/registered-extensions (constantly [])
         vis/get-router (constantly nil)]

        (let
          [rows (settings-rows)
           sections (->> rows
                         (filter #(= :section (:type %)))
                         (mapv :label))]

          ;; flat list, web-shaped: Terminal UI chrome always present. The
          ;; Models section was retired (it only carried reasoning-effort,
          ;; which moved to Ctrl+R).
          (expect (some #{"Terminal UI"} sections))
          (expect (not-any? #{"Models"} sections))
          (expect (some #(= :theme-name (:key %)) rows))
          ;; vis-dark/light are pinned to the TOP; blockether + solarized
          ;; themes follow, sorted by id.
          (expect (= [:vis-light :vis-dark :blockether-dark :blockether-light :solarized-dark
                      :solarized-light]
                     (:choices (first (filter #(= :theme-name (:key %)) rows)))))
          ;; Mouse auto-copy is now ALWAYS ON (`:settings? false`) — out of Settings.
          (expect (not-any? #(= :mouse-selection-copy (:key %)) rows))
          (expect (not-any? #(= "mouse_selection_copy" (:toggle-id %)) rows))
          ;; Network access is ALWAYS ON too — also out of Settings.
          (expect (not-any? #(= "network_enabled" (:toggle-id %)) rows))
          ;; Shell + harness verbs are always on now (no user toggle); the retired
          ;; display gates (show-thinking/iterations/silent/timestamps) and the
          ;; own-control knobs (reasoning-effort :settings? false) stay out.
          (expect (not-any? #(= "shell_enabled" (:toggle-id %)) rows))
          (expect (not-any? #(= "show_thinking" (:toggle-id %)) rows))
          (expect (not-any? #(= "reasoning_level" (:toggle-id %)) rows))
          ;; toggles group by :group now — no single "Feature Toggles" bucket;
          ;; with no declared extensions there is no "Extension Settings" section
          (expect (not-any? #{"Feature Toggles"} sections))
          (expect (not-any? #{"Extension Settings"} sections))))))
  (it "registered extension themes appear in the channel Theme setting"
      (let
        [settings-rows
         (var-get #'dlg/settings-rows)

         settings-option-label
         (var-get #'dlg/settings-option-label)]

        (try (vis/register-themes! {"THEME_NAME" {"PADDING" "0px"}})
             (with-redefs [vis/get-router (constantly nil)]
               (let [row (first (filter #(= :theme-name (:key %)) (settings-rows)))]
                 (expect (= [:vis-light :vis-dark :THEME_NAME :blockether-dark :blockether-light
                             :solarized-dark :solarized-light]
                            (:choices row)))
                 (expect (= "Theme: THEME_NAME"
                            (settings-option-label row {:theme-name :THEME_NAME})))))
             (finally (vis/reset-themes!)))))
  (it
    "extension-declared env vars render under Extensions / Exa without UNKNOWN labels"
    (let
      [settings-rows
       (var-get #'dlg/settings-rows)

       settings-option-label
       (var-get #'dlg/settings-option-label)]

      (with-redefs
        [vis/get-router
         (constantly nil)

         vis/registered-extensions
         (fn []
           [{:ext/name "test.ext"
             :ext/engine {:ext.engine/alias 'exa}
             :ext/env [{:name "EXA_API_KEY"
                        :label "Exa API key"
                        :description "Optional key."
                        :secret? true}]}])

         vis/extension-env-status
         (fn [name]
           {:name name :source :config :value "secret"})]

        (let
          [rows
           (settings-rows)

           row
           (first (filter #(= [:environment "EXA_API_KEY"] (:id %)) rows))]

          ;; "Extension Settings" is the LAST section in the flat list; the
          ;; env-var rows live under it.
          (expect (= "Extension Settings"
                     (->> rows
                          (filter #(= :section (:type %)))
                          (mapv :label)
                          last)))
          (expect (= ["Exa"]
                     (->> rows
                          (filter #(= :subsection (:type %)))
                          (mapv :label))))
          (expect (= :env-var (:type row)))
          (expect (= "Exa API key: set in Vis config" (settings-option-label row {})))
          (expect (not (str/includes? (settings-option-label row {}) "UNKNOWN")))))))
  (it
    "retired extension setting declarations are dropped, registry owns the rows"
    (let [settings-rows (var-get #'dlg/settings-rows)]
      (with-redefs
        [vis/get-router (constantly nil)
         ;; hermetic: the Codex knob's :visible-fn consults the
         ;; CONFIGURED providers — pin "none" so the assertion
         ;; below can't flip on a dev machine that has Codex.
         vis/has-provider? (constantly false)
         vis/registered-extensions
         (fn []
           [{:ext/name "voice"
             :ext/settings [{:key :voice/tui-auto-read? :type :toggle :label "TUI auto-read"}]}
            {:ext/name "provider-openai-codex"
             :ext/providers [{:provider/id :openai-codex :provider/label "OpenAI Codex"}]
             :ext/settings [{:key :openai-codex-verbosity
                             :type :choice
                             :choices [:low :medium :high]
                             :label "Codex verbosity"}]}])]

        (let
          [rows (settings-rows)
           ids (set (map :id rows))
           toggles (set (keep :toggle-id rows))]

          ;; Reasoning-effort has its OWN control (Ctrl+R) — `:settings? false`
          ;; keeps it registered but out of the Settings dialog.
          (expect (not (contains? toggles "reasoning_level")))
          ;; Provider-specific knob: its `:visible-fn` hides it from
          ;; Settings unless a Codex provider is CONFIGURED — this test
          ;; env has none, so it must NOT appear in the rows even
          ;; though it stays registered.
          (expect (not (contains? toggles "openai_codex_verbosity")))
          (expect (contains? ids [:extension-setting "voice" :voice/tui-auto-read?]))
          (expect (not (contains? ids
                                  [:extension-setting "provider-openai-codex"
                                   :openai-codex-verbosity])))))))
  (it "provider-declared legacy settings are ignored"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs
          [vis/get-router (constantly nil)
           vis/registered-extensions (fn []
                                       [{:ext/name "provider-openai-codex"
                                         :ext/providers [{:provider/id :openai-codex
                                                          :provider/label
                                                          "OpenAI Codex (ChatGPT OAuth)"}]
                                         :ext/settings [{:key :openai-codex-verbosity
                                                         :type :choice
                                                         :choices [:low :medium :high]
                                                         :label "Codex verbosity"
                                                         :description "Output detail."}]}])]

          (let [rows (settings-rows)]
            (expect (not-any? #(= [:extension-setting "provider-openai-codex"
                                   :openai-codex-verbosity]
                                  (:id %))
                              rows))))))
  (it "active Z.ai hides reasoning effort and Codex-only provider settings"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs
          [vis/get-router (constantly :router)
           vis/resolve-effective-model (fn [_]
                                         {:provider :zai
                                          :name "glm-4.7"
                                          :reasoning? true
                                          :reasoning-style :zai-thinking
                                          :reasoning-effort? false})
           vis/registered-extensions (fn []
                                       [{:ext/name "provider-openai-codex"
                                         :ext/providers [{:provider/id :openai-codex
                                                          :provider/label
                                                          "OpenAI Codex (ChatGPT OAuth)"}]
                                         :ext/settings [{:key :openai-codex-verbosity
                                                         :type :choice
                                                         :choices [:low :medium :high]
                                                         :label "Codex verbosity"
                                                         :description "Output detail."}]}])]

          (let [rows (settings-rows)]
            ;; Reasoning-effort + verbosity are OUT of Settings entirely now
            ;; (own controls); no "unavailable" placeholder either.
            (expect (not-any? #(= :reasoning-level (:key %)) rows))
            (expect (not-any? #(= "Reasoning effort unavailable" (:label %)) rows))
            (expect (not-any? #(= :openai-codex-verbosity (:key %)) rows))))))
  (it "channel-declared settings render under Channel Settings, once, in the flat list"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs
          [vis/get-router (constantly nil)
           vis/registered-extensions
           (fn []
             [{:ext/name "channel-example"
               :ext/channels [{:channel/id :example :channel/cmd "example"}]
               :ext/settings [{:key :example-notify
                               :type :toggle
                               :label "Example notifications"
                               :description "Send channel notifications."}]}])]

          (let
            [rows (settings-rows)
             row-id [:extension-setting "channel-example" :example-notify]
             row (first (filter #(= row-id (:id %)) rows))]

            (expect (contains? (set (->> rows
                                         (filter #(= :section (:type %)))
                                         (mapv :label)))
                               "Channel Settings"))
            (expect (= ["Example"]
                       (->> rows
                            (filter #(= :subsection (:type %)))
                            (mapv :label))))
            (expect (= :toggle (:type row)))
            ;; appears exactly once — no tab duplicated it
            (expect (= 1 (count (filter #(= row-id (:id %)) rows))))))))
  (it
    "session picker keeps new/fork out of the table and renders justified cells"
    (let
      [session-items
       dlg/session-dialog-items

       body-w
       96

       header
       (dlg/session-dialog-header body-w)

       rows
       (session-items [{"id" "123e4567-e89b-12d3-a456-426614174000"
                        "title" (str "Title " (apply str (repeat 80 "汉")))
                        "turn_count" 2
                        "fork_count" 3
                        "modified_at" #inst "2024-01-03T04:05:00.000-00:00"
                        "created_at" #inst "2024-01-01T01:02:00.000-00:00"}
                       {"id" "abcdef00-e89b-12d3-a456-426614174000"
                        "title" ""
                        "turn_count" 0
                        "modified_at" nil
                        "created_at" #inst "2024-01-02T01:02:00.000-00:00"}]
                      "123e4567-e89b-12d3-a456-426614174000"
                      body-w)

       active-label
       (:label (nth rows 0))

       inactive-label
       (:label (nth rows 1))

       fork-label
       (dlg/session-dialog-label {"id" "fedcba00-e89b-12d3-a456-426614174000"
                                  "title" "Forkable"
                                  "turn_count" 4
                                  "fork_count" 3
                                  "modified_at" #inst "2024-01-04T04:05:00.000-00:00"
                                  "created_at" #inst "2024-01-01T01:02:00.000-00:00"}
                                 nil
                                 body-w)]

      (expect (= [:switch :switch] (mapv :action rows)))
      (expect (not-any? #{:new :fork} (map :action rows)))
      (expect (= [] (session-items [] nil body-w)))
      (expect (= [body-w body-w body-w body-w]
                 (mapv p/display-width [header active-label inactive-label fork-label])))
      (expect (every? #(str/includes? % "│") [header active-label inactive-label]))
      (expect (str/includes? header "ID"))
      (expect (str/includes? header "Turns"))
      ;; The active marker sits in the gutter column. `●` (U+25CF) renders in
      ;; its own cell (` ● `); inactive rows leave that column blank. Assert
      ;; the marker *cell* (between the first two │) rather than the raw glyph.
      (expect (= " ● " (second (str/split active-label #"│"))))
      (expect (= "   " (second (str/split inactive-label #"│"))))
      (expect (str/includes? active-label "│ 123e4567 │"))
      (expect (str/includes? active-label "│     2 │"))
      (expect (str/includes? active-label "2024-01-03"))
      (expect (str/includes? active-label "04:05"))
      (expect (str/includes? active-label "2024-01-01"))
      (expect (str/includes? active-label "01:02"))
      (expect (str/includes? active-label "Title"))
      (expect (str/includes? fork-label "[forks:3]"))
      (expect (str/includes? active-label "…"))
      (expect (str/includes? inactive-label "│ abcdef00 │"))
      (expect (str/includes? inactive-label "│     0 │"))
      (expect (str/includes? inactive-label "-"))
      (expect (str/includes? inactive-label "Untitled session"))))
  (it "draft manager separates trunk from current and parked drafts"
      (let
        [parked
         {"workspace_id" "ws-parked" "label" "feature-b" "root" "/tmp/b" "is_current" false}

         current
         {"workspace_id" "ws-current" "label" "feature-a" "root" "/tmp/a" "is_current" true}

         in-draft
         (dlg/draft-picker-items [parked current])

         on-trunk
         (dlg/draft-picker-items [parked])]

        (expect (= [:trunk :draft :draft] (mapv :action in-draft)))
        (expect (= ["Trunk" "feature-a" "feature-b"] (mapv :label in-draft)))
        (expect (:current? (second in-draft)))
        (expect (= "stash + switch" (:hint (first in-draft))))
        (expect (str/includes? (:description (first in-draft)) "real repository"))
        (expect (= :trunk (:action (first on-trunk))))
        (expect (:current? (first on-trunk)))
        (expect (str/includes? (:description (first on-trunk)) "working tree"))))
  (it
    "draft manager uses standard filter keys and modified management actions"
    (let
      [drafts
       [{"workspace_id" "ws-a" "label" "feature-a" "is_current" true}]

       component
       (dlg/draft-picker-component drafts)

       initial
       (:init component)

       measured
       ((:measure component) initial 120 40)

       state
       ((:reconcile component) initial measured)

       ctrl-key
       (fn [c]
         (KeyStroke. (Character/valueOf c) true false false))

       new-result
       ((:on-key component) state (ctrl-key \n) measured)

       abandon-result
       ((:on-key component) state (ctrl-key \d) measured)

       typed-n
       ((:on-key component) state (char-key \N) measured)]

      (expect (>= (:content-w measured) 72))
      (expect (>= (:content-h-req measured) 12))
      (expect (= :new (:action (done-key new-result))))
      (expect (= :abandon (:action (done-key abandon-result))))
      (expect (= "ws-a" (:workspace-id (done-key abandon-result))))
      (expect (not (contains? typed-n done-key)))
      (expect (= "N" (:query typed-n)))))
  (it "draft filtering shares picker matching and hides nonmatching sections"
      (let
        [items
         (dlg/draft-picker-items [{"workspace_id" "ws-a" "label" "feature-a" "root" "/tmp/alpha"}
                                  {"workspace_id" "ws-b" "label" "docs" "root" "/tmp/beta"}])

         component
         (dlg/draft-picker-component
           [{"workspace_id" "ws-a" "label" "feature-a" "root" "/tmp/alpha"}
            {"workspace_id" "ws-b" "label" "docs" "root" "/tmp/beta"}])

         measure
         (:measure component)

         feature
         (measure (assoc (:init component) :query "FEATURE") 120 40)

         path
         (measure (assoc (:init component) :query "beta") 120 40)

         trunk
         (measure (assoc (:init component) :query "repository") 120 40)

         none
         (measure (assoc (:init component) :query "missing") 120 40)]

        (expect (= ["feature-a"] (mapv :label (:selectable feature))))
        (expect (= ["docs"] (mapv :label (:selectable path))))
        (expect (= ["Trunk"] (mapv :label (:selectable trunk))))
        (expect (empty? (:selectable none)))
        (expect (= (mapv :label items) (mapv :label (dlg/filter-select-items items ""))))))
  (it "draft manager filters interactively and returns the stable workspace id"
      (let
        [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
         (virtual-screen)

         drafts
         [{"workspace_id" "ws-a" "label" "feature-a" "is_current" true}
          {"workspace_id" "ws-b" "label" "feature-b" "is_current" false}]]

        (try (.addInput terminal (KeyStroke. (Character/valueOf \b) false false false))
             (.addInput terminal (KeyStroke. KeyType/Enter))
             (expect (= "ws-b" (:workspace-id (dlg/draft-picker! screen drafts))))
             (finally (.stopScreen screen)))))
  (it "command palette exposes the frequent app verbs; Router is the single provider/settings hub"
      (let
        [palette-commands
         (var-get #'dlg/palette-commands)

         labels
         (mapv :label palette-commands)

         ids
         (set (mapv :id palette-commands))]

        ;; Providers live under the "Router" hub; Settings is its own palette
        ;; verb (no longer a hidden `S` keybind inside the Router).
        (expect (some #{"Router"} labels))
        (expect (some #{"Settings"} labels))
        (expect (not (some #{"Configure Providers"} labels)))
        (expect (contains? ids :providers))
        (expect (contains? ids :settings))
        ;; The palette is THE entry point (Ctrl+P) for the verbs whose Alt chords
        ;; don't survive macOS — so the frequent ones must be present + runnable.
        (expect (every? ids
                        [:cycle-model :cycle-reasoning :search-open :open-resources :show-sessions
                         :open-drafts :pick-file :new-session :fork-session]))))
  (it "command palette filters by a typed query (searchable)"
      ;; The palette is searchable: the filter is a case-insensitive substring
      ;; match on :label, the spine `searchable-select!` applies.
      (let
        [labels
         (mapv :label (var-get #'dlg/palette-commands))

         match
         (fn [q]
           (filterv #(clojure.string/includes? (clojure.string/lower-case %)
                                               (clojure.string/lower-case q))
             labels))]

        (expect (some #{"Cycle Model"} (match "model")))
        (expect (= [] (match "zzz-no-such-command"))))))

(defdescribe fork-turn-items-test
             (it "builds filterable palette rows: message label, tN hint, turn-id, truncation"
                 (let
                   [turns
                    [{:id "s1" :position 1 :user-request "  first   question here  "}
                     {:id "s2" :position 2 :user-request (apply str (repeat 200 "x"))}
                     {:id "s3" :position 3 :user-request "   "}]

                    rows
                    (dlg/fork-turn-items turns)]

                   ;; each row carries the soul id the fork copies THROUGH
                   (expect (= ["s1" "s2" "s3"] (mapv :turn-id rows)))
                   ;; ordinal hint
                   (expect (= ["t1" "t2" "t3"] (mapv :hint rows)))
                   ;; whitespace collapsed for the searchable label
                   (expect (= "first question here" (:label (first rows))))
                   ;; long messages truncated with an ellipsis
                   (expect (<= (count (:label (second rows))) 72))
                   (expect (clojure.string/ends-with? (:label (second rows)) "…"))
                   ;; blank message gets a placeholder
                   (expect (= "(no message)" (:label (nth rows 2)))))))

;; Navigator PROJECT grouping: non-focused rows regroup by `:dir`
;; (first-appearance order = projects by their most recent session),
;; recency preserved inside a group; the focused row stays pinned on top.
(defdescribe
  navigator-project-grouping-test
  (it
    "groups non-focused rows by project dir, groups ordered by recency"
    (let
      [all-rows
       (var-get #'dlg/navigator-all-rows)

       sessions
       [{"id" "s1"
         "title" "A1"
         "turn_count" 1
         "created_at" 0
         "modified_at" 4000
         :work-dir "~/proj-a"}
        {"id" "s2"
         "title" "B1"
         "turn_count" 1
         "created_at" 0
         "modified_at" 3000
         :work-dir "~/proj-b"}
        {"id" "s3"
         "title" "A2"
         "turn_count" 1
         "created_at" 0
         "modified_at" 2000
         :work-dir "~/proj-a"}
        {"id" "s4"
         "title" "B2"
         "turn_count" 1
         "created_at" 0
         "modified_at" 1000
         :work-dir "~/proj-b"}]

       rows
       (all-rows {:active-session-id "s1" :sessions sessions})]

      ;; s1 pinned (focused); remaining [s2 s3 s4] regroup to
      ;; proj-b [s2 s4] (newest group first) then proj-a [s3].
      (expect (= ["s1" "s2" "s4" "s3"] (mapv (comp str :id :target) rows)))))
  (it "sessions without a work-dir share one group and keep recency order"
      (let
        [all-rows
         (var-get #'dlg/navigator-all-rows)

         sessions
         [{"id" "s1" "title" "A" "turn_count" 1 "created_at" 0 "modified_at" 3000}
          {"id" "s2" "title" "B" "turn_count" 1 "created_at" 0 "modified_at" 2000}
          {"id" "s3" "title" "C" "turn_count" 1 "created_at" 0 "modified_at" 1000}]

         rows
         (all-rows {:active-session-id "s1" :sessions sessions})]

        (expect (= ["s1" "s2" "s3"] (mapv (comp str :id :target) rows))))))

(defdescribe
  fit-hint-pairs-test
  "The hint bar must CLIP to the dialog's content width by dropping whole
   trailing chords — `put-str!` clips to the screen, not the box, so an
   unfitted footer (e.g. magit's 162-col one) would paint across the border."
  (it "returns all pairs when they fit exactly"
      (let
        [hints
         (var-get #'dlg/magit-hints)

         w
         (dlg/hint-bar-width hints)]

        (expect (= hints (dlg/fit-hint-pairs hints w)))))
  (it "drops whole trailing pairs when the bar is too wide"
      (let
        [hints
         (var-get #'dlg/magit-hints)

         fitted
         (dlg/fit-hint-pairs hints 112)]

        (expect (< (count fitted) (count hints)))
        (expect (= fitted (subvec (vec hints) 0 (count fitted))))
        (expect (<= (dlg/hint-bar-width (vec fitted)) 112))))
  (it "never exceeds text-w at any width (whole-pair invariant)"
      (let [hints (var-get #'dlg/magit-hints)]
        (expect (every? (fn [tw]
                          (<= (dlg/hint-bar-width (vec (dlg/fit-hint-pairs hints tw))) (max tw 0)))
                        (range 0 200)))))
  (it "fits nothing into a sliver without blowing up"
      (expect (= [] (dlg/fit-hint-pairs (var-get #'dlg/magit-hints) 3)))
      (expect (= [] (dlg/fit-hint-pairs (var-get #'dlg/magit-hints) 0)))))
