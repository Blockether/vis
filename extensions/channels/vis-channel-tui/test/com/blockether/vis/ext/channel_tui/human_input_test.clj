(ns com.blockether.vis.ext.channel-tui.human-input-test
  "Pure-model and paint tests for the TUI half of the human-input dialog.

   The model is a plain map, so navigation, editing, toggling and the
   submit/cancel decisions are asserted without a terminal. Painting is
   asserted against a Lanterna virtual terminal back-buffer, the same
   harness `dialogs-test` uses."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.human-input :as hi]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.internal.human-input :as engine]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize TextCharacter]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- request
  "A request VIEW shaped exactly like `human-input/request->view` emits."
  []
  {:id "req-1"
   :title "Deploy"
   :description "Pick the target"
   :fields
   [{:id "user" :type :plaintext :label "User" :is-required true :max-length 3 :placeholder "who"}
    {:id "pass" :type :password :label "Password" :is-required true}
    {:id "env"
     :type :select
     :label "Env"
     :options [{:value "dev" :label "Dev"} {:value "prod" :label "Prod"}]}
    {:id "tags"
     :type :multiselect
     :label "Tags"
     :options [{:value "a" :label "Alpha"} {:value "b" :label "Beta"}]}
    {:id "ok" :type :checkbox :label "Confirm"}
    {:id "note" :type :multiline :label "Note" :description "Free text"}]
   :submit-label "Submit"
   :cancel-label "Cancel"
   :is-cancellable true
   :timeout-ms 300000})

(defn- feed
  "Apply normalized events in order, keeping only the form."
  [form events]
  (reduce (fn [acc event]
            (:form (hi/handle-event acc event)))
          form
          events))

(defn- ch "A printable character event." [c] {:kind :char :char c})

(defn- virtual-screen
  "A started 80x30 virtual screen plus its text graphics."
  []
  (Thread/interrupted)
  (let
    [terminal
     (DefaultVirtualTerminal. (TerminalSize. 80 30))

     screen
     (TerminalScreen. terminal)]

    (.startScreen screen)
    {:screen screen :g (.newTextGraphics screen)}))

(defn- screen-row
  "The rendered characters of one back-buffer row, right-trimmed."
  [^TerminalScreen screen y]
  (str/trimr (apply str
               (for [x (range 80)]
                 (.getCharacterString (.getBackCharacter screen (int x) (int y)))))))

(defn- screen-text
  "The whole back buffer as one string."
  [screen]
  (str/join "\n" (map #(screen-row screen %) (range 30))))

(defdescribe
  init-form-test
  (it "seeds one value per field from the declared defaults"
      (let [form (hi/init-form (request))]
        (expect (= {"user" "" "pass" "" "env" "dev" "tags" [] "ok" false "note" ""} (:values form)))
        (expect (= 0 (:focus form)))
        (expect (= "req-1" (hi/request-id form)))))
  (it "honors declared defaults, coercing each to its field type"
      (let
        [form (hi/init-form {:id "r"
                             :title "T"
                             :fields [{:id "a" :type :plaintext :label "A" :default 7}
                                      {:id "b" :type :checkbox :label "B" :default true}
                                      {:id "c"
                                       :type :select
                                       :label "C"
                                       :default "y"
                                       :options [{:value "x" :label "X"} {:value "y" :label "Y"}]}
                                      {:id "d"
                                       :type :multiselect
                                       :label "D"
                                       :default ["x"]
                                       :options [{:value "x" :label "X"}]}]})]
        (expect (= {"a" "7" "b" true "c" "y" "d" ["x"]} (:values form)))))
  (it "walks one stop per text field and one stop per declared option"
      (expect (= [{:kind :text :field-id "user"} {:kind :text :field-id "pass"}
                  {:kind :select-option :field-id "env" :value "dev"}
                  {:kind :select-option :field-id "env" :value "prod"}
                  {:kind :multi-option :field-id "tags" :value "a"}
                  {:kind :multi-option :field-id "tags" :value "b"} {:kind :checkbox :field-id "ok"}
                  {:kind :text :field-id "note"} {:kind :action :action :submit :label "Submit"}
                  {:kind :action :action :cancel :label "Cancel"}]
                 (hi/stops (request))))))

(defdescribe
  editing-test
  (it "types into the focused text field and enforces :max-length"
      (let [form (feed (hi/init-form (request)) [(ch \a) (ch \b) (ch \c) (ch \d)])]
        (expect (= "abc" (get-in form [:values "user"])))))
  (it "backspace deletes before the cursor, delete removes under it"
      (let [typed (feed (hi/init-form (request)) [(ch \a) (ch \b)])]
        (expect (= "a" (get-in (feed typed [{:kind :backspace}]) [:values "user"])))
        (expect (= "a" (get-in (feed typed [{:kind :left} {:kind :delete}]) [:values "user"])))))
  (it "Enter inserts a newline inside a multiline field instead of submitting"
      (let
        [form
         (feed (hi/init-form (request)) [{:kind :end} (ch \x) {:kind :enter} (ch \y)])

         multi
         (assoc form :focus 7)

         typed
         (feed multi [(ch \x) {:kind :enter} (ch \y)])]

        (expect (= "x\ny" (get-in typed [:values "note"])))
        (expect (nil? (:action (hi/handle-event typed {:kind :enter}))))
        (expect (= :submit (:action (hi/handle-event typed {:kind :submit}))))))
  (it "keeps a password out of the paint plan"
      (let
        [form
         (feed (hi/init-form (assoc (request)
                               :fields [{:id "pass" :type :password :label "Password"}]))
               [(ch \h) (ch \i)])

         texts
         (map :text (hi/form-rows form))]

        (expect (= "hi" (get-in form [:values "pass"])))
        (expect (some #{"••"} texts))
        (expect (not-any? #{"hi"} texts)))))

(defdescribe navigation-and-toggle-test
             (it "Tab/arrow keys walk the stops and wrap around"
                 (let [form (hi/init-form (request))]
                   (expect (= 1 (:focus (feed form [{:kind :next}]))))
                   (expect (= 9 (:focus (feed form [{:kind :prev}]))))))
             (it "Space picks exactly one option in a select"
                 (let
                   [form (feed (hi/init-form (request))
                               [{:kind :next} {:kind :next} {:kind :next} (ch \space)])]
                   (expect (= "prod" (get-in form [:values "env"])))))
             (it "Space adds and removes multiselect values in declared order"
                 (let
                   [base
                    (assoc (hi/init-form (request)) :focus 5)

                    one
                    (feed base [(ch \space)])

                    both
                    (feed one [{:kind :prev} (ch \space)])

                    back
                    (feed both [(ch \space)])]

                   (expect (= ["b"] (get-in one [:values "tags"])))
                   (expect (= ["a" "b"] (get-in both [:values "tags"])))
                   (expect (= ["b"] (get-in back [:values "tags"])))))
             (it "Space flips a checkbox"
                 (let [form (feed (assoc (hi/init-form (request)) :focus 6) [(ch \space)])]
                   (expect (true? (get-in form [:values "ok"])))
                   (expect (false? (get-in (feed form [(ch \space)]) [:values "ok"]))))))

(defdescribe submit-and-cancel-test
             (it "Enter asks for submit and hands back values keyed by field id"
                 (let
                   [pristine
                    (hi/init-form (request))

                    form
                    (feed pristine [(ch \a) {:kind :next} (ch \p)])

                    {:keys [action]}
                    (hi/handle-event form {:kind :enter})]

                   ;; Even a pristine form with an empty REQUIRED field submits:
                   ;; the band holds no rules, so refusing is the engine's job.
                   (expect (= :submit (:action (hi/handle-event pristine {:kind :enter}))))
                   (expect (= :submit action))
                   (expect (= {"user" "a" "pass" "p" "env" "dev" "tags" [] "ok" false "note" ""}
                              (hi/submit-values form)))))
             (it "Escape cancels a cancellable request and is inert otherwise"
                 (let
                   [form
                    (hi/init-form (request))

                    locked
                    (hi/init-form (assoc (request) :is-cancellable false))]

                   (expect (= :cancel (:action (hi/handle-event form {:kind :cancel}))))
                   (expect (nil? (:action (hi/handle-event locked {:kind :cancel}))))))
             (it "engine rejections attach to their field and move the cursor there"
                 (let
                   [form (-> (hi/init-form (request))
                             (assoc :focus 6)
                             (hi/set-errors {"pass" "Password is required"}))]
                   (expect (= 1 (:focus form)))
                   (expect (= {"pass" "Password is required"} (:errors form)))
                   (expect (some #{"Password is required"} (map :text (hi/form-rows form)))))))

(defdescribe
  key-normalization-test
  (it "maps Lanterna keystrokes onto the pure event vocabulary"
      (expect (= [:cancel :enter :next :prev :next :prev :left :right :backspace :delete :home :end
                  :char :submit]
                 (mapv (comp :kind hi/key->event)
                       [(KeyStroke. KeyType/Escape) (KeyStroke. KeyType/Enter)
                        (KeyStroke. KeyType/Tab) (KeyStroke. KeyType/ReverseTab)
                        (KeyStroke. KeyType/ArrowDown) (KeyStroke. KeyType/ArrowUp)
                        (KeyStroke. KeyType/ArrowLeft) (KeyStroke. KeyType/ArrowRight)
                        (KeyStroke. KeyType/Backspace) (KeyStroke. KeyType/Delete)
                        (KeyStroke. KeyType/Home) (KeyStroke. KeyType/End)
                        (KeyStroke. (Character/valueOf \q) false false false)
                        (KeyStroke. (Character/valueOf \s) true false false)]))))
  (it "ignores keystrokes the form has no meaning for"
      (expect (nil? (hi/key->event (KeyStroke. KeyType/MouseEvent))))
      (expect (nil? (hi/key->event (KeyStroke. (Character/valueOf \k) true false false)))))
  (it "C-g cancels the form, exactly like Esc"
      ;; The chat loop feeds this form RAW keystrokes, so Emacs `keyboard-quit`
      ;; has to be decoded here or C-g does nothing while a request is open.
      (expect (= :cancel
                 (:kind (hi/key->event (KeyStroke. (Character/valueOf \g) true false false)))))
      (expect (= :cancel
                 (:kind (hi/key->event
                          (KeyStroke. (Character/valueOf (char 7)) false false false)))))
      (expect (= :char
                 (:kind (hi/key->event (KeyStroke. (Character/valueOf \g) false false false))))))
  (it "carries the typed character through"
      (expect (= \q
                 (:char (hi/key->event (KeyStroke. (Character/valueOf \q) false false false)))))))

;; The hint bar is NAVIGATION AND TYPING ONLY: the pinned action bar prints the
;; submit/cancel caps with their own chords one row above it, so a hint pair for
;; either action is the same sentence printed twice.
(defdescribe hint-test
             (it "keeps submit and cancel out of the hint bar"
                 (let [pairs (hi/hint (hi/init-form (request)))]
                   ;; Navigation is not a hint: `↑/↓ move` used to lead every
                   ;; pause's bar. A plain text field earns NO chords at all.
                   (expect (empty? pairs))
                   (expect (not-any? #{"submit" "cancel" "press"} (map second pairs)))
                   (expect (not-any? #{"Esc" "^S"} (map first pairs)))))
             (it "offers Enter as a newline chord inside a multiline field"
                 (let [pairs (hi/hint (assoc (hi/init-form (request)) :focus 7))]
                   (expect (some #{["Enter" "newline"]} pairs))
                   (expect (not-any? #{"submit"} (map second pairs))))))

(defdescribe paint-test
             (it "paints the title, the description and every field label"
                 (let
                   [{:keys [screen g]}
                    (virtual-screen)

                    _
                    (hi/paint! g 80 30 (hi/init-form (request)))

                    text
                    (screen-text screen)]

                   (expect (str/includes? text "Deploy"))
                   (expect (str/includes? text "Pick the target"))
                   (expect (str/includes? text "User  REQUIRED"))
                   (expect (str/includes? text "Password  REQUIRED"))
                   (expect (str/includes? text "Dev"))))
             (it "places the terminal cursor inside the focused text field"
                 (let
                   [{:keys [screen g]}
                    (virtual-screen)

                    form
                    (feed (hi/init-form (request)) [(ch \a) (ch \b)])

                    pos
                    (hi/paint! g 80 30 form)

                    row
                    (screen-row screen (.getRow pos))]

                   (expect (some? pos))
                   (expect (str/includes? row "ab"))
                   ;; The cursor sits one column past the typed text. The band is
                   ;; SIDELESS, so nothing is painted to the right of the value:
                   ;; the trimmed row STOPS at the cursor's own blank column.
                   (expect (= (+ 2 (str/index-of row "ab")) (.getColumn pos)))))
             (it "shows the mask, never the password plaintext"
                 (let
                   [{:keys [screen g]}
                    (virtual-screen)

                    form
                    (feed (assoc (hi/init-form (request)) :focus 1) [(ch \h) (ch \i)])

                    _
                    (hi/paint! g 80 30 form)

                    text
                    (screen-text screen)]

                   (expect (str/includes? text "••"))
                   (expect (not (str/includes? text "hi")))))
             (it "scrolls the body so the focused field stays visible"
                 (let
                   [{:keys [screen g]}
                    (virtual-screen)

                    form
                    (assoc (hi/init-form (request)) :focus 7)

                    _
                    (hi/paint! g 80 30 form)

                    text
                    (screen-text screen)]

                   (expect (str/includes? text "Note"))
                   (expect (not (str/includes? text "User *")))))
             (it "renders an engine rejection next to its field"
                 (let
                   [{:keys [screen g]}
                    (virtual-screen)

                    form
                    (hi/set-errors (hi/init-form (request)) {"user" "User is required"})

                    _
                    (hi/paint! g 80 30 form)]

                   (expect (str/includes? (screen-text screen) "User is required")))))

(defn- stroke
  "A printable KeyStroke, exactly as Lanterna delivers it."
  [c]
  (KeyStroke. (Character/valueOf ^Character c) false false))

(defdescribe
  state-wiring-test
  (it "queues a second request and shows it when the first one closes"
      (reset! state/app-db {:render-version 0})
      (state/dispatch [:human-input-open (hi/init-form (request))])
      (state/dispatch [:human-input-open (hi/init-form (assoc (request) :id "req-2"))])
      ;; Only ONE dialog is ever on screen; the rest wait their turn.
      (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
      (expect (= ["req-2"] (mapv #(get-in % [:request :id]) (:human-input-queue @state/app-db))))
      (state/dispatch [:human-input-close "req-1"])
      (expect (= "req-2" (get-in @state/app-db [:human-input :request :id])))
      (expect (empty? (:human-input-queue @state/app-db)))
      (state/dispatch [:human-input-close "req-2"])
      (expect (nil? (:human-input @state/app-db))))
  (it "drops a queued request that settles before it is ever shown"
      ;; A timeout or a sibling channel can answer a QUEUED request. Closing it
      ;; must not promote it onto the screen, and must not disturb the visible one.
      (reset! state/app-db {:render-version 0})
      (state/dispatch [:human-input-open (hi/init-form (request))])
      (state/dispatch [:human-input-open (hi/init-form (assoc (request) :id "req-2"))])
      (state/dispatch [:human-input-close "req-2"])
      (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
      (expect (empty? (:human-input-queue @state/app-db))))
  (it "opens and closes the dialog straight from channel events"
      (reset! state/app-db {:render-version 0})
      (#'screen/handle-channel-event! {:op :human-input/request :request (request)})
      (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
      ;; An open dialog owns the screen: render fast paths off, cursor is ours.
      (expect (true? (#'screen/overlay-locked? @state/app-db)))
      (#'screen/handle-channel-event! {:op :human-input/close :request-id "req-1"})
      (expect (nil? (:human-input @state/app-db)))
      (expect (false? (#'screen/overlay-locked? @state/app-db))))
  (it "submits the typed values and closes once the engine accepts them"
      (let [submitted (atom nil)]
        (with-redefs
          [vis/submit-human-input! (fn [id values]
                                     (reset! submitted [id values])
                                     {:is-accepted true})]
          (reset! state/app-db {:render-version 0})
          (state/dispatch [:human-input-open (hi/init-form (request))])
          (doseq
            [key [(stroke \b) (stroke \o) (KeyStroke. KeyType/ArrowDown) (stroke \p)
                  (KeyStroke. KeyType/Enter)]]
            (#'screen/human-input-key! @state/app-db key))
          (expect (= "req-1" (first @submitted)))
          (expect (= "bo" (get-in @submitted [1 "user"])))
          (expect (= "p" (get-in @submitted [1 "pass"])))
          (expect (nil? (:human-input @state/app-db))))))
  (it "keeps the dialog open and shows the engine's errors on a rejected answer"
      (with-redefs
        [vis/submit-human-input! (fn [_ _]
                                   {:is-accepted false :errors {"pass" "Password is expired"}})]
        (reset! state/app-db {:render-version 0})
        (state/dispatch [:human-input-open (hi/init-form (request))])
        ;; Both required fields are filled, so the form's OWN rules pass and the
        ;; answer actually reaches the engine — which is the one refusing it here.
        (doseq
          [key [(stroke \b) (KeyStroke. KeyType/ArrowDown) (stroke \p) (KeyStroke. KeyType/Enter)]]
          (#'screen/human-input-key! @state/app-db key))
        (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
        (expect (= {"pass" "Password is expired"} (get-in @state/app-db [:human-input :errors])))))
  (it "cancels the pending request on Escape"
      (let [cancelled (atom nil)]
        (with-redefs
          [vis/cancel-human-input! (fn [id]
                                     (reset! cancelled id)
                                     true)]
          (reset! state/app-db {:render-version 0})
          (state/dispatch [:human-input-open (hi/init-form (request))])
          (#'screen/human-input-key! @state/app-db (KeyStroke. KeyType/Escape))
          (expect (= "req-1" @cancelled))
          (expect (nil? (:human-input @state/app-db)))))))

(defn- canonical-row
  "Row text a CANONICAL `dialogs` row painter produces, trimmed. The dialog
   must paint the very same characters for the same row."
  [draw!]
  (let [{:keys [screen g]} (virtual-screen)]
    (draw! g)
    (str/trim (screen-row screen 0))))

(defn- painted-rows
  "Every painted human-input row containing `needle`, with the dialog frame and
   padding stripped, so a row compares directly against a canonical painter."
  [form needle]
  (let
    [{:keys [screen g]}
     (virtual-screen)

     _
     (hi/paint! g 80 30 form)]

    (into []
          (keep (fn [y]
                  (let [row (str/trim (str/replace (screen-row screen y) "│" " "))]
                    (when (str/includes? row needle) row))))
          (range 30))))

(defdescribe
  canonical-presentation-test
  "Cross-validation against the canonical TUI dialog vocabulary: the same row
   painters, the same hint-bar spelling, the same scrollbar gutter."
  (it "paints checkbox rows exactly like every other dialog checkbox"
      (let [rows (painted-rows (assoc (hi/init-form (request)) :focus 6) "Confirm")]
        (expect (some #{(canonical-row
                          (fn [g]
                            (dialogs/draw-checkbox-item! g 0 0 40 true false "Confirm")))}
                      rows))
        ;; The checkbox row IS the label — no duplicate bold label row above it.
        (expect (= 1 (count rows)))))
  (it "paints select options with the shared ●/○ status marks"
      (expect (some #{(canonical-row (fn [g]
                                       (dialogs/draw-radio-item! g 0 0 40 true true "Dev")))}
                    (painted-rows (assoc (hi/init-form (request)) :focus 2) "Dev")))
      (expect (some #{(canonical-row (fn [g]
                                       (dialogs/draw-radio-item! g 0 0 40 false false "Prod")))}
                    (painted-rows (assoc (hi/init-form (request)) :focus 2) "Prod"))))
  (it "paints text fields with the shared borderless input row"
      (expect (some #{(canonical-row (fn [g]
                                       (dialogs/draw-text-input-field! g 0 0 40 "" 0 "who")))}
                    (painted-rows (assoc (hi/init-form (request)) :focus 0) "who"))))
  (it "keeps navigation chords and ASCII button brackets off the screen"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g 80 30 (hi/init-form (request)))

         text
         (screen-text screen)]

        ;; `↑/↓ move` was a permanent row of chrome for the one chord every
        ;; terminal operator already knows.
        (expect (not (str/includes? text "↑/↓")))
        ;; The caps are the SHARED neobrutalist chip: filled pills, never `[ … ]`
        ;; ASCII and never `▏…▕` outlines.
        (expect (not (str/includes? text "[ Submit ]")))
        (expect (not (str/includes? text "[ Cancel ]")))
        (expect (not (str/includes? text "▏")))
        (expect (not (str/includes? text "▕")))
        ;; The two actions are spelled ONCE, on the pinned caps — no chord beside
        ;; them, and no hint bar reprinting them one row lower.
        (expect (not (str/includes? text "Enter")))
        (expect (not (str/includes? text "Esc")))))
  (it "puts a scrollbar thumb in the gutter once the form outgrows the box"
      ;; The band grows UPWARD over the transcript, so on a roomy terminal this
      ;; form fits whole; it is a SHORT terminal that squeezes it into a window.
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 16 (assoc (hi/init-form (request)) :focus 7))
        (expect (str/includes? (screen-text screen) "\u2588"))))
  (it "leaves the gutter clean when the whole form fits"
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g
                   80
                   30
                   (hi/init-form {:id "r"
                                  :title "Tiny"
                                  :fields [{:id "a" :type :checkbox :label "Yes"}]
                                  :is-cancellable true}))
        (expect (not (str/includes? (screen-text screen) "█"))))))

(defn- rule-row?
  "True when a painted row is one of the band's horizontal rules and NOTHING
   else — no `│` rails and no `├`/`┤` junctions, because the session frame whose
   chrome the band borrows is sideless."
  [s]
  (and (str/includes? s "───") (every? #{\space \─} s)))

(defdescribe
  band-test
  ;; The human-input prompt is a magit-style TRANSIENT inside the session, not a
  ;; full-screen modal: it takes over the prompt's rows, grows upward over the
  ;; transcript, and leaves the session's own footer breathing underneath.
  (it "closes with the host's rule directly above its hint bar"
      (let [{:keys [screen g]} (virtual-screen)]
        ;; Focus the multiline field so the bar has a chord worth printing at
        ;; all — navigation alone no longer earns a hint pair.
        (hi/paint! g 80 30 (assoc (hi/init-form (request)) :focus 7))
        ;; `rows - 3` is the prompt box's own closing rule; the hint bar lands
        ;; there and the rule right above it is the band's foot.
        (expect (str/includes? (screen-row screen 27) "Enter newline"))
        (expect (rule-row? (screen-row screen 26)))))
  (it "never swallows the session's bottom chrome"
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 30 (hi/init-form (request)))
        (expect (= "" (screen-row screen 28)))
        (expect (= "" (screen-row screen 29)))))
  (it "stops at the top of the transcript instead of covering the header"
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 30 (assoc (hi/init-form (request)) :focus 7) 12)
        (expect (every? #(= "" (screen-row screen %)) (range 12)))
        (expect (str/includes? (screen-text screen) "Deploy"))))
  (it "frames its title between two rules, the way every transient does"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g
                    80
                    30
                    (hi/init-form (assoc (request)
                                    :fields [{:id "user" :type :plaintext :label "User"}])))

         title-y
         (first (filter #(str/includes? (screen-row screen %) "Deploy") (range 30)))]

        (expect (some? title-y))
        (expect (rule-row? (screen-row screen (dec title-y))))
        (expect (rule-row? (screen-row screen (inc title-y))))))
  ;; Regression, issue #108: a form taller than the band scrolled its own
  ;; the `Submit` / `Cancel` caps out of sight, so the only visible way to end
  ;; the pause was to guess a key.
  (it "pins the action bar above the closing rule, however tall the form"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g 80 30 (hi/init-form (request)))

         foot-rule-y
         (dec (long (:hint-row (hi/band-region 80 30 1))))

         bar
         (screen-row screen (dec foot-rule-y))]

        (expect (rule-row? (screen-row screen foot-rule-y)))
        ;; Ink pill for Submit, muted pill for Cancel — two filled caps, no rails.
        (expect (str/includes? bar " Submit "))
        (expect (str/includes? bar " Cancel"))
        (expect (not (str/includes? bar "▏")))
        (expect (not (str/includes? bar "▕")))))
  ;; Regression: the band said the same two things twice — the pinned
  ;; `[ Submit ]` / `[ Cancel ]` row and, one row under it, a hint bar reading
  ;; `Enter submit · Esc cancel`. Two rows of chrome for one meaning — and the
  ;; chords stencilled ON the caps were a third: a cap is a focus stop ↑/↓ walks
  ;; onto, so it needs no shortcut printed beside it.
  (it "states each action once — on its cap, with no chord and no second row"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g 80 30 (hi/init-form (request)))

         hint-y
         (long (:hint-row (hi/band-region 80 30 1)))

         bar
         (screen-row screen (- hint-y 2))

         hints
         (str/lower-case (screen-row screen hint-y))

         text
         (screen-text screen)]

        (expect (str/includes? bar " Submit "))
        (expect (str/includes? bar " Cancel"))
        (expect (not (str/includes? text "Enter")))
        (expect (not (str/includes? text "Esc")))
        ;; ...and with focus on a plain text field the bar below has nothing to
        ;; say at all — no `move`, and still no submit/cancel.
        (expect (str/blank? hints))
        (expect (not (str/includes? hints "submit")))
        (expect (not (str/includes? hints "cancel")))))
  (it "walks the • cursor onto a cap instead of recolouring it"
      (let
        [form
         (hi/init-form (request))

         cancel-idx
         (dec (count (:stops form)))

         bar-of
         (fn [f]
           (let [{:keys [screen g]} (virtual-screen)]
             (hi/paint! g 80 30 f)
             (screen-row screen (- (long (:hint-row (hi/band-region 80 30 1))) 2))))]

        ;; The `•` is the same cursor glyph every checkbox and radio row wears.
        (expect (str/includes? (bar-of (assoc form :focus (dec cancel-idx))) "•  Submit "))
        (expect (str/includes? (bar-of (assoc form :focus cancel-idx)) "\u2022  Cancel"))
        ;; ...and it is the only thing that moves — Submit keeps its pill either way.
        (expect (str/includes? (bar-of (assoc form :focus cancel-idx)) " Submit "))))
  (it "marks exactly the focused button and keeps the buttons out of the body"
      (let
        [form
         (hi/init-form (request))

         submit-idx
         (- (count (:stops form)) 2)]

        (expect (not-any? #(= :action (:kind %)) (hi/form-rows form)))
        (expect (= [:submit :cancel] (mapv :action (:buttons (hi/action-bar form)))))
        (expect (= [true false]
                   (mapv :is-focused (:buttons (hi/action-bar (assoc form :focus submit-idx))))))
        (expect (= [false true]
                   (mapv :is-focused
                         (:buttons (hi/action-bar (assoc form :focus (inc submit-idx)))))))
        (expect (= [:submit]
                   (mapv :action
                         (:buttons (hi/action-bar (hi/init-form (assoc (request)
                                                                  :is-cancellable false)))))))))
  (it "draws no side rails at all"
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 30 (hi/init-form (request)))
        (let [text (screen-text screen)]
          (expect (not (str/includes? text "│")))
          (expect (not (str/includes? text "├")))
          (expect (not (str/includes? text "┤"))))))
  (it "anchors the band on the prompt's closing rule at any height"
      ;; PURE: whatever the editor grew to, the band's hint row is `rows - 3`.
      (expect (= 27 (:hint-row (hi/band-region 80 30 1))))
      (expect (= 37 (:hint-row (hi/band-region 80 40 1))))
      ;; ...unless the transcript's top would be crossed, which wins.
      (expect (= 1 (:left (hi/band-region 80 30 1))))
      (expect (= 12 (:min-row (hi/band-region 80 30 12))))))

(defn- row-index
  "Index of the first plan row matching `pred`."
  [rows pred]
  (first (keep-indexed (fn [i r]
                         (when (pred r) i))
                       rows)))

(defn- screen-row-of
  "The y of the first back-buffer row containing `needle`, or nil."
  [screen needle]
  (first (keep (fn [y]
                 (when (str/includes? (screen-row screen y) needle) y))
               (range 30))))

(defn- modifiers-at
  "The SGR modifiers the back buffer holds at `x`,`y`."
  [^TerminalScreen screen ^long x ^long y]
  (set (.getModifiers ^TextCharacter (.getBackCharacter screen (int x) (int y)))))

(defn- modifiers-of
  "The SGR modifiers painted on the first character of `needle`."
  [screen needle]
  (let [y (screen-row-of screen needle)]
    (when y (modifiers-at screen (.indexOf ^String (screen-row screen y) ^String needle) y))))

(defn- bg-of
  "The background colour the back buffer holds on the first cell of `needle` —
   which FILL a painted cap wears."
  [^TerminalScreen screen needle]
  (let
    [y
     (long (screen-row-of screen needle))

     x
     (.indexOf ^String (screen-row screen y) ^String needle)]

    (.getBackgroundColor ^TextCharacter (.getBackCharacter screen (int x) (int y)))))

(defn- cap-bgs
  "The fills the painted action row gives `form`'s Submit and Cancel caps."
  [form]
  (let [{:keys [screen g]} (virtual-screen)]
    (hi/paint! g 80 30 form)
    ;; The row is right-trimmed, so the LAST cap keeps only its leading pad cell —
    ;; which is fill either way.
    [(bg-of screen " Submit ") (bg-of screen " Cancel")]))

(defdescribe
  label-and-description-test
  "Three names, three jobs, drawn in that order: the bold LABEL says what the
   field is, the ITALIC description explains it, and only then comes the input.
   Prose that arrives after the box you already filled is prose nobody reads."
  (it "says REQUIRED beside the label of every field the engine will refuse"
      ;; The engine rejects a submission that leaves one of these blank, so the
      ;; dialog names them BEFORE the operator hits enter — in full, not as a `*`
      ;; nobody reads.
      (let
        [rows
         (hi/form-rows (hi/init-form (request)))

         label-of
         (fn [needle]
           (some #(when (and (= :label (:kind %)) (str/starts-with? (str (:text %)) needle))
                    (:text %))
                 rows))

         checkbox-rows
         (hi/form-rows (hi/init-form
                         {:id "r"
                          :title "T"
                          :fields
                          [{:id "ok" :type :checkbox :label "Confirm" :is-required true}]}))]

        (expect (= "User  REQUIRED" (label-of "User")))
        (expect (= "Password  REQUIRED" (label-of "Password")))
        (expect (= "Env" (label-of "Env")))
        ;; A checkbox carries its own label, so the marker rides the box itself.
        (expect (= "Confirm  REQUIRED" (:text (first checkbox-rows))))))
  (it "puts a field's description between its label and its input"
      (let
        [rows
         (hi/form-rows (hi/init-form (request)))

         i
         (row-index rows #(and (= :label (:kind %)) (= "Note" (:text %))))]

        (expect (some? i))
        (expect (= {:kind :description :text "Free text"} (nth rows (inc i))))
        (expect (= :input (:kind (nth rows (+ (long i) 2)))))
        (expect (= "note" (:field-id (nth rows (+ (long i) 2)))))))
  (it "leaves a field with no description with just its label and input"
      (let
        [rows
         (hi/form-rows (hi/init-form (request)))

         i
         (row-index rows #(and (= :label (:kind %)) (= "Env" (:text %))))]

        (expect (some? i))
        (expect (= :option (:kind (nth rows (inc (long i))))))))
  (it "hangs a checkbox description under the box that carries the label"
      (let
        [rows (hi/form-rows (hi/init-form {:id "r"
                                           :title "T"
                                           :fields [{:id "ok"
                                                     :type :checkbox
                                                     :label "Confirm"
                                                     :description "This cannot be undone"}]}))]
        ;; No bold label row: the checkbox row already says "Confirm", and the
        ;; description still explains it right underneath.
        (expect (= [:checkbox :description :blank] (mapv :kind rows)))
        (expect (= "This cannot be undone" (:text (second rows))))))
  (it "paints every description in italic and every label in bold"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         ;; A short form, so both prose rows are inside the dialog viewport.
         (hi/paint! g
                    80
                    30
                    (hi/init-form
                      {:id "r"
                       :title "T"
                       :description "Pick the target"
                       :fields
                       [{:id "note" :type :plaintext :label "Note" :description "Free text"}]}))]

        ;; The request's own description and the field's, both italic, never bold.
        (expect (= #{SGR/ITALIC} (modifiers-of screen "Pick the target")))
        (expect (= #{SGR/ITALIC} (modifiers-of screen "Free text")))
        ;; The label above it is the bold one — the two must not read alike.
        (expect (= #{SGR/BOLD} (modifiers-of screen "Note"))))))

(def ^:private prose
  "Two sentences of dialog prose — wider than any dialog row, so it reaches the
   operator only by wrapping."
  (str "This ships the tagged build straight to production and pages the "
       "engineer on duty, so read the target twice before you submit."))

(defn- prose-words
  "The words `rows` actually carry, in paint order — wrapping may re-break the
   lines, but it may never lose or reorder a word."
  [rows]
  (mapcat #(str/split (str/trim (str (:text %))) #"\s+") rows))

(defdescribe
  wrapped-description-test
  "A description is a SENTENCE, not a token. Ellipsizing it into one row throws
   away exactly the half that explained the ask, so prose wraps onto as many
   rows as it needs — the dialog's own description and every field's."
  (it "wraps the dialog's own description onto as many rows as it needs"
      (let
        [rows
         (hi/form-rows (hi/init-form {:id "r"
                                      :title "Deploy"
                                      :description prose
                                      :fields [{:id "env" :type :plaintext :label "Env"}]})
                       40)

         head
         (vec (take-while #(= :description (:kind %)) rows))]

        (expect (< 1 (count head)))
        (expect (every? #(<= (count (str (:text %))) 40) head))
        ;; Nothing is thrown away: no ellipsis, and every word survives in order.
        (expect (not-any? #(str/includes? (str (:text %)) "…") head))
        (expect (= (str/split prose #"\s+") (prose-words head)))
        ;; The blank spacer still separates the dialog prose from the first field.
        (expect (= :blank (:kind (nth rows (count head)))))))
  (it "wraps a field's description, still between its label and its input"
      (let
        [rows
         (hi/form-rows (hi/init-form
                         {:id "r"
                          :title "Deploy"
                          :fields [{:id "env" :type :plaintext :label "Env" :description prose}]})
                       40)

         i
         (long (row-index rows #(= :label (:kind %))))

         desc
         (vec (take-while #(= :description (:kind %)) (drop (inc i) rows)))]

        (expect (< 1 (count desc)))
        (expect (= (str/split prose #"\s+") (prose-words desc)))
        (expect (= :input (:kind (nth rows (+ i 1 (count desc))))))))
  (it "leaves the plan unwrapped when no width is offered"
      ;; The pure one-arity plan is what a caller measures without a terminal.
      (let
        [rows (hi/form-rows (hi/init-form {:id "r"
                                           :title "Deploy"
                                           :description prose
                                           :fields [{:id "env" :type :plaintext}]}))]
        (expect (= {:kind :description :text prose} (first rows)))
        (expect (= :blank (:kind (second rows))))))
  (it "paints the whole description instead of clipping it at the border"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g
                    80
                    30
                    (hi/init-form {:id "r"
                                   :title "Deploy"
                                   :description prose
                                   :fields [{:id "env" :type :plaintext :label "Env"}]}))]

        ;; The tail of the sentence is on screen only because it wrapped.
        (expect (some? (screen-row-of screen "submit.")))
        (expect (some? (screen-row-of screen "Env"))))))

(defn- squash
  "The back buffer with every space, newline and box-drawing glyph removed, so a
   sentence that was wrapped across rows reads back as one contiguous string."
  [s]
  (str/replace (str s) #"[\s│┃┌┐└┘─━█▀▄▌▐╭╮╯╰]" ""))

(def ^:private url
  "One unbreakable token far wider than any dialog row: no space to wrap at, so
   naive wrapping ellipsizes it and silently eats the half that mattered."
  (str "https://ci.example.com/pipelines/8127/runs/44/artifacts/"
       "release-2026.1.4-linux-arm64.tar.zst"))

(defdescribe
  hostile-description-test
  "The adversarial half: prose that does not fit, prose that is not prose, and
   terminals nobody sizes on purpose. A description that is clipped, doubled or
   fatal is worse than no description at all."
  (it "keeps every character of an unbreakable URL description on screen"
      ;; Wrapping and painting must agree on the width. When they disagree the
      ;; painter clips with `…` and the operator copies a truncated URL.
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g
                    80
                    30
                    (hi/init-form {:id "r"
                                   :title "Fetch"
                                   :description url
                                   :fields [{:id "ok" :type :checkbox :label "Go"}]}))

         painted
         (screen-text screen)]

        (expect (not (str/includes? painted "…")))
        (expect (str/includes? (squash painted) url))))
  (it "keeps wrapped prose whole on a terminal narrow enough to force a scrollbar"
      ;; The gutter steals a column, so the plan has to be re-wrapped narrower
      ;; instead of painted at the width it was measured with.
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g
                   46
                   12
                   (assoc (hi/init-form {:id "r"
                                         :title "Deploy"
                                         :description prose
                                         :fields [{:id "env" :type :plaintext :label "Env"}]})
                     :focus 0))
        (expect (not (str/includes? (screen-text screen) "…")))))
  (it "renders nothing at all for a description that is only whitespace"
      ;; A blank string is not a sentence: it must cost zero rows, not a gap that
      ;; pushes the first field out of view.
      (let
        [base
         {:id "r" :title "T" :fields [{:id "ok" :type :checkbox :label "Go"}]}

         plain
         (hi/form-rows (hi/init-form base) 40)

         blank
         (hi/form-rows (hi/init-form (assoc base :description "   \t  ")) 40)]

        (expect (= plain blank))
        (expect (not-any? #(= :description (:kind %)) blank))))
  (it "paints on every terminal size a split pane or phone can produce"
      ;; Below eleven rows the chrome used to ask for a negative box and Lanterna
      ;; threw, so the dialog took the whole TUI down with it.
      (let
        [form
         (assoc (hi/init-form (assoc (request) :description prose)) :focus 5)

         failures
         (let [{:keys [g]} (virtual-screen)]
           (into []
                 (for
                   [cols (range 1 81 5)
                    rows (range 1 31)
                    :let [failure (try (hi/paint! g cols rows form)
                                       nil
                                       (catch Throwable t [cols rows (.getMessage t)]))]
                    :when failure]

                   failure)))]

        (expect (= [] failures)))))

;; =============================================================================
;; Sliders and the request's own buttons (issue #108)
;; =============================================================================

(defn- slider-request
  "A text field so ↑/↓ has somewhere to come from, then a slider."
  [& {:as slider}]
  (assoc (request)
    :fields [{:id "who" :type :plaintext :label "Who"}
             (merge {:id "pct" :type :range :label "Canary %" :min 0 :max 100 :step 5 :default 25}
                    slider)]))

(defdescribe
  range-field-test
  (it "starts on its declared default and submits a number, not a string"
      (let [form (hi/init-form (slider-request))]
        (expect (= 25 (get-in form [:values "pct"])))
        (expect (= 25 (get (hi/submit-values form) "pct")))))
  (it "←/→ moves one step and the bounds hold"
      (let [form (assoc (hi/init-form (slider-request)) :focus 1)]
        (expect (= 30 (get-in (feed form [{:kind :right}]) [:values "pct"])))
        (expect (= 15 (get-in (feed form [{:kind :left} {:kind :left}]) [:values "pct"])))
        (expect (= 100 (get-in (feed form (repeat 40 {:kind :right})) [:values "pct"])))
        (expect (= 0 (get-in (feed form (repeat 40 {:kind :left})) [:values "pct"])))))
  (it "Home and End jump to the bounds themselves"
      (let [form (assoc (hi/init-form (slider-request)) :focus 1)]
        (expect (= 0 (get-in (feed form [{:kind :home}]) [:values "pct"])))
        (expect (= 100 (get-in (feed form [{:kind :end}]) [:values "pct"])))))
  (it "snaps to a whole number only when the bounds are whole"
      (let
        [decimal (assoc (hi/init-form (slider-request :min 0 :max 1 :step 0.25 :default 0.5))
                   :focus 1)]
        (expect (= 0.75 (get-in (feed decimal [{:kind :right}]) [:values "pct"])))))
  (it "draws a track, the value and the bounds, so the number is never a mystery"
      (let [texts (map #(str (:text %)) (hi/form-rows (hi/init-form (slider-request))))]
        (expect (some #(str/includes? % "●") texts))
        (expect (some #(str/includes? % "25") texts))
        (expect (some #(str/includes? % "(0–100)") texts))))
  (it "offers ←/→ in the hint bar while the slider has focus"
      (let [form (assoc (hi/init-form (slider-request)) :focus 1)]
        (expect (some #{["←/→" "adjust"]} (hi/hint form))))))

;; Regression, issue #108: the request's confirm/cancel labels were painted
;; nowhere and only a bare Enter ended the pause — the operator had to know the
;; chord, and a form with a custom "Ship it" label never showed it at all.
(defdescribe
  action-button-test
  (it "walks onto the buttons after the last field and wraps back to the first"
      (let [form (hi/init-form (slider-request))]
        (expect (= 4 (count (:stops form))))
        (expect (= [:action :action] (mapv :kind (subvec (:stops form) 2))))
        (expect (= 2 (:focus (feed form [{:kind :next} {:kind :next}]))))
        (expect (= 0 (:focus (feed form (repeat 4 {:kind :next})))))
        (expect (= 3 (:focus (feed form [{:kind :prev}]))))))
  (it "Enter and Space press the focused button"
      (let [form (hi/init-form (slider-request))]
        (expect (= :submit (:action (hi/handle-event (assoc form :focus 2) {:kind :enter}))))
        (expect (= :submit (:action (hi/handle-event (assoc form :focus 2) (ch \space)))))
        (expect (= :cancel (:action (hi/handle-event (assoc form :focus 3) {:kind :enter}))))
        (expect (= :cancel (:action (hi/handle-event (assoc form :focus 3) (ch \space)))))))
  (it "wears the request's own labels and drops Cancel when the request forbids it"
      (let
        [custom
         (hi/init-form (assoc (slider-request)
                         :submit-label "Ship it"
                         :cancel-label "Hold"))

         locked
         (hi/init-form (assoc (slider-request) :is-cancellable false))]

        (expect (= ["Ship it" "Hold"] (mapv :label (:buttons (hi/action-bar custom)))))
        (expect (= ["Submit"] (mapv :label (:buttons (hi/action-bar locked)))))
        (expect (= 3 (count (:stops locked))))))
  (it "ranks the caps by FILL, and the ranking never follows the cursor"
      ;; The solid pill used to mean "the cap a chord fires", so walking onto
      ;; Cancel repainted Submit as the quiet action and the form lost its default.
      (let
        [form
         (hi/init-form (request))

         cancel-idx
         (dec (count (:stops form)))

         [submit-bg cancel-bg]
         (cap-bgs form)

         [held-submit-bg held-cancel-bg]
         (cap-bgs (assoc form :focus cancel-idx))]

        ;; Primary vs secondary: two different fills, both solid.
        (expect (not= submit-bg cancel-bg))
        (expect (= submit-bg held-submit-bg))
        (expect (= cancel-bg held-cancel-bg)))))

;; The band is bottom-anchored over exactly the rows the composer occupies, and
;; the prompt — not the composer — owns the keyboard while it is up (issue #108).

(defn- bottom-chrome
  "Paint ONLY the bottom chrome the way the full painter does — composer box,
   echo area, footer — and hand back the screen for inspection."
  [db]
  (let [{:keys [screen g]} (virtual-screen)]
    (#'screen/draw-bottom-chrome!
     screen
     g
     db
     {:input {:lines ["hello draft"] :crow 0 :ccol 11}
      :input-top 24
      :text-rows 1
      :cols 80
      :now-ms 0
      :echo-row 23
      :footer-row 28
      :slash-suggestions nil
      :slash-command-index 0})
    screen))

(defn- idle-db
  "The smallest app-db the bottom chrome reads."
  []
  {:input {:lines ["hello draft"] :crow 0 :ccol 11} :scroll {} :messages []})

(defdescribe
  composer-test
  (it "draws the composer and owns the cursor while nothing is asking"
      (let [screen (bottom-chrome (idle-db))]
        (expect (rule-row? (screen-row screen 24)))
        (expect (str/includes? (screen-row screen 25) "hello draft"))
        (expect (rule-row? (screen-row screen 26)))
        (expect (some? (.getCursorPosition screen)))))
  (it "blanks the composer while a human-input transient is up"
      (let
        [screen (bottom-chrome (assoc (idle-db)
                                 :human-input (hi/init-form
                                                {:id "r"
                                                 :title "Tiny"
                                                 :fields [{:id "a" :type :checkbox :label "Yes"}]
                                                 :is-cancellable true})))]
        (expect (= "" (screen-row screen 24)))
        (expect (= "" (screen-row screen 25)))
        (expect (= "" (screen-row screen 26)))
        (expect (nil? (.getCursorPosition screen)))))
  (it "keeps the footer alive under the transient"
      (let
        [screen (bottom-chrome (assoc (idle-db)
                                 :human-input (hi/init-form
                                                {:id "r"
                                                 :title "Tiny"
                                                 :fields [{:id "a" :type :checkbox :label "Yes"}]
                                                 :is-cancellable true})))]
        (expect (not= "" (screen-row screen 28))))))

;; =============================================================================
;; One-time codes, and errors that arrive ONLY from a confirmation
;; =============================================================================

(defn- otp-request
  "A view built by the ENGINE itself, so these tests pin the real contract and
   not a hand-written guess at it."
  ([] (otp-request 6 6))
  ([lo hi]
   (engine/request->view
     (engine/normalize-request
       {"title" "Confirm the code"
        "fields"
        [{"name" "email" "label" "Email" "is_required" true}
         {"name" "code" "type" "otp" "label" "One-time code" "min_length" lo "max_length" hi}]}))))

(defn- otp-form
  "The OTP request with the cursor already on its boxes."
  ([] (otp-form 6 6))
  ([lo hi] (assoc (hi/init-form (otp-request lo hi)) :focus 1)))

(defn- otp-row [form] (first (filter #(= :otp (:kind %)) (hi/form-rows form 60))))

(defdescribe
  otp-field-test
  (it "draws one box per digit and fills them left to right"
      (expect (= "[ ] [ ] [ ] [ ] [ ] [ ]" (:text (otp-row (otp-form)))))
      (expect (= "[1] [2] [ ] [ ] [ ] [ ]" (:text (otp-row (feed (otp-form) [(ch \1) (ch \2)]))))))
  (it "takes digits only, so a pasted `123-456` arrives as the code"
      ;; A paste reaches the dialog as its characters; dropping everything that
      ;; is not a digit IS the paste handler.
      (let [form (feed (otp-form) (map ch "123-456"))]
        (expect (= "123456" (get-in form [:values "code"])))
        (expect (= "[1] [2] [3] [4] [5] [6]" (:text (otp-row form))))))
  (it "refuses a seventh digit instead of scrolling the boxes"
      (expect (= "123456" (get-in (feed (otp-form) (map ch "1234567")) [:values "code"]))))
  (it "parks the cursor in the box the next digit lands in"
      (expect (= 0 (:cursor (otp-row (otp-form)))))
      (expect (= 2 (:cursor (otp-row (feed (otp-form) [(ch \1) (ch \2)])))))
      (expect (= 1 (:cursor (otp-row (feed (otp-form) [(ch \1) (ch \2) {:kind :backspace}])))))
      ;; Full boxes keep the caret ON the last one — there is no seventh box to
      ;; point at.
      (expect (= 5 (:cursor (otp-row (feed (otp-form) (map ch "123456")))))))
  (it "erases and walks like the text field it is"
      (let [form (feed (otp-form) (map ch "1234"))]
        (expect (= "123" (get-in (feed form [{:kind :backspace}]) [:values "code"])))
        (expect (= "134"
                   (get-in (feed form [{:kind :left} {:kind :left} {:kind :backspace}])
                           [:values "code"])))))
  (it "says so when it accepts a RANGE of lengths"
      ;; Eight empty boxes cannot show that four of them are already enough.
      (expect (= "[ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ]  (4–8 digits)" (:text (otp-row (otp-form 4 8)))))
      (expect (= "[1] [2] [3] [4] [ ] [ ] [ ] [ ]  (4–8 digits)"
                 (:text (otp-row (feed (otp-form 4 8) (map ch "1234")))))))
  (it "offers the digits in the hint bar"
      (expect (some #{["0–9" "fill"]} (hi/hint (otp-form))))
      (expect (nil? (some #{["0–9" "fill"]} (hi/hint (hi/init-form (otp-request))))))))

(defdescribe
  confirm-then-clear-test
  (it "a PRISTINE form shows nothing and still sends"
      ;; Not one rule lives in the terminal: the view carries no validators, so
      ;; an empty required field is confirmed and the ENGINE is what refuses it.
      ;; A form that reddens before anybody confirmed anything is the thing
      ;; every form library exists to prevent.
      (let [form (hi/init-form (otp-request))]
        (expect (= {} (:errors form)))
        (expect (nil? (some #{:error} (map :kind (hi/form-rows form 60)))))
        (expect (= :submit (:action (hi/handle-event form {:kind :submit}))))))
  (it "typing never validates, however wrong the value is"
      (let [form (feed (otp-form) (map ch "123"))]
        (expect (= {} (:errors form)))
        (expect (nil? (some #{:error} (map :kind (hi/form-rows form 60)))))
        (expect (= :submit (:action (hi/handle-event form {:kind :submit}))))))
  (it "the engine's refusal is the only thing that reddens a field"
      (let
        [errors
         {"email" "is required" "code" "must be 6 digits"}

         form
         (hi/set-errors (feed (otp-form) (map ch "123")) errors)]

        (expect (= errors (:errors form)))
        ;; The cursor jumps back to the email, the earliest thing that is wrong.
        (expect (= 0 (:focus form)))
        (expect (some #{"is required"} (map :text (hi/form-rows form 60))))))
  (it "walking between fields neither clears a message nor invents one"
      (let [form (hi/set-errors (hi/init-form (otp-request)) {"email" "that address bounced"})]
        (expect (= {"email" "that address bounced"}
                   (:errors (feed form [{:kind :next} {:kind :prev}]))))))
  (it "the first keystroke clears THAT field's message and no other"
      (let
        [form
         (hi/set-errors (hi/init-form (otp-request))
                        {"email" "that address bounced" "code" "must be 6 digits"})

         typed
         (feed form [(ch \a)])]

        (expect (= {"code" "must be 6 digits"} (:errors typed)))
        (expect (nil? (some #{"that address bounced"} (map :text (hi/form-rows typed 60)))))
        ;; Erasing is a touch as much as typing is.
        (expect (= {"code" "must be 6 digits"} (:errors (feed typed [{:kind :backspace}]))))))
  (it "toggling, picking and nudging are touches too"
      (let
        [request
         (engine/request->view
           (engine/normalize-request
             {"title" "Ship"
              "fields" [{"name" "ok" "type" "checkbox" "label" "Confirm"}
                        {"name" "env" "type" "select" "label" "Env" "options" ["prod" "stg"]}
                        {"name" "risk" "type" "range" "label" "Risk" "min" 0 "max" 10 "step" 1}]}))

         errors
         {"ok" "must be checked" "env" "is required" "risk" "too much"}

         form
         (hi/set-errors (hi/init-form request) errors)

         stop-at
         (fn [kind]
           (first (keep-indexed (fn [i s]
                                  (when (= kind (:kind s)) i))
                                (:stops form))))

         touch
         (fn [kind events]
           (:errors (feed (assoc form :focus (stop-at kind)) events)))]

        (expect (= errors (:errors form)))
        (expect (= (dissoc errors "ok") (touch :checkbox [(ch \space)])))
        (expect (= (dissoc errors "env") (touch :select-option [(ch \space)])))
        (expect (= (dissoc errors "risk") (touch :range [{:kind :right}])))))
  (it "the next confirmation asks the engine all over again"
      ;; The form never decides for itself that the value is fixed now: it drops
      ;; the stale message and sends the whole answer back for a fresh verdict.
      (let
        [retyped
         (feed (hi/set-errors (hi/init-form (otp-request)) {"email" "that address bounced"})
               (map ch "ops@example.com"))

         {:keys [form action]}
         (hi/handle-event retyped {:kind :submit})]

        (expect (= {} (:errors retyped)))
        (expect (= :submit action))
        (expect (= "ops@example.com" (get (hi/submit-values form) "email"))))))

;; =============================================================================
;; Layout groups
;; =============================================================================

(defn- grouped-request
  "A request whose first field is a LAYOUT GROUP, built by the engine so these
   tests pin the real wire contract and not a hand-written guess at it."
  [group]
  (engine/request->view (engine/normalize-request
                          {"title" "Connect" "fields" [group {"name" "notes" "label" "Notes"}]})))

(defn- server-group
  "Two fields that belong together, laid out in `direction`."
  [direction]
  {"type" "group"
   "direction" direction
   "label" "Server"
   "fields" [{"name" "host" "label" "Host" "is_required" true "placeholder" "db.internal"}
             {"name" "port" "label" "Port" "placeholder" "5432"}]})

(defn- ink
  "Every non-blank painted row of `form`, framing stripped."
  [form]
  (into [] (remove str/blank?) (painted-rows form "")))

(defn- caret
  "Where `paint!` parks the terminal caret: `[x y]`."
  [form]
  (let
    [{:keys [g]}
     (virtual-screen)

     pos
     (hi/paint! g 80 30 form)]

    [(.getColumn ^TerminalPosition pos) (.getRow ^TerminalPosition pos)]))

(defdescribe
  group-layout-test
  (it "lays a `row` group side by side: two labels on one line, two inputs on the next"
      (let [rows (ink (hi/init-form (grouped-request (server-group "row"))))]
        (expect (some #(and (str/includes? % "Host") (str/includes? % "Port")) rows))
        (expect (some #(and (str/includes? % "db.internal") (str/includes? % "5432")) rows))
        ;; Grouping is layout only — the field below the group is untouched.
        (expect (some #(= "Notes" %) rows))))
  (it "stacks a `column` group instead, one field per line"
      (let [rows (ink (hi/init-form (grouped-request (server-group "column"))))]
        (expect (nil? (some #(and (str/includes? % "Host") (str/includes? % "Port")) rows)))
        (expect (some #(str/includes? % "Host") rows))
        (expect (some #(str/includes? % "Port") rows))))
  (it "gives the group its own heading, above the fields it owns"
      (let
        [rows (ink (hi/init-form (grouped-request (assoc (server-group "row")
                                                    "description" "Where to connect"))))]
        (expect (< (long (row-index rows #(= "Server" %)))
                   (long (row-index rows #(str/includes? % "Where to connect")))
                   (long (row-index rows #(str/includes? % "Host")))))))
  (it "keeps every LEAF a stop of its own, in reading order — a group is not one"
      (let [form (hi/init-form (grouped-request (server-group "row")))]
        (expect (= ["host" "port" "notes"] (into [] (keep :field-id) (:stops form))))
        (expect (= {"host" "" "port" "" "notes" ""} (:values form)))))
  (it "composes: a `column` group nested inside a `row` group is one of its columns"
      ;; Two directions and no third rule — the tree does the rest.
      (let
        [rows (ink (hi/init-form (grouped-request {"type" "group"
                                                   "direction" "row"
                                                   "fields" [{"type" "group"
                                                              "direction" "column"
                                                              "label" "Left"
                                                              "fields" [{"name" "a" "label" "A"}
                                                                        {"name" "b" "label" "B"}]}
                                                             {"name" "c" "label" "C"}]})))]
        ;; The nested column's heading shares its line with the neighbour column,
        ;; and B — the second row of that column — is BELOW A, not beside it.
        (expect (some #(and (str/includes? % "Left") (str/includes? % "C")) rows))
        (expect (< (long (row-index rows #(str/includes? % "A")))
                   (long (row-index rows #(str/includes? % "B")))))))
  (it "moves the caret ACROSS a shared row, not down it"
      (let
        [form
         (hi/init-form (grouped-request (server-group "row")))

         [x0 y0]
         (caret (assoc form :focus 0))

         [x1 y1]
         (caret (assoc form :focus 1))

         [_ y2]
         (caret (assoc form :focus 2))]

        (expect (= y0 y1))
        (expect (< (long x0) (long x1)))
        (expect (< (long y1) (long y2)))))
  (it "types into the focused column only"
      (let
        [form (feed (assoc (hi/init-form (grouped-request (server-group "row"))) :focus 1)
                    (map ch "5433"))]
        (expect (= {"host" "" "port" "5433" "notes" ""} (:values form)))
        (expect (some #(str/includes? % "5433") (ink form)))))
  (it "prints a grouped field's error inside its own column"
      (let
        [form (hi/set-errors (hi/init-form (grouped-request (server-group "row")))
                             {"host" "is required"})]
        (expect (= {"host" "is required"} (:errors form)))
        (expect (some #(str/includes? % "is required") (ink form)))))
  (it "answers with one flat map of leaves, whatever the layout"
      (let
        [view
         (grouped-request (server-group "row"))

         form
         (feed (hi/init-form view) (map ch "db1"))]

        (expect (= {"host" "db1" "port" "" "notes" ""}
                   (:values (:form (hi/handle-event form {:kind :submit})))))
        (expect (= :submit (:action (hi/handle-event form {:kind :submit}))))))
  (it "focuses ONE option when a row group puts two choice fields side by side"
      ;; Every row resolves its stop through the plan's index, so the second
      ;; column's options stay its own: the same ordinal must never light up in
      ;; both columns.
      (let
        [form
         (hi/init-form (grouped-request
                         {"type" "group"
                          "direction" "row"
                          "fields" [{"name" "env" "type" "select" "options" ["dev" "prod"]}
                                    {"name" "tier" "type" "select" "options" ["free" "paid"]}]}))

         focused
         (fn [focus]
           (into []
                 (comp (mapcat #(if (= :columns (:kind %)) (:cells %) [%]))
                       (filter :is-focused)
                       (map :text))
                 (hi/form-rows (assoc form :focus focus) 60)))]

        (expect (= ["dev" "prod" "free" "paid"] (into [] (keep :value) (:stops form))))
        (expect (= [["dev"] ["prod"] ["free"] ["paid"]] (mapv focused (range 4)))))))
