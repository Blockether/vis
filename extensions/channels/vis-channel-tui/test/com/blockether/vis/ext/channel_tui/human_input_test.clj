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
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna SGR TerminalSize TextCharacter]
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
                  {:kind :text :field-id "note"}]
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
                   (expect (= 7 (:focus (feed form [{:kind :prev}]))))))
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
                   [form
                    (feed (hi/init-form (request)) [(ch \a)])

                    {:keys [action]}
                    (hi/handle-event form {:kind :enter})]

                   (expect (= :submit action))
                   (expect (= {"user" "a" "pass" "" "env" "dev" "tags" [] "ok" false "note" ""}
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
  (it "carries the typed character through"
      (expect (= \q
                 (:char (hi/key->event (KeyStroke. (Character/valueOf \q) false false false)))))))

(defdescribe hint-test
             (it "offers Enter as submit outside a multiline field"
                 (let [pairs (hi/hint (hi/init-form (request)))]
                   (expect (some #{["Enter" "submit"]} pairs))
                   (expect (some #{["Esc" "cancel"]} pairs))))
             (it "swaps Enter for a newline chord inside a multiline field"
                 (let [pairs (hi/hint (assoc (hi/init-form (request)) :focus 7))]
                   (expect (some #{["Enter" "newline"]} pairs))
                   (expect (some #{["^S" "submit"]} pairs))))
             (it "drops the cancel chord when the request forbids cancelling"
                 (let [pairs (hi/hint (hi/init-form (assoc (request) :is-cancellable false)))]
                   (expect (not-any? #(= "Esc" (first %)) pairs)))))

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
                   ;; The cursor sits one column past the typed text.
                   (expect (= \space (.charAt row (.getColumn pos))))))
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
          (doseq [key [(stroke \b) (stroke \o) (KeyStroke. KeyType/Enter)]]
            (#'screen/human-input-key! @state/app-db key))
          (expect (= "req-1" (first @submitted)))
          (expect (= "bo" (get-in @submitted [1 "user"])))
          (expect (nil? (:human-input @state/app-db))))))
  (it "keeps the dialog open and shows the engine's errors on a rejected answer"
      (with-redefs
        [vis/submit-human-input! (fn [_ _]
                                   {:is-accepted false :errors {"pass" "Password is required"}})]
        (reset! state/app-db {:render-version 0})
        (state/dispatch [:human-input-open (hi/init-form (request))])
        (#'screen/human-input-key! @state/app-db (KeyStroke. KeyType/Enter))
        (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
        (expect (= {"pass" "Password is required"} (get-in @state/app-db [:human-input :errors])))))
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
  (it "spells its hint bar the canonical way — ↑/↓ chords, lowercase actions"
      (let
        [{:keys [screen g]}
         (virtual-screen)

         _
         (hi/paint! g 80 30 (hi/init-form (request)))

         text
         (screen-text screen)]

        (expect (str/includes? text "↑/↓ move"))
        (expect (str/includes? text "Enter submit"))
        (expect (str/includes? text "Esc cancel"))))
  (it "puts a scrollbar thumb in the gutter once the form outgrows the box"
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 30 (assoc (hi/init-form (request)) :focus 7))
        (expect (str/includes? (screen-text screen) "█"))))
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
