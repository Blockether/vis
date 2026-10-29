(ns com.blockether.vis.tui.human-input-test
  "Pure-model and paint tests for the TUI half of the human-input dialog.

   The model is a plain map, so navigation, editing, toggling and the
   submit/cancel decisions are asserted without a terminal. Painting is
   asserted against a Lanterna virtual terminal back-buffer, the same
   harness `dialogs-test` uses."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.transient :as tr]
            [com.blockether.vis.tui.view-model :as engine]
            [com.blockether.vis.contract.view :as view-spec]
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
   :nodes
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
   :timeout-ms 300000
   :session-id "session-1"})

(defn- canonical-input-view
  "Turn a compact declaration into the canonical gateway View used by TUI tests."
  [wire]
  (letfn
    [(node [raw]
       (let [value
             (engine/view<-wire raw)

             type
             (or (:type value) :plaintext)]

         (cond (= view-spec/group-type type) (let [children
                                                   (mapv node (:children value))

                                                   id
                                                   (or (:id value)
                                                       (:name value)
                                                       (str "group:"
                                                            (str/join "+" (keep :name children))))]

                                               (assoc value
                                                 :id id
                                                 :name id
                                                 :type type
                                                 :direction (or (:direction value) :column)
                                                 :children children))
               (view-spec/decoration? (assoc value :type type)) (assoc value :type type)
               :else (let [id (or (:id value) (:name value))]
                       (cond-> (assoc value
                                 :id id
                                 :name id
                                 :type type
                                 :label (or (:label value) id)
                                 :is-required (boolean (:is-required value))
                                 :is-secret (contains? view-spec/secret-types type))
                         (contains? view-spec/choice-types type)
                         (update :options
                                 #(mapv (fn [option]
                                          (if (map? option)
                                            (merge {:label (str (:value option))} option)
                                            {:value option :label (str option)}))
                                        %))

                         (= :otp type)
                         (assoc :min-length
                           (or (:min-length value) (:length view-spec/otp-defaults)) :max-length
                           (or (:max-length value) (:length view-spec/otp-defaults))))))))]
    (let [value (engine/view<-wire wire)]
      (-> value
          (assoc :id (or (:id value) "request-fixture")
                 :submit-label (or (:submit-label value) "Submit")
                 :cancel-label (or (:cancel-label value) "Cancel")
                 :is-cancellable (get value :is-cancellable true)
                 :timeout-ms (or (:timeout-ms value) 300000)
                 :session-id (or (:session-id value) "session-1"))
          (update :nodes #(mapv node %))))))

(defn- feed
  "Apply normalized events in order, keeping only the form."
  [form events]
  (reduce (fn [acc event]
            (:form (hi/handle-event acc event)))
          form
          events))

(defn- ch "A printable character event." [c] {:kind :char :char c})

(defn- virtual-screen
  "A started virtual screen plus its text graphics — 80 columns, and 30 rows
   unless a taller probe is asked for."
  ([] (virtual-screen 30))
  ([rows]
   (Thread/interrupted)
   (let [terminal
         (DefaultVirtualTerminal. (TerminalSize. 80 (int rows)))

         screen
         (TerminalScreen. terminal)]

     (.startScreen screen)
     {:screen screen :g (.newTextGraphics screen)})))

(def ^:private probe-rows
  "Rows the VOCABULARY probes paint into. These tests ask what a row is MADE of,
   never how many of them fit, so the probe screen is tall enough to hold the
   whole sample form — the band's capacity and its scrolling are pinned by
   `band-test` instead."
  32)

(defn- screen-row
  "The rendered characters of one back-buffer row, right-trimmed."
  [^TerminalScreen screen y]
  (str/trimr (apply str
               (for [x (range 80)]
                 (.getCharacterString (.getBackCharacter screen (int x) (int y)))))))

(defn- char-at
  "The character the back buffer holds at `x`,`y`."
  [^TerminalScreen screen ^long x ^long y]
  (.getCharacterString (.getBackCharacter screen (int x) (int y))))

(defn- screen-text
  "The whole back buffer as one string."
  [screen]
  (str/join "\n" (map #(screen-row screen %) (range 30))))

(defdescribe
  init-form-test
  (it "honors declared defaults, coercing each to its field type"
      (let [form (hi/init-form {:id "r"
                                :title "T"
                                :nodes [{:id "a" :type :plaintext :label "A" :default 7}
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
      (let [form
            (feed (hi/init-form (request)) [{:kind :end} (ch \x) {:kind :enter} (ch \y)])

            multi
            (assoc form :focus 7)

            typed
            (feed multi [(ch \x) {:kind :enter} (ch \y)])]

        (expect (= "x\ny" (get-in typed [:values "note"])))
        (expect (nil? (:action (hi/handle-event typed {:kind :enter}))))
        (expect (= :submit (:action (hi/handle-event typed {:kind :submit}))))))
  (it "keeps a password out of the paint plan"
      (let [form
            (feed (hi/init-form (assoc (request)
                                  :nodes [{:id "pass" :type :password :label "Password"}]))
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
                 (let [form (feed (hi/init-form (request))
                                  [{:kind :next} {:kind :next} {:kind :next} (ch \space)])]
                   (expect (= "prod" (get-in form [:values "env"])))))
             (it "Space adds and removes multiselect values in declared order"
                 (let [base
                       (assoc (hi/init-form (request)) :focus 5)

                       one
                       (feed base [(ch \space)])

                       both
                       (feed one [{:kind :prev} (ch \space)])

                       back
                       (feed both [(ch \space)])]

                   (expect (= ["b"] (get-in one [:values "tags"])))
                   (expect (= ["a" "b"] (get-in both [:values "tags"])))
                   (expect (= ["b"] (get-in back [:values "tags"]))))))

(defdescribe submit-and-cancel-test
             (it "Enter asks for submit and hands back values keyed by field id"
                 (let [pristine
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
                 (let [form
                       (hi/init-form (request))

                       locked
                       (hi/init-form (assoc (request) :is-cancellable false))]

                   (expect (= :cancel (:action (hi/handle-event form {:kind :cancel}))))
                   (expect (nil? (:action (hi/handle-event locked {:kind :cancel}))))))
             (it "engine rejections attach to their field and move the cursor there"
                 (let [form (-> (hi/init-form (request))
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
  (it "canonical C-g cancels the form, exactly like Esc"
      (expect (= :cancel
                 (:kind (hi/key->event (KeyStroke. (Character/valueOf \g) true false false)))))
      (expect (= :char
                 (:kind (hi/key->event (KeyStroke. (Character/valueOf \g) false false false)))))))

(defdescribe paint-test
             (it "places the terminal cursor inside the focused text field"
                 (let [{:keys [screen g]}
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
                 (let [{:keys [screen g]}
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
                 (let [{:keys [screen g]}
                       (virtual-screen)

                       form
                       (assoc (hi/init-form (request)) :focus 7)

                       _
                       ;; 20 rows, not 30: the whole form FITS in a 30-row terminal, so
                       ;; nothing scrolls there and this assertion only ever proved that
                       ;; the label it looked for was spelled differently.
                       (hi/paint! g 80 20 form)

                       text
                       (screen-text screen)]

                   (expect (str/includes? text "Note"))
                   (expect (not (str/includes? text "User *")))))
             (it "renders an engine rejection next to its field"
                 (let [{:keys [screen g]}
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
      (reset! state/app-db {:session {:id "session-1"} :render-version 0})
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
      (reset! state/app-db {:session {:id "session-1"} :render-version 0})
      (state/dispatch [:human-input-open (hi/init-form (request))])
      (state/dispatch [:human-input-open (hi/init-form (assoc (request) :id "req-2"))])
      (state/dispatch [:human-input-close "req-2"])
      (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
      (expect (empty? (:human-input-queue @state/app-db))))
  (it "opens and closes the dialog straight from channel events"
      (reset! state/app-db {:session {:id "session-1"} :render-version 0})
      (#'screen/handle-channel-event! {:op :view/open :kind :input :view (request)})
      (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
      ;; An open dialog owns the screen: render fast paths off, cursor is ours.
      (expect (true? (#'screen/overlay-locked? @state/app-db)))
      (#'screen/handle-channel-event! {:op :view/close :kind :input :view-id "req-1"})
      (expect (nil? (:human-input @state/app-db)))
      (expect (false? (#'screen/overlay-locked? @state/app-db))))
  (it "submits the typed values and closes once the engine accepts them"
      (let [submitted (atom nil)]
        (with-redefs [vis/gateway-view-action!
                      (fn [sid id action]
                        (reset! submitted [sid id action])
                        {:action (:action action) :view-id id :is-accepted true})]
          (reset! state/app-db {:session {:id "session-1"} :render-version 0})
          (state/dispatch [:human-input-open (hi/init-form (request))])
          (doseq [key [(stroke \b) (stroke \o) (KeyStroke. KeyType/ArrowDown) (stroke \p)
                       (KeyStroke. KeyType/Enter)]]
            (#'screen/human-input-key! @state/app-db key))
          (expect (= "session-1" (first @submitted)))
          (expect (= "req-1" (second @submitted)))
          (expect (= :submit (get-in @submitted [2 :action])))
          (expect (= "bo" (get-in @submitted [2 :values "user"])))
          (expect (= "p" (get-in @submitted [2 :values "pass"])))
          (expect (nil? (:human-input @state/app-db))))))
  (it "keeps the dialog open and shows the engine's errors on a rejected answer"
      (with-redefs [vis/gateway-view-action!
                    (fn [_sid _id action]
                      (expect (= :submit (:action action)))
                      {:action :submit :is-accepted false :errors {"pass" "Password is expired"}})]
        (reset! state/app-db {:session {:id "session-1"} :render-version 0})
        (state/dispatch [:human-input-open (hi/init-form (request))])
        ;; Both required fields are filled, so the form's OWN rules pass and the
        ;; answer actually reaches the engine — which is the one refusing it here.
        (doseq [key [(stroke \b) (KeyStroke. KeyType/ArrowDown) (stroke \p)
                     (KeyStroke. KeyType/Enter)]]
          (#'screen/human-input-key! @state/app-db key))
        (expect (= "req-1" (get-in @state/app-db [:human-input :request :id])))
        (expect (= {"pass" "Password is expired"} (get-in @state/app-db [:human-input :errors])))))
  (it "cancels the pending request on Escape"
      (let [cancelled (atom nil)]
        (with-redefs [vis/gateway-view-action! (fn [sid id action]
                                                 (reset! cancelled [sid id action])
                                                 {:action :cancel :view-id id :is-accepted true})]
          (reset! state/app-db {:session {:id "session-1"} :render-version 0})
          (state/dispatch [:human-input-open (hi/init-form (request))])
          (#'screen/human-input-key! @state/app-db (KeyStroke. KeyType/Escape))
          (expect (= ["session-1" "req-1" {:action :cancel}] @cancelled))
          (expect (nil? (:human-input @state/app-db)))))))

(defn- painted-rows
  "Every painted human-input row containing `needle`, with the dialog frame and
   padding stripped, so a row compares directly against a canonical painter."
  [form needle]
  (let [{:keys [screen g]}
        (virtual-screen probe-rows)

        _
        (hi/paint! g 80 probe-rows form)]

    (into []
          (keep (fn [y]
                  (let [row (str/trim (str/replace (screen-row screen y) "│" " "))]
                    (when (str/includes? row needle) row))))
          (range probe-rows))))

(defdescribe
  canonical-presentation-test
  "Cross-validation against the canonical TUI dialog vocabulary: the same row
   painters, the same hint-bar spelling, the same scrollbar gutter."
  (it "puts a scrollbar thumb in the gutter once the form outgrows the box"
      ;; The band grows UPWARD over the transcript, so on a roomy terminal this
      ;; form fits whole; it is a SHORT terminal that squeezes it into a window.
      (let [{:keys [screen g]} (virtual-screen)]
        (hi/paint! g 80 16 (assoc (hi/init-form (request)) :focus 7))
        (expect (str/includes? (screen-text screen) "█")))))

(defn- rule-row?
  "True when a painted row is one of the band's horizontal rules and NOTHING
   else. The box's own corner caps are part of a rule; a `├`/`┤` junction never
   is — the session frame the band lies on is sideless, so the band brings its
   own box instead of joining rails that do not exist."
  [s]
  (and (str/includes? s "───") (every? #{\space \─ \┌ \┐ \└ \┘ \│} s)))

(defdescribe
  band-test
  ;; The human-input prompt is a TRANSIENT band inside the session, not a
  ;; full-screen modal: it sits ABOVE the prompt box, grows upward over the
  ;; transcript, and leaves the composer and the session's own footer visible.
  ;;
  ;; Regression, issue #147: the band was anchored ON the prompt — it covered the
  ;; input box and painted on the very same dialog paper, so a transient (drafts,
  ;; C-x, a HITL form) hid what was being typed and did not read as a layer at
  ;; all.
  (it "never covers the prompt box, and takes no tint of its own"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g 80 30 (hi/init-form (request)))

            prompt-top
            (- 30 (long tr/prompt-rows) 3)]

        ;; Every row from the prompt's opening rule down belongs to the session.
        (expect (every? #(= "" (screen-row screen %)) (range (inc prompt-top) 30)))))
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
  ;; Regression, issue #108: a form taller than the band scrolled its own
  ;; the `Submit` / `Cancel` caps out of sight, so the only visible way to end
  ;; the pause was to guess a key.
  (it "pins the action bar above the fenced footer, however tall the form"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g 80 30 (hi/init-form (request)))

            close-y
            (long (:hint-row (tr/band-region 80 30 1)))

            bar
            (screen-row screen (- close-y 3))]

        ;; The box closes on the echo row, its hint bar rides one row above it,
        ;; and the rule that fences the footer off sits above that — so the caps
        ;; are the last row of BODY, whatever the form does.
        (expect (rule-row? (screen-row screen close-y)))
        (expect (rule-row? (screen-row screen (- close-y 2))))
        ;; Ink pill for Submit, muted pill for Cancel — two filled caps, no rails.
        (expect (str/includes? bar " Submit "))
        (expect (str/includes? bar " Cancel"))))
  ;; Regression: the band framed the form with no margin of its own — a focused
  ;; row's `▎` ring was painted in the column against the rail, and the pinned
  ;; caps grew straight out of whatever field the body ended on.
  (it
    "insets the body off both rails and keeps a blank row above the pinned caps"
    (let [{:keys [screen g]}
          (virtual-screen)

          _
          (hi/paint! g 80 30 (assoc (hi/init-form (request)) :focus 2))

          {:keys [left inner-w]}
          (tr/band-region 80 30 1)

          left
          (long left)

          right
          (+ left (long inner-w) 1)

          rows
          (mapv #(screen-row screen %) (range 30))

          at
          (fn [pred]
            (long (first (keep-indexed (fn [y r]
                                         (when (pred r) y))
                                       rows))))

          inner
          (fn [y]
            (str/join (map #(char-at screen % y) (range (inc left) right))))

          ring-y
          (at #(str/includes? % "┃"))

          caps-y
          (at #(str/includes? % " Submit "))]

      ;; The rail, then a clear column, and only then the ring a focused row
      ;; wears — the ring is the LAST of the lead, never painted on the box.
      (expect (= "│" (char-at screen left ring-y)))
      (expect (= " " (char-at screen (inc left) ring-y)))
      (expect (= "┃" (char-at screen (+ left 2) ring-y)))
      ;; The same lead answers on the right: the column against the rail is the
      ;; scrollbar's own gutter, so it holds the shared bar or nothing at all —
      ;; never a row's surface.
      (expect (every? #(contains? #{" " "│" "█"} (char-at screen (dec right) %))
                      (range ring-y caps-y)))
      ;; The caps stand off the body on a row of the band's own paper.
      (expect (str/blank? (inner (dec caps-y))))
      (expect (not (str/blank? (inner caps-y))))))
  ;; Regression: the scrollbar's thumb was painted ON the band's right rail, so
  ;; the border itself carried a moving glyph and the bar had no gutter of its
  ;; own — every other scrollable dialog draws it one column INSIDE the frame.
  (it "draws the scrollbar in the gutter lane inside the right rail, never on it"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g 80 16 (assoc (hi/init-form (request)) :focus 7))

            {:keys [left inner-w]}
            (tr/band-region 80 16 1)

            lane
            (+ (long left) (long inner-w))

            rail
            (inc (long lane))

            ys
            (range 16)

            thumb-y
            (long (first (filter #(= "█" (char-at screen lane %)) ys)))]

        ;; The thumb rides in the lane, one column inside the frame …
        (expect (= "█" (char-at screen lane thumb-y)))
        ;; … the rail beside it is plain border on that very row …
        (expect (= "│" (char-at screen rail thumb-y)))
        ;; … and neither rail ever wears a thumb of its own.
        (expect (every? #(not= "█" (char-at screen rail %)) ys))
        (expect (every? #(not= "█" (char-at screen (long left) %)) ys))))
  ;; The transient keeps its side margins above the edge-to-edge composer.
  (it "keeps the band's inset rails within the full-width prompt rules"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g 80 30 (hi/init-form (request)))

            ;; The prompt the band sits above, painted by the session's own painter.
            _
            (render/draw-input-box! g {:lines [""] :crow 0 :ccol 0} 26 1 80 nil)

            {:keys [left inner-w]}
            (tr/band-region 80 30 1)

            left
            (long left)

            right
            (+ left (long inner-w) 1)

            rule-cols
            (filterv #(= "─" (char-at screen % 26)) (range 80))

            top
            (long (first (filter #(= "┌" (char-at screen left %)) (range 30))))]

        (expect (= (vec (range 80)) rule-cols))
        (expect (< 0 left right 79))
        ;; ...and the box really is drawn there, with terminal margin outside it.
        (expect (= "┐" (char-at screen right top)))
        (expect (every? #(= " " (char-at screen % top)) (range 0 left)))
        (expect (every? #(= " " (char-at screen % top)) (range (inc right) 80)))))
  (it "anchors the band on the prompt's closing rule at any height"
      ;; PURE: the band sits ABOVE the prompt, so its hint row is
      ;; `rows - prompt-h - 3` — never on top of the input box.
      (expect (= 24 (:hint-row (tr/band-region 80 30 1))))
      (expect (= 34 (:hint-row (tr/band-region 80 40 1))))
      ;; The left rail IS the prompt rule's own first column.
      (expect (= 2 (:left (tr/band-region 80 30 1))))
      ;; ...unless the transcript's top would be crossed, which wins.
      (expect (= 12 (:min-row (tr/band-region 80 30 12))))))

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
  (it "marks every field the engine will refuse with a `*` after its label"
      ;; The engine rejects a submission that leaves one of these blank, so the
      ;; dialog names them BEFORE the operator hits enter — with the web's own mark.
      ;; Spelling `REQUIRED` out beside every label said the same word down the whole
      ;; form and shoved the labels apart.
      (let [rows
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
                             :nodes
                             [{:id "ok" :type :checkbox :label "Confirm" :is-required true}]}))]

        (expect (= "User *" (label-of "User")))
        (expect (= "Password *" (label-of "Password")))
        (expect (= "Env" (label-of "Env")))
        ;; A checkbox carries its own label, so the marker rides the box itself.
        (expect (= "Confirm *" (:text (first checkbox-rows))))
        ;; The row also SAYS it is required, which is what lets the painter ink
        ;; exactly that cell red instead of guessing at a trailing glyph.
        (expect (true? (:is-required (first checkbox-rows)))))))

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

(def ^:private markdown-description
  "**Environment:** DEV01\n**Service:** glms-plcx-svc\n**Command:** :init-batch-report")

(defdescribe
  markdown-description-test
  ;; Regression #282: exercise the wire request through layout and terminal cells.
  (it "keeps three authored rows and bold runs at every unwrapped width"
      (let [form (hi/init-form
                   (hi/request<-wire
                     {:id "r" :title "Confirm" :description markdown-description :nodes []}))]
        (doseq [width [nil 80]]
          (let [rows (take-while #(= :description (:kind %)) (hi/form-rows form width))]
            (expect (= ["Environment: DEV01" "Service: glms-plcx-svc" "Command: :init-batch-report"]
                       (mapv :text rows)))
            (expect (= ["Environment:" "Service:" "Command:"]
                       (mapv #(-> %
                                  :runs
                                  first
                                  :text)
                             rows)))
            (expect (every? #(contains? (-> %
                                            :runs
                                            first
                                            :style)
                                        :bold)
                            rows))))))
  (it "paints bold labels without markers or bold leaking into their values"
      (let [{:keys [screen g]} (virtual-screen)]
        (try (hi/paint! g
                        80
                        30
                        (hi/init-form
                          {:id "r" :title "Confirm" :description markdown-description :nodes []}))
             (let [ys (mapv #(screen-row-of screen %) ["Environment:" "Service:" "Command:"])]
               (expect (every? some? ys))
               (expect (= [1 1] (mapv - (rest ys) ys))))
             (doseq [label ["Environment:" "Service:" "Command:"]]
               (expect (contains? (modifiers-of screen label) SGR/BOLD)))
             (doseq [value ["DEV01" "glms-plcx-svc" ":init-batch-report"]]
               (expect (not (contains? (modifiers-of screen value) SGR/BOLD))))
             (expect (not (str/includes? (screen-text screen) "**")))
             (finally (.close ^TerminalScreen screen)))))
  (it "wraps Markdown without dropping characters or styles"
      (let [rows (take-while #(= :description (:kind %))
                             (hi/form-rows (hi/init-form {:description "**abcdefghijklmnop**\nlast"
                                                          :nodes []})
                                           8))]
        (expect (= "abcdefghijklmnoplast" (apply str (map :text rows))))
        (expect (= "last" (:text (last rows))))
        (expect (every? #(<= (count (:text %)) 8) rows))
        (expect (every? #(contains? (:style %) :bold) (mapcat :runs (butlast rows))))))
  (it "keeps raw HTML literal rather than treating it as formatting"
      (let [{:keys [screen g]} (virtual-screen)]
        (try (hi/paint! g
                        80
                        30
                        (hi/init-form
                          {:title "Confirm" :nodes [] :description "**Safe** <b>literal</b>"}))
             (expect (contains? (modifiers-of screen "Safe") SGR/BOLD))
             (expect (some? (screen-row-of screen "<b>literal</b>")))
             (expect (not (contains? (modifiers-of screen "literal") SGR/BOLD)))
             (finally (.close ^TerminalScreen screen))))))

(defdescribe
  wrapped-description-test
  "A description is a SENTENCE, not a token. Ellipsizing it into one row throws
   away exactly the half that explained the ask, so prose wraps onto as many
   rows as it needs — the dialog's own description and every field's."
  (it "wraps the dialog's own description onto as many rows as it needs"
      (let [rows
            (hi/form-rows (hi/init-form {:id "r"
                                         :title "Deploy"
                                         :description prose
                                         :nodes [{:id "env" :type :plaintext :label "Env"}]})
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
      (let [rows
            (hi/form-rows (hi/init-form
                            {:id "r"
                             :title "Deploy"
                             :nodes [{:id "env" :type :plaintext :label "Env" :description prose}]})
                          40)

            i
            (long (row-index rows #(= :label (:kind %))))

            desc
            (vec (take-while #(= :description (:kind %)) (drop (+ i 2) rows)))]

        (expect (= :blank (:kind (nth rows (inc i)))))
        (expect (< 1 (count desc)))
        (expect (= (str/split prose #"\s+") (prose-words desc)))
        ;; Then one row of air, and only then the box.
        (expect (= :blank (:kind (nth rows (+ i 2 (count desc))))))
        (expect (= :input (:kind (nth rows (+ i 3 (count desc))))))))
  (it "paints the whole description instead of clipping it at the border"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g
                       80
                       30
                       (hi/init-form {:id "r"
                                      :title "Deploy"
                                      :description prose
                                      :nodes [{:id "env" :type :plaintext :label "Env"}]}))]

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
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g
                       80
                       30
                       (hi/init-form {:id "r"
                                      :title "Fetch"
                                      :description url
                                      :nodes [{:id "ok" :type :checkbox :label "Go"}]}))

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
                                         :nodes [{:id "env" :type :plaintext :label "Env"}]})
                     :focus 0))
        (expect (not (str/includes? (screen-text screen) "…")))))
  (it "renders nothing at all for a description that is only whitespace"
      ;; A blank string is not a sentence: it must cost zero rows, not a gap that
      ;; pushes the first field out of view.
      (let [base
            {:id "r" :title "T" :nodes [{:id "ok" :type :checkbox :label "Go"}]}

            plain
            (hi/form-rows (hi/init-form base) 40)

            blank
            (hi/form-rows (hi/init-form (assoc base :description "   \t  ")) 40)]

        (expect (= plain blank))
        (expect (not-any? #(= :description (:kind %)) blank))))
  (it "paints on every terminal size a split pane or phone can produce"
      ;; Below eleven rows the chrome used to ask for a negative box and Lanterna
      ;; threw, so the dialog took the whole TUI down with it.
      (let [form
            (assoc (hi/init-form (assoc (request) :description prose)) :focus 5)

            failures
            (let [{:keys [g]} (virtual-screen)]
              (into []
                    (for [cols (range 1 81 5)
                          rows (range 1 31)
                          :let [failure (try (hi/paint! g cols rows form)
                                             nil
                                             (catch Throwable t [cols rows (.getMessage t)]))]
                          :when failure]

                      failure)))]

        (expect (= [] failures)))))

;; Sliders and the request's own buttons (issue #108)

(defn- slider-request
  "A text field so ↑/↓ has somewhere to come from, then a slider."
  [& {:as slider}]
  (assoc (request)
    :nodes [{:id "who" :type :plaintext :label "Who"}
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
      (let [decimal (assoc (hi/init-form (slider-request :min 0 :max 1 :step 0.25 :default 0.5))
                      :focus 1)]
        (expect (= 0.75 (get-in (feed decimal [{:kind :right}]) [:values "pct"]))))))

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
        (expect (= :cancel (:action (hi/handle-event (assoc form :focus 3) (ch \space))))))))

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
      ;; No attachments and no agent dock: both bands are empty.
      :dock-top 23
      :rail-top 24
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

(defdescribe composer-test
             ;; Regression, issue #147: the band was painted OVER the prompt box and the
             ;; composer was blanked to make room, so the human could not see what they had
             ;; been typing while the transient asked its question.
             (it "keeps the composer visible UNDER a human-input transient, minus the keyboard"
                 (let [screen (bottom-chrome (assoc (idle-db)
                                               :human-input (hi/init-form {:id "r"
                                                                           :title "Tiny"
                                                                           :nodes [{:id "a"
                                                                                    :type :checkbox
                                                                                    :label "Yes"}]
                                                                           :is-cancellable true})))]
                   (expect (rule-row? (screen-row screen 24)))
                   (expect (str/includes? (screen-row screen 25) "hello draft"))
                   (expect (rule-row? (screen-row screen 26)))
                   ;; ...but the form owns the keyboard, so no caret sits in the composer.
                   (expect (nil? (.getCursorPosition screen))))))

;; One-time codes, and errors that arrive ONLY from a confirmation

(defn- otp-request
  "A view built by the ENGINE itself, so these tests pin the real contract and
   not a hand-written guess at it."
  ([] (otp-request 6 6))
  ([lo hi]
   (canonical-input-view
     {"title" "Confirm the code"
      "nodes"
      [{"name" "email" "label" "Email" "is_required" true}
       {"name" "code" "type" "otp" "label" "One-time code" "min_length" lo "max_length" hi}]})))

(defn- otp-form
  "The OTP request with the cursor already on its boxes."
  ([] (otp-form 6 6))
  ([lo hi] (assoc (hi/init-form (otp-request lo hi)) :focus 1)))

(defn- otp-row [form] (first (filter #(= :otp (:kind %)) (hi/form-rows form 60))))

;; Regression: every field of a form was painted at FULL strength — each label in
;; bold ink, each typed value as bare prose behind a `› ` prompt on the dialog's
;; own paper — so a prompt asking for an email AND the one-time code sent to it
;; said nothing about which of the two took the next keystroke, and neither of
;; them looked like the input boxes the rest of the TUI paints.
(defdescribe
  focus-emphasis-test
  "Focus is an EMPHASIS, not a list: the section the keyboard is in takes the ink
   and the surface, the others recede — and a typed field is drawn as a field."
  (it "parks the terminal cursor inside the box the next digit lands in"
      (let [{:keys [screen g]}
            (virtual-screen)

            ^TerminalPosition pos
            (hi/paint! g 80 30 (feed (otp-form) [(ch \3) (ch \1)]))

            row
            (screen-row screen (.getRow pos))]

        (expect (str/includes? row "[•] [•] [ ]"))
        ;; Inside the THIRD box: its own bracket one column to the left.
        (expect (= \[ (.charAt row (dec (.getColumn pos)))))
        (expect (= \space (.charAt row (.getColumn pos)))))))

(defdescribe
  otp-field-test
  ;; Regression, issue #128: the boxes painted the digits themselves, so a
  ;; one-time code sat in plaintext on the screen, in a screenshot and in a
  ;; screencast of the session.
  (it "masks every digit it took — a code is a credential, like a password"
      (expect (= "[ ] [ ] [ ] [ ] [ ] [ ]" (:text (otp-row (otp-form)))))
      (expect (= "[•] [•] [ ] [ ] [ ] [ ]" (:text (otp-row (feed (otp-form) [(ch \1) (ch \2)])))))
      (expect (not (str/includes? (:text (otp-row (feed (otp-form) (map ch "123456")))) "1"))))
  (it "takes digits only, so a pasted `123-456` arrives as the code"
      ;; A paste reaches the dialog as its characters; dropping everything that
      ;; is not a digit IS the paste handler.
      (let [form (feed (otp-form) (map ch "123-456"))]
        (expect (= "123456" (get-in form [:values "code"])))
        (expect (= "[•] [•] [•] [•] [•] [•]" (:text (otp-row form))))))
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
                           [:values "code"]))))))

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
      (let [errors
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
      (let [form
            (hi/set-errors (hi/init-form (otp-request))
                           {"email" "that address bounced" "code" "must be 6 digits"})

            typed
            (feed form [(ch \a)])]

        (expect (= {"code" "must be 6 digits"} (:errors typed)))
        (expect (nil? (some #{"that address bounced"} (map :text (hi/form-rows typed 60)))))
        ;; Erasing is a touch as much as typing is.
        (expect (= {"code" "must be 6 digits"} (:errors (feed typed [{:kind :backspace}]))))))
  (it "toggling, picking and nudging are touches too"
      (let [request
            (canonical-input-view
              {"title" "Ship"
               "nodes" [{"name" "ok" "type" "checkbox" "label" "Confirm"}
                        {"name" "env" "type" "select" "label" "Env" "options" ["prod" "stg"]}
                        {"name" "risk" "type" "range" "label" "Risk" "min" 0 "max" 10 "step" 1}]})

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
      (let [retyped
            (feed (hi/set-errors (hi/init-form (otp-request)) {"email" "that address bounced"})
                  (map ch "ops@example.com"))

            {:keys [form action]}
            (hi/handle-event retyped {:kind :submit})]

        (expect (= {} (:errors retyped)))
        (expect (= :submit action))
        (expect (= "ops@example.com" (get (hi/submit-values form) "email"))))))

;; Layout groups

(defn- grouped-request
  "A request whose first field is a LAYOUT GROUP, built by the engine so these
   tests pin the real wire contract and not a hand-written guess at it."
  [group]
  (canonical-input-view {"title" "Connect" "nodes" [group {"name" "notes" "label" "Notes"}]}))

(defn- server-group
  "Two fields that belong together, laid out in `direction`."
  [direction]
  {"type" "group"
   "direction" direction
   "label" "Server"
   "children" [{"name" "host" "label" "Host" "is_required" true "placeholder" "db.internal"}
               {"name" "port" "label" "Port" "placeholder" "5432"}]})

(defn- ink
  "Every non-blank painted row of `form`, framing stripped."
  [form]
  (into [] (remove str/blank?) (painted-rows form "")))

(defn- caret
  "Where `paint!` parks the terminal caret: `[x y]`."
  [form]
  (let [{:keys [g]}
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
  (it "keeps every LEAF a stop of its own, in reading order — a group is not one"
      (let [form (hi/init-form (grouped-request (server-group "row")))]
        (expect (= ["host" "port" "notes"] (into [] (keep :field-id) (:stops form))))
        (expect (= {"host" "" "port" "" "notes" ""} (:values form)))))
  (it "composes: a `column` group nested inside a `row` group is one of its columns"
      ;; Two directions and no third rule — the tree does the rest.
      (let [rows (ink (hi/init-form (grouped-request {"type" "group"
                                                      "direction" "row"
                                                      "children" [{"type" "group"
                                                                   "direction" "column"
                                                                   "label" "Left"
                                                                   "children"
                                                                   [{"name" "a" "label" "A"}
                                                                    {"name" "b" "label" "B"}]}
                                                                  {"name" "c" "label" "C"}]})))]
        ;; The nested column's heading shares its line with the neighbour column,
        ;; and B — the second row of that column — is BELOW A, not beside it.
        (expect (some #(and (str/includes? % "Left") (str/includes? % "C")) rows))
        (expect (< (long (row-index rows #(str/includes? % "A")))
                   (long (row-index rows #(str/includes? % "B")))))))
  (it "moves the caret ACROSS a shared row, not down it"
      (let [form
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
      (let [form (feed (assoc (hi/init-form (grouped-request (server-group "row"))) :focus 1)
                       (map ch "5433"))]
        (expect (= {"host" "" "port" "5433" "notes" ""} (:values form)))
        (expect (some #(str/includes? % "5433") (ink form)))))
  (it "prints a grouped field's error inside its own column"
      (let [form (hi/set-errors (hi/init-form (grouped-request (server-group "row")))
                                {"host" "is required"})]
        (expect (= {"host" "is required"} (:errors form)))
        (expect (some #(str/includes? % "is required") (ink form)))))
  (it "answers with one flat map of leaves, whatever the layout"
      (let [view
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
      (let [form
            (hi/init-form (grouped-request
                            {"type" "group"
                             "direction" "row"
                             "children"
                             [{"name" "env" "type" "select" "options" ["dev" "prod"]}
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

(defn- field-group
  "`n` text fields laid out in `direction`."
  [direction n]
  {"type" "group"
   "direction" direction
   "label" "Fields"
   "children" (mapv #(hash-map "name" (str "f" %) "label" (str "F" %)) (range n))})

(defn- column-labels
  "The label texts of every `:columns` row of `form` at `width`."
  [form width]
  (into []
        (comp (filter #(and (= :columns (:kind %)) (:is-label %)))
              (map #(mapv (fn [cell]
                            (when (= :label (:kind cell)) (:text cell)))
                          (:cells %))))
        (hi/form-rows form width)))

(defdescribe
  grid-and-wrap-group-test
  (it "a grid cuts its fields into rows of equal columns, the last row padded"
      (let [form (hi/init-form (grouped-request (field-group "grid" 5)))]
        (expect (= [["F0" "F1"] ["F2" "F3"] ["F4" nil]] (column-labels form 80)))
        (expect (= [5] (mapv count (column-labels form 300)))
                "a wide form puts all five on one row")))
  (it "a grid never takes more than max_columns"
      (let [form (hi/init-form (grouped-request (assoc (field-group "grid" 5) "max_columns" 2)))]
        (expect (= [["F0" "F1"] ["F2" "F3"] ["F4" nil]] (column-labels form 300)))))
  (it "a narrow grid stacks its fields like a column"
      (let [rows (hi/form-rows (hi/init-form (grouped-request (field-group "grid" 5))) 40)]
        (expect (not-any? #(= :columns (:kind %)) rows))
        (expect (= ["Fields" "F0" "F1" "F2" "F3" "F4" "Notes"]
                   (mapv :text (filter #(= :label (:kind %)) rows))))))
  (it "keeps every field of a grid a stop, and types into the focused cell only"
      (let [form
            (hi/init-form (grouped-request (field-group "grid" 3)))

            typed
            (feed (assoc form :focus 2) (map ch "db"))]

        (expect (= ["f0" "f1" "f2" "notes"] (into [] (keep :field-id) (:stops form))))
        (expect (= "db" (get (:values typed) "f2")))
        (expect (some #(str/includes? % "db") (ink typed)))
        (let [[x0 y0]
              (caret (assoc form :focus 0))

              [x1 y1]
              (caret (assoc form :focus 1))]

          (expect (= y0 y1) "two grid cells share a row")
          (expect (< (long x0) (long x1))))))
  (it "a wrap packs short fields at their own width"
      (let [rows
            (hi/form-rows (hi/init-form (grouped-request
                                          {"type" "group"
                                           "direction" "wrap"
                                           "children" [{"name" "ok" "type" "checkbox" "label" "Ok"}
                                                       {"name" "go" "type" "checkbox" "label" "Go"}
                                                       {"name" "host" "label" "Host"}]}))
                          70)

            line
            (first (filter #(= :columns (:kind %)) rows))]

        (expect (= 3 (count (:cells line))))
        (expect (= [8 8 24] (:cell-widths line))
                "a checkbox asks for its mark and label, a typed field for a readable cell"))))

(defdescribe
  responsive-group-layout-test
  (it "stacks row groups below the same readable column width as Live"
      (let [form
            (hi/init-form (grouped-request (server-group "row")))

            split?
            (fn [width]
              (boolean (some #(= :columns (:kind %)) (hi/form-rows form width))))]

        (expect (= [false false true true] (mapv split? [20 53 54 80])))
        (expect (split? nil))))
  (it "retains nested labels, errors and focus when a row becomes a column"
      (let [request
            (grouped-request {"type" "group"
                              "direction" "row"
                              "children" [(server-group "row") {"name" "region" "label" "Region"}]})

            form
            (hi/set-errors (hi/init-form request) {"port" "Port is required"})

            rows
            (hi/form-rows form 40)]

        (expect (not-any? #(= :columns (:kind %)) rows))
        (expect (= ["Server" "Host *" "Port" "Region" "Notes"]
                   (mapv :text (filter #(= :label (:kind %)) rows))))
        (expect (some #(= "Port is required" (:text %)) rows))
        (expect (= "port" (:field-id (hi/focused-stop form))))
        (expect (= 1 (count (filter :is-focused rows))))))
  (it "paints narrow fields on separate rows and keeps their caret inside the frame"
      (let [form
            (hi/init-form (grouped-request (server-group "row")))

            positions
            (mapv (fn [focus]
                    (let [{:keys [screen g]}
                          (virtual-screen)

                          pos
                          (hi/paint! g 40 30 (assoc form :focus focus))

                          lines
                          (mapv #(screen-row screen %) (range 30))]

                      (expect (not-any? #(and (str/includes? % "Host") (str/includes? % "Port"))
                                        lines))
                      (expect (every? #(<= (count %) 40) lines))
                      [(.getColumn ^TerminalPosition pos) (.getRow ^TerminalPosition pos)]))
                  [0 1])]

        (expect (= (ffirst positions) (first (second positions))))
        (expect (< (long (second (first positions))) (long (second (second positions))))))))

;; Decoration — the ink a form needs that answers nothing

(defn- decorated-request
  "A view built by the ENGINE itself: two decorations, a field, and a heading
   INSIDE a group, so these tests pin the real contract."
  []
  (canonical-input-view {"title" "Deploy"
                         "nodes"
                         [{"type" "heading" "text" "Connection"}
                          {"type" "paragraph" "text" "Where the job runs and who it runs as."}
                          {"name" "host" "label" "Host"}
                          {"type" "group"
                           "name" "creds"
                           "children" [{"type" "heading" "text" "Credentials"}
                                       {"name" "pw" "type" "password" "label" "Password"}]}]}))

(defdescribe
  decoration-test
  "A form is not only questions. A heading breaks a long one into sections and a
   paragraph explains one; neither holds a value, neither can be focused, and
   both are what make a form buildable out of more than inputs."
  (it "never makes a decoration a focus stop, so ↑/↓ walks the answers only"
      (expect (= ["host" "pw"] (into [] (keep :field-id) (hi/stops (decorated-request))))))
  (it "keeps a decoration out of the answer map entirely"
      (expect (= #{"host" "pw"} (set (keys (:values (hi/init-form (decorated-request))))))))
  ;; Regression #286: Ask paragraph Markdown must render like the request description.
  (it "renders paragraph marks and list blocks without exposing Markdown source"
      (let [form
            (hi/init-form (canonical-input-view
                            {"title" "Confirm"
                             "description" "**Deployment effect:** Updates the proxy."
                             "nodes" [{"type" "paragraph"
                                       "text" "**Target:** `DEV21`\n\n- **Memory:** 4096 MiB"}]}))

            rows
            (filterv #(= :paragraph (:kind %)) (hi/form-rows form 60))]

        (expect (= "Target: DEV21" (:text (first rows))))
        (expect (contains? (-> rows
                               first
                               :runs
                               first
                               :style)
                           :bold))
        (expect (some #(contains? (:style %) :code) (mapcat :runs rows)))
        (expect (some #(str/includes? (:text %) "Memory: 4096 MiB") rows))
        (let [{:keys [screen g]} (virtual-screen)]
          (try (hi/paint! g 80 30 form)
               (expect (contains? (modifiers-of screen "Target:") SGR/BOLD))
               (expect (not (str/includes? (screen-text screen) "**Target:**")))
               (finally (.close ^TerminalScreen screen))))))
  (it "wraps a long paragraph instead of clipping it to a single `…` row"
      (let [rows (hi/form-rows (hi/init-form
                                 (identity (engine/view<-wire
                                             {"title" "T"
                                              "nodes" [{"type" "paragraph"
                                                        "text" (str/join " " (repeat 30 "words"))}
                                                       {"name" "a" "label" "A"}]})))
                               40)]
        (expect (< 1 (count (filterv #(= :paragraph (:kind %)) rows)))))))

;;; ── The focused field's ring rides its own surface ───────────────────────────
;; Regression (reported from the TUI, photo of a transient band): the accent ring
;; `▎` a focused field wears was painted one column OUTSIDE the row's own
;; surface — on the frame's rail in a band, floating in the margin here — and the
;; line being typed started one column right of the label that names it, so a form
;; that promises ONE text column painted two.
(defdescribe
  focused-field-ring-test
  (it "keeps the ring inside the rails and the typed line under its own label"
      (let [{:keys [screen g]}
            (virtual-screen)

            _
            (hi/paint! g 80 30 (hi/init-form (request)))

            rows
            (mapv #(screen-row screen %) (range 30))

            ring-row
            (first (filter #(str/includes? % "┃") rows))

            label-row
            (first (filter #(str/includes? % "User") rows))]

        ;; the ring is the field's own left edge, and the text it fences starts in
        ;; the very column the label above it does
        (expect (str/includes? ring-row "┃who"))
        (expect (= (str/index-of label-row "User") (str/index-of ring-row "who")))
        ;; the band's rails are the first and the last ink on that row — the ring
        ;; never reaches either of them
        (expect (< (long (str/index-of ring-row "│")) (long (str/index-of ring-row "┃"))))
        (expect (< (long (str/index-of ring-row "┃")) (long (str/last-index-of ring-row "│")))))))

(defdescribe
  band-queue-hint-test
  "What the band says when MORE requests are parked behind the open one. A second
   request used to be invisible: the operator answered the form in front of them,
   the band came straight back with another question, and every INPUT NEEDED badge
   outside this dialog read exactly as it had before the answer."
  (it "leaves the rule alone while this is the only request"
      (expect (= "Deploy" (hi/band-label (hi/init-form (request)) 0))))
  (it "names how many more are waiting"
      (expect (= "Deploy · 2 more waiting" (hi/band-label (hi/init-form (request)) 2))))
  (it "still counts them when the request asked no question of its own"
      (expect (= "1 more waiting" (hi/band-label (hi/init-form (assoc (request) :title "")) 1)))))

;; Issue #392: a subagent's form also reaches its parent session, so the dialog
;; must say which subagent asks and which session is its parent.
(defdescribe
  subagent-origin-test
  (it "names the subagent and its parent above the form"
      (let [form
            (hi/init-form (assoc (request)
                            :origin {:session-id "child-1"
                                     :title "Read logs"
                                     :parent-session-id "leader-1"
                                     :parent-title "Deploy DEV13"}))

            rows
            (hi/form-rows form)]

        (expect (= {:kind :description :text "Subagent Read logs · parent Deploy DEV13"}
                   (first rows)))
        (expect (= {:kind :blank} (second rows)))))
  (it "shortens each long title with an ellipsis, so the parent stays visible"
      (let [text (hi/origin-text {:session-id "child-1"
                                  :title (apply str (repeat 60 "a"))
                                  :parent-session-id "leader-1"
                                  :parent-title (apply str (repeat 60 "b"))})]
        (expect (= (str "Subagent "
                        (apply str (repeat 27 "a"))
                        "… · parent "
                        (apply str (repeat 27 "b"))
                        "…")
                   text))))
  (it
    "keeps the origin on one row and ellipsizes it in a narrow dialog"
    (let [form
          (hi/init-form (assoc (request)
                          :origin {:session-id "child-1"
                                   :title "Collect the CloudWatch logs of every service in DEV13"
                                   :parent-session-id "leader-1"
                                   :parent-title "Deploy the release candidate to DEV13"}))

          plan
          (hi/form-rows form 20)

          {:keys [screen g]}
          (virtual-screen)

          _
          (hi/paint! g 40 30 form 0 tr/prompt-rows 1)

          row
          (first (filter #(str/includes? % "Subagent") (mapv #(screen-row screen %) (range 30))))]

      (expect (= :description (:kind (first plan))))
      (expect (= :blank (:kind (second plan))))
      (expect (str/includes? (:text (first plan)) " · parent "))
      (expect (some? row))
      (expect (str/includes? row "…"))
      (expect (not (str/includes? row "DEV13")))))
  (it "keeps the origin when a view crosses the wire"
      (expect (= {:session-id "child-1" :parent-session-id "leader-1" :parent-title "Lead"}
                 (:origin (engine/view<-wire {"id" "r"
                                              "title" "T"
                                              "origin" {"session_id" "child-1"
                                                        "parent_session_id" "leader-1"
                                                        "parent_title" "Lead"}})))))
  (it "opens a subagent form on the leader tab that received it, and answers its owner"
      ;; Before #392 the TUI routed by the form's own session, so a leader tab
      ;; dropped the forwarded form of a subagent that has no tab.
      (let [submitted
            (atom nil)

            child-form
            (hi/init-form (assoc (request)
                            :id "req-child"
                            :session-id "child-1"))]

        (reset! state/app-db {:session {:id "leader-1"} :render-version 0})
        (state/dispatch [:human-input-open child-form])
        (expect (nil? (:human-input @state/app-db)))
        (state/dispatch [:human-input-open child-form "leader-1"])
        (expect (= "req-child" (get-in @state/app-db [:human-input :request :id])))
        (with-redefs [vis/gateway-view-action!
                      (fn [sid id action]
                        (reset! submitted [sid id action])
                        {:action (:action action) :view-id id :is-accepted true})]
          (#'screen/human-input-key! @state/app-db (KeyStroke. KeyType/Escape)))
        (expect (= ["child-1" "req-child" :cancel]
                   [(first @submitted) (second @submitted) (get-in @submitted [2 :action])])))))
