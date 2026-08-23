(ns com.blockether.vis.ext.channel-tui.live-view-test
  "The live-view pane: what it paints, where the eye stays while the view changes
   underneath it, and what Escape hits while one is open.

   Every view here is DECLARED through the public builders and normalized by the
   engine, so a test can only paint shapes an extension can really produce."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.columns :as columns]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.live-view :as lv]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.human-input :as hi]
            [com.blockether.vis.internal.human-input :as engine]
            [com.blockether.vis.internal.human-input.live :as live]
            [com.blockether.vis.internal.human-input.spec :as hi-spec]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

;;; ── The views under test ────────────────────────────────────────────────────

(defn- mounted
  "A view as the engine mounts one: declared through the public builders, checked
   by the engine, materialized, and stamped with the id its patches name."
  [opts & nodes]
  (-> (apply hi/view
             (merge {:title "CI · fix(loop): move the session pick"
                     :description "Blockether/vis · 32062760734"}
                    (dissoc opts :id))
             nodes)
      engine/normalize-live-view
      live/materialize
      (assoc :id (or (:id opts) "view-1")
             :seq 0
             :created-at (System/currentTimeMillis))))

(defn- patched
  "`pane` after one patch carrying `ops` — normalized by the engine, so the seq
   advances and the ops are the ones a real patch would carry."
  [pane & ops]
  (lv/patched pane (engine/normalize-patch (:view pane) (vec ops))))

(defn- job-rows
  [n]
  (mapv #(hi/table-row (str "job-" %) [(str "job-" %) "success" (str % "m0s")]) (range n)))

(defn- jobs
  ([] (jobs {} 4))
  ([opts n]
   (hi/table "jobs"
             [(hi/table-column "job" "Job") (hi/table-column "state" "State")
              (hi/table-column "took" "Took")]
             (merge {:label "Jobs" :rows (job-rows n)} opts))))

(defn- ci-view
  [& {:keys [rows order id]}]
  (mounted {:id id}
           (hi/status "now" "Polling the run" {:label "Now" :tone :running})
           (jobs (cond-> {}
                   order
                   (assoc :order order))
                 (or rows 4))
           (hi/log "tail" {:label "Output" :lines ["> clojure -M:test" "Ran 314 tests"]})))

(defn- pane [& args] (lv/opened (apply ci-view args)))

(defn- rows-of [p] (lv/plan p 80))

(defn- kinds-of [p kind] (filterv #(= kind (:kind %)) (rows-of p)))

;;; ── Painting ────────────────────────────────────────────────────────────────

(defn- paint-frames
  "One REAL Lanterna paint of `panes` on a `cols`x`rows` terminal — the captured
   frames plus the geometry `paint!` handed back."
  ([panes] (paint-frames panes 96 26))
  ([panes cols rows]
   (let [geom
         (atom nil)

         cap
         (cap/capture! {:cols cols
                        :rows rows
                        :paint! (fn [{:keys [screen]}]
                                  (cr/begin-frame!)
                                  (let [g (.newTextGraphics ^TerminalScreen screen)]
                                    (reset! geom (lv/paint! g cols rows panes 1 3))
                                    (cr/commit-frame!)
                                    (.refresh ^TerminalScreen screen)))})]

     (assoc cap :geometry @geom))))

(defn- painted-text
  ([panes] (painted-text panes 96 26))
  ([panes cols rows] (cap/frame-text (last (:frames (paint-frames panes cols rows))))))

(defn- cell-under
  "The captured cell painting character `idx` of `needle`, on the first row that
   carries it — how a test reads the INK a row was really painted in."
  [frame needle idx]
  (let [row
        (first (filter #(str/includes? (apply str (map :ch %)) needle) frame))

        text
        (apply str (map :ch row))]

    (nth row (+ (long (str/index-of text needle)) (long idx)))))
;;; ── Tests ───────────────────────────────────────────────────────────────────

(deftest live-view-plan-test
  (testing "every node paints under its own label, in declaration order"
    (let [text (str/join "\n" (map :text (rows-of (pane))))]
      (is (str/includes? text "Now"))
      (is (str/includes? text "Polling the run"))
      (is (str/includes? text "Jobs"))
      (is (str/includes? text "job-0"))
      (is (str/includes? text "Output"))
      (is (str/includes? text "Ran 314 tests"))
      (is (< (.indexOf ^String text "Now") (.indexOf ^String text "Jobs"))
          "declaration order, not arrival order")))
  (testing "the view's own description opens the surface"
    (is (= "Blockether/vis · 32062760734" (:text (first (rows-of (pane)))))))
  (testing "an empty table is still a box, and says so between its own rails"
    (let [p
          (lv/opened (mounted {} (jobs {} 0)))

          plan
          (rows-of p)]

      (is (= [:prose :node :trule :thead :trule :empty :trule] (mapv :kind plan))
          "top rail, the header, a rail, the sentence, and the box closed under it")
      (is (str/includes? (:text (nth plan 3)) "Job"))
      (is (str/includes? (:text (nth plan 5)) "no rows yet")
          "the sentence stands INSIDE the box, between its rails"))))

(deftest live-view-window-test
  (testing "a node paints a window and says how much it is holding back"
    (let [p
          (pane :rows 20)

          more
          (first (kinds-of p :more))]

      (is (= lv/node-window (count (kinds-of p :trow))))
      (is (= "+ 8 more rows" (:text more)))
      (is (= "jobs" (:node-id more)))))
  (testing "expanding is a click on that line, and it shows everything"
    (let [p (lv/expanded (pane :rows 20) "jobs")]
      (is (= 20 (count (kinds-of p :trow))))
      (is (empty? (kinds-of p :more)))))
  (testing "a log answers its TAIL — the newest lines are why anybody watches"
    (let [p
          (lv/opened (mounted {}
                              (hi/log "tail"
                                      {:label "Output" :lines (mapv #(str "line " %) (range 40))})))

          lines
          (mapv :text (filterv #(= :log (:kind %)) (rows-of p)))]

      (is (= lv/node-window (count lines)))
      (is (= "line 39" (last lines)))
      (is (= "line 28" (first lines)))
      (is (some #(str/includes? (str (:text %)) "28 earlier lines") (rows-of p))))))

(deftest live-view-follow-test
  (testing "a fresh pane follows the end" (is (:is-following (pane))))
  (testing "scrolling up releases follow; landing back at the bottom re-arms it"
    (let [p
          (-> (pane :rows 30)
              (lv/painted {:offset 40 :total 60 :visible 20}))

          up
          (lv/scrolled p -6)]

      (is (not (:is-following up)))
      (is (= 34 (:offset up)))
      (is (:is-following (lv/scrolled up 6)))
      (is (:is-following (lv/scrolled up 999)) "the wheel cannot run past the end")
      (is (= 40 (:offset (lv/scrolled up 999))))))
  ;; Reported in Vis session 22b3489b-336f-42d0-9bc8-806dff2de86f: a stray tick of
  ;; trackpad inertia re-armed follow-tail and snapped the pane to the live edge
  ;; mid-read.
  (testing "an upward gesture that only CLAMPS at the end does not arm follow"
    (let [shrunk (-> (pane :rows 30)
                     (lv/painted {:offset 40 :total 60 :visible 20})
                     (lv/scrolled -6)
                     (lv/painted {:offset 34 :total 34 :visible 20}))]
      (is (= 14 (:offset (lv/scrolled shrunk -6))))
      (is (not (:is-following (lv/scrolled shrunk -6))))))
  (testing "a view with nothing to scroll keeps following through a stray tick"
    (let [tiny (-> (pane :rows 3)
                   (lv/painted {:offset 0 :total 3 :visible 20}))]
      (is (:is-following (lv/scrolled tiny -1)))))
  (testing "a following pane sits at the end of whatever the plan is now"
    (let [p
          (pane :rows 30)

          rows
          (rows-of p)]

      (is (= (max 0 (- (count rows) 8)) (lv/offset p rows 8))))))

(defn- parked
  "A pane the human has scrolled BACK to, in the order it really happens: the
   paint measures, the wheel releases follow-tail, the next paint records the
   anchor under the eye. Answers `[pane offset anchor]`."
  [p visible target]
  (let [rows
        (rows-of p)

        n
        (count rows)

        limit
        (max 0 (- n (long visible)))

        up
        (- (long target) (long limit))

        scrolled
        (-> p
            (lv/painted {:offset limit :total n :visible visible})
            (lv/scrolled up))

        o
        (lv/offset scrolled rows visible)

        anchor
        (lv/anchor-at rows o)]

    [(lv/painted scrolled {:offset o :anchor anchor :total n :visible visible}) o anchor]))

(defn- trow-index
  [p nth-row]
  (nth (keep-indexed #(when (= :trow (:kind %2)) %1) (rows-of p)) nth-row))

(deftest live-view-anchor-test
  (testing "rows arriving ABOVE the eye move the scrollbar, not the reading position"
    (let [p
          (-> (pane :rows 12 :order :newest-first)
              (lv/expanded "jobs"))

          [parked eye anchor]
          (parked p 6 (trow-index p 4))

          grown
          (patched parked
                   {:op :append
                    :node-id "jobs"
                    :rows (mapv #(hi/table-row (str "new-" %) [(str "new-" %) "queued" "0s"])
                                (range 3))})

          grown-rows
          (rows-of grown)]

      (is (= "jobs" (first anchor)) "the anchor names the node the eye is inside")
      (is (str/starts-with? (str (second anchor)) "job-")
          "and the ROW ID under it, never a line offset")
      (is (not (:is-following parked)) "reading back is a deliberate intent")
      (is (= 6 (- (long (lv/offset grown grown-rows 6)) (long eye)))
          "three rows landed above it, and each brings the rail that separates it")
      (is (= anchor (lv/anchor-at grown-rows (lv/offset grown grown-rows 6)))
          "and the same row is still the top visible one")))
  (testing "a removed row above the eye pulls the viewport back with it"
    (let [p
          (-> (pane :rows 12 :order :newest-first)
              (lv/expanded "jobs"))

          [parked eye anchor]
          (parked p 6 (trow-index p 4))

          cut
          (patched parked {:op :remove :node-id "jobs" :item-ids ["job-11" "job-10"]})

          cut-rows
          (rows-of cut)]

      (is (= (- (long eye) 4) (long (lv/offset cut cut-rows 6)))
          "two rows left, and the two rails between them left with them")
      (is (= anchor (lv/anchor-at cut-rows (lv/offset cut cut-rows 6))))))
  (testing "an anchor whose row is gone falls back to the node it belonged to"
    (let [p
          (-> (pane :rows 12)
              (lv/expanded "jobs"))

          [parked _ anchor]
          (parked p 6 (trow-index p 4))

          cut
          (patched parked {:op :remove :node-id "jobs" :item-ids [(second anchor)]})

          cut-rows
          (rows-of cut)]

      (is (= "jobs" (first (lv/anchor-at cut-rows (lv/offset cut cut-rows 6))))
          "the eye lands inside the same node instead of jumping to the top")))
  (testing "a viewport pinned at the end follows new rows"
    (let [p
          (pane :rows 4)

          rows
          (rows-of p)

          following
          (lv/painted p {:offset 0 :total (count rows) :visible 6})

          grown
          (patched
            following
            {:op :append :node-id "jobs" :rows [(hi/table-row "job-9" ["job-9" "running" "1s"])]})

          grown-rows
          (rows-of grown)]

      (is (:is-following grown))
      (is (= (- (count grown-rows) 6) (long (lv/offset grown grown-rows 6)))))))

(deftest live-view-widths-test
  (testing "a column measured wide stays wide while the view is open"
    (let [p
          (-> (pane :rows 3)
              (lv/expanded "jobs"))

          wide
          (patched p
                   {:op :append
                    :node-id "jobs"
                    :rows [(hi/table-row "job-1" ["job-1" "a state nobody expected" "1m0s"])]})

          measured
          (:widths (meta (rows-of wide)))

          taught
          (lv/painted wide {:offset 0 :total 9 :visible 9 :widths measured})

          narrow
          (patched
            taught
            {:op :append :node-id "jobs" :rows [(hi/table-row "job-1" ["job-1" "ok" "1m0s"])]})

          after
          (:widths (meta (rows-of narrow)))]

      (is (= (get-in measured ["jobs"]) (get-in after ["jobs"]))
          "a shorter value never shuffles the columns the human already read")
      (is (> (long (second (get measured "jobs"))) (count "a state"))))))

(deftest live-view-fresh-test
  (testing "what the LAST patch touched is what is emphasised, and only until the next one"
    (let [p
          (pane :rows 3)

          one
          (patched
            p
            {:op :append :node-id "jobs" :rows [(hi/table-row "job-9" ["job-9" "running" "1s"])]})

          two
          (patched one {:op :set :node-id "now" :text "Still polling"})]

      (is (some #(and (= "job-9" (:item-id %)) (:is-fresh %)) (rows-of one)))
      (is (not-any? #(and (= "job-9" (:item-id %)) (:is-fresh %)) (rows-of two))
          "the next patch clears it — no timer, no repaint that erases itself"))))

(deftest live-view-escape-precedence-test
  (testing "Escape hits the NEWEST view that says it may be stopped"
    (let [a
          (pane :id "view-a")

          b
          (pane :id "view-b")]

      (is (= "view-b" (lv/view-id (lv/interruptible [a b]))))
      (is (nil? (lv/interruptible [])))))
  (testing "EVERY open view answers the key — a view asks nothing, so none may refuse to stop"
    (let [p (lv/opened (ci-view))]
      (is (= (lv/view-id p) (lv/view-id (lv/interruptible [p]))))))
  (testing "the echo row advertises the SAME thing the key does, even mid-turn"
    (let [row (first (#'footer/echo-segments {:loading? true :live-views [(pane)]}))]
      (is (str/includes? (:text row) "stop"))
      (is (str/includes? (:text row) "CI · fix(loop): move the session pick"))
      (is
        (not (str/includes? (:text row) "cancel"))
        "while a view is open the abort key stops the VIEW, so the row must not promise a turn cancel")))
  (testing "with no view open the row goes back to the turn's own hint"
    (is (str/includes? (:text (first (#'footer/echo-segments {:loading? true :live-views []})))
                       "cancel"))))

(deftest live-view-stop-note-test
  (testing "Escape ARMS the stop — and Escape again is what sends it"
    (let [p (lv/armed (lv/opened (ci-view)))]
      (is (= "" (lv/stopping p)) "armed, with nothing typed into it yet")
      (is (= "" (:note (lv/stop-prompt p))))
      (is (str/includes? (:label (lv/stop-prompt p)) "CI · fix(loop): move the session pick")
          "the prompt names the view it would stop, because several may be open")
      (is (nil? (lv/stop-prompt (lv/opened (ci-view)))) "a watched view asks nothing")
      (is (nil? (lv/stopping (lv/disarmed p))) "and `disarmed` is the way back")))
  (testing "the note is typed with the same keyboard a form's fields read"
    (let [typing
          (fn [pane text]
            (reduce (fn [pane c]
                      (:pane (lv/typed pane {:kind :char :char c})))
                    pane
                    text))

          p
          (typing (lv/armed (lv/opened (ci-view))) "wrong subnet")]

      (is (= "wrong subnet" (lv/stopping p)))
      (is (= "wrong subne" (lv/stopping (:pane (lv/typed p {:kind :backspace})))))
      (let [{:keys [pane action note]} (lv/typed p {:kind :enter})]
        (is (= :stop action))
        (is (= "wrong subnet" note) "the words travel WITH the stop, not after it")
        (is (nil? (lv/stopping pane)) "the line is gone the moment the stop is sent"))
      (let [{:keys [action note]} (lv/typed (lv/armed (lv/opened (ci-view))) {:kind :enter})]
        (is (= :stop action) "a stop with no words is still a stop")
        (is (nil? note)))
      (let [{:keys [pane action note]} (lv/typed p {:kind :cancel})]
        (is (= :stop action)
            "Escape STOPS: the key the human reached for to kill the run does that")
        (is (= "wrong subnet" note) "and it carries whatever was written by then")
        (is (nil? (lv/stopping pane))))
      (let [{:keys [pane action]} (lv/typed p {:kind :backspace})]
        (is (nil? action) "Backspace erases while there is something to erase")
        (is (= "wrong subne" (lv/stopping pane))))
      (let [{:keys [pane action]} (lv/typed (lv/armed (lv/opened (ci-view))) {:kind :backspace})]
        (is (= :keep action) "Backspace on an EMPTY line is the one key that can only undo")
        (is (nil? (lv/stopping pane)) "keeping watch forgets the armed stop"))
      (is (nil? (:action (lv/typed p {:kind :next})))
          "a key the line has no use for changes nothing")))
  (testing "the line stops growing where the engine cuts it"
    (let [p (reduce (fn [pane c]
                      (:pane (lv/typed pane {:kind :char :char c})))
                    (lv/armed (lv/opened (ci-view)))
                    (repeat (+ 10 (long hi-spec/note-chars)) \x))]
      (is (= (long hi-spec/note-chars) (count (lv/stopping p)))
          "a field that swallowed more than the model will read would lie about the note")))
  (testing "the band gives the armed line a row of its own, above the fence"
    (let [p
          (lv/opened (ci-view))

          watched
          (painted-text [p])

          armed
          (painted-text [(:pane (reduce (fn [{:keys [pane]} c]
                                          (lv/typed pane {:kind :char :char c}))
                                        {:pane (lv/armed p)}
                                        "wrong subnet"))])

          empty-armed
          (painted-text [(lv/armed p)])]

      (is (not (str/includes? watched "why?")) "a view being watched asks nothing")
      (is (str/includes? armed "why? wrong subnet") "the words are on screen as they are typed")
      (is (str/includes? armed "interrupt CI · fix(loop): move the session pick")
          "and the line says WHICH view they will stop")
      (is (str/includes? empty-armed "⌫ keep watching")
          "with nothing typed the bar offers the way back")
      (is (str/includes? armed "Esc / ⏎ interrupt with the note")
          "once there are words the bar says they will travel with the stop")
      (is (str/includes? armed "⌫ erase") "and Backspace is the key that takes them away again")
      (let [lines
            (str/split-lines armed)

            at
            (first (keep-indexed (fn [idx line]
                                   (when (str/includes? line "why?") idx))
                                 lines))]

        (is
          (str/includes? (nth lines (dec (long at)) "") "─")
          "the line is FENCED above as well as below: it is the band asking, not one more row of the report"))
      (is (str/includes? watched "Esc interrupt CI · fix(loop): move the session pick")
          "while it is only being watched the bar still advertises the one key it takes"))))

;;; ── The registry half: three ops and the tab that owns them ─────────────────

(defn- with-db
  [f]
  (let [old @state/app-db]
    (try (reset! state/app-db {:render-version 0 :session {:id "s1"}})
         (f)
         (finally (reset! state/app-db old)))))

(deftest live-view-state-test
  (testing "open, patch, close — the three ops the channel carries"
    (with-db
      (fn []
        (let [view (assoc (ci-view) :session-id "s1")]
          (state/dispatch [:live-view-open view])
          (is (= ["view-1"] (mapv lv/view-id (:live-views @state/app-db))))
          (state/dispatch [:live-view-patch
                           (engine/normalize-patch view [{:op :set :node-id "now" :text "Done"}])])
          (is (= "Done" (:text (first (:nodes (:view (first (:live-views @state/app-db)))))))
              "the ENGINE advanced the view; the terminal never interprets a patch itself")
          (state/dispatch [:live-view-minimize "view-1"])
          (is (lv/minimized? (first (:live-views @state/app-db)))
              "minimizing is terminal-local pane state, not a close")
          (state/dispatch [:live-view-restore "view-1"])
          (is (not (lv/minimized? (first (:live-views @state/app-db)))))
          (state/dispatch [:live-view-close "view-1" {:reason :completed}])
          (is (lv/settled? (first (:live-views @state/app-db)))
              "the close ends the view and leaves the line that reopens it")
          (is (lv/dormant? (first (:live-views @state/app-db)))
              "collapsed until the human presses it: the band's rows go back to the transcript")))))
  (testing "one view mounted twice keeps the pane the human is scrolling"
    (with-db (fn []
               (let [view (assoc (ci-view :rows 20) :session-id "s1")]
                 (state/dispatch [:live-view-open view])
                 (state/dispatch [:live-view-scroll "view-1" -4])
                 (state/dispatch [:live-view-expand "view-1" "jobs"])
                 (let [before (first (:live-views @state/app-db))]
                   (state/dispatch [:live-view-open view])
                   (is (= 1 (count (:live-views @state/app-db))))
                   (is (= before (first (:live-views @state/app-db)))
                       "a duplicate open is not a reset"))))))
  (testing "a view belonging to a session this terminal is not showing is dropped"
    (with-db (fn []
               (state/dispatch [:live-view-open (assoc (ci-view) :session-id "somewhere-else")])
               (is (empty? (:live-views @state/app-db))))))
  (testing "a close for a view that was never mounted is a no-op, not a throw"
    (with-db (fn []
               (state/dispatch [:live-view-close "never-here"])
               (is (empty? (:live-views @state/app-db)))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: the live
;; surface used half the available terminal and its focusable job rows had no control.
(deftest live-view-height-and-focus-test
  (testing "a busy live surface takes four fifths of the room above the composer"
    (let [[top bottom]
          (lv/band-rows 96 26 [(pane :rows 20)] 1 3)

          height
          (inc (- (long bottom) (long top)))

          available
          (- 26 1 3)]

      (is (>= (* 5 height) (* 4 available))
          "the watched run, rather than stale transcript, owns most of the terminal")))
  (testing "a focusable table paints shared focus and makes every visible row clickable"
    (let [p
          (lv/opened (mounted {} (jobs {:is-focusable true :focused-ids ["job-1"]} 8)))

          plan
          (lv/plan p 80)

          table-rows
          (filterv #(= :trow (:kind %)) plan)]

      (is (= [{:item-id "job-0" :is-focusable true :is-focused false}
              {:item-id "job-1" :is-focusable true :is-focused true}]
             (mapv #(select-keys % [:item-id :is-focusable :is-focused]) (take 2 table-rows))))
      (is (some #{["click" "focus a row"]} (lv/hint p []))
          "the band advertises the mouse control without taking the composer keyboard")
      (cr/reset!)
      (try (let [text
                 (cap/frame-text (last (:frames (paint-frames [p] 96 80))))

                 controls
                 (filterv #(= :live-focus (:kind %)) (cr/current))]

             (is (str/includes? text "○ job-0"))
             (is (str/includes? text "● job-1"))
             (is (= (mapv #(str "job-" %) (range 8)) (mapv :item-id controls))))
           (finally (cr/reset!))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: an active
;; live surface could consume most of the terminal but had no way to minimize it.
(deftest live-view-minimize-test
  (testing "an active live surface exposes a minimize control"
    (cr/reset!)
    (try (paint-frames [(pane)] 96 26)
         (let [controls (filterv #(= :live-minimize (:kind %)) (cr/current))]
           (is (= 1 (count controls)) "the opening rule has one explicit minimize control")
           (is (= "view-1" (:view-id (first controls))))
           (is (some #{["click ▾" "minimize"]} (lv/hint (pane) []))))
         (finally (cr/reset!))))
  (testing "minimizing keeps the run alive behind one restorable status row"
    (let [full
          (pane :rows 20)

          compact
          (lv/minimized full)

          advanced
          (patched compact {:op :set :node-id "now" :text "Still polling"})

          [full-top full-bottom]
          (lv/band-rows 96 26 [full] 1 3)

          [compact-top compact-bottom]
          (lv/band-rows 96 26 [compact] 1 3)]

      (is (lv/minimized? advanced) "ordinary patches do not reopen the local surface")
      (is (< (- (long compact-bottom) (long compact-top)) (- (long full-bottom) (long full-top)))
          "the transcript gets the live surface's body rows back")
      (is (= full (lv/restored compact)) "restoring preserves the exact viewport")
      (is (not (lv/minimized? (lv/armed compact)))
          "arming an interrupt restores the note field before it takes the keyboard")
      (is (some #{["click ▴" "restore live view"]} (lv/hint compact [])))
      (cr/reset!)
      (try (let [{:keys [frames]}
                 (paint-frames [advanced] 96 26)

                 text
                 (cap/frame-text (last frames))

                 controls
                 (filterv #(= :live-restore (:kind %)) (cr/current))]

             (is (str/includes? text "Still polling · minimized")
                 "patches keep updating the compact status while its body is folded")
             (is (not (str/includes? text "job-0")) "the table body is no longer painted")
             (is (= 2 (count controls)) "both the title chevron and status row restore")
             (is (every? #(= "view-1" (:view-id %)) controls)))
           (finally (cr/reset!))))))

;;; ── The screenshot gate ─────────────────────────────────────────────────────

(deftest live-view-paint-test
  (testing "the band paints the whole view when it fits"
    (let [{:keys [frames geometry]}
          (paint-frames [(pane)] 96 60)

          text
          (cap/frame-text (last frames))]

      (is (seq frames))
      (is (str/includes? text "CI · fix(loop): move the session pick"))
      (is (str/includes? text "Now"))
      (is (str/includes? text "Polling the run"))
      (is (str/includes? text "Jobs"))
      (is (str/includes? text "job-0"))
      (is (str/includes? text "Output"))
      (is (str/includes? text "Ran 314 tests"))
      (is (str/includes? text "Esc"))
      (is (= "view-1" (:view-id geometry)))
      (is (pos? (long (:total geometry))))
      (is (pos? (long (:visible geometry))))))
  (testing "and when it does not fit, the END — the newest work — is what is on screen"
    (let [p
          (pane :rows 20)

          text
          (painted-text [p] 96 26)]

      (is (str/includes? text "+ 8 more rows"))
      (is (str/includes? text "Ran 314 tests"))
      (is (not (str/includes? text "Polling the run"))
          "a view follows its tail, exactly like the log it carries")
      (is (str/includes? (lv/footer-text p) "Polling the run")
          "which is why the summary rides the footer while the eye is at the end")))
  (testing "several open views: the newest in full, the older ones one line each"
    (let [text (painted-text [(pane :id "old") (pane :id "new")])]
      (is (str/includes? text "▸"))
      (is (str/includes? text "2") "the hint says how many are open")))
  (testing "a real PNG of the band, before and after one patch"
    (let [before
          (pane :rows 6)

          after
          (patched
            before
            {:op :append :node-id "jobs" :rows [(hi/table-row "job-9" ["job-9" "failed" "0m3s"])]})

          shot!
          (fn [nm panes]
            (cap/shot! {:cols 96
                        :rows 26
                        :font-size 14
                        :out nm
                        :paint! (fn [{:keys [screen]}]
                                  (let [g (.newTextGraphics ^TerminalScreen screen)]
                                    (lv/paint! g 96 26 panes 1 3)
                                    (.refresh ^TerminalScreen screen)))}))

          png-before
          (shot! "vis-live-view-before" [before])

          png-after
          (shot! "vis-live-view-after" [after])

          ;; The stop is a CONVERSATION, so it has to be SEEN: an armed band keeps
          ;; painting the work while it asks why the human is stopping it.
          png-stop
          (shot! "vis-live-view-stop"
                 [(reduce (fn [p ch]
                            (:pane (lv/typed p {:kind :char :char ch})))
                          (lv/armed after)
                          "wrong subnet")])]

      (is (str/ends-with? png-before "vis-live-view-before.png"))
      (is (str/ends-with? png-after "vis-live-view-after.png"))
      (is (pos? (long (cap/ink png-before))) "the band really painted something")
      (is (< (long (cap/ink png-before)) (long (cap/ink png-after)))
          "the row that arrived is ink that was not there before")
      (is (not (str/includes? (painted-text [before]) "job-9")))
      (is (str/includes? (painted-text [after]) "job-9"))
      (is (str/ends-with? png-stop "vis-live-view-stop.png"))
      (is (pos? (long (cap/ink png-stop))) "the armed band still paints the work"))))

;; A tone is DATA the engine already carries, and a fraction is a fraction
;; however it was declared: a surface that paints either as flat prose is
;; dropping what the human came to read.
(deftest live-view-ink-test
  (testing "a counter wears its own tone, and the word naming it recedes"
    (let [p
          (lv/opened (mounted {}
                              (hi/stat "score"
                                       [{:id "failed" :label "Failed" :value-text "1" :tone "error"}
                                        {:id "seen" :label "Seen" :value-text "18"}])))

          frame
          (last (:frames (paint-frames [p] 80 20)))

          label
          (cell-under frame "Failed 1" 0)

          value
          (cell-under frame "Failed 1" 7)

          plain
          (cell-under frame "Seen 18" 5)]

      (is (= "1" (:ch value)))
      (is (not= (:fg label) (:fg value)) "the tone lands on the number, not on the word naming it")
      (is (not= (:fg value) (:fg plain)) "an untoned counter keeps the body's own ink")))
  (testing "a progress that declared its parts paints the bar it earned"
    (let [text (painted-text
                 [(lv/opened (mounted {}
                                      (hi/progress "done" {:label "Finished" :done 15 :total 18})))]
                 80
                 20)]
      (is (str/includes? text "83%"))
      (is (str/includes? text "15/18 done"))
      (is (str/includes? text "▰") "15 of 18 is a measured fraction, not indeterminate work")
      (is (not (str/includes? text "working")))))
  (testing "a progress nobody can size still says the one true thing"
    (let [text
          (painted-text [(lv/opened (mounted {} (hi/progress "done" {:label "Scanning"})))] 80 20)]
      (is (str/includes? text "working"))
      (is (not (str/includes? text "▰"))))))

;;; ── The width the band has, and who stands in it ────────────────────────────


;; A table that hugs its widest word leaves the band half empty and the eye
;; hunting across the gap: the surface IS the width the run was given, so the
;; table takes all of it — and takes it inside a drawn box, because a live table
;; is read while it fills and the eye needs the line saying where a row ends.
(deftest live-view-table-width-test
  (testing "head, rule and every row end on the column the band ends on"
    (doseq [w [80 60]]
      (let [lines (->> (lv/plan (pane) w)
                       (filter #(#{:thead :trule :trow} (:kind %)))
                       (mapv :text))]
        (is (seq lines))
        (is (= [w] (distinct (mapv count lines))) (str "at " w " columns")))))
  (testing "the slack lands on the column that was already the widest"
    (let [mid
          (->> (lv/plan (pane) 80)
               (filter #(= :trule (:kind %)))
               (map :text)
               (filter #(str/includes? % "┼"))
               first)

          cells
          (->> (str/split mid #"[├┼┤]")
               (remove str/blank?)
               (mapv count))]

      (is (= 3 (count cells)))
      (is
        (= (apply max cells) (second cells))
        "`state` holds the longest word, so `state` is the column that grows — the same judge that shrinks first")
      (is (< (long (first cells)) (long (second cells))))))
  (testing "and the table is a BOX, with a rail between every pair of rows"
    (let [rows
          (filterv #(#{:thead :trule :trow} (:kind %)) (lv/plan (pane) 80))

          texts
          (mapv :text rows)]

      (is (str/starts-with? (first texts) "┌"))
      (is (str/ends-with? (first texts) "┐"))
      (is (str/starts-with? (last texts) "└"))
      (is (every? #(and (str/starts-with? % "│") (str/ends-with? % "│"))
                  (map :text (filter #(#{:thead :trow} (:kind %)) rows)))
          "every line of cells stands between the rails")
      (is (= [:trule :thead :trule :trow :trule :trow] (subvec (mapv :kind rows) 0 6))
          "a rule under the head AND between the rows, not only where the box ends"))))

;; Every string a human reads in this program is markdown already — the
;; transcript, the form and the view's own document all speak it — so a live view
;; that painted its strings flat would be the ONE surface where `code` is not code.
(deftest live-view-inline-markdown-test
  (testing "a statement reads as words, and the marks that styled them are gone"
    (let [row
          (->> (lv/plan (lv/opened (mounted {}
                                            (hi/status "now"
                                                       "Bumped `openssl` on **db-2**"
                                                       {:label "Now" :tone :ok})))
                        80)
               (filter #(= :status (:kind %)))
               first)

          run-of
          (fn [text]
            (first (filter #(= text (:text %)) (:runs row))))]

      (is (str/includes? (:text row) "Bumped openssl on db-2"))
      (is (not (str/includes? (:text row) "`")) "the syntax is not the sentence")
      (is (contains? (:style (run-of "openssl")) :code))
      (is (contains? (:style (run-of "db-2")) :bold))))
  (testing "and the code span really wears another ink on the terminal"
    (let [frame (last (:frames (paint-frames [(lv/opened (mounted {}
                                                                  (hi/status "now"
                                                                             "Bumped `openssl` now"
                                                                             {:label "Now"})))]
                                             80
                                             20)))]
      (is (not= (:fg (cell-under frame "Bumped openssl" 0))
                (:fg (cell-under frame "Bumped openssl" 7)))
          "the word and the code span beside it are not painted in one flat ink")))
  (testing "a log line is machine output and stays VERBATIM"
    (let [text (str/join "\n"
                         (map :text
                              (lv/plan (lv/opened (mounted {}
                                                           (hi/log "tail"
                                                                   {:label "Output"
                                                                    :lines ["cat `x` **y**"]})))
                                       80)))]
      (is (str/includes? text "cat `x` **y**")
          "backticks in a build log are the build's own characters, not styling"))))

(defn- reading-pane
  "A table with the paragraph that explains it declared BESIDE it, in the form's
   own `row` — the shape an arrangement exists for."
  []
  (lv/opened
    (mounted
      {}
      (hi/row
        "reading"
        (hi/table "jobs"
                  [(hi/table-column "job" "Job") (hi/table-column "state" "State")]
                  {:label "Jobs"
                   :rows [(hi/table-row "j1" ["job-1" "success"])
                          (hi/table-row "j2" ["job-2" "failed"])]})
        (hi/status
          "note"
          "The `openssl` bump landed on db-2 only, so the sweep keeps going until every host answers on the new subnet."
          {:label "Reading" :tone :warn})))))

;; A live view arranges its work with the REQUEST's own two words, so `row` and
;; `column` mean one thing on every surface. They say the least that can be
;; said — these stand together, those stack — and how wide, or whether there is
;; room at all, the surface decides.
(deftest live-view-group-test
  (testing "a row stands the nodes declared in it side by side"
    (let [columns (filterv #(= :columns (:kind %)) (lv/plan (reading-pane) 80))]
      (is (seq columns))
      (is (= ["Jobs" "Reading"]
             (mapv :text
                   (:cells (first (filter #(every? (comp #{:node} :kind) (:cells %)) columns)))))
          "each column opens under its own label")
      (is (some #(and (= :trule (:kind (first (:cells %)))) (= :status (:kind (second (:cells %)))))
                columns)
          "the table's box and the paragraph explaining it start on ONE row")))
  (testing "the paragraph is JUSTIFIED to its column, so both its edges are straight"
    (let [cell-w
          (columns/cell-width 80 2)

          texts
          (->> (lv/plan (reading-pane) 80)
               (filter #(= :columns (:kind %)))
               (keep #(second (:cells %)))
               (filter #(= :status (:kind %)))
               (mapv :text))]

      (is (= 37 cell-w) "two columns and the gutter between them, out of an 80-column band")
      (is (< 1 (count texts)) "the sentence wrapped")
      (is (every? #(<= (count %) cell-w) texts) "no line ever runs past its column")
      (is (some #(= cell-w (count %)) (butlast texts))
          "and a line with gaps enough to close is flush with both edges")
      (is (> cell-w (count (last texts))) "the last line of a paragraph is never stretched")
      ;; Justification stops where it would open a river: a line short by more
      ;; than it has gaps to grow stays ragged. That is the terminal's ONE
      ;; policy (`markdown-layout/justify-line-runs`), not this pane's opinion.
      (is (every? (fn [line]
                    (let [slack
                          (- cell-w (count line))

                          gaps
                          (dec (count (str/split (str/trim line) #"\s+")))]

                      (or (zero? slack) (>= slack gaps))))
                  (butlast texts))
          "a ragged line is a justification refused, because stretching it would open a river")))
  (testing "a band too narrow to split stacks them instead"
    (let [kinds (set (map :kind (lv/plan (reading-pane) 40)))]
      (is (not (contains? kinds :columns))
          "under the narrowest useful column there is nothing to split")
      (is (contains? kinds :thead))
      (is (contains? kinds :status) "both nodes are still there, one under the other")))
  (testing "and the terminal really paints them on the same row"
    (let [line (->> (str/split-lines (painted-text [(reading-pane)]))
                    (filter #(str/includes? % "Jobs"))
                    first)]
      (is (str/includes? (str line) "Reading") "one screen row carries both labels")))
  ;; The proof a person can LOOK at: one band carrying a boxed table that spans
  ;; its column and the paragraph that explains it, marks and all.
  (testing "a real PNG of a table with its paragraph beside it"
    (let [png (cap/shot! {:cols 96
                          :rows 24
                          :font-size 14
                          :out "vis-live-view-group"
                          :paint! (fn [{:keys [screen]}]
                                    (let [g (.newTextGraphics ^TerminalScreen screen)]
                                      (lv/paint! g 96 24 [(reading-pane)] 1 3)
                                      (.refresh ^TerminalScreen screen)))})]
      (is (str/ends-with? png "vis-live-view-group.png"))
      (is (pos? (long (cap/ink png))) "the split band really painted"))))

;;; ── A finished view: one line, and the door back ─────────────────────────────

(defn- ended
  "The pane a close leaves behind, stamped a minute after the view opened — the
   engine's own verdict shape, so nothing here paints a state a run cannot end in."
  ([p] (ended p {}))
  ([p result]
   (lv/settled p (merge {:reason :completed} result) (+ (long (:created-at (:view p))) 60000))))

(defn- regions-of
  "Every click region ONE paint of `panes` published — how a test reads what the
   human can press, through the same registry the mouse is answered from."
  [panes]
  (cr/reset!)
  (cr/begin-frame!)
  (paint-frames panes)
  (cr/commit-frame!)
  (cr/current))

;; Regression, vis session a64d44c2: `band-rows` measured the band as ONE display
;; row while `paint!` drew every row the view asked for, so the wheel owned only
;; the four rows above the prompt — over the rest of a tall pane the transcript
;; underneath it scrolled instead.
(deftest live-band-height-test
  (testing "the wheel claims exactly the rows the band paints"
    (let [p
          (patched (pane) {:op :append :node-id "tail" :lines (mapv #(str "line " %) (range 40))})

          lines
          (str/split-lines (painted-text [p] 96 40))

          [from to]
          (lv/band-rows 96 40 [p] 1 3)]

      (is (str/includes? (nth lines from) "CI · fix(loop): move the session pick")
          "the first row it claims is the band's own titled rule")
      (is (str/includes? (nth lines to) "└") "the last is the rule that closes it")
      (is (every? str/blank? (take from lines))
          "and nothing of the band is painted above the rows the wheel owns")))
  (testing "a busy view takes four fifths of the available terminal without covering the composer"
    (let [p
          (patched (pane) {:op :append :node-id "tail" :lines (mapv #(str "line " %) (range 40))})

          height
          (fn [rows]
            (let [[from to] (lv/band-rows 96 rows [p] 1 3)]
              (inc (- (long to) (long from)))))]

      (doseq [rows [20 24 40 60]]
        (let [available (- rows 1 3)]
          (is (>= (* 5 (height rows)) (* 4 available))
              (str "on " rows " rows the watched run owns four fifths of the available surface"))
          (is (<= (height rows) available)
              (str "on " rows " rows the composer remains outside the band")))))))
;; Phase 5 of the live-view plan: a view used to vanish the moment it ended, so
;; the log the human had been watching became unreachable one frame after it
;; finished. What a finished run leaves now is a ROW OF THE TRANSCRIPT, in the
;; turn that watched it — the band is only for work that is still happening.
(deftest live-view-settled-test
  (testing "a finished view gives the band back and hands the transcript its row"
    (let [p
          (ended (patched (pane) {:op :append :node-id "tail" :lines ["one" "two" "three"]}))

          row
          (lv/run-row p)]

      (is (str/blank? (painted-text [p])) "the band paints nothing at all for a run that is over")
      (is (nil? (:geometry (paint-frames [p])))
          "there is no paint to hand geometry back from: nothing was drawn")
      (is (nil? (lv/band-rows 96 26 [p] 1 3)) "and the wheel is told there is no band to be over")
      (is (= "view-1" (:view-id row)) "the row names the view its press reads back")
      (is (str/includes? (:title row) "fix(loop): move the session pick"))
      (is (= :completed (:reason row)) "how it ended, in the engine's own word")
      (is (= 5 (:lines row)) "the record it left, not the window it painted")
      (is (= 60000 (:elapsed-ms row)) "and how long the run took, frozen at the close")))
  (testing "pressing that row reads the record back, and pressing it again puts it away"
    (let [p
          (ended (pane))

          open
          (lv/reopened p)]

      (is (str/includes? (painted-text [open]) "Ran 314 tests") "read-only, but all of it")
      (is (not (str/includes? (painted-text [(lv/reopened open)]) "Ran 314 tests"))
          "the same press closes it")
      (is (lv/settled? open) "reopening does not un-end the view")))
  (testing "the band publishes no control for a finished run — the transcript owns it"
    (is (empty? (filterv #(= :live-reopen (:kind %)) (regions-of [(ended (pane :id "done-1"))])))
        "the line that used to sit here is a row of the turn now"))
  (testing "an open view keeps the whole band while a finished one is filed away"
    (let [running
          (pane :id "running-1")

          done
          (ended (pane :id "done-1"))

          text
          (painted-text [done running])]

      (is (str/includes? text "Ran 314 tests") "the open view keeps the body")
      (is (not (str/includes? text "completed"))
          "and keeps it whole: a run that ended is no longer band furniture")
      (is (= "running-1" (lv/view-id (lv/interruptible [done running])))
          "a stop can only reach work that is still running")
      (is (nil? (lv/interruptible [done]))
          "and with nothing running there is nothing left to interrupt")))
  (testing "the ticker names the run the band is painting, and only while it runs"
    (let [running
          (pane :id "running-1")

          done
          (ended (pane :id "done-1"))]

      (is (str/includes? (str (lv/watching-title [done running]))
                         "fix(loop): move the session pick")
          "the sentence over the bubble says which run the human is looking at")
      (is (nil? (lv/watching-title [done]))
          "a finished run is a row of the transcript, never the phase of the turn")
      (is (nil? (lv/watching-title [])) "and an empty band names nothing")))
  (testing "the ops the channel carries: a close settles the pane, it does not drop it"
    (with-db (fn []
               (let [view (assoc (ci-view) :session-id "s1")]
                 (state/dispatch [:live-view-open view])
                 (state/dispatch [:live-view-close "view-1"
                                  {:reason :interrupted :artifact-id "art-1"}])
                 (let [p (first (:live-views @state/app-db))]
                   (is (lv/settled? p))
                   (is (= :interrupted (:reason (:settled p))))
                   (is (= "art-1" (:artifact-id (:settled p)))
                       "the pane knows the artifact it was filed as"))
                 (state/dispatch [:live-view-reopen "view-1"])
                 (is (not (lv/dormant? (first (:live-views @state/app-db)))))
                 (state/dispatch [:live-view-reopen "view-1"])
                 (is (lv/dormant? (first (:live-views @state/app-db))))))))
  ;; Regression, Vis session a64d44c2-8228-455f-926e-b3381f19a93b: run rows
  ;; were filed without their executing-form position and their disclosure stayed
  ;; visually collapsed after a click reopened the record.
  (testing "finished runs retain their execution anchors and disclosure state"
    (with-db
      (fn []
        (swap! state/app-db assoc
          :messages [{:role :user :text "watch it"} {:role :assistant :text "watching"}]
          :progress {:iterations [{:forms [{:code "first_watch()"}]}]})
        (state/dispatch [:live-view-open (assoc (ci-view :id "a") :session-id "s1")])
        (swap! state/app-db assoc-in
          [:progress :iterations]
          [{:forms [{:code "first_watch()"}]} {:forms [{:code "second_watch()"}]}])
        (state/dispatch [:live-view-open (assoc (ci-view :id "b") :session-id "s1")])
        (state/dispatch [:live-view-close "a" {:reason :completed}])
        (state/dispatch [:live-view-close "b" {:reason :failed}])
        (is (= ["a" "b"] (mapv lv/view-id (:live-views @state/app-db)))
            "nothing is retired: every record remains reachable")
        (let [runs (:runs (second (:messages @state/app-db)))]
          (is (= [{:iteration-index 0 :form-index 0} {:iteration-index 1 :form-index 0}]
                 (mapv :anchor runs))
              "each row returns to the form active when its view opened")
          (is (= [:completed :failed] (mapv :reason runs))))
        (state/dispatch [:live-view-reopen "a"])
        (is (true? (get-in @state/app-db [:messages 1 :runs 0 :is-reopened]))
            "opening the record flips the transcript disclosure open")
        (state/dispatch [:live-view-reopen "a"])
        (is (false? (get-in @state/app-db [:messages 1 :runs 0 :is-reopened]))
            "pressing it again collapses both record and disclosure"))))
  ;; The proof a person can LOOK at: one run finished and gone from the band, the
  ;; one still going painting in full.
  (testing "a real PNG of the band a finished run has already left"
    (let [png (cap/shot!
                {:cols 96
                 :rows 24
                 :font-size 14
                 :out "vis-live-view-settled"
                 :paint!
                 (fn [{:keys [screen]}]
                   (let [g (.newTextGraphics ^TerminalScreen screen)]
                     (lv/paint! g 96 24 [(ended (pane :id "done-1")) (pane :id "running-1")] 1 3)
                     (.refresh ^TerminalScreen screen)))})]
      (is (str/ends-with? png "vis-live-view-settled.png"))
      (is (pos? (long (cap/ink png))) "the band belongs entirely to the run still going"))))

(defn- activity-view
  ([] (activity-view "activity-1" 6))
  ([id] (activity-view id 6))
  ([id step-count]
   (-> (apply mounted
              {:id id}
              (hi/status "now" "Polling the run" {:label "Now" :tone :running})
              (hi/stat "counts"
                       [{:id "succeeded" :label "Succeeded" :value-text "2" :tone :ok}
                        {:id "running" :label "Running" :value-text "1" :tone :running}
                        {:id "failed" :label "Failed" :value-text "0" :tone :idle}])
              [(hi/steps "operations"
                         (mapv (fn [idx]
                                 {:id (str "op-" idx)
                                  :label (str (if (zero? (mod idx 3)) "Run tests" "Inspect source")
                                              " · operation "
                                              (inc idx))
                                  :detail (str "Evidence for operation "
                                               (inc idx)
                                               ": bounded terminal-native detail")
                                  :tone (if (= idx (dec step-count)) :running :ok)})
                               (range step-count)))])
       (assoc :title "Activity"
              :classification :activity))))

(defn- settled-activity
  "A reopened Activity carrying the terminal projection the host seals into its receipt."
  [outcome]
  (let [{:keys [reason status tone counts final-tone final-detail]}
        (case outcome
          :succeeded
          {:reason :completed
           :status "succeeded · 6 settled · 0 running"
           :tone :ok
           :counts {"running" "0" "succeeded" "6" "failed" "0" "cancelled" "0"}
           :final-tone :ok
           :final-detail "18 ms"}

          :failed
          {:reason :failed
           :status "failed · 6 settled · 0 running"
           :tone :error
           :counts {"running" "0" "succeeded" "5" "failed" "1" "cancelled" "0"}
           :final-tone :error
           :final-detail "Command failed"}

          :cancelled
          {:reason :interrupted
           :status "cancelled · 6 settled · 0 running"
           :tone :warn
           :counts {"running" "0" "succeeded" "5" "failed" "0" "cancelled" "1"}
           :final-tone :warn
           :final-detail "Cancelled"})

        pane
        (lv/opened (activity-view (str "activity-" (name outcome))))

        nodes
        (mapv (fn [node]
                (case (:type node)
                  :status
                  (assoc node
                    :text status
                    :tone tone)

                  :stat
                  (update node
                          :stats
                          (fn [stats]
                            (mapv (fn [stat]
                                    (assoc stat :value-text (get counts (:id stat))))
                                  stats)))

                  :steps
                  (update node
                          :steps
                          (fn [steps]
                            (mapv (fn [idx step]
                                    (if (= idx (dec (count steps)))
                                      (assoc step
                                        :tone final-tone
                                        :detail final-detail)
                                      (assoc step :tone :ok)))
                                  (range)
                                  steps)))

                  node))
              (get-in pane [:view :nodes]))]

    (-> (ended pane {:reason reason :view {:nodes nodes}})
        lv/reopened)))

(deftest activity-transcript-receipt-test
  (testing "Activity starts as one transcript receipt and never becomes an independent stop target"
    (let [p
          (lv/opened (activity-view))

          row
          (lv/run-row p)]

      (is (lv/activity? p))
      (is (lv/dormant? p) "the default receipt gives no rows to the live band")
      (is (nil? (lv/interruptible [p])) "turn cancellation remains the only stop action")
      (is (:is-activity row))
      (is (= "Polling the run" (:status-text row)))
      (is (= :activity (get-in p [:view :classification])))))
  (testing "explicit disclosure opens a dedicated bounded Activity rail and folds it again"
    (let [p
          (lv/opened (activity-view))

          open
          (lv/reopened p)

          text
          (painted-text [open])

          controls
          (regions-of [open])]

      (is (not (lv/dormant? open)))
      (is (:is-reopened open))
      (is (str/includes? text "ACTIVITY · LIVE"))
      (is (str/includes? text "Run tests · operation 1"))
      (is (not (str/includes? text "CI · fix(loop)"))
          "Activity never inherits generic Live View title chrome")
      (is (some #(and (= :live-reopen (:kind %)) (= "activity-1" (:view-id %))) controls)
          "the dedicated title control folds the transcript disclosure")
      (is (nil? (lv/interruptible [open])))
      (is (not-any? #(= "Esc" (first %)) (lv/hint open []))
          "expanded Activity never advertises an impossible independent interrupt")
      (is (lv/dormant? (lv/reopened open)))
      (is (str/includes? (painted-text [open] 48 20) "ACTIVITY · LIVE")
          "the dedicated rail retains its identity at narrow widths")
      (let [png (cap/shot! {:cols 96
                            :rows 24
                            :font-size 14
                            :out "vis-activity-expanded"
                            :paint! (fn [{:keys [screen]}]
                                      (let [g (.newTextGraphics ^TerminalScreen screen)]
                                        (lv/paint! g 96 24 [open] 1 3)
                                        (.refresh ^TerminalScreen screen)))})]
        (is (str/ends-with? png "vis-activity-expanded.png"))
        (is (pos? (long (cap/ink png))) "the approved expanded Activity state really painted")))))



;; Regression, td-5d63b6: expanded Activity reused the generic four-fifths Live View
;; shell, and its screenshot assertion proved only that the PNG contained some ink.
(deftest activity-dedicated-painter-test
  (let [running
        (lv/reopened (lv/opened (activity-view)))

        settled
        (settled-activity :succeeded)

        failed
        (settled-activity :failed)

        cancelled
        (settled-activity :cancelled)]

    (testing "running and settled Activity use the same bounded transcript geometry"
      (doseq [terminal-rows [24 40]]
        (let [[from to] (lv/band-rows 96 terminal-rows [running] 1 3)]
          (is (= 10 (inc (- (long to) (long from))))
              (str "Activity stays ten rows on a " terminal-rows "-row terminal"))))
      (is (= (lv/band-rows 96 24 [running] 1 3) (lv/band-rows 96 24 [settled] 1 3))
          "settlement changes semantics, never expanded geometry"))
    (testing "the grid carries only Activity chrome and the semantic rail"
      (let [running-lines
            (str/split-lines (painted-text [running] 96 24))

            settled-lines
            (str/split-lines (painted-text [settled] 96 24))

            [from to]
            (lv/band-rows 96 24 [running] 1 3)

            painted
            (subvec (vec running-lines) from (inc to))]

        (is (str/starts-with? (str/triml (nth running-lines from)) "▎ ▾ ACTIVITY · LIVE"))
        (is (str/starts-with? (str/triml (nth settled-lines from))
                              "▎ ▾ ACTIVITY · SETTLED · succeeded · 6 settled · 0 running"))
        (is (str/includes? (painted-text [failed] 96 24)
                           "ACTIVITY · SETTLED · failed · 6 settled · 0 running")
            "failure remains failure after the receipt settles")
        (is (str/includes? (painted-text [cancelled] 96 24)
                           "ACTIVITY · SETTLED · cancelled · 6 settled · 0 running")
            "cancellation is never mislabeled completed")
        (is (not (str/includes? (painted-text [settled] 96 24) "Running 1"))
            "the settled proof carries the terminal counts rather than the opening snapshot")
        (is (every? #(str/starts-with? (str/triml %) "▎") painted)
            "every owned row has the one semantic edge")
        (is (not (str/includes? (str/join "\n" painted) "close Activity"))
            "there is no persistent generic hint footer")
        (is (every? str/blank? (take from running-lines))
            "nothing is painted above the ten rows the wheel claims")))
    (testing "narrow, scrolling, evidence, and focus are terminal-grid behavior"
      (let [narrow
            (painted-text [running] 58 20)

            many
            (-> (lv/opened (activity-view "activity-many" 24))
                lv/reopened)

            first-frame
            (paint-frames [many] 96 24)

            measured
            (lv/painted many (:geometry first-frame))

            parked
            (lv/scrolled measured -5)

            parked-frame
            (paint-frames [parked] 96 24)

            focus-pane
            (-> (lv/opened (activity-view "activity-focus" 8))
                lv/reopened
                (lv/activity-evidence-toggled "op-2"))

            controls
            (regions-of [focus-pane])]

        (is (str/includes? narrow "ACTIVITY · LIVE") "the headline survives at 58 columns")
        (is (every? #(<= (count %) 58) (str/split-lines narrow))
            "nothing crosses the terminal grid")
        (is (> (long (get-in first-frame [:geometry :total]))
               (long (get-in first-frame [:geometry :visible])))
            "long evidence is bounded by a real viewport")
        (is (< (long (get-in parked-frame [:geometry :offset]))
               (long (get-in first-frame [:geometry :offset])))
            "wheel movement reveals earlier evidence")
        (is (some #(= :activity-evidence (:kind %)) controls)
            "an operation row discloses its bounded evidence")
        (is (some #(= :activity-focus (:kind %)) controls)
            "an operation row has Activity-local focus")
        (let [focus-grid (painted-text [focus-pane] 96 24)]
          (is (str/includes? focus-grid "›") "the focused operation has a visible marker")
          (is (str/includes? focus-grid "↳ Evidence for operation 3")
              "disclosed evidence occupies exactly one presenter row"))))
    (testing "the approved states are captured by DefaultVirtualTerminal"
      (let [scrolling
            (-> (lv/opened (activity-view "activity-shot-scroll" 24))
                lv/reopened
                (lv/painted (:geometry (paint-frames
                                         [(lv/reopened
                                            (lv/opened (activity-view "activity-shot-scroll" 24)))]
                                         96
                                         24)))
                (lv/scrolled -5))

            focus
            (-> (lv/opened (activity-view "activity-shot-focus" 8))
                lv/reopened
                (lv/activity-evidence-toggled "op-2"))]

        (doseq [[name cols terminal-rows pane]
                [["vis-activity-running" 96 24 running] ["vis-activity-settled" 96 24 settled]
                 ["vis-activity-failed" 96 24 failed] ["vis-activity-cancelled" 96 24 cancelled]
                 ["vis-activity-narrow" 58 20 running] ["vis-activity-scrolling" 96 24 scrolling]
                 ["vis-activity-focus" 96 24 focus]]]
          (let [png (cap/shot! {:cols cols
                                :rows terminal-rows
                                :font-size 14
                                :out name
                                :paint! (fn [{:keys [screen]}]
                                          (let [g (.newTextGraphics ^TerminalScreen screen)]
                                            (lv/paint! g cols terminal-rows [pane] 1 3)
                                            (.refresh ^TerminalScreen screen)))})]
            (is (str/ends-with? png (str name ".png")))))))
    (testing "fold returns all expanded rows to the transcript receipt"
      (is (lv/dormant? (lv/reopened running)))
      (is (nil? (lv/band-rows 96 24 [(lv/reopened running)] 1 3))))
    (testing "the expanded surface floats directly under its collapsed receipt"
      ;; Regression, td-2d89a0: the expanded Activity band was docked above the
      ;; prompt, so it drifted away from the collapsed anchor row it belongs to
      ;; as the transcript scrolled.
      (let [[from to] (lv/band-rows 96 40 [running] 1 3 8)]
        (is (= 9 (long from)) "the surface opens one row under the anchor")
        (is (= 10 (inc (- (long to) (long from)))) "and keeps its ten-row bound"))
      (is (= (lv/band-rows 96 40 [running] 1 3) (lv/band-rows 96 40 [running] 1 3 100))
          "an anchor at or below the dock leaves the docked geometry alone"))))

(deftest activity-transcript-state-test
  (testing "open, patch, and close replace one anchored Activity row"
    (with-db
      (fn []
        (swap! state/app-db assoc
          :messages [{:role :user :text "work"} {:role :assistant :text "working"}]
          :progress {:iterations [{:forms [{:code "await work()"}]}]})
        (let [view (assoc (activity-view) :session-id "s1")]
          (state/dispatch [:live-view-open view])
          (is (= 1 (count (get-in @state/app-db [:messages 1 :runs]))))
          (is (= {:iteration-index 0 :form-index 0}
                 (get-in @state/app-db [:messages 1 :runs 0 :anchor])))
          (state/dispatch [:activity-focus "activity-1" "op-2"])
          (state/dispatch [:activity-evidence "activity-1" "op-2"])
          (let [pane (first (:live-views @state/app-db))]
            (is (= "op-2" (:activity-focused pane)))
            (is (= #{"op-2"} (:activity-evidence pane))))
          (state/dispatch
            [:live-view-patch
             (engine/normalize-patch
               view
               [{:op :set :node-id "now" :text "1 settled · 1 running" :tone :running}])])
          (is (= "1 settled · 1 running" (get-in @state/app-db [:messages 1 :runs 0 :status-text])))
          (state/dispatch [:live-view-close "activity-1" {:reason :completed}])
          (is (= 1 (count (get-in @state/app-db [:messages 1 :runs]))))
          (is (= :completed (get-in @state/app-db [:messages 1 :runs 0 :reason]))))))))
