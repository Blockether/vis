(ns com.blockether.vis.tui.live-view-test
  "The live-view pane: what it paints, where the eye stays while the view changes
   underneath it, and what Escape hits while one is open.

   Every view here is DECLARED through the public builders and normalized by the
   engine, so a test can only paint shapes an extension can really produce."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.columns :as columns]
            [com.blockether.vis.tui.footer :as footer]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.live-view-fixture :as fixture]
            [com.blockether.vis.tui.view-materializer :as live]
            [com.blockether.vis.contract.view :as hi-spec]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

;;; ── The views under test ────────────────────────────────────────────────────

(defn- mounted
  "A view as the engine mounts one: declared through the public builders, checked
   by the engine, materialized, and stamped with the id its patches name."
  [opts & nodes]
  (-> (apply fixture/view
             (merge {:title "CI · fix(loop): move the session pick"
                     :description "Blockether/vis · 32062760734"}
                    (dissoc opts :id))
             nodes)
      fixture/normalize-live-view
      live/materialize
      (assoc :id (or (:id opts) "view-1")
             :seq 0
             :created-at (System/currentTimeMillis))))

(defn- patched
  "`pane` after one patch carrying `ops` — normalized by the engine, so the seq
   advances and the ops are the ones a real patch would carry."
  [pane & ops]
  (lv/patched pane (fixture/normalize-patch (:view pane) (vec ops))))

(defn- job-rows
  [n]
  (mapv #(fixture/table-row (str "job-" %) [(str "job-" %) "success" (str % "m0s")]) (range n)))

(defn- jobs
  ([] (jobs {} 4))
  ([opts n]
   (fixture/table "jobs"
                  [(fixture/table-column "job" "Job") (fixture/table-column "state" "State")
                   (fixture/table-column "took" "Took")]
                  (merge {:label "Jobs" :rows (job-rows n)} opts))))

(defn- ci-view
  [& {:keys [rows order id]}]
  (mounted {:id id}
           (fixture/status "now" "Polling the run" {:label "Now" :tone :running})
           (jobs (cond-> {}
                   order
                   (assoc :order order))
                 (or rows 4))
           (fixture/log "tail"
                        {:label "Output"
                         :default-expanded true
                         :lines ["> clojure -M:test" "Ran 314 tests"]})))

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
                                  (.beginFrame interactions/hit-map)
                                  (let [g (.newTextGraphics ^TerminalScreen screen)]
                                    (reset! geom (lv/paint! g cols rows panes 1 3))
                                    (.commitFrame interactions/hit-map)
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

(defdescribe live-view-plan-test
             (it "every node paints under its own label, in declaration order"
                 (let [text (str/join "\n" (map :text (rows-of (pane))))]
                   (expect (str/includes? text "Now"))
                   (expect (str/includes? text "Polling the run"))
                   (expect (str/includes? text "Jobs"))
                   (expect (str/includes? text "job-0"))
                   (expect (str/includes? text "Output"))
                   (expect (str/includes? text "Ran 314 tests"))
                   (expect (< (.indexOf ^String text "Now") (.indexOf ^String text "Jobs"))
                           "declaration order, not arrival order")))
             (it "the view's own description opens the surface"
                 (expect (= "Blockether/vis · 32062760734" (:text (first (rows-of (pane)))))))
             (it "an empty table is still a box, and says so between its own rails"
                 (let [p
                       (lv/opened (mounted {} (jobs {} 0)))

                       plan
                       (rows-of p)]

                   (expect
                     (= [:prose :blank :node :trule :thead :trule :empty :trule] (mapv :kind plan))
                     "top rail, the header, a rail, the sentence, and the box closed under it")
                   (expect (str/includes? (:text (nth plan 4)) "Job"))
                   (expect (str/includes? (:text (nth plan 6)) "no rows yet")
                           "the sentence stands INSIDE the box, between its rails"))))

(defdescribe live-view-window-test
             (it "a node paints a window and says how much it is holding back"
                 (let [p
                       (pane :rows 20)

                       more
                       (first (kinds-of p :more))]

                   (expect (= lv/node-window (count (kinds-of p :trow))))
                   (expect (= "+ 8 more rows" (:text more)))
                   (expect (= "jobs" (:node-id more)))))
             (it "expanding is a click on that line, and it shows everything"
                 (let [p (lv/expanded (pane :rows 20) "jobs")]
                   (expect (= 20 (count (kinds-of p :trow))))
                   (expect (empty? (kinds-of p :more)))))
             (it "a log answers its TAIL — the newest lines are why anybody watches"
                 (let [p
                       (lv/opened (mounted {}
                                           (fixture/log "tail"
                                                        {:label "Output"
                                                         :default-expanded true
                                                         :lines (mapv #(str "line " %)
                                                                      (range 40))})))

                       lines
                       (mapv :text (filterv #(= :log (:kind %)) (rows-of p)))]

                   (expect (= 40 (count lines)))
                   (expect (= "line 39" (last lines)))
                   (expect (= "line 0" (first lines)))
                   (expect (empty? (kinds-of p :note))))))

(defdescribe live-view-follow-test
             (it "a fresh pane follows the end" (expect (:is-following (pane))))
             (it "scrolling up releases follow; landing back at the bottom re-arms it"
                 (let [p
                       (-> (pane :rows 30)
                           (lv/painted {:offset 40 :total 60 :visible 20}))

                       up
                       (lv/scrolled p -6)]

                   (expect (not (:is-following up)))
                   (expect (= 34 (:offset up)))
                   (expect (:is-following (lv/scrolled up 6)))
                   (expect (:is-following (lv/scrolled up 999)) "the wheel cannot run past the end")
                   (expect (= 40 (:offset (lv/scrolled up 999))))))
             ;; Reported in Vis session 22b3489b-336f-42d0-9bc8-806dff2de86f: a stray tick of
             ;; trackpad inertia re-armed follow-tail and snapped the pane to the live edge
             ;; mid-read.
             (it "an upward gesture that only CLAMPS at the end does not arm follow"
                 (let [shrunk (-> (pane :rows 30)
                                  (lv/painted {:offset 40 :total 60 :visible 20})
                                  (lv/scrolled -6)
                                  (lv/painted {:offset 34 :total 34 :visible 20}))]
                   (expect (= 14 (:offset (lv/scrolled shrunk -6))))
                   (expect (not (:is-following (lv/scrolled shrunk -6))))))
             (it "a view with nothing to scroll keeps following through a stray tick"
                 (let [tiny (-> (pane :rows 3)
                                (lv/painted {:offset 0 :total 3 :visible 20}))]
                   (expect (:is-following (lv/scrolled tiny -1)))))
             (it "a following pane sits at the end of whatever the plan is now"
                 (let [p
                       (pane :rows 30)

                       rows
                       (rows-of p)]

                   (expect (= (max 0 (- (count rows) 8)) (lv/offset p rows 8))))))

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

(defdescribe
  live-view-anchor-test
  (it "rows arriving ABOVE the eye move the scrollbar, not the reading position"
      (let [p
            (-> (pane :rows 12 :order :newest-first)
                (lv/expanded "jobs"))

            [parked eye anchor]
            (parked p 6 (trow-index p 4))

            grown
            (patched parked
                     {:op :append
                      :node-id "jobs"
                      :rows (mapv #(fixture/table-row (str "new-" %) [(str "new-" %) "queued" "0s"])
                                  (range 3))})

            grown-rows
            (rows-of grown)]

        (expect (= "jobs" (first anchor)) "the anchor names the node the eye is inside")
        (expect (str/starts-with? (str (second anchor)) "job-")
                "and the ROW ID under it, never a line offset")
        (expect (not (:is-following parked)) "reading back is a deliberate intent")
        (expect (= 6 (- (long (lv/offset grown grown-rows 6)) (long eye)))
                "three rows landed above it, and each brings the rail that separates it")
        (expect (= anchor (lv/anchor-at grown-rows (lv/offset grown grown-rows 6)))
                "and the same row is still the top visible one")))
  (it "a removed row above the eye pulls the viewport back with it"
      (let [p
            (-> (pane :rows 12 :order :newest-first)
                (lv/expanded "jobs"))

            [parked eye anchor]
            (parked p 6 (trow-index p 4))

            cut
            (patched parked {:op :remove :node-id "jobs" :item-ids ["job-11" "job-10"]})

            cut-rows
            (rows-of cut)]

        (expect (= (- (long eye) 4) (long (lv/offset cut cut-rows 6)))
                "two rows left, and the two rails between them left with them")
        (expect (= anchor (lv/anchor-at cut-rows (lv/offset cut cut-rows 6))))))
  (it "an anchor whose row is gone falls back to the node it belonged to"
      (let [p
            (-> (pane :rows 12)
                (lv/expanded "jobs"))

            [parked _ anchor]
            (parked p 6 (trow-index p 4))

            cut
            (patched parked {:op :remove :node-id "jobs" :item-ids [(second anchor)]})

            cut-rows
            (rows-of cut)]

        (expect (= "jobs" (first (lv/anchor-at cut-rows (lv/offset cut cut-rows 6))))
                "the eye lands inside the same node instead of jumping to the top")))
  (it "a viewport pinned at the end follows new rows"
      (let [p
            (pane :rows 4)

            rows
            (rows-of p)

            following
            (lv/painted p {:offset 0 :total (count rows) :visible 6})

            grown
            (patched following
                     {:op :append
                      :node-id "jobs"
                      :rows [(fixture/table-row "job-9" ["job-9" "running" "1s"])]})

            grown-rows
            (rows-of grown)]

        (expect (:is-following grown))
        (expect (= (- (count grown-rows) 6) (long (lv/offset grown grown-rows 6)))))))

(defdescribe
  live-log-scroll-keeps-its-line-test
  (it
    "live log scroll keeps its line"
    (let [p
          (lv/opened (mounted {:title "Build log" :description ""}
                              (fixture/log "tail"
                                           {:label "Output"
                                            :default-expanded true
                                            :lines (mapv #(str "line " %) (range 80))})))

          initial
          (:geometry (paint-frames [p] 80 30))

          up
          (lv/scrolled (lv/painted p initial) -9)

          first-capture
          (paint-frames [up] 80 30)

          first-paint
          (:geometry first-capture)

          parked
          (lv/painted up first-paint)

          same-capture
          (paint-frames [parked] 80 30)

          same-paint
          (:geometry same-capture)

          grown
          (patched parked {:op :append :node-id "tail" :lines ["line 80"]})

          grown-paint
          (:geometry (paint-frames [grown] 80 30))

          thumb-rows
          (fn [capture]
            (keep-indexed (fn [row line]
                            (when (str/includes? line "█") row))
                          (str/split-lines (cap/frame-text (last (:frames capture))))))]

      (expect (< 1 (long (:offset first-paint))) "the viewport starts inside the log")
      (expect (= (:offset first-paint) (:offset same-paint))
              "a second paint must not jump from the current log line back to its header")
      (expect (seq (thumb-rows first-capture)) "the long log has a visible scrollbar thumb")
      (expect (= (thumb-rows first-capture) (thumb-rows same-capture))
              "the scrollbar thumb stays put when the same log line is being read")
      (expect (= (:offset first-paint) (:offset grown-paint))
              "new output must not move the line being read while follow-tail is off")
      (expect (= (inc (long (:offset first-paint)))
                 (:offset (:geometry (paint-frames [(lv/scrolled parked 1)] 80 30))))
              "one downward step moves exactly one log line"))
    ;; a late page adds older log lines without moving the line being read
    (let [p
          (lv/opened (mounted {:description ""}
                              (fixture/log "tail"
                                           {:label "Output"
                                            :default-expanded true
                                            :lines (mapv #(str "line " %) (range 80))})))

          lines
          (get-in p [:view :nodes 0 :lines])

          tail
          (assoc-in p [:view :nodes 0 :lines] (subvec lines 40))

          target
          (first (keep-indexed #(when (= "line 45" (:text %2)) %1) (rows-of tail)))

          [reading eye anchor]
          (parked tail 6 target)

          filled
          (update reading :view live/log-head-filled "tail" (subvec lines 0 40) [])

          rows
          (rows-of filled)]

      (expect (= ["tail" [:line 45]] anchor))
      (expect (= (+ eye 39) (lv/offset filled rows 6)))
      (expect (= "line 45" (:text (nth rows (lv/offset filled rows 6))))))))

(defdescribe
  live-view-widths-test
  (it
    "a column measured wide stays wide while the view is open"
    (let [p
          (-> (pane :rows 3)
              (lv/expanded "jobs"))

          wide
          (patched p
                   {:op :append
                    :node-id "jobs"
                    :rows [(fixture/table-row "job-1" ["job-1" "a state nobody expected" "1m0s"])]})

          measured
          (:widths (meta (rows-of wide)))

          taught
          (lv/painted wide {:offset 0 :total 9 :visible 9 :widths measured})

          narrow
          (patched
            taught
            {:op :append :node-id "jobs" :rows [(fixture/table-row "job-1" ["job-1" "ok" "1m0s"])]})

          after
          (:widths (meta (rows-of narrow)))]

      (expect (= (get-in measured ["jobs"]) (get-in after ["jobs"]))
              "a shorter value never shuffles the columns the human already read")
      (expect (> (long (second (get measured "jobs"))) (count "a state"))))))

(defdescribe live-view-fresh-test
             (it "what the LAST patch touched is what is emphasised, and only until the next one"
                 (let [p
                       (pane :rows 3)

                       one
                       (patched p
                                {:op :append
                                 :node-id "jobs"
                                 :rows [(fixture/table-row "job-9" ["job-9" "running" "1s"])]})

                       two
                       (patched one {:op :set :node-id "now" :text "Still polling"})]

                   (expect (some #(and (= "job-9" (:item-id %)) (:is-fresh %)) (rows-of one)))
                   (expect (not-any? #(and (= "job-9" (:item-id %)) (:is-fresh %)) (rows-of two))
                           "the next patch clears it — no timer, no repaint that erases itself"))))

(defdescribe
  live-view-escape-precedence-test
  (it "the stop selector chooses the newest pane supplied by its caller"
      (let [a
            (pane :id "view-a")

            b
            (pane :id "view-b")]

        (expect (= "view-b" (lv/view-id (lv/interruptible [a b]))))
        (expect (nil? (lv/interruptible [])))))
  (it "EVERY open view answers the key — a view asks nothing, so none may refuse to stop"
      (let [p (lv/opened (ci-view))]
        (expect (= (lv/view-id p) (lv/view-id (lv/interruptible [p]))))))
  (it "hidden panes never advertise details or steal the turn cancel hint"
      (let [row (first (#'footer/echo-segments {:loading? true :live-views [(pane)]}))]
        (expect (str/includes? (:text row) "cancel"))
        (expect (not (str/includes? (:text row) "CI · fix(loop): move the session pick")))))
  (it "only the selected transient advertises its controls"
      (let [p
            (pane)

            db
            {:loading? true :live-views [p] :live-viewer-id (lv/view-id p)}]

        (expect (= "Esc close live view · F3 controls" (:text (first (#'footer/echo-segments db)))))
        (expect (str/includes? (:text (first (#'footer/echo-segments
                                              (assoc db :live-views [(lv/armed p)]))))
                               "Esc or Enter interrupt"))))
  (it "with no view open the row goes back to the turn's own hint"
      (expect (str/includes? (:text (first (#'footer/echo-segments
                                            {:loading? true :live-views []})))
                             "cancel"))))

(defdescribe
  live-view-stop-note-test
  (it "Escape ARMS the stop — and Escape again is what sends it"
      (let [p (lv/armed (lv/opened (ci-view)))]
        (expect (= "" (lv/stopping p)) "armed, with nothing typed into it yet")
        (expect (= "" (:note (lv/stop-prompt p))))
        (expect (str/includes? (:label (lv/stop-prompt p)) "CI · fix(loop): move the session pick")
                "the prompt names the view it would stop, because several may be open")
        (expect (nil? (lv/stop-prompt (lv/opened (ci-view)))) "a watched view asks nothing")
        (expect (nil? (lv/stopping (lv/disarmed p))) "and `disarmed` is the way back")))
  (it
    "the note is typed with the same keyboard a form's fields read"
    (let [typing
          (fn [pane text]
            (reduce (fn [pane c]
                      (:pane (lv/typed pane {:kind :char :char c})))
                    pane
                    text))

          p
          (typing (lv/armed (lv/opened (ci-view))) "wrong subnet")]

      (expect (= "wrong subnet" (lv/stopping p)))
      (expect (= "wrong subne" (lv/stopping (:pane (lv/typed p {:kind :backspace})))))
      (let [{:keys [pane action note]} (lv/typed p {:kind :enter})]
        (expect (= :stop action))
        (expect (= "wrong subnet" note) "the words travel WITH the stop, not after it")
        (expect (nil? (lv/stopping pane)) "the line is gone the moment the stop is sent"))
      (let [{:keys [action note]} (lv/typed (lv/armed (lv/opened (ci-view))) {:kind :enter})]
        (expect (= :stop action) "a stop with no words is still a stop")
        (expect (nil? note)))
      (let [{:keys [pane action note]} (lv/typed p {:kind :cancel})]
        (expect (= :stop action)
                "Escape STOPS: the key the human reached for to kill the run does that")
        (expect (= "wrong subnet" note) "and it carries whatever was written by then")
        (expect (nil? (lv/stopping pane))))
      (let [{:keys [pane action]} (lv/typed p {:kind :backspace})]
        (expect (nil? action) "Backspace erases while there is something to erase")
        (expect (= "wrong subne" (lv/stopping pane))))
      (let [{:keys [pane action]} (lv/typed (lv/armed (lv/opened (ci-view))) {:kind :backspace})]
        (expect (= :keep action) "Backspace on an EMPTY line is the one key that can only undo")
        (expect (nil? (lv/stopping pane)) "keeping watch forgets the armed stop"))
      (expect (nil? (:action (lv/typed p {:kind :next})))
              "a key the line has no use for changes nothing")))
  (it "the line stops growing where the engine cuts it"
      (let [p (reduce (fn [pane c]
                        (:pane (lv/typed pane {:kind :char :char c})))
                      (lv/armed (lv/opened (ci-view)))
                      (repeat (+ 10 (long hi-spec/note-chars)) \x))]
        (expect (= (long hi-spec/note-chars) (count (lv/stopping p)))
                "a field that swallowed more than the model will read would lie about the note")))
  (it
    "the band gives the armed line a row of its own, above the fence"
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

      (expect (not (str/includes? watched "why?")) "a view being watched asks nothing")
      (expect (str/includes? armed "why? wrong subnet") "the words are on screen as they are typed")
      (expect (str/includes? armed "interrupt CI · fix(loop): move the session pick")
              "and the line says WHICH view they will stop")
      (expect (str/includes? empty-armed "Backspace keep watching")
              "with nothing typed the bar offers the way back")
      (expect (str/includes? armed "Esc / Enter interrupt with the note")
              "once there are words the bar says they will travel with the stop")
      (expect (str/includes? armed "Backspace erase")
              "and Backspace is the key that takes them away again")
      (let [lines
            (str/split-lines armed)

            at
            (first (keep-indexed (fn [idx line]
                                   (when (str/includes? line "why?") idx))
                                 lines))]

        (expect
          (str/includes? (nth lines (dec (long at)) "") "─")
          "the line is FENCED above as well as below: it is the band asking, not one more row of the report"))
      (expect (str/includes? watched "Esc interrupt CI · fix(loop): move the session pick")
              "while it is only being watched the bar still advertises the one key it takes"))))

;;; ── The registry half: three ops and the tab that owns them ─────────────────

(defn- with-db
  [f]
  (let [old @state/app-db]
    (try (reset! state/app-db {:render-version 0 :session {:id "s1"}})
         (f)
         (finally (reset! state/app-db old)))))

(defdescribe
  live-view-gateway-wire-patch-test
  (it "live view gateway wire patch"
      ;; Regression: the gateway advanced while the TUI kept the opening CI picture.
      ;; Existing pane tests passed keyword ops directly, bypassing the JSON boundary.
      (with-db
        (fn []
          (let [view
                (assoc (ci-view :rows 1) :session-id "s1")

                patch
                {:view-id (:id view)
                 :seq 1
                 :ops [{:op :set :node-id "now" :text "Package Linux x64 failed" :tone :error}
                       {:op :append
                        :node-id "jobs"
                        :rows [{:id "job-0" :cells ["job-0" "failure" "5m"] :tone :error}]}]}

                chunk
                (#'chat/gateway-event->chunk
                 {"type" "view.patch" "kind" "live" "patch" (wire/->wire patch)})]

            (state/dispatch [:live-view-open (hi/live-view<-wire (wire/->wire view))])
            (state/dispatch [:live-view-patch (hi/live-patch<-wire (:patch chunk))])
            (let [panes
                  (:live-views @state/app-db)

                  updated
                  (:view (first panes))

                  frame
                  (painted-text panes 96 40)]

              (expect (= 1 (:seq updated)))
              (expect (= "Package Linux x64 failed" (get-in updated [:nodes 0 :text])))
              (expect (= :error (get-in updated [:nodes 1 :rows 0 :tone])))
              (expect (str/includes? frame "Package Linux x64 failed"))
              (expect (str/includes? frame "failure"))
              (state/dispatch [:live-view-patch (hi/live-patch<-wire (:patch chunk))])
              (expect (= panes (:live-views @state/app-db))
                      "replayed frames remain idempotent")))))))

(defdescribe
  live-view-wire-operation-vocabulary-test
  (it "live view wire operation vocabulary"
      (let [ops
            [{:op :set :node-id "now" :text "set"} {:op :append :node-id "tail" :lines ["append"]}
             {:op :clear :node-id "tail"} {:op :remove :node-id "jobs" :ids ["job-0"]}
             {:op :add-node :after "now" :node-spec {:id "extra" :type :status :text "clear"}}
             {:op :remove-node :node-id "extra"}]

            patch
            {:view-id "view-1" :seq 1 :ops ops}]

        (expect (= patch (hi/live-patch<-wire (wire/->wire patch)))
                "all operation enums are restored without retyping text, lines or ids"))))

(defdescribe
  live-view-state-test
  (it "open, patch, close — the three ops the channel carries"
      (with-db
        (fn []
          (let [view (assoc (ci-view) :session-id "s1")]
            (state/dispatch [:live-view-open view])
            (expect (= ["view-1"] (mapv lv/view-id (:live-views @state/app-db))))
            (state/dispatch [:live-view-patch
                             (fixture/normalize-patch view
                                                      [{:op :set :node-id "now" :text "Done"}])])
            (expect (= "Done" (:text (first (:nodes (:view (first (:live-views @state/app-db)))))))
                    "the ENGINE advanced the view; the terminal never interprets a patch itself")
            (state/dispatch [:live-view-minimize "view-1"])
            (expect (lv/minimized? (first (:live-views @state/app-db)))
                    "minimizing is terminal-local pane state, not a close")
            (state/dispatch [:live-view-restore "view-1"])
            (expect (not (lv/minimized? (first (:live-views @state/app-db)))))
            (state/dispatch [:live-view-close "view-1" {:reason :completed}])
            (expect (lv/settled? (first (:live-views @state/app-db)))
                    "the close ends the view and leaves the line that reopens it")
            (expect
              (lv/dormant? (first (:live-views @state/app-db)))
              "collapsed until the human presses it: the band's rows go back to the transcript")))))
  (it "one view mounted twice keeps the pane the human is scrolling"
      (with-db (fn []
                 (let [view (assoc (ci-view :rows 20) :session-id "s1")]
                   (state/dispatch [:live-view-open view])
                   (state/dispatch [:live-view-scroll "view-1" -4])
                   (state/dispatch [:live-view-expand "view-1" "jobs"])
                   (let [before (first (:live-views @state/app-db))]
                     (state/dispatch [:live-view-open view])
                     (expect (= 1 (count (:live-views @state/app-db))))
                     (expect (= before (first (:live-views @state/app-db)))
                             "a duplicate open is not a reset"))))))
  (it "a view belonging to a session this terminal is not showing is dropped"
      (with-db (fn []
                 (state/dispatch [:live-view-open (assoc (ci-view) :session-id "somewhere-else")])
                 (expect (empty? (:live-views @state/app-db))))))
  (it "a close for a view that was never mounted is a no-op, not a throw"
      (with-db (fn []
                 (state/dispatch [:live-view-close "never-here"])
                 (expect (empty? (:live-views @state/app-db)))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: the live
;; surface used half the available terminal and its selectable job rows had no control.
(defdescribe
  live-view-height-and-selection-test
  (it "a busy live surface takes four fifths of the room above the composer"
      (let [[top bottom]
            (lv/band-rows 96 26 [(pane :rows 20)] 1 3)

            height
            (inc (- (long bottom) (long top)))

            available
            (- 26 1 3)]

        (expect (>= (* 5 height) (* 4 available))
                "the watched run, rather than stale transcript, owns most of the terminal")))
  (it "a selectable table paints shared selection and makes every visible row clickable"
      (let [p
            (lv/opened (mounted {} (jobs {:is-selectable true :selected-ids ["job-1"]} 8)))

            plan
            (lv/plan p 80)

            table-rows
            (filterv #(= :trow (:kind %)) plan)]

        (expect (= [{:item-id "job-0" :is-selectable true :is-selected false}
                    {:item-id "job-1" :is-selectable true :is-selected true}]
                   (mapv #(select-keys % [:item-id :is-selectable :is-selected])
                         (take 2 table-rows))))
        (expect (some #{["click" "select a row"]} (lv/hint p []))
                "the band advertises the mouse control without taking the composer keyboard")
        (.reset interactions/hit-map)
        (try (let [text
                   (cap/frame-text (last (:frames (paint-frames [p] 96 80))))

                   controls
                   (filterv #(= :live-select (:kind %)) (.current interactions/hit-map))]

               (expect (str/includes? text "○ job-0"))
               (expect (str/includes? text "● job-1"))
               (expect (= (mapv #(str "job-" %) (range 8)) (mapv :item-id controls))))
             (finally (.reset interactions/hit-map))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: an active
;; live surface could consume most of the terminal but had no way to minimize it.
(defdescribe
  live-view-minimize-test
  (it "an active live surface exposes a minimize control"
      (.reset interactions/hit-map)
      (try (paint-frames [(pane)] 96 26)
           (let [controls (filterv #(= :live-minimize (:kind %)) (.current interactions/hit-map))]
             (expect (= 1 (count controls)) "the opening rule has one explicit minimize control")
             (expect (= "view-1" (:view-id (first controls))))
             (expect (some #{["click ▾" "minimize"]} (lv/hint (pane) []))))
           (finally (.reset interactions/hit-map))))
  (it
    "minimizing keeps the run alive behind one restorable status row"
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

      (expect (lv/minimized? advanced) "ordinary patches do not reopen the local surface")
      (expect (< (- (long compact-bottom) (long compact-top))
                 (- (long full-bottom) (long full-top)))
              "the transcript gets the live surface's body rows back")
      (expect (= full (lv/restored compact)) "restoring preserves the exact viewport")
      (expect (not (lv/minimized? (lv/armed compact)))
              "arming an interrupt restores the note field before it takes the keyboard")
      (expect (some #{["click ↑" "restore live view"]} (lv/hint compact [])))
      (.reset interactions/hit-map)
      (try (let [{:keys [frames]}
                 (paint-frames [advanced] 96 26)

                 text
                 (cap/frame-text (last frames))

                 controls
                 (filterv #(= :live-restore (:kind %)) (.current interactions/hit-map))]

             (expect (str/includes? text "Still polling · minimized")
                     "patches keep updating the compact status while its body is folded")
             (expect (not (str/includes? text "job-0")) "the table body is no longer painted")
             (expect (= 2 (count controls)) "both the title chevron and status row restore")
             (expect (every? #(= "view-1" (:view-id %)) controls)))
           (finally (.reset interactions/hit-map))))))

;; Regression #220: description follows the title, then one blank row before nodes.
(defdescribe
  live-view-heading-spacing-test
  (it
    "live view heading spacing"
    (doseq [cols
            [40 80 120]

            :let [p
                  (lv/opened (mounted {:title "Release checks" :description "Three jobs"}
                                      (fixture/status "now" "Watching" {:tone :running})))

                  {:keys [frames error]}
                  (paint-frames [p] cols 36)

                  lines
                  (str/split-lines (cap/frame-text (last frames)))

                  [from _]
                  (lv/band-rows cols 36 [p] 1 3)

                  title-row
                  (+ (long from) 2)

                  controls
                  (filterv #(= :live-minimize (:kind %)) (.current interactions/hit-map))

                  {:keys [col width]}
                  (:bounds (first controls))

                  rail
                  (str/index-of (nth lines from) "┐")]]

      (expect (nil? error))
      (expect (str/includes? (nth lines from) "┌"))
      (expect (not (str/includes? (nth lines from) "Release checks")))
      (expect (str/blank? (str/replace (nth lines (inc (long from))) "│" ""))
              "one empty row stands between the top rule and the heading")
      (expect (= 4 (str/index-of (nth lines title-row) "LIVE"))
              "the heading starts one blank row below the border, with the body's inset")
      (expect (str/includes? (nth lines title-row) "Release checks"))
      (expect (str/includes? (nth lines (inc title-row)) "Three jobs"))
      (expect (str/blank? (str/replace (nth lines (+ title-row 2)) "│" ""))
              "one empty row separates the description from the first node")
      (expect (str/includes? (nth lines (+ title-row 3)) "Watching"))
      (expect (= [title-row] (mapv #(get-in % [:bounds :row]) controls))
              "the minimize hit target follows the title")
      (expect (< (+ (long col) (long width)) (long rail))
              "the control keeps one clear column before the band's right edge"))))

(defdescribe live-view-description-separator-test
             (it "live view description separator"
                 ;; #220: a description is a section, not padding before or inside the first node.
                 (doseq [description [nil "" "   "]]
                   (let [p (lv/opened (mounted {:description description}
                                               (fixture/status "now" "Watching" {:tone :running})))]
                     (expect (= :status (:kind (first (rows-of p)))))))
                 (let [p (lv/opened (mounted {:description "Three jobs"}))]
                   (expect (= [:prose] (mapv :kind (rows-of p)))
                           "an empty view has no trailing separator"))))

(defdescribe live-view-short-heading-test
             (it "live view short heading"
                 (let [p
                       (lv/opened (mounted {:title "Release checks" :description ""}
                                           (fixture/status "now" "Watching" {:tone :running})))

                       {:keys [frames geometry error]}
                       (paint-frames [p] 80 12)

                       lines
                       (str/split-lines (cap/frame-text (last frames)))

                       [from to]
                       (lv/band-rows 80 12 [p] 1 3)]

                   (expect (nil? error))
                   (expect (str/includes? (nth lines from) "Release checks"))
                   (expect (str/includes? (str/join "\n" (subvec (vec lines) from (inc (long to))))
                                          "Watching"))
                   (expect (pos? (long (:visible geometry)))))))

(defdescribe live-view-record-close-control-test
             (it "live view record close control"
                 ;; #235: a read-only record keeps a compact, named close target and its state.
                 (binding [interactions/hit-map (interactions/create-hit-map)]
                   (doseq [cols [24 40 96]
                           viewer? [false true]
                           reason [:completed :failed :interrupted]
                           :let [record (-> (pane)
                                            (lv/settled {:reason reason})
                                            lv/reopened
                                            (assoc :is-viewer viewer?))
                                 {:keys [frames error]} (paint-frames [record] cols 26)
                                 text (cap/frame-text (last frames))
                                 controls (filterv #(= (if viewer? :live-viewer-close :live-reopen)
                                                       (:kind %))
                                            (.current interactions/hit-map))
                                 control (first controls)
                                 {:keys [row col width]} (:bounds control)]]

                     (expect (nil? error))
                     (expect (= 1 (count controls)))
                     (expect (= "Close live view" (:label control)))
                     (expect (= 3 width))
                     (expect (str/includes? text "✕"))
                     (expect (not (str/includes? text "Close")))
                     (expect (str/includes? text "LIVE"))
                     (when (= cols 96) (expect (str/includes? text (str/capitalize (name reason)))))
                     (doseq [x (range col (+ col width))]
                       (expect (= control (.lookup interactions/hit-map (int x) (int row)))))
                     (when viewer? (expect (some #{["Esc" "close view"]} (lv/hint record []))))))))

(defdescribe
  live-view-close-button-style-test
  (it
    "live view close button style"
    (binding [interactions/hit-map (interactions/create-hit-map)]
      (doseq [settled? [false true]
              :let [view (cond-> (assoc (pane) :is-viewer true)
                           settled?
                           (lv/settled {:reason :completed})

                           settled?
                           lv/reopened)]
              hovered? [false true false]]

        (let [_ (paint-frames [view])
              control (first (filter #(= :live-viewer-close (:kind %))
                                     (.current interactions/hit-map)))
              {:keys [row col]} (:bounds control)
              _ (.updateHovered interactions/hit-map
                                (MouseAction. MouseActionType/MOVE
                                              0
                                              (TerminalPosition. (if hovered? (int col) 0)
                                                                 (int row))))
              live (paint-frames [view])
              expected {:ch "✕"
                        :fg (get t/default-palette (if hovered? :header-active-tab-fg :button-fg))
                        :bg (get t/default-palette (if hovered? :close-button-hover-fg :button-bg))}
              frame (last (:frames live))
              actual (get-in frame [row (inc (long col))])]

          (expect (nil? (:error live)))
          (expect (= expected (select-keys actual [:ch :fg :bg]))
                  "live and recorded views use a regular button with the dialog X and red hover")
          (doseq [x (range col (+ (long col) 3))]
            (expect (= (select-keys expected [:fg :bg])
                       (select-keys (get-in frame [row x]) [:fg :bg]))
                    "the entire close target has a filled button face")))))))

(defdescribe
  live-log-search-scrollbar-spacing-test
  (it "live log search scrollbar spacing"
      (let [view (mounted {:title "Build output" :description "Current run"}
                          (fixture/log "output"
                                       {:label "Console"
                                        :default-expanded true
                                        :lines (mapv #(str "line " %) (range 60))}))]
        (doseq [following? [false true]]
          (let [pane (assoc (lv/opened view)
                       :is-viewer true
                       :is-following following?)
                {:keys [frames error]} (paint-frames [pane] 80 30)
                lines (str/split-lines (cap/frame-text (last frames)))
                hits (.current interactions/hit-map)
                close (first (filter #(= :live-viewer-close (:kind %)) hits))
                search (first (filter #(= :live-log-search (:kind %)) hits))
                bar-col (first (keep #(str/index-of % "█") lines))
                close-col (get-in close [:bounds :col])
                search-col (get-in search [:bounds :col])]

            (expect (nil? error))
            (expect (every? some? [close-col search-col bar-col]))
            (when (every? some? [close-col search-col bar-col])
              (expect
                (<= (+ search-col 15) close-col)
                "Search is set back from the close button, on the log row or the fixed heading")
              (expect (<= (+ bar-col 3) close-col)
                      "two empty columns separate the scrollbar from the close button")
              (expect (= 4 (- bar-col (+ search-col 8)))
                      "Search remains four columns clear of the scrollbar")))))))

(defdescribe
  live-view-body-inset-and-bottom-gap-test
  (it "live view body inset and bottom gap"
      ;; #235: the final visible content row must not touch the footer boundary.
      (doseq [cols
              [32 40 96]

              rows
              [12 26 40]

              :let [p
                    (lv/opened (apply mounted
                                 {:title "Checks" :description ""}
                                 (concat (map #(fixture/status (str %) (str "Row " %)) (range 30))
                                         [(fixture/status "last" "Full console")])))

                    {:keys [frames geometry error]}
                    (paint-frames [p] cols rows)

                    lines
                    (str/split-lines (cap/frame-text (last frames)))

                    last-row
                    (first (keep-indexed #(when (str/includes? %2 "Full console") %1) lines))

                    [_ bottom]
                    (lv/band-rows cols rows [p] 1 3)]]

        (expect (nil? error))
        (expect (some? last-row))
        (when last-row
          (expect (= 4 (str/index-of (nth lines last-row) "· Full console")))
          (expect (= (- (long bottom) 4) last-row))
          (expect (str/blank? (str/replace (nth lines (inc (long last-row))) "│" ""))))
        (expect (pos? (long (:visible geometry)))))))

;;; ── The screenshot gate ─────────────────────────────────────────────────────

(defdescribe
  live-view-paint-test
  (it "the band paints the whole view when it fits"
      (let [{:keys [frames geometry]}
            (paint-frames [(pane)] 96 60)

            text
            (cap/frame-text (last frames))]

        (expect (seq frames))
        (expect (str/includes? text "CI · fix(loop): move the session pick"))
        (expect (str/includes? text "Now"))
        (expect (str/includes? text "Polling the run"))
        (expect (str/includes? text "Jobs"))
        (expect (str/includes? text "job-0"))
        (expect (str/includes? text "Output"))
        (expect (str/includes? text "Ran 314 tests"))
        (expect (str/includes? text "Esc"))
        (expect (= "view-1" (:view-id geometry)))
        (expect (pos? (long (:total geometry))))
        (expect (pos? (long (:visible geometry))))))
  (it "and when it does not fit, the END — the newest work — is what is on screen"
      (let [p
            (pane :rows 20)

            text
            (painted-text [p] 96 26)]

        (expect (str/includes? text "+ 8 more rows"))
        (expect (str/includes? text "Ran 314 tests"))
        (expect (not (str/includes? text "Polling the run"))
                "a view follows its tail, exactly like the log it carries")
        (expect (str/includes? (lv/footer-text p) "Polling the run")
                "which is why the summary rides the footer while the eye is at the end")))
  (it "several open views: the newest in full, the older ones one line each"
      (let [text (painted-text [(pane :id "old") (pane :id "new")])]
        (expect (str/includes? text "▸"))
        (expect (str/includes? text "2") "the hint says how many are open")))
  (it
    "a real PNG of the band, before and after one patch"
    (let [before
          (pane :rows 6)

          after
          (patched before
                   {:op :append
                    :node-id "jobs"
                    :rows [(fixture/table-row "job-9" ["job-9" "failed" "0m3s"])]})

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

      (expect (str/ends-with? png-before "vis-live-view-before.png"))
      (expect (str/ends-with? png-after "vis-live-view-after.png"))
      (expect (pos? (long (cap/ink png-before))) "the band really painted something")
      (expect (< (long (cap/ink png-before)) (long (cap/ink png-after)))
              "the row that arrived is ink that was not there before")
      (expect (not (str/includes? (painted-text [before]) "job-9")))
      (expect (str/includes? (painted-text [after]) "job-9"))
      (expect (str/ends-with? png-stop "vis-live-view-stop.png"))
      (expect (pos? (long (cap/ink png-stop))) "the armed band still paints the work"))))

;; A tone is DATA the engine already carries, and a fraction is a fraction
;; however it was declared: a surface that paints either as flat prose is
;; dropping what the human came to read.
(defdescribe
  live-view-ink-test
  (it "a counter wears its own tone, and the word naming it recedes"
      (let [p
            (lv/opened (mounted {}
                                (fixture/stat
                                  "score"
                                  [{:id "failed" :label "Failed" :value-text "1" :tone :error}
                                   {:id "seen" :label "Seen" :value-text "18"}])))

            frame
            (last (:frames (paint-frames [p] 80 20)))

            label
            (cell-under frame "Failed 1" 0)

            value
            (cell-under frame "Failed 1" 7)

            plain
            (cell-under frame "Seen 18" 5)]

        (expect (= "1" (:ch value)))
        (expect (not= (:fg label) (:fg value))
                "the tone lands on the number, not on the word naming it")
        (expect (not= (:fg value) (:fg plain)) "an untoned counter keeps the body's own ink")))
  (it "a progress that declared its parts paints the bar it earned"
      (let [text (painted-text [(lv/opened (mounted {}
                                                    (fixture/progress
                                                      "done"
                                                      {:label "Finished" :done 15 :total 18})))]
                               80
                               20)]
        (expect (str/includes? text "83%"))
        (expect (str/includes? text "15/18 done"))
        (expect (str/includes? text "■") "15 of 18 is a measured fraction, not indeterminate work")
        (expect (not (str/includes? text "working")))))
  (it "a progress nobody can size still says the one true thing"
      (let [text (painted-text [(lv/opened (mounted {}
                                                    (fixture/progress "done" {:label "Scanning"})))]
                               80
                               20)]
        (expect (str/includes? text "working"))
        (expect (not (str/includes? text "▰"))))))

;;; ── The width the band has, and who stands in it ────────────────────────────

;; A table that hugs its widest word leaves the band half empty and the eye
;; hunting across the gap: the surface IS the width the run was given, so the
;; table takes all of it — and takes it inside a drawn box, because a live table
;; is read while it fills and the eye needs the line saying where a row ends.
(defdescribe
  live-view-table-width-test
  (it "head, rule and every row end on the column the band ends on"
      (doseq [w [80 60]]
        (let [lines (->> (lv/plan (pane) w)
                         (filter #(#{:thead :trule :trow} (:kind %)))
                         (mapv :text))]
          (expect (seq lines))
          (expect (= [w] (distinct (mapv count lines))) (str "at " w " columns")))))
  (it
    "the slack lands on the column that was already the widest"
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

      (expect (= 3 (count cells)))
      (expect
        (= (apply max cells) (second cells))
        "`state` holds the longest word, so `state` is the column that grows — the same judge that shrinks first")
      (expect (< (long (first cells)) (long (second cells))))))
  (it "and the table is a BOX, with a rail between every pair of rows"
      (let [rows
            (filterv #(#{:thead :trule :trow} (:kind %)) (lv/plan (pane) 80))

            texts
            (mapv :text rows)]

        (expect (str/starts-with? (first texts) "┌"))
        (expect (str/ends-with? (first texts) "┐"))
        (expect (str/starts-with? (last texts) "└"))
        (expect (every? #(and (str/starts-with? % "│") (str/ends-with? % "│"))
                        (map :text (filter #(#{:thead :trow} (:kind %)) rows)))
                "every line of cells stands between the rails")
        (expect (= [:trule :thead :trule :trow :trule :trow] (subvec (mapv :kind rows) 0 6))
                "a rule under the head AND between the rows, not only where the box ends"))))

;; A PARENT is how a producer says which rows belong together, and every
;; surface owes that one field the same shape: one head, shut, standing where the
;; legs were listed — the fold the Companion paints, painted here.
(defdescribe
  live-view-table-parent-test
  (it
    "live view table parent"
    (let [parented
          (fn []
            (lv/opened
              (mounted {}
                       (fixture/table
                         "jobs"
                         [(fixture/table-column "job" "Job") (fixture/table-column "state" "State")]
                         {:label "Jobs"
                          :rows
                          [(fixture/table-row "build" ["build" "success"])
                           (fixture/table-row "t-1" ["ubuntu" "success"] {:parent "tests"})
                           (fixture/table-row "lint" ["lint" "success"])
                           (fixture/table-row "t-2" ["macos" "running"] {:parent "tests"})
                           (fixture/table-row "t-3" ["windows" "queued"] {:parent "tests"})]}))))

          text-of
          (fn [p]
            (str/join "\n" (map :text (rows-of p))))]

      ;; the legs fold under one head that names the parent and counts them
      (let [p
            (parented)

            heads
            (kinds-of p :tparent)

            text
            (text-of p)]

        (expect (= 1 (count heads)) "one head, however many legs the parent holds")
        (expect (str/includes? (:text (first heads)) "▸ tests · 3 rows"))
        (expect (= ["jobs" "tests"] (:node-id (first heads)))
                "keyed by the table AND the parent, so two tables never share one fold")
        (expect (= 2 (count (kinds-of p :trow))) "a shut parent paints no legs")
        (expect (< (.indexOf ^String text "build")
                   (.indexOf ^String text "tests")
                   (.indexOf ^String text "lint"))
                "the head stands where its FIRST leg was listed"))
      ;; opening it is the toggle every other fold in the view already uses
      (let [p
            (lv/expanded (parented) ["jobs" "tests"])

            text
            (text-of p)

            legs
            (filterv #(str/includes? (:text %) "ubuntu") (kinds-of p :trow))]

        (expect (str/includes? (:text (first (kinds-of p :tparent))) "▾ tests · 3 rows"))
        (expect (= 5 (count (kinds-of p :trow))))
        (expect (str/includes? (:text (first legs)) "  ubuntu") "a leg is indented under its head")
        (expect (< (.indexOf ^String text "windows") (.indexOf ^String text "lint"))
                "every leg is gathered at its head, not stranded under the row that follows"))
      ;; one leg is one row, and the head says so
      (let [p (lv/opened (mounted {}
                                  (fixture/table "jobs"
                                                 [(fixture/table-column "job" "Job")]
                                                 {:label "Jobs"
                                                  :rows [(fixture/table-row "t-1"
                                                                            ["ubuntu"]
                                                                            {:parent "tests"})]})))]
        (expect (str/includes? (:text (first (kinds-of p :tparent))) "▸ tests · 1 row")))
      ;; the head is a keyboard control, like any other disclosure
      (expect (some #(and (= :live-expand (:kind %)) (= ["jobs" "tests"] (:node-id %)))
                    (lv/controls [(parented)]))))))

;; A DECLARED group is that same fold given an identity: the producer owns the
;; order the heads paint in, the label each head wears and the tone that says a
;; leg failed — and renaming one no longer shuts it under the reader's hand.
(defdescribe
  live-view-table-group-test
  (it
    "live view table group"
    (let [grouped
          (fn [groups]
            (lv/opened (mounted {}
                                (fixture/table
                                  "jobs"
                                  [(fixture/table-column "job" "Job")]
                                  {:label "Jobs"
                                   :groups groups
                                   :rows
                                   [(fixture/table-row "t-1" ["ubuntu"] {:parent "tests"})
                                    (fixture/table-row "b-1" ["image"] {:parent "build"})]}))))

          heads-of
          (fn [p]
            (mapv :text (kinds-of p :tparent)))]

      ;; the heads paint in the order the table DECLARED, not the order the rows arrived
      (let [heads (heads-of (grouped [(fixture/table-group "tests" {:label "Tests" :order 1})
                                      (fixture/table-group "build" {:label "Build" :order 0})]))]
        (expect (= 2 (count heads)))
        (expect (str/includes? (nth heads 0) "▸ Build · 1 row"))
        (expect (str/includes? (nth heads 1) "▸ Tests · 1 row")
                "and a head wears the group's LABEL, not the id its rows point at"))
      ;; a group nobody declared is still a group, painted after the declared ones
      (let [heads (heads-of (grouped [(fixture/table-group "build" {:label "Build"})]))]
        (expect (str/includes? (nth heads 0) "▸ Build · 1 row"))
        (expect (str/includes? (nth heads 1) "▸ tests · 1 row")
                "an undeclared parent keeps working and wears its own id"))
      ;; a head says a leg failed before anybody opens it
      (let [head (first (kinds-of (grouped [(fixture/table-group "build"
                                                                 {:label "Build" :tone :error})])
                                  :tparent))]
        (expect (= :error (:tone head)))
        (expect (some #(= t/footer-error-fg (:fg %)) (:segments head))
                "the tone is ink on the head, not a word hidden inside the fold"))
      ;; a group declared open starts open, and the reader's own hand still wins
      (let [p (grouped [(fixture/table-group "build" {:label "Build" :is-open true})])]
        (expect (str/includes? (nth (heads-of p) 0) "▾ Build")
                "the producer said which fold is the interesting one")
        (expect (str/includes? (nth (heads-of (lv/expanded p ["jobs" "build"])) 0) "▸ Build")
                "and the reader shuts it again"))
      ;; renaming a group does not shut it under the reader
      (let [p
            (lv/expanded (grouped [(fixture/table-group "build" {:label "Build"})])
                         ["jobs" "build"])

            renamed
            (update-in p
                       [:view :nodes]
                       (partial mapv
                                #(cond-> % (= :table (:type %)) (assoc :groups
                                                                  [{:id "build"
                                                                    :label "Compile"}]))))]

        (expect (str/includes? (nth (heads-of renamed) 0) "▾ Compile · 1 row")
                "expansion is keyed by [table-id group-id], so a new label is the same fold"))
      ;; a band too narrow for the whole name trims the NAME and keeps the count
      (let [head (first (filterv #(= :tparent (:kind %))
                          (lv/plan
                            (grouped
                              [(fixture/table-group
                                 "build"
                                 {:label
                                  "Verify release source / python-package / ubuntu-latest"})])
                            44)))]
        (expect (= 44 (count (:text head))) "a head paints its band exactly, never past the rail")
        (expect (str/includes? (:text head) "▸ Verify release source / pytho… · 1 row")
                "the count is the head's furniture, not the first thing an ellipsis eats")))))

;; A rule is where the rails of the lines around it meet. A group head and an
;; empty line span the whole table and keep no inner rail, so the rule beside one
;; bends (`┴`, `┬`) or runs straight instead of poking a stub into its words.
(def ^:private rule-arms
  "Which way each frame glyph reaches past its own line: [up down]."
  {\┌ [false true]
   \┬ [false true]
   \┐ [false true]
   \├ [true true]
   \┼ [true true]
   \┤ [true true]
   \└ [true false]
   \┴ [true false]
   \┘ [true false]
   \─ [false false]})

(defn- frame-of
  "The lines of the first boxed table in `p`, from its top rule down to its bottom one."
  [p]
  (let [[inside [bottom]] (->> (rows-of p)
                               (drop-while #(not= :trule (:kind %)))
                               (split-with #(not (str/starts-with? (:text %) "└"))))]
    (conj (vec inside) bottom)))

(defn- stray-junctions
  "Where a table's rules and the rails around them disagree, as [line column
   glyph]: an arm reaching into a line with no rail there, or a rail left hanging
   with no arm to meet it."
  [frame]
  (for [[idx {:keys [kind text]}]
        (map-indexed vector frame)

        :when (= :trule kind)
        :let [above
              (:text (get frame (dec (long idx))))

              below
              (:text (get frame (inc (long idx))))]
        [col glyph]
        (map-indexed vector text)

        :let [[up down]
              (get rule-arms glyph)]
        :when (or (not= up (= \│ (get above col))) (not= down (= \│ (get below col))))]

    [idx col glyph]))

(defdescribe
  live-view-table-rule-test
  (it
    "live view table rule"
    (let [table
          (fn [rows]
            (lv/opened (mounted {}
                                (fixture/table "jobs"
                                               [(fixture/table-column "job" "Job")
                                                (fixture/table-column "now" "Now")
                                                (fixture/table-column "took" "Took")]
                                               {:label "Jobs" :rows rows}))))

          run
          (fn ([id] (fixture/table-row id [id "success" "1m0s"])) ([id parent] (fixture/table-row id
                                                                                 [id "success"
                                                                                  "1m0s"]
                                                                                 {:parent parent})))

          heads
          (table [(run "t-1" "tests") (run "b-1" "build") (run "l-1" "lint")])

          mixed
          (table [(run "build") (run "t-1" "tests") (run "t-2" "tests") (run "lint")])]

      ;; every rule joins exactly the rails of the lines around it
      (doseq [[label p] [["cells only" (table [(run "build") (run "lint")])]
                         ["shut heads only" heads] ["a shut head between cells" mixed]
                         ["an open head between cells" (lv/expanded mixed ["jobs" "tests"])]
                         ["an empty table" (table [])]]]
        (expect (empty? (stray-junctions (frame-of p))) label))
      ;; a column of shut heads reads as a list, not as a grid of stubs
      (let [rules (->> (frame-of heads)
                       (filter #(= :trule (:kind %)))
                       (mapv :text))]
        (expect (= 5 (count rules)))
        (expect (re-matches #"├─+┴─+┴─+┤" (nth rules 1))
                "the rule under the header closes its columns")
        (expect (every? #(re-matches #"├─+┤" %) (subvec rules 2 4))
                "a plain line between two heads")
        (expect (re-matches #"└─+┘" (peek rules)) "and a plain bottom under the last one")))))

;; A step is a line with TWO things on it: what it is doing, and what it reports.
;; The band a narrow pane has is not always wide enough for both — so the words
;; give way and the number stays, instead of the row running past the rail.
(defdescribe
  live-view-steps-width-test
  (it "live view steps width"
      (let [stepped
            (fn [label]
              (lv/opened (mounted
                           {}
                           {:id "steps"
                            :type :steps
                            :steps
                            [{:id "s-1" :label label :detail "waiting for a runner" :value 42}]})))

            row-of
            (fn [p w]
              (first (filterv #(= :step (:kind %)) (lv/plan p w))))]

        ;; a step too long for its band trims its words and still reports
        (let [row (row-of (stepped "Verify release source / python-package / ubuntu-latest") 44)]
          (expect (= 44 (count (:text row))) "the row paints its band exactly, never past it")
          (expect (str/includes? (:text row) "…") "the words are what gives way")
          (expect (str/ends-with? (:text row) "42")
                  "and what the step reports still rides the right edge"))
        ;; a step that fits says everything it has
        (let [row (row-of (stepped "Build") 60)]
          (expect (str/includes? (:text row) "Build — waiting for a runner"))
          (expect (str/ends-with? (:text row) "42"))))))

;; Every string a human reads in this program is markdown already — the
;; transcript, the form and the view's own document all speak it — so a live view
;; that painted its strings flat would be the ONE surface where `code` is not code.
(defdescribe
  live-view-inline-markdown-test
  (it "a statement reads as words, and the marks that styled them are gone"
      (let [row
            (->> (lv/plan (lv/opened (mounted {}
                                              (fixture/status "now"
                                                              "Bumped `openssl` on **db-2**"
                                                              {:label "Now" :tone :ok})))
                          80)
                 (filter #(= :status (:kind %)))
                 first)

            run-of
            (fn [text]
              (first (filter #(= text (:text %)) (:runs row))))]

        (expect (str/includes? (:text row) "Bumped openssl on db-2"))
        (expect (not (str/includes? (:text row) "`")) "the syntax is not the sentence")
        (expect (contains? (:style (run-of "openssl")) :code))
        (expect (contains? (:style (run-of "db-2")) :bold))))
  (it "and the code span really wears another ink on the terminal"
      (let [frame (last (:frames (paint-frames [(lv/opened (mounted {}
                                                                    (fixture/status
                                                                      "now"
                                                                      "Bumped `openssl` now"
                                                                      {:label "Now"})))]
                                               80
                                               20)))]
        (expect (not= (:fg (cell-under frame "Bumped openssl" 0))
                      (:fg (cell-under frame "Bumped openssl" 7)))
                "the word and the code span beside it are not painted in one flat ink")))
  (it "a log line is machine output and stays VERBATIM"
      (let [text (str/join "\n"
                           (map :text
                                (lv/plan (lv/opened (mounted {}
                                                             (fixture/log "tail"
                                                                          {:label "Output"
                                                                           :default-expanded true
                                                                           :lines
                                                                           ["cat `x` **y**"]})))
                                         80)))]
        (expect (str/includes? text "cat `x` **y**")
                "backticks in a build log are the build's own characters, not styling"))))

(defn- reading-pane
  "A table with the paragraph that explains it declared BESIDE it, in the form's
   own `row` — the shape an arrangement exists for."
  []
  (lv/opened
    (mounted
      {}
      (fixture/row
        "reading"
        (fixture/table "jobs"
                       [(fixture/table-column "job" "Job") (fixture/table-column "state" "State")]
                       {:label "Jobs"
                        :rows [(fixture/table-row "j1" ["job-1" "success"])
                               (fixture/table-row "j2" ["job-2" "failed"])]})
        (fixture/status
          "note"
          "The `openssl` bump landed on db-2 only, so the sweep keeps going until every host answers on the new subnet."
          {:label "Reading" :tone :warn})))))

;; A live view arranges its work with the REQUEST's own two words, so `row` and
;; `column` mean one thing on every surface. They say the least that can be
;; said — these stand together, those stack — and how wide, or whether there is
;; room at all, the surface decides.
(defdescribe
  live-view-group-test
  (it "a row stands the nodes declared in it side by side"
      (let [columns (filterv #(= :columns (:kind %)) (lv/plan (reading-pane) 80))]
        (expect (seq columns))
        (expect (= ["Jobs" "Reading"]
                   (mapv :text
                         (:cells (first (filter #(every? (comp #{:node} :kind) (:cells %))
                                                columns)))))
                "each column opens under its own label")
        (expect (some #(and (= :trule (:kind (first (:cells %))))
                            (= :status (:kind (second (:cells %)))))
                      columns)
                "the table's box and the paragraph explaining it start on ONE row")))
  (it "the paragraph is JUSTIFIED to its column, so both its edges are straight"
      (let [cell-w
            (columns/cell-width 80 2)

            texts
            (->> (lv/plan (reading-pane) 80)
                 (filter #(= :columns (:kind %)))
                 (keep #(second (:cells %)))
                 (filter #(= :status (:kind %)))
                 (mapv :text))]

        (expect (= 37 cell-w) "two columns and the gutter between them, out of an 80-column band")
        (expect (< 1 (count texts)) "the sentence wrapped")
        (expect (every? #(<= (count %) cell-w) texts) "no line ever runs past its column")
        (expect (some #(= cell-w (count %)) (butlast texts))
                "and a line with gaps enough to close is flush with both edges")
        (expect (> cell-w (count (last texts))) "the last line of a paragraph is never stretched")
        ;; Justification stops where it would open a river: a line short by more
        ;; than it has gaps to grow stays ragged. That is the terminal's ONE
        ;; policy (`markdown-layout/justify-line-runs`), not this pane's opinion.
        (expect
          (every? (fn [line]
                    (let [slack
                          (- cell-w (count line))

                          gaps
                          (dec (count (str/split (str/trim line) #"\s+")))]

                      (or (zero? slack) (>= slack gaps))))
                  (butlast texts))
          "a ragged line is a justification refused, because stretching it would open a river")))
  (it "a band too narrow to split stacks them instead"
      (let [kinds (set (map :kind (lv/plan (reading-pane) 40)))]
        (expect (not (contains? kinds :columns))
                "under the narrowest useful column there is nothing to split")
        (expect (contains? kinds :thead))
        (expect (contains? kinds :status) "both nodes are still there, one under the other")))
  (it "and the terminal really paints them on the same row"
      (let [line (->> (str/split-lines (painted-text [(reading-pane)]))
                      (filter #(str/includes? % "Jobs"))
                      first)]
        (expect (str/includes? (str line) "Reading") "one screen row carries both labels")))
  ;; The proof a person can LOOK at: one band carrying a boxed table that spans
  ;; its column and the paragraph that explains it, marks and all.
  (it "a real PNG of a table with its paragraph beside it"
      (let [png (cap/shot! {:cols 96
                            :rows 24
                            :font-size 14
                            :out "vis-live-view-group"
                            :paint! (fn [{:keys [screen]}]
                                      (let [g (.newTextGraphics ^TerminalScreen screen)]
                                        (lv/paint! g 96 24 [(reading-pane)] 1 3)
                                        (.refresh ^TerminalScreen screen)))})]
        (expect (str/ends-with? png "vis-live-view-group.png"))
        (expect (pos? (long (cap/ink png))) "the split band really painted"))))

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
  (.reset interactions/hit-map)
  (.beginFrame interactions/hit-map)
  (paint-frames panes)
  (.commitFrame interactions/hit-map)
  (.current interactions/hit-map))

;; Regression, vis session a64d44c2: `band-rows` measured the band as ONE display
;; row while `paint!` drew every row the view asked for, so the wheel owned only
;; the four rows above the prompt — over the rest of a tall pane the transcript
;; underneath it scrolled instead.
(defdescribe
  live-band-height-test
  (it "the wheel claims exactly the rows the band paints"
      (let [p
            (patched (pane) {:op :append :node-id "tail" :lines (mapv #(str "line " %) (range 40))})

            lines
            (str/split-lines (painted-text [p] 96 40))

            [from to]
            (lv/band-rows 96 40 [p] 1 3)]

        (expect (str/includes? (nth lines from) "┌")
                "the first row it claims is the band's opening border")
        (expect (str/includes? (nth lines (+ (long from) 2))
                               "CI · fix(loop): move the session pick")
                "the padded heading remains inside the wheel's band")
        (expect (str/includes? (nth lines to) "└") "the last is the rule that closes it")
        (expect (every? str/blank? (take from lines))
                "and nothing of the band is painted above the rows the wheel owns")))
  (it "a busy view takes four fifths of the available terminal without covering the composer"
      (let [p
            (patched (pane) {:op :append :node-id "tail" :lines (mapv #(str "line " %) (range 40))})

            height
            (fn [rows]
              (let [[from to] (lv/band-rows 96 rows [p] 1 3)]
                (inc (- (long to) (long from)))))]

        (doseq [rows [20 24 40 60]]
          (let [available (- rows 1 3)]
            (expect
              (>= (* 5 (height rows)) (* 4 available))
              (str "on " rows " rows the watched run owns four fifths of the available surface"))
            (expect (<= (height rows) available)
                    (str "on " rows " rows the composer remains outside the band")))))))

;; Phase 5 of the live-view plan: a view used to vanish the moment it ended, so
;; the log the human had been watching became unreachable one frame after it
;; finished. What a finished run leaves now is a ROW OF THE TRANSCRIPT, in the
;; turn that watched it — the band is only for work that is still happening.
(defdescribe
  live-view-settled-test
  (it "a finished view gives the band back and hands the transcript its row"
      (let [p
            (ended (patched (pane) {:op :append :node-id "tail" :lines ["one" "two" "three"]}))

            row
            (lv/run-row p)]

        (expect (str/blank? (painted-text [p]))
                "the band paints nothing at all for a run that is over")
        (expect (nil? (:geometry (paint-frames [p])))
                "there is no paint to hand geometry back from: nothing was drawn")
        (expect (nil? (lv/band-rows 96 26 [p] 1 3))
                "and the wheel is told there is no band to be over")
        (expect (= "view-1" (:view-id row)) "the row names the view its press reads back")
        (expect (str/includes? (:title row) "fix(loop): move the session pick"))
        (expect (= :completed (:reason row)) "how it ended, in the engine's own word")
        (expect (= 5 (:lines row)) "the record it left, not the window it painted")
        (expect (= 60000 (:elapsed-ms row)) "and how long the run took, frozen at the close")))
  (it "pressing that row reads the record back, and pressing it again puts it away"
      (let [p
            (ended (pane))

            open
            (lv/reopened p)]

        ;; Regression #189: a reopened receipt starts with each log collapsed.
        (expect (not (str/includes? (painted-text [open]) "Ran 314 tests")))
        (expect (str/includes? (painted-text [(lv/expanded open "tail")]) "Ran 314 tests")
                "retained, read-only output")
        (expect (not (str/includes? (painted-text [(lv/reopened open)]) "Ran 314 tests"))
                "the same press closes it")
        (expect (lv/settled? open) "reopening does not un-end the view")))
  (it "the band publishes no control for a finished run — the transcript owns it"
      (expect (empty? (filterv #(= :live-reopen (:kind %))
                        (regions-of [(ended (pane :id "done-1"))])))
              "the line that used to sit here is a row of the turn now"))
  (it "an open view keeps the whole band while a finished one is filed away"
      (let [running
            (pane :id "running-1")

            done
            (ended (pane :id "done-1"))

            text
            (painted-text [done running])]

        (expect (str/includes? text "Ran 314 tests") "the open view keeps the body")
        (expect (not (str/includes? text "completed"))
                "and keeps it whole: a run that ended is no longer band furniture")
        (expect (= "running-1" (lv/view-id (lv/interruptible [done running])))
                "a stop can only reach work that is still running")
        (expect (nil? (lv/interruptible [done]))
                "and with nothing running there is nothing left to interrupt")))
  (it "the ticker names the run the band is painting, and only while it runs"
      (let [running
            (pane :id "running-1")

            done
            (ended (pane :id "done-1"))]

        (expect (str/includes? (str (lv/watching-title [done running]))
                               "fix(loop): move the session pick")
                "the sentence over the bubble says which run the human is looking at")
        (expect (nil? (lv/watching-title [done]))
                "a finished run is a row of the transcript, never the phase of the turn")
        (expect (nil? (lv/watching-title [])) "and an empty band names nothing")))
  (it "the ops the channel carries: a close settles the pane, it does not drop it"
      (with-db (fn []
                 (let [view (assoc (ci-view) :session-id "s1")]
                   (state/dispatch [:live-view-open view])
                   (state/dispatch [:live-view-close "view-1"
                                    {:reason :interrupted :artifact-id "art-1"}])
                   (let [p (first (:live-views @state/app-db))]
                     (expect (lv/settled? p))
                     (expect (= :interrupted (:reason (:settled p))))
                     (expect (= "art-1" (:artifact-id (:settled p)))
                             "the pane knows the artifact it was filed as"))
                   (state/dispatch [:live-view-reopen "view-1"])
                   (expect (= "view-1" (:live-viewer-id @state/app-db)))
                   (state/dispatch [:live-viewer-close])
                   (expect (lv/dormant? (first (:live-views @state/app-db))))))))
  ;; Regression, Vis session a64d44c2-8228-455f-926e-b3381f19a93b: run rows
  ;; were filed without their executing-form position and their disclosure stayed
  ;; visually collapsed after a click reopened the record.
  ;; #222: ownership comes from the invocation, never whichever form is newest.
  (it
    "finished runs retain their explicit owners and disclosure state"
    (with-db
      (fn []
        (swap! state/app-db assoc
          :messages [{:role :user :text "watch it"} {:role :assistant :text "watching"}]
          :progress {:iterations [{:forms [{:code "first_watch()"}]}]})
        (state/dispatch [:live-view-open
                         (assoc (ci-view :id "a")
                           :session-id "s1"
                           :owner {:invocation-id "first" :activity-id "history-a"})])
        (swap! state/app-db assoc-in
          [:progress :iterations]
          [{:forms [{:code "first_watch()"}]} {:forms [{:code "second_watch()"}]}])
        (state/dispatch [:live-view-open
                         (assoc (ci-view :id "b")
                           :session-id "s1"
                           :owner {:invocation-id "second"})])
        (state/dispatch [:live-view-close "a" {:reason :completed}])
        (state/dispatch [:live-view-close "b" {:reason :failed}])
        (expect (= ["a" "b"] (mapv lv/view-id (:live-views @state/app-db)))
                "nothing is retired: every record remains reachable")
        (let [runs (:runs (second (:messages @state/app-db)))]
          (expect (= [{:invocation-id "first" :activity-id "history-a"} {:invocation-id "second"}]
                     (mapv :owner runs))
                  "each row carries the host's exact owner even when forms arrive later")
          (expect (not-any? :anchor runs) "opening a view must not guess its current form")
          (expect (= [:completed :failed] (mapv :reason runs))))
        (state/dispatch [:live-view-reopen "a"])
        (expect (= "a" (:live-viewer-id @state/app-db)))
        (expect (not (get-in @state/app-db [:messages 1 :runs 0 :is-reopened])))
        (state/dispatch [:live-viewer-close])
        (expect (nil? (:live-viewer-id @state/app-db)))
        (expect (not (get-in @state/app-db [:messages 1 :runs 0 :is-reopened]))))))
  ;; The proof a person can LOOK at: one run finished and gone from the band, the
  ;; one still going painting in full.
  (it "a real PNG of the band a finished run has already left"
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
        (expect (str/ends-with? png "vis-live-view-settled.png"))
        (expect (pos? (long (cap/ink png))) "the band belongs entirely to the run still going"))))

(defn recorded-ci-source
  "Durable gateway record for the restored-view render and pointer regression."
  []
  (str (wire/json-str {:kind :open
                       :at 1000
                       :view (assoc (ci-view :rows 3)
                               :session-id "s1"
                               :created-at 1000)})
       "\n"
       (wire/json-str {:kind :close
                       :at 5000
                       :result {:reason :completed
                                :is-completed true
                                :view {:title "Release checks"
                                       :nodes [(fixture/status "now"
                                                               "All checks passed"
                                                               {:tone :ok}) (jobs {} 3)]}}})
       "\n"))

(defdescribe
  recorded-live-view-test
  (it "recorded live view"
      (let [pane
            (lv/recorded-pane (recorded-ci-source) "s1")

            text
            (painted-text [pane])]

        (expect (= "view-1" (lv/view-id pane)))
        (expect (= "s1" (get-in pane [:view :session-id])))
        (expect (lv/settled? pane))
        (expect (= :completed (get-in pane [:settled :reason])))
        (expect (= 5000 (get-in pane [:settled :ended-at])))
        (expect (not (lv/dormant? pane)))
        (expect (nil? (lv/interruptible [pane])))
        (expect (str/includes? text "All checks passed"))
        (expect (str/includes? text "job-0"))
        (expect (not (str/includes? text "Polling the run")))
        (expect (some #(= :live-reopen (:kind %)) (regions-of [pane])))
        (with-db (fn []
                   (state/dispatch [:live-record-open "s1" pane])
                   (state/dispatch [:live-record-open "s1" pane])
                   (expect (= 1 (count (:live-views @state/app-db))))
                   (state/dispatch [:live-view-reopen "view-1"])
                   (expect (lv/dormant? (first (:live-views @state/app-db))))
                   (state/dispatch [:live-record-open "elsewhere"
                                    (assoc-in pane [:view :id] "other")])
                   (expect (= 1 (count (:live-views @state/app-db)))))))
      (expect (try (lv/recorded-pane "" "s1") false (catch clojure.lang.ExceptionInfo _ true)))))

(defn disclosure-review-pane
  "A nested log and its sibling, through the production live-view materializer."
  ([] (disclosure-review-pane false))
  ([recorded?]
   (let [view (mounted {:title "Worker checks" :description "Latest observed state"}
                       {:id "pool"
                        :type :group
                        :direction :column
                        :label "Pool state"
                        :is-collapsible true
                        :default-expanded true
                        :fields [(fixture/log "tail"
                                              {:label "Observed workers"
                                               :default-expanded true
                                               :lines ["monitor revision=42 active=4/4"
                                                       "  worker ready" "checks finished"]})]}
                       (fixture/status "other" "Unrelated output" {:label "Other checks"}))]
     (if recorded?
       (-> (lv/recorded-pane (str (wire/json-str {:kind :open :at 1000 :view view})
                                  "\n"
                                  (wire/json-str
                                    {:kind :close
                                     :at 5000
                                     :result {:reason :completed :is-completed true :view view}})
                                  "\n")
                             "s1")
           (lv/expanded "pool")
           (lv/expanded "tail"))
       (lv/opened view)))))

;; #219 keeps details nested; #250 moves Search into the log's own header.
(defdescribe
  live-disclosure-nesting-test
  (it
    "live disclosure nesting"
    (doseq [cols
            [40 80 120]

            recorded?
            [false true]]

      (let [pane
            (disclosure-review-pane recorded?)

            capture
            (paint-frames [pane] cols 40)

            lines
            (str/split-lines (cap/frame-text (last (:frames capture))))

            position
            (fn [text]
              (first (keep-indexed (fn [row line]
                                     (when-let [col (str/index-of line text)]
                                       {:row row :col col}))
                                   lines)))

            group
            (position "▾ Pool state")

            log
            (position "▾ Observed")

            search
            (position " Search ")

            detail
            (position "monitor revision=42")

            whitespace
            (position "worker ready")

            sibling
            (position "Other checks")

            hits
            (.current interactions/hit-map)

            log-hit
            (first (filter #(and (= :live-expand (:kind %)) (= "tail" (:node-id %))) hits))

            search-hit
            (first (filter #(= :live-log-search (:kind %)) hits))]

        (expect (= recorded? (lv/settled? pane)))
        (expect (nil? (:error capture)))
        (expect (every? some? [group log search detail whitespace sibling]))
        (expect (= (+ 2 (long (:col group))) (:col log)) "child toggle gains one level")
        (expect (= (+ 2 (long (:col log))) (:col detail)) "log rows gain another level")
        (expect (= (:row log) (:row search)) "Search shares the log header")
        (expect (< (+ (long (:col log)) (long (get-in log-hit [:bounds :width])))
                   (long (:col search)))
                "a gap separates the disclosure and Search targets")
        (expect (= (+ 2 (long (:col detail))) (:col whitespace))
                "the log's own whitespace is preserved")
        (expect (= (:col group) (:col sibling)) "unrelated siblings return to the parent edge")
        (expect (= (:col log) (get-in log-hit [:bounds :col])))
        (expect (= (:col search) (get-in search-hit [:bounds :col])))
        (expect (not= :live-log-search
                      (:kind
                        (.lookup interactions/hit-map (dec (long (:col search))) (:row search))))
                "the header gap is not part of the Search button")
        (doseq [col (range (:col search) (+ (long (:col search)) 8))]
          (expect (= search-hit (.lookup interactions/hit-map col (:row search)))))
        (expect (= log-hit (.lookup interactions/hit-map (:col log) (:row log))))
        (let [collapsed
              (lv/expanded pane "tail")

              text
              (painted-text [collapsed] cols 40)]

          (expect (str/includes? text "▸ Observed workers"))
          (expect (not (str/includes? text "monitor revision=42")))
          (expect (not (str/includes? text " Search ")))
          (expect (= lines
                     (str/split-lines (painted-text [(lv/expanded collapsed "tail")] cols 40)))))
        (expect (not (str/includes? (painted-text [(lv/expanded pane "pool")] cols 40)
                                    "Observed workers")))))))

(defdescribe disclosed-column-width-test
             (it "disclosed column width"
                 ;; #219: reserve the disclosure inset before deciding whether columns still fit.
                 (let [pane
                       (update-in (reading-pane)
                                  [:view :nodes 0]
                                  assoc
                                  :label "Grouped checks"
                                  :is-collapsible true
                                  :default-expanded true)

                       narrow
                       (lv/plan pane 54)

                       wide
                       (lv/plan pane 56)]

                   (expect (not-any? #(= :columns (:kind %)) narrow))
                   (expect (some #(= :columns (:kind %)) wide))
                   (expect (= (:widths (meta (lv/plan (reading-pane) 54))) (:widths (meta wide))))
                   (expect (every? #(= 2 (:indent %)) (filter #(= :columns (:kind %)) wide)))
                   (doseq [cols [40 80 120]]
                     (let [capture (paint-frames [pane] cols 40)
                           text (cap/frame-text (last (:frames capture)))]

                       (expect (nil? (:error capture)))
                       (expect (str/includes? text "Jobs"))
                       (expect (str/includes? text "Reading")))))))

(defn link-grid-review-pane
  "Build results for the production link-grid review, live or retained (#221)."
  ([] (link-grid-review-pane false))
  ([recorded?]
   (let [view (mounted {:title "Build results" :description "Latest verification"}
                       {:id "builds"
                        :type :link
                        :label "Builds and reviews"
                        :links (mapv (fn [i]
                                       {:id (str "build-" i)
                                        :label (str "Build " i " · SUCCESS")
                                        :target-kind :url
                                        :target (str "https://gateway.example.com/build/" i)})
                                     (range 1 7))})]
     (if recorded?
       (lv/recorded-pane
         (str (wire/json-str {:kind :open :at 1000 :view view})
              "\n"
              (wire/json-str
                {:kind :close :at 5000 :result {:reason :completed :is-completed true :view view}})
              "\n")
         "s1")
       (lv/opened view)))))

(defdescribe
  live-link-grid-test
  (it "live link grid"
      ;; #221: a multi-item link result is one framed, row-major grid, not loose rows.
      (doseq [recorded? [false true]]
        (let [pane (link-grid-review-pane recorded?)
              wide (lv/plan pane 100)
              narrow (lv/plan pane 28)
              rows (filterv #(= :link-grid (:kind %)) wide)
              lines (str/split-lines (painted-text [pane] 110 40))]

          (expect (= 2 (count (filter #(= :trule (:kind %)) wide))))
          (expect (< (count rows) 6))
          (expect (= (mapv #(str "build-" %) (range 1 7))
                     (mapv :item-id (mapcat #(remove nil? (:links %)) rows))))
          (expect (= 6 (count (filter #(= :link-grid (:kind %)) narrow))))
          (expect (some #(and (str/includes? % "Build 1") (str/includes? % "Build 2")) lines))
          (expect (some #(str/includes? % "┌") lines))
          (expect (some #(str/includes? % "└") lines))
          (let [hits (filterv #(= :url (:kind %)) (.current interactions/hit-map))]
            (expect (= (mapv #(str "https://gateway.example.com/build/" %) (range 1 7))
                       (mapv :url hits)))
            (expect (= (get-in (first hits) [:bounds :row]) (get-in (second hits) [:bounds :row])))
            (doseq [{:keys [bounds url]} hits]
              (expect (= url
                         (:url (.lookup interactions/hit-map (:col bounds) (:row bounds)))))))))))

(defdescribe
  live-link-grid-boundaries-test
  (it
    "live link grid boundaries"
    ;; #221: framing must not discard labels, invent targets, or absorb its gutter.
    (let [link
          {:id "one"
           :label "One build"
           :target-kind :url
           :target "https://gateway.example.com/build/one"}

          open-links
          (fn [links]
            (lv/opened (mounted {:description nil} {:id "links" :type :link :links links})))]

      (expect (= [:empty] (mapv :kind (lv/plan (open-links []) 28))))
      (expect (= [:link] (mapv :kind (lv/plan (open-links [link]) 28))))
      (let [long-label
            "Full console for the release build with detailed verification results"

            pane
            (open-links
              [(assoc link
                 :label long-label
                 :target-kind :path
                 :target "/tmp/build.log")
               {:id "two" :label "Review 界 · SUCCESS" :target-kind :document :target "receipt-1"}])

            plan
            (lv/plan pane 28)

            cells
            (mapcat :links (filter #(= :link-grid (:kind %)) plan))

            label-text
            (str/join " " (mapcat #(map :text (:runs %)) (filter #(= "one" (:item-id %)) cells)))

            capture
            (paint-frames [pane] 40 40)

            hits
            (.current interactions/hit-map)

            file-hits
            (filter #(= :file (:kind %)) hits)]

        (expect (every? #(str/includes? label-text %) (str/split long-label #" ")))
        (expect (not (str/includes? label-text "…")))
        (expect (nil? (:error capture)))
        (expect (seq file-hits))
        (expect (every? #(= "/tmp/build.log" (:url %)) file-hits))
        (expect (not-any? #(= "receipt-1" (:url %)) hits))
        (doseq [{:keys [bounds]} file-hits]
          (expect (not= :file
                        (:kind (.lookup interactions/hit-map
                                        (dec (long (:col bounds)))
                                        (:row bounds)))))))
      (let [links
            (mapv #(assoc link
                     :id (str "link-" %)
                     :label (str "Build " %))
                  (range 40))

            pane
            (open-links links)

            first-plan
            (lv/plan pane 80)

            more
            (first (filter #(= :more (:kind %)) first-plan))

            expanded
            (lv/plan (lv/expanded pane "links") 80)

            ids
            (fn [plan]
              (mapv :item-id
                    (mapcat #(remove nil? (:links %)) (filter #(= :link-grid (:kind %)) plan))))]

        (expect (some? more))
        (expect (= 40 (+ (count (ids first-plan)) (long (:count more)))))
        (expect (= (mapv :id links) (ids expanded)))
        (expect (not-any? #(= :more (:kind %)) expanded))))))

(defn divider-review-pane
  "Section dividers in the production live view, active or retained."
  ([] (divider-review-pane false))
  ([recorded?]
   (let [view (mounted {:title "Build review" :description "Verification and next steps"}
                       {:id "build" :type :paragraph :text "Build finished"}
                       {:id "results-break" :type :divider}
                       {:id "review"
                        :type :group
                        :direction :column
                        :label "Verification"
                        :is-collapsible true
                        :default-expanded true
                        :fields [{:id "checks" :type :paragraph :text "All checks passed"}
                                 {:id "review-break" :type :divider}
                                 {:id "approval" :type :paragraph :text "Ready for review"}]}
                       {:id "next" :type :paragraph :text "Next: publish when approved"})]
     (if recorded?
       (-> (lv/recorded-pane (str (wire/json-str {:kind :open :at 1000 :view view})
                                  "\n"
                                  (wire/json-str
                                    {:kind :close
                                     :at 5000
                                     :result {:reason :completed :is-completed true :view view}})
                                  "\n")
                             "s1")
           (lv/expanded "review"))
       (lv/opened view)))))

(defdescribe
  live-divider-test
  (it "live divider"
      (doseq [recorded?
              [false true]

              width
              [1 28 80 120]]

        (let [pane
              (divider-review-pane recorded?)

              rules
              (filterv #(= :trule (:kind %)) (lv/plan pane width))]

          (expect (= ["results-break" "review-break"] (mapv :node-id rules)))
          (expect (= [width (max 1 (- width (min 2 (max 0 (dec width)))))]
                     (mapv #(count (:text %)) rules)))
          (expect (every? #(every? #{\─} (:text %)) rules))
          (expect (not-any? #(#{"results-break" "review-break"} (:node-id %)) (lv/controls [pane])))
          (expect (= ["results-break"]
                     (mapv :node-id
                           (filter #(= :trule (:kind %))
                                   (lv/plan (lv/expanded pane "review") width)))))))
      (let [pane
            (lv/opened (mounted {:description nil}
                                (fixture/row "pair"
                                             {:id "left" :type :divider}
                                             {:id "right" :type :divider})))

            narrow
            (lv/plan pane 28)

            wide
            (lv/plan pane 80)]

        (expect (= [:trule :blank :trule] (mapv :kind narrow)))
        (expect (= [28 28] (mapv #(count (:text %)) (filter :text narrow))))
        (expect (= 1 (count wide)))
        (expect (= :columns (:kind (first wide))))
        (expect (= ["left" "right"] (mapv :node-id (:cells (first wide)))))
        (expect (= (repeat 2 (columns/cell-width 80 2))
                   (mapv #(count (:text %)) (:cells (first wide))))))))

(defn- type-search
  [search text]
  (reduce (fn [search ch]
            (lv/log-search-typed search (KeyStroke. (Character/valueOf (char ch)) false false)))
          search
          text))

(defdescribe inline-log-search-edits-only-its-query
             (it "inline log search edits only its query"
                 ;; #235 follow-up: the transient owns a separate, single-line search editor.
                 (let [search (-> (lv/log-search-opened "output")
                                  (type-search "abcd")
                                  (lv/log-search-typed (KeyStroke. KeyType/ArrowLeft))
                                  (lv/log-search-typed (KeyStroke. KeyType/Backspace))
                                  (lv/log-search-typed (KeyStroke. KeyType/Delete))
                                  (lv/log-search-typed (KeyStroke. KeyType/Home))
                                  (type-search "X"))]
                   (expect (= "Xab" (input/input->text (:input search))))
                   (expect (= 1 (get-in search [:input :ccol])))
                   (expect (= "Xa"
                              (-> search
                                  (lv/log-search-typed (KeyStroke. KeyType/End))
                                  (lv/log-search-typed (KeyStroke. KeyType/Backspace))
                                  :input
                                  input/input->text)))
                   (let [pasted (-> search
                                    (lv/log-search-typed (KeyStroke. KeyType/PasteStart))
                                    (type-search "one")
                                    (lv/log-search-typed (KeyStroke. KeyType/Enter))
                                    (type-search "two")
                                    (lv/log-search-typed (KeyStroke. KeyType/PasteEnd)))]
                     (expect (= 1 (count (get-in pasted [:input :lines]))))
                     (expect (str/includes? (input/input->text (:input pasted)) "one"))
                     (expect (str/includes? (input/input->text (:input pasted)) "two"))
                     (expect (not (contains? pasted :paste)))))))

(defdescribe inline-log-search-fences-stale-reads
             (it "inline log search fences stale reads"
                 (let [search
                       (type-search (lv/log-search-opened "output") "ERROR [x]")

                       pending
                       (lv/log-search-requested search 0 :first)

                       page
                       {"matched" 1 "total" 900 "lines" ["ERROR [x]"] "line_numbers" [3]}

                       edited
                       (type-search pending "!")

                       newer
                       (lv/log-search-requested search 0 :second)]

                   (expect (:loading? pending))
                   (expect (= edited (lv/log-search-loaded edited :first {:page page})))
                   (expect (= newer (lv/log-search-loaded newer :first {:page page})))
                   (expect (nil? (lv/log-search-loaded nil :first {:page page})))
                   (let [loaded (lv/log-search-loaded newer :second {:page page})]
                     (expect (= page (:page loaded)))
                     (expect (not (:loading? loaded)))
                     (expect
                       (= page (:page (lv/log-search-typed loaded (KeyStroke. KeyType/ArrowLeft)))))
                     (expect (nil? (:page (type-search loaded "!"))))))))

(defdescribe inline-log-search-pages-and-wraps-literal-results
             (it "inline log search pages and wraps literal results"
                 (let [search
                       (assoc (lv/log-search-opened "output")
                         :page {"matched" 401
                                "total" 2000
                                "line_numbers" [42]
                                "lines"
                                [(str "[tag]*ERROR* " (apply str (repeat 200 "x")) " end-marker")]})

                       plan
                       (lv/log-search-plan search 24)

                       lines
                       (mapv :text (filter #(= :log (:kind %)) plan))]

                   (expect (nil? (lv/log-search-page-from search -1)))
                   (expect (= 200 (lv/log-search-page-from search 1)))
                   (expect (= 200 (lv/log-search-page-from (assoc search :from 400) -1)))
                   (expect (nil? (lv/log-search-page-from (assoc search :from 400) 1)))
                   (expect (= [1] (mapv :direction (filter #(= :log-search-page (:kind %)) plan))))
                   (expect (> (count lines) 2))
                   (expect (every? #(<= (count %) 24) lines))
                   (expect (str/includes? (str/join " " lines) "[tag]*ERROR*"))
                   (expect (str/includes? (last lines) "end-marker")))))

(defdescribe inline-log-search-loading-empty-and-failure-states
             (it "inline log search loading empty and failure states"
                 (let [search
                       (lv/log-search-opened "output")

                       text
                       #(-> (lv/log-search-plan % 80)
                            first
                            :text)]

                   (expect (str/includes? (text search) "entire retained log"))
                   (expect (= "Searching…" (text (lv/log-search-requested search 0 :request))))
                   (expect (= "No matching lines"
                              (text (assoc search :page {"matched" 0 "total" 900 "lines" []}))))
                   (expect (= "Could not read log. Enter to retry."
                              (text (assoc search :error true)))))))
