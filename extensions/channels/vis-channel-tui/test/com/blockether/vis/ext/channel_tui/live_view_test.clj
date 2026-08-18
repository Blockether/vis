(ns com.blockether.vis.ext.channel-tui.live-view-test
  "The live-view pane: what it paints, where the eye stays while the view changes
   underneath it, and what Escape hits while one is open.

   Every view here is DECLARED through the public builders and normalized by the
   engine, so a test can only paint shapes an extension can really produce."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
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
   (let
     [geom
      (atom nil)

      cap
      (cap/capture! {:cols cols
                     :rows rows
                     :paint! (fn [{:keys [screen]}]
                               (let [g (.newTextGraphics ^TerminalScreen screen)]
                                 (reset! geom (lv/paint! g cols rows panes 1 3))
                                 (.refresh ^TerminalScreen screen)))})]

     (assoc cap :geometry @geom))))

(defn- painted-text
  ([panes] (painted-text panes 96 26))
  ([panes cols rows] (cap/frame-text (last (:frames (paint-frames panes cols rows))))))

(defn- cell-under
  "The captured cell painting character `idx` of `needle`, on the first row that
   carries it — how a test reads the INK a row was really painted in."
  [frame needle idx]
  (let
    [row
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
  (testing "a table always paints its header and its rule, even holding nothing"
    (let
      [p
       (lv/opened (mounted {} (jobs {} 0)))

       plan
       (rows-of p)]

      (is (= [:prose :node :thead :trule :empty] (mapv :kind plan)))
      (is (str/includes? (:text (nth plan 2)) "Job"))
      (is (= "no rows yet" (:text (last plan)))))))

(deftest live-view-window-test
  (testing "a node paints a window and says how much it is holding back"
    (let
      [p
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
    (let
      [p
       (lv/opened
         (mounted {} (hi/log "tail" {:label "Output" :lines (mapv #(str "line " %) (range 40))})))

       lines
       (mapv :text (filterv #(= :log (:kind %)) (rows-of p)))]

      (is (= lv/node-window (count lines)))
      (is (= "line 39" (last lines)))
      (is (= "line 28" (first lines)))
      (is (some #(str/includes? (str (:text %)) "28 earlier lines") (rows-of p))))))

(deftest live-view-follow-test
  (testing "a fresh pane follows the end" (is (:is-following (pane))))
  (testing "scrolling up releases follow; landing back at the bottom re-arms it"
    (let
      [p
       (-> (pane :rows 30)
           (lv/painted {:offset 40 :total 60 :visible 20}))

       up
       (lv/scrolled p -6)]

      (is (not (:is-following up)))
      (is (= 34 (:offset up)))
      (is (:is-following (lv/scrolled up 6)))
      (is (:is-following (lv/scrolled up 999)) "the wheel cannot run past the end")
      (is (= 40 (:offset (lv/scrolled up 999))))))
  (testing "a following pane sits at the end of whatever the plan is now"
    (let
      [p
       (pane :rows 30)

       rows
       (rows-of p)]

      (is (= (max 0 (- (count rows) 8)) (lv/offset p rows 8))))))

(defn- parked
  "A pane the human has scrolled BACK to, in the order it really happens: the
   paint measures, the wheel releases follow-tail, the next paint records the
   anchor under the eye. Answers `[pane offset anchor]`."
  [p visible target]
  (let
    [rows
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
    (let
      [p
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
      (is (= 3 (- (long (lv/offset grown grown-rows 6)) (long eye)))
          "three rows landed above it, so the viewport starts three rows later")
      (is (= anchor (lv/anchor-at grown-rows (lv/offset grown grown-rows 6)))
          "and the same row is still the top visible one")))
  (testing "a removed row above the eye pulls the viewport back with it"
    (let
      [p
       (-> (pane :rows 12 :order :newest-first)
           (lv/expanded "jobs"))

       [parked eye anchor]
       (parked p 6 (trow-index p 4))

       cut
       (patched parked {:op :remove :node-id "jobs" :item-ids ["job-11" "job-10"]})

       cut-rows
       (rows-of cut)]

      (is (= (- (long eye) 2) (long (lv/offset cut cut-rows 6))))
      (is (= anchor (lv/anchor-at cut-rows (lv/offset cut cut-rows 6))))))
  (testing "an anchor whose row is gone falls back to the node it belonged to"
    (let
      [p
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
    (let
      [p
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
    (let
      [p
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
       (patched taught
                {:op :append :node-id "jobs" :rows [(hi/table-row "job-1" ["job-1" "ok" "1m0s"])]})

       after
       (:widths (meta (rows-of narrow)))]

      (is (= (get-in measured ["jobs"]) (get-in after ["jobs"]))
          "a shorter value never shuffles the columns the human already read")
      (is (> (long (second (get measured "jobs"))) (count "a state"))))))

(deftest live-view-fresh-test
  (testing "what the LAST patch touched is what is emphasised, and only until the next one"
    (let
      [p
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
    (let
      [a
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
  (testing "Escape ARMS the stop instead of firing it — nothing ends until Enter"
    (let [p (lv/armed (lv/opened (ci-view)))]
      (is (= "" (lv/stopping p)) "armed, with nothing typed into it yet")
      (is (= "" (:note (lv/stop-prompt p))))
      (is (str/includes? (:label (lv/stop-prompt p)) "CI · fix(loop): move the session pick")
          "the prompt names the view it would stop, because several may be open")
      (is (nil? (lv/stop-prompt (lv/opened (ci-view)))) "a watched view asks nothing")
      (is (nil? (lv/stopping (lv/disarmed p))) "Escape again is watching again")))
  (testing "the note is typed with the same keyboard a form's fields read"
    (let
      [typing
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
      (let [{:keys [pane action]} (lv/typed p {:kind :cancel})]
        (is (= :keep action))
        (is (nil? (lv/stopping pane)) "keeping watch forgets what was typed"))
      (is (nil? (:action (lv/typed p {:kind :next})))
          "a key the line has no use for changes nothing")))
  (testing "the line stops growing where the engine cuts it"
    (let
      [p (reduce (fn [pane c]
                   (:pane (lv/typed pane {:kind :char :char c})))
                 (lv/armed (lv/opened (ci-view)))
                 (repeat (+ 10 (long hi-spec/note-chars)) \x))]
      (is (= (long hi-spec/note-chars) (count (lv/stopping p)))
          "a field that swallowed more than the model will read would lie about the note")))
  (testing "the band gives the armed line a row of its own, above the fence"
    (let
      [p
       (lv/opened (ci-view))

       watched
       (painted-text [p])

       armed
       (painted-text [(:pane (reduce (fn [{:keys [pane]} c]
                                       (lv/typed pane {:kind :char :char c}))
                                     {:pane (lv/armed p)}
                                     "wrong subnet"))])]

      (is (not (str/includes? watched "why?")) "a view being watched asks nothing")
      (is (str/includes? armed "why? wrong subnet") "the words are on screen as they are typed")
      (is (str/includes? armed "interrupt CI · fix(loop): move the session pick")
          "and the line says WHICH view they will stop")
      (is (str/includes? armed "keep watching")
          "the hint bar switches to the two keys that end the typing")
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
    (with-db (fn []
               (let [view (assoc (ci-view) :session-id "s1")]
                 (state/dispatch [:live-view-open view])
                 (is (= ["view-1"] (mapv lv/view-id (:live-views @state/app-db))))
                 (state/dispatch
                   [:live-view-patch
                    (engine/normalize-patch view [{:op :set :node-id "now" :text "Done"}])])
                 (is (= "Done" (:text (first (:nodes (:view (first (:live-views @state/app-db)))))))
                     "the ENGINE advanced the view; the terminal never interprets a patch itself")
                 (state/dispatch [:live-view-close "view-1"])
                 (is (empty? (:live-views @state/app-db)))))))
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

;;; ── The screenshot gate ─────────────────────────────────────────────────────

(deftest live-view-paint-test
  (testing "the band paints the whole view when it fits"
    (let
      [{:keys [frames geometry]}
       (paint-frames [(pane)] 96 40)

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
    (let
      [p
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
    (let
      [before
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
    (let
      [p
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
    (let
      [text (painted-text
              [(lv/opened (mounted {} (hi/progress "done" {:label "Finished" :done 15 :total 18})))]
              80
              20)]
      (is (str/includes? text "83%"))
      (is (str/includes? text "15/18 done"))
      (is (str/includes? text "▰") "15 of 18 is a measured fraction, not indeterminate work")
      (is (not (str/includes? text "working")))))
  (testing "a progress nobody can size still says the one true thing"
    (let
      [text
       (painted-text [(lv/opened (mounted {} (hi/progress "done" {:label "Scanning"})))] 80 20)]
      (is (str/includes? text "working"))
      (is (not (str/includes? text "▰"))))))
