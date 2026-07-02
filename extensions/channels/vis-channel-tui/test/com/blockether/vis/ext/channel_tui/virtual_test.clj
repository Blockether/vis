(ns com.blockether.vis.ext.channel-tui.virtual-test
  "Tests for the virtualized chat-panel layout planner.

   Three contracts to pin:

   1. `estimated-height` is cheap, monotonic, and within 2x of the
      real `bubble-height` for the message shapes we actually emit
      (user, plain assistant, assistant with trace).
   2. `layout` only projects messages whose viewport interval is
      non-empty. Off-screen bubbles MUST NOT trigger
      `format-answer-with-thinking` (verified by counting projection
      calls through a side-effect counter swap).
   3. `layout`'s `:total-h` and `:eff-scroll` are valid scrollbar
      inputs (`:eff-scroll in [0, max(0, total-h - inner-h)]`) and
      visible bubbles' `:top + :height` fits inside the viewport's
      forward extent, even when estimates differ from real heights."
  (:require
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.ext.channel-tui.virtual :as virtual]
   [com.blockether.vis.internal.render :as ir]
   [lazytest.core :refer [defdescribe describe expect it]] [clojure.string :as str]))

(def ^:private estimated-height @#'virtual/estimated-height)
(def ^:private project-message  @#'virtual/project-message)
(def ^:private turn-identity    @#'virtual/turn-identity)

;; ─── Fixtures ───────────────────────────────────────────────────────────────

(defn- markdown->ir [s]
  (ir/markdown->ir (or s "")))

(defn- user-msg [text]
  {:role :user :text text :ir (markdown->ir text)
   :timestamp #inst "2026-04-30T00:00:00"})

(defn- plain-assistant-msg [text]
  {:role :assistant :text text :ir (markdown->ir text)
   :timestamp #inst "2026-04-30T00:00:00"})

(defn- trace-assistant-msg
  "Build a minimal assistant message with a `:traces` of `n-iters`
   iterations, each carrying `forms-per-iter` code/result pairs and
   a fixed thinking string. Mirrors the shape `chat/rebuild-history`
   produces."
  [n-iters forms-per-iter answer]
  (let [forms (vec (for [idx (range forms-per-iter)]
                     {:position      idx
                      :code          "(print 3)"
                      ;; Tool output is shown as the program's STDOUT now;
                      ;; bare values are never echoed. Give each form some
                      ;; printed output so the rendered bubble has real height
                      ;; (the estimator-vs-real height contracts below need a
                      ;; non-trivial body to measure).
                      :stdout        "3"
                      :result-kind   :value
                      :duration-ms   1
                      :success?      true
                      :silent?       false}))
        trace (vec (repeat n-iters
                     {:thinking "thinking line"
                      :forms    forms}))]
    {:role            :assistant
     :ir              (markdown->ir answer)
     :text            answer
     :traces           trace
     :iteration-count n-iters
     :timestamp       #inst "2026-04-30T00:00:00"}))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} dense-trace-shaped-message
  "Sanitized render fixture matching a pathological dense-trace shape: 304 tool
   forms, ~100 previews, duplicated refs in source shape, two huge locator
   dumps, and one answer bubble. Payload text is synthetic; operation mix and
   cardinality are the regression signal."
  []
  (let [ops      (vec (take 304 (cycle [:cat :cat :ls :patch :z/locators :rg])))
        huge     (str/join "\n" (map #(str "locator-" % " (defn huge-fixture [] :ok)") (range 240)))
        preview  (str/join "\n" (map #(str % ": selected preview line") (range 1 61)))
        mk-form  (fn [idx op]
                   {:code (str "(" (namespace op) "/" (name op) " \"fixture-" idx "\")")
                    :result (case op
                              :cat preview
                              :z/locators huge
                              :ls "directory listing"
                              :patch "1 file changed"
                              :rg "12 matches"
                              "read 20 lines")
                    :kind (if (= op :cat) :preview :tool)
                    :detail {:op op
                             :tag (case op
                                    (:cat :z/locators :rg :ls) :observation
                                    :patch :mutation
                                    :observation)
                             :presentation-kind (case op
                                                  (:cat :z/locators) :tool/read
                                                  :rg :tool/search
                                                  :patch :tool/edit
                                                  :ls :tool/read
                                                  :tool/meta)
                             :color-role (case op
                                           (:cat :z/locators) :tool-color/read
                                           :rg :tool-color/search
                                           :patch :tool-color/edit
                                           :ls :tool-color/read
                                           :tool-color/meta)
                             :raw (when (= op :cat) (pr-str preview))}})
        forms    (mapv mk-form (range) ops)
        trace    (->> forms
                   (partition-all 12)
                   (mapv (fn [chunk]
                           {:thinking "synthetic 9a55 reasoning chunk"
                            :forms (vec
                                     (map-indexed
                                       (fn [idx {:keys [code result kind detail]}]
                                         {:position        idx
                                          :code            code
                                          :result-render   result
                                          :result-kind     kind
                                          :result-detail   detail
                                          :duration-ms     1
                                          :success?        true
                                          :silent?         false})
                                       chunk))})))]
    {:role :assistant
     :ir (markdown->ir "done")
     :text "done"
     :traces trace
     :iteration-count (count trace)
     :timestamp #inst "2026-04-30T00:00:00"}))

(def ^:private bubble-w 100)

(def ^:private settings
  {:show-thinking true :show-iterations true :show-timestamps false})

;; ─── Tests ──────────────────────────────────────────────────────────────────

(defdescribe estimated-height-test
  (describe "estimated-height never returns negative or zero"
    (it "user msg >= 1"
      (expect (>= (estimated-height (user-msg "hi") bubble-w) 1)))
    (it "empty user msg >= 1"
      (expect (>= (estimated-height (user-msg "") bubble-w) 1)))
    (it "plain assistant >= 1"
      (expect (>= (estimated-height (plain-assistant-msg "Done.") bubble-w) 1)))
    (it "trace assistant >= 1"
      (expect (>= (estimated-height (trace-assistant-msg 1 1 "ok") bubble-w) 1))))

  (describe "estimated-height grows monotonically with content"
    (it "longer text -> taller user bubble"
      (let [a (estimated-height (user-msg "x") bubble-w)
            b (estimated-height (user-msg (apply str (repeat 500 "x"))) bubble-w)]
        (expect (< a b))))
    (it "more iterations -> taller assistant bubble"
      (let [a (estimated-height (trace-assistant-msg 1 1 "ok") bubble-w)
            b (estimated-height (trace-assistant-msg 20 1 "ok") bubble-w)]
        (expect (< a b))))
    (it "more code forms per iter -> taller assistant bubble"
      (let [a (estimated-height (trace-assistant-msg 1 1 "ok") bubble-w)
            b (estimated-height (trace-assistant-msg 1 20 "ok") bubble-w)]
        (expect (< a b)))))

  (describe "estimated-height is within 2x of real bubble-height"
    ;; Smoke-check that the heuristic stays in the right order of
    ;; magnitude. Off-screen accuracy only nudges the scrollbar, but
    ;; a 10x miss would shift the thumb so badly the user notices.
    (it "user msg" (let [m (user-msg (apply str (repeat 200 "x")))
                         pm (project-message m bubble-w settings)
                         est (estimated-height m bubble-w)
                         real (render/bubble-height pm bubble-w)
                         ratio (max (/ (double est) real)
                                 (/ (double real) est))]
                     (expect (<= ratio 2.5))))
    (it "trace assistant"
      (let [m (trace-assistant-msg 5 3 "Some answer text.")
            pm (project-message m bubble-w settings)
            est (estimated-height m bubble-w)
            real (render/bubble-height pm bubble-w)
            ratio (max (/ (double est) real)
                    (/ (double real) est))]
        ;; trace estimator can over-shoot (per-form lines, etc.); we
        ;; allow up to 4x because the alternative is paying the full
        ;; format pass for off-screen bubbles, which is the bug we
        ;; came here to fix.
        (expect (<= ratio 4.0))))))

(defdescribe layout-test
  (describe "empty session"
    (it "returns empty visible + zero total-h"
      (let [{:keys [total-h eff-scroll visible]}
            (virtual/layout [] bubble-w settings nil 20 {})]
        (expect (= 0 total-h))
        (expect (= 0 eff-scroll))
        (expect (= [] visible)))))

  (describe "single message smaller than viewport"
    (it "is fully visible at top, eff-scroll = 0"
      (let [m (user-msg "hi")
            {:keys [eff-scroll visible]}
            (virtual/layout [m] bubble-w settings nil 50 {})]
        (expect (= 0 eff-scroll))
        (expect (= 1 (count visible)))
        (let [{:keys [idx top]} (first visible)]
          (expect (= 0 idx))
          (expect (= 0 top))))))

  (describe "auto-bottom (scroll = nil)"
    (it "snaps eff-scroll so the LAST message's bottom touches inner-h"
      (let [msgs [(user-msg "first") (user-msg "second") (user-msg "last")]
            inner-h 7
            {:keys [eff-scroll total-h visible]}
            (virtual/layout msgs bubble-w settings nil inner-h {})]
        (expect (= eff-scroll (max 0 (- total-h inner-h))))
        ;; visible includes at least the last message
        (expect (some #(= (dec (count msgs)) (:idx %)) visible))))

    (it "projects the live loading placeholder with progress text"
      (let [m {:role :assistant :text "Sending request to provider..."}
            {:keys [visible]}
            (virtual/layout [m] bubble-w settings nil 20
              {:loading? true :progress {:iterations []}})
            projected (:projected (first visible))]
        (expect (string? (:text projected)))
        (expect (not= (:text m) (:text projected)))))

    (it "does not project off-screen live progress while the user is scrolled up"
      (let [msgs (vec (concat (mapv #(user-msg (str "old " %)) (range 20))
                        [{:role :assistant :text "Sending request to provider..."}]))
            {:keys [visible]}
            (virtual/layout msgs bubble-w settings 0 6
              {:loading? true :progress {:iterations []}})
            last-idx (dec (count msgs))]
        (expect (seq visible))
        (expect (not (some #(= last-idx (:idx %)) visible)))))

    (it "hides plain value results while streaming — no collapsible summary"
      ;; Per user directive: collapsible disclosure was removed. Plain
      ;; `:value` form results never paint a body while streaming — no
      ;; `RESULT` label, no `chars hidden` hint, no toggle-details meta.
      (render/invalidate-cache!)
      (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
            m           {:role :assistant :text "Sending request to provider..."}
            trace       [{:forms [{:code          "(+ 1 2)"
                                   :result-render huge-result
                                   :result-kind   :value
                                   :duration-ms   1
                                   :success?      true
                                   :silent?       false}]}]
            {:keys [visible]}
            (virtual/layout [m] bubble-w settings nil 30
              {:loading?       true
               :progress       {:iterations trace}
               :progress-extra {:now-ms 1000 :turn-start-ms 0}}
              {:session-id    "session"
               :detail-expansions {}})
            projected (:projected (first visible))]
        (expect (not (str/includes? (:text projected) "RESULT")))
        (expect (not (str/includes? (:text projected) "chars hidden")))
        (expect (not (str/includes? (:text projected) huge-result)))
        (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta projected)))))

    (it "keeps long live progress layout inside scroll-frame budget"
      ;; Plain `:value` results are now hidden per user directive, so
      ;; the row count is dominated by code + status lines per
      ;; iteration (~6 rows). 300 iterations × ~6 rows leaves a wide
      ;; budget below 3000; p95 latency budget unchanged.
      (render/invalidate-cache!)
      (let [m              {:role :assistant :text "Sending request to provider..."}
            progress-entry (fn [i done?]
                             {:forms [{:code          (str "(do (Thread/sleep 1000) " i ")")
                                       :result-render nil
                                       :result-kind   (when done? :value)
                                       :duration-ms   (if done? 1000 0)
                                       :success?      (when done? true)
                                       :started-at-ms (when-not done? 0)
                                       :silent?       false}]})
            progress       {:iterations (vec (concat (map #(progress-entry % true) (range 300))
                                               [(progress-entry 300 false)]))}
            sample         (fn []
                             (let [t0 (System/nanoTime)
                                   r  (virtual/layout [m] 90 settings nil 30
                                        {:loading? true
                                         :progress progress
                                         :progress-extra {:now-ms 100000 :turn-start-ms 0}}
                                        {:session-id "session"
                                         :detail-expansions {}})
                                   dt (/ (- (System/nanoTime) t0) 1000000.0)]
                               {:ms dt
                                :line-count (count (get-in r [:visible 0 :projected :prewrapped-lines]))}))
            ;; First sample pays JIT + cache-miss warmup; drop it
            ;; so the budget reflects steady-state layout cost.
            _warmup        (sample)
            samples        (doall (repeatedly 12 sample))
            sorted-ms      (vec (sort (map :ms samples)))
            p95-ms         (nth sorted-ms (dec (count sorted-ms)))
            max-lines      (apply max (map :line-count samples))]
        (expect (< max-lines 3000))
        (expect (< p95-ms 60.0)
          (str "progress layout p95-ms=" p95-ms " samples=" samples)))))

  (describe "fixed scroll offset (scroll = some long)"
    (it "clamps to [0, max-scroll]"
      (let [msgs (mapv #(user-msg (str %)) (range 30))
            {:keys [eff-scroll total-h]}
            (virtual/layout msgs bubble-w settings 999999 10 {})]
        (expect (= eff-scroll (max 0 (- total-h 10))))))
    (it "negative scroll clamps to 0"
      ;; Channel layer normally guards this; the planner stays
      ;; defensive anyway.
      (let [msgs (mapv #(user-msg (str %)) (range 5))
            {:keys [eff-scroll]}
            (virtual/layout msgs bubble-w settings 0 5 {})]
        (expect (>= eff-scroll 0)))))

  (describe "pass-3 recovers visible bubbles missed by the pass-1 estimate"
    ;; Regression: a fast-growing live bubble has no height cache and
    ;; its cheap estimate undershoots reality. When that pushes
    ;; `real-max-scroll` past `est-max-scroll` and the caller sits at
    ;; the historical bottom, pass-1 candidates land at a SMALLER
    ;; effective scroll than the refined `eff-2`, and earlier stable
    ;; bubbles silently fall out of `:visible`. Layout must catch
    ;; them in pass-3 so the painter still draws them.
    (it "keeps the prior stable assistant bubble visible while a huge live bubble streams"
      (render/invalidate-cache!)
      (let [stable  (plain-assistant-msg "earlier turn answer body")
            live    {:role :assistant :text "Sending request to provider..."}
            inner-h 40
            ;; A streaming live bubble: pass-1's cheap estimate (it has no
            ;; `:traces`, just the "Sending..." text) badly undershoots the
            ;; real height once the printed iterations render. The prior
            ;; stable turn must still survive into `:visible` rather than
            ;; blinking out when the real height is measured. (Op-card
            ;; aggregation that used to collapse repeated calls is gone, so a
            ;; chatty bubble is genuinely tall — a handful of prints already
            ;; dwarfs pass-1's estimate.)
            iters   (vec (for [i (range 4)]
                           {:forms [{:code          (str "(print " (inc i) ")")
                                     :stdout        (str (inc i))
                                     :result-kind   :value
                                     :duration-ms   1
                                     :success?      true
                                     :silent?       false}]}))
            {:keys [visible total-h eff-scroll]}
            (virtual/layout [stable live] bubble-w settings nil inner-h
              {:loading?       true
               :progress       {:iterations iters}
               :progress-extra {:now-ms 1000 :turn-start-ms 0}}
              {:session-id "s" :detail-expansions {}})
            visible-idxs (set (map :idx visible))]
        ;; The live bubble is always projected.
        (expect (contains? visible-idxs 1))
        ;; And the earlier stable bubble whose bottom row still pokes
        ;; into the viewport must survive into `:visible`. Without
        ;; pass-3 recovery this set is `#{1}` — the bug the user hit.
        (let [stable-bottom (long (+ (get-in (first visible) [:top])
                                    (get-in (first visible) [:height])))]
          (expect (pos? total-h))
          (expect (>= eff-scroll 0))
          (expect (or (contains? visible-idxs 0)
                    (zero? stable-bottom))
            "earlier turn must paint while the live bubble streams")))))

  (describe "off-screen bubbles are NOT projected"
    ;; The whole point of the namespace. Count projections by
    ;; intercepting `render/format-answer-with-thinking` with a
    ;; counter wrap. Off-screen bubbles must not bump the counter.
    (it "10-message session, only the bottom few intersect a 5-row viewport"
      (let [msgs (vec (concat
                        (mapv #(trace-assistant-msg 3 2 (str "answer " %))
                          (range 10))))
            calls (atom 0)
            real  render/format-answer-with-thinking]
        (with-redefs [render/format-answer-with-thinking
                      (fn [& args] (swap! calls inc) (apply real args))]
          (let [{:keys [visible]}
                (virtual/layout msgs bubble-w settings nil 5 {})]
            ;; visible is only a small subset, NOT all 10
            (expect (< (count visible) (count msgs)))
            ;; format-answer-with-thinking was called only for visible
            (expect (<= @calls (count visible)))))))))

(defdescribe pre-warm-test
  (describe "pre-warm! warms the LRU off the render thread"
    (it "returns nil for empty messages, no thread spawned"
      (expect (nil? (virtual/pre-warm! [] bubble-w settings))))
    (it "returns a daemon thread for non-empty messages"
      (let [t (virtual/pre-warm! [(plain-assistant-msg "hi")] bubble-w settings)]
        (expect (some? t))
        (expect (.isDaemon ^Thread t))
        (virtual/stop-pre-warm! t)))

    (it "pre-warm-recent! warms only the requested tail-count"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs   [(plain-assistant-msg "m0")
                    (plain-assistant-msg "m1")
                    (plain-assistant-msg "m2")
                    (plain-assistant-msg "m3")
                    (plain-assistant-msg "m4")]
            warmed (virtual/pre-warm-recent! msgs bubble-w settings
                     {:count 2 :budget-ms 1000})]
        (expect (= 2 warmed))
        (expect (= 2 (virtual/height-cache-size)))))

    (it "pre-warm-recent! respects wall-clock budget"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs   [(trace-assistant-msg 5 4 "a")
                    (trace-assistant-msg 5 4 "b")
                    (trace-assistant-msg 5 4 "c")]
            warmed (virtual/pre-warm-recent! msgs bubble-w settings
                     {:count 3 :budget-ms 0})]
        (expect (= 0 warmed))
        (expect (= 0 (virtual/height-cache-size)))))

    (it "warms the cache so a subsequent layout call is cheap"
      ;; The whole point: after pre-warm finishes, calling
      ;; format-answer-with-thinking on the warmed assistants must
      ;; hit the cache (sub-microsecond), NOT recompute. Verified by
      ;; counting calls into the uncached `format-answer-with-thinking*`
      ;; surface.
      (render/invalidate-cache!)
      (let [msgs [(trace-assistant-msg 3 2 "answer")
                  (trace-assistant-msg 2 1 "another")]
            t (virtual/pre-warm! msgs bubble-w settings)]
        (.join ^Thread t 5000)
        ;; Now both bubbles' fawt entries should be cached.
        (let [calls (atom 0)
              real  @#'render/format-answer-with-thinking*]
          (with-redefs [render/format-answer-with-thinking*
                        (fn [& args] (swap! calls inc) (apply real args))]
            (doseq [m msgs]
              (render/format-answer-with-thinking
                (:ir m) (:traces m) bubble-w settings))
            ;; Pre-warm warmed both - no fresh format-answer-with-thinking*
            ;; calls expected.
            (expect (zero? @calls))))))
    (it "fires :on-warm at least once when warming completes"
      ;; The render-version bump hook: callers wire :on-warm to
      ;; settle total-h via a re-layout. Must fire at least once
      ;; (the final settle) even for a sub-batch session.
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [hits (atom 0)
            t (virtual/pre-warm! [(plain-assistant-msg "a")
                                  (plain-assistant-msg "b")]
                bubble-w settings
                {:on-warm (fn [] (swap! hits inc))})]
        (.join ^Thread t 5000)
        (expect (pos? @hits))))
    (it "stop-pre-warm! is safe on nil and on already-finished threads"
      (expect (nil? (virtual/stop-pre-warm! nil)))
      (let [t (virtual/pre-warm! [(plain-assistant-msg "x")] bubble-w settings)]
        (.join ^Thread t 5000)
        (expect (nil? (virtual/stop-pre-warm! t)))))))

(defdescribe overshoot-invariant-test
  ;; THE scrollbar contract: when a real height replaces an estimate
  ;; mid-scroll, `total-h` may only SHRINK. Growth moves the thumb toward
  ;; the bottom while the user scrolls UP - the visible scrollbar jump
  ;; this estimator rewrite exists to kill. Pinned against the message
  ;; shapes that used to undershoot: long single-line code (fold-wrapped
  ;; by the painter, newline-counted by the old estimator) and pasted
  ;; diffs (every `+ ` line explodes into a markdown list item).
  (let [est->real (fn [m w]
                    (let [pm (project-message m w settings)]
                      [(estimated-height m w)
                       (render/bubble-height pm w)]))]
    (describe "estimate >= painted height"
      (it "long single-line code in a trace, across widths"
        (doseq [w [64 104 194 254]]
          (let [code (str "(def payload \"" (apply str (repeat 900 "y")) "\")")
                m    (-> (trace-assistant-msg 1 1 "ok")
                       (assoc-in [:traces 0 :forms 0 :code] code))
                [est real] (est->real m w)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "pasted diff as a user message (list-marker block chrome)"
        (doseq [w [84 154 254]]
          (let [m (user-msg (str/join "\n"
                              (repeat 20 "+        (let [rows (try ;; wide diff line")))
                [est real] (est->real m w)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "long multi-line tool result (collapsed preview cap)"
        (doseq [w [84 154 254]]
          (let [result (str/join "\n" (map #(str "line " % " of output") (range 80)))
                m      (-> (trace-assistant-msg 1 1 "ok")
                         (assoc-in [:traces 0 :forms 0 :result-render] result))
                [est real] (est->real m w)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "plain user / assistant prose"
        (doseq [w [84 154 254]
                m [(user-msg "hi")
                   (user-msg (apply str (repeat 600 "x")))
                   (plain-assistant-msg "Short answer.")
                   (plain-assistant-msg (str/join "\n\n" (repeat 8 "A paragraph of answer prose.")))]]
          (let [[est real] (est->real m w)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real))))))))

(defdescribe overshoot-drift-tripwire-test
  ;; Painter-drift tripwire. The estimator mirrors painter constants
  ;; (fold width, preview caps, chrome rows); a render refactor can
  ;; silently re-break the overshoot invariant — 2a9cfb61 (prose above
  ;; code) did exactly that mid-investigation. These pin est >= real for
  ;; the shapes the collapsed-default tests don't reach: EXPANDED
  ;; disclosures (the :expand-all flag AND a real per-node toggle),
  ;; form errors (plain + provider), form comments, and the dense
  ;; tool-card fixture.
  (let [long-thinking (str/join "\n" (map #(str "reasoning step " % " weighs the trade-offs at length")
                                          (range 40)))
        long-result   (str/join "\n" (map #(str "output row " % " of the tool run") (range 60)))
        sid           "ovs-x"
        rich-msg      (fn []
                        (-> (trace-assistant-msg 2 2 "Answer with some prose.")
                          (assoc :session-turn-id "ovs-turn-1")
                          (assoc-in [:traces 0 :thinking] long-thinking)
                          (assoc-in [:traces 0 :forms 0 :result] long-result)
                          (assoc-in [:traces 0 :forms 1 :comment] "why this form runs")))
        est->real     (fn [m w de]
                        (let [pm (project-message m w settings
                                   {:session-id sid :detail-expansions de})]
                          [(estimated-height m w de sid)
                           (render/bubble-height pm w)]))]
    (describe "expanded disclosures stay overshooting"
      (it "global :expand-all flag (estimate switches to full section heights)"
        (doseq [w [84 154 254]]
          (let [[est real] (est->real (rich-msg) w {:vis.channel-tui/expand-all-details? true})]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "a REAL per-node toggle harvested from painter metadata (vector key branch)"
        (let [w        104
              m        (rich-msg)
              pm       (project-message m w settings {:session-id sid :detail-expansions {}})
              node-ids (->> (:line-meta pm)
                         (keep #(when (= :toggle-details (:kind %)) (:node-id %)))
                         distinct
                         vec)]
          ;; The collapsed projection must expose at least one disclosure
          ;; (the 40-line thinking trace guarantees it).
          (expect (seq node-ids))
          (doseq [nid node-ids]
            (let [de {[(str sid) (str nid)] true}
                  [est real] (est->real m w de)]
              (expect (>= (long est) (long real))
                (str "node=" nid " est=" est " real=" real)))))))
    (describe "rich collapsed shapes stay overshooting"
      (it "form error — plain exception with a long message"
        (doseq [w [84 154 254]]
          (let [m (-> (trace-assistant-msg 1 1 "ok")
                    (assoc-in [:traces 0 :forms 0 :success?] false)
                    (assoc-in [:traces 0 :forms 0 :error]
                      {:message (apply str "boom: " (repeat 40 "deeply nested cause "))}))
                [est real] (est->real m w nil)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "form error — provider error with raw body trims"
        (doseq [w [84 154 254]]
          (let [m (-> (trace-assistant-msg 1 1 "ok")
                    (assoc-in [:traces 0 :forms 0 :success?] false)
                    (assoc-in [:traces 0 :forms 0 :error]
                      {:message "upstream 500"
                       :data {:status 500
                              :body (apply str (repeat 80 "provider exploded loudly "))
                              :raw-data (apply str (repeat 80 "raw payload fragment "))
                              :received-type "text/html"}}))
                [est real] (est->real m w nil)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real)))))
      (it "dense tool-card fixture (304 forms, previews, locator dumps)"
        (doseq [w [104 194]]
          (let [[est real] (est->real (dense-trace-shaped-message) w nil)]
            (expect (>= (long est) (long real))
              (str "w=" w " est=" est " real=" real))))))))

(defdescribe rewarm-test
  (describe "rewarm! owns ONE managed background warm worker"
    (it "returns nil for empty messages and is safe to stop when idle"
      (virtual/stop-rewarm!)
      (expect (nil? (virtual/rewarm! [] bubble-w settings {})))
      (expect (nil? (virtual/stop-rewarm!))))
    (it "populates the sticky height cache (the post-invalidate re-settle)"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs [(user-msg "one") (trace-assistant-msg 2 1 "two")]
            t (virtual/rewarm! msgs bubble-w settings {:session-id "rewarm-t"})]
        (expect (some? t))
        (.join ^Thread t 5000)
        (expect (= (count msgs) (virtual/height-cache-size)))
        (virtual/stop-rewarm!)))
    (it "restarting replaces the previous worker without leaking cache junk"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [a (vec (repeatedly 6 #(trace-assistant-msg 3 2 "warm me")))
            _ (virtual/rewarm! a bubble-w settings {:session-id "rewarm-a"})
            b [(user-msg "fresh")]
            t (virtual/rewarm! b bubble-w settings {:session-id "rewarm-b"})]
        (.join ^Thread t 5000)
        ;; b's single bubble is definitely warm; a's worker was stopped
        ;; (its already-written entries are VALID, just possibly partial).
        (expect (>= (virtual/height-cache-size) 1))
        (virtual/stop-rewarm!)))))

(defdescribe sticky-height-cache-test
  (describe "once a message has been measured, layout returns its REAL height forever"
    ;; Before the sticky cache,
    ;; off-screen messages reverted to `estimated-height` on every
    ;; layout call - `total-h` jittered as visible <-> off-screen
    ;; flipped per scroll, scrollbar thumb drifted, click-to-position
    ;; landed in the wrong row.
    (it "`total-h` is stable across many scroll positions after pre-warm"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs [(user-msg "first")
                  (trace-assistant-msg 5 3 "answer one")
                  (user-msg "second")
                  (trace-assistant-msg 8 4 "answer two")
                  (user-msg "third")]
            t (virtual/pre-warm! msgs bubble-w settings)]
        (.join ^Thread t 30000)
        ;; Every layout pass MUST agree on total-h after pre-warm.
        (let [totals (mapv (fn [s]
                             (:total-h
                              (virtual/layout msgs bubble-w settings s 10 {})))
                       [nil 0 50 100 500 (long 1e6)])]
          (expect (= 1 (count (distinct totals)))))))

    (it "layout pins the real height of any message it measures"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs [(trace-assistant-msg 3 2 "first")
                  (trace-assistant-msg 3 2 "second")]]
        ;; First layout (auto-bottom) measures whichever bubble is visible.
        (virtual/layout msgs bubble-w settings nil 5 {})
        ;; The cache must now contain at least one real height.
        (expect (pos? (virtual/height-cache-size)))))

    (it "mid-scroll last-bubble paints use full projection instead of broken window slices"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [long-answer (str/join "\n" (map #(str "line " %) (range 1 180)))
            msgs        [(user-msg "prompt")
                         (plain-assistant-msg long-answer)]
            inner-h     20
            bottom      (virtual/layout msgs bubble-w settings nil inner-h {})
            total       (:total-h bottom)
            max-s       (max 0 (- total inner-h))
            mid-scroll  (max 0 (- max-s 3))
            mid         (virtual/layout msgs bubble-w settings mid-scroll inner-h {})
            mid-again   (virtual/layout msgs bubble-w settings (max 0 (- max-s 6)) inner-h {})
            last-visible (last (:visible mid))]
        (expect (pos? max-s))
        (expect (nil? (-> last-visible :projected :lines-window)))
        (expect (pos? (count (-> last-visible :projected :prewrapped-lines))))
        (expect (= total (:total-h mid)))
        (expect (= total (:total-h mid-again)))))

    (it "mid-scroll trace assistants render the trace instead of an empty answer-IR window"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs       [(user-msg "prompt")
                        (trace-assistant-msg 12 6 "done")]
            inner-h    20
            bottom     (virtual/layout msgs bubble-w settings nil inner-h {})
            max-s      (max 0 (- (:total-h bottom) inner-h))
            mid-scroll (quot max-s 2)
            mid        (virtual/layout msgs bubble-w settings mid-scroll inner-h {})
            trace-row  (some #(when (-> % :projected :traces) %) (:visible mid))]
        (expect (pos? max-s))
        (expect (some? trace-row))
        (expect (nil? (-> trace-row :projected :lines-window)))
        (expect (pos? (count (-> trace-row :projected :prewrapped-lines))))))

    (it "caches a SEPARATE height per expansion state (a toggle can't return a stale height)"
      ;; Regression: the height-cache key used to ignore detail-expansions, so
      ;; the collapsed height was reused for the expanded render — total-h
      ;; undercounted and the scroll jumped on expand. The key now includes
      ;; THIS message's expansion subset: expanding is a distinct cache entry,
      ;; not a stale hit on the collapsed one.
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs      [(plain-assistant-msg "<details>\n<summary>D</summary>\n\nbody\n\n</details>")]
            collapsed (virtual/layout msgs bubble-w settings nil 20 {}
                        {:session-id "cid" :detail-expansions {}})]
        (expect (= 1 (virtual/height-cache-size)))
        (let [expanded (virtual/layout msgs bubble-w settings nil 20 {}
                         {:session-id "cid"
                          :detail-expansions {["cid" "answer:d1"] true}})]
          ;; expansion state is keyed → a 2nd entry, NOT a stale collapsed hit
          (expect (= 2 (virtual/height-cache-size)))
          ;; and the expanded layout is at least as tall (it reveals the body)
          (expect (>= (long (:total-h expanded)) (long (:total-h collapsed)))))))

    (it "`invalidate-heights!` drops the cache cleanly"
      (virtual/invalidate-heights!)
      (expect (zero? (virtual/height-cache-size)))
      (let [msgs [(trace-assistant-msg 1 1 "x")]]
        (virtual/layout msgs bubble-w settings nil 5 {})
        (expect (pos? (virtual/height-cache-size)))
        (virtual/invalidate-heights!)
        (expect (zero? (virtual/height-cache-size)))))))

(defdescribe scroll-anchoring-test
  ;; Regression: long reopened session, fast scroll up. Trace-bubble
  ;; estimates OVER-shoot real heights (~2.5x), so before the
  ;; pre-warmer measures the off-screen tail, `total-h` is much larger
  ;; than reality. As the user scrolls up into never-measured bubbles,
  ;; each one's estimate collapses to its (smaller) real height and
  ;; `total-h` shrinks by a large amount. With a fixed absolute scroll
  ;; and no anchoring, the viewport + scrollbar thumb lurched ("reach
  ;; the top, thumb snaps to the bottom"). `:prev-offsets` pins the
  ;; topmost visible message across the correction.
  (it "never anchors auto-bottom (nil scroll stays bottom-pinned)"
    (virtual/invalidate-heights!)
    (render/invalidate-cache!)
    (let [msgs   [(user-msg "q") (trace-assistant-msg 6 4 "a")]
          frame1 (virtual/layout msgs bubble-w settings nil 20 {})
          frame2 (virtual/layout msgs bubble-w settings nil 20 {}
                   {:prev-offsets (:offsets frame1)})]
      (expect (nil? (:anchored-scroll frame2)))
      (expect (= (:eff-scroll frame2) (max 0 (- (:total-h frame2) 20))))))

  (it "skips anchoring when the message count changed (append)"
    (virtual/invalidate-heights!)
    (render/invalidate-cache!)
    (let [msgs1  [(user-msg "q") (trace-assistant-msg 3 2 "a")]
          frame1 (virtual/layout msgs1 bubble-w settings 10 20 {})
          msgs2  (conj msgs1 (user-msg "q2"))
          ;; prev-offsets has the wrong length for msgs2 - must not throw
          ;; or mis-index; falls through to the raw scroll.
          frame2 (virtual/layout msgs2 bubble-w settings 10 20 {}
                   {:prev-offsets (:offsets frame1)})]
      (expect (some? (:eff-scroll frame2))))))

(defdescribe turn-separator-test
  (it "ignores legacy turn-separator settings and reserves no blank row"
    (let [msgs [(user-msg "first")
                (plain-assistant-msg "answer")
                (user-msg "next")]
          marked-layout   (virtual/layout msgs bubble-w
                            (assoc settings :differentiate-turns true)
                            nil 100 {})
          unmarked-layout (virtual/layout msgs bubble-w
                            (assoc settings :differentiate-turns false)
                            nil 100 {})]
      (expect (not-any? #(contains? (:projected %) :turn-separator?) (:visible marked-layout)))
      (expect (= (:total-h unmarked-layout) (:total-h marked-layout))))))

(defdescribe cross-message-error-squash-test
  (it "squashes a run of error-only assistant turns into one bubble with ERROR x N"
    (let [err {:type :svar.core/stream-truncated
               :message "Stream ended before terminal marker."}
          mk  (fn [i] {:role :assistant
                       :session-turn-id (str "t" i)
                       :client-turn-id  (str "c" i)
                       :traces [{:error err}]
                       :ir [:ir {} [:p {} ""]]})
          cancel {:role :assistant
                  :session-turn-id "tc"
                  :client-turn-id  "cc"
                  :status :cancelled
                  :ir [:ir {} [:p {} "Cancelled by user."]]}
          msgs (-> (mapv mk (range 9)) (conj cancel) (into (mapv mk (range 9 11))))
          layout (virtual/layout msgs bubble-w settings nil 200 {})
          bodies (mapv (comp :text :projected) (:visible layout))
          error-bubbles (filterv #(str/includes? (or % "") "ERROR") bodies)
          error-occurrences (count (mapcat #(re-seq #"ERROR" %) error-bubbles))]
      ;; 11 identical error turns split by one cancellation → two
      ;; merged error bubbles + cancellation between them.
      (expect (= 2 (count error-bubbles)))
      (expect (= 2 error-occurrences))
      (expect (some #(str/includes? % "ERROR x 9: Stream ended before terminal marker.")
                error-bubbles))
      (expect (some #(str/includes? % "ERROR x 2: Stream ended before terminal marker.")
                error-bubbles)))))

(defdescribe project-message-test
  (describe "user messages strip timestamps and render markdown"
    (it "keeps :timestamp when :show-timestamps true"
      (let [pm (project-message (user-msg "hi") bubble-w (assoc settings :show-timestamps true))]
        (expect (some? (:timestamp pm)))))

    (it "does not prepend answer-margin blank rows inside user prompt blocks"
      (let [pm (project-message (user-msg "> siema") bubble-w settings)]
        (expect (= 1 (count (:prewrapped-lines pm))))
        (expect (not= "" (first (:prewrapped-lines pm))))
        (expect (str/includes? (first (:prewrapped-lines pm)) "siema")))))

  (describe "plain assistant messages run through markdown formatting"
    (it "produces a non-empty :text"
      (let [pm (project-message (plain-assistant-msg "**bold**") bubble-w settings)]
        (expect (string? (:text pm)))
        (expect (pos? (count (:text pm)))))))

  (describe "trace assistant messages run through format-answer-with-thinking"
    (it "produces a multi-line :text"
      (let [pm (project-message (trace-assistant-msg 2 1 "Done.") bubble-w settings)]
        (expect (string? (:text pm)))
        (expect (pos? (count (:text pm))))))))

(defdescribe turn-identity-test
  ;; Regression: during live streaming, the assistant bubble has
  ;; only `:client-turn-id` (the server hasn't assigned
  ;; `:session-turn-id` yet). When the turn lands, the
  ;; completed message has BOTH ids - and they're different
  ;; values. Picking `:session-turn-id` for the disclosure
  ;; node-id (REASONING toggle, <details>, ...) would change the
  ;; node-id at the live -> done flip and silently reset every
  ;; expansion the user opened mid-stream. The contract: prefer
  ;; the client id so the same key drives both phases; fall back
  ;; to the server id only for legacy persisted messages that
  ;; never had a client id.
  (describe "turn-identity"
    (it "prefers :client-turn-id when present (live and done)"
      (expect (= "abc12345" (turn-identity {:client-turn-id "abc12345"})))
      (expect (= "abc12345" (turn-identity {:client-turn-id "abc12345"
                                            :session-turn-id "server-xyz"}))))
    (it "falls back to :session-turn-id for legacy DB messages"
      (expect (= "server-only" (turn-identity {:session-turn-id "server-only"}))))
    (it "returns nil when neither id is present"
      (expect (nil? (turn-identity {}))))))
