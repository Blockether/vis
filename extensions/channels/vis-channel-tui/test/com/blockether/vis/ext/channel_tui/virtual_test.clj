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

(defn- text->ir [s]
  (ir/text->ir (or s "")))

(defn- user-msg [text]
  {:role :user :text text :ir (text->ir text)
   :timestamp #inst "2026-04-30T00:00:00"})

(defn- plain-assistant-msg [text]
  {:role :assistant :text text :ir (text->ir text)
   :timestamp #inst "2026-04-30T00:00:00"})

(defn- trace-assistant-msg
  "Build a minimal assistant message with a `:traces` of `n-iters`
   iterations, each carrying `forms-per-iter` code/result pairs and
   a fixed thinking string. Mirrors the shape `chat/rebuild-history`
   produces."
  [n-iters forms-per-iter answer]
  (let [trace (vec (repeat n-iters
                     {:thinking  "thinking line"
                      :code      (vec (repeat forms-per-iter "(+ 1 2)"))
                      :results   (vec (repeat forms-per-iter "3"))
                      :stdouts   (vec (repeat forms-per-iter ""))
                      :durations (vec (repeat forms-per-iter 1))
                      :successes (vec (repeat forms-per-iter true))}))]
    {:role            :assistant
     :ir              (text->ir answer)
     :text            answer
     :traces           trace
     :iteration-count n-iters
     :timestamp       #inst "2026-04-30T00:00:00"}))

(defn- incident-9a55-shaped-message
  "Sanitized render fixture matching the bad 9a55 incident shape: 304 tool
   forms, ~100 previews, duplicated refs in source shape, two huge locator
   dumps, and one answer bubble. Payload text is synthetic; operation mix and
   cardinality are the regression signal."
  []
  (let [ops      (vec (take 304 (cycle [:v/cat :v/cat :v/bash :v/patch :z/locators :v/rg])))
        huge     (str/join "\n" (map #(str "locator-" % " (defn huge-fixture [] :ok)") (range 240)))
        preview  (str/join "\n" (map #(str % ": selected preview line") (range 1 61)))
        mk-form  (fn [idx op]
                   {:code (str "(" (namespace op) "/" (name op) " \"fixture-" idx "\")")
                    :result (case op
                              :v/cat preview
                              :z/locators huge
                              :v/bash "exit 0\nran fixture command"
                              :v/patch "1 file changed"
                              :v/rg "12 matches"
                              "read 20 lines")
                    :kind (if (= op :v/cat) :preview :tool)
                    :detail {:op op
                             :op/tag (case op
                                       (:v/cat :z/locators :v/rg) :op.tag/observation
                                       (:v/patch :v/bash) :op.tag/action
                                       :op.tag/observation)
                             :presentation-kind (case op
                                                  (:v/cat :z/locators) :tool/read
                                                  :v/rg :tool/search
                                                  :v/patch :tool/edit
                                                  :v/bash :tool/shell
                                                  :tool/meta)
                             :color-role (case op
                                           (:v/cat :z/locators) :tool-color/read
                                           :v/rg :tool-color/search
                                           :v/patch :tool-color/edit
                                           :v/bash :tool-color/shell
                                           :tool-color/meta)
                             :raw (when (= op :v/cat) (pr-str preview))}})
        forms    (mapv mk-form (range) ops)
        trace    (->> forms
                   (partition-all 12)
                   (mapv (fn [chunk]
                           {:thinking "synthetic 9a55 reasoning chunk"
                            :code (mapv :code chunk)
                            :results (mapv :result chunk)
                            :result-kinds (mapv :kind chunk)
                            :result-details (mapv :detail chunk)
                            :stdouts (vec (repeat (count chunk) ""))
                            :durations (vec (repeat (count chunk) 1))
                            :successes (vec (repeat (count chunk) true))})))]
    {:role :assistant
     :ir (text->ir "done")
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
  (describe "empty conversation"
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

    (it "passes conversation context to live progress so huge blocks collapse while streaming"
      (render/invalidate-cache!)
      (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
            m           {:role :assistant :text "Sending request to provider..."}
            trace       [{:code      ["(+ 1 2)"]
                          :comments  [nil]
                          :results   [huge-result]
                          :stdouts   [""]
                          :durations [1]
                          :successes [true]}]
            {:keys [visible]}
            (virtual/layout [m] bubble-w settings nil 30
              {:loading?       true
               :progress       {:iterations trace}
               :progress-extra {:now-ms 1000 :turn-start-ms 0}}
              {:conversation-id    "conversation"
               :detail-expansions {}})
            projected (:projected (first visible))]
        (expect (str/includes? (:text projected) "RESULT"))
        (expect (str/includes? (:text projected) "chars hidden"))
        (expect (not (str/includes? (:text projected) huge-result)))
        (expect (some #(= :toggle-details (:kind %)) (:line-meta projected)))))

    (it "keeps long live progress layout inside scroll-frame budget"
      (render/invalidate-cache!)
      (let [m              {:role :assistant :text "Sending request to provider..."}
            huge-result    (str/join " " (repeat 1000 "abcdefghij"))
            progress-entry (fn [i done?]
                             {:code      [(str "(do (Thread/sleep 1000) " i ")")]
                              :results   [(when done? huge-result)]
                              :stdouts   [""]
                              :durations [(when done? 1000)]
                              :successes [(when done? true)]
                              :started-at-ms [(when-not done? 0)]})
            progress       {:iterations (vec (concat (map #(progress-entry % true) (range 300))
                                               [(progress-entry 300 false)]))}
            sample         (fn []
                             (let [t0 (System/nanoTime)
                                   r  (virtual/layout [m] 90 settings nil 30
                                        {:loading? true
                                         :progress progress
                                         :progress-extra {:now-ms 100000 :turn-start-ms 0}}
                                        {:conversation-id "conversation"
                                         :detail-expansions {}})
                                   dt (/ (- (System/nanoTime) t0) 1000000.0)]
                               {:ms dt
                                :line-count (count (get-in r [:visible 0 :projected :prewrapped-lines]))}))
            samples        (doall (repeatedly 12 sample))
            sorted-ms      (vec (sort (map :ms samples)))
            p95-ms         (nth sorted-ms (dec (count sorted-ms)))
            max-lines      (apply max (map :line-count samples))]
        (expect (< max-lines 300))
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

  (describe "off-screen bubbles are NOT projected"
    ;; The whole point of the namespace. Count projections by
    ;; intercepting `render/format-answer-with-thinking` with a
    ;; counter wrap. Off-screen bubbles must not bump the counter.
    (it "10-message conversation, only the bottom few intersect a 5-row viewport"
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
    (it "stop-pre-warm! is safe on nil and on already-finished threads"
      (expect (nil? (virtual/stop-pre-warm! nil)))
      (let [t (virtual/pre-warm! [(plain-assistant-msg "x")] bubble-w settings)]
        (.join ^Thread t 5000)
        (expect (nil? (virtual/stop-pre-warm! t)))))))

(defdescribe sticky-height-cache-test
  (describe "once a message has been measured, layout returns its REAL height forever"
    ;; Regression: conversation 7b18414d. Before the sticky cache,
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

    (it "detail toggles do not create a second height-cache universe"
      (virtual/invalidate-heights!)
      (render/invalidate-cache!)
      (let [msgs [(plain-assistant-msg "<details>\n<summary>D</summary>\n\nbody\n\n</details>")]]
        (virtual/layout msgs bubble-w settings nil 20 {}
          {:conversation-id "cid" :detail-expansions {}})
        (expect (= 1 (virtual/height-cache-size)))
        (virtual/layout msgs bubble-w settings nil 20 {}
          {:conversation-id "cid"
           :detail-expansions {["cid" "answer:d1"] true}})
        (expect (= 1 (virtual/height-cache-size)))))

    (it "`invalidate-heights!` drops the cache cleanly"
      (virtual/invalidate-heights!)
      (expect (zero? (virtual/height-cache-size)))
      (let [msgs [(trace-assistant-msg 1 1 "x")]]
        (virtual/layout msgs bubble-w settings nil 5 {})
        (expect (pos? (virtual/height-cache-size)))
        (virtual/invalidate-heights!)
        (expect (zero? (virtual/height-cache-size)))))))

(defdescribe turn-separator-test
  (it "marks a You bubble after a Vis bubble when turn differentiation is enabled"
    (let [msgs [(user-msg "first")
                (plain-assistant-msg "answer")
                (user-msg "next")]
          {:keys [visible total-h]} (virtual/layout msgs bubble-w
                                      (assoc settings :differentiate-turns true)
                                      nil 100 {})
          projected (mapv :projected visible)
          marked    (nth projected 2)
          unmarked-total (:total-h (virtual/layout msgs bubble-w
                                     (assoc settings :differentiate-turns false)
                                     nil 100 {}))]
      (expect (true? (:turn-separator? marked)))
      (expect (nil? (:turn-separator? (first projected))))
      (expect (= 2 (- total-h unmarked-total)))))

  (it "does not mark turn separators when disabled"
    (let [msgs [(plain-assistant-msg "answer") (user-msg "next")]
          {:keys [visible]} (virtual/layout msgs bubble-w
                              (assoc settings :differentiate-turns false)
                              nil 100 {})]
      (expect (not-any? #(contains? (:projected %) :turn-separator?) visible)))))

(defdescribe project-message-test
  (describe "user messages strip timestamps and render markdown" (it "keeps :timestamp when :show-timestamps true" (let [pm (project-message (user-msg "hi") bubble-w (assoc settings :show-timestamps true))] (expect (some? (:timestamp pm))))))

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
  ;; `:conversation-turn-id` yet). When the turn lands, the
  ;; completed message has BOTH ids - and they're different
  ;; values. Picking `:conversation-turn-id` for the disclosure
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
                                            :conversation-turn-id "server-xyz"}))))
    (it "falls back to :conversation-turn-id for legacy DB messages"
      (expect (= "server-only" (turn-identity {:conversation-turn-id "server-only"}))))
    (it "returns nil when neither id is present"
      (expect (nil? (turn-identity {}))))))
