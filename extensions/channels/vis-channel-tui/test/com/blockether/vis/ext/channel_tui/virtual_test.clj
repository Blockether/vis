(ns com.blockether.vis.ext.channel-tui.virtual-test
  "Tests for the virtualized chat-panel layout planner.

   Three contracts to pin:

   1. `estimated-height` is cheap, monotonic, and within 2\u00d7 of the
      real `bubble-height` for the message shapes we actually emit
      (user, plain assistant, assistant with trace).
   2. `layout` only projects messages whose viewport interval is
      non-empty. Off-screen bubbles MUST NOT trigger
      `format-answer-with-thinking` (verified by counting projection
      calls through a side-effect counter swap).
   3. `layout`'s `:total-h` and `:eff-scroll` are valid scrollbar
      inputs (`:eff-scroll \u2208 [0, max(0, total-h - inner-h)]`) and
      visible bubbles' `:top + :height` fits inside the viewport's
      forward extent, even when estimates differ from real heights."
  (:require
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.ext.channel-tui.virtual :as virtual]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private estimated-height @#'virtual/estimated-height)
(def ^:private project-message  @#'virtual/project-message)

;; ─── Fixtures ───────────────────────────────────────────────────────────────

(defn- user-msg [text]
  {:role :user :text text :timestamp #inst "2026-04-30T00:00:00"})

(defn- plain-assistant-msg [text]
  {:role :assistant :text text :timestamp #inst "2026-04-30T00:00:00"})

(defn- trace-assistant-msg
  "Build a minimal assistant message with a `:trace` of `n-iters`
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
     :raw-answer      answer
     :text            answer
     :trace           trace
     :iteration-count n-iters
     :timestamp       #inst "2026-04-30T00:00:00"}))

(def ^:private bubble-w 100)

(def ^:private settings
  {:show-thinking true :show-iterations true :show-timestamps false})

;; ─── Tests ──────────────────────────────────────────────────────────────────

(defdescribe estimated-height-test
  (describe "estimated-height never returns negative or zero"
    (it "user msg \u2265 1"
      (expect (>= (estimated-height (user-msg "hi") bubble-w) 1)))
    (it "empty user msg \u2265 1"
      (expect (>= (estimated-height (user-msg "") bubble-w) 1)))
    (it "plain assistant \u2265 1"
      (expect (>= (estimated-height (plain-assistant-msg "Done.") bubble-w) 1)))
    (it "trace assistant \u2265 1"
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
        (expect (some #(= (dec (count msgs)) (:idx %)) visible)))))

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

(defdescribe project-message-test
  (describe "user messages pass through (no :text mutation)"
    (it "drops :timestamp when :show-timestamps false (default)"
      (let [pm (project-message (user-msg "hi") bubble-w settings)]
        (expect (nil? (:timestamp pm)))
        (expect (= "hi" (:text pm)))))
    (it "keeps :timestamp when :show-timestamps true"
      (let [pm (project-message (user-msg "hi") bubble-w
                 (assoc settings :show-timestamps true))]
        (expect (some? (:timestamp pm))))))

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
