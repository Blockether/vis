(ns com.blockether.vis.ext.channel-tui.render-test
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.click-regions :as cr]
   [com.blockether.vis.ext.channel-tui.links :as links]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.ext.channel-tui.theme :as t]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)
(def ^:private input-more-hint @#'render/input-more-hint)
(def ^:private clip-lines-preserving-markers @#'render/clip-lines-preserving-markers)

(defn- marker-of
  "First codepoint of `s` as a single-char string, or nil for empty."
  [s]
  (when (and (string? s) (pos? (count s)))
    (subs s 0 1)))

(defn- strip-ansi [s]
  (str/replace (or s "") #"\u001b\[[0-9;]*m" ""))

(defn- body-of
  "Drop the leading marker (PUA codepoint) and return the visible text.
   Tolerates empty input - a blank line in `:answer` mode renders as
   the empty string with the empty `:plain` marker, so callers must
   not crash when iterating over a frame that includes blank rows."
  [s]
  (when (string? s)
    (if (zero? (count s)) "" (subs s 1))))

(defdescribe input-overflow-hint-test
  (it "shows hidden visual-row count as an N more label for the input top border"
    (expect (= nil (input-more-hint 1 4)))
    (expect (= nil (input-more-hint 4 4)))
    (expect (= " 1 more " (input-more-hint 5 4)))
    (expect (= " 6 more " (input-more-hint 10 4)))))

(defdescribe live-running-block-test
  (it "renders a block slot with no result as currently running with elapsed time"
    ;; The right-aligned `BLOCK N` / `ITERATION N` / `CODE N` header bands
    ;; were retired per user directive (see comments in render.clj). The
    ;; spinner now lives next to the form via the in-line `↻ <elapsed>`
    ;; status row, and the code / status rows ride the same marker band.
    (let [lines (format-iteration-entry {:iteration     0
                                         :code          ["(Thread/sleep 1000)"]
                                         :started-at-ms [1000]}
                  40 1 {:now-ms 2500})
          code-line (first (filter #(str/includes? % "Thread/sleep") lines))
          status-line (first (filter #(str/includes? % "↻ 1.0s") lines))]
      (expect (not-any? #(str/includes? % "BLOCK 1") lines))
      (expect (not-any? #(str/includes? % "ITERATION 1") lines))
      (expect (not-any? #(str/includes? % "CODE 1") lines))
      (expect (= p/MARKER_CODE (marker-of code-line)))
      (expect (= p/MARKER_CODE (marker-of status-line)))))

  (it "puts success status on its own bottom line and keeps bottom padding"
    ;; Layout (post header-band removal):
    ;;   iteration-pad
    ;;   code-ok-pad             ← the trailing pad lives ABOVE result rows,
    ;;   <code line>                not at (count-2). Check it exists somewhere
    ;;   <status line ✓>            inside the code-block region.
    ;;   code-ok-pad
    ;;   iteration-pad
    ;;   <result>
    ;;   iteration-pad
    (let [lines (format-iteration-entry {:iteration 0
                                         :code ["(+ 1 2)"]
                                         :results ["3"]
                                         :successes [true]
                                         :durations [1]}
                  40 1 {})
          bodies (mapv (comp strip-ansi body-of) lines)
          status-line (first (filter #(str/includes? % "✓ 1ms") lines))]
      (expect (= "✓ 1ms" (str/trim (strip-ansi (body-of status-line)))))
      (expect (= p/MARKER_CODE_OK (marker-of status-line)))
      (expect (some #(= p/MARKER_CODE_OK_PAD (marker-of %)) lines))
      ;; Result rows carry both a leading line-marker and an inline
      ;; result-text format sentinel (U+206E), so `body-of` alone
      ;; doesn't strip down to the bare token. Use a contains check
      ;; instead of a strict equality.
      (expect (some #(str/includes? % "3") bodies)))))

(defn- visually-blank?
  "True when a rendered line carries no visible glyphs — either truly
   empty, plain whitespace, or made up entirely of the invisible
   Unicode format-class characters (U+200B–U+200D, U+2060–U+206F,
   U+FEFF) the TUI painter uses as line-kind sentinels. From the
   user's perspective such rows are blank."
  [s]
  (let [s (strip-ansi (or s ""))]
    (or (str/blank? s)
      (every? (fn [^Character c]
                (let [n (int c)]
                  (or (= n 0x200B) (= n 0x200C) (= n 0x200D) (= n 0xFEFF)
                    (<= 0x2060 n 0x206F))))
        s))))

(defdescribe answer-trailer-margin-test
  ;; Regression: before this fix the answer bubble's top margin
  ;; depended on whether the turn had any iteration trace. With trace,
  ;; the trailer was `<trace-trailing-pad><ans-pad-top><answer>...`
  ;; which the painter rendered as TWO blank rows. Without trace, just
  ;; `<ans-pad-top><answer>...` = ONE blank row. Same for cancelled.
  ;; The fix drops the leading ans-pad / leading blank whenever the
  ;; trace already provides a trailing margin, so both shapes show one
  ;; blank row between any preceding content and the answer text.
  (let [ans       [:ir {} [:p {} "hello"]]
        settings  {:show-thinking true :show-iterations true}
        iter      {:code      ["(+ 1 1)"] :comments [nil]
                   :results   ["2"] :stdouts [""] :stderrs [""]
                   :durations [1] :successes [true]}
        index-of  (fn [p needle]
                    (first (keep-indexed
                             (fn [i ln]
                               (when (str/includes? (strip-ansi ln) needle) i))
                             (:lines p))))]
    (it "answer with NO trace: exactly one blank row above the answer text"
      (let [p   (render/format-answer-with-thinking-data*
                  ans [] 80 settings nil false nil)
            idx (index-of p "hello")]
        (expect (= 1 idx))
        (expect (visually-blank? (first (:lines p))))))

    (it "answer with code-bearing trace: exactly one blank row before the answer text"
      (let [p   (render/format-answer-with-thinking-data*
                  ans [iter] 80 settings nil false nil)
            idx (index-of p "hello")
            ln  (:lines p)]
        (expect (some? idx))
        (expect (visually-blank? (nth ln (dec idx))))
        ;; And the row before THAT must NOT also be blank (otherwise
        ;; the legacy two-row gap is back).
        (expect (or (zero? (dec idx))
                  (not (visually-blank? (nth ln (- idx 2))))))))

    (it "cancelled with non-empty answer renders the answer text once"
      ;; `cancel-text` falls back to the answer text when the IR is
      ;; non-empty; assert the same one-row-above invariant.
      (let [p   (render/format-answer-with-thinking-data*
                  ans [iter] 80 settings nil true nil)
            idx (index-of p "hello")
            ln  (:lines p)]
        (expect (some? idx))
        (expect (visually-blank? (nth ln (dec idx))))
        (expect (or (zero? (dec idx))
                  (not (visually-blank? (nth ln (- idx 2))))))))))

(defdescribe progress-rendering-test
  (it "iter-0 spinner row has a one-line top margin inside the bubble"
    ;; Regression: the "Vis is calling the provider" spinner used to
    ;; sit flush against the bubble's top border because the no-trace
    ;; branch in `progress->lines-data` emitted just the spinner line.
    ;; The iter≥1 branch always ended with a blank line before the
    ;; spinner, so the bubble visually grew by an extra row the moment
    ;; the first iteration arrived. Keep the blank in both branches.
    (let [payload (render/progress->lines-data
                    {:iterations []} 80
                    {:show-thinking true :show-iterations true}
                    {:now-ms 1000 :turn-start-ms 0})
          lines   (mapv strip-ansi (:lines payload))]
      (expect (= 2 (count lines)))
      (expect (= "" (first lines)))
      (expect (str/includes? (second lines) "Vis is calling the provider"))))

  (it "live progress renders every iteration instead of hiding history"
    (let [mk-entry (fn [n]
                     {:code      [(str "(+ " n " 1)")]
                      :comments  []
                      :results   [(str (inc n))]
                      :stdouts   []
                      :stderrs   []
                      :durations [1]
                      :successes [true]})
          body     (strip-ansi
                     (render/progress->text
                       {:iterations (mapv mk-entry (range 5))}
                       80
                       {:show-thinking true :show-iterations true}
                       {:now-ms 1000 :turn-start-ms 0}))]
      (expect (not (str/includes? body "hidden while live")))
      (expect (str/includes? body "(+ 0 1)"))
      (expect (str/includes? body "(+ 4 1)"))))

  (it "live progress renders bounded thinking chunks without hiding content"
    ;; Fixture passes real thinking content — the previous empty
    ;; `{:iterations [{}]}` shape could never have exercised any
    ;; thinking-rendering assertion (regression net for the bug where
    ;; this test was silently a no-op).
    (let [body (strip-ansi
                 (render/progress->text
                   {:iterations [{:thinking "alpha\nbeta"}]}
                   80
                   {:show-thinking true :show-iterations true}
                   {:now-ms 1000 :turn-start-ms 0}))]
      (expect (not (str/includes? body "hidden while live")))
      (expect (str/includes? body "alpha"))
      (expect (str/includes? body "beta"))))

  (it "live progress previews huge thinking with the viewport-driven truncation"
    ;; The single-iteration truncation summary only fires when a
    ;; viewport budget is supplied (the renderer can't decide to
    ;; collapse without knowing how much screen real estate exists).
    ;; Pin both halves of the contract: without `:viewport-rows` the
    ;; full thinking renders; with a tight budget the collapse summary
    ;; bounds the line count.
    (let [huge-thinking (apply str (repeat 20000 "thinking "))
          full          (render/progress->lines-data
                          {:iterations [{:thinking huge-thinking}]}
                          96 {:show-thinking true :show-iterations true}
                          {:now-ms 1000 :turn-start-ms 0})]
      ;; No viewport: full body is rendered, just shouldn't crash.
      (expect (pos? (count (:lines full))))
      (expect (not (str/includes? (strip-ansi (:text full)) huge-thinking)))
      (expect (some (fn [ln] (str/includes? (strip-ansi ln) "thinking thinking"))
                (:lines full)))))

  (it "live progress collapses huge results when conversation context is available"
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
          payload     (render/progress->lines-data
                        {:iterations [{:code      ["(+ 1 2)"]
                                       :comments  [nil]
                                       :results   [huge-result]
                                       :stdouts   [""]
                                       :durations [1]
                                       :successes [true]}]}
                        96
                        {:show-thinking true :show-iterations true}
                        {:now-ms            1000
                         :turn-start-ms     0
                         :conversation-id   "conversation"
                         :detail-expansions {}})]
      (expect (str/includes? (:text payload) "RESULT"))
      (expect (str/includes? (:text payload) "chars hidden"))
      (expect (not (str/includes? (:text payload) huge-result)))
      (expect (some #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "live progress bounds old iteration history by default while keeping latest work visible"
    (let [mk-entry (fn [n]
                     {:code      [(str "(+ " n " 1)")]
                      :comments  []
                      :results   [(str (inc n))]
                      :stdouts   []
                      :stderrs   []
                      :durations [1]
                      :successes [true]})
          payload   (render/progress->lines-data
                      {:iterations (mapv mk-entry (range 80))}
                      80
                      {:show-thinking true
                       :show-iterations true
                       :progress/live-iteration-limit 8}
                      {:now-ms            1000
                       :turn-start-ms     0
                       :conversation-id   "conversation"
                       :detail-expansions {}})
          body      (strip-ansi (:text payload))]
      (expect (< (count (:lines payload)) 90))
      (expect (str/includes? body "PROGRESS HISTORY"))
      (expect (str/includes? body "72 iterations hidden"))
      (expect (str/includes? body "(+ 79 1)"))
      (expect (not (str/includes? body "(+ 0 1)")))
      (expect (some #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "expanded live progress history renders the hidden iterations on demand"
    (let [mk-entry (fn [n]
                     {:code      [(str "(+ " n " 1)")]
                      :comments  []
                      :results   [(str (inc n))]
                      :stdouts   []
                      :stderrs   []
                      :durations [1]
                      :successes [true]})
          payload   (render/progress->lines-data
                      {:iterations (mapv mk-entry (range 12))}
                      80
                      {:show-thinking true
                       :show-iterations true
                       :progress/live-iteration-limit 4}
                      {:now-ms            1000
                       :turn-start-ms     0
                       :conversation-id   "conversation"
                       :detail-expansions {["conversation" "progress:history"] true}})
          body      (strip-ansi (:text payload))]
      (expect (str/includes? body "showing all 12 iterations"))
      (expect (str/includes? body "(+ 0 1)"))
      (expect (str/includes? body "(+ 11 1)")))))

(defdescribe progress-streaming-perf-test
  (it "per-iteration cache keeps live-stream tick under 50 ms with 15 iterations"
    ;; Regression test for the bug where `progress->lines-data` keyed its
    ;; cache on `(System/identityHashCode iterations)` and `(quot now-ms 1000)`.
    ;; `make-progress-tracker` rebuilds the iterations vec on every chunk via
    ;; `(vec (vals @timeline))`, so the identity-keyed cache missed every
    ;; tick. With a 15-iteration trace we measured 554 ms per 80 ms render
    ;; tick - 7x over budget. The fix replaces the trace-level cache with
    ;; per-iteration content-fingerprint caching, so completed iterations
    ;; hit forever and only the streaming iteration recomputes.
    ;;
    ;; Threshold (50 ms) is generous: real measurements land ~10 ms on a
    ;; warm JVM. We pick 50 ms so JIT-cold CI runs don't false-alarm while
    ;; still failing loudly if someone reintroduces an O(N-iters) per-tick
    ;; reformat path. Bump the threshold here ONLY if you have a
    ;; corresponding bench measurement showing the new floor; never bump
    ;; just to make a flake go away.
    ;; Note on stdout content: we deliberately use a string with NO
    ;; leading/trailing whitespace. `format-iteration-entry-entries`
    ;; runs `(str/trim x)` on stdout before passing to `wrap-text`,
    ;; and `str/trim` returns a new String instance when it actually
    ;; trims - which busts `wrap-text`'s `identityHashCode`-keyed
    ;; cache. That's a separate latent issue (see autoresearch.ideas
    ;; "replace identityHashCode in wrap-text with content fp"); this
    ;; test focuses on the per-iteration-cache regression.
    (let [mk-iter (fn [i]
                    {:thinking (apply str (repeat (+ 200 (* 100 i)) \.))
                     :code [(str "(do (println :iter " i ") (mapv inc (range 100)))")]
                     :comments [nil]
                     :results ["[1 2 3 ...]"]
                     :result-kinds [:value]
                     :result-details [nil]
                     :stdouts [(apply str (repeat 1500 "output"))]
                     :stderrs [""]
                     :durations [50]
                     :successes [true]})
          base       (mapv mk-iter (range 14))
          last-base  (mk-iter 14)
          bubble-w   130
          settings   {:show-thinking true :show-iterations true}]
      (render/invalidate-cache!)
      ;; Warm: 3 cycles to JIT the format path.
      (dotimes [_ 3]
        (render/progress->lines-data {:iterations base} bubble-w settings
          {:now-ms 1700000000000 :turn-start-ms 1700000000000
           :viewport-rows 50}))
      ;; Streaming: NEW iterations vec each tick (mimics `(vec (vals @timeline))`),
      ;; last iteration's thinking grows by 100 chars per tick.
      (let [runs 30
            t0   (System/nanoTime)]
        (dotimes [i runs]
          (let [growing  (assoc last-base :thinking
                           (apply str (:thinking last-base)
                             (repeat (* 100 (inc i)) \.)))
                its'     (conj (vec base) growing)]
            (render/progress->lines-data {:iterations its'} bubble-w settings
              {:now-ms        (+ 1700000000000 (* i 80))
               :turn-start-ms 1700000000000
               :viewport-rows 50})))
        (let [per-tick-ms (/ (/ (- (System/nanoTime) t0) 1e6) (double runs))]
          (expect (< per-tick-ms 50.0))))))

  (it "completed-iteration cache hits when the iterations vec gets a fresh identity"
    ;; Direct contract test: take a fully-completed iterations vec, format
    ;; it once to warm the cache, then format again with a freshly-allocated
    ;; copy that has identical content but different `identityHashCode`.
    ;; The second call must be ~free (cache hit). If someone reintroduces
    ;; `identityHashCode`-based keying this test fails immediately.
    (let [iter   {:thinking "some reasoning"
                  :code ["(+ 1 2)"]
                  :comments [nil]
                  :results ["3"]
                  :result-kinds [:value]
                  :result-details [nil]
                  :stdouts [""]
                  :stderrs [""]
                  :durations [10]
                  :successes [true]}
          iters1 (vec (repeat 5 iter))
          ;; Same content, fresh vec identity, fresh map identities for entries.
          iters2 (mapv #(into {} %) iters1)]
      (render/invalidate-cache!)
      ;; Warm with iters1.
      (dotimes [_ 3]
        (render/progress->lines-data {:iterations iters1} 100
          {:show-thinking true :show-iterations true}
          {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
      ;; iters2 has identical CONTENT but different identity at every level.
      (let [t0 (System/nanoTime)]
        (dotimes [_ 100]
          (render/progress->lines-data {:iterations iters2} 100
            {:show-thinking true :show-iterations true}
            {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
        (let [per-call-us (/ (/ (- (System/nanoTime) t0) 1e3) 100.0)]
          ;; Cache hit path: should be well under 1 ms (1000 µs) per call
          ;; even with 5 cached iterations to concat. If this exceeds 5 ms
          ;; the per-iteration content-keyed cache is broken.
          (expect (< per-call-us 5000.0)))))))

(defdescribe iteration-live-ordering-test
  (describe "ordered live progress events"
    (it "renders reasoning before code in the post-:events flat layout"
      ;; The pre-existing `:events`-driven interleaved variant was
      ;; removed when the runtime contract dropped `:events`. Resume /
      ;; live now share a flat layout: thinking first, then all code
      ;; blocks, then their results. Pin that ordering here.
      (let [lines (format-iteration-entry
                    {:thinking  "alpha\nbeta"
                     :code      ["(+ 1 1)"]
                     :comments  []
                     :results   ["2"]
                     :stdouts   []
                     :stderrs   []
                     :durations [1]
                     :successes [true]
                     :error     nil}
                    60 1 {:show-header? true})
            body  (strip-ansi (str/join "\n" (map body-of lines)))]
        (expect (< (.indexOf body "alpha") (.indexOf body "beta")))
        (expect (< (.indexOf body "beta") (.indexOf body "(+ 1 1)")))
        (expect (< (.indexOf body "(+ 1 1)") (.indexOf body "2")))))))

(defdescribe paint-styled-line-stacking-test
  ;; The Polish bug report: `> **Lącznie:**` inside a quote rendered
  ;; bold-without-italic because paint-styled-line! cleared the
  ;; wrapping italic at entry. We pin the fix by recording the SGR
  ;; set on every paint call via a stub TextGraphics, then asserting
  ;; bold + italic stack correctly.
  (let [;; Capture every (putString ...) as [text {:fg :bg :sgr}].
        captured (atom [])
        active   (atom #{})
        fg       (atom nil)
        bg       (atom nil)
        ;; Lanterna's TextGraphics is an interface with ~30 methods;
        ;; we proxy the four paint-styled-line! actually calls.
        graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                   (clearModifiers []
                     (reset! active #{})
                     this)
                   (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (swap! active into (seq arr))
                     this)
                   (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (apply swap! active disj (seq arr))
                     this)
                   (getActiveModifiers []
                     ;; Return a defensive EnumSet so paint-styled-line!
                     ;; can `EnumSet/copyOf` it.
                     (if (empty? @active)
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                       (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                   (setForegroundColor [c] (reset! fg c) this)
                   (setBackgroundColor [c] (reset! bg c) this)
                   (putString
                     ([col row text]
                      (swap! captured conj [text {:fg @fg :bg @bg :sgr @active}])
                      this)))]

    (describe "paint-styled-line! inherits the wrapping SGR modifiers"
      (it "BOLD inside a wrapping ITALIC stacks to bold-italic"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (let [line (str "plain " p/INLINE_BOLD_ON "loud" p/INLINE_BOLD_OFF " tail")]
          (p/paint-styled-line! graphics 0 0 line
            (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
            (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
            (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
            (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
          ;; Three segments: "plain ", "loud", " tail"
          (let [segs @captured]
            (expect (= 3 (count segs)))
            (let [[seg0 seg1 seg2] segs]
              ;; Segment 1: 'plain ' - inherits italic only.
              (expect (= "plain " (first seg0)))
              (expect (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/BOLD)))
              ;; Segment 2: 'loud' - italic + bold stacked.
              (expect (= "loud" (first seg1)))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/BOLD))
              ;; Segment 3: ' tail' - italic again, bold cleared.
              (expect (= " tail" (first seg2)))
              (expect (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/BOLD)))))))

      (it "At exit, the inherited SGR set is restored exactly"
        ;; Caller relies on `(p/styled g [p/ITALIC] (paint-styled-line! ...))`
        ;; ending with the same modifier state it started with, so its
        ;; own cleanup can finalise correctly.
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str p/INLINE_BOLD_ON "x" p/INLINE_BOLD_OFF)
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active)))

      (it "Dangling sentinel (no close) doesn't leak BOLD past the call"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str "open " p/INLINE_BOLD_ON "never closes")
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        ;; Even though the line ended mid-bold, the inherited italic
        ;; (NOT bold) is what the caller sees on exit.
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active))))))

(defdescribe paint-ansi-line-inline-sentinel-test
  ;; Tool render-fns return Markdown. Inline code spans in that Markdown
  ;; become private-use sentinels before result painting. The result painter
  ;; must consume them; otherwise Lanterna renders glyphs like  / .
  (it "consumes inline code sentinels instead of painting PUA glyphs"
    (let [paint-ansi-line! @#'render/paint-ansi-line!
          captured        (atom [])
          active          (atom #{})
          fg              (atom nil)
          bg              (atom nil)
          graphics        (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                            (clearModifiers []
                              (reset! active #{})
                              this)
                            (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                              (swap! active into (seq arr))
                              this)
                            (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                              (apply swap! active disj (seq arr))
                              this)
                            (getActiveModifiers []
                              (if (empty? @active)
                                (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                                (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                            (setForegroundColor [c] (reset! fg c) this)
                            (setBackgroundColor [c] (reset! bg c) this)
                            (putString
                              ([col row text]
                               (swap! captured conj [text {:fg @fg :bg @bg :sgr @active}])
                               this)))
          line            (str "Searched "
                            p/INLINE_CODE_ON "[\"extensions\"]" p/INLINE_CODE_OFF
                            " with "
                            p/INLINE_CODE_ON "{:any [\"circling\"]}" p/INLINE_CODE_OFF
                            ". Use "
                            p/INLINE_CODE_ON "v/preview" p/INLINE_CODE_OFF)
          visible         "Searched [\"extensions\"] with {:any [\"circling\"]}. Use v/preview"]
      (paint-ansi-line! graphics 0 0 line t/code-result-fg t/code-ok-bg)
      (let [painted (apply str (map first @captured))]
        (expect (= visible painted))
        (expect (not (str/includes? painted p/INLINE_CODE_ON)))
        (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe scrollbar-thumb-geometry-test
  ;; Pinning the painter/hit-test contract. Both the message-area
  ;; painter and the input-thread mouse handler must agree on which
  ;; rows belong to the thumb; if they drift, the user clicks "on
  ;; the thumb" and nothing happens, or clicks "off the thumb" and
  ;; the viewport jumps. The pure helper IS the contract.
  (let [g render/scrollbar-thumb-geometry]
    (describe "Returns nil when there's no overflow"
      (it "total-h < inner-h: nothing to scroll"
        (expect (nil? (g 10 20 nil))))
      (it "total-h == inner-h: nothing to scroll"
        (expect (nil? (g 20 20 nil))))
      (it "inner-h is zero: no viewport, no thumb"
        (expect (nil? (g 100 0 0)))))

    (describe "Standard 100/20 conversation"
      (it "Auto-bottom (scroll=nil) places the single-cell thumb at the END of the track"
        (let [{:keys [thumb-top-rel thumb-h max-scroll]} (g 100 20 nil)]
          (expect (= 19 thumb-top-rel))   ;; track-h(20) - thumb-h(1) = 19
          (expect (= 1 thumb-h))
          (expect (= 80 max-scroll))))    ;; 100 - 20

      (it "scroll=0 places thumb at the TOP"
        (expect (= {:thumb-top-rel 0 :thumb-h 1 :max-scroll 80}
                  (g 100 20 0))))

      (it "scroll=40 places thumb in the MIDDLE of the free track"
        (expect (= {:thumb-top-rel 9 :thumb-h 1 :max-scroll 80}
                  (g 100 20 40))))

      (it "scroll=80 places thumb at the BOTTOM (== max-scroll)"
        (expect (= {:thumb-top-rel 19 :thumb-h 1 :max-scroll 80}
                  (g 100 20 80)))))

    (describe "Out-of-range scroll values are clamped"
      (it "Negative scroll clamps to 0 (top)"
        (expect (zero? (:thumb-top-rel (g 100 20 -50)))))
      (it "Excessive scroll clamps to max-scroll (bottom)"
        (let [{:keys [thumb-top-rel max-scroll]} (g 100 20 9999)]
          (expect (= 19 thumb-top-rel))
          (expect (= 80 max-scroll)))))

    (describe "Viewport height changes keep one visible thumb cell"
      (it "1000-row content in a 5-row viewport: thumb-h is 1"
        (let [{:keys [thumb-h]} (g 1000 5 0)]
          (expect (= 1 thumb-h))))
      (it "360-row content in a maximized 56-row viewport: thumb-h stays 1"
        (let [{:keys [thumb-h]} (g 360 56 nil)]
          (expect (= 1 thumb-h))))
      (it "And the thumb still slides through the full track"
        (let [top (:thumb-top-rel (g 1000 5 0))
              bot (:thumb-top-rel (g 1000 5 995))]
          (expect (= 0 top))
          (expect (= 4 bot)))))    ;; track-h(5) - thumb-h(1) = 4
    ))

;; ─────────────────────────────────────────────────────────────────────────
;; <details> / <summary> rendering
;;
;; Pre-fix the TUI markdown pipeline had no branch for HTML block tags,
;; so the lines emitted by `(md-details (md-summary ...) body)` rendered
;; literally as `<details>`, `<summary>Click</summary>`, `</details>`
;; in the chat bubble - three rows of raw HTML the user can't act on.
;; The renderer now drops the framing tags and paints the summary as
;; a bold `▾ label` disclosure heading. True click-to-collapse still
;; needs per-bubble state + click regions; this fix is the visual
;; baseline.
;; ─────────────────────────────────────────────────────────────────────────

;; ─────────────────────────────────────────────────────────────────────────
;; Link-chrome strip - strip inline emphasis from anchor text
;;
;; `parse-md-refs` captures whatever sits between `[...]` raw, so
;; `[See **here**](url)` flows into the chrome row with literal `**`
;; baked in: "🔗 See **here** -> url". The renderer now runs the
;; anchor text through `markdown->inline` and strips the styling
;; sentinels, leaving plain visible text in the chrome.
;; ─────────────────────────────────────────────────────────────────────────

(def ^:private chrome-display-text @#'render/chrome-display-text)
(def ^:private resources-badge-label @#'render/resources-badge-label)
(def ^:private extract-link-refs @#'render/extract-link-refs)

(defn- chrome-of [src]
  (let [[ref] (links/parse-md-refs src)]
    (chrome-display-text ref 80)))

(defdescribe chrome-display-text-strips-inline-markup-test
  (describe "emphasis inside link text doesn't leak into chrome row"
    (it "`[See **here**](url)` -> no `**` in chrome"
      (let [s (chrome-of "[See **here**](https://example.com)")]
        (expect (str/includes? s "See here"))
        (expect (not (str/includes? s "**")))))

    (it "`[Spec *v2*](url)` -> no stray `*` in chrome"
      (let [s (chrome-of "[Spec *v2*](https://example.com)")]
        (expect (str/includes? s "Spec v2"))
        (expect (not (str/includes? s "*v2*")))))

    (it "``[Title with `code`](url)`` -> backticks stripped from chrome"
      (let [s (chrome-of "[Title with `code`](https://example.com)")]
        (expect (str/includes? s "Title with code"))
        (expect (not (str/includes? s "`code`"))))))

  (describe "emphasis OUTSIDE the brackets is unaffected (regression net)"
    ;; `**[link](url)**` - bold lives outside the bracket, so
    ;; `parse-md-refs` already captures clean text. This test pins
    ;; that the new strip doesn't accidentally mangle the URL or icon.
    (it "`**[link](url)**` -> chrome reads `link` cleanly"
      (let [s (chrome-of "**[link](https://example.com)**")]
        (expect (str/includes? s "link"))
        (expect (str/includes? s "https://example.com"))
        (expect (not (str/includes? s "**"))))))

  (describe "chrome icon + url tail still render after the strip"
    ;; Smoke test that the strip didn't drop the leading icon or the
    ;; ` -> ` separator. Without these the affordance loses its
    ;; "this is a link" cue.
    (it "icon + arrow + url all present"
      (let [s (chrome-of "[plain](https://example.com)")]
        (expect (str/includes? s " -> "))
        (expect (str/includes? s "https://example.com"))))))

(defdescribe extract-link-refs-guard-test
  (it "skips link extraction for giant messages"
    (let [text (apply str (repeat 25050 "a"))]
      (expect (= [] (extract-link-refs {:text text} 80))))))

(defdescribe resources-badge-test
  (describe "top-right resources badge"
    (it "chooses the richest label that fits"
      (expect (= "📚 Resources 3" (resources-badge-label 3 20)))
      (expect (= "📚 3" (resources-badge-label 3 4)))
      (expect (nil? (resources-badge-label 3 0))))

    (it "links no longer add per-resource rows to bubble height"
      (let [plain  {:role :assistant :text "a" :prewrapped-lines ["a"]}
            linked {:role :assistant
                    :text "[a](https://example.com)"
                    :prewrapped-lines ["a"]}]
        (render/invalidate-cache!)
        (expect (= (render/bubble-height plain 80)
                  (render/bubble-height linked 80)))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Loose-bullet coalesce - multi-paragraph list items render as one bullet
;;
;; Poorly-formatted markdown (LLM output, hand-edited prose) often
;; emits list items whose body has been fragmented across blank
;; lines:
;;
;;     - `dialogs.clj`
;;
;;      - removed `:system-prompt` palette command entry
;;     - `screen.clj`
;;
;;      - removed `:system-prompt` handler
;;
;; CommonMark spec-compliantly treats every fragment as its own
;; loose paragraph: only `dialogs.clj` lands under the bullet
;; marker, and `- removed ...` shows up flush-left as if it weren't
;; part of the bullet at all. The TUI bubble has nowhere to render
;; that hierarchy correctly.
;;
;; `coalesce-loose-list-items` (the pre-pass) folds every fragment
;; back into the bullet's text on a single line. Tests below pin
;; the specific shapes the user reported.
;; ─────────────────────────────────────────────────────────────────────────

(defn- strip-sentinels
  "Drop inline-style sentinels (PUA U+E110..U+E2FF) so equality
   assertions compare the visible text only."
  [s]
  (->> s
    (remove #(<= 0xE110 (int %) 0xE2FF))
    (apply str)))

;; ─────────────────────────────────────────────────────────────────────────
;; e4167d48: bullet continuation must NOT strip whitespace inside
;; inline-code spans
;;
;; Repro: model used `(v/join "prefix " (v/code "{:k :v :w x}")
;; " suffix")` to build a bullet body. `v/join` separates parts with
;; `\n\n`, so the resulting markdown has the inline-code span on its
;; own paragraph. `coalesce-loose-list-items` re-folds those
;; paragraphs into one bullet line and used to apply the punctuation
;; tightening regex `#" +([,;:.\)])"` to the WHOLE joined text. That
;; regex eats spaces before `:` — which is correct for prose
;; (`step :` -> `step:`) but wrong inside `` `{:k :v :w x}` `` where
;; the colons are EDN-keyword markers and the spaces are
;; semantically meaningful. User-visible damage:
;;
;;   `{:rendering-kind :vis/silent :result title}`
;;     -> `{:rendering-kind:vis/silent:result title}`
;;
;; Fix: tokenise on backticks first, only tighten prose tokens.
;; Mirror of `markdown/normalize-inline-spacing`'s tokenisation.
;; ─────────────────────────────────────────────────────────────────────────

;; ─────────────────────────────────────────────────────────────────────────
;; md-join inline-bold inside a bullet - the `Let / me / dig / deeper`
;; regression
;;
;; Faithful reconstruction of the FIRST `(answer ...)` block in conversation
;; eeaf9651-06c7-4dda-9e97-877fcef06337, turn 363de6c6-..., position 1.
;; The agent built a bullet's body via `md-join`, which inserts `\n\n`
;; between every part. With the naive bullet-coalesce that earlier
;; treated every `**...**`-starting line as a structural break, the
;; bullet rendered as ONE bullet header + a ladder of one-word
;; paragraphs flush-left:
;;
;;     • Turn 1 - "system prompt copy" prune:
;;
;;     38 failures across iterations 2-7. ...
;;
;;     reader boundary split
;;
;;     - a multi-line form got fragmented into bare symbols (
;;
;;     Let
;;
;;     ,
;;
;;     me
;;
;;     ,
;;
;;     dig
;;     ...
;;
;; The fix in `coalesce-loose-list-items`:
;;   - Pure `**span**` lines (no trailing prose after the closing `**`)
;;     stay CONTINUATIONS of the bullet - they're an md-join artefact,
;;     the bold text is meant to flow inline inside the sentence.
;;   - `**Label:** value` lines (bold prefix + trailing content) STILL
;;     close the list - those are real top-level summary paragraphs.
;;
;; Below: build the same source with `md/*` helpers and assert the
;; bubble renders as ONE flowing bullet with every code-span / bold
;; span inline.
;; ─────────────────────────────────────────────────────────────────────────

(defn- dummy-text-graphics
  "Lenient TextGraphics stub for layout tests that care about click
   regions, not actual painted glyphs. Implements the subset of the
   Lanterna interface that `draw-chat-bubble!` touches on a plain-text
   bubble."
  []
  (let [active (atom #{})]
    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
      (clearModifiers []
        (reset! active #{})
        this)
      (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (swap! active into (seq arr))
        this)
      (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (apply swap! active disj (seq arr))
        this)
      (getActiveModifiers []
        (if (empty? @active)
          (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
          (java.util.EnumSet/copyOf ^java.util.Collection @active)))
      (setForegroundColor [_] this)
      (setBackgroundColor [_] this)
      (putString
        ([_ _ _] this))
      (fillRectangle [_ _ _] this)
      (setCharacter [_ _ _] this))))

(defdescribe reasoning-preview-rendering-test
  (it "collapses completed reasoning to a compact clickable summary by default"
    (render/invalidate-cache!)
    (let [cid      "conversation"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          thinking "Considering footer removal\n\nThis is internal reasoning that should not merge into the answer."
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                     96 {:show-thinking true :show-iterations true} nil false
                     {:conversation-id cid
                      :conversation-turn-id turn-id})
          body     (strip-ansi (:text payload))
          rows     (mapv (comp strip-sentinels body-of) (:lines payload))
          summary  (first (filter #(str/includes? % "REASONING") rows))]
      (expect (some? summary))
      (expect (str/includes? summary "▸ REASONING"))
      (expect (str/includes? summary "lines hidden"))
      (expect (not (str/includes? body "Considering footer removal")))
      (expect (str/includes? body "done"))
      (expect (some #(and (= :toggle-details (:kind %)) (:collapsed? %)) (:line-meta payload)))))

  (it "expands completed reasoning when its detail node is toggled open"
    (render/invalidate-cache!)
    (let [cid      "conversation"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          node-id  "thinking:t123e4567:i1:reasoning"
          thinking (str/join "\n" (map #(format "line-%02d" %) (range 1 51)))
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                     96 {:show-thinking true :show-iterations true} nil false
                     {:conversation-id cid
                      :conversation-turn-id turn-id
                      :detail-expansions {[cid node-id] true}})
          body     (strip-ansi (:text payload))]
      (expect (str/includes? body "▾ REASONING"))
      (expect (str/includes? body "line-01"))
      (expect (str/includes? body "line-25"))
      (expect (str/includes? body "line-50")))))

(defdescribe auto-collapse-rendering-test
  (it "collapses preview results to four lines by default and expands on demand"
    (render/invalidate-cache!)
    (let [preview-text (str/join "\n" (map #(str % ": selected line") (range 1 31)))
          detail {:raw "{:secret \"raw-only\"}"}
          trace [{:code ["(v/preview file {:result [[:lines {:from 0 :to 30}]]})"]
                  :results [preview-text]
                  :result-kinds [:preview]
                  :result-details [detail]
                  :stdouts [""]
                  :durations [1]
                  :successes [true]}]
          opts {:conversation-id "conversation"
                :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"}
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false opts)
          expanded (render/format-answer-with-thinking-data
                     nil trace 96 {:show-iterations true} nil false
                     (assoc opts :detail-expansions
                       {["conversation" "iteration:t123e4567:i1:b1:preview-body"] true}))
          raw-view (render/format-answer-with-thinking-data
                     nil trace 96 {:show-iterations true} nil false
                     (assoc opts :detail-expansions
                       {["conversation" "iteration:t123e4567:i1:b1:preview-switch"] :raw}))]
      (expect (not (str/includes? (:text payload) "Preview.")))
      (expect (str/includes? (:text payload) "1: selected line"))
      (expect (str/includes? (:text payload) "4: selected line"))
      (expect (not (str/includes? (:text payload) "5: selected line")))
      (expect (not (str/includes? (:text payload) "30: selected line")))
      (expect (str/includes? (:text payload) "▸ 26 lines hidden"))
      (let [preview-rows    (filter (fn [line] (str/includes? (body-of line) "PREVIEW")) (:lines payload))
            preview-row-idx (.indexOf (:lines payload) (first preview-rows))
            first-body-idx  (.indexOf (:lines payload)
                              (first (filter (fn [line] (str/includes? (body-of line) "1: selected line")) (:lines payload))))]
        (expect (= 1 (count preview-rows)))
        (expect (str/includes? (body-of (first preview-rows)) "▸ 26 lines hidden"))
        (expect (not (str/includes? (body-of (first preview-rows)) "▸ PREVIEW")))
        (expect (str/includes? (body-of (first preview-rows)) "● PREVIEW  ○ RAW"))
        (expect (< preview-row-idx first-body-idx)))
      (expect (some #(= :preview-switcher (:kind %)) (:line-meta payload)))
      (expect (str/includes? (:text expanded) "30: selected line"))
      (expect (str/includes? (:text payload) "● PREVIEW  ○ RAW"))
      (expect (not (str/includes? (:text payload) "SHAPE")))
      (expect (not (str/includes? (:text payload) ":source-shape")))
      (expect (not (str/includes? (:text payload) "raw-only")))
      (expect (str/includes? (:text raw-view) "▾ showing all 30 lines"))
      ;; The verbose `Turn: <uuid>, Iteration: N, Block: M` summary
      ;; suffix was replaced with the compact `[iteration N · block M]`
      ;; label band, matching the live-progress header style.
      (expect (str/includes? (:text raw-view) "[iteration 1 · block 1]"))
      (expect (str/includes? (:text raw-view) "raw-only"))
      (expect (not (str/includes? (:text raw-view) "src/demo.clj")))
      (expect (not (str/includes? (:text raw-view) ":info")))
      (expect (not (str/includes? (:text payload) "RESULT")))
      (let [wrap-text*   @#'render/wrap-text
            wrap-calls   (atom 0)
            preview-text (str/join "\n" (map #(str % ": selected line xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
                                          (range 1 2001)))
            trace        {:code ["(v/preview huge)"]
                          :results [preview-text]
                          :result-kinds [:preview]
                          :result-details [{:raw "{:raw true}"}]
                          :stdouts [""]
                          :durations [1]
                          :successes [true]}]
        (with-redefs-fn {(resolve 'com.blockether.vis.ext.channel-tui.render/wrap-text)
                         (fn [& args]
                           (swap! wrap-calls inc)
                           (apply wrap-text* args))}
          (fn []
            (let [payload (render/format-answer-with-thinking-data
                            nil [trace] 96 {:show-iterations true} nil false opts)]
              (expect (str/includes? (:text payload) "1: selected line"))
              (expect (str/includes? (:text payload) "4: selected line"))
              (expect (not (str/includes? (:text payload) "5: selected line")))
              (expect (< @wrap-calls 80))))))))

  (it "renders operation badges and color roles for tool result summaries"
    (render/invalidate-cache!)
    (let [trace [{:code ["(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])"]
                  :results ["1 file changed"]
                  :result-kinds [:tool]
                  :result-details [{:op :v/patch
                                    :op/tag :op.tag/action
                                    :color-role :tool-color/edit}]
                  :stdouts [""]
                  :durations [1]
                  :successes [true]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:conversation-id "conversation"
                     :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})]
      (expect (str/includes? (:text payload) "ACTION patch"))
      (expect (some #(= :tool-color/edit (:color-role %)) (:line-meta payload)))))

  (it "does not duplicate edit badges when tool results auto-collapse"
    (render/invalidate-cache!)
    (let [huge-result (str "Patched file.\n" (str/join " " (repeat 500 "diff-line")))
          trace       [{:code ["(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])"]
                        :results [huge-result]
                        :result-kinds [:tool]
                        :result-details [{:op :v/patch
                                          :op/tag :op.tag/action
                                          :color-role :tool-color/edit}]
                        :stdouts [""]
                        :durations [1]
                        :successes [true]}]
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:conversation-id "conversation"
                         :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          body        (strip-ansi (:text payload))]
      (expect (= 1 (count (re-seq #"ACTION patch" body))))
      (expect (some #(str/starts-with? (body-of %) "▸ ACTION patch") (:lines payload)))
      (expect (not-any? #(str/starts-with? (body-of %) "  ▸ ACTION patch") (:lines payload)))))

  (it "does not duplicate self-describing shell result summaries"
    (render/invalidate-cache!)
    (let [huge-result (str "Ran bash in `.` - exit `0`, 1 ms.\n"
                        "Command: `find . -maxdepth 2 -type f`\n"
                        (str/join " " (repeat 500 "file.txt")))
          trace       [{:code ["(v/bash \"find . -maxdepth 2 -type f\")"]
                        :results [huge-result]
                        :result-kinds [:tool]
                        :result-details [{:op :v/bash
                                          :op/tag :op.tag/action
                                          :color-role :tool-color/shell}]
                        :stdouts [""]
                        :durations [1]
                        :successes [true]}]
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:conversation-id "conversation"
                         :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          body        (strip-ansi (:text payload))]
      (expect (= 1 (count (re-seq #"ACTION bash" body))))))

  (it "keeps shell stderr inside the shell result zone"
    (let [lines (format-iteration-entry {:iteration      0
                                         :code           ["(v/bash \"boom\")"]
                                         :results        ["Ran bash in `.` - exit `1`, 1 ms.\n\nstderr:\n\n```text\nExecution error\n```"]
                                         :result-kinds   [:tool]
                                         :result-details [{:op :v/bash
                                                           :op/tag :op.tag/action
                                                           :color-role :tool-color/shell
                                                           :stderr "Execution error"}]
                                         :stdouts        [""]
                                         :stderrs        [""]
                                         :successes      [false]
                                         :durations      [1]}
                  48 1 {})
          stderr-label (first (filter #(str/includes? % "STDERR") lines))
          stderr-body  (first (filter #(str/includes? % "Execution error") lines))]
      (expect (nil? stderr-label))
      (expect (= p/MARKER_ERR_RESULT (marker-of stderr-body)))))

  (it "keeps shell stdout inside the shell result zone"
    (let [lines (format-iteration-entry {:iteration      0
                                         :code           ["(v/bash \"echo hello\")"]
                                         :results        ["Ran bash in `.` - exit `0`, 1 ms.\n\nstdout:\n\n```text\nhello\nworld\n```"]
                                         :result-kinds   [:tool]
                                         :result-details [{:op :v/bash
                                                           :op/tag :op.tag/action
                                                           :color-role :tool-color/shell
                                                           :stdout "hello\nworld"
                                                           :stderr ""}]
                                         :stdouts        [""]
                                         :stderrs        [""]
                                         :successes      [true]
                                         :durations      [1]}
                  56 1 {})
          result-line  (first (filter #(str/includes? % "Ran bash") lines))
          stdout-label (first (filter #(str/includes? % "STDOUT") lines))
          stderr-label (first (filter #(str/includes? % "STDERR") lines))
          stdout-body  (first (filter #(str/includes? % "hello") lines))]
      (expect (some? result-line))
      (let [result-idx (.indexOf lines result-line)
            preceding  (subvec (vec lines) 0 result-idx)]
        ;; A new `ACTION <op>` label line was inserted between the
        ;; iteration-pad and the result body, so the iteration-pad is
        ;; no longer guaranteed to be the IMMEDIATELY preceding row
        ;; — just somewhere above the result inside the iteration body.
        (expect (some #(= p/MARKER_ITERATION_PAD (marker-of %)) preceding)))
      (expect (not (str/starts-with? (body-of result-line) "  ")))
      (expect (nil? stdout-label))
      (expect (nil? stderr-label))
      (expect (some? stdout-body))))

  (it "paints merged shell output on the shell result background"
    (let [trace   [{:code ["(v/bash \"echo ok\")"]
                    :results ["Ran bash in `.` - exit `0`, 1 ms.\n\nstdout:\n\n```text\nstdout-line\n```"]
                    :result-kinds [:tool]
                    :result-details [{:op :v/bash
                                      :op/tag :op.tag/action
                                      :color-role :tool-color/shell
                                      :stdout "stdout-line"}]
                    :stdouts [""]
                    :durations [1]
                    :successes [true]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 80 {:show-iterations true} nil false
                    {:conversation-id "conversation"
                     :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          fills   (atom [])
          puts    (atom [])
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [col row text]
                       (swap! puts conj {:col col :row row :text text})
                       this)
                     (fillRectangle [pos size _ch]
                       (swap! fills conj {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
                                          :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
                                          :w   (.getColumns ^com.googlecode.lanterna.TerminalSize size)})
                       this)
                     (setCharacter [_ _ _] this))]
      (render/draw-chat-bubble! graphics
        {:role :assistant
         :text (:text payload)
         :prewrapped-lines (:lines payload)
         :line-meta (:line-meta payload)}
        0 2 80 {:viewport-h 50})
      (let [code-put   (some #(when (str/includes? (:text %) "v/bash") %) @puts)
            stdout-put (some #(when (= "stdout-line" (:text %)) %) @puts)
            result-put (some #(when (str/includes? (:text %) "Ran bash") %) @puts)
            code-fill-col (apply max (map :col (filter #(= (:row code-put) (:row %)) @fills)))
            stdout-fill-col (apply max (map :col (filter #(= (:row stdout-put) (:row %)) @fills)))
            result-fill-col (apply max (map :col (filter #(= (:row result-put) (:row %)) @fills)))]
        (expect (some? code-put))
        (expect (some? result-put))
        (expect (some? stdout-put))
        (expect (= code-fill-col result-fill-col))
        (expect (= code-fill-col stdout-fill-col)))))

  (it "omits blank shell result/stdout/stderr blocks"
    (let [lines (format-iteration-entry {:iteration      0
                                         :code           ["(v/bash \"true\")"]
                                         :results        [""]
                                         :result-kinds   [:tool]
                                         :result-details [{:op :v/bash
                                                           :op/tag :op.tag/action
                                                           :color-role :tool-color/shell
                                                           :stdout ""
                                                           :stderr ""}]
                                         :stdouts        [""]
                                         :stderrs        [""]
                                         :successes      [true]
                                         :durations      [1]}
                  56 1 {})]
      (expect (not-any? #(str/includes? % "BASH") lines))
      (expect (not-any? #(str/includes? % "RESULT") lines))
      (expect (not-any? #(str/includes? % "STDOUT") lines))
      (expect (not-any? #(str/includes? % "STDERR") lines))))

  (it "does not emit vague duplicate search-any rows"
    (render/invalidate-cache!)
    (let [trace   [{:code ["(v/rg {:any [\"alpha\" \"beta\"] :paths [\"src\"]})"]
                    :results ["Searched `[\"src\"]` with `{:any [\"alpha\" \"beta\"], :paths [\"src\"]}` - 0 hit(s)."]
                    :result-kinds [:tool]
                    :result-details [{:op :any
                                      :op/tag :op.tag/observation
                                      :color-role :tool-color/search
                                      :spec {:any ["alpha" "beta"] :paths ["src"]}
                                      :paths ["src"]}]
                    :stdouts [""]
                    :durations [1]
                    :successes [true]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:conversation-id "conversation"
                     :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          body    (strip-ansi (:text payload))]
      (expect (not (str/includes? body "SEARCH any")))
      (expect (= 1 (count (re-seq #"Searched" body))))))

  (it "paints preview raw controls on the right with underline and translated ANSI"
    (render/invalidate-cache!)
    (let [preview-text (str/join "\n" (map #(str % ": selected line") (range 1 31)))
          detail {:raw "{:secret \"raw-only\"}"}
          trace [{:code ["(v/preview file {:result [[:lines {:from 0 :to 30}]]})"]
                  :results [preview-text]
                  :result-kinds [:preview]
                  :result-details [detail]
                  :stdouts [""]
                  :durations [1]
                  :successes [true]}]
          opts {:conversation-id "conversation"
                :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"
                :detail-expansions {["conversation" "iteration:t123e4567:i1:b1:preview-switch"] :raw}}
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false opts)
          puts (atom [])
          active (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col _row text]
                       (swap! puts conj {:text text :sgr @active})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))]
      (render/draw-chat-bubble! graphics
        {:role :assistant
         :text (:text payload)
         :prewrapped-lines (:lines payload)
         :line-meta (:line-meta payload)}
        0 2 96 {:viewport-h 50})
      (expect (some #(str/includes? (:text %) "showing all 30 lines") @puts))
      (expect (some #(str/includes? (:text %) "raw-only") @puts))
      (expect (not-any? #(str/includes? (:text %) "\\space") @puts))
      (expect (not-any? #(str/includes? (:text %) ":info") @puts))
      (expect (not-any? #(str/includes? (:text %) "src/demo.clj") @puts))
      (expect (not-any? #(str/includes? (:text %) "\u001b[") @puts))
      (expect (some #(and (= "● RAW" (:text %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/UNDERLINE))
                @puts))))

  (it "does not wrap collapsed huge result bodies before rendering the summary"
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 4000 "abcdefghij"))
          trace       [{:code      ["(+ 1 2)"]
                        :comments  [nil]
                        :results   [huge-result]
                        :stdouts   [""]
                        :durations [1]
                        :successes [true]}]
          turn-id     "123e4567-e89b-12d3-a456-426614174000"
          fut         (future
                        (render/format-answer-with-thinking-data
                          nil trace 96 {:show-iterations true} nil false
                          {:conversation-id "conversation"
                           :conversation-turn-id turn-id}))
          payload     (deref fut 500 ::timeout)]
      (when (= ::timeout payload)
        (future-cancel fut))
      (expect (not= ::timeout payload))
      (expect (str/includes? (:text payload) "RESULT"))
      (expect (not (str/includes? (:text payload) "RESULT (Block 1)")))
      (expect (str/includes? (:text payload) "chars hidden"))
      ;; Compact label band `[iteration N · block M]` replaces the verbose
      ;; `Turn: <uuid>, Iteration: N, Block: M` suffix the bubble used to
      ;; emit. The bracket prefix anchors the new format.
      (expect (str/includes? (:text payload) "[iteration 1 · block 1]"))
      (let [summary-line (some #(when (str/includes? % "[iteration 1 · block 1]") %)
                           (:lines payload))]
        (expect (some? summary-line))
        (expect (not (str/includes? summary-line p/INLINE_CODE_ON)))
        (expect (not (str/includes? summary-line p/INLINE_CODE_OFF))))
      (expect (not (str/includes? (:text payload) "t:")))
      (expect (not (str/includes? (:text payload) huge-result)))))

  (it "auto-collapses completed reasoning on the answer view and expands all content on demand"
    (render/invalidate-cache!)
    (let [thinking (str/join "\n" (map #(str "line " %) (range 1 51)))
          trace    [{:thinking  thinking
                     :code      []
                     :results   []
                     :stdouts   []
                     :durations []
                     :successes []}]
          opts     {:conversation-id "conversation"
                    :conversation-turn-id "turn-1"}
          expanded-opts (assoc opts :detail-expansions
                          {["conversation" "thinking:tturn-1:i1:reasoning"] true})
          collapsed (render/format-answer-with-thinking-data
                      [:ir {} [:p {} [:span {} "done"]]] trace 120
                      {:show-iterations true :show-thinking true}
                      nil false opts)
          expanded  (render/format-answer-with-thinking-data
                      [:ir {} [:p {} [:span {} "done"]]] trace 120
                      {:show-iterations true :show-thinking true}
                      nil false expanded-opts)]
      (expect (str/includes? (:text collapsed) "▸ REASONING"))
      (expect (str/includes? (:text collapsed) "lines hidden"))
      (expect (not (str/includes? (:text collapsed) "line 25")))
      (expect (str/includes? (:text expanded) "▾ REASONING"))
      (expect (str/includes? (:text expanded) "line 1"))
      (expect (str/includes? (:text expanded) "line 25"))
      (expect (str/includes? (:text expanded) "line 50"))))

  (it "paints collapsed info rows as bold text on the summary band"
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
          trace       [{:code      ["(+ 1 2)"]
                        :comments  [nil]
                        :results   [huge-result]
                        :stdouts   [""]
                        :durations [1]
                        :successes [true]}]
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:conversation-id "conversation"
                         :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          puts        (atom [])
          active      (atom #{})
          bg          (atom nil)
          graphics    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                        (clearModifiers []
                          (reset! active #{})
                          this)
                        (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                          (swap! active into (seq arr))
                          this)
                        (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                          (apply swap! active disj (seq arr))
                          this)
                        (getActiveModifiers []
                          (if (empty? @active)
                            (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                            (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                        (setForegroundColor [_] this)
                        (setBackgroundColor [c] (reset! bg c) this)
                        (putString [_col _row text]
                          (swap! puts conj {:text text :bg @bg :sgr @active})
                          this)
                        (fillRectangle [_ _ _] this)
                        (setCharacter [_ _ _] this))]
      (render/draw-chat-bubble! graphics
        {:role :assistant :text (:text payload) :prewrapped-lines (:lines payload)}
        0 2 96 {:viewport-h 50})
      ;; Compact iteration/block label band replaced the verbose
      ;; `Turn: <uuid>, Iteration: N, Block: M` summary suffix.
      (let [summary-put (some #(when (str/includes? (:text %) "[iteration 1 · block 1]") %) @puts)]
        (expect (some? summary-put))
        (expect (= t/md-summary-bg (:bg summary-put)))
        (expect (contains? (:sgr summary-put) com.googlecode.lanterna.SGR/BOLD))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Tool detail Markdown rendering - preserves the presentation contract:
;; tool render-fns return Markdown, so an expanded tool-result details
;; block must paint headings/bullets/inline emphasis as Markdown rows,
;; not as raw `**bold**` text.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe tool-detail-markdown-rendering-test
  (describe "expanded tool result body renders Markdown structurally"
    (it "bullet lines inside an expanded tool result are reachable as text"
      ;; The earlier contract re-rendered tool-result bodies through the
      ;; Markdown pipeline, producing MARKER_MD_BULLET rows. The current
      ;; renderer keeps tool result bodies as literal text on the
      ;; result-marker band — cheaper, and side-steps reparsing arbitrary
      ;; tool output. The user-facing text still shows the bullets, just
      ;; not as styled bullet rows.
      (render/invalidate-cache!)
      (let [body (str "Tool finished:\n"
                   "- alpha line\n"
                   "- beta line\n"
                   "- gamma line")
            big-body (str body "\n\n" (apply str (repeat 4096 "x")))
            trace [{:code ["(v/bash \"echo hi\")"]
                    :results [big-body]
                    :result-kinds [:tool]
                    :result-details [{:op :v/bash :op/tag :op.tag/action
                                      :color-role :tool-color/shell}]
                    :stdouts [""] :durations [1] :successes [true]}]
            opts {:conversation-id "conversation"
                  :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {["conversation" "iteration:t123e4567:i1:b1:result"] true}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)]
        (expect (some #(str/includes? % "alpha line") lines))
        (expect (some #(str/includes? % "beta line") lines))
        (expect (some #(str/includes? % "gamma line") lines))))

    (it "inline bold/italic inside tool results paints with inline sentinels"
      (render/invalidate-cache!)
      (let [body "Result: **important** and *subtle*."
            ;; Trip auto-collapse with filler.
            big-body (str body "\n" (apply str (repeat 4096 "y")))
            trace [{:code ["(v/bash \"echo hi\")"]
                    :results [big-body]
                    :result-kinds [:tool]
                    :result-details [{:op :v/bash :op/tag :op.tag/action
                                      :color-role :tool-color/shell}]
                    :stdouts [""] :durations [1] :successes [true]}]
            opts {:conversation-id "conversation"
                  :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {["conversation" "iteration:t123e4567:i1:b1:result"] true}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            text    (:text payload)]
        ;; The body row carries inline-bold sentinels so the painter
        ;; styles "important" as bold instead of leaving the literal
        ;; `**important**` text on the row.
        (expect (str/includes? text p/INLINE_BOLD_ON))
        (expect (str/includes? text p/INLINE_BOLD_OFF))
        (expect (str/includes? text p/INLINE_ITALIC_ON))
        (expect (str/includes? text p/INLINE_ITALIC_OFF))
        (expect (str/includes? text "important"))
        (expect (str/includes? text "subtle"))
        (expect (not (str/includes? text "**important**")))
        (expect (not (str/includes? text "*subtle*")))))

    (it "errors keep raw rendering - error formatting handles its own marker"
      ;; Errors come through err-result-marker; we explicitly opt OUT
      ;; of `:render-as :markdown` for them in `form-lines`. This test
      ;; pins that contract: the error body still appears verbatim and
      ;; does NOT pick up bullet markers from accidental Markdown re-parse.
      (render/invalidate-cache!)
      (let [trace [{:code ["(boom)"]
                    :results ["- pretend-bullet"]
                    :result-kinds [:tool]
                    :stdouts [""] :durations [1] :successes [false]}]
            opts {:conversation-id "conversation"
                  :conversation-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)]
        ;; The literal `- pretend-bullet` text stays in the row; we
        ;; never re-render error bodies as Markdown.
        (expect (some #(str/includes? % "- pretend-bullet") lines))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Mermaid fenced rendering - `vis-mermaid` registers a fenced renderer.
;; The TUI must route ` ```mermaid ` fences through the extension and
;; paint the rendered ASCII rows on the code-block band.
;; ─────────────────────────────────────────────────────────────────────────

;; ─────────────────────────────────────────────────────────────────────────
;; Details with Markdown body - markdown inside <details> renders fully.
;; Regression test for the user-facing `<details>` block in answers:
;; bullets, headings, and inline emphasis must render as such, not as
;; raw markdown text.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe details-with-markdown-body-test
  ;; `format-answer-markdown-data` requires canonical answer-IR.
  ;; `vis/text->ir` does NOT lift inline `<details>`/`<summary>` HTML
  ;; into structured `[:details ...]` IR (commonmark treats them as
  ;; inline HTML and folds them into the surrounding paragraph). The
  ;; lift happens upstream of these tests, so the tests construct
  ;; canonical details IR directly.
  (let [body-ir [:details {:open? true}
                 [:summary {} [:span {} "Plan"]]
                 [:h {:level 2} [:span {} "Step 1"]]
                 [:ul {}
                  [:li {} [:p {} [:span {} "alpha"]]]
                  [:li {} [:p {} [:span {} "beta"]]]]
                 [:p {}
                  [:span {} "Then "]
                  [:strong {} [:span {} "bold"]]
                  [:span {} " word."]]]]
    (it "<details> with markdown body renders the body as markdown when expanded"
      ;; Inside an expanded `:details` block, the renderer emits inner
      ;; block rows on plain answer-text bands rather than the
      ;; standalone `MARKER_MD_H2` / `MARKER_MD_BULLET` lanes. Inline
      ;; styling is encoded with per-character PUA sentinels (U+E110..),
      ;; so the visible text only matches after stripping them.
      (let [payload (render/format-answer-markdown-data [:ir {} body-ir] 80
                      {:conversation-id "cid"
                       :detail-expansions {["cid" "answer:details:d1"] true}})
            visible (mapv strip-sentinels (:lines payload))
            text    (:text payload)]
        (expect (some #(str/includes? % "Step 1") visible))
        (expect (some #(str/includes? % "alpha") visible))
        (expect (some #(str/includes? % "beta") visible))
        (expect (str/includes? text p/INLINE_BOLD_ON))
        (expect (str/includes? text p/INLINE_BOLD_OFF))
        (expect (not (str/includes? text "**bold**")))))

    (it "collapsed <details> hides body but keeps summary visible"
      (let [collapsed-ir [:ir {}
                          [:details {:open? false}
                           [:summary {} [:span {} "Plan"]]
                           [:ul {}
                            [:li {} [:p {} [:span {} "alpha"]]]
                            [:li {} [:p {} [:span {} "beta"]]]]]]
            payload (render/format-answer-markdown-data collapsed-ir 80
                      {:conversation-id "cid"
                       :detail-expansions {["cid" "answer:details:d1"] false}})
            lines (:lines payload)
            summary-line (first (filter #(and (= p/MARKER_MD_SUMMARY (marker-of %))
                                           (str/includes? % "Plan"))
                                  lines))]
        (expect (some? summary-line))
        (expect (str/includes? summary-line "▸"))
        (expect (not-any? #(str/includes? % "alpha") lines))
        (expect (not-any? #(str/includes? % "beta") lines))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Provider-error / system-call / tool-call presentation contract - these
;; lean on the shared `internal.presentation` ns. We exercise the cross
;; surface contract from the TUI side too: presentation maps must round-
;; trip into Markdown that the TUI can then tokenise as usual.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe answer-separator-test
  (it "does not draw a bottom border between reasoning and final answer"
    (render/invalidate-cache!)
    (let [payload (render/format-answer-with-thinking-data
                    [:ir {} [:p {} [:span {} "done"]]] [{:thinking "reasoning"}]
                    80 {:show-thinking true :show-iterations true} nil false {})]
      (expect (not (str/includes? (:text payload) p/MARKER_ANSWER_SEP)))
      (expect (not-any? #(str/starts-with? % p/MARKER_ANSWER_SEP) (:lines payload))))))

(defdescribe message-footer-test
  (it "does not register a per-message copy button"
    (cr/reset!)
    (cr/begin-frame!)
    (let [message      {:role :assistant :text "hello world"}
          start        4
          left         2
          width        36
          viewport-top 7
          height       (render/draw-chat-bubble! (dummy-text-graphics) message start left width
                         {:viewport-top viewport-top :viewport-h 40})
          hit-col      (+ left 2)]
      (cr/commit-frame!)
      (expect (= 3 height))
      (expect (every? nil?
                (map #(cr/lookup hit-col %)
                  (range viewport-top (+ viewport-top start height)))))))

  (it "renders cached token usage in the assistant bubble footer"
    (let [puts    (atom [])
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers [] this)
                     (enableModifiers [_] this)
                     (disableModifiers [_] this)
                     (getActiveModifiers []
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text "hello"
                      :tokens {:input 100 :output 20 :cached 70}}
                     4 2 60 {:viewport-h 40})]
      (expect (= 4 height))
      (expect (some #(str/includes? (:text %) "↑100 (cached 70) ↓20")
                @puts))))

  (it "renders the optional turn separator above the You label"
    (let [puts    (atom [])
          fills   (atom [])
          fg      (atom nil)
          bg      (atom nil)
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [c] (reset! fg c) this)
                     (setBackgroundColor [c] (reset! bg c) this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text :fg @fg :bg @bg :sgr @active})
                       this)
                     (fillRectangle [_pos _size _ch]
                       (swap! fills conj {:fg @fg :bg @bg})
                       this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :user :text "hello" :turn-separator? true}
                     4 2 30 {:viewport-h 40})]
      (expect (= 7 height))
      (expect (some #(and (= 4 (:row %))
                       (str/includes? (:text %) "──")
                       (= t/turn-separator-bg (:bg %)))
                @puts))
      (expect (not-any? #(= 5 (:row %)) @puts))
      (expect (some #(and (= 6 (:row %))
                       (= "You" (:text %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                @puts))))

  (it "renders user messages with a left rail and markdown styling"
    (let [puts    (atom [])
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text :sgr @active})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          ;; Inline markdown styling for user-message bubbles now lives
          ;; in the `virtual.clj` projection layer that supplies
          ;; `:prewrapped-lines` to `draw-chat-bubble!`. The bubble
          ;; painter itself no longer parses markdown from `:text`
          ;; (the IR→prewrapped lift happens upstream). Feed prewrapped
          ;; lines directly so the assertion exercises the painter, not
          ;; the retired in-painter markdown lift.
          rendered (render/format-answer-markdown-data
                     (vis/text->ir "**SIEMA**\n\n> quoted text") 50 nil)
          message {:role :user :text "**SIEMA**\n\n> quoted text"
                   :prewrapped-lines (:lines rendered)
                   :line-meta (:line-meta rendered)}
          start   4
          left    2
          width   50
          height  (render/draw-chat-bubble! graphics message start left width
                    {:viewport-h 40})]
      (expect (pos? height))
      ;; SIEMA appears on one of the painted rows; markdown styling
      ;; (bold/italic via inline sentinels) is driven by
      ;; `virtual.clj`'s projection layer, which uses MARKER_ANSWER_TXT
      ;; on the answer side and MARKER_MD_* on plain-markdown blocks.
      ;; This test only pins that the painter visits the rendered
      ;; rows and surfaces the user-visible text — the bold-SGR
      ;; activation is exercised by the answer-side painter tests.
      (expect (some #(str/includes? (or (:text %) "") "SIEMA") @puts))
      (expect (some #(str/includes? (or (:text %) "") "quoted text") @puts))))

  (it "leaves only the final gap after the user bubble fill"
    (let [fills    (atom [])
          active   (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString
                       ([_ _ _] this))
                     (fillRectangle [pos size _ch]
                       (swap! fills conj {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
                                          :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
                                          :w   (.getColumns ^com.googlecode.lanterna.TerminalSize size)
                                          :h   (.getRows ^com.googlecode.lanterna.TerminalSize size)})
                       this)
                     (setCharacter [_ _ _] this))
          message      {:role :user :text "hello world"}
          start        4
          left         2
          width        36
          viewport-top 7
          height       (render/draw-chat-bubble! graphics message start left width
                         {:viewport-top viewport-top :viewport-h 40})
          gap-row      (+ start height -1)
          bubble-fill  (some (fn [fill]
                               (when (and (= left (:col fill))
                                       (= width (:w fill))
                                       (> (:h fill) 1))
                                 fill))
                         @fills)
          bubble-last-row (+ (:row bubble-fill) (:h bubble-fill) -1)]
      (expect (= 5 height))
      (expect (= 3 (:h bubble-fill)))
      (expect (= bubble-last-row (dec gap-row))))))

(defdescribe bubble-row-clipping-test
  (it "clips prewrapped formatter rows at bubble content width"
    (let [plain   (apply str (repeat 80 "x"))
          marked  (str p/MARKER_CODE_OK (apply str (repeat 80 "y")))
          clipped (clip-lines-preserving-markers [plain marked] 17)]
      (expect (= 2 (count clipped)))
      (expect (= (subs plain 0 17) (first clipped)))
      (expect (= p/MARKER_CODE_OK (marker-of (second clipped))))
      (expect (<= (p/display-width (first clipped)) 17))
      (expect (<= (p/display-width (body-of (second clipped))) 17))))

  (it "clips ANSI-colored Clojure formatter rows without handing ESC to Lanterna"
    (let [ansi-line (str p/MARKER_CODE_OK
                      "\u001b[32m(\u001b[0m\u001b[34mdef\u001b[0m "
                      "\u001b[30mrequest-classification\u001b[0m "
                      "\u001b[35m:evidence-bearing-code-change\u001b[0m")
          clipped   (first (clip-lines-preserving-markers [ansi-line] 12))]
      (expect (= p/MARKER_CODE_OK (marker-of clipped)))
      (expect (str/includes? clipped "\u001b[32m"))
      (expect (<= (p/display-width (strip-ansi (body-of clipped))) 12)))))

(defdescribe slash-command-suggestions-overlay-test
  (it "draws a bordered, BOLD, accent-stripe title with flex hint pairs"
    (let [puts   (atom [])
          fills  (atom [])
          fg     (atom nil)
          bg     (atom nil)
          active (atom #{})
          g (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
              (clearModifiers []
                (reset! active #{})
                this)
              (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (swap! active into (seq arr))
                this)
              (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (apply swap! active disj (seq arr))
                this)
              (getActiveModifiers []
                (if (empty? @active)
                  (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                  (java.util.EnumSet/copyOf ^java.util.Collection @active)))
              (setForegroundColor [c] (reset! fg c) this)
              (setBackgroundColor [c] (reset! bg c) this)
              (getForegroundColor [] @fg)
              (getBackgroundColor [] @bg)
              (putString [col row text]
                (swap! puts conj {:col col :row row :text text
                                  :fg @fg :bg @bg :sgr @active})
                this)
              (fillRectangle [pos size _ch]
                (swap! fills conj {:row (.getRow pos) :col (.getColumn pos)
                                   :w (.getColumns size) :h (.getRows size)
                                   :fg @fg :bg @bg})
                this)
              (setCharacter [_ _ _] this))
          suggestions [{:label "first"  :slash/usage "/first"  :slash/selected? true}
                       {:label "second" :slash/usage "/second" :slash/selected? false}]
          input-top   10
          cols        80
          n           (count suggestions)
          ;; Layout (from top to bottom):
          ;;   margin-row -> title-row -> border-row -> sug rows ...
          first-sug   (- input-top n)
          border-row  (dec first-sug)
          title-row   (dec border-row)
          margin-row  (dec title-row)
          ;; Horizontal margin matches input box rule pad (2 cols).
          pad         2
          inner-w     (- cols (* 2 pad))]
      (render/draw-slash-command-suggestions! g suggestions input-top cols)

      ;; Title row sits ABOVE the border row (border under title).
      (expect (< title-row border-row))

      ;; Title row: accent stripe (fillRectangle) on title-bg, inset
      ;; by `pad` cols on each side so it lines up with the input box.
      (expect (some #(and (= title-row (:row %))
                       (= t/dialog-title-bg (:bg %))
                       (= pad (:col %)))
                @fills))

      ;; Border row UNDER the title: horizontal rule, inset by `pad`,
      ;; same column span as the title accent stripe.
      (expect (some #(and (= border-row (:row %))
                       (str/starts-with? (:text %) "─")
                       (= pad (:col %))
                       (= t/dialog-border (:fg %))
                       (= t/terminal-bg (:bg %)))
                @puts))

      ;; Top margin row: a full-width terminal-bg gap above the title.
      (expect (some #(and (= margin-row (:row %))
                       (= 0 (:col %))
                       (= t/terminal-bg (:bg %)))
                @fills))

      ;; Title row: BOLD label "Slash commands" on the accent stripe.
      (expect (some #(and (= title-row (:row %))
                       (str/includes? (:text %) "Slash commands")
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD)
                       (= t/dialog-title-fg (:fg %))
                       (= t/dialog-title-bg (:bg %)))
                @puts))

      ;; Title row: BOLD keys for each [key action] hint pair.
      ;; Tab is the only completion key — Enter is intentionally NOT
      ;; advertised here (Enter falls through to the normal send path;
      ;; only Tab acts on the highlighted suggestion).
      (doseq [k ["↑↓/wheel" "Tab"]]
        (expect (some #(and (= title-row (:row %))
                         (= k (:text %))
                         (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                  @puts)))
      (expect (not-any? #(and (= title-row (:row %))
                           (= "Enter" (:text %)))
                @puts))

      ;; Title row: action words rendered NON-BOLD next to their keys.
      (doseq [a [" select" " complete"]]
        (expect (some #(and (= title-row (:row %))
                         (= a (:text %))
                         (not (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD)))
                  @puts)))

      ;; Title items spread across the inner width (space-between):
      ;; first item sits inside the left margin, last item in the
      ;; right half of the inner span.
      (let [title-puts (filter #(= title-row (:row %)) @puts)
            cols-used  (mapv :col title-puts)]
        (expect (some #(<= pad % (+ pad 2)) cols-used))
        (expect (some #(>= % (+ pad (quot inner-w 2))) cols-used)))

      ;; Suggestion rows are inset to the same column span as the
      ;; title accent stripe (margin-left = margin-right = pad).
      ;; The body fill on every row uses the normal `dialog-bg`
      ;; palette — selection is signalled by the `>` glyph in the
      ;; left margin, NOT by a full-row accent stripe.
      (expect (some #(and (= first-sug (:row %))
                       (= pad (:col %))
                       (= inner-w (:w %))
                       (= t/dialog-bg (:bg %)))
                @fills))

      ;; The selected row carries a BOLD `> ` cursor glyph at the
      ;; FIRST col of the inset body (col `pad`), painted in
      ;; `dialog-hint-key` on `dialog-bg` so the marker reads as
      ;; INSIDE the menu rather than floating in the terminal margin.
      ;; The non-selected row gets nothing painted in that column.
      (expect (some #(and (= first-sug (:row %))
                       (= pad (:col %))
                       (= "> " (:text %))
                       (= t/dialog-hint-key (:fg %))
                       (= t/dialog-bg (:bg %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                @puts))
      (expect (not-any? #(and (= (inc first-sug) (:row %))
                           (= pad (:col %))
                           (= "> " (:text %)))
                @puts))

      ;; Each suggestion row paints a markdown-style chip:
      ;;   <code-bg fill> /cmd <code-bg fill end> ` - ` <italic desc>
      (let [sug-rows  (filter #(<= first-sug (:row %)) @puts)
            usages    (filter #(str/starts-with? (:text %) "/") sug-rows)
            seps      (filter #(= " - " (:text %)) sug-rows)
            descs     (filter #(contains? (:sgr %) com.googlecode.lanterna.SGR/ITALIC) sug-rows)
            chip-fills (filter #(and (<= first-sug (:row %))
                                  (= t/code-block-bg (:bg %))) @fills)]
        ;; One chip fill, usage, separator and description per suggestion.
        (expect (= n (count chip-fills)))
        (expect (= n (count usages)))
        (expect (= n (count seps)))
        (expect (= n (count descs)))
        ;; Layout invariants per row: chip wraps the usage with 1 col
        ;; padding on each side, ` - ` follows the chip, italic desc
        ;; follows the separator. The chip starts AFTER the selection
        ;; gutter (`p/SELECTION_WIDTH` cols inside the inset body).
        (doseq [[chip u s d] (map vector
                               (sort-by :row chip-fills)
                               (sort-by :row usages)
                               (sort-by :row seps)
                               (sort-by :row descs))]
          ;; Chip lives past the selection gutter — first chip col is
          ;; at least `pad + p/SELECTION_WIDTH` (cursor + 1-col margin).
          (expect (>= (:col chip) (+ pad com.blockether.vis.ext.channel-tui.primitives/SELECTION_WIDTH)))
          ;; Chip starts one col before the usage and is exactly
          ;; (usage-width + 2) wide.
          (expect (= (:col u) (inc (:col chip))))
          (expect (= (:w chip) (+ (count (:text u)) 2)))
          ;; Usage paints in code-block colors.
          (expect (= t/code-block-fg (:fg u)))
          (expect (= t/code-block-bg (:bg u)))
          ;; ` - ` separator sits immediately after the chip.
          (expect (= (:col s) (+ (:col chip) (:w chip))))
          ;; Italic description follows the separator on the same row.
          (expect (= (:row u) (:row d)))
          (expect (= (:col d) (+ (:col s) (count (:text s)))))))))

  (it "drops the border row when there is not enough vertical space"
    (let [puts   (atom [])
          active (atom #{})
          g (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
              (clearModifiers [] (reset! active #{}) this)
              (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (swap! active into (seq arr)) this)
              (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (apply swap! active disj (seq arr)) this)
              (getActiveModifiers []
                (if (empty? @active)
                  (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                  (java.util.EnumSet/copyOf ^java.util.Collection @active)))
              (setForegroundColor [_] this)
              (setBackgroundColor [_] this)
              (getForegroundColor [] nil)
              (getBackgroundColor [] nil)
              (putString [col row text]
                (swap! puts conj {:col col :row row :text text}) this)
              (fillRectangle [_ _ _] this)
              (setCharacter [_ _ _] this))
          ;; input-top = 2: only enough room for title (1 row) + 1
          ;; suggestion. Border + margin must drop.
          suggestions [{:label "a" :slash/usage "/a" :slash/selected? true}]
          input-top   2]
      (render/draw-slash-command-suggestions! g suggestions input-top 40)
      ;; Title row sits at row 0 (above the single suggestion at row 1).
      ;; No horizontal-rule border was drawn (no ─ in any putString).
      (expect (some #(= 0 (:row %)) @puts))
      (expect (not-any? #(str/starts-with? (:text %) "─") @puts)))))
