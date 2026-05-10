(ns com.blockether.vis.bench-stream
  "Bench harness for the live-streaming TUI bubble pipeline.

   Two modes:

     1. `run-progressive!` - simulate streaming. Grow body chunk by
        chunk, time the full TUI render path per frame. Produces the
        worst-case curve (cache always misses, each chunk produces a
        fresh IR). Reports p50/p95/p99/max per body-size bucket.

     2. `run-criterium!` - per-stage criterium quick-bench at fixed
        body sizes. Isolates `text->ir`, walker (`ir->entries`),
        wrap, full pipeline. Use this to attribute cost.

   Goal: per-frame cost should be FLAT across body size. Today it is
   linear in body length and overshoots the 80 ms render throttle by
   the 50k-100k bucket.

   Run from nREPL:

     (require '[com.blockether.vis.bench-stream :as b] :reload)
     (b/run-progressive! {})
     (b/run-criterium! {})

   Results print as a table; also returned as data."
  (:require [clojure.string :as str]
            [criterium.core :as crit]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]))

(def ^:private lorem
  "Lorem **bold** _ipsum_ dolor sit amet, `consectetur` adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi.

```clojure
(defn foo [x] (+ x 1))
```

- one
- two
- three

> a quote line that wraps a bit and contains some `code` inline.

")

(defn- body-of-length
  "Synthetic markdown body of at least `n` chars, structured like a
   real assistant answer (paragraphs, list, code block, quote)."
  ^String [^long n]
  (let [sb (StringBuilder.)]
    (while (< (.length sb) n)
      (.append sb lorem))
    (.substring sb 0 (min n (.length sb)))))

(defn- percentile [sorted-ns ^double p]
  (let [n (count sorted-ns)]
    (if (zero? n) 0
      (nth sorted-ns (min (dec n) (long (* p (dec n))))))))

(defn- summarize [samples-ns]
  (let [s (vec (sort samples-ns))
        n (count s)
        sum (reduce + 0 s)]
    {:n n
     :total-ms (/ sum 1e6)
     :p50-ms (/ (percentile s 0.50) 1e6)
     :p95-ms (/ (percentile s 0.95) 1e6)
     :p99-ms (/ (percentile s 0.99) 1e6)
     :max-ms (/ (or (peek s) 0) 1e6)
     :mean-ms (/ (double (/ sum (max 1 n))) 1e6)}))

(defn- bench-frame!
  "Run one streaming-frame: lift fresh IR, time
   `format-answer-markdown-data`, return [ns lines-count].

   `extra-opts` (optional) is merged into the opts map - lets callers
   pass `{:max-lines 200}` to exercise the windowed walker path."
  ([^String text ^long bubble-w]
   (bench-frame! text bubble-w nil))
  ([^String text ^long bubble-w extra-opts]
   (let [ir (vis/text->ir text)
         ;; force realization with a unique opts key so cache always misses
         opts (merge {:conversation-turn-id (str "bench-" (System/nanoTime))}
                     extra-opts)
         t0 (System/nanoTime)
         result (render/format-answer-markdown-data ir bubble-w opts)
         dt (- (System/nanoTime) t0)]
     [dt (count (:lines result))])))

(defn run-progressive-end-to-end!
  "END-TO-END PRODUCTION bench: drives `screen/render-frame!` against
   a real `TerminalScreen` backed by a `DefaultVirtualTerminal` (in-
   memory, no actual terminal I/O). Measures: virtual/layout +
   draw-messages-area + chrome paints + Lanterna DELTA refresh.

   This is the realistic per-frame cost. Includes EVERYTHING the
   render thread does in production except the actual stdout write.

   Each frame: append `chunk-chars` to the live iteration's :thinking,
   bump :render-version, call render-frame!, time it.

   Emits `stream_e2e_*` metrics."
  [{:keys [target-chars chunk-chars cols rows warmup-frames]
    :or   {target-chars  100000
           chunk-chars   1000
           cols          200
           rows          50
           warmup-frames 5}}]
  (let [screen-cls (Class/forName "com.googlecode.lanterna.screen.TerminalScreen")
        vt-cls     (Class/forName "com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal")
        size-cls   (Class/forName "com.googlecode.lanterna.TerminalSize")
        size       (.newInstance (.getConstructor size-cls
                                   (into-array Class [Integer/TYPE Integer/TYPE]))
                     (object-array [(int cols) (int rows)]))
        term       (.newInstance (.getConstructor vt-cls (into-array Class [size-cls]))
                     (object-array [size]))
        screen     (.newInstance (.getConstructor screen-cls
                                   (into-array Class
                                     [(Class/forName "com.googlecode.lanterna.terminal.Terminal")]))
                     (object-array [term]))
        _          (.startScreen screen)
        render-frame! (do (require 'com.blockether.vis.ext.channel-tui.screen)
                          (ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'render-frame!))
        render-live!  (ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'render-live-bubble-frame!)
        ;; phase-isolation knobs. Set via dynamic var so the bench can
        ;; force each phase off without touching production code.
        skip-chrome?  (Boolean/parseBoolean (System/getProperty "bench.skip-chrome" "false"))
        skip-refresh? (Boolean/parseBoolean (System/getProperty "bench.skip-refresh" "false"))
        skip-bubble?  (Boolean/parseBoolean (System/getProperty "bench.skip-bubble" "false"))
        previous-layout (atom nil)
        ;; Stable scrollback (3 short bubbles + 1 user) so layout has
        ;; some baseline work to do.
        stable-msgs [{:role :user :ir (vis/text->ir "What's up?")}
                     {:role :assistant :ir (vis/text->ir (body-of-length 800))
                      :conversation-turn-id (str (gensym))}
                     {:role :user :ir (vis/text->ir "And then?")
                      :conversation-turn-id (str (gensym))}]
        bench-frame!
        (fn [^String thinking-text ^long version]
          (let [progress {:iterations [{:thinking thinking-text}]}
                ;; Pending assistant placeholder = the loading bubble.
                msgs (conj stable-msgs
                       {:role :assistant :pending? true
                        :ir [:ir {} [:p {} [:span {} "Sending..."]]]
                        :conversation-turn-id (str (gensym))})
                db {:messages msgs
                    :progress progress
                    :loading? true
                    :render-version version
                    :input {:lines [""] :crow 0 :ccol 0}
                    :settings {:show-thinking true
                               :show-iterations true
                               :differentiate-turns false}
                    :detail-expansions {}
                    :conversation {:id "bench-conv"}
                    :turn-start-ms 0}
                t0 (System/nanoTime)
                ;; First frame uses render-frame! to populate previous-layout;
                ;; subsequent frames use render-live-bubble-frame! (production
                ;; fast path for 80ms live ticks).
                ;; `bench.force-full=true` keeps using render-frame! every
                ;; frame - models the workload where user types in the input
                ;; box during streaming (each keystroke bumps version, the
                ;; partial-live? predicate sees app-db diff outside the
                ;; whitelisted keys, falls through to full render).
                force-full? (Boolean/parseBoolean (System/getProperty "bench.force-full" "false"))
                layout (cond
                         (or (nil? @previous-layout) force-full?)
                         (render-frame! screen cols rows db 0)

                         (and skip-chrome? skip-refresh? skip-bubble?)
                         ;; only the virtual/layout work
                         (let [bubble-w (max 1 (- cols 4))
                               inner-h  (- rows 4)]
                           ((ns-resolve 'com.blockether.vis.ext.channel-tui.virtual 'layout)
                            (:messages db) bubble-w (:settings db) nil inner-h
                            {:progress (:progress db) :loading? true
                             :progress-extra {:now-ms 0 :turn-start-ms 0}}
                            {:conversation-id "x" :detail-expansions {}}))

                         :else
                         (render-live! screen cols rows db 0 @previous-layout))
                _  (reset! previous-layout layout)
                dt (- (System/nanoTime) t0)]
            dt))]
    (try
      (dotimes [i warmup-frames]
        (bench-frame! (body-of-length 2000) (long i)))
      (let [frames (atom [])
            steps  (long (/ target-chars chunk-chars))]
        (loop [i 0]
          (when (< i steps)
            (let [size (* (inc i) chunk-chars)
                  ns   (bench-frame! (body-of-length size)
                                     (long (+ i warmup-frames 1)))]
              (swap! frames conj {:size size :ns ns})
              (recur (inc i)))))
        (let [all @frames
              bucket-of (fn [s]
                          (cond
                            (< s 5000)   "<5k"
                            (< s 20000)  "5k-20k"
                            (< s 50000)  "20k-50k"
                            (< s 100000) "50k-100k"
                            :else        "100k+"))
              buckets (->> all
                        (group-by #(bucket-of (:size %)))
                        (into (sorted-map-by
                                (fn [a b]
                                  (compare
                                    (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                    (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
              tot (summarize (mapv :ns all))]
          (println)
          (println (format "END-TO-END render-frame! @ %dx%d: %d steps × %d chars"
                     cols rows steps chunk-chars))
          (println "──────────────────────────────────────────────────────────────────────")
          (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
            "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
          (doseq [[name xs] buckets]
            (let [s (summarize (mapv :ns xs))]
              (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
                name (:n s) (:mean-ms s) (:p50-ms s)
                (:p95-ms s) (:p99-ms s) (:max-ms s))))
          (println)
          (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
            (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
          (println "METRIC stream_e2e_total_ms=" (format "%.2f" (double (:total-ms tot))))
          (println "METRIC stream_e2e_mean_ms=" (format "%.3f" (double (:mean-ms tot))))
          (println "METRIC stream_e2e_p99_ms="   (format "%.2f" (double (:p99-ms tot))))
          (println "METRIC stream_e2e_max_ms="   (format "%.2f" (double (:max-ms tot))))
          (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
            (println "METRIC stream_e2e_50k_100k_mean_ms=" (format "%.3f" (double big))))
          (when-let [bigger (some-> (get buckets "100k+") (->> (mapv :ns) summarize :mean-ms))]
            (println "METRIC stream_e2e_100k_plus_mean_ms=" (format "%.3f" (double bigger))))
          {:per-frame all
           :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
           :total     tot}))
      (finally
        (try (.stopScreen screen) (catch Throwable _ nil))))))

(defn run-progressive-progress!
  "Bench `render/progress->lines-data` directly - the ACTUAL production
   hot path for live streaming. Each frame: extend the LAST iteration's
   `:thinking` text by `chunk-chars`, call `progress->lines-data`,
   time it. Earlier iterations stay frozen (completed).

   `:n-iterations` simulates a deep-reasoning model that runs many
   iterations. Default 1; set higher to stress trace-rendering.

   Emits `stream_progress_50k_100k_mean_ms` and `_100k_plus_`."
  [{:keys [target-chars chunk-chars bubble-w warmup-frames n-iterations]
    :or   {target-chars  100000
           chunk-chars   1000
           bubble-w      96
           warmup-frames 5
           n-iterations  1}}]
  (let [;; Frozen earlier iterations: each is 3-5k thinking + small
        ;; code + small result, like a real deep-reasoning trace.
        frozen-iters
        (let [r (java.util.Random. 4242)]
          (vec (for [_ (range (max 0 (dec n-iterations)))]
                 {:thinking (body-of-length (+ 1500 (.nextInt r 3000)))
                  :code     [(str "(+ 1 " (.nextInt r 100) ")")]
                  :results  ["42"]
                  :result-kinds [:value]
                  :stdouts  [""]
                  :stderrs  [""]
                  :durations [10]
                  :successes [true]
                  :started-at-ms [0]})))
        bench-frame!
        (fn [^String thinking-text]
          (let [live-iter {:thinking thinking-text}
                progress {:iterations (conj frozen-iters live-iter)}
                t0 (System/nanoTime)
                _  (render/progress->lines-data progress bubble-w
                     {:show-thinking true :show-iterations true}
                     {:now-ms 0 :turn-start-ms 0})
                dt (- (System/nanoTime) t0)]
            dt))]
    (dotimes [_ warmup-frames]
      (bench-frame! (body-of-length 2000)))
    (render/invalidate-cache!)
    (let [frames (atom [])
          steps  (long (/ target-chars chunk-chars))]
      (loop [i 0]
        (when (< i steps)
          (let [size (* (inc i) chunk-chars)
                ns   (bench-frame! (body-of-length size))]
            (swap! frames conj {:size size :ns ns})
            (recur (inc i)))))
      (let [all @frames
            bucket-of (fn [s]
                        (cond
                          (< s 5000)   "<5k"
                          (< s 20000)  "5k-20k"
                          (< s 50000)  "20k-50k"
                          (< s 100000) "50k-100k"
                          :else        "100k+"))
            buckets (->> all
                      (group-by #(bucket-of (:size %)))
                      (into (sorted-map-by
                              (fn [a b]
                                (compare
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
            tot (summarize (mapv :ns all))]
        (println)
        (println (format "progress->lines-data live-stream: %d steps × %d chars (bubble-w=%d)"
                   steps chunk-chars bubble-w))
        (println "──────────────────────────────────────────────────────────────────────")
        (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
          "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
        (doseq [[name xs] buckets]
          (let [s (summarize (mapv :ns xs))]
            (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
              name (:n s) (:mean-ms s) (:p50-ms s)
              (:p95-ms s) (:p99-ms s) (:max-ms s))))
        (println)
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
        (println "METRIC stream_progress_total_ms=" (format "%.2f" (double (:total-ms tot))))
        (println "METRIC stream_progress_mean_ms=" (format "%.3f" (double (:mean-ms tot))))
        (println "METRIC stream_progress_p99_ms="   (format "%.2f" (double (:p99-ms tot))))
        (println "METRIC stream_progress_max_ms="   (format "%.2f" (double (:max-ms tot))))
        (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_progress_50k_100k_mean_ms=" (format "%.3f" (double big))))
        (when-let [bigger (some-> (get buckets "100k+") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_progress_100k_plus_mean_ms=" (format "%.3f" (double bigger))))
        {:per-frame all
         :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
         :total     tot}))))

(defn run-progressive-window!
  "Bench `ir->lines-window` vs `(subvec (ir->lines ...) start (+ start n))`.

   Models user scrolled into a streaming bubble. The viewport is at
   fixed row `start` (= scroll-within-bubble). Each frame the body
   grows but the viewport row doesn't move. Window walker should
   bail at `start + num + slack`, skipping the rest of the body.

   `:mode :full` baseline = `(subvec (ir->lines ...) start (+ start num))`.
   `:mode :window` optimized = `ir->lines-window`.

   `:start-row` is the bubble-internal row offset; default 0 = head."
  [{:keys [target-chars chunk-chars bubble-w start-row viewport-rows
           warmup-frames mode]
    :or   {target-chars   100000
           chunk-chars    1000
           bubble-w       100
           start-row      0
           viewport-rows  40
           warmup-frames  5
           mode           :full}}]
  (let [content-w (- (long bubble-w) 4)
        bench-frame!
        (case mode
          :full
          (fn [^String text]
            (let [ir (vis/text->ir text)
                  t0 (System/nanoTime)
                  lines (ir-tui/ir->lines ir content-w)
                  have  (count lines)
                  _     (if (>= (long start-row) have)
                          []
                          (vec (subvec lines start-row
                                       (min have (+ (long start-row)
                                                   (long viewport-rows))))))
                  dt (- (System/nanoTime) t0)]
              [dt have]))
          :window
          (fn [^String text]
            (let [ir (vis/text->ir text)
                  t0 (System/nanoTime)
                  lines (ir-tui/ir->lines-window ir content-w start-row viewport-rows)
                  dt (- (System/nanoTime) t0)]
              [dt (count lines)])))]
    (dotimes [_ warmup-frames]
      (bench-frame! (body-of-length 2000)))
    (let [frames (atom [])
          steps  (long (/ target-chars chunk-chars))]
      (loop [i 0]
        (when (< i steps)
          (let [size (* (inc i) chunk-chars)
                [ns lines] (bench-frame! (body-of-length size))]
            (swap! frames conj {:size size :ns ns :lines lines})
            (recur (inc i)))))
      (let [all @frames
            bucket-of (fn [s]
                        (cond
                          (< s 5000)   "<5k"
                          (< s 20000)  "5k-20k"
                          (< s 50000)  "20k-50k"
                          (< s 100000) "50k-100k"
                          :else        "100k+"))
            buckets (->> all
                      (group-by #(bucket-of (:size %)))
                      (into (sorted-map-by
                              (fn [a b]
                                (compare
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
            tot (summarize (mapv :ns all))]
        (println)
        (println (format "Window-walker (mode=%s start=%d num=%d): %d steps × %d chars"
                   mode start-row viewport-rows steps chunk-chars))
        (println "──────────────────────────────────────────────────────────────────────")
        (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
          "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
        (doseq [[name xs] buckets]
          (let [s (summarize (mapv :ns xs))]
            (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
              name (:n s) (:mean-ms s) (:p50-ms s)
              (:p95-ms s) (:p99-ms s) (:max-ms s))))
        (println)
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
        (println "METRIC stream_window_total_ms="  (format "%.2f" (double (:total-ms tot))))
        (println "METRIC stream_window_p99_ms="    (format "%.2f" (double (:p99-ms tot))))
        (println "METRIC stream_window_max_ms="    (format "%.2f" (double (:max-ms tot))))
        (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_window_50k_100k_mean_ms=" (format "%.3f" (double big))))
        {:per-frame all
         :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
         :total     tot}))))

(defn run-progressive-tail!
  "Bench `ir->lines-tail` vs `(take-last N (ir->lines ...))`.

   `:mode :full` (default baseline) - full O(body) walk + take-last.
   `:mode :tail` - `ir->lines-tail` (back-walk subset).
   Output is bit-identical between the two by contract; the only
   difference is per-frame cost.

   `:viewport-rows` is the tail length we want.

   Emits `stream_tail_50k_100k_mean_ms`."
  [{:keys [target-chars chunk-chars bubble-w viewport-rows warmup-frames mode]
    :or   {target-chars   100000
           chunk-chars    1000
           bubble-w       100
           viewport-rows  80
           warmup-frames  5
           mode           :full}}]
  (let [content-w (- (long bubble-w) 4)
        bench-frame!
        (case mode
          :full
          (fn [^String text]
            (let [ir (vis/text->ir text)
                  t0 (System/nanoTime)
                  lines (ir-tui/ir->lines ir content-w)
                  _     (vec (take-last viewport-rows lines))
                  dt (- (System/nanoTime) t0)]
              [dt (count lines)]))
          :tail
          (fn [^String text]
            (let [ir (vis/text->ir text)
                  t0 (System/nanoTime)
                  lines (ir-tui/ir->lines-tail ir content-w viewport-rows)
                  dt (- (System/nanoTime) t0)]
              [dt (count lines)])))]
    (dotimes [_ warmup-frames]
      (bench-frame! (body-of-length 2000)))
    (let [frames (atom [])
          steps  (long (/ target-chars chunk-chars))]
      (loop [i 0]
        (when (< i steps)
          (let [size (* (inc i) chunk-chars)
                [ns lines] (bench-frame! (body-of-length size))]
            (swap! frames conj {:size size :ns ns :lines lines})
            (recur (inc i)))))
      (let [all @frames
            bucket-of (fn [s]
                        (cond
                          (< s 5000)   "<5k"
                          (< s 20000)  "5k-20k"
                          (< s 50000)  "20k-50k"
                          (< s 100000) "50k-100k"
                          :else        "100k+"))
            buckets (->> all
                      (group-by #(bucket-of (:size %)))
                      (into (sorted-map-by
                              (fn [a b]
                                (compare
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
            tot (summarize (mapv :ns all))]
        (println)
        (println (format "Tail-walker (mode=%s) stream: %d steps × %d chars (bubble-w=%d, tail-n=%d)"
                   mode steps chunk-chars bubble-w viewport-rows))
        (println "──────────────────────────────────────────────────────────────────────")
        (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
          "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
        (doseq [[name xs] buckets]
          (let [s (summarize (mapv :ns xs))]
            (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
              name (:n s) (:mean-ms s) (:p50-ms s)
              (:p95-ms s) (:p99-ms s) (:max-ms s))))
        (println)
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
        (let [small (some-> (get buckets "<5k")    (->> (mapv :ns) summarize :mean-ms))
              big   (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (when (and small big (pos? small))
            (printf "LINEARITY: 50k-100k / <5k = %.2fx  (target: ≤ 1.5x)%n"
              (double (/ big small)))))
        (println "METRIC stream_tail_total_ms="  (format "%.2f" (double (:total-ms tot))))
        (println "METRIC stream_tail_p99_ms="    (format "%.2f" (double (:p99-ms tot))))
        (println "METRIC stream_tail_max_ms="    (format "%.2f" (double (:max-ms tot))))
        (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_tail_50k_100k_mean_ms=" (format "%.3f" (double big))))
        {:per-frame all
         :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
         :total     tot}))))

(defn run-progressive-layout!
  "End-to-end production-realistic bench: drive `virtual/layout` with
   a small scrollback (3 stable bubbles) plus one growing assistant
   bubble. Each frame: extend the streaming bubble's body by
   `chunk-chars`, lift to fresh IR, call `virtual/layout`, time wall
   clock. Measures the path the TUI critical loop actually runs.

   `:scroll-mode` controls which scroll-state is exercised:
     `:auto`     - scroll=nil (auto-pinned to bottom; tail-walker fast path).
     `:scrolled` - scroll=0    (user scrolled to top; full render fallback).
     `:mid`      - scroll=:mid (numeric mid-position; full render fallback).
   Default `:auto`. Use `:scrolled` to verify the scroll-up path
   doesn't regress catastrophically.

   Emits `stream_layout_50k_100k_mean_ms` so this can be a separate
   autoresearch metric from the synthetic format-answer bench."
  [{:keys [target-chars chunk-chars bubble-w inner-h warmup-frames scroll-mode]
    :or   {target-chars  100000
           chunk-chars   1000
           bubble-w      96
           inner-h       40
           warmup-frames 5
           scroll-mode   :auto}}]
  (let [stable-bodies [(body-of-length 200)
                       (body-of-length 1500)
                       (body-of-length 800)]
        stable-msgs (into [{:role :user :ir (vis/text->ir "What's up?")}]
                      (mapv (fn [body]
                              {:role :assistant
                               :ir   (vis/text->ir body)
                               :conversation-turn-id (str (gensym))})
                        stable-bodies))
        bench-frame!
        (fn [^String stream-body]
          (let [stream-ir (vis/text->ir stream-body)
                msgs (conj stable-msgs
                       {:role :assistant
                        :ir   stream-ir
                        :conversation-turn-id (str "stream-" (System/nanoTime))})
                ;; scroll arg per scroll-mode (see docstring).
                ;; `:scrolled` puts the viewport INTO the streaming
                ;; bubble so its full O(body) render fires (the path
                ;; the tail-pinned fast-path falls back to). Picking
                ;; an enormous scroll number is safe - layout clamps.
                scroll-arg (case scroll-mode
                             :auto     nil
                             :scrolled 999999
                             :mid      (max 0 (long (/ (count stream-body) 8))))
                t0 (System/nanoTime)
                _  (virtual/layout msgs bubble-w {} scroll-arg inner-h
                     {:loading? false}
                     {:conversation-id "bench"
                      :detail-expansions {}})
                dt (- (System/nanoTime) t0)]
            dt))]
    ;; Warmup
    (dotimes [_ warmup-frames]
      (bench-frame! (body-of-length 2000)))
    (render/invalidate-cache!)
    (let [frames (atom [])
          steps  (long (/ target-chars chunk-chars))]
      (loop [i 0]
        (when (< i steps)
          (let [size (* (inc i) chunk-chars)
                ns   (bench-frame! (body-of-length size))]
            (swap! frames conj {:size size :ns ns})
            (recur (inc i)))))
      (let [all @frames
            bucket-of (fn [s]
                        (cond
                          (< s 5000)   "<5k"
                          (< s 20000)  "5k-20k"
                          (< s 50000)  "20k-50k"
                          (< s 100000) "50k-100k"
                          :else        "100k+"))
            buckets (->> all
                      (group-by #(bucket-of (:size %)))
                      (into (sorted-map-by
                              (fn [a b]
                                (compare
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
            tot (summarize (mapv :ns all))]
        (println)
        (println (format "Layout-driven stream: %d steps × %d chars (bubble-w=%d, inner-h=%d)"
                   steps chunk-chars bubble-w inner-h))
        (println "──────────────────────────────────────────────────────────────────────")
        (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
          "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
        (doseq [[name xs] buckets]
          (let [s (summarize (mapv :ns xs))]
            (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
              name (:n s) (:mean-ms s) (:p50-ms s)
              (:p95-ms s) (:p99-ms s) (:max-ms s))))
        (println)
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
        (let [small (some-> (get buckets "<5k")    (->> (mapv :ns) summarize :mean-ms))
              big   (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (when (and small big (pos? small))
            (printf "LINEARITY: 50k-100k / <5k = %.2fx  (target: ≤ 1.5x)%n"
              (double (/ big small)))))
        (println "METRIC stream_layout_total_ms=" (format "%.2f" (double (:total-ms tot))))
        (println "METRIC stream_layout_p99_ms="   (format "%.2f" (double (:p99-ms tot))))
        (println "METRIC stream_layout_max_ms="   (format "%.2f" (double (:max-ms tot))))
        (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_layout_50k_100k_mean_ms=" (format "%.3f" (double big))))
        {:per-frame all
         :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
         :total     tot}))))

(defn run-progressive-windowed!
  "Like `run-progressive!` but passes `:max-lines viewport-rows` so
   the walker only emits roughly the last viewport-h rows of the
   bubble. Models the realistic TUI workload: the terminal can only
   ever paint `viewport-rows` lines per bubble per frame, so walking
   1500 lines of a 100k buffer to use 30 of them is wasted work.

   The metric `stream_windowed_50k_100k_mean_ms` measures the cost
   of bounded-output streaming. Goal: flat curve across body sizes."
  [{:keys [target-chars chunk-chars bubble-w warmup-frames viewport-rows]
    :or   {target-chars   100000
           chunk-chars    1000
           bubble-w       100
           viewport-rows  200
           warmup-frames  5}}]
  (let [extra-opts {:max-lines viewport-rows}]
    (dotimes [_ warmup-frames]
      (bench-frame! (body-of-length 2000) bubble-w extra-opts))
    (render/invalidate-cache!)
    (let [frames (atom [])
          steps  (long (/ target-chars chunk-chars))]
      (loop [i 0]
        (when (< i steps)
          (let [size (* (inc i) chunk-chars)
                text (body-of-length size)
                [ns lines] (bench-frame! text bubble-w extra-opts)]
            (swap! frames conj {:size size :ns ns :lines lines})
            (recur (inc i)))))
      (let [all @frames
            bucket-of (fn [s]
                        (cond
                          (< s 5000)   "<5k"
                          (< s 20000)  "5k-20k"
                          (< s 50000)  "20k-50k"
                          (< s 100000) "50k-100k"
                          :else        "100k+"))
            buckets (->> all
                      (group-by #(bucket-of (:size %)))
                      (into (sorted-map-by
                              (fn [a b]
                                (compare
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                  (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
            tot (summarize (mapv :ns all))]
        (println)
        (println (format "Windowed stream: %d steps × %d chars (bubble-w=%d, max-lines=%d)"
                   steps chunk-chars bubble-w viewport-rows))
        (println "──────────────────────────────────────────────────────────────────────")
        (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
          "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
        (doseq [[name xs] buckets]
          (let [s (summarize (mapv :ns xs))]
            (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
              name (:n s) (:mean-ms s) (:p50-ms s)
              (:p95-ms s) (:p99-ms s) (:max-ms s))))
        (println)
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
        (let [small (some-> (get buckets "<5k")    (->> (mapv :ns) summarize :mean-ms))
              big   (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (when (and small big (pos? small))
            (printf "LINEARITY: 50k-100k / <5k = %.2fx  (target: ≤ 1.5x)%n"
              (double (/ big small)))))
        (println "METRIC stream_windowed_total_ms="  (long (:total-ms tot)))
        (println "METRIC stream_windowed_p99_ms="    (long (:p99-ms tot)))
        (println "METRIC stream_windowed_max_ms="    (long (:max-ms tot)))
        (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
          (println "METRIC stream_windowed_50k_100k_mean_ms=" (long big)))
        {:per-frame all
         :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
         :total     tot}))))

(defn run-progressive!
  "Simulate progressive streaming: grow body by `chunk-chars` per step
   up to `target-chars`, time each step's render. Reports per-bucket
   stats so you can see the cost curve as the buffer grows.

   This is THE primary metric. We want all buckets to converge to
   the same mean - constant per-frame cost regardless of body size."
  [{:keys [target-chars chunk-chars bubble-w warmup-frames]
    :or   {target-chars   100000
           chunk-chars    1000
           bubble-w       100
           warmup-frames  5}}]
  (dotimes [_ warmup-frames]
    (bench-frame! (body-of-length 2000) bubble-w))
  (render/invalidate-cache!)
  (let [frames (atom [])
        steps  (long (/ target-chars chunk-chars))]
    (loop [i 0]
      (when (< i steps)
        (let [size (* (inc i) chunk-chars)
              text (body-of-length size)
              [ns lines] (bench-frame! text bubble-w)]
          (swap! frames conj {:size size :ns ns :lines lines})
          (recur (inc i)))))
    (let [all @frames
          bucket-of (fn [s]
                      (cond
                        (< s 5000)   "<5k"
                        (< s 20000)  "5k-20k"
                        (< s 50000)  "20k-50k"
                        (< s 100000) "50k-100k"
                        :else        "100k+"))
          buckets (->> all
                    (group-by #(bucket-of (:size %)))
                    (into (sorted-map-by
                            (fn [a b]
                              (compare
                                (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))
          tot (summarize (mapv :ns all))]
      (println)
      (println (format "Progressive stream: %d steps × %d chars (bubble-w=%d)"
                 steps chunk-chars bubble-w))
      (println "─────────────────────────────────────────────────────────────────────")
      (printf "%-10s %6s %10s %10s %10s %10s %10s%n"
        "bucket" "n" "mean ms" "p50 ms" "p95 ms" "p99 ms" "max ms")
      (doseq [[name xs] buckets]
        (let [s (summarize (mapv :ns xs))]
          (printf "%-10s %6d %10.2f %10.2f %10.2f %10.2f %10.2f%n"
            name (:n s) (:mean-ms s) (:p50-ms s)
            (:p95-ms s) (:p99-ms s) (:max-ms s))))
      (println)
      (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
        (:n tot) (:total-ms tot) (:mean-ms tot) (:p99-ms tot) (:max-ms tot))
      ;; Linearity coefficient: mean(100k bucket) / mean(<5k bucket).
      ;; A constant-time renderer would have this ≈ 1.0. Today: ~12.
      (let [small (some-> (get buckets "<5k")    (->> (mapv :ns) summarize :mean-ms))
            big   (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
        (when (and small big (pos? small))
          (printf "LINEARITY: 50k-100k / <5k = %.2fx  (target: ≤ 1.5x)%n"
            (double (/ big small)))))
      (println "METRIC stream_total_ms=" (long (:total-ms tot)))
      (println "METRIC stream_p99_ms="   (long (:p99-ms tot)))
      (println "METRIC stream_max_ms="   (long (:max-ms tot)))
      (when-let [big (some-> (get buckets "50k-100k") (->> (mapv :ns) summarize :mean-ms))]
        (println "METRIC stream_50k_100k_mean_ms=" (long big)))
      {:per-frame all
       :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
       :total     tot})))

(defn- crit-quick
  "Short criterium benchmark: ~3s warmup + ~1s sample. Good enough for
   relative comparisons across optimization passes; not publication-grade."
  [label f]
  (println (format "── %s ──" label))
  (let [r (crit/benchmark* f
            {:warmup-jit-period       (long 3e9)   ; 3 s
             :samples                 6
             :target-execution-time   (long 5e8)   ; 0.5 s per sample
             :overhead                0
             :supress-jvm-option-warnings true})
        mean-ns (* 1e9 (first (:mean r)))]
    (printf "  mean = %.3f ms   (%.2f-%.2f ms)%n"
      (/ mean-ns 1e6)
      (/ (* 1e9 (first (:lower-q r))) 1e6)
      (/ (* 1e9 (first (:upper-q r))) 1e6))
    {:label label :mean-ms (/ mean-ns 1e6)}))

(defn run-criterium!
  "Per-stage criterium attribution at fixed body sizes."
  [{:keys [sizes bubble-w] :or {sizes [5000 50000 100000] bubble-w 100}}]
  (println)
  (println "Per-stage criterium (cache cold each call - matches streaming)")
  (println "═════════════════════════════════════════════════════════════════════")
  (doall
    (for [size sizes]
      (let [text (body-of-length size)]
        (println)
        (println (format "▸ body=%d chars" size))
        (let [parse (crit-quick "text->ir              "
                      #(vis/text->ir text))
              ir   (vis/text->ir text)
              walk (crit-quick "ir->entries (walker)  "
                     #(vec (ir-tui/ir->entries ir (- bubble-w 4) nil)))
              full (crit-quick "format-answer-md-data "
                     #(render/format-answer-markdown-data
                        (vis/text->ir text) bubble-w
                        {:conversation-turn-id (str (System/nanoTime))}))]
          {:size size :parse parse :walk walk :full full})))))
