(ns com.blockether.vis.bench-stream
  "Bench harness for the live-streaming TUI bubble pipeline.

   Drives `format-answer-markdown-data` with a fresh assistant body
   (re-parsed from a growing markdown string) at each `chunk` step, so
   identity-keyed caches in `render.clj` always miss - exactly the
   path that fires on every streamed token in production.

   Run from nREPL:

     (require '[com.blockether.vis.bench-stream :as b] :reload)
     (b/run-progressive! {})         ; default 150k cap, 500-char chunks
     (b/run-sweep! {})               ; one-shot at fixed sizes

   Results print as a table; also returned as data for further analysis."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.render :as render]))

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
   `format-answer-markdown-data`, return [ns lines-count]."
  [^String text ^long bubble-w]
  (let [ir (vis/text->ir text)
        ;; force realization with a unique opts key so cache always misses
        opts {:conversation-turn-id (str "bench-" (System/nanoTime))}
        t0 (System/nanoTime)
        result (render/format-answer-markdown-data ir bubble-w opts)
        dt (- (System/nanoTime) t0)]
    [dt (count (:lines result))]))

(defn run-progressive!
  "Simulate progressive streaming: grow body by `chunk-chars` per step
   up to `target-chars`, time each step's render. Reports per-bucket
   stats so you can see the cost curve as the buffer grows."
  [{:keys [target-chars chunk-chars bubble-w warmup-frames]
    :or   {target-chars 150000
           chunk-chars  500
           bubble-w     100
           warmup-frames 5}}]
  ;; warmup
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
          buckets (->> all
                    (group-by #(let [s (:size %)]
                                 (cond
                                   (< s 5000)   "<5k"
                                   (< s 20000)  "5k-20k"
                                   (< s 50000)  "20k-50k"
                                   (< s 100000) "50k-100k"
                                   :else        "100k+")))
                    (into (sorted-map-by
                            (fn [a b]
                              (compare
                                (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] a)
                                (.indexOf ["<5k" "5k-20k" "20k-50k" "50k-100k" "100k+"] b))))))]
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
            (:p95-ms s) (:p95-ms s) (:max-ms s))))
      (println)
      (let [s (summarize (mapv :ns all))]
        (printf "TOTAL: n=%d wall=%.0f ms  mean=%.2f  p99=%.2f  max=%.2f%n"
          (:n s) (:total-ms s) (:mean-ms s) (:p99-ms s) (:max-ms s)))
      (println "METRIC stream_total_ms=" (long (:total-ms (summarize (mapv :ns all)))))
      (println "METRIC stream_p99_ms="   (long (:p99-ms   (summarize (mapv :ns all)))))
      (println "METRIC stream_max_ms="   (long (:max-ms   (summarize (mapv :ns all)))))
      {:per-frame all
       :buckets   (into {} (map (fn [[k xs]] [k (summarize (mapv :ns xs))]) buckets))
       :total     (summarize (mapv :ns all))})))

(defn run-sweep!
  "One-shot: render each fixed body size N times, summarize."
  [{:keys [sizes reps bubble-w]
    :or   {sizes    [1000 5000 20000 50000 100000 150000]
           reps     20
           bubble-w 100}}]
  (let [results
        (into (sorted-map)
          (for [size sizes]
            (let [text (body-of-length size)
                  ;; warmup
                  _ (dotimes [_ 3] (bench-frame! text bubble-w))
                  ns-samples
                  (vec (for [_ (range reps)]
                         (let [[ns _] (bench-frame! text bubble-w)] ns)))]
              (render/invalidate-cache!)
              [size (summarize ns-samples)])))]
    (println)
    (println "Fixed-size sweep (each row is a fresh-IR render, cache cold)")
    (println "─────────────────────────────────────────────────────────────────────")
    (printf "%10s %6s %10s %10s %10s %10s%n"
      "size" "reps" "mean ms" "p50 ms" "p95 ms" "max ms")
    (doseq [[size s] results]
      (printf "%10d %6d %10.2f %10.2f %10.2f %10.2f%n"
        size (:n s) (:mean-ms s) (:p50-ms s) (:p95-ms s) (:max-ms s)))
    results))
