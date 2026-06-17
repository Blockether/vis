(ns com.blockether.vis.internal.foundation.editing.hashline-bench-test
  "Microbenchmarks for the hashline layer (`patch/line-hash`,
   `lines->anchors`) — the per-line hot path that runs on every `cat`
   read and every `patch` resolve.

   NOT auto-run: criterium benches live behind `run-bench!` so the
   default lazytest suite stays fast (see deps.edn `:test` note). One
   tiny correctness `it` guards the optimized `line-hash` against the
   original `format`-based reference so a future tweak can't silently
   drift the hash values.

   Run the benches from a REPL:
     (require '[...editing.hashline-bench-test :as b] :reload)
     (b/run-bench!)
   or against a specific file:
     (b/run-bench! \"path/to/file.clj\")"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- reference-line-hash
  "Independent reference: java.util.Formatter over the trimmed
   `String/hashCode`, at the current `patch/hash-width`. The optimized
   `patch/line-hash` (toHexString + pad) MUST stay byte-identical to
   this — hashes are the edit address, so a formatting/masking drift
   would silently break every anchor."
  [s]
  (let [w    (long patch/hash-width)
        mask (long (dec (bit-shift-left 1 (* 4 w))))]
    (format (str "%0" w "x") (bit-and (.hashCode (str/trim (str s))) mask))))

(defn- file->tuples
  "Load `path` as `[[line-number text] ...]` tuples like `cat` produces."
  [path]
  (->> (str/split-lines (slurp path))
    (map-indexed (fn [i s] [(inc i) s]))
    vec))

(defn- min-ns-per-call
  "BEST (minimum) wall-clock ns/call across `batches` runs of `n` iters each,
   after a JIT warmup. The MIN reflects the cost when the OS scheduler gave a
   clean slice, so it's far more stable under machine load than a single timed
   run (whose mean inflates arbitrarily when other processes compete)."
  [f n batches]
  (dotimes [_ 20000] (f))
  (reduce min
    (for [_ (range batches)]
      (let [t0 (System/nanoTime)]
        (dotimes [_ n] (f))
        (/ (- (System/nanoTime) t0) (double n))))))

(defn run-bench!
  "Print criterium quick-benchmark stats for the hashline hot path.
   `path` defaults to this very source's editing/core.clj (a ~3k-line
   real file). Requires criterium (present under the `:test` alias)."
  ([] (run-bench!
        "src/com/blockether/vis/internal/foundation/editing/core.clj"))
  ([path]
   (require 'criterium.core)
   (let [quick-bench (resolve 'criterium.core/quick-benchmark)
         report      (resolve 'criterium.core/report-result)
         tuples      (file->tuples path)
         line        "  (defn foo [x] (bar (baz x)))  "]
     (println "── hashline bench —" path "(" (count tuples) "lines) ──")
     (println "\nline-hash (single line):")
     (report @(quick-bench (patch/line-hash line) {}))
     (println "\nlines->anchors (whole file):")
     (report @(quick-bench (patch/lines->anchors tuples) {}))
     :done)))

(defdescribe hashline-bench-guard-test
  ;; The ONLY auto-run case here: cheap correctness + a soft perf floor.
  (it "optimized line-hash stays byte-identical to the format reference"
    (doseq [s ["" "   " "\t tab \t" "(defn f [])" "éé unicode ✓"
               (apply str (repeat 400 \z)) "  leading+trailing  "]]
      (expect (= (reference-line-hash s) (patch/line-hash s)))))

  (it "line-hash stays faster than the Formatter reference it replaced (load-robust)"
    ;; A fixed absolute ns threshold flakes under machine load (a busy box can
    ;; read 600ns for a 55ns call). Instead compare the optimized path to the
    ;; Formatter REFERENCE measured in the SAME conditions — load hits both
    ;; equally, so it cancels. The optimized toHexString+pad path (~55ns) must
    ;; stay comfortably faster than the Formatter path (~85ns) it replaced; a
    ;; regression back to Formatter (or an O(n^2) blow-up) pushes the ratio
    ;; to/over 1.0. MIN-of-batches keeps the measurement steady under load.
    (let [line "  (defn foo [x] (bar x))  "
          opt  (min-ns-per-call #(patch/line-hash line)       50000 5)
          ref  (min-ns-per-call #(reference-line-hash line)   50000 5)]
      (expect (< opt (* 0.85 ref))))))
