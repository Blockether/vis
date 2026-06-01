(ns com.blockether.vis.ext.foundation-core.editing.hashline-bench-test
  "Microbenchmarks for the hashline layer (`patch/line-hash`,
   `lines->hashes`, `render-hashline-block`) — the per-line hot path
   that runs on every `v/cat` render and every `v/patch` resolve.

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
   [com.blockether.vis.ext.foundation-core.editing.patch :as patch]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- reference-line-hash
  "The ORIGINAL implementation: java.util.Formatter `%06x` over the
   trimmed `String/hashCode`. The optimized `patch/line-hash` MUST stay
   byte-identical to this — hashes are the edit address."
  [s]
  (format "%06x" (bit-and (.hashCode (str/trim (str s))) 0xffffff)))

(defn- file->tuples
  "Load `path` as `[[line-number text] ...]` tuples like `v/cat` produces."
  [path]
  (->> (str/split-lines (slurp path))
    (map-indexed (fn [i s] [(inc i) s]))
    vec))

(defn- ms-per-call
  "Crude wall-clock ms/call over `n` iters after a warmup. Good enough to
   catch a regression; use `run-bench!` (criterium) for rigorous numbers."
  [f n]
  (dotimes [_ 5] (f))
  (let [t0 (System/nanoTime)]
    (dotimes [_ n] (f))
    (/ (- (System/nanoTime) t0) 1e6 (double n))))

(defn run-bench!
  "Print criterium quick-benchmark stats for the hashline hot path.
   `path` defaults to this very source's editing/core.clj (a ~3k-line
   real file). Requires criterium (present under the `:test` alias)."
  ([] (run-bench!
        "extensions/common/vis-foundation-core/src/com/blockether/vis/ext/foundation_core/editing/core.clj"))
  ([path]
   (require 'criterium.core)
   (let [quick-bench (resolve 'criterium.core/quick-benchmark)
         report      (resolve 'criterium.core/report-result)
         tuples      (file->tuples path)
         line        "  (defn foo [x] (bar (baz x)))  "]
     (println "── hashline bench —" path "(" (count tuples) "lines) ──")
     (println "\nline-hash (single line):")
     (report @(quick-bench (patch/line-hash line) {}))
     (println "\nlines->hashes (whole file):")
     (report @(quick-bench (patch/lines->hashes tuples) {}))
     (println "\nrender-hashline-block (whole file):")
     (report @(quick-bench (patch/render-hashline-block tuples) {}))
     :done)))

(defdescribe hashline-bench-guard-test
  ;; The ONLY auto-run case here: cheap correctness + a soft perf floor.
  (it "optimized line-hash stays byte-identical to the format reference"
    (doseq [s ["" "   " "\t tab \t" "(defn f [])" "éé unicode ✓"
               (apply str (repeat 400 \z)) "  leading+trailing  "]]
      (expect (= (reference-line-hash s) (patch/line-hash s)))))

  (it "line-hash is comfortably under 200ns/call (regression tripwire)"
    ;; Optimized path benches ~55ns; 200ns leaves slack for slow CI boxes
    ;; while still catching a return to the Formatter path (~85ns+) plus
    ;; any accidental O(n^2) blow-up.
    (let [t (ms-per-call #(patch/line-hash "  (defn foo [x] (bar x))  ") 200000)]
      (expect (< (* t 1e6) 200.0)))))
