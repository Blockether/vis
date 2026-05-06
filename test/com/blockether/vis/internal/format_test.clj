(ns com.blockether.vis.internal.format-test
  "Smoke coverage for `internal/format`.

   Why it exists: AGENTS.md hard rule — every production namespace ships a
   corresponding `_test.clj`. The concurrency regression itself lives in
   `env_test.clj` because the user-visible failure crossed the sandbox/render
   seam; this file pins the public formatter outputs and the parse/fallback
   contract of `format-clojure`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as format]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe format-test
  (it "pretty-prints compact Clojure and falls back on malformed input"
    (expect (= "(defn f\n  [x]\n  (let [a 1]\n    (+ a x)))"
              (format/format-clojure "(defn f[x](let[a 1](+ a x)))" 20)))
    (expect (= "(defn broken [x"
              (format/format-clojure "(defn broken [x" 20))))

  (it "formats whole source strings instead of only the first parsed expression"
    (let [src ";; file header\n{:paths [\"src\"]}\n{:aliases {:test {}}}"
          plain (format/format-clojure src 80)
          ansi  (format/format-clojure-ansi src 80)]
      (expect (str/includes? plain ";; file header"))
      (expect (str/includes? plain ":aliases"))
      (expect (str/includes? ansi ";; file header"))
      (expect (str/includes? ansi ":aliases"))))

  (it "formats turn-summary atoms through the public helpers"
    (expect (= "1 iter" (format/format-iterations 1)))
    (expect (= "3 iters" (format/format-iterations 3)))
    (expect (= "↑10 (cached 7) ↓5"
              (format/format-tokens {:input 10 :output 5
                                     :cached 7})))
    (expect (= "↑10 ↓5"
              (format/format-tokens {:input 10 :output 5
                                     :cached 0})))
    (expect (= "~$0.123456" (format/format-cost 0.1234564)))
    (expect (= "input ~$0.008000, input cached ~$0.001000, output ~$0.003000, total ~$0.012000"
              (format/format-cost {:total-cost 0.012
                                   :input-uncached-cost 0.008
                                   :input-cached-cost 0.001
                                   :output-cost 0.003})))
    (expect (= "1.5s" (format/format-duration 1500))))

  (it "composes the canonical meta line"
    (let [line (format/format-meta-line
                 {:iteration-count 2
                  :duration-ms 1200
                  :tokens {:input 100 :output 20 :cached 60}
                  :cost {:total-cost 0.00042
                         :input-uncached-cost 0.00020
                         :input-cached-cost 0.00005
                         :output-cost 0.00017
                         :provider :openai
                         :model "gpt-4o"}})]
      (expect (str/includes? line "openai/gpt-4o"))
      (expect (str/includes? line "2 iters"))
      (expect (str/includes? line "↑100 (cached 60) ↓20"))
      (expect (str/includes? line "input ~$0.000200"))
      (expect (str/includes? line "input cached ~$0.000050"))
      (expect (str/includes? line "output ~$0.000170"))
      (expect (str/includes? line "total ~$0.000420"))
      (expect (str/includes? line "1.2s")))))
