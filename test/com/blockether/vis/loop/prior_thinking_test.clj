(ns com.blockether.vis.loop.prior-thinking-test
  "Unit tests for the <prior_thinking> block assembly.

   Rule: within a single query, every iteration's `:thinking` is
   visible to the next iteration — not just the most recent one.
   Previously the loop read `*reasoning*` from the SCI sandbox which
   only holds the last thinking, so iter N+2 had no structural view
   of what iter N reasoned about. With the full chain the agent can
   cross-reference its own earlier steps without re-deriving them.

   Cross-query visibility is deliberately NOT part of this — older
   turns' reasoning is reachable via `(var-history '*reasoning*)`
   on demand only. Packing every prior turn into every prompt would
   balloon context for no benefit on most queries."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.core :as sut]))

(defdescribe format-prior-thinking-chain-test
  (describe "format-prior-thinking-chain"
    (it "returns nil when no iterations ran yet"
      (expect (nil? (sut/format-prior-thinking-chain []))))

    (it "returns nil when every iteration's thinking is blank"
      (expect (nil? (sut/format-prior-thinking-chain
                      [{:iteration 0 :thinking ""}
                       {:iteration 1 :thinking nil}
                       {:iteration 2 :thinking "   "}]))))

    (it "formats a single iteration inline"
      (let [s (sut/format-prior-thinking-chain
                [{:iteration 0 :thinking "searching src for delete icon"}])]
        (expect (string? s))
        (expect (str/includes? s "searching src for delete icon"))
        (expect (str/includes? s "[iter 0]"))))

    (it "formats the full chain across multiple iterations, oldest first"
      (let [s (sut/format-prior-thinking-chain
                [{:iteration 0 :thinking "first step"}
                 {:iteration 1 :thinking "second step"}
                 {:iteration 2 :thinking "third step"}])]
        (expect (string? s))
        ;; Oldest first — the index of "first step" must come before "third step".
        (expect (< (.indexOf s "first step") (.indexOf s "third step")))
        ;; All three are present with their iteration tags.
        (expect (str/includes? s "[iter 0]"))
        (expect (str/includes? s "[iter 1]"))
        (expect (str/includes? s "[iter 2]"))))

    (it "skips iterations with blank thinking but keeps the surrounding ones"
      (let [s (sut/format-prior-thinking-chain
                [{:iteration 0 :thinking "kept"}
                 {:iteration 1 :thinking ""}
                 {:iteration 2 :thinking "also kept"}])]
        (expect (str/includes? s "kept"))
        (expect (str/includes? s "also kept"))
        ;; Iter 1 was blank — its tag must not appear.
        (expect (not (str/includes? s "[iter 1]")))))))
