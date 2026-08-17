(ns com.blockether.vis.ext.language-clojure.paren-repair
  "Delimiter repair for Clojure source the model hand-wrote.

   Ported from bhauman/clojure-mcp-light (`clojure-mcp-light.delimiter-repair`,
   Apache-2.0): repair via parinfer indent-mode, which trusts the INDENTATION to
   place the missing / extra `( [ {` and so matches how the model intended the
   code to nest. The parinfer-rust shell path + stats/json bits from upstream are
   dropped; this is the pure JVM path only, over `com.blockether/parinferish` —
   Blockether's linear-time rewrite of parinferish 0.8.0.

   Two readers, two questions, and neither answers the other's. parinferish says
   whether the DELIMITERS balance, so the gate is the same reader that performs
   the repair and a whole file costs one linear scan. edamame says whether text
   READS as Clojure, which balanced delimiters do not promise: source cut
   mid-token comes back closed as `(:)` — balanced, and not a keyword.

   `fix-delimiters` is the entry point, and it repairs WHOLE Clojure source: `format`
   runs it before cljfmt, and the pack publishes it as the editors' `:balance-fn`, which
   the foundation applies to the whole file an edit would write and keeps only when the
   repair stays on that edit's own lines. Handing it a partial form instead balances the
   fragment into a complete one that means something else."
  (:require [com.blockether.parinferish :as parinferish]
            [edamame.core :as e]))

(defn delimiter-error?
  "True when the delimiters of `s` do not balance — parinferish reading it and
   repairing nothing: `EOF while reading`, `Unmatched delimiter`, `Unbalanced
   quote`, `Backslash at end of line`. Code it cannot READ is not this question;
   nil and \"\" have nothing to balance."
  [s]
  (some? (parinferish/error (parinferish/parse (str s)))))

(defn reads-clean?
  "True when `s` reads as Clojure end to end — every reader conditional feature,
   unknown tags accepted, any read failure a false. This is what a repair has to
   earn before it may be written."
  [s]
  (try (e/parse-string-all s
                           {:all true
                            :features #{:bb :clj :cljs :cljr :default}
                            :read-cond :allow
                            :readers (fn [_tag]
                                       identity)
                            :auto-resolve name})
       true
       (catch Exception _ false)))

(defn fix-delimiters
  "Repair the delimiters of Clojure source `s`:
     - they balance → `s` unchanged
     - repairable   → the repaired string, which reads clean
     - unrepairable → nil
   Pure; never throws on a normal failure."
  [s]
  (if (delimiter-error? s)
    (let [repaired (parinferish/repair s {:mode :indent})]
      (when (reads-clean? repaired) repaired))
    s))
