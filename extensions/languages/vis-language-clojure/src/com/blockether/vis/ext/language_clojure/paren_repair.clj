(ns com.blockether.vis.ext.language-clojure.paren-repair
  "Pure-Clojure delimiter repair for Clojure source the model hand-wrote.

   Ported from bhauman/clojure-mcp-light (`clojure-mcp-light.delimiter-repair`,
   Apache-2.0): detect a real delimiter error with edamame, then repair via
   parinfer indent-mode — parinfer trusts the INDENTATION to place the missing
   / extra `( [ {`, which matches how the model intended the code to nest. The
   parinfer-rust shell path + stats/json bits from upstream are dropped; this is
   the pure JVM path only, over `com.blockether/parinferish` — Blockether's
   linear-time rewrite of parinferish 0.8.0, which re-scanned the rest of the file
   for every token and cost seconds on a ten-thousand-line namespace.

   `fix-delimiters` is the entry point, and it repairs WHOLE Clojure source: `format`
   runs it before cljfmt, and the pack publishes it as the editors' `:balance-fn`, which
   the foundation applies to the whole file an edit would write and keeps only when the
   repair stays on that edit's own lines. Handing it a partial form instead balances the
   fragment into a complete one that means something else."
  (:require [com.blockether.parinferish :as parinferish]
            [edamame.core :as e]))

(defn delimiter-error?
  "True when `s` fails to read specifically because of an unbalanced delimiter
   (edamame reports `:edamame/opened-delimiter`). A non-delimiter read failure
   still returns true — running parinfer is benign and may fix a hidden
   imbalance — while clean source returns false."
  [s]
  (try (e/parse-string-all s
                           {:all true
                            :features #{:bb :clj :cljs :cljr :default}
                            :read-cond :allow
                            :readers (fn [_tag]
                                       (fn [data]
                                         data))
                            :auto-resolve name})
       false
       (catch clojure.lang.ExceptionInfo ex
         (let [data (ex-data ex)]
           (and (= :edamame/error (:type data)) (contains? data :edamame/opened-delimiter))))
       (catch Exception _ true)))

(defn parinferish-repair
  "Repair `s` with parinferish indent-mode. Returns `{:success bool :text S?
   :error msg?}`."
  [s]
  (try {:success true :text (parinferish/repair s {:mode :indent}) :error nil}
       (catch Exception e {:success false :error (.getMessage e)})))

(defn fix-delimiters
  "Repair the delimiters of Clojure source `s`:
     - no delimiter error → `s` unchanged
     - repairable          → the repaired string (which now parses clean)
     - unrepairable        → nil
   Pure; never throws on a normal failure."
  [s]
  (if (delimiter-error? s)
    (let [{:keys [text success]} (parinferish-repair s)]
      (when (and success text (not (delimiter-error? text))) text))
    s))
