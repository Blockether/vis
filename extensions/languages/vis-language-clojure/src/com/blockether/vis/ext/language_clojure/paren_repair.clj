(ns com.blockether.vis.ext.language-clojure.paren-repair
  "Pure-Clojure delimiter repair for Clojure source the model hand-wrote.

   Ported from bhauman/clojure-mcp-light (`clojure-mcp-light.delimiter-repair`,
   Apache-2.0): detect a real delimiter error with edamame, then repair via
   parinferish indent-mode — parinfer trusts the INDENTATION to place the missing
   / extra `( [ {`, which matches how the model intended the code to nest. The
   parinfer-rust shell path + stats/json bits from upstream are dropped; this is
   the pure JVM path only.

   `fix-delimiters` is the entry point used by the `clj_paren_repair` tool."
  (:require [edamame.core :as e]
            [parinferish.core :as parinferish]))

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
  (try {:success true :text (parinferish/flatten (parinferish/parse s {:mode :indent})) :error nil}
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
