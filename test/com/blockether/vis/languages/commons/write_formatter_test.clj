(ns com.blockether.vis.languages.commons.write-formatter-test
  "Per-tool formatter tests for write-file. Asserts the invariants
   (arity/purity/isolation) AND the expected shape on realistic return values."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.languages.commons.write :as write]
    [com.blockether.vis.loop.tool-formatter-invariants-test :as inv]))

(defn- fmt [] (:format-result write/tool-def))

(defdescribe write-file-formatter
  (describe "format-result produces a compact, deterministic summary"
    (it "handles nil (validator probe)"
      (expect (= "" (inv/check-formatter-invariants! (fmt) nil))))

    (it "formats a newly-created file with preview"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/tmp/new.txt" :lines 2 :created? true
                   :preview ["hello" "world"]})]
        (expect (re-find #"wrote /tmp/new\.txt — 2 lines \(created\)" out))
        (expect (re-find #"(?m)^hello$" out))
        (expect (re-find #"(?m)^world$" out))))

    (it "formats an overwrite with diff"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/tmp/x.txt" :lines 3 :old-lines 2
                   :diff ["@@ -1,2 +1,3 @@" " foo" "+bar" " baz"]})]
        (expect (re-find #"wrote /tmp/x\.txt — 3 lines \(was 2\)" out))
        (expect (re-find #"\+bar" out))))

    (it "truncates long diffs"
      (let [long-diff (mapv #(str "+line " %) (range 50))
            out (inv/check-formatter-invariants! (fmt)
                  {:path "/a" :lines 1 :old-lines 0 :diff long-diff})]
        (expect (re-find #"\.\.\. 30 more diff lines" out))))

    (it "pluralizes correctly for 1 line"
      (let [out ((fmt) {:path "/a" :lines 1 :created? true})]
        (expect (re-find #"1 line \(" out))
        (expect (not (re-find #"1 lines" out)))))))
