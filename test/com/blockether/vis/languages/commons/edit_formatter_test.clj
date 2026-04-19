(ns com.blockether.vis.languages.commons.edit-formatter-test
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.languages.commons.edit :as edit]
    [com.blockether.vis.loop.tool-formatter-invariants-test :as inv]))

(defn- fmt [] (:format-result edit/tool-def))

(defdescribe edit-file-formatter
  (describe "format-result produces a compact per-file summary"
    (it "handles nil"
      (expect (= "" (inv/check-formatter-invariants! (fmt) nil))))

    (it "formats single-file update"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:files [{:action :update :path "/a.txt" :hunks 2
                            :diff ["@@ -1 +1 @@" "-old" "+new"]}]
                   :total-files 1 :total-hunks 2})]
        (expect (re-find #"edited 1 file, 2 hunks" out))
        (expect (re-find #"update /a\.txt \[2 hunks\]" out))
        (expect (re-find #"\+new" out))))

    (it "formats multi-file with mixed actions"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:files [{:action :add :path "/new.txt"}
                           {:action :delete :path "/old.txt"}
                           {:action :update :path "/mod.txt" :hunks 1
                            :diff ["@@" "-x" "+y"]}]
                   :total-files 3 :total-hunks 1})]
        (expect (re-find #"edited 3 files, 1 hunk" out))
        (expect (re-find #"add /new\.txt" out))
        (expect (re-find #"delete /old\.txt" out))
        (expect (re-find #"update /mod\.txt \[1 hunk\]" out))))

    (it "truncates very long diffs"
      (let [long-diff (mapv #(str "+line " %) (range 20))
            out (inv/check-formatter-invariants! (fmt)
                  {:files [{:action :update :path "/a" :hunks 1 :diff long-diff}]
                   :total-files 1 :total-hunks 1})]
        (expect (re-find #"\.\.\. 15 more diff lines" out))))))
