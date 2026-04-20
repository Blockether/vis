(ns com.blockether.vis.languages.commons.list-formatter-test
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.languages.commons.list :as list-tool]
    [com.blockether.vis.loop.tool-formatter-invariants-test :as inv]))

(defn- fmt [] (:format-result list-tool/tool-def))

(defdescribe list-dir-formatter
  (describe "format-result produces a compact directory listing"
    (it "handles nil"
      (expect (= "" (inv/check-formatter-invariants! (fmt) nil))))

    (it "formats a non-truncated listing"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/src" :entries ["core.clj" "util.clj" "sub/"]
                   :total 3 :offset 0 :truncated? false :next-offset nil})]
        (expect (re-find #"/src — showing 3/3 entries" out))
        (expect (not (re-find #"more, retry" out)))
        (expect (re-find #"(?m)^  core\.clj$" out))
        (expect (re-find #"(?m)^  sub/$" out))))

    (it "formats a truncated listing with next-offset hint"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/big" :entries ["a" "b"] :total 500
                   :offset 0 :truncated? true :next-offset 2})]
        (expect (re-find #"/big — showing 2/500 entries — 498 more, retry with `:offset 2`" out))))

    (it "shows offset in header when paginating"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/big" :entries ["c" "d"] :total 500
                   :offset 2 :truncated? true :next-offset 4})]
        (expect (re-find #"/big — showing 2/500 entries \(offset 2\) — 496 more, retry with `:offset 4`" out))))

    (it "singular 'entry' for total of 1"
      (let [out ((fmt) {:path "/x" :entries ["only.clj"] :total 1
                        :offset 0 :truncated? false :next-offset nil})]
        (expect (re-find #"showing 1/1 entry" out))
        (expect (not (re-find #"entries" out)))))

    (it "handles empty directory"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/empty" :entries [] :total 0
                   :offset 0 :truncated? false :next-offset nil})]
        (expect (re-find #"/empty — showing 0/0 entries" out))))))
