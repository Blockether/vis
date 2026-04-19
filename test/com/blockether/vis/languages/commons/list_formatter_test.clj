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
                   :total 3 :truncated false})]
        (expect (re-find #"/src — 3 entries" out))
        (expect (not (re-find #"truncated" out)))
        (expect (re-find #"(?m)^  core\.clj$" out))
        (expect (re-find #"(?m)^  sub/$" out))))

    (it "formats a truncated listing"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/big" :entries ["a" "b"] :total 500 :truncated true})]
        (expect (re-find #"/big — 500 entries \(truncated\)" out))))

    (it "singular 'entry' for 1"
      (let [out ((fmt) {:path "/x" :entries ["only.clj"] :total 1 :truncated false})]
        (expect (re-find #"— 1 entry" out))
        (expect (not (re-find #"entries" out)))))

    (it "handles empty directory"
      (let [out (inv/check-formatter-invariants! (fmt)
                  {:path "/empty" :entries [] :total 0 :truncated false})]
        (expect (re-find #"/empty — 0 entries" out))))))
