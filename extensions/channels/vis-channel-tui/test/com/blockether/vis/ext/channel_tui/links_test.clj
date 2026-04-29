(ns com.blockether.vis.ext.channel-tui.links-test
  (:require
   [com.blockether.vis.ext.channel-tui.links :as links]
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe parse-md-refs-test
  (it "no refs in plain prose"
    (expect (= [] (links/parse-md-refs "no links here"))))

  (it "extracts a single [text](url) as :url"
    (let [out (links/parse-md-refs "see [docs](https://example.com)")]
      (expect (= 1 (count out)))
      (expect (= :url    (:kind (first out))))
      (expect (= "docs"  (:text (first out))))
      (expect (= "https://example.com" (:url (first out))))
      (expect (= :https  (:scheme (first out))))
      (expect (true? (:enabled? (first out))))))

  (it "extracts ![alt](url) as :image"
    (let [out (links/parse-md-refs "![diagram](https://x.example.com/d.png)")]
      (expect (= [:image] (mapv :kind out)))
      (expect (= "diagram" (:text (first out))))
      (expect (true? (:enabled? (first out))))))

  (it "classifies (md/file-link path) shape as :file"
    (let [out (links/parse-md-refs "[deps.edn](deps.edn)")]
      (expect (= [:file] (mapv :kind out)))
      (expect (= :rel    (:scheme (first out))))
      (expect (true?     (:enabled? (first out))))
      (expect (nil?      (:line (first out))))))

  (it "classifies (md/file-link path line) shape as :file with :line"
    (let [out (links/parse-md-refs "[deps.edn:42](deps.edn#L42)")]
      (expect (= [:file] (mapv :kind out)))
      (expect (= 42      (:line (first out))))))

  (it "interleaved url + image + file in one string"
    (let [out (links/parse-md-refs
                (str "[a](https://a) and ![b](https://b/img.png) "
                  "and [deps.edn](deps.edn)"))]
      (expect (= [:url :image :file] (mapv :kind out)))))

  (it "anchor-only links are :rejected (nothing for opener to do)"
    (let [out (links/parse-md-refs "see [later](#section)")]
      (expect (= 1 (count out)))
      (expect (= :rejected (:scheme (first out))))
      (expect (false? (:enabled? (first out))))))

  (it "javascript: / data: schemes flagged disabled"
    (let [out (links/parse-md-refs "[click](javascript:alert(1))")]
      (expect (= 1 (count out)))
      (expect (= :rejected (:scheme (first out))))
      (expect (false? (:enabled? (first out))))))

  (it "..-traversal disables a relative-path entry"
    (let [out (links/parse-md-refs "[evil](../../../etc/passwd)")]
      (expect (= 1 (count out)))
      (expect (= :rel (:scheme (first out))))
      (expect (false? (:enabled? (first out))))))

  (it "links with title attributes still parse"
    (let [out (links/parse-md-refs
                "[docs](https://example.com \"Project docs\")")]
      (expect (= 1 (count out)))
      (expect (= "docs" (:text (first out))))
      (expect (= "https://example.com" (:url (first out))))))

  (it "preserves order across many refs"
    (let [out (links/parse-md-refs
                (str "[1](https://a)\n"
                  "[2](https://b)\n"
                  "[3](https://c)"))]
      (expect (= ["1" "2" "3"] (mapv :text out)))))

  (it "handles nil / empty input"
    (expect (= [] (links/parse-md-refs nil)))
    (expect (= [] (links/parse-md-refs "")))))
