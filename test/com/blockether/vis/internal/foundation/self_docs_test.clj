(ns com.blockether.vis.internal.foundation.self-docs-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.self-docs :as self-docs]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private vis-docs-tool @#'self-docs/vis-docs-tool)

(defdescribe vis-docs-listing-test
             (it "lists every manifest page as a lean slug/title/section entry"
                 (let [result (vis-docs-tool)]
                   (expect (extension/envelope-success? result))
                   (let [pages (:pages (:result result))
                         slugs (set (map :slug pages))]

                     ;; the pages the prompt fragment advertises must exist
                     (expect (contains? slugs "index"))
                     (expect (contains? slugs "extending"))
                     (expect (contains? slugs "configuration"))
                     ;; lean listing: never the content
                     (expect (every? #(and (:slug %) (:title %)) pages))
                     (expect (not-any? :content pages))
                     (expect (not-any? :md pages))))))

(defdescribe vis-docs-fetch-test
             (it "returns a page's full markdown by slug"
                 (let [result (vis-docs-tool "extending")]
                   (expect (extension/envelope-success? result))
                   (let [{:keys [slug title content]} (:result result)]
                     (expect (= "extending" slug))
                     (expect (string? title))
                     ;; the extension-authoring guide teaches the real API
                     (expect (str/includes? content "vis/extension"))
                     (expect (str/includes? content "register-extension!"))
                     (expect (str/includes? content "META-INF/vis-extension/vis.edn")))))
             (it "every manifest page resolves to non-blank markdown"
                 (doseq [{:keys [slug]} (:pages (docs/collect))]
                   (let [result (vis-docs-tool slug)]
                     (expect (extension/envelope-success? result))
                     (expect (not (str/blank? (:content (:result result)))))))))

(defdescribe vis-docs-unknown-slug-test
             (it "fails with a message + hint listing valid slugs"
                 (let [result (vis-docs-tool "no-such-page")]
                   (expect (extension/envelope-failure? result))
                   (let [{:keys [message hint]} (:error result)]
                     (expect (str/includes? message "no-such-page"))
                     (expect (str/includes? hint "extending"))))))

(defdescribe vis-docs-symbol-test
             (it "registers as an observation tool named vis-docs"
                 (expect (= 'vis-docs (:ext.symbol/symbol self-docs/vis-docs-symbol)))
                 (expect (= :observation (:ext.symbol/tag self-docs/vis-docs-symbol))))
             (it "prompt fragment only advertises slugs that exist"
                 (let [slugs (set (map :slug (:pages (docs/collect))))]
                   (doseq [advertised ["extending" "python-extensions" "configuration" "index"]]
                     (expect (contains? slugs advertised))
                     (expect (str/includes? self-docs/prompt advertised))))))
