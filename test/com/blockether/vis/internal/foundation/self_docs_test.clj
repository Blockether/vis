(ns com.blockether.vis.internal.foundation.self-docs-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.env-python :as boundary]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.self-docs :as self-docs]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private vis-docs-tool @#'self-docs/vis-docs-tool)

(defn- result-of
  "The tool envelope's `:result` payload viewed through the strings-only
   Python boundary. Throws on any stray keyword key/value."
  [envelope]
  (boundary/boundary-view (:result envelope)))

(defdescribe vis-docs-listing-test
             (it "lists every manifest page as a lean slug/title/section/blurb entry"
                 (let [result (vis-docs-tool)]
                   (expect (extension/envelope-success? result))
                   (let [pages (get (result-of result) "pages")
                         slugs (set (map #(get % "slug") pages))]

                     ;; the pages the prompt fragment advertises must exist
                     (expect (contains? slugs "index"))
                     (expect (contains? slugs "extending"))
                     (expect (contains? slugs "configuration"))
                     ;; lean listing: never the content
                     (expect (every? #(and (get % "slug") (get % "title")) pages))
                     ;; every declared page carries a one-line blurb of what it covers
                     (expect (some #(get % "blurb") pages))
                     ;; whenever a blurb is present it is a non-blank string, never a keyword
                     (expect (every? #(let [b (get % "blurb")]

                                        (or (nil? b) (and (string? b) (not (str/blank? b)))))
                                     pages))
                     ;; the pages the prompt fragment advertises all carry a blurb
                     (let [by-slug (into {} (map (juxt #(get % "slug") identity)) pages)]
                       (doseq [slug ["index" "extending" "configuration"]]
                         (expect (string? (get (by-slug slug) "blurb")))))
                     (expect (not-any? #(contains? % "content") pages))
                     (expect (not-any? #(contains? % "md") pages))))))

(defdescribe vis-docs-blurb-test
             (it "surfaces the manifest blurb verbatim for each declared page"
                 (let [manifest
                       (into {}
                             (for [{:keys [slug blurb]}
                                   (:pages (docs/collect))

                                   :when blurb]

                               [slug blurb]))

                       listed
                       (into {}
                             (map (juxt #(get % "slug") #(get % "blurb")))
                             (get (result-of (vis-docs-tool)) "pages"))]

                   ;; the manifest declares blurbs to surface
                   (expect (seq manifest))
                   ;; and every one shows up unchanged in the lean listing
                   (doseq [[slug blurb] manifest]
                     (expect (= blurb (get listed slug))))))
             (it "prompt fragment teaches that the listing carries a blurb"
                 (expect (str/includes? self-docs/prompt "blurb"))))

(defdescribe vis-docs-fetch-test
             (it "returns a page's full markdown by slug"
                 (let [result (vis-docs-tool "extending")]
                   (expect (extension/envelope-success? result))
                   (let [{slug "slug" title "title" content "content"} (result-of result)]
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
                     (expect (not (str/blank? (get (result-of result) "content"))))))))

(defdescribe vis-docs-forgiving-slug-test
             (it "resolves the same page across map/kwargs, trailing .md, whitespace, and case"
                 (let [canonical (result-of (vis-docs-tool "extending"))]
                   (expect (extension/envelope-success? (vis-docs-tool "extending")))
                   ;; every one of these shapes must land on the SAME page
                   (doseq [variant ["extending.md" "  extending  " "Extending" "EXTENDING.MD"
                                    {"slug" "extending.md"} {:slug "Extending"}]]
                     (let [result (vis-docs-tool variant)]
                       (expect (extension/envelope-success? result))
                       (expect (= (get canonical "slug") (get (result-of result) "slug"))))))))

(defdescribe vis-docs-blank-slug-test
             (it "treats a blank/absent slug (\"\", {}, whitespace) as the no-arg page list"
                 (let [no-arg (result-of (vis-docs-tool))]
                   (expect (extension/envelope-success? (vis-docs-tool)))
                   ;; every blank shape must return the identical page listing
                   (doseq [variant ["" "   " {} {"slug" ""} {"slug" "  "} {:slug nil}]]
                     (let [result (vis-docs-tool variant)]
                       (expect (extension/envelope-success? result))
                       (expect (= no-arg (result-of result))))))))

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
