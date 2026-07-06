(ns com.blockether.vis.internal.docs-test
  "Docs renderer: cross-page markdown links must resolve in BOTH output
   modes (live `/docs/<slug>`, static `<slug>.html`), and the live
   handler tolerates literal `<slug>.md` deep links with a redirect."
  (:require [com.blockether.vis.internal.docs :as docs]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private rewrite-md-links @#'docs/rewrite-md-links)

(defdescribe rewrite-md-links-test
             (it "live mode: relative page.md -> /docs/page, fragment preserved"
                 (expect (= "<a href=\"/docs/skills\">x</a>"
                            (rewrite-md-links "<a href=\"skills.md\">x</a>" :live)))
                 (expect (= "<a href=\"/docs/configuration#router\">x</a>"
                            (rewrite-md-links "<a href=\"configuration.md#router\">x</a>" :live))))
             (it "static mode: relative page.md -> page.html"
                 (expect (= "<a href=\"skills.html\">x</a>"
                            (rewrite-md-links "<a href=\"skills.md\">x</a>" :static))))
             (it "absolute URLs and absolute paths pass through untouched"
                 (let [ext
                       "<a href=\"https://example.com/readme.md\">x</a>"

                       abs
                       "<a href=\"/raw/readme.md\">x</a>"]

                   (expect (= ext (rewrite-md-links ext :live)))
                   (expect (= abs (rewrite-md-links abs :live))))))

(defdescribe
  rendered-pages-test
  (it "no live page body carries a dangling .md href (regression: every cross-link resolves)"
      (let [{:keys [pages] :as site} (docs/collect)]
        (expect (seq pages))
        (doseq [page pages]
          (let [html (docs/page-html site page :live)]
            (expect (not (re-find #"href=\"[^\"/:][^\":]*\.md[\"#]" html))
                    (str "dangling .md link in live page " (:slug page))))))))

(defdescribe handle-md-redirect-test
             (it "GET /docs/<slug>.md permanent-redirects to /docs/<slug>"
                 (let [resp (docs/handle {:uri "/docs/skills.md" :headers {}})]
                   (expect (= 301 (:status resp)))
                   (expect (= "/docs/skills" (get-in resp [:headers "location"])))))
             (it "an unknown .md path still falls through as nil"
                 (expect (nil? (docs/handle {:uri "/docs/nope-zzz.md" :headers {}})))))
