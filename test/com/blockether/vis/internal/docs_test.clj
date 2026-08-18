(ns com.blockether.vis.internal.docs-test
  "Docs renderer: cross-page markdown links must resolve in BOTH output
   modes (live `/docs/<slug>`, static `<slug>.html`), and the live
   handler tolerates literal `<slug>.md` deep links with a redirect.
   Plus the CONTENT invariants: `extending.md` is where an extension author learns
   what `doc(name)` renders, what an `apropos` row previews, and how a live view is
   driven from Python."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.docs :as docs]
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

(defdescribe
  collect-memoization-test
  "Every docs request and every corpus rebuild re-read and re-rendered all 16
   pages, so serving `/docs` and asking `apropos` a question both paid ~8 ms of
   markdown rendering that nothing had invalidated."
  (it "answers the identical site while no page has changed"
      (expect (identical? (docs/collect) (docs/collect))))
  (it "does not tick its generation for an unchanged tree"
      (let [g (docs/generation)]
        (docs/collect)
        (expect (= g (docs/generation))))))

(defn- extending-md [] (:md (first (filter #(= "extending" (:slug %)) (:pages (docs/collect))))))

;; A tool page is rendered from FOUR entry keys (`extension/symbol-signature`,
;; `symbol-keys-line`, `symbol-doc-text`) and previewed as a FOUR-key row
;; (`doc-corpus/gist`). `extending.md` is the only place an author is told either,
;; so when one of those renderers changes, this test names the page that must
;; change with it.
(defdescribe
  extending-page-teaches-its-renderings-test
  (it "names every entry key `doc(name)` renders, and both structural lines"
      (let [md (extending-md)]
        (expect (string? md))
        (doseq [needle [":description" ":params" ":result" ":call" "Keys:" "(REQUIRED)"
                        "Raw result:"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle)))))
  (it "shows an `apropos` row with all four of its keys"
      (let [md (extending-md)]
        (doseq [needle ["'kind'" "'gist'" "'at'" "'hit'"]]
          (expect (str/includes? md needle)
                  (str "extending.md never shows " needle " in an apropos row")))))
  (it "names the shim keys that drive discovery and the page they answer with"
      (let [md (extending-md)]
        (doseq [needle [":shim/imports" ":shim/globals" ":shim/description" ":shim/docs"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle))))))

;; A live view is the one primitive an author cannot infer from the field builders:
;; its verbs differ per node type, and `vis.output` deliberately does not match the
;; `log` node it builds (`vis.log` is the engine log line). When that Python surface
;; is renamed, this test names the page that has to be renamed with it.
(defdescribe
  extending-page-teaches-the-live-view-test
  (it "names the opener, the log builder that could not be called `log`, and what a loop reads"
      (let [md (extending-md)]
        (doseq [needle ["vis.live(" "vis.output(" "upsert(" "is_interrupted" "vis.Interrupted"
                        "flush_ms" "view.is_from_human" "view.note"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle)))))
  ;; Layout is the half an author cannot infer: without these two paragraphs a run writes a
  ;; second node where one paragraph BESIDE the table was meant, and marks up a string the
  ;; page never promised to paint.
  (it "teaches where a node stands and what a human-facing string may carry"
      (let [md (extending-md)]
        (doseq [needle ["vis.row(" "vis.column(" "inline markdown" "wraps and justifies"
                        "stay verbatim" "`Escape` or `Enter` sends the stop"]]
          (expect (str/includes? md needle) (str "extending.md never mentions " needle))))))
