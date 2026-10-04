(ns com.blockether.vis.internal.docs.core-test
  "Documentation rendering, navigation, supported features and Python examples."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.core :as docs]
            [com.blockether.vis.internal.docs.corpus :as dc]
            [com.blockether.vis.test-prose :as prose]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- rendered-theme
  [html mode]
  (if (= mode :static)
    (slurp (io/resource "vis-docs/assets/theme.css"))
    (second (re-find #"(?s)<style>(.*?)</style>" html))))

(defdescribe
  python-presentation-test
  (it "loads the bundled Python highlighter and reduces Python code size in both modes"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first (filter #(= "human-input" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site page mode)]]

          (let [css (rendered-theme html mode)]
            (if (= mode :static)
              (do (expect (str/includes? html "src=\"assets/prism.min.js\" defer"))
                  (expect (str/includes? html "src=\"assets/docs.js\" defer"))
                  (expect (not (str/includes? html "<script>"))))
              ;; Live pages inline the same docs.js that static pages load.
              (do (expect (str/includes? html "Prism.languages.python="))
                  (expect (str/includes? html
                                         (str "\n"
                                              (slurp (io/resource "vis-docs/assets/docs.js"))
                                              "</script>")))))
            (expect (re-find #"\.content p img\s*\{\s*max-width: 100%;\s*height: auto;\s*\}" css))
            (expect (re-find #"\.content pre code\.language-python\s*\{\s*font-size: 0\.75rem;\s*\}"
                             css))))))
  (it
    "resolves screenshot sources and full-size links in both modes"
    (let [{:keys [pages] :as site} (docs/collect)]
      (doseq [[slug image] [["human-input" "ask"] ["live-views" "live-running"]
                            ["live-views" "live-stop"] ["extension-design" "nesting-finding"]
                            ["extension-design" "nesting-clear"] ["index" "ios-conversation"]
                            ["index" "ios-sessions"] ["index" "ios-project"]
                            ["index" "desktop-conversation"] ["index" "desktop-project"]
                            ["index" "desktop-release"] ["index" "tui-conversation"]
                            ["index" "tui-sessions"] ["index" "tui-project"]]
              mode [:static :live]
              :let [page (first (filter #(= slug (:slug %)) pages))
                    html (docs/page-html site page mode)
                    path (str (when (= mode :live) "/docs/") "assets/screenshots/" image ".png")]]

        (expect (str/includes? html (str "src=\"" path "\"")))
        (expect (str/includes? html (str "href=\"" path "\"")))))))

(defdescribe
  dropdown-assets-test
  (it "loads the shared dropdown entrypoint in static and live documentation"
      (let [{:keys [pages] :as site} (docs/collect)]
        (doseq [[mode prefix] [[:static "assets/"] [:live "/docs/assets/"]]]
          (expect (str/includes?
                    (docs/page-html site (first pages) mode)
                    (str "<script type=\"module\" src=\"" prefix "select-init.js\"></script>"))))))
  (it "copies and serves the shared dropdown modules with a JavaScript media type"
      (doseq [name
              ["select.js" "select-init.js"]

              :let [response
                    (docs/handle {:uri (str "/docs/assets/" name)})]]

        (expect (= (str "assets/" name) (get @#'docs/asset-files (str "vis-docs/assets/" name))))
        (expect (= 200 (:status response)))
        (expect (= "text/javascript; charset=utf-8" (get-in response [:headers "content-type"])))
        (when-let [body (:body response)]
          (with-open [in ^java.io.InputStream body]
            (expect (str/includes? (slurp in) "mountSelects")))))))

(defdescribe
  search-test
  (it "mounts the search box and its entrypoint in static and live documentation"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first pages)]

        (doseq [[mode prefix] [[:static "assets/"] [:live "/docs/assets/"]]]
          (let [html (docs/page-html site page mode)]
            (expect (str/includes?
                      html
                      (str "<search class=\"search\" data-index=\"" prefix "search.json\">")))
            (expect (str/includes? html
                                   (str "<script type=\"module\" src=\""
                                        prefix
                                        "search-init.js\"></script>")))))))
  (it "grows the header search across narrower rows, with no spacer to strand it"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first pages)]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site page mode)

                      header
                      (second (re-find #"(?s)<header class=\"top\">(.*?)</header>" html))

                      search
                      (second (re-find #"(?s)\.top \.search\s*\{([^}]+)\}"
                                       (rendered-theme html mode)))]]

          ;; A growing spacer beside the box takes half the free width and strands the
          ;; search next to the brand; only the catalog header, without a box, keeps one.
          (expect (str/includes? header "<search class=\"search\""))
          (expect (not (str/includes? header "class=\"spacer\"")))
          (expect (str/includes? search "flex: 1 1 auto"))
          (expect (not (str/includes? search "max-width"))))))
  (it "fits the desktop search box to the content column, between sidebar and rail"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first pages)]

        (doseq [mode
                [:static :live]

                :let [css
                      (rendered-theme (docs/page-html site page mode) mode)

                      desktop
                      (second (re-find
                                #"(?s)@media\s*\(min-width: 1101px\).*?\.top \.search\s*\{([^}]+)\}"
                                css))]]

          ;; The box is pinned to the shell's middle column, so it measures exactly the
          ;; content a reader sees instead of the whole header row.
          (expect (str/includes? css
                                 "grid-template-columns: var(--side) minmax(0, 1fr) var(--rail);"))
          (expect (str/includes? desktop "position: absolute"))
          (expect (str/includes? desktop "+ var(--side)"))
          (expect (str/includes? desktop "+ var(--rail)"))
          ;; Nothing grows in the row any more, so the icons hold the right edge.
          (expect (re-find #"\.top \.center-link,\s+\.top \.gh\s*\{\s*margin-left: auto;" css))
          (expect (re-find #"\.top \.center-link ~ \.gh\s*\{\s*margin-left: 0;" css)))))
  (it
    "collapses the phone header to a magnifier that opens the box as the row below"
    (let [{:keys [pages] :as site}
          (docs/collect)

          page
          (first pages)]

      (doseq [mode
              [:static :live]

              :let [html
                    (docs/page-html site page mode)

                    header
                    (second (re-find #"(?s)<header class=\"top\">(.*?)</header>" html))

                    css
                    (rendered-theme html mode)

                    mobile
                    (second (re-find
                              #"(?s)@media\s*\(max-width: 820px\).*?\.top \.search\s*\{([^}]+)\}"
                              css))]]

        ;; The checkbox has to precede the box it reveals, and its label is the only
        ;; search control the header shows while the box is collapsed.
        (expect (< (str/index-of header "id=\"searchtoggle\"")
                   (str/index-of header "class=\"search-open\"")
                   (str/index-of header "<search class=\"search\"")))
        (expect (str/includes? header "<label for=\"searchtoggle\""))
        (expect (str/includes? header "aria-label=\"Search\""))
        ;; Wide screens keep the plain box: the magnifier is a phone-only control.
        (expect (str/includes? css ".search-open {\n  display: none;"))
        (expect (re-find #"\.search-open\s*\{\s*display: inline-flex;\s*margin-left: auto;" css))
        ;; The revealed row spans the header from edge to edge, under its own row.
        (expect (str/includes? mobile "position: absolute"))
        (expect (str/includes? mobile "top: 100%"))
        (expect (str/includes? mobile "display: none"))
        (expect (re-find #"\.searchtoggle:checked ~ \.search\s*\{\s*display: flex;" css)))))
  (it "copies the search modules with the other static assets"
      (doseq [name ["search.js" "search-init.js"]]
        (expect (= (str "assets/" name) (get @#'docs/asset-files (str "vis-docs/assets/" name))))))
  (it
    "serves a live JSON index whose sections answer their page's own anchors"
    (let [{:keys [pages]}
          (docs/collect)

          anchors
          (into {} (map (juxt :slug #(set (map :id (:toc %))))) pages)

          {:keys [status headers body]}
          (docs/handle {:uri "/docs/assets/search.json" :headers {}})

          entries
          (:pages (json/read-json body :key-fn keyword))

          slug-of
          (fn [href]
            (-> href
                (str/replace #"^/docs/" "")
                (str/split #"#")
                first))]

      (expect (= 200 status))
      (expect (= "application/json; charset=utf-8" (get headers "content-type")))
      (expect (= (count pages) (count (distinct (map (comp slug-of :href) entries)))))
      (doseq [{:keys [slug]} pages]
        (expect (some #(and (= slug (slug-of (:href %))) (str/blank? (:heading %))) entries)
                (str slug " has no lead section in the search index")))
      (doseq [{:keys [href]}
              entries

              :let [frag
                    (second (str/split href #"#"))]
              :when frag]

        (expect (contains? (get anchors (slug-of href)) frag)
                (str href " points at no anchor on its page")))))
  (it "writes the static index beside the pages it searches"
      (let [dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-docs-search-" (java.util.UUID/randomUUID)))]
        (try (let [built (docs/build-site! dir {:public? true})
                   entries (:pages (json/read-json (slurp (io/file dir "assets" "search.json"))
                                                   :key-fn
                                                   keyword))]

               (expect (= (count (:pages built))
                          (count (distinct (map #(-> (:href %)
                                                     (str/split #"#")
                                                     first
                                                     (str/replace #".html$" ""))
                                                entries)))))
               (expect (some #(= "index.html" (:href %)) entries))
               (doseq [{:keys [href]} entries]
                 (expect (re-find #"^[a-z0-9-]+\.html(#|$)" href)
                         (str href " is not a static page link"))))
             (finally (doseq [f (reverse (file-seq dir))]
                        (.delete f)))))))

(defdescribe
  screenshot-assets-test
  (it "serves every screenshot as a PNG and includes it in static assets"
      (doseq [name
              ["ask" "live-running" "live-stop" "nesting-finding" "nesting-clear" "ios-conversation"
               "ios-sessions" "ios-project" "desktop-conversation" "desktop-project"
               "desktop-release" "tui-conversation" "tui-sessions" "tui-project"]

              :let [rel
                    (str "screenshots/" name ".png")

                    response
                    (docs/handle {:uri (str "/docs/assets/" rel)})]]

        (expect (= (str "assets/" rel) (get @#'docs/asset-files (str "vis-docs/assets/" rel))))
        (expect (= 200 (:status response)))
        (expect (= "image/png" (get-in response [:headers "content-type"])))
        (with-open [body ^java.io.InputStream (:body response)]
          (expect (= [137 80 78 71 13 10 26 10] (vec (repeatedly 8 #(.read body)))))))))

(defdescribe
  council-diagrams-test
  (it
    "renders linked diagrams in both modes and serves their source assets"
    (let [{:keys [pages] :as site}
          (docs/collect)

          page
          (first (filter #(= "council" (:slug %)) pages))]

      (doseq [name
              ["council-messages" "council-modules"]

              ext
              ["svg" "mmd"]

              :let [rel
                    (str "diagrams/" name "." ext)

                    response
                    (docs/handle {:uri (str "/docs/assets/" rel)})]]

        (expect (= (str "assets/" rel) (get @#'docs/asset-files (str "vis-docs/assets/" rel))))
        (expect (= 200 (:status response)))
        (expect (= (if (= ext "svg") "image/svg+xml" "text/plain; charset=utf-8")
                   (get-in response [:headers "content-type"])))
        (with-open [body ^java.io.InputStream (:body response)]
          (let [text (slurp body)]
            (if (= ext "svg")
              (do (expect (str/includes? text "<svg"))
                  (expect (str/includes? text "aria-labelledby="))
                  (expect (str/includes? text "aria-describedby="))
                  (expect (not (str/includes? text "<script"))))
              (expect (str/includes? text "accTitle: Council")))))
        (doseq [mode
                [:static :live]

                :when (= ext "svg")
                :let [html
                      (docs/page-html site page mode)

                      path
                      (str (when (= mode :live) "/docs/") "assets/" rel)]]

          (expect (str/includes? html (str "href=\"" path "\"")))
          (expect (str/includes? html (str "src=\"" path "\""))))))))

(defdescribe
  experimental-feature-docs-test
  (it
    "omits experimental guides and instructions from the published manual"
    (let [{:keys [pages]} (docs/collect)]
      (expect (not (contains? (set (map :slug pages)) "working-with-plans")))
      (expect (nil? (io/resource "vis-docs/working-with-plans.md")))
      (doseq [{:keys [slug md]} pages]
        (expect
          (not
            (re-find
              #"(?im)subagents?|publish_spawn|autocomplain|complain_entry_id|working-with-plans|plan before (?:coding|doing)|`improve`|\*\*Improve\*\*|^\s+improve(?:_mode)?:"
              md))
          slug)))))

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

(defdescribe
  mobile-zoom-test
  (it "pins phones to the mobile layout instead of letting them zoom the page"
      (let [{:keys [pages] :as site}
            (docs/collect)

            home
            (first (filter #(= "index" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site home mode)

                      css
                      (rendered-theme html mode)]]

          (expect (str/includes?
                    html
                    (str "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1,"
                         "maximum-scale=1,user-scalable=no,viewport-fit=cover\">")))
          ;; `manipulation` would still allow pinch zoom; only panning may stay.
          (expect (str/includes? css "touch-action: pan-x pan-y")))
        ;; Safari obeys neither the meta nor touch-action, so the static script
        ;; refuses its pinch gestures.
        (expect (str/includes? (slurp (io/resource "vis-docs/assets/docs.js")) "gesturestart")))))

(defdescribe mobile-navigation-test
             (it "keeps the menu control without a hover or tap highlight rectangle"
                 (let [{:keys [pages] :as site}
                       (docs/collect)

                       home
                       (first (filter #(= "index" (:slug %)) pages))]

                   (doseq [mode
                           [:static :live]

                           :let [html
                                 (docs/page-html site home mode)

                                 css
                                 (rendered-theme html mode)]]

                     (expect (str/includes? html "class=\"hamburger\""))
                     (expect (str/includes? html "id=\"navtoggle\""))
                     (expect (not (str/includes? css ".hamburger:hover")))
                     (expect (str/includes? css "-webkit-tap-highlight-color: transparent"))))))

(defdescribe
  mobile-sidebar-scroll-test
  (it "keeps the mobile drawer scrollable within the visible viewport"
      (let [{:keys [pages] :as site}
            (docs/collect)

            home
            (first (filter #(= "index" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site home mode)

                      css
                      (rendered-theme html mode)

                      drawer
                      (second (re-find #"(?s)@media\s*\(max-width: 820px\).*?\.side\s*\{([^}]+)\}"
                                       css))]]

          ;; WebKit expands a fixed grid item with height:auto to its contents,
          ;; leaving no internal overflow even when the last links are offscreen.
          (expect (str/includes? drawer "height: calc(100dvh - 4rem - env(safe-area-inset-top))"))
          (expect (not (str/includes? drawer "height: auto")))
          (expect (re-find #"\.side\s*\{[^}]*overflow-y: auto" css))
          (expect (str/includes? drawer "overscroll-behavior-y: contain"))
          (expect (str/includes? drawer
                                 "padding-bottom: calc(3rem + env(safe-area-inset-bottom))"))))))

(defdescribe
  responsive-typography-test
  (it "uses one bundled font and compact, wrapping tables in both outputs"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first (filter #(= "extending" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site page mode)

                      css
                      (rendered-theme html mode)]]

          (expect (str/includes? css "font-family: var(--font)"))
          (expect (str/includes? css "--font: 'JetBrains Mono', monospace"))
          (expect (not (str/includes? css "Hanken")))
          (expect (str/includes? css "--text-small: 0.8125rem"))
          (expect (str/includes? css "--text-small: 0.75rem"))
          (expect (str/includes? css "overflow-wrap: anywhere"))
          (expect (str/includes? css "table-layout: fixed"))
          (expect (re-find #"\.content pre\s*\{\s*font-family: inherit" css))
          (expect (re-find #"\.content th,\s*\.content td\s*\{[^}]*white-space: normal" css))
          (expect (re-find #"\.content th code,\s*\.content td code\s*\{\s*font-size: inherit" css))
          (expect (str/includes? html "<thead>"))))))

(defdescribe shared-theme-test
             (it "shares one stylesheet, external in static output and embedded in live output"
                 (let [{:keys [pages] :as site}
                       (docs/collect)

                       theme
                       (slurp (io/resource "vis-docs/assets/theme.css"))]

                   (doseq [[mode prefix] [[:static "./fonts/"] [:live "/docs/assets/fonts/"]]]
                     (let [html (docs/page-html site (first pages) mode)
                           stylesheet (rendered-theme html mode)]

                       (expect (= (str/replace theme "./fonts/" prefix) stylesheet))
                       (expect (some? (io/resource
                                        "vis-docs/assets/fonts/jetbrains-mono.woff2"))))))))

(defdescribe
  getting-started-page-test
  (it "uses ordinary documentation links and one install command in both outputs"
      (let [{:keys [pages] :as site}
            (docs/collect)

            home
            (first (filter #(= "index" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site home mode)]]

          (expect (= 1 (count (re-seq #"curl -fsSL" html))))
          (expect (str/includes? html "href=\"#install\""))
          (expect (re-find #"class=\"brand\"[^>]*>Vis</a>" html))
          (expect (not (str/includes? html "role=\"tablist\"")))
          (expect (not (str/includes? html "hero-install")))
          (expect (not (str/includes? html "data-copy-active")))
          (expect (str/includes? html "id=\"first-session\"")))))
  (it "opens on the intro module of Rationale and Getting started without a separate app guide"
      (let [slugs (mapv :slug (:pages (docs/collect)))]
        (expect (= ["rationale" "index"] (subvec slugs 0 2)))
        (expect (not (some #{"gateway"} slugs)))
        (expect (nil? (io/resource "vis-docs/gateway.md")))
        (expect (< (.indexOf slugs "sessions") (.indexOf slugs "python-sandbox")))))
  (it "renames the motivation, token and prompt pages without keeping the old files"
      (let [slugs (set (map :slug (:pages (docs/collect))))]
        (doseq [[old new] {"motivation" "rationale"
                           "token-optimization" "context-management"
                           "context-and-prompts" "project-instructions"}]
          (expect (contains? slugs new) new)
          (expect (not (contains? slugs old)) old)
          (expect (nil? (io/resource (str "vis-docs/" old ".md"))) old))))
  (it "keeps controlling, managing and exporting sessions in one page"
      (let [{:keys [pages]}
            (docs/collect)

            slugs
            (set (map :slug pages))

            anchors
            (set (map :id (:toc (first (filter #(= "sessions" (:slug %)) pages)))))]

        (doseq [old ["queue-and-cancel" "exporting-sessions"]]
          (expect (not (contains? slugs old)) old)
          (expect (nil? (io/resource (str "vis-docs/" old ".md"))) old))
        ;; A redirect keeps the fragment of an old bookmark, so every old anchor stays.
        (doseq [id ["queue-a-message" "cancel-a-turn" "quit" "export-a-session" "markdown" "html"]]
          (expect (contains? anchors id) id))))
  (it "removes the Java and Clojure SDK guide"
      (let [{:keys [pages]} (docs/collect)]
        (expect (not (some #{"jvm-sdk"} (map :slug pages))))
        (expect (nil? (io/resource "vis-docs/jvm-sdk.md")))))
  (it
    "keeps setup on the landing page and gateway details in their own guide"
    (let [{:keys [pages] :as site}
          (docs/collect)

          home
          (first (filter #(= "index" (:slug %)) pages))

          gateway
          (first (filter #(= "gateway-service" (:slug %)) pages))

          sessions
          (first (filter #(= "sessions" (:slug %)) pages))

          md
          (:md home)]

      (doseq [mode
              [:static :live]

              :let [html
                    (docs/page-html site home mode)]
              anchor
              ["why-vis" "see-vis-in-action" "connecting-the-companion-app" "connect-an-app"
               "get-the-phone-app" "connect-the-desktop-app" "pair-a-phone"
               "access-from-anywhere-with-tailscale" "work-with-a-project"
               "put-your-expertise-into-code" "combine-steps-in-python"
               "follow-the-work-on-every-screen" "keep-useful-work-when-you-return" "updating-vis"
               "native-vs-jvm" "gateway-reference" "starting-the-gateway"
               "using-a-remote-gateway-from-the-cli" "tokens-and-http-401" "http-api" "python-sdk"
               "resource-limits" "see-also"]]

        (expect (str/includes? html (str "id=\"" anchor "\"")) anchor))
      (doseq [content ["vis-agent gateway start --host 127.0.0.1"
                       "vis-agent gateway start --host 10.0.0.5 --require-token --pair"
                       "vis-agent gateway pair" "gateway-service.md"]]
        (expect (str/includes? md content) content))
      (doseq [content ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "HTTP 401" "HTTP 426"
                       "VIS_GATEWAY_MAX_CONCURRENT_TURNS" "VIS_GATEWAY_EVENT_RING_MAX"
                       "VIS_ENV_CACHE_MAX" "VIS_ENV_IDLE_TTL_MS" "VIS_ENV_RSS_BUDGET_MB"
                       "does not encrypt HTTP" "Stopping a busy gateway interrupts" "Amber means"]]
        (expect (not (str/includes? md content)) content)
        (expect (str/includes? (:md gateway) content) content))
      (expect (not (str/includes? md "**Compact mode**")))
      (expect (str/includes? (:md sessions) "**Compact mode**"))
      (expect (< (str/index-of md "## First session") (str/index-of md "## Gateway reference")))
      (expect (not (str/includes? md "gateway.md"))))))

(defdescribe
  reader-first-onboarding-test
  ;; Readers should not need the model's execution contract to install Vis.
  (it "keeps execution reference out of the introduction"
      (doseq [source
              [(io/file "README.md") (io/resource "vis-docs/index.md")]

              :let [intro
                    (first (str/split (slurp source) #"(?m)^## Install$" 2))]
              term
              ["python_execution" "await gather" "apropos()" "Path.write_text()" "toggles.shell"]]

        (expect (not (str/includes? intro term)) (str source " introduces " term " before setup"))))
  (it "starts with a short introduction and installation rather than feature summaries"
      (let [md
            (:md (first (filter #(= "index" (:slug %)) (:pages (docs/collect)))))

            lead
            (first (str/split md #"\n\s*\n"))]

        (expect (<= (count (str/split lead #"\s+")) 30))
        (expect (= "Install" (second (re-find #"(?m)^## (.+)$" md))))
        (doseq [heading ["## Why Vis" "## Work with a project" "## Native vs JVM"]]
          (expect (not (str/includes? md heading)) heading))))
  (it "connects the landing page to desktop downloads and setup in both outputs"
      (let [{:keys [pages] :as site}
            (docs/collect)

            page
            (first (filter #(= "index" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site page mode)

                      setup
                      (str (if (= mode :static) "distributions.html" "/docs/distributions")
                           "#open-the-desktop-app")]]

          (expect (str/includes? html "href=\"https://github.com/Blockether/vis/releases/latest\""))
          (expect (str/includes? html (str "href=\"" setup "\"")))))))

;; Regression: fixed-width shortcuts let long labels spill into adjacent links.
;; Keep only setup actions here. Motivation and session guides belong in Learn more.
(defdescribe
  getting-started-quick-links-test
  (it
    "renders a wrapping navigation group with intact links in static and live docs"
    (let [{:keys [pages] :as site}
          (docs/collect)

          home
          (first (filter #(= "index" (:slug %)) pages))]

      (doseq [mode
              [:static :live]

              :let [html
                    (docs/page-html site home mode)

                    css
                    (rendered-theme html mode)

                    navigation
                    (second
                      (re-find
                        #"(?s)<nav class=\"quick-links\" aria-label=\"Getting started\">(.*?)</nav>"
                        html))

                    links
                    (mapv (fn [[_ href label]]
                            [href label])
                          (re-seq #"<a href=\"([^\"]+)\">([^<]+)</a>" (or navigation "")))]]

        (expect (= [["#install" "Install"] ["#first-session" "Try a task"]
                    ["#connect-an-app" "Connect an app"]]
                   links))
        (expect (not (str/includes? (or navigation "") "·")))
        (expect (not (str/includes? html "<p><nav class=\"quick-links\"")))
        (let [link-css (second (re-find #"(?s)(?:^|\})\s*\.quick-links a\s*\{([^}]+)\}" css))]
          (doseq [fragment ["flex: 0 1 auto" "min-height: 2.75rem" "max-width: 100%"
                            "white-space: normal"]]
            (expect (str/includes? (or link-css "") fragment) fragment)))
        (expect (re-find #"\.quick-links\s*\{\s*display: flex;\s*flex-wrap: wrap;\s*gap: 0\.5rem"
                         css))
        (expect (not (str/includes? css "flex-basis: calc(50% - 0.25rem)")))
        (doseq [fragment [".quick-links a:hover" "a:focus-visible"]]
          (expect (str/includes? css fragment) fragment))))))

(defdescribe
  portable-store-buttons-test
  ;; GitHub does not load the docs stylesheet: linked images must carry the labels.
  (it
    "keeps mobile and published-release desktop buttons readable without the documentation stylesheet"
    (let [published
          (second (re-find #"releases/download/v([0-9]+\.[0-9]+\.[0-9]+)/vis-companion-"
                           (slurp "README.md")))

          release
          (str "https://github.com/Blockether/vis/releases/download/v" published
               "/vis-companion-" published)]

      ;; VIS_VERSION can advance before a release exists. Both guides keep the last
      ;; published download until the new artifacts are available.
      (expect (some? published))
      (doseq [[source prefix]
              [[(io/file "README.md") "resources/vis-docs/"] [(io/resource "vis-docs/index.md") ""]]

              [name url label]
              [["testflight" "https://testflight.apple.com/join/4anYT4Wk"
                "TestFlight for iOS and iPadOS"]
               ["google-play" "https://play.google.com/apps/testing/com.blockether.viscompanion"
                "Google Play beta for Android"]
               ["windows" (str release "-windows-x64.msi") "Download Vis for Windows x64"]
               ["macos" (str release "-macos-universal.dmg") "Download Vis for macOS"]
               ["linux" (str release "-linux-x64.AppImage") "Download Vis for Linux x64"]]]

        (let [md
              (slurp source)

              body
              (some (fn [[_ attrs contents]]
                      (when (and (str/includes? attrs (str "href=\"" url "\""))
                                 (str/includes? contents (str "assets/install-" name ".png")))
                        contents))
                    (re-seq #"(?s)<a\b([^>]*)>(.*?)</a>" md))]

          (expect (str/includes? (or body "")
                                 (str "src=\"" prefix "assets/install-" name ".png\"")))
          (expect (str/includes? (or body "") (str "alt=\"" label "\"")))
          (expect (str/includes? (or body "") "width=\"224\" height=\"56\""))))))
  (it "serves and exports all mobile and desktop image buttons"
      (doseq [name ["testflight" "google-play" "windows" "macos" "linux"]]
        (let [rel (str "install-" name ".png")
              response (docs/handle {:uri (str "/docs/assets/" rel)})]

          (expect (= 200 (:status response)))
          (expect (= "image/png" (get-in response [:headers "content-type"])))
          (expect (= (str "assets/" rel) (get @#'docs/asset-files (str "vis-docs/assets/" rel))))
          (when-let [body (:body response)]
            (with-open [^java.io.InputStream in body]
              (expect (= [137 80 78 71 13 10 26 10] (vec (repeatedly 8 #(.read in)))))))))))

(defdescribe
  reading-layout-test
  (it
    "justifies prose, aligns list markers, wraps shell commands and links mobile installs"
    (let [{:keys [pages] :as site}
          (docs/collect)

          home
          (first (filter #(= "index" (:slug %)) pages))]

      (doseq [mode
              [:static :live]

              :let [html
                    (docs/page-html site home mode)]]

        (doseq [needle ["text-align: justify" "text-align-last: start" "padding-inline-start: 3ch"
                        "pre:has(> code.language-bash)" "white-space: pre-wrap"
                        "class=\"store-links\"" "https://testflight.apple.com/join/4anYT4Wk"
                        "https://play.google.com/apps/testing/com.blockether.viscompanion"
                        "TestFlight" "Google Play beta" "hyphens: none"
                        "list-style-position: outside" "a:focus-visible" "class=\"store-apple\""
                        "class=\"store-android\""]]
          (expect (or (str/includes? html needle) (str/includes? (rendered-theme html mode) needle))
                  needle))
        (let [command (second (re-find #"(?s)<pre><code class=\"language-bash\">(.*?)</code></pre>"
                                       html))]
          (expect
            (=
              "curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash"
              (some-> command
                      (str/replace #"<[^>]+>" "")
                      str/trim))))))))

;; Regression: native ordered-marker suffixes can put numbers outside the prose edge.
(defdescribe
  ordered-marker-spacing-test
  (it "sets an explicit ordered-marker suffix in static and live documentation"
      (let [{:keys [pages] :as site}
            (docs/collect)

            home
            (first (filter #(= "index" (:slug %)) pages))]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site home mode)

                      css
                      (rendered-theme html mode)]]

          (expect (str/includes? html "<ol>"))
          (expect (re-find
                    #"\.content ol > li::marker\s*\{\s*content: counter\(list-item\) '\. ';\s*\}"
                    css)
                  "Ordered markers must not depend on the browser's native separator width.")))))

(defdescribe
  handle-md-redirect-test
  (it "GET /docs/<slug>.md permanent-redirects to /docs/<slug>"
      (let [resp (docs/handle {:uri "/docs/skills.md" :headers {}})]
        (expect (= 301 (:status resp)))
        (expect (= "/docs/skills" (get-in resp [:headers "location"])))))
  (it "redirects old app-guide URLs to getting started without replacing their fragments"
      (doseq [uri ["/docs/gateway" "/docs/gateway/" "/docs/gateway.md" "/docs/gateway.html"]]
        (expect (= {:status 301 :headers {"location" "/docs"} :body ""}
                   (docs/handle {:uri uri :headers {}}))
                uri)))
  (it "redirects merged session pages to Sessions without replacing their fragments"
      (doseq [page
              ["queue-and-cancel" "exporting-sessions"]

              suffix
              ["" "/" ".md" ".html"]

              :let [uri
                    (str "/docs/" page suffix)]]

        (expect (= {:status 301 :headers {"location" "/docs/sessions"} :body ""}
                   (docs/handle {:uri uri :headers {}}))
                uri)))
  (it "redirects renamed pages to their new slugs without replacing their fragments"
      (doseq [[page target]
              {"motivation" "/docs/rationale"
               "token-optimization" "/docs/context-management"
               "context-and-prompts" "/docs/project-instructions"}

              suffix
              ["" "/" ".md" ".html"]

              :let [uri
                    (str "/docs/" page suffix)]]

        (expect (= {:status 301 :headers {"location" target} :body ""}
                   (docs/handle {:uri uri :headers {}}))
                uri)))
  (it "redirects the removed Java and Clojure SDK guide to HTTP API basics"
      (doseq [suffix
              ["" "/" ".md" ".html"]

              :let [uri
                    (str "/docs/jvm-sdk" suffix)]]

        (expect (= {:status 301 :headers {"location" "/docs/http-api"} :body ""}
                   (docs/handle {:uri uri :headers {}}))
                uri)))
  (it "redirects merged Python and HTTP pages to their API page and variant"
      (doseq [topic
              ["automations" "configuration" "context-management" "council" "drafts"
               "project-instructions" "sessions"]

              variant
              ["python" "http"]

              suffix
              ["" "/" ".md" ".html"]

              :let [uri
                    (str "/docs/" variant "-" topic suffix)

                    target
                    (str "/docs/" topic "-api?variant=" variant)]]

        (expect (= {:status 301 :headers {"location" target} :body ""}
                   (docs/handle {:uri uri :headers {}}))
                uri)
        (expect (= 200 (:status (docs/handle {:uri (str "/docs/" topic "-api") :headers {}})))
                target)))
  (it "an unknown .md path still falls through as nil"
      (expect (nil? (docs/handle {:uri "/docs/nope-zzz.md" :headers {}})))))

(defdescribe
  collect-memoization-test
  "Every docs request and every corpus rebuild re-read and re-rendered every
   page, so serving `/docs` and asking `apropos` a question both paid ~8 ms of
   markdown rendering that nothing had invalidated."
  (it "renders the site once and answers the same value"
      (expect (identical? (docs/collect) (docs/collect)))))

(defn- page-md
  "The markdown of the page at `slug`."
  [slug]
  (:md (first (filter #(= slug (:slug %)) (:pages (docs/collect))))))

(defdescribe
  supported-extension-docs-test
  (it "does not publish the removed Clojure extension guide"
      (expect (not-any? #(= "clojure-extensions" (:slug %)) (:pages (docs/collect)))))
  (it
    "does not advertise removed extension APIs in current guides"
    (doseq [{:keys [slug md]} (:pages (docs/collect))]
      (expect
        (not
          (re-find
            #"clojure-extensions|clojure-builders|vis/request-human-input!|vis/register-extension!|:ext/engine|vis-agent extension test|Routes added by extensions"
            md))
        (str "unsupported extension documentation in " slug))))
  (it "does not serve the removed guide or redirect its Markdown URL"
      (doseq [uri ["/docs/clojure-extensions" "/docs/clojure-extensions.md"]]
        (expect (nil? (docs/handle {:uri uri :headers {}}))))))

(defdescribe editable-package-docs-test
             ;; #175: the authoring guide must not undo editable installs in its instructions.
             (it "documents editable package metadata instead of copied local wheels"
                 (let [md (page-md "extension-development")]
                   (expect (not (re-find #"--no-editable|installed\s+noneditably" md)))
                   (doseq [file ["einmal/pyproject.toml" "einmal/src/einmal/__init__.py"
                                 ".vis/extensions/einmal_tools.py" "einmal/tests/test_status.py"]]
                     (expect (str/includes? md (str "# " file "\n"))
                             (str "Missing executable package example: " file)))
                   (expect (str/includes? md "editable = true")))))

;; A live view is the one primitive an author cannot infer from the field builders:
;; its verbs differ per node type, and `vis.output` deliberately does not match the
;; `log` node it builds (`vis.log` is the engine log line). When that Python surface
;; is renamed, this test names the page that has to be renamed with it.
(defdescribe
  live-views-page-teaches-the-live-view-test
  (it "names the opener, the log builder that could not be called `log`, and what a loop reads"
      (let [md (page-md "live-views")]
        (doseq [needle ["vis.live(" "vis.output(" "upsert(" "is_interrupted" "vis.Interrupted"
                        "flush_ms" "view.is_from_human" "view.note"]]
          (expect (str/includes? md needle) (str "live-views.md never mentions " needle)))))
  ;; #209: document visible terminal-control escapes, not unsafe verbatim output.
  (it "documents layout, text formatting, and interruption"
      (let [md (page-md "live-views")]
        (doseq [needle ["vis.row(" "vis.column(" "inline Markdown" "wraps and is justified"
                        "Terminal controls in logs show as visible escapes"
                        "Other text stays literal" "`Escape` or `Enter` confirms"]]
          (expect (str/includes? md needle) (str "live-views.md never mentions " needle))))))

;;; ── The page contract ───────────────────────────────────────────────────────
;; Every rule below is one the RENDERER already assumes (see the `docs` ns
;; docstring, which states the contract): a title that disagrees with the
;; sidebar, a heading too deep to be given an anchor, a `#fragment` pointing at
;; nothing and a page nothing links to are all invisible until a reader walks
;; into them.

(def ^:private fence-languages
  "Languages a fenced block may declare. ONE set, so the same kind of block is
   highlighted the same way on every page."
  #{"bash" "clojure" "edn" "ini" "java" "json" "markdown" "python" "text" "toml" "xml" "yaml"})

(defn- scan
  "PURE: `{:headings [[line level text] …] :fences [[line info] …]}` for `md`.
   Fenced blocks are skipped, so a `#` comment inside a shell example is not
   mistaken for a heading."
  [^String md]
  (loop [ls
         (str/split-lines md)

         n
         1

         in-fence?
         false

         acc
         {:headings [] :fences []}]

    (if-let [l (first ls)]
      (cond (str/starts-with? l "```")
            (recur (rest ls)
                   (inc n)
                   (not in-fence?)
                   (if in-fence? acc (update acc :fences conj [n (str/trim (subs l 3))])))
            in-fence? (recur (rest ls) (inc n) in-fence? acc)
            :else (if-let [[_ hashes text] (re-matches #"(#{1,6}) (.*)" l)]
                    (recur (rest ls)
                           (inc n)
                           in-fence?
                           (update acc :headings conj [n (count hashes) (str/trim text)]))
                    (recur (rest ls) (inc n) in-fence? acc)))
      acc)))

(defn- lead-paragraph
  "PURE: the prose between a page's H1 and its first `##`."
  [^String md]
  (->> (str/split-lines md)
       (drop-while #(not (str/starts-with? % "# ")))
       (drop 1)
       (take-while #(not (str/starts-with? % "## ")))
       (str/join "\n")
       str/trim))

(defn- see-also-links
  "PURE: the relative page links under a page's `## See also` heading."
  [^String md]
  (let [tail (second (str/split md #"(?m)^## See also$" 2))]
    (re-seq #"\]\(([A-Za-z0-9._-]+\.md)" (str tail))))

(defn- use-cases
  "PURE: the top-level list items under a page's `## When to use` heading, up to
   the next `##`: the problems the page says it solves."
  [^String md]
  (let [tail (second (str/split md #"(?m)^## When to use$" 2))]
    (re-seq #"(?m)^[-*+] " (first (str/split (str tail) #"(?m)^## " 2)))))

(def ^:private variant-names
  "The variants a paired page can give, in the order a pair gives them."
  ["python" "http"])

(defn- variant-breaks
  "PURE: every way the variant blocks of `md` break the page contract, as
   reader-facing lines without the page name. Commonmark renders the Markdown in
   a block only after a blank line, and a heading in a block would give one
   variant a table-of-contents entry that the other hides."
  [^String md]
  (let [lines
        (map-indexed (fn [i line]
                       (assoc line :n (inc (long i))))
                     (dc/variant-lines md))

        body?
        (fn [{:keys [variant tag fenced?]}]
          (and variant (not tag) (not fenced?)))

        order
        (->> lines
             (keep (fn [{:keys [variant tag text]}]
                     (cond (= :open tag) variant
                           (and (nil? variant) (not (str/blank? text))) :shared)))
             (partition-by #{:shared})
             (mapcat #(if (= :shared (first %)) [:shared] %)))]

    (concat (for [{:keys [n tag variant]}
                  lines

                  :when (and (= :open tag) (not (some #{variant} variant-names)))]

              (str "line " n " opens the unknown variant " (pr-str variant)))
            (for [[a b]
                  (partition 2 1 lines)

                  :when (or (and (= :open (:tag a)) (not (str/blank? (:text b))))
                            (and (= :close (:tag b)) (not (str/blank? (:text a)))))]

              (str "line " (:n a) " needs a blank line between the variant tag and the Markdown"))
            (for [{:keys [n text] :as line}
                  lines

                  :when (and (body? line) (re-matches #"#{1,6} .*" text))]

              (str "line " n " is a heading inside a variant block"))
            (for [{:keys [n text] :as line}
                  lines

                  :when (and (body? line) (str/includes? text "<div data-variant="))]

              (str "line " n " opens a variant block inside another one"))
            (let [{:keys [variant tag]} (last lines)]
              (when (and variant (not= :close tag))
                ["a variant block is not closed at the end of the page"]))
            (for [[a b]
                  (partition 2 1 (concat [:shared] order [:shared]))

                  :when (or (and (= "python" a) (not= "http" b))
                            (and (= "http" b) (not= "python" a)))]

              (str "a " (pr-str a) " block is followed by " (pr-str b) ", not by its pair")))))

(defn- page-canon
  "PURE: every way `page` breaks the page contract, as reader-facing lines.
   `anchors` is `{slug #{anchor-id}}` for the whole site, so a cross-page
   fragment is checked against the toc of the page it points AT, and `pages` is
   the whole page list, which the landing page has to be a map of."
  [{:keys [slug title section intro? md blurb toc]} anchors pages]
  (let [home?
        (= "index" slug)

        {:keys [headings fences]}
        (scan md)

        h1s
        (filter (fn [[_ lvl _]]
                  (= 1 lvl))
                headings)

        h2-texts
        (keep (fn [[_ lvl text]]
                (when (= 2 lvl) text))
              headings)

        first-line
        (str/trim (str (first (remove str/blank? (str/split-lines md)))))

        ids
        (map :id toc)

        say
        (fn [& parts]
          (str slug ": " (apply str parts)))]

    (concat
      (if home?
        (concat (when (seq h1s)
                  [(say
                     "the landing page carries no H1 of its own — the themed hero is its title")])
                (for [{other-slug :slug other-title :title}
                      pages

                      :when (not= "index" other-slug)
                      :let [link
                            (str "[" other-title "](" other-slug ".md)")]
                      :when (not (str/includes? md link))]

                  (say "the landing page never links " link " — it is the map of this manual")))
        (concat
          (when-not (= 1 (count h1s)) [(say "wants exactly one H1, has " (count h1s))])
          (when-not (= (str "# " title) first-line)
            [(say "opens with " (pr-str first-line)
                  ", not with its manifest title " (pr-str (str "# " title)))])
          (when (< (count (lead-paragraph md)) 60)
            [(say "has no lead paragraph between its H1 and the first `##`")])
          (when (and section (not intro?) (not= "When to use" (first h2-texts)))
            [(say "opens on `## "
                  (first h2-texts)
                  "` — a page outside the intro module starts with `## When to use`")])
          (when (and section (not intro?) (< (count (use-cases md)) 2))
            [(say "`When to use` names fewer than two problems the page solves")])
          (when-not (= "See also" (last h2-texts))
            [(say "ends on `## " (last h2-texts) "` — the last `##` of a page is `See also`")])
          (when (< (count (see-also-links md)) 2)
            [(say "`See also` names fewer than two sibling pages")])))
      (for [[line lvl text]
            headings

            :when (> (long lvl) 3)]

        (say "line " line " is an h" lvl " (" text ") — too deep to be given an anchor"))
      (->> headings
           (map (fn [[_ lvl _]]
                  lvl))
           (partition 2 1)
           (keep (fn [[a b]]
                   (when (> (long b) (inc (long a))) (say "a heading jumps h" a " → h" b)))))
      (for [[id n]
            (frequencies ids)

            :when (> (long n) 1)]

        (say "anchor #" id " is claimed " n " times"))
      (for [[line lang]
            fences

            :when (not (contains? fence-languages lang))]

        (say "the fence on line " line
             " declares " (if (str/blank? lang) "no language" (pr-str lang))))
      (map say (prose/breaks md))
      (map say (variant-breaks md))
      (when (str/blank? (str blurb)) [(say "has no `:blurb` in vis-docs/site.edn")])
      (for [[_ target frag]
            (re-seq #"\]\((?!https?:|/|#)([A-Za-z0-9._-]+\.md)(#[A-Za-z0-9._-]+)?\)" md)

            :let [target-slug
                  (str/replace target #"\.md$" "")]
            :when (or (not (contains? anchors target-slug))
                      (and frag (not (contains? (get anchors target-slug) (subs frag 1)))))]

        (say "links to " target (str frag) ", which no page answers"))
      (for [[_ frag]
            (re-seq #"\]\((#[A-Za-z0-9._-]+)\)" md)

            :when (not (contains? (set ids) (subs frag 1)))]

        (say "links to " frag " on this page, which is not a heading here")))))

(defdescribe
  docs-page-canon-test
  "One canonical page shape, so the pages read as ONE manual instead of sixteen
   documents: the contract is stated in the `docs` ns docstring and checked here."
  (it "every page keeps it"
      (let [{:keys [pages]}
            (docs/collect)

            anchors
            (into {} (map (juxt :slug #(set (map :id (:toc %))))) pages)

            broken
            (mapcat #(page-canon % anchors pages) pages)]

        (expect (seq pages))
        (expect (empty? broken)
                (str/join "\n" (cons "pages that break the docs page contract:" broken))))))

(defn- canon-fixture
  "A minimal page in nav `section` whose first `##` is `first-h2`, listing `items`."
  [section first-h2 items]
  {:slug "fixture"
   :title "Fixture"
   :section section
   :intro? (= "Intro" section)
   :md (str "# Fixture\n\nA lead paragraph long enough to count as the page's introduction.\n\n## "
            first-h2
            "\n\n"
            (str/join "\n" (map #(str "- " %) items))
            "\n\n## See also\n")})

(defn- when-to-use-breaks
  "The page-contract lines `page` earns for its `When to use` section."
  [page]
  (filter #(str/includes? % "When to use") (page-canon page {} [])))

(defdescribe
  when-to-use-canon-test
  "A page outside the intro module opens with the problems it solves, so a reader
   who arrives with a problem learns first whether this is the right page. The
   intro module of Rationale and Getting started is exempt."
  (it "flags a module page that opens elsewhere or names fewer than two problems"
      (expect (seq (when-to-use-breaks (canon-fixture "Guides" "Install" ["a" "b"]))))
      (expect (seq (when-to-use-breaks (canon-fixture "Guides" "When to use" ["only one"])))))
  (it "accepts a module page that opens with two or more problems"
      (expect (empty? (when-to-use-breaks (canon-fixture "Guides" "When to use" ["a" "b"])))))
  (it "leaves the intro module alone"
      (expect (empty? (when-to-use-breaks (canon-fixture "Intro" "Rationale" []))))))

(defn- links-to?
  "True when `md` links the page `slug`."
  [md slug]
  (str/includes? md (str "(" slug ".md")))

(defdescribe
  variant-canon-test
  "A Feature API page gives each example as a Python block and then an HTTP block.
   Commonmark renders the Markdown in a block only after a blank line."
  (let [pair (str "<div data-variant=\"python\">\n\nPython text.\n\n</div>\n\n"
                  "<div data-variant=\"http\">\n\nHTTP text.\n\n</div>\n")]
    (it "accepts paired blocks with blank lines around their Markdown"
        (expect (empty? (variant-breaks (str "# Page\n\nLead.\n\n" pair "\nShared.\n\n" pair)))))
    (it "accepts the markup as an example inside fenced code"
        (expect (empty? (variant-breaks
                          "```markdown\n<div data-variant=\"ruby\">\n## Heading\n</div>\n```\n"))))
    (describe
      "flags"
      (it "Markdown right after a tag"
          (expect (seq (variant-breaks (str/replace-first pair "\">\n\n" "\">\n")))))
      (it "Markdown right before a closing tag"
          (expect (seq (variant-breaks
                         (str/replace-first pair "text.\n\n</div>" "text.\n</div>")))))
      (it "a heading inside a block"
          (expect (seq (variant-breaks (str/replace-first pair "Python text." "## Python")))))
      (it "an unknown variant"
          (expect (seq (variant-breaks (str/replace pair "\"http\"" "\"curl\"")))))
      (it "a Python block without its HTTP pair"
          (expect (seq (variant-breaks (str (first (str/split pair #"(?=<div data-variant=\"http)"))
                                            "\nShared.\n")))))
      (it "an HTTP block before its Python pair"
          (expect (seq (variant-breaks (str/join "\n" (reverse (str/split pair #"(?=<div)")))))))
      (it "a block that is never closed"
          (expect (seq (variant-breaks "<div data-variant=\"python\">\n\nText.\n")))))))

(defdescribe
  docs-modules-test
  "The manual reads in modules: the intro, the concepts, the programmatic access to
   those concepts and the guides built on both. A Feature API page gives each
   example in Python and as HTTP requests, so a reader changes between them with
   one switch."
  (it "orders the modules and the programmatic groups"
      (let [{:keys [pages]} (docs/collect)]
        (expect (= ["Intro" "Concepts" "Programmatic access" "Guides" "Extensions" "Reference"]
                   (distinct (map :section pages))))
        (expect (= ["rationale" "index"] (mapv :slug (filter :intro? pages))))
        (expect (= ["Basics" "Feature APIs"] (distinct (keep :group pages))))
        (expect (= ["python-sdk" "http-api"]
                   (mapv :slug (filter #(= "Basics" (:group %)) pages))))))
  (it "gives each concept that a program drives one API page with both variants"
      (let [{:keys [pages]}
            (docs/collect)

            concept-md
            (into {} (comp (filter #(= "Concepts" (:section %))) (map (juxt :slug :md))) pages)

            api-pages
            (filter #(= "Feature APIs" (:group %)) pages)]

        (expect (seq api-pages))
        (doseq [{:keys [slug variants]}
                api-pages

                :let [concept
                      (str/replace slug #"-api$" "")]]

          (expect (str/ends-with? slug "-api") slug)
          (expect (= variant-names variants) slug)
          (expect (links-to? (str (concept-md concept)) slug) concept))
        (expect (= (set (map :slug api-pages))
                   (set (map :slug (filter (comp seq :variants) pages))))
                "only Feature API pages give paired variants")))
  (it "names gateway routes only in HTTP blocks and on HTTP API basics"
      (doseq [{:keys [slug md]}
              (:pages (docs/collect))

              :when (not= "http-api" slug)]

        (expect (not (re-find #"\b(GET|POST|PUT|PATCH|DELETE) /" (dc/variant-text md "python")))
                slug)))
  (it "builds every guide on a concept page and a programmatic page"
      (let [{:keys [pages]}
            (docs/collect)

            slugs-of
            (fn [section]
              (map :slug (filter #(= section (:section %)) pages)))]

        (doseq [{:keys [slug section md]}
                pages

                :when (= "Guides" section)]

          (expect (some #(links-to? md %) (slugs-of "Concepts")) slug)
          (expect (some #(links-to? md %) (slugs-of "Programmatic access")) slug)))))

(defn- plain-english-breaks
  "The page-contract lines a page whose body is `prose` earns for its sentences."
  [prose]
  (filter #(re-find #"sentence|semicolon" %)
          (page-canon
            {:slug "fixture"
             :title "Fixture"
             :md
             (str
               "# Fixture\n\nA lead paragraph long enough to count as the page's introduction.\n\n"
               prose
               "\n\n## See also\n")}
            {}
            [])))

(defn- sentence-of "A sentence of `n` plain words." [n] (str (str/join " " (repeat n "word")) "."))

(defdescribe
  plain-english-canon-test
  "ASD-STE100 Simplified Technical English keeps a page readable with basic English
   and through translation tools, so a sentence over 25 words, a paragraph over six
   sentences or a semicolon between clauses breaks the page contract."
  (it "accepts a 25-word sentence and a six-sentence paragraph"
      (expect (empty? (plain-english-breaks (sentence-of 25))))
      (expect (empty? (plain-english-breaks (str/join " " (repeat 6 (sentence-of 3)))))))
  (it "flags a 26-word sentence and a seven-sentence paragraph"
      (expect (seq (plain-english-breaks (sentence-of 26))))
      (expect (seq (plain-english-breaks (str/join " " (repeat 7 (sentence-of 3)))))))
  (it "counts a code span as one word and a link as its text"
      (expect (empty? (plain-english-breaks (str
                                              "Run `vis sessions fork --at 3 --title copy` or read "
                                              "[Managing sessions](sessions.md#fork-a-session) "
                                              (sentence-of 19))))))
  (it "flags a semicolon between clauses, not one in code or in an HTML entity"
      (expect (seq (plain-english-breaks "The fork keeps its turns; the original stays.")))
      (expect (empty? (plain-english-breaks "Type `a; b` and a&nbsp;space.")))))

(defdescribe
  extension-center-public-link-test
  (it "links the catalog on the same origin only in the public site, never live docs or corpus"
      (let [site
            (assoc (docs/collect) :public? true)

            page
            (first (:pages site))]

        (let [header
              (re-find #"<header[^>]*>.*?</header>" (docs/page-html site page :static))

              link
              (re-find #"<a class=\"center-link\"[^>]*>.*?</a>" header)

              label
              (get-in site [:site :extension-center :title])]

          (expect (str/includes? link "href=\"/extensions/\""))
          ;; The header link is always the grid icon: the written-out name is its
          ;; title and aria-label, never visible text.
          (expect (str/includes? link "<svg "))
          (expect (str/includes? link (str "title=\"" label "\"")))
          (expect (str/includes? link (str "aria-label=\"" label "\"")))
          (expect (not (str/includes? link (str ">" label "<")))))
        (expect (not (str/includes? (docs/page-html site page :live) "href=\"/extensions/\"")))
        (expect (not-any? #(= "extension-center" (:slug %)) (:pages site))))))
