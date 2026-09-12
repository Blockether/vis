(ns com.blockether.vis.internal.docs.core-test
  "Documentation rendering, navigation, supported features and Python examples."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.core :as docs]
            [lazytest.core :refer [defdescribe expect it]]))

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
              (do (expect (str/includes? html "Prism.languages.python="))
                  (expect (str/includes? html "Prism.highlightAll();</script>"))))
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
          (expect (str/includes? html "<thead>"))
          (expect (str/includes? html "initial-scale=1,viewport-fit=cover"))
          (expect (not (re-find #"user-scalable=no|maximum-scale=" html)))))))

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
  (it "keeps motivation next to getting started without a separate app guide"
      (let [slugs (mapv :slug (:pages (docs/collect)))]
        (expect (= ["index" "motivation"] (subvec slugs 0 2)))
        (expect (not (some #{"gateway"} slugs)))
        (expect (nil? (io/resource "vis-docs/gateway.md")))
        (expect (< (.indexOf slugs "queue-and-cancel") (.indexOf slugs "python-sandbox")))))
  (it "keeps app setup and gateway reference in the landing page"
      (let [{:keys [pages] :as site}
            (docs/collect)

            home
            (first (filter #(= "index" (:slug %)) pages))

            md
            (:md home)]

        (doseq [mode
                [:static :live]

                :let [html
                      (docs/page-html site home mode)]
                anchor
                ["connecting-the-companion-app" "connect-the-desktop-app" "pair-a-phone"
                 "access-from-anywhere-with-tailscale" "gateway-reference" "starting-the-gateway"
                 "using-a-remote-gateway-from-the-cli" "tokens-and-http-401" "http-api" "python-sdk"
                 "resource-limits" "see-also"]]

          (expect (str/includes? html (str "id=\"" anchor "\"")) anchor))
        (doseq [content ["vis-agent gateway start --host 127.0.0.1"
                         "vis-agent gateway start --host 10.0.0.5 --require-token --pair"
                         "vis-agent gateway pair" "vis-agent gateway stop --if-idle"
                         "VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "HTTP 401" "HTTP 426"
                         "VIS_GATEWAY_MAX_CONCURRENT_TURNS" "VIS_GATEWAY_EVENT_RING_MAX"
                         "VIS_ENV_CACHE_MAX" "VIS_ENV_MAX_TURNS_PER_CTX" "VIS_ENV_RSS_BUDGET_MB"
                         "GatewayClient" "LocalEngine" "does not encrypt HTTP"
                         "Stopping a busy gateway interrupts"]]
          (expect (str/includes? md content) content))
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

;; Regression: quick links inherited paragraph justification and split at separators on mobile.
;; Motivation must open the full guide, not jump to the homepage summary.
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

        (expect (= [[(if (= mode :static) "motivation.html" "/docs/motivation") "Motivation"]
                    ["#install" "Install"] ["#first-session" "First session"]
                    ["#connecting-the-companion-app" "Desktop and mobile"]]
                   links))
        (expect (not (str/includes? (or navigation "") "·")))
        (expect (not (str/includes? html "<p><nav class=\"quick-links\"")))
        (let [link-css (second (re-find #"(?s)(?:^|\})\s*\.quick-links a\s*\{([^}]+)\}" css))]
          (doseq [fragment ["min-height: 2.75rem" "white-space: nowrap"]]
            (expect (str/includes? (or link-css "") fragment) fragment)))
        (expect (re-find #"\.quick-links\s*\{\s*display: flex;\s*flex-wrap: wrap;\s*gap: 0\.5rem"
                         css))
        (expect (re-find #"\.quick-links a\s*\{\s*flex-basis: calc\(50% - 0\.25rem\);\s*\}" css))
        (doseq [fragment [".quick-links a:hover" "a:focus-visible"]]
          (expect (str/includes? css fragment) fragment))))))

(defdescribe
  portable-store-buttons-test
  ;; GitHub does not load the docs stylesheet: linked images must carry the labels.
  (it
    "keeps mobile and latest-release desktop buttons readable without the documentation stylesheet"
    (doseq [[source prefix]
            [[(io/file "README.md") "resources/vis-docs/"] [(io/resource "vis-docs/index.md") ""]]

            [name url label]
            [["testflight" "https://testflight.apple.com/join/4anYT4Wk"
              "TestFlight for iOS and iPadOS"]
             ["google-play" "https://play.google.com/apps/testing/com.blockether.viscompanion"
              "Google Play beta for Android"]
             ["macos" "https://github.com/Blockether/vis/releases/latest"
              "Latest desktop release for macOS"]
             ["linux" "https://github.com/Blockether/vis/releases/latest"
              "Latest desktop release for Linux"]]]

      (let [md
            (slurp source)

            body
            (some (fn [[_ attrs contents]]
                    (when (and (str/includes? attrs (str "href=\"" url "\""))
                               (str/includes? contents (str "assets/install-" name ".png")))
                      contents))
                  (re-seq #"(?s)<a\b([^>]*)>(.*?)</a>" md))]

        (expect (str/includes? (or body "") (str "src=\"" prefix "assets/install-" name ".png\"")))
        (expect (str/includes? (or body "") (str "alt=\"" label "\"")))
        (expect (str/includes? (or body "") "width=\"224\" height=\"56\"")))))
  (it "serves and exports all mobile and desktop image buttons"
      (doseq [name ["testflight" "google-play" "macos" "linux"]]
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

(defdescribe handle-md-redirect-test
             (it "GET /docs/<slug>.md permanent-redirects to /docs/<slug>"
                 (let [resp (docs/handle {:uri "/docs/skills.md" :headers {}})]
                   (expect (= 301 (:status resp)))
                   (expect (= "/docs/skills" (get-in resp [:headers "location"])))))
             (it "redirects old app-guide URLs to getting started without replacing their fragments"
                 (doseq [uri ["/docs/gateway" "/docs/gateway/" "/docs/gateway.md"
                              "/docs/gateway.html"]]
                   (expect (= {:status 301 :headers {"location" "/docs"} :body ""}
                              (docs/handle {:uri uri :headers {}}))
                           uri)))
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
                        "Log terminal controls display as visible escapes"
                        "other text stays literal" "`Escape` or `Enter` confirms"]]
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
  #{"bash" "clojure" "edn" "ini" "json" "markdown" "python" "text" "toml" "yaml"})

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

(def ^:private max-unit-chars
  "Characters one paragraph — or one list item with its continuation lines — may
   carry before it stops being prose and becomes a table nobody drew. Roughly 120
   words: past that a reader scans instead of reading, and the structure is
   already inside the sentence."
  800)

(defn- text-units
  "PURE: `[[line text] …]` — every prose paragraph of `md`, plus every list item
   with the lines that continue it, joined into one string. Fenced blocks,
   headings, tables and quotes carry their own shape and are skipped."
  [^String md]
  (let [close (fn [acc {:keys [line buf]}]
                (if (seq buf) (conj acc [line (str/join " " buf)]) acc))]
    (loop [ls (str/split-lines md)
           n 1
           in-fence? false
           cur {:line 0 :buf []}
           acc []]

      (if (empty? ls)
        (close acc cur)
        (let [l (str/trim (str (first ls)))
              item? (boolean (re-matches #"(?s)([-*+]|\d+[.)])\s.*" l))
              skip? (or (str/blank? l)
                        (str/starts-with? l "#")
                        (str/starts-with? l "|")
                        (str/starts-with? l ">"))]

          (cond (str/starts-with? l "```")
                (recur (rest ls) (inc n) (not in-fence?) {:line 0 :buf []} (close acc cur))
                in-fence? (recur (rest ls) (inc n) in-fence? cur acc)
                skip? (recur (rest ls) (inc n) in-fence? {:line 0 :buf []} (close acc cur))
                item? (recur (rest ls) (inc n) in-fence? {:line n :buf [l]} (close acc cur))
                :else (recur (rest ls)
                             (inc n)
                             in-fence?
                             (if (seq (:buf cur)) (update cur :buf conj l) {:line n :buf [l]})
                             acc)))))))

(defn- page-canon
  "PURE: every way `page` breaks the page contract, as reader-facing lines.
   `anchors` is `{slug #{anchor-id}}` for the whole site, so a cross-page
   fragment is checked against the toc of the page it points AT, and `pages` is
   the whole page list, which the landing page has to be a map of."
  [{:keys [slug title md blurb toc]} anchors pages]
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
      (for [[line text]
            (text-units md)

            :when (> (count text) (long max-unit-chars))]

        (say "the paragraph on line "
             line
             " runs "
             (count text)
             " characters — over "
             max-unit-chars
             ", so it is a list or a table wearing prose"))
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

(defdescribe
  extension-center-public-link-test
  (it "links the catalog on the same origin only in the public site, never live docs or corpus"
      (let [site
            (assoc (docs/collect) :public? true)

            page
            (first (:pages site))]

        (expect (str/includes? (re-find #"<header[^>]*>.*?</header>"
                                        (docs/page-html site page :static))
                               "href=\"/extensions/\""))
        (expect (not (str/includes? (docs/page-html site page :live) "href=\"/extensions/\"")))
        (expect (not-any? #(= "extension-center" (:slug %)) (:pages site))))))
