(ns com.blockether.vis.internal.docs.core-test
  "Documentation rendering, navigation, supported features and Python examples."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.core :as docs]
            [com.blockether.vis.internal.docs.corpus :as dc]
            [com.blockether.vis.test-prose :as prose]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  python-presentation-test
  (it
    "resolves screenshot sources and full-size links in both modes"
    (let [{:keys [pages] :as site} (docs/collect)]
      (doseq [[slug image] [["human-input" "ask"] ["live-views" "live-running"]
                            ["live-views" "live-stop"] ["extension-design" "nesting-finding"]
                            ["extension-design" "nesting-clear"] ["index" "ios-conversation"]
                            ["index" "ios-sessions"] ["index" "ios-project"]
                            ["index" "desktop-conversation"] ["index" "desktop-settings"]
                            ["index" "desktop-release"] ["index" "tui-conversation"]
                            ["index" "tui-sessions"] ["index" "tui-settings"]]
              mode [:static :live]
              :let [page (first (filter #(= slug (:slug %)) pages))
                    html (docs/page-html site page mode)
                    path (str (when (= mode :live) "/docs/") "assets/screenshots/" image ".png")]]

        (expect (str/includes? html (str "src=\"" path "\"")))
        (expect (str/includes? html (str "href=\"" path "\"")))))))

(defdescribe
  dropdown-assets-test
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
               "ios-sessions" "ios-project" "desktop-conversation" "desktop-settings"
               "desktop-release" "tui-conversation" "tui-sessions" "tui-settings"]

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
            (when (= ext "svg")
              (expect (str/includes? text "<svg"))
              (expect (not (str/includes? text "<script"))))))
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

(defdescribe
  portable-store-buttons-test
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

(defn- links-to?
  "True when `md` links the page `slug`."
  [md slug]
  (str/includes? md (str "(" slug ".md")))

(defdescribe
  docs-modules-test
  "The manual reads in modules: the intro, the concepts, the programmatic access to
   those concepts, the extensions and the reference. A Feature API page gives each
   example in Python and as HTTP requests, so a reader changes between them with
   one switch."
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
                "only Feature API pages give paired variants"))))
