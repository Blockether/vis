(ns com.blockether.vis.internal.docs.core
  "Embedded documentation from the explicit records listed by
   `META-INF/vis/manifest.edn`.

   Each documentation record names one exact Markdown resource; no classpath
   enumeration or alternate docs manifest exists. The same records feed the live
   docs site and the sandbox `doc`/`apropos` surface, and how a page is TITLED,
   grouped and ordered — everything only this site reads — is `vis-docs/site.edn`.
   THE PAGE CONTRACT — one canonical shape for every page, enforced by
   `docs-test/docs-page-canon-test`:

     * The `:title` in `vis-docs/site.edn` IS the page's `# H1`, spelled
       identically, on the FIRST line of the file: the sidebar, the browser tab
       and the page itself must never disagree about a page's name. `index.md` is
       the ONE exception — its title is rendered from the site navigation, so it
       carries no `#` at all.
     * Under the H1 comes a LEAD paragraph, before the first `##`: what this page
       covers, so a reader who stops there still knows what they found.
     * `##` and `###` only. A deeper heading gets no `id` and no on-this-page
       entry (see `anchors+toc`), so nothing — not even this page — can link to it.
     * Anchors are unique within a page, and every relative `page.md#anchor` link
       resolves against the TARGET page's own toc.
     * Every fenced block declares a language, one of `bash`, `clojure`, `edn`,
       `ini`, `json`, `markdown`, `python`, `text`, `toml`, `yaml`.
     * `index.md` is the MAP: it links every other page under `## Learn more`,
       with that page's title as the link text, so a page nobody can reach from
       the landing page does not exist for a reader.
     * The last `##` of every page is `See also` — two or more sibling pages, each
       with the reason to follow it. That web is what keeps ONE topic in ONE page:
       a topic explained twice is a cross-link somebody never wrote.
     * Every page carries a `:blurb` in `vis-docs/site.edn`, the one sentence the
       sidebar and the index cards show.
     * NO WALL OF TEXT: one paragraph — or one list item with its continuation
       lines — stays under 800 characters. Past that the reader is handed a
       table or a list as prose, and the structure is usually already in the
       sentence (`A; B; C`, `first … then … finally`): write it as the list it is.

   One renderer, two outputs:
     * `build-site!` writes a static, themed HTML bundle for the public Worker.
     * `handle` serves the same pages live (HTMX nav), mountable on the gateway
       via its `:gateway.slot/http-routes` slot.

   Markdown → HTML uses commonmark-java. Static and live pages share a
   responsive layout with navigation, article content and a table of contents."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.corpus :as doc-corpus]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip GZIPOutputStream]
           [org.commonmark.parser Parser]
           [org.commonmark.renderer.html HtmlRenderer]
           [org.commonmark.ext.gfm.tables TablesExtension]
           [org.commonmark.ext.gfm.strikethrough StrikethroughExtension]))

(set! *warn-on-reflection* true)

;; commonmark markdown -> HTML

(def ^:private extensions [(TablesExtension/create) (StrikethroughExtension/create)])

(def ^:private ^Parser md-parser
  (-> (Parser/builder)
      (.extensions extensions)
      (.build)))

(def ^:private ^HtmlRenderer md-renderer
  (-> (HtmlRenderer/builder)
      (.extensions extensions)
      (.build)))

(defn md->html
  ^String [^String md]
  (.render ^HtmlRenderer md-renderer (.parse ^Parser md-parser md)))

(defn- first-h1
  [^String md]
  (some->> (str/split-lines md)
           (some (fn [l]
                   (when (str/starts-with? l "# ") (str/trim (subs l 2)))))))

(defn- strip-tags
  ^String [^String s]
  (-> s
      (str/replace #"<[^>]+>" "")
      (str/replace "&amp;" "&")
      (str/replace "&lt;" "<")
      (str/replace "&gt;" ">")
      str/trim))

(defn- slugify
  [^String s]
  (-> (strip-tags s)
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn- anchors+toc
  "Inject id= on h2/h3 and return [html-with-ids toc] where toc is
   [{:level 2|3 :id :text} …] — the right-rail 'on this page' index."
  [^String html]
  (let [toc
        (atom [])

        html'
        (str/replace html
                     #"<h([23])>(.*?)</h[23]>"
                     (fn [[_ lvl inner]]
                       (let [id (slugify inner)]
                         (swap! toc conj {:level (parse-long lvl) :id id :text (strip-tags inner)})
                         (str "<h"
                              lvl
                              " id=\""
                              id
                              "\">"
                              "<a class=\"anchor\" href=\"#"
                              id
                              "\">"
                              inner
                              "</a></h"
                              lvl
                              ">"))))]

    [html' @toc]))

 ;; explicit documentation records

(def ^:private site-resource
  "The docs site navigation and metadata. Not a searchable document."
  "vis-docs/site.edn")

(defn- site-file
  "`{:site {...} :nav [{:section _ :pages [{:page :title :blurb} ...]} ...]}`, read
   from `site-resource` and checked. The nav VECTOR is the order — a page's place in
   the manual is where it stands here, so there is no order number to keep in step."
  []
  (let [url
        (or (io/resource site-resource)
            (throw (ex-info "Missing docs site resource"
                            {:type ::missing-site :resource site-resource})))

        parsed
        (edn/read-string
          {:default (fn [tag _]
                      (throw (ex-info "Tagged literal in the docs site resource"
                                      {:type ::tagged-literal :resource site-resource :tag tag})))}
          (slurp url))]

    (when-not (and (map? parsed)
                   (map? (:site parsed))
                   (vector? (:nav parsed))
                   (every? #(and (map? %) (vector? (:pages %))) (:nav parsed)))
      (throw (ex-info "Malformed docs site resource"
                      {:type ::malformed-site :resource site-resource})))
    parsed))

(defn- collect*
  []
  (let [{:keys [site nav]}
        (site-file)

        by-name
        (into {} (map (juxt :name identity)) (doc-corpus/pages))

        pages
        (into []
              (for [{section :section group :pages}
                    nav

                    {:keys [page title blurb]}
                    group]

                (let [record
                      (or (get by-name page)
                          (throw (ex-info "The docs site navigates to a page no record carries"
                                          {:type ::unknown-page :page page})))

                      md
                      (str (:text record))

                      [html toc]
                      (anchors+toc (md->html md))]

                  {:slug page
                   :title (or title (first-h1 md) page)
                   :section section
                   :blurb blurb
                   :md md
                   :html html
                   :toc toc})))]

    (when-let [unreachable (seq (remove (set (map :slug pages)) (keys by-name)))]
      (throw (ex-info "A documentation page the site never navigates to"
                      {:type ::unnavigated-page :pages (vec unreachable)})))
    {:site site :pages pages}))

(defonce ^:private rendered (delay (collect*)))

(defn collect
  "The whole site: every documentation record the corpus read, rendered to HTML
   with anchors and a table of contents, pages in manifest order within their
   sections. Rendered ONCE — the records are read at load and a distribution's
   documents cannot change under a running process, so there is nothing to
   invalidate and no freshness check to pay for."
  []
  @rendered)

;; theme (VIS palette) — enterprise docs layout

(defn- asset
  "Rooted URL to a docs asset, correct from any page depth.
   :live  → \"/docs/assets/<rel>\"  (absolute, survives nested page paths)
   :static → assets/<rel>         (index and pages at the static site root)"
  [mode rel]
  (case mode
    :static
    (str "assets/" rel)

    :live
    (str "/docs/assets/" rel)))

(defn- theme-css
  "The stylesheet shared with Extension Center, with font URLs rooted for `mode`."
  [mode]
  (str/replace (slurp (io/resource "vis-docs/assets/theme.css")) "./fonts/" (asset mode "fonts/")))

(defn- esc
  ^String [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

;; :static → relative ("slug.html"), so the bundle works from any host/subpath
;; of a static host. :live → ABSOLUTE ("/docs/slug"), so nav resolves the same
;; from the index (/docs) AND from a deep page (/docs/<slug>); a relative href
;; would resolve to /docs/docs/<slug> on deep pages → 404 "no such doc".
(defn- href
  [mode slug]
  (case mode
    :static
    (str slug ".html")

    :live
    (str "/docs/" slug)))

(defn- rewrite-md-links
  "Cross-page links in rendered page BODIES. Authors write plain relative
   markdown links (`[Skills](skills.md)`, `[X](configuration.md#router)`) —
   commonmark emits them verbatim, which 404s in BOTH modes (live serves
   `/docs/<slug>`, static serves `<slug>.html`). Rewrite every RELATIVE
   `*.md` href through the same mode-aware `href` the sidebar nav uses;
   absolute URLs (scheme or leading `/`) pass through untouched."
  ^String [^String html mode]
  (-> html
      (str/replace #"href=\"([^\"#:/][^\":]*?)\.md(#[^\"]*)?\""
                   (fn [[_ slug frag]]
                     (str "href=\"" (href mode slug) (or frag "") "\"")))
      (str/replace #"(src|href)=\"assets/([^\"]+)\""
                   (fn [[_ attr rel]]
                     (str attr "=\"" (asset mode rel) "\"")))))

(defn- nav-html
  [{:keys [pages site public?]} active-slug mode]
  (let [by-sec
        (group-by :section pages)

        sections
        (cons nil (distinct (remove nil? (map :section pages))))]

    (str "<nav class=\"nav\">"
         (apply str
           (for [sec
                 sections

                 :let [ps
                       (get by-sec sec)]
                 :when (seq ps)]

             (str (when sec (str "<div class=\"nav-sec\">" (esc sec) "</div>"))
                  (apply str
                    (for [{:keys [slug title]} ps]
                      (str "<a href=\""
                           (href mode slug)
                           "\""
                           (when (= slug active-slug) " class=\"active\"")
                           ">"
                           (esc title)
                           "</a>"))))))
         (when (and (= mode :static) public?)
           (str "<div class=\"nav-sec\">"
                (esc (get-in site [:extension-center :section]))
                "</div><a href=\"/extensions/\">"
                (esc (get-in site [:extension-center :title]))
                "</a>"))
         "</nav>")))

(defn- toc-html
  [toc]
  (when (seq toc)
    (str "<aside class=\"toc\"><div class=\"lbl\">On this page</div>"
         (apply str
           (for [{:keys [level id text]} toc]
             (str "<a class=\"lvl-" level "\" href=\"#" id "\">" (esc text) "</a>")))
         "</aside>")))

(def ^:private prism-js (delay (slurp (io/resource "vis-transcript/prism.min.js"))))

(defn page-html
  "Full HTML document for one page. `mode` ∈ #{:static :live}."
  [{:keys [site] :as site-data} {:keys [slug title html toc] :as _page} mode]
  (let [home? (= slug "index")]
    (str
      "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1,viewport-fit=cover\">"
      "<title>"
      (esc title)
      " · "
      (esc (:title site))
      "</title>"
      "<meta name=\"description\" content=\""
      (esc (:tagline site))
      "\">"
      "<link rel=\"preload\" href=\""
      (asset mode "fonts/jetbrains-mono.woff2")
      "\" as=\"font\" type=\"font/woff2\" crossorigin>"
      (if (= mode :static)
        "<link rel=\"stylesheet\" href=\"assets/theme.css\">"
        (str "<style>" (theme-css mode) "</style>"))
      "</head><body>"
      ;; CSS-only mobile nav toggle (checkbox precedes .shell so it can target .side)
      "<input type=\"checkbox\" id=\"navtoggle\" class=\"navtoggle\" aria-label=\"Toggle navigation\">"
      "<header class=\"top\">"
      "<label for=\"navtoggle\" class=\"hamburger\" title=\"Menu\"><span></span><span></span><span></span></label>"
      "<a class=\"brand\" href=\""
      (href mode "index")
      "\" title=\""
      (esc (:title site))
      "\" aria-label=\""
      (esc (:title site))
      "\">"
      (esc (:title site))
      "</a>"
      (when (and (= mode :static) (:public? site-data))
        (str "<a class=\"center-link\" href=\"/extensions/\">"
             (esc (get-in site [:extension-center :title]))
             "</a>"))
      "<span class=\"spacer\"></span>"
      (when-let [r (:repo site)]
        (str
          "<a class=\"gh\" href=\"" (esc r)
          "\" title=\"GitHub\" aria-label=\"GitHub\" target=\"_blank\" rel=\"noopener\">"
          "<svg width=\"20\" height=\"20\" viewBox=\"0 0 16 16\" fill=\"currentColor\" aria-hidden=\"true\">"
          "<path d=\"M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z\"/>"
          "</svg></a>"))
      "</header>"
      ;; body grid
      "<label for=\"navtoggle\" class=\"scrim\"></label>"
      "<div class=\"shell\"><aside class=\"side\">"
      "<div class=\"tagline\">"
      (esc (:tagline site))
      "</div>"
      (nav-html site-data slug mode)
      "</aside>"
      "<main class=\"main\"><article class=\"content\">"
      (when home? (str "<h1>" (esc title) "</h1>"))
      (rewrite-md-links html mode)
      "<div class=\"foot\">"
      "<a class=\"bk\" href=\"https://blockether.com\" title=\"Blockether\">"
      "<img class=\"bk-mark\" src=\""
      (asset mode "blockether.png")
      "\" alt=\"Blockether\"></a>"
      "<span class=\"spacer\"></span>"
      (when-let [r (:repo site)]
        (str "<a href=\"" (esc r) "\">Edit on GitHub ↗</a>"))
      "</div>"
      "</article></main>"
      (or (toc-html toc) "<div></div>")
      "</div>"
      (if (= mode :static)
        "<script src=\"assets/prism.min.js\" defer></script><script src=\"assets/docs.js\" defer></script>"
        (str "<script>" @prism-js "\nPrism.highlightAll();</script>"))
      "</body></html>")))

;; static site

(def ^:private asset-files
  {"vis-docs/assets/logo.png" "assets/logo.png"
   "vis-docs/assets/blockether.png" "assets/blockether.png"
   "vis-docs/assets/install-testflight.png" "assets/install-testflight.png"
   "vis-docs/assets/install-google-play.png" "assets/install-google-play.png"
   "vis-docs/assets/install-macos.png" "assets/install-macos.png"
   "vis-docs/assets/install-linux.png" "assets/install-linux.png"
   "vis-docs/assets/screenshots/ask.png" "assets/screenshots/ask.png"
   "vis-docs/assets/screenshots/live-running.png" "assets/screenshots/live-running.png"
   "vis-docs/assets/screenshots/live-stop.png" "assets/screenshots/live-stop.png"
   "vis-docs/assets/screenshots/nesting-finding.png" "assets/screenshots/nesting-finding.png"
   "vis-docs/assets/screenshots/nesting-clear.png" "assets/screenshots/nesting-clear.png"
   "vis-docs/assets/screenshots/ios-conversation.png" "assets/screenshots/ios-conversation.png"
   "vis-docs/assets/screenshots/ios-sessions.png" "assets/screenshots/ios-sessions.png"
   "vis-docs/assets/screenshots/ios-project.png" "assets/screenshots/ios-project.png"
   "vis-docs/assets/screenshots/desktop-conversation.png"
   "assets/screenshots/desktop-conversation.png"
   "vis-docs/assets/screenshots/desktop-project.png" "assets/screenshots/desktop-project.png"
   "vis-docs/assets/screenshots/desktop-release.png" "assets/screenshots/desktop-release.png"
   "vis-docs/assets/screenshots/tui-conversation.png" "assets/screenshots/tui-conversation.png"
   "vis-docs/assets/screenshots/tui-sessions.png" "assets/screenshots/tui-sessions.png"
   "vis-docs/assets/screenshots/tui-project.png" "assets/screenshots/tui-project.png"
   "vis-docs/assets/diagrams/council-messages.svg" "assets/diagrams/council-messages.svg"
   "vis-docs/assets/diagrams/council-messages.mmd" "assets/diagrams/council-messages.mmd"
   "vis-docs/assets/diagrams/council-modules.svg" "assets/diagrams/council-modules.svg"
   "vis-docs/assets/diagrams/council-modules.mmd" "assets/diagrams/council-modules.mmd"
   "vis-docs/assets/fonts/jetbrains-mono.woff2" "assets/fonts/jetbrains-mono.woff2"
   "vis-docs/assets/theme.css" "assets/theme.css"
   "vis-docs/assets/docs.js" "assets/docs.js"
   "vis-transcript/prism.min.js" "assets/prism.min.js"})

(defn- copy-assets!
  [out-dir]
  (doseq [[res out] asset-files]
    (when-let [u (io/resource res)]
      (let [f (io/file out-dir out)]
        (io/make-parents f)
        (with-open [in (io/input-stream u)]
          (io/copy in f))))))

(defn build-site!
  "Render the docs and shared assets to a static bundle. :public? adds same-origin
   Extension Center navigation; the catalog is not a document or part of live docs."
  ([out-dir] (build-site! out-dir {}))
  ([out-dir {:keys [public?]}]
   (let [{:keys [pages] :as site-data} (assoc (collect) :public? public?)]
     (when (empty? pages) (throw (ex-info "no vis-docs pages found on classpath" {})))
     (io/make-parents (io/file out-dir "x"))
     (copy-assets! out-dir)
     (doseq [{:keys [slug] :as page} pages]
       (spit (io/file out-dir (str slug ".html")) (page-html site-data page :static)))
     (let [home (or (first (filter #(= "index" (:slug %)) pages)) (first pages))]
       (spit (io/file out-dir "index.html") (page-html site-data home :static)))
     {:out out-dir :pages (mapv :slug pages)})))

;; live serving — Ring handler for the gateway `:gateway.slot/http-routes` slot.

;; `collect` is memoized on a stat pass, so the handler simply calls it: an
;; edit under `resources/vis-docs` shows on the next refresh and an unchanged
;; tree costs no re-render.

(defn- gzip-bytes
  ^bytes [^String s]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [gz (GZIPOutputStream. baos)]
      (.write gz (util/utf8 s)))
    (.toByteArray baos)))

(defn- ok-html
  "HTML response, gzipped when the client advertises support (the inline CSS
   makes the doc ~12 KB; gzip ~4×). Assets are already immutable-cached; the
   doc is intentionally NOT cache-tagged so a docs edit shows on refresh."
  ([body] (ok-html body nil))
  ([body accept-encoding]
   (let [gz? (and accept-encoding (str/includes? (str/lower-case accept-encoding) "gzip"))]
     {:status 200
      :headers (cond-> {"content-type" "text/html; charset=utf-8"}
                 gz?
                 (assoc "content-encoding"
                   "gzip" "vary"
                   "Accept-Encoding"))
      :body (if gz? (gzip-bytes body) body)})))

(defn- asset-response
  [^String rel]
  (when-let [u (io/resource (str "vis-docs/assets/" rel))]
    (let [ct (cond (str/ends-with? rel ".woff2") "font/woff2"
                   (str/ends-with? rel ".png") "image/png"
                   (str/ends-with? rel ".svg") "image/svg+xml"
                   (str/ends-with? rel ".mmd") "text/plain; charset=utf-8"
                   :else "application/octet-stream")]
      {:status 200
       :headers {"content-type" ct "cache-control" "public,max-age=31536000,immutable"}
       :body (io/input-stream u)})))

(defn handle
  "Ring handler for the docs site. Returns nil for paths it does not own (so the
   gateway can fall through). Owns `/docs`, `/docs/<slug>`, `/docs/assets/**`."
  [{:keys [uri headers] :or {uri ""}}]
  (let [{:keys [pages] :as site-data}
        (collect)

        path
        (-> uri
            (str/replace #"^/docs/?" "")
            (str/replace #"/$" ""))

        accept-encoding
        (get headers "accept-encoding")]

    (cond (str/starts-with? path "assets/") (asset-response (subs path (count "assets/")))
          (#{"gateway" "gateway.md" "gateway.html"} path)
          {:status 301 :headers {"location" "/docs"} :body ""}
          (or (= path "") (= path "index"))
          (ok-html (page-html site-data
                              (or (first (filter #(= "index" (:slug %)) pages)) (first pages))
                              :live)
                   accept-encoding)
          :else (or (when-let [page (first (filter #(= path (:slug %)) pages))]
                      (ok-html (page-html site-data page :live) accept-encoding))
                    ;; Tolerate literal `<slug>.md` deep links (old bookmarks, raw
                    ;; markdown cross-links) — permanent-redirect to the slug route.
                    (when (str/ends-with? path ".md")
                      (let [slug (str/replace path #"\.md$" "")]
                        (when (some #(= slug (:slug %)) pages)
                          {:status 301 :headers {"location" (str "/docs/" slug)} :body ""})))))))
