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
     * `build-site!` writes a static, themed HTML bundle (for GitHub Pages).
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

(def ^:private font-tokens {"jetbrains-mono.woff2" "FONTPATH_jbm"})

(defn- asset
  "Rooted URL to a docs asset, correct from any page depth.
   :live  → \"/docs/assets/<rel>\"  (absolute, survives nested page paths)
   :static → \"assets/<rel>\"         (GitHub Pages: index at site root)"
  [mode rel]
  (case mode
    :static
    (str "assets/" rel)

    :live
    (str "/docs/assets/" rel)))

(defn- theme-css
  "The theme stylesheet with font URLs rooted for `mode`. Tokens in the CSS
   base are replaced with rooted asset paths so fonts load from any page depth."
  [mode]
  (let
    [base
     "
:root{
  --bg:#fff; --bg-soft:#f8f8f8; --panel:#f8f8f8; --header:rgba(255,255,255,.82);
  --fg:#1e1e1e; --fg-soft:#1e1e1e; --dim:#505050; --faint:#666;
  --line:#e8e8e8; --line-soft:#f0f0f0;
  --primary:#2563eb; --primary-press:#0a32a0; --accent:#2563eb;
  --link:#1e5ac8; --link-hover:#0a32a0;
  --gold:#2563eb; --gold-deep:#1d4ed8; --amber:#4b5563; --amber-deep:#334155;
  --success:#28a03c; --warning:#b59100; --danger:#dc3232; --info:var(--primary);
  --code-bg:#f0f3f8; --code-fg:#1e1e1e; --sel:#dbe7fc;
  --radius:0; --r-sm:0; --measure:44rem; --maxw:88rem;
  --font:'JetBrains Mono',monospace;
  --text-body:.9375rem; --text-small:.8125rem; --text-heading:1.125rem; --text-title:1.75rem;
  --shadow:0 1px 2px rgba(30,30,30,.05);
  --ease-out: cubic-bezier(0.25,1,0.5,1);
  --ease-decisive: cubic-bezier(0.16,1,0.3,1);
}
@font-face{font-family:'JetBrains Mono';font-weight:100 800;font-display:swap;font-style:normal;src:url(FONTPATH_jbm) format('woff2')}
*{box-sizing:border-box}
html{scroll-behavior:smooth;scroll-padding-top:5.5rem;scrollbar-gutter:stable;
  -webkit-text-size-adjust:100%;text-size-adjust:100%}
body{margin:0;background:var(--bg);color:var(--fg-soft);font-family:var(--font);
  font-size:var(--text-body);line-height:1.65;-webkit-font-smoothing:antialiased;
  text-rendering:optimizeLegibility}
p{margin:1rem 0}
::selection{background:var(--sel)}
a{color:var(--link);text-decoration:underline;text-decoration-color:rgba(37,99,235,.35);
  text-underline-offset:2px;transition:color .15s var(--ease-out),text-decoration-color .15s var(--ease-out)}
a:hover{color:var(--link-hover);text-decoration-color:var(--link-hover)}
/* sticky header */
.top{position:sticky;top:0;z-index:50;height:4rem;display:flex;align-items:center;
  gap:1rem;padding:0 clamp(1rem,3vw,2rem);background:var(--header);
  backdrop-filter:saturate(160%) blur(10px);border-bottom:1px solid var(--line)}
.top .brand{display:flex;align-items:center;gap:.6rem;font-weight:700;font-size:var(--text-heading);
  letter-spacing:-.02em;color:var(--fg)}
.top .brand .dot{width:.85rem;height:.85rem;border-radius:50%;background:var(--primary);
  box-shadow:0 0 0 3px #dbe7fc,0 1px 3px rgba(30,30,30,.15)}
.top .spacer{flex:1}
.top .gh{display:inline-flex;align-items:center;color:var(--dim);transition:color .12s}
.top .gh:hover{color:var(--link-hover)}
.top .gh svg{display:block}
.shell{max-width:var(--maxw);margin:0 auto;display:grid;
  grid-template-columns:16rem minmax(0,1fr) 15rem;gap:0}
/* sidebar */
.side{position:sticky;top:4rem;align-self:start;height:calc(100vh - 4rem);overflow-y:auto;
  padding:2rem 1.1rem 3rem;border-right:1px solid var(--line-soft)}
.side .tagline{color:var(--dim);font-size:var(--text-small);line-height:1.5;margin:0 .3rem 1.4rem;
  padding-bottom:1.2rem;border-bottom:1px solid var(--line-soft)}
.nav-sec{color:var(--amber-deep);font-size:var(--text-small);font-weight:700;margin:1.5rem .6rem .5rem}
.nav a{display:block;padding:.34rem .7rem;border-radius:0;color:var(--fg-soft);
  font-size:var(--text-small);font-weight:500;transition:background .12s,color .12s}
.nav a:hover{background:var(--panel);color:var(--fg)}
.nav a.active{background:#eef3fe;
  color:var(--primary-press);font-weight:600;box-shadow:inset 2px 0 0 var(--primary)}
/* content */
.main{padding:3.4rem clamp(1.2rem,4vw,3.5rem) 5rem;min-width:0}
.content{max-width:var(--measure);overflow-wrap:anywhere}
.content h1{font-size:var(--text-title);line-height:1.25;letter-spacing:-.02em;
  margin:0 0 1.1rem;font-weight:700;text-wrap:balance;color:var(--fg)}
.content h2{font-size:var(--text-heading);line-height:1.4;font-weight:600;
  margin:2.4rem 0 .9rem;padding-top:1.2rem;border-top:1px solid var(--line-soft);text-wrap:balance}
.content h3{font-size:var(--text-body);line-height:1.4;font-weight:600;margin:1.7rem 0 .5rem;text-wrap:balance}
.content h2 .anchor,.content h3 .anchor{color:inherit}
.content h2 .anchor:hover::after,.content h3 .anchor:hover::after{content:' #';color:var(--faint);font-weight:400}
.content p,.content li{color:var(--fg-soft)}
.content p{text-align:justify;text-align-last:start;hyphens:auto}
.content li{text-align:start}
.content li>p{text-align:inherit}
.content strong{color:var(--fg);font-weight:650}
.content blockquote{margin:1.6rem 0;padding:.9rem 1.3rem;background:var(--bg-soft);
  border:1px solid var(--line);border-radius:var(--r-sm);
  color:var(--fg-soft)}
.content blockquote p{margin:.2rem 0}
.content code{font:inherit;font-size:var(--text-small);hyphens:none;background:var(--code-bg);color:var(--code-fg);
  padding:.13em .42em;border-radius:0;border:1px solid var(--line-soft)}
.content pre{font-family:inherit;position:relative;background:var(--code-bg);border:1px solid var(--line);
  border-radius:0;padding:1.25rem 1.4rem;overflow:auto;margin:1.4rem 0;box-shadow:var(--shadow)}
.content pre code{display:block;background:none;border:none;padding:0;font-size:var(--text-small);line-height:1.65;color:var(--code-fg)}
/* Wrap shell examples visually; preserve their text for selection and copying. */
.content pre:has(>code.language-bash){white-space:pre-wrap;overflow-wrap:anywhere}
.content ul,.content ol{padding-inline-start:3ch;list-style-position:outside}
.content li{margin:.5rem 0;padding-inline-start:.5ch}
.content li>p{margin:.5rem 0}
.content li::marker{color:var(--gold-deep);font-variant-numeric:tabular-nums}
.store-links{display:flex;flex-wrap:wrap;gap:.75rem;margin:1rem 0}
.store-links a{display:flex;flex:1 1 14rem;align-items:center;gap:.75rem;min-width:0;min-height:3.5rem;
  padding:.625rem .875rem;border:1px solid var(--fg);background:var(--fg);color:var(--bg);text-decoration:none;text-align:start}
.store-links a:hover{background:var(--dim);border-color:var(--dim);color:var(--bg)}
.store-links a:focus-visible{outline:2px solid var(--primary);outline-offset:3px}
/* Brand icons: Simple Icons (CC0), https://simpleicons.org/ */
.store-links a::before{content:'';flex:none;width:26px;height:26px;background:currentColor;
  mask:var(--store-icon) center/contain no-repeat;-webkit-mask:var(--store-icon) center/contain no-repeat}
.store-links .store-apple{--store-icon:url('data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHZpZXdCb3g9IjAgMCAyNCAyNCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48dGl0bGU+QXBwbGU8L3RpdGxlPjxwYXRoIGQ9Ik0xMi4xNTIgNi44OTZjLS45NDggMC0yLjQxNS0xLjA3OC0zLjk2LTEuMDQtMi4wNC4wMjctMy45MSAxLjE4My00Ljk2MSAzLjAxNC0yLjExNyAzLjY3NS0uNTQ2IDkuMTAzIDEuNTE5IDEyLjA5IDEuMDEzIDEuNDU0IDIuMjA4IDMuMDkgMy43OTIgMy4wMzkgMS41Mi0uMDY1IDIuMDktLjk4NyAzLjkzNS0uOTg3IDEuODMxIDAgMi4zNS45ODcgMy45Ni45NDggMS42MzctLjAyNiAyLjY3Ni0xLjQ4IDMuNjc2LTIuOTQ4IDEuMTU2LTEuNjg4IDEuNjM2LTMuMzI1IDEuNjYyLTMuNDE1LS4wMzktLjAxMy0zLjE4Mi0xLjIyMS0zLjIyLTQuODU3LS4wMjYtMy4wNCAyLjQ4LTQuNDk0IDIuNTk3LTQuNTU5LTEuNDI5LTIuMDktMy42MjMtMi4zMjQtNC4zOS0yLjM3Ni0yLS4xNTYtMy42NzUgMS4wOS00LjYxIDEuMDl6TTE1LjUzIDMuODNjLjg0My0xLjAxMiAxLjQtMi40MjcgMS4yNDUtMy44My0xLjIwNy4wNTItMi42NjIuODA1LTMuNTMyIDEuODE4LS43OC44OTYtMS40NTQgMi4zMzgtMS4yNzMgMy43MTQgMS4zMzguMTA0IDIuNzE1LS42ODggMy41NTktMS43MDEiLz48L3N2Zz4=')}
.store-links .store-android{--store-icon:url('data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHZpZXdCb3g9IjAgMCAyNCAyNCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48dGl0bGU+QW5kcm9pZDwvdGl0bGU+PHBhdGggZD0iTTE4LjQzOTUgNS41NTg2Yy0uNjc1IDEuMTY2NC0xLjM1MiAyLjMzMTgtMi4wMjc0IDMuNDk4LS4wMzY2LS4wMTU1LS4wNzQyLS4wMjg2LS4xMTEzLS4wNDMtMS44MjQ5LS42OTU3LTMuNDg0LS44LTQuNDItLjc4Ny0xLjg1NTEuMDE4NS0zLjM1NDQuNDY0My00LjI1OTcuODIwMy0uMDg0LS4xNDk0LTEuNzUyNi0zLjAyMS0yLjAyMTUtMy40ODY0YTEuMTQ1MSAxLjE0NTEgMCAwIDAtLjE0MDYtLjE5MTRjLS4zMzEyLS4zNjQtLjkwNTQtLjQ4NTktMS4zNzktLjIwMy0uNDc1LjI4Mi0uNzEzNi45MzYxLS4zODg2IDEuNTAxOSAxLjk0NjYgMy4zNjk2LS4wOTY2LS4yMTU4IDEuOTQ3MyAzLjM1OTMuMDE3Mi4wMzEtLjQ5NDYuMjY0Mi0xLjM5MjYgMS4wMTc3QzIuODk4NyAxMi4xNzYuNDUyIDE0Ljc3MiAwIDE4Ljk5MDJoMjRjLS4xMTktMS4xMTA4LS4zNjg2LTIuMDk5LS43NDYxLTMuMDY4My0uNzQzOC0xLjkxMTgtMS44NDM1LTMuMjkyOC0yLjc0MDItNC4xODM2YTEyLjEwNDggMTIuMTA0OCAwIDAgMC0yLjEzMDktMS42ODc1Yy42NTk0LTEuMTIyIDEuMzEyLTIuMjU1OSAxLjk2NDktMy4zODQ4LjIwNzctLjM2MTUuMTg4Ni0uNzk1Ni0uMDA3OS0xLjExOTFhMS4xMDAxIDEuMTAwMSAwIDAgMC0uODUxNS0uNTMzMmMtLjUyMjUtLjA1MzYtLjkzOTIuMzEyOC0xLjA0ODguNTQ0OXptLS4wMzkxIDguNDYxYy4zOTQ0LjU5MjYuMzI0IDEuMzMwNi0uMTU2MyAxLjY1MDMtLjQ3OTkuMzE5Ny0xLjE4OC4wOTg1LTEuNTgyLS40OTQxLS4zOTQ0LS41OTI3LS4zMjQtMS4zMzA3LjE1NjMtMS42NTA0LjQ3MjctLjMxNSAxLjE4MTItLjEwODYgMS41ODIuNDk0MXpNNy4yMDcgMTMuNTI3M2MuNDgwMy4zMTk3LjU1MDYgMS4wNTc3LjE1NjMgMS42NTA0LS4zOTQuNTkyNi0xLjEwMzguODEzOC0xLjU4NC40OTQxLS40OC0uMzE5Ny0uNTUwMy0xLjA1NzctLjE1NjMtMS42NTA0LjQwMDgtLjYwMjEgMS4xMDg3LS44MTA2IDEuNTg0LS40OTQxeiIvPjwvc3ZnPg==')}
.store-links span{display:flex;flex-direction:column;min-width:0}
.store-links small{font-size:var(--text-small)}
.store-links strong{font-size:var(--text-body);color:inherit}
.content table{table-layout:fixed;border-collapse:collapse;width:100%;margin:1.4rem 0;font-size:var(--text-small);line-height:1.5;
  border:1px solid var(--line)}
.content table:has(th:nth-child(2):last-child) th:first-child{width:38%}
.content table:has(th:nth-child(3):last-child) th:nth-child(-n+2){width:26%}
.content th,.content td{border-bottom:1px solid var(--line-soft);padding:.5rem .625rem;text-align:left;
  vertical-align:top;white-space:normal;overflow-wrap:anywhere}
.content th{background:var(--panel);font-weight:600;color:var(--amber-deep)}
.content th code,.content td code{font-size:inherit;padding:0 .2em}
.content tr:last-child td{border-bottom:none}
.content tr:hover td{background:var(--bg-soft)}
.content hr{border:0;border-top:1px solid var(--line);margin:2.6rem 0}
.foot{margin-top:4rem;padding-top:1.5rem;border-top:1px solid var(--line);
  color:var(--dim);font-size:var(--text-small);display:flex;justify-content:space-between;gap:1rem;flex-wrap:wrap}
/* right rail: on this page */
.toc{position:sticky;top:4rem;align-self:start;height:calc(100vh - 4rem);overflow-y:auto;
  padding:3.4rem 1.2rem 3rem;font-size:var(--text-small)}
.toc .lbl{color:var(--amber-deep);font-size:inherit;font-weight:700;margin-bottom:.7rem}
.toc a{display:block;color:var(--dim);padding:.2rem 0;line-height:1.4;border-left:2px solid var(--line);
  padding-left:.8rem;transition:color .12s,border-color .12s}
.toc a:hover{color:var(--amber-deep);border-color:var(--gold)}
.toc a.lvl-3{padding-left:1.5rem}
/* syntax tokens — match the TUI's code-syntax-* palette (theme.clj light) */
.token.comment{color:var(--faint);font-style:italic}
.token.keyword,.token.boolean{color:#196e76;font-weight:600}
.token.string,.token.char{color:#965028}
.token.function,.token.class-name{color:#1e5ab4;font-weight:600}
.token.number,.token.symbol{color:#7846aa}
.token.punctuation{color:#505050}
/* brand logo */
.top .brand .logo{height:1.7rem;width:auto;display:block}
/* footer Blockether mark */
.foot{align-items:center}
.bk{display:inline-flex;align-items:center;gap:.5rem;color:var(--amber-deep);font-weight:600}
.bk:hover{color:var(--amber-deep)}
.bk-mark{height:1.5rem;width:auto;display:block;opacity:.9;transition:opacity .12s}
.bk:hover .bk-mark{opacity:1}
/* mobile nav toggle (CSS-only drawer) */
.navtoggle{position:absolute;opacity:0;pointer-events:none}
.hamburger{display:none;flex-direction:column;justify-content:center;gap:5px;
  width:2.75rem;height:2.75rem;align-items:center;border-radius:0;cursor:pointer;
  border:0;background:transparent;margin-left:-.6rem;margin-right:.1rem;-webkit-tap-highlight-color:transparent}
.hamburger span{display:block;width:1.15rem;height:2px;border-radius:0;background:var(--fg);
  transition:transform .25s var(--ease-out),opacity .2s var(--ease-out)}
.navtoggle:checked ~ .top .hamburger span:nth-child(1){transform:translateY(7px) rotate(45deg)}
.navtoggle:checked ~ .top .hamburger span:nth-child(2){opacity:0;transform:scaleX(0)}
.navtoggle:checked ~ .top .hamburger span:nth-child(3){transform:translateY(-7px) rotate(-45deg)}
.navtoggle:focus-visible ~ .top .hamburger{outline:2px solid var(--primary);outline-offset:2px}
.scrim{display:none}
@media(pointer:coarse){
  .nav a{display:flex;align-items:center;min-height:2.75rem}
  .top .gh{justify-content:center;min-width:2.75rem;min-height:2.75rem}
}
/* tablet: drop the right rail */
@media(max-width:1100px){.shell{grid-template-columns:15rem minmax(0,1fr)}.toc{display:none}}
/* mobile: sidebar becomes a slide-in drawer triggered by the hamburger */
@media(max-width:820px){
  :root{--text-body:.875rem; --text-small:.75rem; --text-heading:1rem; --text-title:1.5rem}
  body{line-height:1.6}
  .shell{grid-template-columns:1fr}
  .hamburger{display:flex}
  .top{height:calc(4rem + env(safe-area-inset-top));padding-top:env(safe-area-inset-top);
    padding-left:max(1rem,env(safe-area-inset-left));padding-right:max(1rem,env(safe-area-inset-right))}
  .main{padding:1.6rem max(1rem,env(safe-area-inset-right)) 3rem max(1rem,env(safe-area-inset-left))}
  .content pre{padding:.875rem 1rem}
  .content th,.content td{padding:.45rem .5rem}
  .side{position:fixed;top:calc(4rem + env(safe-area-inset-top));left:0;bottom:0;height:auto;width:min(20rem,82vw);z-index:60;
    background:var(--bg);border-right:1px solid var(--line);box-shadow:var(--shadow);
    transform:translateX(-100%);transition:transform .22s ease;padding-top:1.4rem;
    padding-left:max(1rem,env(safe-area-inset-left))}
  .navtoggle:checked ~ .shell .side{transform:translateX(0)}
  .navtoggle:checked ~ .scrim{display:block;position:fixed;
    top:calc(4rem + env(safe-area-inset-top));left:0;right:0;bottom:0;z-index:55;
    background:rgba(30,30,30,.32);backdrop-filter:blur(1px)}
  .foot{flex-direction:column;align-items:flex-start;gap:.6rem;
    padding-left:env(safe-area-inset-left);padding-right:env(safe-area-inset-right)}
  }
/* prose rhythm + accessibility */
.content p{text-wrap:pretty}
@media (prefers-reduced-motion: reduce){
  *{animation-duration:.001ms !important;transition-duration:.001ms !important;scroll-behavior:auto !important}
}
"]
    (reduce (fn [css [rel tok]]
              (str/replace css tok (asset mode (str "fonts/" rel))))
            base
            font-tokens)))

(defn- esc
  ^String [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

;; :static → relative ("slug.html"), so the bundle works from any host/subpath
;; on GitHub Pages. :live → ABSOLUTE ("/docs/slug"), so nav resolves the same
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
  (str/replace html
               #"href=\"([^\"#:/][^\":]*?)\.md(#[^\"]*)?\""
               (fn [[_ slug frag]]
                 (str "href=\"" (href mode slug) (or frag "") "\""))))

(defn- nav-html
  [{:keys [pages]} active-slug mode]
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
         "</nav>")))

(defn- toc-html
  [toc]
  (when (seq toc)
    (str "<aside class=\"toc\"><div class=\"lbl\">On this page</div>"
         (apply str
           (for [{:keys [level id text]} toc]
             (str "<a class=\"lvl-" level "\" href=\"#" id "\">" (esc text) "</a>")))
         "</aside>")))

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
      "<style>"
      (theme-css mode)
      "</style></head><body>"
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
      "</div></body></html>")))

;; static site

(def ^:private asset-files
  {"vis-docs/assets/logo.png" "assets/logo.png"
   "vis-docs/assets/blockether.png" "assets/blockether.png"
   "vis-docs/assets/fonts/jetbrains-mono.woff2" "assets/fonts/jetbrains-mono.woff2"})

(defn- copy-assets!
  [out-dir]
  (doseq [[res out] asset-files]
    (when-let [u (io/resource res)]
      (let [f (io/file out-dir out)]
        (io/make-parents f)
        (with-open [in (io/input-stream u)]
          (io/copy in f))))))

(defn build-site!
  "Render the discovered docs to a static themed HTML bundle under `out-dir`."
  [out-dir]
  (let [{:keys [pages] :as site-data} (collect)]
    (when (empty? pages) (throw (ex-info "no vis-docs pages found on classpath" {})))
    (io/make-parents (io/file out-dir "x"))
    (copy-assets! out-dir)
    (doseq [{:keys [slug] :as page} pages]
      (spit (io/file out-dir (str slug ".html")) (page-html site-data page :static)))
    (let [home (or (first (filter #(= "index" (:slug %)) pages)) (first pages))]
      (spit (io/file out-dir "index.html") (page-html site-data home :static)))
    {:out out-dir :pages (mapv :slug pages)}))

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
