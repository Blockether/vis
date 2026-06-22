(ns com.blockether.vis.internal.docs
  "Embedded documentation subsystem.

   Any artifact on the classpath may ship docs by placing markdown under
   `resources/vis-docs/` with a `resources/vis-docs/vis-docs.edn` manifest:

     {:site  {:title \"Vis\" :tagline \"…\" :repo \"https://…\"}   ; optional, site-level
      :pages [{:file \"index.md\" :title \"Introduction\" :section nil :order 0}
              {:file \"x.md\"     :title \"X\"            :section \"Runtime\" :order 30}]}

   Every `vis-docs/vis-docs.edn` on the classpath is discovered (the same
   per-artifact auto-discovery native-image config uses), so an extension adds a
   page by dropping a markdown file + a manifest entry — no central registry.

   One renderer, two outputs:
     * `build-site!` writes a static, themed HTML bundle (for GitHub Pages).
     * `page-response` / `asset-response` serve the same pages live (HTMX nav),
       mountable on the gateway via its `:gateway.slot/http-routes` slot.

   Markdown → HTML uses commonmark-java (already a dependency); the theme reuses
   the VIS palette (cream / gold / amber, Inter + JetBrains Mono)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URL]
           [java.util Enumeration]
           [org.commonmark.parser Parser]
           [org.commonmark.renderer.html HtmlRenderer]
           [org.commonmark.ext.gfm.tables TablesExtension]
           [org.commonmark.ext.gfm.strikethrough StrikethroughExtension]))

(set! *warn-on-reflection* true)

(def ^:private manifest-resource "vis-docs/vis-docs.edn")

;; ---------------------------------------------------------------------------
;; commonmark markdown -> HTML
;; ---------------------------------------------------------------------------

(def ^:private extensions
  [(TablesExtension/create) (StrikethroughExtension/create)])

(def ^:private ^Parser md-parser
  (-> (Parser/builder) (.extensions extensions) (.build)))

(def ^:private ^HtmlRenderer md-renderer
  (-> (HtmlRenderer/builder) (.extensions extensions) (.build)))

(defn md->html ^String [^String md]
  (.render md-renderer (.parse md-parser md)))

(defn- first-h1
  "First level-1 heading text in `md`, for page titles / breadcrumbs."
  [^String md]
  (some->> (str/split-lines md)
           (some (fn [l] (when (str/starts-with? l "# ") (str/trim (subs l 2)))))))

;; ---------------------------------------------------------------------------
;; classpath discovery
;; ---------------------------------------------------------------------------

(defn- classloader ^ClassLoader []
  (or (.getContextClassLoader (Thread/currentThread))
      (clojure.lang.RT/baseLoader)))

(defn- sibling-url
  "URL of `file` sitting next to the manifest at `manifest-url`."
  ^URL [^URL manifest-url ^String file]
  (URL. (str/replace (.toString manifest-url) #"vis-docs\.edn$" file)))

(defn collect
  "Discover every vis-docs manifest on the classpath and return
   {:site {…} :pages [{:slug :title :section :order :md :html} …]} with pages
   sorted by (section, order, title). Later site-meta wins ties; the page with
   slug \"index\" is always first."
  []
  (let [^Enumeration urls (.getResources (classloader) manifest-resource)
        manifests (loop [acc []]
                    (if (.hasMoreElements urls)
                      (recur (conj acc ^URL (.nextElement urls)))
                      acc))
        site (atom {:title "Vis" :tagline "" :repo nil})
        pages (vec
                (mapcat
                  (fn [^URL mu]
                    (let [m (edn/read-string (slurp mu))]
                      (when-let [s (:site m)] (swap! site merge s))
                      (keep
                        (fn [{:keys [file title section order]}]
                          (when-let [md (try (slurp (sibling-url mu file)) (catch Exception _ nil))]
                            {:slug    (str/replace file #"\.md$" "")
                             :title   (or title (first-h1 md) file)
                             :section section
                             :order   (or order 100)
                             :md      md
                             :html    (md->html md)}))
                        (:pages m))))
                  manifests))
        ordered (sort-by (juxt #(if (= "index" (:slug %)) 0 1)
                               #(or (:section %) "")
                               :order :title)
                         pages)]
    {:site @site :pages (vec ordered)}))

;; ---------------------------------------------------------------------------
;; theme (VIS palette) + page template
;; ---------------------------------------------------------------------------

(def theme-css
  "VIS docs theme — cream/gold/amber light palette, Inter + JetBrains Mono."
  "
:root{
  --bg:#fffdf8; --panel:#f8f4eb; --fg:#1e1e1e; --dim:#6b6b6b; --line:#e7e1d3;
  --gold:#facc15; --gold-deep:#be9628; --amber:#a16207; --amber-deep:#503c00;
  --link:#a16207; --link-hover:#503c00; --code-bg:#f3efe4; --sel:#fbeec1;
  --radius:12px; --measure:46rem;
  --sans:'Inter',system-ui,-apple-system,Segoe UI,Roboto,sans-serif;
  --mono:'JetBrains Mono',ui-monospace,SFMono-Regular,Menlo,monospace;
}
@font-face{font-family:'Inter';font-weight:400;font-display:swap;src:url(assets/fonts/inter-400.woff2) format('woff2')}
@font-face{font-family:'Inter';font-weight:600;font-display:swap;src:url(assets/fonts/inter-600.woff2) format('woff2')}
@font-face{font-family:'Inter';font-weight:700;font-display:swap;src:url(assets/fonts/inter-700.woff2) format('woff2')}
@font-face{font-family:'JetBrains Mono';font-weight:400;font-display:swap;src:url(assets/fonts/jetbrains-mono-400.woff2) format('woff2')}
*{box-sizing:border-box}
html{scroll-behavior:smooth}
body{margin:0;background:var(--bg);color:var(--fg);font-family:var(--sans);
  font-size:17px;line-height:1.7;-webkit-font-smoothing:antialiased}
::selection{background:var(--sel)}
a{color:var(--link);text-decoration:none}
a:hover{color:var(--link-hover);text-decoration:underline;text-underline-offset:3px}
.layout{display:grid;grid-template-columns:17rem 1fr;min-height:100vh}
/* sidebar */
.side{position:sticky;top:0;align-self:start;height:100vh;overflow-y:auto;
  background:var(--panel);border-right:1px solid var(--line);padding:1.6rem 1.2rem}
.brand{display:flex;align-items:center;gap:.6rem;font-weight:700;font-size:1.3rem;
  letter-spacing:-.02em;margin:.2rem 0 .2rem}
.brand .dot{width:.7rem;height:.7rem;border-radius:50%;
  background:linear-gradient(135deg,var(--gold),var(--amber));box-shadow:0 0 0 3px #faecc0}
.tagline{color:var(--dim);font-size:.86rem;line-height:1.5;margin:.1rem 0 1.4rem}
.nav-sec{color:var(--amber-deep);font-size:.72rem;font-weight:700;letter-spacing:.08em;
  text-transform:uppercase;margin:1.3rem 0 .4rem}
.nav a{display:block;padding:.32rem .6rem;border-radius:8px;color:var(--fg);
  font-size:.95rem;font-weight:500}
.nav a:hover{background:#f0e8d4;text-decoration:none}
.nav a.active{background:linear-gradient(90deg,#fbeec1,transparent);
  color:var(--amber-deep);font-weight:600;box-shadow:inset 3px 0 0 var(--gold)}
.repo{margin-top:1.6rem;font-size:.85rem}
/* content */
.main{padding:3.2rem clamp(1.2rem,5vw,4rem)}
.content{max-width:var(--measure);margin:0 auto}
.content h1{font-size:2.4rem;line-height:1.15;letter-spacing:-.03em;margin:.2rem 0 1.2rem}
.content h2{font-size:1.5rem;letter-spacing:-.02em;margin:2.4rem 0 .8rem;
  padding-top:1.2rem;border-top:1px solid var(--line)}
.content h3{font-size:1.15rem;margin:1.8rem 0 .6rem}
.content p,.content li{color:#2a2a2a}
.content blockquote{margin:1.4rem 0;padding:.6rem 1.2rem;background:var(--panel);
  border-left:3px solid var(--gold);border-radius:0 8px 8px 0;color:#3a3a3a}
.content code{font-family:var(--mono);font-size:.86em;background:var(--code-bg);
  padding:.12em .4em;border-radius:6px}
.content pre{background:#fbf8f0;border:1px solid var(--line);border-radius:var(--radius);
  padding:1rem 1.2rem;overflow:auto;box-shadow:0 1px 2px rgba(80,60,0,.04)}
.content pre code{background:none;padding:0;font-size:.84rem;line-height:1.6}
.content table{border-collapse:collapse;width:100%;margin:1.4rem 0;font-size:.95rem}
.content th,.content td{border:1px solid var(--line);padding:.5rem .8rem;text-align:left}
.content th{background:var(--panel);font-weight:600}
.content tr:nth-child(even) td{background:#fdfbf5}
.content hr{border:0;border-top:1px solid var(--line);margin:2.4rem 0}
.foot{max-width:var(--measure);margin:4rem auto 0;padding-top:1.4rem;
  border-top:1px solid var(--line);color:var(--dim);font-size:.85rem}
/* prism-ish token colors (gold/amber family) */
.token.comment{color:#9a8c66;font-style:italic}
.token.keyword,.token.boolean{color:#a16207;font-weight:600}
.token.string,.token.char{color:#3f6212}
.token.function,.token.class-name{color:#503c00;font-weight:600}
.token.number,.token.symbol{color:#9a3412}
.token.punctuation{color:#8a8a8a}
@media(max-width:820px){.layout{grid-template-columns:1fr}.side{position:static;height:auto}}
")

(defn- esc ^String [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- href [mode slug]
  (case mode :static (str slug ".html") :live (str "docs/" slug)))

(defn- nav-html [{:keys [pages]} active-slug mode]
  (let [by-sec (->> pages (group-by :section))
        ;; nil section first, then the rest in first-seen order
        sections (cons nil (distinct (remove nil? (map :section pages))))]
    (str
      "<nav class=\"nav\">"
      (apply str
        (for [sec sections
              :let [ps (get by-sec sec)]
              :when (seq ps)]
          (str (when sec (str "<div class=\"nav-sec\">" (esc sec) "</div>"))
               (apply str
                 (for [{:keys [slug title]} ps]
                   (str "<a href=\"" (href mode slug) "\""
                        (when (= slug active-slug) " class=\"active\"")
                        ">" (esc title) "</a>"))))))
      "</nav>")))

(defn page-html
  "Full HTML document for one page. `mode` ∈ #{:static :live}."
  [{:keys [site] :as site-data} {:keys [slug title html] :as _page} mode]
  (str
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
    "<title>" (esc title) " · " (esc (:title site)) "</title>"
    "<meta name=\"description\" content=\"" (esc (:tagline site)) "\">"
    "<style>" theme-css "</style></head><body><div class=\"layout\">"
    "<aside class=\"side\">"
    "<a class=\"brand\" href=\"" (href mode "index") "\"><span class=\"dot\"></span>"
    (esc (:title site)) "</a>"
    "<div class=\"tagline\">" (esc (:tagline site)) "</div>"
    (nav-html site-data slug mode)
    (when-let [r (:repo site)] (str "<div class=\"repo\"><a href=\"" (esc r) "\">GitHub →</a></div>"))
    "</aside>"
    "<main class=\"main\"><article class=\"content\">" html "</article>"
    "<div class=\"foot\">" (esc (:title site)) " — built from embedded docs.</div>"
    "</main></div></body></html>"))

;; ---------------------------------------------------------------------------
;; static site
;; ---------------------------------------------------------------------------

(def ^:private asset-files
  ;; self-contained docs resources: classpath resource -> output path under site
  {"vis-docs/assets/fonts/inter-400.woff2"          "assets/fonts/inter-400.woff2"
   "vis-docs/assets/fonts/inter-600.woff2"          "assets/fonts/inter-600.woff2"
   "vis-docs/assets/fonts/inter-700.woff2"          "assets/fonts/inter-700.woff2"
   "vis-docs/assets/fonts/jetbrains-mono-400.woff2" "assets/fonts/jetbrains-mono-400.woff2"})

(defn- copy-assets! [out-dir]
  (doseq [[res out] asset-files]
    (when-let [u (io/resource res)]
      (let [f (io/file out-dir out)]
        (io/make-parents f)
        (with-open [in (io/input-stream u)] (io/copy in f))))))

(defn build-site!
  "Render the discovered docs to a static themed HTML bundle under `out-dir`."
  [out-dir]
  (let [{:keys [pages] :as site-data} (collect)]
    (when (empty? pages) (throw (ex-info "no vis-docs pages found on classpath" {})))
    (io/make-parents (io/file out-dir "x"))
    (copy-assets! out-dir)
    (doseq [{:keys [slug] :as page} pages]
      (spit (io/file out-dir (str slug ".html")) (page-html site-data page :static)))
    ;; index.html mirrors the index page (or the first page if none named index)
    (let [home (or (first (filter #(= "index" (:slug %)) pages)) (first pages))]
      (spit (io/file out-dir "index.html") (page-html site-data home :static)))
    {:out out-dir :pages (mapv :slug pages)}))

;; ---------------------------------------------------------------------------
;; live serving — a Ring handler for the gateway `:gateway.slot/http-routes`
;; slot. Renders the SAME pages (mode :live) at /docs[/slug] and serves the
;; woff2 assets at /docs/assets/fonts/*. Docs are collected lazily and cached.
;; ---------------------------------------------------------------------------

(def ^:private site-cache (delay (collect)))

(defn- ok-html [body]
  {:status 200 :headers {"content-type" "text/html; charset=utf-8"} :body body})

(defn- font-response [^String rel]
  ;; rel = "fonts/inter-400.woff2"; served from the docs module's own resources.
  (when-let [u (io/resource (str "vis-docs/assets/" rel))]
    {:status 200
     :headers {"content-type" "font/woff2" "cache-control" "public,max-age=31536000,immutable"}
     :body (io/input-stream u)}))

(defn handle
  "Ring handler for the docs site. Returns nil for paths it does not own (so the
   gateway can fall through). Owns `/docs`, `/docs/<slug>`, `/docs/assets/**`."
  [{:keys [uri] :or {uri ""}}]
  (let [{:keys [pages] :as site-data} @site-cache
        path (-> uri (str/replace #"^/docs/?" "") (str/replace #"/$" ""))]
    (cond
      (str/starts-with? path "assets/")
      (font-response (subs path (count "assets/")))

      (or (= path "") (= path "index"))
      (ok-html (page-html site-data
                          (or (first (filter #(= "index" (:slug %)) pages)) (first pages))
                          :live))

      :else
      (when-let [page (first (filter #(= path (:slug %)) pages))]
        (ok-html (page-html site-data page :live))))))
