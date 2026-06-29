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
     * `handle` serves the same pages live (HTMX nav), mountable on the gateway
       via its `:gateway.slot/http-routes` slot.

   Markdown → HTML uses commonmark-java (already a dependency); the theme is an
   enterprise-grade docs layout (sticky header, sidebar, on-this-page rail) in
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

(defn- first-h1 [^String md]
  (some->> (str/split-lines md)
    (some (fn [l] (when (str/starts-with? l "# ") (str/trim (subs l 2)))))))

(defn- strip-tags ^String [^String s]
  (-> s (str/replace #"<[^>]+>" "") (str/replace "&amp;" "&")
    (str/replace "&lt;" "<") (str/replace "&gt;" ">") str/trim))

(defn- slugify [^String s]
  (-> (strip-tags s) str/lower-case
    (str/replace #"[^a-z0-9]+" "-") (str/replace #"^-+|-+$" "")))

(defn- anchors+toc
  "Inject id= on h2/h3 and return [html-with-ids toc] where toc is
   [{:level 2|3 :id :text} …] — the right-rail 'on this page' index."
  [^String html]
  (let [toc (atom [])
        html' (str/replace html #"<h([23])>(.*?)</h[23]>"
                (fn [[_ lvl inner]]
                  (let [id (slugify inner)]
                    (swap! toc conj {:level (parse-long lvl) :id id :text (strip-tags inner)})
                    (str "<h" lvl " id=\"" id "\">"
                      "<a class=\"anchor\" href=\"#" id "\">" inner "</a></h" lvl ">"))))]
    [html' @toc]))

;; ---------------------------------------------------------------------------
;; classpath discovery
;; ---------------------------------------------------------------------------

(defn- classloader ^ClassLoader []
  (or (.getContextClassLoader (Thread/currentThread))
    (clojure.lang.RT/baseLoader)))

(defn- sibling-url ^URL [^URL manifest-url ^String file]
  (URL. (str/replace (.toString manifest-url) #"vis-docs\.edn$" file)))

(defn collect
  "Discover every vis-docs manifest on the classpath →
   {:site {…} :pages [{:slug :title :section :order :md :html :toc} …]}, sorted
   by (section, order, title); slug \"index\" is always first."
  []
  (let [^Enumeration urls (.getResources (classloader) manifest-resource)
        manifests (loop [acc []]
                    (if (.hasMoreElements urls) (recur (conj acc ^URL (.nextElement urls))) acc))
        site (atom {:title "Vis" :tagline "" :repo nil})
        pages (vec
                (mapcat
                  (fn [^URL mu]
                    (let [m (edn/read-string (slurp mu))]
                      (when-let [s (:site m)] (swap! site merge s))
                      (keep
                        (fn [{:keys [file title section order]}]
                          (when-let [md (try (slurp (sibling-url mu file)) (catch Exception _ nil))]
                            (let [[html toc] (anchors+toc (md->html md))]
                              {:slug (str/replace file #"\.md$" "")
                               :title (or title (first-h1 md) file)
                               :section section :order (or order 100)
                               :md md :html html :toc toc})))
                        (:pages m))))
                  manifests))
        ordered (sort-by (juxt #(if (= "index" (:slug %)) 0 1)
                           #(or (:section %) "") :order :title)
                  pages)]
    {:site @site :pages (vec ordered)}))

;; ---------------------------------------------------------------------------
;; theme (VIS palette) — enterprise docs layout
;; ---------------------------------------------------------------------------

(def ^:private font-tokens
  {"inter-400.woff2"          "FONTPATH_inter-400"
   "inter-600.woff2"          "FONTPATH_inter-600"
   "inter-700.woff2"          "FONTPATH_inter-700"
   "jetbrains-mono-400.woff2" "FONTPATH_jbm-400"})

(defn- asset
  "Rooted URL to a docs asset, correct from any page depth.
   :live  → \"/docs/assets/<rel>\"  (absolute, survives nested page paths)
   :static → \"assets/<rel>\"         (GitHub Pages: index at site root)"
  [mode rel]
  (case mode
    :static (str "assets/" rel)
    :live   (str "/docs/assets/" rel)))

(defn- theme-css [mode]
  "The theme stylesheet with font URLs rooted for `mode`. Tokens in the CSS
   base are replaced with rooted asset paths so fonts load from any page depth."
  (let [base "
:root{
  --bg:#fffdf9; --bg-soft:#faf6ee; --panel:#f6f1e6; --header:rgba(255,253,249,.82);
  --fg:#1a1813; --fg-soft:#3a362d; --dim:#7c7565; --faint:#a8a08c;
  --line:#ece4d4; --line-soft:#f2ebdc;
  --gold:#eab308; --gold-deep:#a67c00; --amber:#9a6a00; --amber-deep:#4d3a00;
  --link:#9a6a00; --link-hover:#4d3a00; --accent:linear-gradient(135deg,#f5cf4d,#b8860b);
  --code-bg:#f6f1e6; --code-fg:#41372a; --sel:#fbe7bd;
  --radius:14px; --r-sm:10px; --measure:44rem; --maxw:88rem;
  --sans:'Inter',system-ui,-apple-system,Segoe UI,Roboto,sans-serif;
  --mono:'JetBrains Mono',ui-monospace,SFMono-Regular,Menlo,monospace;
  --shadow:0 1px 2px rgba(77,58,0,.04),0 8px 28px rgba(77,58,0,.06);
}
@font-face{font-family:'Inter';font-weight:400;font-display:swap;src:url(FONTPATH_inter-400) format('woff2')}
@font-face{font-family:'Inter';font-weight:600;font-display:swap;src:url(FONTPATH_inter-600) format('woff2')}
@font-face{font-family:'Inter';font-weight:700;font-display:swap;src:url(FONTPATH_inter-700) format('woff2')}
@font-face{font-family:'JetBrains Mono';font-weight:400;font-display:swap;src:url(FONTPATH_jbm-400) format('woff2')}
*{box-sizing:border-box}
html{scroll-behavior:smooth;scroll-padding-top:5.5rem}
body{margin:0;background:var(--bg);color:var(--fg);font-family:var(--sans);
  font-size:17px;line-height:1.75;-webkit-font-smoothing:antialiased;
  text-rendering:optimizeLegibility;letter-spacing:-.011em;
  font-feature-settings:'cv05' 1,'cv11' 1,'ss01' 1,'liga' 1}
p{margin:1.05rem 0}
::selection{background:var(--sel)}
a{color:var(--link);text-decoration:none;transition:color .12s}
a:hover{color:var(--link-hover)}
/* sticky header */
.top{position:sticky;top:0;z-index:50;height:4rem;display:flex;align-items:center;
  gap:1rem;padding:0 clamp(1rem,3vw,2rem);background:var(--header);
  backdrop-filter:saturate(160%) blur(10px);border-bottom:1px solid var(--line)}
.top .brand{display:flex;align-items:center;gap:.6rem;font-weight:700;font-size:1.2rem;
  letter-spacing:-.02em;color:var(--fg)}
.top .brand .dot{width:.85rem;height:.85rem;border-radius:50%;background:var(--accent);
  box-shadow:0 0 0 3px #faecc0,0 1px 3px rgba(77,58,0,.3)}
.top .spacer{flex:1}
.top .gh{display:inline-flex;align-items:center;color:var(--dim);transition:color .12s}
.top .gh:hover{color:var(--link-hover)}
.top .gh svg{display:block}
.shell{max-width:var(--maxw);margin:0 auto;display:grid;
  grid-template-columns:16rem minmax(0,1fr) 15rem;gap:0}
/* sidebar */
.side{position:sticky;top:4rem;align-self:start;height:calc(100vh - 4rem);overflow-y:auto;
  padding:2rem 1.1rem 3rem;border-right:1px solid var(--line-soft)}
.side .tagline{color:var(--dim);font-size:.85rem;line-height:1.5;margin:0 .3rem 1.4rem;
  padding-bottom:1.2rem;border-bottom:1px solid var(--line-soft)}
.nav-sec{color:var(--amber-deep);font-size:.7rem;font-weight:700;letter-spacing:.09em;
  text-transform:uppercase;margin:1.5rem .6rem .5rem}
.nav a{display:block;padding:.34rem .7rem;border-radius:9px;color:var(--fg-soft);
  font-size:.92rem;font-weight:500;transition:background .12s,color .12s}
.nav a:hover{background:var(--panel);color:var(--fg)}
.nav a.active{background:linear-gradient(90deg,#fbeec1 0%,rgba(251,238,193,.25) 100%);
  color:var(--amber-deep);font-weight:600;box-shadow:inset 2px 0 0 var(--gold)}
/* content */
.main{padding:3.4rem clamp(1.2rem,4vw,3.5rem) 5rem;min-width:0}
.content{max-width:var(--measure)}
.eyebrow{font-size:.74rem;font-weight:700;letter-spacing:.1em;text-transform:uppercase;
  color:var(--amber);margin-bottom:.7rem}
/* hero (landing) */
.hero{padding:1rem 0 1.2rem;margin-bottom:1.6rem}
.hero-title{font-size:clamp(2.3rem,4.6vw,3.4rem);line-height:1.06;letter-spacing:-.04em;
  font-weight:750;margin:.2rem 0 1.1rem;max-width:20ch;
  background:linear-gradient(180deg,#2a2410,#6b5410 140%);-webkit-background-clip:text;
  background-clip:text;color:transparent}
.hero-sub{font-size:1.18rem;line-height:1.55;color:var(--dim);max-width:40rem;margin:0 0 1.7rem}
.hero-cta{display:flex;gap:1.1rem;flex-wrap:wrap;align-items:baseline}
.btn{display:inline-flex;align-items:baseline;gap:.3rem;padding:0;border-radius:0;background:none;
  border:0;box-shadow:none;font-size:1.02rem;font-weight:600;letter-spacing:-.01em;transition:color .12s}
.btn:hover{text-decoration:underline;text-underline-offset:3px}
.btn-primary{color:var(--link)}
.btn-primary:hover{color:var(--link-hover)}
.btn-ghost{color:var(--dim);font-weight:500}
.btn-ghost:hover{color:var(--link)}
.content h1{font-size:2.6rem;line-height:1.1;letter-spacing:-.035em;margin:0 0 1.1rem;font-weight:700}
.content h2{font-size:1.45rem;letter-spacing:-.02em;font-weight:650;margin:2.8rem 0 .9rem;
  padding-top:1.4rem;border-top:1px solid var(--line-soft)}
.content h3{font-size:1.12rem;font-weight:650;margin:1.9rem 0 .5rem}
.content h2 .anchor,.content h3 .anchor{color:inherit}
.content h2 .anchor:hover::after,.content h3 .anchor:hover::after{content:' #';color:var(--faint);font-weight:400}
.content p,.content li{color:var(--fg-soft)}
.content strong{color:var(--fg);font-weight:650}
.content blockquote{margin:1.6rem 0;padding:.9rem 1.3rem;background:var(--bg-soft);
  border:1px solid var(--line);border-left:3px solid var(--gold);border-radius:0 var(--r-sm) var(--r-sm) 0;
  color:var(--fg-soft)}
.content blockquote p{margin:.2rem 0}
.content code{font-family:var(--mono);font-size:.85em;background:var(--code-bg);color:var(--code-fg);
  padding:.13em .42em;border-radius:6px;border:1px solid var(--line-soft)}
.content pre{position:relative;background:#fcf9f1;border:1px solid var(--line);
  border-radius:var(--radius);padding:1.1rem 1.3rem;overflow:auto;margin:1.4rem 0;box-shadow:var(--shadow)}
.content pre code{background:none;border:none;padding:0;font-size:.83rem;line-height:1.65;color:#3a3024}
.content ul,.content ol{padding-left:1.3rem}
.content li{margin:.3rem 0}
.content li::marker{color:var(--gold-deep)}
.content table{border-collapse:collapse;width:100%;margin:1.6rem 0;font-size:.92rem;
  border:1px solid var(--line);border-radius:var(--r-sm);overflow:hidden}
.content th,.content td{border-bottom:1px solid var(--line-soft);padding:.6rem .9rem;text-align:left}
.content th{background:var(--panel);font-weight:650;color:var(--amber-deep);
  font-size:.74rem;letter-spacing:.04em;text-transform:uppercase}
.content tr:last-child td{border-bottom:none}
.content tr:hover td{background:var(--bg-soft)}
.content hr{border:0;border-top:1px solid var(--line);margin:2.6rem 0}
.foot{margin-top:4rem;padding-top:1.5rem;border-top:1px solid var(--line);
  color:var(--dim);font-size:.84rem;display:flex;justify-content:space-between;gap:1rem;flex-wrap:wrap}
/* right rail: on this page */
.toc{position:sticky;top:4rem;align-self:start;height:calc(100vh - 4rem);overflow-y:auto;
  padding:3.4rem 1.2rem 3rem;font-size:.85rem}
.toc .lbl{color:var(--amber-deep);font-size:.7rem;font-weight:700;letter-spacing:.09em;
  text-transform:uppercase;margin-bottom:.7rem}
.toc a{display:block;color:var(--dim);padding:.2rem 0;line-height:1.4;border-left:2px solid var(--line);
  padding-left:.8rem;transition:color .12s,border-color .12s}
.toc a:hover{color:var(--amber-deep);border-color:var(--gold)}
.toc a.lvl-3{padding-left:1.5rem;font-size:.82rem}
/* prism-ish tokens */
.token.comment{color:#9a8c66;font-style:italic}
.token.keyword,.token.boolean{color:#9a6a00;font-weight:600}
.token.string,.token.char{color:#3f6212}
.token.function,.token.class-name{color:#4d3a00;font-weight:600}
.token.number,.token.symbol{color:#9a3412}
.token.punctuation{color:#8a8a8a}
/* brand logo */
.top .brand .logo{height:1.7rem;width:auto;display:block;border-radius:6px}
/* footer Blockether mark */
.foot{align-items:center}
.bk{display:inline-flex;align-items:center;gap:.5rem;color:var(--amber-deep);font-weight:600}
.bk:hover{color:var(--amber-deep)}
.bk-mark{height:1.5rem;width:auto;display:block;opacity:.9;transition:opacity .12s}
.bk:hover .bk-mark{opacity:1}
/* mobile nav toggle (CSS-only drawer) */
.navtoggle{position:absolute;opacity:0;pointer-events:none}
.hamburger{display:none;flex-direction:column;gap:4px;width:2.2rem;height:2.2rem;
  align-items:center;justify-content:center;border-radius:9px;cursor:pointer;
  border:1px solid var(--line);background:var(--bg-soft);margin-right:.3rem}
.hamburger span{display:block;width:1.05rem;height:2px;border-radius:2px;background:var(--amber-deep)}
.scrim{display:none}
/* tablet: drop the right rail */
@media(max-width:1100px){.shell{grid-template-columns:15rem minmax(0,1fr)}.toc{display:none}}
/* mobile: sidebar becomes a slide-in drawer triggered by the hamburger */
@media(max-width:820px){
  body{font-size:16px}
  .shell{grid-template-columns:1fr}
  .hamburger{display:flex}
  .top .tnav .ghost{display:none}
  .main{padding:2.2rem 1.2rem 3.5rem}
  .content h1{font-size:2rem}
  .content pre{border-radius:10px}
  .content table{display:block;overflow-x:auto;white-space:nowrap}
  .side{position:fixed;top:4rem;left:0;bottom:0;width:min(20rem,82vw);z-index:60;
    background:var(--bg);border-right:1px solid var(--line);box-shadow:var(--shadow);
    transform:translateX(-100%);transition:transform .22s ease;padding-top:1.4rem}
  .navtoggle:checked ~ .shell .side{transform:translateX(0)}
  .navtoggle:checked ~ .scrim{display:block;position:fixed;inset:4rem 0 0 0;z-index:55;
    background:rgba(26,24,19,.32);backdrop-filter:blur(1px)}
  .foot{flex-direction:column;align-items:flex-start;gap:.6rem}
  }
"]
    (reduce (fn [css [rel tok]]
              (str/replace css tok (asset mode (str "fonts/" rel))))
            base font-tokens)))

(defn- esc ^String [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- href [mode slug] (case mode :static (str slug ".html") :live (str "docs/" slug)))

(defn- nav-html [{:keys [pages]} active-slug mode]
  (let [by-sec (group-by :section pages)
        sections (cons nil (distinct (remove nil? (map :section pages))))]
    (str "<nav class=\"nav\">"
      (apply str
        (for [sec sections :let [ps (get by-sec sec)] :when (seq ps)]
          (str (when sec (str "<div class=\"nav-sec\">" (esc sec) "</div>"))
            (apply str
              (for [{:keys [slug title]} ps]
                (str "<a href=\"" (href mode slug) "\""
                  (when (= slug active-slug) " class=\"active\"") ">" (esc title) "</a>"))))))
      "</nav>")))

(defn- toc-html [toc]
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
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
      "<title>" (esc title) " · " (esc (:title site)) "</title>"
      "<meta name=\"description\" content=\"" (esc (:tagline site)) "\">"
      "<style>" (theme-css mode) "</style></head><body>"
      ;; CSS-only mobile nav toggle (checkbox precedes .shell so it can target .side)
      "<input type=\"checkbox\" id=\"navtoggle\" class=\"navtoggle\" aria-label=\"Toggle navigation\">"
      ;; header
      "<header class=\"top\">"
      "<label for=\"navtoggle\" class=\"hamburger\" title=\"Menu\"><span></span><span></span><span></span></label>"
      "<a class=\"brand\" href=\"" (href mode "index") "\" title=\"" (esc (:title site)) "\">"
      "<img class=\"logo\" src=\"" (asset mode "logo.png") "\" alt=\"" (esc (:title site)) "\"></a>"
      "<span class=\"spacer\"></span>"
      (when-let [r (:repo site)]
        (str "<a class=\"gh\" href=\"" (esc r) "\" title=\"GitHub\" aria-label=\"GitHub\" target=\"_blank\" rel=\"noopener\">"
             "<svg width=\"20\" height=\"20\" viewBox=\"0 0 16 16\" fill=\"currentColor\" aria-hidden=\"true\">"
             "<path d=\"M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z\"/>"
             "</svg></a>"))
      "</header>"
      ;; body grid
      "<label for=\"navtoggle\" class=\"scrim\"></label>"
      "<div class=\"shell\"><aside class=\"side\">"
      "<div class=\"tagline\">" (esc (:tagline site)) "</div>"
      (nav-html site-data slug mode) "</aside>"
      "<main class=\"main\"><article class=\"content\">"
      (when home?
        (str "<section class=\"hero\">"
          (when-let [e (:eyebrow site)] (str "<div class=\"eyebrow\">" (esc e) "</div>"))
          "<h1 class=\"hero-title\">" (esc (or (:headline site) (:title site))) "</h1>"
          (when-let [s (:sub site)] (str "<p class=\"hero-sub\">" (esc s) "</p>"))
          "<div class=\"hero-cta\">"
          (when-let [c (:cta site)]
            (str "<a class=\"btn btn-primary\" href=\"" (href mode (:slug c)) "\">" (esc (:label c)) " →</a>"))
          "</div></section>"))
      html
      "<div class=\"foot\">"
      "<a class=\"bk\" href=\"https://blockether.com\" title=\"Blockether\">"
      "<img class=\"bk-mark\" src=\"" (asset mode "blockether.png") "\" alt=\"Blockether\"></a>"
      "<span class=\"spacer\"></span>"
      (when-let [r (:repo site)] (str "<a href=\"" (esc r) "\">Edit on GitHub ↗</a>")) "</div>"
      "</article></main>"
      (or (toc-html toc) "<div></div>")
      "</div></body></html>")))

;; ---------------------------------------------------------------------------
;; static site
;; ---------------------------------------------------------------------------

(def ^:private asset-files
  {"vis-docs/assets/logo.png"                       "assets/logo.png"
   "vis-docs/assets/blockether.png"                 "assets/blockether.png"
   "vis-docs/assets/fonts/inter-400.woff2"          "assets/fonts/inter-400.woff2"
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
    (let [home (or (first (filter #(= "index" (:slug %)) pages)) (first pages))]
      (spit (io/file out-dir "index.html") (page-html site-data home :static)))
    {:out out-dir :pages (mapv :slug pages)}))

;; ---------------------------------------------------------------------------
;; live serving — Ring handler for the gateway `:gateway.slot/http-routes` slot.
;; ---------------------------------------------------------------------------

(def ^:private site-cache (delay (collect)))

(def ^:dynamic *live-reload?*
  "When true, `handle` re-`collect`s the docs (re-reads the markdown from the
   classpath) on EVERY request, so editing `resources/vis-docs/*.md` during
   development shows on a browser refresh — no gateway restart. Cheap (a handful
   of small markdown files) and docs traffic is tiny. Bind false to serve the
   frozen `site-cache` snapshot if you ever want it."
  true)

(defn- ok-html [body]
  {:status 200 :headers {"content-type" "text/html; charset=utf-8"} :body body})

(defn- asset-response [^String rel]
  (when-let [u (io/resource (str "vis-docs/assets/" rel))]
    (let [ct (cond (str/ends-with? rel ".woff2") "font/woff2"
               (str/ends-with? rel ".png")   "image/png"
               (str/ends-with? rel ".svg")   "image/svg+xml"
               :else                          "application/octet-stream")]
      {:status 200
       :headers {"content-type" ct "cache-control" "public,max-age=31536000,immutable"}
       :body (io/input-stream u)})))

(defn handle
  "Ring handler for the docs site. Returns nil for paths it does not own (so the
   gateway can fall through). Owns `/docs`, `/docs/<slug>`, `/docs/assets/**`."
  [{:keys [uri] :or {uri ""}}]
  (let [{:keys [pages] :as site-data} (if *live-reload?* (collect) @site-cache)
        path (-> uri (str/replace #"^/docs/?" "") (str/replace #"/$" ""))]
    (cond
      (str/starts-with? path "assets/") (asset-response (subs path (count "assets/")))
      (or (= path "") (= path "index"))
      (ok-html (page-html site-data
                 (or (first (filter #(= "index" (:slug %)) pages)) (first pages)) :live))
      :else (when-let [page (first (filter #(= path (:slug %)) pages))]
              (ok-html (page-html site-data page :live))))))
