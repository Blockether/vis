(ns com.blockether.vis.ext.channel-web.core
  "Web companion channel - the gateway's `/ui` chat instrument.

   Chat-first anatomy (the ChatGPT/Claude shape): the conversation is a
   centered document column - user messages as compact right-aligned
   pills, vis answers as flat typeset prose behind a small accent avatar,
   the execution trace folded into a 'Work' disclosure, a floating
   pill composer pinned to the bottom (Enter sends, Shift+Enter breaks,
   auto-grows), a thin blurred sticky header, and CONTEXT - the same
   ctx mirror the model reads - as a quiet right rail.

   Pure Clojure SSR: hiccup renders HTML, HTMX does declarative swaps,
   the live feed is the htmx SSE extension consuming
   `/ui/session/:sid/stream` - a gateway SSE stream of named HTML
   fragments (activity/thinking/context) rendered server-side. Every
   script is VENDORED on the classpath and served from memory
   (htmx 2.0.10 + its SSE extension + ui.js) - a page
   never loads anything from outside vis.

   This namespace is the THIRD canonical-IR walker: the TUI walks IR
   into ANSI cells, Telegram walks it into its HTML subset, and
   `ir->hiccup` walks the same IR into DOM (GATEWAY.md §4.1 ALWAYS IR).

   AUTO-MOUNT: the extension declares a `:gateway.slot/http-routes`
   contribution; the gateway pulls it whenever it builds its handler
   (whiteboard pattern - no ordering requirement, see GATEWAY.md §10.1).

   Auth: none on the loopback default (`gateway-auth-required?` false);
   when the gate is on, POST /ui/auth exchanges the bearer token for an
   HttpOnly `vis_token` cookie that EventSource carries on SSE connect.

   Theme: vis-light - white surfaces, slate neutrals, one indigo accent -
   CSS variables lifted 1:1 from `internal/theme.clj` light-palette."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [hiccup2.core :as h]
            [ring.core.protocols :as ring-protocols])
  (:import [java.io OutputStream]
           [java.nio.charset StandardCharsets]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

(def ^:private HEARTBEAT_MS 15000)

;; The reasoning ticker (#thinking) shows only the LATEST thought and is
;; transient — the gateway already coalesces reasoning to sentence granularity,
;; but a burst of short sentences still fires several `reasoning.delta` frames, and
;; on a Cloudflare-tunneled SSE each becomes its own write+flush. Coalesce that
;; burst to at most one ticker write per this window so we stop hammering the
;; edge; the pinned thought (`iteration.completed`) and every non-ticker frame
;; still pass through untouched.
(def ^:private ^:const THINKING_COALESCE_MS 100)

(def ^:private ui-load-stamp
  "Re-evaluated on every namespace (re)load — deliberately `def`, NOT
   `defonce`. The gateway contribution's `:rev` rides it so a REPL
   `:reload` that ADDS routes remounts the route table."
  (System/currentTimeMillis))

;; Every script the pages load is VENDORED on the classpath under
;; resources/vis-channel-web/public/ and served by this channel from
;; memory — no CDN, no request ever leaves the host.
(def ^:private JS_ASSETS
  {"htmx.min.js" "vis-channel-web/public/htmx.min.js"
   "htmx-sse.js" "vis-channel-web/public/htmx-sse.js"
   "marked.min.js" "vis-channel-web/public/marked.min.js"
   "prism.min.js" "vis-channel-web/public/prism.min.js"
   "purify.min.js" "vis-channel-web/public/purify.min.js"
   "ui.js" "vis-channel-web/public/ui.js"
   ;; AudioWorklet module (voice capture off the main thread) — fetched by
   ;; ui.js via audioWorklet.addModule, not a <script> tag.
   "rec-worklet.js" "vis-channel-web/public/rec-worklet.js"})

(def ^:private asset-version
  "Cache-buster appended as `?v=` to the /ui/app.css + /ui/js/* URLs. Stamped
   once per JVM (gateway start), so every restart (= a deploy) yields a fresh URL
   and phones/browsers refetch instead of serving a stale cached CSS/JS — iOS
   Safari ignores `Cache-Control: no-cache` on reopened tabs. The asset handlers
   match on PATH and ignore the query, so `?v=…` never affects serving."
  (str (System/currentTimeMillis)))

(def ^:private js-asset-cache
  "Asset name -> file content, read from the classpath ONCE and served
   from memory — no per-request resource IO on the hot path."
  (delay (into {}
               (keep (fn [[nm path]]
                       (when-let [resource (io/resource path)]
                         [nm (slurp resource)])))
               JS_ASSETS)))

(defn- js-asset-handler
  [request]
  (if-let [content (get @js-asset-cache (get-in request [:path-params :asset]))]
    {:status 200
     :headers {"Content-Type" "application/javascript; charset=utf-8" "Cache-Control" "no-cache"}
     :body content}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "unknown asset"}))

;; Vendored fonts (SIL Open Font License 1.1 — free commercial use and
;; redistribution; license texts ship next to the woff2 files):
;;   Hanken Grotesk               — UI + prose
;;   JetBrains Mono (JetBrains)   — code
(def ^:private FONT_ASSETS
  ;; ONE shared copy: the fonts live in core's resources (resources/vis-docs/
  ;; assets/fonts) and are reused by BOTH the docs site and this web UI — core is
  ;; on the extension classpath, so io/resource resolves them. No per-module dup.
  {"hanken-grotesk.woff2" "vis-docs/assets/fonts/hanken-grotesk.woff2"
   "jetbrains-mono.woff2" "vis-docs/assets/fonts/jetbrains-mono.woff2"})

(def ^:private font-asset-cache
  "Asset name -> bytes, read from the classpath ONCE (fonts are binary —
   the js cache slurps strings and would corrupt them)."
  (delay (into {}
               (keep (fn [[nm path]]
                       (when-let [resource (io/resource path)]
                         (with-open [in (io/input-stream resource)
                                     out (java.io.ByteArrayOutputStream.)]

                           (io/copy in out)
                           [nm (.toByteArray out)]))))
               FONT_ASSETS)))

(defn- font-asset-handler
  [request]
  (if-let [^bytes content (get @font-asset-cache (get-in request [:path-params :asset]))]
    {:status 200
     :headers {"Content-Type" "font/woff2" "Cache-Control" "public, max-age=86400"}
     :body (java.io.ByteArrayInputStream. content)}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "unknown font"}))

(def ^:private icons-sprite
  "Feather Icons (MIT) sprite, vendored on the classpath."
  (delay (some-> (io/resource "vis-channel-web/public/icons.svg")
                 slurp)))

(defn- icons-handler
  [_]
  (if-let [sprite @icons-sprite]
    {:status 200
     :headers {"Content-Type" "image/svg+xml; charset=utf-8"
               "Cache-Control" "public, max-age=31536000, immutable"}
     :body sprite}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "no sprite"}))

(defn- icon
  "Inline reference into the vendored Feather sprite. The jump-bottom arrow is
   fully inline so Safari never loses the icon while the fixed dock resizes."
  [id]
  (if (= id "arrow-down")
    [:svg.icon
     {:aria-hidden "true"
      :viewBox "0 0 24 24"
      :fill "none"
      :stroke "currentColor"
      :stroke-width "2"
      :stroke-linecap "round"
      :stroke-linejoin "round"} [:line {:x1 "12" :y1 "5" :x2 "12" :y2 "19"}]
     [:polyline {:points "19 12 12 19 5 12"}]]
    [:svg.icon
     {:aria-hidden "true"
      :focusable "false"
      :viewBox "0 0 24 24"
      :fill "none"
      :stroke "currentColor"
      :stroke-width "2"
      :stroke-linecap "round"
      :stroke-linejoin "round"} [:use {:href (str "/ui/icons.svg#" id)}]]))

;; =============================================================================
;; Canonical IR -> hiccup (the web IR walker)
;; =============================================================================

(def ^:private ansi-sgr-pattern #"\u001B\[([0-9;]*)m")

(defn- ansi-class
  "CSS class for the SGR color codes commonly emitted by test runners.

  The raw shell result stays untouched for the model; this is only the web
  presentation layer so ESC bytes don't show up as literal `^[32m` garbage."
  [codes]
  (let [codes (set (if (str/blank? codes) ["0"] (str/split codes #";")))]
    (cond (contains? codes "7") "rg-hit"
          (contains? codes "0") nil
          (contains? codes "31") "ansi-fg-red"
          (contains? codes "32") "ansi-fg-green"
          (contains? codes "33") "ansi-fg-yellow"
          (contains? codes "34") "ansi-fg-blue"
          (contains? codes "35") "ansi-fg-magenta"
          (contains? codes "36") "ansi-fg-cyan"
          (contains? codes "90") "ansi-fg-dim"
          :else nil)))

(defn- ansi-span [class text] (when (seq text) (if class [:span {:class class} text] text)))

(defn- ansi->hiccup
  "Render ANSI SGR colored text as spans for web code blocks.

  Supports the small color vocabulary used by shell/test output and strips
  reset/control sequences instead of leaking them into the DOM."
  [s]
  (let [m (re-matcher ansi-sgr-pattern s)]
    (loop [idx 0
           class nil
           out []]

      (if (.find m idx)
        (let [start (.start m)
              end (.end m)
              out (cond-> out
                    (< idx start)
                    (conj (ansi-span class (subs s idx start))))]

          (recur end (ansi-class (.group m 1)) out))
        (let [out (cond-> out
                    (< idx (count s))
                    (conj (ansi-span class (subs s idx))))]
          (seq (keep identity out)))))))

;; Mutually recursive with `ir->hiccup` (defined below): code-children->hiccup
;; recurses into IR children, and ir->hiccup calls back here for code nodes.
(declare ast->hiccup)

(defn- code-children->hiccup
  [children]
  (if (and (= 1 (count children))
           (string? (first children))
           (re-find ansi-sgr-pattern (first children)))
    (ansi->hiccup (first children))
    (keep ast->hiccup children)))

(def ^:private diff-kind->class
  "Shared `vis/diff-line-kind` → the web's `df-*` CSS class. The TUI maps the SAME
   kind to an ANSI colour, so a diff fence colours identically in both."
  {:meta "df-meta" :hunk "df-hunk" :add "df-add" :del "df-del" :ctx "df-ctx"})

(defn- diff-line-class [^String line] (diff-kind->class (vis/diff-line-kind line)))

(defn- diff->hiccup
  "Color a unified-diff fence body SERVER-SIDE: one block `<span>` per line,
   classed add / del / hunk / meta so `.ir-diff` paints +/- rows without the
   (un-vendored) Prism diff component. `children` is the fenced body — normally a
   single raw string. Empty lines keep a space so the row height survives."
  [children]
  (let [text
        (apply str (filter string? children))

        text
        (if (str/blank? text) (apply str children) text)]

    (for [line (str/split text #"\n" -1)]
      [:span {:class (diff-line-class line)} (if (= "" line) " " line)])))

(def ^:private ir-tag->html
  "IR tags with a direct HTML counterpart. Anything else renders as a
   div carrying `ir-<tag>` so unknown/new IR never breaks the page."
  {"ir" :div
   "p" :p
   "span" :span
   "strong" :strong
   "b" :strong
   "em" :em
   "i" :em
   "h1" :h1
   "h2" :h2
   "h3" :h3
   "h4" :h4
   "h5" :h5
   "h6" :h6
   "ul" :ul
   "ol" :ol
   "li" :li
   "blockquote" :blockquote
   "hr" :hr
   "br" :br
   "table" :table
   "thead" :thead
   "tbody" :tbody
   "tr" :tr
   "td" :td
   "th" :th
   "del" :del
   "s" :del
   "a" :a})

(defn ast->hiccup
  "Walk a canonical wire IR node (`[\"tag\" {attrs}? & children]`, strings,
   seqs) into hiccup. Total: unknown tags degrade to classed divs,
   non-IR leaves degrade to strings. Never throws on model output."
  [node]
  (cond (string? node) node
        (nil? node) nil
        (number? node) (str node)
        (seq? node) (keep ast->hiccup node)
        (vector? node) (let [[tag second-el & rest-els]
                             node

                             attrs?
                             (map? second-el)

                             attrs
                             (if attrs? second-el {})

                             children
                             (if attrs? rest-els (cons second-el rest-els))]

                         (cond
                           ;; The renderer-local Markdown AST vocabulary: headings are
                           ;; `[:h {:level N} …]`, inline code is `[:c …]`, and `[:code
                           ;; {:lang …} "…"]` is the fenced BLOCK.
                           (= tag "h") (into [(keyword (str "h"
                                                            (-> (or (get attrs "level") 2)
                                                                long
                                                                (max 1)
                                                                (min 6))))]
                                             (keep ast->hiccup children))
                           (= tag "c") [:code (keep ast->hiccup children)]
                           (or (= tag "code") (= tag "pre") (= tag "code-block"))
                           ;; `language-*` is the Prism convention (marked emits it too),
                           ;; so server-rendered and client-rendered fences highlight alike.
                           ;; `.ir-pre` is LOAD-BEARING: it carries overflow-x:auto — without
                           ;; it a wide tool line (cat gutter, diff row) stretches the whole
                           ;; thread horizontally instead of scrolling inside the block.
                           ;; A `diff` fence (patch / write evidence) is colored server-side per
                           ;; line via `diff->hiccup` so adds/dels read without a Prism diff plugin.
                           (if (= "diff"
                                  (some-> (get attrs "lang")
                                          str
                                          str/lower-case))
                             [:pre.ir-pre.ir-diff [:code (diff->hiccup children)]]
                             [:pre.ir-pre
                              [:code {:class (str "language-" (or (get attrs "lang") "txt"))}
                               (code-children->hiccup children)]])
                           (= tag "a") (into [:a {:href (str (get attrs "href")) :rel "noreferrer"}]
                                             (keep ast->hiccup children))
                           :else (if-let [html-tag (ir-tag->html tag)]
                                   (into [html-tag] (keep ast->hiccup children))
                                   (into [:div {:class (str "ir-" (str tag))}]
                                         (keep ast->hiccup children)))))
        :else (str node)))

(defn- md->hiccup
  [markdown]
  (when-not (str/blank? (str markdown))
    (try (ast->hiccup (vis/wire-canonical (vis/markdown->ast (str markdown))))
         (catch Throwable _ [:pre.ir-pre (str markdown)]))))

(defn- inline-md->hiccup
  "Render a SHORT one-line markdown string (a tool-result SUMMARY) as INLINE
   hiccup. `markdown->ast` wraps even a one-liner in a block `[:p …]`; a <p> in
   the badge row would break its flex baseline and force a line break, so we lift
   the paragraph's INLINE children out. The upshot: a summary's inline markdown
   (`code`, *em*, **strong**) renders instead of leaking literal backticks /
   asterisks next to the op label. Falls back to the raw string on any trouble."
  [s]
  (when-not (str/blank? (str s))
    (try (let [ir
               (vis/wire-canonical (vis/markdown->ast (str s)))

               blocks
               (drop 2 ir)

               inline
               (if (and (= 1 (count blocks)) (vector? (first blocks)) (= "p" (ffirst blocks)))
                 (drop 2 (first blocks))
                 blocks)]

           (keep ast->hiccup inline))
         (catch Throwable _ [(str s)]))))

(def ^:private mpl-image-dir
  "Confinement root for the matplotlib PNGs the sandbox shim writes host-side
   (`__vis_mpl_render_file__` → <java.io.tmpdir>/vis-mpl). Only files DIRECTLY
   under this dir are inlined; any other path leaves its fence untouched."
  (delay (.getCanonicalFile (java.io.File. (System/getProperty "java.io.tmpdir") "vis-mpl"))))

(defn- mpl-confined-file
  "The figure `path` as a File IFF it canonicalizes to a regular file directly
   inside the vis-mpl temp dir, else nil — a path-traversal / foreign-path guard
   so a crafted `vis-image` fence can't make the server read arbitrary files."
  [path]
  (try (let [f (.getCanonicalFile (java.io.File. (str path)))]
         (when (and (.isFile f)
                    (= @mpl-image-dir
                       (some-> (.getParentFile f)
                               .getCanonicalFile)))
           f))
       (catch Throwable _ nil)))

(defn- html-text-escape
  "Minimal HTML escape for text spliced into the raw `<figure>`/`<pre>` block
   below — marked passes raw HTML through verbatim, so we escape it ourselves."
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- vis-image-fence->html
  "Turn one `vis-image` fence BODY (header lines summary / abs-path / mime / WxH /
   size, then the optional ASCII plot) into a SELF-CONTAINED HTML block: the PNG
   inlined as a base64 data-URI `<figure><img>` (so a downloaded export stays
   offline-complete), the ASCII plot kept as a collapsed `<details>` fallback.
   nil when the file can't be read confined — the caller then leaves the fence
   as-is, degrading to the plain code-block (path + ASCII)."
  [body]
  (let [lines
        (str/split (str body) #"\n" -1)

        [summary path _mime dims size-label]
        lines

        [w h]
        (when (seq dims) (str/split (str dims) #"x"))

        ascii
        (let [a (str/join "\n" (drop 5 lines))]
          (when-not (str/blank? a) a))]

    (when-let [^java.io.File f (mpl-confined-file path)]
      (let [b64 (.encodeToString (java.util.Base64/getEncoder)
                                 (java.nio.file.Files/readAllBytes (.toPath f)))
            cap (html-text-escape (str/trim (str summary)))]

        (str "\n\n<figure class=\"mpl-fig\">"
             "<img src=\"data:image/png;base64,"
             b64
             "\" alt=\""
             cap
             "\""
             (when (not-empty (some-> w
                                      str/trim))
               (str " width=\"" (str/trim w) "\""))
             (when (not-empty (some-> h
                                      str/trim))
               (str " height=\"" (str/trim h) "\""))
             " loading=\"lazy\">"
             "<figcaption>"
             cap
             (when (not-empty (str/trim (str size-label)))
               (str " · " (html-text-escape (str/trim (str size-label)))))
             "</figcaption></figure>"
             (when ascii
               (str "<details class=\"mpl-ascii\"><summary>ASCII plot</summary>"
                    "<pre>"
                    (html-text-escape ascii)
                    "</pre></details>"))
             "\n\n")))))

(defn- attachment->figure
  "One persisted iteration attachment riding the CANONICAL turn-trace wire
   (`{\"kind\" \"media_type\" \"filename\" \"size\" \"base64\"}`, gateway-hydrated from the
   attachment store) → hiccup, INLINED from the DB bytes (base64 data-URI).
   Image kinds paint as a `<figure><img>`; any other kind degrades to a
   labelled download link so a non-image artifact (csv/json/…) still surfaces.
   This is the restart-durable twin of the live `vis-image` fence: the temp
   PNG is gone after a restart, the persisted bytes are not — so history
   re-renders from the wire, never the disk temp."
  [{:strs [kind media_type filename size base64]}]
  (when (not-empty (str base64))
    (let [mt
          (or (not-empty (str media_type)) "application/octet-stream")

          data-uri
          (str "data:" mt ";base64," base64)

          nm
          (or (not-empty (str filename)) "artifact")

          cap
          (html-text-escape nm)

          sz
          (when (and size (pos? (long size))) (str " · " (long size) " B"))]

      (if (or (= "image" kind) (str/starts-with? mt "image/"))
        [:figure.mpl-fig [:img {:src data-uri :alt cap :loading "lazy"}] [:figcaption cap sz]]
        [:a.artifact-link {:href data-uri :download nm} (str "⬇ " nm) sz]))))


(defn- resolve-image-fences
  "Replace every ````vis-image```` fence in a tool-result markdown string with an
   inline base64 `<img>` (+ ASCII `<details>` fallback) so the web paints
   matplotlib `plt.show()` figures INLINE — the web twin of the TUI's
   Kitty/iTerm2 inline paint, decided from the SAME channel-neutral fence. Cheap
   no-op when the string carries no fence; an unreadable PNG leaves its fence
   untouched (degrades to the code-block path/ASCII)."
  [md]
  (if (and md (str/includes? md "````vis-image"))
    (str/replace md
                 #"(?s)````vis-image\r?\n(.*?)\r?\n````"
                 (fn [[whole body]]
                   (or (vis-image-fence->html body) whole)))
    md))

(defn- strip-image-fences
  "Remove every ````vis-image```` fence from a tool-result markdown string. Used
   by the DB-restored HISTORY trace, where `attachment->figure` re-paints the
   figure from durable `session_attachment` bytes - so the persisted stdout fence
   is redundant (and its temp PNG is gone after a restart, leaving only path/ASCII
   noise). Live streaming keeps `resolve-image-fences`, which paints from the temp
   file that still exists then (no DB round-trip mid-stream)."
  [md]
  (if (and md (str/includes? md "````vis-image"))
    (str/replace md #"(?s)\n?````vis-image\r?\n(.*?)\r?\n````\n?" "")
    md))

(defn- normalize-thinking-text
  "Collapse blank-line runs in reasoning before rendering the web thinking block.
   Delegates to the SHARED `vis/normalize-reasoning` so the web card and the TUI
   thinking bubble normalize reasoning identically (one source of truth)."
  [text]
  (vis/normalize-reasoning text))

(defn- html ^String [hiccup-form] (str (h/html hiccup-form)))

(defn- json-text
  [m]
  (str "{"
       (str/join ","
                 (for [[k v] m]
                   (str (pr-str (name k)) ":" (pr-str (str v)))))
       "}"))

(defn- current-theme-id
  []
  (or (try (some-> (vis/toggle-value :vis-channel-web/theme)
                   name)
           (catch Throwable _ nil))
      vis/default-theme-id))

(defn- current-web-css-root [] (try (vis/web-css-root (current-theme-id)) (catch Throwable _ "")))

(defn- theme-bg-color [css-root] (or (second (re-find #"--bg:([^;}]+)" (str css-root))) "#ffffff"))

(defn- page
  ^String [title & body]
  (let [css-root
        (current-web-css-root)

        theme-bg
        (theme-bg-color css-root)]

    (str
      "<!DOCTYPE html>"
      (h/html
        [:html
         [:head [:meta {:charset "utf-8"}]
          [:meta
           {:name "viewport"
            :content
            "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"}]
          [:meta {:id "theme-color-meta" :name "theme-color" :content theme-bg}]
          [:meta {:name "apple-mobile-web-app-status-bar-style" :content "default"}]
          ;; Mobile Safari data detectors turned cat anchors/ranges like `123:456`
          ;; into tappable phone links. Keep transcript labels literal text.
          [:meta {:name "format-detection" :content "telephone=no, date=no, address=no, email=no"}]
          [:title (str title " · vis")]
          ;; `?v=` cache-buster (asset-version, new every gateway start) so a
          ;; restart/deploy forces a refetch — iOS Safari otherwise serves a stale
          ;; cached app.css/ui.js even with no-cache, on reopened tabs.
          ;; Preload the two vendored variable fonts so first paint has the real
          ;; typeface (no FOUT) — they're same-origin under /ui/fonts.
          [:link
           {:rel "preload"
            :href "/ui/fonts/hanken-grotesk.woff2"
            :as "font"
            :type "font/woff2"
            :crossorigin "anonymous"}]
          [:link
           {:rel "preload"
            :href "/ui/fonts/jetbrains-mono.woff2"
            :as "font"
            :type "font/woff2"
            :crossorigin "anonymous"}]
          [:link {:id "theme-css" :rel "stylesheet" :href (str "/ui/app.css?v=" asset-version)}]
          [:style {:id "theme-vars"} css-root]
          ;; All vendored, all local — nothing loads from outside vis.
          [:script {:src (str "/ui/js/htmx.min.js?v=" asset-version) :defer true}]
          [:script {:src (str "/ui/js/htmx-sse.js?v=" asset-version) :defer true}]
          [:script {:src (str "/ui/js/marked.min.js?v=" asset-version) :defer true}]
          [:script {:src (str "/ui/js/prism.min.js?v=" asset-version) :defer true}]
          [:script {:src (str "/ui/js/purify.min.js?v=" asset-version) :defer true}]
          [:script {:src (str "/ui/js/ui.js?v=" asset-version) :defer true}]]
         (into [:body] body)]))))

;; =============================================================================
;; Canonical wire readers — the gateway serves ONE string-keyed snake_case
;; shape on BOTH transports (`wire/canonical`), so every read below is a
;; plain `(get m "field")` — no tolerant fallbacks, no key normalization.

(defn- status-chip
  [status]
  [:span {:class (str "chip chip-" (or status "idle"))} (or status "idle")])

(defn- bar-title-content
  "Children of the header `.bar-title` - re-rendered over SSE at turn
   boundaries so the status chip and the host-generated title stay live."
  [soul]
  (list [:span.bar-name
         {:role "button"
          :tabindex "0"
          :data-rename (or (get soul "title") "")
          :title "Rename this session"} (or (get soul "title") "Untitled")]
        (status-chip (get soul "status"))))

(defn- routing-footer
  "Provider/model this session routes through, shown as a compact chip in the
   bottom dock DIRECTLY UNDER the composer (moved here from the context rail —
   the session's model belongs next to the input, like the TUI footer). The
   chip opens the same per-session model picker. Resolution mirrors the old
   rail: the PENDING per-session preference wins (what the user just picked /
   the next turn will use), falling back to the engine's last actual routing.
   Takes only `sid` so the SSE `footer` frames can re-render it."
  [sid]
  (when sid
    (let [->name
          (fn [v]
            (cond (keyword? v) (name v)
                  (some? v) (str v)
                  :else nil))

          routing
          (try (get (vis/gateway-context-snapshot sid) "session_routing") (catch Throwable _ nil))

          actual-provider
          (->name (or (get routing "provider") (get routing "current_provider")))

          actual-model
          (->name (or (get routing "model") (get routing "current_model")))

          pref
          (try (vis/gateway-session-model sid) (catch Throwable _ nil))

          provider
          (or (->name (:provider pref)) actual-provider)

          model
          (or (:model pref) actual-model)

          ;; Circuit-breaker awareness: when the intended provider is
          ;; overloaded, surface what's actually serving instead.
          overload
          (when provider (try (vis/model-routing-status provider model) (catch Throwable _ nil)))]

      [:button.foot-routing
       {:type "button"
        :hx-get (str "/ui/session/" sid "/model")
        :hx-target "#modal"
        :hx-swap "innerHTML"
        :aria-label "Change this session's model"
        :title "Change this session's model"} (icon "zap")
       [:span.foot-routing-name
        ;; Display-only: path-style model ids flatten `/`→`-` so the chip
        ;; stays `provider / model`, never three ambiguous segments.
        (let [m (vis/display-model-name model)]
          (cond (and provider m) (str provider " / " m)
                provider provider
                :else "router default"))]
       (when overload
         [:span.foot-routing-overload (icon "info")
          (str (:overloaded-model overload) " → " (or (:serving-model overload) "none"))])])))
(def ^:private reasoning-level-order [:quick :balanced :deep])

(defn- session-resolved-model-info
  "Root model descriptor the router resolves to — the same `resolve-effective-model`
  `providers-modal` reads for its 'default-active' line. Used only to GATE the
  effort chip (does this model accept a reasoning level?). The backend re-gates
  per turn (`loop.clj`), so a stale read here is harmless: at worst the chip
  shows for a model the engine then ignores. Returns nil when the router can't
  resolve (not configured, transient error)."
  []
  (try (vis/resolve-effective-model (vis/get-router)) (catch Throwable _ nil)))

(defn- reasoning-effort-configurable?
  "Twin of the backend gate (`loop.clj/reasoning-effort-configurable?`) and the
  TUI's — true when the resolved model accepts a caller-selected reasoning
  effort. Keeps the chip from offering a lever the model would silently drop."
  [info]
  (and (boolean (:reasoning? info))
       (not= false (:reasoning-effort? info))
       (not= :zai-thinking (:reasoning-style info))))

(defn- session-reasoning-level
  "Current reasoning effort for the chip, mirroring the TUI's global
  `:vis/reasoning-level` toggle (persisted in `~/.vis/config.edn`, shared across
  every channel). Falls back to the toggle's registered default (:balanced) when
  the store hasn't been touched."
  []
  (or (try (vis/toggle-value :vis/reasoning-level) (catch Throwable _ nil)) :balanced))

(defn- reasoning-footer
  "Reasoning-effort control — a TWIN of `routing-footer` in the bottom dock. An
  inline SLIDER across the 3 levels (quick / balanced / deep), the same values
  the TUI footer cycles with Ctrl+R — dragging it replaces the old popup picker.
  Source of truth is the global `:vis/reasoning-level` toggle, so a change here
  is immediately visible to the TUI/Telegram and vice versa. On change the slider
  POSTs /reasoning, which persists the level and OOB-refreshes `#footwrap`.

  Rendered only when the resolved model is reasoning-configurable — matching the
  backend gate (`loop.clj`), so the control never appears for models that would
  ignore the level. Takes only `sid` so the SSE `footer` frames re-render it."
  [sid]
  (when sid
    (let [info (session-resolved-model-info)]
      ;; `nil? info` (router unresolvable) still shows the control: the change is
      ;; harmless and the backend re-gates per turn anyway. Only a resolved,
      ;; non-configurable model hides it.
      (when (or (nil? info) (reasoning-effort-configurable? info))
        (let
          [level (session-reasoning-level)
           idx (case level
                 :quick
                 0

                 :balanced
                 1

                 :deep
                 2

                 1)
           live-label-js
           "var levels=['quick','balanced','deep']; var next=levels[Number(this.value)]||levels[1]; var root=this.closest('.foot-effort'); if(root){ root.dataset.level=next; } var out=root&&root.querySelector('.foot-effort-value'); if(out){ out.textContent=next; }"]

          [:label.foot-effort
           {:title "Reasoning effort — slide quick → balanced → deep"
            :aria-label "Reasoning effort"
            :data-level (name level)} (icon "activity")
           [:span.foot-effort-label [:strong.foot-effort-value (name level)]]
           [:input.foot-effort-slide
            {:type "range"
             :min 0
             :max 2
             :step 1
             :value idx
             :aria-label "Reasoning effort"
             :oninput live-label-js
             :hx-post (str "/ui/session/" sid "/reasoning")
             :hx-trigger "change"
             :hx-vals "js:{level: ['quick','balanced','deep'][Number(event.target.value)]}"
             :hx-swap "none"}]])))))

(defn- abbrev-home
  "Shorten an absolute path for DISPLAY by replacing the user's home dir with
   `~`, matching the TUI footer/navigator. Display-only: the raw absolute path
   still rides hx-vals/aria so remove + canonical comparisons stay exact."
  [path]
  (let [path
        (str path)

        home
        (System/getProperty "user.home")]

    (if (and (seq path) home (str/starts-with? path home))
      (let [suffix (subs path (count home))]
        (if (str/blank? suffix) "~/" (str "~" suffix)))
      path)))

;; =============================================================================

(defn- bubble-foot
  "TUI-faithful bubble footer: the CANONICAL `meta-summary-line`
   (provider/model · in→out · ~$cost · duration) — the same words and
   numbers the TUI bubble footer and the Telegram tagline show. No
   status badge; only a failure states itself, in red."
  [turn]
  (let [meta-line
        (try (vis/meta-summary-line {:tokens (get turn "tokens")
                                     :cost (get turn "cost")
                                     :duration-ms (get turn "duration_ms")})
             (catch Throwable _ nil))

        status
        (str (get turn "status"))

        cancelled?
        (= "cancelled" status)

        failed?
        (= "failed" status)]

    (when (or (seq meta-line) failed? cancelled?)
      [:div.bubble-foot
       ;; a real failure states itself in red; a user-stop is neutral
       (when failed? [:span.foot-bad "failed"])
       ;; a partial answer shows in the bubble body; the stop note replaces it
       ;; when blank, so only badge "stopped" here when there IS a partial answer
       ;; (otherwise the bubble already says "Stopped - you cancelled this turn").
       (when (and cancelled? (seq (get turn "content"))) [:span.foot-stopped "stopped"])
       (when (seq meta-line) [:span.foot-meta meta-line])])))

(defn- role-time
  "Date + time next to the role label — ALWAYS shown now (the
   `:vis/show-timestamps` gate was retired, both channels always show it).
   nil only when no timestamp is known."
  [epoch-ms]
  (when (number? epoch-ms)
    (let [^DateTimeFormatter formatter
          (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")

          ^LocalDateTime time
          (LocalDateTime/ofInstant (Instant/ofEpochMilli (long epoch-ms)) (ZoneId/systemDefault))]

      [:span.role-time (.format formatter time)])))

(defn- live-key-attr [k] (when k {:data-live-key (str k)}))

(defn- with-live-key
  "Attach a `data-live-key` to a hiccup element's ROOT so ui.js dedupes a
   replayed/reconnected `beforeend` fragment (see `hasExistingLiveKey`). A frame
   with NO key is appended AGAIN on every replay window — the web-only duplicate
   the TUI never shows. No-op on a nil key / non-element node."
  [k node]
  (if (and k (vector? node) (keyword? (first node)))
    (let [[tag x & more] node]
      (if (map? x)
        (into [tag (merge x (live-key-attr k))] more)
        (into [tag (live-key-attr k)] (when (some? x) (cons x more)))))
    node))

(defn- turn-live-key
  "Stable idempotency key for a rendered turn fragment.

  Live gateway turns have a transient gateway `:turn_id`, but the persisted
  transcript rehydrates with the engine row id. Terminal live events carry that
  row id as `:engine_turn_id`; prefer it so a refreshed/reconnected web page can
  drop replayed answer/trace fragments instead of showing the same turn twice."
  [prefix turn]
  (when-let [tid (or (not-empty (str (get turn "engine_turn_id")))
                     (not-empty (str (get turn "turn_id"))))]
    (str prefix ":" tid)))

(defn- with-live-before
  "Attach `data-live-before` — a live-key PREFIX — to a hiccup element's ROOT.
   ui.js (and the server-side inflight replay) inserts such a `beforeend`
   fragment BEFORE the first existing sibling whose `data-live-key` starts
   with the prefix, falling back to a plain append. Used to pin the
   `iteration.completed` thinking card ABOVE the iteration's already-streamed
   tool cards, matching the settled transcript's order (thinking first, then
   forms). No-op on a nil prefix / non-element node."
  [prefix node]
  (if (and prefix (vector? node) (keyword? (first node)))
    (let [[tag x & more] node]
      (if (map? x)
        (into [tag (assoc x :data-live-before (str prefix))] more)
        (into [tag {:data-live-before (str prefix)}] (when (some? x) (cons x more)))))
    node))

(defn- attachment-media
  "Inline image attachments shown INSIDE a user bubble. `atts` is
   `[{:media-type :base64 :filename}]` (persisted `session_turn_attachment`
   rows, or the just-uploaded parts); each image renders as a data-URL
   thumbnail that opens full size in a new tab. Non-image / byte-less entries
   are skipped; returns nil when there's nothing to show so the bubble stays
   text-only."
  [atts]
  (when-let [imgs (seq (filter (fn [a]
                                 (and (get a "base64")
                                      (str/starts-with? (str (or (get a "media_type") "image/"))
                                                        "image/")))
                               atts))]
    [:div.bubble-media
     (for [a imgs]
       (let [src (str "data:" (or (get a "media_type") "image/png") ";base64," (get a "base64"))]
         [:a.bubble-img {:href src :target "_blank" :rel "noopener"}
          [:img {:src src :alt (str (or (get a "filename") "image")) :loading "lazy"}]]))]))

(defn- user-bubble
  "TUI anatomy: 'You' role label in slate (:user-role-fg). The raw text
   rides in data-md; ui.js re-renders it through the vendored `marked`
   (MIT) for full markdown fidelity, falling back to the plain text. Any
   image attachments render as inline thumbnails above the text."
  ([text] (user-bubble text nil nil nil))
  ([text epoch-ms] (user-bubble text epoch-ms nil nil))
  ([text epoch-ms live-key] (user-bubble text epoch-ms live-key nil))
  ([text epoch-ms live-key attachments]
   [:div.bubble.b-user (live-key-attr live-key) [:div.role.role-user "You" (role-time epoch-ms)]
    (attachment-media attachments) [:div.prose.md {:data-md (str text)} [:p (str text)]]]))

(defn- turn-attachments
  "Image attachments for a RESTORED turn, read straight off the gateway wire
   turn's `:attachments` — the gateway populates it uniformly for live turns
   (the just-uploaded parts) and persisted history (`gateway-list-turns` /
   `gateway-transcript` hydrate it from the DB). Reading it here keeps the web
   bubble on the SAME gateway-sourced payload every other channel consumes,
   instead of reaching around the gateway into persistence. Returns
   `[{\"media_type\" \"base64\" \"filename\" …}]` or nil."
  [turn]
  (seq (get turn "attachments")))

(defn- strip-label
  "Drop a leading `WHAT HAPPENED: ` / `NEXT STEP: ` prefix — the card supplies its
   own visual section labels, so the ALL-CAPS prose lead is redundant."
  [s]
  (some-> s
          str
          (str/replace #"^(?i)(WHAT HAPPENED|NEXT STEP):\s*" "")))

(defn- provider-error-card
  "A styled provider-failure CARD built from the canonical `:vis/provider-error-data`
   (title / explanation / next-step / facts / raw body) — a distinct, scannable
   surface instead of undifferentiated prose. Kind drives the accent color via a
   `pe-<kind>` class (see app.css)."
  [{:strs [title kind explanation next_step status provider_id request_id attempts body]}]
  (let [facts (cond-> []
                status
                (conj ["HTTP" (str status)])

                provider_id
                (conj ["Provider" (str provider_id)])

                request_id
                (conj ["Request id" (str request_id)]))]
    [:div.provider-error-card {:class (str "pe-" (or kind "generic"))}
     [:div.pe-head [:span.pe-icon "⚠"] [:span.pe-title (str (or title "Provider unavailable"))]]
     (when explanation [:p.pe-what (strip-label explanation)])
     (when next_step [:p.pe-next [:span.pe-next-label "Next step"] (strip-label next_step)])
     ;; Per-provider breakdown — WHY each provider bowed out.
     (when (seq attempts)
       [:ul.pe-attempts
        (for [{:strs [provider model status reason]} attempts]
          [:li {:key (str provider "/" model)}
           [:span.pe-prov
            (str provider
                 (when-let [m (vis/display-model-name (some-> model
                                                              str))]
                   (str "/" m)))] (when status [:span.pe-status (str status)])
           (when reason [:span.pe-reason (str reason)])])])
     (when (seq facts)
       [:ul.pe-facts
        (for [[k v] facts]
          [:li {:key k} [:span.pe-k (str k)] [:span.pe-v (str v)]])])
     (when body [:details.pe-body [:summary "Provider response"] [:pre (str body)]])]))

(defn- content-block->hiccup
  "Render one canonical string-keyed content block."
  [block]
  (case (get block "type")
    "prose"
    (let [md (get block "markdown" "")]
      [:div.prose.md {:data-md md} (md->hiccup md)])

    "code"
    [:pre
     [:code
      {:class (some-> (get block "language")
                      (str "language-"))} (get block "text" "")]]

    "error"
    (provider-error-card {"title" (get block "code")
                          "explanation" (get block "message")
                          "status" (get block "status")
                          "provider_id" (get block "provider")
                          "request_id" (get block "request_id")
                          "attempts" (get block "attempts")
                          "body" (get block "body")})

    "notice"
    [:div.prose.md [:p (get block "message" "")]]

    "tool"
    [:details [:summary (str (get block "tool") " · " (get block "status"))]
     [:pre (str (or (get block "output") (get block "error") ""))]]

    "attachment"
    [:a {:href (str "/v1/attachments/" (get block "attachment_id"))}
     (get block "name" "attachment")]

    "reasoning"
    (when (= "visible" (get block "visibility"))
      [:details [:summary "Reasoning"] [:pre (get block "text" "")]])

    [:div.prose.md [:p (str "Unsupported content block: " (get block "type"))]]))

(defn- vis-bubble
  "Render the assistant's canonical ordered content blocks."
  [turn]
  (let [status
        (str (get turn "status"))

        cancelled?
        (= "cancelled" status)

        blocks
        (vec (or (get turn "content") []))]

    [:div.bubble.b-vis (live-key-attr (turn-live-key "vis" turn))
     [:div.role.role-vis "Vis" (role-time (or (get turn "created_at") (get turn "started_at")))]
     (if (and cancelled? (empty? blocks))
       [:p.bubble-stopped "Stopped — you cancelled this turn."]
       (for [block blocks]
         ^{:key (get block "id")} (content-block->hiccup block))) (bubble-foot turn)]))

;; ── Trace blocks — the TUI transcript's code/result/error cells ──
;; Shown INLINE in the thread (no Work fold, nothing hidden): the live
;; SSE stream and the DB-restored history render the SAME blocks.

(def ^:private silent-result-sentinels
  "Engine RESULT markers meaning 'this form produced nothing to show':
   `done` returns \"vis_answer\" (the answer renders as the chat bubble),
   the title setter returns \"vis_silent\". A form carrying either is engine
   chrome — never a visible code/result row. This sentinel is the ONLY hide
   signal; there is no engine-verb name list (the same rule the live stream
   gates on via the gateway's `:silent`)."
  #{"vis_answer" "vis_silent"})

(defn- engine-chrome-form?
  "True when a persisted form envelope is engine chrome — its result is a
   `vis_answer` / `vis_silent` sentinel, so its raw call source is noise in
   the chat thread."
  [form]
  (contains? silent-result-sentinels (:result form)))

(def ^:private trace-preview-line-limit
  "Rows that stay visible before a trace block offers a `+N more` disclosure.
   Canonical height — shared with the TUI thinking bubble via the gateway."
  vis/reasoning-preview-line-limit)

(def ^:private trace-collapse-min-hidden
  "Minimum hidden rows before a trace folds behind `+N more` at all — below this
   the whole trace renders inline. Canonical, shared with the TUI via the gateway."
  vis/reasoning-collapse-min-hidden)

(defn- split-preview-tail
  [text]
  (let [lines
        (str/split-lines (str/trimr (str text)))

        hidden
        (long (max 0 (- (count lines) (long trace-preview-line-limit))))]

    ;; Don't split off a tiny tail: a `+1 more` toggle that reveals one extra
    ;; line is pure friction. Below the min, render the whole trace inline.
    (if (< hidden (long trace-collapse-min-hidden))
      {:preview (str/join "\n" lines) :tail "" :hidden-count 0}
      {:preview (str/join "\n" (take trace-preview-line-limit lines))
       :tail (str/join "\n" (drop trace-preview-line-limit lines))
       :hidden-count hidden})))

(defn- block-code
  [code]
  ;; The model writes Python (RLM contract) — tag the block so the
  ;; vendored Prism highlights it natively. Beautified with ruff (cached,
  ;; via `vis/beautify-python`) before streaming so the trace shows tidy,
  ;; consistently-wrapped Python; falls back to verbatim source if ruff is
  ;; unavailable. Long bodies scroll, never clipped.
  (let [code-str (vis/beautify-python code)]
    [:div.block-code-card [:div.block.block-code [:pre.ir-pre [:code.language-python code-str]]]]))

(defn- result-markdown
  "Human trace result body. Stdout is model-context only; channels render the
   form RETURN value as markdown for people. Strings are markdown verbatim;
   non-strings are fenced as EDN so they stay readable and copyable."
  [result]
  (when-not (contains? silent-result-sentinels (str result))
    (cond (nil? result) nil
          (string? result) (not-empty (str/trimr result))
          :else (str "```edn\n" (pr-str result) "\n```"))))

(def ^:private tool-color-var
  "Per-tool BADGE color for native tool result cards as THEMED CSS vars — the
   `--tool-*` custom properties `theme->web-css-vars` emits from the SAME palette
   tokens the TUI paints op-cards with, so the badge tracks every theme
   (light/dark/…). Keyed by the `:color-role` a native tool declares; mirrors the
   TUI's `tool-color-role->fg`."
  {:tool-color/read "var(--tool-read)"
   :tool-color/search "var(--tool-search)"
   :tool-color/preview "var(--tool-preview)"
   :tool-color/edit "var(--tool-edit)"
   :tool-color/create "var(--tool-create)"
   :tool-color/delete "var(--tool-delete)"
   :tool-color/move "var(--tool-move)"
   :tool-color/shell "var(--tool-shell)"
   :tool-color/meta "var(--tool-meta)"
   :tool-color/test "var(--tool-test)"})

(defn- result-card->hiccup
  "One op-card descriptor (`vis/result-card`) → its hiccup: a collapsible
   `<details>` when it has a body (chevroned badge row is the `<summary>`, body
   reveals on expand), else a flat summary-only badge row. Shared by a single
   native-tool form AND each card of a print-many block, so every op-card paints
   identically however many a form carries. nil when there's neither body nor
   summary."
  [{:keys [label color-role summary body]} & [strip-fences?]]
  (let [body-md
        ((if strip-fences? strip-image-fences resolve-image-fences) (result-markdown body))

        ;; A body that STARTS with a blank line is the tool's BREATHE signal
        ;; (rg emits one so the hits don't glue to `N hits in M files`). The
        ;; markdown renderer drops that leading blank, so we honor it here with
        ;; a top-margin class — the TUI card mirrors this with a spacer row.
        head-gap?
        (boolean (some-> body
                         str
                         (str/starts-with? "\n")))]

    (when (or body-md summary)
      ;; The op-card TOOL-NAME badge (the colored uppercase CAT/RG/PATCH label),
      ;; painted in the tool's themed color; non-tool forms fall back to "result".
      (let [color
            (get tool-color-var color-role)

            label-attr
            (if (and label color) {:style (str "color:" color)} {})

            head
            [(or (when color label) "result")
             (when summary (into [:span.block-result-summary] (inline-md->hiccup summary)))]]

        (if body-md
          [:details.block-result-card (into [:summary.block-sum.block-result-label label-attr] head)
           [:div.block.block-result.md
            (cond-> {:data-md body-md}
              head-gap?
              (assoc :class "has-head-gap")) (md->hiccup body-md)]]
          [:div.block-result-card (into [:div.block-result-label label-attr] head)])))))

(defn- block-result
  "The form's RETURN value as a result card. A native tool form (cat/rg/patch/…)
   labels the card with its TOOL name, painted in the tool's color — the web twin
   of the TUI's colored op-card badge. Non-tool forms keep the plain `result`
   label.

   Mirrors the TUI's `maybe-collapse-block`: a tool result with a body is a
   COLLAPSIBLE disclosure — collapsed by default it shows only the chevron + op
   badge + one-line summary; clicking the header expands the FULL body in place.
   The `.block-sum` chevron (▸ → ▾) and `details[open]` rotation are the same
   affordance `block-thinking` uses. A summary-only tool (move/delete/exists)
   has no body, so it stays a flat badge row with no chevron."
  ([result] (block-result result nil false))
  ([result form] (block-result result form false))
  ([result form strip-fences?]
   ;; The op-card decision — `tool?`, the badge LABEL/colour, the HEADLINE
   ;; `:summary` ("5 hits in 1 file"), the `:body`, and whether it's COLLAPSIBLE
   ;; — is made ONCE in the gateway (`vis/result-card`); we just paint it. Both the
   ;; TUI and web consume that same descriptor via `vis/result-cards`, so the
   ;; badge/colour/summary can't drift between channels. A print-many python block
   ;; yields SEVERAL cards (one per printed tool result); a single native form one;
   ;; a non-tool form none — then we render its raw `:result` value (never a
   ;; pr-str of the map).
   (let [nodes (if-let [cards (seq (vis/result-cards form))]
                 (vec (keep #(result-card->hiccup % strip-fences?) cards))
                 ;; non-tool form: synthesize a labelless card from the raw value so
                 ;; the SAME renderer paints it (collapsible "result" disclosure).
                 (when-let [n (result-card->hiccup {:body result} strip-fences?)]
                   [n]))]
     (cond (empty? nodes) nil
           (= 1 (count nodes)) (first nodes)
           ;; SEVERAL cards: stack each as its own disclosure under one container.
           :else (into [:div.block-result-cards] nodes)))))

(defn- block-prose
  "The model's commentary returned ALONGSIDE a tool call — rendered as plain
   MARKDOWN (the same `.prose.md` treatment as the final answer), NOT as a
   `thinking` card. It's the model talking, not reasoning."
  ([text] (block-prose text nil))
  ([text live-key]
   (when-not (str/blank? (str text))
     (let [t (str/trim (str text))]
       [:div.block.block-prose.prose.md (merge {:data-md t} (live-key-attr live-key))
        (md->hiccup t)]))))

(defn- error-text
  "LEAN error body: message (+ line/col, + hint when not already in the
   message). A persisted error map nests host trace/data chains nobody can
   act on in the thread — never pr-str it. Wire strings pass verbatim."
  [error]
  (if-not (map? error)
    (str error)
    (let [msg
          (or (not-empty (str (get error "message")))
              (not-empty (some-> (get error "type")
                                 str))
              ;; No message/type: surface the data (minus bulky trace/raw)
              ;; instead of a blank "error" — a dropped :message must NOT hide
              ;; the real failure behind a content-free word.
              (when-let [d (not-empty (dissoc (get error "data") "trace" "raw_data"))]
                (str "error: " (pr-str d)))
              "error (the engine produced no message — please report)")

          hint
          (get error "hint")

          {:strs [line column]}
          (get error "data")]

      (cond-> msg
        (and line column)
        (str " (line " line ", col " column ")")

        (and hint (not (str/includes? msg (str hint))))
        (str "\nhint: " hint)))))

(defn- cancellation-error?
  "True when `error` is the internal cancellation/interrupt thrown when the
   user STOPS a turn — not a real failure, so it shouldn't render as a red
   error with a raw `java.util.concurrent.CancellationException`."
  [error]
  (let [s (str (if (map? error) (or (get error "message") (get error "type")) error))]
    (boolean (re-find #"(?i)cancellation|cancelled|canceled|interrupt" s))))

(defn- block-error
  ([error] (block-error error nil))
  ([error live-key]
   (if (cancellation-error? error)
     ;; user stopped the turn — a muted note, not a red error row
     [:div.block.block-stopped (live-key-attr live-key) [:span.block-tag "stopped"]
      [:span.act-dim "you stopped this turn"]]
     [:div.block.block-error (live-key-attr live-key) [:span.block-tag.bad "error"]
      [:pre.ir-pre.act-error [:code (error-text error)]]])))

(defn- live-error-frame
  "The live error fragment for an `iteration.error` / `block.output` event. When
   the gateway attached the canonical `:provider-error-data` (a genuine provider
   failure), paint the SAME styled CARD the final settled turn bubble renders
   — one source of truth, so live and settled never diverge. Otherwise the
   plain lean error row."
  [event live-key]
  (if-let [ped (get event "provider_error_data")]
    [:div.pe-live (live-key-attr live-key) (provider-error-card ped)]
    (block-error (get event "error") live-key)))

(defn- think-md->hiccup
  "Reasoning is line-oriented (a thinking trace, not flowing prose): lift each
  bare newline to a HARD break so the server-side IR matches ui.js `marked`
  ({:breaks true}) - both render a <br>, so the live #thinking ticker and the
  pinned block paint identically with no merge-then-split flicker. Routed through
  the SHARED `vis/reasoning->ir` (normalize + :soft-break :hard) — the SAME entry
  point the TUI thinking bubble uses."
  [markdown]
  (when-not (str/blank? (str markdown))
    (try (ast->hiccup (vis/reasoning->ast (str markdown)))
         (catch Throwable _ (md->hiccup markdown)))))

(defn- block-thinking
  "The iteration's reasoning, pinned PERMANENTLY into the thread at the
   iteration boundary - the #thinking ticker shows only the moving tail
   while streaming and is wiped at turn end; without this block the
   thinking vanished the moment streaming finished. Text is TRIMMED and
   then rides in data-md so ui.js re-renders it through `marked`, matching
   answer bubbles while retaining the server-side IR fallback.

   Optional `live-key` makes replay/reconnect/poll duplicate deliveries
   idempotent: ui.js drops beforeend fragments whose data-live-key is
   already present in the thread."
  ([text] (block-thinking text nil))
  ([text live-key]
   (let [t (normalize-thinking-text text)]
     (when-not (str/blank? t)
       (let [{:keys [preview tail hidden-count]} (split-preview-tail t)]
         [:div.block-thinking-card (live-key-attr live-key) [:div.block-thinking-label "thinking"]
          [:div.block.block-thinking
           [:div.block-think-body.md {:data-md preview} (think-md->hiccup preview)]
           (when (pos? (long hidden-count))
             [:details.block-thinking-more
              [:summary.block-sum [:span.block-summary-label (str "+" hidden-count " more")]]
              [:div.block-think-body.md {:data-md tail} (think-md->hiccup tail)]])]])))))

;; ── Virtualised thread (web twin of the TUI react-window scrollback) ──
;; The page renders only the most recent INITIAL_TURN_WINDOW turns; older
;; turns load on scroll-up via the `.load-older` sentinel, and each turn's
;; trace (code/results) loads only when expanded. Off-screen / collapsed
;; content never crosses the wire — the same "paint only what's near the
;; viewport" rule `virtual.clj` enforces for Lanterna.

(def ^:private INITIAL_TURN_WINDOW
  "How many recent turns the initial page renders. Older ones page in on
   scroll-up."
  6)
(def ^:private ^:const OLDER_TURN_PAGE
  "How many older turns one scroll-up `.load-older` fetch returns."
  6)

(defn- older-sentinel
  "Top-of-thread infinite-scroll trigger. When it scrolls into view (user
   scrolls up) it hx-gets the previous page of turns and replaces itself with
   them plus a fresh sentinel. Uses `intersect` (IntersectionObserver) rather
   than `revealed`: it fires reliably when the sentinel enters the viewport —
   including when it's already visible on a short thread — whereas `revealed`
   is scroll-event-driven and misses programmatic scrolls / already-visible
   edge cases."
  [sid before-tid]
  [:div.load-older
   {:hx-get (str "/ui/session/" sid "/turns?before=" before-tid)
    :hx-trigger "intersect once"
    :hx-swap "outerHTML"} [:div.block-loading "loading earlier…"]])

(defn- empty-iteration-error?
  "True for the engine's bookkeeping-only `empty iteration` artifact.
   It means the provider produced no actionable block for that loop lap;
   it is useful in logs/DB for forensics, but showing it as a red user-facing
   error makes a normal transcript look broken."
  [error]
  (let [msg (str (if (map? error) (or (get error "message") (get error "type")) error))]
    (= "empty iteration" (str/trim (str/lower-case msg)))))

(defn- engine-empty-iteration-form?
  "The persisted placeholder form written when a provider loop lap had no
   code, no tools, and no answer. Hide it from transcript trace rendering; the
   surrounding real iterations still render normally."
  [form]
  (and (str/blank? (str (:src form)))
       (str/blank? (str (:stdout form)))
       (nil? (:result form))
       (empty-iteration-error? (:error form))))

(defn- iter-has-trace?
  "True when iteration `it` produced something worth SHOWING — reasoning,
   real code (not an engine verb like done()), a tool op, a surfaced error,
   or a non-answer result. Direct answer / engine-bookkeeping-only iterations
   have none, so their trace body should not render a blank/error artifact."
  [it]
  (or (not (str/blank? (str (get it "thinking"))))
      (some? (get it "error"))
      (boolean (some (fn [form]
                       (when-not (engine-empty-iteration-form? form)
                         (or (and (:src form)
                                  (not (str/blank? (str (:src form))))
                                  (not (engine-chrome-form? form)))
                             (not (str/blank? (str (:stdout form))))
                             (some? (:error form)))))
                     (get it "forms")))))

(defn- trace-visible-iterations [iters] (filterv iter-has-trace? iters))

(defn- wire-env->form
  "One canonical wire form envelope (a `:forms` entry off `gateway-turn-trace`)
   → the kebab display contract the SHARED projections consume
   (`vis/coalesce-forms`, `vis/hide-tool-code?`, `vis/result-cards`): the
   display-key set via `vis/form<-wire` (tolerant of snake_case keys +
   stringified keyword values), plus the raw trace surfaces
   (`:src`/`:stdout`/`:result`/`:error`) and the `:success?` flag layered
   verbatim — the web twin of the TUI's restored-envelope projection."
  [env]
  (cond-> (merge (vis/form<-wire env)
                 {:src (or (get env "src") (get env "code"))
                  :stdout (get env "stdout")
                  :success? (nil? (get env "error"))})
    (contains? env "result")
    (assoc :result (get env "result"))

    (contains? env "error")
    (assoc :error (get env "error"))))

(defn- trace-body
  "The code/results/tools body of one finished turn's trace, read from the
   gateway's CANONICAL turn-trace wire (`vis/gateway-turn-trace`) — the same
   shape a remote client (TUI / mobile) receives, so the web holds no
   in-process shape privilege and live == replay == every-channel by
   construction. Returns the `.trace-body` div (so an hx-get outerHTML swap
   drops it straight over the lazy placeholder). A turn that was a DIRECT
   answer (no code/tools) renders a short note instead of a blank body. nil
   only on read failure / no rows visible yet.

   Generated artifacts (matplotlib figures / other produced files) re-render
   from the persisted attachment BYTES the gateway hydrates onto each
   iteration's `:attachments`, not the live `$TMPDIR/vis-mpl` temp file — so
   an image still paints after a restart that wiped the temp dir."
  [turn]
  (try
    (when-let [tid (or (not-empty (str (get turn "engine_turn_id")))
                       (not-empty (str (get turn "turn_id"))))]
      (let [iters (vis/gateway-turn-trace (get turn "session_id") tid)
            visible-iters (trace-visible-iterations (vec iters))]

        (cond (empty? iters) nil
              (empty? visible-iters) [:div.trace-body
                                      [:p.empty "Direct answer — no tool calls or code this turn."]]
              :else
              [:div.trace-body
               (for [it visible-iters]
                 (list (block-thinking (get it "thinking"))
                       (when (get it "error") (block-error (get it "error")))
                       ;; The model's markdown prose returned ALONGSIDE its tool call —
                       ;; its commentary, rendered as MARKDOWN ABOVE the code+result
                       ;; ("here's what I did"), distinct from the dim thinking trace.
                       ;; Placed BETWEEN thinking and code to match the live stream
                       ;; (loop emits `:assistant-prose` before the code runs) — else a
                       ;; refresh moved it below the code and it read as missing.
                       (block-prose (:assistant_prose it))
                       ;; Per form: the raw code the model wrote, then what it PRINTED
                       ;; (the single display surface), then any error. Each wire
                       ;; envelope is projected through `wire-env->form` so the shared
                       ;; projections below see the canonical display contract.
                       ;; Adjacent coalescable native cards fold via shared
                       ;; `vis/coalesce-forms` (the SAME projection the TUI applies):
                       ;; same-file cat/patch become one multi-span/multi-diff card,
                       ;; and per-file format_code acks become one roll-up card.
                       (for [form (vis/coalesce-forms (into []
                                                            (comp (remove
                                                                    engine-empty-iteration-form?)
                                                                  (map wire-env->form))
                                                            (or (get it "forms") [])))]
                         (list (when-let [src (:src form)]
                                 (when-not (or (str/blank? (str src))
                                               (engine-chrome-form? form)
                                               (vis/hide-tool-code? form))
                                   (block-code src)))
                               (block-result (:result form) form true)
                               (when (:error form) (block-error (:error form)))))
                       ;; Produced artifacts (figures/files) restored from the gateway-
                       ;; hydrated bytes — durable across a restart, unlike the temp fence.
                       (keep attachment->figure (get it "attachments"))))])))
    (catch Throwable _ nil)))

(defn- trace-lazy
  "Collapsed trace disclosure whose code/results body is fetched only
   when first expanded (hx-get on the native <details> toggle). Keeps the
   initial page and scroll-up payloads tiny - the heavy code/results blob
   crosses the wire on demand.

   Usually rendered when the turn has more than one DISPLAYABLE iteration.
   Failed provider turns render even with one visible iteration, because that
   single iteration carries the durable provider-error evidence. In vis's loop
   a tool/code call ends the reply (the engine feeds results back on the NEXT
   iteration), so a single visible successful iteration is usually a direct
   answer with no useful trace. Engine bookkeeping artifacts like `empty
   iteration` are deliberately excluded from this count."
  [turn]
  ;; `not-empty` guards the Clojure footgun where an EMPTY-string
  ;; engine_turn_id (truthy!) would win over the real turn_id and build a
  ;; broken `/turn//trace` URL.
  (when-let [tid (or (not-empty (str (get turn "engine_turn_id")))
                     (not-empty (str (get turn "turn_id"))))]
    (let [sid (get turn "session_id")
          status (str (get turn "status"))
          raw (get turn "iteration_count")
          visible-count (try (when-let [iters (vis/gateway-turn-trace sid tid)]
                               (count (trace-visible-iterations (vec iters))))
                             (catch Throwable _ nil))
          n (cond (some? visible-count) visible-count
                  (= 1 raw) 1
                  (number? raw) raw
                  :else 0)]

      (when (or (> (long n) 1)
                (and (contains? #{"failed" ":failed" "error" ":error"} status) (pos? (long n))))
        (let [url (str "/ui/session/" sid "/turn/" tid "/trace")]
          [:details.trace
           (merge {:hx-get url
                   ;; Put the listener on the <details> itself. A
                   ;; child trigger (`toggle from:closest details`)
                   ;; can miss in HTMX/proxy/browser combinations
                   ;; and leaves the user staring at the lazy loader.
                   :hx-trigger "toggle"
                   :hx-target "find .trace-body"
                   ;; JS fallback reads this when a proxy/browser misses htmx
                   ;; on the native details toggle.
                   :data-trace-url url
                   :hx-swap "outerHTML"}
                  (live-key-attr (turn-live-key "trace" turn)))
           [:summary.block-sum.trace-head
            [:span.block-tag (str n " iteration" (when (not= 1 n) "s"))]]
           [:div.trace-body [:div.block-loading "loading…"]]])))))

(defn- settled-vis-bubble
  "A finished Vis bubble with the durable trace INSIDE it. The live shell is
   replaced by this exact shape at turn end, and restored history uses it too,
   so streamed work neither floats outside Vis nor disappears after refresh."
  [turn]
  (let [[tag attrs role & answer] (vis-bubble turn)]
    (into [tag attrs role (or (trace-body turn) (trace-lazy turn))] answer)))

(defn- turn-block
  "One restored turn. A running turn's Vis shell is rendered in `#live` from
   its replayed frames; a finished turn keeps its full work trace in the Vis
   bubble, followed by the final answer."
  ([turn] (turn-block turn false))
  ([turn live-replay?]
   (let [status
         (get turn "status")

         running?
         (= "streaming" status)]

     (list
       [:div.tsep]
       (user-bubble (get turn "request")
                    (get turn "started_at")
                    (turn-live-key "user" turn)
                    (turn-attachments turn))
       (cond
         (or (seq (get turn "content")) (get turn "error")) (settled-vis-bubble turn)
         (and running? live-replay?) nil
         running? nil
         (= "error" status)
         [:div.bubble.b-vis [:div.role.role-vis "Vis"]
          [:p.bubble-stopped
           "This turn ended in an error before producing an answer (provider / infrastructure failure). Re-send your message to retry."]]
         :else [:div.bubble.b-vis [:div.role.role-vis "Vis"]
                [:p.empty (str "(" (or status "no answer") ")")]])))))

;; ── Footer — channel contribution slot `:web.slot/footer` ──────────
;; Extensions contribute footer items by declaring, on their extension
;; map:
;;   {:ext/channel-contributions
;;    {:web.slot/footer [{:id :my.ext/footer
;;                        :fn (fn [{:session/keys [id]}] -> IR | nil)}]}}
;; The fn returns CANONICAL IR (walked by ir->hiccup) — the same
;; contract as the TUI's :tui.slot/header-row, web-flavored.

(defn- footer-context-button
  "Compact footer entry opening a session-scoped modal — the small-icon
   replacement for the removed right context rail. `n`, when positive, rides
   as a count badge beside the icon."
  [{:keys [icon-name label href n]}]
  [:button.foot-chip
   {:type "button"
    :hx-get href
    :hx-target "#modal"
    :hx-swap "innerHTML"
    :aria-label label
    :title label} (icon icon-name) (when (and n (pos? (long n))) [:span.foot-chip-n n])])

(defn- session-usage-totals
  "Cumulative token + cost totals across the session's turns — the SAME numbers
   the TUI footer shows on the right, summed from each turn's per-turn `tokens`
   / `cost` (the gateway populates both uniformly for live + persisted turns).
   Tokens keep the canonical `input`/`output`/`cached` slots so `vis/meta-tokens`
   renders them identically to the per-bubble line; cost collapses to one double.
   Returns `{:tokens {..} :cost <double>}`; either key is absent when nothing
   carried it yet."
  [turns]
  (letfn [(add-slot [acc tokens out-k in-k]
            (let [v (get tokens in-k)]
              (if (number? v) (update-in acc [:tokens out-k] (fnil + 0) (long v)) acc)))]
    (reduce (fn [acc t]
              (let [tokens
                    (get t "tokens")

                    cost
                    (get t "cost")

                    acc
                    (if (map? tokens)
                      (-> acc
                          (add-slot tokens "input" "input")
                          (add-slot tokens "output" "output")
                          (add-slot tokens "cached" "cached"))
                      acc)

                    c
                    (cond (number? cost) cost
                          (and (map? cost) (number? (get cost "total_cost"))) (get cost
                                                                                   "total_cost")
                          :else nil)]

                (cond-> acc
                  (number? c)
                  (update :cost (fnil + 0.0) (double c)))))
            {}
            turns)))

(defn- usage-footer
  "Cumulative session usage chip in the bottom dock — tokens (`11.5k→35
   (cached 4.1k)`) and cost (`~$0.0070`) summed across every turn, rendered with
   the SAME `vis/meta-tokens` / `vis/meta-cost` helpers the per-bubble footer and
   the TUI footer use, so the three surfaces can never drift. nil until a turn
   carries usage. Re-renders on every `footer` SSE frame like the rest of the dock."
  [sid]
  (when sid
    (let [turns
          (try (vis/gateway-list-turns sid) (catch Throwable _ nil))

          {:keys [tokens cost]}
          (session-usage-totals turns)

          tok-text
          (when tokens (vis/meta-tokens tokens))

          cost-text
          (vis/meta-cost cost)]

      (when (or tok-text cost-text)
        [:span.foot-usage {:title "Cumulative session tokens & cost"} (icon "activity")
         (when tok-text [:span.foot-usage-tok tok-text])
         (when cost-text [:span.foot-usage-cost cost-text])]))))

(defn- footer-content
  [sid]
  ;; Bottom dock under the composer — ONE compact line: the session's
  ;; provider/model chip (`routing-footer`) + reasoning-effort chip
  ;; (`reasoning-footer`) + small icon buttons for the session's filesystem
  ;; permissions and backgrounds (folded in from the old right context rail),
  ;; then any extension footer contributions. Re-renders on every `footer` SSE
  ;; frame and on the fs/bg OOB swaps.
  (let [contribs
        (try (vis/channel-contributions-for :web :web.slot/footer) (catch Throwable _ []))

        wi
        (when sid (try (vis/gateway-session-workspace sid) (catch Throwable _ nil)))

        fs-n
        (when wi (+ (if (get wi "root") 1 0) (count (get wi "filesystem_roots"))))

        res
        (when sid (try (vis/list-resources sid) (catch Throwable _ [])))]

    [:footer.foot (routing-footer sid) (reasoning-footer sid) (usage-footer sid)
     (when sid
       (footer-context-button {:icon-name "folder"
                               :label "Filesystem permissions"
                               :href (str "/ui/session/" sid "/fs-picker")
                               :n fs-n}))
     (when sid
       (footer-context-button {:icon-name "layers"
                               :label "Backgrounds"
                               :href (str "/ui/session/" sid "/resources")
                               :n (count res)}))
     (for [{:keys [id] f :fn} contribs]
       (when-let [ir (try (f {:session/id sid}) (catch Throwable _ nil))]
         [:span.foot-item {:data-contrib (str id)} (ast->hiccup ir)]))]))

(defn- session-footer-handler
  "GET /ui/session/:sid/footer — the bottom dock (`footer-content`), lazy-loaded
   by the session page AFTER first paint (hx-trigger=load). Keeps the dock's
   several gateway roundtrips (workspace + resources + routing + reasoning) OFF
   the page's critical render path so a session-switch paints the thread first."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid])
                    parse-uuid)]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if (and sid (vis/gateway-soul sid)) (html (footer-content sid)) "")}))

(defn- user-bubble-html
  ([text] (user-bubble-html text nil nil))
  ([text live-key] (user-bubble-html text live-key nil))
  ([text live-key attachments]
   (html (list [:div.tsep] (user-bubble text (System/currentTimeMillis) live-key attachments)))))

(defn- vis-message-html
  "A full vis chat bubble from a terminal turn event — flies into the
   thread (`#live`), NOT the Work log. Same anatomy as restored turns."
  [event]
  (html (vis-bubble event)))

(declare sidebar-content)

(defn- running-turn-id
  "The turn_id (string) of `sid`'s currently-running turn, or nil."
  [sid]
  (some #(when (= "streaming" (get % "status")) (str (get % "turn_id")))
        (when sid (vis/gateway-list-turns sid))))

(defn- stop-button
  "The composer's stop control - shown only while a turn runs. POSTs the cancel
   route; the SSE turn-finish frame clears #turnctl back to empty."
  [sid]
  [:button.stop-turn
   {:type "button"
    :hx-post (str "/ui/session/" sid "/cancel-turn")
    :hx-swap "none"
    :aria-label "Stop turn"
    :title "Stop this turn"} (icon "x") [:span "Stop"]])

(defn- cancel-turn-handler
  "POST /ui/session/:sid/cancel-turn - fire the running turn's cancellation
   token via vis/gateway-cancel-turn!. The live SSE stream carries the resulting
   `:cancelled` turn and clears the stop control, so this returns an empty body."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        tid
        (running-turn-id sid)]

    (when (and sid tid) (try (vis/gateway-cancel-turn! sid tid) (catch Throwable _ nil)))
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body ""}))


(defn- queued-content
  ;; 1-arity fetches the turns (SSE `queued` frame / OOB swap); the 2-arity
  ;; reuses turns the caller ALREADY fetched (session-page) so a page render
  ;; doesn't pay a second `gateway-list-turns` loopback just to find the queue.
  ([sid] (queued-content sid (vis/gateway-list-turns sid)))
  ([sid turns]
   (let [items (seq (filter #(= "queued" (get % "status")) turns))] ; oldest-first (item #1 fires next, at top) — matches the TUI queue order
     (when items
       [:div.queued-panel [:div.queued-title "Queued"]
        (for [{:strs [turn_id request]} items]
          [:form.queued-item {:data-turn-id (str turn_id)}
           [:textarea.queued-edit
            {:name "request"
             :rows 2
             :aria-label "Queued message"
             :autocomplete "off"
             :autocapitalize "off"
             :autocorrect "off"
             :spellcheck "false"
             :hx-post (str "/ui/session/" sid "/queued/" turn_id "/update")
             :hx-trigger "input changed delay:600ms"
             :hx-swap "none"} (str request)]
           [:div.queued-actions
            [:button.queued-del
             {:type "button"
              :aria-label "Remove queued message"
              :title "Remove queued message"
              :hx-post (str "/ui/session/" sid "/queued/" turn_id "/delete")
              :hx-target "#queued"
              :hx-swap "innerHTML"} (icon "trash")]]])]))))

(defn- oob-queued [sid] (html [:div#queued {:hx-swap-oob "innerHTML"} (queued-content sid)]))

(defn- chrome-frames
  "Page-chrome SSE frames: the header title/status chip and the session
   drawer. Without these the RUNNING chip and the sidebar dot freeze at
   page-load state and a host-set title never shows up."
  [sid]
  (let [soul (try (vis/gateway-soul sid) (catch Throwable _ nil))]
    [{:event "bartitle" :html (html (bar-title-content soul))}
     {:event "sidebar" :html (html (sidebar-content sid))}]))

;; =============================================================================
;; SSE: engine events -> named HTML fragments for htmx sse-swap
;; =============================================================================

(defn- activity-one-line
  "First line of a shell command / op name, trimmed and length-capped, for the
   live ticker label."
  [s n]
  (let [s (-> (str s)
              (str/split #"\n")
              first
              str/trim)]
    (if (> (count s) (long n)) (str (subs s 0 (dec (long n))) "…") s)))

(defn- activity-label
  "Human ticker label for a coarse `activity` event (provider wait, response
   parse, nested shell/tool call)."
  [{:strs [activity cmd op]}]
  (let [what (activity-one-line (or cmd op "") 80)]
    (case (str activity)
      "provider-call"
      "Vis is calling the provider…"

      "response-parse"
      "Vis is parsing the model response…"

      "shell-run"
      (str "Vis is running: " (if (str/blank? what) "shell" what))

      "shell-bg"
      (str "Vis is starting: " (if (str/blank? what) "shell" what))

      "tool"
      (str "Vis is running: " (if (str/blank? what) "tool" what))

      "Vis is working…")))

(defn- activity-ticker
  "Transient `#thinking` ticker: animated dots PLUS a label naming what Vis is
   doing right now, so a long provider/shell/tool call never leaves the bubble
   frozen. Reset to bare dots at the iteration boundary."
  [event]
  (list [:div.dots [:span] [:span] [:span]] [:div.thinking-activity (activity-label event)]))

(defn- live-vis-shell
  "The single live Vis bubble. Trace frames append inside `.live-vis-trace`;
   cumulative answer deltas replace `.live-vis-answer`. At the terminal event
   the whole shell is replaced by `settled-vis-bubble`."
  ([turn] (live-vis-shell turn nil nil))
  ([turn trace-html answer-html]
   [:div.bubble.b-vis
    (merge {:sse-swap "vis-final" :hx-swap "outerHTML"}
           (live-key-attr (turn-live-key "stream" turn)))
    [:div.role.role-vis "Vis" (role-time (get turn "started_at"))]
    [:div.live-vis-trace {:sse-swap "trace-message" :hx-swap "beforeend"}
     (when (seq (str trace-html)) (h/raw trace-html))]
    [:div.live-vis-answer {:sse-swap "content" :hx-swap "beforeend"}
     (when (seq (str answer-html)) (h/raw answer-html))]]))

(defn- event->frames
  "One gateway event -> seq of `{:event name :html fragment}` for the
   htmx SSE extension (`sse-swap=\"<name>\"`)."
  [sid raw-event]
  ;; Normalize the wire event ONCE, at the single choke point every path (SSE
  ;; loop, poll, in-flight replay) funnels through, so a MIRRORED event (crossed
  ;; the cross-process bus with snake_case keys) and a same-process event (kebab)
  ;; read IDENTICALLY below — no branch re-derives key-spelling tolerance, one
  ;; contract, the SAME keys the TUI client normalizes to.
  (let [event
        raw-event

        type
        (get event "type")]

    (case type
      ;; A turn gets ONE Vis shell. Every trace/result stays inside it.
      "turn.started"
      (cond-> [{:event "message" :html (html (live-vis-shell event))}
               {:event "thinking" :html (html (list [:div.dots [:span] [:span] [:span]]))}
               {:event "turnctl" :html (html (stop-button sid))}
               {:event "queued" :html (html (queued-content sid))}]
        (get event "is_queued")
        (into [{:event "message"
                :html (user-bubble-html (get event "request") (turn-live-key "user" event))}])

        true
        (into (chrome-frames sid)))

      ;; Typed-block streaming delta — INCREMENTAL text (`stream-delta`) appended
      ;; per block_id. `field "text"` is reasoning: repaint the transient
      ;; #thinking ticker with the moving tail. `field "markdown"` is provider
      ;; content / end-of-iteration prose: append the raw fragment into
      ;; `.live-vis-answer` (`hx-swap "beforeend"`), so the BROWSER accumulates
      ;; the stream — no server-side per-connection state, and the SAME ordered
      ;; deltas replay identically over SSE, poll, and reconnect. The complete
      ;; reasoning + prose pin at `iteration.completed`; the fully rendered
      ;; answer lands at `turn.completed` (settled bubble).
      "content.block.delta"
      (let [delta
            (str (get event "text"))

            ;; Reasoning frames also carry the bounded CUMULATIVE text — the
            ;; increment alone is a sentence fragment, and REPLACING the ticker
            ;; with it painted a bare/stub THINKING card mid-turn.
            cumulative
            (str (get event "cumulative"))]

        (if (= "text" (get event "field"))
          (when-let [thought (block-thinking (if (str/blank? cumulative) delta cumulative))]
            [{:event "thinking" :html (html thought)}])
          (when-not (str/blank? delta)
            [{:event "content" :html (str/replace (html-text-escape delta) "\n" "<br>")}])))

      ;; Coarse live-progress: a provider wait, response parse, or a nested
      ;; shell/tool call. Repaint the transient #thinking ticker with a labeled
      ;; spinner so the user sees SOMETHING is happening; iteration.completed
      ;; resets it to bare dots.
      "activity"
      [{:event "thinking" :html (html (activity-ticker event))}]

      "block.started"
      ;; Nothing painted at form START: the code row is emitted at
      ;; `block.output` instead, because only THEN is the result known — and
      ;; the result sentinel (vis_answer/vis_silent → `:silent`) is the sole
      ;; signal that the form is engine chrome to fold away. (No head parsing.)
      nil

      "block.output"
      ;; `:silent` is the engine's display contract: a structurally code-free
      ;; block, or one whose result is a vis_answer (done) / vis_silent (title)
      ;; sentinel, is pure chrome — no code, no result row.
      (when-not (get event "silent")
        (let [;; Project the event through the SAME `vis/form<-wire` contract the
              ;; DB-restored trace uses (`wire-env->form`): a MIRRORED event
              ;; arriving over the cross-process bus has its kebab keys munged to
              ;; snake_case (`:vis/tool-name` → `:vis/tool_name`, `:tool-color-role`
              ;; → `:tool_color_role`), so reading them raw yielded nil and the
              ;; op-card badge/label silently vanished live. `<-wire` is tolerant of
              ;; BOTH spellings, so local and mirrored events paint identically.
              form
              (wire-env->form event)

              code
              (get event "code")

              ;; Work frames stay under the live Vis role.
              code-frame
              (when-not (or (str/blank? (str code)) (vis/hide-tool-code? form))
                {:event "trace-message"
                 :html (html (with-live-key (turn-live-key (str "code:" (get event "iteration")
                                                                ":" (get event "block_id"))
                                                           event)
                                            (block-code code)))})

              result-frame
              (when-let [out (block-result (get event "result") form)]
                {:event "trace-message"
                 :html (html (with-live-key (turn-live-key (str "result:" (get event "iteration")
                                                                ":" (get event "block_id"))
                                                           event)
                                            out))})

              error-frame
              (when (get event "error")
                {:event "trace-message"
                 :html (html (live-error-frame event
                                               (turn-live-key (str "error:" (get event "iteration"))
                                                              event)))})]

          (into [] (keep identity [code-frame result-frame error-frame]))))

      "iteration.error"
      [{:event "trace-message"
        :html (html (live-error-frame event
                                      (turn-live-key (str "error:" (get event "iteration"))
                                                     event)))}]

      "iteration.completed"
      (let [thought
            ;; `with-live-before`: the settled transcript renders an iteration's
            ;; reasoning ABOVE its tool cards, but this pin streams AFTER the
            ;; cards — the prefix tells the client to insert it before the
            ;; iteration's first streamed `code:` card so mid-turn and settled
            ;; ordering match.
            (some->> (block-thinking (get event "thinking")
                                     (turn-live-key (str "thinking:" (get event "iteration"))
                                                    event))
                     (with-live-before (str "code:" (get event "iteration") ":")))

            prose
            (block-prose (get event "assistant_prose")
                         (turn-live-key (str "prose:" (get event "iteration")) event))]

        ;; Complete reasoning + prose pin permanently inside this Vis shell.
        (cond-> []
          thought
          (conj {:event "trace-message" :html (html thought)})

          ;; Pinning the prose into the durable trace makes the transient
          ;; `.live-vis-answer` streaming PREVIEW (the same text, streamed by
          ;; `content.delta`) redundant — clear it so the answer never shows
          ;; twice while the shell awaits its terminal `vis-final` collapse.
          prose
          (conj {:event "trace-message" :html (html prose)} {:event "content" :html ""})

          ;; the ticker RESETS to dots (not empty) - the turn is still running,
          ;; so the bottom indicator must survive the iteration boundary; only
          ;; turn.completed/failed clears it. Do NOT re-render chrome here:
          ;; replaying a running turn during session switches otherwise swaps the
          ;; sidebar/header once per iteration, making icons and rails visibly flicker.
          true
          (conj {:event "thinking" :html (html [:div.dots [:span] [:span] [:span]])})))

      ("turn.completed" "turn.failed")
      (into [{:event "thinking" :html ""} {:event "turnctl" :html ""}
             ;; Replace the live shell atomically with persisted work + answer.
             {:event "vis-final" :html (html (settled-vis-bubble event))}
             {:event "queued" :html (html (queued-content sid))}
             {:event "footer" :html (html (footer-content sid))}]
            (chrome-frames sid))

      ("turn.queued" "turn.queued.updated" "turn.queued.deleted")
      [{:event "queued" :html (html (queued-content sid))}]

      ;; a session title changed (this one or another) - re-render the
      ;; header chip + the session drawer so generated titles land live,
      ;; even while the user is looking at a DIFFERENT session.
      "session.title_updated"
      (chrome-frames sid)

      nil)))

(def ^:private MAX_INFLIGHT_REPLAY_EVENTS
  "Upper bound on the number of in-flight EVENTS a session-switch / refresh
   renders SERVER-SIDE before handing off to the live stream. A turn with
   hundreds of tool calls emits thousands of events; running every one through
   `event->frames` on the request thread is what made switching INTO a busy
   streaming session take many seconds (up to a stall). We only need the RECENT
   tail — it matches the client's own `LIVE_MAX` trim (ui.js keeps ~400 `#live`
   nodes) — so the newest state paints instantly and the stream, which resumes
   at the SAME pinned cursor, carries everything after. Capping the EVENTS (not
   the frames) is what saves the work: `event->frames` runs on the tail only."
  500)

(defn- inflight-live-frames
  "Replay the IN-FLIGHT turn's events `(from, to]` through the SAME
   `event->frames` the SSE/poll stream uses, returning its `{:event :html}`
   frames. `session-page` renders these into `#live` / `#thinking` server-side,
   so a refresh OR a session-switch paints the running turn's CURRENT state
   INSTANTLY — instead of streaming the whole turn back into an empty `#live`
   (the visible 'recreation of state'). `from` is the running turn's
   `turn.started` seq, so its already-server-rendered chrome (the user bubble in
   `turn-block`, the dots/stop/queued the page draws for `running?`) is NOT
   replayed; `to` is the page's pinned cursor, so nothing overlaps what the
   stream — which now starts at the SAME cursor — will deliver next.

   Bounded to the last `MAX_INFLIGHT_REPLAY_EVENTS` events: a very long running
   turn would otherwise render thousands of frames on the request thread and
   stall the switch. The tail is what's on-screen anyway (ui.js trims older
   `#live` nodes), and the durable trace re-renders in full once the turn
   settles. Capping the EVENTS (before `event->frames`) is what avoids the work."
  [sid from to]
  (let [events (->> (vis/gateway-events-since sid from)
                    (take-while #(<= (long (get % "seq")) (long to))))]
    (->> (take-last MAX_INFLIGHT_REPLAY_EVENTS events)
         (mapcat #(event->frames sid %)))))

(defn- query-long
  "Unsigned long query param by name, or nil when absent/invalid."
  [request k]
  (try (some-> (get-in request [:query-params (name k)])
               parse-long)
       (catch Throwable _ nil)))

(defn- query-from
  "The `?from=N` replay cursor of a stream/poll request; nil when absent."
  [request]
  (query-long request :from))

(def ^:private POLL_CHUNK_CHARS
  "Maximum HTML characters returned by one /poll response. Polling is the
  fallback for proxy-buffered SSE; without a cap, one long thinking/tool-result
  frame can make the browser wait on a multi-MB JSON response."
  32768)

(defn- json-quote [s] (wire/json-str (str s)))

(defn- frame-json
  [{:keys [event html]}]
  (str "{\"event\":" (json-quote event) ",\"html\":" (json-quote html) "}"))

(defn- partial-json
  [{:keys [event html done?]}]
  (str "{\"event\":"
       (json-quote event)
       ",\"html\":"
       (json-quote html)
       ",\"done\":"
       (if done? "true" "false")
       "}"))

(defn- poll-page
  "Bound one polling response. Complete frames go in :frames. If a single frame
  is larger than the response budget, its html is split across :partials; the
  client buffers those chunks and applies the frame only when :done is true."
  [sid from frame-index offset]
  (loop [events
         (seq (vis/gateway-events-since sid from))

         next-seq
         (long from)

         frame-i
         (long frame-index)

         offset
         (long offset)

         budget
         (long POLL_CHUNK_CHARS)

         frames
         []

         partials
         []]

    (if (or (nil? events) (<= budget 0))
      {:next next-seq
       :frame frame-i
       :offset offset
       :more? (boolean events)
       :frames frames
       :partials partials}
      (let [event
            (first events)

            event-frames
            (vec (event->frames sid event))]

        (cond (empty? event-frames)
              (recur (next events) (long (get event "seq")) 0 0 budget frames partials)
              (>= frame-i (count event-frames))
              (recur (next events) (long (get event "seq")) 0 0 budget frames partials)
              :else (let [{frame-event :event :keys [html] :as frame}
                          (nth event-frames frame-i)

                          html
                          (str html)

                          remain
                          (- (count html) offset)]

                      (if (<= remain budget)
                        (recur events
                               next-seq
                               (inc frame-i)
                               0
                               (- budget remain)
                               (cond-> frames
                                 (zero? offset)
                                 (conj (select-keys frame [:event :html])))
                               (cond-> partials
                                 (pos? offset)
                                 (conj {:event frame-event :html (subs html offset) :done? true})))
                        {:next next-seq
                         :frame frame-i
                         :offset (+ offset budget)
                         :more? true
                         :frames frames
                         :partials (conj partials
                                         {:event frame-event
                                          :html (subs html offset (+ offset budget))
                                          :done? false})})))))))

(defn- write-frame!
  [^OutputStream out {:keys [event html id]}]
  ;; `id:` carries the gateway event seq. ui.js tracks it
  ;; (MessageEvent.lastEventId) and rewinds reconnects to it via the
  ;; htmx.createEventSource override — without it a reconnect reuses the
  ;; page-render ?from= cursor and replays the whole page-life of frames
  ;; into #live again.
  (let [frame (str "event: "
                   event
                   "\n"
                   (when id (str "id: " id "\n"))
                   (->> (str/split-lines (str html))
                        (map #(str "data: " %))
                        (str/join "\n"))
                   "\n\n")]
    (.write out (.getBytes frame StandardCharsets/UTF_8))))

(defn stream-handler
  "Live-only SSE stream of HTML fragments for the session page. Same
   replay-then-live monitor discipline as the JSON stream, but the
   cursor pins to `gateway-current-seq` so the page (which rendered
   current state) receives only what happens next."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        ;; ?from=N pins the replay cursor at the PAGE's render seq (the page
        ;; computed it; see session-page) - without it we degrade to the old
        ;; live-only behavior.
        from
        (query-from request)

        ;; A forwarding header means an edge proxy sits between us and the
        ;; client (cloudflared stamps cf-ray/cf-connecting-ip) — only then is
        ;; the anti-buffering pad worth its bytes.
        proxied?
        (boolean (some #(get-in request [:headers %])
                       ["cf-ray" "cf-connecting-ip" "x-forwarded-for" "via"]))]

    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "text/html"} :body "unknown session"}
      {:status 200
       ;; no-transform + X-Accel-Buffering: intermediaries (Cloudflare
       ;; tunnels, nginx) BUFFER a streaming body unless told not to —
       ;; buffered SSE delivers nothing until disconnect, which reads as
       ;; "streaming dead until refresh" through the tunnel.
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache, no-transform"
                 "X-Accel-Buffering" "no"}
       :body (reify
               ring-protocols/StreamableResponseBody
                 (write-body-to-stream [_ _ output-stream]
                   (let [^OutputStream out
                         output-stream

                         sub-id
                         (str (java.util.UUID/randomUUID))

                         ;; Tracks the last ticker write so a `reasoning.delta`
                         ;; token-burst coalesces to one write per window.
                         last-thinking
                         (volatile! 0)

                         sink
                         (fn [event]
                           (locking out
                             (let [now (System/currentTimeMillis)]
                               ;; Drop intermediate reasoning ticks that land inside
                               ;; the coalesce window: they only re-render the
                               ;; transient #thinking ticker, so skipping a few costs
                               ;; nothing but spares the tunnel a write+flush each.
                               (when-not (and (= "text" (get event "field"))
                                              (< (- now (long @last-thinking))
                                                 THINKING_COALESCE_MS))
                                 (when (= "text" (get event "field")) (vreset! last-thinking now))
                                 (doseq [frame (event->frames sid event)]
                                   ;; stamp the gateway seq so the client can
                                   ;; rewind a reconnect to it (pings carry none —
                                   ;; an id-less frame leaves lastEventId alone)
                                   (write-frame! out (assoc frame :id (get event "seq"))))
                                 (.flush out)))))]

                     (try (locking out
                            ;; 8KB SSE comment pad (clients ignore comments): proxy
                            ;; edges (Cloudflare tunnel) buffer a streaming body until
                            ;; a byte threshold — without the pad the first real frames
                            ;; sit in the edge buffer and live streaming looks dead.
                            ;; ONLY for proxied requests; a direct localhost client
                            ;; needs no pad and shouldn't pay the bytes.
                            (when proxied?
                              (.write out
                                      (.getBytes (str ": " (apply str (repeat 8192 " ")) "\n\n")
                                                 StandardCharsets/UTF_8))
                              (.flush out))
                            ;; Immediate NAMED ping (not a comment): the page's
                            ;; watchdog (#ssewatch + ui.js) listens for it — a healthy
                            ;; stream proves itself within a second; silence means an
                            ;; edge proxy is buffering the body and ui.js falls back
                            ;; to polling /ui/session/:sid/poll.
                            (write-frame! out {:event "ping" :html ""})
                            (.flush out)
                            (doseq [event (vis/gateway-subscribe!
                                            sid
                                            sub-id
                                            sink
                                            (or from (vis/gateway-current-seq sid)))]
                              (sink event)))
                          (loop []

                            (Thread/sleep (long HEARTBEAT_MS))
                            (locking out (write-frame! out {:event "ping" :html ""}) (.flush out))
                            (recur))
                          (catch Throwable _ nil)
                          (finally (vis/gateway-unsubscribe! sid sub-id)
                                   (try (.close out) (catch Throwable _ nil)))))))})))

(defn- poll-handler
  "GET /ui/session/:sid/poll?from=N — the SSE stream's PULL twin for
  clients whose stream an edge proxy silently buffers (free Cloudflare
  quick tunnels hold SSE bodies forever: 200 + text/event-stream + zero
  bytes). Returns the SAME named HTML fragments the stream would have
  pushed, but caps each response and chunks over-large frames. The response
  carries next/frame/offset cursors, complete frames, and partial frame chunks;
  ui.js applies complete frames immediately and buffers partials until done."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        from
        (max 0 (long (or (query-from request) 0)))

        frame
        (max 0 (long (or (query-long request :frame) 0)))

        offset
        (max 0 (long (or (query-long request :offset) 0)))]

    (if-not (and sid (vis/gateway-soul sid))
      {:status 404
       :headers {"Content-Type" "application/json; charset=utf-8"}
       :body "{\"error\":\"unknown session\"}"}
      (let [{:keys [next frame offset more? frames partials]} (poll-page sid from frame offset)]
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"
                   "Cache-Control" "no-cache, no-transform"}
         :body (str "{\"next\":"
                    next
                    ",\"frame\":"
                    frame
                    ",\"offset\":"
                    offset
                    ",\"more\":"
                    (if more? "true" "false")
                    ",\"frames\":["
                    (str/join "," (map frame-json frames))
                    "]"
                    ",\"partials\":["
                    (str/join "," (map partial-json partials))
                    "]}")}))))

;; =============================================================================
;; Pages
;; =============================================================================

(defn- token-form-page
  "The connect screen - a centered glass card over soft gold orbs. The
   bearer token is REQUIRED here (`required` + server-side check); the
   form contract stays POST /ui/auth with the `token` field."
  [& [error]]
  (page
    "connect"
    [:main.auth
     [:div.auth-card [:div.auth-mark "vis"] [:p.tagline "see it think"]
      [:div.auth-lock
       [:svg
        {:viewBox "0 0 24 24"
         :width "14"
         :height "14"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"} [:rect {:x "3" :y "11" :width "18" :height "11" :rx "2"}]
        [:path {:d "M7 11V7a5 5 0 0 1 10 0v4"}]] [:span "bearer token required"]]
      (when error [:p.auth-error error])
      [:form {:method "post" :action "/ui/auth"}
       [:input.auth-input
        {:type "password"
         :name "token"
         :placeholder "paste your gateway token"
         :autofocus true
         :autocomplete "off"
         :autocapitalize "off"
         :spellcheck "false"
         :required true
         :aria-label "gateway bearer token"}]
       [:button.auth-go {:type "submit"} "Connect"
        [:svg
         {:viewBox "0 0 24 24"
          :width "15"
          :height "15"
          :fill "none"
          :stroke "currentColor"
          :stroke-width "2"
          :stroke-linecap "round"
          :stroke-linejoin "round"} [:path {:d "M5 12h14"}] [:path {:d "m12 5 7 7-7 7"}]]]]
      [:p.auth-hint "the token lives at " [:code "~/.vis/gateway.token"] " on the host"]]]))

(defn- not-found-page
  "Styled error page for a wrong /ui address or a missing session. Reuses the
   connect screen's centered glass card (`.auth*` classes) so it needs no new
   CSS, and always offers a one-click way back to a real session."
  [&
   [{:keys [code title detail]
     :or {code "404" title "page not found" detail "that address doesn't exist on this vis."}}]]
  (page title
        [:main.auth
         [:div.auth-card [:div.auth-mark code] [:p.tagline title] [:p.auth-error detail]
          [:p.auth-hint [:a {:href "/ui"} "← back to your sessions"]]]]))

(defn- project-hue
  "Stable 0-359 hue for a project id, so each project gets its OWN vivid accent
   with no fixed palette to exhaust. Deterministic — the same project always
   paints the same color across renders and across the drawer / pick modal."
  [id]
  (mod (reduce (fn [^long h ^Character c]
                 (+ (unchecked-multiply 31 h) (int c)))
               17
               (str id))
       360))

(defn- project-accent
  "CSS color for a project: its explicit `:color` (hex) when set, else a hashed
   vivid hue. Fed to markup as the `--proj` custom property the CSS tints from."
  [{:strs [id color]}]
  (if (not-empty color) color (str "hsl(" (project-hue id) " 62% 58%)")))

(defn- session-row-li
  "One session row — inside a project FOLDER or the ungrouped list: the
   bulk-select checkbox, the row link (active highlight + running dot), and the
   hover actions. When rendered inside a project folder it also gets reorder
   arrows (the web twin of dragging a TUI tab); every row gets move-to-project
   + delete. Carries `data-sid` for delegated JS."
  ([session active-sid] (session-row-li session active-sid nil))
  ([{:strs [id title status]} active-sid {:keys [reorder]}]
   [:li.side-item {:data-sid (str id)}
    ;; bulk-delete checkbox - hidden until the aside carries .select-mode;
    ;; checked rows ride hx-include to the confirm modal
    [:input.side-check {:type "checkbox" :name "sid" :value (str id) :aria-label "Select session"}]
    [:a
     {:class (str "side-row" (when (= (str id) (str active-sid)) " active"))
      :href (str "/ui/session/" id)} [:span.side-title (or title "Untitled")]
     (when (= status "running") [:span.side-dot])]
    ;; reorder arrows - only inside a project folder (movable tabs); each posts
    ;; to /ui/session/:sid/reorder and the drawer re-renders in one hop
    (when reorder
      [:span.side-reorder
       [:button
        {:type "button"
         :aria-label "Move up"
         :disabled (boolean (:first? reorder))
         :hx-post (str "/ui/session/" id "/reorder")
         :hx-vals "{\"dir\":\"up\"}"
         :hx-swap "none"} "\u25b4"]
       [:button
        {:type "button"
         :aria-label "Move down"
         :disabled (boolean (:last? reorder))
         :hx-post (str "/ui/session/" id "/reorder")
         :hx-vals "{\"dir\":\"down\"}"
         :hx-swap "none"} "\u25be"]])
    ;; hover-revealed move: open the project picker for THIS session
    [:button.side-move
     {:type "button"
      :aria-label "Move to project"
      :hx-get (str "/ui/session/" id "/move")
      :hx-target "#modal"
      :hx-swap "innerHTML"} (icon "folder")]
    ;; hover-revealed delete - DELETE /ui/session/:sid (TUI Ctrl+D parity)
    [:button.side-del
     {:type "button"
      :aria-label "Delete session"
      :hx-get (str "/ui/session/" id "/delete")
      :hx-target "#modal"
      :hx-swap "innerHTML"} (icon "x")]]))

(defn- project-section
  "One project rendered as a COLLAPSIBLE FOLDER (the Claude/Codex model): an
   accent-tinted card whose header carries the collapse toggle (chevron + accent
   dot + name + session count), a '+' that starts a NEW session already filed in
   this project, and a manage (rename/delete) button — over the nested list of
   its sessions (each reorderable), or an empty hint. Carries `data-pid` so
   ui.js can persist the collapsed/expanded state across SSE re-renders."
  [p sessions active-sid]
  (let [pid
        (str (get p "id"))

        nm
        (or (not-empty (get p "name")) "Project")

        rows
        (sort-by (juxt #(get % "project_position") #(get % "name")) sessions)

        n
        (count rows)]

    [:section.side-project {:data-pid pid :style (str "--proj:" (project-accent p))}
     [:div.side-project-head
      [:button.side-project-toggle
       {:type "button" :data-proj-toggle pid :aria-label (str "Collapse or expand " nm)}
       [:span.side-project-chev (icon "chevron-right")] [:span.side-project-dot]
       [:span.side-project-name nm] [:span.side-project-count n]]
      ;; '+' starts a new session INSIDE this project (Claude/Codex 'new chat in project')
      [:form.side-project-newform {:method "post" :action (str "/ui/project/" pid "/session")}
       [:button.side-project-new
        {:type "submit" :aria-label "New session in project" :title "New session in this project"}
        (icon "plus")]]
      ;; rename / delete this project
      [:button.side-project-edit
       {:type "button"
        :aria-label "Manage project"
        :hx-get (str "/ui/project/" pid)
        :hx-target "#modal"
        :hx-swap "innerHTML"} (icon "edit")]]
     (if (seq rows)
       [:ul.side-list
        (map-indexed
          (fn [i s]
            (session-row-li s active-sid {:reorder {:first? (zero? i) :last? (= i (dec n))}}))
          rows)]
       [:div.side-project-empty "No sessions yet"])]))


(defn- sidebar-content
  "Children of the session drawer - extracted so the SSE `sidebar` frame can
   re-render titles and running dots without replacing the <aside> (which
   carries the sse-swap target). Select-mode (bulk delete) is CSS state on the
   <aside> and SURVIVES the SSE innerHTML re-render (re-applied by ui.js); the
   per-project collapsed/expanded state is persisted client-side the same way.

   Projects render as COLLAPSIBLE FOLDER GROUPS at the top (the Claude/Codex
   model) — each a `project-section` grouping its sessions — followed by the
   ungrouped sessions under a 'Recents' label. New-project lives in the head."
  [active-sid]
  (let [sessions
        (vis/gateway-list-sessions)

        projects
        (try (vis/gateway-list-projects) (catch Throwable _ nil))

        ordered
        (sort-by (juxt #(get % "position") #(get % "name")) projects)

        pid-of
        (fn [s]
          (some-> (get s "project_id")
                  str
                  not-empty))

        by-project
        (group-by pid-of sessions)

        unfiled
        (sort-by #(get % "name") (get by-project nil))]

    (list [:div.side-head
           [:form.newchat {:method "post" :action "/ui/sessions"}
            [:button.newchat-btn {:type "submit"} [:span.newchat-plus "+"] "New session"]]
           [:button.side-newproject
            {:type "button"
             :aria-label "New project"
             :title "New project"
             :hx-get "/ui/projects/new"
             :hx-target "#modal"
             :hx-swap "innerHTML"} (icon "folder-plus")]
           [:button.side-select-toggle
            {:type "button" :data-select-toggle "1" :aria-label "Select sessions"}
            [:span.when-idle "Select"] [:span.when-select "Done"]]]
          ;; the single scroll region: project folders first, then ungrouped
          [:div.side-scroll
           (for [p ordered]
             (project-section p (get by-project (str (get p "id"))) active-sid))
           (when (seq unfiled)
             (list (when (seq ordered) [:div.side-project-label "Recents"])
                   [:ul.side-list.side-ungrouped (map #(session-row-li % active-sid nil) unfiled)]))
           (when (and (empty? ordered) (empty? unfiled)) [:div.side-empty "No sessions yet"])]
          ;; select-mode action bar - the confirm modal receives the checked ids
          ;; as repeated `sid` query params via hx-include
          [:div.side-bulkbar
           [:button.btn-danger.side-bulk-del
            {:type "button"
             :disabled true
             :hx-get "/ui/sessions/delete"
             :hx-include ".side-check:checked"
             :hx-target "#modal"
             :hx-swap "innerHTML"} "Delete selected"]]
          ;; config actions live at the BOTTOM of the sidebar (margin-top:auto), not
          ;; in the cramped mobile header.
          [:div.side-foot
           [:button.side-foot-btn
            {:type "button"
             :aria-label "Providers"
             :hx-get (str "/ui/session/" active-sid "/providers")
             :hx-target "#modal"
             :hx-swap "innerHTML"} (icon "zap") [:span "Providers"]]
           [:button.side-foot-btn
            {:type "button"
             :aria-label "Settings"
             :hx-get "/ui/settings"
             :hx-target "#modal"
             :hx-swap "innerHTML"} (icon "settings") [:span "Settings"]]
           [:a.side-foot-btn
            {:href (str "/ui/session/" active-sid "/export.html")
             :download true
             :aria-label "Export transcript as HTML"} (icon "download") [:span "Export"]]])))

(defn- sessions-sidebar
  "Left rail: the session drawer. The active session is highlighted; a
   running one carries a gold pulse dot. SSE re-renders the contents on
   every turn boundary (`sidebar` frame)."
  [active-sid]
  [:aside.sidebar {:sse-swap "sidebar" :hx-swap "innerHTML"} (sidebar-content active-sid)])

(defn- session-page
  [sid]
  (let [soul
        (vis/gateway-soul sid)

        all-turns
        (vec (vis/gateway-list-turns sid))

        turns
        (remove #(= "queued" (get % "status")) all-turns)

        window
        (vec (take-last INITIAL_TURN_WINDOW turns))

        older?
        (> (count turns) (count window))

        oldest-tid
        (some-> (first window)
                (get "turn_id")
                str)

        running-turn
        (some #(when (= "streaming" (get % "status")) %) all-turns)

        running?
        (boolean running-turn)

        ;; The SSE/poll cursor pins at PAGE RENDER (not at connect) so nothing falls
        ;; into the render->connect gap. The stream ALWAYS starts at `page-seq` (the
        ;; current cursor) — a running turn's accumulated trace is rendered
        ;; server-side below (`inflight-live-frames`), so a refresh / session-switch
        ;; paints the CURRENT state instantly and the stream carries only NEW events.
        page-seq
        (vis/gateway-current-seq sid)

        event-start-seq
        (get running-turn "event_start_seq")

        from
        page-seq

        live-replay?
        (some? event-start-seq)

        ;; Server-rendered current trace of the in-flight turn (events AFTER its
        ;; turn.started, up to the pinned cursor) — the SAME frames the stream
        ;; produces, painted now instead of replayed.
        live-frames
        (when event-start-seq (inflight-live-frames sid event-start-seq page-seq))

        live-trace-html
        ;; Mirrors ui.js's `data-live-before` placement server-side: a pinned
        ;; fragment carrying that prefix (the iteration.completed thinking
        ;; card) lands BEFORE the first already-collected fragment whose
        ;; live-key starts with the prefix, so a mid-turn refresh paints the
        ;; same order the live DOM shows.
        (->> live-frames
             (filter #(= "trace-message" (:event %)))
             (map :html)
             (reduce (fn [acc html]
                       (let [html
                             (str html)

                             prefix
                             (second (re-find #"data-live-before=\"([^\"]+)\"" html))

                             needle
                             (when prefix (str "data-live-key=\"" prefix))

                             idx
                             (when needle
                               (first (keep-indexed (fn [i h]
                                                      (when (str/includes? (str h) needle) i))
                                                    acc)))]

                         (if idx
                           (-> (subvec acc 0 (long idx))
                               (conj html)
                               (into (subvec acc (long idx))))
                           (conj acc html))))
                     [])
             (apply str))

        live-answer-html
        ;; Replay the beforeend/clear stream server-side so a refresh / session
        ;; switch paints the CURRENT answer: each `content` frame APPENDS its
        ;; fragment; an empty one (the iteration boundary clear) resets it.
        (reduce (fn [acc {:keys [event html]}]
                  (cond (not= "content" event) acc
                        (str/blank? (str html)) ""
                        :else (str acc html)))
                ""
                live-frames)

        live-html
        (when running-turn (html (live-vis-shell running-turn live-trace-html live-answer-html)))

        live-think
        (->> live-frames
             (filter #(= "thinking" (:event %)))
             last
             :html)]

    (page
      (or (get soul "title") "session")
      [:div.app
       {:hx-ext "sse"
        :sse-connect (str "/ui/session/" sid "/stream?from=" from)
        :data-sid (str sid)
        :data-from (str from)}
       ;; SSE watchdog target: the stream pings this (hx-swap none) so
       ;; ui.js can tell a LIVE stream from one an edge proxy buffers —
       ;; silence after connect flips the page to /poll pulling.
       [:div#ssewatch {:sse-swap "ping" :hx-swap "none" :hidden true}]
       [:header.bar
        [:button#toggle-left.bar-toggle {:type "button" :aria-label "Toggle sessions"}
         (icon "sidebar")]
        [:div.bar-title {:sse-swap "bartitle" :hx-swap "innerHTML"} (bar-title-content soul)]
        [:span.session-id (subs (str sid) 0 8)]] [:div#modal]
       ;; mobile-drawer backdrop: dim + tap-to-close when the sidebar/rail is open
       [:div.scrim {:aria-hidden "true"}]
       [:div.layout (sessions-sidebar sid)
        ;; ONE center flex column holds the thread AND the composer dock,
        ;; so both center in the SAME box between the rails — the input
        ;; can never drift out of line with the chat column again.
        [:div.center
         [:main.thread
          [:div.column
           (if (seq turns)
             (list (when older? (older-sentinel sid oldest-tid))
                   (map #(turn-block % live-replay?) window))
             [:div.hello-wrap [:h1.hello "What are we building?"]
              [:p.hello-sub "vis works in this workspace — ask for anything."]])
           ;; Live bubbles land here (user message from the form response,
           ;; the answer from the `message` SSE event). Work below holds
           ;; ONLY trace: code, results, iteration ticks.
           [:div#live.live {:sse-swap "message" :hx-swap "beforeend"}
            ;; Running turn: its current trace, server-rendered (see
            ;; inflight-live-frames). Empty for an idle session — history sits in
            ;; the turn-blocks above.
            (when (seq live-html) (h/raw live-html))]
           [:div#thinking.thinking {:sse-swap "thinking" :hx-swap "innerHTML"}
            (cond
              ;; Current thinking ticker (server-rendered) so it's there on load,
              ;; not popped in by the next reasoning delta.
              (seq (str live-think)) (h/raw live-think)
              running? [:div.dots [:span] [:span] [:span]])] [:div.thread-tail]]]
         [:div.dock
          [:button#jump-bottom.jump-bottom
           {:type "button"
            :hidden true
            :aria-label "Jump to bottom"
            :title "Jump to newest message"} (icon "arrow-down") [:span "Bottom"]]
          [:div#suggest.suggest {:hidden true}]
          ;; Stop control - SSE fills #turnctl with the stop button on turn.started
          ;; and clears it on turn finish. Pre-rendered when the page loads mid-turn.
          [:div#queued.queued {:sse-swap "queued" :hx-swap "innerHTML"}
           (queued-content sid all-turns)]
          [:div#turnctl.turnctl {:sse-swap "turnctl" :hx-swap "innerHTML"}
           (when running? (stop-button sid))]
          [:form.composer
           {:hx-post (str "/ui/session/" sid "/turns")
            :hx-target "#live"
            :hx-swap "beforeend"
            :hx-encoding "multipart/form-data"
            :data-files-url (str "/v1/sessions/" sid "/suggest")
            "hx-on::after-request" "if(event.detail.successful) this.reset()"}
           ;; No add-file BUTTON — type `@` to attach a file (the composer's
           ;; @-picker), keeping the input edges symmetric and uncluttered.
           [:textarea
            {:name "request"
             :rows 1
             :autocomplete "off"
             :autocapitalize "off"
             :autocorrect "off"
             :spellcheck "false"
             :placeholder "Ask vis…   (@ to add a file)"}]
           ;; Trailing controls kept in ONE tight actions cluster so the
           ;; attach / mic / send icons hug each other on the right edge
           ;; instead of drifting apart. Attach is a file affordance
           ;; (paperclip) posting multipart `attachment` parts; ui.js paints a
           ;; removable preview tray with per-file thumbnails so an upload is
           ;; never silent.
           [:div.composer-actions
            [:label.attach {:aria-label "Attach a file" :title "Attach files or images"}
             (icon "paperclip")
             [:input.attach-input
              {:type "file" :name "attachment" :accept "image/*" :multiple true}]]
            [:button.mic
             {:type "button"
              :aria-label "Dictate"
              :data-voice-url (str "/v1/sessions/" sid "/voice")} (icon "mic")]
            [:button.send {:type "submit" :aria-label "Send"} (icon "arrow-up")]]]]
         ;; The footer dock (provider/model + reasoning chips, fs/bg counts) makes
         ;; several gateway roundtrips (workspace + resources + routing + reasoning);
         ;; loading it AFTER first paint (hx-trigger=load) keeps it off the page's
         ;; critical render path so a session-switch paints the thread immediately.
         ;; SSE `footer` frames + the OOB `#footwrap` swaps still refresh it live.
         [:div#footwrap
          {:sse-swap "footer"
           :hx-swap "innerHTML"
           :hx-get (str "/ui/session/" sid "/footer")
           :hx-trigger "load"}]]]])))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- cookie-token [request] (get-in request [:cookies "vis_token" :value]))

(defn- ui-authed?
  "True when the gateway runs authless (the loopback default) or the
   request carries the gateway token as the browser cookie. Registered
   as the contribution's :request-authed-fn."
  [request ^String token]
  (or (not (vis/gateway-auth-required?)) (= token (cookie-token request))))

(defn- epoch-of
  [soul]
  (let [v (or (get soul "last_active_at") (get soul "created_at"))]
    (cond (number? v) (long v)
          (instance? java.util.Date v) (.getTime ^java.util.Date v)
          :else 0)))

(defn- enter-app
  "303 into the most recent conversation (creating one on a fresh
   install). `extra` is merged onto the response — used to drop the auth
   cookie on the magic-link path."
  [extra]
  (let [sessions
        (vis/gateway-list-sessions)

        target
        (if (seq sessions)
          (get (apply max-key epoch-of sessions) "id")
          (get (vis/gateway-create-session! {}) "id"))]

    (merge {:status 303 :headers {"Location" (str "/ui/session/" target)} :body ""} extra)))

(defn- index-handler
  "GET /ui - jumps STRAIGHT into the most recent conversation (creating
   one on a fresh install); there is no separate home page — the
   sessions drawer is the navigation.

   Auth, in priority order:
   1. already carrying the cookie (or authless loopback) → straight in;
   2. MAGIC LINK — `?token=` in the URL matches the gateway token →
      drop the HttpOnly cookie and bounce in, so the link I hand out
      logs you in with no form to fill (the query token never sticks in
      the address bar: the 303 lands you on a clean /ui/session/… URL);
   3. otherwise → the token form."
  [request token]
  (let [q-token (some-> (get-in request [:query-params "token"])
                        str/trim)]
    (cond (ui-authed? request token) (enter-app nil)
          (and (vis/gateway-auth-required?) (= token q-token))
          (enter-app {:cookies {"vis_token"
                                {:value token :http-only true :same-site :lax :path "/"}}})
          :else {:status 200
                 :headers {"Content-Type" "text/html; charset=utf-8"}
                 :body (token-form-page)})))

(defn- auth-handler
  "POST /ui/auth - exchange the bearer token for the HttpOnly cookie."
  [request token]
  (if (= token
         (some-> (get-in request [:form-params "token"])
                 str/trim))
    {:status 303
     :headers {"Location" "/ui"}
     :cookies {"vis_token" {:value token :http-only true :same-site :lax :path "/"}}
     :body ""}
    {:status 401
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (token-form-page "that token does not match — check ~/.vis/gateway.token")}))

(defn- create-session-handler
  "POST /ui/sessions - create and bounce to the session page."
  [request]
  (let [title
        (let [t (str (get-in request [:form-params "title"]))]
          (when-not (str/blank? t) t))

        id
        (get (vis/gateway-create-session! {:title title}) "id")]

    {:status 303 :headers {"Location" (str "/ui/session/" id)} :body ""}))

(defn- session-handler
  [request]
  (let [sid (some-> (get-in request [:path-params :sid])
                    parse-uuid)]
    (if (and sid (vis/gateway-soul sid))
      {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body (session-page sid)}
      ;; Unknown / deleted session id: show a real "session not found" page so a
      ;; wrong address reads as an error, with a one-click way back.
      {:status 404
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (not-found-page {:title "session not found"
                              :detail "that conversation doesn't exist or was deleted."})})))

(declare export-session-html)

(defn- export-handler
  "GET /ui/session/:sid/export.html — download the session as a STANDALONE,
   self-contained HTML file that is the SAME chat view /ui renders (user/vis
   bubbles + inline op-card trace + a session-summary card on top, with the
   marked/DOMPurify/Prism scripts inlined). No per-turn summaries — exactly the
   web view, just offline."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid])
                    parse-uuid)]
    (if (and sid (vis/gateway-soul sid))
      (let [fname (str "vis-transcript-" (subs (str sid) 0 8) ".html")]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"
                   "Content-Disposition" (str "attachment; filename=\"" fname "\"")}
         :body (export-session-html sid)})
      {:status 404
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (not-found-page {:title "session not found"
                              :detail "that conversation doesn't exist or was deleted."})})))

(defn- delete-session-ui-handler
  "DELETE /ui/session/:sid — permanently delete a session from the
   sidebar. `gateway-close-session!` disposes the live environment and
   deletes the DB tree (same path as the TUI's Ctrl+D). Redirect: when
   the OPEN session was deleted, land on /ui (it re-picks the most
   recent session); deleting another row stays on the current page.
   htmx ships the page URL in HX-Current-URL — no query plumbing."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        current
        (some->> (get-in request [:headers "hx-current-url"])
                 (re-find #"/ui/session/([0-9a-fA-F-]{36})")
                 second
                 parse-uuid)]

    (when sid (vis/gateway-close-session! sid))
    {:status 200
     :headers {"HX-Redirect"
               (if (and current (not= current sid)) (str "/ui/session/" current) "/ui")}
     :body ""}))

(defn- sid-params
  "Normalize a repeated `sid` request param - Ring hands back a STRING
   for one value and a VECTOR for many - into a seq of parsed UUIDs."
  [v]
  (->> (if (coll? v) v [v])
       (keep #(some-> %
                      str
                      parse-uuid))))

(defn- delete-sessions-bulk-handler
  "POST /ui/sessions/delete - permanently delete EVERY checked session
   (sidebar select-mode). Each sid rides the same close path as the
   single delete (`gateway-close-session!` disposes the live env and
   deletes the DB tree). Redirect: when the OPEN session was among the
   deleted, land on /ui (it re-picks the most recent session); else
   stay on the current page."
  [request]
  (let [sids
        (set (sid-params (get-in request [:form-params "sid"])))

        current
        (some->> (get-in request [:headers "hx-current-url"])
                 (re-find #"/ui/session/([0-9a-fA-F-]{36})")
                 second
                 parse-uuid)]

    (doseq [sid sids]
      (vis/gateway-close-session! sid))
    {:status 200
     :headers {"HX-Redirect"
               (if (and current (not (contains? sids current))) (str "/ui/session/" current) "/ui")}
     :body ""}))

(defn- slash-bubble
  [result]
  (html
    (list
      [:div.bubble.b-vis [:div.role.role-vis "Vis"]
       (if-let [error (:error result)]
         [:p.empty.slash-error (str error)]
         ;; The slash-dispatch envelope carries its title/body under `:result`.
         ;; Project canonical content directly; no duplicate persisted shape.
         (let [r (:result result)
               title (some-> (:slash/title r)
                             str
                             str/trim
                             not-empty)
               body (:slash/body r)
               body* (cond (nil? body) nil
                           (string? body) (md->hiccup body)
                           (vector? body) (ast->hiccup body)
                           :else (md->hiccup (pr-str body)))]

           [:div.prose.md
            (cond (and title body*) (list [:p [:strong title]] body*)
                  title [:p [:strong title]]
                  body* body*
                  :else [:p "done"])]))])))

(defn- run-slash
  "Dispatch a /command through the engine's slash handling — the same
   `slash/dispatch` the TUI and Telegram ride, with this channel's id.
   The ctx carries the session-state-id + workspace-id (resolved via the
   rehydrated env) so workspace-scoped slashes (`/fs add|remove`, `/draft
   new|apply|abandon`) can find the session's workspace — without them they
   answered \"No active workspace\" in the web."
  [sid text]
  (let [env
        (vis/env-for sid)

        db
        (:db-info env)

        state-id
        (or (:session/state-id env)
            (try (vis/db-latest-session-state-id db (str sid)) (catch Throwable _ nil)))]

    (vis/slash-dispatch env
                        {:channel/id :web
                         :session/id sid
                         :session/state-id state-id
                         :workspace/id (:workspace/id env)
                         :db-info db}
                        text)))

;; The modal renderers live further down; forward-declare them so the
;; composer slash branches can open the SAME panels the sidebar buttons do.
(declare settings-handler sessions-switch-handler providers-modal)

(defn- oob-modal
  "Wrap already-rendered modal HTML in an out-of-band swap into `#modal`, so
   a composer slash opens the panel in ONE hop (no loader round-trip). The
   main response body is otherwise empty, so nothing lands in the thread.
   `content-html` is trusted server-rendered HTML (modal-shell output)."
  [content-html]
  (str "<div id=\"modal\" hx-swap-oob=\"innerHTML\">" content-html "</div>"))

(defn- queued-update-handler
  "POST /ui/session/:sid/queued/:tid/update — autosave one queued message.
  Edits are intentionally per-message so multiple queued prompts stay queued;
  nothing re-renders on success, which preserves focus/caret while typing."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        tid
        (get-in request [:path-params :tid])

        text
        (str/trim (str (get-in request [:form-params "request"])))]

    (when (and sid tid (not (str/blank? text))) (vis/gateway-update-queued-turn! sid tid text))
    {:status 204 :headers {"Content-Type" "text/html; charset=utf-8"} :body ""}))

(defn- queued-delete-handler
  "POST /ui/session/:sid/queued/:tid/delete — delete one queued message."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        tid
        (get-in request [:path-params :tid])]

    (when (and sid tid) (vis/gateway-delete-queued-turn! sid tid))
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if sid (html (queued-content sid)) "")}))

(defn- queued-clear-handler
  "POST /ui/session/:sid/queued/clear — delete every queued message."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid])
                    parse-uuid)]
    (when sid
      (doseq [t (->> (vis/gateway-list-turns sid)
                     (filter #(= "queued" (get % "status"))))]
        (vis/gateway-delete-queued-turn! sid (get t "turn_id"))))
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if sid (html (queued-content sid)) "")}))

(defn- composer-attachments
  "Composer image uploads ride as one or more `attachment` parts in the
   multipart form body; Ring's byte-array store hands each as `{:filename
   :content-type :bytes}`. Base64 the bytes into the `[{:base64 :media-type
   :filename}]` shape the engine's `attachments/prepare-inline-attachments`
   validates (magic-byte sniff + size/count caps) AND the web bubble renders
   inline. Empty parts (no file chosen) are dropped; a plain base64 string is
   still accepted (JSON/legacy). The declared media type is re-sniffed
   engine-side, so only the bytes matter for validation."
  [raw]
  (->> (cond (nil? raw) []
             (sequential? raw) raw
             :else [raw])
       (keep (fn [part]
               (cond (and (map? part) (:bytes part))
                     (let [^bytes b (:bytes part)]
                       (when (pos? (alength b))
                         (cond-> {:base64 (.encodeToString (java.util.Base64/getEncoder) b)}
                           (:content-type part)
                           (assoc :media-type (str (:content-type part)))

                           (:filename part)
                           (assoc :filename (str (:filename part))))))
                     (and (string? part) (not (str/blank? part))) {:base64 part})))
       vec))

(defn- submit-turn-handler
  "POST /ui/session/:sid/turns (htmx form). A leading `/` dispatches the
   engine slash dispatch and answers inline (no LLM turn); anything
   else submits a turn — the live stream carries what follows."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        text
        (str/trim (str (or (get-in request [:multipart-params "request"])
                           (get-in request [:form-params "request"]))))

        ;; Accept BOTH `/new-session` (TUI's name — parity) and the shorter
        ;; `/new`, with an optional title arg.
        new-m
        (re-matches #"(?i)/new(?:-session)?(\s+.*)?" text)

        ;; Other web-native channel slashes (TUI palette parity): open a panel
        ;; or fork. Captured group is the bare command word, or nil.
        native
        (some-> (re-matches #"(?i)/(settings|providers|switch-session|fork-session)(?:\s.*)?" text)
                second
                str/lower-case)

        ;; A leading-`/` message that is NOT one of the web-native channel
        ;; slashes above. Dispatch it through the engine (`run-slash`) so real
        ;; `/`-commands answer inline.
        slash?
        (and sid (str/starts-with? text "/") (not new-m) (nil? native))

        slash-result
        (when slash?
          (try (run-slash sid text) (catch Throwable t {:handled? true :error (ex-message t)})))

        ;; PARITY with `run-turn!` (the TUI + Telegram path): a slash NO
        ;; registered extension claims (`:reason :unknown`) gets one more chance
        ;; as a PROMPT TEMPLATE — `.vis/prompts/*.md` file prompts and harness
        ;; `/skill:<name>` skills. When one matches we DON'T answer inline; we
        ;; fall through to `:else` and submit a normal streaming turn, letting
        ;; the engine re-expand + run it. The web was the ONLY channel skipping
        ;; this, so `/skill:impeccable` wrongly answered "Unknown slash command".
        slash-template?
        (and slash-result
             (= :unknown (:reason slash-result))
             (try (some? (prompt-templates/expand (vis/env-for sid) text))
                  (catch Throwable _ false)))]

    (cond
      ;; Web-native `/new-session [title]` (alias `/new`): create a NEW session
      ;; and redirect. Session creation is a CHANNEL action (the sidebar "+ New
      ;; session" button does the same `gateway-create-session!`), not an engine
      ;; slash — so the web handles it here instead of through `run-slash`.
      new-m (let [seed
                  (some-> (second new-m)
                          str
                          str/trim
                          not-empty)

                  id
                  (get (vis/gateway-create-session! {}) "id")]

              ;; `/new-session <text>` (TUI parity): the trailing text is the new
              ;; session's FIRST MESSAGE — submit it as a turn so the session opens
              ;; already running on that prompt (not as the title).
              (when (and id seed)
                (try (vis/gateway-submit-turn! id {:request seed}) (catch Throwable _ nil)))
              {:status 200
               :headers {"Content-Type" "text/html; charset=utf-8"
                         "HX-Redirect" (str "/ui/session/" id)}
               :body ""})
      ;; `/settings`, `/providers`, `/switch-session` — open the panel the
      ;; sidebar buttons open (TUI palette parity), via an OOB #modal swap.
      (and sid (= native "settings")) {:status 200
                                       :headers {"Content-Type" "text/html; charset=utf-8"}
                                       :body (oob-modal (:body (settings-handler request)))}
      (and sid (= native "providers")) {:status 200
                                        :headers {"Content-Type" "text/html; charset=utf-8"}
                                        :body (oob-modal (providers-modal sid))}
      (and sid (= native "switch-session")) {:status 200
                                             :headers {"Content-Type" "text/html; charset=utf-8"}
                                             :body (oob-modal (:body (sessions-switch-handler
                                                                       request)))}
      ;; `/fork-session [title]` — mint a fresh workspace clone + fork the
      ;; current session's latest state (TUI parity), then refresh so the page
      ;; loads the forked branch.
      (and sid (= native "fork-session"))
      (let [title
            (some-> (re-matches #"(?i)/fork-session\s+(.*)" text)
                    second
                    str/trim
                    not-empty)

            db
            (vis/db-info)

            ws
            (try (:id (vis/workspace-ensure-workspace! db {})) (catch Throwable _ nil))

            fork
            (when ws
              (try (vis/db-fork-session! db
                                         (str sid)
                                         (cond-> {:workspace-id ws}
                                           title
                                           (assoc :title title)))
                   (catch Throwable _ nil)))]

        {:status 200
         :headers (cond-> {"Content-Type" "text/html; charset=utf-8"}
                    fork
                    (assoc "HX-Refresh" "true"))
         :body (if fork
                 ""
                 (str (user-bubble-html text) (slash-bubble {:error "could not fork session"})))})
      ;; Real engine slash — handled command or genuinely unknown — answered
      ;; inline. A prompt-template match (`slash-template?`) is deliberately
      ;; excluded here so it falls to `:else` and runs as a normal turn.
      (and slash? (not slash-template?)) {:status 200
                                          :headers {"Content-Type" "text/html; charset=utf-8"}
                                          :body (str (user-bubble-html text)
                                                     (if (:handled? slash-result)
                                                       (slash-bubble slash-result)
                                                       (slash-bubble
                                                         {:error (str "unknown command: " text)})))}
      :else
      (let [atts
            (composer-attachments (get-in request [:multipart-params "attachment"]))

            result
            (when sid
              (vis/gateway-submit-turn! sid
                                        (cond-> {:request text}
                                          (seq atts)
                                          (assoc :attachments atts))))]

        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body
         (let [turn (:turn result)]
           (cond (and turn (= "queued" (get turn "status"))) (oob-queued sid)
                 turn (user-bubble-html text (turn-live-key "user" turn) (vis/wire-canonical atts))
                 :else (html [:div.bubble.b-vis [:div.role.role-vis "Vis"]
                              [:p.empty
                               (str "rejected: " (or (:message result) "invalid request"))]])))}))))

(defn- slash-list-handler
  "GET /ui/slash — the typed-`/` palette for the composer's `/` autocomplete
   and the Cmd+P command menu. Just the canonical `vis/slash-palette` for the
   `:web` channel (registered leaf slashes + prompt-template skills / file
   prompts, TUI-parity) with the web-native channel slashes prepended — those
   (`/new-session`, `/fork-session`, `/switch-session`, `/providers`,
   `/settings`) are handled in `submit-turn-handler`, not engine slashes."
  [_request]
  (let [specs (vis/slash-palette :web
                                 [{:name "/new-session" :doc "Start a new session"}
                                  {:name "/fork-session" :doc "Fork the current session"}
                                  {:name "/switch-session" :doc "Switch to another session"}
                                  {:name "/providers" :doc "Configure providers & model"}
                                  {:name "/settings" :doc "Open settings"}])]
    {:status 200
     :headers {"Content-Type" "application/json; charset=utf-8"}
     :body (str "[" (str/join "," (map #(json-text %) specs)) "]")}))

;; =============================================================================
;; Modals: Settings (toggles) + Providers — the TUI dialogs, as overlays
;; =============================================================================

(defn- toggle-id->wire [id] (str (namespace id) "/" (name id)))
(defn- wire->toggle-id
  [s]
  (let [[ns* n] (str/split (str s) #"/" 2)]
    (when (and ns* n) (keyword ns* n))))

(defn- toggle-row
  "One settings row. Boolean toggles render a switch; enum toggles show
   the current value and a cycle button. The row swaps itself on change."
  [{:keys [id label description]}]
  (let [wire-id
        (toggle-id->wire id)

        choices
        (try (vis/toggle-choices id) (catch Throwable _ nil))]

    [:div.toggle-row {:id (str "tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))}
     [:div.toggle-text [:div.toggle-label (str (or label id))]
      (when description [:div.toggle-desc (str description)])]
     (if (seq choices)
       [:button.toggle-cycle
        {:type "button"
         :hx-post "/ui/settings/cycle"
         :hx-vals (json-text {:id wire-id})
         :hx-target (str "#tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))
         :hx-swap "outerHTML"} (str (try (vis/toggle-value id) (catch Throwable _ "?")))]
       [:button
        {:type "button"
         :class (str "switch" (when (try (vis/toggle-enabled? id) (catch Throwable _ false)) " on"))
         :aria-label (str "Toggle " (or label id))
         :hx-post "/ui/settings/toggle"
         :hx-vals (json-text {:id wire-id})
         :hx-target (str "#tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))
         :hx-swap "outerHTML"} [:span.knob]])]))

(defn- modal-shell
  "Standard overlay+modal frame. An optional opts map as the FIRST body
   arg sets per-modal tweaks: :class adds a modifier (e.g. modal-wide)
   to the .modal box so a content-heavy dialog (Settings, Plan) can be
   wider than the default."
  [title & body]
  (let [opts
        (when (map? (first body)) (first body))

        body
        (if opts (rest body) body)]

    (html [:div.overlay {:data-close-modal "backdrop"}
           [:div.modal (when (:class opts) {:class (:class opts)})
            [:div.modal-head [:h2 title]
             [:button.bar-toggle {:type "button" :data-close-modal "x" :aria-label "Close"}
              (icon "x")]] (into [:div.modal-body] body)]])))

(defn- delete-session-confirm-handler
  "GET /ui/session/:sid/delete — styled confirm dialog replacing the
   native `hx-confirm` browser prompt. Cancel closes the modal; the
   danger button issues the real DELETE (handler semantics unchanged)."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        title
        (some #(when (= (str (get % "id")) (str sid)) (get % "title")) (vis/gateway-list-sessions))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Delete session"
                        [:div.confirm-del
                         [:p.confirm-del-text "Delete " [:strong (or title "Untitled")] "?" [:br]
                          "This permanently removes the session and its history."]
                         [:div.confirm-del-actions
                          [:button.btn-ghost {:type "button" :data-close-modal "x"} "Cancel"]
                          [:button.btn-danger
                           {:type "button" :hx-delete (str "/ui/session/" sid) :hx-swap "none"}
                           "Delete session"]]])}))

(defn- delete-sessions-confirm-handler
  "GET /ui/sessions/delete - confirm dialog for the sidebar's bulk
   select-mode. htmx `hx-include` ships every checked checkbox as a
   repeated `sid` query param; the danger button re-posts the same ids
   as hidden inputs to the real bulk DELETE."
  [request]
  (let [sids
        (sid-params (get-in request [:query-params "sid"]))

        n
        (count sids)]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if (zero? n)
             (modal-shell "Delete sessions"
                          [:div.confirm-del [:p.confirm-del-text "No sessions selected."]
                           [:div.confirm-del-actions
                            [:button.btn-ghost {:type "button" :data-close-modal "x"} "Close"]]])
             (modal-shell "Delete sessions"
                          [:form.confirm-del {:hx-post "/ui/sessions/delete" :hx-swap "none"}
                           (for [sid sids]
                             [:input {:type "hidden" :name "sid" :value (str sid)}])
                           [:p.confirm-del-text "Delete "
                            [:strong (str n (if (= n 1) " session" " sessions"))] "?" [:br]
                            "This permanently removes them and their history."]
                           [:div.confirm-del-actions
                            [:button.btn-ghost {:type "button" :data-close-modal "x"} "Cancel"]
                            [:button.btn-danger {:type "submit"} "Delete selected"]]]))}))

(defn- settings-handler
  "GET /ui/settings — the settings dialog as a VS Code-style overlay: a
   full-width search that filters every row live (ui.js), a left category
   rail (table of contents), and the grouped, in-place-flippable toggles on
   the right. Provider-specific knobs (e.g. OpenAI Codex verbosity) declare a
   `:visible-fn` and only appear when their provider is configured."
  [_request]
  (let [toggles
        (vis/toggles-for-channel :web)

        grouped
        (sort-by (comp str key) (group-by #(or (:group %) :other) toggles))

        ;; Group keywords are internal (:provider, :display, …); present them
        ;; title-cased ("Provider", "Display") not raw.
        group-title
        (fn [g]
          (str/capitalize (str/replace (name g) #"[-_]+" " ")))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Settings"
                        {:class "modal-wide settings-modal"}
                        [:div.settings-pane
                         [:div.settings-search
                          [:svg.settings-search-icon
                           {:aria-hidden "true"
                            :viewBox "0 0 24 24"
                            :fill "none"
                            :stroke "currentColor"
                            :stroke-width "2"
                            :stroke-linecap "round"
                            :stroke-linejoin "round"} [:circle {:cx "11" :cy "11" :r "8"}]
                           [:line {:x1 "21" :y1 "21" :x2 "16.65" :y2 "16.65"}]]
                          [:input#settings-search
                           {:type "text"
                            :autocomplete "off"
                            :spellcheck "false"
                            :placeholder "Search settings…"
                            :aria-label "Search settings"}] [:span#settings-count.settings-count]]
                         [:div.settings-cols
                          [:nav.settings-toc {:aria-label "Settings categories"}
                           (for [[group _] grouped]
                             [:button.settings-toc-item {:type "button" :data-group (name group)}
                              (group-title group)])]
                          [:div.settings-groups
                           (for [[group specs] grouped]
                             [:section.settings-group {:data-group (name group)}
                              [:h3 (group-title group)] (map toggle-row specs)])]]])}))

(defn- sessions-switch-handler
  "GET /ui/sessions/switch — a session picker modal (the web twin of the
   TUI's `/switch-session`). Lists every session; clicking one navigates."
  [_request]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (modal-shell "Switch session"
                      (let [sessions (vis/gateway-list-sessions)]
                        (if (seq sessions)
                          [:ul.modal-sessions
                           (for [{:strs [id title status]} sessions]
                             [:li
                              [:a.modal-session-row {:href (str "/ui/session/" id)}
                               [:span (or (not-empty title) "Untitled")]
                               (when (= status "running") [:span.side-dot])]])]
                          [:p.empty "no sessions yet"])))})

(defn- sessions-list-handler
  "GET /ui/sessions/list?q= — JSON session list for the composer's INLINE
   `/switch-session` selector (the arrow-navigable dropdown — consistent with
   the `/` and `@` pickers, no modal). Each item carries a `nav` URL the
   picker navigates to on select."
  [request]
  (let [q
        (str/lower-case (str (get-in request [:query-params "q"])))

        items
        (->> (vis/gateway-list-sessions)
             (filter (fn [s]
                       (or (str/blank? q)
                           (str/includes? (str/lower-case (str (or (get s "title") "untitled")))
                                          q))))
             (map (fn [s]
                    {:name (or (not-empty (get s "title")) "Untitled")
                     :doc (str (when (= "running" (get s "status")) "● ")
                               (subs (str (get s "id")) 0 8))
                     :nav (str "/ui/session/" (get s "id"))})))]

    {:status 200
     :headers {"Content-Type" "application/json; charset=utf-8"}
     :body (str "[" (str/join "," (map #(json-text %) items)) "]")}))
;; ── Projects: create / rename / delete / move project sessions ───────
;;
;; The web twin of the TUI navigator's Ctrl+B move-to-project. All four
;; mutations flow through the SAME gateway projects API the TUI uses
;; (`vis/gateway-*-project!`, channel "web") and answer with HX-Refresh so the
;; project sidebar (`sidebar-content`) re-renders in one hop.

(defn- hx-refresh
  "Empty response that tells htmx to reload the page — the simplest reliable
   way to re-render the whole project sidebar after a mutation."
  []
  {:status 200 :headers {"HX-Refresh" "true"} :body ""})

(defn- path-project-id
  "Parse the `:pid` path param into a UUID (nil when absent / malformed)."
  [request]
  (some-> (get-in request [:path-params :pid])
          parse-uuid))

(defn- web-projects
  "All projects (cross-channel), newest gateway view. Never throws — an unreachable
   daemon yields nil (the drawer then shows only projectless sessions)."
  []
  (try (vis/gateway-list-projects) (catch Throwable _ nil)))

(defn- new-project-modal-handler
  "GET /ui/projects/new — name a new project."
  [_request]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body
   (modal-shell
     "New project"
     [:form.proj-form {:hx-post "/ui/projects" :hx-swap "none"}
      [:input.proj-input
       {:type "text" :name "name" :autocomplete "off" :autofocus true :placeholder "Project name"}]
      [:div.proj-actions [:button.btn-ghost {:type "button" :data-close-modal "x"} "Cancel"]
       [:button.btn-primary {:type "submit"} "Create project"]]])})

(defn- create-project-handler
  "POST /ui/projects {name} — create a cross-channel project, then refresh the drawer."
  [request]
  (let [name (str/trim (str (get-in request [:form-params "name"])))]
    (when-not (str/blank? name)
      (try (vis/gateway-create-project! {:name name}) (catch Throwable _ nil)))
    (hx-refresh)))

(defn- manage-project-handler
  "GET /ui/project/:pid — rename OR delete a project."
  [request]
  (let [pid
        (path-project-id request)

        p
        (some #(when (= (str (get % "id")) (str pid)) %) (web-projects))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell
             "Manage project"
             [:form.proj-form {:hx-post (str "/ui/project/" pid "/rename") :hx-swap "none"}
              [:input.proj-input
               {:type "text"
                :name "name"
                :autocomplete "off"
                :autofocus true
                :value (or (get p "name") "")
                :placeholder "Project name"}]
              [:div.proj-actions
               [:button.btn-danger
                {:type "button" :hx-post (str "/ui/project/" pid "/delete") :hx-swap "none"}
                "Delete"] [:span.proj-actions-spacer]
               [:button.btn-ghost {:type "button" :data-close-modal "x"} "Cancel"]
               [:button.btn-primary {:type "submit"} "Save"]]])}))

(defn- rename-project-handler
  "POST /ui/project/:pid/rename {name} — rename a project."
  [request]
  (let [pid
        (path-project-id request)

        name
        (str/trim (str (get-in request [:form-params "name"])))]

    (when (and pid (not (str/blank? name)))
      (try (vis/gateway-update-project! pid {:name name}) (catch Throwable _ nil)))
    (hx-refresh)))

(defn- delete-project-handler
  "POST /ui/project/:pid/delete — delete a project; its project sessions
   scatter back to projectless (never deleted)."
  [request]
  (when-let [pid (path-project-id request)]
    (try (vis/gateway-delete-project! pid) (catch Throwable _ nil)))
  (hx-refresh))

(defn- move-session-modal-handler
  "GET /ui/session/:sid/move — pick a project for THIS session (or clear it).
   The current project is highlighted; a shortcut opens the new-project modal."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        cur
        (some #(when (= (str (get % "id")) (str sid)) (get % "project_id"))
              (vis/gateway-list-sessions))

        projects
        (sort-by (juxt #(get % "position") #(get % "name")) (web-projects))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell
             "Move to project"
             [:div.proj-pick
              ;; A single native <select> is the reliable move control (the user
              ;; picks a project — or "No project" to clear — and the change POSTs
              ;; straight to /move). The current project is preselected.
              [:form {:hx-post (str "/ui/session/" sid "/move") :hx-swap "none"}
               [:select.proj-select
                {:name "project_id"
                 :aria-label "Move session to project"
                 :hx-post (str "/ui/session/" sid "/move")
                 :hx-trigger "change"
                 :hx-swap "none"}
                [:option {:value "" :selected (str/blank? (str cur))} "— No project —"]
                (for [p projects]
                  [:option {:value (str (get p "id")) :selected (= (str (get p "id")) (str cur))}
                   (or (not-empty (get p "name")) "Project")])]]
              [:button.proj-pick-row.proj-pick-new
               {:type "button" :hx-get "/ui/projects/new" :hx-target "#modal" :hx-swap "innerHTML"}
               (icon "folder-plus") [:span "New project…"]]])}))

(defn- assign-session-handler
  "POST /ui/session/:sid/move {project_id} — assign (blank clears), then refresh."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        pid
        (let [p (str/trim (str (get-in request [:form-params "project_id"])))]
          (when-not (str/blank? p) p))]

    (when sid (try (vis/gateway-assign-project! sid pid) (catch Throwable _ nil)))
    (hx-refresh)))
(defn- reorder-session-handler
  "POST /ui/session/:sid/reorder {dir} — move THIS project session up or down
   within its project and persist the new order via the gateway
   (`gateway-reorder-project-sessions!`). The web twin of dragging a TUI tab."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        dir
        (str/trim (str (get-in request [:form-params "dir"])))

        sessions
        (vis/gateway-list-sessions)

        pid
        (some #(when (= (str (get % "id")) (str sid)) (get % "project_id")) sessions)

        ordered
        (when pid
          (->> sessions
               (filter #(= (str (get % "project_id")) (str pid)))
               (sort-by (juxt #(get % "project_position") #(get % "name")))
               (mapv #(str (get % "id")))))

        idx
        (when ordered (.indexOf ^java.util.List ordered (str sid)))

        swap-with
        (when (and idx (>= (long idx) 0))
          (case dir
            "up"
            (when (> (long idx) 0) (dec (long idx)))

            "down"
            (when (< (long idx) (dec (count ordered))) (inc (long idx)))

            nil))

        reordered
        (when swap-with
          (assoc ordered
            idx (nth ordered swap-with)
            swap-with (nth ordered idx)))]

    (when (and pid reordered)
      (try (vis/gateway-reorder-project-sessions! pid reordered) (catch Throwable _ nil)))
    (hx-refresh)))

(defn- create-project-session-handler
  "POST /ui/project/:pid/session - create a NEW session already filed under this
   project, then bounce to it (Claude/Codex 'new chat in project')."
  [request]
  (let [pid
        (path-project-id request)

        id
        (get (vis/gateway-create-session! {}) "id")]

    (when (and pid id)
      (try (vis/gateway-assign-project! (parse-uuid (str id)) (str pid)) (catch Throwable _ nil)))
    {:status 303 :headers {"Location" (str "/ui/session/" id)} :body ""}))


;; ── Backgrounds (managed resources): progressive "add" flow ──────────
;;
;; The Start section is data-driven from :ext/startable-resources. Rather
;; than dump every field of every startable at once, each startable (or a
;; :group of variant startables, e.g. MCP Local/Remote) renders COLLAPSED —
;; one card with a label + an Add button. Clicking Add swaps the card's
;; inner HTML for the form (htmx fragment), an SPA "smooth filling" feel
;; with no modal repaint. Grouped startables get a segmented transport
;; chooser (Local/Remote) that swaps only the field panel.

(defn- bg-slug
  "URL/CSS/id-safe token from an arbitrary string (lowercase, non-alnum → -)."
  [s]
  (-> (str s)
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-+|-+$)" "")))

(defn- startable-entries
  "Fold the flat startable list into ordered ENTRIES, collapsing startables
   that share a :group into one entry with several :variants (first-seen
   order preserved). Each entry: {:eid :label :variants}."
  [startables]
  (reduce (fn [acc sr]
            (let [k
                  (or (some->> (:group sr)
                               bg-slug
                               (str "g-"))
                      (str "k-" (name (:kind sr))))

                  i
                  (first (keep-indexed (fn [i e]
                                         (when (= (:eid e) k) i))
                                       acc))]

              (if i
                (update-in acc [i :variants] conj sr)
                (conj acc {:eid k :label (or (:group sr) (:label sr)) :variants [sr]}))))
          []
          startables))

(defn- entry-by-eid
  [startables eid]
  (some #(when (= eid (:eid %)) %) (startable-entries startables)))

(defn- bg-field
  "One declared field → its labelled control. Honours :type (:textarea,
   :password, else text) and an optional :hint under the input."
  [{fname :name :keys [label placeholder required type hint]}]
  [:label.modal-res-field [:span (or label (name fname))]
   (if (= type :textarea)
     [:textarea
      {:name (str "field_" (name fname))
       :rows 3
       :placeholder (or placeholder "")
       :required (when required "required")}]
     [:input
      {:type (if (= type :password) "password" "text")
       :name (str "field_" (name fname))
       :placeholder (or placeholder "")
       :required (when required "required")}]) (when hint [:span.modal-res-fieldhint hint])])

(defn- bg-start-form
  "The submit form for ONE startable variant `sr`: its option chips or its
   declared fields, ending in Cancel + Add. Posts the canonical
   /resources/start; `eid` lets Cancel restore the collapsed card."
  [sid eid sr env]
  (let [opts
        (try (when-let [f (:options-fn sr)]
               (f env))
             (catch Throwable _ nil))

        olabel
        (or (:options-label sr) "options")

        fields
        (seq (:fields sr))

        root
        (str (:workspace/root env))]

    [:form.modal-res-start-form
     {:hx-post (str "/ui/session/" sid "/resources/start") :hx-target "#modal" :hx-swap "innerHTML"}
     [:input {:type "hidden" :name "kind" :value (name (:kind sr))}]
     (when (:dir? sr)
       [:label.modal-res-field [:span "Directory"]
        [:input
         {:type "text"
          :name "dir"
          :value root
          :placeholder root
          :autocomplete "off"
          :spellcheck "false"}]
        [:span.modal-res-fieldhint "where the REPL boots — defaults to the workspace root"]])
     (when (:options-fn sr)
       (if (seq opts)
         [:div.alias-chips
          (for [o opts]
            [:label.alias-chip [:input {:type "checkbox" :name "option" :value (str o)}]
             [:span (str o)]])]
         [:p.modal-res-hint (str "no " olabel " here")]))
     (when fields [:div.modal-res-fields (map bg-field fields)])
     [:div.modal-res-form-actions
      [:button.btn-secondary
       {:type "button"
        :hx-get (str "/ui/session/" sid "/backgrounds/add?entry=" eid "&collapse=1")
        :hx-target (str "#bg-" eid)
        :hx-swap "innerHTML"} "Cancel"] [:button.btn-primary {:type "submit"} "Add"]]]))

(defn- bg-expanded
  "Expanded card body. A grouped entry shows a segmented transport chooser
   (its variants) above the selected variant's form; a single startable
   shows its form directly."
  [sid entry variant-id env]
  (let [variants
        (:variants entry)

        grouped?
        (> (count variants) 1)

        sel
        (or (some #(when (= variant-id (get-in % [:variant :id])) %) variants) (first variants))

        eid
        (:eid entry)]

    (list [:div.modal-res-start-head
           [:span.modal-res-start-label (:label entry)
            (when-let [h (get-in sel [:variant :hint])]
              [:span.modal-res-start-hint h])]]
          (when grouped?
            [:div.bg-seg
             (for [v
                   variants

                   :let [vid
                         (get-in v [:variant :id])]]

               [:button
                {:type "button"
                 :class (str "bg-seg-btn" (when (= v sel) " is-active"))
                 :hx-get (str "/ui/session/" sid "/backgrounds/add?entry=" eid "&variant=" vid)
                 :hx-target (str "#bg-" eid)
                 :hx-swap "innerHTML"} (get-in v [:variant :label])])])
          (bg-start-form sid eid sel env))))

(defn- bg-collapsed
  "Collapsed card body: label + one-line summary + a single Add button that
   reveals the form via an htmx fragment swap."
  [sid entry]
  (let [eid
        (:eid entry)

        vs
        (:variants entry)

        v0
        (first vs)

        hint
        (cond (> (count vs) 1) (str/join " · " (map #(get-in % [:variant :label]) vs))
              (:fields v0) "configure and add"
              (:options-fn v0) (str "pick " (or (:options-label v0) "options") " and start")
              :else "start")]

    (list [:div.modal-res-start-head
           [:span.modal-res-start-label (:label entry) [:span.modal-res-start-hint hint]]
           [:button.modal-res-go
            {:type "button"
             :hx-get (str "/ui/session/" sid "/backgrounds/add?entry=" eid)
             :hx-target (str "#bg-" eid)
             :hx-swap "innerHTML"
             :aria-label (str "Add " (:label entry))
             :title (str "Add " (:label entry))} (icon "plus")]])))

(defn- bg-card
  "Collapsed background card, a stable `#bg-<eid>` swap target."
  [sid entry]
  [:div.modal-res-start {:id (str "bg-" (:eid entry))} (bg-collapsed sid entry)])

(defn- resources-modal
  "The managed-resources panel (the web twin of the TUI F4 dialog): a Start
   nREPL control (deps.edn aliases as selectable chips, from the clojure ext) +
   every live session-scoped resource with stop / restart actions. Returns the
   modal-shell HTML string. `notice` is an optional status line at the top."
  ([sid] (resources-modal sid nil))
  ([sid notice]
   (modal-shell
     "Backgrounds"
     {:class "modal-mid"}
     (when notice [:p.slash-error notice])
     ;; One collapsed card PER declared startable ENTRY (data-driven from
     ;; :ext/startable-resources). Clicking a card's Add button reveals its
     ;; form via an htmx fragment swap — grouped startables (e.g. MCP) show a
     ;; Local/Remote transport chooser. Nothing resource-specific here.
     (let [startables (try (vis/registered-startable-resources) (catch Throwable _ []))]
       (for [entry (startable-entries startables)]
         (bg-card sid entry)))
     (let [rs (try (vis/list-resources sid) (catch Throwable _ []))]
       (if (seq rs)
         [:ul.modal-resources
          (for [r rs]
            (let [rid (str (or (get r "id") (get r "name")))
                  kind (some-> (or (get r "kind") (get r "type"))
                               str)]

              [:li.modal-res-row [:span.res-dot]
               [:span.modal-res-name
                ;; the resource's own label is the readable NAME (e.g.
                ;; "nREPL vis :dev :test"); port/status ride below it.
                [:span.modal-res-title
                 ;; lead with the kind as a prominent TYPE badge so the row
                 ;; reads "what it is" at a glance, then the readable name.
                 (when kind [:span.modal-res-type kind])
                 [:span.modal-res-title-text (str (or (get r "label") (get r "id") "resource"))]]
                [:span.modal-res-meta
                 (str/join "  ·  "
                           (remove nil?
                             [(when-let [p (or (get-in r ["detail" "port"]) (get r "port"))]
                                (str ":" p))
                              (when-let [s (get r "status")]
                                (str s))]))]]
               ;; rid rides in the BODY (hx-vals), not the path — resource ids
               ;; contain slashes (e.g. "nrepl:/Users/…") that a path segment
               ;; can't carry.
               [:span.modal-res-actions
                (when (get r "can_logs")
                  [:button.btn-sm
                   {:type "button"
                    :hx-post (str "/ui/session/" sid "/resources/logs")
                    :hx-vals (json-text {:rid rid})
                    :hx-target "#modal"
                    :hx-swap "innerHTML"} "logs"])
                [:button.btn-sm
                 {:type "button"
                  :hx-post (str "/ui/session/" sid "/resources/restart")
                  :hx-vals (json-text {:rid rid})
                  :hx-target "#modal"
                  :hx-swap "innerHTML"} "restart"]
                [:button.btn-sm.btn-danger
                 {:type "button"
                  :hx-post (str "/ui/session/" sid "/resources/stop")
                  :hx-vals (json-text {:rid rid})
                  :hx-target "#modal"
                  :hx-swap "innerHTML"} "stop"]]]))]
         [:p.empty "nothing running yet — add a background above"])))))

(defn- resources-logs-pre
  "The scrollable, self-tailing <pre> for a background's ring buffer. Polls
   its own lines endpoint every 2s and auto-follows the newest line (real
   `tail -f`), releasing the follow only while the user has scrolled up to
   read history — scrolling back to the bottom re-arms it."
  [sid rid]
  (let [lines (try (vis/resource-logs sid rid) (catch Throwable _ nil))]
    [:pre.modal-res-logs
     {:id "res-logs"
      :data-follow "1"
      :hx-post (str "/ui/session/" sid "/resources/logs/lines")
      :hx-vals (json-text {:rid rid})
      :hx-trigger "load, every 2s"
      :hx-swap "innerHTML"
      :onscroll
      "this.dataset.follow=(this.scrollHeight-this.scrollTop-this.clientHeight<24)?'1':'0'"
      :hx-on:htmx:after-swap "if(this.dataset.follow!=='0'){this.scrollTop=this.scrollHeight;}"}
     (if (seq lines) (str/join "\n" lines) "no output captured yet")]))

(defn- resources-logs-modal
  "Captured output of a background resource (the web twin of the TUI logs
   viewer): a live-tailing ring-buffer <pre> that auto-follows the newest
   line, with a Back button that re-opens the resources modal. `rid` is the
   resource id."
  [sid rid]
  (let [label (some-> (try (vis/get-resource sid rid) (catch Throwable _ nil))
                      (get "label"))]
    (modal-shell (str "Logs — " (or label rid))
                 {:class "modal-wide"}
                 [:div.modal-res-actions {:style "margin-bottom:8px"}
                  [:button.btn-sm
                   {:type "button"
                    :hx-get (str "/ui/session/" sid "/resources")
                    :hx-target "#modal"
                    :hx-swap "innerHTML"} "← back"] [:span.modal-res-tail "● tailing live"]]
                 (resources-logs-pre sid rid))))

(defn- resources-modal-handler
  "GET /ui/session/:sid/resources — open the managed-resources modal."
  [request]
  (if-let [sid (some-> (get-in request [:path-params :sid])
                       parse-uuid)]
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body (resources-modal sid)}
    {:status 404 :headers {"Content-Type" "text/html; charset=utf-8"} :body "unknown session"}))

(defn- backgrounds-add-handler
  "GET /ui/session/:sid/backgrounds/add?entry=&variant=&collapse= — swap ONE
   background card between its collapsed row and its expanded add-form
   (transport chooser + fields). Pure htmx fragment, no modal repaint."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        eid
        (get-in request [:query-params "entry"])

        vid
        (get-in request [:query-params "variant"])

        coll?
        (some? (get-in request [:query-params "collapse"]))

        stbls
        (try (vis/registered-startable-resources) (catch Throwable _ []))

        entry
        (entry-by-eid stbls eid)]

    (if (and sid entry)
      (let [env (try (vis/env-for sid) (catch Throwable _ nil))]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (html (if coll? (bg-collapsed sid entry) (bg-expanded sid entry vid env)))})
      {:status 404 :headers {"Content-Type" "text/html; charset=utf-8"} :body ""})))

(defn- resource-action-handler
  "POST /ui/session/:sid/resources/:rid/(stop|restart) — run the canonical
   `vis/stop-resource!` / `vis/restart-resource!` (same path TUI F4 uses), then
   re-render the modal so the list reflects the new state."
  [request action]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        rid
        (get-in request [:form-params "rid"])]

    (when (and sid rid)
      (try (case action
             :stop
             (vis/stop-resource! sid rid)

             :restart
             (vis/restart-resource! sid rid))
           (catch Throwable _ nil)))
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if sid (resources-modal sid) "")}))

(defn- resource-stop-handler [request] (resource-action-handler request :stop))
(defn- resource-restart-handler [request] (resource-action-handler request :restart))

(defn- resource-logs-handler
  "POST /ui/session/:sid/resources/logs — render the selected background's
   captured output (via `vis/resource-logs`). `rid` rides in the body because
   resource ids can contain slashes."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        rid
        (get-in request [:form-params "rid"])]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if (and sid rid) (resources-logs-modal sid rid) "")}))
(defn- resource-logs-lines-handler
  "POST /ui/session/:sid/resources/logs/lines — the tail fragment polled by
   the live <pre>: just the ring-buffer text (escaped), swapped in place so
   the viewer's scroll position and follow state survive each refresh."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        rid
        (get-in request [:form-params "rid"])

        lines
        (when (and sid rid) (try (vis/resource-logs sid rid) (catch Throwable _ nil)))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (html (list (if (seq lines) (str/join "\n" lines) "no output captured yet")))}))

(defn- resource-start-handler
  "POST /ui/session/:sid/resources/start — start the declared startable of
   `kind`. Option-based startables receive selected chips; field-based
   startables receive a keyword map of submitted fields. Generic: no
   per-resource knowledge here. Re-renders the modal (new resource appears,
   or a notice)."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        kind
        (some-> (get-in request [:form-params "kind"])
                keyword)

        raw
        (get-in request [:form-params "option"])

        opts
        (cond (sequential? raw) raw
              (some? raw) [raw]
              :else [])

        sr
        (some #(when (= kind (:kind %)) %)
              (try (vis/registered-startable-resources) (catch Throwable _ [])))

        submitted-fields
        (into {}
              (keep (fn [{fname :name}]
                      (let [k
                            (name fname)

                            v
                            (str/trim (str (get-in request
                                                   [:form-params (str "field_" (name fname))])))]

                        (when-not (str/blank? v) [k v]))))
              (:fields sr))

        selected
        (if (seq (:fields sr)) submitted-fields opts)

        dir
        (let [d (str/trim (str (get-in request [:form-params "dir"])))]
          (when-not (str/blank? d) d))

        notice
        (cond (not (and sid kind)) "missing session or kind"
              (nil? sr) (str "unknown startable: " kind)
              :else (try ((:start-fn sr)
                           (cond-> (vis/env-for sid)
                             dir
                             (assoc :startable/dir dir))
                           selected)
                         nil
                         (catch Throwable t
                           (str "Could not start " (:label sr) ": " (ex-message t)))))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if sid (resources-modal sid notice) "")}))

(defn- settings-mutate-handler
  "POST /ui/settings/toggle | /ui/settings/cycle - flip or cycle one
   toggle, answer with the refreshed row. A theme change ALSO swaps the
   inline `#theme-vars` custom properties out-of-band, so the already-loaded
   stylesheet never detaches and Safari keeps the notch/status color in sync."
  [request]
  (let [id
        (wire->toggle-id (get-in request [:form-params "id"]))

        cycle?
        (str/ends-with? (str (:uri request)) "/cycle")]

    (if-not id
      {:status 400 :headers {"Content-Type" "text/html"} :body "bad toggle id"}
      (let [spec (some #(when (= id (:id %)) %)
                       (try (vis/registered-toggles) (catch Throwable _ [])))]
        (try (if cycle?
               (vis/toggle-cycle-value! id)
               (vis/toggle-set-enabled! id (not (vis/toggle-enabled? id))))
             (catch Throwable _ nil))
        (let [css-root (when (= id :vis-channel-web/theme) (current-web-css-root))]
          {:status 200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body (html (list (toggle-row (or spec {:id id :label (str id)}))
                             (when css-root
                               [:style {:id "theme-vars" :hx-swap-oob "true"} css-root])
                             (when css-root
                               [:meta
                                {:id "theme-color-meta"
                                 :name "theme-color"
                                 :content (theme-bg-color css-root)
                                 :hx-swap-oob "true"}])))})))))

;; ── Providers: the TUI Router dialog on the web ─────────────────────
;;
;; Everything below renders FROM the channel-neutral provider service
;; (`internal/providers.clj` via `vis.core`) — the same primitives the
;; TUI Router dialog uses: configured fleet (priority order), live
;; auth/limits diagnostics, presets, live model catalogs, config
;; persistence. The web adds only HTMX interaction.

(defn- providers-base [sid] (str "/ui/session/" sid "/providers"))

(defn- modal-back
  "‹ back row into a parent modal view."
  [url label]
  [:button.modal-back {:type "button" :hx-get url :hx-target "#modal" :hx-swap "innerHTML"} "‹ "
   label])

(defn- provider-card
  "One fleet card, the TUI `draw-provider-card!` in HTML:
     line 1: (N) Label            host ●
     line 2: ★ primary (+N models) / quota summary   — or the error
     chips:  session-scoped model picker
     row:    Models · Status · Key · Remove

   `diag` is `{:status .. :limits ..}` or nil; nil renders a skeleton
   that pulls its own diagnostics (`hx-trigger load`), so the modal
   opens instantly and the cards light up as probes answer — the web
   twin of the TUI's worker-future refresh. Provider priority moves via
   tap-friendly arrows instead of drag-and-drop, so it works on mobile."
  [sid {:keys [id models] :as provider} idx total diag]
  (let [base
        (providers-base sid)

        pid
        (name id)

        label
        (vis/display-label id)

        host
        (vis/provider-url-host (or (vis/provider-base-url provider) ""))

        loading?
        (nil? diag)

        status
        (:status diag)

        limits
        (:limits diag)

        ok?
        (boolean (:authenticated? status))

        error
        (when diag (or (:error status) (get-in limits [:error :message])))

        summary
        (when diag
          (or (vis/limits-dynamic-summary limits)
              (some->> (get-in limits [:static :rpm])
                       (str "catalog RPM "))))

        primary
        (or (:name (first models)) "--")

        suffix
        (if (<= (count models) 1) "(1 model)" (str "(+" (dec (count models)) " models)"))

        act
        (fn [label* attrs]
          [:button.pcard-act (merge {:type "button" :hx-target "#modal" :hx-swap "innerHTML"} attrs)
           label*])

        move-act
        (fn [dir label* disabled?]
          [:button.pcard-move
           {:type "button"
            :aria-label (str "Move " label " " label*)
            :title (str "Move " label*)
            :disabled disabled?
            :hx-post (str base "/reorder")
            :hx-vals (json-text {:pid pid :dir dir})
            :hx-target "#provider-cards"
            :hx-swap "outerHTML"} label*])]

    [:div.pcard
     (cond-> {:id (str "pcard-" pid) :data-pid pid}
       loading?
       (assoc :hx-get
         (str base "/p/" pid "/diag") :hx-trigger
         "load" :hx-swap
         "outerHTML"))
     [:div.pcard-line [:span.pcard-pri (inc (long idx))]
      [:span.pcard-moves (move-act "up" "↑" (zero? (long idx)))
       (move-act "down" "↓" (>= (long idx) (dec (long total))))] [:span.pcard-label label]
      [:span.pcard-host host]
      [:span
       {:class (str "provider-dot"
                    (cond loading? ""
                          ok? " on"
                          :else " bad"))}]]
     (if error
       [:div.pcard-err (str "⚠ " error)]
       [:div.pcard-sub [:span.pcard-primary (str "★ " primary)] [:span.pcard-count suffix]])
     ;; limits ride a DEDICATED line that exists in EVERY card state
     ;; (skeleton, loaded, no-data) — the async diag swap can't change
     ;; the card height, so the modal never jumps as probes answer
     (when-not error
       [:div.pcard-limits-line
        (cond loading? [:span.pcard-checking "checking auth / limits…"]
              summary [:span.pcard-limits summary]
              :else [:span.pcard-limits.pcard-nodata "no quota data"])])
     [:div.pcard-acts (act "Models" {:hx-get (str base "/p/" pid "/models")})
      (act "Status" {:hx-get (str base "/p/" pid "/status")})
      (when (= :api-key (vis/provider-auth-kind id))
        (act "API key" {:hx-get (str base "/p/" pid "/key")}))
      (act "Remove"
           {:hx-post (str base "/p/" pid "/remove") :hx-confirm (str "Remove " label "?")})]]))

(defn- provider-cards-view
  "Swappable provider fleet body. Reorder arrows target this node so the
   overlay/modal shell stays mounted and does not re-run its entry animation."
  [sid providers]
  [:div#provider-cards.pcards
   (if (seq providers)
     (map-indexed (fn [idx provider]
                    (provider-card sid provider idx (count providers) nil))
                  providers)
     [:p.empty "No providers configured yet."])])

(defn- providers-modal
  "Providers dialog: the session model preference + the persisted
   provider fleet (the TUI Router), with add/manage/remove."
  [sid]
  (let [providers
        (vis/configured-providers)

        pref
        (vis/gateway-session-model sid)

        default-active
        (try (vis/resolve-effective-model (vis/get-router)) (catch Throwable _ nil))]

    (modal-shell "Providers"
                 ;; No standalone "router default" chip: on the default route the
                 ;; session line ALREADY names what the router resolves to — a
                 ;; second router button was noise. The reset affordance appears
                 ;; only while a session override is set.
                 ;; READ-ONLY here: this card manages the GLOBAL fleet + ★ Primary.
                 ;; The per-session model is chosen from the rail's Routing panel (tap "Change").
                 [:p.active-model "This session: "
                  [:strong
                   (if pref
                     (str (:provider pref) "/" (:model pref))
                     (str (some-> (:provider default-active)
                                  name)
                          "/"
                          (:name default-active)
                          " (default)"))]
                  [:span.active-model-hint " · change from the Routing panel in the context rail"]]
                 (provider-cards-view sid providers)
                 [:button.add-provider
                  {:type "button"
                   :hx-get (str (providers-base sid) "/add")
                   :hx-target "#modal"
                   :hx-swap "innerHTML"} "+ Add provider"])))

(defn- fmt-mtok-price
  "Compact USD label for a per-MILLION-token price: `5.0` -> \"$5\",
   `1.75` -> \"$1.75\", `0.75` -> \"$0.75\". nil for non-numbers."
  [n]
  (when (number? n)
    (str "$"
         (if (== (double n) (Math/floor (double n)))
           (str (long n))
           (-> (String/format java.util.Locale/ROOT "%.2f" (object-array [(double n)]))
               (str/replace #"0+$" "")
               (str/replace #"\.$" ""))))))

(defn- model-pick-body
  "The picker's SWAPPABLE inner content (active-model line + chips), wrapped
   in `#model-pick`. Picking a chip POSTs /provider and swaps JUST this node
   (`hx-target #model-pick`, outerHTML) — so the modal shell + overlay stay
   put: the modal does NOT close and does NOT re-run its entry animation
   (the 'whole picker jumps' on every click). Only the active-model line and
   the highlighted chip update in place."
  [sid]
  (let [providers
        (vis/configured-providers)

        pref
        (vis/gateway-session-model sid)

        ; {:provider :model} or nil
        default-active
        (try (vis/resolve-effective-model (vis/get-router)) (catch Throwable _ nil))

        chip
        (fn [provider-id model-name label]
          (let [current?
                (and (= (not-empty model-name) (:model pref))
                     (= (not-empty provider-id) (:provider pref)))

                router-default?
                (str/blank? (str model-name))

                price
                (vis/model-pricing model-name)

                in$
                (some-> price
                        :input
                        fmt-mtok-price)

                out$
                (some-> price
                        :output
                        fmt-mtok-price)]

            [:button
             {:type "button"
              :class (str "model-chip" (when current? " current"))
              :hx-post (str "/ui/session/" sid "/provider")
              :hx-vals (json-text {:provider (or provider-id "") :model (or model-name "")})
              :hx-target "#model-pick"
              :hx-swap "outerHTML"}
             ;; Icon LEFT (selection state), name, price RIGHT (in / out per 1M).
             [:span.mc-icon
              (icon (cond current? "check"
                          router-default? "star"
                          :else "cpu"))] [:span.mc-name label]
             (when (and in$ out$)
               [:span.mc-price {:title "input / output per 1M tokens"} in$ [:span.mc-price-sep "/"]
                out$])]))]

    [:div#model-pick
     [:p.active-model "This session: "
      [:strong
       (if pref
         (str (:provider pref) "/" (:model pref))
         (str (some-> (:provider default-active)
                      name)
              "/"
              (:name default-active)
              " (default)"))]]
     (if (seq providers)
       [:div.model-groups
        ;; Router default sits on its own, above the per-provider groups.
        [:div.model-chips.pick (chip "" "" "router default")]
        ;; One group per provider: the provider name is a header and the
        ;; models hang under it as bare chips (no repeated `provider/` prefix).
        (for [p
              providers

              :let [pid
                    (name (:id p))

                    models
                    (filter :name (:models p))]
              :when (seq models)]

          [:div.model-group [:p.model-group-label (vis/display-label (:id p))]
           [:div.model-chips.pick
            (for [m
                  models

                  :let [nm
                        (:name m)]]

              (chip pid nm nm))]])]
       [:p.empty "No providers configured yet — add one under Providers."])]))

(defn- session-model-picker
  "Footer-opened per-session model chooser — the ONE place a session's model
   is set. Lists every configured model as a chip (the active one
   highlighted) plus a `router default` reset. Picking POSTs /provider, which
   hoists that model to the router root for this session (see
   `prepare-turn-context`), OOB-refreshes the rail's routing, and re-renders
   the picker body IN PLACE (modal stays open). Distinct from the provider
   cards' ★ Primary, which sets the GLOBAL default for every session."
  [sid]
  (modal-shell "Session model" (model-pick-body sid)))

(defn- reasoning-pick-body
  "The effort picker's SWAPPABLE inner content (active level line + 3 chips),
  wrapped in `#reasoning-pick`. Picking a chip POSTs /reasoning and swaps JUST
  this node (`hx-target #reasoning-pick`, outerHTML) — so the modal shell +
  overlay stay put (same trick as `model-pick-body`). Source of truth is the
  global `:vis/reasoning-level` toggle: a level set here is what the next turn's
  `:reasoning-default` carries (see `submit-turn-handler`)."
  [sid]
  (let [info
        (session-resolved-model-info)

        configurable?
        (reasoning-effort-configurable? info)

        current
        (session-reasoning-level)

        chip
        (fn [level label]
          [:button.model-chip
           {:type "button"
            :class (str "model-chip" (when (= level current) " current"))
            :hx-post (str "/ui/session/" sid "/reasoning")
            :hx-vals (json-text {:level (name level)})
            :hx-target "#reasoning-pick"
            :hx-swap "outerHTML"} label])]

    [:div#reasoning-pick
     [:p.active-model "Reasoning effort: " [:strong (name current)]
      [:span.active-model-hint
       (if configurable?
         " · how hard reasoning-capable models think before answering"
         (str " · this model ("
              (or (:name info) "?")
              ") ignores the level — it is kept for the next reasoning model"))]]
     [:div.model-chips.pick (chip :quick "quick") (chip :balanced "balanced")
      (chip :deep "deep")]]))

(defn- session-reasoning-picker
  "Footer-opened reasoning-effort chooser — the twin of `session-model-picker`.
  Three levels (quick / balanced / deep), the same cycle the TUI footer walks
  with Ctrl+R and `:vis/reasoning-level` persists globally. The backend maps
  these onto the provider wire (and clamps Copilot+Claude `:deep` → `:balanced`
  when the premium cap is off, see `copilot-claude-safe-reasoning-level`)."
  [sid]
  (modal-shell "Reasoning effort" (reasoning-pick-body sid)))

(defn- configured-provider [pid] (some #(when (= pid (:id %)) %) (vis/configured-providers)))

(defn- preset-by-id [pid] (some #(when (= pid (:id %)) %) (vis/provider-presets)))

(defn- add-provider-picker
  "Step 1 of Add Provider: the preset list (`vis/provider-presets-available`
   — same source as the TUI picker), each row labeled with how it
   authenticates."
  [sid]
  (let [base
        (providers-base sid)

        available
        (vis/provider-presets-available)]

    (modal-shell "Add Provider"
                 (modal-back base "Providers")
                 (if (empty? available)
                   [:p.empty "All providers already configured."]
                   [:div.preset-rows
                    (for [{:keys [id label]} available]
                      [:button.preset-row
                       {:type "button"
                        :hx-get (str base "/add/" (name id))
                        :hx-target "#modal"
                        :hx-swap "innerHTML"} [:span.preset-label label]
                       [:span.preset-kind
                        (case (vis/provider-auth-kind id)
                          :oauth
                          "Sign in"

                          :none
                          "local"

                          "API key")]])]))))

(defn- add-model-picker
  "Model selection for a preset being added: live-fetched catalog +
   preset defaults through `vis/provider-model-options` — the same
   list the TUI shows, with the same 'Show all models…' affordance."
  [sid preset api-key show-all?]
  (let [base
        (providers-base sid)

        pid
        (:id preset)

        probe
        (cond-> {:id pid :base-url (:base-url preset) :default-models (:default-models preset)}
          (seq api-key)
          (assoc :api-key api-key))

        {:keys [models hidden-count]}
        (vis/provider-model-options probe (:default-models preset) show-all?)]

    (modal-shell
      (str (:label preset) " — Select Model")
      (modal-back (str base "/add") "Add Provider")
      (if (empty? models)
        [:p.empty "No models reported. Is the provider reachable / the key valid?"]
        [:div.model-chips.pick
         (for [m models]
           [:button.model-chip
            {:type "button"
             :hx-post (str base "/add/" (name pid) "/confirm")
             :hx-vals (json-text
                        {:model m :api_key (or api-key "") :base_url (or (:base-url preset) "")})
             :hx-target "#modal"
             :hx-swap "innerHTML"} m])])
      (when (and (not show-all?) (pos? (long hidden-count)))
        [:button.show-all
         {:type "button"
          :hx-post (str base "/add/" (name pid) "/models")
          :hx-vals (json-text
                     {:api_key (or api-key "") :base_url (or (:base-url preset) "") :show_all "1"})
          :hx-target "#modal"
          :hx-swap "innerHTML"} (str "Show all models… (" hidden-count " hidden)")]))))

(def ^:private oauth-web-flows
  "In-memory web OAuth handshakes keyed by provider id. The provider owns
   the real protocol; the web channel only supplies an output sink and,
   for browser redirect flows, a form-backed manual-code collector."
  (atom {}))

(defn- oauth-provider-detected?
  [pid]
  (let [provider
        (vis/provider-by-id pid)

        detect-fn
        (:provider/detect-fn provider)]

    (boolean (try (when detect-fn (detect-fn)) (catch Throwable _ nil)))))

(defn- oauth-flow-status
  [flow]
  (cond (nil? flow) :idle
        (realized? (:result flow)) (try (let [v @(:result flow)]
                                          (if (#{:ok :already-authenticated} v) :ok :done))
                                        (catch Throwable _ :error))
        :else :pending))

(defn- oauth-flow-lines
  [flow]
  (vec (or (some-> flow
                   :lines
                   deref)
           [])))

(defn- url-line?
  [line]
  (some->> (str line)
           (re-find #"https?://\S+")))

(defn- start-oauth-web-flow!
  [sid pid]
  (let [auth-fn (some-> (vis/provider-by-id pid)
                        :provider/auth-fn)]
    (when auth-fn
      (let [lines (atom [])
            input (promise)
            print! (fn [line]
                     (swap! lines conj (str line)))
            result (future (try (auth-fn print!
                                         {:originator "vis-web"
                                          :open-browser-fn (constantly false)
                                          :manual-code-fn (fn [_]
                                                            @input)})
                                (catch clojure.lang.ArityException _ (auth-fn print!))))
            flow {:sid sid
                  :pid pid
                  :lines lines
                  :input input
                  :result result
                  :started-at-ms (System/currentTimeMillis)}]

        (swap! oauth-web-flows assoc pid flow)
        flow))))

(defn- current-oauth-web-flow
  [pid]
  (let [flow (get @oauth-web-flows pid)]
    (when-not (#{:ok :done :error} (oauth-flow-status flow)) flow)))

(defn- oauth-web-view
  [sid pid]
  (let [base
        (providers-base sid)

        preset
        (preset-by-id pid)

        flow
        (current-oauth-web-flow pid)

        status
        (oauth-flow-status flow)

        lines
        (oauth-flow-lines flow)

        detected?
        (oauth-provider-detected? pid)]

    (modal-shell
      (str (:label preset) " — Sign In")
      (modal-back (str base "/add") "Add Provider")
      (cond detected? [:div.oauth-ready
                       [:p "Authenticated — credentials are now available on this machine."]
                       [:button.send-wide
                        {:type "button"
                         :hx-post (str base "/add/" (name pid) "/confirm")
                         :hx-vals (json-text {:api_key ""})
                         :hx-target "#modal"
                         :hx-swap "innerHTML"} (str "Add " (:label preset))]]
            (= status :pending)
            [:div.oauth-pending [:p "Finish the provider-owned sign-in, then return here."]
             (when (seq lines)
               [:div
                (for [line lines]
                  (if-let [url (url-line? line)]
                    [:p [:a {:href url :target "_blank" :rel "noreferrer"} url]]
                    [:p line]))])
             [:form.key-form
              {:hx-post (str base "/add/" (name pid) "/auth/finish")
               :hx-target "#modal"
               :hx-swap "innerHTML"}
              [:input
               {:type "text"
                :name "redirect"
                :autocomplete "off"
                :placeholder "Paste final redirect URL / authorization code if the provider asks"}]
              [:button.send-wide {:type "submit"} "Submit code / check"]]
             [:button.send-wide
              {:type "button"
               :hx-get (str base "/add/" (name pid) "/auth")
               :hx-target "#modal"
               :hx-swap "innerHTML"} "I authorized — check status"]]
            :else [:div.oauth-pending
                   [:p
                    (str (:label preset)
                         " supports OAuth. Vis can now start that flow from the web UI.")]
                   [:button.send-wide
                    {:type "button"
                     :hx-post (str base "/add/" (name pid) "/auth/start")
                     :hx-target "#modal"
                     :hx-swap "innerHTML"} "Start sign-in"] [:p "Terminal fallback:"]
                   [:pre.cmd (str "vis providers auth " (name pid))]]))))

(defn- add-provider-step
  "Step 2 of Add Provider, by auth kind:
     :api-key → key form (masked), then the model picker
     :none    → straight to the model picker (local endpoint)
     :oauth   → run the provider-owned OAuth flow from the web UI, then add."
  [sid pid]
  (let [base
        (providers-base sid)

        preset
        (preset-by-id pid)]

    (if-not preset
      (providers-modal sid)
      (case (vis/provider-auth-kind pid)
        ;; :none — local endpoint (LM Studio / Ollama). Let the user point
        ;; Vis at whatever host:port they run it on before picking a model.
        :none
        (modal-shell (str (:label preset) " Setup")
                     (modal-back (str base "/add") "Add Provider")
                     [:form.key-form
                      {:hx-post (str base "/add/" (name pid) "/models")
                       :hx-target "#modal"
                       :hx-swap "innerHTML"}
                      [:input
                       {:type "text"
                        :name "base_url"
                        :placeholder "Base URL"
                        :value (or (:base-url preset) "")
                        :autofocus true
                        :autocomplete "off"}] [:button.send-wide {:type "submit"} "Continue"]])

        :oauth
        (oauth-web-view sid pid)

        ;; :api-key
        (modal-shell (str (:label preset) " Setup")
                     (modal-back (str base "/add") "Add Provider")
                     [:form.key-form
                      {:hx-post (str base "/add/" (name pid) "/models")
                       :hx-target "#modal"
                       :hx-swap "innerHTML"}
                      [:input
                       {:type "password"
                        :name "api_key"
                        :placeholder "API key"
                        :autofocus true
                        :autocomplete "off"}] [:button.send-wide {:type "submit"} "Continue"]])))))

(defn- confirm-add-provider!
  "Persist a new provider through the core service (the exact configs
   the TUI writes): OAuth presets get their default models; everyone
   else gets the chosen model (+ the key when one was entered)."
  [sid pid api-key base-url model]
  (when-let [preset0 (preset-by-id pid)]
    (let [preset (cond-> preset0
                   (seq base-url)
                   (assoc :base-url base-url))
          oauth? (= :oauth (vis/provider-auth-kind pid))
          cfg (if oauth?
                (vis/provider-config-with-models preset (vis/provider-default-model-configs preset))
                (cond-> (vis/provider-config-with-models preset [{:name model}])
                  (seq api-key)
                  (assoc :api-key api-key)))]

      (when (or oauth? (seq model)) (vis/add-config-provider! cfg :web-provider-add))))
  (providers-modal sid))

(defn- provider-models-view
  "Per-provider model manager: primary first (= svar's default routing
   root), make-primary / remove per row, live-fetched add list."
  [sid pid]
  (let [base
        (providers-base sid)

        provider
        (configured-provider pid)

        models
        (vec (:models provider))]

    (modal-shell (str (vis/display-label pid) " Models")
                 (modal-back base "Providers")
                 [:div.model-rows
                  (for [[idx m]
                        (map-indexed vector models)

                        :let [nm
                              (:name m)]
                        :when nm]

                    [:div.model-row [:span.model-name nm]
                     (if (zero? (long idx))
                       [:span.model-primary "★ Primary"]
                       [:button.pcard-act
                        {:type "button"
                         :hx-post (str base "/p/" (name pid) "/models/primary")
                         :hx-vals (json-text {:model nm})
                         :hx-target "#modal"
                         :hx-swap "innerHTML"} "make primary"])
                     (when (> (count models) 1)
                       [:button.pcard-act.danger
                        {:type "button"
                         :hx-post (str base "/p/" (name pid) "/models/remove")
                         :hx-vals (json-text {:model nm})
                         :hx-confirm (str "Remove " nm "?")
                         :hx-target "#modal"
                         :hx-swap "innerHTML"} "remove"])])]
                 [:button.add-provider
                  {:type "button"
                   :hx-get (str base "/p/" (name pid) "/models/options")
                   :hx-target "#modal"
                   :hx-swap "innerHTML"} "+ Add model"])))

(defn- provider-model-options-view
  "Addable models for a configured provider (live catalog minus the
   ones already on it)."
  [sid pid show-all?]
  (let [base
        (providers-base sid)

        provider
        (configured-provider pid)

        existing
        (into #{} (keep :name) (:models provider))

        {:keys [models hidden-count]}
        (vis/provider-model-options provider (vis/provider-default-model-names provider) show-all?)

        addable
        (remove existing models)]

    (modal-shell (str (vis/display-label pid) " — Add Model")
                 (modal-back (str base "/p/" (name pid) "/models") "Models")
                 (if (empty? addable)
                   [:p.empty "No further models reported."]
                   [:div.model-chips.pick
                    (for [m addable]
                      [:button.model-chip
                       {:type "button"
                        :hx-post (str base "/p/" (name pid) "/models/add")
                        :hx-vals (json-text {:model m})
                        :hx-target "#modal"
                        :hx-swap "innerHTML"} m])])
                 (when (and (not show-all?) (pos? (long hidden-count)))
                   [:button.show-all
                    {:type "button"
                     :hx-get (str base "/p/" (name pid) "/models/options?show_all=1")
                     :hx-target "#modal"
                     :hx-swap "innerHTML"} (str "Show all models… (" hidden-count " hidden)")]))))

(defn- provider-status-view
  "Status + limits as the RICH canonical markdown form
   (`vis/provider-status-md`) — the same report the TUI paints through
   its IR walker, here rendered as markdown (tables and all)."
  [sid pid]
  (let [provider
        (configured-provider pid)

        md
        (vis/provider-status-md provider)]

    (modal-shell (str (vis/display-label pid) " Status & Limits")
                 (modal-back (providers-base sid) "Providers")
                 [:div.status-md.md {:data-md (str md)} (md->hiccup md)])))

(defn- provider-key-view
  [sid pid]
  (modal-shell (str (vis/display-label pid) " Authentication")
               (modal-back (providers-base sid) "Providers")
               [:form.key-form
                {:hx-post (str (providers-base sid) "/p/" (name pid) "/key")
                 :hx-target "#modal"
                 :hx-swap "innerHTML"}
                [:input
                 {:type "password"
                  :name "api_key"
                  :placeholder "New API key"
                  :autofocus true
                  :autocomplete "off"}] [:button.send-wide {:type "submit"} "Save key"]]))

;; ── Providers: handlers ─────────────────────────────────────────────

(defn- html-ok [body] {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body body})

(defn- with-session
  "Run `(f sid)` for a valid session id, else 404. Every providers
   handler routes through here so URLs stay session-scoped."
  [request f]
  (if-let [sid (some-> (get-in request [:path-params :sid])
                       parse-uuid)]
    (html-ok (f sid))
    {:status 404 :headers {"Content-Type" "text/html"} :body "unknown session"}))

(defn- path-pid
  [request]
  (some-> (get-in request [:path-params :pid])
          keyword))

(defn- turn-trace-handler
  "GET /ui/session/:sid/turn/:tid/trace — the lazily-fetched body of one
   turn's trace disclosure (code/results/tools). Replaces the collapsed
   placeholder via hx-swap=outerHTML, so collapsed trace costs ~0 bytes
   on the initial page until the user expands it."
  [request]
  (let [sid
        (get-in request [:path-params :sid])

        tid
        (get-in request [:path-params :tid])]

    (html-ok
      (html (or
              ;; The completion event can reach Safari before the gateway has made
              ;; the iteration rows visible to lazy trace reads. Wait briefly so the
              ;; first post-run click expands reliably; cap at 3s to match the web
              ;; edge-proxy watchdog budget.
              (let [deadline (+ (System/currentTimeMillis) 3000)]
                (loop []

                  (or (trace-body {"session_id" sid "turn_id" tid})
                      (when (< (System/currentTimeMillis) deadline) (Thread/sleep 100) (recur)))))
              ;; Empty/failed read (e.g. starved by a concurrent live-stream
              ;; write). Keep the lazy trigger so re-opening retries instead
              ;; of permanently swapping in a dead empty body.
              [:div.trace-body
               [:div.block-loading "no trace recorded yet — close and re-open to retry"]])))))

(defn- turns-older-handler
  "GET /ui/session/:sid/turns?before=<turn-id> — one page of OLDER turns for
   infinite scroll-up. Returns a fresh `.load-older` sentinel (when even older
   turns remain) above the batch, oldest→newest, so the outerHTML swap keeps
   chronological order and the sentinel back at the top."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        before
        (some->> (:query-string request)
                 (re-find #"(?:^|&)before=([0-9a-fA-F-]+)")
                 second)]

    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "text/html; charset=utf-8"} :body ""}
      (let [turns-all
            (vec (vis/gateway-list-turns sid))

            idx
            (or (some (fn [[i t]]
                        (when (= before
                                 (some-> (get t "turn_id")
                                         str))
                          i))
                      (map-indexed vector turns-all))
                (count turns-all))

            start
            (long (max 0 (- (long idx) OLDER_TURN_PAGE)))

            batch
            (subvec turns-all start idx)

            more?
            (pos? start)

            new-oldest
            (some-> (first batch)
                    (get "turn_id")
                    str)]

        (html-ok (html (list (when more? (older-sentinel sid new-oldest))
                             (map turn-block batch))))))))

(defn- session-providers-handler
  "GET /ui/session/:sid/providers — the providers dialog."
  [request]
  (with-session request providers-modal))

(defn- set-provider-handler
  "POST /ui/session/:sid/provider {provider model} — set/clear this session's
   PROVIDER + MODEL preference (blank model = router default). Re-renders the
   picker BODY in place (`#model-pick`, the chip's hx-target) so the modal
   STAYS OPEN with the new active model + highlight, and out-of-band refreshes
   the rail's `#routewrap` routing. Swapping only the body (not the modal
   shell) keeps the overlay from re-running its entry animation — that
   re-animation, not the content, was the 'whole picker jumps' the
   close-on-select hid."
  [request]
  (with-session request
                (fn [sid]
                  (vis/gateway-set-session-model! sid
                                                  (get-in request [:form-params "provider"])
                                                  (get-in request [:form-params "model"]))
                  (str (html (model-pick-body sid))
                       ;; Routing lives in the footer dock now — the #footwrap OOB swap
                       ;; re-renders the provider/model chip after a model change.
                       (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"}
                              (footer-content sid)])))))

(defn- session-model-handler
  "GET /ui/session/:sid/model — open the per-session model picker."
  [request]
  (with-session request session-model-picker))

(defn- set-reasoning-handler
  "POST /ui/session/:sid/reasoning {level} — set the global reasoning-effort
   toggle (`:vis/reasoning-level`) for this install. Shared with the TUI/Telegram
   footers, so a change here shows up there instantly. Re-renders the picker BODY
   in place (`#reasoning-pick`) so the modal stays open with the new highlight,
   and OOB-refreshes `#footwrap` so the effort chip's label updates — the same
   dual-swap `set-provider-handler` uses for the model chip."
  [request]
  (with-session
    request
    (fn [sid]
      (when-let [raw (not-empty (get-in request [:form-params "level"]))]
        (let [kw (keyword (str/trim raw))]
          ;; Only accept the registered enum values — guards against
          ;; a hand-crafted POST. Unknown levels are a silent no-op
          ;; (the picker re-renders unchanged), never a throw.
          (when (contains? (set reasoning-level-order) kw)
            (try (vis/toggle-set-value! :vis/reasoning-level kw) (catch Throwable _ nil)))))
      (str (html (reasoning-pick-body sid))
           ;; The effort chip lives in the footer dock — OOB-swap it
           ;; so its `effort: <level>` label tracks the new value.
           (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"} (footer-content sid)])))))

(defn- session-reasoning-handler
  "GET /ui/session/:sid/reasoning — open the reasoning-effort picker."
  [request]
  (with-session request session-reasoning-picker))

(defn- provider-diag-handler
  "GET .../providers/:pid/diag — one card with auth + limits computed
   (the slow probes); swapped over the skeleton card."
  [request]
  (with-session request
                (fn [sid]
                  (let [pid
                        (path-pid request)

                        providers
                        (vis/configured-providers)

                        idx
                        (or (some (fn [[i p]]
                                    (when (= pid (:id p)) i))
                                  (map-indexed vector providers))
                            0)

                        provider
                        (configured-provider pid)]

                    (if-not provider
                      (providers-modal sid)
                      (html (provider-card sid
                                           provider
                                           idx
                                           (count providers)
                                           {:status (vis/provider-status provider)
                                            :limits (vis/provider-limits-safe provider)})))))))

(defn- provider-add-picker-handler [request] (with-session request add-provider-picker))

(defn- provider-add-oauth-handler
  [request]
  (with-session request #(oauth-web-view % (path-pid request))))

(defn- provider-add-oauth-start-handler
  [request]
  (with-session request
                (fn [sid]
                  (let [pid (path-pid request)]
                    (start-oauth-web-flow! sid pid)
                    (Thread/sleep 250)
                    (oauth-web-view sid pid)))))

(defn- provider-add-oauth-finish-handler
  [request]
  (with-session request
                (fn [sid]
                  (let [pid
                        (path-pid request)

                        input
                        (str/trim (str (get-in request [:form-params "redirect"])))

                        flow
                        (get @oauth-web-flows pid)]

                    (when (and flow (seq input)) (deliver (:input flow) input) (Thread/sleep 500))
                    (oauth-web-view sid pid)))))

(defn- provider-add-step-handler
  [request]
  (with-session request #(add-provider-step % (path-pid request))))

(defn- provider-add-models-handler
  "POST .../providers/add/:pid/models {api_key show_all} — the model
   picker for a preset being added."
  [request]
  (with-session request
                (fn [sid]
                  (let [pid
                        (path-pid request)

                        api-key
                        (get-in request [:form-params "api_key"])

                        base-url
                        (not-empty (get-in request [:form-params "base_url"]))

                        show-all?
                        (= "1" (get-in request [:form-params "show_all"]))]

                    (if-let [preset (some-> (preset-by-id pid)
                                            (cond->
                                              base-url
                                              (assoc :base-url base-url)))]
                      (add-model-picker sid preset (not-empty api-key) show-all?)
                      (providers-modal sid))))))

(defn- provider-add-confirm-handler
  [request]
  (with-session request
                (fn [sid]
                  (confirm-add-provider! sid
                                         (path-pid request)
                                         (not-empty (get-in request [:form-params "api_key"]))
                                         (not-empty (get-in request [:form-params "base_url"]))
                                         (get-in request [:form-params "model"])))))

(defn- provider-remove-handler
  [request]
  (with-session request
                (fn [sid]
                  (vis/remove-config-provider! (path-pid request) :web-provider-remove)
                  (try (vis/reload-config!) (catch Throwable _ nil))
                  (providers-modal sid))))

(defn- provider-models-handler
  [request]
  (with-session request #(provider-models-view % (path-pid request))))

(defn- provider-model-options-handler
  [request]
  (with-session request
                #(provider-model-options-view %
                                              (path-pid request)
                                              (= "1" (get-in request [:query-params "show_all"])))))

(defn- move-model-first
  [models nm]
  (let [models
        (vec models)

        hit
        (some #(when (= nm (:name %)) %) models)]

    (if hit (into [hit] (remove #(= nm (:name %)) models)) models)))

(defn- provider-models-mutate-handler
  "POST .../:pid/models/primary|remove|add {model} — persist through
   the core service, re-render the relevant view."
  [request]
  (with-session
    request
    (fn [sid]
      (let [pid
            (path-pid request)

            nm
            (get-in request [:form-params "model"])

            uri
            (str (:uri request))

            op
            (cond (str/ends-with? uri "/primary") :primary
                  (str/ends-with? uri "/remove") :remove
                  :else :add)]

        (when (seq nm)
          (vis/update-config-provider! pid
                                       (fn [provider]
                                         (update provider
                                                 :models
                                                 (fn [models]
                                                   (case op
                                                     :primary
                                                     (move-model-first models nm)

                                                     :remove
                                                     (vec (remove #(= nm (:name %)) models))

                                                     :add
                                                     (if (some #(= nm (:name %)) models)
                                                       (vec models)
                                                       (conj (vec models) {:name nm}))))))
                                       :web-provider-models))
        (provider-models-view sid pid)))))

(defn- reorder-providers-by-ids
  "Reorder `providers` to match the string id sequence `ids`. Ids not present
   are ignored; providers missing from `ids` keep their relative order and are
   appended after the named ones (defensive against a stale drag payload)."
  [providers ids]
  (let [by-id
        (into {}
              (map (fn [p]
                     [(name (:id p)) p]))
              providers)

        named
        (vec (keep by-id ids))

        named-set
        (set (map :id named))]

    (into named (remove #(named-set (:id %))) providers)))

(defn- provider-reorder-list-handler
  "POST .../providers/reorder — persist the fleet order and re-render the
   Providers modal. Mobile-friendly arrow buttons send `pid` + `dir`; the older
   `order` payload is still accepted defensively."
  [request]
  (with-session
    request
    (fn [sid]
      (let [providers
            (vis/configured-providers)

            order
            (->> (str/split (str (get-in request [:form-params "order"])) #",")
                 (remove str/blank?)
                 vec)

            pid
            (get-in request [:form-params "pid"])

            dir
            (get-in request [:form-params "dir"])

            reordered
            (if (seq order)
              (reorder-providers-by-ids providers order)
              (let [v
                    (vec providers)

                    idx
                    (some (fn [[i p]]
                            (when (= pid (name (:id p))) i))
                          (map-indexed vector v))

                    j
                    (case dir
                      "up"
                      (when idx (dec (long idx)))

                      "down"
                      (when idx (inc (long idx)))

                      nil)]

                (if (and idx j (<= 0 (long j)) (< (long j) (count v)))
                  (assoc v
                    idx (v j)
                    j (v idx))
                  providers)))]

        (when (not= reordered providers)
          (vis/save-config-providers! reordered :web-provider-reorder))
        (html (provider-cards-view sid reordered))))))

(defn- provider-status-handler
  [request]
  (with-session request #(provider-status-view % (path-pid request))))

(defn- provider-key-form-handler
  [request]
  (with-session request #(provider-key-view % (path-pid request))))

(defn- provider-key-save-handler
  [request]
  (with-session
    request
    (fn [sid]
      (let [pid
            (path-pid request)

            api-key
            (not-empty (get-in request [:form-params "api_key"]))]

        (when api-key
          (vis/update-config-provider! pid #(assoc % :api-key api-key) :web-provider-key))
        (providers-modal sid)))))

;; =============================================================================
;; CSS - the whole theme, one file, no inline styles.
;; vis-light tokens (internal/theme.clj -> theme->web-css-vars):
;;   --bg :terminal-bg | --fg :text-fg | --panel2 / --surface :dialog-bg
;;   --code-bg :code-block-bg
;;   --primary / --accent :header-active-tab-bg / -accent | --primary-fg :header-active-tab-fg
;;   --secondary :code-result-fg | --warning :warning-fg | --warning-bg :warning-bg
;;   --ok :status-ok | --err :status-bad
;; =============================================================================

(def ^:private app-css
  "The stylesheet, vendored as a REAL file (resources/vis-channel-web/public/
   app.css) and read from the classpath ONCE — consistent with the JS assets
   (`JS_ASSETS`), instead of a ~480-line string literal jammed into this ns.
   Edit it with real CSS tooling; served at /ui/app.css by `css-handler`."
  (delay (some-> (io/resource "vis-channel-web/public/app.css")
                 slurp)))

(vis/register-toggle! {:id :vis-channel-web/theme
                       :label "Web theme"
                       :description (str
                                      "Theme for the web companion UI - picked from the SAME shared"
                                      " registry the TUI paints from (internal/theme.clj), so every"
                                      " registered theme works in both places.")
                       :type :enum
                       :choices (mapv keyword (vis/available-theme-ids))
                       :default (keyword vis/default-theme-id)
                       ;; Web-only control: scope it to the web Settings so it never leaks into
                       ;; the TUI's dialog (and the TUI's own theme picker stays the TUI's).
                       :channels #{:web}
                       :owner :vis
                       :group :channels
                       :persist? true})

(defn- css-handler
  "Serves app.css with a theme-driven `:root` override APPENDED: the static
   stylesheet stays the layout/base (vis-light defaults baked in), and the
   override re-binds every shared color var (`vis/web-css-root`) to the
   selected `:vis-channel-web/theme` - the same registry the TUI uses."
  [_]
  {:status 200
   :headers {"Content-Type" "text/css; charset=utf-8" "Cache-Control" "no-cache"}
   :body (str (or @app-css "")
              "
/* theme override - generated from the shared theme registry */
"
              (current-web-css-root))})

;; =============================================================================
;; Standalone HTML export — the SAME chat view /ui paints, self-contained.
;; Reuses `user-bubble` / `trace-body` / `vis-bubble` so the exported thread is
;; byte-identical to the live UI; app.css + theme + the vendored
;; marked/DOMPurify/Prism scripts are INLINED so the file renders + highlights
;; offline. A session-summary card sits on top; no SSE/composer/sidebar chrome.
;; =============================================================================

(defn- inline-js
  "Slurp a vendored /public JS asset for inlining. nil (skipped) when absent."
  [name]
  (some-> (io/resource (str "vis-channel-web/public/" name))
          slurp))

(def ^:private export-render-js
  "Static-export render, distilled from ui.js: render every `[data-md]` through
   marked -> DOMPurify -> innerHTML, colorize `language-diff` fences into df-*
   spans (the vendored Prism has NO diff grammar, so this is what paints the
   green/red add/del backgrounds — same classifier as render.clj
   `diff-line-kind`), then Prism-highlight every remaining `language-*` block.
   No htmx/SSE — a standalone file has no server to talk to."
  (str
    "(function(){"
    "function diffLineClass(line){"
    "if(line.indexOf('+++')===0||line.indexOf('---')===0){return 'df-meta';}"
    "if(line.indexOf('@@')===0){return 'df-hunk';}"
    "if(line.length&&line.charAt(0)==='+'){return 'df-add';}"
    "if(line.length&&line.charAt(0)==='-'){return 'df-del';}" "return 'df-ctx';}"
    "function colorizeDiff(el){"
    "var pre=el.closest&&el.closest('pre');if(pre){pre.classList.add('ir-diff');}"
    "var lines=el.textContent.split('\\n');var frag=document.createDocumentFragment();"
    "lines.forEach(function(line){var span=document.createElement('span');"
    "span.className=diffLineClass(line);span.textContent=line===''?' ':line;"
    "frag.appendChild(span);});el.textContent='';el.appendChild(frag);}"
    "function render(){" "if(typeof marked!=='undefined'){"
    "document.querySelectorAll('[data-md]:not([data-md-done])').forEach(function(el){"
    "try{var raw=el.getAttribute('data-md')||'';"
    "var out=marked.parse?marked.parse(raw):marked(raw);"
    "if(typeof DOMPurify!=='undefined')out=DOMPurify.sanitize(out);"
    "el.innerHTML=out;el.setAttribute('data-md-done','1');}catch(e){}});}"
    "document.querySelectorAll('code.language-diff:not([data-hl-done])').forEach(function(el){"
    "el.setAttribute('data-hl-done','1');try{colorizeDiff(el);}catch(e){}});"
    "if(typeof Prism!=='undefined'&&Prism.highlightElement){"
    "document.querySelectorAll('code[class*=\"language-\"]:not([data-hl-done])').forEach(function(el){"
    "try{Prism.highlightElement(el);el.setAttribute('data-hl-done','1');}catch(e){}});}}"
    "if(document.readyState==='loading'){document.addEventListener('DOMContentLoaded',render);}else{render();}"
    "})();"))

(def ^:private export-css
  "Layout override for the standalone export: the live UI centers the thread via
   the `.app/.layout/.center` grid (which carries the sidebars we DON'T export),
   so re-center `.thread` here and style the summary card. Colors/bubbles/op-cards
   all come from the inlined app.css + theme."
  (str "body.export{margin:0}"
       ".export .thread{max-width:860px;margin:0 auto;padding:1rem 1.25rem 4rem}"
       ".export .column{width:100%}"
       ;; Session-summary breaks out of the 860px thread to use the FULL
       ;; viewport (capped + centered) as a responsive grid of stat cards.
       ".export-summary{position:relative;left:50%;transform:translateX(-50%);"
       "width:min(1360px,94vw);margin:1.75rem 0 1.25rem}"
       ".export-summary-grid{display:grid;gap:.9rem;align-items:start;"
       "grid-template-columns:repeat(auto-fit,minmax(16rem,1fr))}"
       ".esum-card{border:1px solid var(--line);border-radius:10px;background:var(--code-bg);"
       "padding:.4rem 1rem .9rem}"
       ".esum-card-title{color:var(--dim);font-family:ui-monospace,SFMono-Regular,Menlo,monospace;"
       "font-size:.72rem;text-transform:uppercase;letter-spacing:.08em;padding:.7rem 0 .1rem}"
       ".esum-row{display:flex;justify-content:space-between;gap:1rem;align-items:baseline;"
       "padding:.4rem 0;border-top:1px solid var(--line)}"
       ".esum-card-title + .esum-row{border-top:0}"
       ".esum-k{color:var(--dim);white-space:nowrap;flex:0 0 auto}"
       ".esum-v{color:var(--fg);text-align:right;word-break:break-word;min-width:0}"
       ".esum-v.esum-mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.88em}"
       "@media (max-width:640px){.export-summary{width:100%;left:0;transform:none}"
       ".esum-card{padding:.35rem .85rem .8rem}}"))

(defn- export-summary-card
  "Session summary shown ABOVE the transcript - the CANONICAL grouped
   `transcript/session-summary` rows (Session / Timing / Activity / Providers &
   models / Cost & tokens). Each group is its own stat card in a full-viewport
   responsive grid, the SAME data every summary surface emits. One source of truth."
  [_sid _title data]
  [:section.export-summary
   [:div.export-summary-grid
    (for [[label rows] (transcript/session-summary data)]
      [:div.esum-card [:div.esum-card-title label]
       (for [[k v mono?] rows]
         [:div.esum-row [:span.esum-k k]
          [:span {:class (str "esum-v" (when mono? " esum-mono"))} (str v)]])])]])

(defn- export-turn-static
  "Turn rendered for a STATIC export — identical to `turn-block`, but the trace
   op-cards render INLINE (`trace-body`) instead of the lazy hx-get placeholder,
   since a standalone file can't fetch them."
  [turn]
  (list [:div.tsep]
        (user-bubble (get turn "request") (get turn "started_at") nil)
        (trace-body turn)
        (when (or (seq (get turn "content")) (get turn "error")) (vis-bubble turn))))

(defn export-session-html
  "Standalone, self-contained HTML export of `sid` — the SAME chat view the web
   /ui renders (user/vis bubbles + inline op-card trace), styled with the inlined
   app.css + theme + the vendored marked/DOMPurify/Prism scripts, with a session-
   summary card on top. Returns the HTML string; a 'Session not found' note (no
   throw) on a bad id so pipelines stay clean."
  [sid]
  (let [data
        (try (transcript/transcript (vis/db-info) sid) (catch Throwable _ nil))

        soul
        (try (vis/gateway-soul sid) (catch Throwable _ nil))]

    (if-not (or data soul)
      (str "Session not found: " sid "\n")
      (let [turns
            (remove #(= "queued" (get % "status"))
              (vec (try (vis/gateway-list-turns sid) (catch Throwable _ []))))

            title
            (or (get soul "title") (get-in data [:session :title]) "vis session")

            css
            (str (or @app-css "") "\n" (current-web-css-root) "\n" export-css)

            js
            (str/join "\n" (keep inline-js ["marked.min.js" "purify.min.js" "prism.min.js"]))]

        (str "<!doctype html>\n"
             (html [:html {:lang "en"}
                    [:head [:meta {:charset "utf-8"}]
                     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                     [:title (str title " — transcript")] [:style (h/raw css)]]
                    [:body.export
                     [:main.thread
                      [:div.column (export-summary-card sid title data)
                       (map export-turn-static turns)]] [:script (h/raw js)]
                     [:script (h/raw export-render-js)]]]))))))

;; =============================================================================
;; Route contribution (whiteboard slot) + channel registration
;; =============================================================================

(defn- dir-enc
  "URL-encode a path for a `?path=` query param."
  [s]
  (java.net.URLEncoder/encode (str s) "UTF-8"))

(defn- fs-picker-base
  "Directory the picker should browse: the `path` query param, else the
   session's primary workspace root, else the user's home directory."
  [sid path]
  (or (some-> path
              str/trim
              not-empty)
      (try (some-> (vis/workspace-root (vis/env-for sid))
                   str
                   not-empty)
           (catch Throwable _ nil))
      (System/getProperty "user.home")))

(defn- dir-crumbs
  "Clickable breadcrumb from filesystem root to `canon`. The first hop is a
   home glyph (root `/`); each later hop re-browses that ancestor as a
   fragment swap (no modal repaint). The trailing segment is the folder
   you are in (bold, not a link)."
  [sid canon]
  (let [segs
        (->> (str/split canon #"/")
             (remove str/blank?)
             vec)

        paths
        (rest (reductions (fn [acc s]
                            (str acc "/" s))
                          ""
                          segs))

        hop
        (fn [p]
          {:hx-get (str "/ui/session/" sid "/fs-picker?frag=1&path=" (dir-enc p))
           :hx-target "#dir-browser"
           :hx-swap "outerHTML"})

        last-i
        (dec (count segs))]

    [:nav.dir-crumbs {:aria-label "Current location"}
     [:button.dir-crumb.dir-crumb-home
      (merge {:type "button" :aria-label "Filesystem root"} (hop "/")) (icon "home")]
     (map-indexed (fn [i [seg p]]
                    (if (= i last-i)
                      [:span.dir-crumb.dir-crumb-here seg]
                      [:button.dir-crumb (merge {:type "button"} (hop p)) seg]))
                  (map vector segs paths))]))

(defn- dir-browser
  "Inner, swap-on-navigate part of the picker. Lives in `#dir-browser`;
   every navigation row / breadcrumb hop targets `#dir-browser` with
   `outerHTML`, so moving around folders swaps ONLY this block - the modal
   frame never repaints (that whole-modal repaint was the flicker).

   Mirrors the TUI picker: this session's FILESYSTEM ROOTS ride at the top as a
   titled list (each mark + abbreviated path + a workspace/added tag, added
   ones removable in place), then the breadcrumb, then the subfolders - each
   carrying a membership mark so you read what's in the session BEFORE you
   act. Add/remove re-render this block in place and OOB-refresh the rail,
   so the picker stays open."
  [sid dir & {:keys [err notice]}]
  (let [canon
        (str (vis/workspace-normalize-root dir))

        fname
        (let [n (.getName (io/file canon))]
          (if (str/blank? n) "/" n))

        kids
        (try (vis/workspace-subdirs canon) (catch Throwable _ []))

        home
        (System/getProperty "user.home")

        ;; Session roots: the workspace root is the implicit base; extras are the
        ;; listed filesystem-roots. Compare on the SAME canonical form so a folder's
        ;; membership mark (and "already added") never lies.
        wi
        (try (vis/gateway-session-workspace sid) (catch Throwable _ nil))

        norm
        (fn [p]
          (some-> p
                  str
                  not-empty
                  (#(try (str (vis/workspace-normalize-root %)) (catch Throwable _ %)))))

        base
        (norm (get wi "root"))

        extras
        (get wi "filesystem_roots")

        extra
        (set (keep #(norm (get % "trunk")) extras))

        path-within?
        (fn [root p]
          (boolean
            (and root p (or (= p root) (str/starts-with? p (str root java.io.File/separator))))))

        covered-by
        (fn [p]
          (some #(when (path-within? % p) %) (cons base extra)))

        root-of?
        (fn [p]
          (boolean (covered-by (norm p))))

        workspace?
        (= canon base)

        already?
        (contains? extra canon)

        covered-root
        (covered-by canon)

        covered-inside?
        (and covered-root (not workspace?) (not already?))

        roots-total
        (+ (if base 1 0) (count extras))

        nav
        (fn [path]
          {:hx-get (str "/ui/session/" sid "/fs-picker?frag=1&path=" (dir-enc path))
           :hx-target "#dir-browser"
           :hx-swap "outerHTML"})]

    [:div#dir-browser.dir-browser
     ;; FILESYSTEM ROOTS - the same session-scoped list the rail shows, promoted
     ;; INTO the picker (like the TUI dialog) so you read membership as you
     ;; browse. Each root carries a filled mark; the workspace base can't be
     ;; removed, added ones carry an x that drops them and re-renders in place.
     [:div.dir-zone.dir-roots-zone [:span.dir-zone-label (str "Filesystem \u00b7 " roots-total)]
      [:ul.dir-roots
       (when base
         [:li.dir-root [:span.dir-root-mark (icon "check")]
          [:span.ctx-mono.fs-root-path (abbrev-home base)] [:span.fs-root-tag "workspace"]])
       (for [{:strs [trunk]} extras]
         [:li.dir-root [:span.dir-root-mark (icon "check")]
          [:span.ctx-mono.fs-root-path (abbrev-home trunk)] [:span.fs-root-tag "added"]
          [:button.dir-root-remove
           {:type "button"
            :hx-post (str "/ui/session/" sid "/fs-remove?frag=1&at=" (dir-enc canon))
            :hx-vals (json-text {:path (str trunk)})
            :hx-target "#dir-browser"
            :hx-swap "outerHTML"
            :aria-label (str "Remove " trunk)} (icon "x")]])
       (when (empty? extras) [:li.dir-root-empty "Open a folder below and add it as a root."])]]
     ;; WHERE YOU ARE - the breadcrumb goes UP (tap any ancestor) and Home jumps
     ;; out of the project, so an added root can be ANY folder, inside or out.
     [:div.dir-zone
      [:div.dir-zone-head [:span.dir-zone-label "Browse"]
       (when home
         [:button.dir-jump (merge {:type "button" :title (str "Go to " home)} (nav home))
          (icon "home") [:span "Home"]])] (dir-crumbs sid canon)]
     ;; SUBFOLDERS YOU CAN OPEN - each carries a filled/hollow membership mark.
     [:div.dir-zone [:span.dir-zone-label (str "Folders inside " fname " - tap to open")]
      (if (seq kids)
        [:ul.dir-list
         (for [k kids]
           (let [child (str canon "/" k)
                 r? (root-of? child)]

             [:li
              [:button.dir-row (merge {:type "button"} (nav child))
               [:span.dir-row-mark {:class (if r? "is-root" "is-add")}
                (icon (if r? "check" "plus"))] (icon "folder") [:span.dir-row-name k]
               (icon "chevron-right")]]))]
        [:p.dir-empty "This folder has no subfolders."])]
     (when err [:p.dir-err (icon "info") [:span (str err)]])
     (when notice [:p.dir-notice (icon "check") [:span (str notice)]])
     ;; ACTIONS
     [:form.fs-create
      {:hx-post (str "/ui/session/" sid "/fs-create")
       :hx-target "#dir-browser"
       :hx-swap "outerHTML"} [:input {:type "hidden" :name "parent" :value canon}]
      [:input.fs-create-input
       {:type "text"
        :name "name"
        :autocomplete "off"
        :placeholder (str "Name a new folder in " fname)}]
      [:button.fs-create-btn {:type "submit"} (icon "folder-plus") [:span "Create"]]]
     (let [set-root-form
           ;; Promote the browsed folder to the session's PRIMARY root —
           ;; shell cwd, file tools, and search retarget from the next turn.
           ;; Offered everywhere except when it already IS the root.
           [:form.fs-root-form
            {:hx-post (str "/ui/session/" sid "/fs-root?frag=1")
             :hx-target "#dir-browser"
             :hx-swap "outerHTML"} [:input {:type "hidden" :name "path" :value canon}]
            [:button.fs-root-btn {:type "submit"} (icon "home")
             [:span "Make this the session's root"]]]]
       (cond
         ;; the session's current root — nothing to add or change here
         workspace? [:div.fs-add-form.dir-already (icon "check")
                     [:span.fs-add-text [:span.fs-add-main "This is the session's root"]
                      [:span.fs-add-sub (abbrev-home canon)]]]
         ;; already an ADDITIONAL filesystem root — can still be promoted
         already? [:div.fs-actions
                   [:div.fs-add-form.dir-already (icon "check")
                    [:span.fs-add-text
                     [:span.fs-add-main "Already a filesystem root for this session"]
                     [:span.fs-add-sub (abbrev-home canon)]]] set-root-form]
         ;; a parent directory already grants access — adding an inner dir would be redundant
         covered-inside? [:div.fs-actions
                          [:div.fs-add-form.dir-already (icon "check")
                           [:span.fs-add-text
                            [:span.fs-add-main "Already covered by an allowed directory"]
                            [:span.fs-add-sub (abbrev-home covered-root)]]] set-root-form]
         :else [:div.fs-actions
                [:form.fs-add-form
                 {:hx-post (str "/ui/session/" sid "/fs-add?frag=1")
                  :hx-target "#dir-browser"
                  :hx-swap "outerHTML"} [:input {:type "hidden" :name "path" :value canon}]
                 [:button.fs-add-btn {:type "submit"} (icon "check")
                  [:span "Add permission for this folder"]]] set-root-form]))]))

(defn- fs-picker-modal
  "Filesystem picker overlay rooted at `dir`. SCOPED to this session —
   it manages only this session's filesystem permissions: add/remove
   additional roots, or make a folder the session's ROOT. The shell is
   static; only `dir-browser` swaps as you navigate."
  [sid dir]
  (modal-shell "Filesystem Permissions"
               [:p.dir-intro (icon "info")
                [:span "Folders vis can read and edit, " [:strong "this session only"]
                 ". Add extra roots or set the session root — here or anywhere on your computer."]]
               (dir-browser sid dir)))

(defn- fs-picker-handler
  "GET /ui/session/:sid/fs-picker - the filesystem picker. `?path=` selects
   the directory to browse (defaults to the workspace root). `?frag=1`
   returns ONLY the `#dir-browser` fragment (navigation, no modal repaint)."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        path
        (get-in request [:query-params "path"])

        frag
        (= "1" (get-in request [:query-params "frag"]))

        dir
        (fs-picker-base sid path)]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if frag (html (dir-browser sid dir)) (fs-picker-modal sid dir))}))

(defn- fs-create-handler
  "POST /ui/session/:sid/fs-create - make a new folder under `parent` and
   re-open the picker INSIDE it. A bad name re-renders with an inline error."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        parent
        (str (get-in request [:form-params "parent"]))

        nm
        (str (get-in request [:form-params "name"]))]

    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (html (try (dir-browser sid (str (vis/workspace-create-dir! parent nm)))
                      (catch Throwable e
                        (dir-browser sid parent :err (or (ex-message e) (str e))))))}))

(defn- fs-add-handler
  "POST /ui/session/:sid/fs-add - widen THIS session to also work under
   `path`, close the modal, and OOB-refresh the rail/footer immediately. This
   mirrors the `/fs add` slash through the picker UI."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        path
        (str/trim (str (get-in request [:form-params "path"])))

        dir
        (fs-picker-base sid path)

        frag
        (= "1" (get-in request [:query-params "frag"]))]

    (try
      (let [wi
            (vis/gateway-session-workspace sid)

            workspace-id
            (:id wi)]

        (cond (str/blank? path) (throw (ex-info "Choose a directory to add." {}))
              (nil? workspace-id) (throw (ex-info "No active workspace for this session yet." {}))
              :else (vis/gateway-add-filesystem-root! sid path))
        (if frag
          ;; From the picker: keep it OPEN, re-render the browser in place
          ;; (the added folder now shows as a root), OOB-refresh rail + footer.
          {:status 200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body (str (html (dir-browser sid path :notice "Added this folder to the session"))
                      (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"} (footer-content sid)]))}
          {:status 200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body (str (oob-modal "")
                      (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"}
                             (footer-content sid)]))}))
      (catch Throwable e
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (html (dir-browser sid dir :err (or (ex-message e) (str e))))}))))

(defn- fs-remove-handler
  "POST /ui/session/:sid/fs-remove {path} - narrow THIS session, dropping
   `path` from its filesystem roots, then OOB-refresh the rail/footer immediately."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        path
        (str/trim (str (get-in request [:form-params "path"])))

        frag
        (= "1" (get-in request [:query-params "frag"]))

        at
        (get-in request [:query-params "at"])

        wi
        (vis/gateway-session-workspace sid)

        wid
        (:id wi)]

    (when (and (seq path) wid) (vis/gateway-remove-filesystem-root! sid path))
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if frag
             ;; Removed from INSIDE the picker: re-render the browser at the
             ;; folder being viewed, plus OOB-refresh the rail + footer.
             (str (html (dir-browser sid
                                     (fs-picker-base sid at)
                                     :notice
                                     "Removed this folder from the session"))
                  (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"} (footer-content sid)]))
             (str (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"} (footer-content sid)])))}))

(defn- fs-root-handler
  "POST /ui/session/:sid/fs-root {path} - change THIS session's PRIMARY
   filesystem root: the session then works in `path` (shell cwd, file tools,
   and search all follow from the next turn; additional roots carry over).
   Refuses while the session is in a draft — apply/abandon it first. Renders
   the picker back in place with the outcome, plus OOB rail/footer refresh."
  [request]
  (let [sid
        (some-> (get-in request [:path-params :sid])
                parse-uuid)

        path
        (str/trim (str (get-in request [:form-params "path"])))

        wi
        (vis/gateway-session-workspace sid)

        state-id
        (:id wi)]

    (try (when (str/blank? path) (throw (ex-info "No directory given." {})))
         (when-not state-id (throw (ex-info "Session not ready yet — send a message first." {})))
         (vis/gateway-change-root! sid path)
         {:status 200
          :headers {"Content-Type" "text/html; charset=utf-8"}
          :body (str (html (dir-browser sid path :notice "This folder is now the session's root"))
                     (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"} (footer-content sid)]))}
         (catch Throwable e
           {:status 200
            :headers {"Content-Type" "text/html; charset=utf-8"}
            :body (html (dir-browser sid path :err (or (ex-message e) (str e))))}))))

(defn- ui-not-found-handler
  "The contribution's `:on-not-found`: any unmatched `/ui/...` address renders a
   styled HTML 404 instead of the gateway's raw JSON `no such route` (which a
   browser shouldn't see). Wired through the gateway default-handler, NOT a
   reitit route — a `/ui/*` catch-all route conflicts with the `:sid` routes and
   reitit refuses to build the router. The gateway's JSON 404 still owns non-/ui
   paths (the API namespace)."
  [_request]
  {:status 404 :headers {"Content-Type" "text/html; charset=utf-8"} :body (not-found-page {})})

(defn- ui-routes
  "Reitit route data for the contribution; closes over the gateway token
   so /ui and /ui/auth can run the cookie exchange. Handlers go in as
   VARS so a REPL :reload serves new code on the very next request."
  [token]
  [["/ui" {:get #(index-handler % token)}] ["/ui/auth" {:post #(auth-handler % token)}]
   ["/ui/app.css" {:get #'css-handler}] ["/ui/icons.svg" {:get #'icons-handler}]
   ["/ui/js/:asset" {:get #'js-asset-handler}] ["/ui/fonts/:asset" {:get #'font-asset-handler}]
   ["/ui/settings" {:get #'settings-handler}]
   ["/ui/settings/toggle" {:post #'settings-mutate-handler}]
   ["/ui/settings/cycle" {:post #'settings-mutate-handler}]
   ["/ui/session/:sid/providers" {:get #'session-providers-handler}]
   ["/ui/session/:sid/model" {:get #'session-model-handler}]
   ["/ui/session/:sid/reasoning" {:get #'session-reasoning-handler :post #'set-reasoning-handler}]
   ["/ui/session/:sid/resources" {:get #'resources-modal-handler}]
   ["/ui/session/:sid/resources/stop" {:post #'resource-stop-handler}]
   ["/ui/session/:sid/resources/restart" {:post #'resource-restart-handler}]
   ["/ui/session/:sid/resources/logs" {:post #'resource-logs-handler}]
   ["/ui/session/:sid/resources/logs/lines" {:post #'resource-logs-lines-handler}]
   ["/ui/session/:sid/resources/start" {:post #'resource-start-handler}]
   ["/ui/session/:sid/backgrounds/add" {:get #'backgrounds-add-handler}]
   ["/ui/session/:sid/provider" {:post #'set-provider-handler}]
   ["/ui/session/:sid/providers/add" {:get #'provider-add-picker-handler}]
   ["/ui/session/:sid/providers/add/:pid" {:get #'provider-add-step-handler}]
   ["/ui/session/:sid/providers/add/:pid/auth" {:get #'provider-add-oauth-handler}]
   ["/ui/session/:sid/providers/add/:pid/auth/start" {:post #'provider-add-oauth-start-handler}]
   ["/ui/session/:sid/providers/add/:pid/auth/finish" {:post #'provider-add-oauth-finish-handler}]
   ["/ui/session/:sid/providers/add/:pid/models" {:post #'provider-add-models-handler}]
   ["/ui/session/:sid/providers/add/:pid/confirm" {:post #'provider-add-confirm-handler}]
   ["/ui/session/:sid/providers/p/:pid/diag" {:get #'provider-diag-handler}]
   ["/ui/session/:sid/providers/p/:pid/remove" {:post #'provider-remove-handler}]
   ["/ui/session/:sid/providers/p/:pid/models" {:get #'provider-models-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/options" {:get #'provider-model-options-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/primary" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/remove" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/add" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/reorder" {:post #'provider-reorder-list-handler}]
   ["/ui/session/:sid/providers/p/:pid/status" {:get #'provider-status-handler}]
   ["/ui/session/:sid/providers/p/:pid/key"
    {:get #'provider-key-form-handler :post #'provider-key-save-handler}]
   ["/ui/sessions" {:post #'create-session-handler}]
   ["/ui/sessions/switch" {:get #'sessions-switch-handler}]
   ["/ui/sessions/list" {:get #'sessions-list-handler}]
   ["/ui/sessions/delete"
    {:get #'delete-sessions-confirm-handler :post #'delete-sessions-bulk-handler}]
   ["/ui/projects" {:post #'create-project-handler}]
   ["/ui/projects/new" {:get #'new-project-modal-handler}]
   ["/ui/project/:pid" {:get #'manage-project-handler}]
   ["/ui/project/:pid/rename" {:post #'rename-project-handler}]
   ["/ui/project/:pid/delete" {:post #'delete-project-handler}]
   ["/ui/project/:pid/session" {:post #'create-project-session-handler}]
   ["/ui/session/:sid/move" {:get #'move-session-modal-handler :post #'assign-session-handler}]
   ["/ui/session/:sid/reorder" {:post #'reorder-session-handler}]
   ["/ui/session/:sid" {:get #'session-handler :delete #'delete-session-ui-handler}]
   ["/ui/session/:sid/delete" {:get #'delete-session-confirm-handler}]
   ["/ui/session/:sid/export.html" {:get #'export-handler}]
   ["/ui/slash" {:get #'slash-list-handler}]
   ["/ui/session/:sid/fs-picker" {:get #'fs-picker-handler}]
   ["/ui/session/:sid/fs-create" {:post #'fs-create-handler}]
   ["/ui/session/:sid/fs-add" {:post #'fs-add-handler}]
   ["/ui/session/:sid/turns" {:post #'submit-turn-handler :get #'turns-older-handler}]
   ["/ui/session/:sid/queued/:tid/update" {:post #'queued-update-handler}]
   ["/ui/session/:sid/queued/:tid/delete" {:post #'queued-delete-handler}]
   ["/ui/session/:sid/queued/clear" {:post #'queued-clear-handler}]
   ["/ui/session/:sid/turn/:tid/trace" {:get #'turn-trace-handler}]
   ["/ui/session/:sid/stream" {:get #'stream-handler}]
   ["/ui/session/:sid/footer" {:get #'session-footer-handler}]
   ["/ui/session/:sid/cancel-turn" {:post #'cancel-turn-handler}]
   ["/ui/session/:sid/fs-remove" {:post #'fs-remove-handler}]
   ["/ui/session/:sid/fs-root" {:post #'fs-root-handler}]
   ["/ui/session/:sid/poll" {:get #'poll-handler}]])

(defn- ui-contribution
  "The gateway pulls this through the `:gateway.slot/http-routes`
   whiteboard slot whenever it (re)builds its handler — no registration
   call, no ordering requirement between gateway start and this
   extension loading."
  []
  {:prefix "/ui"
   ;; :rev rides the namespace load stamp: a :reload that ADDS routes
   ;; moves the gateway fingerprint and remounts the route table
   ;; (handler #'vars are live on their own; the table is not).
   :rev ui-load-stamp
   :routes ui-routes
   :open-uris #{"/ui" "/ui/auth" "/ui/app.css" "/ui/icons.svg" "/ui/js/htmx.min.js"
                "/ui/js/htmx-sse.js" "/ui/js/marked.min.js" "/ui/js/prism.min.js"
                "/ui/js/purify.min.js" "/ui/js/ui.js" "/ui/js/rec-worklet.js"
                "/ui/fonts/hanken-grotesk.woff2" "/ui/fonts/jetbrains-mono.woff2"}
   :request-authed-fn ui-authed?
   :on-unauthorized (fn [_request]
                      {:status 303 :headers {"Location" "/ui"} :body ""})
   :on-not-found #'ui-not-found-handler
   :form-params? true
   :multipart? true})


(defn- start-cloudflared!
  "Spawn `cloudflared tunnel --url <local-url>` (a Cloudflare quick tunnel)
   and block until the public trycloudflare URL shows up in its output (or
   30s pass). Returns {:process Process :url String-or-nil}. Throws ex-info
   {:cloudflared/missing? true} with a friendly message when the
   `cloudflared` binary is not on PATH."
  [local-url]
  ;; --protocol http2: the QUIC transport is the worse of the two for
  ;; long-lived streams through quick tunnels; http2 at least keeps the
  ;; origin hop boring. (The edge may STILL buffer SSE bodies — the
  ;; ui.js watchdog + /poll fallback covers that.)
  (let
    [pb
     (doto (let [^java.util.List argv ["cloudflared" "tunnel" "--protocol" "http2" "--url"
                                       local-url]]
             (ProcessBuilder. argv))
       (.redirectErrorStream true))

     ^java.lang.Process process
     (try
       (.start pb)
       (catch java.io.IOException _
         (throw
           (ex-info
             (str
               "cloudflared binary not found on PATH. "
               "Install it first (e.g. `brew install cloudflared`) - "
               "https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/")
             {:cloudflared/missing? true}))))

     url-promise
     (promise)

     reader
     (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream process)))]

    (doto (Thread. ^Runnable
                   (fn []
                     (loop []

                       (when-let [line (.readLine reader)]
                         (when-let [url (re-find #"https://[a-z0-9-]+\.trycloudflare\.com" line)]
                           (deliver url-promise url))
                         (recur)))))
      (.setDaemon true)
      (.start))
    {:process process :url (deref url-promise 30000 nil)}))

(defn- tailscale-active?
  "Best-effort probe: true when the `tailscale` CLI is present AND its backend
   is Running (so Tailscale's MagicDNS resolver is in play on this box). Any
   failure - missing binary, non-zero exit, timeout - yields false rather than
   throwing, so callers can treat it as a pure hint."
  []
  (try (let [^java.util.List args
             ["tailscale" "status"]

             process
             (.start (doto (ProcessBuilder. args) (.redirectErrorStream true)))

             _drain
             (future (slurp (.getInputStream process)))

             done?
             (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)]

         (if done? (zero? (.exitValue process)) (do (.destroyForcibly process) false)))
       (catch Exception _ false)))

(defn- start-tailscale!
  "Spawn `tailscale serve <port>` (a Tailscale tailnet HTTPS proxy that exposes
   the local gateway at https://<host>.<tailnet>.ts.net to devices signed into
   your tailnet) and block until that URL shows up in its output (or 30s pass).
   Because the hostname is a real `*.ts.net` name served by your own tailnet, it
   resolves cleanly through Tailscale MagicDNS - unlike the `*.trycloudflare.com`
   names cloudflared mints, which a Tailscale resolver often can't see. `serve`
   (unlike `funnel`) needs no admin-console setup and keeps the URL tailnet-
   private. Returns {:process Process :url String-or-nil :hint String-or-nil} -
   `:hint` carries tailscale's own guidance (e.g. the one-time enable-Serve link)
   when no URL appears, so the caller can print something actionable instead of a
   bare timeout. Throws ex-info {:tailscale/missing? true} with a friendly message
   when the `tailscale` binary is not on PATH."
  [port]
  (let [pb
        (doto (let [^java.util.List argv ["tailscale" "serve" (str port)]]
                (ProcessBuilder. argv))
          (.redirectErrorStream true))

        ^java.lang.Process process
        (try (.start pb)
             (catch java.io.IOException _
               (throw
                 (ex-info
                   (str "tailscale binary not found on PATH. "
                        "Install it first (https://tailscale.com/download) and run `tailscale up`.")
                   {:tailscale/missing? true}))))

        url-promise
        (promise)

        ;; Keep the last handful of non-blank lines so a failed spawn (Serve not
        ;; enabled on the tailnet, HTTPS certs off, ...) can echo tailscale's own
        ;; guidance - including the admin-console enable link - instead of nothing.
        lines
        (atom [])

        reader
        (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream process)))]

    (doto (Thread. ^Runnable
                   (fn []
                     (loop []

                       (when-let [line (.readLine reader)]
                         (when-not (clojure.string/blank? line)
                           (swap! lines (fn [xs]
                                          (vec (take-last 8
                                                          (conj xs (clojure.string/trim line)))))))
                         (when-let [url (re-find #"https://[a-z0-9.-]+\.ts\.net" line)]
                           (deliver url-promise url))
                         (recur)))))
      (.setDaemon true)
      (.start))
    (let [url (deref url-promise 30000 nil)]
      {:process process
       :url url
       :hint (when-not url
               (let [captured (seq @lines)]
                 (when captured (clojure.string/join " " captured))))})))

(defn- attach-tunnel!
  "Spawn a public tunnel via `starter` (a 0-arg fn returning
   {:process Process :url String-or-nil}), register a JVM shutdown hook that
   tears the child down forcibly on exit (SIGTERM, then SIGKILL if it lingers -
   so an interrupted parent stops orphaning tunnels), and print the resulting
   `<url>/ui` (or a fallback note). `label` names the tunnel in the printed
   lines; `missing-key` is the ex-data flag whose presence turns a spawn failure
   into a friendly one-line message instead of a re-throw."
  [label missing-key starter]
  (try (let [{:keys [^Process process url hint]} (starter)]
         (.addShutdownHook
           (Runtime/getRuntime)
           (Thread. ^Runnable
                    (fn []
                      (when (.isAlive process)
                        (.destroy process)
                        (when-not (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)
                          (.destroyForcibly process))))))
         (cond url (println (str label " tunnel: " url "/ui"))
               hint (println (str label ": no URL yet - " hint))
               :else (println (str label
                                   ": tunnel started, but no public URL appeared within 30s (check "
                                   label
                                   " logs)"))))
       (catch clojure.lang.ExceptionInfo e
         (if (missing-key (ex-data e)) (println (str label ": " (ex-message e))) (throw e)))))

(defn channel-main
  "`vis channels web` ensures the detached gateway daemon is running AND
   actually serving `/ui` (the routes are auto-mounted in that daemon
   because this namespace is on the extension classpath; if it attached to a
   stale daemon that lacks them, `gateway-ensure-serving!` respawns a fresh
   one from this process). Prints the /ui address, then parks as a thin
   client. The command no longer starts/stops an in-process engine; killing
   this wrapper only releases its client lease."
  [args]
  (let [cloudflared?
        (boolean (some #{"--cloudflared"} args))

        tailscale?
        (boolean (some #{"--tailscale"} args))

        flag-val
        (fn [flag]
          (second (drop-while #(not= flag %) args)))

        req-port
        (some-> (flag-val "--port")
                not-empty
                parse-long)

        req-host
        (not-empty (flag-val "--host"))

        {:keys [host port secret]}
        (vis/gateway-ensure-serving! "/ui" {:host req-host :port req-port})

        {:strs [require_token]}
        (vis/gateway-daemon-status)]

    (println (str "vis web companion: http://" host ":" port "/ui"))
    (if require_token
      (println (str "bearer token: " (or secret "<registry secret unavailable>")))
      (println "auth: disabled (loopback daemon default)"))
    ;; A public tunnel is requested. --tailscale serves over your own tailnet
    ;; (a *.ts.net name that Tailscale MagicDNS resolves cleanly); --cloudflared
    ;; mints a *.trycloudflare.com name that a Tailscale resolver often can't
    ;; see - so when Tailscale is active we nudge toward --tailscale up front.
    (when (or cloudflared? tailscale?)
      (when-not require_token
        (println
          (if tailscale?
            (str
              "tailscale: note - gateway auth is disabled; this URL is reachable by every "
              "device signed into your tailnet. Restart the daemon with --require-token to gate it.")
            "cloudflared: WARNING gateway auth is disabled; restart the daemon with --require-token before exposing it publicly.")))
      (when (and cloudflared? (not tailscale?) (tailscale-active?))
        (println
          (str "cloudflared: heads-up - Tailscale is active on this machine, and its MagicDNS "
               "often can't resolve *.trycloudflare.com, so the URL below may look unreachable "
               "locally even though the tunnel is live. Re-run with `--tailscale` to serve over "
               "your tailnet (a *.ts.net URL that resolves cleanly) instead.")))
      (if tailscale?
        (attach-tunnel! "tailscale" :tailscale/missing? #(start-tailscale! port))
        (attach-tunnel! "cloudflared"
                        :cloudflared/missing?
                        #(start-cloudflared! (str "http://" host ":" port)))))
    @(promise)))

(vis/register-extension!
  (vis/extension
    {:ext/name "channel-web"
     :ext/description "Web companion - the gateway /ui chat instrument (hiccup + HTMX + SSE)."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/channels
     [{:channel/id :web
       :channel/cmd "web"
       :channel/doc "Serve the gateway with the /ui web companion."
       :channel/usage
       "vis channels web [--port 7890] [--host 127.0.0.1] [--cloudflared] [--tailscale]"
       :channel/main-fn #'channel-main}]
     :ext/channel-contributions {:gateway.slot/http-routes [{:id :web/ui :fn #'ui-contribution}]}}))
