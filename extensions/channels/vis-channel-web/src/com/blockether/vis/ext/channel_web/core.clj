(ns com.blockether.vis.ext.channel-web.core
  "Web companion channel - the gateway's `/ui` chat instrument.

   Chat-first anatomy (the ChatGPT/Claude shape): the conversation is a
   centered document column - user messages as compact right-aligned
   pills, vis answers as flat typeset prose behind a small gold avatar,
   the execution machinery folded into a 'Work' disclosure, a floating
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

   Theme: vis-light - white surfaces, gold fills, amber accent text -
   CSS variables lifted 1:1 from `internal/theme.clj` light-palette."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [hiccup2.core :as h]
   [ring.core.protocols :as ring-protocols])
  (:import
   [java.io OutputStream]
   [java.nio.charset StandardCharsets]))

(def ^:private HEARTBEAT_MS 15000)

(def ^:private ui-load-stamp
  "Re-evaluated on every namespace (re)load — deliberately `def`, NOT
   `defonce`. The gateway contribution's `:rev` rides it so a REPL
   `:reload` that ADDS routes remounts the route table."
  (System/currentTimeMillis))

;; Every script the pages load is VENDORED on the classpath under
;; resources/vis-channel-web/public/ and served by this channel from
;; memory — no CDN, no request ever leaves the host.
(def ^:private JS_ASSETS
  {"htmx.min.js"   "vis-channel-web/public/htmx.min.js"
   "htmx-sse.js"   "vis-channel-web/public/htmx-sse.js"
   "marked.min.js" "vis-channel-web/public/marked.min.js"
   "prism.min.js"  "vis-channel-web/public/prism.min.js"
   "ui.js"         "vis-channel-web/public/ui.js"})

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
  (delay
    (into {}
      (keep (fn [[nm path]]
              (when-let [resource (io/resource path)]
                [nm (slurp resource)])))
      JS_ASSETS)))

(defn- js-asset-handler [request]
  (if-let [content (get @js-asset-cache (get-in request [:path-params :asset]))]
    {:status 200
     :headers {"Content-Type" "application/javascript; charset=utf-8"
               "Cache-Control" "no-cache"}
     :body content}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "unknown asset"}))

;; Vendored fonts (SIL Open Font License 1.1 — free commercial use and
;; redistribution; license texts ship next to the woff2 files):
;;   Inter (rsms/inter)            — UI + prose
;;   JetBrains Mono (JetBrains)    — code
(def ^:private FONT_ASSETS
  {"inter-400.woff2"          "vis-channel-web/public/fonts/inter-400.woff2"
   "inter-600.woff2"          "vis-channel-web/public/fonts/inter-600.woff2"
   "inter-700.woff2"          "vis-channel-web/public/fonts/inter-700.woff2"
   "jetbrains-mono-400.woff2" "vis-channel-web/public/fonts/jetbrains-mono-400.woff2"
   "jetbrains-mono-700.woff2" "vis-channel-web/public/fonts/jetbrains-mono-700.woff2"})

(def ^:private font-asset-cache
  "Asset name -> bytes, read from the classpath ONCE (fonts are binary —
   the js cache slurps strings and would corrupt them)."
  (delay
    (into {}
      (keep (fn [[nm path]]
              (when-let [resource (io/resource path)]
                (with-open [in (io/input-stream resource)
                            out (java.io.ByteArrayOutputStream.)]
                  (io/copy in out)
                  [nm (.toByteArray out)]))))
      FONT_ASSETS)))

(defn- font-asset-handler [request]
  (if-let [^bytes content (get @font-asset-cache (get-in request [:path-params :asset]))]
    {:status 200
     :headers {"Content-Type" "font/woff2"
               "Cache-Control" "public, max-age=86400"}
     :body (java.io.ByteArrayInputStream. content)}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "unknown font"}))

(def ^:private icons-sprite
  "Feather Icons (MIT) sprite, vendored on the classpath."
  (delay (some-> (io/resource "vis-channel-web/public/icons.svg") slurp)))

(defn- icons-handler [_]
  (if-let [sprite @icons-sprite]
    {:status 200
     :headers {"Content-Type" "image/svg+xml; charset=utf-8"
               "Cache-Control" "no-cache"}
     :body sprite}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "no sprite"}))

(defn- icon
  "Inline reference into the vendored Feather sprite."
  [id]
  [:svg.icon {:aria-hidden "true"} [:use {:href (str "/ui/icons.svg#" id)}]])

;; =============================================================================
;; Canonical IR -> hiccup (the web IR walker)
;; =============================================================================

(def ^:private ir-tag->html
  "IR tags with a direct HTML counterpart. Anything else renders as a
   div carrying `ir-<tag>` so unknown/new IR never breaks the page."
  {:ir :div :p :p :span :span :strong :strong :b :strong :em :em :i :em
   :h1 :h1 :h2 :h2 :h3 :h3 :h4 :h4 :h5 :h5 :h6 :h6
   :ul :ul :ol :ol :li :li :blockquote :blockquote :hr :hr :br :br
   :table :table :thead :thead :tbody :tbody :tr :tr :td :td :th :th
   :del :del :s :del :a :a})

(defn ir->hiccup
  "Walk a canonical IR node (`[:tag {attrs}? & children]`, strings,
   seqs) into hiccup. Total: unknown tags degrade to classed divs,
   non-IR leaves degrade to strings. Never throws on model output."
  [node]
  (cond
    (string? node) node
    (nil? node) nil
    (number? node) (str node)
    (seq? node) (keep ir->hiccup node)

    (vector? node)
    (let [[tag second-el & rest-els] node
          attrs?   (map? second-el)
          attrs    (if attrs? second-el {})
          children (if attrs? rest-els (cons second-el rest-els))]
      (cond
        ;; The ENGINE vocabulary (markdown->ir): headings are
        ;; `[:h {:level N} …]`, inline code is `[:c …]`, and `[:code
        ;; {:lang …} "…"]` is the fenced BLOCK.
        (= tag :h)
        (into [(keyword (str "h" (-> (or (:level attrs) 2) long (max 1) (min 6))))]
          (keep ir->hiccup children))

        (= tag :c)
        [:code (keep ir->hiccup children)]

        (or (= tag :code) (= tag :pre) (= tag :code-block))
        ;; `language-*` is the Prism convention (marked emits it too),
        ;; so server-rendered and client-rendered fences highlight alike.
        ;; `.ir-pre` is LOAD-BEARING: it carries overflow-x:auto — without
        ;; it a wide tool line (cat gutter, diff row) stretches the whole
        ;; thread horizontally instead of scrolling inside the block.
        [:pre.ir-pre
         [:code {:class (str "language-" (name (or (:lang attrs) "txt")))}
          (keep ir->hiccup children)]]

        (= tag :a)
        (into [:a {:href (str (:href attrs)) :rel "noreferrer"}]
          (keep ir->hiccup children))

        :else
        (if-let [html-tag (ir-tag->html tag)]
          (into [html-tag] (keep ir->hiccup children))
          (into [:div {:class (str "ir-" (name tag))}]
            (keep ir->hiccup children)))))

    :else (str node)))

(defn- md->hiccup [markdown]
  (when-not (str/blank? (str markdown))
    (try
      (ir->hiccup (vis/markdown->ir (str markdown)))
      (catch Throwable _ [:pre.ir-pre (str markdown)]))))

(defn- html ^String [hiccup-form]
  (str (h/html hiccup-form)))

(defn- json-text [m]
  (str "{" (str/join "," (for [[k v] m] (str (pr-str (name k)) ":" (pr-str (str v))))) "}"))

(defn- page ^String [title & body]
  (str "<!DOCTYPE html>"
    (h/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport"
                :content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"}]
        [:title (str title " · vis")]
        ;; `?v=` cache-buster (asset-version, new every gateway start) so a
        ;; restart/deploy forces a refetch — iOS Safari otherwise serves a stale
        ;; cached app.css/ui.js even with no-cache, on reopened tabs.
        [:link {:id "theme-css" :rel "stylesheet" :href (str "/ui/app.css?v=" asset-version)}]
        ;; All vendored, all local — nothing loads from outside vis.
        [:script {:src (str "/ui/js/htmx.min.js?v=" asset-version) :defer true}]
        [:script {:src (str "/ui/js/htmx-sse.js?v=" asset-version) :defer true}]
        [:script {:src (str "/ui/js/marked.min.js?v=" asset-version) :defer true}]
        [:script {:src (str "/ui/js/prism.min.js?v=" asset-version) :defer true}]
        [:script {:src (str "/ui/js/ui.js?v=" asset-version) :defer true}]]
       (into [:body] body)])))

;; =============================================================================
;; Defensive readers (context snapshot values may carry string OR kw
;; keys - the GraalPy boundary keeps snake_case strings verbatim)
;; =============================================================================

(defn- pick [m k]
  (when (map? m)
    (or (get m k) (get m (name k)) (get m (keyword (name k))))))

(defn- fact-entries [facts]
  (cond
    (map? facts) (sort-by (comp str key) facts)
    (sequential? facts) (map-indexed (fn [i f] [(or (pick f :key) (str "fact-" i)) f]) facts)
    :else nil))

;; =============================================================================
;; Fragments
;; =============================================================================

(defn- status-chip [status]
  [:span {:class (str "chip chip-" (or status "idle"))} (or status "idle")])

(defn- bar-title-content
  "Children of the header `.bar-title` - re-rendered over SSE at turn
   boundaries so the status chip and the host-generated title stay live."
  [soul]
  (list
    [:span.bar-name (or (:title soul) "Untitled")]
    (status-chip (:status soul))))

(defn- fmt-tok
  "Compact token count: 842 / 12.4k / 1.2M. Hand-rolled so no JVM
   locale can inject a comma separator."
  [n]
  (when (number? n)
    (let [n (long n)]
      (cond
        (>= n 1000000) (str (quot n 1000000) "." (mod (quot n 100000) 10) "M")
        (>= n 1000)    (str (quot n 1000) "." (mod (quot n 100) 10) "k")
        :else          (str n)))))

(defn- utilization-bar [utilization]
  (let [pct (or (pick utilization :pct-of-limit) (pick utilization :pct_of_limit))]
    (when (number? pct)
      [:div.util
       [:div.util-track [:div.util-fill {:style (str "width:" (min 100 (long pct)) "%")}]]
       [:span.util-label (str (long pct) "%")]])))

(defn- window-section
  "The context window numbers the model itself reads in
   `:session/utilization` - spelled out in plain words (with hover
   tooltips) so a user can read them without engine jargon."
  [utilization]
  (let [last-req (or (pick utilization :last-request-tokens) (pick utilization :last_request_tokens))
        limit    (or (pick utilization :model-input-limit) (pick utilization :model_input_limit))
        turn-tot (or (pick utilization :turn-total-tokens) (pick utilization :turn_total_tokens))
        fold     (or (pick utilization :auto-compress-above) (pick utilization :auto_compress_above))]
    (when (number? last-req)
      [:section.rail-section
       [:h3 "Context window"]
       (utilization-bar utilization)
       [:p.ctx-hint
        (str "How full the model's working memory is: "
          (fmt-tok last-req)
          (when (number? limit) (str " of " (fmt-tok limit)))
          " tokens used on the last request.")]
       [:dl.ctx-kv
        [:dt {:title "Tokens sent to the model on the most recent request, out of its maximum input size"}
         "Last request"]
        [:dd (str (fmt-tok last-req) (when (number? limit) (str " / " (fmt-tok limit))))]
        (when (number? turn-tot)
          (list [:dt {:title "Total tokens spent across every model request in the current turn"}
                 "Spent this turn"]
            [:dd (fmt-tok turn-tot)]))
        (when (and (number? fold) (pos? (long fold)))
          (list [:dt {:title "When the conversation grows past this size, the engine auto-summarizes the oldest history (stale results and facts) into compact recaps to free up room"}
                 "Auto-summarizes at"]
            [:dd (fmt-tok fold)]))]])))

(defn- routing-section
  "`:session/routing` — the provider/model the engine is actually
   routing this session's calls through. Provider and model on their
   OWN lines, names without the keyword colon."
  [routing]
  (when (map? routing)
    (let [->name   (fn [v] (cond (keyword? v) (name v)
                             (some? v) (str v)
                             :else nil))
          provider (->name (or (pick routing :provider) (pick routing :current-provider)))
          model    (->name (or (pick routing :model) (pick routing :current-model)))]
      (when (or provider model)
        [:section.rail-section
         [:h3 "Routing"]
         [:dl.ctx-kv
          (when provider (list [:dt "provider"] [:dd provider]))
          (when model (list [:dt "model"] [:dd model]))]]))))

(defn- resources-section
  "`:session/resources` — the live stateful-resource registry (the
   footer's ●N): running nREPLs, shell processes, ..."
  [resources]
  (when (seq resources)
    [:section.rail-section
     [:h3 (str "Resources · " (count resources))]
     [:ul.ctx-resources
      (for [r resources]
        [:li.ctx-resource
         [:span.res-dot]
         [:span.ctx-mono
          (str (or (pick r :kind) (pick r :type) "resource")
            (when-let [id (or (pick r :id) (pick r :name))] (str " · " id))
            (when-let [s (pick r :status)] (str " · " s)))]])]]))

(defn- hints-section
  "`:session/hints` — the engine's own advisory feed to the model."
  [hints]
  (when (seq hints)
    [:section.rail-section
     [:h3 (str "Hints · " (count hints))]
     [:ul.ctx-hints
      (for [h hints]
        [:li.ctx-hint
         (str (or (pick h :text) (pick h :message) (pick h :content) (pr-str h)))])]]))

(defn- env-section
  "`:session/env` — the host/project/extensions digest the model sees."
  [env]
  (when (map? env)
    (let [project (pick env :project)
          host    (pick env :host)
          exts    (pick env :extensions)]
      [:section.rail-section
       [:h3 "Env"]
       [:dl.ctx-kv
        (when-let [root (or (pick project :root) (pick project :dir) (pick project :path))]
          (list [:dt "project"] [:dd.ctx-mono (str root)]))
        (when-let [os (or (pick host :os) (pick host :platform))]
          (list [:dt "host"] [:dd (str os)]))
        (when exts
          (list [:dt "extensions"]
            [:dd (str (if (or (seq? exts) (vector? exts) (set? exts)) (count exts) exts))]))]])))

;; ── Generic ctx rendering — every `:session/*` key gets a REAL form ──
;; Keys with dedicated sections (window/routing/resources/plan/facts/
;; archived/hints/env) render richly; everything else — workspace,
;; symbols, trailer, archive-digest, extension-contributed slices like
;; :session/voice — walks through this kv-tree renderer instead of an
;; EDN dump. Large values fold closed but are always reachable.

(def ^:private ctx-rail-handled-keys
  #{:session/utilization :session/routing :session/resources :session/tasks
    :session/facts :session/archived :session/hints :session/env
    :session/trailer
    ;; the engine's LIVE turn cursor — meaningless between turns
    :session/id :session/turn :session/scope})

(defn- trailer-section
  "`:session/trailer` — the turn's working trace (per-iteration pins
   with form envelopes). Too nested for the kv-tree; rendered as
   pretty, syntax-highlighted JSON in the canonical wire shape,
   collapsed by default."
  [trailer]
  (when (seq trailer)
    [:section.rail-section
     [:details.ctx-fold
      [:summary [:span.ctx-fold-label (str "Trailer · " (count trailer))]]
      [:div.ctx-fold-body
       [:pre.ir-pre [:code.language-json (vis/wire-json-pretty trailer)]]]]]))

(defn- humanize-ctx-key [k]
  (let [s (if (keyword? k) (name k) (str k))]
    (-> s (str/replace #"[-_]" " ") str/capitalize)))

(defn- ctx-value
  "Walk one ctx value into tidy DOM: maps as dt/dd grids, sequences as
   lists, scalars as mono text. Depth-capped to keep pathological
   nesting readable (the cap falls back to pr-str, still visible)."
  [v depth]
  (cond
    (and (map? v) (seq v) (< depth 3))
    [:dl.ctx-kv
     (for [[k val] (sort-by (comp str key) v)]
       (list [:dt (humanize-ctx-key k)]
         [:dd (ctx-value val (inc depth))]))]

    (and (sequential? v) (seq v) (< depth 3))
    [:ul.ctx-list
     (for [item v] [:li (ctx-value item (inc depth))])]

    (nil? v)     [:span.ctx-mono "–"]
    (string? v)  [:span.ctx-str v]
    (keyword? v) [:span.ctx-mono (name v)]
    (coll? v)    [:span.ctx-mono (pr-str v)]
    :else        [:span.ctx-mono (str v)]))

(defn- ctx-extra-section
  "One rail section for a ctx key without a dedicated renderer. Values
   whose printed size is large render inside a collapsed fold so the
   rail stays scannable — opened on demand, never omitted."
  [k v]
  (when (and (some? v) (or (not (coll? v)) (seq v)))
    (let [title (humanize-ctx-key k)
          big?  (> (count (pr-str v)) 600)]
      [:section.rail-section
       (if big?
         [:details.ctx-fold
          [:summary [:span.ctx-fold-label title]]
          [:div.ctx-fold-body (ctx-value v 0)]]
         (list [:h3 title] (ctx-value v 0)))])))

(defn- ctx-extra-sections
  "Sections for every snapshot key not covered by a dedicated renderer,
   stable alphabetical order."
  [snapshot]
  (->> snapshot
    (filter (fn [[k _]] (and (keyword? k) (not (contains? ctx-rail-handled-keys k)))))
    (sort-by (comp str key))
    (keep (fn [[k v]] (ctx-extra-section k v)))))

(defn- fact-card [[fact-key fact]]
  [:details.fact
   ;; THE canonical display rule (internal/format.clj humanize-fact-key,
   ;; shared with the TUI ctx panel): turn_<N> -> "Turn <N>", everything
   ;; else first-letter capitalized. Display only; keys stay verbatim.
   [:summary [:span.fact-key (vis/humanize-fact-key fact-key)]]
   [:div.fact-body
    ;; Fact :content is MARKDOWN by the engine's own contract — the
    ;; turn_<N> auto-fact is one `## Question` / `## Answer` blob
    ;; (ctx_engine.clj). Render it, don't print it.
    (when-let [content (pick fact :content)]
      [:div.fact-content.md {:data-md (str content)} (md->hiccup content)])
    (for [file (or (pick fact :files) [])
          :let [path (pick file :path)]
          :when path]
      [:div.fact-file
       [:span.fact-path (str path)]
       (for [region (or (pick file :regions) [])
             :let [from-hash (let [h (pick region :from_hash)]
                               ;; "n/a" is a model-written placeholder, not an
                               ;; anchor — showing "@n/a" reads as a glitch.
                               (when (and h (not (#{"n/a" "na" ""} (str/lower-case (str h)))))
                                 (str h)))
                   note      (pick region :note)]]
         [:div.fact-region
          (when (or from-hash note)
            [:div.fact-region-meta
             (when from-hash [:span.fact-hash (str "@" from-hash)])
             (when note [:span.fact-note (str note)])])
          (when-let [src (pick region :src)]
            [:pre.ir-pre.fact-src [:code (str src)]])])])
    (when-not (or (pick fact :content) (seq (or (pick fact :files) [])))
      [:pre.ir-pre [:code (pr-str fact)]])]])

(def ^:private task-glyph
  {"done" "✓" "completed" "✓" "in_progress" "◐" "running" "◐"
   "candidate" "◇" "pending" "○"})

(defn- task-row [task]
  (let [status-raw (pick task :status)
        status (cond
                 (keyword? status-raw) (name status-raw)
                 (some? status-raw)    (str status-raw)
                 :else                 "pending")]
    [:li {:class (str "task task-" status)}
     [:span.task-glyph (get task-glyph status "○")]
     [:span.task-title (str (or (pick task :title) (pick task :id) (pr-str task)))]]))

(defn- archived-section
  "`:session/archived` — entities compaction moved OUT of the model's
   live ctx (reachable to the model only via `recall`). Shown to the
   USER so archived work never silently disappears: archived tasks as
   dimmed task rows, archived facts as the same fold-open fact cards."
  [archived]
  (when (seq archived)
    (let [entries (vals archived)
          tasks   (filter #(= :task (:vis/kind %)) entries)
          facts   (keep (fn [f] (when (= :fact (:vis/kind f))
                                  [(or (:vis/key f) (:id f)) f]))
                    entries)]
      [:section.rail-section.archived
       [:h3 (str "Archived · " (count entries))]
       (when (seq tasks)
         [:ul.tasks.archived-rows (map task-row tasks)])
       (when (seq facts)
         [:div.facts.archived-rows (map fact-card (sort-by (comp str first) facts))])])))

(defn- plan-history-section
  "Past plan GENERATIONS from the append-only task ledger
   (`vis/plan-timeline`) — every plan a whole-replace dropped, dimmed,
   each step frozen at the status it had when the replace dropped it.
   The CURRENT generation is the live Plan section above, so it is
   skipped here. Newest dropped plan first."
  [timeline]
  (let [past (remove :current? (or timeline []))]
    (when (seq past)
      [:section.rail-section.plan-history
       [:h3 (str "Plan history · " (count past)
              " earlier plan" (when (not= 1 (count past)) "s"))]
       (for [{:keys [gen steps]} (reverse past)]
         [:div.plan-gen
          [:div.plan-gen-head (str "Plan #" gen " · " (count steps)
                                " step" (when (not= 1 (count steps)) "s"))]
          [:ul.tasks.archived-rows (map task-row steps)]])])))

(defn- session-plan-timeline
  "Timeline read for the rail. `vis/plan-timeline` is TOTAL: it logs any
   read failure itself and returns nil — no silent catch needed here."
  [sid]
  (vis/plan-timeline (vis/db-info) sid))

(defn- context-panel
  "The right rail: CONTEXT — the same ctx mirror the model reads,
   rendered as an instrument: window numbers, routing, live resources,
   plan, facts, env digest, hints, and the raw EDN at the bottom so
   NOTHING the model sees is hidden from the user."
  ([snapshot] (context-panel snapshot nil))
  ([snapshot timeline]
  [:div#context.context
   ;; mobile-only: the rail opens FULL WIDTH (covering the scrim), so it needs
   ;; its own close button (CSS hides this on desktop).
   [:div.rail-head
    [:button.rail-close {:type "button" :data-close-drawer "1" :aria-label "Close context"}
     (icon "x")]]
   (window-section (pick snapshot :session/utilization))
   (routing-section (pick snapshot :session/routing))
   (resources-section (pick snapshot :session/resources))
   (let [tasks (pick snapshot :session/tasks)
         rows  (cond
                 (map? tasks)        (->> tasks
                                       (map (fn [[k t]] (if (map? t) (assoc t :key k) t)))
                                       (sort-by #(or (pick % :order) 0)))
                 (sequential? tasks) tasks
                 :else               nil)]
     [:section.rail-section
      [:h3 (str "Plan" (when (seq rows) (str " \u00b7 " (count rows))))]
      (if (seq rows)
        [:ul.tasks (map task-row rows)]
        [:p.empty "no plan yet"])])
   (plan-history-section timeline)
   (let [facts (fact-entries (pick snapshot :session/facts))]
     [:section.rail-section
      [:h3 (str "Facts" (when (seq facts) (str " · " (count facts))))]
      (if (seq facts)
        [:div.facts (map fact-card facts)]
        [:p.empty "no facts yet"])])
   (archived-section (pick snapshot :session/archived))
   (hints-section (pick snapshot :session/hints))
   (trailer-section (pick snapshot :session/trailer))
   ;; Everything else the model reads — workspace, symbols, trailer,
   ;; archive-digest, extension ctx slices — rendered generically, so
   ;; NO ctx key is ever invisible. (:session/id/turn/scope stay out:
   ;; the live turn cursor is meaningless between turns.)
   (ctx-extra-sections snapshot)]))

;; =============================================================================
;; Plan review — the Antigravity-style annotation card. When the model
;; proposes `:candidate` steps and stops (needs-input), the thread grows
;; an inline card: per-step Approve/Reject chips + a comment box, plus an
;; overall note. Submit compiles EVERYTHING through the SAME
;; `vis/plan-review-message` grammar the TUI dialog uses and sends it as
;; the next user turn — the MODEL re-emits the plan (approve → pending,
;; reject → rejected, comment → revised candidate + another stop), no
;; host-side status flip ever. The card lives in `#planreview`, an
;; SSE-swapped slot: every turn end re-renders it (or clears it).
;; =============================================================================

(defn- plan-review-card
  "The annotation card for the CURRENT proposal, or nil when no
   `:candidate` step is awaiting review (nil empties the slot)."
  [sid snapshot]
  (let [steps (vis/plan-reviewable (pick snapshot :session/tasks))]
    (when (some :candidate? steps)
      [:div.plan-review
       [:div.pr-head
        [:span.pr-glyph "◇"]
        [:span.pr-title "Plan review"]
        [:span.pr-sub "annotate the proposal — the agent revises and re-proposes"]]
       [:form.pr-form {:hx-post (str "/ui/session/" sid "/plan-review")
                       :hx-target "#live" :hx-swap "beforeend"}
        (for [{:keys [key title acceptance candidate? status]} steps]
          (if candidate?
            [:div.pr-step
             [:div.pr-step-head
              [:span.pr-step-title title]
              [:div.pr-verdict
               [:label.pr-chip.pr-approve
                [:input {:type "radio" :name (str "verdict_" key) :value "approve"}]
                [:span "approve"]]
               [:label.pr-chip.pr-reject
                [:input {:type "radio" :name (str "verdict_" key) :value "reject"}]
                [:span "reject"]]]]
             (when acceptance [:div.pr-accept acceptance])
             [:textarea.pr-note {:name (str "note_" key) :rows 1
                                 :placeholder "comment — the agent revises this step"}]]
            ;; Accepted / resolved steps ride along read-only so the
            ;; proposal reads in its surrounding context.
            [:div.pr-step.pr-frozen
             [:div.pr-step-head
              [:span.pr-step-title title]
              [:span.pr-status (name (or status :todo))]]]))
        [:textarea.pr-note.pr-overall {:name "overall" :rows 1
                                       :placeholder "overall note (optional)"}]
        [:div.pr-actions
         [:button.pr-send {:type "submit"} "Send review"]]]])))

(defn- plan-review-slot
  "The persistent SSE-swapped container. `card?` renders the current
   card inline (initial page paint); SSE `planreview` frames own its
   innerHTML afterwards."
  [sid snapshot]
  [:div#planreview.planreview-slot {:sse-swap "planreview" :hx-swap "innerHTML"}
   (when snapshot (plan-review-card sid snapshot))])

(defn- bubble-foot
  "TUI-faithful bubble footer: the CANONICAL `meta-summary-line`
   (provider/model · in→out · ~$cost · duration) — the same words and
   numbers the TUI bubble footer and the Telegram tagline show. No
   status badge; only a failure states itself, in red."
  [turn]
  (let [meta-line (try
                    (vis/meta-summary-line
                      {:tokens (pick turn :tokens)
                       :cost (pick turn :cost)
                       :duration-ms (pick turn :duration_ms)})
                    (catch Throwable _ nil))
        status (str (pick turn :status))
        failed? (contains? #{"failed" "cancelled"} status)]
    (when (or (seq meta-line) failed?)
      [:div.bubble-foot
       (when failed? [:span.foot-bad status])
       (when (seq meta-line) [:span.foot-meta meta-line])])))

(defn- role-time
  "`:vis/show-timestamps` honored on the web exactly like the TUI:
   date + time next to the role label. nil when the toggle is off or
   no timestamp is known. Applies to bubbles rendered AFTER the flip
   (live arrivals / next page load) — the DOM already on screen is not
   rewritten."
  [epoch-ms]
  (when (and (number? epoch-ms)
          (try (vis/toggle-enabled? :vis/show-timestamps) (catch Throwable _ false)))
    [:span.role-time
     (.format (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
       (java.time.LocalDateTime/ofInstant
         (java.time.Instant/ofEpochMilli (long epoch-ms))
         (java.time.ZoneId/systemDefault)))]))

(defn- user-bubble
  "TUI anatomy: 'You' role label in amber (:user-role-fg). The raw text
   rides in data-md; ui.js re-renders it through the vendored `marked`
   (MIT) for full markdown fidelity, falling back to the plain text."
  ([text] (user-bubble text nil))
  ([text epoch-ms]
   [:div.bubble.b-user
    [:div.role.role-user "You" (role-time epoch-ms)]
    [:div.prose.md {:data-md (str text)} [:p (str text)]]]))

(defn- vis-bubble
  "TUI anatomy: 'Vis' role label in green (:ai-role-fg), canonical meta
   footer. Server renders the IR walk as the instant fallback; the raw
   markdown rides in data-md and ui.js re-renders it through `marked`."
  [turn]
  (let [ir (pick turn :answer_ir)
        md (or (pick turn :answer_md) (pick turn :error) "")]
    [:div.bubble.b-vis
     [:div.role.role-vis "Vis" (role-time (pick turn :started_at))]
     (if (and (vector? ir) (= :ir (first ir)))
       ;; The engine handed back a canonical IR AST (provider-error / fatal
       ;; fallback), not markdown — walk it directly. md->hiccup on the
       ;; stringified vector dumped the raw `[:ir …]` into the bubble.
       [:div.prose.md (ir->hiccup ir)]
       [:div.prose.md {:data-md (str md)} (md->hiccup md)])
     (bubble-foot turn)]))

;; ── Machinery blocks — the TUI transcript's code/result/error cells ──
;; Shown INLINE in the thread (no Work fold, nothing hidden): the live
;; SSE stream and the DB-restored history render the SAME blocks.

(defn- display-result
  "Result value -> display string, the transcript's rule: runtime refs
   get a placeholder, strings stay verbatim, everything else pr-str."
  [result]
  (cond
    (and (map? result) (= :expr (:vis/ref result)))
    "<runtime value; re-evaluate expression to restore>"
    (string? result) result
    :else (pr-str result)))

(defn- engine-verb-src?
  "True when the form source is an engine ctx-verb call (update_plan /
   plan_step / fact_set / done / set_session_title / introspect_*).
   Those forms paint as engine op cards (or the turn's answer bubble) -
   the raw Python call source is noise in the chat thread."
  [src]
  (boolean
    (re-find #"^\s*(?:update_plan|plan_step|fact_set|done|set_session_title|introspect_\w+)\s*\("
      (str src))))

(defn- mach-code [code]
  ;; The model writes Python (RLM contract) — tag the block so the
  ;; vendored Prism highlights it natively. VERBATIM, never clipped.
  [:div.mach.mach-code
   [:span.mach-tag "code"]
   [:pre.ir-pre [:code.language-python (str code)]]])

(defn- mach-dur
  "Tiny `· 840ms` suffix — every finished run says how long it took."
  [duration-ms]
  (when (number? duration-ms)
    [:span.mach-dur (str (long duration-ms) "ms")]))

(defn- mach-result
  ([result] (mach-result result nil))
  ([result duration-ms]
   ;; COLLAPSED by default — a code result is noise until you ask for it; tap
   ;; the `result` row to expand. (Errors stay open; only results fold.)
   [:details.mach.mach-result
    [:summary.mach-sum
     [:span.mach-tag "result"] (mach-dur duration-ms)]
    [:pre.ir-pre [:code (display-result result)]]]))

(defn- error-text
  "LEAN error body: message (+ line/col, + hint when not already in the
   message). A persisted error map nests host trace/data chains nobody can
   act on in the thread — never pr-str it. Wire strings pass verbatim."
  [error]
  (if-not (map? error)
    (str error)
    (let [msg  (or (:message error) (some-> (:type error) str) "error")
          hint (:hint error)
          {:keys [line column]} (:data error)]
      (cond-> msg
        (and line column) (str " (line " line ", col " column ")")
        (and hint (not (str/includes? msg (str hint)))) (str "\nhint: " hint)))))

(defn- mach-error [error]
  [:div.mach.mach-error
   [:span.mach-tag.bad "error"]
   [:pre.ir-pre.act-error [:code (error-text error)]]])

(defn- form-error-covered-by-op?
  "True when the persisted form's `:error` is the SAME failure one of its
   errored tool sink entries already renders as an op card — painting the
   block-level error too showed the same failure twice (TUI dedupes this
   via error signatures; this is the web twin)."
  [form]
  (boolean
    (when-let [msg (and (map? (:error form)) (:message (:error form)))]
      (some #(and (false? (:success? %))
               (= msg (get-in % [:error :message])))
        (:channel form)))))

(defn- ir-header-inline
  "Unwrap a tool display's LEADING header block (`[:ir {} header & body]`,
   where `header` is `[:p {} …]`) to its inline children so the line sits
   ON the collapsible `<summary>` row — no nested block, no `<p>` margins.
   Non-`:p` headers render whole; nil yields nil."
  [header]
  (when (vector? header)
    (let [[tag second-el & rest-els] header
          children (if (map? second-el) rest-els (cons second-el rest-els))]
      (if (= tag :p)
        (seq (keep ir->hiccup children))
        [(ir->hiccup header)]))))

(defn- mach-tool
  "One tool-call op as the extension's OWN renderer drew it — the
   `{:summary :display}` canonical-IR contract walked into DOM. The
   web twin of the TUI's `▶ LABEL …` op rows; raw result blobs are
   never shown when the tool rendered itself.

   A tool's `:display` is `[:ir {} <header-p> <body…>]` where the header-p
   is the SAME line the `:summary` zones encode (`SHELL  echo… → exit 0`).
   The TUI shows EITHER the summary (collapsed) OR the display (expanded),
   never both — so in the web card the display's own header BECOMES the
   collapsible `<summary>` and only the remaining blocks fold into the body.
   That kills the double header (`SHELL` tag on top of `SHELL echo… → exit
   0`) without dropping any information. No separate op-tag, no `mach-dur`:
   the header IS the label and carries the tool's own timing. A tool with no
   foldable body is a flat one-line head; no display falls back to the
   summary zones with the TUI `▶` marker."
  [{:keys [op tag status summary display duration_ms]}]
  (let [error? (= "error" (some-> status name))
        label  (str (or op tag "tool") (when error? " ✗"))
        ;; A BODY-ONLY display can be the empty doc `[:ir {}]` (e.g. an
        ;; engine op card with nothing to fold). The head line then lives
        ;; on the plain-IR summary, so paint THAT as the display; a zone
        ;; summary takes the no-display branch below instead.
        display (if (and (vector? display) (= :ir (first display))
                      (<= (count display) 2))
                  (when (and (vector? summary) (= :ir (first summary))
                          (> (count summary) 2))
                    summary)
                  display)]
    (if display
      (let [[_ir _attrs header & rest] (if (and (vector? display)
                                             (= :ir (first display)))
                                         display
                                         [:ir {} display])
            head (or (ir-header-inline header) [[:span.mach-tag label]])
            body (seq (keep identity rest))]
        (if body
          ;; self-headering display → COLLAPSIBLE: the tool's own header line
          ;; is the summary (collapsed already says cmd → exit · timing); the
          ;; remaining blocks fold away (errors start open).
          [:details.mach.mach-tool (cond-> {:class (when error? "mach-tool-err")}
                                     error? (assoc :open true))
           (into [:summary.mach-tool-head.mach-sum] head)
           [:div.mach-tool-body (keep ir->hiccup body)]]
          ;; header-only display → flat one-line row (nothing to fold)
          [:div.mach.mach-tool {:class (when error? "mach-tool-err")}
           (into [:div.mach-tool-head] head)]))
      ;; no display → flat head from the summary zones (keeps the TUI ▶ marker)
      [:div.mach.mach-tool {:class (when error? "mach-tool-err")}
       [:div.mach-tool-head
        [:span.mach-tag (str "▶ " label)]
        (when-let [left (pick summary :left)]
          [:span.mach-tool-sum (ir->hiccup left)])
        (when-let [right (pick summary :right)]
          [:span.mach-tool-sum.dim (ir->hiccup right)])
        (mach-dur duration_ms)]])))

(defn- form-ops
  "Tool ops for one PERSISTED form envelope: project its `:channel`
   sink slice through the canonical `vis/tool-sink-entry->op` (the
   exact projection the TUI resume path uses)."
  [form]
  (->> (or (:channel form) [])
    (sort-by :position)
    (keep (fn [entry]
            (try
              (let [op (vis/tool-sink-entry->op entry)
                    {:keys [started-at-ms finished-at-ms]} op]
                (mach-tool {:op (when-let [o (:op op)]
                                  (if (keyword? o) (subs (str o) 1) (str o)))
                            :tag (some-> (:tag op) name)
                            :status (name (:status op))
                            :summary (:summary op)
                            :display (:display op)
                            :duration_ms (when (and (number? started-at-ms)
                                                 (number? finished-at-ms))
                                           (max 0 (- (long finished-at-ms)
                                                    (long started-at-ms))))}))
              (catch Throwable _ nil))))
    seq))

(defn- mach-thinking
  "The iteration's reasoning, pinned PERMANENTLY into the thread at the
   iteration boundary - the #thinking ticker shows only the moving tail
   while streaming and is wiped at turn end; without this block the
   thinking vanished the moment streaming finished. Text is TRIMMED:
   the body renders white-space:pre-wrap, so stray leading/trailing
   whitespace from the model would paint as empty space."
  [text]
  (let [t (str/trim (str text))]
    (when-not (str/blank? t)
      [:div.mach.mach-thinking
       [:span.mach-tag "thinking"]
       [:div.mach-think-body t]])))

(defn- mach-iter-tick [position duration-ms]
  [:div.mach.mach-iter
   (str "iteration" (when position (str " " position)) " done"
     (when (number? duration-ms) (str " · " (long duration-ms) "ms")))])

;; ── Virtualised thread (web twin of the TUI react-window scrollback) ──
;; The page renders only the most recent INITIAL_TURN_WINDOW turns; older
;; turns load on scroll-up via the `.load-older` sentinel, and each turn's
;; machinery (code/results) loads only when expanded. Off-screen / collapsed
;; content never crosses the wire — the same "paint only what's near the
;; viewport" rule `virtual.clj` enforces for Lanterna.

(def ^:private INITIAL_TURN_WINDOW
  "How many recent turns the initial page renders. Older ones page in on
   scroll-up." 6)
(def ^:private OLDER_TURN_PAGE
  "How many older turns one scroll-up `.load-older` fetch returns." 6)

(defn- older-sentinel
  "Top-of-thread infinite-scroll trigger. When revealed (user scrolls up)
   it hx-gets the previous page of turns and replaces itself with them plus
   a fresh sentinel."
  [sid before-tid]
  [:div.load-older {:hx-get (str "/ui/session/" sid "/turns?before=" before-tid)
                    :hx-trigger "revealed"
                    :hx-swap "outerHTML"}
   [:div.mach-loading "loading earlier…"]])

(defn- machinery-body
  "The code/results/tools body of one finished turn's machinery, read from
   the engine DB - the same blocks the live stream showed. Returns the
   `.machinery-body` div (so an hx-get outerHTML swap drops it straight over
   the lazy placeholder); nil on empty / read failure."
  [turn]
  (try
    (when-let [tid (some-> (or (pick turn :engine_turn_id) (pick turn :turn_id)) str parse-uuid)]
      (let [iters (vis/db-list-session-turn-iterations (vis/db-info) tid)]
        (when (seq iters)
          [:div.machinery-body
           (for [it iters]
             (list
               (mach-thinking (:thinking it))
               (for [form (or (:forms it) [])]
                 (let [ops (form-ops form)]
                   (list
                     (when-let [src (:src form)]
                       (when-not (or (str/blank? (str src)) (engine-verb-src? src))
                         (mach-code src)))
                     ;; A form whose tools rendered themselves shows the
                     ;; tool ops AND its own collapsed result row below.
                     ops
                     (cond
                       (and (:error form)
                         (not (form-error-covered-by-op? form)))
                       (mach-error (:error form))
                       ;; nil results are the engine's silent blocks
                       ;; (defs, imports) - noise, same rule as live. The
                       ;; "vis_silent" sentinel (task_set!/fact_set! mutators)
                       ;; and "vis_answer" are engine markers, never output.
                       (and (some? (:result form))
                         (not (contains? #{"vis_answer" "vis_silent"}
                                (:result form))))
                       ;; Show the result the way the MODEL reads it (recall
                       ;; window, rg gutter, shell model-render, else Python
                       ;; printer) - NOT pr-str'd Clojure. Same compression
                       ;; the live SSE path sends; degrades to the raw value.
                       (mach-result (try (vis/render-form-value (:src form) (:result form))
                                      (catch Throwable _ (:result form)))
                         (let [{:keys [started-at-ms finished-at-ms]} form]
                           (when (and (number? started-at-ms) (number? finished-at-ms))
                             (max 0 (- (long finished-at-ms) (long started-at-ms))))))
                       :else nil))))
               (when (> (count iters) 1)
                 (mach-iter-tick (:position it) (:duration-ms it)))))])))
    (catch Throwable _ nil)))

(defn- machinery-lazy
  "Collapsed machinery disclosure whose code/results body is fetched only
   when first expanded (hx-get on `toggle`). Keeps the initial page and
   scroll-up payloads tiny - the heavy code/results blob crosses the wire
   on demand. Rendered for any finished turn that ran at least one
   iteration."
  [turn]
  (when-let [tid (some-> (or (pick turn :engine_turn_id) (pick turn :turn_id)) str)]
    (let [sid   (pick turn :session_id)
          iters (pick turn :iteration_count)
          n     (if (number? iters) iters 1)]
      (when (pos? n)
        [:details.machinery
         [:summary.mach-sum.machinery-head
          [:span.mach-tag "machinery"]
          (when (number? iters)
            [:span.machinery-meta (str iters " iter" (when (not= 1 iters) "s"))])]
         [:div.machinery-body {:hx-get (str "/ui/session/" sid "/turn/" tid "/machinery")
                               :hx-trigger "toggle once from:closest details"
                               :hx-swap "outerHTML"}
          [:div.mach-loading "loading…"]]]))))

(defn- turn-block
  "One restored turn. `live-replay?` true means the turn is RUNNING and
   the SSE stream will replay its WHOLE machinery from the event ring -
   render only the user bubble (no DB machinery, no static dots bubble:
   the bottom #thinking ticker owns the typing indicator, so '...' never
   sits stale above the streaming content)."
  ([turn] (turn-block turn false))
  ([turn live-replay?]
   (let [status   (pick turn :status)
         running? (= "running" status)]
     (list
      [:div.tsep]
      (user-bubble (pick turn :request) (pick turn :started_at))
      (when-not (and running? live-replay?)
        (machinery-lazy turn))
      (cond
        (or (pick turn :answer_md) (pick turn :error))
        (vis-bubble turn)

         ;; running: the answer streams into #live and the bottom ticker
         ;; shows the dots - nothing static to pin here.
        running? nil

        :else
        [:div.bubble.b-vis
         [:div.role.role-vis "Vis"]
         [:p.empty (str "(" (or status "no answer") ")")]])))))

;; ── Footer — channel contribution slot `:web.slot/footer` ──────────
;; Extensions contribute footer items by declaring, on their extension
;; map:
;;   {:ext/channel-contributions
;;    {:web.slot/footer [{:id :my.ext/footer
;;                        :fn (fn [{:session/keys [id]}] -> IR | nil)}]}}
;; The fn returns CANONICAL IR (walked by ir->hiccup) — the same
;; contract as the TUI's :tui.slot/header-row, web-flavored.

(defn- footer-content [sid]
  (let [pref (vis/gateway-session-model sid)
        active (when-not pref
                 (try (vis/resolve-effective-model (vis/get-router))
                   (catch Throwable _ nil)))
        contribs (try (vis/channel-contributions-for :web :web.slot/footer)
                   (catch Throwable _ []))]
    [:footer.foot
     ;; The footer model is the per-session model surface: tap to open the
     ;; session-model picker. (Global "primary" lives in Providers → Models;
     ;; this overrides it for THIS session only.)
     [:button.foot-item.foot-model {:type "button"
                                    :hx-get (str "/ui/session/" sid "/model")
                                    :hx-target "#modal" :hx-swap "innerHTML"
                                    :aria-label "Change this session's model"}
      (icon "zap")
      [:span (or pref
               (when active
                 (str (some-> (:provider active) name) "/" (:name active)))
               "default model")]]
     (for [{:keys [id] f :fn} contribs]
       (when-let [ir (try (f {:session/id sid}) (catch Throwable _ nil))]
         [:span.foot-item {:data-contrib (str id)} (ir->hiccup ir)]))]))

(defn- user-bubble-html [text]
  (html (list [:div.tsep] (user-bubble text (System/currentTimeMillis)))))

(defn- vis-message-html
  "A full vis chat bubble from a terminal turn event — flies into the
   thread (`#live`), NOT the Work log. Same anatomy as restored turns."
  [event]
  (html (vis-bubble event)))

(declare sidebar-content)

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

(defn- code-snip [code]
  (let [s (str code)]
    (if (> (count s) 400) (str (subs s 0 400) " …") s)))

(defn- event->frames
  "One gateway event -> seq of `{:event name :html fragment}` for the
   htmx SSE extension (`sse-swap=\"<name>\"`)."
  [sid {:keys [type] :as event}]
  (case type
    ;; EVERYTHING flows into the thread (`message` -> #live, in arrival
    ;; order): user bubble (form response), machinery blocks, answer.
    ;; Nothing is folded away - TUI parity, the Work disclosure is gone.
    "turn.started"
    (into [{:event "thinking"
            :html (html (list [:div.dots [:span] [:span] [:span]]))}]
      ;; status flips to running -> header chip + sidebar dot light up
      (chrome-frames sid))

    "reasoning.delta"
    (let [t (str/trim (str (:text event)))]
      (when-not (str/blank? t)
        [{:event "thinking"
          :html (html [:div.mach.mach-thinking
                       [:span.mach-tag "thinking"]
                       [:div.mach-think-body.act-dim (code-snip t)]])}]))

    "block.started"
    (when-not (engine-verb-src? (:code event))
      [{:event "message" :html (html (mach-code (:code event)))}])

    "block.output"
    (let [ops (seq (:ops event))]
      (cond
        ;; Tool calls render through their extension's OWN render-fn
        ;; IR (`> LABEL ...` + display body) - never the raw blob.
        ops
        (-> (mapv (fn [op] {:event "message" :html (html (mach-tool op))}) ops)
          (into (when (:error event)
                  [{:event "message" :html (html (mach-error (:error event)))}]))
          ;; the tool card draws itself, but the FORM's own collapsed result
          ;; row still shows - real results are never hidden behind an op card
          (into (when (and (not (:silent event)) (some? (:result event)))
                  [{:event "message"
                    :html (html (mach-result (:result event) (:duration_ms event)))}])))

        (:error event)
        [{:event "message" :html (html (mach-error (:error event)))}]

        ;; :silent is the engine's own display contract (same as the TUI):
        ;; a silent block's result is noise (defs, nil), not output.
        (:silent event)
        nil

        :else
        (when (some? (:result event))
          [{:event "message"
            :html (html (mach-result (:result event) (:duration_ms event)))}])))

    "iteration.error"
    [{:event "message" :html (html (mach-error (:error event)))}]

    "iteration.completed"
    (let [snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))
          thought  (mach-thinking (:thinking event))]
      ;; The ctx mirror moves on every iteration boundary (facts, plan,
      ;; utilization) - refresh the rail mid-turn, not only at turn end.
      ;; The iteration's reasoning pins into the thread HERE (and the
      ;; ticker clears) so thinking stays readable after streaming.
      (cond-> []
  thought  (conj {:event "message" :html (html thought)})
  ;; the ticker RESETS to dots (not empty) - the turn is still running,
  ;; so the bottom indicator must survive the iteration boundary; only
  ;; turn.completed/failed clears it.
  true     (into [{:event "thinking"
                   :html (html [:div.dots [:span] [:span] [:span]])}
                  {:event "message"
                   :html (html (mach-iter-tick (:iteration event) nil))}])
  snapshot (conj {:event "context" :html (html (context-panel snapshot (session-plan-timeline sid)))})
  true     (into (chrome-frames sid))))

    ("turn.completed" "turn.failed")
    (let [snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))]
      (cond-> [{:event "thinking" :html ""}
               {:event "message" :html (vis-message-html event)}
               {:event "footer" :html (html (footer-content sid))}]
        snapshot (conj {:event "context" :html (html (context-panel snapshot (session-plan-timeline sid)))})
        ;; Re-render (or CLEAR - nil card -> empty fragment) the review
        ;; card at every turn boundary: a fresh proposal grows the card,
        ;; a resolved plan removes it, a revision replaces it.
        snapshot (conj {:event "planreview"
                        :html (html (or (plan-review-card sid snapshot) ""))})
        ;; the chip leaves `running` and the title may have just been
        ;; generated - re-render header + session drawer
        true     (into (chrome-frames sid))))

    ;; a session title changed (this one or another) - re-render the
    ;; header chip + the session drawer so generated titles land live,
    ;; even while the user is looking at a DIFFERENT session.
    "session.title_updated"
    (chrome-frames sid)

    nil))

(defn- query-from
  "The `?from=N` replay cursor of a stream/poll request; nil when absent."
  [request]
  (some->> (:query-string request)
    (re-find #"(?:^|&)from=(\d+)")
    second
    parse-long))

(defn- write-frame! [^OutputStream out {:keys [event html id]}]
  ;; `id:` carries the gateway event seq. ui.js tracks it
  ;; (MessageEvent.lastEventId) and rewinds reconnects to it via the
  ;; htmx.createEventSource override — without it a reconnect reuses the
  ;; page-render ?from= cursor and replays the whole page-life of frames
  ;; into #live again.
  (let [frame (str "event: " event "\n"
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
  (let [sid  (some-> (get-in request [:path-params :sid]) parse-uuid)
        ;; ?from=N pins the replay cursor at the PAGE's render seq (the page
        ;; computed it; see session-page) - without it we degrade to the old
        ;; live-only behavior.
        from (query-from request)
        ;; A forwarding header means an edge proxy sits between us and the
        ;; client (cloudflared stamps cf-ray/cf-connecting-ip) — only then is
        ;; the anti-buffering pad worth its bytes.
        proxied? (boolean (some #(get-in request [:headers %])
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
       :body
       (reify ring-protocols/StreamableResponseBody
         (write-body-to-stream [_ _ output-stream]
           (let [^OutputStream out output-stream
                 sub-id (str (java.util.UUID/randomUUID))
                 sink   (fn [event]
                          (locking out
                            (doseq [frame (event->frames sid event)]
                              ;; stamp the gateway seq so the client can
                              ;; rewind a reconnect to it (pings carry none —
                              ;; an id-less frame leaves lastEventId alone)
                              (write-frame! out (assoc frame :id (:seq event))))
                            (.flush out)))]
             (try
               (locking out
                 ;; 8KB SSE comment pad (clients ignore comments): proxy
                 ;; edges (Cloudflare tunnel) buffer a streaming body until
                 ;; a byte threshold — without the pad the first real frames
                 ;; sit in the edge buffer and live streaming looks dead.
                 ;; ONLY for proxied requests; a direct localhost client
                 ;; needs no pad and shouldn't pay the bytes.
                 (when proxied?
                   (.write out (.getBytes (str ": " (apply str (repeat 8192 " ")) "\n\n")
                                 StandardCharsets/UTF_8))
                   (.flush out))
                 ;; Immediate NAMED ping (not a comment): the page's
                 ;; watchdog (#ssewatch + ui.js) listens for it — a healthy
                 ;; stream proves itself within a second; silence means an
                 ;; edge proxy is buffering the body and ui.js falls back
                 ;; to polling /ui/session/:sid/poll.
                 (write-frame! out {:event "ping" :html ""})
                 (.flush out)
                 (doseq [event (vis/gateway-subscribe! sid sub-id sink
                        (or from (vis/gateway-current-seq sid)))]
                   (sink event)))
               (loop []
                 (Thread/sleep (long HEARTBEAT_MS))
                 (locking out
                   (write-frame! out {:event "ping" :html ""})
                   (.flush out))
                 (recur))
               (catch Throwable _ nil)
               (finally
                 (vis/gateway-unsubscribe! sid sub-id)
                 (try (.close out) (catch Throwable _ nil)))))))})))

(defn- poll-handler
  "GET /ui/session/:sid/poll?from=N — the SSE stream's PULL twin for
   clients whose stream an edge proxy silently buffers (free Cloudflare
   quick tunnels hold SSE bodies forever: 200 + text/event-stream + zero
   bytes). Returns the SAME named HTML fragments the stream would have
   pushed since cursor N, in order, plus the next cursor:
     {\"next\": M, \"frames\": [{\"event\": e, \"html\": h}, …]}
   ui.js (sse watchdog) applies each frame to its [sse-swap=e] target
   exactly like the htmx SSE extension would — same renderers, pulled."
  [request]
  (let [sid  (some-> (get-in request [:path-params :sid]) parse-uuid)
        from (or (query-from request) 0)]
    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "application/json; charset=utf-8"}
       :body "{\"error\":\"unknown session\"}"}
      (let [events   (vis/gateway-events-since sid from)
            frames   (into [] (mapcat #(event->frames sid %)) events)
            next-seq (long (or (:seq (peek events)) from))]
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"
                   "Cache-Control" "no-cache, no-transform"}
         :body (str "{\"next\":" next-seq ",\"frames\":["
                 (str/join "," (map #(json-text (select-keys % [:event :html])) frames))
                 "]}")}))))

;; =============================================================================
;; Pages
;; =============================================================================

(defn- token-form-page
  "The connect screen - a centered glass card over soft gold orbs. The
   bearer token is REQUIRED here (`required` + server-side check); the
   form contract stays POST /ui/auth with the `token` field."
  [& [error]]
  (page "connect"
    [:main.auth
     [:div.auth-orb {:aria-hidden "true"}]
     [:div.auth-orb.auth-orb-2 {:aria-hidden "true"}]
     [:div.auth-card
      [:div.auth-mark "vis"]
      [:p.tagline "see it think"]
      [:div.auth-lock
       [:svg {:viewBox "0 0 24 24" :width "14" :height "14" :fill "none"
              :stroke "currentColor" :stroke-width "2"
              :stroke-linecap "round" :stroke-linejoin "round"}
        [:rect {:x "3" :y "11" :width "18" :height "11" :rx "2"}]
        [:path {:d "M7 11V7a5 5 0 0 1 10 0v4"}]]
       [:span "bearer token required"]]
      (when error [:p.auth-error error])
      [:form {:method "post" :action "/ui/auth"}
       [:input.auth-input {:type "password" :name "token"
                           :placeholder "paste your gateway token"
                           :autofocus true :autocomplete "off"
                           :autocapitalize "off" :spellcheck "false"
                           :required true :aria-label "gateway bearer token"}]
       [:button.auth-go {:type "submit"} "Connect"
        [:svg {:viewBox "0 0 24 24" :width "15" :height "15" :fill "none"
               :stroke "currentColor" :stroke-width "2"
               :stroke-linecap "round" :stroke-linejoin "round"}
         [:path {:d "M5 12h14"}]
         [:path {:d "m12 5 7 7-7 7"}]]]]
      [:p.auth-hint "the token lives at " [:code "~/.vis/gateway.token"] " on the host"]]]))

(defn- sidebar-content
  "Children of the session drawer - extracted so the SSE `sidebar` frame
   can re-render titles and running dots without replacing the <aside>
   (which carries the sse-swap target itself). Select-mode (bulk delete)
   is a CSS class on the <aside> toggled in ui.js, so it SURVIVES the
   SSE innerHTML re-render."
  [active-sid]
  (list
    [:div.side-head
 [:form.newchat {:method "post" :action "/ui/sessions"}
  [:button.newchat-btn {:type "submit"}
   [:span.newchat-plus "+"] "New session"]]
 [:button.side-select-toggle {:type "button" :data-select-toggle "1"
                              :aria-label "Select sessions"}
  [:span.when-idle "Select"]
  [:span.when-select "Done"]]]

    [:ul.side-sessions
     (for [{:keys [id title status]} (vis/gateway-list-sessions)]
       [:li.side-item
       ;; bulk-delete checkbox - hidden until the aside carries
       ;; .select-mode; checked rows ride hx-include to the confirm modal
        [:input.side-check {:type "checkbox" :name "sid" :value (str id)
                            :aria-label "Select session"}]
        [:a {:class (str "side-row" (when (= (str id) (str active-sid)) " active"))
             :href (str "/ui/session/" id)}
         [:span.side-title (or title "Untitled")]
         (when (= status "running") [:span.side-dot])]
        ;; hover-revealed delete - DELETE /ui/session/:sid (the gateway
        ;; disposes the live env and deletes the DB tree; TUI Ctrl+D parity)
        [:button.side-del {:type "button" :aria-label "Delete session"
                           :hx-get (str "/ui/session/" id "/delete")
                           :hx-target "#modal" :hx-swap "innerHTML"}
         (icon "x")]])]
   ;; select-mode action bar - the confirm modal receives the checked ids
   ;; as repeated `sid` query params via hx-include
    [:div.side-bulkbar
     [:button.btn-danger.side-bulk-del {:type "button" :disabled true
                                        :hx-get "/ui/sessions/delete"
                                        :hx-include ".side-check:checked"
                                        :hx-target "#modal" :hx-swap "innerHTML"}
      "Delete selected"]]
    ;; config actions live at the BOTTOM of the sidebar (margin-top:auto), not
    ;; in the cramped mobile header.
    [:div.side-foot
     [:button.side-foot-btn {:type "button" :aria-label "Providers"
                             :hx-get (str "/ui/session/" active-sid "/providers")
                             :hx-target "#modal" :hx-swap "innerHTML"}
      (icon "zap") [:span "Providers"]]
     [:button.side-foot-btn {:type "button" :aria-label "Settings"
                             :hx-get "/ui/settings" :hx-target "#modal" :hx-swap "innerHTML"}
      (icon "settings") [:span "Settings"]]]))

(defn- sessions-sidebar
  "Left rail: the session drawer. The active session is highlighted; a
   running one carries a gold pulse dot. SSE re-renders the contents on
   every turn boundary (`sidebar` frame)."
  [active-sid]
  [:aside.sidebar {:sse-swap "sidebar" :hx-swap "innerHTML"}
   (sidebar-content active-sid)])

(defn- session-page [sid]
  (let [soul     (vis/gateway-soul sid)
 turns    (reverse (vis/gateway-list-turns sid))
 window   (vec (take-last INITIAL_TURN_WINDOW turns))
 older?   (> (count turns) (count window))
 oldest-tid (some-> (first window) (pick :turn_id) str)
 snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))
 running? (boolean (some #(= "running" (pick % :status)) turns))
 ;; The SSE cursor pins at PAGE RENDER (not at connect) so nothing
 ;; falls into the render->connect gap. A RUNNING turn rewinds to
 ;; just before its own turn.started, so the WHOLE in-flight turn
 ;; replays into #live - a refresh mid-stream loses nothing.
 page-seq (vis/gateway-current-seq sid)
 run-seq  (when running?
            (some->> (try (vis/gateway-events-since sid 0)
                          (catch Throwable _ nil))
                     (filter #(= "turn.started" (:type %)))
                     last :seq dec))
 from     (or run-seq page-seq)
 live-replay? (some? run-seq)]
    (page (or (:title soul) "session")
      [:div.app {:hx-ext "sse"
 :sse-connect (str "/ui/session/" sid "/stream?from=" from)
                 :data-sid (str sid) :data-from (str from)}
       ;; SSE watchdog target: the stream pings this (hx-swap none) so
       ;; ui.js can tell a LIVE stream from one an edge proxy buffers —
       ;; silence after connect flips the page to /poll pulling.
       [:div#ssewatch {:sse-swap "ping" :hx-swap "none" :hidden true}]
       [:header.bar
        [:button#toggle-left.bar-toggle {:type "button" :aria-label "Toggle sessions"}
         (icon "sidebar")]
        [:div.bar-title {:sse-swap "bartitle" :hx-swap "innerHTML"}
         (bar-title-content soul)]
        [:span.session-id (subs (str sid) 0 8)]
        ;; Providers + Settings moved to the sidebar foot (sessions-sidebar) —
        ;; the header keeps only the two rail toggles.
        [:button#toggle-right.bar-toggle {:type "button" :aria-label "Toggle context"}
         (icon "layers")]]
       [:div#modal]
       ;; mobile-drawer backdrop: dim + tap-to-close when the sidebar/rail is open
       [:div.scrim {:aria-hidden "true"}]
       [:div.layout
        (sessions-sidebar sid)
        ;; ONE center flex column holds the thread AND the composer dock,
        ;; so both center in the SAME box between the rails — the input
        ;; can never drift out of line with the chat column again.
        [:div.center
         [:main.thread
          [:div.column
           (if (seq turns)
             (list
               (when older? (older-sentinel sid oldest-tid))
               (map #(turn-block % live-replay?) window))
             [:div.hello-wrap
              [:h1.hello "What are we building?"]
              [:p.hello-sub "vis works in this workspace — ask for anything."]])
           ;; Live bubbles land here (user message from the form response,
           ;; the answer from the `message` SSE event). Work below holds
           ;; ONLY machinery: code, results, iteration ticks.
           [:div#live.live {:sse-swap "message" :hx-swap "beforeend"}]
           (plan-review-slot sid snapshot)
           [:div#thinking.thinking {:sse-swap "thinking" :hx-swap "innerHTML"}
 (when running? [:div.dots [:span] [:span] [:span]])]
           [:div.thread-tail]]]
         [:div.dock
          [:div#suggest.suggest {:hidden true}]
          [:form.composer {:hx-post (str "/ui/session/" sid "/turns")
                           :hx-target "#live" :hx-swap "beforeend"
                           :data-files-url (str "/ui/session/" sid "/files")
                           "hx-on::after-request" "if(event.detail.successful) this.reset()"}
           [:textarea {:name "request" :rows 1
                       :placeholder "Ask vis…"}]
           [:button.mic {:type "button" :aria-label "Dictate"
                         :data-voice-url (str "/ui/session/" sid "/voice")}
            (icon "mic")]
           [:button.send {:type "submit" :aria-label "Send"} (icon "arrow-up")]]]
         [:div#footwrap {:sse-swap "footer" :hx-swap "innerHTML"}
          (footer-content sid)]]
        [:aside.rail {:sse-swap "context" :hx-swap "innerHTML"}
         (if snapshot
           (context-panel snapshot (session-plan-timeline sid))
           [:div#context.context
            [:div.rail-head
             [:button.rail-close {:type "button" :data-close-drawer "1" :aria-label "Close context"}
              (icon "x")]]
            [:p.empty "wakes on the first turn"]])]]])))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- cookie-token [request]
  (get-in request [:cookies "vis_token" :value]))

(defn- ui-authed?
  "True when the gateway runs authless (the loopback default) or the
   request carries the gateway token as the browser cookie. Registered
   as the contribution's :request-authed-fn."
  [request ^String token]
  (or (not (vis/gateway-auth-required?))
    (= token (cookie-token request))))

(defn- epoch-of [soul]
  (let [v (or (:last_active_at soul) (:created_at soul))]
    (cond
      (number? v) (long v)
      (instance? java.util.Date v) (.getTime ^java.util.Date v)
      :else 0)))

(defn- index-handler
  "GET /ui - jumps STRAIGHT into the most recent conversation (creating
   one on a fresh install); there is no separate home page — the
   sessions drawer is the navigation. Unauthed (gate on): token form.
   Also kicks the dev source watcher alive on first open."
  [request token]
  (if-not (ui-authed? request token)
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (token-form-page)}
    (let [sessions (vis/gateway-list-sessions)
          target (if (seq sessions)
                   (:id (apply max-key epoch-of sessions))
                   (:id (vis/gateway-create-session! {})))]
      {:status 303 :headers {"Location" (str "/ui/session/" target)} :body ""})))

(defn- auth-handler
  "POST /ui/auth - exchange the bearer token for the HttpOnly cookie."
  [request token]
  (if (= token (some-> (get-in request [:form-params "token"]) str/trim))
    {:status 303
     :headers {"Location" "/ui"}
     :cookies {"vis_token" {:value token :http-only true
                            :same-site :lax :path "/"}}
     :body ""}
    {:status 401
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (token-form-page "that token does not match — check ~/.vis/gateway.token")}))

(defn- create-session-handler
  "POST /ui/sessions - create and bounce to the session page."
  [request]
  (let [title (let [t (str (get-in request [:form-params "title"]))]
                (when-not (str/blank? t) t))
        {:keys [id]} (vis/gateway-create-session! {:title title})]
    {:status 303 :headers {"Location" (str "/ui/session/" id)} :body ""}))

(defn- session-handler [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (if (and sid (vis/gateway-soul sid))
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (session-page sid)}
      {:status 303 :headers {"Location" "/ui"} :body ""})))

(defn- delete-session-ui-handler
  "DELETE /ui/session/:sid — permanently delete a session from the
   sidebar. `gateway-close-session!` disposes the live environment and
   deletes the DB tree (same path as the TUI's Ctrl+D). Redirect: when
   the OPEN session was deleted, land on /ui (it re-picks the most
   recent session); deleting another row stays on the current page.
   htmx ships the page URL in HX-Current-URL — no query plumbing."
  [request]
  (let [sid     (some-> (get-in request [:path-params :sid]) parse-uuid)
        current (some->> (get-in request [:headers "hx-current-url"])
                  (re-find #"/ui/session/([0-9a-fA-F-]{36})")
                  second
                  parse-uuid)]
    (when sid (vis/gateway-close-session! sid))
    {:status 200
     :headers {"HX-Redirect" (if (and current (not= current sid))
                               (str "/ui/session/" current)
                               "/ui")}
     :body ""}))

(defn- sid-params
  "Normalize a repeated `sid` request param - Ring hands back a STRING
   for one value and a VECTOR for many - into a seq of parsed UUIDs."
  [v]
  (->> (if (coll? v) v [v])
    (keep #(some-> % str parse-uuid))))

(defn- delete-sessions-bulk-handler
  "POST /ui/sessions/delete - permanently delete EVERY checked session
   (sidebar select-mode). Each sid rides the same close path as the
   single delete (`gateway-close-session!` disposes the live env and
   deletes the DB tree). Redirect: when the OPEN session was among the
   deleted, land on /ui (it re-picks the most recent session); else
   stay on the current page."
  [request]
  (let [sids    (set (sid-params (get-in request [:form-params "sid"])))
        current (some->> (get-in request [:headers "hx-current-url"])
                  (re-find #"/ui/session/([0-9a-fA-F-]{36})")
                  second
                  parse-uuid)]
    (doseq [sid sids] (vis/gateway-close-session! sid))
    {:status 200
     :headers {"HX-Redirect" (if (and current (not (contains? sids current)))
                               (str "/ui/session/" current)
                               "/ui")}
     :body ""}))

(defn- slash-bubble [result]
  (html
    (list
      [:div.bubble.b-vis
       [:div.role.role-vis "Vis"]
       (if-let [error (:error result)]
         [:p.empty.slash-error (str error)]
         [:div.prose.md (ir->hiccup (or (:ir result) (:result result)
                                      [:p "done"]))])])))

(defn- run-slash
  "Dispatch a /command through the engine's slash machinery — the same
   `slash/dispatch` the TUI and Telegram ride, with this channel's id."
  [sid text]
  (let [env (vis/env-for sid)]
    (vis/slash-dispatch env
      {:channel/id :web
       :session/id sid
       :db-info (:db-info env)}
      text)))

(defn- submit-turn-handler
  "POST /ui/session/:sid/turns (htmx form). A leading `/` dispatches the
   engine slash machinery and answers inline (no LLM turn); anything
   else submits a turn — the live stream carries what follows."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        text (str/trim (str (get-in request [:form-params "request"])))]
    (if (and sid (str/starts-with? text "/"))
      (let [result (try (run-slash sid text)
                     (catch Throwable t {:handled? true :error (ex-message t)}))]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (str (user-bubble-html text)
                 (if (:handled? result)
                   (slash-bubble result)
                   (slash-bubble {:error (str "unknown command: " text)})))})
      (let [result (when sid (vis/gateway-submit-turn! sid {:request text}))]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (cond
                 (:turn result)
                 (user-bubble-html text)

                 (= :turn-in-progress (:error result))
                 (html [:div.bubble.b-vis [:div.role.role-vis "Vis"]
                        [:p.empty "a turn is already running — wait for it to finish"]])

                 :else
                 (html [:div.bubble.b-vis [:div.role.role-vis "Vis"]
                        [:p.empty (str "rejected: " (or (:message result) "invalid request"))]]))}))))

(defn- plan-review-handler
  "POST /ui/session/:sid/plan-review (the card's htmx form). Reads the
   verdict radios (`verdict_<key>`) + comment boxes (`note_<key>`) +
   `overall`, compiles them through the SAME `vis/plan-review-message`
   grammar the TUI dialog submits, and sends the result as the next
   user turn. Step keys come from the LIVE snapshot (plan order), so a
   stale form field for a step the model has since dropped is ignored.
   Response: the user bubble for `#live` + an out-of-band swap that
   empties `#planreview` (the SSE `planreview` frame re-grows it if the
   model re-proposes)."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        params (:form-params request)
        snapshot (when sid (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil)))
        steps (when snapshot (vis/plan-reviewable (pick snapshot :session/tasks)))
        entries (into []
                  (keep (fn [{:keys [key candidate?]}]
                          (when candidate?
                            (let [verdict (case (str (get params (str "verdict_" key)))
                                            "approve" :approve
                                            "reject" :reject
                                            nil)
                                  note (let [n (str/trim (str (get params (str "note_" key))))]
                                         (when-not (str/blank? n) n))
                                  verdict (or verdict (when note :comment))]
                              (when verdict {:key key :verdict verdict :note note})))))
                  steps)
        msg (vis/plan-review-message entries (get params "overall"))
        vis-note (fn [text] (html [:div.bubble.b-vis [:div.role.role-vis "Vis"]
                                   [:p.empty text]]))]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (cond
             (nil? sid) (vis-note "unknown session")

             (nil? msg)
             (vis-note "nothing to send — pick a verdict or write a comment first")

             :else
             (let [result (vis/gateway-submit-turn! sid {:request msg})]
               (cond
                 (:turn result)
                 ;; The submitted review reads back as a normal user
                 ;; bubble (the canonical message IS the review), and the
                 ;; oob fragment collapses the card right away.
                 (str (user-bubble-html msg)
                   (html [:div#planreview.planreview-slot
                          {:sse-swap "planreview" :hx-swap "innerHTML"
                           :hx-swap-oob "true"}]))

                 (= :turn-in-progress (:error result))
                 (vis-note "a turn is already running — wait for it to finish")

                 :else
                 (vis-note (str "rejected: " (or (:message result) "invalid request"))))))}))

(defn- slash-list-handler
  "GET /ui/slash — top-level, non-hidden slash specs for the composer's
   `/` autocomplete."
  [_request]
  (let [specs (->> (vis/registered-slashes)
                (remove :slash/hidden?)
                (filter #(empty? (:slash/parent %)))
                (map (fn [s] {:name (str "/" (:slash/name s))
                              :doc (str (:slash/doc s))})))]
    {:status 200 :headers {"Content-Type" "application/json; charset=utf-8"}
     :body (str "[" (str/join "," (map #(json-text %) specs)) "]")}))

(defn- files-handler
  "GET /ui/session/:sid/files?q= — fuzzy-ish file list for the
   composer's `@` file picker, from the engine's file-picker index."
  [request]
  (let [q (str/lower-case (str (get-in request [:query-params "q"])))
        entries (try
                  ((requiring-resolve
                     'com.blockether.vis.internal.file-picker/collect-file-picker-entries))
                  (catch Throwable _ []))
        paths (->> entries
                (remove #(or (:ignored? %) (:ignored %)))
                (keep (fn [e] (or (:display-path e) (some-> (:path e) str))))
                (filter #(or (str/blank? q)
                           (str/includes? (str/lower-case %) q)))
                (sort-by count)
                (take 20))]
    {:status 200 :headers {"Content-Type" "application/json; charset=utf-8"}
     :body (str "[" (str/join "," (map pr-str paths)) "]")}))

;; =============================================================================
;; Modals: Settings (toggles) + Providers — the TUI dialogs, as overlays
;; =============================================================================

(defn- toggle-id->wire [id] (str (namespace id) "/" (name id)))
(defn- wire->toggle-id [s]
  (let [[ns* n] (str/split (str s) #"/" 2)]
    (when (and ns* n) (keyword ns* n))))

(defn- toggle-row
  "One settings row. Boolean toggles render a switch; enum toggles show
   the current value and a cycle button. The row swaps itself on change."
  [{:keys [id label description]}]
  (let [wire-id (toggle-id->wire id)
        choices (try (vis/toggle-choices id) (catch Throwable _ nil))]
    [:div.toggle-row {:id (str "tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))}
     [:div.toggle-text
      [:div.toggle-label (str (or label id))]
      (when description [:div.toggle-desc (str description)])]
     (if (seq choices)
       [:button.toggle-cycle
        {:type "button"
         :hx-post "/ui/settings/cycle" :hx-vals (json-text {:id wire-id})
         :hx-target (str "#tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))
         :hx-swap "outerHTML"}
        (str (try (vis/toggle-value id) (catch Throwable _ "?")))]
       [:button {:type "button"
                 :class (str "switch" (when (try (vis/toggle-enabled? id)
                                              (catch Throwable _ false)) " on"))
                 :aria-label (str "Toggle " (or label id))
                 :hx-post "/ui/settings/toggle" :hx-vals (json-text {:id wire-id})
                 :hx-target (str "#tg-" (str/replace wire-id #"[^a-zA-Z0-9]" "-"))
                 :hx-swap "outerHTML"}
        [:span.knob]])]))

(defn- modal-shell [title & body]
  (html
    [:div.overlay {:data-close-modal "backdrop"}
     [:div.modal
      [:div.modal-head
       [:h2 title]
       [:button.bar-toggle {:type "button" :data-close-modal "x" :aria-label "Close"}
        (icon "x")]]
      (into [:div.modal-body] body)]]))

(defn- delete-session-confirm-handler
  "GET /ui/session/:sid/delete — styled confirm dialog replacing the
   native `hx-confirm` browser prompt. Cancel closes the modal; the
   danger button issues the real DELETE (handler semantics unchanged)."
  [request]
  (let [sid   (some-> (get-in request [:path-params :sid]) parse-uuid)
        title (some #(when (= (str (:id %)) (str sid)) (:title %))
                (vis/gateway-list-sessions))]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Delete session"
             [:div.confirm-del
              [:p.confirm-del-text
               "Delete " [:strong (or title "Untitled")] "?"
               [:br]
               "This permanently removes the session and its history."]
              [:div.confirm-del-actions
               [:button.btn-ghost {:type "button" :data-close-modal "x"}
                "Cancel"]
               [:button.btn-danger {:type "button"
                                    :hx-delete (str "/ui/session/" sid)
                                    :hx-swap "none"}
                "Delete session"]]])}))

(defn- delete-sessions-confirm-handler
  "GET /ui/sessions/delete - confirm dialog for the sidebar's bulk
   select-mode. htmx `hx-include` ships every checked checkbox as a
   repeated `sid` query param; the danger button re-posts the same ids
   as hidden inputs to the real bulk DELETE."
  [request]
  (let [sids (sid-params (get-in request [:query-params "sid"]))
        n    (count sids)]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (if (zero? n)
             (modal-shell "Delete sessions"
               [:div.confirm-del
                [:p.confirm-del-text "No sessions selected."]
                [:div.confirm-del-actions
                 [:button.btn-ghost {:type "button" :data-close-modal "x"}
                  "Close"]]])
             (modal-shell "Delete sessions"
               [:form.confirm-del {:hx-post "/ui/sessions/delete"
                                   :hx-swap "none"}
                (for [sid sids]
                  [:input {:type "hidden" :name "sid" :value (str sid)}])
                [:p.confirm-del-text
                 "Delete " [:strong (str n (if (= n 1) " session" " sessions"))] "?"
                 [:br]
                 "This permanently removes them and their history."]
                [:div.confirm-del-actions
                 [:button.btn-ghost {:type "button" :data-close-modal "x"}
                  "Cancel"]
                 [:button.btn-danger {:type "submit"}
                  "Delete selected"]]]))}))

(defn- settings-handler
  "GET /ui/settings — the TUI settings dialog as an overlay: every
   VISIBLE registered toggle, grouped, flippable in place.
   Provider-specific knobs (e.g. OpenAI Codex verbosity) declare a
   `:visible-fn` and only appear when their provider is configured."
  [_request]
  (let [toggles (try (vis/visible-toggles) (catch Throwable _ []))
        grouped (group-by #(or (:group %) :other) toggles)]
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Settings"
             (for [[group specs] (sort-by (comp str key) grouped)]
               [:section.modal-section
                [:h3 (name group)]
                (map toggle-row specs)]))}))

(defn- settings-mutate-handler
  "POST /ui/settings/toggle | /ui/settings/cycle - flip or cycle one
   toggle, answer with the refreshed row. A theme change ALSO swaps the
   `#theme-css` stylesheet <link> out-of-band with a cache-busted href,
   so the new theme paints immediately - no manual refresh needed."
  [request]
  (let [id (wire->toggle-id (get-in request [:form-params "id"]))
        cycle? (str/ends-with? (str (:uri request)) "/cycle")]
    (if-not id
      {:status 400 :headers {"Content-Type" "text/html"} :body "bad toggle id"}
      (let [spec (some #(when (= id (:id %)) %)
                       (try (vis/registered-toggles) (catch Throwable _ [])))]
        (try
          (if cycle?
            (vis/toggle-cycle-value! id)
            (vis/toggle-set-enabled! id (not (vis/toggle-enabled? id))))
          (catch Throwable _ nil))
        {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (html
                (list
                 (toggle-row (or spec {:id id :label (str id)}))
                 (when (= id :vis-channel-web/theme)
                   [:link {:id "theme-css" :rel "stylesheet"
                           :href (str "/ui/app.css?v=" asset-version
                                      "&t=" (System/currentTimeMillis))
                           :hx-swap-oob "true"}])))}))))

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
  [:button.modal-back {:type "button"
                       :hx-get url :hx-target "#modal" :hx-swap "innerHTML"}
   "‹ " label])

(defn- provider-card
  "One fleet card, the TUI `draw-provider-card!` in HTML:
     line 1: (N) Label            host ●
     line 2: ★ primary (+N models) / quota summary   — or the error
     chips:  session-scoped model picker
     row:    Models · Status · Key · Remove

   `diag` is `{:status .. :limits ..}` or nil; nil renders a skeleton
   that pulls its own diagnostics (`hx-trigger load`), so the modal
   opens instantly and the cards light up as probes answer — the web
   twin of the TUI's worker-future refresh."
  [sid {:keys [id models] :as provider} idx diag]
  (let [base     (providers-base sid)
        pid      (name id)
        label    (vis/display-label id)
        host     (vis/provider-url-host (or (vis/provider-base-url provider) ""))
        loading? (nil? diag)
        status   (:status diag)
        limits   (:limits diag)
        ok?      (boolean (:authenticated? status))
        error    (when diag (or (:error status) (get-in limits [:error :message])))
        summary  (when diag
                   (or (vis/limits-dynamic-summary limits)
                     (some->> (get-in limits [:static :rpm]) (str "catalog RPM "))))
        primary  (or (:name (first models)) "--")
        suffix   (if (<= (count models) 1)
                   "(1 model)"
                   (str "(+" (dec (count models)) " models)"))
        act      (fn [label* attrs]
                   [:button.pcard-act (merge {:type "button"
                                              :hx-target "#modal" :hx-swap "innerHTML"}
                                        attrs)
                    label*])]
    [:div.pcard
     (cond-> {:id (str "pcard-" pid)}
       loading? (assoc :hx-get (str base "/p/" pid "/diag")
                  :hx-trigger "load" :hx-swap "outerHTML"))
     [:div.pcard-line
      [:span.pcard-pri (str "(" (inc idx) ")")]
      [:span.pcard-label label]
      [:span.pcard-host host]
      [:span {:class (str "provider-dot"
                       (cond loading? "" ok? " on" :else " bad"))}]]
     (if error
       [:div.pcard-err (str "⚠ " error)]
       [:div.pcard-sub
        [:span.pcard-primary (str "★ " primary)]
        [:span.pcard-count suffix]])
     ;; limits ride a DEDICATED line that exists in EVERY card state
     ;; (skeleton, loaded, no-data) — the async diag swap can't change
     ;; the card height, so the modal never jumps as probes answer
     (when-not error
       [:div.pcard-limits-line
        (cond
          loading? [:span.pcard-checking "checking auth / limits…"]
          summary  [:span.pcard-limits summary]
          :else    [:span.pcard-limits.pcard-nodata "no quota data"])])
     [:div.pcard-acts
      (act "Models" {:hx-get (str base "/p/" pid "/models")})
      (act "Status" {:hx-get (str base "/p/" pid "/status")})
      (when (= :api-key (vis/provider-auth-kind id))
        (act "API key" {:hx-get (str base "/p/" pid "/key")}))
      (act "Remove" {:hx-post (str base "/p/" pid "/remove")
                     :hx-confirm (str "Remove " label "?")})]]))

(defn- providers-modal
  "Providers dialog: the session model preference + the persisted
   provider fleet (the TUI Router), with add/manage/remove."
  [sid]
  (let [providers (vis/configured-providers)
        pref      (vis/gateway-session-model sid)
        default-active (try (vis/resolve-effective-model (vis/get-router))
                         (catch Throwable _ nil))]
    (modal-shell "Providers"
      ;; No standalone "router default" chip: on the default route the
      ;; session line ALREADY names what the router resolves to — a
      ;; second router button was noise. The reset affordance appears
      ;; only while a session override is set.
      ;; READ-ONLY here: this card manages the GLOBAL fleet + ★ Primary.
      ;; The per-session model is chosen from the footer (tap the model name).
      [:p.active-model
       "This session: "
       [:strong (or pref (str (some-> (:provider default-active) name)
                           "/" (:name default-active) " (default)"))]
       [:span.active-model-hint " · change from the footer"]]
      [:div.pcards
       (if (seq providers)
         (map-indexed (fn [idx provider] (provider-card sid provider idx nil))
           providers)
         [:p.empty "No providers configured yet."])]
      [:button.add-provider {:type "button"
                             :hx-get (str (providers-base sid) "/add")
                             :hx-target "#modal" :hx-swap "innerHTML"}
       "+ Add provider"])))

(defn- session-model-picker
  "Footer-opened per-session model chooser — the ONE place a session's model
   is set. Lists every configured model as a chip (the active one
   highlighted) plus a `router default` reset. Picking POSTs /provider, which
   hoists that model to the router root for this session (see
   `prepare-turn-context`), OOB-refreshes the footer, and re-renders this
   picker. Distinct from the provider cards' ★ Primary, which sets the
   GLOBAL default for every session."
  [sid]
  (let [providers (vis/configured-providers)
        pref      (vis/gateway-session-model sid)
        default-active (try (vis/resolve-effective-model (vis/get-router))
                         (catch Throwable _ nil))
        chip (fn [model-name label]
               [:button {:type "button"
                         :class (str "model-chip" (when (= model-name pref) " current"))
                         :hx-post (str "/ui/session/" sid "/provider")
                         :hx-vals (json-text {:model (or model-name "")})
                         :hx-target "#modal" :hx-swap "innerHTML"}
                label])]
    (modal-shell "Session model"
      [:p.active-model "This session: "
       [:strong (or pref (str (some-> (:provider default-active) name)
                           "/" (:name default-active) " (default)"))]]
      (if (seq providers)
        [:div.model-chips.pick
         (chip "" "★ router default")
         (for [p providers
               m (:models p)
               :let [nm (:name m)]
               :when nm]
           (chip nm (str (name (:id p)) " / " nm)))]
        [:p.empty "No providers configured yet — add one under Providers."]))))

(defn- configured-provider [pid]
  (some #(when (= pid (:id %)) %) (vis/configured-providers)))

(defn- preset-by-id [pid]
  (some #(when (= pid (:id %)) %) (vis/provider-presets)))

(defn- add-provider-picker
  "Step 1 of Add Provider: the preset list (`vis/provider-presets-available`
   — same source as the TUI picker), each row labeled with how it
   authenticates."
  [sid]
  (let [base (providers-base sid)
        available (vis/provider-presets-available)]
    (modal-shell "Add Provider"
      (modal-back base "Providers")
      (if (empty? available)
        [:p.empty "All providers already configured."]
        [:div.preset-rows
         (for [{:keys [id label]} available]
           [:button.preset-row {:type "button"
                                :hx-get (str base "/add/" (name id))
                                :hx-target "#modal" :hx-swap "innerHTML"}
            [:span.preset-label label]
            [:span.preset-kind (case (vis/provider-auth-kind id)
                                 :oauth "Sign in"
                                 :none  "local"
                                 "API key")]])]))))

(defn- add-model-picker
  "Model selection for a preset being added: live-fetched catalog +
   preset defaults through `vis/provider-model-options` — the same
   list the TUI shows, with the same 'Show all models…' affordance."
  [sid preset api-key show-all?]
  (let [base    (providers-base sid)
        pid     (:id preset)
        probe   (cond-> {:id pid
                         :base-url (:base-url preset)
                         :default-models (:default-models preset)}
                  (seq api-key) (assoc :api-key api-key))
        {:keys [models hidden-count]}
        (vis/provider-model-options probe (:default-models preset) show-all?)]
    (modal-shell (str (:label preset) " — Select Model")
      (modal-back (str base "/add") "Add Provider")
      (if (empty? models)
        [:p.empty "No models reported. Is the provider reachable / the key valid?"]
        [:div.model-chips.pick
         (for [m models]
           [:button.model-chip {:type "button"
                                :hx-post (str base "/add/" (name pid) "/confirm")
                                :hx-vals (json-text {:model m :api_key (or api-key "")})
                                :hx-target "#modal" :hx-swap "innerHTML"}
            m])])
      (when (and (not show-all?) (pos? hidden-count))
        [:button.show-all {:type "button"
                           :hx-post (str base "/add/" (name pid) "/models")
                           :hx-vals (json-text {:api_key (or api-key "") :show_all "1"})
                           :hx-target "#modal" :hx-swap "innerHTML"}
         (str "Show all models… (" hidden-count " hidden)")]))))

(defn- add-provider-step
  "Step 2 of Add Provider, by auth kind:
     :api-key → key form (masked), then the model picker
     :none    → straight to the model picker (local endpoint)
     :oauth   → add directly when the provider extension already holds
                credentials; otherwise point at the extension's own
                flow (TUI / `vis providers auth`) with a re-check."
  [sid pid]
  (let [base   (providers-base sid)
        preset (preset-by-id pid)]
    (if-not preset
      (providers-modal sid)
      (case (vis/provider-auth-kind pid)
        :none
        (add-model-picker sid preset nil false)

        :oauth
        (let [registered (vis/provider-by-id pid)
              detect-fn  (:provider/detect-fn registered)
              detected?  (boolean (try (when detect-fn (detect-fn))
                                    (catch Throwable _ nil)))]
          (modal-shell (str (:label preset) " — Sign In")
            (modal-back (str base "/add") "Add Provider")
            (if detected?
              [:div.oauth-ready
               [:p "Already authenticated — credentials found on this machine."]
               [:button.send-wide {:type "button"
                                   :hx-post (str base "/add/" (name pid) "/confirm")
                                   :hx-vals (json-text {:api_key ""})
                                   :hx-target "#modal" :hx-swap "innerHTML"}
                (str "Add " (:label preset))]]
              [:div.oauth-pending
               [:p (str (:label preset) " signs in through an interactive browser flow "
                     "owned by its provider extension.")]
               [:p "Run it from any vis surface, then come back:"]
               [:pre.cmd (str "vis providers auth " (name pid))]
               [:button.send-wide {:type "button"
                                   :hx-get (str base "/add/" (name pid))
                                   :hx-target "#modal" :hx-swap "innerHTML"}
                "I signed in — check again"]])))

        ;; :api-key
        (modal-shell (str (:label preset) " Setup")
          (modal-back (str base "/add") "Add Provider")
          [:form.key-form {:hx-post (str base "/add/" (name pid) "/models")
                           :hx-target "#modal" :hx-swap "innerHTML"}
           [:input {:type "password" :name "api_key" :placeholder "API key"
                    :autofocus true :autocomplete "off"}]
           [:button.send-wide {:type "submit"} "Continue"]])))))

(defn- confirm-add-provider!
  "Persist a new provider through the core service (the exact configs
   the TUI writes): OAuth presets get their default models; everyone
   else gets the chosen model (+ the key when one was entered)."
  [sid pid api-key model]
  (when-let [preset (preset-by-id pid)]
    (let [oauth? (= :oauth (vis/provider-auth-kind pid))
          cfg    (if oauth?
                   (vis/provider-config-with-models preset
                     (vis/provider-default-model-configs preset))
                   (cond-> (vis/provider-config-with-models preset [{:name model}])
                     (seq api-key) (assoc :api-key api-key)))]
      (when (or oauth? (seq model))
        (vis/add-config-provider! cfg :web-provider-add))))
  (providers-modal sid))

(defn- provider-models-view
  "Per-provider model manager: primary first (= svar's default routing
   root), make-primary / remove per row, live-fetched add list."
  [sid pid]
  (let [base     (providers-base sid)
        provider (configured-provider pid)
        models   (vec (:models provider))]
    (modal-shell (str (vis/display-label pid) " Models")
      (modal-back base "Providers")
      [:div.model-rows
       (for [[idx m] (map-indexed vector models)
             :let [nm (:name m)] :when nm]
         [:div.model-row
          [:span.model-name nm]
          (if (zero? idx)
            [:span.model-primary "★ Primary"]
            [:button.pcard-act {:type "button"
                                :hx-post (str base "/p/" (name pid) "/models/primary")
                                :hx-vals (json-text {:model nm})
                                :hx-target "#modal" :hx-swap "innerHTML"}
             "make primary"])
          (when (> (count models) 1)
            [:button.pcard-act.danger {:type "button"
                                       :hx-post (str base "/p/" (name pid) "/models/remove")
                                       :hx-vals (json-text {:model nm})
                                       :hx-confirm (str "Remove " nm "?")
                                       :hx-target "#modal" :hx-swap "innerHTML"}
             "remove"])])]
      [:button.add-provider {:type "button"
                             :hx-get (str base "/p/" (name pid) "/models/options")
                             :hx-target "#modal" :hx-swap "innerHTML"}
       "+ Add model"])))

(defn- provider-model-options-view
  "Addable models for a configured provider (live catalog minus the
   ones already on it)."
  [sid pid show-all?]
  (let [base     (providers-base sid)
        provider (configured-provider pid)
        existing (into #{} (keep :name) (:models provider))
        {:keys [models hidden-count]}
        (vis/provider-model-options provider (vis/provider-default-model-names provider) show-all?)
        addable  (remove existing models)]
    (modal-shell (str (vis/display-label pid) " — Add Model")
      (modal-back (str base "/p/" (name pid) "/models") "Models")
      (if (empty? addable)
        [:p.empty "No further models reported."]
        [:div.model-chips.pick
         (for [m addable]
           [:button.model-chip {:type "button"
                                :hx-post (str base "/p/" (name pid) "/models/add")
                                :hx-vals (json-text {:model m})
                                :hx-target "#modal" :hx-swap "innerHTML"}
            m])])
      (when (and (not show-all?) (pos? hidden-count))
        [:button.show-all {:type "button"
                           :hx-get (str base "/p/" (name pid) "/models/options?show_all=1")
                           :hx-target "#modal" :hx-swap "innerHTML"}
         (str "Show all models… (" hidden-count " hidden)")]))))

(defn- provider-status-view
  "Status + limits as the RICH canonical markdown form
   (`vis/provider-status-md`) — the same report the TUI paints through
   its IR walker, here rendered as markdown (tables and all)."
  [sid pid]
  (let [provider (configured-provider pid)
        md (vis/provider-status-md provider)]
    (modal-shell (str (vis/display-label pid) " Status & Limits")
      (modal-back (providers-base sid) "Providers")
      [:div.status-md.md {:data-md (str md)} (md->hiccup md)])))

(defn- provider-key-view
  [sid pid]
  (modal-shell (str (vis/display-label pid) " Authentication")
    (modal-back (providers-base sid) "Providers")
    [:form.key-form {:hx-post (str (providers-base sid) "/p/" (name pid) "/key")
                     :hx-target "#modal" :hx-swap "innerHTML"}
     [:input {:type "password" :name "api_key" :placeholder "New API key"
              :autofocus true :autocomplete "off"}]
     [:button.send-wide {:type "submit"} "Save key"]]))

;; ── Providers: handlers ─────────────────────────────────────────────

(defn- html-ok [body]
  {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body body})

(defn- with-session
  "Run `(f sid)` for a valid session id, else 404. Every providers
   handler routes through here so URLs stay session-scoped."
  [request f]
  (if-let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (html-ok (f sid))
    {:status 404 :headers {"Content-Type" "text/html"} :body "unknown session"}))

(defn- path-pid [request]
  (some-> (get-in request [:path-params :pid]) keyword))

(defn- turn-machinery-handler
  "GET /ui/session/:sid/turn/:tid/machinery — the lazily-fetched body of one
   turn's machinery disclosure (code/results/tools). Replaces the collapsed
   placeholder via hx-swap=outerHTML, so collapsed machinery costs ~0 bytes
   on the initial page until the user expands it."
  [request]
  (let [tid (get-in request [:path-params :tid])]
    (html-ok (html (or (machinery-body {:turn_id tid}) [:div.machinery-body])))))

(defn- turns-older-handler
  "GET /ui/session/:sid/turns?before=<turn-id> — one page of OLDER turns for
   infinite scroll-up. Returns a fresh `.load-older` sentinel (when even older
   turns remain) above the batch, oldest→newest, so the outerHTML swap keeps
   chronological order and the sentinel back at the top."
  [request]
  (let [sid    (some-> (get-in request [:path-params :sid]) parse-uuid)
        before (some->> (:query-string request)
                 (re-find #"(?:^|&)before=([0-9a-fA-F-]+)")
                 second)]
    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "text/html; charset=utf-8"} :body ""}
      (let [turns-all  (vec (reverse (vis/gateway-list-turns sid)))
            idx        (or (some (fn [[i t]] (when (= before (some-> (pick t :turn_id) str)) i))
                             (map-indexed vector turns-all))
                         (count turns-all))
            start      (max 0 (- idx OLDER_TURN_PAGE))
            batch      (subvec turns-all start idx)
            more?      (pos? start)
            new-oldest (some-> (first batch) (pick :turn_id) str)]
        (html-ok
          (html
            (list
              (when more? (older-sentinel sid new-oldest))
              (map turn-block batch))))))))

(defn- session-providers-handler
  "GET /ui/session/:sid/providers — the providers dialog."
  [request]
  (with-session request providers-modal))

(defn- set-provider-handler
  "POST /ui/session/:sid/provider {model} — set/clear this session's model
   preference (blank model = router default). Re-renders the session-model
   picker AND out-of-band swaps `#footwrap` so the footer's model name
   updates immediately (the picker targets #modal; without the OOB the
   footer stayed stale until the next turn boundary re-rendered it via the
   `footer` SSE frame)."
  [request]
  (with-session request
    (fn [sid]
      (vis/gateway-set-session-model! sid (get-in request [:form-params "model"]))
      (str (session-model-picker sid)
        (html [:div {:id "footwrap" :hx-swap-oob "innerHTML"}
               (footer-content sid)])))))

(defn- session-model-handler
  "GET /ui/session/:sid/model — open the per-session model picker."
  [request]
  (with-session request session-model-picker))

(defn- provider-diag-handler
  "GET .../providers/:pid/diag — one card with auth + limits computed
   (the slow probes); swapped over the skeleton card."
  [request]
  (with-session request
    (fn [sid]
      (let [pid (path-pid request)
            providers (vis/configured-providers)
            idx (or (some (fn [[i p]] (when (= pid (:id p)) i))
                      (map-indexed vector providers))
                  0)
            provider (configured-provider pid)]
        (if-not provider
          (providers-modal sid)
          (html (provider-card sid provider idx
                  {:status (vis/provider-status provider)
                   :limits (vis/provider-limits-safe provider)})))))))

(defn- provider-add-picker-handler
  [request]
  (with-session request add-provider-picker))

(defn- provider-add-step-handler
  [request]
  (with-session request #(add-provider-step % (path-pid request))))

(defn- provider-add-models-handler
  "POST .../providers/add/:pid/models {api_key show_all} — the model
   picker for a preset being added."
  [request]
  (with-session request
    (fn [sid]
      (let [pid (path-pid request)
            api-key (get-in request [:form-params "api_key"])
            show-all? (= "1" (get-in request [:form-params "show_all"]))]
        (if-let [preset (preset-by-id pid)]
          (add-model-picker sid preset (not-empty api-key) show-all?)
          (providers-modal sid))))))

(defn- provider-add-confirm-handler
  [request]
  (with-session request
    (fn [sid]
      (confirm-add-provider! sid (path-pid request)
        (not-empty (get-in request [:form-params "api_key"]))
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
    #(provider-model-options-view % (path-pid request)
       (= "1" (get-in request [:query-params "show_all"])))))

(defn- move-model-first [models nm]
  (let [models (vec models)
        hit    (some #(when (= nm (:name %)) %) models)]
    (if hit
      (into [hit] (remove #(= nm (:name %)) models))
      models)))

(defn- provider-models-mutate-handler
  "POST .../:pid/models/primary|remove|add {model} — persist through
   the core service, re-render the relevant view."
  [request]
  (with-session request
    (fn [sid]
      (let [pid (path-pid request)
            nm  (get-in request [:form-params "model"])
            uri (str (:uri request))
            op  (cond
                  (str/ends-with? uri "/primary") :primary
                  (str/ends-with? uri "/remove")  :remove
                  :else                           :add)]
        (when (seq nm)
          (vis/update-config-provider! pid
            (fn [provider]
              (update provider :models
                (fn [models]
                  (case op
                    :primary (move-model-first models nm)
                    :remove  (vec (remove #(= nm (:name %)) models))
                    :add     (if (some #(= nm (:name %)) models)
                               (vec models)
                               (conj (vec models) {:name nm}))))))
            :web-provider-models))
        (provider-models-view sid pid)))))

(defn- provider-status-handler
  [request]
  (with-session request #(provider-status-view % (path-pid request))))

(defn- provider-key-form-handler
  [request]
  (with-session request #(provider-key-view % (path-pid request))))

(defn- provider-key-save-handler
  [request]
  (with-session request
    (fn [sid]
      (let [pid (path-pid request)
            api-key (not-empty (get-in request [:form-params "api_key"]))]
        (when api-key
          (vis/update-config-provider! pid #(assoc % :api-key api-key)
            :web-provider-key))
        (providers-modal sid)))))

(defn- wav-file?
  "RIFF/WAVE magic + minimum header length. MANDATORY before handing a
   file to the ASR: sherpa-onnx's native WaveReader ABORTS THE WHOLE JVM
   on malformed input (observed live: Abort trap 6 on a garbage body) —
   an exception we can catch is not on offer, so we refuse early."
  [^java.io.File f]
  (and (>= (.length f) 44)
    (with-open [in (io/input-stream f)]
      (let [head (byte-array 12)]
        (and (= 12 (.read in head))
          (= "RIFF" (String. head 0 4 "US-ASCII"))
          (= "WAVE" (String. head 8 4 "US-ASCII")))))))

(defn- voice-handler
  "POST /ui/session/:sid/voice — body is a WAV blob recorded+encoded in
   the browser (ui.js). Transcribes through the LOCAL Parakeet model
   (vis-foundation-voice / sherpa-onnx; soft-resolved so a build without
   the voice extension answers 501 instead of failing to load). First
   use downloads the model — same behavior as TUI voice input."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        transcribe (try
                     (requiring-resolve
                       'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)
                     (catch Throwable _ nil))]
    (cond
      (not (and sid (vis/gateway-soul sid)))
      {:status 404 :headers {"Content-Type" "application/json"}
       :body (json-text {:error "unknown session"})}

      (nil? transcribe)
      {:status 501 :headers {"Content-Type" "application/json"}
       :body (json-text {:error "voice extension is not on the classpath"})}

      :else
      (let [tmp (java.io.File/createTempFile "vis-voice" ".wav")]
        (try
          (with-open [in ^java.io.InputStream (:body request)
                      out (io/output-stream tmp)]
            (io/copy in out))
          (if-not (wav-file? tmp)
            {:status 400 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:error "body must be a RIFF/WAVE audio file"})}
            {:status 200 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:text (str/trim (str (transcribe (str tmp))))})})
          (catch Throwable t
            {:status 400 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:error (or (ex-message t) "transcription failed")})})
          (finally
            (.delete tmp)))))))

;; =============================================================================
;; CSS - the whole theme, one file, no inline styles.
;; vis-light tokens (internal/theme.clj light-palette):
;;   --bg :terminal-bg | --fg :text-fg | --panel2 :dialog-bg
;;   --code-bg :code-block-bg | --cream :turn-separator-bg
;;   --gold :header-tab-number-fg | --gold2 :turn-separator-fg
;;   --amber :code-result-fg | --amber-deep :warning-fg
;;   --warn-bg :warning-bg | --indigo :header-active-tab-bg
;;   --ok :status-ok | --err :status-bad
;; =============================================================================

(def ^:private app-css
  "The stylesheet, vendored as a REAL file (resources/vis-channel-web/public/
   app.css) and read from the classpath ONCE — consistent with the JS assets
   (`JS_ASSETS`), instead of a ~480-line string literal jammed into this ns.
   Edit it with real CSS tooling; served at /ui/app.css by `css-handler`."
  (delay (some-> (io/resource "vis-channel-web/public/app.css") slurp)))

(vis/register-toggle!
  {:id :vis-channel-web/theme :label "Web theme"
   :description (str "Theme for the web companion UI - picked from the SAME shared"
                  " registry the TUI paints from (internal/theme.clj), so every"
                  " registered theme works in both places.")
   :type :enum
   :choices (mapv keyword (vis/available-theme-ids))
   :default (keyword vis/default-theme-id)
   :owner :vis :group :channels :persist? true})

(defn- css-handler
  "Serves app.css with a theme-driven `:root` override APPENDED: the static
   stylesheet stays the layout/base (vis-light defaults baked in), and the
   override re-binds every shared color var (`vis/web-css-root`) to the
   selected `:vis-channel-web/theme` - the same registry the TUI uses."
  [_]
  (let [theme-id (or (try (some-> (vis/toggle-value :vis-channel-web/theme) name)
                       (catch Throwable _ nil))
                   vis/default-theme-id)]
    {:status 200
     :headers {"Content-Type" "text/css; charset=utf-8"
               "Cache-Control" "no-cache"}
     :body (str (or @app-css "")
             "\n/* theme override - generated from the shared theme registry */\n"
             (try (vis/web-css-root theme-id)
               (catch Throwable _ "")))}))

;; =============================================================================
;; Route contribution (whiteboard slot) + channel registration
;; =============================================================================

(defn- ui-routes
  "Reitit route data for the contribution; closes over the gateway token
   so /ui and /ui/auth can run the cookie exchange. Handlers go in as
   VARS so a REPL :reload serves new code on the very next request."
  [token]
  [["/ui" {:get #(index-handler % token)}]
   ["/ui/auth" {:post #(auth-handler % token)}]
   ["/ui/app.css" {:get #'css-handler}]
   ["/ui/icons.svg" {:get #'icons-handler}]
   ["/ui/js/:asset" {:get #'js-asset-handler}]
   ["/ui/fonts/:asset" {:get #'font-asset-handler}]
   ["/ui/settings" {:get #'settings-handler}]
   ["/ui/settings/toggle" {:post #'settings-mutate-handler}]
   ["/ui/settings/cycle" {:post #'settings-mutate-handler}]
   ["/ui/session/:sid/providers" {:get #'session-providers-handler}]
   ["/ui/session/:sid/model" {:get #'session-model-handler}]
   ["/ui/session/:sid/provider" {:post #'set-provider-handler}]
   ["/ui/session/:sid/providers/add" {:get #'provider-add-picker-handler}]
   ["/ui/session/:sid/providers/add/:pid" {:get #'provider-add-step-handler}]
   ["/ui/session/:sid/providers/add/:pid/models" {:post #'provider-add-models-handler}]
   ["/ui/session/:sid/providers/add/:pid/confirm" {:post #'provider-add-confirm-handler}]
   ["/ui/session/:sid/providers/p/:pid/diag" {:get #'provider-diag-handler}]
   ["/ui/session/:sid/providers/p/:pid/remove" {:post #'provider-remove-handler}]
   ["/ui/session/:sid/providers/p/:pid/models" {:get #'provider-models-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/options" {:get #'provider-model-options-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/primary" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/remove" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/p/:pid/models/add" {:post #'provider-models-mutate-handler}]
   ["/ui/session/:sid/providers/p/:pid/status" {:get #'provider-status-handler}]
   ["/ui/session/:sid/providers/p/:pid/key" {:get #'provider-key-form-handler
                                             :post #'provider-key-save-handler}]
   ["/ui/sessions" {:post #'create-session-handler}]
   ["/ui/sessions/delete" {:get #'delete-sessions-confirm-handler
                           :post #'delete-sessions-bulk-handler}]
   ["/ui/session/:sid" {:get #'session-handler
                        :delete #'delete-session-ui-handler}]
   ["/ui/session/:sid/delete" {:get #'delete-session-confirm-handler}]
   ["/ui/slash" {:get #'slash-list-handler}]
   ["/ui/session/:sid/files" {:get #'files-handler}]
   ["/ui/session/:sid/turns" {:post #'submit-turn-handler :get #'turns-older-handler}]
   ["/ui/session/:sid/turn/:tid/machinery" {:get #'turn-machinery-handler}]
   ["/ui/session/:sid/plan-review" {:post #'plan-review-handler}]
   ["/ui/session/:sid/voice" {:post #'voice-handler}]
   ["/ui/session/:sid/stream" {:get #'stream-handler}]
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
   :open-uris #{"/ui" "/ui/auth" "/ui/app.css" "/ui/icons.svg"
                "/ui/js/htmx.min.js" "/ui/js/htmx-sse.js" "/ui/js/marked.min.js"
                "/ui/js/prism.min.js" "/ui/js/ui.js"
                "/ui/fonts/inter-400.woff2" "/ui/fonts/inter-600.woff2"
                "/ui/fonts/inter-700.woff2" "/ui/fonts/jetbrains-mono-400.woff2"
                "/ui/fonts/jetbrains-mono-700.woff2"}
   :request-authed-fn ui-authed?
   :on-unauthorized (fn [_request] {:status 303 :headers {"Location" "/ui"} :body ""})
   :form-params? true})

(defn- parse-flag [args flag]
  (some (fn [[a b]] (when (= a flag) b)) (partition 2 1 args)))

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
  (let [pb (doto (ProcessBuilder. ["cloudflared" "tunnel" "--protocol" "http2" "--url" local-url])
             (.redirectErrorStream true))
        process (try
                  (.start pb)
                  (catch java.io.IOException _
                    (throw (ex-info (str "cloudflared binary not found on PATH. "
                                      "Install it first (e.g. `brew install cloudflared`) - "
                                      "https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/")
                             {:cloudflared/missing? true}))))
        url-promise (promise)
        reader (java.io.BufferedReader.
                 (java.io.InputStreamReader. (.getInputStream process)))]
    (doto (Thread.
            ^Runnable
            (fn []
              (loop []
                (when-let [line (.readLine reader)]
                  (when-let [url (re-find #"https://[a-z0-9-]+\.trycloudflare\.com" line)]
                    (deliver url-promise url))
                  (recur)))))
      (.setDaemon true)
      (.start))
    {:process process
     :url (deref url-promise 30000 nil)}))

(defn channel-main
  "`vis channels web` - start the gateway (UI auto-mounted because this
   namespace is loaded), print the /ui address, park until SIGTERM.
   `--cloudflared` additionally exposes the UI through a Cloudflare quick
   tunnel (requires the `cloudflared` binary on PATH) and FORCES
   `--require-token` - a public tunnel never runs without auth."
  [args]
  (let [cloudflared? (boolean (some #{"--cloudflared"} args))
        {:keys [port host token-file require-token?]}
        (vis/gateway-start! {:port (some-> (parse-flag args "--port") parse-long)
                             :host (parse-flag args "--host")
                             :token-file (parse-flag args "--token-file")
                             ;; a Cloudflare tunnel is PUBLIC internet - the
                             ;; bearer token is non-negotiable there.
                             :require-token? (or cloudflared?
                                               (boolean (some #{"--require-token"} args)))})]
    (println (str "vis web companion: http://" host ":" port "/ui"))
    (if require-token?
      (println (str "bearer token: " token-file))
      (println "auth: disabled (loopback default; pass --require-token to enable)"))
    (when cloudflared?
      (println "cloudflared: token auth FORCED on (public tunnel) - paste the bearer token from the file above into the connect page")
      (try
        (let [{:keys [process url]} (start-cloudflared! (str "http://" host ":" port))]
          (.addShutdownHook (Runtime/getRuntime)
            (Thread. ^Runnable (fn [] (.destroy ^Process process))))
          (if url
            (println (str "cloudflared tunnel: " url "/ui"))
            (println "cloudflared: tunnel started, but no trycloudflare URL appeared within 30s (check cloudflared logs)")))
        (catch clojure.lang.ExceptionInfo e
          (if (:cloudflared/missing? (ex-data e))
            (println (str "cloudflared: " (ex-message e)))
            (throw e)))))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable vis/gateway-stop!))
    @(promise)))

(vis/register-extension!
  (vis/extension
    {:ext/name        "channel-web"
     :ext/description "Web companion - the gateway /ui chat instrument (hiccup + HTMX + SSE)."
     :ext/version     "0.2.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/channels    [{:channel/id      :web
                        :channel/cmd     "web"
                        :channel/doc     "Serve the gateway with the /ui web companion."
                        :channel/usage   "vis channels web [--port 7890] [--host 127.0.0.1] [--cloudflared]"
                        :channel/main-fn #'channel-main}]
     :ext/channel-contributions
     {:gateway.slot/http-routes [{:id :web/ui :fn #'ui-contribution}]}}))
