(ns com.blockether.vis.ext.channel-web.core
  "Web companion channel - the gateway's `/ui` two-pane instrument.

   Pure Clojure SSR: hiccup renders HTML, HTMX does declarative swaps,
   the live feed is the htmx SSE extension consuming
   `/ui/session/:sid/stream` - a gateway SSE stream of named HTML
   fragments (activity/thinking/mind) rendered server-side. Every
   script is VENDORED on the classpath and served by this channel
   (htmx 2.0.10 + its SSE extension + the auto-reload listener) - the
   page never loads anything from outside vis.

   AUTO-MOUNT: loading this namespace registers a route contribution
   via `vis/gateway-register-routes!`. Namespaces load through the
   META-INF/vis-extension manifest classpath scan, so dropping this jar
   on the classpath mounts `/ui` into any process that starts the
   gateway - `vis serve`, `vis channels web`, or an embedded
   `gateway-start!`. Removing the jar leaves the pure JSON API.

   This namespace is the THIRD canonical-IR walker: the TUI walks IR
   into ANSI cells, Telegram walks it into its HTML subset, and
   `ir->hiccup` walks the same IR into DOM (GATEWAY.md §4.1 ALWAYS IR).

   Auth: the JSON API stays bearer-only; the browser flow exchanges the
   same token once via POST /ui/auth for an HttpOnly `vis_token`
   cookie, which EventSource then carries automatically on SSE connect.
   The contribution declares the cookie as an extra auth carrier and
   shapes unauthorized /ui hits as a 303 back to the token form."
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
   `defonce`. The /ui/dev-reload stream watches this var: a REPL
   `:reload` of this namespace moves the stamp, connected browsers get
   a `reload` event and refresh themselves. Always on - the listener is
   one parked virtual thread per open page."
  (System/currentTimeMillis))

(def ^:private DEV_RELOAD_POLL_MS 2000)

;; Every script the pages load is VENDORED on the classpath under
;; resources/vis-channel-web/public/ and served by this channel — no
;; CDN, no request ever leaves the host: htmx 2.0.10 (htmx.min.js), its
;; SSE extension (htmx-sse.js), and the auto-reload listener
;; (dev-reload.js). Auto-reload is ALWAYS on — the listener is a
;; near-free idle SSE connection and it makes every :reload / daemon
;; restart instantly visible in the browser.
(def ^:private JS_ASSETS
  {"htmx.min.js"   "vis-channel-web/public/htmx.min.js"
   "htmx-sse.js"   "vis-channel-web/public/htmx-sse.js"
   "dev-reload.js" "vis-channel-web/public/dev-reload.js"})

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
        (= tag :code)
        [:code.ir-code (keep ir->hiccup children)]

        (or (= tag :pre) (= tag :code-block))
        [:pre.ir-pre
         [:code {:class (str "lang-" (name (or (:lang attrs) "txt")))}
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

(defn- page ^String [title & body]
  (str "<!DOCTYPE html>"
    (h/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:title (str title " · vis")]
        [:link {:rel "stylesheet" :href "/ui/app.css"}]
        ;; All scripts are vendored on the classpath and served by this
        ;; channel — the page never loads anything from outside vis.
        [:script {:src "/ui/js/htmx.min.js" :defer true}]
        [:script {:src "/ui/js/htmx-sse.js" :defer true}]
        [:script {:src "/ui/js/dev-reload.js" :defer true}]]
       (into [:body] body)])))

;; =============================================================================
;; Defensive readers (mind snapshot values may carry string OR kw keys -
;; the GraalPy boundary keeps snake_case strings verbatim)
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

(defn- utilization-bar [utilization]
  (let [pct (or (pick utilization :pct-of-limit) (pick utilization :pct_of_limit))]
    (when (number? pct)
      [:div.util
       [:div.util-track [:div.util-fill {:style (str "width:" (min 100 (long pct)) "%")}]]
       [:span.util-label (str (long pct) "% of context")]])))

(defn- fact-card [[fact-key fact]]
  [:details.fact
   [:summary [:span.fact-key (str fact-key)]]
   [:div.fact-body
    (when-let [content (pick fact :content)]
      [:p.fact-content (str content)])
    (for [file (or (pick fact :files) [])
          :let [path (pick file :path)]
          :when path]
      [:div.fact-file
       [:span.fact-path (str path)]
       (for [region (or (pick file :regions) [])]
         [:div.fact-region
          (when-let [from-hash (pick region :from_hash)]
            [:span.fact-hash (str "@" from-hash)])
          (when-let [src (pick region :src)]
            [:pre.ir-pre [:code (str src)]])])])
    (when-not (or (pick fact :content) (seq (or (pick fact :files) [])))
      [:pre.ir-pre [:code (pr-str fact)]])]])

(defn- task-row [task]
  (let [status (str (or (pick task :status) "pending"))]
    [:li {:class (str "task task-" status)}
     [:span.task-status status]
     [:span.task-title (str (or (pick task :title) (pick task :id) (pr-str task)))]]))

(defn- mind-panel
  "The right pane: the same ctx mirror the model reads, as DOM."
  [snapshot]
  [:div#mind.mind
   [:div.pane-head [:h2 "The Mind"] (utilization-bar (pick snapshot :session/utilization))]
   (let [tasks (pick snapshot :session/tasks)]
     [:section.mind-section
      [:h3 (str "Plan" (when (seq tasks) (str " · " (count tasks))))]
      (if (seq tasks)
        [:ul.tasks (map task-row tasks)]
        [:p.empty "no plan yet"])])
   (let [facts (fact-entries (pick snapshot :session/facts))]
     [:section.mind-section
      [:h3 (str "Facts" (when (seq facts) (str " · " (count facts))))]
      (if (seq facts)
        [:div.facts (map fact-card facts)]
        [:p.empty "no facts yet"])])
   (when-let [scope (pick snapshot :session/scope)]
     [:section.mind-section
      [:h3 "Scope"]
      [:pre.ir-pre [:code (str (or (pick scope :cursor) (pr-str scope)))]]])])

(defn- turn-block [turn]
  (let [status (pick turn :status)]
    [:div.turn
     [:div.bubble.bubble-user [:p (str (pick turn :request))]]
     (cond
       (pick turn :answer_md)
       [:div.bubble.bubble-vis
        (md->hiccup (pick turn :answer_md))
        [:div.bubble-meta
         (status-chip status)
         (when-let [cost (pick (pick turn :cost) :total-cost)]
           [:span.meta-cost (format "$%.4f" (double cost))])
         (when-let [n (pick turn :iteration_count)]
           [:span.meta-iters (str n " iterations")])]]

       (= "running" status)
       [:div.bubble.bubble-vis.bubble-running [:p "thinking…"] (status-chip status)]

       :else
       [:div.bubble.bubble-vis
        [:p.empty (str "(" (or status "no answer") ")")]
        (status-chip status)])]))

(defn- activity-item [kind & children]
  (html (into [:div {:class (str "act act-" kind)}] children)))

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
    "turn.started"
    [{:event "activity"
      :html (activity-item "turn" [:strong "turn started"] [:span.act-dim (str (:request event))])}
     {:event "thinking" :html ""}]

    "reasoning.delta"
    [{:event "thinking"
      :html (html [:span.act-dim (code-snip (:text event))])}]

    "block.started"
    [{:event "activity"
      :html (activity-item "code" [:pre.ir-pre [:code (code-snip (:code event))]])}]

    "block.output"
    [{:event "activity"
      :html (activity-item (if (:error event) "error" "result")
              (when-let [error (:error event)] [:pre.ir-pre.act-error [:code (code-snip error)]])
              (when-let [result (:result event)] [:pre.ir-pre [:code (code-snip result)]]))}]

    "iteration.completed"
    [{:event "activity"
      :html (activity-item "iter" [:span.act-dim "iteration done"])}]

    ("turn.completed" "turn.failed")
    (let [snapshot (try (vis/gateway-mind-snapshot sid) (catch Throwable _ nil))]
      (cond-> [{:event "thinking" :html ""}
               {:event "activity"
                :html (activity-item "answer"
                        (status-chip (:status event))
                        (md->hiccup (or (:answer_md event) (:error event) "")))}]
        snapshot (conj {:event "mind" :html (html (mind-panel snapshot))})))

    nil))

(defn- write-frame! [^OutputStream out {:keys [event html]}]
  (let [frame (str "event: " event "\n"
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
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "text/html"} :body "unknown session"}
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache"}
       :body
       (reify ring-protocols/StreamableResponseBody
         (write-body-to-stream [_ _ output-stream]
           (let [^OutputStream out output-stream
                 sub-id (str (java.util.UUID/randomUUID))
                 sink   (fn [event]
                          (locking out
                            (doseq [frame (event->frames sid event)]
                              (write-frame! out frame))
                            (.flush out)))]
             (try
               (locking out
                 (doseq [event (vis/gateway-subscribe! sid sub-id sink
                                 (vis/gateway-current-seq sid))]
                   (sink event)))
               (loop []
                 (Thread/sleep (long HEARTBEAT_MS))
                 (locking out
                   (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out))
                 (recur))
               (catch Throwable _ nil)
               (finally
                 (vis/gateway-unsubscribe! sid sub-id)
                 (try (.close out) (catch Throwable _ nil)))))))})))

;; =============================================================================
;; Pages
;; =============================================================================

(defn- token-form-page [& [error]]
  (page "connect"
    [:main.auth
     [:h1 "vis"]
     [:p.tagline "see it think"]
     (when error [:p.auth-error error])
     [:form {:method "post" :action "/ui/auth"}
      [:input {:type "password" :name "token" :placeholder "gateway bearer token"
               :autofocus true :autocomplete "off"}]
      [:button {:type "submit"} "connect"]]
     [:p.auth-hint "token lives at ~/.vis/gateway.token on the host"]]))

(defn- sessions-page []
  (page "sessions"
    [:main.shell
     [:header.top
      [:h1 "vis " [:span.dim "· sessions"]]
      [:form.newsession {:method "post" :action "/ui/sessions"}
       [:input {:type "text" :name "title" :placeholder "new session title"}]
       [:button {:type "submit"} "open"]]]
     (let [sessions (vis/gateway-list-sessions)]
       (if (seq sessions)
         [:ul.sessions
          (for [{:keys [id title status] :as soul} sessions]
            [:li.session-row
             [:a {:href (str "/ui/session/" id)}
              [:span.session-title (or title "(untitled)")]
              (status-chip status)
              [:span.session-id (subs (str id) 0 8)]
              (when-let [active (:last_active_at soul)]
                [:span.dim (str "active " active)])]])]
         [:p.empty "no sessions yet — open one above"]))]))

(defn- session-page [sid]
  (let [soul     (vis/gateway-soul sid)
        turns    (reverse (vis/gateway-list-turns sid))
        snapshot (try (vis/gateway-mind-snapshot sid) (catch Throwable _ nil))]
    (page (or (:title soul) "session")
      [:main.shell.two-pane {:hx-ext "sse"
                             :sse-connect (str "/ui/session/" sid "/stream")}
       [:section.pane.conversation
        [:header.top
         [:a.back {:href "/ui"} "← sessions"]
         [:h1 (or (:title soul) "(untitled)") " " (status-chip (:status soul))]
         [:span.session-id (str sid)]]
        [:div#turns.turns
         (if (seq turns)
           (map turn-block turns)
           [:p.empty "no turns yet — say something below"])]
        [:div#thinking.thinking {:sse-swap "thinking" :hx-swap "innerHTML"}]
        [:div#activity.activity {:sse-swap "activity" :hx-swap "beforeend"}]
        [:form.composer {:hx-post (str "/ui/session/" sid "/turns")
                         :hx-target "#activity" :hx-swap "beforeend"
                         "hx-on::after-request" "if(event.detail.successful) this.reset()"}
         [:textarea {:name "request" :rows 3
                     :placeholder "ask vis to do something in this workspace…"}]
         [:button {:type "submit"} "send"]]]
       [:section.pane.mindwrap {:sse-swap "mind" :hx-swap "innerHTML"}
        (if snapshot
          (mind-panel snapshot)
          [:div#mind.mind [:div.pane-head [:h2 "The Mind"]] [:p.empty "wakes on first turn"]])]])))

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

(defn- index-handler
  "GET /ui - session list when the cookie is valid, token form
   otherwise. This route is open; it never leaks data unauthenticated."
  [request token]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (if (ui-authed? request token)
           (sessions-page)
           (token-form-page))})

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

(defn- submit-turn-handler
  "POST /ui/session/:sid/turns (htmx form) - submit and return a small
   activity fragment; the live stream carries everything that follows."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        text (str (get-in request [:form-params "request"]))
        result (when sid (vis/gateway-submit-turn! sid {:request text}))]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (cond
             (:turn result)
             (activity-item "sent" [:span.act-dim "sent — watching the mind…"])

             (= :turn-in-progress (:error result))
             (activity-item "error" [:span.act-error "a turn is already running — wait for it"])

             :else
             (activity-item "error"
               [:span.act-error (str "rejected: " (or (:message result) "invalid request"))]))}))

;; =============================================================================
;; CSS - the whole theme, one file, no inline styles
;; =============================================================================

(def ^:private APP_CSS
  "/* vis web companion - vis-light theme.
   Colors lifted from internal/theme.clj `light-palette` tokens:
     --bg      :terminal-bg            [255 255 255]
     --fg      :text-fg                [30 30 30]
     --panel2  :dialog-bg              [248 248 248]
     --code-bg :code-block-bg          [240 243 248]
     --cream   :turn-separator-bg      [248 244 235]
     --gold    :header-tab-number-fg   [250 204 21]
     --gold2   :turn-separator-fg      [190 150 40]
     --amber   :code-result-fg         [161 98 7]
     --amber-deep :warning-fg          [80 60 0]
     --warn-bg :warning-bg             [255 245 180]
     --indigo  :header-active-tab-bg   [37 99 235]
     --ok      :status-ok              [40 160 60]
     --err     :status-bad             [220 50 50]
   Yellow does the FILLS (buttons, badges, bars, highlights); amber does
   accent TEXT (gold text on white fails contrast). */
:root{--bg:#ffffff;--panel:#ffffff;--panel2:#f8f8f8;--code-bg:#f0f3f8;
--cream:#f8f4eb;--line:#e2e2e2;--line2:#d0d0d0;
--fg:#1e1e1e;--dim:#787878;
--gold:#facc15;--gold2:#be9628;--amber:#a16207;--amber-deep:#503c00;--warn-bg:#fff5b4;
--indigo:#2563eb;--err:#dc3232;--ok:#28a03c;
--mono:ui-monospace,SFMono-Regular,Menlo,monospace}
*{box-sizing:border-box;margin:0;padding:0}
body{background:var(--bg);color:var(--fg);font:15px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
a{color:var(--indigo);text-decoration:none}
h1{font-size:1.15rem;font-weight:600}h2{font-size:1rem}h3{font-size:.78rem;text-transform:uppercase;letter-spacing:.08em;color:var(--dim);margin:0 0 .5rem}
.dim,.act-dim{color:var(--dim)}.empty{color:var(--dim);font-style:italic;padding:.5rem 0}
/* auth */
.auth{max-width:22rem;margin:18vh auto;text-align:center;display:flex;flex-direction:column;gap:.9rem}
.auth h1{font-size:2.6rem;letter-spacing:.04em;border-bottom:4px solid var(--gold);display:inline-block;margin:0 auto;padding:0 .4rem .2rem}
.tagline{color:var(--dim)}
.auth form{display:flex;gap:.5rem}
.auth-error{color:var(--err)}.auth-hint{color:var(--dim);font-size:.8rem}
input,textarea,button{background:var(--bg);color:var(--fg);border:1px solid var(--line2);border-radius:8px;padding:.55rem .8rem;font:inherit}
input{flex:1}textarea{width:100%;resize:vertical;font-family:inherit}
input:focus,textarea:focus{outline:2px solid var(--gold);border-color:var(--gold)}
button{cursor:pointer;background:var(--gold);color:var(--amber-deep);font-weight:700;border:0}
button:hover{filter:brightness(1.06)}
/* shell */
.shell{max-width:80rem;margin:0 auto;padding:1.2rem}
.top{display:flex;align-items:center;gap:.9rem;padding-bottom:1rem;border-bottom:2px solid var(--gold);margin-bottom:.8rem;flex-wrap:wrap}
.top .back{color:var(--dim)}.newsession{margin-left:auto;display:flex;gap:.5rem}
.session-id{font-family:var(--mono);font-size:.75rem;color:var(--dim)}
/* sessions index */
.sessions{list-style:none;display:flex;flex-direction:column;gap:.5rem}
.session-row a{display:flex;align-items:center;gap:.8rem;background:var(--panel);border:1px solid var(--line);border-left:4px solid var(--gold);border-radius:10px;padding:.8rem 1rem;color:var(--fg)}
.session-row a:hover{border-color:var(--gold2);background:var(--warn-bg)}
.session-title{font-weight:600}
/* two-pane */
.two-pane{display:grid;grid-template-columns:minmax(0,1.5fr) minmax(0,1fr);gap:1.2rem;align-items:start}
@media(max-width:60rem){.two-pane{grid-template-columns:1fr}}
.pane{min-width:0}
/* conversation */
.turns{display:flex;flex-direction:column;gap:1rem;padding:.5rem 0}
.bubble{border-radius:12px;padding:.7rem .95rem;max-width:95%;overflow-wrap:anywhere}
.bubble-user{background:var(--cream);border:1px solid var(--gold2);align-self:flex-end;margin-left:2rem}
.bubble-vis{background:var(--panel);border:1px solid var(--line2);margin-right:2rem}
.bubble-running{border-style:dashed;color:var(--dim)}
.turn{display:flex;flex-direction:column;gap:.55rem}
.bubble-meta{display:flex;gap:.6rem;align-items:center;margin-top:.55rem;font-size:.75rem;color:var(--dim)}
.bubble p{margin:.3rem 0}.bubble ul,.bubble ol{margin:.3rem 0 .3rem 1.2rem}
.bubble h1,.bubble h2,.bubble h3{margin:.5rem 0 .3rem;font-size:1rem}
/* chips */
.chip{font-size:.68rem;font-weight:700;text-transform:uppercase;letter-spacing:.06em;border-radius:99px;padding:.12rem .55rem;border:1px solid var(--line2);background:var(--bg)}
.chip-running{color:var(--amber);border-color:var(--gold);background:var(--warn-bg)}
.chip-completed,.chip-idle{color:var(--ok);border-color:var(--ok)}
.chip-failed,.chip-cancelled{color:var(--err);border-color:var(--err)}
.chip-suspended{color:var(--indigo);border-color:var(--indigo)}
/* live activity + thinking */
.thinking{min-height:1.2rem;font-size:.8rem;color:var(--dim);font-style:italic;padding:.2rem 0;overflow-wrap:anywhere}
.activity{display:flex;flex-direction:column;gap:.4rem;padding:.3rem 0 1rem}
.act{border-left:3px solid var(--line2);padding:.3rem .7rem;font-size:.82rem;background:var(--panel2);border-radius:0 6px 6px 0}
.act-code{border-left-color:var(--indigo)}
.act-result{border-left-color:var(--gold)}
.act-error{border-left-color:var(--err)}.act .act-error{color:var(--err)}
.act-answer{border-left-color:var(--ok)}
.act-sent,.act-turn{border-left-color:var(--gold);background:var(--warn-bg)}
/* code */
.ir-pre{background:var(--code-bg);border:1px solid var(--line);border-radius:8px;padding:.55rem .7rem;overflow-x:auto;font-size:.8rem;margin:.3rem 0;color:var(--fg)}
.ir-pre code,.ir-code{font-family:var(--mono)}
.ir-code{background:var(--code-bg);border-radius:4px;padding:.05rem .3rem;font-size:.85em;color:var(--amber)}
/* composer */
.composer{display:flex;gap:.6rem;align-items:flex-end;border-top:2px solid var(--gold);padding-top:.9rem}
/* the mind */
.mind{background:var(--panel);border:1px solid var(--line);border-top:4px solid var(--gold);border-radius:12px;padding:1rem;display:flex;flex-direction:column;gap:1rem;position:sticky;top:1rem;box-shadow:0 1px 4px rgba(30,30,30,.06)}
.pane-head{display:flex;align-items:center;justify-content:space-between;gap:.8rem}
.mind-section{border-top:1px solid var(--line);padding-top:.8rem}
.util{display:flex;align-items:center;gap:.5rem;min-width:9rem}
.util-track{flex:1;height:6px;border-radius:3px;background:var(--panel2);overflow:hidden}
.util-fill{height:100%;background:linear-gradient(90deg,var(--gold),var(--gold2))}
.util-label{font-size:.7rem;color:var(--dim);white-space:nowrap}
.tasks{list-style:none;display:flex;flex-direction:column;gap:.35rem}
.task{display:flex;gap:.6rem;align-items:baseline;font-size:.85rem}
.task-status{font-size:.65rem;font-weight:700;text-transform:uppercase;color:var(--dim);min-width:4.5rem}
.task-done .task-status{color:var(--ok)}.task-candidate .task-status{color:var(--indigo)}
.task-in_progress .task-status,.task-running .task-status{color:var(--amber)}
.facts{display:flex;flex-direction:column;gap:.4rem}
.fact{background:var(--panel2);border:1px solid var(--line);border-radius:8px;padding:.45rem .6rem}
.fact summary{cursor:pointer;list-style:none}
.fact-key{font-family:var(--mono);font-size:.8rem;color:var(--amber);background:var(--warn-bg);border-radius:4px;padding:.05rem .35rem}
.fact-content{font-size:.85rem;margin:.4rem 0}
.fact-path{font-family:var(--mono);font-size:.75rem;color:var(--indigo)}
.fact-hash{font-family:var(--mono);font-size:.7rem;color:var(--gold2)}
")

(defn- css-handler [_]
  {:status 200
   :headers {"Content-Type" "text/css; charset=utf-8"
             ;; no-cache so an auto-reload refresh always re-fetches.
             "Cache-Control" "no-cache"}
   :body APP_CSS})

(defn- dev-reload-handler
  "SSE stream backing the always-on auto-reload script. Emits the
   namespace load stamp on connect, then a `reload` event the moment
   the stamp moves (a REPL :reload of this ns) — the browser refreshes
   itself. The poll loop parks a virtual thread (Jetty runs handlers on
   virtual threads), so an idle connection costs ~nothing."
  [_request]
  (let [stamp-at-connect @#'ui-load-stamp]
    {:status 200
     :headers {"Content-Type" "text/event-stream"
               "Cache-Control" "no-cache"}
     :body
     (reify ring-protocols/StreamableResponseBody
       (write-body-to-stream [_ _ output-stream]
         (let [^OutputStream out output-stream]
           (try
             (.write out (.getBytes (str "event: stamp\ndata: " stamp-at-connect "\n\n")
                           StandardCharsets/UTF_8))
             (.flush out)
             (loop []
               (Thread/sleep (long DEV_RELOAD_POLL_MS))
               (if (not= stamp-at-connect @#'ui-load-stamp)
                 (do (.write out (.getBytes "event: reload\ndata: now\n\n"
                                   StandardCharsets/UTF_8))
                   (.flush out))
                 (do (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out)
                   (recur))))
             (catch Throwable _ nil)
             (finally
               (try (.close out) (catch Throwable _ nil)))))))}))

;; =============================================================================
;; Route contribution (classpath auto-mount) + channel registration
;; =============================================================================

(defn- ui-routes
  "Reitit route data for the contribution; closes over the gateway token
   so /ui and /ui/auth can run the cookie exchange."
  [token]
  ;; Handlers go in as VARS so a REPL :reload of this namespace serves
  ;; the new code on the very next request without an app rebuild.
  [["/ui" {:get #(index-handler % token)}]
   ["/ui/auth" {:post #(auth-handler % token)}]
   ["/ui/app.css" {:get #'css-handler}]
   ["/ui/js/:asset" {:get #'js-asset-handler}]
   ["/ui/dev-reload" {:get #'dev-reload-handler}]
   ["/ui/sessions" {:post #'create-session-handler}]
   ["/ui/session/:sid" {:get #'session-handler}]
   ["/ui/session/:sid/turns" {:post #'submit-turn-handler}]
   ["/ui/session/:sid/stream" {:get #'stream-handler}]])

(defn- ui-contribution
  "The gateway pulls this through the `:gateway.slot/http-routes`
   whiteboard slot whenever it (re)builds its handler — no registration
   call, no ordering requirement between gateway start and this
   extension loading."
  []
  {:prefix "/ui"
   :routes ui-routes
   :open-uris #{"/ui" "/ui/auth" "/ui/app.css"
                "/ui/js/htmx.min.js" "/ui/js/htmx-sse.js" "/ui/js/dev-reload.js"}
   :request-authed-fn ui-authed?
   :on-unauthorized (fn [_request] {:status 303 :headers {"Location" "/ui"} :body ""})
   :form-params? true})

(defn- parse-flag [args flag]
  (some (fn [[a b]] (when (= a flag) b)) (partition 2 1 args)))

(defn channel-main
  "`vis channels web` - start the gateway (UI auto-mounted because this
   namespace is loaded), print the /ui address, park until SIGTERM."
  [args]
  (let [{:keys [port host token-file require-token?]}
        (vis/gateway-start! {:port (some-> (parse-flag args "--port") parse-long)
                             :host (parse-flag args "--host")
                             :token-file (parse-flag args "--token-file")
                             :require-token? (boolean (some #{"--require-token"} args))})]
    (println (str "vis web companion: http://" host ":" port "/ui"))
    (if require-token?
      (println (str "bearer token: " token-file))
      (println "auth: disabled (loopback default; pass --require-token to enable)"))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable vis/gateway-stop!))
    @(promise)))

(vis/register-extension!
  (vis/extension
    {:ext/name        "channel-web"
     :ext/description "Web companion - the gateway /ui two-pane instrument (hiccup + HTMX + SSE)."
     :ext/version     "0.1.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/channels    [{:channel/id      :web
                        :channel/cmd     "web"
                        :channel/doc     "Serve the gateway with the /ui web companion."
                        :channel/usage   "vis channels web [--port 7890] [--host 127.0.0.1]"
                        :channel/main-fn #'channel-main}]
     :ext/channel-contributions
     {:gateway.slot/http-routes [{:id :web/ui :fn #'ui-contribution}]}}))
