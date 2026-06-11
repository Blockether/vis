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
   (htmx 2.0.10 + its SSE extension + ui.js + dev-reload.js) - a page
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
   `defonce`. The /ui/dev-reload stream watches this var: a REPL
   `:reload` of this namespace moves the stamp, connected browsers get
   a `reload` event and refresh themselves. Always on - the listener is
   one parked virtual thread per open page."
  (System/currentTimeMillis))

(def ^:private DEV_RELOAD_POLL_MS 2000)

;; Every script the pages load is VENDORED on the classpath under
;; resources/vis-channel-web/public/ and served by this channel from
;; memory — no CDN, no request ever leaves the host.
(def ^:private JS_ASSETS
  {"htmx.min.js"   "vis-channel-web/public/htmx.min.js"
   "htmx-sse.js"   "vis-channel-web/public/htmx-sse.js"
   "marked.min.js" "vis-channel-web/public/marked.min.js"
   "ui.js"         "vis-channel-web/public/ui.js"
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
        [:pre
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

(defn- json-text [m]
  (str "{" (str/join "," (for [[k v] m] (str (pr-str (name k)) ":" (pr-str (str v))))) "}"))

(defn- page ^String [title & body]
  (str "<!DOCTYPE html>"
    (h/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:title (str title " · vis")]
        [:link {:rel "stylesheet" :href "/ui/app.css"}]
        ;; All vendored, all local — nothing loads from outside vis.
        [:script {:src "/ui/js/htmx.min.js" :defer true}]
        [:script {:src "/ui/js/htmx-sse.js" :defer true}]
        [:script {:src "/ui/js/marked.min.js" :defer true}]
        [:script {:src "/ui/js/ui.js" :defer true}]
        [:script {:src "/ui/js/dev-reload.js" :defer true}]]
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

(defn- utilization-bar [utilization]
  (let [pct (or (pick utilization :pct-of-limit) (pick utilization :pct_of_limit))]
    (when (number? pct)
      [:div.util
       [:div.util-track [:div.util-fill {:style (str "width:" (min 100 (long pct)) "%")}]]
       [:span.util-label (str (long pct) "%")]])))

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
       (for [region (or (pick file :regions) [])]
         [:div.fact-region
          (when-let [from-hash (pick region :from_hash)]
            [:span.fact-hash (str "@" from-hash)])
          (when-let [src (pick region :src)]
            [:pre.ir-pre [:code (str src)]])])])
    (when-not (or (pick fact :content) (seq (or (pick fact :files) [])))
      [:pre.ir-pre [:code (pr-str fact)]])]])

(def ^:private task-glyph
  {"done" "✓" "completed" "✓" "in_progress" "◐" "running" "◐"
   "candidate" "◇" "pending" "○"})

(defn- task-row [task]
  (let [status (str (or (pick task :status) "pending"))]
    [:li {:class (str "task task-" status)}
     [:span.task-glyph (get task-glyph status "○")]
     [:span.task-title (str (or (pick task :title) (pick task :id) (pr-str task)))]]))

(defn- context-panel
  "The right rail: CONTEXT - the same ctx mirror the model reads."
  [snapshot]
  [:div#context.context
   [:div.rail-head [:h2 "Context"] (utilization-bar (pick snapshot :session/utilization))]
   (let [tasks (pick snapshot :session/tasks)]
     [:section.rail-section
      [:h3 (str "Plan" (when (seq tasks) (str " · " (count tasks))))]
      (if (seq tasks)
        [:ul.tasks (map task-row tasks)]
        [:p.empty "no plan yet"])])
   (let [facts (fact-entries (pick snapshot :session/facts))]
     [:section.rail-section
      [:h3 (str "Facts" (when (seq facts) (str " · " (count facts))))]
      (if (seq facts)
        [:div.facts (map fact-card facts)]
        [:p.empty "no facts yet"])])
   (when-let [scope (pick snapshot :session/scope)]
     [:section.rail-section
      [:h3 "Scope"]
      [:pre.ir-pre [:code (str (or (pick scope :cursor) (pr-str scope)))]]])])

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

(defn- user-bubble
  "TUI anatomy: 'You' role label in amber (:user-role-fg). The raw text
   rides in data-md; ui.js re-renders it through the vendored `marked`
   (MIT) for full markdown fidelity, falling back to the plain text."
  [text]
  [:div.bubble.b-user
   [:div.role.role-user "You"]
   [:div.prose.md {:data-md (str text)} [:p (str text)]]])

(defn- vis-bubble
  "TUI anatomy: 'Vis' role label in green (:ai-role-fg), canonical meta
   footer. Server renders the IR walk as the instant fallback; the raw
   markdown rides in data-md and ui.js re-renders it through `marked`."
  [turn]
  (let [md (or (pick turn :answer_md) (pick turn :error) "")]
    [:div.bubble.b-vis
     [:div.role.role-vis "Vis"]
     [:div.prose.md {:data-md (str md)} (md->hiccup md)]
     (bubble-foot turn)]))

(defn- turn-block [turn]
  (let [status (pick turn :status)]
    (list
      [:div.tsep]
      (user-bubble (pick turn :request))
      (cond
        (or (pick turn :answer_md) (pick turn :error))
        (vis-bubble turn)

        (= "running" status)
        [:div.bubble.b-vis
         [:div.role.role-vis "Vis"]
         [:div.dots [:span] [:span] [:span]]]

        :else
        [:div.bubble.b-vis
         [:div.role.role-vis "Vis"]
         [:p.empty (str "(" (or status "no answer") ")")]]))))

(defn- activity-item [kind & children]
  (html (into [:div {:class (str "act act-" kind)}] children)))

(defn- user-bubble-html [text]
  (html (list [:div.tsep] (user-bubble text))))

(defn- vis-message-html
  "A full vis chat bubble from a terminal turn event — flies into the
   thread (`#live`), NOT the Work log. Same anatomy as restored turns."
  [event]
  (html (vis-bubble event)))

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
    ;; The thread gets BUBBLES (user message via the form response, the
    ;; answer via the `message` event); Work gets ONLY machinery.
    "turn.started"
    [{:event "thinking"
      :html (html (list [:div.dots [:span] [:span] [:span]]))}]

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
    (let [snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))]
      (cond-> [{:event "thinking" :html ""}
               {:event "message" :html (vis-message-html event)}]
        snapshot (conj {:event "context" :html (html (context-panel snapshot))})))

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

(defn- dev-reload-handler
  "SSE stream backing the always-on auto-reload script. Emits the
   namespace load stamp on connect, then a `reload` event the moment
   the stamp moves (a REPL :reload of this ns) — the browser refreshes
   itself. The poll loop parks a virtual thread."
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
;; Pages
;; =============================================================================

(defn- token-form-page [& [error]]
  (page "connect"
    [:main.auth
     [:div.auth-card
      [:h1 "vis"]
      [:p.tagline "see it think"]
      (when error [:p.auth-error error])
      [:form {:method "post" :action "/ui/auth"}
       [:input {:type "password" :name "token" :placeholder "gateway bearer token"
                :autofocus true :autocomplete "off"}]
       [:button.send-wide {:type "submit"} "Connect"]]
      [:p.auth-hint "the token lives at ~/.vis/gateway.token on the host"]]]))

(defn- sessions-sidebar
  "Left rail: the session drawer. The active session is highlighted; a
   running one carries a gold pulse dot."
  [active-sid]
  [:aside.sidebar
   [:form.newchat {:method "post" :action "/ui/sessions"}
    [:button.newchat-btn {:type "submit"} "+ New session"]]
   [:ul.side-sessions
    (for [{:keys [id title status]} (vis/gateway-list-sessions)]
      [:li
       [:a {:class (str "side-row" (when (= (str id) (str active-sid)) " active"))
            :href (str "/ui/session/" id)}
        [:span.side-title (or title "Untitled")]
        (when (= status "running") [:span.side-dot])]])]])

(defn- session-page [sid]
  (let [soul     (vis/gateway-soul sid)
        turns    (reverse (vis/gateway-list-turns sid))
        snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))]
    (page (or (:title soul) "session")
      [:div.app {:hx-ext "sse"
                 :sse-connect (str "/ui/session/" sid "/stream")}
       [:header.bar
        [:button#toggle-left.bar-toggle {:type "button" :aria-label "Toggle sessions"}
         (icon "sidebar")]
        [:div.bar-title
         [:span.bar-name (or (:title soul) "Untitled")]
         (status-chip (:status soul))]
        [:span.session-id (subs (str sid) 0 8)]
        [:button.bar-toggle {:type "button" :aria-label "Providers"
                             :hx-get "/ui/providers" :hx-target "#modal" :hx-swap "innerHTML"}
         (icon "zap")]
        [:button.bar-toggle {:type "button" :aria-label "Settings"
                             :hx-get "/ui/settings" :hx-target "#modal" :hx-swap "innerHTML"}
         (icon "settings")]
        [:button#toggle-right.bar-toggle.flip {:type "button" :aria-label "Toggle context"}
         (icon "sidebar")]]
       [:div#modal]
       [:div.layout
        (sessions-sidebar sid)
        ;; ONE center flex column holds the thread AND the composer dock,
        ;; so both center in the SAME box between the rails — the input
        ;; can never drift out of line with the chat column again.
        [:div.center
         [:main.thread
          [:div.column
           (if (seq turns)
             (map turn-block turns)
             [:div.hello-wrap
              [:h1.hello "What are we building?"]
              [:p.hello-sub "vis works in this workspace — ask for anything."]])
           ;; Live bubbles land here (user message from the form response,
           ;; the answer from the `message` SSE event). Work below holds
           ;; ONLY machinery: code, results, iteration ticks.
           [:div#live.live {:sse-swap "message" :hx-swap "beforeend"}]
           [:div#thinking.thinking {:sse-swap "thinking" :hx-swap "innerHTML"}]
           [:details.work
            [:summary "Work"]
            [:div#activity.activity {:sse-swap "activity" :hx-swap "beforeend"}]]
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
           [:button.send {:type "submit" :aria-label "Send"} (icon "arrow-up")]]]]
        [:aside.rail {:sse-swap "context" :hx-swap "innerHTML"}
         (if snapshot
           (context-panel snapshot)
           [:div#context.context
            [:div.rail-head [:h2 "Context"]]
            [:p.empty "wakes on the first turn"]])]]])))

;; =============================================================================
;; Handlers
;; =============================================================================

(declare ^:private source-watcher)

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
  (force source-watcher)
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
  (force source-watcher)
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (if (and sid (vis/gateway-soul sid))
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (session-page sid)}
      {:status 303 :headers {"Location" "/ui"} :body ""})))

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

(defn- settings-handler
  "GET /ui/settings — the TUI settings dialog as an overlay: every
   registered toggle, grouped, flippable in place."
  [_request]
  (let [toggles (try (vis/registered-toggles) (catch Throwable _ []))
        grouped (group-by #(or (:group %) :other) toggles)]
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Settings"
             (for [[group specs] (sort-by (comp str key) grouped)]
               [:section.modal-section
                [:h3 (name group)]
                (map toggle-row specs)]))}))

(defn- settings-mutate-handler
  "POST /ui/settings/toggle | /ui/settings/cycle — flip or cycle one
   toggle, answer with the refreshed row."
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
         :body (html (toggle-row (or spec {:id id :label (str id)})))}))))

(defn- providers-handler
  "GET /ui/providers — the registered provider fleet plus the active
   provider/model, mirroring `vis providers list`."
  [_request]
  (let [providers (try (vis/registered-providers) (catch Throwable _ []))
        active (try (vis/resolve-effective-model (vis/get-router))
                 (catch Throwable _ nil))]
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (modal-shell "Providers"
             (when active
               [:p.active-model
                "Active: " [:strong (str (some-> (:provider active) name) "/" (:name active))]])
             [:div.provider-rows
              (for [{:provider/keys [id doc]} providers
                    :let [active? (= id (:provider active))]]
                [:div.provider-row {:class (when active? "active")}
                 [:span.provider-dot {:class (when active? "on")}]
                 [:div.provider-text
                  [:div.provider-name (name id)]
                  (when doc [:div.provider-doc (str doc)])]])])}))

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

(def ^:private APP_CSS
  "/* vis web companion - vis-light, chat-first */
/* ── reset (modern-normalize spirit): same rendering everywhere ──── */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
html{-webkit-text-size-adjust:100%;tab-size:4}
img,svg,video,canvas{display:block;max-width:100%}
input,button,textarea,select{font:inherit;color:inherit;letter-spacing:inherit}
button{background:none;border:0;cursor:pointer}
p,h1,h2,h3,h4,h5,h6{overflow-wrap:break-word}
ul[class],ol[class]{list-style:none}
a{text-decoration:none;color:inherit}
summary{list-style:none}summary::-webkit-details-marker{display:none}
:root{--bg:#ffffff;--panel2:#f8f8f8;--code-bg:#f0f3f8;--cream:#f8f4eb;
--line:#ececec;--line2:#dcdcdc;--fg:#1e1e1e;--dim:#8a8a8a;
--gold:#facc15;--gold2:#be9628;--amber:#a16207;--amber-deep:#503c00;--warn-bg:#fff5b4;
--indigo:#2563eb;--err:#dc3232;--ok:#28a03c;
--radius:16px;--shadow:0 1px 2px rgba(20,20,20,.05),0 6px 24px rgba(20,20,20,.07);
--mono:ui-monospace,SFMono-Regular,Menlo,monospace;
--serif:Charter,'Iowan Old Style',Georgia,serif}
html,body{height:100%}
body{background:var(--bg);color:var(--fg);
font:15px/1.6 -apple-system,BlinkMacSystemFont,'Segoe UI',Inter,sans-serif;
-webkit-font-smoothing:antialiased}
a{color:var(--indigo);text-decoration:none}
::selection{background:var(--warn-bg)}
.empty{color:var(--dim);font-style:italic}
@keyframes rise{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}
/* ── app shell ─────────────────────────────────────────────────── */
.app{height:100dvh;display:flex;flex-direction:column}
.bar{display:flex;align-items:center;gap:.8rem;padding:.6rem 1.1rem;
position:sticky;top:0;z-index:10;background:rgba(255,255,255,.82);
backdrop-filter:blur(12px) saturate(1.4);border-bottom:1px solid var(--line)}
.bar .back{font-size:1.05rem;color:var(--dim);padding:.1rem .45rem;border-radius:8px}
.bar .back:hover{background:var(--panel2);color:var(--fg)}
.bar-title{display:flex;align-items:center;gap:.6rem;min-width:0}
.bar-name{font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.bar-toggle{width:30px;height:30px;border-radius:8px;color:var(--dim);font-size:.95rem;
display:flex;align-items:center;justify-content:center;transition:background .12s,color .12s}
.bar-toggle:hover{background:var(--panel2);color:var(--fg)}
.app.hide-left .sidebar{display:none}
.app.hide-right .rail{display:none}
.session-id{margin-left:auto;font-family:var(--mono);font-size:.72rem;color:var(--dim)}
.layout{flex:1;display:flex;min-height:0}
/* center = thread + dock in ONE box between the rails, so the chat
   column and the composer share the exact same centering geometry */
.center{flex:1;display:flex;flex-direction:column;min-width:0}
/* ── thread ────────────────────────────────────────────────────── */
/* no scroll-behavior:smooth — the initial jump-to-bottom must be
   INSTANT or every refresh visibly scrolls down the whole thread */
.thread{flex:1;overflow-y:auto;scrollbar-gutter:stable both-edges}
.column{max-width:46rem;margin:0 auto;padding:1.6rem 1.2rem 2.5rem;
display:flex;flex-direction:column;gap:1.3rem}
.hello-wrap{margin:16vh auto 0;text-align:center}
.hello{font-size:1.7rem;font-weight:650;letter-spacing:-.01em}
.hello-sub{color:var(--dim);margin-top:.5rem}
/* TUI bubble anatomy: WHITE boxes both roles, colored role labels
   (theme tokens: :user-role-fg #825a00 amber, :ai-role-fg #50a050
   green), cream turn-separator band (:turn-separator-bg/-fg), footer =
   the canonical meta-summary-line. No avatars, no status chips. */
/* turn separator: pure breathing room, no band/border */
.tsep{height:.6rem}
.bubble{overflow-wrap:anywhere}
/* animate ONLY live arrivals — replaying the entrance on every page
   refresh for the whole history reads as flicker */
#live .bubble,#live .tsep{animation:rise .25s ease both}
.role{font-size:.72rem;font-weight:750;letter-spacing:.05em;margin-bottom:.3rem}
.role-user{color:#825a00}
.role-vis{color:#50a050}
.bubble-foot{display:flex;gap:.7rem;align-items:baseline;margin-top:.55rem;
font-size:.72rem;color:var(--dim);font-family:var(--mono)}
.foot-bad{color:var(--err);font-weight:700}
/* ── .md: ONE complete markdown typography system. Styles BOTH the
   server-side IR fallback and marked's client-rendered output, in
   bubbles AND Context-rail fact contents. Headings stay close to body
   size (chat, not a document), lists get real markers (the reset
   strips them from classed lists), everything tight. */
.md{line-height:1.65;overflow-wrap:anywhere}
.prose.md{font-family:var(--serif);font-size:1.02rem}
.fact-content.md{font-size:.84rem;margin:.4rem 0}
.md p{margin:.55rem 0}
.md h1,.md h2,.md h3,.md h4,.md h5,.md h6{
font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
font-weight:650;line-height:1.3;margin:.9rem 0 .35rem}
.md h1{font-size:1.15rem}.md h2{font-size:1.05rem}
.md h3{font-size:.95rem}.md h4,.md h5,.md h6{font-size:.9rem}
.md ul,.md ol{margin:.5rem 0;padding-left:1.45rem}
.md ul{list-style:disc}.md ol{list-style:decimal}
.md ul ul{list-style:circle}
.md li{margin:.2rem 0}.md li p{margin:.15rem 0}
.md code{font-family:var(--mono);background:var(--code-bg);
border-radius:4px;padding:.06rem .32rem;font-size:.85em;color:var(--amber)}
.md pre{background:var(--code-bg);border:1px solid var(--line);
border-radius:10px;padding:.6rem .8rem;overflow-x:auto;margin:.55rem 0;line-height:1.5}
.md pre code{background:none;border-radius:0;padding:0;color:var(--fg);font-size:.8rem}
.md blockquote{border-left:3px solid var(--gold);padding-left:.9rem;color:var(--dim);margin:.55rem 0}
.md hr{border:0;border-top:1px solid var(--line);margin:.8rem 0}
.md table{border-collapse:collapse;margin:.55rem 0;font-size:.88rem;
font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
.md td,.md th{border:1px solid var(--line2);padding:.3rem .6rem}
.md th{background:var(--panel2)}
.md a{color:var(--indigo);text-decoration:underline}
.md > :first-child{margin-top:0}
.md > :last-child{margin-bottom:0}
/* typing dots */
.dots{display:inline-flex;gap:5px;padding:.4rem 0}
.dots span{width:7px;height:7px;border-radius:50%;background:var(--gold2);
animation:blink 1.2s infinite ease-in-out}
.dots span:nth-child(2){animation-delay:.18s}
.dots span:nth-child(3){animation-delay:.36s}
@keyframes blink{0%,80%,100%{opacity:.25;transform:scale(.85)}40%{opacity:1;transform:scale(1)}}
.thinking{min-height:1rem;font-size:.8rem;color:var(--dim);font-style:italic;overflow-wrap:anywhere}
/* work disclosure */
.work{border:1px solid var(--line);border-radius:12px;background:var(--panel2)}
.work summary{cursor:pointer;list-style:none;padding:.45rem .9rem;font-size:.74rem;
font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--dim);user-select:none}
.work summary::before{content:'▸ ';color:var(--gold2)}
.work[open] summary::before{content:'▾ '}
.activity{display:flex;flex-direction:column;gap:.4rem;padding:.2rem .9rem .8rem}
.act{border-left:2px solid var(--line2);padding:.15rem .7rem;font-size:.8rem;animation:rise .2s ease both}
.act-code{border-left-color:var(--indigo)}
.act-result{border-left-color:var(--gold)}
.act-error{border-left-color:var(--err)}.act .act-error{color:var(--err)}
.act-answer{border-left-color:var(--ok)}
.act-sent,.act-turn{border-left-color:var(--gold)}
/* chips */
.chip{font-size:.66rem;font-weight:650;text-transform:lowercase;letter-spacing:.03em;
border-radius:99px;padding:.1rem .55rem;background:var(--panel2);color:var(--dim)}
.chip-running{background:var(--warn-bg);color:var(--amber)}
.chip-completed,.chip-idle{background:#e9f7ec;color:var(--ok)}
.chip-failed,.chip-cancelled{background:#fdeaea;color:var(--err)}
.chip-suspended{background:#e8eefc;color:var(--indigo)}
/* code */
.ir-pre{background:var(--code-bg);border:1px solid var(--line);border-radius:10px;
padding:.6rem .8rem;overflow-x:auto;font-size:.8rem;margin:.4rem 0;line-height:1.5}
.ir-pre code,.ir-code{font-family:var(--mono)}
.ir-code{background:var(--code-bg);border-radius:5px;padding:.06rem .32rem;font-size:.86em;color:var(--amber)}
/* suggestions popup (slash + @files) */
.dock{position:relative}
.suggest{position:absolute;bottom:100%;left:50%;transform:translateX(-50%);
width:min(44rem,92%);margin-bottom:.4rem;background:var(--bg);
border:1px solid var(--line2);border-radius:12px;box-shadow:var(--shadow);
overflow:hidden;z-index:20}
.suggest-row{display:flex;gap:.8rem;align-items:baseline;padding:.45rem .9rem;
cursor:pointer;font-size:.85rem}
.suggest-row.active,.suggest-row:hover{background:var(--warn-bg)}
.suggest-name{font-family:var(--mono);color:var(--amber);white-space:nowrap}
.suggest-doc{color:var(--dim);font-size:.78rem;overflow:hidden;
text-overflow:ellipsis;white-space:nowrap}
.slash-error{color:var(--err)}
/* ── composer dock ─────────────────────────────────────────────── */
.dock{padding:.4rem 1.2rem 1.1rem;background:linear-gradient(to top,var(--bg) 65%,rgba(255,255,255,0))}
.composer{max-width:46rem;margin:0 auto;display:flex;align-items:flex-end;gap:.5rem;
background:var(--bg);border:1px solid var(--line2);border-radius:24px;
padding:.5rem .55rem .5rem 1.1rem;box-shadow:var(--shadow);transition:border-color .15s}
.composer:focus-within{border-color:var(--gold)}
.composer textarea{flex:1;border:0;outline:0;resize:none;background:transparent;
font:inherit;line-height:1.5;max-height:200px;padding:.25rem 0}
.composer input{flex:1;border:0;outline:0;background:transparent;font:inherit;padding:.25rem 0}
.send{flex:none;width:34px;height:34px;border-radius:50%;border:0;cursor:pointer;
background:var(--gold);color:var(--amber-deep);font-size:1.05rem;font-weight:800;
display:flex;align-items:center;justify-content:center;transition:filter .15s}
.send:hover{filter:brightness(1.07)}
/* ── sessions index ────────────────────────────────────────────── */
.index{padding-top:14vh;gap:1.6rem}
.index .hello{text-align:center}
.newsession{margin-top:.2rem}
.sessions-block{margin-top:1.6rem}
.block-label{font-size:.72rem;text-transform:uppercase;letter-spacing:.1em;color:var(--dim);margin-bottom:.6rem}
.sessions{list-style:none;display:flex;flex-direction:column;gap:.45rem}
.session-row{display:flex;align-items:center;gap:.7rem;background:var(--bg);
border:1px solid var(--line);border-radius:14px;padding:.75rem 1rem;color:var(--fg);
transition:border-color .12s,box-shadow .12s,transform .12s}
.session-row:hover{border-color:var(--gold);box-shadow:var(--shadow);transform:translateY(-1px)}
.session-title{font-weight:600}
.session-spacer{flex:1}
/* ── sessions sidebar (left) ───────────────────────────────────── */
.sidebar{width:15.5rem;flex:none;overflow-y:auto;border-right:1px solid var(--line);
padding:1rem .8rem;background:#fcfcfb;display:flex;flex-direction:column;gap:.9rem}
@media(max-width:60rem){.sidebar{display:none}}
.newchat-btn{width:100%;text-align:left;border:1px dashed var(--line2);border-radius:10px;
padding:.55rem .8rem;font-weight:600;font-size:.85rem;color:var(--amber);
transition:border-color .12s,background .12s}
.newchat-btn:hover{border-color:var(--gold);background:var(--warn-bg)}
.side-sessions{display:flex;flex-direction:column;gap:.15rem}
.side-row{display:flex;align-items:center;gap:.5rem;border-radius:9px;
padding:.45rem .7rem;font-size:.86rem;color:var(--fg);white-space:nowrap;
overflow:hidden;text-overflow:ellipsis;transition:background .1s}
.side-row:hover{background:var(--panel2)}
.side-row.active{background:var(--warn-bg);font-weight:600}
.side-title{overflow:hidden;text-overflow:ellipsis;flex:1}
.side-dot{flex:none;width:7px;height:7px;border-radius:50%;background:var(--gold2);
animation:blink 1.2s infinite ease-in-out}
/* live bubbles region */
.live{display:flex;flex-direction:column;gap:1.3rem}
.live:empty{display:none}
/* icons (vendored Feather sprite) */
.icon{width:16px;height:16px;display:block}
.flip .icon{transform:scaleX(-1)}
@keyframes spin{to{transform:rotate(360deg)}}
.icon.spin{animation:spin 1s linear infinite}
/* mic + live waveform + recording controls */
.composer.recording,.composer.transcribing-on{align-items:center}
.composer.recording textarea{display:none}
.composer.recording .mic,.composer.recording .send{display:none}
.composer.transcribing-on textarea{display:none}
.composer.transcribing-on .send{display:none}
.transcribing{flex:1;display:flex;align-items:center;justify-content:center;gap:.5rem;
height:34px;color:var(--amber);font-size:.85rem}
.rec-time{flex:none;font-family:var(--mono);font-size:.8rem;color:var(--amber);
min-width:2.6rem;text-align:center}
.rec-cancel,.rec-accept{flex:none;width:34px;height:34px;border-radius:50%;
display:flex;align-items:center;justify-content:center;font-size:.95rem;
font-weight:700;transition:filter .15s,background .15s}
.rec-cancel{background:var(--panel2);color:var(--err)}
.rec-cancel:hover{background:#fdeaea}
.rec-accept{background:var(--gold);color:var(--amber-deep)}
.rec-accept:hover{filter:brightness(1.07)}
.send:disabled{background:var(--line2);color:var(--bg);cursor:default;filter:none}
.wave{flex:1;display:flex;align-items:center;justify-content:center;gap:3px;
height:34px;padding:0 .4rem}
.wave span{width:3px;min-height:4px;height:4px;border-radius:2px;
background:linear-gradient(180deg,var(--gold),var(--gold2));
transition:height .09s ease;box-shadow:0 0 6px rgba(250,204,21,.35)}
.mic{flex:none;width:34px;height:34px;border-radius:50%;display:flex;align-items:center;
justify-content:center;color:var(--dim);transition:color .15s,background .15s}
.mic:hover{background:var(--panel2);color:var(--amber)}
.mic.recording{background:var(--err);color:#fff;animation:pulse 1.2s infinite}
.mic.mic-error{color:var(--err)}
@keyframes pulse{0%,100%{box-shadow:0 0 0 0 rgba(220,50,50,.35)}50%{box-shadow:0 0 0 7px rgba(220,50,50,0)}}
/* ── context rail ──────────────────────────────────────────────── */
.rail{width:21rem;flex:none;overflow-y:auto;border-left:1px solid var(--line);
padding:1.2rem 1.1rem;background:#fcfcfb}
@media(max-width:68rem){.rail{display:none}}
.rail-head{display:flex;align-items:center;justify-content:space-between;gap:.8rem;margin-bottom:.9rem}
.rail-head h2{font-size:.78rem;text-transform:uppercase;letter-spacing:.12em;color:var(--gold2)}
.rail-section{border-top:1px solid var(--line);padding:.85rem 0}
.rail-section h3{font-size:.7rem;text-transform:uppercase;letter-spacing:.1em;color:var(--dim);margin-bottom:.5rem}
.util{display:flex;align-items:center;gap:.5rem;min-width:7rem}
.util-track{flex:1;height:5px;border-radius:3px;background:var(--panel2);overflow:hidden}
.util-fill{height:100%;background:linear-gradient(90deg,var(--gold),var(--gold2))}
.util-label{font-size:.68rem;color:var(--dim);white-space:nowrap}
.tasks{list-style:none;display:flex;flex-direction:column;gap:.4rem}
.task{display:flex;gap:.55rem;align-items:baseline;font-size:.84rem}
.task-glyph{color:var(--dim)}
.task-done .task-glyph,.task-completed .task-glyph{color:var(--ok)}
.task-candidate .task-glyph{color:var(--indigo)}
.task-in_progress .task-glyph,.task-running .task-glyph{color:var(--amber)}
.facts{display:flex;flex-direction:column;gap:.45rem}
.fact{background:var(--bg);border:1px solid var(--line);border-radius:10px;padding:.45rem .65rem}
.fact summary{cursor:pointer;list-style:none}
.fact-key{font-family:var(--mono);font-size:.78rem;color:var(--amber);
background:var(--warn-bg);border-radius:5px;padding:.04rem .4rem}
.fact-content{font-size:.83rem;margin:.4rem 0}
.fact-path{font-family:var(--mono);font-size:.74rem;color:var(--indigo)}
.fact-hash{font-family:var(--mono);font-size:.68rem;color:var(--gold2)}
/* ── auth ──────────────────────────────────────────────────────── */
.auth{min-height:100dvh;display:flex;align-items:center;justify-content:center;padding:1rem}
.auth-card{width:22rem;text-align:center;display:flex;flex-direction:column;gap:.9rem;
background:var(--bg);border:1px solid var(--line);border-radius:var(--radius);
box-shadow:var(--shadow);padding:2.2rem 1.8rem}
.auth-card h1{font-size:2.4rem;letter-spacing:.03em}
.auth-card h1{border-bottom:4px solid var(--gold);margin:0 auto;padding:0 .4rem .2rem}
.tagline{color:var(--dim)}
.auth-card form{display:flex;flex-direction:column;gap:.6rem}
.auth-card input{border:1px solid var(--line2);border-radius:10px;padding:.6rem .8rem;font:inherit}
.auth-card input:focus{outline:2px solid var(--gold);border-color:var(--gold)}
.send-wide{border:0;border-radius:10px;padding:.6rem;cursor:pointer;
background:var(--gold);color:var(--amber-deep);font-weight:700;font-size:.95rem}
.auth-error{color:var(--err);font-size:.85rem}
.auth-hint{color:var(--dim);font-size:.78rem}
/* ── modals (the TUI dialogs as overlays) ──────────────────────── */
.overlay{position:fixed;inset:0;background:rgba(30,30,30,.35);z-index:50;
display:flex;align-items:center;justify-content:center;padding:1.2rem;
animation:rise .15s ease both}
.modal{width:min(34rem,100%);max-height:80dvh;overflow-y:auto;background:var(--bg);
border:1px solid var(--line);border-top:4px solid var(--gold);border-radius:14px;
box-shadow:0 8px 40px rgba(20,20,20,.18)}
.modal-head{display:flex;align-items:center;justify-content:space-between;
padding:.8rem 1.1rem;border-bottom:1px solid var(--line);position:sticky;top:0;
background:var(--bg)}
.modal-head h2{font-size:1rem}
.modal-body{padding:.6rem 1.1rem 1.1rem}
.modal-section{padding:.6rem 0}
.modal-section h3{font-size:.7rem;text-transform:uppercase;letter-spacing:.1em;
color:var(--dim);margin:.4rem 0 .5rem}
/* settings rows */
.toggle-row{display:flex;align-items:center;gap:1rem;padding:.5rem 0;
border-bottom:1px solid var(--panel2)}
.toggle-text{flex:1;min-width:0}
.toggle-label{font-weight:600;font-size:.9rem}
.toggle-desc{font-size:.78rem;color:var(--dim);margin-top:.1rem}
.switch{flex:none;width:40px;height:22px;border-radius:99px;background:var(--line2);
position:relative;transition:background .15s;cursor:pointer}
.switch .knob{position:absolute;top:2px;left:2px;width:18px;height:18px;
border-radius:50%;background:var(--bg);box-shadow:0 1px 2px rgba(20,20,20,.25);
transition:left .15s}
.switch.on{background:var(--gold)}
.switch.on .knob{left:20px}
.toggle-cycle{flex:none;font-family:var(--mono);font-size:.8rem;color:var(--amber);
background:var(--warn-bg);border-radius:7px;padding:.25rem .7rem;cursor:pointer}
/* provider rows */
.active-model{font-size:.88rem;margin:.4rem 0 .7rem;color:var(--dim)}
.active-model strong{color:var(--amber)}
.provider-rows{display:flex;flex-direction:column}
.provider-row{display:flex;align-items:center;gap:.8rem;padding:.55rem 0;
border-bottom:1px solid var(--panel2)}
.provider-row.active .provider-name{color:var(--amber)}
.provider-dot{flex:none;width:8px;height:8px;border-radius:50%;background:var(--line2)}
.provider-dot.on{background:var(--gold);box-shadow:0 0 6px rgba(250,204,21,.5)}
.provider-text{min-width:0}
.provider-name{font-weight:600;font-size:.9rem;font-family:var(--mono)}
.provider-doc{font-size:.78rem;color:var(--dim)}
/* scrollbars */
*::-webkit-scrollbar{width:10px;height:10px}
*::-webkit-scrollbar-thumb{background:var(--line2);border-radius:5px;border:2px solid var(--bg)}
*::-webkit-scrollbar-track{background:transparent}
")

(defn- css-handler [_]
  {:status 200
   :headers {"Content-Type" "text/css; charset=utf-8"
             "Cache-Control" "no-cache"}
   :body APP_CSS})

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
   ["/ui/dev-reload" {:get #'dev-reload-handler}]
   ["/ui/settings" {:get #'settings-handler}]
   ["/ui/settings/toggle" {:post #'settings-mutate-handler}]
   ["/ui/settings/cycle" {:post #'settings-mutate-handler}]
   ["/ui/providers" {:get #'providers-handler}]
   ["/ui/sessions" {:post #'create-session-handler}]
   ["/ui/session/:sid" {:get #'session-handler}]
   ["/ui/slash" {:get #'slash-list-handler}]
   ["/ui/session/:sid/files" {:get #'files-handler}]
   ["/ui/session/:sid/turns" {:post #'submit-turn-handler}]
   ["/ui/session/:sid/voice" {:post #'voice-handler}]
   ["/ui/session/:sid/stream" {:get #'stream-handler}]])

(defn- ui-contribution
  "The gateway pulls this through the `:gateway.slot/http-routes`
   whiteboard slot whenever it (re)builds its handler — no registration
   call, no ordering requirement between gateway start and this
   extension loading."
  []
  {:prefix "/ui"
   :routes ui-routes
   :open-uris #{"/ui" "/ui/auth" "/ui/app.css" "/ui/icons.svg"
                "/ui/js/htmx.min.js" "/ui/js/htmx-sse.js" "/ui/js/marked.min.js"
                "/ui/js/ui.js" "/ui/js/dev-reload.js"}
   :request-authed-fn ui-authed?
   :on-unauthorized (fn [_request] {:status 303 :headers {"Location" "/ui"} :body ""})
   :form-params? true})

;; =============================================================================
;; Dev hot-reload: source watcher
;; =============================================================================
;;
;; The browser auto-reload reacts to `ui-load-stamp` moving — which only
;; happens when THIS NAMESPACE reloads inside the running JVM. A `git
;; pull` or an editor save changes files on DISK; a running daemon never
;; sees that by itself. This watcher closes the gap: when vis runs from
;; a source checkout (resources resolve to file: URLs, not jars), a
;; parked virtual thread polls the mtimes of this namespace's source
;; file and the vendored public/ assets; on change it `:reload`s the
;; namespace in-process — the stamp moves, every connected browser
;; refreshes, and the #'var handlers serve the new code. Edit (or pull)
;; → daemon hot-reloads → tab repaints. No restart.

(defn- watched-files []
  (let [as-file (fn [resource-path]
                  (when-let [url (io/resource resource-path)]
                    (when (= "file" (.getProtocol url))
                      (io/file (.toURI url)))))
        src (as-file "com/blockether/vis/ext/channel_web/core.clj")
        pub (some-> (as-file "vis-channel-web/public/ui.js") (.getParentFile))]
    (concat
      (when src [src])
      (when pub (.listFiles pub)))))

(defn- watched-stamp []
  (reduce max 0 (map #(.lastModified ^java.io.File %) (watched-files))))

(defonce ^:private source-watcher
  ;; Forced from the page handlers, NOT at namespace load: the watcher
  ;; thread starts on the FIRST /ui open, so TUI runs, one-shot CLI
  ;; invocations, and compile checks never spin it. `delay` makes the
  ;; force idempotent; `defonce` keeps one watcher across :reloads.
  (delay
    (when (seq (watched-files))
      (vis/worker-future "vis-web-source-watcher"
        (fn []
          (loop [last-stamp (watched-stamp)]
            (Thread/sleep 1500)
            (let [now (try (watched-stamp) (catch Throwable _ last-stamp))]
              (when (> now last-stamp)
                (try
                  (require 'com.blockether.vis.ext.channel-web.core :reload)
                  (catch Throwable _ nil)))
              (recur (max now last-stamp)))))))))

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
     :ext/description "Web companion - the gateway /ui chat instrument (hiccup + HTMX + SSE)."
     :ext/version     "0.2.0"
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
